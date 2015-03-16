unit mainwindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Dialogs, StdCtrls,
  ComCtrls, Buttons, ExtCtrls, Menus, SSockets, Math, LCLType;

const
  MaximumSelectors = 4096;
  MaximumHistory = 4096;

  ByteBufferSize = 1024 * 64; // 64kb

  InitialGopherHole = 'gopher://gopher.floodgap.com';

type
  { Gopher stuff }
  TGopherSelectorType = (gstPlainText, gstDirectory,
    gstError, gstSearch, gstBinary, gstInfo, gstImage, gstEmpty, gstUnknown);

  TGopherSelector = record
    SelectorType: TGopherSelectorType;
    Caption: string;
    Selector: string;
    Hostname: string;
    Port: word;
  end;

  TUserAction = (uaViewSelector, uaDownloadSelector);

  { TForm1 }

  TForm1 = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Image1: TImage;
    ImageList1: TImageList;
    Label3: TLabel;
    ListView1: TListView;
    Memo1: TMemo;
    MenuItem1: TMenuItem;
    PopupMenu1: TPopupMenu;
    GopherUriEdit: TEdit;
    SaveDialog1: TSaveDialog;
    Timer1: TTimer;
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GopherUriEditChange(Sender: TObject);
    procedure GopherUriEditKeyDown(Sender: TObject; var Key: word;
      Shift: TShiftState);
    procedure ListView1DblClick(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
    FEditedSelector: TGopherSelector;
    FEditedSelectorValid: boolean;
    FCurrentSelector: TGopherSelector;
    FCurrentReadBytes: integer;
    FMemoryStream: TMemoryStream;
    FInetSocket: TInetSocket;
    FSelectorList: array[1..MaximumSelectors] of TGopherSelector;
    FSelectorHistory: array[1..MaximumHistory] of TGopherSelector;
    FHistoryCount: integer;
    FUserAction: TUserAction;

    procedure LoadSelector(const Selector: TGopherSelector;
      const UserAction: TUserAction);

    procedure AddHistory(const Selector: TGopherSelector);
    procedure LoadView;
    procedure HideElements;

    procedure LoadDirectory;
    procedure LoadImage;
    procedure LoadText;
    procedure DownloadSelector;

  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{
    Parse a given string to a gopher selector
}
function ParseGopherURI(const s: string; out Selector: TGopherSelector): boolean;
const
  UriStart = 'gopher://';
var
  i, l: integer;
  tmp: ShortString;

  // little helper to convert the port
  function ParsePort: boolean;
  var
    port: integer;
  begin
    Result := False;
    if not TryStrToInt(Trim(tmp), port) then
      Exit;
    if not (port in [0..65535]) then
      Exit;
    // ok..parsed port
    Selector.Port := port;
    Result := True;
  end;

begin
  // reset selector
  Selector.Port := 70;
  Selector.Hostname := '';
  Selector.Caption := '';
  Selector.Selector := '/';
  Selector.SelectorType := gstDirectory;
  Result := False;

  // check for gopher://
  if LeftStr(s, Length(UriStart)) = UriStart then
    i := Length(UriStart) + 1
  else
    i := 1;
  l := Length(s);
  if i > l then
    Exit;

  // parse hostname
  tmp := '';
  repeat
    tmp := tmp + s[i];
    Inc(i);
    if i > l then
    begin
      Selector.Hostname := Trim(tmp);
      Result := True;
      Exit;
    end;
  until s[i] in [':', '/'];
  Selector.Hostname := Trim(tmp);
  if Selector.Hostname = '' then
    Exit;

  // parse port
  if s[i] = ':' then
  begin
    tmp := '';
    Inc(i);
    if i > l then
      exit;
    repeat
      tmp := tmp + s[i];
      Inc(i);
      // check end of string
      if i > l then
      begin
        if not ParsePort then
          Exit;
        Result := True;
        Exit;
      end;
    until s[i] = '/';
    if not ParsePort then
      Exit;
  end;

  // "parse" selector
  Selector.Selector := Trim(RightStr(s, l - i));
  Selector.Selector := StringReplace(Selector.Selector, '%09', Chr(9), [rfReplaceAll]);
  if Selector.Selector = '' then
    Selector.Selector := '/';

  Result := True; // ok now...
end;

function CreateGopherUri(const Selector: TGopherSelector): string;
begin
  Result := 'gopher://' + Selector.Hostname + ':' + IntToStr(Selector.Port) +
    Selector.Selector;
end;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  FHistoryCount := 0;
  BitBtn2.Enabled := False;
  HideElements;
  GopherUriEdit.Text := InitialGopherHole; // should invoke OnChange
end;

procedure TForm1.GopherUriEditChange(Sender: TObject);
begin
  // try to decode the given gopher URI
  FEditedSelectorValid := ParseGopherURI(Trim(GopherUriEdit.Text), FEditedSelector);
  BitBtn1.Enabled := FEditedSelectorValid;
end;

procedure TForm1.GopherUriEditKeyDown(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  if FEditedSelectorValid and (Key = VK_RETURN) then
  begin
    AddHistory(FEditedSelector);
    LoadSelector(FEditedSelector, uaViewSelector);
  end;
end;

procedure TForm1.BitBtn1Click(Sender: TObject);
begin
  if FEditedSelectorValid then
  begin
    AddHistory(FEditedSelector);
    LoadSelector(FEditedSelector, uaViewSelector);
  end;
end;

procedure TForm1.ListView1DblClick(Sender: TObject);
var
  Selector: TGopherSelector;
begin
  // get selector and load it
  Selector := FSelectorList[ListView1.Selected.Index + 1];
  if Selector.SelectorType in [gstPlainText, gstDirectory, gstImage] then
  begin
    AddHistory(Selector);
    LoadSelector(Selector, uaViewSelector);
  end;
end;

{
    Download selected Selector to disk...
}
procedure TForm1.MenuItem1Click(Sender: TObject);
var
  Selector: TGopherSelector;
begin
  // get selector and load it for download
  Selector := FSelectorList[ListView1.Selected.Index + 1];
  if Selector.SelectorType in [gstPlainText, gstImage, gstBinary] then
  begin
    AddHistory(Selector);
    LoadSelector(Selector, uaDownloadSelector);
  end;
end;

{
    Recurring timer tick to download bytes from server.
    When all bytes are downloaded, the FUserAction variable will be checked
    to see what to do.
}
procedure TForm1.Timer1Timer(Sender: TObject);
var
  buffer: array[1..ByteBufferSize] of byte;
  ReadBytes: integer;
begin
  ReadBytes := FInetSocket.Read(buffer, ByteBufferSize);
  if ReadBytes > 0 then
  begin
    FMemoryStream.WriteBuffer(buffer, ReadBytes);
    Inc(FCurrentReadBytes, ReadBytes);
    Label3.Caption := 'Read ' + IntToStr(FCurrentReadBytes div 1024) + 'KB ...';
  end
  else
  begin
    Timer1.Enabled := False;
    Label3.Caption := Label3.Caption + ' done.';
    FMemoryStream.Seek(0, soFromBeginning);
    FreeAndNil(FInetSocket);
    case FUserAction of
      uaViewSelector: LoadView;
      uaDownloadSelector: DownloadSelector;
    end;
  end;
end;

{
    History stuff...
}
procedure TForm1.AddHistory(const Selector: TGopherSelector);
begin
  Inc(FHistoryCount);
  if FHistoryCount > MaximumHistory then
    FHistoryCount := 1;
  FSelectorHistory[FHistoryCount] := Selector;
  BitBtn2.Enabled := FHistoryCount > 1;
end;

{
    Hide elements
}
procedure TForm1.HideElements;
begin
  ListView1.Visible := False;
  Image1.Visible := False;
  Memo1.Visible := False;
  Label3.Visible := False;
end;

{
    Start loading the requested selector
}
procedure TForm1.LoadSelector(const Selector: TGopherSelector;
  const UserAction: TUserAction);
var
  SelectorString: string;
begin
  // abort if any timer is running
  if Timer1.Enabled then
    exit;

  // hide elements and setup things
  HideElements;
  Label3.Caption := '';
  Label3.Visible := True;
  FCurrentReadBytes := 0;
  FCurrentSelector := Selector;
  FMemoryStream := TMemoryStream.Create;
  FUserAction := UserAction;
  GopherUriEdit.Text := CreateGopherUri(Selector);

  // connect to host
  SelectorString := Selector.Selector + Chr(13) + Chr(10);
  FInetSocket := TInetSocket.Create(Selector.Hostname, Selector.Port);
  FInetSocket.WriteBuffer(Pointer(SelectorString)^, Length(SelectorString));

  // start timer
  Timer1.Enabled := True;
end;

{
    Loads the view from the MemoryStream data...
}
procedure TForm1.LoadView;
begin
  // load appropiate view
  case FCurrentSelector.SelectorType of
    gstDirectory: LoadDirectory;
    gstImage: LoadImage;
    gstPlainText: LoadText;
  end;
  FreeAndNil(FMemoryStream);
end;

{
    Load a Gopher Directory
}
procedure TForm1.LoadDirectory;
var
  i, items: integer;
  item: TListItem;
  Lines, Parts: TStringList;
begin
  // clear entries
  for i := 1 to MaximumSelectors do
    with FSelectorList[i] do
    begin
      Caption := '';
      Hostname := '';
      Selector := '';
      Port := 0;
      SelectorType := gstUnknown;
    end;

  // load lines
  Lines := TStringList.Create;
  Lines.TextLineBreakStyle := tlbsCRLF;
  Lines.LoadFromStream(FMemoryStream);

  // parse lines
  Parts := TStringList.Create;
  Parts.Delimiter := Chr(9);
  Parts.StrictDelimiter := True;
  items := Min(Lines.Count, MaximumSelectors);
  for i := 1 to items do
    with FSelectorList[i] do
    begin
      Parts.DelimitedText := Lines.Strings[i - 1];
      if Parts.Count = 4 then
      begin
        SelectorType := gstUnknown;
        case Parts.Strings[0][1] of
          '0': SelectorType := gstPlainText;
          '1': SelectorType := gstDirectory;
          '3': SelectorType := gstError;
          '7': SelectorType := gstSearch;
          'i', '.': SelectorType := gstInfo;
          'I', 'g': SelectorType := gstImage;
        end;
        Selector := Trim(Parts.Strings[1]);
        Hostname := Trim(Parts.Strings[2]);
        if Hostname = '' then
          Hostname := FCurrentSelector.Hostname;
        Port := StrToInt(Parts.Strings[3]);
        Caption := RightStr(Parts.Strings[0], Length(Parts.Strings[0]) - 1);
        if SelectorType = gstUnknown then
          Caption := '[' + LeftStr(Parts.Strings[0], 1) + ']' + Caption;
      end
      else
        SelectorType := gstEmpty;
    end;
  Parts.Free;
  Lines.Free;

  // show all elements in list view
  HideElements;
  ListView1.Visible := True;
  ListView1.Clear;
  for i := 1 to items do
    with ListView1.Items.Add do
    begin
      Caption := FSelectorList[i].Caption;
      ImageIndex := -1;

      case FSelectorList[i].SelectorType of
        gstInfo: ImageIndex := -1;
        gstError: ImageIndex := 1;
        gstDirectory:
          if FSelectorList[i].Hostname = FCurrentSelector.Hostname then
            ImageIndex := 2
          else
            ImageIndex := 3;
        gstPlainText: ImageIndex := 4;
        gstImage: ImageIndex := 5;
        gstUnknown: ImageIndex := 0;
      end;
    end;
  if items > 0 then
    ListView1.Items[0].MakeVisible(False);
end;

{
    Load an Image
}
procedure TForm1.LoadImage;
begin
  HideElements;
  Image1.Visible := True;
  Image1.Picture.LoadFromStream(FMemoryStream);
end;

{
    Load Textfile
}
procedure TForm1.LoadText;
begin
  HideElements;
  Memo1.Visible := True;
  Memo1.Lines.LoadFromStream(FMemoryStream);
end;

{
    "Download" selector

    This function just presents a Save File dialog to store the downloaded
    bytes in the memory stream.
}
procedure TForm1.DownloadSelector;
begin
  SaveDialog1.FileName := ExtractFileName(FCurrentSelector.Selector);
  if SaveDialog1.Execute then
    FMemoryStream.SaveToFile(SaveDialog1.FileName);
end;

procedure TForm1.BitBtn2Click(Sender: TObject);
begin
  // go back in history :)
  Dec(FHistoryCount);
  LoadSelector(FSelectorHistory[FHistoryCount], uaViewSelector);
  BitBtn2.Enabled := FHistoryCount > 1;
end;

end.
