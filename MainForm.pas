unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfrmMain = class(TForm)
    mmoSource: TMemo;
    Button1: TButton;
    mmoResult: TMemo;
    mmoOutput: TMemo;
    procedure Button1Click(Sender: TObject);
  private
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses
  PasToYamlParser;

{$R *.dfm}

procedure TfrmMain.Button1Click(Sender: TObject);
var
  parser: TPas2YamlParser;
  strm: TMemoryStream;
begin
  mmoResult.Clear;
  mmoOutput.Clear;

  strm   := TMemoryStream.Create;
  strm.Position := 0;
  mmoSource.Lines.SaveToStream(strm, TEncoding.Unicode);

  parser := TPas2YamlParser.Create;
  parser.OnDebugOutput   :=
    procedure(const aLine: string)
    begin
      mmoResult.Lines.Add(aLine);
    end;
  parser.OnYamlOutput    :=
    procedure(const aLine: string)
    begin
      mmoOutput.Lines.Add(aLine);
    end;
  parser.OnReplaceOutput :=
    procedure(const aSearch, aReplace: string)
    begin
      mmoOutput.Lines.Text := StringReplace(mmoOutput.Lines.Text, aSearch, aReplace, [rfReplaceAll]);
    end;
  parser.Run('test', strm);
end;


end.


