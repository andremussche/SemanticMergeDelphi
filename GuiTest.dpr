program GuiTest;

uses
  Forms,
  MainForm in 'MainForm.pas' {frmMain},
  TestFile in 'TestFile.pas',
  PasToYamlParser in 'PasToYamlParser.pas',
  SemanticYaml in 'SemanticYaml.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
