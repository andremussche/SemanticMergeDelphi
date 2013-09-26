namespace SemanticMergeOxygene;

interface

uses
  SemanticYaml,
  PasToYamlParser;

type
  ConsoleApp = class
  public
    class method Main(args: array of String);
  end;

implementation

uses 
  System.IO, 
  System.Text;

class method ConsoleApp.Main(args: array of String);
var
  i: Integer;
  bShell: Boolean;
  sFlagFile, sLine: String;
  sFileToParse, sOutputFile: String;
  parser: TPas2YamlParser;
  sData: String;
begin
  try
    bShell := False;
    sFileToParse := '';
    sOutputFile  := '';
    // there are two arguments to consider:
    // 1) "shell" saying you must run in "shell mode"
    //    - don't exit basically and wait for commands
    // 2) A "flag file" - write it when you're done just
    //    in case you need initialization (like starting
    //    up the Java VM
    for i := 0 to args.Length - 1 do
    begin
      if 'shell' = args[i].ToLower() then bShell := true
      else
      begin
        if String.IsNullOrEmpty(sFlagFile) then sFlagFile := args[i]
        //in case shell=false we assume we get filenames via params (for easier debugging)
        else if String.IsNullOrEmpty(sLine) then sLine :=args[i]
        else if String.IsNullOrEmpty(sOutputFile) then sOutputFile := args[i]
      end;
    end;

    //create stuff
    parser     := TPas2YamlParser.Create;
    // Write the "flagfile" when you're ready
    if sFlagFile <> '' then File.WriteAllText(sFlagFile, 'READY');
    //debug:
    File.AppendAllText('debug.log', 'READY'#13);

    // Loop until Semantic writes "end"
    if bShell then sLine := Console.ReadLine();
    //debug:
    File.AppendAllText('debug.log', 'Received line: ' + sLine +#13);
    while sLine <> 'end' do
    begin
      // read the file to parse first
      if sFileToParse = '' then sFileToParse := sLine;
      // then where to put the resulting tree
      if bShell and (sOutputFile = '') then sOutputFile := Console.ReadLine();
      //debug:
      File.AppendAllText('debug.log', 'Received outputfile: ' + sOutputFile +#13);

      // load the file
      sData := File.ReadAllText(sFileToParse);
      // Parse the "fileToParse"
      try
        parser.Run(sFileToParse, sData + #0);
      except
        parser.Yaml.parsingErrorsDetected := True;
      end;
       
      //debug:
      File.AppendAllText('debug.log', 'Parsed and writing to outputfile: ' + sOutputFile +#13);
      // Write the result to "outputFile"
      File.WriteAllText(sOutputFile, parser.Yaml.Generate(''));
      File.WriteAllText(Path.GetFileName(sFileToParse) + '.tree', parser.Yaml.Generate(''));
      //strYaml.Clear;
      // write OK when you're done or KO if it didn't work
      writeLn('OK');
      if not bShell then Break;
      sLine := Console.ReadLine();
      //debug:
      File.AppendAllText('debug.log', 'Received line: ' + sLine +#13);
      sFileToParse := '';
      sOutputFile  := '';
    end;
  except
    on E: Exception do
    begin
      //debug:
      File.AppendAllText('debug.log', 'Exception: ' + E.Message +#13);
      writeLn('KO');
      writeLn(E.Message);
    end;
  end;
end;

end.
