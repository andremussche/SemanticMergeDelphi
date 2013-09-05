program pas2yaml;

{$APPTYPE CONSOLE}

//tip: for testing, use these params (simulation without shell)
//  pas2yaml.exe test.flag ./test/src.pas ./test/result.pas
//or, with shell mode:
//  pas2yaml.exe shell test.flag
//and enter the following line in the commandline:
//  ./test/src.pas ./test/result.pas

uses
  Classes, SysUtils, IOUtils,
  PasToYamlParser;

var
  i: Integer;
  bShell: Boolean;
  sFlagFile, sLine: string;
  sFileToParse, sOutputFile: string;
  parser: TPas2YamlParser;
  strYaml: TStringBuilder;
  strmstring, strmstring2: TStringStream;
  sData: string;
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
    for i := 1 to ParamCount do
    begin
      if SameText('shell', ParamStr(i)) then
        bShell := True
      else
      begin
        if sFlagFile = '' then
          sFlagFile := ParamStr(i)
        //in case shell=false we assume we get filenames via params (for easier debugging)
        else if sLine = '' then
          sLine := ParamStr(i)
        else if sOutputFile = '' then
          sOutputFile := ParamStr(i)
      end;
    end;

    //create stuff
    parser     := TPas2YamlParser.Create;
    strmstring := TStringStream.Create;
    strYaml    := TStringBuilder.Create;
    try
      //attach output events
      parser.OnYamlOutput :=
        procedure(const aLine: string)
        begin
          strYaml.AppendLine(aLine);
        end;
      parser.OnReplaceOutput :=
        procedure(const aSearch, aReplace: string)
        var sold: string;
        begin
          sold := strYaml.ToString;
          sold := StringReplace(sold, aSearch, aReplace, [rfReplaceAll]);
          strYaml.Clear;
          strYaml.Append(sold);
        end;

      // Write the "flagfile" when you're ready
      if sFlagFile <> '' then
        TFile.WriteAllText(sFlagFile, 'READY');
      //debug:
      TFile.AppendAllText('debug.log', 'READY'#13);

      // Loop until Semantic writes "end"
      if bShell then
        Readln(sLine);
      //debug:
      TFile.AppendAllText('debug.log', 'Received line: ' + sLine +#13);
      while sLine <> 'end' do
      begin
        with TStringList.Create do
        begin
          Delimiter     := ' ';
          DelimitedText := sLine;
          if Count > 1 then
          begin
            sFileToParse := Strings[0];
            sOutputFile  := Strings[1];
          end;
          Free;
        end;

        // read the file to parse first
        if sFileToParse = '' then
          sFileToParse := sLine;
        // then where to put the resulting tree
        if bShell and (sOutputFile = '') then
          ReadLn(sOutputFile);
        //debug:
        TFile.AppendAllText('debug.log', 'Received outputfile: ' + sOutputFile +#13);

        // load the file
        strYaml.Clear;
        strmstring.LoadFromFile(sFileToParse);
        sData := strmstring.DataString;
        strmstring.Clear;
        // we convert the string to unicode string (utf16) because parser needs this
        strmstring2 := TStringStream.Create(sData, TEncoding.Unicode);
        try
          strmstring2.Position := 0;
          // Parse the "fileToParse"
          parser.Run(sFileToParse, strmstring2);
        finally
          strmstring2.Free;
        end;

        //debug:
        TFile.AppendAllText('debug.log', 'Parsed and writing to outputfile: ' + sOutputFile +#13);
        // Write the result to "outputFile"
        TFile.WriteAllText(sOutputFile, strYaml.ToString);
        strYaml.Clear;
        // write OK when you're done or KO if it didn't work
        WriteLn('OK');
        Flush(Output);  //important!

        if not bShell then Break;
        Readln(sLine);
        //debug:
        TFile.AppendAllText('debug.log', 'Received line: ' + sLine +#13);
        sFileToParse := '';
        sOutputFile  := '';
      end;
    finally
      strYaml.Free;
      strmstring.Free;
      parser.Free;
    end;

  except
    on E: Exception do
    begin
      //debug:
      TFile.AppendAllText('debug.log', 'Exception: ' + e.Message +#13);

      WriteLn('KO');
      Writeln(E.ClassName, ': ', E.Message);
      Flush(Output);  //important!
    end;
  end;
end.
