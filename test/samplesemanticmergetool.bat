echo off

set semanticmergetool="c:\-Andre-\Tools\SemanticMerge\semanticmergetool.exe"

set contributorFiles= -s="%~dp0src.pas" -b="%~dp0base.pas" -d="%~dp0dest.pas"

set resultFile= -r="%~dp0semanticmergetoolresult.pas"

set difftoolexe=""mergetool.exe""
set difftoolparams= -s=""@sourcefile"" -sn=""@sourcesymbolic"" -d=""@destinationfile"" -dn=""@destinationsymbolic"" -t=""@filetype"" -i=""@comparationmethod"" -e=""@fileencoding""
set difftool= -edt="%difftoolexe%%difftoolparams%"

set mergetoolexe=""mergetool.exe""
set mergetoolparams= -b=""@basefile"" -bn=""@basesymbolic"" -s=""@sourcefile"" -sn=""@sourcesymbolic"" -d=""@destinationfile"" -dn=""@destinationsymbolic"" -r=""@output"" -t=""@filetype"" -i=""@comparationmethod"" -e=""@fileencoding""
set mergetool= -emt="%mergetoolexe%%mergetoolparams%"

set externalparser= -ep="..\pas2yaml.exe"

set mergeOptions=%contributorFiles%%resultFile% %externalparser% %difftool%%mergetool%

%semanticmergetool% %mergeOptions%


::  if you want to launch manually instead, you can try the following :-)

::  ..\..\..\semanticmergetool.exe -s=src.cs -b=base.cs -d=dst.cs -r=semanticmergetoolresult.cs -emt="..\..\..\mergetool.exe -b=""@basefile"" -bn=""@basesymbolic"" -s=""@sourcefile"" -sn=""@sourcesymbolic"" -d=""@destinationfile"" -dn=""@destinationsymbolic"" -r=""@output"" -t=""@filetype"" -i=""@comparationmethod"" -e=""@fileencoding""" -edt="..\..\..\mergetool.exe  -s=""@sourcefile"" -sn=""@sourcesymbolic"" -d=""@destinationfile"" -dn=""@destinationsymbolic"" -t=""@filetype"" -i=""@comparationmethod"" -e=""@fileencoding"""

pause