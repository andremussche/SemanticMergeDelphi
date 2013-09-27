{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License Version
1.1 (the "License"); you may not use this file except in compliance with the
License. You may obtain a copy of the License at
http://www.mozilla.org/NPL/NPL-1_1Final.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: mwPasLex.PAS, released August 17, 1999.

The Initial Developer of the Original Code is Martin Waldenburg
(Martin.Waldenburg@T-Online.de).
Portions created by Martin Waldenburg are Copyright (C) 1998, 1999 Martin
Waldenburg.
All Rights Reserved.

Contributor(s): James Jacobson _____________________________________.

Last Modified: mm/dd/yyyy
Current Version: 2.2

Notes: This program is a very fast Pascal tokenizer. I'd like to invite the
Delphi community to develop it further and to create a fully featured Object
Pascal parser.

Modification history:

Daniel Rolf between 20010723 and 20020116

Made ready for Delphi 6

platform
deprecated
varargs
local

Known Issues:
-----------------------------------------------------------------------------}

unit CastaliaPasLex;
{$IFNDEF OXYGENE}
  {$I CastaliaParserDefines.inc}
{$ENDIF}

interface

uses
  {$IFNDEF OXYGENE}
  SysUtils, Classes, Controls, 
  {$ELSE}
  System.Collections.Generic,
  {$ENDIF} 
  CastaliaPasLexTypes;

var
  Identifiers: array[#0..#255] of ByteBool;
  mHashTable: array[#0..#255] of Integer;

type
  TmwBasePasLex = class;
  TDirectiveEvent = procedure(Sender: TmwBasePasLex) of object;

  PDefineRec = ^TDefineRec;
  TDefineRec = record
    Defined: Boolean;
    StartCount: Integer;
    Next: PDefineRec;
  end;

  {$IFDEF OXYGENE}
  TTptTokenKindDelegate = delegate: TptTokenKind; 
  {$ENDIF}  

  TmwBasePasLex = class(TObject)
  private
    fDirectiveParamOrigin: PChar;
    {$IFDEF OXYGENE}
    fProcTable: array[#0..#255] of Delegate;
    fIdentFuncTable: array[0..192] of TTptTokenKindDelegate; 
    {$ELSE}
    fProcTable: array[#0..#255] of procedure of object;
    fIdentFuncTable: array[0..191] of function: TptTokenKind of object;
    {$ENDIF}  
    fDefines: TStrings;
    RunAhead: Integer;
    TempRun: Integer;   
    FOnMessage: TMessageEvent;
    fOnCompDirect: TDirectiveEvent;
    fOnElseDirect: TDirectiveEvent;
    fOnEndIfDirect: TDirectiveEvent;
    fOnIfDefDirect: TDirectiveEvent;
    fOnIfNDefDirect: TDirectiveEvent;
    fOnResourceDirect: TDirectiveEvent;
    fOnIncludeDirect: TDirectiveEvent;
    fOnDefineDirect: TDirectiveEvent;
    fOnIfOptDirect: TDirectiveEvent;
    fOnIfDirect: TDirectiveEvent;
    fOnIfEndDirect: TDirectiveEvent;
    fOnElseIfDirect: TDirectiveEvent;
	  fOnUnDefDirect: TDirectiveEvent;  
  	fAsmCode : Boolean;		// DR 2002-01-14  
    FDefineStack: Integer;
    FTopDefineRec: PDefineRec;
    FUseDefines: Boolean;

	  function KeyHash: Integer;
    function KeyComp(const aKey: String): Boolean;
    function Func9: TptTokenKind;
    function Func15: TptTokenKind;
    function Func19: TptTokenKind;
    function Func20: TptTokenKind;
	  function Func21: TptTokenKind;
    function Func23: TptTokenKind;
    function Func25: TptTokenKind;
    function Func27: TptTokenKind;
    function Func28: TptTokenKind;
    function Func29: TptTokenKind;
    function Func30: TptTokenKind;
    function Func32: TptTokenKind;
    function Func33: TptTokenKind;
    function Func35: TptTokenKind;
    function Func36: TptTokenKind;
    function Func37: TptTokenKind;
    function Func38: TptTokenKind;
    function Func39: TptTokenKind;
    function Func40: TptTokenKind;
    function Func41: TptTokenKind;
    {$IFDEF D8_NEWER} //JThurman 2004-03-2003
    function Func42: TptTokenKind;
    {$ENDIF}
    function Func43: TptTokenKind;
    function Func44: TptTokenKind;
    function Func45: TptTokenKind;
    function Func46: TptTokenKind;
    function Func47: TptTokenKind;
    function Func49: TptTokenKind;
    function Func52: TptTokenKind;
    function Func54: TptTokenKind;
	  function Func55: TptTokenKind;
    function Func56: TptTokenKind;
    function Func57: TptTokenKind;
    function Func58: TptTokenKind;
    function Func59: TptTokenKind;
    function Func60: TptTokenKind;
    function Func61: TptTokenKind;
    function Func62: TptTokenKind;
    function Func63: TptTokenKind;
    function Func64: TptTokenKind;
	  function Func65: TptTokenKind;
    function Func66: TptTokenKind;
    function Func69: TptTokenKind;
    function Func71: TptTokenKind;
    {$IFDEF D8_NEWER} //JThurman 2004-03-2003
    function Func72: TptTokenKind;
    {$ENDIF}
    function Func73: TptTokenKind;
    function Func75: TptTokenKind;
    function Func76: TptTokenKind;
    function Func77: TptTokenKind;
    function Func78: TptTokenKind;
    function Func79: TptTokenKind;
    function Func81: TptTokenKind;
    function Func84: TptTokenKind;
	  function Func85: TptTokenKind;
	  function Func86: TptTokenKind;
    function Func87: TptTokenKind;
    function Func88: TptTokenKind;
    {$IFDEF D8_NEWER}
    function Func89: TptTokenKind; //JThurman 2004-03-03
    {$ENDIF}
    function Func91: TptTokenKind;
    function Func92: TptTokenKind;
    function Func94: TptTokenKind;
    function Func95: TptTokenKind;
    function Func96: TptTokenKind;
    function Func97: TptTokenKind;
    function Func98: TptTokenKind;
    function Func99: TptTokenKind;
    function Func100: TptTokenKind;
    function Func101: TptTokenKind;
    function Func102: TptTokenKind;
    function Func103: TptTokenKind;
    function Func104: TptTokenKind;
    function Func105: TptTokenKind;
    function Func106: TptTokenKind;
    function Func107: TptTokenKind;
    function Func108: TptTokenKind;
    function Func112: TptTokenKind;
    function Func117: TptTokenKind;
	  function Func123: TptTokenKind;
    function Func126: TptTokenKind;
    function Func127: TptTokenKind;
    function Func128: TptTokenKind;
    function Func129: TptTokenKind;
    function Func130: TptTokenKind;
    function Func132: TptTokenKind;
    function Func133: TptTokenKind;
    function Func136: TptTokenKind;
    function Func141: TptTokenKind;
    function Func143: TptTokenKind;
    function Func166: TptTokenKind;
    function Func167: TptTokenKind;
    function Func168: TptTokenKind;
    function Func191: TptTokenKind;
    function AltFunc: TptTokenKind;
    procedure InitIdent;
    function GetPosXY: TTokenPoint; // !! changed to TokenPoint //jdj 7/18/1999
    function IdentKind: TptTokenKind;
    procedure SetRunPos(Value: Integer);
    procedure MakeMethodTables;
    procedure AddressOpProc;
    {$IFDEF D8_NEWER} //JThurman 2004-04-06
    procedure AmpersandOpProc;
    {$ENDIF}
    procedure AsciiCharProc;
    procedure AnsiProc;
    procedure BorProc;
    procedure BraceCloseProc;
    procedure BraceOpenProc;
    procedure ColonProc;
    procedure CommaProc;
    procedure CRProc;
    procedure EqualProc;
    procedure GreaterProc;
    procedure IdentProc;
    procedure IntegerProc;
	  procedure LFProc;
    procedure LowerProc;
    procedure MinusProc;
    procedure NullProc;
    procedure NumberProc;
    procedure PlusProc;
    procedure PointerSymbolProc;
    procedure PointProc;
    procedure RoundCloseProc;
    procedure RoundOpenProc;
    procedure SemiColonProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure SquareCloseProc;
    procedure SquareOpenProc;
    procedure StarProc;
	  procedure StringProc;
	  procedure StringDQProc;
    procedure SymbolProc;
    procedure UnknownProc;
    function GetToken: String;
    function GetTokenLen: Integer;
    function GetCommentState: Pointer;
    function GetCompilerDirective: String;
    {$IFNDEF OXYGENE}
    procedure SetCommentState(const Value: Pointer);
    {$ENDIF}
    procedure InitLine;
    function GetDirectiveKind: TptTokenKind;
    function GetDirectiveParam: String;
    function GetStringContent: String;
    function GetIsJunk: Boolean;
    function GetIsSpace: Boolean;
    function GetIsOrdIdent: Boolean;
    function GetIsRealType: Boolean;
    function GetIsStringType: Boolean;
    function GetIsVarantType: Boolean;
    function GetIsAddOperator: Boolean;
    function GetIsMulOperator: Boolean;
    function GetIsRelativeOperator: Boolean;
    function GetIsCompilerDirective: Boolean;
    function GetIsOrdinalType: Boolean;
    function GetGenID: TptTokenKind;procedure SetOnElseIfDirect(const Value: TDirectiveEvent);
    function IsDefined(const ADefine: String): Boolean;
    procedure EnterDefineBlock(ADefined: Boolean); {$IFDEF OXYGENE}unsafe;{$ENDIF}
    procedure ExitDefineBlock;

    procedure DoProcTable(AChar: Char);
    function IsIdentifiers(AChar: Char): Boolean;
    function HashValue(AChar: Char): Integer;
    function GetLine: String;
  {$IFDEF OXYGENE}assembly or {$ENDIF}protected
    procedure SetLine(const Value: String); virtual;
    procedure SetOrigin(NewValue: PChar); virtual;
    procedure SetOnCompDirect(const Value: TDirectiveEvent); virtual;
    procedure SetOnDefineDirect(const Value: TDirectiveEvent); virtual;
    procedure SetOnElseDirect(const Value: TDirectiveEvent); virtual;
    procedure SetOnEndIfDirect(const Value: TDirectiveEvent); virtual;
    procedure SetOnIfDefDirect(const Value: TDirectiveEvent); virtual;
    procedure SetOnIfNDefDirect(const Value: TDirectiveEvent); virtual;
    procedure SetOnIfOptDirect(const Value: TDirectiveEvent); virtual;
    procedure SetOnIncludeDirect(const Value: TDirectiveEvent); virtual;
    procedure SetOnResourceDirect(const Value: TDirectiveEvent); virtual;
    procedure SetOnUnDefDirect(const Value: TDirectiveEvent); virtual;
    procedure SetOnIfDirect(const Value: TDirectiveEvent); virtual;
    procedure SetOnIfEndDirect(const Value: TDirectiveEvent); virtual;
    procedure CloneDefinesFrom(ALexer: TmwBasePasLex); {$IFDEF OXYGENE}unsafe;{$ENDIF}
    fLineNumber: Integer;
    fLinePos: Integer;
    fCommentState: TCommentState;
    fExID: TptTokenKind;
    fOrigin: PChar;
    Run: Integer;
    fTokenPos: Integer;
    FTokenID: TptTokenKind;
  public
    constructor Create;
    {$IFDEF OXYGENE}
    finalizer; 
    {$ELSE}
    destructor Destroy; override; 
    {$ENDIF}
    function CharAhead: Char;
    procedure Next;
    procedure NextID(ID: TptTokenKind);
    procedure NextNoJunk;
    procedure NextNoSpace;
    procedure Init;
    procedure InitFrom(ALexer: TmwBasePasLex);
    function FirstInLine: Boolean;

    procedure AddDefine(const ADefine: String);
    procedure RemoveDefine(const ADefine: String);
    procedure ClearDefines;
    procedure InitDefines;
    {$IFDEF OXYGENE}
    property CommentState: TCommentState read fCommentState write fCommentState;
    {$ELSE}
    property CommentState: Pointer read GetCommentState write SetCommentState;
    {$ENDIF}
    property CompilerDirective: String read GetCompilerDirective;
    property DirectiveParam: String read GetDirectiveParam;
  	property IsJunk: Boolean read GetIsJunk;
    property IsSpace: Boolean read GetIsSpace;
    property Line: String read GetLine write SetLine;
    //Note: setting the following two properties does not GO to that line, it just sets the internal counters
    property LineNumber: Integer read fLineNumber write fLineNumber;
    property LinePos: Integer read fLinePos write fLinePos;
    property Origin: PChar read fOrigin write SetOrigin;
    property PosXY: TTokenPoint read GetPosXY; // !! changed to TokenPoint //jdj 7/18/1999
    property RunPos: Integer read Run write SetRunPos;
    property Token: String read GetToken;
    property TokenLen: Integer read GetTokenLen;
    property TokenPos: Integer read fTokenPos;
    property TokenID: TptTokenKind read FTokenID;
    property ExID: TptTokenKind read fExID;
    property GenID: TptTokenKind read GetGenID;
    property StringContent: String read GetStringContent;
    property IsOrdIdent: Boolean read GetIsOrdIdent;
    property IsOrdinalType: Boolean read GetIsOrdinalType;
    property IsRealType: Boolean read GetIsRealType;
    property IsStringType: Boolean read GetIsStringType;
    property IsVariantType: Boolean read GetIsVarantType;
    property IsRelativeOperator: Boolean read GetIsRelativeOperator;
    property IsAddOperator: Boolean read GetIsAddOperator;
    property IsMulOperator: Boolean read GetIsMulOperator;
    property IsCompilerDirective: Boolean read GetIsCompilerDirective;
    property OnMessage: TMessageEvent read FOnMessage write FOnMessage;
    property OnCompDirect: TDirectiveEvent read fOnCompDirect write SetOnCompDirect;
    property OnDefineDirect: TDirectiveEvent read fOnDefineDirect write SetOnDefineDirect;
    property OnElseDirect: TDirectiveEvent read fOnElseDirect write SetOnElseDirect;
    property OnEndIfDirect: TDirectiveEvent read fOnEndIfDirect write SetOnEndIfDirect;
    property OnIfDefDirect: TDirectiveEvent read fOnIfDefDirect write SetOnIfDefDirect;
    property OnIfNDefDirect: TDirectiveEvent read fOnIfNDefDirect write SetOnIfNDefDirect;
    property OnIfOptDirect: TDirectiveEvent read fOnIfOptDirect write SetOnIfOptDirect;
    property OnIncludeDirect: TDirectiveEvent read fOnIncludeDirect write SetOnIncludeDirect;
    property OnIfDirect: TDirectiveEvent read fOnIfDirect write SetOnIfDirect;
    property OnIfEndDirect: TDirectiveEvent read fOnIfEndDirect write 
    SetOnIfEndDirect;
    property OnElseIfDirect: TDirectiveEvent read fOnElseIfDirect write 
    SetOnElseIfDirect;
	  property OnResourceDirect: TDirectiveEvent read fOnResourceDirect write SetOnResourceDirect;
	  property OnUnDefDirect: TDirectiveEvent read fOnUnDefDirect write SetOnUnDefDirect;

	  property AsmCode : Boolean read fAsmCode write fAsmCode; // DR 2002-01-14
    property DirectiveParamOrigin: PChar read fDirectiveParamOrigin;

    property UseDefines: Boolean read FUseDefines write FUseDefines;
    {$IFDEF OXYGENE}
    class constructor; 
    {$ENDIF}
    
  end;

  TmwPasLex = class(TmwBasePasLex)
  private
    fAheadLex: TmwBasePasLex;
    function GetAheadExID: TptTokenKind;
    function GetAheadGenID: TptTokenKind;
    function GetAheadToken: String;
    function GetAheadTokenID: TptTokenKind;
    function GetStatus: TmwPasLexStatus;
    procedure SetStatus(const Value: TmwPasLexStatus);
  {$IFDEF OXYGENE}assembly or {$ENDIF}protected 
    procedure SetLine(const Value: String); override;
    procedure SetOrigin(NewValue: PChar); override;
    procedure SetOnCompDirect(const Value: TDirectiveEvent); override;
    procedure SetOnDefineDirect(const Value: TDirectiveEvent); override;
    procedure SetOnElseDirect(const Value: TDirectiveEvent); override;
    procedure SetOnEndIfDirect(const Value: TDirectiveEvent); override;
    procedure SetOnIfDefDirect(const Value: TDirectiveEvent); override;
    procedure SetOnIfNDefDirect(const Value: TDirectiveEvent); override;
    procedure SetOnIfOptDirect(const Value: TDirectiveEvent); override;
    procedure SetOnIncludeDirect(const Value: TDirectiveEvent); override;
    procedure SetOnResourceDirect(const Value: TDirectiveEvent); override;
    procedure SetOnUnDefDirect(const Value: TDirectiveEvent); override;
  public
    constructor Create;
    {$IFDEF OXYGENE}
    finalizer;
    {$ELSE}
    destructor Destroy; override; 
    {$ENDIF}  
    procedure InitAhead;
    procedure AheadNext;
    property AheadLex: TmwBasePasLex read fAheadLex;
    property AheadToken: String read GetAheadToken;
    property AheadTokenID: TptTokenKind read GetAheadTokenID;
    property AheadExID: TptTokenKind read GetAheadExID;
    property AheadGenID: TptTokenKind read GetAheadGenID;
    property Status: TmwPasLexStatus read GetStatus write SetStatus;
  end;

implementation

{$IFNDEF OXYGENE}
uses Windows;
{$ENDIF}

{$IFDEF OXYGENE}
class constructor TmwBasePasLex;
begin
  MakeIdentTable;
end;
{$ENDIF}

procedure MakeIdentTable;
var
  I, J: Char;
begin
  for I := #0 to #255 do
  begin
    case I of
      '_', '0'..'9', 'a'..'z', 'A'..'Z': Identifiers[I] := True;
    else Identifiers[I] := False;
    end;
    {$IFDEF OXYGENE}
    J := Char.ToUpper(I);
    {$ELSE}
    J := UpperCase(I)[1]; 
    {$ENDIF}
    case I of
	  'a'..'z', 'A'..'Z', '_': mHashTable[I] := ord(J) - 64;
    else mHashTable[Char(I)] := 0;
    end;
  end;
end;

function TmwBasePasLex.CharAhead: Char;
begin
  RunAhead := Run;
//  while fOrigin[RunAhead] in [#1..#32] do
  while (fOrigin[RunAhead] > #0) and (fOrigin[RunAhead] < #33) do

    inc(RunAhead);
  Result := fOrigin[RunAhead];
end;

procedure TmwBasePasLex.ClearDefines;
var
  Frame: PDefineRec;
begin
  while FTopDefineRec <> nil do
  begin
    Frame := FTopDefineRec;
    FTopDefineRec := Frame^.Next;
    {$IFDEF OXYGENE}
    disposeAndNil(Frame);
    {$ELSE}
    Dispose(Frame);
    {$ENDIF}  
  end;
  fDefines.Clear;
  FDefineStack := 0;
end;

procedure TmwBasePasLex.CloneDefinesFrom(ALexer: TmwBasePasLex);
var
  Frame, LastFrame, SourceFrame: PDefineRec;
begin
  ClearDefines;
  {$IFDEF OXYGENE}
  fDefines := new TStrings(ALexer.fDefines);
  {$ELSE}
  fDefines.Assign(ALexer.fDefines); 
  {$ENDIF}
  
  FDefineStack := ALexer.FDefineStack;

  Frame := nil;
  SourceFrame := ALexer.FTopDefineRec;
  while SourceFrame <> nil do
  begin
    {$IFDEF OXYGENE}
    var fr := new TDefineRec();
    Frame := @fr;
    {$ELSE}
    New(Frame);
    {$ENDIF}
    if FTopDefineRec = nil then
      FTopDefineRec := Frame
    else
      LastFrame^.Next := Frame;
    Frame^.Defined := SourceFrame^.Defined;
    Frame^.StartCount := SourceFrame^.StartCount;
    LastFrame := Frame;

    SourceFrame := SourceFrame^.Next;
  end;
  if Frame <> nil then
    Frame^.Next := nil;

//  New(StackFrame);
//  StackFrame^.Next := FTopDefineRec;
//  StackFrame^.Defined := ADefined;
//  StackFrame^.StartCount := FDefineStack;
//  FTopDefineRec := StackFrame;
//  if not ADefined then
//    Inc(FDefineStack);

end;

function TmwBasePasLex.GetPosXY: TTokenPoint;
begin //jdj 7/18/1999
  // !! changed setting code
  Result.X:= fTokenPos - fLinePos;
  Result.Y:= fLineNumber;
end;

procedure TmwBasePasLex.InitIdent;
var
  I: Integer;
begin
  for I := 0 to 191 do
    {$IFDEF OXYGENE}
    case I of
      9: fIdentFuncTable[I] := @Func9;
      15: fIdentFuncTable[I] := @Func15;
	    19: fIdentFuncTable[I] := @Func19;
      20: fIdentFuncTable[I] := @Func20;
      21: fIdentFuncTable[I] := @Func21;
	    23: fIdentFuncTable[I] := @Func23;
      25: fIdentFuncTable[I] := @Func25;
      27: fIdentFuncTable[I] := @Func27;
      28: fIdentFuncTable[I] := @Func28;
      29: fIdentFuncTable[I] := @Func29;
      30: fIdentFuncTable[I] := @Func30;
      32: fIdentFuncTable[I] := @Func32;
      33: fIdentFuncTable[I] := @Func33;
      35: fIdentFuncTable[I] := @Func35;
      36: fIdentFuncTable[I] := @Func36;
      37: fIdentFuncTable[I] := @Func37;
      38: fIdentFuncTable[I] := @Func38;
	    39: fIdentFuncTable[I] := @Func39;
      40: fIdentFuncTable[I] := @Func40;
	    41: fIdentFuncTable[I] := @Func41;
      42: fIdentFuncTable[I] := @Func42;
	    43: fIdentFuncTable[I] := @Func43;
      44: fIdentFuncTable[I] := @Func44;
      45: fIdentFuncTable[I] := @Func45;
      46: fIdentFuncTable[I] := @Func46;
      47: fIdentFuncTable[I] := @Func47;
      49: fIdentFuncTable[I] := @Func49;
      52: fIdentFuncTable[I] := @Func52;
      54: fIdentFuncTable[I] := @Func54;
      55: fIdentFuncTable[I] := @Func55;
      56: fIdentFuncTable[I] := @Func56;
      57: fIdentFuncTable[I] := @Func57;
      58: fIdentFuncTable[I] := @Func58;
	    59: fIdentFuncTable[I] := @Func59;
      60: fIdentFuncTable[I] := @Func60;
      61: fIdentFuncTable[I] := @Func61;
	    62: fIdentFuncTable[I] := @Func62;
      63: fIdentFuncTable[I] := @Func63;
	    64: fIdentFuncTable[I] := @Func64;
      65: fIdentFuncTable[I] := @Func65;
      66: fIdentFuncTable[I] := @Func66;
      69: fIdentFuncTable[I] := @Func69;
      71: fIdentFuncTable[I] := @Func71;
      72: fIdentFuncTable[I] := @Func72;
      73: fIdentFuncTable[I] := @Func73;
      75: fIdentFuncTable[I] := @Func75;
      76: fIdentFuncTable[I] := @Func76;
      77: fIdentFuncTable[I] := @Func77;
      78: fIdentFuncTable[I] := @Func78;
      79: fIdentFuncTable[I] := @Func79;
      81: fIdentFuncTable[I] := @Func81;
      84: fIdentFuncTable[I] := @Func84;
	    85: fIdentFuncTable[I] := @Func85;
	    86: fIdentFuncTable[I] := @Func86;
      87: fIdentFuncTable[I] := @Func87;
      88: fIdentFuncTable[I] := @Func88;  
      89: fIdentFuncTable[I] := @Func89;
      91: fIdentFuncTable[I] := @Func91;
      92: fIdentFuncTable[I] := @Func92;
      94: fIdentFuncTable[I] := @Func94;
      95: fIdentFuncTable[I] := @Func95;
      96: fIdentFuncTable[I] := @Func96;
      97: fIdentFuncTable[I] := @Func97;
      98: fIdentFuncTable[I] := @Func98;
      99: fIdentFuncTable[I] := @Func99;
      100: fIdentFuncTable[I] := @Func100;
	    101: fIdentFuncTable[I] := @Func101;
      102: fIdentFuncTable[I] := @Func102;
      103: fIdentFuncTable[I] := @Func103;
      104: fIdentFuncTable[I] := @Func104;
      105: fIdentFuncTable[I] := @Func105;
	    106: fIdentFuncTable[I] := @Func106;
      107: fIdentFuncTable[I] := @Func107;
      108: fIdentFuncTable[I] := @Func108;
      112: fIdentFuncTable[I] := @Func112;
      117: fIdentFuncTable[I] := @Func117;
      123: fIdentFuncTable[I] := @Func123;
      126: fIdentFuncTable[I] := @Func126;
      127: fIdentFuncTable[I] := @Func127;
      128: fIdentFuncTable[I] := @Func128;
	    129: fIdentFuncTable[I] := @Func129;
      130: fIdentFuncTable[I] := @Func130;
      132: fIdentFuncTable[I] := @Func132;
      133: fIdentFuncTable[I] := @Func133;
	    136: fIdentFuncTable[I] := @Func136;
      141: fIdentFuncTable[I] := @Func141;
      143: fIdentFuncTable[I] := @Func143;
      166: fIdentFuncTable[I] := @Func166;
      167: fIdentFuncTable[I] := @Func167;
      168: fIdentFuncTable[I] := @Func168;
      191: fIdentFuncTable[I] := @Func191;
    else fIdentFuncTable[I] := @AltFunc;
    end;
    {$ELSE}
    case I of
      {$IFDEF D8_NEWER}
      9: fIdentFuncTable[I] := Func9;
      {$ENDIF}
      15: fIdentFuncTable[I] := Func15;
	  19: fIdentFuncTable[I] := Func19;
      20: fIdentFuncTable[I] := Func20;
      21: fIdentFuncTable[I] := Func21;
	  23: fIdentFuncTable[I] := Func23;
      25: fIdentFuncTable[I] := Func25;
      27: fIdentFuncTable[I] := Func27;
      28: fIdentFuncTable[I] := Func28;
      29: fIdentFuncTable[I] := Func29;
      30: fIdentFuncTable[I] := Func30;
      32: fIdentFuncTable[I] := Func32;
      33: fIdentFuncTable[I] := Func33;
      35: fIdentFuncTable[I] := Func35;
      36: fIdentFuncTable[I] := Func36;
      37: fIdentFuncTable[I] := Func37;
      38: fIdentFuncTable[I] := Func38;
	  39: fIdentFuncTable[I] := Func39;
      40: fIdentFuncTable[I] := Func40;
	  41: fIdentFuncTable[I] := Func41;
    {$IFDEF D8_NEWER} //JThurman 2004-03-2003
    42: fIdentFuncTable[I] := Func42;
    {$ENDIF}
	  43: fIdentFuncTable[I] := Func43;
      44: fIdentFuncTable[I] := Func44;
      45: fIdentFuncTable[I] := Func45;
      46: fIdentFuncTable[I] := Func46;
      47: fIdentFuncTable[I] := Func47;
      49: fIdentFuncTable[I] := Func49;
      52: fIdentFuncTable[I] := Func52;
      54: fIdentFuncTable[I] := Func54;
      55: fIdentFuncTable[I] := Func55;
      56: fIdentFuncTable[I] := Func56;
      57: fIdentFuncTable[I] := Func57;
      58: fIdentFuncTable[I] := Func58;
	  59: fIdentFuncTable[I] := Func59;
      60: fIdentFuncTable[I] := Func60;
      61: fIdentFuncTable[I] := Func61;
	  62: fIdentFuncTable[I] := Func62;
      63: fIdentFuncTable[I] := Func63;
	  64: fIdentFuncTable[I] := Func64;
      65: fIdentFuncTable[I] := Func65;
      66: fIdentFuncTable[I] := Func66;
      69: fIdentFuncTable[I] := Func69;
      71: fIdentFuncTable[I] := Func71;
      {$IFDEF D8_NEWER} //JThurman 2004-03-2003
      72: fIdentFuncTable[I] := Func72;
      {$ENDIF}
      73: fIdentFuncTable[I] := Func73;
      75: fIdentFuncTable[I] := Func75;
      76: fIdentFuncTable[I] := Func76;
      77: fIdentFuncTable[I] := Func77;
      78: fIdentFuncTable[I] := Func78;
      79: fIdentFuncTable[I] := Func79;
      81: fIdentFuncTable[I] := Func81;
      84: fIdentFuncTable[I] := Func84;
	  85: fIdentFuncTable[I] := Func85;
	  86: fIdentFuncTable[I] := Func86;
      87: fIdentFuncTable[I] := Func87;
      88: fIdentFuncTable[I] := Func88;
      {$IFDEF D8_NEWER} //JThurman 2004-03-03
      89: fIdentFuncTable[I] := Func89;
      {$ENDIF}
      91: fIdentFuncTable[I] := Func91;
      92: fIdentFuncTable[I] := Func92;
      94: fIdentFuncTable[I] := Func94;
      95: fIdentFuncTable[I] := Func95;
      96: fIdentFuncTable[I] := Func96;
      97: fIdentFuncTable[I] := Func97;
      98: fIdentFuncTable[I] := Func98;
      99: fIdentFuncTable[I] := Func99;
      100: fIdentFuncTable[I] := Func100;
	  101: fIdentFuncTable[I] := Func101;
      102: fIdentFuncTable[I] := Func102;
      103: fIdentFuncTable[I] := Func103;
      104: fIdentFuncTable[I] := Func104;
      105: fIdentFuncTable[I] := Func105;
	  106: fIdentFuncTable[I] := Func106;
      107: fIdentFuncTable[I] := Func107;
      108: fIdentFuncTable[I] := Func108;
      112: fIdentFuncTable[I] := Func112;
      117: fIdentFuncTable[I] := Func117;
      123: fIdentFuncTable[I] := Func123;
      126: fIdentFuncTable[I] := Func126;
      127: fIdentFuncTable[I] := Func127;
      128: fIdentFuncTable[I] := Func128;
	  129: fIdentFuncTable[I] := Func129;
      130: fIdentFuncTable[I] := Func130;
      132: fIdentFuncTable[I] := Func132;
      133: fIdentFuncTable[I] := Func133;
	  136: fIdentFuncTable[I] := Func136;
      141: fIdentFuncTable[I] := Func141;
      143: fIdentFuncTable[I] := Func143;
      166: fIdentFuncTable[I] := Func166;
      167: fIdentFuncTable[I] := Func167;
      168: fIdentFuncTable[I] := Func168;
      191: fIdentFuncTable[I] := Func191;
    else fIdentFuncTable[I] := AltFunc;
    end;
    {$ENDIF}
end;

function TmwBasePasLex.KeyHash: Integer;
begin
  Result := 0;
  while IsIdentifiers(fOrigin[Run]) do
  begin
    inc(Result, HashValue(fOrigin[Run]));
    //inc(Result, mHashTable[fOrigin[Run]]);
    inc(Run);
  end;
end; { KeyHash }

function TmwBasePasLex.KeyComp(const aKey: String): Boolean;
var
  {$IFNDEF OXYGENE}
  I: Integer;
  {$ENDIF}
  Temp: PChar;
begin
  if length(aKey) = TokenLen then
  begin
    Temp := fOrigin + fTokenPos;
    Result := True;
    {$IFDEF OXYGENE}
    for i: Int32 := 0 to TokenLen - 1 do
    begin
      if mHashTable[Temp.CurChar] <> mHashTable[aKey[i]] then
    {$ELSE}
    for I := 1 to TokenLen do 
    begin
      if mHashTable[Temp^] <> mHashTable[aKey[i]] then
    {$ENDIF} 
      begin
        Result := False;
        break;
      end;
      {$IFDEF OXYGENE}
      Temp.Position := Temp.Position + 1;
      {$ELSE}
      inc(Temp);
      {$ENDIF}
    end;
  end
  else Result := False;
end; { KeyComp }

function TmwBasePasLex.Func9: TptTokenKind;
begin
  result := TptTokenKind.ptIdentifier;
  if KeyComp('Add') then   
    fExID := TptTokenKind.ptAdd; 
end;

function TmwBasePasLex.Func15: TptTokenKind;
begin
  result := TptTokenKind.ptIdentifier;
  if KeyComp('If') then result := TptTokenKind.ptIf;
end;

function TmwBasePasLex.Func19: TptTokenKind;
begin
  result := TptTokenKind.ptIdentifier;
  if KeyComp('Do') then result := TptTokenKind.ptDo else
    if KeyComp('And') then result := TptTokenKind.ptAnd;
end;

function TmwBasePasLex.Func20: TptTokenKind;
begin
  result := TptTokenKind.ptIdentifier;
  if KeyComp('As') then result := TptTokenKind.ptAs;
end;

function TmwBasePasLex.Func21: TptTokenKind;
begin
  Result := TptTokenKind.ptIdentifier;
  if KeyComp('Of') then Result := TptTokenKind.ptOf else
    if KeyComp('At') then fExID := TptTokenKind.ptAt;
end;

function TmwBasePasLex.Func23: TptTokenKind;
begin
  Result := TptTokenKind.ptIdentifier;
  if KeyComp('End') then Result := TptTokenKind.ptEnd else
    if KeyComp('In') then Result := TptTokenKind.ptIn;
end;

function TmwBasePasLex.Func25: TptTokenKind;
begin 
  Result := TptTokenKind.ptIdentifier;
  if KeyComp('Far') then fExID := TptTokenKind.ptFar; 
end;

function TmwBasePasLex.Func27: TptTokenKind;
begin
  Result := TptTokenKind.ptIdentifier;
  if KeyComp('Cdecl') then fExID := TptTokenKind.ptCdecl;
end;

function TmwBasePasLex.Func28: TptTokenKind;
begin
  Result := TptTokenKind.ptIdentifier;
  if KeyComp('Read') then fExID := TptTokenKind.ptRead else
    if KeyComp('Case') then Result := TptTokenKind.ptCase else
      if KeyComp('Is') then Result := TptTokenKind.ptIs;
end;

function TmwBasePasLex.Func29: TptTokenKind;
begin
  Result := TptTokenKind.ptIdentifier;
  if KeyComp('On') then fExID := TptTokenKind.ptOn;
end;

function TmwBasePasLex.Func30: TptTokenKind;
begin
  Result := TptTokenKind.ptIdentifier;
  if KeyComp('Char') then fExID := TptTokenKind.ptChar; 
end;

function TmwBasePasLex.Func32: TptTokenKind;
begin
  Result := TptTokenKind.ptIdentifier;
  if KeyComp('File') then Result := TptTokenKind.ptFile else
    if KeyComp('Label') then Result := TptTokenKind.ptLabel else
      if KeyComp('Mod') then Result := TptTokenKind.ptMod;
end;

function TmwBasePasLex.Func33: TptTokenKind;
begin
  Result := TptTokenKind.ptIdentifier;
  if KeyComp('Or') then Result := TptTokenKind.ptOr else
    if KeyComp('Name') then fExID := TptTokenKind.ptName else
      if KeyComp('Asm') then Result := TptTokenKind.ptAsm;
end;

function TmwBasePasLex.Func35: TptTokenKind;
begin
  Result := TptTokenKind.ptIdentifier;
  if KeyComp('Nil') then Result := TptTokenKind.ptNil else
    if KeyComp('To') then Result := TptTokenKind.ptTo else
      if KeyComp('Div') then Result := TptTokenKind.ptDiv;
end;

function TmwBasePasLex.Func36: TptTokenKind;
begin
  Result := TptTokenKind.ptIdentifier;
  if KeyComp('Real') then fExID := TptTokenKind.ptReal else
    if KeyComp('Real48') then fExID := TptTokenKind.ptReal48;
end;

function TmwBasePasLex.Func37: TptTokenKind;
begin
  Result := TptTokenKind.ptIdentifier;
  if KeyComp('Begin') then Result := TptTokenKind.ptBegin else
    if KeyComp('Break') then fExID := TptTokenKind.ptBreak;
end;

function TmwBasePasLex.Func38: TptTokenKind;
begin
  Result := TptTokenKind.ptIdentifier;
  if KeyComp('Near') then fExID := TptTokenKind.ptNear;
end;

function TmwBasePasLex.Func39: TptTokenKind;
begin
  Result := TptTokenKind.ptIdentifier;
  if KeyComp('For') then Result := TptTokenKind.ptFor else
    if KeyComp('Shl') then Result := TptTokenKind.ptShl;
end;

function TmwBasePasLex.Func40: TptTokenKind;
begin
  Result := TptTokenKind.ptIdentifier;
  if KeyComp('Packed') then Result := TptTokenKind.ptPacked;
end;

function TmwBasePasLex.Func41: TptTokenKind;
begin
  Result := TptTokenKind.ptIdentifier;
  if KeyComp('Var') then Result := TptTokenKind.ptVar else
    if KeyComp('Else') then Result := TptTokenKind.ptElse else
      if KeyComp('Halt') then fExID := TptTokenKind.ptHalt;
end;

{$IFDEF D8_NEWER} //JThurman 2004-03-2003
function TmwBasePasLex.Func42: TptTokenKind;
begin
  Result := TptTokenKind.ptIdentifier;
  if KeyComp('Final') then
    fExID := TptTokenKind.ptFinal;
end;
{$ENDIF}

function TmwBasePasLex.Func43: TptTokenKind;
begin
   Result := TptTokenKind.ptIdentifier;
  if KeyComp('Int64') then fExID := TptTokenKind.ptInt64
  else if KeyComp('local') then fExID := TptTokenKind.ptLocal;
end;

function TmwBasePasLex.Func44: TptTokenKind;
begin
  Result := TptTokenKind.ptIdentifier;
  if KeyComp('Set') then Result := TptTokenKind.ptSet else
    if KeyComp('Package') then fExID := TptTokenKind.ptPackage;
end;

function TmwBasePasLex.Func45: TptTokenKind;
begin
  Result := TptTokenKind.ptIdentifier;
  if KeyComp('Shr') then Result := TptTokenKind.ptShr;
end;

function TmwBasePasLex.Func46: TptTokenKind;
begin
   Result := TptTokenKind.ptIdentifier;
  if KeyComp('PChar') then fExID := TptTokenKind.ptPChar
  {$IFDEF D8_NEWER} //JThurman 2004-03-19
   else
    if KeyComp('Sealed') then Result := TptTokenKind.ptSealed;
  {$ELSE}
  ;
  {$ENDIF} 
end;

function TmwBasePasLex.Func47: TptTokenKind;
begin
  Result := TptTokenKind.ptIdentifier;
  if KeyComp('Then') then Result := TptTokenKind.ptThen else
    if KeyComp('Comp') then fExID := TptTokenKind.ptComp;
end;

function TmwBasePasLex.Func49: TptTokenKind;
begin
  Result := TptTokenKind.ptIdentifier;
  if KeyComp('Not') then Result := TptTokenKind.ptNot;
end;

function TmwBasePasLex.Func52: TptTokenKind;
begin
  Result := TptTokenKind.ptIdentifier;
  if KeyComp('Byte') then fExID := TptTokenKind.ptByte else
    if KeyComp('Raise') then Result := TptTokenKind.ptRaise else
      if KeyComp('Pascal') then fExID := TptTokenKind.ptPascal;
end;

function TmwBasePasLex.Func54: TptTokenKind;
begin
  Result := TptTokenKind.ptIdentifier;
  if KeyComp('Class') then Result := TptTokenKind.ptClass;
end;

function TmwBasePasLex.Func55: TptTokenKind;
begin
  Result := TptTokenKind.ptIdentifier;
  if KeyComp('Object') then Result := TptTokenKind.ptObject;
end;

function TmwBasePasLex.Func56: TptTokenKind;
begin
  Result := TptTokenKind.ptIdentifier;
  if KeyComp('Index') then fExID := TptTokenKind.ptIndex else
    if KeyComp('Out') then fExID := TptTokenKind.ptOut else // bug in Delphi's documentation: OUT is a directive
      if KeyComp('Abort') then fExID := TptTokenKind.ptAbort;
end;

function TmwBasePasLex.Func57: TptTokenKind;
begin
  Result := TptTokenKind.ptIdentifier;
  if KeyComp('While') then Result := TptTokenKind.ptWhile else
    if KeyComp('Xor') then Result := TptTokenKind.ptXor else
      if KeyComp('Goto') then Result := TptTokenKind.ptGoto;
end;

function TmwBasePasLex.Func58: TptTokenKind;
begin
   Result := TptTokenKind.ptIdentifier;
  if KeyComp('Exit') then fExID := TptTokenKind.ptExit;
end;

function TmwBasePasLex.Func59: TptTokenKind;
begin
   Result := TptTokenKind.ptIdentifier;
  if KeyComp('Safecall') then fExID := TptTokenKind.ptSafeCall else
    if KeyComp('Double') then fExID := TptTokenKind.ptDouble;
end;

function TmwBasePasLex.Func60: TptTokenKind;
begin
  Result := TptTokenKind.ptIdentifier;
  if KeyComp('With') then Result := TptTokenKind.ptWith else
    if KeyComp('Word') then fExID := TptTokenKind.ptWord;
end;

function TmwBasePasLex.Func61: TptTokenKind;
begin
  Result := TptTokenKind.ptIdentifier;
  if KeyComp('Dispid') then fExID := TptTokenKind.ptDispid;
end;

function TmwBasePasLex.Func62: TptTokenKind;
begin
   Result := TptTokenKind.ptIdentifier;
  if KeyComp('Cardinal') then fExID := TptTokenKind.ptCardinal;
end;

function TmwBasePasLex.Func63: TptTokenKind;
begin
  Result := TptTokenKind.ptIdentifier;
  case fOrigin[fTokenPos] of
    'P', 'p': if KeyComp('Public') then fExID := TptTokenKind.ptPublic;
    'A', 'a': if KeyComp('Array') then Result := TptTokenKind.ptArray;
    'T', 't': if KeyComp('Try') then Result := TptTokenKind.ptTry;
    'R', 'r': if KeyComp('Record') then Result := TptTokenKind.ptRecord;
    'I', 'i': if KeyComp('Inline') then
     begin
       Result := TptTokenKind.ptInline;
       fExID := TptTokenKind.ptInline;
     end;
  end;
end;

function TmwBasePasLex.Func64: TptTokenKind;
begin
  Result := TptTokenKind.ptIdentifier;
  case fOrigin[fTokenPos] of
    'B', 'b': if KeyComp('Boolean') then fExID := TptTokenKind.ptBoolean;
    'D', 'd': if KeyComp('DWORD') then fExID := TptTokenKind.ptDWORD;
    'U', 'u': if KeyComp('Uses') then Result := TptTokenKind.ptUses
    else
      if KeyComp('Unit') then Result := TptTokenKind.ptUnit;
    {$IFDEF D8_NEWER}
    'H', 'h': if KeyComp('Helper') then Result := TptTokenKind.ptHelper;
    {$ENDIF}
  end;
end;

function TmwBasePasLex.Func65: TptTokenKind;
begin
  Result := TptTokenKind.ptIdentifier;
  if KeyComp('Method') then Result := TptTokenKind.ptMethod else
    if KeyComp('Repeat') then Result := TptTokenKind.ptRepeat;
end;

function TmwBasePasLex.Func66: TptTokenKind;
begin
  Result := TptTokenKind.ptIdentifier;
  if KeyComp('Single') then fExID := TptTokenKind.ptSingle else
    if KeyComp('Type') then Result := TptTokenKind.ptType else 
      if KeyComp('Unsafe') then Result := TptTokenKind.ptUnsafe;
end;

function TmwBasePasLex.Func69: TptTokenKind;
begin
   Result := TptTokenKind.ptIdentifier;
  if KeyComp('Default') then fExID := TptTokenKind.ptDefault else
    if KeyComp('Dynamic') then fExID := TptTokenKind.ptDynamic else
      if KeyComp('Message') then fExID := TptTokenKind.ptMessage;
end;

function TmwBasePasLex.Func71: TptTokenKind;
begin
  Result := TptTokenKind.ptIdentifier;
  if KeyComp('WideChar') then fExID := TptTokenKind.ptWideChar else
    if KeyComp('Stdcall') then fExID := TptTokenKind.ptStdcall else
      if KeyComp('Const') then Result := TptTokenKind.ptConst;
end;

{$IFDEF D8_NEWER} //JThurman 2004-03-2003
function TmwBasePasLex.Func72: TptTokenKind;
begin
   Result := TptTokenKind.ptIdentifier;
  if KeyComp('Static') then
    fExID := TptTokenKind.ptStatic;
end;
{$ENDIF}

function TmwBasePasLex.Func73: TptTokenKind;
begin
  Result := TptTokenKind.ptIdentifier;
  if KeyComp('Except') then Result := TptTokenKind.ptExcept;
end;

function TmwBasePasLex.Func75: TptTokenKind;
begin
  Result := TptTokenKind.ptIdentifier;
  if KeyComp('Write') then fExID := TptTokenKind.ptWrite;
end;

function TmwBasePasLex.Func76: TptTokenKind;
begin
  Result := TptTokenKind.ptIdentifier;
  if KeyComp('Until') then Result := TptTokenKind.ptUntil;
end;

function TmwBasePasLex.Func77: TptTokenKind;
begin
  Result := TptTokenKind.ptIdentifier;
  if KeyComp('Namespace') then Result := TptTokenKind.ptUnit;
end;

function TmwBasePasLex.Func78: TptTokenKind;
begin
   Result := TptTokenKind.ptIdentifier;
   if KeyComp('Integer') then fExID := TptTokenKind.ptInteger 
    else if KeyComp('Remove') then
      fExID := TptTokenKind.ptRemove;
end;

function TmwBasePasLex.Func79: TptTokenKind;
begin
  Result := TptTokenKind.ptIdentifier;
  if KeyComp('Finally') then Result := TptTokenKind.ptFinally
  {$IFDEF D12_NEWER}
  else if KeyComp('Reference') then fExID := TptTokenKind.ptReference;
  {$ENDIF}  
end;

function TmwBasePasLex.Func81: TptTokenKind;
begin
  Result := TptTokenKind.ptIdentifier;
  if KeyComp('Extended') then fExID := TptTokenKind.ptExtended else
    if KeyComp('Stored') then fExID := TptTokenKind.ptStored else
	  if KeyComp('Interface') then Result := TptTokenKind.ptInterface
	    else if KeyComp('Deprecated') then fExID := TptTokenKind.ptDeprecated; // DR 2001-10-20
end;

function TmwBasePasLex.Func84: TptTokenKind;
begin
  Result := TptTokenKind.ptIdentifier;
  if KeyComp('Abstract') then fExID := TptTokenKind.ptAbstract;
end;

function TmwBasePasLex.Func85: TptTokenKind;
begin
   Result := TptTokenKind.ptIdentifier;
  if KeyComp('Library') then Result := TptTokenKind.ptLibrary else
  	if KeyComp('Forward') then fExID := TptTokenKind.ptForward else
	    if KeyComp('Variant') then fExID := TptTokenKind.ptVariant;
end;

function TmwBasePasLex.Func87: TptTokenKind;
begin
  Result := TptTokenKind.ptIdentifier;
  if KeyComp('String') then Result := TptTokenKind.ptString;
end;

function TmwBasePasLex.Func88: TptTokenKind;
begin
  Result := TptTokenKind.ptIdentifier;
  if KeyComp('Program') then Result := TptTokenKind.ptProgram;
end;

{$IFDEF D8_NEWER} //JThurman 2004-03-03
function TmwBasePasLex.Func89: TptTokenKind;
begin
  Result := TptTokenKind.ptIdentifier;
  if KeyComp('Strict') then Result := TptTokenKind.ptStrict; 
end;
{$ENDIF}

function TmwBasePasLex.Func91: TptTokenKind;
begin
  Result := TptTokenKind.ptIdentifier;
  if KeyComp('Downto') then Result := TptTokenKind.ptDownto else
    if KeyComp('Private') then fExID := TptTokenKind.ptPrivate else
      if KeyComp('Longint') then fExID := TptTokenKind.ptLongint;
end;

function TmwBasePasLex.Func92: TptTokenKind;
begin
  Result := TptTokenKind.ptIdentifier;
  if KeyComp('Inherited') then Result := TptTokenKind.ptInherited else
    if KeyComp('LongBool') then fExID := TptTokenKind.ptLongBool else
      if KeyComp('Overload') then fExID := TptTokenKind.ptOverload;
end;

function TmwBasePasLex.Func94: TptTokenKind;
begin
  Result := TptTokenKind.ptIdentifier;
  if KeyComp('Resident') then fExID := TptTokenKind.ptResident else
    if KeyComp('Readonly') then fExID := TptTokenKind.ptReadonly else
      if KeyComp('Assembler') then fExID := TptTokenKind.ptAssembler;
end;

function TmwBasePasLex.Func95: TptTokenKind;
begin
  Result := TptTokenKind.ptIdentifier;
  if KeyComp('Contains') then fExID := TptTokenKind.ptContains else
    if KeyComp('Absolute') then fExID := TptTokenKind.ptAbsolute;
end;

function TmwBasePasLex.Func96: TptTokenKind;
begin
  Result := TptTokenKind.ptIdentifier;
  if KeyComp('ByteBool') then fExID := TptTokenKind.ptByteBool else
    if KeyComp('Override') then fExID := TptTokenKind.ptOverride else
      if KeyComp('Published') then fExID := TptTokenKind.ptPublished;
end;

function TmwBasePasLex.Func97: TptTokenKind;
begin
  Result := TptTokenKind.ptIdentifier;
  if KeyComp('Threadvar') then Result := TptTokenKind.ptThreadvar;
end;

function TmwBasePasLex.Func98: TptTokenKind;
begin
  Result := TptTokenKind.ptIdentifier;
  if KeyComp('Export') then fExID := TptTokenKind.ptExport else
    if KeyComp('Nodefault') then fExID := TptTokenKind.ptNodefault;
end;

function TmwBasePasLex.Func99: TptTokenKind;
begin
   Result := TptTokenKind.ptIdentifier;
  if KeyComp('External') then fExID := TptTokenKind.ptExternal;
end;

function TmwBasePasLex.Func100: TptTokenKind;
begin
  Result := TptTokenKind.ptIdentifier;
  if KeyComp('Automated') then fExID := TptTokenKind.ptAutomated else
    if KeyComp('Smallint') then fExID := TptTokenKind.ptSmallint;
end;

function TmwBasePasLex.Func101: TptTokenKind;
begin
  Result := TptTokenKind.ptIdentifier;
  if KeyComp('Register') then fExID := TptTokenKind.ptRegister
  else if KeyComp('Platform') then fExID := TptTokenKind.ptPlatform // DR 2001-10-20
  else if KeyComp('Continue') then fExID := TptTokenKind.ptContinue;
end;

function TmwBasePasLex.Func102: TptTokenKind;
begin
   Result := TptTokenKind.ptIdentifier;
  if KeyComp('Function') then Result := TptTokenKind.ptFunction;
end;

function TmwBasePasLex.Func103: TptTokenKind;
begin
  Result := TptTokenKind.ptIdentifier;
  if KeyComp('Virtual') then fExID := TptTokenKind.ptVirtual;
end;

function TmwBasePasLex.Func104: TptTokenKind;
begin
  Result := TptTokenKind.ptIdentifier;
  if KeyComp('WordBool') then fExID := TptTokenKind.ptWordBool;
end;

function TmwBasePasLex.Func105: TptTokenKind;
begin
  Result := TptTokenKind.ptIdentifier;
  if KeyComp('Procedure') then Result := TptTokenKind.ptProcedure;
end;

function TmwBasePasLex.Func106: TptTokenKind;
begin
  Result := TptTokenKind.ptIdentifier;
  if KeyComp('Protected') then fExID := TptTokenKind.ptProtected;
end;

function TmwBasePasLex.Func107: TptTokenKind;
begin
  Result := TptTokenKind.ptIdentifier;
  if KeyComp('Currency') then fExID := TptTokenKind.ptCurrency;
end;

function TmwBasePasLex.Func108: TptTokenKind;
begin
  Result := TptTokenKind.ptIdentifier;
  if KeyComp('Longword') then fExID := TptTokenKind.ptLongword;
  if KeyComp('Operator') then fExID := TptTokenKind.ptOperator;
end;

function TmwBasePasLex.Func112: TptTokenKind;
begin
   Result := TptTokenKind.ptIdentifier;
  if KeyComp('Requires') then fExID := TptTokenKind.ptRequires;
end;

function TmwBasePasLex.Func117: TptTokenKind;
begin
  Result := TptTokenKind.ptIdentifier;
  if KeyComp('Exports') then Result := TptTokenKind.ptExports else
    if KeyComp('OleVariant') then fExID := TptTokenKind.ptOleVariant;
end;

function TmwBasePasLex.Func123: TptTokenKind;
begin
   Result := TptTokenKind.ptIdentifier;
  if KeyComp('Shortint') then fExID := TptTokenKind.ptShortint;
end;

function TmwBasePasLex.Func126: TptTokenKind;
begin
  Result := TptTokenKind.ptIdentifier;
  if KeyComp('Implements') then fExID := TptTokenKind.ptImplements;
end;

function TmwBasePasLex.Func127: TptTokenKind;
begin
   Result := TptTokenKind.ptIdentifier;
  if KeyComp('Runerror') then fExID := TptTokenKind.ptRunError;
end;

function TmwBasePasLex.Func128: TptTokenKind;
begin
  if KeyComp('WideString') then fExID := TptTokenKind.ptWideString;
  Result := TptTokenKind.ptIdentifier;
end;

function TmwBasePasLex.Func129: TptTokenKind;
begin
  Result := TptTokenKind.ptIdentifier;
  if KeyComp('Dispinterface') then Result := TptTokenKind.ptDispinterface
end;

function TmwBasePasLex.Func130: TptTokenKind;
begin
  Result := TptTokenKind.ptIdentifier;
  if KeyComp('AnsiString') then fExID := TptTokenKind.ptAnsiString;
end;

function TmwBasePasLex.Func132: TptTokenKind;
begin
  Result := TptTokenKind.ptIdentifier;
  if KeyComp('Reintroduce') then fExID := TptTokenKind.ptReintroduce;
end;

function TmwBasePasLex.Func133: TptTokenKind;
begin
  Result := TptTokenKind.ptIdentifier;
  if KeyComp('Property') then Result := TptTokenKind.ptProperty;
end;

function TmwBasePasLex.Func136: TptTokenKind;
begin
  Result := TptTokenKind.ptIdentifier;
  if KeyComp('Finalization') then Result := TptTokenKind.ptFinalization;
end;

function TmwBasePasLex.Func141: TptTokenKind;
begin
  Result := TptTokenKind.ptIdentifier;
  if KeyComp('Writeonly') then fExID := TptTokenKind.ptWriteonly;
end;

function TmwBasePasLex.Func143: TptTokenKind;
begin
  Result := TptTokenKind.ptIdentifier;
  if KeyComp('Destructor') then Result := TptTokenKind.ptDestructor;
end;

function TmwBasePasLex.Func166: TptTokenKind;
begin
  Result := TptTokenKind.ptIdentifier;
  if KeyComp('Constructor') then Result := TptTokenKind.ptConstructor else
    if KeyComp('Implementation') then Result := TptTokenKind.ptImplementation;
end;

function TmwBasePasLex.Func167: TptTokenKind;
begin
  Result := TptTokenKind.ptIdentifier;
  if KeyComp('ShortString') then fExID := TptTokenKind.ptShortString;
end;

function TmwBasePasLex.Func168: TptTokenKind;
begin
  Result := TptTokenKind.ptIdentifier;
  if KeyComp('Initialization') then Result := TptTokenKind.ptInitialization;
end;

function TmwBasePasLex.Func191: TptTokenKind;
begin
  Result := TptTokenKind.ptIdentifier;
  if KeyComp('Resourcestring') then Result := TptTokenKind.ptResourcestring else
    if KeyComp('Stringresource') then fExID := TptTokenKind.ptStringresource;
end;

function TmwBasePasLex.AltFunc: TptTokenKind;
begin
  Result := TptTokenKind.ptIdentifier;
end;


function TmwBasePasLex.IdentKind: TptTokenKind;
var
  HashKey: Integer;
begin
  HashKey := KeyHash;
  if HashKey < 192 then
	Result := fIdentFuncTable[HashKey]{$IFDEF OXYGENE}(){$ENDIF}
  else Result := TptTokenKind.ptIdentifier;
end;

procedure TmwBasePasLex.MakeMethodTables;
{$IFDEF OXYGENE}
begin
  for i: Char := #0 to #255 do
    case i of
      #0: fProcTable[i] := @NullProc;
	    #10: fProcTable[i] := @LFProc;
	    #13: fProcTable[i] := @CRProc;
      #1..#9, #11, #12, #14..#32:
        fProcTable[i] := @SpaceProc;
      '#': fProcTable[i] := @AsciiCharProc;
      '$': fProcTable[i] := @IntegerProc;
      #39: fProcTable[i] := @StringProc;
      '0'..'9': fProcTable[i] := @NumberProc;
      'A'..'Z', 'a'..'z', '_':
        fProcTable[i] := @IdentProc;
      '{': fProcTable[i] := @BraceOpenProc;
      '}': fProcTable[i] := @BraceCloseProc;
      '!', '"', '%', '&', '('..'/', ':'..'@', '['..'^', '`', '~':
        begin
          case i of
            '(': fProcTable[i] := @RoundOpenProc;
            ')': fProcTable[i] := @RoundCloseProc;
            '*': fProcTable[i] := @StarProc;
			      '+': fProcTable[i] := @PlusProc;
            ',': fProcTable[i] := @CommaProc;
            '-': fProcTable[i] := @MinusProc;
            '.': fProcTable[i] := @PointProc;
            '/': fProcTable[i] := @SlashProc;
            ':': fProcTable[i] := @ColonProc;
            ';': fProcTable[i] := @SemiColonProc;
            '<': fProcTable[i] := @LowerProc;
            '=': fProcTable[i] := @EqualProc;
            '>': fProcTable[i] := @GreaterProc;
            '@': fProcTable[i] := @AddressOpProc;
            '[': fProcTable[i] := @SquareOpenProc;
            ']': fProcTable[i] := @SquareCloseProc;
			      '^': fProcTable[i] := @PointerSymbolProc;
		       '"': fProcTable[i] := @StringDQProc;
           '&': fProcTable[i] := @AmpersandOpProc;    
          else fProcTable[i] := @SymbolProc;
          end;
        end;
    else fProcTable[i] := @UnknownProc;
    end;  
{$ELSE}
var
  I: Char;
begin
  for I := #0 to #255 do
    case I of
      #0: fProcTable[I] := NullProc;
	  #10: fProcTable[I] := LFProc;
	  #13: fProcTable[I] := CRProc;
      #1..#9, #11, #12, #14..#32:
        fProcTable[I] := SpaceProc;
      '#': fProcTable[I] := AsciiCharProc;
      '$': fProcTable[I] := IntegerProc;
      #39: fProcTable[I] := StringProc;
      '0'..'9': fProcTable[I] := NumberProc;
      'A'..'Z', 'a'..'z', '_':
        fProcTable[I] := IdentProc;
      '{': fProcTable[I] := BraceOpenProc;
      '}': fProcTable[I] := BraceCloseProc;
      '!', '"', '%', '&', '('..'/', ':'..'@', '['..'^', '`', '~':
        begin
          case I of
            '(': fProcTable[I] := RoundOpenProc;
            ')': fProcTable[I] := RoundCloseProc;
            '*': fProcTable[I] := StarProc;
			'+': fProcTable[I] := PlusProc;
            ',': fProcTable[I] := CommaProc;
            '-': fProcTable[I] := MinusProc;
            '.': fProcTable[I] := PointProc;
            '/': fProcTable[I] := SlashProc;
            ':': fProcTable[I] := ColonProc;
            ';': fProcTable[I] := SemiColonProc;
            '<': fProcTable[I] := LowerProc;
            '=': fProcTable[I] := EqualProc;
            '>': fProcTable[I] := GreaterProc;
            '@': fProcTable[I] := AddressOpProc;
            '[': fProcTable[I] := SquareOpenProc;
            ']': fProcTable[I] := SquareCloseProc;
			'^': fProcTable[I] := PointerSymbolProc;
			'"': fProcTable[I] := StringDQProc; // DR 2002-01-14
      {$IFDEF D8_NEWER} //JThurman 2004-04-06
      '&': fProcTable[I] := AmpersandOpProc;
      {$ENDIF}
          else fProcTable[I] := SymbolProc;
          end;
        end;
    else fProcTable[I] := UnknownProc;
    end; 
{$ENDIF}
end;

constructor TmwBasePasLex.Create;
begin
  inherited Create;
  {$IFNDEF OXYGENE}
  fOrigin := nil;
  {$ENDIF}
  InitIdent;
  MakeMethodTables;
  fExID := TptTokenKind.ptUnknown;
  fDefines := TStringList.Create;
  FUseDefines := True; 
  FTopDefineRec := nil;
  InitDefines;
end; { Create }

{$IFDEF OXYGENE}
finalizer TmwBasePasLex;
begin
  inherited;
end;  
{$ELSEIF}
destructor TmwBasePasLex.Destroy;
begin
  ClearDefines; //If we don't do this, we get a memory leak
  FDefines.Free;
  fOrigin := nil;
  inherited Destroy;
end;
{$ENDIF}


procedure TmwBasePasLex.DoProcTable(AChar: Char);
begin
  if AChar <= #255 then
    fProcTable[AChar]
  else
  begin
    IdentProc;
  end;
end;

{ Destroy }

procedure TmwBasePasLex.SetOrigin(NewValue: PChar);
begin
  fOrigin := NewValue;
  Init;
  Next;
end; { SetOrigin }

procedure TmwBasePasLex.SetRunPos(Value: Integer);
begin
  Run := Value;
  Next;
end;

procedure TmwBasePasLex.AddDefine(const ADefine: String);
begin
  fDefines.Add(ADefine);
end;

procedure TmwBasePasLex.AddressOpProc;
begin
  case fOrigin[Run + 1] of
    '@':
      begin 
        FTokenID := TptTokenKind.ptDoubleAddressOp; 
        inc(Run, 2);
      end;
  else
    begin   
      FTokenID := TptTokenKind.ptAddressOp;    
      inc(Run);
    end;
  end;
end;

procedure TmwBasePasLex.AsciiCharProc;
begin
  FTokenID := TptTokenKind.ptAsciiChar;
  inc(Run);
  if fOrigin[Run] = '$' then
  begin
    inc(Run);
    while fOrigin[Run] in ['0'..'9', 'A'..'F', 'a'..'f'] do inc(Run);
  end else
  begin
    while fOrigin[Run] in ['0'..'9'] do
      inc(Run);
  end;
end;

procedure TmwBasePasLex.BraceCloseProc;
begin
  inc(Run);
  FTokenID := TptTokenKind.ptError;
  if assigned(FOnMessage) then
	FOnMessage(Self, TMessageEventType.meError, 'Illegal character', PosXY.X, PosXY.Y);
end;

procedure TmwBasePasLex.BorProc;
begin
  FTokenID := TptTokenKind.ptBorComment;
  case fOrigin[Run] of
    #0:
      begin
		NullProc;
        if assigned(FOnMessage) then
        {$IFDEF OXYGENE}
        FOnMessage(Self, TMessageEventType.meError, 'Unexpected file end', PosXY.X, PosXY.Y);
        {$ELSE}
        FOnMessage(Self, meError, 'Unexpected file end', PosXY.X, PosXY.Y);
        {$ENDIF}       
        exit;
      end;
  end;

  while fOrigin[Run] <> #0 do
	case fOrigin[Run] of
	  '}':
		begin
  
      fCommentState := TCommentState.csNo;		  
		  inc(Run);
		  break;
		end;
	  #10:
		begin
			inc(Run);
			inc(fLineNumber);
			fLinePos := Run;
		end;
	  #13:
		begin
			inc(Run);
			if fOrigin[Run] = #10 then inc( Run );
			inc(fLineNumber);
			fLinePos := Run;
		end;
	else inc(Run);
	end;
end;

procedure TmwBasePasLex.BraceOpenProc;
var
  Param, Def: String;
begin
  case fOrigin[Run + 1] of
    '$': FTokenID := GetDirectiveKind;
  else
    begin     
      FTokenID := TptTokenKind.ptBorComment;
      fCommentState := TCommentState.csBor;
    end;
  end;
  inc(Run);
  while fOrigin[Run] <> #0 do
    case fOrigin[Run] of
      '}':
        begin        
          fCommentState := TCommentState.csNo;     
          inc(Run);
          break;
		end;
	  #10:
		begin
			inc(Run);
			inc(fLineNumber);
			fLinePos := Run;
		end;
	  #13:
		begin
			inc(Run);
			if fOrigin[Run] = #10 then inc( Run );
			inc(fLineNumber);
			fLinePos := Run;
		end;
    else inc(Run);
    end;
  case FTokenID of
    TptTokenKind.ptCompDirect:
      begin
        if assigned(fOnCompDirect) then
          fOnCompDirect(Self);
      end;
    TptTokenKind.ptDefineDirect: 
      begin
        if FUseDefines then
          AddDefine(DirectiveParam);
        if assigned(fOnDefineDirect) then
          fOnDefineDirect(Self);
      end;
    TptTokenKind.ptElseDirect:
      begin
        if FUseDefines then
        begin
          if FTopDefineRec <> nil then
          begin
            if FTopDefineRec^.Defined then
              inc(FDefineStack)
            else
              if FDefineStack > 0 then
                dec(FDefineStack);
          end;
        end;
        if assigned(fOnElseDirect) then
          fOnElseDirect(Self);
      end;
    TptTokenKind.ptEndIfDirect:
      begin
        if FUseDefines then
          ExitDefineBlock;
        if assigned(fOnEndIfDirect) then
          fOnEndIfDirect(Self);
      end;
    TptTokenKind.ptIfDefDirect:
      begin
        if FUseDefines then
          EnterDefineBlock(IsDefined(DirectiveParam));
        if assigned(fOnIfDefDirect) then
          fOnIfDefDirect(Self);
      end;
    TptTokenKind.ptIfNDefDirect:  
     begin
        if FUseDefines then
          EnterDefineBlock(not IsDefined(DirectiveParam));
    		if assigned(fOnIfNDefDirect) then
          fOnIfNDefDirect(Self);
      end;
    TptTokenKind.ptIfOptDirect:
      begin
        if assigned(fOnIfOptDirect) then
          fOnIfOptDirect(Self);
      end;
    TptTokenKind.ptIfDirect:  
      begin
        if FUseDefines then
        begin
          Param := DirectiveParam;
          if Pos('DEFINED', Param) = 1 then
          begin      
            Def := Copy(Param, 9, length(Param) - 9);
            EnterDefineBlock(IsDefined(Def));
          end;
        end;
        if assigned(fOnIfDirect) then
          fOnIfDirect(Self);
      end;
    TptTokenKind.ptIfEndDirect:
      begin
        if FUseDefines then
          ExitDefineBlock;
        if assigned(fOnIfEndDirect) then
          fOnIfEndDirect(Self);
      end;
    TptTokenKind.ptElseIfDirect: 
      begin
        if FUseDefines then
        begin
          if FTopDefineRec <> nil then
          begin
            if FTopDefineRec^.Defined then
              inc(FDefineStack)
            else
            begin
              if FDefineStack > 0 then
                dec(FDefineStack);
              Param := DirectiveParam;
              if Pos('DEFINED', Param) = 1 then  
              begin
                Def := Copy(Param, 9, length(Param) - 9);
                EnterDefineBlock(IsDefined(Def));
              end;
            end;
          end;
        end;
        if assigned(fOnElseIfDirect) then
          fOnElseIfDirect(Self);
      end;
    TptTokenKind.ptIncludeDirect:  
      begin
        if assigned(fOnIncludeDirect) then
          fOnIncludeDirect(Self);
      end;
    TptTokenKind.ptResourceDirect:
      begin
        if assigned(fOnResourceDirect) then
          fOnResourceDirect(Self);
      end;
    TptTokenKind.ptUndefDirect:
      begin
        if FUseDefines then
          RemoveDefine(DirectiveParam);
        if assigned(fOnUnDefDirect) then
          fOnUnDefDirect(Self);
      end;
  end;
end;

procedure TmwBasePasLex.ColonProc;
begin
  case fOrigin[Run + 1] of
    '=':
      begin
        inc(Run, 2);       
        FTokenID := TptTokenKind.ptAssign;       
	  end;
  else
    begin
      inc(Run);  
      FTokenID := TptTokenKind.ptColon;  
    end;
  end;
end;

procedure TmwBasePasLex.CommaProc;
begin
  inc(Run);
  FTokenID := TptTokenKind.ptComma;
end;

procedure TmwBasePasLex.CRProc;
begin
  case fCommentState of
    TCommentState.csBor: FTokenID := TptTokenKind.ptCRLFCo;
    TCommentState.csAnsi: FTokenID := TptTokenKind.ptCRLFCo;
  else FTokenID := TptTokenKind.ptCRLF;
  end;
  case fOrigin[Run + 1] of
    #10: inc(Run, 2);
  else inc(Run);
  end;
  inc(fLineNumber);
  fLinePos := Run;
end;

procedure TmwBasePasLex.EnterDefineBlock(ADefined: Boolean);
var
  StackFrame: PDefineRec;
begin
  {$IFDEF OXYGENE} 
  var dr := new TDefineRec;
  StackFrame := @dr;
  {$ELSE}
  New(StackFrame);
  {$ENDIF}
  StackFrame^.Next := FTopDefineRec;
  StackFrame^.Defined := ADefined;
  StackFrame^.StartCount := FDefineStack;
  FTopDefineRec := StackFrame;
  if not ADefined then
    inc(FDefineStack);
end;

procedure TmwBasePasLex.EqualProc;
begin
  inc(Run);
  FTokenID := TptTokenKind.ptEqual;
end;

procedure TmwBasePasLex.ExitDefineBlock;
var
  StackFrame: PDefineRec;
begin
  StackFrame := FTopDefineRec;
  if StackFrame <> nil then
  begin
    FDefineStack := StackFrame^.StartCount;
    FTopDefineRec := StackFrame^.Next;
    {$IFDEF OXYGENE}
    disposeAndNil(StackFrame);  
    {$ELSE}
    Dispose(StackFrame); 
    {$ENDIF}  
  end;
end;
procedure TmwBasePasLex.GreaterProc;
begin
  case fOrigin[Run + 1] of
    '=':
      begin
        inc(Run, 2);
        FTokenID := TptTokenKind.ptGreaterEqual;
      end;
  else
    begin
      inc(Run);
      FTokenID := TptTokenKind.ptGreater;
	end;
  end;
end;

function TmwBasePasLex.HashValue(AChar: Char): Integer;
begin
  if AChar <= #255 then
    Result := mHashTable[fOrigin[Run]]
  else
    Result := ord(AChar);
end;

procedure TmwBasePasLex.IdentProc;
begin
  FTokenID := IdentKind;
end;

procedure TmwBasePasLex.IntegerProc;
begin
  inc(Run);
  FTokenID := TptTokenKind.ptIntegerConst;
  while fOrigin[Run] in ['0'..'9', 'A'..'F', 'a'..'f'] do
    inc(Run);
end;

function TmwBasePasLex.IsDefined(const ADefine: String): Boolean;
begin
  Result := fDefines.IndexOf(ADefine) > -1;
end;

function TmwBasePasLex.IsIdentifiers(AChar: Char): Boolean;
begin
  if AChar <= #255 then
    Result := Identifiers[AChar]
  else
    Result := True;
end;

procedure TmwBasePasLex.LFProc;
begin
  case fCommentState of
	  TCommentState.csBor: FTokenID := TptTokenKind.ptCRLFCo;
	  TCommentState.csAnsi: FTokenID := TptTokenKind.ptCRLFCo;
  else FTokenID := TptTokenKind.ptCRLF;
  end;
  inc(Run);
  inc(fLineNumber);
  fLinePos := Run;
end;

procedure TmwBasePasLex.LowerProc;
begin
  case fOrigin[Run + 1] of
    '=':
      begin
        inc(Run, 2);
        FTokenID := TptTokenKind.ptLowerEqual;
      end;
    '>':
      begin
        inc(Run, 2);
        FTokenID := TptTokenKind.ptNotEqual;
      end
  else
    begin
      inc(Run);
      FTokenID := TptTokenKind.ptLower;
    end;
  end;
end;

procedure TmwBasePasLex.MinusProc;
begin
  inc(Run);
  FTokenID := TptTokenKind.ptMinus;
end;

procedure TmwBasePasLex.NullProc;
begin
  FTokenID := TptTokenKind.ptNull;
end;

procedure TmwBasePasLex.NumberProc;
begin
  inc(Run);
  FTokenID := TptTokenKind.ptIntegerConst;
  while fOrigin[Run] in ['0'..'9', '.', 'e', 'E'] do
  begin
    case fOrigin[Run] of
      '.':
        if fOrigin[Run + 1] = '.' then
          break
        else FTokenID := TptTokenKind.ptFloat
    end;
    inc(Run);
  end;
end;

procedure TmwBasePasLex.PlusProc;
begin
  inc(Run);
  FTokenID := TptTokenKind.ptPlus;
end;

procedure TmwBasePasLex.PointerSymbolProc;
begin
  inc(Run);
  FTokenID := TptTokenKind.ptPointerSymbol;
  
  //This is a wierd Pascal construct that rarely appears, but needs to be 
  //supported. ^M is a valid char reference (#13, in this case)
  if fOrigin[Run] in ['a'..'z','A'..'Z'] then
  begin
    inc(Run);
    FTokenID := TptTokenKind.ptAsciiChar;
  end;
end;

procedure TmwBasePasLex.PointProc;
begin
  case fOrigin[Run + 1] of
    '.':
      begin
        inc(Run, 2);
        FTokenID := TptTokenKind.ptDotDot;
      end;
    ')':
      begin
        inc(Run, 2);
        FTokenID := TptTokenKind.ptSquareClose;
      end;
  else
    begin
      inc(Run);
      FTokenID := TptTokenKind.ptPoint;
    end;
  end;
end;

procedure TmwBasePasLex.RemoveDefine(const ADefine: String);
var
  I: Integer;
begin
  I := fDefines.IndexOf(ADefine);
  if I > -1 then
    fDefines.Delete(I);
end;

procedure TmwBasePasLex.RoundCloseProc;
begin
  inc(Run);
  FTokenID := TptTokenKind.ptRoundClose;
end;

procedure TmwBasePasLex.AnsiProc;
begin
  FTokenID := TptTokenKind.ptAnsiComment;
  case fOrigin[Run] of
    #0:
      begin
        NullProc;
        if assigned(FOnMessage) then
          FOnMessage(Self, TMessageEventType.meError, 'Unexpected file end', PosXY.X, PosXY.Y);
        exit;
      end;

{				DR 2001-08-02
	#10:
      begin
		LFProc;
        exit;
      end;

    #13:
      begin
        CRProc;
        exit;
      end;
}
  end;

  while fOrigin[Run] <> #0 do
    case fOrigin[Run] of
      '*':
        if fOrigin[Run + 1] = ')' then
        begin
          fCommentState := TCommentState.csNo;
          inc(Run, 2);
          break;
        end
        else inc(Run);
{                DR 2001-08-02
      #10: break;

	  #13: break;
}
	  #10:
		begin
			inc(Run);
			inc(fLineNumber);
			fLinePos := Run;
		end;
	  #13:
		begin
			inc(Run);
			if fOrigin[Run] = #10 then inc( Run );
			inc(fLineNumber);
			fLinePos := Run;
		end;
	else inc(Run);
    end;
end;

procedure TmwBasePasLex.RoundOpenProc;
begin
  inc(Run);
  case fOrigin[Run] of
    '*':
      begin
        FTokenID := TptTokenKind.ptAnsiComment;
        if fOrigin[Run + 1] = '$' then
          FTokenID := GetDirectiveKind
        else fCommentState := TCommentState.csAnsi;
        inc(Run);
        while fOrigin[Run] <> #0 do
          case fOrigin[Run] of
            '*':
			  if fOrigin[Run + 1] = ')' then
			  begin
				fCommentState := TCommentState.csNo;
				inc(Run, 2);
				break;
			  end
			  else inc(Run);
{								DR 2001-08-02
			#10: break;
			#13: break;
}
			  #10:
				begin
					inc(Run);
					inc(fLineNumber);
					fLinePos := Run;
				end;
			  #13:
				begin
					inc(Run);
					if fOrigin[Run] = #10 then inc( Run );
					inc(fLineNumber);
					fLinePos := Run;
				end;
			else inc(Run);
          end;
      end;
    '.':
      begin
        inc(Run);
        FTokenID := TptTokenKind.ptSquareOpen;
      end;
  else FTokenID := TptTokenKind.ptRoundOpen;
  end;
  case FTokenID of
    TptTokenKind.ptCompDirect:
      begin
        if assigned(fOnCompDirect) then
          fOnCompDirect(Self);
      end;
    TptTokenKind.ptDefineDirect:
      begin
        if assigned(fOnDefineDirect) then
          fOnDefineDirect(Self);
      end;
    TptTokenKind.ptElseDirect:
      begin
        if assigned(fOnElseDirect) then
          fOnElseDirect(Self);
      end;
    TptTokenKind.ptEndIfDirect:
      begin
        if assigned(fOnEndIfDirect) then
          fOnEndIfDirect(Self);
      end;
    TptTokenKind.ptIfDefDirect:
      begin
        if assigned(fOnIfDefDirect) then
          fOnIfDefDirect(Self);
      end;
    TptTokenKind.ptIfNDefDirect:
      begin
        if assigned(fOnIfNDefDirect) then
          fOnIfNDefDirect(Self);
      end;
    TptTokenKind.ptIfOptDirect:
      begin
        if assigned(fOnIfOptDirect) then
          fOnIfOptDirect(Self);
      end;
    TptTokenKind.ptIncludeDirect:
      begin
        if assigned(fOnIncludeDirect) then
          fOnIncludeDirect(Self);
      end;
    TptTokenKind.ptResourceDirect:
      begin
        if assigned(fOnResourceDirect) then
          fOnResourceDirect(Self);
      end;
    TptTokenKind.ptUndefDirect:
      begin
        if assigned(fOnUnDefDirect) then
          fOnUnDefDirect(Self);
      end;
  end;
end;

procedure TmwBasePasLex.SemiColonProc;
begin
  inc(Run);
  FTokenID := TptTokenKind.ptSemiColon;
end;

procedure TmwBasePasLex.SlashProc;
begin
  case fOrigin[Run + 1] of
    '/':
      begin
        inc(Run, 2);
        FTokenID := TptTokenKind.ptSlashesComment;
        while fOrigin[Run] <> #0 do
        begin
          case fOrigin[Run] of
            #10, #13: break;
          end;
          inc(Run);
        end;
      end;
  else
    begin
      inc(Run);
      FTokenID := TptTokenKind.ptSlash;
    end;
  end;
end;

procedure TmwBasePasLex.SpaceProc;
begin
  inc(Run);
  FTokenID := TptTokenKind.ptSpace;
  while fOrigin[Run] in [#1..#9, #11, #12, #14..#32] do
    inc(Run);
end;

procedure TmwBasePasLex.SquareCloseProc;
begin
  inc(Run);
  FTokenID := TptTokenKind.ptSquareClose;
end;

procedure TmwBasePasLex.SquareOpenProc;
begin
  inc(Run);
  FTokenID := TptTokenKind.ptSquareOpen;
end;

procedure TmwBasePasLex.StarProc;
begin
  inc(Run);
  FTokenID := TptTokenKind.ptStar;
end;

procedure TmwBasePasLex.StringProc;
begin
  FTokenID := TptTokenKind.ptStringConst;
  repeat
	inc(Run);
	case fOrigin[Run] of
	  #0, #10, #13:
		begin
		  if assigned(FOnMessage) then
			FOnMessage(Self, TMessageEventType.meError, 'Unterminated string', PosXY.X, PosXY.Y);
		  break;
		end;
	  #39:
		begin
		  while (fOrigin[Run] = #39) and (fOrigin[Run + 1] = #39) do
		  begin
			inc(Run, 2);
		  end;
		end;
	end;
  until fOrigin[Run] = #39;
  if fOrigin[Run] = #39 then
  begin
	inc(Run);
	if TokenLen = 3 then
	begin
	  FTokenID := TptTokenKind.ptAsciiChar;
	end;
  end;
end;

procedure TmwBasePasLex.SymbolProc;
begin
  inc(Run);
  FTokenID := TptTokenKind.ptSymbol;
end;

procedure TmwBasePasLex.UnknownProc;
begin
  inc(Run);
  FTokenID := TptTokenKind.ptUnknown;
  if assigned(FOnMessage) then
   FOnMessage(Self, TMessageEventType.meError, 'Unknown Character', PosXY.X, PosXY.Y);
end;

procedure TmwBasePasLex.Next;
begin
  fExID := TptTokenKind.ptUnknown;
  fTokenPos := Run;
  case fCommentState of
    TCommentState.csNo:
    begin
      DoProcTable(fOrigin[Run]);
     (*{$IFDEF D10_NEWER}
     if fOrigin[Run] < #256 then
       fProcTable[fOrigin[Run]]
     else //non-ASCII unicode char
       IdentProc;
     {$ELSE}
     fProcTable[fOrigin[Run]];
     {$ENDIF}*)
    end;
  else
    case fCommentState of
      TCommentState.csBor: BorProc;
      TCommentState.csAnsi: AnsiProc;
    end;
  end;
end;


function TmwBasePasLex.GetIsJunk: Boolean;
begin
  result := IsTokenIDJunk(FTokenID) or (FUseDefines and (FDefineStack > 0) and (TokenID <> TptTokenKind.ptNull));
//  Result := fTokenID in [ptAnsiComment, ptBorComment, ptCRLF, ptCRLFCo, ptSlashesComment, ptSpace]; //XM 20001210
end;

function TmwBasePasLex.GetIsSpace: Boolean;
begin
  Result := FTokenID in [TptTokenKind.ptCRLF, TptTokenKind.ptSpace];
end;

function TmwBasePasLex.GetToken: String;
begin
  SetString(Result, (fOrigin + fTokenPos), GetTokenLen);
end;

function TmwBasePasLex.GetTokenLen: Integer;
begin
  Result := Run - fTokenPos;
end;

procedure TmwBasePasLex.NextID(ID: TptTokenKind);
begin
  repeat
    case FTokenID of
      TptTokenKind.ptNull: break;
    else Next;
    end;
  until FTokenID = ID;
end;

procedure TmwBasePasLex.NextNoJunk;
begin
  repeat
    Next;
  until not IsJunk;
end;

procedure TmwBasePasLex.NextNoSpace;
begin
  repeat
    Next;
  until not IsSpace;
end;

function TmwBasePasLex.FirstInLine: Boolean;
var
  RunBack: Integer;
begin
  Result := True;
  if fTokenPos = 0 then exit;
  RunBack := fTokenPos;
  dec(RunBack);
  while fOrigin[RunBack] in [#1..#9, #11, #12, #14..#32] do
    dec(RunBack);
  if RunBack = 0 then exit;
  case fOrigin[RunBack] of
    #10, #13: exit;
  else
    begin
      Result := False;
      exit;
    end;
  end;
end;

function TmwBasePasLex.GetCommentState: Pointer;
begin
  Result := Pointer(fCommentState);
end;

function TmwBasePasLex.GetCompilerDirective: String;
var
  DirectLen: Integer;
begin
  if TokenID <> TptTokenKind.ptCompDirect then
    Result := ''
  else
    case fOrigin[fTokenPos] of
      '(':
        begin
          DirectLen := Run - fTokenPos - 4;
          SetString(Result, (fOrigin + fTokenPos + 2), DirectLen);
          Result := UpperCase(Result);
        end;
      '{':
        begin
          DirectLen := Run - fTokenPos - 2;
          SetString(Result, (fOrigin + fTokenPos + 1), DirectLen);
          Result := UpperCase(Result);
        end;
    end;
end;

function TmwBasePasLex.GetDirectiveKind: TptTokenKind;
var
  TempPos: Integer;
begin
  case fOrigin[fTokenPos] of
    '(': Run := fTokenPos + 3;
    '{': Run := fTokenPos + 2;
  end;
  fDirectiveParamOrigin := fOrigin + fTokenPos;
  TempPos := fTokenPos;
  fTokenPos := Run;
  case KeyHash of
    9:
      if KeyComp('I') then
        Result := TptTokenKind.ptIncludeDirect else
        Result := TptTokenKind.ptCompDirect;
    15:
      if KeyComp('IF') then
        Result := TptTokenKind.ptIfDirect else
        Result := TptTokenKind.ptCompDirect;
    18:
      if KeyComp('R') then
      begin
        if not (fOrigin[Run] in ['+', '-']) then
          Result := TptTokenKind.ptResourceDirect else Result := TptTokenKind.ptCompDirect;
      end else Result := TptTokenKind.ptCompDirect;
    30:
      if KeyComp('IFDEF') then
        Result := TptTokenKind.ptIfDefDirect else
        Result := TptTokenKind.ptCompDirect;
    38:
      if KeyComp('ENDIF') then
        Result := TptTokenKind.ptEndIfDirect else
      if KeyComp('IFEND') then
        Result := TptTokenKind.ptIfEndDirect else
        Result := TptTokenKind.ptCompDirect;
    41:
      if KeyComp('ELSE') then
        Result := TptTokenKind.ptElseDirect else
        Result := TptTokenKind.ptCompDirect;
    43:
      if KeyComp('DEFINE') then
        Result := TptTokenKind.ptDefineDirect else
        Result := TptTokenKind.ptCompDirect;
    44:
      if KeyComp('IFNDEF') then
        Result := TptTokenKind.ptIfNDefDirect else
        Result := TptTokenKind.ptCompDirect;
    50:
      if KeyComp('UNDEF') then
        Result := TptTokenKind.ptUndefDirect else
        Result := TptTokenKind.ptCompDirect;
    56:
      if KeyComp('ELSEIF') then
        Result := TptTokenKind.ptElseIfDirect else
        Result := TptTokenKind.ptCompDirect;
    66:
      if KeyComp('IFOPT') then
        Result := TptTokenKind.ptIfOptDirect else
        Result := TptTokenKind.ptCompDirect;
    68:
      if KeyComp('INCLUDE') then
        Result := TptTokenKind.ptIncludeDirect else
        Result := TptTokenKind.ptCompDirect;
    104:
      if KeyComp('Resource') then
        Result := TptTokenKind.ptResourceDirect else
        Result := TptTokenKind.ptCompDirect;
  else Result := TptTokenKind.ptCompDirect;
  end;
  fTokenPos := TempPos;
  dec(Run);
end;

function TmwBasePasLex.GetDirectiveParam: String;
var
  EndPos: Integer;
  ParamLen: Integer;
begin
  // !! without this set... there is a warning?
  EndPos:= 0;
  case fOrigin[fTokenPos] of
    '(':
      begin
        TempRun := fTokenPos + 3;
        EndPos := Run - 2;
      end;
    '{':
      begin
        TempRun := fTokenPos + 2;
        EndPos := Run - 1;
      end;
  end;
  while IsIdentifiers(fOrigin[TempRun]) do
    inc(TempRun);
  while fOrigin[TempRun] in ['+', ',', '-'] do
  begin
    inc(TempRun);
    while IsIdentifiers(fOrigin[TempRun]) do
      inc(TempRun);
    if (fOrigin[TempRun - 1] in ['+', ',', '-']) and (fOrigin[TempRun] = ' ')
      then inc(TempRun);
  end;
  if fOrigin[TempRun] = ' ' then inc(TempRun);
  ParamLen := EndPos - TempRun;
  SetString(Result, (fOrigin + TempRun), ParamLen);
  Result := UpperCase(Result);
end;

procedure TmwBasePasLex.Init;
begin
  fCommentState := TCommentState.csNo;
  fLineNumber := 0;
  fLinePos := 0;
  Run := 0;
  InitDefines;
end;

procedure TmwBasePasLex.InitFrom(ALexer: TmwBasePasLex);
begin
  Origin := ALexer.Origin;
  fCommentState := ALexer.fCommentState;
  fLineNumber := ALexer.fLineNumber;
  fLinePos := ALexer.fLinePos;
  Run := ALexer.Run;
  CloneDefinesFrom(ALexer);
end;

procedure TmwBasePasLex.InitDefines;
begin
  ClearDefines;
  //Set up the defines that are defined by the compiler
  {$IFDEF VER130}
  AddDefine('VER130');
  {$ENDIF}
  {$IFDEF VER140}
  AddDefine('VER140');
  {$ENDIF}
  {$IFDEF VER150}
  AddDefine('VER150');
  {$ENDIF}
  {$IFDEF VER160}
  AddDefine('VER160');
  {$ENDIF}
  {$IFDEF VER170}
  AddDefine('VER170');
  {$ENDIF}
  {$IFDEF VER180}
  AddDefine('VER180');
  {$ENDIF}
  {$IFDEF VER185}
  AddDefine('VER185');
  {$ENDIF}
  {$IFDEF VER190}
  AddDefine('VER190');
  {$ENDIF}
  {$IFDEF VER200}
  AddDefine('VER200');
  {$ENDIF}
  {$IFDEF WIN32}
  AddDefine('WIN32');
  {$ENDIF}
  {$IFDEF LINUX}
  AddDefine('LINUX');
  {$ENDIF}
  {$IFDEF CPU386}
  AddDefine('CPU386');
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  AddDefine('MSWINDOWS');
  {$ENDIF}
  {$IFDEF CONDITIONALEXPRESSIONS}
  AddDefine('CONDITIONALEXPRESSIONS');
  {$ENDIF}
  {$IFDEF UNICODE}
  AddDefine('UNICODE');
  {$ENDIF}
end;

procedure TmwBasePasLex.InitLine;
begin
  fLineNumber := 0;
  fLinePos := 0;
  Run := 0;
end;

{$IFNDEF OXYGENE}
procedure TmwBasePasLex.SetCommentState(const Value: Pointer);
begin
  fCommentState := TCommentState(Value);
end;
{$ENDIF}

procedure TmwBasePasLex.SetLine(const Value: String);
begin
  fOrigin := PChar(Value);
  InitLine;
  Next;
end;

function TmwBasePasLex.GetStringContent: String;
var
  TempString: String;
  sEnd: Integer;
begin
  if TokenID <> TptTokenKind.ptStringConst then
    Result := ''
  else
  begin
    TempString := Token;
    sEnd := length(TempString);
    if TempString[sEnd] <> #39 then inc(sEnd);
    Result := Copy(TempString, 2, sEnd - 2);
    TempString := '';
  end;
end;

function TmwBasePasLex.GetIsOrdIdent: Boolean;
begin
  Result := False;
  if FTokenID = TptTokenKind.ptIdentifier then
    Result := fExID in [TptTokenKind.ptBoolean, TptTokenKind.ptByte, TptTokenKind.ptChar, TptTokenKind.ptDWORD, TptTokenKind.ptInt64, TptTokenKind.ptInteger,
      TptTokenKind.ptLongint, TptTokenKind.ptLongword, TptTokenKind.ptPChar, TptTokenKind.ptShortint, TptTokenKind.ptSmallint, TptTokenKind.ptWideChar, TptTokenKind.ptWord]
end;

function TmwBasePasLex.GetIsOrdinalType: Boolean;
begin
  Result := GetIsOrdIdent or (FTokenID in [TptTokenKind.ptAsciiChar, TptTokenKind.ptIntegerConst]);
end;

function TmwBasePasLex.GetIsRealType: Boolean;
begin
  Result := False;
  if FTokenID = TptTokenKind.ptIdentifier then
    Result := fExID in [TptTokenKind.ptComp, TptTokenKind.ptCurrency, TptTokenKind.ptDouble, TptTokenKind.ptExtended, TptTokenKind.ptReal, TptTokenKind.ptReal48, TptTokenKind.ptSingle]
end;

function TmwBasePasLex.GetIsStringType: Boolean;
begin
  Result := False;
  if FTokenID = TptTokenKind.ptIdentifier then
    Result := fExID in [TptTokenKind.ptAnsiString, TptTokenKind.ptWideString]
  else
    if FTokenID = TptTokenKind.ptString then
      Result := True
    else
      if FTokenID = TptTokenKind.ptStringConst then Result := True;
end;

function TmwBasePasLex.GetIsVarantType: Boolean;
begin
  Result := False;
  if FTokenID = TptTokenKind.ptIdentifier then
    Result := fExID in [TptTokenKind.ptOleVariant, TptTokenKind.ptVariant]
end;

function TmwBasePasLex.GetLine: String;
var i: Integer;
begin
  //fOrigin[LinePos]
  i := LinePos;
  while (fOrigin[i] > #0) and not CharInSet(fOrigin[i], [#10,#13]) do
    inc(i);
  Result := Copy(fOrigin, LinePos+1, i-LinePos);
end;

function TmwBasePasLex.GetIsAddOperator: Boolean;
begin
  Result := FTokenID in [TptTokenKind.ptMinus, TptTokenKind.ptOr, TptTokenKind.ptPlus, TptTokenKind.ptXor];
end;

function TmwBasePasLex.GetIsMulOperator: Boolean;
begin
  Result := FTokenID in [TptTokenKind.ptAnd, TptTokenKind.ptAs, TptTokenKind.ptDiv, TptTokenKind.ptMod, TptTokenKind.ptShl, TptTokenKind.ptShr, TptTokenKind.ptSlash, TptTokenKind.ptStar];
end;

function TmwBasePasLex.GetIsRelativeOperator: Boolean;
begin
  Result := FTokenID in [TptTokenKind.ptAs, TptTokenKind.ptEqual, TptTokenKind.ptGreater, TptTokenKind.ptGreaterEqual, TptTokenKind.ptLower, TptTokenKind.ptLowerEqual,
    TptTokenKind.ptIn, TptTokenKind.ptIs, TptTokenKind.ptNotEqual];
end;

function TmwBasePasLex.GetIsCompilerDirective: Boolean;
begin
  Result := FTokenID in [TptTokenKind.ptCompDirect, TptTokenKind.ptDefineDirect, TptTokenKind.ptElseDirect,
    TptTokenKind.ptEndIfDirect, TptTokenKind.ptIfDefDirect, TptTokenKind.ptIfNDefDirect, TptTokenKind.ptIfOptDirect,
    TptTokenKind.ptIncludeDirect, TptTokenKind.ptResourceDirect, TptTokenKind.ptUndefDirect];
end;

function TmwBasePasLex.GetGenID: TptTokenKind;
begin
  Result := FTokenID;
  if FTokenID = TptTokenKind.ptIdentifier then
    if fExID <> TptTokenKind.ptUnknown then Result := fExID;
end;

{ TmwPasLex }

constructor TmwPasLex.Create;
begin
  inherited Create;
  fAheadLex := TmwBasePasLex.Create;
end;

{$IFDEF OXYGENE}
finalizer TmwPasLex;
begin
  inherited;
end;
{$ELSEIF}
destructor TmwPasLex.Destroy;
begin
  fAheadLex.Free;
  inherited Destroy;
end;
{$ENDIF}


procedure TmwPasLex.SetOrigin(NewValue: PChar);
begin
  inherited SetOrigin(NewValue);
  fAheadLex.SetOrigin(NewValue);
end;

procedure TmwPasLex.SetLine(const Value: String);
begin
  inherited SetLine(Value);
  fAheadLex.SetLine(Value);
end;

procedure TmwPasLex.AheadNext;
begin
  fAheadLex.NextNoJunk;
end;

function TmwPasLex.GetAheadExID: TptTokenKind;
begin
  Result := fAheadLex.ExID;
end;

function TmwPasLex.GetAheadGenID: TptTokenKind;
begin
  Result := fAheadLex.GenID;
end;

function TmwPasLex.GetAheadToken: String;
begin
  Result := fAheadLex.Token;
end;

function TmwPasLex.GetAheadTokenID: TptTokenKind;
begin
  Result := fAheadLex.TokenID;
end;

procedure TmwPasLex.InitAhead;
begin
  fAheadLex.CommentState := CommentState;
  fAheadLex.RunPos := RunPos;
  fAheadLex.fLineNumber := fLineNumber;
  fAheadLex.fLinePos := fLinePos;

  fAheadLex.CloneDefinesFrom(Self);

  //FAheadLex.FTokenPos := FTokenPos;
  while fAheadLex.IsJunk do
    fAheadLex.Next;
end;

function TmwPasLex.GetStatus: TmwPasLexStatus;
begin
  Result.CommentState := fCommentState;
  Result.ExID := fExID;
  Result.LineNumber := fLineNumber;
  Result.LinePos := fLinePos;
  Result.Origin := fOrigin;
  Result.RunPos := Run;
  Result.TokenPos := fTokenPos;
  Result.TokenID := FTokenID;
end;

procedure TmwPasLex.SetStatus(const Value: TmwPasLexStatus);
begin
  fCommentState := Value.CommentState;
  fExID := Value.ExID;
  fLineNumber := Value.LineNumber;
  fLinePos := Value.LinePos;
  fOrigin := Value.Origin;
  Run := Value.RunPos;
  fTokenPos := Value.TokenPos;
  FTokenID := Value.TokenID;
  fAheadLex.Origin := Value.Origin;
end;

procedure TmwBasePasLex.SetOnCompDirect(const Value: TDirectiveEvent);
begin
  fOnCompDirect := Value;
end;

procedure TmwBasePasLex.SetOnDefineDirect(const Value: TDirectiveEvent);
begin
  fOnDefineDirect := Value;
end;

procedure TmwBasePasLex.SetOnElseDirect(const Value: TDirectiveEvent);
begin
  fOnElseDirect := Value;
end;

procedure TmwBasePasLex.SetOnElseIfDirect(const Value: TDirectiveEvent);
begin
  fOnElseIfDirect := Value;
end;

procedure TmwBasePasLex.SetOnEndIfDirect(const Value: TDirectiveEvent);
begin
  fOnEndIfDirect := Value;
end;

procedure TmwBasePasLex.SetOnIfDefDirect(const Value: TDirectiveEvent);
begin
  fOnIfDefDirect := Value;
end;

procedure TmwBasePasLex.SetOnIfDirect(const Value: TDirectiveEvent);
begin
  fOnIfDirect := Value;
end;

procedure TmwBasePasLex.SetOnIfEndDirect(const Value: TDirectiveEvent);
begin
  fOnIfEndDirect := Value;
end;

procedure TmwBasePasLex.SetOnIfNDefDirect(const Value: TDirectiveEvent);
begin
  fOnIfNDefDirect := Value;
end;

procedure TmwBasePasLex.SetOnIfOptDirect(const Value: TDirectiveEvent);
begin
  fOnIfOptDirect := Value;
end;

procedure TmwBasePasLex.SetOnIncludeDirect(const Value: TDirectiveEvent);
begin
  fOnIncludeDirect := Value;
end;

procedure TmwBasePasLex.SetOnResourceDirect(const Value: TDirectiveEvent);
begin
  fOnResourceDirect := Value;
end;

procedure TmwBasePasLex.SetOnUnDefDirect(const Value: TDirectiveEvent);
begin
  fOnUnDefDirect := Value;
end;

procedure TmwPasLex.SetOnCompDirect(const Value: TDirectiveEvent);
begin
  inherited;
  //AheadLex.OnCompDirect := Value;
end;

procedure TmwPasLex.SetOnDefineDirect(const Value: TDirectiveEvent);
begin
  inherited;
  //AheadLex.OnDefineDirect := Value;
end;

procedure TmwPasLex.SetOnElseDirect(const Value: TDirectiveEvent);
begin
  inherited;
  //AheadLex.OnElseDirect := Value;
end;

procedure TmwPasLex.SetOnEndIfDirect(const Value: TDirectiveEvent);
begin
  inherited;
  //AheadLex.OnEndIfDirect := Value;
end;

procedure TmwPasLex.SetOnIfDefDirect(const Value: TDirectiveEvent);
begin
  inherited;
  //AheadLex.OnIfDefDirect := Value;
end;

procedure TmwPasLex.SetOnIfNDefDirect(const Value: TDirectiveEvent);
begin
  inherited;
  //AheadLex.OnIfNDefDirect := Value;
end;

procedure TmwPasLex.SetOnIfOptDirect(const Value: TDirectiveEvent);
begin
  inherited;
  //AheadLex.OnIfOptDirect := Value;
end;

procedure TmwPasLex.SetOnIncludeDirect(const Value: TDirectiveEvent);
begin
  inherited;
  //AheadLex.OnIncludeDirect := Value;
end;

procedure TmwPasLex.SetOnResourceDirect(const Value: TDirectiveEvent);
begin
  inherited;
  //AheadLex.OnResourceDirect := Value;
end;

procedure TmwPasLex.SetOnUnDefDirect(const Value: TDirectiveEvent);
begin
  inherited;
  //AheadLex.OnUnDefDirect := Value;
end;

function TmwBasePasLex.Func86: TptTokenKind;
begin
  Result := TptTokenKind.ptIdentifier;
  if KeyComp('Varargs') then fExID := TptTokenKind.ptVarargs;
end;

procedure TmwBasePasLex.StringDQProc;
begin
	if not fAsmCode then
	begin
		SymbolProc;
		Exit;
	end;
  FTokenID := TptTokenKind.ptStringDQConst;
  repeat
	inc(Run);
	case fOrigin[Run] of
	  #0, #10, #13:
		begin
		  if assigned(FOnMessage) then
			FOnMessage(Self, TMessageEventType.meError, 'Unterminated string', PosXY.X, PosXY.Y);
		  break;
		end;
	  '\':
		begin
		  inc( Run );
		  if fOrigin[Run] in [#32..#255] then inc( Run );
		end;
	end;
  until fOrigin[Run] = '"';
  if fOrigin[Run] = '"' then
	inc(Run);
end;

{$IFDEF D8_NEWER} //JThurman 2004-04-06
procedure TmwBasePasLex.AmpersandOpProc;
begin
  FTokenID := TptTokenKind.ptAmpersand;
  inc(Run);
  while fOrigin[Run] in ['a'..'z', 'A'..'Z','0'..'9'] do
    inc(Run);
  FTokenID := TptTokenKind.ptIdentifier;
end;
{$ENDIF}
{$IFNDEF OXYGENE}
initialization
  MakeIdentTable;
{$ENDIF}

end.

