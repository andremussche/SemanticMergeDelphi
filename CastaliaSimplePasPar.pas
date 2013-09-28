{---------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License Version
1.1 (the "License"); you may not use this file except in compliance with the
License. You may obtain a copy of the License at
http://www.mozilla.org/NPL/NPL-1_1Final.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: mwSimplePasPar.pas, released November 14, 1999.

The Initial Developer of the Original Code is Martin Waldenburg
(Martin.Waldenburg@T-Online.de).
Portions created by Martin Waldenburg are Copyright (C) 1998, 1999 Martin
Waldenburg.
All Rights Reserved.
Portions CopyRight by Robert Zierer.

Contributor(s): James Jacobson, Dean Hill, Vladimir Churbanov___________________.

Last Modified: 2002/01/16
Current Version: 1.02

Notes: This program is an early beginning of a Pascal parser.
I'd like to invite the Delphi community to develop it further and to create
a fully featured Object Pascal parser.

Modification history:

Jacob Thurman between 20040301 and 20020401

Made ready for Delphi 8:

Added new directives and keywords: static, sealed, final, operator, unsafe.

Added parsing for custom attributes (based on ECMA C# specification).

Added support for nested types in class declarations.

Jeff Rafter between 20020116 and 20020302

Added AncestorId and AncestorIdList back in, but now treat them as Qualified
Identifiers per Daniel Rolf's fix. The separation from QualifiedIdentifierList
is need for descendent classes.

Added VarName and VarNameList back in for descendent classes, fixed to correctly
use Identifiers as in Daniel's verison

Removed fInJunk flags (they were never used, only set)

Pruned uses clause to remove windows dependency. This required changing
"TPoint" to "TTokenPoint". TTokenPoint was declared in mwPasLexTypes

Daniel Rolf between 20010723 and 20020116

Made ready for Delphi 6

ciClassClass for "class function" etc.
ciClassTypeEnd marks end of a class declaration (I needed that for the delphi-objectif-connector)
ciEnumeratedTypeItem for items of enumerations
ciDirectiveXXX for the platform, deprecated, varargs, local
ciForwardDeclaration for "forward" (until now it has been read but no event)
ciIndexSpecifier for properties
ciObjectTypeEnd marks end of an object declaration
ciObjectProperty property for objects
ciObjectPropertySpecifiers property for objects
ciPropertyDefault marking default of property
ciDispIDSpecifier for dispid

patched some functions for implementing the above things and patching the following bugs/improv.:

ObjectProperty handling overriden properties
ProgramFile, UnitFile getting Identifier instead of dropping it
InterfaceHeritage: Qualified identifiers
bugs in variant records
typedconstant failed with complex set constants. simple patch using ConstantExpression

German localization for the two string constants. Define GERMAN for german string constants.

Greg Chapman on 20010522
Better handling of defaut array property
Separate handling of X and Y in property Pixels[X, Y: Integer through identifier "event"
corrected spelling of "ForwardDeclaration"

James Jacobson on 20010223
semi colon before finalization fix

James Jacobson on 20010223
RecordConstant Fix

Martin waldenburg on 2000107
Even Faster lexer implementation !!!!

James Jacobson on 20010107
  Improper handling of the construct
      property TheName: Integer read FTheRecord.One.Two; (stop at second point)
      where one and two are "qualifiable" structures.

James Jacobson on 20001221
   Stops at the second const.
   property Anchor[const Section: string; const Ident:string]: string read
   changed TmwSimplePasPar.PropertyParameterList

On behalf of  Martin Waldenburg and James Jacobson
 Correction in array property Handling (Matin and James) 07/12/2000
 Use of ExId instead of TokenId in ExportsElements (James) 07/12/2000
 Reverting to old behavior in Statementlist [PtintegerConst put back in] (James) 07/12/2000

Xavier Masson InnerCircleProject : XM : 08/11/2000
  Integration of the new version delivered by Martin Waldenburg with the modification I made described just below

Xavier Masson InnerCircleProject : XM : 07/15/2000
  Added "states/events " for      spaces( SkipSpace;) CRLFco (SkipCRLFco) and
    CRLF (SkipCRLF) this way the parser can give a complete view on code allowing
    "perfect" code reconstruction.
    (I fully now that this is not what a standard parser will do but I think it is more usefull this way ;) )
    go to www.innercircleproject.com for more explanations or express your critisism ;)

previous modifications not logged sorry ;)

Known Issues:
-----------------------------------------------------------------------------}
{----------------------------------------------------------------------------
 Last Modified: 05/22/2001
 Current Version: 1.1
 official version
   Maintained by InnerCircle

   http://www.innercircleproject.org

 02/07/2001
   added property handling in Object types
   changed handling of forward declarations in ExportedHeading method
-----------------------------------------------------------------------------}
unit CastaliaSimplePasPar;

interface

uses
  {$IFNDEF OXYGENE}
  SysUtils,
  Classes,
  {$ENDIF}
  CastaliaPasLexTypes,
  CastaliaPasLex,
  CastaliaSimplePasParTypes;
{$IFNDEF OXYGENE}
{$INCLUDE CastaliaParserDefines.inc}
{$ENDIF}

{$IFDEF GERMAN} // DR 2002-01-16
resourcestring
  rsExpected = '''%s'' erwartet, aber ''%s'' gefunden';
  rsEndOfFile = 'Dateiende';
{$ELSE}
  {$IFNDEF OXYGENE}
  resourcestring
  rsExpected = '''%s'' expected found ''%s''';
  rsEndOfFile = 'end of file';
  {$ENDIF}
{$ENDIF}

const

 ClassMethodDirectiveEnum{$IFDEF OXYGENE}: set of TptTokenKind{$ENDIF} = [TptTokenKind.ptAbstract, TptTokenKind.ptCdecl, TptTokenKind.ptDynamic, TptTokenKind.ptMessage, TptTokenKind.ptOverride,
    TptTokenKind.ptOverload, TptTokenKind.ptPascal, TptTokenKind.ptRegister, TptTokenKind.ptReintroduce, TptTokenKind.ptSafeCall, TptTokenKind.ptStdcall,
    TptTokenKind.ptVirtual,
    TptTokenKind.ptDeprecated, TptTokenKind.ptLibrary, TptTokenKind.ptPlatform // DR 2001-10-20
    {$IFDEF D8_NEWER}
    , TptTokenKind.ptStatic //JThurman 2004-11-10
    {$ENDIF}
    {$IFDEF D9_NEWER}
    , TptTokenKind.ptInline
    {$ENDIF}
    ];  //XM 2002-01-29

type
  ESyntaxError = class(Exception)
  private //jdj 7/18/1999
    FPosXY: TTokenPoint;
  protected

  public
    {$IFDEF OXYGENE}
    constructor Create(const Msg: String);
    constructor Create(const Msg: String; const Args: array of Object);
    constructor Create(const Msg: String; aPosXY: TTokenPoint);
    {$ELSE}
    constructor Create(const Msg: String);
    constructor CreateFmt(const Msg: string; const Args: array of const);
    constructor CreatePos(const Msg: string; aPosXY: TTokenPoint);
    {$ENDIF}
    property PosXY: TTokenPoint read FPosXY write FPosXY;
  end;

  PDefineRec = ^TDefineRec;
  TDefineRec = record
    Defined: Boolean;
    StartCount: Integer;
    Next: PDefineRec;
  end;

type
  TmwSimplePasPar = class(TObject)
  private
    FOnMessage: TMessageEvent;
    fLexer: TmwPasLex;
    fOwnStream: Boolean;
    fStream: TCustomMemoryStream;
    fInterfaceOnly: Boolean;
    fLastNoJunkPos: Integer;
    fLastNoJunkLen: Integer;

    FUseDefines: Boolean;
    FDefines: TStrings;

    AheadParse: TmwSimplePasPar;


    FTopDefineRec: PDefineRec;
    procedure EnterDefineBlock(ADefined: Boolean);{$IFDEF OXYGENE} unsafe;{$ENDIF}
    procedure ExitDefineBlock;
    procedure ClearDefines;

    procedure InitAhead;

  protected
    FDefineStack: Integer;
    fInRound: Boolean;
// !! removed fInJunk
    procedure Expected(Sym: TptTokenKind); virtual;
    procedure ExpectedEx(Sym: TptTokenKind); virtual;
    procedure ExpectedFatal(Sym: TptTokenKind); virtual;
    procedure HandlePtCompDirect(Sender: TmwBasePasLex); virtual;
    procedure HandlePtDefineDirect(Sender: TmwBasePasLex); virtual;
    procedure HandlePtElseDirect(Sender: TmwBasePasLex); virtual;
    procedure HandlePtEndIfDirect(Sender: TmwBasePasLex); virtual;
    procedure HandlePtIfDefDirect(Sender: TmwBasePasLex); virtual;
    procedure HandlePtIfNDefDirect(Sender: TmwBasePasLex); virtual;
    procedure HandlePtIfOptDirect(Sender: TmwBasePasLex); virtual;
    procedure HandlePtIncludeDirect(Sender: TmwBasePasLex); virtual;
    procedure HandlePtResourceDirect(Sender: TmwBasePasLex); virtual;
    procedure HandlePtUndefDirect(Sender: TmwBasePasLex); virtual;
    procedure HandlePtIfDirect(Sender: TmwBasePasLex); virtual;
    procedure HandlePtIfEndDirect(Sender: TmwBasePasLex); virtual;
    procedure HandlePtElseIfDirect(Sender: TmwBasePasLex); virtual;
    procedure NextToken; virtual;
    procedure SkipJunk; virtual;
    procedure TerminateStream; virtual;
    procedure SEMICOLON; virtual;
    function GetExID: TptTokenKind; virtual;
    function GetTokenID: TptTokenKind; virtual;
    function GetGenID: TptTokenKind; virtual;
    procedure AccessSpecifier; virtual;
    procedure AdditiveOperator; virtual;
    procedure AncestorIdList; virtual; // !! Added ancestorIdList back in...
    procedure AncestorId; virtual; // !! Added ancestorId back in...
    procedure AnonymousMethod; virtual;
    procedure AnonymousMethodType; virtual;
    procedure ArrayConstant; virtual;
    procedure ArrayType; virtual;
    procedure AsmStatement; virtual;
    procedure Block; virtual;
    procedure CaseLabel; virtual;
    procedure CaseSelector; virtual;
    procedure CaseStatement; virtual;
    procedure CharString; virtual;
    procedure ClassField; virtual;
    procedure ClassForward; virtual;
    procedure ClassFunctionHeading; virtual;
    procedure ClassHeritage; virtual;
    procedure ClassMemberList; virtual;
    procedure ClassMethodDirective; virtual;
    procedure ClassMethodHeading; virtual;
    procedure ClassMethodOrProperty; virtual;
    procedure ClassMethodResolution; virtual;
    procedure ClassProcedureHeading; virtual;
    procedure ClassClass; virtual;
    procedure ClassProperty; virtual;
    procedure ClassReferenceType; virtual;
    procedure ClassType; virtual;
    procedure ClassTypeEnd; virtual; // DR 2001-07-31
    procedure ClassVisibility; virtual;
    procedure CompoundStatement; virtual;
    procedure ConstantColon; virtual;
    procedure ConstantDeclaration; virtual;
    procedure ConstantEqual; virtual;
    procedure ConstantExpression; virtual;
    procedure ConstantName; virtual;
//JR added constant type
    procedure ConstantType; virtual;
    procedure ConstantValue; virtual;
    procedure ConstantValueTyped; virtual;
    procedure ConstParameter; virtual;
    procedure ConstructorHeading; virtual;
    procedure ConstructorName; virtual;
    procedure ConstSection; virtual;
    procedure ContainsClause; virtual;
    procedure ContainsExpression; virtual;
    procedure ContainsIdentifier; virtual;
    procedure ContainsStatement; virtual;
    {$IFDEF D8_NEWER}
    procedure CustomAttribute; virtual; //JThurman 2004-03-03
    {$ENDIF}
    procedure DeclarationSection; virtual;
    procedure Designator; virtual;
    procedure DestructorHeading; virtual;
    procedure DestructorName; virtual;
    procedure Directive16Bit; virtual;
    procedure DirectiveBinding; virtual;
    procedure DirectiveCalling; virtual;
    procedure DirectiveDeprecated; virtual; // DR 2001-10-20
    procedure DirectiveLibrary; virtual; // DR 2001-10-20
    procedure DirectiveLocal; virtual; // DR 2001-11-14
    procedure DirectivePlatform; virtual; // DR 2001-10-20
    procedure DirectiveVarargs; virtual; // DR 2001-11-14
    procedure DispInterfaceForward; virtual;
    procedure DispIDSpecifier; virtual; // DR 2001-07-26
    procedure EmptyStatement; virtual;
    procedure EnumeratedType; virtual;
    procedure EnumeratedTypeItem; virtual; // DR 2001-10-29
    procedure ExceptBlock; virtual;
    procedure ExceptionBlockElseBranch; virtual;
    procedure ExceptionClassTypeIdentifier; virtual;
    procedure ExceptionHandler; virtual;
    procedure ExceptionHandlerList; virtual;
    procedure ExceptionIdentifier; virtual;
    procedure ExceptionVariable; virtual;
    procedure ExplicitType; virtual; // !! changed spelling to "Explicit"
    procedure ExportedHeading; virtual;
    procedure ExportsClause; virtual;
    procedure ExportsElement; virtual;
    procedure Expression; virtual;
    procedure ExpressionList; virtual;
    procedure ExternalDirective; virtual;
    procedure ExternalDirectiveThree; virtual;
    procedure ExternalDirectiveTwo; virtual;
    procedure Factor; virtual;
    procedure FieldDeclaration; virtual;
    procedure FieldList; virtual;
    procedure FieldNameList; virtual;
    procedure FieldName; virtual;
    procedure FileType; virtual;
    procedure FormalParameterList; virtual;
    procedure FormalParameterSection; virtual;
    procedure ForStatement; virtual;
    procedure ForwardDeclaration; virtual; {GLC: corrected spelling}
    procedure FunctionHeading; virtual;
    procedure FunctionMethodDeclaration; virtual;
    procedure FunctionMethodName; virtual;
    procedure FunctionProcedureBlock; virtual;
    procedure FunctionProcedureName; virtual;
    procedure Identifier; virtual;
    procedure IdentifierList; virtual;
    procedure IfStatement; virtual;
    procedure ImplementationSection; virtual;
    procedure IncludeFile; virtual;
    procedure IndexSpecifier; virtual; // DR 2001-07-26
    procedure InheritedStatement; virtual;
    procedure InitializationSection; virtual;
    procedure InlineStatement; virtual;
    procedure InParameter; virtual;
    procedure InterfaceDeclaration; virtual;
    procedure InterfaceForward; virtual;
    procedure InterfaceGUID; virtual;
    procedure InterfaceHeritage; virtual;
    procedure InterfaceMemberList; virtual;
    procedure InterfaceSection; virtual;
    procedure InterfaceType; virtual;
    procedure LabelDeclarationSection; virtual;
    procedure LabeledStatement; virtual;
    procedure LabelId; virtual;
    procedure LibraryFile; virtual;
    procedure MainUsedUnitExpression; virtual;
    procedure MainUsedUnitName; virtual;
    procedure MainUsedUnitStatement; virtual;
    procedure MainUsesClause; virtual;
    procedure MultiplicativeOperator; virtual;
    procedure NewFormalParameterType; virtual;
    procedure Number; virtual;
    procedure ObjectConstructorHeading; virtual;
    procedure ObjectDestructorHeading; virtual;
    procedure ObjectField; virtual;
    procedure ObjectForward; virtual;
    procedure ObjectFunctionHeading; virtual;
    procedure ObjectHeritage; virtual;
    procedure ObjectMemberList; virtual;
    procedure ObjectMethodDirective; virtual;
    procedure ObjectMethodHeading; virtual;
    procedure ObjectNameOfMethod; virtual;
    procedure ObjectProperty; virtual;
    procedure ObjectPropertySpecifiers; virtual;
    procedure ObjectProcedureHeading; virtual;
    procedure ObjectType; virtual;
    procedure ObjectTypeEnd; virtual; // DR 2001-08-07
    procedure ObjectVisibility; virtual;
    procedure OldFormalParameterType; virtual;
    procedure OrdinalIdentifier; virtual;
    procedure OrdinalType; virtual;
    procedure OutParameter; virtual;
    procedure PackageFile; virtual;
    procedure ParameterFormal; virtual;
    procedure ParameterName; virtual;
    procedure ParameterNameList; virtual;
    procedure ParseFile; virtual;
    procedure PointerType; virtual;
    procedure ProceduralDirective; virtual;
    procedure ProceduralType; virtual;
    procedure ProcedureDeclarationSection; virtual;
    procedure ProcedureHeading; virtual;
    procedure ProcedureMethodDeclaration; virtual;
    procedure ProcedureMethodName; virtual;
    procedure ProgramBlock; virtual;
    procedure ProgramFile; virtual;
    procedure PropertyDefault; virtual;
    procedure PropertyInterface; virtual;
    procedure PropertyName; virtual;
    procedure PropertyParameterConst; virtual;
    procedure PropertyParameterList; virtual;
    procedure PropertySpecifiers; virtual;
    procedure QualifiedIdentifier; virtual;
    procedure QualifiedIdentifierList; virtual;
    procedure RaiseStatement; virtual;
    procedure ReadAccessIdentifier; virtual;
    procedure RealIdentifier; virtual;
    procedure RealType; virtual;
    procedure RecordConstant; virtual;
    procedure RecordFieldConstant; virtual;
    procedure RecordType; virtual;
    procedure RecordVariant; virtual;
    procedure RelativeOperator; virtual;
    procedure RepeatStatement; virtual;
    procedure RequiresClause; virtual;
    procedure RequiresIdentifier; virtual;
    procedure ResolutionInterfaceName; virtual;
    procedure ResourceDeclaration; virtual;
    procedure ReturnType; virtual;
    procedure SetConstructor; virtual;
    procedure SetElement; virtual;
    procedure SetType; virtual;
    procedure SimpleExpression; virtual;
    procedure SimpleStatement; virtual;
    procedure SimpleType; virtual;
    procedure SkipAnsiComment; virtual;
    procedure SkipBorComment; virtual;
    procedure SkipSlashesComment; virtual;
    procedure SkipSpace; virtual; //XM Jul-2000
    procedure SkipCRLFco; virtual; //XM Jul-2000
    procedure SkipCRLF; virtual; //XM Jul-2000
    procedure Statement; virtual;
    procedure StatementList; virtual;
    procedure StorageExpression; virtual;
    procedure StorageIdentifier; virtual;
    procedure StorageDefault; virtual;
    procedure StorageNoDefault; virtual;
    procedure StorageSpecifier; virtual;
    procedure StorageStored; virtual;
    procedure StringIdentifier; virtual;
    procedure StringStatement; virtual;
    procedure StringType; virtual;
    procedure StructuredType; virtual;
    procedure SubrangeType; virtual;
    procedure TagField; virtual;
    procedure TagFieldName; virtual;
    procedure TagFieldTypeName; virtual;
    procedure Term; virtual;
    procedure TryStatement; virtual;
    procedure TypedConstant; virtual;
    procedure TypeDeclaration; virtual;
    procedure TypeId; virtual;
    procedure TypeKind; virtual;
    procedure TypeName; virtual;
    //generics
    procedure TypeArgs; virtual;
    procedure TypeParams; virtual;
    procedure TypeParamDecl; virtual;
    procedure TypeParamDeclList; virtual;
    procedure TypeParamList; virtual;
    procedure ConstraintList; virtual;
    procedure Constraint; virtual;
    //end generics
    procedure TypeSection; virtual;
    procedure UnitFile; virtual;
    procedure UnitId; virtual;
    procedure UnitName; virtual;
    procedure UsedUnitName; virtual;
    procedure UsedUnitsList; virtual;
    procedure UsesClause; virtual;
    procedure VarAbsolute; virtual;
    procedure VarEqual; virtual;
    procedure VarDeclaration; virtual;
    procedure Variable; virtual;
    procedure VariableList; virtual;
    procedure VariableReference; virtual;
    procedure VariableTwo; virtual;
    procedure VariantIdentifier; virtual;
    procedure VariantSection; virtual;
    procedure VarParameter; virtual;
    procedure VarName; virtual; //!! Added VarName and VarNameList back in...
    procedure VarNameList; virtual;
    procedure VarSection; virtual;
    procedure VisibilityAutomated; virtual;
    procedure VisibilityPrivate; virtual;
    procedure VisibilityProtected; virtual;
    procedure VisibilityPublic; virtual;
    procedure VisibilityPublished; virtual;
    procedure VisibilityUnknown; virtual;
    procedure WhileStatement; virtual;
    procedure WithStatement; virtual;
    procedure WriteAccessIdentifier; virtual;
    {$IFDEF D8_NEWER}//JThurman 2004-03-21
    {This is the syntax for custom attributes, based quite strictly on the
    ECMA syntax specifications for C#, but with a Delphi expression being
    used at the bottom as opposed to a C# expression}
    procedure GlobalAttributes;
    procedure GlobalAttributeSections;
    procedure GlobalAttributeSection;
    procedure GlobalAttributeTargetSpecifier;
    procedure GlobalAttributeTarget;
    procedure Attributes;
    procedure AttributeSections;
    procedure AttributeSection;
    procedure AttributeTargetSpecifier;
    procedure AttributeTarget;
    procedure AttributeList;
    procedure Attribute;
    procedure AttributeName;
    procedure AttributeArguments;
    procedure PositionalArgumentList;
    procedure PositionalArgument;
    procedure NamedArgumentList;
    procedure NamedArgument;
    procedure AttributeArgumentExpression; 
    {$ENDIF}
    {$IFDEF OXYGENE}
    method AfterConstruction; virtual; empty;
    {$ENDIF}
    property ExID: TptTokenKind read GetExID;
    property GenID: TptTokenKind read GetGenID;
    property TokenID: TptTokenKind read GetTokenID;
  public
    constructor Create;
    {$IFDEF OXYGENE}
    procedure Run(UnitName: String; Source: String); virtual;
    {$ELSE}
    destructor Destroy; override;
    procedure Run(UnitName: String; SourceStream: TCustomMemoryStream); virtual;
    {$ENDIF}
    procedure SynError(Error: TmwParseError); virtual;
    procedure InitDefines;
    procedure AddDefine(const ADefine: String);
    procedure RemoveDefine(const ADefine: String);
    function IsDefined(const ADefine: String): Boolean;

    property InterfaceOnly: Boolean read fInterfaceOnly write fInterfaceOnly;
    property Lexer: TmwPasLex read fLexer;
    property OnMessage: TMessageEvent read FOnMessage write FOnMessage;
    property LastNoJunkPos: Integer read fLastNoJunkPos;
    property LastNoJunkLen: Integer read fLastNoJunkLen;

    property UseDefines: Boolean read FUseDefines write FUseDefines;
  published
  end;

implementation

{$IFDEF OXYGENE}
uses
  System;
{$ELSE}
uses Windows;
{$ENDIF}
{ ESyntaxError }

constructor ESyntaxError.Create(const Msg: String);
begin
  // !! changed initialization for TTokenPoint
  FPosXY.X:= -1;
  FPosXY.Y:= -1;
  inherited Create(Msg);
end;

{$IFDEF OXYGENE}
constructor ESyntaxError.Create(const Msg: String; const Args: array of Object);
{$ELSE}
constructor ESyntaxError.CreateFmt(const Msg: String; const Args: array of const);
{$ENDIF}
begin
  // !! changed initialization for TTokenPoint
  FPosXY.X:= -1;
  FPosXY.Y:= -1;
  {$IFDEF OXYGENE}
  inherited constructor(Msg);
  {$ELSE}
  inherited CreateFmt(Msg, Args);
  {$ENDIF}
end;

{$IFDEF OXYGENE}
constructor ESyntaxError.Create(const Msg: String; aPosXY: TTokenPoint);
begin
  inherited constructor(Msg);
{$ELSE}
constructor ESyntaxError.CreatePos(const Msg: String; aPosXY: TTokenPoint);
begin
  Message := Msg;
{$ENDIF}
  FPosXY := aPosXY;
end;

{ TmwSimplePasPar }
(* DR 2002-01-16
const
  cnExpected = 'Expected ''%s'' found ''%s''';
//  cnOrExpected = 'Expected ''%s'' or ''%s'' found ''%s''';
  cnEndOfFile = 'end of file'; {jdj 7/22/1999}
//  cnIntegerOverflow = 'Integer constant too large'; {jdj 7/22/1999}
*)

 {range checks a ptIntegerConst-slightly faster than StrToInt}
{function IsValidInteger(const S: string): Boolean; jdj 7/22/1999
var jdj removed 02/07/2001
  C: Integer;
  N: Integer;
begin
  Val(S, N, C);
  Result := (C = 0);
end;}

procedure TmwSimplePasPar.ForwardDeclaration;
begin {jdj added method 02/07/2001}
  NextToken;
  SEMICOLON;
end;

procedure TmwSimplePasPar.ObjectProperty;
begin {jdj added method 02/07/2001}
 // DR 2001-08-07 -> changed. for array-property override failure
  Expected(TptTokenKind.ptProperty);
  PropertyName;
  case TokenID of
    TptTokenKind.ptColon, TptTokenKind.ptSquareOpen:
      begin
        PropertyInterface;
      end;
  end;
  ObjectPropertySpecifiers;
  case ExID of
    TptTokenKind.ptDefault:
      begin
        PropertyDefault; //DR 2001-07-16
        SEMICOLON;
      end;
  end;
end;

procedure TmwSimplePasPar.ObjectPropertySpecifiers;
begin {jdj added method 02/07/2001}
  if ExID = TptTokenKind.ptIndex then
  begin
    IndexSpecifier; // DR 2001-08-07
  end;
  while ExID in [TptTokenKind.ptRead, TptTokenKind.ptReadonly, TptTokenKind.ptWrite, TptTokenKind.ptWriteonly] do
  begin
    AccessSpecifier;
  end;
  while ExID in [TptTokenKind.ptDefault, TptTokenKind.ptNodefault, TptTokenKind.ptStored] do
  begin
    StorageSpecifier;
  end;
  SEMICOLON;
end;
{$IFDEF OXYGENE}
procedure TmwSimplePasPar.Run(UnitName: String; Source: String);
begin 
  fLexer.Origin := new PChar(Data := Source);
  ParseFile;
end;
{$ELSE}
procedure TmwSimplePasPar.Run(UnitName: String; SourceStream: TCustomMemoryStream);
begin 
  fStream := nil;
  fOwnStream := False;
  fStream := SourceStream;
  TerminateStream;
  fLexer.Origin := fStream.Memory;
  ParseFile;
  if fOwnStream then fStream.Free;
end;
{$ENDIF} 
constructor TmwSimplePasPar.Create;
begin
  inherited Create;
  fLexer := TmwPasLex.Create;
  {$IFDEF OXYGENE}
  fLexer.OnCompDirect := @HandlePtCompDirect;
  fLexer.OnDefineDirect := @HandlePtDefineDirect;
  fLexer.OnElseDirect := @HandlePtElseDirect;
  fLexer.OnEndIfDirect := @HandlePtEndIfDirect;
  fLexer.OnIfDefDirect := @HandlePtIfDefDirect;
  fLexer.OnIfNDefDirect := @HandlePtIfNDefDirect;
  fLexer.OnIfOptDirect := @HandlePtIfOptDirect;
  fLexer.OnIncludeDirect := @HandlePtIncludeDirect;
  fLexer.OnResourceDirect := @HandlePtResourceDirect;
  fLexer.OnUnDefDirect := @HandlePtUndefDirect;
  fLexer.OnIfDirect := @HandlePtIfDirect;
  fLexer.OnIfEndDirect := @HandlePtIfEndDirect;
  fLexer.OnElseIfDirect := @HandlePtElseIfDirect;
  {$ELSE}
  fLexer.OnCompDirect := HandlePtCompDirect;
  fLexer.OnDefineDirect := HandlePtDefineDirect;
  fLexer.OnElseDirect := HandlePtElseDirect;
  fLexer.OnEndIfDirect := HandlePtEndIfDirect;
  fLexer.OnIfDefDirect := HandlePtIfDefDirect;
  fLexer.OnIfNDefDirect := HandlePtIfNDefDirect;
  fLexer.OnIfOptDirect := HandlePtIfOptDirect;
  fLexer.OnIncludeDirect := HandlePtIncludeDirect;
  fLexer.OnResourceDirect := HandlePtResourceDirect;
  fLexer.OnUnDefDirect := HandlePtUndefDirect;
  fLexer.OnIfDirect := HandlePtIfDirect;
  fLexer.OnIfEndDirect := HandlePtIfEndDirect;
  fLexer.OnElseIfDirect := HandlePtElseIfDirect;
  {$ENDIF}
  FDefines := TStringList.Create;
  {$IFNDEF OXYGENE}
  with TStringList(FDefines) do
  begin
    Sorted := True;
    Duplicates := dupIgnore;
  end;
  {$ENDIF}
  InitDefines;
  FDefineStack := 0;
  FUseDefines := False;
  {$IFDEF OXYGENE}
  AfterConstruction(); 
  {$ENDIF}
end;

{$IFNDEF OXYGENE}
destructor TmwSimplePasPar.Destroy;
begin
  ClearDefines; //Must do this here to avoid a memory leak
  FDefines.Free;

  AheadParse.Free;

  fLexer.Free;
  inherited Destroy;
end;
{$ENDIF}
{next two check for ptNull and ExpectedFatal for an EOF Error}

procedure TmwSimplePasPar.Expected(Sym: TptTokenKind);
begin
  if Sym <> Lexer.TokenID then
  begin
    if TokenID = TptTokenKind.ptNull then
      ExpectedFatal(Sym) {jdj 7/22/1999}
    else
    begin
      {$IFNDEF OXYGENE}
      if assigned(FOnMessage) then
        FOnMessage(Self, TMessageEventType.meError, Format(rsExpected, [TokenName(Sym), fLexer.Token]),
          fLexer.PosXY.X, fLexer.PosXY.Y);
      {$ENDIF}
    end;
  end
  else
    NextToken;
end;

procedure TmwSimplePasPar.ExpectedEx(Sym: TptTokenKind);
begin
  if Sym <> Lexer.ExID then
  begin
    if Lexer.TokenID = TptTokenKind.ptNull then
      ExpectedFatal(Sym) {jdj 7/22/1999}
    {$IFDEF OXYGENE};
    {$ELSE}
    else if assigned(FOnMessage) then
      FOnMessage(Self, TMessageEventType.meError, Format(rsExpected, ['EX:' + TokenName(Sym), fLexer.Token]),
        fLexer.PosXY.X, fLexer.PosXY.Y);{$ENDIF}
  end
  else
    NextToken;
end;

{Replace Token with cnEndOfFile if TokenId = ptnull}

procedure TmwSimplePasPar.ExpectedFatal(Sym: TptTokenKind);
var
  tS: String;
begin
  if Sym <> Lexer.TokenID then
  begin
    {--jdj 7/22/1999--}
    if Lexer.TokenID = TptTokenKind.ptNull then
    {$IFDEF OXYGENE}
      tS := 'End of file'
    else
      tS := fLexer.Token;
    raise ESyntaxError.Create('Unexpected Token', fLexer.PosXY);
    {$ELSE}
      tS := rsEndOfFile
    else
      tS := fLexer.Token;
    {--jdj 7/22/1999--}
    raise ESyntaxError.CreatePos(Format(rsExpected, [TokenName(Sym), tS]), fLexer.PosXY);
    {$ENDIF}  
  end
  else
    NextToken;
end;

procedure TmwSimplePasPar.HandlePtCompDirect(Sender: TmwBasePasLex);
begin
  if assigned(FOnMessage) then
    FOnMessage(Self, TMessageEventType.meNotSupported, 'Currently not supported ' + fLexer.Token, fLexer.PosXY.X, fLexer.PosXY.Y);
  //  Sender.NextNoJunk;
  Sender.Next; //XM Jul-2000
  { ToDo }
end;

procedure TmwSimplePasPar.HandlePtDefineDirect(Sender: TmwBasePasLex);
begin
//  if Assigned(FOnMessage) then
//    FOnMessage(Self, meNotSupported, 'Currently not supported ' + fLexer.Token, fLexer.PosXY.X, fLexer.PosXY.Y);

//  if FUseDefines then
//    AddDefine(Lexer.DirectiveParam);
//  else

  //  Sender.NextNoJunk;
  Sender.Next; //XM Jul-2000

  { ToDo }
end;

procedure TmwSimplePasPar.HandlePtElseDirect(Sender: TmwBasePasLex);
begin
//  if Assigned(FOnMessage) then
//    FOnMessage(Self, meNotSupported, 'Currently not supported ' + fLexer.Token, fLexer.PosXY.X, fLexer.PosXY.Y);

//  if FUseDefines then
//  begin
//    if FTopDefineRec <> nil then
//    begin
//      if FTopDefineRec^.Defined then
//        Inc(FDefineStack)
//      else
//        if FDefineStack > 0 then
//          Dec(FDefineStack);
//    end;
//  end;

  //  Sender.NextNoJunk;
  if Sender = Lexer then
    NextToken
  else
    Sender.Next; //XM Jul-2000

  { ToDo }
end;

procedure TmwSimplePasPar.HandlePtElseIfDirect(Sender: TmwBasePasLex);
var
  Param: String;
  Def: String;
begin
//  if FUseDefines then
//  begin
//    if FTopDefineRec <> nil then
//    begin
//      if FTopDefineRec^.Defined then
//        Inc(FDefineStack)
//      else
//      begin
//        if FDefineStack > 0 then
//          Dec(FDefineStack);
//        Param := Sender.DirectiveParam;
//        if Pos('DEFINED', Param) = 1 then
//        begin
//          Def := Copy(Param, 9, Length(Param) - 9);
//          EnterDefineBlock(IsDefined(Def));
//        end;
//      end;
//    end;
//  end;

  if Sender = Lexer then
    NextToken
  else
    Sender.Next;
end;

procedure TmwSimplePasPar.HandlePtEndIfDirect(Sender: TmwBasePasLex);
begin
//  if Assigned(FOnMessage) then
//    FOnMessage(Self, meNotSupported, 'Currently not supported ' + fLexer.Token, fLexer.PosXY.X, fLexer.PosXY.Y);

//  if FUseDefines then
//  begin
//    ExitDefineBlock;
//  end;

  //  Sender.NextNoJunk;
  if Sender = Lexer then
    NextToken
  else
    Sender.Next; //XM Jul-2000

  { ToDo }
end;

procedure TmwSimplePasPar.HandlePtIfDefDirect(Sender: TmwBasePasLex);
begin
//  if Assigned(FOnMessage) then
//    FOnMessage(Self, meNotSupported, 'Currently not supported ' + fLexer.Token, fLexer.PosXY.X, fLexer.PosXY.Y);
//  if FUseDefines then
//  begin
//    EnterDefineBlock(IsDefined(Sender.DirectiveParam));
//  end;

  //  Sender.NextNoJunk;
  if Sender = Lexer then
    NextToken
  else
    Sender.Next; //XM Jul-2000

  { ToDo }
end;

procedure TmwSimplePasPar.HandlePtIfDirect(Sender: TmwBasePasLex);
var
  Def: String;
  Param: String;
begin
//  Param := Sender.DirectiveParam;
//  if FUseDefines then
//  begin
//    if Pos('DEFINED', Param) = 1 then
//    begin
//      Def := Copy(Param, 9, Length(Param) - 9);
//      EnterDefineBlock(IsDefined(Def));
//    end;
//  end;
  if Sender = Lexer then
    NextToken
  else
    Sender.Next;
end;

procedure TmwSimplePasPar.HandlePtIfEndDirect(Sender: TmwBasePasLex);
begin
//  if FUseDefines then
//    ExitDefineBlock;

  if Sender = Lexer then
    NextToken
  else
    Sender.Next;
end;

procedure TmwSimplePasPar.HandlePtIfNDefDirect(Sender: TmwBasePasLex);
begin
//  if Assigned(FOnMessage) then
//    FOnMessage(Self, meNotSupported, 'Currently not supported ' + fLexer.Token, fLexer.PosXY.X, fLexer.PosXY.Y);
//  if FUseDefines then
//  begin
//    EnterDefineBlock(not IsDefined(Sender.DirectiveParam));
//  end;

  //  Sender.NextNoJunk;
  if Sender = Lexer then
    NextToken
  else
    Sender.Next; //XM Jul-2000

  { ToDo }
end;

procedure TmwSimplePasPar.HandlePtIfOptDirect(Sender: TmwBasePasLex);
begin
  if assigned(FOnMessage) then
    FOnMessage(Self, TMessageEventType.meNotSupported, 'Currently not supported ' + fLexer.Token, fLexer.PosXY.X, fLexer.PosXY.Y);
  //  Sender.NextNoJunk;
  Sender.Next; //XM Jul-2000

  { ToDo }
end;

procedure TmwSimplePasPar.HandlePtIncludeDirect(Sender: TmwBasePasLex);
begin
  if assigned(FOnMessage) then
    FOnMessage(Self, TMessageEventType.meNotSupported, 'Currently not supported ' + fLexer.Token, fLexer.PosXY.X, fLexer.PosXY.Y);
  //  Sender.NextNoJunk;
  Sender.Next; //XM Jul-2000

  { ToDo }
end;

procedure TmwSimplePasPar.HandlePtResourceDirect(Sender: TmwBasePasLex);
begin
  if assigned(FOnMessage) then
    FOnMessage(Self, TMessageEventType.meNotSupported, 'Currently not supported ' + fLexer.Token, fLexer.PosXY.X, fLexer.PosXY.Y);
  //  Sender.NextNoJunk;
  Sender.Next; //XM Jul-2000

  { ToDo }
end;

procedure TmwSimplePasPar.HandlePtUndefDirect(Sender: TmwBasePasLex);
begin
//  if Assigned(FOnMessage) then
//    FOnMessage(Self, meNotSupported, 'Currently not supported ' + fLexer.Token, fLexer.PosXY.X, fLexer.PosXY.Y);

//  if FUseDefines then
//    RemoveDefine(Lexer.DirectiveParam);

  //  Sender.NextNoJunk;
  Sender.Next; //XM Jul-2000

  { ToDo }
end;

procedure TmwSimplePasPar.NextToken;
begin
//  if FUseDefines then
//  begin
//    repeat
//      FLexer.Next;
//    until (FDefineStack = 0) or (TokenID = ptNull);
//    SkipJunk;
//  end else
//  begin
    fLexer.NextNoJunk;
    //fLexer.Next;
    //SkipJunk;
//  end;
end;

procedure TmwSimplePasPar.SkipJunk;
begin
  if Lexer.IsJunk then
  begin
    case TokenID of
      TptTokenKind.ptAnsiComment:
        begin
          SkipAnsiComment;
        end;
      TptTokenKind.ptBorComment:
        begin
          SkipBorComment;
        end;
      TptTokenKind.ptSlashesComment:
        begin
          SkipSlashesComment;
        end;
      TptTokenKind.ptSpace:
        begin
          SkipSpace; //XM Jul-2000
        end;
      TptTokenKind.ptCRLFCo:
        begin
          SkipCRLFco;
        end;
      TptTokenKind.ptCRLF:
        begin
          SkipCRLF;
        end;
      {$IFDEF D8_NEWER} //JThurman 2004-3-19
      TptTokenKind.ptSquareOpen:
        begin
          CustomAttribute;
        end;
      {$ENDIF}
    else
      begin
        Lexer.Next;
      end;
    end;
  end;
  fLastNoJunkPos := Lexer.TokenPos;
  fLastNoJunkLen := Lexer.TokenLen;
end;

procedure TmwSimplePasPar.SkipAnsiComment;
begin
  Expected(TptTokenKind.ptAnsiComment);
  while TokenID in [TptTokenKind.ptAnsiComment] do
    Lexer.Next;
end;

procedure TmwSimplePasPar.SkipBorComment;
begin
  Expected(TptTokenKind.ptBorComment);
  while TokenID in [TptTokenKind.ptBorComment] do
    Lexer.Next;
end;

procedure TmwSimplePasPar.SkipSlashesComment;
begin
  Expected(TptTokenKind.ptSlashesComment);
end;

procedure TmwSimplePasPar.TerminateStream;
var
  aChar: Char;
begin
  fStream.Position := fStream.Size;
  aChar := #0;
  {$IFNDEF OXYGENE}
  fStream.Write(aChar, sizeOf(char));
  {$ENDIF}
end;

procedure TmwSimplePasPar.SEMICOLON;
begin
  case Lexer.TokenID of
    TptTokenKind.ptElse, TptTokenKind.ptEnd, TptTokenKind.ptExcept, TptTokenKind.ptFinally, TptTokenKind.ptFinalization, TptTokenKind.ptRoundClose, TptTokenKind.ptUntil: // jdj 2.23.20001 added ptFinalization
      ;
  else
    Expected(TptTokenKind.ptSemiColon);
    //Check for semicolon before else - common syntax error - JT 11.10.2007
    //Doesn't work here - it fails a CASE statement
//    if Lexer.TokenID = ptElse then
//    begin
//      if Assigned(FOnMessage) then
//      begin
//        FOnMessage(Self, meError, ''';'' not allowed before ''ELSE''',
//          FLexer.PosXY.X, FLexer.PosXY.Y);
//      end;
//    end;
  end;
end;

function TmwSimplePasPar.GetExID: TptTokenKind;
begin
  Result := fLexer.ExID;
end;

function TmwSimplePasPar.GetTokenID: TptTokenKind;
begin
  Result := fLexer.TokenID;
end;

function TmwSimplePasPar.GetGenID: TptTokenKind;
begin
  Result := fLexer.GenID;
end;

procedure TmwSimplePasPar.SynError(Error: TmwParseError);
begin
  if assigned(FOnMessage) then
    FOnMessage(Self, TMessageEventType.meError, ParserErrorName(Error) + ' found ' + fLexer.Token, fLexer.PosXY.X,
      fLexer.PosXY.Y);

end;

(******************************************************************************
 This part is oriented at the official grammar of Delphi 4
 and parialy based on Robert Zierers Delphi grammar.
 For more information about Delphi grammars take a look at:
 http://www.stud.mw.tu-muenchen.de/~rz1/Grammar.html
******************************************************************************)

procedure TmwSimplePasPar.ParseFile;
var
  I: Integer;
begin
//  OutputDebugString('ParseFile');
  //Copy the defines into the lexer
  for I := 0 to FDefines.Count - 1 do
  begin
    Lexer.AddDefine(FDefines[I]);
  end;

  SkipJunk;
  case GenID of
    TptTokenKind.ptLibrary:
      begin
        LibraryFile;
      end;
    TptTokenKind.ptPackage:
      begin
        PackageFile;
      end;
    TptTokenKind.ptProgram:
      begin
        ProgramFile;
      end;
    TptTokenKind.ptUnit:
      begin
        UnitFile;
      end;
  else
    begin
      IncludeFile;
    end;
  end;
end;

procedure TmwSimplePasPar.LibraryFile;
begin
  Expected(TptTokenKind.ptLibrary);
  Expected(TptTokenKind.ptIdentifier);
  SEMICOLON;
  ProgramBlock;
  Expected(TptTokenKind.ptPoint);
end;

procedure TmwSimplePasPar.PackageFile;
begin
  ExpectedEx(TptTokenKind.ptPackage);
  Expected(TptTokenKind.ptIdentifier);

  {$IFDEF D8_NEWER}
  while Lexer.TokenID = TptTokenKind.ptPoint do
  begin
    NextToken;
    Expected(TptTokenKind.ptIdentifier);
  end;
  {$ENDIF}

  SEMICOLON;
  case ExID of
    TptTokenKind.ptRequires:
      begin
        RequiresClause;
      end;
  end;
  case ExID of
    TptTokenKind.ptContains:
      begin
        ContainsClause;
      end;
  end;

  {$IFDEF D8_NEWER}
  while Lexer.TokenID = TptTokenKind.ptSquareOpen do
  begin
    CustomAttribute;
  end;
  {$ENDIF}

  Expected(TptTokenKind.ptEnd);
  Expected(TptTokenKind.ptPoint);
end;

procedure TmwSimplePasPar.ProgramFile;
begin
 // DR 2002-01-11
  Expected(TptTokenKind.ptProgram);
  QualifiedIdentifier;
  if TokenID = TptTokenKind.ptRoundOpen then
  begin
    NextToken;
    IdentifierList;
    Expected(TptTokenKind.ptRoundClose);
  end;
  if not InterfaceOnly then
  begin
    SEMICOLON;
    ProgramBlock;
    Expected(TptTokenKind.ptPoint);
  end;
end;

procedure TmwSimplePasPar.UnitFile;
begin
 // DR 2002-01-11

//??
  Expected(TptTokenKind.ptUnit);
  UnitName;

  while ExID in [TptTokenKind.ptDeprecated, TptTokenKind.ptLibrary, TptTokenKind.ptPlatform] do
    case ExID of
      TptTokenKind.ptDeprecated: DirectiveDeprecated;
      TptTokenKind.ptLibrary: DirectiveLibrary;
      TptTokenKind.ptPlatform: DirectivePlatform;
    end;

  SEMICOLON;
  InterfaceSection;
  if not InterfaceOnly then
  begin
    ImplementationSection;
    InitializationSection;
    Expected(TptTokenKind.ptPoint);
  end;
end;

procedure TmwSimplePasPar.ProgramBlock;
begin
  if TokenID = TptTokenKind.ptUses then
  begin
    MainUsesClause;
  end;
  Block;
end;

procedure TmwSimplePasPar.MainUsesClause;
begin
  Expected(TptTokenKind.ptUses);
  MainUsedUnitStatement;
  while TokenID = TptTokenKind.ptComma do
  begin
    NextToken;
    MainUsedUnitStatement;
  end;
  SEMICOLON;
end;

procedure TmwSimplePasPar.MainUsedUnitStatement;
begin
  MainUsedUnitName;
  if Lexer.TokenID = TptTokenKind.ptIn then
  begin
    NextToken;
    MainUsedUnitExpression;
  end;
end;

procedure TmwSimplePasPar.MainUsedUnitName;
begin
//  Expected(ptIdentifier);
  UsedUnitName; //JThurman 2004-11-10
end;

procedure TmwSimplePasPar.MainUsedUnitExpression;
begin
  ConstantExpression;
end;

procedure TmwSimplePasPar.UsesClause;
begin
  Expected(TptTokenKind.ptUses);
  UsedUnitsList;
  SEMICOLON;
end;

procedure TmwSimplePasPar.UsedUnitsList;
begin
  UsedUnitName;
  while TokenID = TptTokenKind.ptComma do
  begin
    NextToken;
    UsedUnitName;
  end;
end;

procedure TmwSimplePasPar.UsedUnitName;
begin
  Lexer.PreviousIdentifierText := Lexer.Token;
  Expected(TptTokenKind.ptIdentifier); 
  while TokenID = TptTokenKind.ptPoint do
  begin
    NextToken;
    Lexer.PreviousIdentifierText := Lexer.PreviousIdentifierText + '.' + Lexer.Token;
    Expected(TptTokenKind.ptIdentifier); 
  end;
end;

procedure TmwSimplePasPar.Block;
begin
  while TokenID in [TptTokenKind.ptClass, TptTokenKind.ptConst, TptTokenKind.ptConstructor, TptTokenKind.ptDestructor, TptTokenKind.ptExports,
    TptTokenKind.ptFunction, TptTokenKind.ptLabel, TptTokenKind.ptProcedure, TptTokenKind.ptResourcestring, TptTokenKind.ptThreadvar, TptTokenKind.ptType,
    TptTokenKind.ptVar{$IFDEF D8_NEWER}, TptTokenKind.ptSquareOpen{$ENDIF}] do
  begin
    DeclarationSection;
  end;
  case TokenID of
    TptTokenKind.ptAsm:
      begin
        AsmStatement;
      end;
  else
    begin
      CompoundStatement;
    end;
  end;
end;

procedure TmwSimplePasPar.DeclarationSection;
begin
  case TokenID of
    TptTokenKind.ptClass:
      begin
        ProcedureDeclarationSection;
      end;
    TptTokenKind.ptConst:
      begin
        ConstSection;
      end;
    TptTokenKind.ptConstructor:
      begin
        ProcedureDeclarationSection;
      end;
    TptTokenKind.ptDestructor:
      begin
        ProcedureDeclarationSection;
      end;
    TptTokenKind.ptExports:
      begin
        ExportsClause;
      end;
    TptTokenKind.ptFunction:
      begin
        ProcedureDeclarationSection;
      end;
    TptTokenKind.ptLabel:
      begin
        LabelDeclarationSection;
      end;
    TptTokenKind.ptProcedure:
      begin
        ProcedureDeclarationSection;
      end;
    TptTokenKind.ptResourcestring:
      begin
        ConstSection;
      end;
    TptTokenKind.ptType:
      begin
        TypeSection;
      end;
    TptTokenKind.ptThreadvar:
      begin
        VarSection;
      end;
    TptTokenKind.ptVar:
      begin
        VarSection;
      end;
    {$IFDEF D8_NEWER} //JThurman
    TptTokenKind.ptSquareOpen:
      begin
        CustomAttribute;
      end;
    {$ENDIF}
  else
    begin
      SynError(TmwParseError.InvalidDeclarationSection);
    end;
  end;
end;

procedure TmwSimplePasPar.UnitId;
begin
  Expected(TptTokenKind.ptIdentifier);
end;

procedure TmwSimplePasPar.UnitName;
begin
  Lexer.PreviousIdentifierText := Lexer.Token;
  Expected(TptTokenKind.ptIdentifier); 
  while TokenID = TptTokenKind.ptPoint do
  begin
    NextToken;
    Lexer.PreviousIdentifierText := Lexer.PreviousIdentifierText + '.' + Lexer.Token;
    Expected(TptTokenKind.ptIdentifier); 
  end;
end;

procedure TmwSimplePasPar.InterfaceHeritage;
begin
  Expected(TptTokenKind.ptRoundOpen);
  AncestorIdList; // JR moved qualified check into ancestorIdList // DR 2001-11-01 can also be qualified!
  Expected(TptTokenKind.ptRoundClose);
end;

procedure TmwSimplePasPar.InterfaceGUID;
begin
  Expected(TptTokenKind.ptSquareOpen);
  CharString;
  Expected(TptTokenKind.ptSquareClose);
end;

procedure TmwSimplePasPar.AccessSpecifier;
begin
  case ExID of
    TptTokenKind.ptRead:
      begin
        NextToken;
        ReadAccessIdentifier;
      end;
    TptTokenKind.ptWrite:
      begin
        NextToken;
        WriteAccessIdentifier;
      end;
    TptTokenKind.ptReadonly:
      begin
        NextToken;
      end;
    TptTokenKind.ptWriteonly:
      begin
        NextToken;
      end;
    {$IFDEF D8_NEWER}
    TptTokenKind.ptAdd:
      begin
        NextToken;
        QualifiedIdentifier; //TODO: AddAccessIdentifier
      end;
    TptTokenKind.ptRemove:
      begin
        NextToken;
        QualifiedIdentifier; //TODO: RemoveAccessIdentifier
      end;
    {$ENDIF}
  else
    begin
      SynError(TmwParseError.InvalidAccessSpecifier);
    end;
  end;
end;

procedure TmwSimplePasPar.ReadAccessIdentifier;
begin
  QualifiedIdentifier;
  (* XM removed at Martin suggestion. Martin send a more general fix in QualifiedIdentifier
    //jdj 12/05/2000
    if (TokenID =  ptSquareOpen) then
      begin
        ConstantExpression;
      end;
    //jdj 12/05/2000*)
end;

procedure TmwSimplePasPar.WriteAccessIdentifier;
begin
  QualifiedIdentifier;
  (* XM removed at Martin suggestion. Martin send a more general fix in QualifiedIdentifier
   //jdj 12/05/2000
    if (TokenID =  ptSquareOpen) then
      begin
        ConstantExpression;
      end;
    //jdj 12/05/2000*)
end;

procedure TmwSimplePasPar.StorageSpecifier;
begin
  case ExID of
    TptTokenKind.ptStored:
      begin
        StorageStored;
      end;
    TptTokenKind.ptDefault:
      begin
        StorageDefault;
      end;
    TptTokenKind.ptNodefault:
      begin
        StorageNoDefault;
      end
  else
    begin
      SynError(TmwParseError.InvalidStorageSpecifier);
    end;
  end;
end;

procedure TmwSimplePasPar.StorageDefault;
begin
  ExpectedEx(TptTokenKind.ptDefault);
  StorageExpression;
end;

procedure TmwSimplePasPar.StorageNoDefault;
begin
  ExpectedEx(TptTokenKind.ptNodefault);
end;

procedure TmwSimplePasPar.StorageStored;
begin
  ExpectedEx(TptTokenKind.ptStored);
  case TokenID of
    TptTokenKind.ptIdentifier:
      begin
        StorageIdentifier;
      end;
  else
    if TokenID <> TptTokenKind.ptSemiColon then
    begin
      StorageExpression;
    end;
  end;
end;

procedure TmwSimplePasPar.StorageExpression;
begin
  ConstantExpression;
end;

procedure TmwSimplePasPar.StorageIdentifier;
begin
  Expected(TptTokenKind.ptIdentifier);
end;

procedure TmwSimplePasPar.PropertyParameterList;
//changed James Jacobson on 20001221
begin
  Expected(TptTokenKind.ptSquareOpen);
  if TokenID = TptTokenKind.ptConst then
  begin
    PropertyParameterConst;
  end;
  IdentifierList;
  Expected(TptTokenKind.ptColon);
  TypeId;
  while TokenID = TptTokenKind.ptSemiColon do
  begin
    SEMICOLON;
    if TokenID = TptTokenKind.ptConst then
    begin //jdj 12-21-2000
      PropertyParameterConst;
    end;
    IdentifierList;
    Expected(TptTokenKind.ptColon);
    TypeId;
  end;
  Expected(TptTokenKind.ptSquareClose);
end;

(*begin
  Expected(ptSquareOpen);
  if TokenID = ptConst then
  begin
    PropertyParameterConst;
  end;
  IdentifierList;
  Expected(ptColon);
  TypeId;
  while TokenID = ptSemiColon do
  begin
    SEMICOLON;
    IdentifierList;
    Expected(ptColon);
    TypeId;
  end;
  Expected(ptSquareClose);
end;*)

procedure TmwSimplePasPar.PropertyParameterConst;
begin
  Expected(TptTokenKind.ptConst);
end;

procedure TmwSimplePasPar.PropertySpecifiers;
begin
  if ExID = TptTokenKind.ptIndex then
  begin
    IndexSpecifier; // DR 2001-07-26
  end;
  while ExID in [TptTokenKind.ptRead, TptTokenKind.ptReadonly, TptTokenKind.ptWrite, TptTokenKind.ptWriteonly
    {$IFDEF D8_NEWER}, TptTokenKind.ptAdd, TptTokenKind.ptRemove{$ENDIF}] do
  begin
    AccessSpecifier;
  end;
  if ExID = TptTokenKind.ptDispid then
  begin
    DispIDSpecifier; // DR 2001-07-26
  end;
  while ExID in [TptTokenKind.ptDefault, TptTokenKind.ptNodefault, TptTokenKind.ptStored] do
  begin
    StorageSpecifier;
  end;
  if ExID = TptTokenKind.ptImplements then
  begin
    NextToken;
    QualifiedIdentifierList;
  end;
  SEMICOLON;
end;

procedure TmwSimplePasPar.PropertyInterface;
begin
  if TokenID = TptTokenKind.ptSquareOpen then
  begin
    PropertyParameterList;
  end;
  Expected(TptTokenKind.ptColon);
  TypeId;
end;

procedure TmwSimplePasPar.ClassMethodHeading;
begin
  case TokenID of
    TptTokenKind.ptConstructor:
      begin
        ConstructorHeading;
      end;
    TptTokenKind.ptDestructor:
      begin
        DestructorHeading;
      end;
    {$IFDEF D8_NEWER} //JThurman 2004-03-2003
    TptTokenKind.ptFunction, TptTokenKind.ptIdentifier:
      begin
        if (TokenID = TptTokenKind.ptIdentifier) and (Lexer.ExID <> TptTokenKind.ptOperator) then
          Expected(TptTokenKind.ptOperator);
    {$ELSE}
    ptFunction:
      begin
    {$ENDIF}
        Lexer.InitAhead;
        Lexer.AheadNext;
        case Lexer.AheadTokenID of
          TptTokenKind.ptPoint:
            begin
              ClassMethodResolution;
            end;
        else
          begin
            ClassFunctionHeading;
          end;
        end;
      end;
    TptTokenKind.ptProcedure:
      begin
        Lexer.InitAhead;
        Lexer.AheadNext;
        case Lexer.AheadTokenID of
          TptTokenKind.ptPoint:
            begin
              ClassMethodResolution;
            end;
        else
          begin
            ClassProcedureHeading;
          end;
        end;
      end;
  else
    SynError(TmwParseError.InvalidClassMethodHeading);
  end;
end;

procedure TmwSimplePasPar.ClassFunctionHeading;
begin
  {$IFDEF D8_NEWER} //JThurman 2004-03-2003
  if (TokenID = TptTokenKind.ptIdentifier) and (Lexer.ExID = TptTokenKind.ptOperator) then
    Expected(TptTokenKind.ptIdentifier) else
  {$ENDIF}
  Expected(TptTokenKind.ptFunction);
  FunctionMethodName;
  if TokenID = TptTokenKind.ptRoundOpen then
  begin
    FormalParameterList;
  end;
  Expected(TptTokenKind.ptColon);
  ReturnType;
  if TokenID = TptTokenKind.ptSemiColon then // DR 2002-01-14
    SEMICOLON;
  if ExID = TptTokenKind.ptDispid then
  begin
    DispIDSpecifier; // DR 2001-07-26
    if TokenID = TptTokenKind.ptSemiColon then // DR 2002-01-14
      SEMICOLON;
  end;
  if ExID in ClassMethodDirectiveEnum     //XM 2002-01-29
   then ClassMethodDirective; //XM 2002-01-26
end;

procedure TmwSimplePasPar.FunctionMethodName;
begin
  Expected(TptTokenKind.ptIdentifier);
end;

procedure TmwSimplePasPar.ClassProcedureHeading;
begin
  Expected(TptTokenKind.ptProcedure);
  ProcedureMethodName;
  if TokenID = TptTokenKind.ptRoundOpen then
  begin
    FormalParameterList;
  end;

  if TokenID = TptTokenKind.ptSemiColon then // DR 2002-01-14
    SEMICOLON;

  if ExID = TptTokenKind.ptDispid then
  begin
    DispIDSpecifier; // DR 2001-07-26
    if TokenID = TptTokenKind.ptSemiColon then // DR 2002-01-14
      SEMICOLON;
  end;
  if ExID in ClassMethodDirectiveEnum then // XM 2002-01-29
  ClassMethodDirective;
end;

procedure TmwSimplePasPar.ProcedureMethodName;
begin
  Expected(TptTokenKind.ptIdentifier);
end;

procedure TmwSimplePasPar.ClassMethodResolution;
begin
  case TokenID of
    TptTokenKind.ptFunction:
      begin
        NextToken;
      end;
    TptTokenKind.ptProcedure:
      begin
        NextToken;
      end;
    {$IFDEF D8_NEWER} //JThurman 2004-03-2003
    TptTokenKind.ptIdentifier:
      begin
        if Lexer.ExID = TptTokenKind.ptOperator then
          NextToken;
      end;
    {$ENDIF}
  end;
  ResolutionInterfaceName;
  Expected(TptTokenKind.ptPoint);
  Expected(TptTokenKind.ptIdentifier);
  Expected(TptTokenKind.ptEqual);
  Expected(TptTokenKind.ptIdentifier);
  SEMICOLON;
end;

procedure TmwSimplePasPar.ResolutionInterfaceName;
begin
  Expected(TptTokenKind.ptIdentifier);
end;

procedure TmwSimplePasPar.Constraint;
begin
  while TokenID in [TptTokenKind.ptConstructor, TptTokenKind.ptRecord, TptTokenKind.ptClass, TptTokenKind.ptIdentifier] do
  begin
    case TokenID of
      TptTokenKind.ptConstructor, TptTokenKind.ptRecord, TptTokenKind.ptClass: NextToken;
      TptTokenKind.ptIdentifier: TypeId;
    end;
    if TokenID = TptTokenKind.ptComma then
      NextToken;
  end;
end;

procedure TmwSimplePasPar.ConstraintList;
begin
  Constraint;
  while TokenID = TptTokenKind.ptComma do
  begin
    Constraint;
  end;
end;

procedure TmwSimplePasPar.ConstructorHeading;
begin
  Expected(TptTokenKind.ptConstructor);
  ConstructorName;
  if TokenID = TptTokenKind.ptRoundOpen then
  begin
    FormalParameterList;
  end;
  SEMICOLON;
  ClassMethodDirective;
end;

procedure TmwSimplePasPar.ConstructorName;
begin
  Expected(TptTokenKind.ptIdentifier);
end;

procedure TmwSimplePasPar.DestructorHeading;
begin
  Expected(TptTokenKind.ptDestructor);
  DestructorName;
  if TokenID = TptTokenKind.ptRoundOpen then
  begin
    FormalParameterList;
  end;
  SEMICOLON;
  ClassMethodDirective;
end;

procedure TmwSimplePasPar.DestructorName;
begin
  Expected(TptTokenKind.ptIdentifier);
end;

procedure TmwSimplePasPar.ClassMethodDirective;
begin
  while ExID in ClassMethodDirectiveEnum do
  begin
    ProceduralDirective;
    if TokenID = TptTokenKind.ptSemiColon then // DR 2002-01-14
      SEMICOLON;
  end;
end;

procedure TmwSimplePasPar.ObjectMethodHeading;
begin
  case TokenID of
    TptTokenKind.ptConstructor:
      begin
        ObjectConstructorHeading;
      end;
    TptTokenKind.ptDestructor:
      begin
        ObjectDestructorHeading;
      end;
    TptTokenKind.ptFunction:
      begin
        ObjectFunctionHeading;
      end;
    TptTokenKind.ptProcedure:
      begin
        ObjectProcedureHeading;
      end;
  else
    begin
      SynError(TmwParseError.InvalidMethodHeading);
    end;
  end;
end;

procedure TmwSimplePasPar.ObjectFunctionHeading;
begin
  Expected(TptTokenKind.ptFunction);
  FunctionMethodName;
  if TokenID = TptTokenKind.ptRoundOpen then
  begin
    FormalParameterList;
  end;
  Expected(TptTokenKind.ptColon);
  ReturnType;
  if TokenID = TptTokenKind.ptSemiColon then  SEMICOLON;
  ObjectMethodDirective;
end;

procedure TmwSimplePasPar.ObjectProcedureHeading;
begin
  Expected(TptTokenKind.ptProcedure);
  ProcedureMethodName;
  if TokenID = TptTokenKind.ptRoundOpen then
  begin
    FormalParameterList;
  end;
  if TokenID = TptTokenKind.ptSemiColon then SEMICOLON;
  ObjectMethodDirective;
end;

procedure TmwSimplePasPar.ObjectConstructorHeading;
begin
  Expected(TptTokenKind.ptConstructor);
  ConstructorName;
  if TokenID = TptTokenKind.ptRoundOpen then
  begin
    FormalParameterList;
  end;
  if TokenID = TptTokenKind.ptSemiColon then SEMICOLON;
  ObjectMethodDirective;
end;

procedure TmwSimplePasPar.ObjectDestructorHeading;
begin
  Expected(TptTokenKind.ptDestructor);
  DestructorName;
  if TokenID = TptTokenKind.ptRoundOpen then
  begin
    FormalParameterList;
  end;
  if TokenID = TptTokenKind.ptSemiColon then SEMICOLON;
  ObjectMethodDirective;
end;

procedure TmwSimplePasPar.ObjectMethodDirective;
begin
  while ExID in [TptTokenKind.ptAbstract, TptTokenKind.ptCdecl, TptTokenKind.ptDynamic, TptTokenKind.ptExport, TptTokenKind.ptExternal, TptTokenKind.ptFar,
    TptTokenKind.ptMessage, TptTokenKind.ptNear,
    TptTokenKind.ptOverload, // DR 2001-08-07
    TptTokenKind.ptPascal, TptTokenKind.ptRegister, TptTokenKind.ptSafeCall, TptTokenKind.ptStdcall, TptTokenKind.ptVirtual,
    TptTokenKind.ptDeprecated, TptTokenKind.ptLibrary, TptTokenKind.ptPlatform // DR 2001-10-20
    {$IFDEF D8_NEWER}
    , TptTokenKind.ptStatic
    {$ENDIF}
    {$IFDEF D9_NEWER}
    , TptTokenKind.ptInline
    {$ENDIF}
    ] do
  begin
    ProceduralDirective;
    if TokenID = TptTokenKind.ptSemiColon then SEMICOLON;
  end;
end;

procedure TmwSimplePasPar.Directive16Bit;
begin
  case ExID of
    TptTokenKind.ptNear:
      begin
        NextToken;
      end;
    TptTokenKind.ptFar:
      begin
        NextToken;
      end;
    TptTokenKind.ptExport:
      begin
        NextToken;
      end;
  else
    begin
      SynError(TmwParseError.InvalidDirective16Bit);
    end;
  end;
end;

procedure TmwSimplePasPar.DirectiveBinding;
begin
  case ExID of
    TptTokenKind.ptVirtual:
      begin
        NextToken;
      end;
    TptTokenKind.ptDynamic:
      begin
        NextToken;
      end;
    TptTokenKind.ptMessage:
      begin
        NextToken;
        ConstantExpression;
      end;
    TptTokenKind.ptOverride:
      begin
        NextToken;
      end;
    TptTokenKind.ptOverload:
      begin
        NextToken;
      end;
    TptTokenKind.ptReintroduce:
      begin
        NextToken;
      end;
  else
    begin
      SynError(TmwParseError.InvalidDirectiveBinding);
    end;
  end;
end;

procedure TmwSimplePasPar.ReturnType;
begin
  {$IFDEF D8_NEWER}
  while TokenID = TptTokenKind.ptSquareOpen do
    CustomAttribute;
  {$ENDIF}
  case TokenID of
    TptTokenKind.ptString:
      begin
        StringType;
      end;
  else
    begin
      TypeId;
    end;
  end;
end;

procedure TmwSimplePasPar.FormalParameterList;
begin
  Expected(TptTokenKind.ptRoundOpen);
  FormalParameterSection;
  while TokenID = TptTokenKind.ptSemiColon do
  begin
    SEMICOLON;
    FormalParameterSection;
  end;
  Expected(TptTokenKind.ptRoundClose);
end;

procedure TmwSimplePasPar.FormalParameterSection;
begin
  {$IFDEF D8_NEWER}//JThurman 2004-03-23
  while TokenID = TptTokenKind.ptSquareOpen do
    CustomAttribute;
  {$ENDIF}
  case TokenID of
    TptTokenKind.ptConst:
      begin
        ConstParameter;
      end;
    TptTokenKind.ptIdentifier:
      case ExID of
        TptTokenKind.ptOut: OutParameter;
      else
        ParameterFormal;
      end;
    TptTokenKind.ptIn:
      begin
        InParameter;
      end;
    TptTokenKind.ptVar:
      begin
        VarParameter;
      end;
  end;
end;

procedure TmwSimplePasPar.ConstParameter;
begin
  Expected(TptTokenKind.ptConst);
  ParameterNameList;
  case TokenID of
    TptTokenKind.ptColon:
      begin
        NextToken;
        NewFormalParameterType;
        if TokenID = TptTokenKind.ptEqual then
        begin
          NextToken;
          TypedConstant;
        end;
      end
  end;
end;

procedure TmwSimplePasPar.VarParameter;
begin
  Expected(TptTokenKind.ptVar);
  ParameterNameList;
  case TokenID of
    TptTokenKind.ptColon:
      begin
        NextToken;
        NewFormalParameterType;
      end
  end;
end;

procedure TmwSimplePasPar.OutParameter;
begin
  ExpectedEx(TptTokenKind.ptOut);
  ParameterNameList;
  case TokenID of
    TptTokenKind.ptColon:
      begin
        NextToken;
        NewFormalParameterType;
      end
  end;
end;

procedure TmwSimplePasPar.ParameterFormal;
begin
  case TokenID of
    TptTokenKind.ptIdentifier:
      begin
        ParameterNameList;
        Expected(TptTokenKind.ptColon);
        NewFormalParameterType;
        if TokenID = TptTokenKind.ptEqual then
        begin
          NextToken;
          TypedConstant;
        end;
      end;
  else
    begin
      SynError(TmwParseError.InvalidParameter);
    end;
  end;
end;

procedure TmwSimplePasPar.ParameterNameList;
begin
  ParameterName;
  while TokenID = TptTokenKind.ptComma do
  begin
    NextToken;
    ParameterName;
  end;
end;

procedure TmwSimplePasPar.ParameterName;
begin
  Expected(TptTokenKind.ptIdentifier);
end;

procedure TmwSimplePasPar.NewFormalParameterType;
begin
  case TokenID of
    TptTokenKind.ptArray:
      begin
        NextToken;
        Expected(TptTokenKind.ptOf);
        case TokenID of
          TptTokenKind.ptConst: (*new in ObjectPascal80*)
            begin
              NextToken;
            end;
        else
          begin
            OldFormalParameterType;
          end;
        end;
      end;
  else
    begin
      OldFormalParameterType;
    end;
  end;
end;

procedure TmwSimplePasPar.OldFormalParameterType;
begin
  case TokenID of
    TptTokenKind.ptString:
      begin
        NextToken;
      end;
    TptTokenKind.ptFile:
      begin
        FileType;
      end;
  else
    begin
      TypeId;
    end;
  end;
end;

procedure TmwSimplePasPar.FunctionMethodDeclaration;
begin
  {$IFDEF D8_NEWER} //JThurman 2004-03-2003
  if (TokenID = TptTokenKind.ptIdentifier) and (Lexer.ExID = TptTokenKind.ptOperator) then
    NextToken else
  {$ENDIF}
  Expected(TptTokenKind.ptFunction);
  Lexer.InitAhead;
  if Lexer.AheadTokenID in [TptTokenKind.ptPoint, TptTokenKind.ptLower] then
  begin
    ObjectNameOfMethod;
    Expected(TptTokenKind.ptPoint);
  end;
  FunctionProcedureName;
  if TokenID = TptTokenKind.ptRoundOpen then
  begin
    FormalParameterList;
  end;
  case TokenID of
    TptTokenKind.ptSemiColon:
      begin
        FunctionProcedureBlock;
      end;
  else
    begin
      Expected(TptTokenKind.ptColon);
      ReturnType;
      FunctionProcedureBlock;
    end;
  end;
end;

procedure TmwSimplePasPar.ProcedureMethodDeclaration;
begin
  case TokenID of
    TptTokenKind.ptConstructor:
      begin
        NextToken;
      end;
    TptTokenKind.ptDestructor:
      begin
        NextToken;
      end;
    TptTokenKind.ptProcedure:
      begin
        NextToken;
      end;
  else
    begin
      SynError(TmwParseError.InvalidProcedureMethodDeclaration);
    end;
  end;
  Lexer.InitAhead;
  if Lexer.AheadTokenID in [TptTokenKind.ptPoint, TptTokenKind.ptLower] then
  begin
    ObjectNameOfMethod;
    Expected(TptTokenKind.ptPoint);
  end;
  FunctionProcedureName;
  if TokenID = TptTokenKind.ptRoundOpen then
  begin
    FormalParameterList;
  end;
  FunctionProcedureBlock;
end;

procedure TmwSimplePasPar.FunctionProcedureName;
begin
  Expected(TptTokenKind.ptIdentifier);
end;

procedure TmwSimplePasPar.ObjectNameOfMethod;
begin
  Expected(TptTokenKind.ptIdentifier);
  {$IFDEF D8_NEWER} //JThurman 2004-03-22
  if TokenID = TptTokenKind.ptLower then
    TypeParams;
  Lexer.InitAhead;
  Lexer.AheadNext;
  if Lexer.AheadTokenID = TptTokenKind.ptPoint then
  begin
    Expected(TptTokenKind.ptPoint);
    ObjectNameOfMethod;
  end;
  {$ENDIF}
end;

procedure TmwSimplePasPar.FunctionProcedureBlock;
var
  NoExternal: Boolean;
begin
  NoExternal := True;
  if TokenID = TptTokenKind.ptSemiColon
    then SEMICOLON;
  case ExID of
    TptTokenKind.ptForward:
      ForwardDeclaration; // DR 2001-07-23
  else
    while ExID in [TptTokenKind.ptAbstract, TptTokenKind.ptCdecl, TptTokenKind.ptDynamic, TptTokenKind.ptExport, TptTokenKind.ptExternal, TptTokenKind.ptFar,
      TptTokenKind.ptMessage, TptTokenKind.ptNear, TptTokenKind.ptOverload, TptTokenKind.ptOverride, TptTokenKind.ptPascal, TptTokenKind.ptRegister,
      TptTokenKind.ptReintroduce, TptTokenKind.ptSafeCall, TptTokenKind.ptStdcall, TptTokenKind.ptVirtual,
      TptTokenKind.ptDeprecated, TptTokenKind.ptLibrary, TptTokenKind.ptPlatform, // DR 2001-10-20
      TptTokenKind.ptLocal, TptTokenKind.ptVarargs,
      TptTokenKind.ptAssembler //JT 2004-10-29
      {$IFDEF D8_NEWER}
      , TptTokenKind.ptStatic
      {$ENDIF}
      {$IFDEF D9_NEWER}
      , TptTokenKind.ptInline
      {$ENDIF}
       ] // DR 2001-11-14
    do
      begin
        case ExID of
          TptTokenKind.ptExternal:
            begin
              ProceduralDirective;
              if TokenID = TptTokenKind.ptSemiColon then SEMICOLON;
              NoExternal := False;
            end;
        else
          begin
            ProceduralDirective;
            if TokenID = TptTokenKind.ptSemiColon then SEMICOLON;
          end;
        end;
      end;
    if ExID = TptTokenKind.ptForward then
      ForwardDeclaration // DR 2001-07-23
    else if NoExternal then
    begin
      if ExID = TptTokenKind.ptAssembler then
      begin
        NextToken;
        SEMICOLON;
      end;
      case TokenID of
        TptTokenKind.ptAsm:
          begin
            AsmStatement;
          end;
      else
        begin
          Block;
        end;
      end;
      SEMICOLON;
    end;
  end;
end;

procedure TmwSimplePasPar.ExternalDirective;
begin
  ExpectedEx(TptTokenKind.ptExternal);
  case TokenID of
    TptTokenKind.ptSemiColon:
      begin
        SEMICOLON;
      end;
  else
    begin
      SimpleExpression;
      ExternalDirectiveTwo;
    end;
  end;
end;

procedure TmwSimplePasPar.ExternalDirectiveTwo;
begin
  case fLexer.ExID of
    TptTokenKind.ptIndex:
      begin
        NextToken;
      end;
    TptTokenKind.ptName:
      begin
        NextToken;
        SimpleExpression;
      end;
    TptTokenKind.ptSemiColon:
      begin
        SEMICOLON;
        ExternalDirectiveThree;
      end;
  end
end;

procedure TmwSimplePasPar.ExternalDirectiveThree;
begin
  case TokenID of
    TptTokenKind.ptMinus:
      begin
        NextToken;
      end;
  end;
  case TokenID of
    TptTokenKind.ptIdentifier, TptTokenKind.ptIntegerConst:
      begin
        NextToken;
      end;
  end;
end;

procedure TmwSimplePasPar.ForStatement;
begin
  Expected(TptTokenKind.ptFor);
  QualifiedIdentifier;
  {$IFDEF D8_NEWER}
  if Lexer.TokenID = TptTokenKind.ptAssign then
  begin
    Expected(TptTokenKind.ptAssign);
    Expression;
    case TokenID of
      TptTokenKind.ptTo:
        begin
          NextToken;
        end;
      TptTokenKind.ptDownto:
        begin
          NextToken;
        end;
    else
      begin
        SynError(TmwParseError.InvalidForStatement);
      end;
    end;
    Expression;
  end else
  if Lexer.TokenID = TptTokenKind.ptIn then
  begin
    Expected(TptTokenKind.ptIn);
    //QualifiedIdentifier;
    Expression;
  end;
  {$ELSE}
  Expected(ptAssign);
  Expression;
  case TokenID of
    ptTo:
      begin
        NextToken;
      end;
    ptDownTo:
      begin
        NextToken;
      end;
  else
    begin
      SynError(InvalidForStatement);
    end;
  end;
  Expression;
  {$ENDIF}
  Expected(TptTokenKind.ptDo);
  Statement;
end;

procedure TmwSimplePasPar.WhileStatement;
begin
  Expected(TptTokenKind.ptWhile);
  Expression;
  Expected(TptTokenKind.ptDo);
  Statement;
end;

procedure TmwSimplePasPar.RepeatStatement;
begin
  Expected(TptTokenKind.ptRepeat);
  StatementList;
  Expected(TptTokenKind.ptUntil);
  Expression;
end;

procedure TmwSimplePasPar.CaseStatement;
begin
  Expected(TptTokenKind.ptCase);
  Expression;
  Expected(TptTokenKind.ptOf);
  CaseSelector;
  while TokenID = TptTokenKind.ptSemiColon do
  begin
    SEMICOLON;
    case TokenID of
      TptTokenKind.ptElse, TptTokenKind.ptEnd: ;
    else
      CaseSelector;
    end;
  end;
  if TokenID = TptTokenKind.ptElse then
  begin
    NextToken;
    StatementList;
    SEMICOLON;
  end;
  Expected(TptTokenKind.ptEnd);
end;

procedure TmwSimplePasPar.CaseSelector;
begin
  CaseLabel;
  while TokenID = TptTokenKind.ptComma do
  begin
    NextToken;
    CaseLabel;
  end;
  Expected(TptTokenKind.ptColon);
  case TokenID of
    TptTokenKind.ptSemiColon: ;
  else
    Statement;
  end;
end;

procedure TmwSimplePasPar.CaseLabel;
begin
  ConstantExpression;
  if TokenID = TptTokenKind.ptDotDot then
  begin
    NextToken;
    ConstantExpression;
  end;
end;

procedure TmwSimplePasPar.IfStatement;
begin
  Expected(TptTokenKind.ptIf);
  Expression;
  Expected(TptTokenKind.ptThen);
  Statement;
  //This breaks if you have an if statement immediately preceding the else 
  //clause of a case statement
{  Lexer.InitAhead;
  if (TokenID = ptSemicolon) and (Lexer.AheadTokenID = ptElse) then
  begin
    if Assigned(FOnMessage) then
    begin
      FOnMessage(Self, meError, ''';'' not allowed before ''ELSE''',
        FLexer.PosXY.X, FLexer.PosXY.Y);
    end;
  end;}
  if TokenID = TptTokenKind.ptElse then
  begin
    NextToken;
    Statement;
  end;
end;

procedure TmwSimplePasPar.ExceptBlock;
begin
  case ExID of
    TptTokenKind.ptOn:
      begin
        ExceptionHandlerList;
        ExceptionBlockElseBranch
      end;
  else
    begin
      StatementList;
    end;
  end;
end;

procedure TmwSimplePasPar.ExceptionHandlerList;
begin
  while fLexer.ExID = TptTokenKind.ptOn do
  begin
    ExceptionHandler;
    SEMICOLON;
  end;
end;

procedure TmwSimplePasPar.ExceptionHandler;
begin
  ExpectedEx(TptTokenKind.ptOn);
  ExceptionIdentifier;
  Expected(TptTokenKind.ptDo);
  Statement;
end;

procedure TmwSimplePasPar.ExceptionBlockElseBranch;
begin
  case TokenID of
    TptTokenKind.ptElse:
      begin
        NextToken;
        StatementList;
      end;
  end;
end;

procedure TmwSimplePasPar.ExceptionIdentifier;
begin
  Lexer.InitAhead;
  case Lexer.AheadTokenID of
    TptTokenKind.ptPoint:
      begin
        ExceptionClassTypeIdentifier;
      end;
  else
    begin
      ExceptionVariable;
      case Lexer.TokenID of
        TptTokenKind.ptColon:
          begin
            NextToken;
            ExceptionClassTypeIdentifier;
          end;
      end;
    end;
  end;
end;

procedure TmwSimplePasPar.ExceptionClassTypeIdentifier;
begin
  QualifiedIdentifier;
end;

procedure TmwSimplePasPar.ExceptionVariable;
begin
  Expected(TptTokenKind.ptIdentifier);
end;

procedure TmwSimplePasPar.InlineStatement;
begin
  Expected(TptTokenKind.ptInline);
  Expected(TptTokenKind.ptRoundOpen);
  Expected(TptTokenKind.ptIntegerConst);
  while (TokenID = TptTokenKind.ptSlash) do
  begin
    NextToken;
    Expected(TptTokenKind.ptIntegerConst);
  end;
  Expected(TptTokenKind.ptRoundClose);
end;

procedure TmwSimplePasPar.InParameter;
begin
  Expected(TptTokenKind.ptIn);
  ParameterNameList;
  case TokenID of
    TptTokenKind.ptColon:
      begin
        NextToken;
        NewFormalParameterType;
        if TokenID = TptTokenKind.ptEqual then
        begin
          NextToken;
          TypedConstant;
        end;
      end
  end;
end;

procedure TmwSimplePasPar.AsmStatement;
begin
  Lexer.AsmCode := True;
  Expected(TptTokenKind.ptAsm);
  { should be replaced with a Assembler lexer }
  while TokenID <> TptTokenKind.ptEnd do
    case fLexer.TokenID of
      TptTokenKind.ptBegin, TptTokenKind.ptCase, TptTokenKind.ptEnd, TptTokenKind.ptIf, TptTokenKind.ptFunction, TptTokenKind.ptProcedure, TptTokenKind.ptRepeat, TptTokenKind.ptWhile: break;
      TptTokenKind.ptAddressOp:
        begin
          NextToken;
          NextToken;
        end;
      TptTokenKind.ptDoubleAddressOp:
        begin
          NextToken;
          NextToken;
        end;
      TptTokenKind.ptNull: //JThurman 10-26-2004.  Need another way out of this.
        begin
          Expected(TptTokenKind.ptEnd);
          Exit;
        end;
    else
      NextToken;
    end;
  Lexer.AsmCode := False;
  Expected(TptTokenKind.ptEnd);
end;

procedure TmwSimplePasPar.RaiseStatement;
begin
  Expected(TptTokenKind.ptRaise);
  case TokenID of
    TptTokenKind.ptAddressOp, TptTokenKind.ptDoubleAddressOp, TptTokenKind.ptIdentifier, TptTokenKind.ptPointerSymbol, TptTokenKind.ptRoundOpen:
      begin
        Designator;
      end;
  end;
  if ExID = TptTokenKind.ptAt then
  begin
    NextToken;
    Expression;
  end;
end;

procedure TmwSimplePasPar.TryStatement;
begin
  Expected(TptTokenKind.ptTry);
  StatementList;
  case TokenID of
    TptTokenKind.ptExcept:
      begin
        NextToken;
        ExceptBlock;
        Expected(TptTokenKind.ptEnd);
      end;
    TptTokenKind.ptFinally:
      begin
        NextToken;
        StatementList;
        Expected(TptTokenKind.ptEnd);
      end;
  else
    begin
      SynError(TmwParseError.InvalidTryStatement);
    end;
  end;
end;

procedure TmwSimplePasPar.WithStatement;
begin
  Expected(TptTokenKind.ptWith);
  VariableList;
  Expected(TptTokenKind.ptDo);
  Statement;
end;

procedure TmwSimplePasPar.VariableList;
begin
  VariableReference; (* acessing func.recordfield not allowed here;as well as UNITNAMEID *)
  while fLexer.TokenID = TptTokenKind.ptComma do
  begin
    NextToken;
    VariableReference;
  end;
end;

procedure TmwSimplePasPar.StatementList;
begin {removed ptIntegerConst jdj-Put back in for labels}
  while TokenID in [TptTokenKind.ptAddressOp, TptTokenKind.ptAsm, TptTokenKind.ptBegin, TptTokenKind.ptCase, TptTokenKind.ptDoubleAddressOp,
    TptTokenKind.ptFor, TptTokenKind.ptGoto, TptTokenKind.ptIdentifier, TptTokenKind.ptIf, TptTokenKind.ptInherited, TptTokenKind.ptInline, TptTokenKind.ptIntegerConst,
    TptTokenKind.ptPointerSymbol, TptTokenKind.ptRaise, TptTokenKind.ptRoundOpen, TptTokenKind.ptRepeat, TptTokenKind.ptSemiColon, TptTokenKind.ptString,
    TptTokenKind.ptTry, TptTokenKind.ptWhile, TptTokenKind.ptWith] do
  begin
    Statement;
    SEMICOLON;
  end;
end;

procedure TmwSimplePasPar.SimpleStatement;
begin
  case TokenID of
    TptTokenKind.ptAddressOp, TptTokenKind.ptDoubleAddressOp, TptTokenKind.ptIdentifier, TptTokenKind.ptPointerSymbol, TptTokenKind.ptRoundOpen:
      begin
        Designator;
        if TokenID = TptTokenKind.ptAssign then
        begin
          NextToken;
          if TokenID = TptTokenKind.ptInherited then
          begin
            NextToken;
          end;
          Expression;
        end;
      end;
    TptTokenKind.ptGoto:
      begin
        NextToken;
        LabelId;
      end;
  end;
end;

procedure TmwSimplePasPar.Statement;
begin
  case TokenID of
    TptTokenKind.ptAsm:
      begin
        AsmStatement;
      end;
    TptTokenKind.ptBegin:
      begin
        CompoundStatement;
      end;
    TptTokenKind.ptCase:
      begin
        CaseStatement;
      end;
    TptTokenKind.ptFor:
      begin
        ForStatement;
      end;
    TptTokenKind.ptIf:
      begin
        IfStatement;
      end;
    TptTokenKind.ptIdentifier:
      begin
        fLexer.InitAhead;
        case Lexer.AheadTokenID of
          TptTokenKind.ptColon:
            begin
              LabeledStatement;
            end;
        else
          begin
            SimpleStatement;
          end;
        end;
      end;
    TptTokenKind.ptInherited:
      begin
        InheritedStatement;
      end;
    TptTokenKind.ptInline:
      begin
        InlineStatement;
      end;
    TptTokenKind.ptIntegerConst:
      begin
        fLexer.InitAhead;
        case Lexer.AheadTokenID of
          TptTokenKind.ptColon:
            begin
              LabeledStatement;
            end;
        else
          begin
            SynError(TmwParseError.InvalidLabeledStatement);
            NextToken;
          end;
        end;
      end;
    TptTokenKind.ptRepeat:
      begin
        RepeatStatement;
      end;
    TptTokenKind.ptRaise:
      begin
        RaiseStatement;
      end;
    TptTokenKind.ptSemiColon:
      begin
        EmptyStatement;
      end;
    TptTokenKind.ptString:
      begin
        StringStatement;
      end;
    TptTokenKind.ptTry:
      begin
        TryStatement;
      end;
    TptTokenKind.ptWhile:
      begin
        WhileStatement;
      end;
    TptTokenKind.ptWith:
      begin
        WithStatement;
      end;
  else
    begin
      SimpleStatement;
    end;
  end;
end;

procedure TmwSimplePasPar.EmptyStatement;
begin
  { Nothing to do here.
    The semicolon will be removed in StatementList }
end;

procedure TmwSimplePasPar.InheritedStatement;
begin
  Expected(TptTokenKind.ptInherited);
  case TokenID of
    TptTokenKind.ptSemiColon: ;
  else
    begin
      Statement;
    end;
  end;
end;

procedure TmwSimplePasPar.LabeledStatement;
begin
  case TokenID of
    TptTokenKind.ptIdentifier:
      begin
        NextToken;
        Expected(TptTokenKind.ptColon);
        Statement;
      end;
    TptTokenKind.ptIntegerConst:
      begin
        NextToken;
        Expected(TptTokenKind.ptColon);
        Statement;
      end;
  else
    begin
      SynError(TmwParseError.InvalidLabeledStatement);
    end;
  end;
end;

procedure TmwSimplePasPar.StringStatement;
begin
  Expected(TptTokenKind.ptString);
  Statement;
end;

procedure TmwSimplePasPar.SetElement;
begin
  Expression;
  if TokenID = TptTokenKind.ptDotDot then
  begin
    NextToken;
    Expression;
  end;
end;

procedure TmwSimplePasPar.QualifiedIdentifier;
begin //mw 12/7/2000
  Expected(TptTokenKind.ptIdentifier);
  case TokenID of
    TptTokenKind.ptPoint:
      begin
        while TokenID = TptTokenKind.ptPoint do
        begin //jdj 1/7/2001
          NextToken;
          {$IFDEF D8_NEWER}
          if TokenID in [TptTokenKind.ptAnd, TptTokenKind.ptArray, TptTokenKind.ptAs, TptTokenKind.ptAsm, TptTokenKind.ptBegin, TptTokenKind.ptCase, TptTokenKind.ptClass,
            TptTokenKind.ptConst, TptTokenKind.ptConstructor, TptTokenKind.ptDestructor, TptTokenKind.ptDispinterface, TptTokenKind.ptDiv, TptTokenKind.ptDo,
            TptTokenKind.ptDownto, TptTokenKind.ptElse, TptTokenKind.ptEnd, TptTokenKind.ptExcept, TptTokenKind.ptExports, TptTokenKind.ptFile, TptTokenKind.ptFinal,
            TptTokenKind.ptFinalization, TptTokenKind.ptFinally, TptTokenKind.ptFor, TptTokenKind.ptFunction, TptTokenKind.ptGoto, TptTokenKind.ptIf,
            TptTokenKind.ptImplementation, TptTokenKind.ptIn, TptTokenKind.ptInherited, TptTokenKind.ptInitialization, TptTokenKind.ptInline,
            TptTokenKind.ptInterface, TptTokenKind.ptIs, TptTokenKind.ptLabel, TptTokenKind.ptLibrary, TptTokenKind.ptMod, TptTokenKind.ptNil, TptTokenKind.ptNot, TptTokenKind.ptObject,
            TptTokenKind.ptOf, TptTokenKind.ptOr, TptTokenKind.ptOut, TptTokenKind.ptPacked, TptTokenKind.ptProcedure, TptTokenKind.ptProgram, TptTokenKind.ptProperty,
            TptTokenKind.ptRaise, TptTokenKind.ptRecord, TptTokenKind.ptRepeat, TptTokenKind.ptResourcestring, TptTokenKind.ptSealed, TptTokenKind.ptSet,
            TptTokenKind.ptShl, TptTokenKind.ptShr, TptTokenKind.ptStatic, TptTokenKind.ptString, TptTokenKind.ptThen, TptTokenKind.ptThreadvar, TptTokenKind.ptTo, TptTokenKind.ptTry,
            TptTokenKind.ptType, TptTokenKind.ptUnit, TptTokenKind.ptUnsafe, TptTokenKind.ptUntil, TptTokenKind.ptUses, TptTokenKind.ptVar, TptTokenKind.ptWhile, TptTokenKind.ptWith,
            TptTokenKind.ptXor] then
              NextToken
          else
          {$ENDIF}
          Expected(TptTokenKind.ptIdentifier);
          if (TokenID = TptTokenKind.ptSquareOpen) then
          begin
            ConstantExpression;
          end;
        end;
      end;
    TptTokenKind.ptSquareOpen:
      begin
        ConstantExpression;
      end;
  end;
(*  Expected(ptIdentifier); // old code for information removed in next versions
  case TokenID of
    ptPoint:
      begin
        NextToken;
        Expected(ptIdentifier);
        if (TokenID = ptSquareOpen) then
        begin
          ConstantExpression;
        end;
      end;
    ptSquareOpen: {MW 20001207}
      begin
        ConstantExpression;
      end;
  end;*)

end;

procedure TmwSimplePasPar.SetConstructor;
begin
  Expected(TptTokenKind.ptSquareOpen);
  SetElement;
  while TokenID = TptTokenKind.ptComma do
  begin
    NextToken;
    SetElement;
  end;
  Expected(TptTokenKind.ptSquareClose);
end;

procedure TmwSimplePasPar.Number;
begin
  case TokenID of
    TptTokenKind.ptFloat:
      begin
        NextToken;
      end;
    TptTokenKind.ptIntegerConst:
      begin
        NextToken;
      end;
    TptTokenKind.ptIdentifier:
      begin
        NextToken;
      end;
  else
    begin
      SynError(TmwParseError.InvalidNumber);
    end;
  end;
end;

procedure TmwSimplePasPar.ExpressionList;
begin
  Expression;
  if TokenID = TptTokenKind.ptAssign then //JT Nov 26, 2004 - supporting ole automation syntax
    begin
      Expected(TptTokenKind.ptAssign);
      Expression;
    end;
  while TokenID = TptTokenKind.ptComma do
  begin
    NextToken;
    Expression;
    if TokenID = TptTokenKind.ptAssign then //JT Nov 26, 2004 - supporting ole automation syntax
    begin
      Expected(TptTokenKind.ptAssign);
      Expression;
    end;
  end;
end;

procedure TmwSimplePasPar.Designator;
begin
  VariableReference;
end;

procedure TmwSimplePasPar.MultiplicativeOperator;
begin
  case TokenID of
    TptTokenKind.ptAnd:
      begin
        NextToken;
      end;
    TptTokenKind.ptDiv:
      begin
        NextToken;
      end;
    TptTokenKind.ptMod:
      begin
        NextToken;
      end;
    TptTokenKind.ptShl:
      begin
        NextToken;
      end;
    TptTokenKind.ptShr:
      begin
        NextToken;
      end;
    TptTokenKind.ptSlash:
      begin
        NextToken;
      end;
    TptTokenKind.ptStar:
      begin
        NextToken;
      end;
  else
    begin SynError(TmwParseError.InvalidMultiplicativeOperator);
    end;
  end;
end;

procedure TmwSimplePasPar.Factor;
begin
  case TokenID of
    TptTokenKind.ptAsciiChar, TptTokenKind.ptStringConst:
      begin
        CharString;
      end;
    TptTokenKind.ptAddressOp, TptTokenKind.ptDoubleAddressOp, TptTokenKind.ptIdentifier, TptTokenKind.ptInherited, TptTokenKind.ptPointerSymbol,
      TptTokenKind.ptRoundOpen:
      begin
        Designator;
      end;
    TptTokenKind.ptIntegerConst, TptTokenKind.ptFloat:
      begin
        Number;
      end;
    TptTokenKind.ptNil:
      begin
        NextToken;
      end;
    TptTokenKind.ptMinus:
      begin
        NextToken;
        Factor;
      end;
    TptTokenKind.ptNot:
      begin
        NextToken;
        Factor;
      end;
    TptTokenKind.ptPlus:
      begin
        NextToken;
        Factor;
      end;
    TptTokenKind.ptSquareOpen:
      begin
        SetConstructor;
      end;
    TptTokenKind.ptString:
      begin
        NextToken;
        Factor;
      end;
    TptTokenKind.ptFunction, TptTokenKind.ptProcedure:
      AnonymousMethod;
  end;
end;

procedure TmwSimplePasPar.AdditiveOperator;
begin
  if TokenID in [TptTokenKind.ptMinus, TptTokenKind.ptOr, TptTokenKind.ptPlus, TptTokenKind.ptXor] then
  begin
    NextToken; // DR 2001-12-19
 {
   case TokenID of
  ptMinus, ptPlus:
    begin
   while TokenID in [ptMinus, ptPlus] do
     case TokenID of
    ptMinus:
      begin
     NextToken;
      end;
    ptPlus:
      begin
     NextToken;
      end;
     end;
    end;
  ptOr:
    begin
   NextToken;
    end;
  ptXor:
    begin
   NextToken;
    end;
   end;}
  end
  else
  begin
    SynError(TmwParseError.InvalidAdditiveOperator);
  end;
end;

procedure TmwSimplePasPar.Term;
begin
  Factor;
  while TokenID in [TptTokenKind.ptAnd, TptTokenKind.ptDiv, TptTokenKind.ptMod, TptTokenKind.ptShl, TptTokenKind.ptShr, TptTokenKind.ptSlash, TptTokenKind.ptStar] do
  begin
    MultiplicativeOperator;
    Factor;
  end;
end;

procedure TmwSimplePasPar.RelativeOperator;
begin
  case TokenID of
    TptTokenKind.ptAs:
      begin
        NextToken;
      end;
    TptTokenKind.ptEqual:
      begin
        NextToken;
      end;
    TptTokenKind.ptGreater:
      begin
        NextToken;
      end;
    TptTokenKind.ptGreaterEqual:
      begin
        NextToken;
      end;
    TptTokenKind.ptIn:
      begin
        NextToken;
      end;
    TptTokenKind.ptIs:
      begin
        NextToken;
      end;
    TptTokenKind.ptLower:
      begin
        NextToken;
      end;
    TptTokenKind.ptLowerEqual:
      begin
        NextToken;
      end;
    TptTokenKind.ptNotEqual:
      begin
        NextToken;
      end;
  else
    begin
      SynError(TmwParseError.InvalidRelativeOperator);
    end;
  end;
end;

procedure TmwSimplePasPar.SimpleExpression;
begin
(*  while TokenID in [ptMinus, ptPlus] do
 begin
   NextToken;								// DR 2001-12-19
 end;
*)
  Term;
  while TokenID in [TptTokenKind.ptMinus, TptTokenKind.ptOr, TptTokenKind.ptPlus, TptTokenKind.ptXor] do
  begin
    AdditiveOperator;
    Term;
  end;
end;

procedure TmwSimplePasPar.Expression;
begin
  SimpleExpression;

  //JT 2006-07-17 The Delphi language guide has this as
  //Expression -> SimpleExpression [RelOp SimpleExpression]...
  //So this needs to be able to repeat itself.
  case TokenID of
  TptTokenKind.ptEqual, TptTokenKind.ptGreater, TptTokenKind.ptGreaterEqual, TptTokenKind.ptLower, TptTokenKind.ptLowerEqual, TptTokenKind.ptIn, TptTokenKind.ptIs,
    TptTokenKind.ptNotEqual:
    begin
      while TokenID in [TptTokenKind.ptEqual, TptTokenKind.ptGreater, TptTokenKind.ptGreaterEqual, TptTokenKind.ptLower, TptTokenKind.ptLowerEqual,
        TptTokenKind.ptIn, TptTokenKind.ptIs, TptTokenKind.ptNotEqual{, ptColon}] do
      begin
        RelativeOperator;
        SimpleExpression;
      end;
    end;
  TptTokenKind.ptColon:
    begin
      case fInRound of
        False: ;
        True:
          while TokenID = TptTokenKind.ptColon do
          begin
            NextToken;
            SimpleExpression;
          end;
      end;
    end;
  end;
end;

procedure TmwSimplePasPar.VarDeclaration;
begin
  // !! Changed back to var name list from IdentifierList
  VarNameList;
  Expected(TptTokenKind.ptColon);
  TypeKind;
  while ExID in [TptTokenKind.ptDeprecated, TptTokenKind.ptLibrary, TptTokenKind.ptPlatform] do // DR 2001-10-20
    case ExID of
      TptTokenKind.ptDeprecated: DirectiveDeprecated;
      TptTokenKind.ptLibrary: DirectiveLibrary;
      TptTokenKind.ptPlatform: DirectivePlatform;
    end;
  case GenID of
    TptTokenKind.ptAbsolute:
      begin
        VarAbsolute;
      end;
    TptTokenKind.ptEqual:
      begin
        VarEqual;
      end;
  end;
  while ExID in [TptTokenKind.ptDeprecated, TptTokenKind.ptLibrary, TptTokenKind.ptPlatform] do // DR 2001-10-20
    case ExID of
      TptTokenKind.ptDeprecated: DirectiveDeprecated;
      TptTokenKind.ptLibrary: DirectiveLibrary;
      TptTokenKind.ptPlatform: DirectivePlatform;
    end;
end;

procedure TmwSimplePasPar.VarAbsolute;
begin
  ExpectedEx(TptTokenKind.ptAbsolute);
  ConstantValue;
end;

procedure TmwSimplePasPar.VarEqual;
begin
  Expected(TptTokenKind.ptEqual);
  ConstantValueTyped;
end;

procedure TmwSimplePasPar.VarNameList;
begin
  VarName;
  while TokenID = TptTokenKind.ptComma do
    begin
      NextToken;
      VarName;
    end;
end;

procedure TmwSimplePasPar.VarName;
begin
  Expected(TptTokenKind.ptIdentifier);
end;

procedure TmwSimplePasPar.DirectiveCalling;
begin
  case ExID of
    TptTokenKind.ptCdecl:
      begin
        NextToken;
      end;
    TptTokenKind.ptPascal:
      begin
        NextToken;
      end;
    TptTokenKind.ptRegister:
      begin
        NextToken;
      end;
    TptTokenKind.ptSafeCall:
      begin
        NextToken;
      end;
    TptTokenKind.ptStdcall:
      begin
        NextToken;
      end;
  else
    begin
      SynError(TmwParseError.InvalidDirectiveCalling);
    end;
  end;
end;

procedure TmwSimplePasPar.RecordVariant;
begin
  ConstantExpression;
  while (TokenID = TptTokenKind.ptComma) do
  begin
    NextToken;
    ConstantExpression;
  end;
  Expected(TptTokenKind.ptColon);
  Expected(TptTokenKind.ptRoundOpen);
  if TokenID <> TptTokenKind.ptRoundClose then
  begin
    FieldList;
  end;
  Expected(TptTokenKind.ptRoundClose);
end;

procedure TmwSimplePasPar.VariantSection;
begin
  Expected(TptTokenKind.ptCase);
  TagField;
  Expected(TptTokenKind.ptOf);
  RecordVariant;
  while TokenID = TptTokenKind.ptSemiColon do
  begin
    SEMICOLON;
    case TokenID of //DR 2001-12-11
      TptTokenKind.ptEnd, TptTokenKind.ptRoundClose: Break;
    else
      RecordVariant;
    end;
  end;
end;

procedure TmwSimplePasPar.TagField;
begin
  TagFieldName;
  case fLexer.TokenID of
    TptTokenKind.ptColon:
      begin
        NextToken;
        TagFieldTypeName;
      end;
  end;
end;

procedure TmwSimplePasPar.TagFieldName;
begin
  Expected(TptTokenKind.ptIdentifier);
end;

procedure TmwSimplePasPar.TagFieldTypeName;
begin
  QualifiedIdentifier;
end;

procedure TmwSimplePasPar.FieldDeclaration;
begin
  //IdentifierList;
  FieldNameList;
  Expected(TptTokenKind.ptColon);
  TypeKind;
  while ExID in [TptTokenKind.ptDeprecated, TptTokenKind.ptLibrary, TptTokenKind.ptPlatform] do // DR 2002-01-09
    case ExID of
      TptTokenKind.ptDeprecated: DirectiveDeprecated;
      TptTokenKind.ptLibrary: DirectiveLibrary;
      TptTokenKind.ptPlatform: DirectivePlatform;
    end;
end;

procedure TmwSimplePasPar.FieldList;
begin
  while TokenID = TptTokenKind.ptIdentifier do
  begin
    FieldDeclaration;
    SEMICOLON;
  end;
  if TokenID = TptTokenKind.ptCase then
  begin
    VariantSection;
  end;
end;

procedure TmwSimplePasPar.FieldName;
begin
  Expected(TptTokenKind.ptIdentifier);
end;

procedure TmwSimplePasPar.FieldNameList;
begin
  FieldName;
  while TokenID = TptTokenKind.ptComma do
  begin
    NextToken;
    FieldName;
  end;
end;

procedure TmwSimplePasPar.RecordType;
begin
  Expected(TptTokenKind.ptRecord);
  if TokenID = TptTokenKind.ptSemiColon then
    Exit;
  {$IFDEF D8_NEWER}
  if TokenID = TptTokenKind.ptRoundOpen then
  begin
    ClassHeritage;
    if TokenID = TptTokenKind.ptSemiColon then
      Exit;
  end;
  ClassMemberList;
  {$ELSE}
  FieldList;
  {$ENDIF}
  Expected(TptTokenKind.ptEnd);
end;

procedure TmwSimplePasPar.FileType;
begin
  Expected(TptTokenKind.ptFile);
  if TokenID = TptTokenKind.ptOf then
  begin
    NextToken;
    TypeId;
  end;
end;

procedure TmwSimplePasPar.SetType;
begin
  Expected(TptTokenKind.ptSet);
  Expected(TptTokenKind.ptOf);
  OrdinalType;
end;

procedure TmwSimplePasPar.ArrayType;
begin
  Expected(TptTokenKind.ptArray);
  if TokenID = TptTokenKind.ptSquareOpen then
  begin
    NextToken;
    OrdinalType;
    while TokenID = TptTokenKind.ptComma do
    begin
      NextToken;
      OrdinalType;
    end;
    Expected(TptTokenKind.ptSquareClose);
  end;
  Expected(TptTokenKind.ptOf);
  TypeKind;
end;

procedure TmwSimplePasPar.EnumeratedType;
begin
  Expected(TptTokenKind.ptRoundOpen);
  EnumeratedTypeItem;
  while TokenID = TptTokenKind.ptComma do
  begin
    NextToken;
    EnumeratedTypeItem;
  end;
  Expected(TptTokenKind.ptRoundClose);
end;

procedure TmwSimplePasPar.SubrangeType;
begin
  ConstantExpression;
  if TokenID = TptTokenKind.ptDotDot then
  begin
    NextToken;
    ConstantExpression;
  end;
end;

procedure TmwSimplePasPar.RealIdentifier;
begin
  case ExID of
    TptTokenKind.ptReal48:
      begin
        NextToken;
      end;
    TptTokenKind.ptReal:
      begin
        NextToken;
      end;
    TptTokenKind.ptSingle:
      begin
        NextToken;
      end;
    TptTokenKind.ptDouble:
      begin
        NextToken;
      end;
    TptTokenKind.ptExtended:
      begin
        NextToken;
      end;
    TptTokenKind.ptCurrency:
      begin
        NextToken;
      end;
    TptTokenKind.ptComp:
      begin
        NextToken;
      end;
  else
    begin
      SynError(TmwParseError.InvalidRealIdentifier);
    end;
  end;
end;

procedure TmwSimplePasPar.RealType;
begin
  case TokenID of
    TptTokenKind.ptMinus:
      begin
        NextToken;
      end;
    TptTokenKind.ptPlus:
      begin
        NextToken;
      end;
  end;
  case TokenID of
    TptTokenKind.ptFloat:
      begin
        NextToken;
      end;
  else
    begin
      VariableReference;
    end;
  end;
end;

procedure TmwSimplePasPar.OrdinalIdentifier;
begin
  case ExID of
    TptTokenKind.ptBoolean:
      begin
        NextToken;
      end;
    TptTokenKind.ptByte:
      begin
        NextToken;
      end;
    TptTokenKind.ptByteBool:
      begin
        NextToken;
      end;
    TptTokenKind.ptCardinal:
      begin
        NextToken;
      end;
    TptTokenKind.ptChar:
      begin
        NextToken;
      end;
    TptTokenKind.ptDWORD:
      begin
        NextToken;
      end;
    TptTokenKind.ptInt64:
      begin
        NextToken;
      end;
    TptTokenKind.ptInteger:
      begin
        NextToken;
      end;
    TptTokenKind.ptLongBool:
      begin
        NextToken;
      end;
    TptTokenKind.ptLongint:
      begin
        NextToken;
      end;
    TptTokenKind.ptLongword:
      begin
        NextToken;
      end;
    TptTokenKind.ptPChar:
      begin
        NextToken;
      end;
    TptTokenKind.ptShortint:
      begin
        NextToken;
      end;
    TptTokenKind.ptSmallint:
      begin
        NextToken;
      end;
    TptTokenKind.ptWideChar:
      begin
        NextToken;
      end;
    TptTokenKind.ptWord:
      begin
        NextToken;
      end;
    TptTokenKind.ptWordBool:
      begin
        NextToken;
      end;
  else
    begin
      SynError(TmwParseError.InvalidOrdinalIdentifier);
    end;
  end;
end;

procedure TmwSimplePasPar.OrdinalType;
begin
  case TokenID of
    TptTokenKind.ptIdentifier:
      begin
        Lexer.InitAhead;
        case Lexer.AheadTokenID of
          TptTokenKind.ptPoint:
            begin
              Expression;
            end;
          TptTokenKind.ptRoundOpen:
            begin //jdj
              ConstantExpression;
            end;
        else
          begin
            TypeId;
          end;
        end;
      end;
    TptTokenKind.ptRoundOpen:
      begin
        EnumeratedType;
      end;
    TptTokenKind.ptSquareOpen:
      begin
        NextToken;
        SubrangeType;
        Expected(TptTokenKind.ptSquareClose);
      end;
  else
    begin
      Expression;
    end;
  end;
  if TokenID = TptTokenKind.ptDotDot then
  begin
    NextToken;
    ConstantExpression;
  end;
end;

procedure TmwSimplePasPar.VariableReference;
begin
  case TokenID of
    TptTokenKind.ptAddressOp:
      begin
        NextToken;
        Variable;
      end;
    TptTokenKind.ptDoubleAddressOp:
      begin
        NextToken;
        Variable;
      end;
    TptTokenKind.ptPointerSymbol:
      begin
        NextToken;
        case TokenID of
          TptTokenKind.ptRoundClose, TptTokenKind.ptSquareClose: ;
        else
          begin
            Variable;
          end;
        end;
      end;
  else
    Variable;
  end;
end;

procedure TmwSimplePasPar.Variable; (* Attention: could also came from proc_call ! ! *)
begin
  case TokenID of
    TptTokenKind.ptInherited:
      begin
        NextToken;
        QualifiedIdentifier;
      end;
    TptTokenKind.ptPoint:
      begin
        VariableTwo;
      end;
    TptTokenKind.ptPointerSymbol:
      begin
        VariableTwo;
      end;
    TptTokenKind.ptRoundOpen:
      begin
        VariableTwo;
      end;
    TptTokenKind.ptSquareOpen:
      begin
        VariableTwo;
      end;
  else
    QualifiedIdentifier;
  end;
  VariableTwo;
  case TokenID of
    TptTokenKind.ptAs:
      begin
        NextToken;
        QualifiedIdentifier;
      end;
  end;
end;

procedure TmwSimplePasPar.VariableTwo;
begin
  case TokenID of
    TptTokenKind.ptPoint:
      begin
        NextToken;
        case TokenID of
          TptTokenKind.ptAddressOp, TptTokenKind.ptDoubleAddressOp, TptTokenKind.ptIdentifier:
            begin
              VariableReference;
            end;
          TptTokenKind.ptPointerSymbol, TptTokenKind.ptRoundOpen, TptTokenKind.ptSquareOpen:
            begin
              VariableTwo;
            end;
        end;
      end;
    TptTokenKind.ptPointerSymbol:
      begin
        NextToken;
        case TokenID of
          TptTokenKind.ptAddressOp, TptTokenKind.ptDoubleAddressOp, TptTokenKind.ptIdentifier:
            begin
              VariableReference;
            end;
          TptTokenKind.ptPoint, TptTokenKind.ptPointerSymbol, TptTokenKind.ptRoundOpen, TptTokenKind.ptSquareOpen:
            begin
              VariableTwo;
            end;
        end;
      end;
    TptTokenKind.ptRoundOpen:
      begin
        NextToken;
        fInRound := True;
        case TokenID of
          TptTokenKind.ptRoundClose:
            begin
              NextToken;
              //Expected(ptRoundClose);
              fInRound := False;
            end;
        else
          begin
            case TokenID of
              TptTokenKind.ptAddressOp, TptTokenKind.ptDoubleAddressOp:
                begin
                  VariableReference;
                end;
              TptTokenKind.ptPoint, TptTokenKind.ptPointerSymbol, TptTokenKind.ptRoundOpen, TptTokenKind.ptSquareOpen:
                begin
                  VariableTwo;
                end;
            end;
            fInRound := True;
            ExpressionList;
            fInRound := True;
            Expected(TptTokenKind.ptRoundClose);
            fInRound := False;
          end;
        end;
        case TokenID of
          TptTokenKind.ptAddressOp, TptTokenKind.ptDoubleAddressOp:
            begin
              VariableReference;
            end;
          TptTokenKind.ptPoint, TptTokenKind.ptPointerSymbol, TptTokenKind.ptRoundOpen, TptTokenKind.ptSquareOpen:
            begin
              VariableTwo;
            end;
        end;
      end;
    TptTokenKind.ptSquareOpen:
      begin
        Lexer.InitAhead;
        while Lexer.AheadTokenID <> TptTokenKind.ptSemiColon do
        begin
          case Lexer.AheadTokenID of
            TptTokenKind.ptBegin, TptTokenKind.ptClass, TptTokenKind.ptConst, TptTokenKind.ptEnd, TptTokenKind.ptDotDot, TptTokenKind.ptIn, TptTokenKind.ptNull, TptTokenKind.ptThreadvar, TptTokenKind.ptType,
              TptTokenKind.ptVar: break;
          else
            Lexer.AheadNext;
          end;
        end;
        case Lexer.AheadTokenID of
          TptTokenKind.ptDotDot:
            begin
              SubrangeType;
            end;
        else
          begin
            NextToken;
            case TokenID of
              TptTokenKind.ptSquareClose:
                begin
                  NextToken;
                end;
            else
              begin
                case TokenID of
                  TptTokenKind.ptAddressOp, TptTokenKind.ptDoubleAddressOp:
                    begin
                      VariableReference;
                    end;
                  TptTokenKind.ptPoint, TptTokenKind.ptPointerSymbol, TptTokenKind.ptRoundOpen, TptTokenKind.ptSquareOpen:
                    begin
                      VariableTwo;
                    end;
                end;
                ExpressionList;
                Expected(TptTokenKind.ptSquareClose);
              end;
            end;
            case TokenID of
              TptTokenKind.ptAddressOp, TptTokenKind.ptDoubleAddressOp:
                begin
                  VariableReference;
                end;
              TptTokenKind.ptPoint, TptTokenKind.ptPointerSymbol, TptTokenKind.ptRoundOpen, TptTokenKind.ptSquareOpen:
                begin
                  VariableTwo;
                end;
            end;

          end;
        end;
      end;
    {$IFDEF D11_NEWER}
    TptTokenKind.ptLower:
      begin
        InitAhead;
        AheadParse.NextToken;
        AheadParse.TypeKind;

        if AheadParse.TokenID = TptTokenKind.ptGreater then
        begin
          NextToken;
          TypeKind;
          Expected(TptTokenKind.ptGreater);
          case TokenID of
          TptTokenKind.ptAddressOp, TptTokenKind.ptDoubleAddressOp, TptTokenKind.ptIdentifier:
            begin
              VariableReference;
            end;
          TptTokenKind.ptPoint, TptTokenKind.ptPointerSymbol, TptTokenKind.ptRoundOpen, TptTokenKind.ptSquareOpen:
            begin
              VariableTwo;
            end;
          end;
        end;
      end;
    {$ENDIF}
  end;
end;

procedure TmwSimplePasPar.InterfaceType;
begin
  case TokenID of
    TptTokenKind.ptInterface:
      begin
        NextToken;
      end;
    TptTokenKind.ptDispinterface:
      begin
        NextToken;
      end
  else
    begin
      SynError(TmwParseError.InvalidInterfaceType);
    end;
  end;
  case TokenID of
    TptTokenKind.ptEnd:
      begin
        NextToken; { Direct descendant without new members }
      end;
    TptTokenKind.ptRoundOpen:
      begin
        InterfaceHeritage;
        case TokenID of
          TptTokenKind.ptEnd:
            begin
              NextToken; { No new members }
            end;
          TptTokenKind.ptSemiColon: ; { No new members }
        else
          begin
            if TokenID = TptTokenKind.ptSquareOpen then
            begin
              InterfaceGUID;
            end;
            InterfaceMemberList;
            Expected(TptTokenKind.ptEnd);
          end;
        end;
      end;
  else
    begin
      if TokenID = TptTokenKind.ptSquareOpen then
      begin
        InterfaceGUID;
      end;
      InterfaceMemberList; { Direct descendant }
      Expected(TptTokenKind.ptEnd);
    end;
  end;
end;

procedure TmwSimplePasPar.InterfaceMemberList;
begin
  while TokenID in [TptTokenKind.ptFunction, TptTokenKind.ptProcedure, TptTokenKind.ptProperty] do
  begin
    ClassMethodOrProperty;
  end;
end;

procedure TmwSimplePasPar.ClassType;
begin
  Expected(TptTokenKind.ptClass);
  {$IFDEF D8_NEWER} //JThurman 2004-03-19
  case TokenID of
    TptTokenKind.ptIdentifier: //NASTY hack because Abstract is generally an ExID, except in this case when it should be a keyword.
      begin
        if Lexer.ExID = TptTokenKind.ptAbstract then
          Expected(TptTokenKind.ptIdentifier);
      end;
    TptTokenKind.ptSealed:
      Expected(TptTokenKind.ptSealed);
    TptTokenKind.ptHelper:
      begin
        Expected(TptTokenKind.ptHelper);
        Expected(TptTokenKind.ptFor);
        Expected(TptTokenKind.ptIdentifier);
      end;
  end;
  {$ENDIF}
  case TokenID of
    TptTokenKind.ptEnd:
      begin
        ClassTypeEnd; // DR 2001-07-31
        NextToken; { Direct descendant of TObject without new members }
      end;
    TptTokenKind.ptRoundOpen:
      begin
        ClassHeritage;
        case TokenID of
          TptTokenKind.ptEnd:
            begin
              Expected(TptTokenKind.ptEnd); // DR 2001-07-31
              ClassTypeEnd; // DR 2001-07-31
            end;
          TptTokenKind.ptSemiColon: ClassTypeEnd; // DR 2001-07-31
        else
          begin
            ClassMemberList; { Direct descendant of TObject }
            Expected(TptTokenKind.ptEnd); // DR 2001-07-31
            ClassTypeEnd; // DR 2001-07-31
          end;
        end;
      end;
  else
    begin
      ClassMemberList; { Direct descendant of TObject }
      Expected(TptTokenKind.ptEnd); // DR 2001-07-31
      ClassTypeEnd; // DR 2001-07-31
    end;
  end;
end;

procedure TmwSimplePasPar.ClassHeritage;
begin
  Expected(TptTokenKind.ptRoundOpen);
  AncestorIdList;
  Expected(TptTokenKind.ptRoundClose);
end;

procedure TmwSimplePasPar.ClassVisibility;
begin
  {$IFDEF D8_NEWER} //JThurman 2004-03-03
  if TokenID = TptTokenKind.ptStrict then
    Expected(TptTokenKind.ptStrict);
  {$ENDIF}
  while ExID in [TptTokenKind.ptAutomated, TptTokenKind.ptPrivate, TptTokenKind.ptProtected, TptTokenKind.ptPublic, TptTokenKind.ptPublished] do
  begin
    Lexer.InitAhead;
    case Lexer.AheadExID of
      TptTokenKind.ptColon, TptTokenKind.ptComma: ;
    else
      case ExID of
        TptTokenKind.ptAutomated:
          begin
            VisibilityAutomated;
          end;
        TptTokenKind.ptPrivate:
          begin
            VisibilityPrivate;
          end;
        TptTokenKind.ptProtected:
          begin
            VisibilityProtected;
          end;
        TptTokenKind.ptPublic:
          begin
            VisibilityPublic;
          end;
        TptTokenKind.ptPublished:
          begin
            VisibilityPublished;
          end;
      end;
    end;
  end;
end;

procedure TmwSimplePasPar.VisibilityAutomated;
begin
  ExpectedEx(TptTokenKind.ptAutomated);
end;

procedure TmwSimplePasPar.VisibilityPrivate;
begin
  ExpectedEx(TptTokenKind.ptPrivate);
end;

procedure TmwSimplePasPar.VisibilityProtected;
begin
  ExpectedEx(TptTokenKind.ptProtected);
end;

procedure TmwSimplePasPar.VisibilityPublic;
begin
  ExpectedEx(TptTokenKind.ptPublic);
end;

procedure TmwSimplePasPar.VisibilityPublished;
begin
  ExpectedEx(TptTokenKind.ptPublished);
end;

procedure TmwSimplePasPar.VisibilityUnknown;
begin
  //
end;

procedure TmwSimplePasPar.ClassMemberList;
begin
  ClassVisibility;
  while TokenID in [TptTokenKind.ptClass, TptTokenKind.ptConstructor, TptTokenKind.ptDestructor, TptTokenKind.ptFunction,
    TptTokenKind.ptIdentifier, TptTokenKind.ptProcedure, TptTokenKind.ptProperty
    {$IFDEF D8_NEWER}, TptTokenKind.ptType, TptTokenKind.ptSquareOpen, TptTokenKind.ptVar, TptTokenKind.ptConst, TptTokenKind.ptStrict,
     TptTokenKind.ptCase{$ENDIF}] do
  begin
    while (TokenID = TptTokenKind.ptIdentifier) and
      not (ExID in [TptTokenKind.ptPrivate, TptTokenKind.ptProtected, TptTokenKind.ptPublished, TptTokenKind.ptPublic]) do
    begin
      ClassField;
      SEMICOLON;
      ClassVisibility;
    end;
    while TokenID in [TptTokenKind.ptClass, TptTokenKind.ptConstructor, TptTokenKind.ptDestructor, TptTokenKind.ptFunction,
      TptTokenKind.ptProcedure, TptTokenKind.ptProperty{$IFDEF D8_NEWER}, TptTokenKind.ptSquareOpen, TptTokenKind.ptVar, TptTokenKind.ptConst{$ENDIF}] do
    begin
      ClassMethodOrProperty;
    end;
    {$IFDEF D8_NEWER}//JThurman 2004-03-22
    {Nested types for D8}
    while TokenID = TptTokenKind.ptType do
      TypeSection;
    while TokenID = TptTokenKind.ptCase do
    begin
      VariantSection;
    end;
    {$ENDIF}
    ClassVisibility;
  end;
end;

procedure TmwSimplePasPar.ClassMethodOrProperty;
begin
  {$IFDEF D8_NEWER}
  if TokenID = TptTokenKind.ptSquareOpen then
    CustomAttribute;
  {$ENDIF}
  if TokenID = TptTokenKind.ptClass
    then ClassClass; //DR 2001-07-16
  case TokenID of
    TptTokenKind.ptProperty:
      begin
        ClassProperty;
      end;
    {$IFDEF D8_NEWER}
    TptTokenKind.ptVar:
      begin
        NextToken;
        while (TokenID = TptTokenKind.ptIdentifier) and (ExID = TptTokenKind.ptUnknown) do
        begin
          ClassField;
          SEMICOLON;
        end;
      end;
    TptTokenKind.ptConst:
      begin
        NextToken;
        while (TokenID = TptTokenKind.ptIdentifier) and (ExID = TptTokenKind.ptUnknown) do
        begin
          ConstantDeclaration;
          SEMICOLON;
        end;
      end;
    {$ENDIF}
  else
    begin
      ClassMethodHeading;
    end;
  end;
end;

procedure TmwSimplePasPar.ClassProperty;
begin
 // DR 2001-07-19 -> changed. for array-property override failure
  Expected(TptTokenKind.ptProperty);
  PropertyName;
  case TokenID of
    TptTokenKind.ptColon, TptTokenKind.ptSquareOpen:
      begin
        PropertyInterface;
      end;
  end;
  PropertySpecifiers;
  case ExID of
    TptTokenKind.ptDefault:
      begin
        PropertyDefault; //DR 2001-07-16
        SEMICOLON;
      end;
  end;
end;

procedure TmwSimplePasPar.PropertyName;
begin
  Expected(TptTokenKind.ptIdentifier);
end;

procedure TmwSimplePasPar.ClassField;
begin
  //IdentifierList;
  FieldNameList;
  Expected(TptTokenKind.ptColon);
  TypeKind;
  while ExID in [TptTokenKind.ptDeprecated, TptTokenKind.ptLibrary, TptTokenKind.ptPlatform] do // DR 2001-10-20
    case ExID of
      TptTokenKind.ptDeprecated: DirectiveDeprecated;
      TptTokenKind.ptLibrary: DirectiveLibrary;
      TptTokenKind.ptPlatform: DirectivePlatform;
    end;
end;

procedure TmwSimplePasPar.ObjectType;
begin
  Expected(TptTokenKind.ptObject);
  case TokenID of
    TptTokenKind.ptEnd:
      begin
        ObjectTypeEnd; // DR 2001-07-31
        NextToken; { Direct descendant without new members }
      end;
    TptTokenKind.ptRoundOpen:
      begin
        ObjectHeritage;
        case TokenID of
          TptTokenKind.ptEnd:
            begin
              Expected(TptTokenKind.ptEnd); // DR 2001-07-31
              ObjectTypeEnd; // DR 2001-07-31
            end;
          TptTokenKind.ptSemiColon: ObjectTypeEnd; // DR 2001-07-31
        else
          begin
            ObjectMemberList; { Direct descendant }
            Expected(TptTokenKind.ptEnd); // DR 2001-07-31
            ObjectTypeEnd; // DR 2001-07-31
          end;
        end;
      end;
  else
    begin
      ObjectMemberList; { Direct descendant }
      Expected(TptTokenKind.ptEnd); // DR 2001-07-31
      ObjectTypeEnd; // DR 2001-07-31
    end;
  end;
end;

procedure TmwSimplePasPar.ObjectHeritage;
begin
  Expected(TptTokenKind.ptRoundOpen);
  AncestorIdList;
  Expected(TptTokenKind.ptRoundClose);
end;

procedure TmwSimplePasPar.ObjectMemberList;
begin {jdj added ptProperty-call to ObjectProperty 02/07/2001}
  ObjectVisibility;
  while TokenID in [TptTokenKind.ptConstructor, TptTokenKind.ptDestructor, TptTokenKind.ptFunction, TptTokenKind.ptIdentifier,
    TptTokenKind.ptProcedure, TptTokenKind.ptProperty] do
  begin
    while TokenID = TptTokenKind.ptIdentifier do
    begin
      ObjectField;
      SEMICOLON;
      ObjectVisibility;
    end;
    while TokenID in [TptTokenKind.ptConstructor, TptTokenKind.ptDestructor, TptTokenKind.ptFunction, TptTokenKind.ptProcedure, TptTokenKind.ptProperty] do
    begin
      case TokenID of
        TptTokenKind.ptConstructor, TptTokenKind.ptDestructor, TptTokenKind.ptFunction, TptTokenKind.ptProcedure:
          ObjectMethodHeading;
        TptTokenKind.ptProperty:
          ObjectProperty;
      end;
    end;
    ObjectVisibility;
  end;
end;

procedure TmwSimplePasPar.ObjectVisibility;
begin
  while ExID in [TptTokenKind.ptPrivate, TptTokenKind.ptProtected, TptTokenKind.ptPublic] do
  begin
    Lexer.InitAhead;
    case Lexer.AheadExID of
      TptTokenKind.ptColon, TptTokenKind.ptComma: ;
    else
      case ExID of
        TptTokenKind.ptPrivate:
          begin
            VisibilityPrivate;
          end;
        TptTokenKind.ptProtected:
          begin
            VisibilityProtected;
          end;
        TptTokenKind.ptPublic:
          begin
            VisibilityPublic;
          end;
      end;
    end;
  end;
end;

procedure TmwSimplePasPar.ObjectField;
begin
  IdentifierList;
  Expected(TptTokenKind.ptColon);
  TypeKind;
  while ExID in [TptTokenKind.ptDeprecated, TptTokenKind.ptLibrary, TptTokenKind.ptPlatform] do // DR 2001-10-20
    case ExID of
      TptTokenKind.ptDeprecated: DirectiveDeprecated;
      TptTokenKind.ptLibrary: DirectiveLibrary;
      TptTokenKind.ptPlatform: DirectivePlatform;
    end;
end;

procedure TmwSimplePasPar.ClassReferenceType;
begin
  Expected(TptTokenKind.ptClass);
  Expected(TptTokenKind.ptOf);
  TypeId;
end;

procedure TmwSimplePasPar.VariantIdentifier;
begin
  case ExID of
    TptTokenKind.ptOleVariant:
      begin
        NextToken;
      end;
    TptTokenKind.ptVariant:
      begin
        NextToken;
      end;
  else
    begin
      SynError(TmwParseError.InvalidVariantIdentifier);
    end;
  end;
end;

procedure TmwSimplePasPar.ProceduralType;
var
  TheTokenID: TptTokenKind;
begin
  case TokenID of
    TptTokenKind.ptFunction:
      begin
        NextToken;
        if TokenID = TptTokenKind.ptRoundOpen then
        begin
          FormalParameterList;
        end;
        Expected(TptTokenKind.ptColon);
        ReturnType;
      end;
    TptTokenKind.ptProcedure:
      begin
        NextToken;
        if TokenID = TptTokenKind.ptRoundOpen then
        begin
          FormalParameterList;
        end;
      end;
  else
    begin
      SynError(TmwParseError.InvalidProceduralType);
    end;
  end;
  if TokenID = TptTokenKind.ptOf then
  begin
    NextToken;
    Expected(TptTokenKind.ptObject);
  end;
  Lexer.InitAhead;
  case TokenID of
    TptTokenKind.ptSemiColon: TheTokenID := Lexer.AheadExID;
  else
    TheTokenID := ExID;
  end;
  while TheTokenID in [TptTokenKind.ptAbstract, TptTokenKind.ptCdecl, TptTokenKind.ptDynamic, TptTokenKind.ptExport, TptTokenKind.ptExternal, TptTokenKind.ptFar,
    TptTokenKind.ptMessage, TptTokenKind.ptNear, TptTokenKind.ptOverload, TptTokenKind.ptOverride, TptTokenKind.ptPascal, TptTokenKind.ptRegister,
    TptTokenKind.ptReintroduce, TptTokenKind.ptSafeCall, TptTokenKind.ptStdcall, TptTokenKind.ptVirtual
    {$IFDEF D8_NEWER}, TptTokenKind.ptStatic{$ENDIF}{$IFDEF D9_NEWER}, TptTokenKind.ptInline{$ENDIF}
    ] do
 // DR 2001-11-14 no checking for deprecated etc. since it's captured by the typedecl
  begin
    if TokenID = TptTokenKind.ptSemiColon then SEMICOLON;
    ProceduralDirective;
    Lexer.InitAhead;
    case TokenID of
      TptTokenKind.ptSemiColon: TheTokenID := Lexer.AheadExID;
    else
      TheTokenID := ExID;
    end;
  end;
end;

procedure TmwSimplePasPar.StringIdentifier;
begin
  case ExID of
    TptTokenKind.ptAnsiString:
      begin
        NextToken;
      end;
    TptTokenKind.ptShortString:
      begin
        NextToken;
      end;
    TptTokenKind.ptWideString:
      begin
        NextToken;
      end;
  else
    begin
      SynError(TmwParseError.InvalidStringIdentifier);
    end;
  end;
end;

procedure TmwSimplePasPar.StringType;
begin
  case TokenID of
    TptTokenKind.ptString:
      begin
        NextToken;
        if TokenID = TptTokenKind.ptSquareOpen then
        begin
          NextToken;
          ConstantExpression;
          Expected(TptTokenKind.ptSquareClose);
        end;
      end;
  else
    begin
      VariableReference;
    end;
  end;
end;

procedure TmwSimplePasPar.PointerType;
begin
  Expected(TptTokenKind.ptPointerSymbol);
  TypeId;
end;

procedure TmwSimplePasPar.StructuredType;
begin
  if TokenID = TptTokenKind.ptPacked then
  begin
    NextToken;
  end;
  case TokenID of
    TptTokenKind.ptArray:
      begin
        ArrayType;
      end;
    TptTokenKind.ptFile:
      begin
        FileType;
      end;
    TptTokenKind.ptRecord:
      begin
        RecordType;
      end;
    TptTokenKind.ptSet:
      begin
        SetType;
      end;
  else
    begin
      SynError(TmwParseError.InvalidStructuredType);
    end;
  end;
end;

procedure TmwSimplePasPar.SimpleType;
begin
  case TokenID of
    TptTokenKind.ptMinus:
      begin
        NextToken;
      end;
    TptTokenKind.ptPlus:
      begin
        NextToken;
      end;
  end;
  case fLexer.TokenID of
    TptTokenKind.ptAsciiChar, TptTokenKind.ptIntegerConst:
      begin
        OrdinalType;
      end;
    TptTokenKind.ptFloat:
      begin
        RealType;
      end;
    TptTokenKind.ptIdentifier:
      begin
        fLexer.InitAhead;
        case Lexer.AheadTokenID of
          TptTokenKind.ptPoint, TptTokenKind.ptSemiColon:
            begin
              TypeId;
            end;
        else
          begin
            SimpleExpression;
            if fLexer.TokenID = TptTokenKind.ptDotDot then
            begin
              NextToken;
              SimpleExpression;
            end;
          end;
        end;
      end;
    TptTokenKind.ptRoundOpen:
      begin
        EnumeratedType;
      end;
    TptTokenKind.ptSquareOpen:
      begin
        SubrangeType;
      end;
  else
    begin
      VariableReference;
    end;
  end;
end;

procedure TmwSimplePasPar.RecordFieldConstant;
begin
  Expected(TptTokenKind.ptIdentifier);
  Expected(TptTokenKind.ptColon);
  TypedConstant;
end;

procedure TmwSimplePasPar.RecordConstant;
begin
  Expected(TptTokenKind.ptRoundOpen);
  RecordFieldConstant;
  while (TokenID = TptTokenKind.ptSemiColon) do
  begin
    SEMICOLON;
    if TokenID <> TptTokenKind.ptRoundClose then //jdj 2.23.2001
      RecordFieldConstant;
  end;
  Expected(TptTokenKind.ptRoundClose);
end;

procedure TmwSimplePasPar.ArrayConstant;
begin
  Expected(TptTokenKind.ptRoundOpen);
  TypedConstant;
  while (TokenID = TptTokenKind.ptComma) do
  begin
    NextToken;
    TypedConstant;
  end;
  Expected(TptTokenKind.ptRoundClose);
end;

procedure TmwSimplePasPar.ClassForward;
begin
  Expected(TptTokenKind.ptClass);
end;

procedure TmwSimplePasPar.DispInterfaceForward;
begin
  Expected(TptTokenKind.ptDispinterface);
end;

procedure TmwSimplePasPar.InterfaceForward;
begin
  Expected(TptTokenKind.ptInterface);
end;

procedure TmwSimplePasPar.ObjectForward;
begin
  Expected(TptTokenKind.ptObject);
end;

procedure TmwSimplePasPar.TypeDeclaration;
begin
  TypeName;
  //For generics
//  if TokenId = ptLower then
//    TypeParams;
  //end generics
  Expected(TptTokenKind.ptEqual);
  if TokenID = TptTokenKind.ptType then
  begin
    ExplicitType;
  end;
  if (TokenID = TptTokenKind.ptIdentifier) and (ExID in [TptTokenKind.ptPublic, TptTokenKind.ptAssembly]) then
  begin
    //visibility identifier for type
    NextToken();
  end;
  if (TokenID = TptTokenKind.ptIdentifier) and (ExID = TptTokenKind.ptPartial) then
  begin
    //partial class
    NextToken();
  end;
  Lexer.InitAhead;
  case TokenID of
    TptTokenKind.ptClass:
      begin
        case Lexer.AheadTokenID of
          TptTokenKind.ptOf:
            begin
              ClassReferenceType;
            end;
          TptTokenKind.ptSemiColon:
            begin
              ClassForward;
            end;
        else
          begin
            ClassType;
          end;
        end;
      end;
    TptTokenKind.ptInterface:
      begin
        case Lexer.AheadTokenID of
          TptTokenKind.ptSemiColon:
            begin
              InterfaceForward;
            end;
        else
          begin
            InterfaceType;
          end;
        end;
      end;
    TptTokenKind.ptDispinterface:
      begin
        case Lexer.AheadTokenID of
          TptTokenKind.ptSemiColon:
            begin
              DispInterfaceForward;
            end;
        else
          begin
            InterfaceType;
          end;
        end;
      end;
    TptTokenKind.ptObject:
      begin
        case Lexer.AheadTokenID of
          TptTokenKind.ptSemiColon:
            begin
              ObjectForward;
            end;
        else
          begin
            ObjectType;
          end;
        end;
      end;
  else
    begin
      {$IFDEF D12_NEWER}
      if ExID = TptTokenKind.ptReference then
        AnonymousMethodType
      else
      {$ENDIF}
      TypeKind;
    end;
  end;
  while ExID in [TptTokenKind.ptDeprecated, TptTokenKind.ptLibrary, TptTokenKind.ptPlatform] do // DR 2001-10-20
    case ExID of
      TptTokenKind.ptDeprecated: DirectiveDeprecated;
      TptTokenKind.ptLibrary: DirectiveLibrary;
      TptTokenKind.ptPlatform: DirectivePlatform;
    end;
end;

procedure TmwSimplePasPar.TypeName;
begin
  Lexer.PreviousIdentifierText := Lexer.Token;
  Expected(TptTokenKind.ptIdentifier);

  if TokenID = TptTokenKind.ptLower then
    TypeParams;
end;

procedure TmwSimplePasPar.ExplicitType;
begin
  Expected(TptTokenKind.ptType);
end;

procedure TmwSimplePasPar.TypeKind;
begin
  case TokenID of
    TptTokenKind.ptAsciiChar, TptTokenKind.ptFloat, TptTokenKind.ptIntegerConst, TptTokenKind.ptMinus, TptTokenKind.ptNil, TptTokenKind.ptPlus, TptTokenKind.ptRoundOpen,
      TptTokenKind.ptSquareOpen, TptTokenKind.ptStringConst:
      begin
        SimpleType;
      end;
    TptTokenKind.ptArray, TptTokenKind.ptFile, TptTokenKind.ptPacked, TptTokenKind.ptRecord, TptTokenKind.ptSet:
      begin
        StructuredType;
      end;
    TptTokenKind.ptFunction, TptTokenKind.ptProcedure:
      begin
        ProceduralType;
      end;
    TptTokenKind.ptIdentifier:
      begin
        Lexer.InitAhead;
        case Lexer.AheadTokenID of
          TptTokenKind.ptPoint, TptTokenKind.ptSemiColon, TptTokenKind.ptLower:
            begin
              TypeId;
            end;
        else
          begin
            SimpleExpression;
            if Lexer.TokenID = TptTokenKind.ptDotDot then
            begin
              NextToken;
              SimpleExpression;
            end;
          end;
        end;
      end;
    TptTokenKind.ptPointerSymbol:
      begin
        PointerType;
      end;
    TptTokenKind.ptString:
      begin
        StringType;
      end;
  else
    begin
      SynError(TmwParseError.InvalidTypeKind);
    end;
  end;
end;

procedure TmwSimplePasPar.TypeArgs;
begin
  Expected(TptTokenKind.ptLower);
  TypeId;
  while TokenID = TptTokenKind.ptComma do
  begin
    NextToken;
    TypeId;
  end;
  Expected(TptTokenKind.ptGreater);
end;

procedure TmwSimplePasPar.TypedConstant;
begin
  case TokenID of
    TptTokenKind.ptRoundOpen:
      begin
        Lexer.InitAhead;
        while Lexer.AheadTokenID <> TptTokenKind.ptSemiColon do
          case Lexer.AheadTokenID of
            TptTokenKind.ptAnd, TptTokenKind.ptBegin, TptTokenKind.ptCase, TptTokenKind.ptColon, TptTokenKind.ptEnd, TptTokenKind.ptElse, TptTokenKind.ptIf, TptTokenKind.ptMinus, TptTokenKind.ptNull,
              TptTokenKind.ptOr, TptTokenKind.ptPlus, TptTokenKind.ptShl, TptTokenKind.ptShr, TptTokenKind.ptSlash, TptTokenKind.ptStar, TptTokenKind.ptWhile, TptTokenKind.ptWith,
              TptTokenKind.ptXor: break;
            TptTokenKind.ptRoundOpen:
              begin
                repeat
                  case Lexer.AheadTokenID of
                    TptTokenKind.ptBegin, TptTokenKind.ptCase, TptTokenKind.ptEnd, TptTokenKind.ptElse, TptTokenKind.ptIf, TptTokenKind.ptNull, TptTokenKind.ptWhile, TptTokenKind.ptWith: break;
                  else
                    begin
                      case Lexer.AheadTokenID of
                        TptTokenKind.ptRoundClose:
                          begin
                            NextToken;
                            break;
                          end;
                      else
                        Lexer.AheadNext;
                      end;
                    end;
                  end;
                until Lexer.AheadTokenID = TptTokenKind.ptRoundClose;
              end;
          else
            Lexer.AheadNext;
          end;
        case Lexer.AheadTokenID of
          TptTokenKind.ptColon:
            begin
              RecordConstant;
            end;
          TptTokenKind.ptNull: ;
          TptTokenKind.ptAnd, TptTokenKind.ptMinus, TptTokenKind.ptOr, TptTokenKind.ptPlus, TptTokenKind.ptShl, TptTokenKind.ptShr, TptTokenKind.ptSlash, TptTokenKind.ptStar, TptTokenKind.ptXor:
            begin
              ConstantExpression;
            end;
        else
          begin
            ArrayConstant;
          end;
        end;
      end;
    TptTokenKind.ptSquareOpen:
      ConstantExpression; // DR 2002-01-11

 { DR: fails with constructed set constants like
    WordDelimiters: set of Char = [#0..#255] - ['a'..'z','A'..'Z','1'..'9','0'];

 (*empty; there mustn't be all fields of a record mentioned*)
   begin
  NextToken;
  if TokenID <> ptSquareClose then
    begin
   case TokenID of
     ptDotDot:
    begin
      NextToken;
      NextToken;
    end;
     else
    NextToken;
    case TokenID of
      ptDotDot:
     begin
       NextToken;
       NextToken;
     end;
    end;
   end;
   while TokenID = ptComma do
     begin
    NextToken;
    NextToken;
    case TokenID of
      ptDotDot:
     begin
       NextToken;
       NextToken;
     end;
    end;
     end;
   Expected(ptSquareClose);
    end
  else NextToken;
   end;}
  else
    begin
      ConstantExpression;
    end;
  end;
end;

procedure TmwSimplePasPar.TypeId;
begin
  Lexer.InitAhead;
  {$IFDEF D8_NEWER} //JThurman 2004-03-03
  while Lexer.AheadTokenID = TptTokenKind.ptPoint do
  begin
    //UnitId;
    NextToken;
    Expected(TptTokenKind.ptPoint);
    Lexer.InitAhead;
  end;
  {$ELSE}
  if Lexer.AheadTokenID = ptPoint then
  begin
    UnitId;
    Expected(ptPoint);
  end;
  {$ENDIF}
  case GenID of
    TptTokenKind.ptBoolean, TptTokenKind.ptByte, TptTokenKind.ptChar, TptTokenKind.ptDWORD, TptTokenKind.ptInt64, TptTokenKind.ptInteger, TptTokenKind.ptLongint,
      TptTokenKind.ptLongword, TptTokenKind.ptPChar, TptTokenKind.ptShortint, TptTokenKind.ptSmallint, TptTokenKind.ptWideChar, TptTokenKind.ptWord:
      begin
        OrdinalIdentifier;
      end;
    TptTokenKind.ptComp, TptTokenKind.ptCurrency, TptTokenKind.ptDouble, TptTokenKind.ptExtended, TptTokenKind.ptReal, TptTokenKind.ptReal48, TptTokenKind.ptSingle:
      begin
        RealIdentifier;
      end;
    TptTokenKind.ptAnsiString, TptTokenKind.ptShortString, TptTokenKind.ptWideString:
      begin
        StringIdentifier;
      end;
    TptTokenKind.ptOleVariant, TptTokenKind.ptVariant:
      begin
        VariantIdentifier;
      end;
    TptTokenKind.ptString:
      begin
        StringType;
      end;
  else
    begin
      //Problem: Delphi 8 allows things like 'Object' to be types
      //when they are fully qualified (as System.Object, etc...), so
      //ptIdentifier doesn't quite work right in this context
      //TODO: Come up with a more elegant solution to the 'Object' dilemna
      {$IFDEF D8_NEWER}//JThurman 2004-03-03
      NextToken;
      {$ELSE}
      Expected(ptIdentifier);
      {$ENDIF}
      if TokenID = TptTokenKind.ptLower then
        TypeArgs;
    end;
  end;
end;

procedure TmwSimplePasPar.ConstantExpression;
begin
  Expression;
end;

procedure TmwSimplePasPar.ResourceDeclaration;
begin
  Identifier;
  Expected(TptTokenKind.ptEqual);
  CharString;
  while ExID in [TptTokenKind.ptDeprecated, TptTokenKind.ptLibrary, TptTokenKind.ptPlatform] do // DR 2002-01-10
    case ExID of
      TptTokenKind.ptDeprecated: DirectiveDeprecated;
      TptTokenKind.ptLibrary: DirectiveLibrary;
      TptTokenKind.ptPlatform: DirectivePlatform;
    end;
end;

procedure TmwSimplePasPar.ConstantDeclaration;
begin
  ConstantName;
  case TokenID of
    TptTokenKind.ptEqual:
      begin
        ConstantEqual;
      end;
    TptTokenKind.ptColon:
      begin
        ConstantColon;
      end;
  else
    begin
      SynError(TmwParseError.InvalidConstantDeclaration);
    end;
  end;
  while ExID in [TptTokenKind.ptDeprecated, TptTokenKind.ptLibrary, TptTokenKind.ptPlatform] do // DR 2001-10-20
    case ExID of
      TptTokenKind.ptDeprecated: DirectiveDeprecated;
      TptTokenKind.ptLibrary: DirectiveLibrary;
      TptTokenKind.ptPlatform: DirectivePlatform;
    end;
end;

procedure TmwSimplePasPar.ConstantColon;
begin
  Expected(TptTokenKind.ptColon);
//JR changed to constant Type
  ConstantType;
  Expected(TptTokenKind.ptEqual);
  ConstantValueTyped;
end;

procedure TmwSimplePasPar.ConstantEqual;
begin
  Expected(TptTokenKind.ptEqual);
  ConstantValue;
end;

procedure TmwSimplePasPar.ConstantValue;
begin
  ConstantExpression;
end;

procedure TmwSimplePasPar.ConstantValueTyped;
begin
  TypedConstant;
end;

procedure TmwSimplePasPar.ConstantName;
begin
  Expected(TptTokenKind.ptIdentifier);
end;

procedure TmwSimplePasPar.ConstantType;
begin
  TypeKind;
end;
procedure TmwSimplePasPar.LabelId;
begin
  case TokenID of
    TptTokenKind.ptIntegerConst:
      begin
        NextToken;
      end;
    TptTokenKind.ptIdentifier:
      begin
        NextToken;
      end;
  else
    begin
      SynError(TmwParseError.InvalidLabelId);
    end;
  end;
end;

procedure TmwSimplePasPar.ProcedureDeclarationSection;
begin
  if TokenID = TptTokenKind.ptClass then
  begin
    NextToken;
  end;
  case TokenID of
    TptTokenKind.ptConstructor:
      begin
        ProcedureMethodDeclaration;
      end;
    TptTokenKind.ptDestructor:
      begin
        ProcedureMethodDeclaration;
      end;
    TptTokenKind.ptProcedure:
      begin
        ProcedureMethodDeclaration;
      end;
    TptTokenKind.ptFunction:
      begin
        FunctionMethodDeclaration;
      end;
    {$IFDEF D8_NEWER} //JThurman 2004-03-2003
    TptTokenKind.ptIdentifier:
      begin
        if Lexer.ExID = TptTokenKind.ptOperator then
        begin
          FunctionMethodDeclaration;
        end
        else
          SynError(TmwParseError.InvalidProcedureDeclarationSection);
      end;
    {$ENDIF}
  else
    begin
      SynError(TmwParseError.InvalidProcedureDeclarationSection);
    end;
  end;
end;

procedure TmwSimplePasPar.LabelDeclarationSection;
begin
  Expected(TptTokenKind.ptLabel);
  LabelId;
  while (TokenID = TptTokenKind.ptComma) do
  begin
    NextToken;
    LabelId;
  end;
  SEMICOLON;
end;

procedure TmwSimplePasPar.ProceduralDirective; //TODO: Add STATIC and FINAL
begin
  case ExID of
    TptTokenKind.ptAbstract:
      begin
        NextToken;
      end;
    TptTokenKind.ptCdecl, TptTokenKind.ptPascal, TptTokenKind.ptRegister, TptTokenKind.ptSafeCall, TptTokenKind.ptStdcall:
      begin
        DirectiveCalling;
      end;
    TptTokenKind.ptExport, TptTokenKind.ptFar, TptTokenKind.ptNear:
      begin
        Directive16Bit;
      end;
    TptTokenKind.ptExternal:
      begin
        ExternalDirective;
      end;
    TptTokenKind.ptDynamic, TptTokenKind.ptMessage, TptTokenKind.ptOverload, TptTokenKind.ptOverride, TptTokenKind.ptReintroduce, TptTokenKind.ptVirtual:
      begin
        DirectiveBinding;
      end;
    TptTokenKind.ptAssembler:
      begin
        NextToken;
      end;
    {$IFDEF D8_NEWER}
    TptTokenKind.ptStatic:
      begin
        NextToken;
      end;
    {$ENDIF}
    {$IFDEF D9_NEWER}
     TptTokenKind.ptInline:
       begin
         NextToken;
       end;
    {$ENDIF}
    TptTokenKind.ptDeprecated:
      DirectiveDeprecated; // DR 2001-10-20
    TptTokenKind.ptLibrary:
      DirectiveLibrary; // DR 2001-10-20
    TptTokenKind.ptPlatform:
      DirectivePlatform; // DR 2001-10-20
    TptTokenKind.ptLocal:
      DirectiveLocal; // DR 2001-11-14
    TptTokenKind.ptVarargs:
      DirectiveVarargs; // DR 2001-11-14
  else
    begin
      SynError(TmwParseError.InvalidProceduralDirective);
    end;
  end;
end;

procedure TmwSimplePasPar.ExportedHeading;
begin
  case TokenID of
    TptTokenKind.ptFunction:
      begin
        FunctionHeading;
      end;
    TptTokenKind.ptProcedure:
      begin
        ProcedureHeading;
      end;
  else
    begin
      SynError(TmwParseError.InvalidExportedHeading);
    end;
  end;
  if TokenID = TptTokenKind.ptSemiColon then SEMICOLON;
  case ExID of
    TptTokenKind.ptForward:
      begin
        ForwardDeclaration; //jdj added 02/07/2001
//        NextToken;
//        SEMICOLON;
      end;
    TptTokenKind.ptAssembler:
      begin
        NextToken;
        SEMICOLON;
        if ExID = TptTokenKind.ptForward then
          ForwardDeclaration; //jdj added 02/07/2001
      end;
  else  //TODO: Add STATIC and FINAL
    while ExID in [TptTokenKind.ptAbstract, TptTokenKind.ptCdecl, TptTokenKind.ptDynamic, TptTokenKind.ptExport, TptTokenKind.ptExternal, TptTokenKind.ptFar,
      TptTokenKind.ptMessage, TptTokenKind.ptNear, TptTokenKind.ptOverload, TptTokenKind.ptOverride, TptTokenKind.ptPascal, TptTokenKind.ptRegister,
      TptTokenKind.ptReintroduce, TptTokenKind.ptSafeCall, TptTokenKind.ptStdcall, TptTokenKind.ptVirtual,
      TptTokenKind.ptDeprecated, TptTokenKind.ptLibrary, TptTokenKind.ptPlatform, // DR 2001-10-20
      TptTokenKind.ptLocal, TptTokenKind.ptVarargs // DR 2001-11-14
      {$IFDEF D8_NEWER}, TptTokenKind.ptStatic{$ENDIF}{$IFDEF D9_NEWER}, TptTokenKind.ptInline{$ENDIF}
      ] do
    begin
      ProceduralDirective;
      if TokenID = TptTokenKind.ptSemiColon then SEMICOLON;
    end;
    if ExID = TptTokenKind.ptForward then
      ForwardDeclaration; //jdj added 02/07/2001
  end;
end;

procedure TmwSimplePasPar.FunctionHeading;
begin
  Expected(TptTokenKind.ptFunction);
  FunctionProcedureName;
  if TokenID = TptTokenKind.ptRoundOpen then
  begin
    FormalParameterList;
  end;
  Expected(TptTokenKind.ptColon);
  ReturnType;
end;

procedure TmwSimplePasPar.ProcedureHeading;
begin
  Expected(TptTokenKind.ptProcedure);
  FunctionProcedureName;
  if TokenID = TptTokenKind.ptRoundOpen then
  begin
    FormalParameterList;
  end;

end;

procedure TmwSimplePasPar.VarSection;
begin
  case TokenID of
    TptTokenKind.ptThreadvar:
      begin
        NextToken;
      end;
    TptTokenKind.ptVar:
      begin
        NextToken;
      end;
  else
    begin
      SynError(TmwParseError.InvalidVarSection);
    end;
  end;
  {$IFDEF D8_NEWER}//JThurman 2004-03-22
  while TokenID in [TptTokenKind.ptIdentifier, TptTokenKind.ptSquareOpen] do
  begin
    if TokenID = TptTokenKind.ptSquareOpen then
      CustomAttribute
    else
    begin
      VarDeclaration;
      SEMICOLON;
    end;
  end;
  {$ELSE}
  while TokenID = ptIdentifier do
  begin
    VarDeclaration;
    SEMICOLON;
  end;
  {$ENDIF}
end;

procedure TmwSimplePasPar.TypeSection;
begin
  Expected(TptTokenKind.ptType);  
  while ((TokenID = TptTokenKind.ptIdentifier) and (Lexer.ExID in ExTypes)) or
        (Lexer.TokenID = TptTokenKind.ptSquareOpen) do
  begin
    if TokenID = TptTokenKind.ptSquareOpen then
      CustomAttribute
    else
    begin
      TypeDeclaration;
      if TokenID = TptTokenKind.ptEqual then
        TypedConstant;
      SEMICOLON;
    end;
  end;
end;

procedure TmwSimplePasPar.TypeParamDecl;
begin
  TypeParamList;
  if TokenID = TptTokenKind.ptColon then
  begin
    NextToken;
    ConstraintList;
  end;
end;

procedure TmwSimplePasPar.TypeParamDeclList;
begin
  TypeParamDecl;
  while TokenID = TptTokenKind.ptSemiColon do
  begin
    NextToken;
    TypeParamDecl;
  end;
end;

procedure TmwSimplePasPar.TypeParamList;
begin
  {$IFDEF D8_NEWER}
  if TokenID = TptTokenKind.ptSquareOpen then
    AttributeSection;
  {$ENDIF}
  Identifier;
  while TokenID = TptTokenKind.ptComma do
  begin
    NextToken;
    {$IFDEF D8_NEWER}
    if TokenID = TptTokenKind.ptSquareOpen then
      AttributeSection;
    {$ENDIF}
    Identifier;
  end;
end;

procedure TmwSimplePasPar.TypeParams;
begin
  Expected(TptTokenKind.ptLower);
  TypeParamDeclList;
  Expected(TptTokenKind.ptGreater);
end;

procedure TmwSimplePasPar.ConstSection;
begin
  case TokenID of
    TptTokenKind.ptConst:
      begin
        NextToken;
        {$IFDEF D8_NEWER} //JThurman 2004-03-22
        while TokenID in [TptTokenKind.ptIdentifier, TptTokenKind.ptSquareOpen] do
        begin
          if TokenID = TptTokenKind.ptSquareOpen then
            CustomAttribute
          else
          begin
            ConstantDeclaration;
            SEMICOLON;
          end;
        end;
        {$ELSE}
        while (TokenID = ptIdentifier) do
        begin
          ConstantDeclaration;
          SEMICOLON;
        end;
        {$ENDIF}
      end;
    TptTokenKind.ptResourcestring:
      begin
        NextToken;
        while (TokenID = TptTokenKind.ptIdentifier) do
        begin
          ResourceDeclaration;
          SEMICOLON;
        end;
      end
  else
    begin
      SynError(TmwParseError.InvalidConstSection);
    end;
  end;
end;

procedure TmwSimplePasPar.InterfaceDeclaration;
begin
  case TokenID of
    TptTokenKind.ptConst:
      begin
        ConstSection;
      end;
    TptTokenKind.ptFunction:
      begin
        ExportedHeading;
      end;
    TptTokenKind.ptProcedure:
      begin
        ExportedHeading;
      end;
    TptTokenKind.ptResourcestring:
      begin
        ConstSection;
      end;
    TptTokenKind.ptType:
      begin
        TypeSection;
      end;
    TptTokenKind.ptThreadvar:
      begin
        VarSection;
      end;
    TptTokenKind.ptVar:
      begin
        VarSection;
      end;
    TptTokenKind.ptExports:
      begin
        ExportsClause;
      end;
    {$IFDEF D8_NEWER} //JThurman 2004-03-03
    TptTokenKind.ptSquareOpen:
      begin
        CustomAttribute;
      end;
    {$ENDIF}
  else
    begin
      SynError(TmwParseError.InvalidInterfaceDeclaration);
    end;
  end;
end;

procedure TmwSimplePasPar.ExportsElement;
begin
  Expected(TptTokenKind.ptIdentifier);
  //  if TokenID = ptIndex then
  if fLexer.ExID = TptTokenKind.ptIndex then //jdj 20001207
  begin
    NextToken;
    Expected(TptTokenKind.ptIntegerConst);
  end;
  //  if TokenID = ptName then
  if fLexer.ExID = TptTokenKind.ptName then //jdj 20001207
  begin
    NextToken;
    CharString;
  end;
  //  if TokenID = ptResident then
  if fLexer.ExID = TptTokenKind.ptResident then //jdj 20001207
  begin
    NextToken;
  end;
end;

procedure TmwSimplePasPar.CompoundStatement;
begin
  Expected(TptTokenKind.ptBegin);
  StatementList;
  Expected(TptTokenKind.ptEnd);
end;

procedure TmwSimplePasPar.ExportsClause;
begin
  Expected(TptTokenKind.ptExports);
  ExportsElement;
  while TokenID = TptTokenKind.ptComma do
  begin
    NextToken;
    ExportsElement;
  end;
  SEMICOLON;
end;

procedure TmwSimplePasPar.ContainsClause;
begin
  ExpectedEx(TptTokenKind.ptContains);
  ContainsStatement;
  while TokenID = TptTokenKind.ptComma do
  begin
    NextToken;
    ContainsStatement;
  end;
  SEMICOLON;
end;

procedure TmwSimplePasPar.ContainsStatement;
begin
  ContainsIdentifier;
  if fLexer.TokenID = TptTokenKind.ptIn then
  begin
    NextToken;
    ContainsExpression;
  end;
end;

procedure TmwSimplePasPar.ContainsIdentifier;
begin
  Expected(TptTokenKind.ptIdentifier);
end;

procedure TmwSimplePasPar.ContainsExpression;
begin
  ConstantExpression;
end;

procedure TmwSimplePasPar.RequiresClause;
begin
  ExpectedEx(TptTokenKind.ptRequires);
  RequiresIdentifier;
  while TokenID = TptTokenKind.ptComma do
  begin
    NextToken;
    RequiresIdentifier;
  end;
  SEMICOLON;
end;

procedure TmwSimplePasPar.RequiresIdentifier;
begin
  Expected(TptTokenKind.ptIdentifier);
  {$IFDEF D8_NEWER}
  while Lexer.TokenID = TptTokenKind.ptPoint do
  begin
    NextToken;
    Expected(TptTokenKind.ptIdentifier);
  end;
  {$ENDIF}
end;

procedure TmwSimplePasPar.InitializationSection;
begin
  case TokenID of
    TptTokenKind.ptInitialization:
      begin
        NextToken;
        StatementList;
        if TokenID = TptTokenKind.ptFinalization then
        begin
          NextToken;
          StatementList;
        end;
        Expected(TptTokenKind.ptEnd);
      end;
    TptTokenKind.ptBegin:
      begin
        CompoundStatement;
      end;
    TptTokenKind.ptEnd:
      begin
        NextToken;
      end;
  else
    begin
      SynError(TmwParseError.InvalidInitializationSection);
    end;
  end;
end;

procedure TmwSimplePasPar.ImplementationSection;
begin
  Expected(TptTokenKind.ptImplementation);
  if TokenID = TptTokenKind.ptUses then
  begin
    UsesClause;
  end;
  while TokenID in [TptTokenKind.ptClass, TptTokenKind.ptConst, TptTokenKind.ptConstructor, TptTokenKind.ptDestructor, TptTokenKind.ptFunction,
    TptTokenKind.ptLabel, TptTokenKind.ptProcedure, TptTokenKind.ptResourcestring, TptTokenKind.ptThreadvar, TptTokenKind.ptType, TptTokenKind.ptVar,
    TptTokenKind.ptExports
    {$IFDEF D8_NEWER}//JThurman 2004-03-22
    , TptTokenKind.ptSquareOpen
    {$ENDIF}
    ] do //ptResourceString added jdj
  begin
    DeclarationSection;
  end;
end;

procedure TmwSimplePasPar.InterfaceSection;
begin
  Expected(TptTokenKind.ptInterface);
  if TokenID = TptTokenKind.ptUses then
  begin
    UsesClause;
  end;
  while TokenID in [TptTokenKind.ptConst, TptTokenKind.ptFunction, TptTokenKind.ptResourcestring, TptTokenKind.ptProcedure,
    TptTokenKind.ptThreadvar, TptTokenKind.ptType, TptTokenKind.ptVar, TptTokenKind.ptExports, TptTokenKind.ptSquareOpen] do
  begin
    InterfaceDeclaration;
  end;
end;

procedure TmwSimplePasPar.IdentifierList;
begin
  Identifier; // DR 2001-10-20
  while TokenID = TptTokenKind.ptComma do
  begin
    NextToken;
    Identifier;
  end;
end;

procedure TmwSimplePasPar.QualifiedIdentifierList;
begin
  QualifiedIdentifier;
  while (TokenID = TptTokenKind.ptComma) do
  begin
    NextToken;
    QualifiedIdentifier;
  end;
end;

procedure TmwSimplePasPar.CharString;
begin //updated mw 2/22/00, JThurman 6/24/2004
  case TokenID of
    TptTokenKind.ptAsciiChar, TptTokenKind.ptIdentifier, TptTokenKind.ptRoundOpen, TptTokenKind.ptStringConst:
      while TokenID in
        [TptTokenKind.ptAsciiChar, TptTokenKind.ptIdentifier, TptTokenKind.ptPlus, TptTokenKind.ptRoundOpen, TptTokenKind.ptStringConst,
        TptTokenKind.ptString] do
      begin
        case TokenID of
          TptTokenKind.ptIdentifier, TptTokenKind.ptRoundOpen:
            begin
              VariableReference;
            end;
          TptTokenKind.ptString: //JT
            begin
              StringStatement;
            end;
        else
          NextToken;
        end;
        {$IFDEF D8_NEWER}
        if Lexer.TokenID = TptTokenKind.ptPoint then
        begin
          NextToken;
          VariableReference;
        end;
        {$ENDIF}
      end;
  else
    begin
      SynError(TmwParseError.InvalidCharString);
    end;
  end;
end;

(*procedure TmwSimplePasPar.CharString;
begin //updated mw 2/22/00
  case TokenID of
    ptAsciiChar, ptIdentifier, ptRoundOpen, ptStringConst:
      while TokenID in
        [ptAsciiChar, ptIdentifier, ptPlus, ptRoundOpen, ptStringConst] do
      begin
        case TokenID of
          ptIdentifier, ptRoundOpen:
            begin
              VariableReference;
            end;
        else
          NextToken;
        end;
      end;
  else
    begin
      SynError(InvalidCharString);
    end;
  end;
end;*)

(*procedure TmwSimplePasPar.CharString;
begin
  case TokenID of
    ptAsciiChar, ptStringConst:
      while TokenID in [ptAsciiChar, ptPlus, ptStringConst] do
      begin
        case TokenID of
          ptPlus:
            begin
              NextToken;
              if TokenID = ptIdentifier then
              begin
                VariableReference;
              end;
            end;
        else
          begin
            NextToken;
          end;
        end;
      end;
    ptIdentifier:
      begin
        VariableReference;
        case TokenID of
          ptPlus:
            begin
              NextToken;
              while TokenID in [ptAsciiChar, ptPlus, ptStringConst] do
              begin
                case TokenID of
                  ptPlus:
                    begin
                      NextToken;
                      if TokenID = ptIdentifier then
                      begin
                        VariableReference;
                      end;
                    end;
                else
                  begin
                    NextToken;
                  end;
                end;
              end;
   end;
        end;
      end
  else
    begin
      SynError(InvalidCharString);
    end;
  end;
end;*)

procedure TmwSimplePasPar.IncludeFile;
begin
  while TokenID <> TptTokenKind.ptNull do
    case TokenID of
      TptTokenKind.ptClass:
        begin
          ProcedureDeclarationSection;
        end;
      TptTokenKind.ptConst:
        begin
          ConstSection;
        end;
      TptTokenKind.ptConstructor:
        begin
          ProcedureDeclarationSection;
        end;
      TptTokenKind.ptDestructor:
        begin
          ProcedureDeclarationSection;
        end;
      TptTokenKind.ptExports:
        begin
          ExportsClause;
        end;
      TptTokenKind.ptFunction:
        begin
          ProcedureDeclarationSection;
        end;
      TptTokenKind.ptIdentifier:
        begin
          Lexer.InitAhead;
          if Lexer.AheadTokenID in [TptTokenKind.ptColon, TptTokenKind.ptEqual] then
          begin
            ConstantDeclaration;
            if TokenID = TptTokenKind.ptSemiColon then SEMICOLON;
          end
          else
            NextToken;
        end;
      TptTokenKind.ptLabel:
        begin
          LabelDeclarationSection;
        end;
      TptTokenKind.ptProcedure:
        begin
          ProcedureDeclarationSection;
        end;
      TptTokenKind.ptResourcestring:
        begin
          ConstSection;
        end;
      TptTokenKind.ptType:
        begin
          TypeSection;
        end;
      TptTokenKind.ptThreadvar:
        begin
          VarSection;
        end;
      TptTokenKind.ptVar:
        begin
          VarSection;
        end;
      TptTokenKind.ptInterface:
         InterfaceSection;
      TptTokenKind.ptImplementation:
         ImplementationSection;
    else
      begin
        NextToken;
      end;
    end;
end;

procedure TmwSimplePasPar.SkipSpace; //XM Jul-2000
begin
  Expected(TptTokenKind.ptSpace);
  while TokenID in [TptTokenKind.ptSpace] do
    Lexer.Next;
end;

procedure TmwSimplePasPar.SkipCRLFco; //XM Jul-2000
begin
  Expected(TptTokenKind.ptCRLFCo);
  while TokenID in [TptTokenKind.ptCRLFCo] do
    Lexer.Next;
end;

procedure TmwSimplePasPar.SkipCRLF; //XM Jul-2000
begin
  Expected(TptTokenKind.ptCRLF);
  while TokenID in [TptTokenKind.ptCRLF] do
    Lexer.Next;
end;

procedure TmwSimplePasPar.ClassClass;
begin
  Expected(TptTokenKind.ptClass);
end;

procedure TmwSimplePasPar.PropertyDefault;
begin
  ExpectedEx(TptTokenKind.ptDefault);
end;

procedure TmwSimplePasPar.DispIDSpecifier; // DR 2001-07-26
begin
  ExpectedEx(TptTokenKind.ptDispid);
  ConstantExpression;
end;

procedure TmwSimplePasPar.IndexSpecifier;
begin
  ExpectedEx(TptTokenKind.ptIndex);
  ConstantExpression;
end;

procedure TmwSimplePasPar.ClassTypeEnd;
begin
end;

procedure TmwSimplePasPar.ObjectTypeEnd;
begin
end;

procedure TmwSimplePasPar.DirectiveDeprecated;
begin
  ExpectedEx(TptTokenKind.ptDeprecated);
  if TokenID = TptTokenKind.ptStringConst then
    NextToken;
end;

procedure TmwSimplePasPar.DirectiveLibrary;
begin
  ExpectedEx(TptTokenKind.ptLibrary);
end;

procedure TmwSimplePasPar.DirectivePlatform;
begin
  ExpectedEx(TptTokenKind.ptPlatform);
end;

procedure TmwSimplePasPar.EnumeratedTypeItem;
begin
  QualifiedIdentifier;
  if TokenID = TptTokenKind.ptEqual then
  begin
    Expected(TptTokenKind.ptEqual);
    ConstantExpression;
  end;
end;

procedure TmwSimplePasPar.Identifier;
begin
  Expected(TptTokenKind.ptIdentifier);
end;

procedure TmwSimplePasPar.DirectiveLocal;
begin
  ExpectedEx(TptTokenKind.ptLocal);
end;

procedure TmwSimplePasPar.DirectiveVarargs;
begin
  ExpectedEx(TptTokenKind.ptVarargs);
end;

procedure TmwSimplePasPar.AncestorId;
begin
  // !! Although I re-added this function I modified it
  // so that it now calls QualifiedIdentifier, per DR's change
  QualifiedIdentifier;
end;

procedure TmwSimplePasPar.AncestorIdList;
begin
  // !! Added this function back in
  AncestorId;
  while(TokenID = TptTokenKind.ptComma) do
    begin
      NextToken;
      AncestorId;
    end;
end;



procedure TmwSimplePasPar.AnonymousMethod;
begin
  case TokenID of
    TptTokenKind.ptFunction:
      begin
        NextToken;
        if TokenID = TptTokenKind.ptRoundOpen then
          FormalParameterList;
        Expected(TptTokenKind.ptColon);
        ReturnType;
      end;
    TptTokenKind.ptProcedure:
      begin
        NextToken;
        if TokenID = TptTokenKind.ptRoundOpen then
          FormalParameterList;
      end;
  end;
  Block;
end;

procedure TmwSimplePasPar.AnonymousMethodType;
begin
{$IFDEF D11_NEWER}
  ExpectedEx(TptTokenKind.ptReference); //ExID = ptReference
  Expected(TptTokenKind.ptTo);
  case TokenID of
    TptTokenKind.ptProcedure:
      begin
        NextToken;
        if TokenID = TptTokenKind.ptRoundOpen then
          FormalParameterList;
      end;
    TptTokenKind.ptFunction:
      begin
        NextToken;
        if TokenID = TptTokenKind.ptRoundOpen then
          FormalParameterList;
        Expected(TptTokenKind.ptColon);
        ReturnType;
      end;
  end;
{$ENDIF}
end;

procedure TmwSimplePasPar.AddDefine(const ADefine: String);
begin
  FDefines.Add(ADefine);
end;

procedure TmwSimplePasPar.RemoveDefine(const ADefine: String);
var
  I: Integer;
begin
  I := FDefines.IndexOf(ADefine);
  if I > -1 then
    FDefines.Delete(I);
end;

function TmwSimplePasPar.IsDefined(const ADefine: String): Boolean;
begin
  Result := FDefines.IndexOf(ADefine) > -1;
end;

procedure TmwSimplePasPar.ClearDefines;
var
  Frame: PDefineRec;
begin
  FDefines.Clear;
  FDefineStack := 0;
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
end;

procedure TmwSimplePasPar.InitAhead;
begin
  if AheadParse = nil then
    AheadParse := TmwSimplePasPar.Create;
  AheadParse.Lexer.InitFrom(Lexer);
end;

procedure TmwSimplePasPar.InitDefines;
begin
  ClearDefines;
  //Set up the defines that are defined by the compiler
//  {$IFDEF VER130}
//  AddDefine('VER130');
//  {$ENDIF}
//  {$IFDEF VER140}
//  AddDefine('VER140');
//  {$ENDIF}
//  {$IFDEF VER150}
//  AddDefine('VER150');
//  {$ENDIF}
//  {$IFDEF VER160}
//  AddDefine('VER160');
//  {$ENDIF}
//  {$IFDEF VER170}
//  AddDefine('VER170');
//  {$ENDIF}
//  {$IFDEF VER180}
//  AddDefine('VER180');
//  {$ENDIF}
//  {$IFDEF VER185}
//  AddDefine('VER185');
//  {$ENDIF}
//  {$IFDEF VER190}
//  AddDefine('VER190');
//  {$ENDIF}
//  {$IFDEF VER200}
//  AddDefine('VER200');
//  {$ENDIF}
//  {$IFDEF WIN32}
//  AddDefine('WIN32');
//  {$ENDIF}
//  {$IFDEF LINUX}
//  AddDefine('LINUX');
//  {$ENDIF}
//  {$IFDEF CPU386}
//  AddDefine('CPU386');
//  {$ENDIF}
//  {$IFDEF MSWINDOWS}
//  AddDefine('MSWINDOWS');
//  {$ENDIF}
//  {$IFDEF CONDITIONALEXPRESSIONS}
//  AddDefine('CONDITIONALEXPRESSIONS');
//  {$ENDIF}
end;

procedure TmwSimplePasPar.EnterDefineBlock(ADefined: Boolean);
var
  StackFrame: PDefineRec;
begin
  Exit;
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
//  if not ADefined then
//  begin
//    Inc(FDefineStack);
//    repeat
//      NextToken;
//      if TokenID = ptNull then
//        Break;
//    until FDefineStack = 0;
//  end
//  else
//    NextToken;
  if not ADefined then
    inc(FDefineStack);

//  while FDefineStack > 0 do
//  begin
//    NextToken;
//    if TokenID = ptNull then
//      Break;
//  end;
end;

procedure TmwSimplePasPar.ExitDefineBlock;
var
  StackFrame: PDefineRec;
begin
  Exit;
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

{$IFDEF D8_NEWER} //JThurman 2004-03-03

procedure TmwSimplePasPar.GlobalAttributes;
begin
  GlobalAttributeSections;
end;

procedure TmwSimplePasPar.GlobalAttributeSections;
begin
  while TokenID = TptTokenKind.ptSquareOpen do
    GlobalAttributeSection;
end;

procedure TmwSimplePasPar.GlobalAttributeSection;
begin
  Expected(TptTokenKind.ptSquareOpen);
  GlobalAttributeTargetSpecifier;
  AttributeList;
  while TokenID = TptTokenKind.ptComma do
  begin
    Expected(TptTokenKind.ptComma);
    GlobalAttributeTargetSpecifier;
    AttributeList;
  end;
  Expected(TptTokenKind.ptSquareClose);
end;

procedure TmwSimplePasPar.GlobalAttributeTargetSpecifier;
begin
  GlobalAttributeTarget;
  Expected(TptTokenKind.ptColon);
end;

procedure TmwSimplePasPar.GlobalAttributeTarget;
begin
  Expected(TptTokenKind.ptIdentifier);
end;

procedure TmwSimplePasPar.Attributes;
begin
  AttributeSections;
end;

procedure TmwSimplePasPar.AttributeSections;
begin
  while TokenID = TptTokenKind.ptSquareOpen do
    AttributeSection;
end;

procedure TmwSimplePasPar.AttributeSection;
begin
  Expected(TptTokenKind.ptSquareOpen);
  Lexer.InitAhead;
  if Lexer.AheadTokenID = TptTokenKind.ptColon then
    AttributeTargetSpecifier;
  AttributeList;
  while TokenID = TptTokenKind.ptComma do
  begin
    Lexer.InitAhead;
    if Lexer.AheadTokenID = TptTokenKind.ptColon then
      AttributeTargetSpecifier;
    AttributeList;
  end;
  Expected(TptTokenKind.ptSquareClose);
end;

procedure TmwSimplePasPar.AttributeTargetSpecifier;
begin
  AttributeTarget;
  Expected(TptTokenKind.ptColon);
end;

procedure TmwSimplePasPar.AttributeTarget;
begin
  case TokenID of
    TptTokenKind.ptProperty:
      Expected(TptTokenKind.ptProperty);
    TptTokenKind.ptType:
      Expected(TptTokenKind.ptType);
    else
      Expected(TptTokenKind.ptIdentifier);
  end;
end;

procedure TmwSimplePasPar.AttributeList;
begin
  Attribute;
  while TokenID = TptTokenKind.ptComma do
  begin
    Expected(TptTokenKind.ptComma);
    AttributeList;
  end;
end;

procedure TmwSimplePasPar.Attribute;
begin
  AttributeName;
  if TokenID = TptTokenKind.ptRoundOpen then
    AttributeArguments;
end;

procedure TmwSimplePasPar.AttributeName;
begin
  case TokenID of
    TptTokenKind.ptIn, TptTokenKind.ptOut, TptTokenKind.ptConst, TptTokenKind.ptVar:
      NextToken;
  else
    Expected(TptTokenKind.ptIdentifier);
  end;
end;

procedure TmwSimplePasPar.AttributeArguments;
begin
  Expected(TptTokenKind.ptRoundOpen);
  if TokenID <> TptTokenKind.ptRoundClose then
  begin
    Lexer.InitAhead;
    if Lexer.AheadTokenID = TptTokenKind.ptEqual then
      NamedArgumentList
    else
      PositionalArgumentList;
    if Lexer.TokenID = TptTokenKind.ptEqual then
      NamedArgumentList;
  end;
  Expected(TptTokenKind.ptRoundClose);
end;

procedure TmwSimplePasPar.PositionalArgumentList;
begin
  PositionalArgument;
  while TokenID = TptTokenKind.ptComma do
  begin
    Expected(TptTokenKind.ptComma);
    PositionalArgument;
  end;
end;

procedure TmwSimplePasPar.PositionalArgument;
begin
  AttributeArgumentExpression;
end;

procedure TmwSimplePasPar.NamedArgumentList;
begin
  NamedArgument;
  while TokenID = TptTokenKind.ptComma do
  begin
    Expected(TptTokenKind.ptComma);
    NamedArgument;
  end;
end;

procedure TmwSimplePasPar.NamedArgument;
begin
  Expected(TptTokenKind.ptIdentifier);
  Expected(TptTokenKind.ptEqual);
  AttributeArgumentExpression;
end;

procedure TmwSimplePasPar.AttributeArgumentExpression;
begin
  Expression;
end;

procedure TmwSimplePasPar.CustomAttribute;
begin
  AttributeSection;//TODO: Global vs. Local attributes
{  Lexer.InitAhead;
  if (Lexer.AheadToken = 'assembly') or (Lexer.AheadToken = 'module') then
    GlobalAttributeSections
  else}
    AttributeSections;

end;
{$ENDIF}

end.

