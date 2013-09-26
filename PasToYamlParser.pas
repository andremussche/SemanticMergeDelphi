unit PasToYamlParser;

interface

uses
  {$IF NOT OXYGENE}
  Classes, Generics.Collections,
  {$ENDIF}
  CastaliaSimplePasPar, SemanticYaml;

type
  TStringEvent  = reference to procedure(const aText: String);
  TReplaceEvent = reference to procedure(const aSearch, aReplace: String);

  TPas2YamlParser = class(TmwSimplePasPar)
  protected
    FIndent: String;
    procedure EnterHandler(const aName: String);
    procedure ExitHandler(const aName: String);
  protected
    FUnitName: String;
    FCurrentType: String;
    FObjectNameOfMethod: String;
    FFunctionProcedureName: String;
    //FLastLine, FLastLineChar, FLastChar: Integer;
  private
    FOnDebugOutput: TStringEvent;

    FYamlMaster: TSemanticMasterYaml;
    FPrevParent,
    FCurrentParent: TSemanticParentYaml;
    FPrevItem,
    FCurrentItem: TSemanticItemYaml;
    FPrevSpan: TSemanticSpan;
    FPrevClassVisibility: TSemanticParentYaml;
    function  GetYaml: TSemanticMasterYaml;
    procedure UpdatePrevYamlLocation(aNode: TBaseSemanticYaml);
    procedure UpdateNextYamlLocation(aNode: TBaseSemanticYaml);

    var FCurrentMethod: TSemanticItemYaml;
    function  ProcessItem_Before(const aType: String; aExactPos: Boolean; aDebugHandler: String): TSemanticItemYaml;
    procedure ProcessItem_Next(const aItemYaml: TSemanticItemYaml; aExactPos: Boolean;
      aDebugHandler: String);

    var
      FParentRecursion: Integer;
      FParentStack: TStack<TSemanticParentYaml>;
    function  ProcessParent_Before(const aType, aDebugHandler: String): TSemanticParentYaml;
    procedure ProcessParent_Next(const aParentYaml: TSemanticParentYaml; aIncludeLastLine: Boolean;
      aDebugHandler: String);
  public
    procedure AfterConstruction; override;
    {$IF NOT OXYGENE}
    destructor Destroy; override;
    {$ENDIF}
    procedure Run(aUnitName: String; SourceStream: TCustomMemoryStream); override;

    property  Yaml: TSemanticMasterYaml read GetYaml;
    property  OnDebugOutput: TStringEvent read FOnDebugOutput write FOnDebugOutput;
  protected {parser overrides, long list!}
    (*
    procedure Expected(Sym: TptTokenKind); overide;
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
    *)
    procedure AccessSpecifier; override;
    procedure AdditiveOperator; override;
    procedure AncestorIdList; override; // !! Added ancestorIdList back in...
    procedure AncestorId; override; // !! Added ancestorId back in...
    procedure AnonymousMethod; override;
    procedure AnonymousMethodType; override;
    procedure ArrayConstant; override;
    procedure ArrayType; override;
    procedure AsmStatement; override;
    procedure Block; override;
    procedure CaseLabel; override;
    procedure CaseSelector; override;
    procedure CaseStatement; override;
    procedure CharString; override;
    procedure ClassField; override;
    procedure ClassForward; override;
    procedure ClassFunctionHeading; override;
    procedure ClassHeritage; override;
    procedure ClassMemberList; override;
    procedure ClassMethodDirective; override;
    procedure ClassMethodHeading; override;
    procedure ClassMethodOrProperty; override;
    procedure ClassMethodResolution; override;
    procedure ClassProcedureHeading; override;
    procedure ClassClass; override;
    procedure ClassProperty; override;
    procedure ClassReferenceType; override;
    procedure ClassType; override;
    procedure ClassTypeEnd; override; // DR 2001-07-31
    procedure ClassVisibility; override;
    procedure CompoundStatement; override;
    procedure ConstantColon; override;
    procedure ConstantDeclaration; override;
    procedure ConstantEqual; override;
    procedure ConstantExpression; override;
    procedure ConstantName; override;
//JR added constant type
    procedure ConstantType; override;
    procedure ConstantValue; override;
    procedure ConstantValueTyped; override;
    procedure ConstParameter; override;
    procedure ConstructorHeading; override;
    procedure ConstructorName; override;
    procedure ConstSection; override;
    procedure ContainsClause; override;
    procedure ContainsExpression; override;
    procedure ContainsIdentifier; override;
    procedure ContainsStatement; override;
    {$IFDEF D8_NEWER}
    procedure CustomAttribute; override; //JThurman 2004-03-03
    {$ENDIF}
    procedure DeclarationSection; override;
    procedure Designator; override;
    procedure DestructorHeading; override;
    procedure DestructorName; override;
    procedure Directive16Bit; override;
    procedure DirectiveBinding; override;
    procedure DirectiveCalling; override;
    procedure DirectiveDeprecated; override; // DR 2001-10-20
    procedure DirectiveLibrary; override; // DR 2001-10-20
    procedure DirectiveLocal; override; // DR 2001-11-14
    procedure DirectivePlatform; override; // DR 2001-10-20
    procedure DirectiveVarargs; override; // DR 2001-11-14
    procedure DispInterfaceForward; override;
    procedure DispIDSpecifier; override; // DR 2001-07-26
    procedure EmptyStatement; override;
    procedure EnumeratedType; override;
    procedure EnumeratedTypeItem; override; // DR 2001-10-29
    procedure ExceptBlock; override;
    procedure ExceptionBlockElseBranch; override;
    procedure ExceptionClassTypeIdentifier; override;
    procedure ExceptionHandler; override;
    procedure ExceptionHandlerList; override;
    procedure ExceptionIdentifier; override;
    procedure ExceptionVariable; override;
    procedure ExplicitType; override; // !! changed spelling to "Explicit"
    procedure ExportedHeading; override;
    procedure ExportsClause; override;
    procedure ExportsElement; override;
    procedure Expression; override;
    procedure ExpressionList; override;
    procedure ExternalDirective; override;
    procedure ExternalDirectiveThree; override;
    procedure ExternalDirectiveTwo; override;
    procedure Factor; override;
    procedure FieldDeclaration; override;
    procedure FieldList; override;
    procedure FieldNameList; override;
    procedure FieldName; override;
    procedure FileType; override;
    procedure FormalParameterList; override;
    procedure FormalParameterSection; override;
    procedure ForStatement; override;
    procedure ForwardDeclaration; override; {GLC: corrected spelling}
    procedure FunctionHeading; override;
    procedure FunctionMethodDeclaration; override;
    procedure FunctionMethodName; override;
    procedure FunctionProcedureBlock; override;
    procedure FunctionProcedureName; override;
    procedure Identifier; override;
    procedure IdentifierList; override;
    procedure IfStatement; override;
    procedure ImplementationSection; override;
    procedure IncludeFile; override;
    procedure IndexSpecifier; override; // DR 2001-07-26
    procedure InheritedStatement; override;
    procedure InitializationSection; override;
    procedure InlineStatement; override;
    procedure InParameter; override;
    procedure InterfaceDeclaration; override;
    procedure InterfaceForward; override;
    procedure InterfaceGUID; override;
    procedure InterfaceHeritage; override;
    procedure InterfaceMemberList; override;
    procedure InterfaceSection; override;
    procedure InterfaceType; override;
    procedure LabelDeclarationSection; override;
    procedure LabeledStatement; override;
    procedure LabelId; override;
    procedure LibraryFile; override;
    procedure MainUsedUnitExpression; override;
    procedure MainUsedUnitName; override;
    procedure MainUsedUnitStatement; override;
    procedure MainUsesClause; override;
    procedure MultiplicativeOperator; override;
    procedure NewFormalParameterType; override;
    procedure Number; override;
    procedure ObjectConstructorHeading; override;
    procedure ObjectDestructorHeading; override;
    procedure ObjectField; override;
    procedure ObjectForward; override;
    procedure ObjectFunctionHeading; override;
    procedure ObjectHeritage; override;
    procedure ObjectMemberList; override;
    procedure ObjectMethodDirective; override;
    procedure ObjectMethodHeading; override;
    procedure ObjectNameOfMethod; override;
    procedure ObjectProperty; override;
    procedure ObjectPropertySpecifiers; override;
    procedure ObjectProcedureHeading; override;
    procedure ObjectType; override;
    procedure ObjectTypeEnd; override; // DR 2001-08-07
    procedure ObjectVisibility; override;
    procedure OldFormalParameterType; override;
    procedure OrdinalIdentifier; override;
    procedure OrdinalType; override;
    procedure OutParameter; override;
    procedure PackageFile; override;
    procedure ParameterFormal; override;
    procedure ParameterName; override;
    procedure ParameterNameList; override;
    procedure ParseFile; override;
    procedure PointerType; override;
    procedure ProceduralDirective; override;
    procedure ProceduralType; override;
    procedure ProcedureDeclarationSection; override;
    procedure ProcedureHeading; override;
    procedure ProcedureMethodDeclaration; override;
    procedure ProcedureMethodName; override;
    procedure ProgramBlock; override;
    procedure ProgramFile; override;
    procedure PropertyDefault; override;
    procedure PropertyInterface; override;
    procedure PropertyName; override;
    procedure PropertyParameterConst; override;
    procedure PropertyParameterList; override;
    procedure PropertySpecifiers; override;
    procedure QualifiedIdentifier; override;
    procedure QualifiedIdentifierList; override;
    procedure RaiseStatement; override;
    procedure ReadAccessIdentifier; override;
    procedure RealIdentifier; override;
    procedure RealType; override;
    procedure RecordConstant; override;
    procedure RecordFieldConstant; override;
    procedure RecordType; override;
    procedure RecordVariant; override;
    procedure RelativeOperator; override;
    procedure RepeatStatement; override;
    procedure RequiresClause; override;
    procedure RequiresIdentifier; override;
    procedure ResolutionInterfaceName; override;
    procedure ResourceDeclaration; override;
    procedure ReturnType; override;
    procedure SetConstructor; override;
    procedure SetElement; override;
    procedure SetType; override;
    procedure SimpleExpression; override;
    procedure SimpleStatement; override;
    procedure SimpleType; override;
    procedure SkipAnsiComment; override;
    procedure SkipBorComment; override;
    procedure SkipSlashesComment; override;
    procedure SkipSpace; override; //XM Jul-2000
    procedure SkipCRLFco; override; //XM Jul-2000
    procedure SkipCRLF; override; //XM Jul-2000
    procedure Statement; override;
    procedure StatementList; override;
    procedure StorageExpression; override;
    procedure StorageIdentifier; override;
    procedure StorageDefault; override;
    procedure StorageNoDefault; override;
    procedure StorageSpecifier; override;
    procedure StorageStored; override;
    procedure StringIdentifier; override;
    procedure StringStatement; override;
    procedure StringType; override;
    procedure StructuredType; override;
    procedure SubrangeType; override;
    procedure TagField; override;
    procedure TagFieldName; override;
    procedure TagFieldTypeName; override;
    procedure Term; override;
    procedure TryStatement; override;
    procedure TypedConstant; override;
    procedure TypeDeclaration; override;
    procedure TypeId; override;
    procedure TypeKind; override;
    procedure TypeName; override;
    //generics
    procedure TypeArgs; override;
    procedure TypeParams; override;
    procedure TypeParamDecl; override;
    procedure TypeParamDeclList; override;
    procedure TypeParamList; override;
    procedure ConstraintList; override;
    procedure Constraint; override;
    //end generics
    procedure TypeSection; override;
    procedure UnitFile; override;
    procedure UnitId; override;
    procedure UnitName; override;
    procedure UsedUnitName; override;
    procedure UsedUnitsList; override;
    procedure UsesClause; override;
    procedure VarAbsolute; override;
    procedure VarEqual; override;
    procedure VarDeclaration; override;
    procedure Variable; override;
    procedure VariableList; override;
    procedure VariableReference; override;
    procedure VariableTwo; override;
    procedure VariantIdentifier; override;
    procedure VariantSection; override;
    procedure VarParameter; override;
    procedure VarName; override; //!! Added VarName and VarNameList back in...
    procedure VarNameList; override;
    procedure VarSection; override;
    procedure VisibilityAutomated; override;
    procedure VisibilityPrivate; override;
    procedure VisibilityProtected; override;
    procedure VisibilityPublic; override;
    procedure VisibilityPublished; override;
    procedure VisibilityUnknown; override;
    procedure WhileStatement; override;
    procedure WithStatement; override;
    procedure WriteAccessIdentifier; override;
  end;

implementation
{$IF NOT OXYGENE}
uses
  SysUtils;
{$ENDIF}
{ TMySimplePasPar }

procedure TPas2YamlParser.AccessSpecifier;
begin
  EnterHandler('AccessSpecifier');
  inherited;
  ExitHandler('AccessSpecifier');
end;

procedure TPas2YamlParser.AdditiveOperator;
begin
  EnterHandler('AdditiveOperator');
  inherited;
  ExitHandler('AdditiveOperator');
end;

procedure TPas2YamlParser.AfterConstruction;
begin
  inherited;
  FParentStack := TStack<TSemanticParentYaml>.Create;
end;

procedure TPas2YamlParser.AncestorId;
begin
  EnterHandler('AncestorId');
  inherited;
  ExitHandler('AncestorId');
end;

procedure TPas2YamlParser.AncestorIdList;
begin
  EnterHandler('AncestorIdList');
  inherited;
  ExitHandler('AncestorIdList');
end;

procedure TPas2YamlParser.AnonymousMethod;
begin
  EnterHandler('AnonymousMethod');
  inherited;
  ExitHandler('AnonymousMethod');
end;

procedure TPas2YamlParser.AnonymousMethodType;
begin
  EnterHandler('AnonymousMethodType');
  inherited;
  ExitHandler('AnonymousMethodType');
end;

procedure TPas2YamlParser.ArrayConstant;
begin
  EnterHandler('ArrayConstant');
  inherited;
  ExitHandler('ArrayConstant');
end;

procedure TPas2YamlParser.ArrayType;
begin
  EnterHandler('ArrayType');
  inherited;
  ExitHandler('ArrayType');
end;

procedure TPas2YamlParser.AsmStatement;
begin
  EnterHandler('AsmStatement');
  inherited;
  ExitHandler('AsmStatement');
end;

procedure TPas2YamlParser.Block;
begin
  EnterHandler('Block');
  inherited;
  ExitHandler('Block');

  //end of a procedure implementation block
  if FCurrentItem <> nil then
  begin
    FCurrentItem.locationSpan.end_.a := Lexer.LineNumber+1;
    FCurrentItem.locationSpan.end_.b := length(Lexer.Line);
    FCurrentItem.span.b := Lexer.LinePos + length(Lexer.Line);
  end;
end;

procedure TPas2YamlParser.CaseLabel;
begin
  EnterHandler('CaseLabel');
  inherited;
  ExitHandler('CaseLabel');
end;

procedure TPas2YamlParser.CaseSelector;
begin
  EnterHandler('CaseSelector');
  inherited;
  ExitHandler('CaseSelector');
end;

procedure TPas2YamlParser.CaseStatement;
begin
  EnterHandler('CaseStatement');
  inherited;
  ExitHandler('CaseStatement');
end;

procedure TPas2YamlParser.CharString;
begin
  EnterHandler('CharString');
  inherited;
  ExitHandler('CharString');
end;

procedure TPas2YamlParser.ClassClass;
begin
  EnterHandler('ClassClass');
  inherited;
  ExitHandler('ClassClass');
end;

procedure TPas2YamlParser.ClassField;
var itemyaml: TSemanticItemYaml;
begin
  itemyaml := ProcessItem_Before('field', False {full line}, 'ClassField');
  FCurrentMethod := itemyaml;
  inherited;
  ProcessItem_Next(itemyaml, False {full line}, 'ClassField');

//  EnterHandler('ClassField');
//  inherited;
//  ExitHandler('ClassField');
end;

procedure TPas2YamlParser.ClassForward;
begin
  EnterHandler('ClassForward');
  inherited;
  ExitHandler('ClassForward');
end;

procedure TPas2YamlParser.ClassFunctionHeading;
begin
  EnterHandler('ClassFunctionHeading');
  inherited;
  //ExitHandler('ClassFunctionHeading');
  ProcessItem_Next(FCurrentMethod, False {full line}, 'ClassFunctionHeading');
  //for start, see FunctionMethodName
end;

procedure TPas2YamlParser.ClassHeritage;
begin
  EnterHandler('ClassHeritage');
  inherited;
  ExitHandler('ClassHeritage');
end;

procedure TPas2YamlParser.ClassMemberList;
begin
  EnterHandler('ClassMemberList');
  inherited;
  ExitHandler('ClassMemberList');
end;

procedure TPas2YamlParser.ClassMethodDirective;
begin
  EnterHandler('ClassMethodDirective');
  inherited;
  ExitHandler('ClassMethodDirective');
end;

procedure TPas2YamlParser.ClassMethodHeading;
begin
  EnterHandler('ClassMethodHeading');
  inherited;
  ExitHandler('ClassMethodHeading');
end;

procedure TPas2YamlParser.ClassMethodOrProperty;
begin
  EnterHandler('ClassMethodOrProperty');
  inherited;
  ExitHandler('ClassMethodOrProperty');
end;

procedure TPas2YamlParser.ClassMethodResolution;
begin
  EnterHandler('ClassMethodResolution');
  inherited;
  ExitHandler('ClassMethodResolution');
end;

procedure TPas2YamlParser.ClassProcedureHeading;
begin
  EnterHandler('ClassProcedureHeading');
  inherited;
  //ExitHandler('ClassProcedureHeading');
  ProcessItem_Next(FCurrentMethod, False {full line}, 'ClassProcedureHeading');
  //for start, see ProcedureMethodName
end;

procedure TPas2YamlParser.ClassProperty;
begin
  EnterHandler('ClassProperty');
  inherited;
  //ExitHandler('ClassProperty');
  ProcessItem_Next(FCurrentMethod, False {full line}, 'ClassProperty');
  //for start, see PropertyName

//  EnterHandler('ClassProperty');
//  inherited;
//  ExitHandler('ClassProperty');
end;

procedure TPas2YamlParser.ClassReferenceType;
begin
  EnterHandler('ClassReferenceType');
  inherited;
  ExitHandler('ClassReferenceType');
end;

procedure TPas2YamlParser.ClassType;
var parentyaml: TSemanticParentYaml;
begin
  FPrevClassVisibility := nil;
  parentyaml := ProcessParent_Before('class', 'ClassType');
  //EnterHandler('UsesClause');

  inherited;

  //ExitHandler('UsesClause');
  if FPrevClassVisibility <> nil then
    ProcessParent_Next(FPrevClassVisibility, False, 'end of Visibility');
  FPrevClassVisibility := nil;

  ProcessParent_Next(parentyaml, True{incl last line}, 'ClassType');
end;
(*
var classyaml: TSemanticParentYaml;
begin
  (*
  - type : class
    name : TTest
    locationSpan : {start: [6,0], end: [9,0]}
    headerSpan : [34, 59]
    footerSpan : [81, 88]
    children :
  )
  classyaml := FCurrentParent.children.AddNewParent;
  classyaml.type_ := 'class';
  classyaml.name  := FCurrentType;
  classyaml.locationSpan.start.a := Lexer.LineNumber+1;
  classyaml.locationSpan.start.b := 0;
  classyaml.headerSpan.a  := Lexer.LinePos;
  classyaml.headerSpan.b  := Lexer.LinePos + Length(Lexer.Line);

  //UpdatePrevYamlLocation(classyaml);
  UpdateNextYamlLocation(classyaml);

  FCurrentParent := classyaml;
  EnterHandler('ClassType');
  inherited;
  ExitHandler('ClassType');
  FCurrentParent := classyaml;

  classyaml.locationSpan.end_.a  := Lexer.LineNumber+1;
  classyaml.locationSpan.end_.b  := Length(Lexer.Line);
  classyaml.footerSpan.a  := Lexer.LinePos;
  classyaml.footerSpan.b  := Lexer.LinePos + Length(Lexer.Line);

  FPrevSpan := classyaml.footerSpan;
end;
*)

procedure TPas2YamlParser.ClassTypeEnd;
begin
  EnterHandler('ClassTypeEnd');
  inherited;
  ExitHandler('ClassTypeEnd');
end;

procedure TPas2YamlParser.ClassVisibility;
begin
  EnterHandler('ClassVisibility');
  inherited;
  ExitHandler('ClassVisibility');
end;

procedure TPas2YamlParser.CompoundStatement;
begin
  EnterHandler('CompoundStatement');
  inherited;
  ExitHandler('CompoundStatement');
end;

procedure TPas2YamlParser.ConstantColon;
begin
  EnterHandler('ConstantColon');
  inherited;
  ExitHandler('ConstantColon');
end;

procedure TPas2YamlParser.ConstantDeclaration;
begin
  EnterHandler('ConstantDeclaration');
  inherited;
  ExitHandler('ConstantDeclaration');
end;

procedure TPas2YamlParser.ConstantEqual;
begin
  EnterHandler('ConstantEqual');
  inherited;
  ExitHandler('ConstantEqual');
end;

procedure TPas2YamlParser.ConstantExpression;
begin
  EnterHandler('ConstantExpression');
  inherited;
  ExitHandler('ConstantExpression');
end;

procedure TPas2YamlParser.ConstantName;
begin
  EnterHandler('ConstantName');
  inherited;
  ExitHandler('ConstantName');
end;

procedure TPas2YamlParser.ConstantType;
begin
  EnterHandler('ConstantType');
  inherited;
  ExitHandler('ConstantType');
end;

procedure TPas2YamlParser.ConstantValue;
begin
  EnterHandler('ConstantValue');
  inherited;
  ExitHandler('ConstantValue');
end;

procedure TPas2YamlParser.ConstantValueTyped;
begin
  EnterHandler('ConstantValueTyped');
  inherited;
  ExitHandler('ConstantValueTyped');
end;

procedure TPas2YamlParser.ConstParameter;
begin
  EnterHandler('ConstParameter');
  inherited;
  ExitHandler('ConstParameter');
end;

procedure TPas2YamlParser.Constraint;
begin
  EnterHandler('Constraint');
  inherited;
  ExitHandler('Constraint');
end;

procedure TPas2YamlParser.ConstraintList;
begin
  EnterHandler('ConstraintList');
  inherited;
  ExitHandler('ConstraintList');
end;

procedure TPas2YamlParser.ConstructorHeading;
begin
  EnterHandler('ConstructorHeading');
  inherited;
  ExitHandler('ConstructorHeading');
end;

procedure TPas2YamlParser.ConstructorName;
begin
  EnterHandler('ConstructorName');
  inherited;
  ExitHandler('ConstructorName');
end;

procedure TPas2YamlParser.ConstSection;
begin
  EnterHandler('ConstSection');
  inherited;
  ExitHandler('ConstSection');
end;

procedure TPas2YamlParser.ContainsClause;
begin
  EnterHandler('ContainsClause');
  inherited;
  ExitHandler('ContainsClause');
end;

procedure TPas2YamlParser.ContainsExpression;
begin
  EnterHandler('ContainsExpression');
  inherited;
  ExitHandler('ContainsExpression');
end;

procedure TPas2YamlParser.ContainsIdentifier;
begin
  EnterHandler('ContainsIdentifier');
  inherited;
  ExitHandler('ContainsIdentifier');
end;

procedure TPas2YamlParser.ContainsStatement;
begin
  EnterHandler('ContainsStatement');
  inherited;
  ExitHandler('ContainsStatement');
end;

procedure TPas2YamlParser.DeclarationSection;
begin
  //include last empty line in previous footer + reset
  UpdatePrevYamlLocation(nil);
  if (FPrevSpan <> nil) then
    if FPrevSpan.b < (Lexer.TokenPos-1) then
      FPrevSpan.b := Lexer.TokenPos-1;
  FPrevSpan := nil;

  EnterHandler('DeclarationSection');
  inherited;
  ExitHandler('DeclarationSection');
end;

procedure TPas2YamlParser.Designator;
begin
  EnterHandler('Designator');
  inherited;
  ExitHandler('Designator');
end;
{$IF NOT OXYGENE}
destructor TPas2YamlParser.Destroy;
begin
  FParentStack.Free;
  inherited;
end;
{$ENDIF}
procedure TPas2YamlParser.DestructorHeading;
begin
  EnterHandler('DestructorHeading');
  inherited;
  ExitHandler('DestructorHeading');
end;

procedure TPas2YamlParser.DestructorName;
begin
  EnterHandler('DestructorName');
  inherited;
  ExitHandler('DestructorName');
end;

procedure TPas2YamlParser.Directive16Bit;
begin
  EnterHandler('Directive16Bit');
  inherited;
  ExitHandler('Directive16Bit');
end;

procedure TPas2YamlParser.DirectiveBinding;
begin
  EnterHandler('DirectiveBinding');
  inherited;
  ExitHandler('DirectiveBinding');
end;

procedure TPas2YamlParser.DirectiveCalling;
begin
  EnterHandler('DirectiveCalling');
  inherited;
  ExitHandler('DirectiveCalling');
end;

procedure TPas2YamlParser.DirectiveDeprecated;
begin
  EnterHandler('DirectiveDeprecated');
  inherited;
  ExitHandler('DirectiveDeprecated');
end;

procedure TPas2YamlParser.DirectiveLibrary;
begin
  EnterHandler('DirectiveLibrary');
  inherited;
  ExitHandler('DirectiveLibrary');
end;

procedure TPas2YamlParser.DirectiveLocal;
begin
  EnterHandler('DirectiveLocal');
  inherited;
  ExitHandler('DirectiveLocal');
end;

procedure TPas2YamlParser.DirectivePlatform;
begin
  EnterHandler('DirectivePlatform');
  inherited;
  ExitHandler('DirectivePlatform');
end;

procedure TPas2YamlParser.DirectiveVarargs;
begin
  EnterHandler('DirectiveVarargs');
  inherited;
  ExitHandler('DirectiveVarargs');
end;

procedure TPas2YamlParser.DispIDSpecifier;
begin
  EnterHandler('DispIDSpecifier');
  inherited;
  ExitHandler('DispIDSpecifier');
end;

procedure TPas2YamlParser.DispInterfaceForward;
begin
  EnterHandler('DispInterfaceForward');
  inherited;
  ExitHandler('DispInterfaceForward');
end;

procedure TPas2YamlParser.EmptyStatement;
begin
  EnterHandler('EmptyStatement');
  inherited;
  ExitHandler('EmptyStatement');
end;

procedure TPas2YamlParser.EnumeratedType;
begin
  EnterHandler('EnumeratedType');
  inherited;
  ExitHandler('EnumeratedType');
end;

procedure TPas2YamlParser.EnumeratedTypeItem;
begin
  EnterHandler('EnumeratedTypeItem');
  inherited;
  ExitHandler('EnumeratedTypeItem');
end;

procedure TPas2YamlParser.ExceptBlock;
begin
  EnterHandler('ExceptBlock');
  inherited;
  ExitHandler('ExceptBlock');
end;

procedure TPas2YamlParser.ExceptionBlockElseBranch;
begin
  EnterHandler('ExceptionBlockElseBranch');
  inherited;
  ExitHandler('ExceptionBlockElseBranch');
end;

procedure TPas2YamlParser.ExceptionClassTypeIdentifier;
begin
  EnterHandler('ExceptionClassTypeIdentifier');
  inherited;
  ExitHandler('ExceptionClassTypeIdentifier');
end;

procedure TPas2YamlParser.ExceptionHandler;
begin
  EnterHandler('ExceptionHandler');
  inherited;
  ExitHandler('ExceptionHandler');
end;

procedure TPas2YamlParser.ExceptionHandlerList;
begin
  EnterHandler('ExceptionHandlerList');
  inherited;
  ExitHandler('ExceptionHandlerList');
end;

procedure TPas2YamlParser.ExceptionIdentifier;
begin
  EnterHandler('ExceptionIdentifier');
  inherited;
  ExitHandler('ExceptionIdentifier');
end;

procedure TPas2YamlParser.ExceptionVariable;
begin
  EnterHandler('ExceptionVariable');
  inherited;
  ExitHandler('ExceptionVariable');
end;

procedure TPas2YamlParser.ExplicitType;
begin
  EnterHandler('ExplicitType');
  inherited;
  ExitHandler('ExplicitType');
end;

procedure TPas2YamlParser.ExportedHeading;
begin
  EnterHandler('ExportedHeading');
  inherited;
  ExitHandler('ExportedHeading');
end;

procedure TPas2YamlParser.ExportsClause;
begin
  EnterHandler('ExportsClause');
  inherited;
  ExitHandler('ExportsClause');
end;

procedure TPas2YamlParser.ExportsElement;
begin
  EnterHandler('ExportsElement');
  inherited;
  ExitHandler('ExportsElement');
end;

procedure TPas2YamlParser.Expression;
begin
  EnterHandler('Expression');
  inherited;
  ExitHandler('Expression');
end;

procedure TPas2YamlParser.ExpressionList;
begin
  EnterHandler('ExpressionList');
  inherited;
  ExitHandler('ExpressionList');
end;

procedure TPas2YamlParser.ExternalDirective;
begin
  EnterHandler('ExternalDirective');
  inherited;
  ExitHandler('ExternalDirective');
end;

procedure TPas2YamlParser.ExternalDirectiveThree;
begin
  EnterHandler('ExternalDirectiveThree');
  inherited;
  ExitHandler('ExternalDirectiveThree');
end;

procedure TPas2YamlParser.ExternalDirectiveTwo;
begin
  EnterHandler('ExternalDirectiveTwo');
  inherited;
  ExitHandler('ExternalDirectiveTwo');
end;

procedure TPas2YamlParser.Factor;
begin
  EnterHandler('Factor');
  inherited;
  ExitHandler('Factor');
end;

procedure TPas2YamlParser.FieldDeclaration;
begin
  EnterHandler('FieldDeclaration');
  inherited;
  ExitHandler('FieldDeclaration');
end;

procedure TPas2YamlParser.FieldList;
begin
  EnterHandler('FieldList');
  inherited;
  ExitHandler('FieldList');
end;

procedure TPas2YamlParser.FieldName;
begin
  EnterHandler('FieldName');
  inherited;
  ExitHandler('FieldName');
end;

procedure TPas2YamlParser.FieldNameList;
begin
  EnterHandler('FieldNameList');
  inherited;
  ExitHandler('FieldNameList');
end;

procedure TPas2YamlParser.FileType;
begin
  EnterHandler('FileType');
  inherited;
  ExitHandler('FileType');
end;

procedure TPas2YamlParser.FormalParameterList;
begin
  EnterHandler('FormalParameterList');
  inherited;
  ExitHandler('FormalParameterList');
end;

procedure TPas2YamlParser.FormalParameterSection;
begin
  EnterHandler('FormalParameterSection');
  inherited;
  ExitHandler('FormalParameterSection');
end;

procedure TPas2YamlParser.ForStatement;
begin
  EnterHandler('ForStatement');
  inherited;
  ExitHandler('ForStatement');
end;

procedure TPas2YamlParser.ForwardDeclaration;
begin
  EnterHandler('ForwardDeclaration');
  inherited;
  ExitHandler('ForwardDeclaration');
end;

procedure TPas2YamlParser.FunctionHeading;
begin
  EnterHandler('FunctionHeading');
  inherited;
  ExitHandler('FunctionHeading');
end;

procedure TPas2YamlParser.FunctionMethodDeclaration;
begin
  EnterHandler('FunctionMethodDeclaration');
  inherited;
  ExitHandler('FunctionMethodDeclaration');
end;

procedure TPas2YamlParser.FunctionMethodName;
var itemyaml: TSemanticItemYaml;
begin
  itemyaml := ProcessItem_Before('method', False {full line}, 'FunctionMethodName');
  FCurrentMethod := itemyaml;
  inherited;
  //  ProcessItem_Next(itemyaml, 'FunctionMethodName');  see ClassFunctionHeading
  ExitHandler('FunctionMethodName');
end;

procedure TPas2YamlParser.FunctionProcedureBlock;
var itemyaml: TSemanticItemYaml;
begin
  itemyaml := ProcessItem_Before('method implementation', False {full line}, 'FunctionProcedureBlock');
  itemyaml.name  := FObjectNameOfMethod + '.' + FFunctionProcedureName;
  FCurrentMethod := itemyaml;
  inherited;
  ProcessItem_Next(itemyaml, False {full line}, 'FunctionProcedureBlock');
  //ExitHandler('FunctionProcedureBlock');
end;
(*
var procyaml: TSemanticItemYaml;
begin
(*
      - type : procedure
        name : TTest.Test
        locationSpan : {start: [11,0], end: [18,0]}
        span : [107, 163]
)
  procyaml := FCurrentParent.children.AddNewItem;
  procyaml.type_ := 'method implementation';
  procyaml.name  := FObjectNameOfMethod + '.' + FFunctionProcedureName;
  procyaml.locationSpan.start.a := Lexer.LineNumber+1;
  procyaml.locationSpan.start.b := 0;
  procyaml.span.a := Lexer.LinePos;

  FCurrentItem := procyaml;
  UpdatePrevYamlLocation(procyaml);

  EnterHandler('FunctionProcedureBlock');
  inherited;
  ExitHandler('FunctionProcedureBlock');

  FCurrentItem := nil;
  FPrevSpan := procyaml.span;

  //here we are a token too far, so we set end of block in "procedure TPas2YamlParser.Block;"
//  procyaml.locationSpan.end_.a  := Lexer.LineNumber+1;
//  procyaml.locationSpan.end_.b  := Length(Lexer.Line);
//  procyaml.span.b  := Lexer.LinePos + Length(Lexer.Line);
end;
*)

procedure TPas2YamlParser.FunctionProcedureName;
begin
  FFunctionProcedureName := Lexer.Token;

  EnterHandler('FunctionProcedureName');
  inherited;
  ExitHandler('FunctionProcedureName');
end;

function TPas2YamlParser.GetYaml: TSemanticMasterYaml;
begin
  Result := FYamlMaster;
end;

procedure TPas2YamlParser.Identifier;
begin
  EnterHandler('Identifier');
  inherited;
  ExitHandler('Identifier');
end;

procedure TPas2YamlParser.IdentifierList;
begin
  EnterHandler('IdentifierList');
  inherited;
  ExitHandler('IdentifierList');
end;

procedure TPas2YamlParser.IfStatement;
begin
  EnterHandler('IfStatement');
  inherited;
  ExitHandler('IfStatement');
end;

procedure TPas2YamlParser.ImplementationSection;
var implyaml: TSemanticParentYaml;
begin
  (*
  - type : implementation
    name : implementation
    locationSpan : {start: [9,0], end: [19,4]}
    headerSpan : [89, 106]
    footerSpan : [164, 169]   //todo
    children :
  *)
  implyaml := ProcessParent_Before('implementation', 'ImplementationSection');
  {
  implyaml := FYamlMaster.children.AddNewParent;
  implyaml.type_ := 'implementation';
  implyaml.name  := Lexer.Token;
  implyaml.locationSpan.start.a := Lexer.LineNumber+1;
  implyaml.locationSpan.start.b := 0;
  implyaml.headerSpan.a  := Lexer.LinePos;
  implyaml.headerSpan.b  := Lexer.LinePos + Length(Lexer.Line);

  UpdatePrevYamlLocation(implyaml);

  FCurrentParent := implyaml;
  EnterHandler('ImplementationSection');
  }
  inherited;

  ProcessParent_Next(implyaml, True{incl last line}, 'ImplementationSection');
  {
  ExitHandler('ImplementationSection');
  FCurrentParent := implyaml;

  implyaml.locationSpan.end_.a  := Lexer.LineNumber+1;
  implyaml.locationSpan.end_.b  := Length(Lexer.Line);

  //include last empty span in footer!  -> no, include in last method implementation
  //if FPrevItem <> nil then
  //  implyaml.footerSpan.a  := FPrevItem.span.b+1
  //else if FPrevParent <> nil then
  //  implyaml.footerSpan.a  := FPrevParent.footerSpan.b+1;
  //include last empty line in previous footer
  if FPrevSpan <> nil then
    FPrevSpan.b := Lexer.LinePos-1;

  implyaml.footerSpan.a    := Lexer.LinePos;
  implyaml.footerSpan.b    := Lexer.LinePos + Length(Lexer.Line);

  FPrevSpan := implyaml.footerSpan;
  }
end;

procedure TPas2YamlParser.IncludeFile;
begin
  EnterHandler('IncludeFile');
  inherited;
  ExitHandler('IncludeFile');
end;

procedure TPas2YamlParser.IndexSpecifier;
begin
  EnterHandler('IndexSpecifier');
  inherited;
  ExitHandler('IndexSpecifier');
end;

procedure TPas2YamlParser.InheritedStatement;
begin
  EnterHandler('InheritedStatement');
  inherited;
  ExitHandler('InheritedStatement');
end;

procedure TPas2YamlParser.InitializationSection;
begin
  EnterHandler('InitializationSection');
  inherited;
  ExitHandler('InitializationSection');
end;

procedure TPas2YamlParser.InlineStatement;
begin
  EnterHandler('InlineStatement');
  inherited;
  ExitHandler('InlineStatement');
end;

procedure TPas2YamlParser.InParameter;
begin
  EnterHandler('InParameter');
  inherited;
  ExitHandler('InParameter');
end;

procedure TPas2YamlParser.InterfaceDeclaration;
begin
  EnterHandler('InterfaceDeclaration');
  inherited;
  ExitHandler('InterfaceDeclaration');
end;

procedure TPas2YamlParser.InterfaceForward;
begin
  EnterHandler('InterfaceForward');
  inherited;
  ExitHandler('InterfaceForward');
end;

procedure TPas2YamlParser.InterfaceGUID;
begin
  EnterHandler('InterfaceGUID');
  inherited;
  ExitHandler('InterfaceGUID');
end;

procedure TPas2YamlParser.InterfaceHeritage;
begin
  EnterHandler('InterfaceHeritage');
  inherited;
  ExitHandler('InterfaceHeritage');
end;

procedure TPas2YamlParser.InterfaceMemberList;
begin
  EnterHandler('InterfaceMemberList');
  inherited;
  ExitHandler('InterfaceMemberList');
end;

procedure TPas2YamlParser.InterfaceSection;
var
  intyaml: TSemanticParentYaml;
begin
  (*
  - type : interface
    name : interface
    locationSpan : {start: [2,0], end: [9,0]}
    headerSpan : [13, 25]
    footerSpan : [0, -1]
    children :
  *)

  intyaml := ProcessParent_Before('interface', 'InterfaceSection');
  {
  intyaml := FYamlMaster.children.AddNewParent;
  intyaml.type_ := 'interface';
  intyaml.name  := Lexer.Token;
  intyaml.locationSpan.start.a := Lexer.LineNumber+1;
  intyaml.locationSpan.start.b := 0;
  intyaml.headerSpan.a  := Lexer.LinePos;
  intyaml.headerSpan.b  := Lexer.LinePos + Length(Lexer.Line);

  UpdatePrevYamlLocation(intyaml);

  FCurrentParent := intyaml;
  EnterHandler('InterfaceSection');
  }
  inherited;
  ProcessParent_Next(intyaml, False {not full line}, 'InterfaceSection');
  {
  ExitHandler('InterfaceSection');
  FCurrentParent := intyaml;

  intyaml.locationSpan.end_.a  := Lexer.LineNumber+1;
  intyaml.locationSpan.end_.b  := -1; //-1 //Length(Lexer.Line);  implementation?
  //no footer?
  //intyaml.footerSpan.a  := 0;
  //intyaml.footerSpan.b  := -1;

  //include last empty line in footer! (e.g. from "class" footerspan)
  if FPrevSpan <> nil then
    intyaml.footerSpan.a := FPrevSpan.b+1
  else
    intyaml.footerSpan.a := Lexer.LinePos;
  intyaml.footerSpan.b  := Lexer.LinePos-1;
  FPrevSpan := intyaml.footerSpan;
  }
end;

procedure TPas2YamlParser.InterfaceType;
begin
  EnterHandler('InterfaceType');
  inherited;
  ExitHandler('InterfaceType');
end;

procedure TPas2YamlParser.LabelDeclarationSection;
begin
  EnterHandler('LabelDeclarationSection');
  inherited;
  ExitHandler('LabelDeclarationSection');
end;

procedure TPas2YamlParser.LabeledStatement;
begin
  EnterHandler('LabeledStatement');
  inherited;
  ExitHandler('LabeledStatement');
end;

procedure TPas2YamlParser.LabelId;
begin
  EnterHandler('LabelId');
  inherited;
  ExitHandler('LabelId');
end;

procedure TPas2YamlParser.LibraryFile;
begin
  EnterHandler('LibraryFile');
  inherited;
  ExitHandler('LibraryFile');
end;

procedure TPas2YamlParser.MainUsedUnitExpression;
begin
  EnterHandler('MainUsedUnitExpression');
  inherited;
  ExitHandler('MainUsedUnitExpression');
end;

procedure TPas2YamlParser.MainUsedUnitName;
begin
  EnterHandler('MainUsedUnitName');
  inherited;
  ExitHandler('MainUsedUnitName');
end;

procedure TPas2YamlParser.MainUsedUnitStatement;
begin
  EnterHandler('MainUsedUnitStatement');
  inherited;
  ExitHandler('MainUsedUnitStatement');
end;

procedure TPas2YamlParser.MainUsesClause;
begin
  EnterHandler('MainUsesClause');
  inherited;
  ExitHandler('MainUsesClause');
end;

procedure TPas2YamlParser.MultiplicativeOperator;
begin
  EnterHandler('MultiplicativeOperator');
  inherited;
  ExitHandler('MultiplicativeOperator');
end;

procedure TPas2YamlParser.NewFormalParameterType;
begin
  EnterHandler('NewFormalParameterType');
  inherited;
  ExitHandler('NewFormalParameterType');
end;

procedure TPas2YamlParser.Number;
begin
  EnterHandler('Number');
  inherited;
  ExitHandler('Number');
end;

procedure TPas2YamlParser.ObjectConstructorHeading;
begin
  EnterHandler('ObjectConstructorHeading');
  inherited;
  ExitHandler('ObjectConstructorHeading');
end;

procedure TPas2YamlParser.ObjectDestructorHeading;
begin
  EnterHandler('ObjectDestructorHeading');
  inherited;
  ExitHandler('ObjectDestructorHeading');
end;

procedure TPas2YamlParser.ObjectField;
begin
  EnterHandler('ObjectField');
  inherited;
  ExitHandler('ObjectField');
end;

procedure TPas2YamlParser.ObjectForward;
begin
  EnterHandler('ObjectForward');
  inherited;
  ExitHandler('ObjectForward');
end;

procedure TPas2YamlParser.ObjectFunctionHeading;
begin
  EnterHandler('ObjectFunctionHeading');
  inherited;
  ExitHandler('ObjectFunctionHeading');
end;

procedure TPas2YamlParser.ObjectHeritage;
begin
  EnterHandler('ObjectHeritage');
  inherited;
  ExitHandler('ObjectHeritage');
end;

procedure TPas2YamlParser.ObjectMemberList;
begin
  EnterHandler('ObjectMemberList');
  inherited;
  ExitHandler('ObjectMemberList');
end;

procedure TPas2YamlParser.ObjectMethodDirective;
begin
  EnterHandler('ObjectMethodDirective');
  inherited;
  ExitHandler('ObjectMethodDirective');
end;

procedure TPas2YamlParser.ObjectMethodHeading;
begin
  EnterHandler('ObjectMethodHeading');
  inherited;
  ExitHandler('ObjectMethodHeading');
end;

procedure TPas2YamlParser.ObjectNameOfMethod;
begin
  FObjectNameOfMethod := Lexer.Token;

  EnterHandler('ObjectNameOfMethod');
  inherited;
  ExitHandler('ObjectNameOfMethod');
end;

procedure TPas2YamlParser.ObjectProcedureHeading;
begin
  EnterHandler('ObjectProcedureHeading');
  inherited;
  ExitHandler('ObjectProcedureHeading');
end;

procedure TPas2YamlParser.ObjectProperty;
begin
  EnterHandler('ObjectProperty');
  inherited;
  ExitHandler('ObjectProperty');
end;

procedure TPas2YamlParser.ObjectPropertySpecifiers;
begin
  EnterHandler('ObjectPropertySpecifiers');
  inherited;
  ExitHandler('ObjectPropertySpecifiers');
end;

procedure TPas2YamlParser.ObjectType;
begin
  EnterHandler('ObjectType');
  inherited;
  ExitHandler('ObjectType');
end;

procedure TPas2YamlParser.ObjectTypeEnd;
begin
  EnterHandler('ObjectTypeEnd');
  inherited;
  ExitHandler('ObjectTypeEnd');
end;

procedure TPas2YamlParser.ObjectVisibility;
begin
  EnterHandler('ObjectVisibility');
  inherited;
  ExitHandler('ObjectVisibility');
end;

procedure TPas2YamlParser.OldFormalParameterType;
begin
  EnterHandler('OldFormalParameterType');
  inherited;
  ExitHandler('OldFormalParameterType');
end;

procedure TPas2YamlParser.OrdinalIdentifier;
begin
  EnterHandler('OrdinalIdentifier');
  inherited;
  ExitHandler('OrdinalIdentifier');
end;

procedure TPas2YamlParser.OrdinalType;
begin
  EnterHandler('OrdinalType');
  inherited;
  ExitHandler('OrdinalType');
end;

procedure TPas2YamlParser.OutParameter;
begin
  EnterHandler('OutParameter');
  inherited;
  ExitHandler('OutParameter');
end;

procedure TPas2YamlParser.PackageFile;
begin
  EnterHandler('PackageFile');
  inherited;
  ExitHandler('PackageFile');
end;

procedure TPas2YamlParser.ParameterFormal;
begin
  EnterHandler('ParameterFormal');
  inherited;
  ExitHandler('ParameterFormal');
end;

procedure TPas2YamlParser.ParameterName;
begin
  EnterHandler('ParameterName');
  inherited;
  ExitHandler('ParameterName');
end;

procedure TPas2YamlParser.ParameterNameList;
begin
  EnterHandler('ParameterNameList');
  inherited;
  ExitHandler('ParameterNameList');
end;

procedure TPas2YamlParser.ParseFile;
var
  unityaml: TSemanticItemYaml;
begin
  (*
  ---
  type : file
  name : /path/to/file
  locationSpan : {start: [1,0], end: [19,4]}
  footerSpan : [0, -1]
  parsingErrorsDetected : false
  children :
  *)

  FYamlMaster.type_ := 'file';
  FYamlMaster.name  := FUnitName;
  FYamlMaster.locationSpan.start.a := Lexer.LineNumber+1;
  FYamlMaster.locationSpan.start.b := Lexer.LinePos;
  FYamlMaster.footerSpan.a := 0;
  FYamlMaster.footerSpan.b := -1;

  (*
  - type : unit
    name : Unit1
    locationSpan : {start: [1,0], end: [1,13]}
    span : [0, 12]
  *)
  unityaml := FYamlMaster.children.AddNewItem;
  unityaml.type_ := 'unit';
  Lexer.InitAhead;
  unityaml.name  := Lexer.AheadToken;
  unityaml.locationSpan.start.a := Lexer.LineNumber+1;
  unityaml.locationSpan.start.b := 0;
  unityaml.locationSpan.end_.a  := Lexer.LineNumber+1;
  unityaml.locationSpan.end_.b  := Lexer.AheadLex.RunPos;
  unityaml.span.a  := Lexer.LinePos;
  unityaml.span.b  := Lexer.LinePos + length(Lexer.Line);

//  FLastLine     := Lexer.LineNumber+1;
//  FLastLineChar := Lexer.AheadLex.RunPos;
//  FLastChar     := Lexer.LinePos + Length(Lexer.Line);
  UpdatePrevYamlLocation(unityaml);

  //FCurrentParent := FYamlMaster;
  EnterHandler('ParseFile');
  inherited;
  ExitHandler('ParseFile');
  //FCurrentParent := FYamlMaster;

  //end postion of file
  FYamlMaster.locationSpan.end_.a := Lexer.LineNumber+1;
  FYamlMaster.locationSpan.end_.b := length(Lexer.Line);
end;

procedure TPas2YamlParser.PointerType;
begin
  EnterHandler('PointerType');
  inherited;
  ExitHandler('PointerType');
end;

procedure TPas2YamlParser.ProceduralDirective;
begin
  EnterHandler('ProceduralDirective');
  inherited;
  ExitHandler('ProceduralDirective');
end;

procedure TPas2YamlParser.ProceduralType;
begin
  EnterHandler('ProceduralType');
  inherited;
  ExitHandler('ProceduralType');
end;

procedure TPas2YamlParser.ProcedureDeclarationSection;
begin
  EnterHandler('ProcedureDeclarationSection');
  inherited;
  ExitHandler('ProcedureDeclarationSection');
end;

procedure TPas2YamlParser.ProcedureHeading;
begin
  EnterHandler('ProcedureHeading');
  inherited;
  ExitHandler('ProcedureHeading');
end;

procedure TPas2YamlParser.ProcedureMethodDeclaration;
begin
  FObjectNameOfMethod := '';

  EnterHandler('ProcedureMethodDeclaration');
  inherited;
  ExitHandler('ProcedureMethodDeclaration');
end;

procedure TPas2YamlParser.ProcedureMethodName;
var itemyaml: TSemanticItemYaml;
begin
  itemyaml := ProcessItem_Before('method', False {full line}, 'ProcedureMethodName');
  FCurrentMethod := itemyaml;
  inherited;
  //ProcessItem_Next(itemyaml, 'ProcedureMethodName');   see ClassProcedureHeading
  ExitHandler('ProcedureMethodName');
end;
(*
var procyaml: TSemanticItemYaml;
begin
  //     - type : method
  //       name : preVisit
  //       locationSpan : {start: [11,4], end: [27,3]}
  //       span : [251, 865]
  procyaml := FCurrentParent.children.AddNewItem;
  procyaml.type_ := 'method';
  procyaml.name  := Lexer.Token;
  procyaml.locationSpan.start.a := Lexer.LineNumber+1;
  procyaml.locationSpan.start.b := 0;

  UpdatePrevYamlLocation(procyaml);
  FPrevItem := nil;  //no reverse setting end location for this item

  EnterHandler('ProcedureMethodName');
  inherited;
  ExitHandler('ProcedureMethodName');

  procyaml.locationSpan.end_.a  := Lexer.LineNumber+1;
  procyaml.locationSpan.end_.b  := Length(Lexer.Line);
  procyaml.span.a  := Lexer.LinePos;
  procyaml.span.b  := Lexer.LinePos + Length(Lexer.Line);

  FPrevSpan := procyaml.span;
end;
  *)

function TPas2YamlParser.ProcessItem_Before(const aType: String; aExactPos: Boolean; aDebugHandler: String): TSemanticItemYaml;
var itemyaml: TSemanticItemYaml;
begin
  //     - type : method
  //       name : preVisit
  //       locationSpan : {start: [11,4], end: [27,3]}
  //       span : [251, 865]
  itemyaml := FCurrentParent.children.AddNewItem;
  itemyaml.type_ := aType; //'method';
  itemyaml.name  := Lexer.Token;
  itemyaml.locationSpan.start.a := Lexer.LineNumber+1;
  if aExactPos and (FCurrentParent.children.Count > 1) then   //treat first item without exactpos (e.g. fetch indent of UsedUnitName)
  begin
    itemyaml.locationSpan.start.b := Lexer.TokenPos - Lexer.LinePos;
    itemyaml.span.a  := Lexer.TokenPos;
 end
  else
  begin
    itemyaml.locationSpan.start.b := 0;
    itemyaml.span.a  := Lexer.LinePos;
  end;

  UpdatePrevYamlLocation(itemyaml);
  //include last empty line in previous footer
  if (FPrevSpan <> nil) then
  begin
    if aExactPos then
    begin
      if FPrevSpan.b < (Lexer.TokenPos-1) then
        FPrevSpan.b := Lexer.TokenPos-1;
    end
    else
    begin
      if FPrevSpan.b < (Lexer.LinePos-1) then
        FPrevSpan.b := Lexer.LinePos-1;
    end;
  end;
  FPrevItem := nil;  //no reverse setting end location for this item

  EnterHandler(aDebugHandler); //'ProcedureMethodName');
  Result := itemyaml;
end;

procedure TPas2YamlParser.ProcessItem_Next(const aItemYaml: TSemanticItemYaml; aExactPos: Boolean; aDebugHandler: String);
begin
  ExitHandler(aDebugHandler); //'ProcedureMethodName');

  aItemYaml.locationSpan.end_.a    := Lexer.LineNumber+1;
  //same line? then till end of line
  if aItemYaml.locationSpan.start.a = aItemYaml.locationSpan.end_.a then
  begin
    if aExactPos then
    begin
      aItemYaml.locationSpan.end_.b  := Lexer.TokenPos - Lexer.LinePos;
      aItemYaml.span.b               := Lexer.TokenPos;
    end
    else
    begin
      aItemYaml.locationSpan.end_.b  := length(Lexer.Line);
      aItemYaml.span.b               := Lexer.LinePos + length(Lexer.Line);
    end;
  end
  //else start of next line (without first char at pos 0!)
  else
  begin
    aItemYaml.locationSpan.end_.b  := -1;
    aItemYaml.span.b  := Lexer.LinePos-1;
  end;

  FPrevSpan := aItemYaml.span;
end;

function TPas2YamlParser.ProcessParent_Before(const aType,
  aDebugHandler: String): TSemanticParentYaml;
begin
  (*
  - type : implementation
    name : implementation
    locationSpan : {start: [9,0], end: [19,4]}
    headerSpan : [89, 106]
    footerSpan : [164, 169]   //todo
    children :
  *)
  if FParentRecursion <= 0 then
  begin
    Result := FYamlMaster.children.AddNewParent;
    FParentStack.Clear;
  end
  else
    Result := FCurrentParent.children.AddNewParent;
  FParentStack.Push(Result);
  inc(FParentRecursion);
  Result.type_ := aType;
  Result.name  := Lexer.Token;
  Result.locationSpan.start.a := Lexer.LineNumber+1;
  Result.locationSpan.start.b := 0;
  Result.headerSpan.a  := Lexer.LinePos;
  Result.headerSpan.b  := Lexer.LinePos + length(Lexer.Line);

  UpdatePrevYamlLocation(Result);
  FPrevItem := nil;  //no reverse setting end location for this item
  FPrevSpan := nil;

  FCurrentParent := Result;
  EnterHandler(aDebugHandler);
end;

procedure TPas2YamlParser.ProcessParent_Next(
  const aParentYaml: TSemanticParentYaml; aIncludeLastLine: Boolean; aDebugHandler: String);
begin
  ExitHandler(aDebugHandler);
  //FCurrentParent := aParentYaml;
  dec(FParentRecursion);
  FParentStack.Pop();
  if FParentStack.Count > 0 then
    FCurrentParent := FParentStack.Peek
  else
    FCurrentParent := nil;

  aParentYaml.locationSpan.end_.a  := Lexer.LineNumber+1;
  //same line? then till end of line
  if (aParentYaml.locationSpan.start.a = aParentYaml.locationSpan.end_.a) or
     aIncludeLastLine then
  begin
    aParentYaml.locationSpan.end_.b  := length(Lexer.Line);
    //aParentYaml.span.b               := Lexer.LinePos + Length(Lexer.Line);
  end
  //else start of next line (without first char at pos 0!)
  else
  begin
    aParentYaml.locationSpan.end_.b  := -1;
    //aParentYaml.span.b  := Lexer.LinePos-1;
  end;

  //include last empty line in previous footer
  if FPrevSpan <> nil then
    FPrevSpan.b := Lexer.LinePos-1;

  //include last empty line in footer! (e.g. from "class" footerspan)
  if FPrevSpan <> nil then
    aParentYaml.footerSpan.a := FPrevSpan.b+1
  else
    aParentYaml.footerSpan.a := Lexer.LinePos;
  if aIncludeLastLine then
    aParentYaml.footerSpan.b := Lexer.LinePos + length(Lexer.Line)
  else
    aParentYaml.footerSpan.b := Lexer.LinePos-1;

  //invalid footer?
  if (aParentYaml.footerSpan.a >= aParentYaml.footerSpan.b) or
     (aParentYaml.footerSpan.a <= aParentYaml.headerSpan.b) then
  begin
    aParentYaml.footerSpan.a := 0;
    aParentYaml.footerSpan.b := -1;
  end
  else
    FPrevSpan := aParentYaml.footerSpan;
end;

procedure TPas2YamlParser.ProgramBlock;
begin
  EnterHandler('ProgramBlock');
  inherited;
  ExitHandler('ProgramBlock');
end;

procedure TPas2YamlParser.ProgramFile;
begin
  EnterHandler('ProgramFile');
  inherited;
  ExitHandler('ProgramFile');
end;

procedure TPas2YamlParser.PropertyDefault;
begin
  EnterHandler('PropertyDefault');
  inherited;
  ExitHandler('PropertyDefault');
end;

procedure TPas2YamlParser.PropertyInterface;
begin
  EnterHandler('PropertyInterface');
  inherited;
  ExitHandler('PropertyInterface');
end;

procedure TPas2YamlParser.PropertyName;
var itemyaml: TSemanticItemYaml;
begin
  itemyaml := ProcessItem_Before('property', False {full line}, 'PropertyName');
  FCurrentMethod := itemyaml;
  inherited;
  //  ProcessItem_Next(itemyaml, 'PropertyName');  see ClassProperty
  ExitHandler('PropertyName');

//  EnterHandler('PropertyName');
//  inherited;
//  ExitHandler('PropertyName');
end;

procedure TPas2YamlParser.PropertyParameterConst;
begin
  EnterHandler('PropertyParameterConst');
  inherited;
  ExitHandler('PropertyParameterConst');
end;

procedure TPas2YamlParser.PropertyParameterList;
begin
  EnterHandler('PropertyParameterList');
  inherited;
  ExitHandler('PropertyParameterList');
end;

procedure TPas2YamlParser.PropertySpecifiers;
begin
  EnterHandler('PropertySpecifiers');
  inherited;
  ExitHandler('PropertySpecifiers');
end;

procedure TPas2YamlParser.QualifiedIdentifier;
begin
  EnterHandler('QualifiedIdentifier');
  inherited;
  ExitHandler('QualifiedIdentifier');
end;

procedure TPas2YamlParser.QualifiedIdentifierList;
begin
  EnterHandler('QualifiedIdentifierList');
  inherited;
  ExitHandler('QualifiedIdentifierList');
end;

procedure TPas2YamlParser.RaiseStatement;
begin
  EnterHandler('RaiseStatement');
  inherited;
  ExitHandler('RaiseStatement');
end;

procedure TPas2YamlParser.ReadAccessIdentifier;
begin
  EnterHandler('ReadAccessIdentifier');
  inherited;
  ExitHandler('ReadAccessIdentifier');
end;

procedure TPas2YamlParser.RealIdentifier;
begin
  EnterHandler('RealIdentifier');
  inherited;
  ExitHandler('RealIdentifier');
end;

procedure TPas2YamlParser.RealType;
begin
  EnterHandler('RealType');
  inherited;
  ExitHandler('RealType');
end;

procedure TPas2YamlParser.RecordConstant;
begin
  EnterHandler('RecordConstant');
  inherited;
  ExitHandler('RecordConstant');
end;

procedure TPas2YamlParser.RecordFieldConstant;
begin
  EnterHandler('RecordFieldConstant');
  inherited;
  ExitHandler('RecordFieldConstant');
end;

procedure TPas2YamlParser.RecordType;
begin
  EnterHandler('RecordType');
  inherited;
  ExitHandler('RecordType');
end;

procedure TPas2YamlParser.RecordVariant;
begin
  EnterHandler('RecordVariant');
  inherited;
  ExitHandler('RecordVariant');
end;

procedure TPas2YamlParser.RelativeOperator;
begin
  EnterHandler('RelativeOperator');
  inherited;
  ExitHandler('RelativeOperator');
end;

procedure TPas2YamlParser.RepeatStatement;
begin
  EnterHandler('RepeatStatement');
  inherited;
  ExitHandler('RepeatStatement');
end;

procedure TPas2YamlParser.RequiresClause;
begin
  EnterHandler('RequiresClause');
  inherited;
  ExitHandler('RequiresClause');
end;

procedure TPas2YamlParser.RequiresIdentifier;
begin
  EnterHandler('RequiresIdentifier');
  inherited;
  ExitHandler('RequiresIdentifier');
end;

procedure TPas2YamlParser.ResolutionInterfaceName;
begin
  EnterHandler('ResolutionInterfaceName');
  inherited;
  ExitHandler('ResolutionInterfaceName');
end;

procedure TPas2YamlParser.ResourceDeclaration;
begin
  EnterHandler('ResourceDeclaration');
  inherited;
  ExitHandler('ResourceDeclaration');
end;

procedure TPas2YamlParser.ReturnType;
begin
  EnterHandler('ReturnType');
  inherited;
  ExitHandler('ReturnType');
end;

procedure TPas2YamlParser.Run(aUnitName: String; SourceStream: TCustomMemoryStream);
begin
  {$IFDEF NOT OXYGENE}
  FYamlMaster.Free; //remove old
  {$ENDIF}
  FYamlMaster    := TSemanticMasterYaml.Create;
  FCurrentParent := nil;

  FUnitName := aUnitName;
  inherited;
end;

procedure TPas2YamlParser.SetConstructor;
begin
  EnterHandler('SetConstructor');
  inherited;
  ExitHandler('SetConstructor');
end;

procedure TPas2YamlParser.SetElement;
begin
  EnterHandler('SetElement');
  inherited;
  ExitHandler('SetElement');
end;

procedure TPas2YamlParser.SetType;
begin
  EnterHandler('SetType');
  inherited;
  ExitHandler('SetType');
end;

procedure TPas2YamlParser.SimpleExpression;
begin
  EnterHandler('SimpleExpression');
  inherited;
  ExitHandler('SimpleExpression');
end;

procedure TPas2YamlParser.SimpleStatement;
begin
  EnterHandler('SimpleStatement');
  inherited;
  ExitHandler('SimpleStatement');
end;

procedure TPas2YamlParser.SimpleType;
begin
  EnterHandler('SimpleType');
  inherited;
  ExitHandler('SimpleType');
end;

procedure TPas2YamlParser.SkipAnsiComment;
begin
  EnterHandler('SkipAnsiComment');
  inherited;
  ExitHandler('SkipAnsiComment');
end;

procedure TPas2YamlParser.SkipBorComment;
begin
  EnterHandler('SkipBorComment');
  inherited;
  ExitHandler('SkipBorComment');
end;

procedure TPas2YamlParser.SkipCRLF;
begin
  EnterHandler('SkipCRLF');
  inherited;
  ExitHandler('SkipCRLF');
end;

procedure TPas2YamlParser.SkipCRLFco;
begin
  EnterHandler('SkipCRLFco');
  inherited;
  ExitHandler('SkipCRLFco');
end;

procedure TPas2YamlParser.SkipSlashesComment;
begin
  EnterHandler('SkipSlashesComment');
  inherited;
  ExitHandler('SkipSlashesComment');
end;

procedure TPas2YamlParser.SkipSpace;
begin
  EnterHandler('SkipSpace');
  inherited;
  ExitHandler('SkipSpace');
end;

procedure TPas2YamlParser.Statement;
begin
  EnterHandler('Statement');
  inherited;
  ExitHandler('Statement');
end;

procedure TPas2YamlParser.StatementList;
begin
  EnterHandler('StatementList');
  inherited;
  ExitHandler('StatementList');
end;

procedure TPas2YamlParser.StorageDefault;
begin
  EnterHandler('StorageDefault');
  inherited;
  ExitHandler('StorageDefault');
end;

procedure TPas2YamlParser.StorageExpression;
begin
  EnterHandler('StorageExpression');
  inherited;
  ExitHandler('StorageExpression');
end;

procedure TPas2YamlParser.StorageIdentifier;
begin
  EnterHandler('StorageIdentifier');
  inherited;
  ExitHandler('StorageIdentifier');
end;

procedure TPas2YamlParser.StorageNoDefault;
begin
  EnterHandler('StorageNoDefault');
  inherited;
  ExitHandler('StorageNoDefault');
end;

procedure TPas2YamlParser.StorageSpecifier;
begin
  EnterHandler('StorageSpecifier');
  inherited;
  ExitHandler('StorageSpecifier');
end;

procedure TPas2YamlParser.StorageStored;
begin
  EnterHandler('StorageStored');
  inherited;
  ExitHandler('StorageStored');
end;

procedure TPas2YamlParser.StringIdentifier;
begin
  EnterHandler('StringIdentifier');
  inherited;
  ExitHandler('StringIdentifier');
end;

procedure TPas2YamlParser.StringStatement;
begin
  EnterHandler('StringStatement');
  inherited;
  ExitHandler('StringStatement');
end;

procedure TPas2YamlParser.StringType;
begin
  EnterHandler('StringType');
  inherited;
  ExitHandler('StringType');
end;

procedure TPas2YamlParser.StructuredType;
begin
  EnterHandler('StructuredType');
  inherited;
  ExitHandler('StructuredType');
end;

procedure TPas2YamlParser.SubrangeType;
begin
  EnterHandler('SubrangeType');
  inherited;
  ExitHandler('SubrangeType');
end;

procedure TPas2YamlParser.TagField;
begin
  EnterHandler('TagField');
  inherited;
  ExitHandler('TagField');
end;

procedure TPas2YamlParser.TagFieldName;
begin
  EnterHandler('TagFieldName');
  inherited;
  ExitHandler('TagFieldName');
end;

procedure TPas2YamlParser.TagFieldTypeName;
begin
  EnterHandler('TagFieldTypeName');
  inherited;
  ExitHandler('TagFieldTypeName');
end;

procedure TPas2YamlParser.Term;
begin
  EnterHandler('Term');
  inherited;
  ExitHandler('Term');
end;

procedure TPas2YamlParser.TryStatement;
begin
  EnterHandler('TryStatement');
  inherited;
  ExitHandler('TryStatement');
end;

procedure TPas2YamlParser.TypeArgs;
begin
  EnterHandler('TypeArgs');
  inherited;
  ExitHandler('TypeArgs');
end;

procedure TPas2YamlParser.TypedConstant;
begin
  EnterHandler('TypedConstant');
  inherited;
  ExitHandler('TypedConstant');
end;

procedure TPas2YamlParser.TypeDeclaration;
begin
  EnterHandler('TypeDeclaration');
  FCurrentType := Lexer.Token;
  inherited;
  ExitHandler('TypeDeclaration');
end;

procedure TPas2YamlParser.TypeId;
begin
  EnterHandler('TypeId');
  inherited;
  ExitHandler('TypeId');
end;

procedure TPas2YamlParser.TypeKind;
begin
  EnterHandler('TypeKind');
  inherited;
  ExitHandler('TypeKind');
end;

procedure TPas2YamlParser.TypeName;
begin
  EnterHandler('TypeName');
  inherited;
  ExitHandler('TypeName');
end;

procedure TPas2YamlParser.TypeParamDecl;
begin
  EnterHandler('TypeParamDecl');
  inherited;
  ExitHandler('TypeParamDecl');
end;

procedure TPas2YamlParser.TypeParamDeclList;
begin
  EnterHandler('TypeParamDeclList');
  inherited;
  ExitHandler('TypeParamDeclList');
end;

procedure TPas2YamlParser.TypeParamList;
begin
  EnterHandler('TypeParamList');
  inherited;
  ExitHandler('TypeParamList');
end;

procedure TPas2YamlParser.TypeParams;
begin
  EnterHandler('TypeParams');
  inherited;
  ExitHandler('TypeParams');
end;

procedure TPas2YamlParser.TypeSection;
var parentyaml: TSemanticParentYaml;
begin
  parentyaml := ProcessParent_Before('type', 'TypeSection');
  //EnterHandler('UsesClause');
  inherited;
  //ExitHandler('UsesClause');
  ProcessParent_Next(parentyaml, False{no full last line}, 'TypeSection');
end;
(*
var
  typeyaml: TSemanticParentYaml;
begin
  (*
  - type : type
    name : type
    locationSpan : {start: [4,0], end: [9,0]}
    headerSpan : [26, 33]
    footerSpan : [0, -1]
    children :
  )
  typeyaml := FCurrentParent.children.AddNewParent;
  typeyaml.type_ := 'type';
  typeyaml.name  := Lexer.Token;
  typeyaml.locationSpan.start.a := Lexer.LineNumber+1;
  typeyaml.locationSpan.start.b := 0;
  typeyaml.headerSpan.a  := Lexer.LinePos;
  typeyaml.headerSpan.b  := Lexer.LinePos + Length(Lexer.Line);

  UpdatePrevYamlLocation(typeyaml);
  //UpdateNextYamlLocation(typeyaml); doesnt work because of missing span info?

  FCurrentParent := typeyaml;
  EnterHandler('TypeSection');
  inherited;
  ExitHandler('TypeSection');
  FCurrentParent := typeyaml;

  typeyaml.locationSpan.end_.a  := Lexer.LineNumber+1;
  typeyaml.locationSpan.end_.b  := -1; //-1; //Length(Lexer.Line);  "implementation"
  //typeyaml.footerSpan.a  := Lexer.LinePos;
  //typeyaml.footerSpan.b  := Lexer.LinePos; // + Length(Lexer.Line);
  //no footer?
  typeyaml.footerSpan.a  := 0;
  typeyaml.footerSpan.b  := -1;
  //no footer, then keep last known span
  //FPrevSpan := classyaml.footerSpan;
end;
*)

procedure TPas2YamlParser.UnitFile;
begin
  EnterHandler('UnitFile');
  inherited;
  ExitHandler('UnitFile');
end;

procedure TPas2YamlParser.UnitId;
begin
  EnterHandler('UnitId');
  inherited;
  ExitHandler('UnitId');
end;

procedure TPas2YamlParser.UnitName;
begin
  EnterHandler('UnitName');
  inherited;
  ExitHandler('UnitName');
end;

procedure TPas2YamlParser.UpdateNextYamlLocation(aNode: TBaseSemanticYaml);
var
  item: TSemanticItemYaml;
  parent: TSemanticParentYaml;
begin
  //update start location of current part with previous end location
  //so e.g. all comments after previous token are "spanned"
  if aNode is TSemanticItemYaml then
  begin
    item := (aNode as TSemanticItemYaml);

    if FPrevItem <> nil then
    begin
      item.locationSpan.start.a := FPrevItem.locationSpan.end_.a;
      item.locationSpan.start.b := FPrevItem.locationSpan.end_.b+1;
      item.span.a := FPrevItem.span.b + 1;
    end
    else if FPrevParent <> nil then
    begin
      item.locationSpan.start.a := FPrevParent.headerSpan.a;
      item.locationSpan.start.b := FPrevParent.headerSpan.b+1;
      //item.span.a := FPrevParent.headerSpan.b + 1;   missing info?
    end;
  end
  else if aNode is TSemanticParentYaml then
  begin
    parent := aNode as TSemanticParentYaml;

    if FPrevItem <> nil then
    begin
//      parent.locationSpan.start.a := FPrevItem.locationSpan.end_.a;
//      parent.locationSpan.start.b := FPrevItem.locationSpan.end_.b+1;
//      parent.span.a := FPrevItem.span.b + 1;
    end
    else if FPrevParent <> nil then
    begin
      //parent.locationSpan.start.a := FPrevParent.headerSpan.a;
      //parent.locationSpan.start.b := FPrevParent.headerSpan.b+1;
      parent.headerSpan.a := FPrevParent.headerSpan.b+1;
      //parent.headerSpan.b := FPrevParent.headerSpan.b+1;
    end;
  end;

  FPrevParent := nil;
  FPrevItem   := nil;
  if aNode is TSemanticParentYaml then
    FPrevParent := aNode as TSemanticParentYaml
  else if aNode is TSemanticItemYaml then
    FPrevItem := aNode as TSemanticItemYaml
end;

procedure TPas2YamlParser.UpdatePrevYamlLocation(aNode: TBaseSemanticYaml);
begin
  //update end location of previous part (so e.g. all comments till next
  //token are "spanned")
  if FPrevItem <> nil then
  begin
    FPrevItem.locationSpan.end_.a := Lexer.LineNumber+1;
    FPrevItem.locationSpan.end_.b := -1;
    FPrevItem.span.b := Lexer.LinePos-1;
  end
  else if FPrevParent <> nil then
  begin
//    FPrevParent.locationSpan.end_.a := Lexer.LineNumber+1;
//    FPrevParent.locationSpan.end_.b := -1;
    FPrevParent.headerSpan.b := Lexer.LinePos-1;
  end;

  FPrevParent := nil;
  FPrevItem   := nil;
  if aNode is TSemanticParentYaml then
    FPrevParent := aNode as TSemanticParentYaml
  else if aNode is TSemanticItemYaml then
    FPrevItem := aNode as TSemanticItemYaml
end;

procedure TPas2YamlParser.UsedUnitName;
var itemyaml: TSemanticItemYaml;
begin
  itemyaml := ProcessItem_Before('used unit', True{exact pos}, 'UsedUnitName');
  FCurrentMethod := itemyaml;
  inherited;
  ProcessItem_Next(itemyaml, True{exact pos}, 'UsedUnitName');
  //ExitHandler('UsedUnitName');
end;

procedure TPas2YamlParser.UsedUnitsList;
begin
  EnterHandler('UsedUnitsList');
  inherited;
  ExitHandler('UsedUnitsList');
end;

procedure TPas2YamlParser.UsesClause;
var parentyaml: TSemanticParentYaml;
begin
  parentyaml := ProcessParent_Before('uses', 'UsesClause');
  //EnterHandler('UsesClause');
  inherited;
  //ExitHandler('UsesClause');
  ProcessParent_Next(parentyaml, False{no full last line}, 'UsesClause');
end;

procedure TPas2YamlParser.VarAbsolute;
begin
  EnterHandler('VarAbsolute');
  inherited;
  ExitHandler('VarAbsolute');
end;

procedure TPas2YamlParser.VarDeclaration;
begin
  EnterHandler('VarDeclaration');
  inherited;
  ExitHandler('VarDeclaration');
end;

procedure TPas2YamlParser.VarEqual;
begin
  EnterHandler('VarEqual');
  inherited;
  ExitHandler('VarEqual');
end;

procedure TPas2YamlParser.Variable;
begin
  EnterHandler('Variable');
  inherited;
  ExitHandler('Variable');
end;

procedure TPas2YamlParser.VariableList;
begin
  EnterHandler('VariableList');
  inherited;
  ExitHandler('VariableList');
end;

procedure TPas2YamlParser.VariableReference;
begin
  EnterHandler('VariableReference');
  inherited;
  ExitHandler('VariableReference');
end;

procedure TPas2YamlParser.VariableTwo;
begin
  EnterHandler('VariableTwo');
  inherited;
  ExitHandler('VariableTwo');
end;

procedure TPas2YamlParser.VariantIdentifier;
begin
  EnterHandler('VariantIdentifier');
  inherited;
  ExitHandler('VariantIdentifier');
end;

procedure TPas2YamlParser.VariantSection;
begin
  EnterHandler('VariantSection');
  inherited;
  ExitHandler('VariantSection');
end;

procedure TPas2YamlParser.VarName;
begin
  EnterHandler('VarName');
  inherited;
  ExitHandler('VarName');
end;

procedure TPas2YamlParser.VarNameList;
begin
  EnterHandler('VarNameList');
  inherited;
  ExitHandler('VarNameList');
end;

procedure TPas2YamlParser.VarParameter;
begin
  EnterHandler('VarParameter');
  inherited;
  ExitHandler('VarParameter');
end;

procedure TPas2YamlParser.VarSection;
begin
  EnterHandler('VarSection');
  inherited;
  ExitHandler('VarSection');
end;

procedure TPas2YamlParser.VisibilityAutomated;
begin
  EnterHandler('VisibilityAutomated');
  inherited;
  ExitHandler('VisibilityAutomated');
end;

procedure TPas2YamlParser.VisibilityPrivate;
begin
  if FPrevClassVisibility <> nil then
    ProcessParent_Next(FPrevClassVisibility, False, 'end of Visibility');
  FPrevClassVisibility := ProcessParent_Before('visibility', 'VisibilityPrivate');
  inherited;
//  ProcessItem_Next(itemyaml, False {full line}, 'VisibilityPrivate');
//  EnterHandler('VisibilityPrivate');
//  inherited;
//  ExitHandler('VisibilityPrivate');
end;

procedure TPas2YamlParser.VisibilityProtected;
begin
  if FPrevClassVisibility <> nil then
    ProcessParent_Next(FPrevClassVisibility, False, 'end of Visibility');
  FPrevClassVisibility := ProcessParent_Before('visibility', 'VisibilityProtected');
  inherited;
//  ProcessItem_Next(itemyaml, False {full line}, 'VisibilityProtected');
//  EnterHandler('VisibilityProtected');
//  inherited;
//  ExitHandler('VisibilityProtected');
end;

procedure TPas2YamlParser.VisibilityPublic;
//var itemyaml: TSemanticParentYaml;
begin
  if FPrevClassVisibility <> nil then
    ProcessParent_Next(FPrevClassVisibility, False, 'end of Visibility');
  FPrevClassVisibility := ProcessParent_Before('visibility', 'VisibilityPublic');
  inherited;
  //ProcessParent_Next(itemyaml, False, 'VisibilityPublic');
//  EnterHandler('VisibilityPublic');
//  inherited;
//  ExitHandler('VisibilityPublic');
end;

procedure TPas2YamlParser.VisibilityPublished;
begin
  if FPrevClassVisibility <> nil then
    ProcessParent_Next(FPrevClassVisibility, False, 'end of Visibility');
  FPrevClassVisibility := ProcessParent_Before('visibility', 'VisibilityPublished');
  inherited;
//  ProcessItem_Next(itemyaml, False {full line}, 'VisibilityPublished');
//  EnterHandler('VisibilityPublished');
//  inherited;
//  ExitHandler('VisibilityPublished');
end;

procedure TPas2YamlParser.VisibilityUnknown;
begin
  EnterHandler('VisibilityUnknown');
  inherited;
  ExitHandler('VisibilityUnknown');
end;

procedure TPas2YamlParser.WhileStatement;
begin
  EnterHandler('WhileStatement');
  inherited;
  ExitHandler('WhileStatement');
end;

procedure TPas2YamlParser.WithStatement;
begin
  EnterHandler('WithStatement');
  inherited;
  ExitHandler('WithStatement');
end;

procedure TPas2YamlParser.WriteAccessIdentifier;
begin
  EnterHandler('WriteAccessIdentifier');
  inherited;
  ExitHandler('WriteAccessIdentifier');
end;

//procedure TPas2YamlParser.WriteOutput(const aLine: string);
//begin
//  if Assigned(OnYamlOutput) then
//    OnYamlOutput(FOutputIndent + aLine);
//end;

//procedure TPas2YamlParser.WriteOutput(const aLine: string; const Args: array of const);
//begin
//  if Assigned(OnYamlOutput) then
//    OnYamlOutput(FOutputIndent + Format(aLine, Args));
//end;

//procedure TPas2YamlParser.ReplaceOutput(const aSearch, aReplace: string);
//begin
//  if Assigned(OnReplaceOutput) then
//    OnReplaceOutput(aSearch, aReplace);
//end;

procedure TPas2YamlParser.EnterHandler(const aName: String);
begin
  if assigned(OnDebugOutput) then
    {$IFDEF OXYGENE}
    OnDebugOutput(String.Format('{0}{1} -> {2} at {3}:{4}', FIndent, aName, Lexer.Token, Lexer.LineNumber+1, Lexer.LinePos));
    {$ELSE}
    OnDebugOutput(Format('%s%s -> %s at %d:%d',
                         [FIndent, aName, Lexer.Token, Lexer.LineNumber+1, Lexer.LinePos]));
    {$ENDIF}    
  FIndent := FIndent + '+ ';
end;

procedure TPas2YamlParser.ExitHandler(const aName: String);
begin
  FIndent := Copy(FIndent, 1, length(FIndent)-2);
  if assigned(OnDebugOutput) then
    {$IFDEF OXYGENE}
    OnDebugOutput(String.Format('{0}{1} <- {2} at {3}:{4}', FIndent, aName, Lexer.Token, Lexer.LineNumber+1, Lexer.LinePos));
    {$ELSE}
    OnDebugOutput(Format('%s%s <- %s at %d:%d',
                         [FIndent, aName, Lexer.Token, Lexer.LineNumber+1, Lexer.LinePos]));
    {$ENDIF}  
end;

procedure TPas2YamlParser.CustomAttribute;
begin

end;

end.

(*
---
type : file
name : /path/to/file
locationSpan : {start: [1,0], end: [19,4]}
footerSpan : [0, -1]
parsingErrorsDetected : false
children :

  - type : unit
    name : Unit1
    locationSpan : {start: [1,0], end: [1,13]}
    span : [0, 12]

  - type : interface
    name : interface
    locationSpan : {start: [2,0], end: [9,0]}
    headerSpan : [13, 25]
    footerSpan : [0, -1]
    children :

      - type : type
        name : type
        locationSpan : {start: [4,0], end: [9,0]}
        headerSpan : [26, 33]
        footerSpan : [0, -1]
        children :

          - type : class
            name : TTest
            locationSpan : {start: [6,0], end: [9,0]}
            headerSpan : [34, 59]
            footerSpan : [81, 88]
            children :

              - type : procedure declaration
                name : Test
                locationSpan : {start: [7,0], end: [7,21]}
                span : [60, 80]

  - type : implementation
    name : implementation
    locationSpan : {start: [9,0], end: [19,4]}
    headerSpan : [89, 106]
    footerSpan : [164, 169]
    children :

      - type : procedure
        name : TTest.Test
        locationSpan : {start: [11,0], end: [18,0]}
        span : [107, 163]
*)
