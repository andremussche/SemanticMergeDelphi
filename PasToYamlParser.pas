unit PasToYamlParser;

interface

uses
  Classes,
  CastaliaSimplePasPar;

type
  TStringEvent  = reference to procedure(const aText: string);
  TReplaceEvent = reference to procedure(const aSearch, aReplace: string);

  TPas2YamlParser = class(TmwSimplePasPar)
  protected
    FIndent: string;
    procedure EnterHandler(const aName: string);
    procedure ExitHandler(const aName: string);
  protected
    FUnitName: string;
    FCurrentType: string;
    FOutputIndent: string;
    procedure WriteOutput(const aLine: string);overload;
    procedure WriteOutput(const aLine: string; const Args: array of const);overload;
    procedure ReplaceOutput(const aSearch, aReplace: string);
  private
    FOnYamlOutput: TStringEvent;
    FOnDebugOutput: TStringEvent;
    FOnReplaceOutput: TReplaceEvent;
  public
    procedure Run(aUnitName: string; SourceStream: TCustomMemoryStream); override;

    property  OnDebugOutput: TStringEvent read FOnDebugOutput write FOnDebugOutput;
    property  OnYamlOutput : TStringEvent read FOnYamlOutput write  FOnYamlOutput;
    property  OnReplaceOutput: TReplaceEvent read FOnReplaceOutput write FOnReplaceOutput;
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

uses
  SysUtils;

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
begin
  EnterHandler('ClassField');
  inherited;
  ExitHandler('ClassField');
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
  ExitHandler('ClassFunctionHeading');
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
  ExitHandler('ClassProcedureHeading');
end;

procedure TPas2YamlParser.ClassProperty;
begin
  EnterHandler('ClassProperty');
  inherited;
  ExitHandler('ClassProperty');
end;

procedure TPas2YamlParser.ClassReferenceType;
begin
  EnterHandler('ClassReferenceType');
  inherited;
  ExitHandler('ClassReferenceType');
end;

procedure TPas2YamlParser.ClassType;
begin
//  - type : class
//    name : PlasticVisitor
//    locationSpan : {start: [7,0], end: [52,0]}
//    headerSpan : [151, 202]
//    footerSpan : [1751, 1751]
//    children :
  WriteOutput('- type : class');
  WriteOutput('  name : %s', [FCurrentType]);
  WriteOutput('  locationSpan : {start: [%d,%d], end: [&classposendline&,&classposendchar&]}', [Lexer.LineNumber, 0]);
  WriteOutput('  headerSpan : [%d,%d]', [Lexer.LinePos, Lexer.LinePos + Length(Lexer.Line)]);
  WriteOutput('  footerSpan : [&classfooterspan&]', []);
  WriteOutput('  children :');
  WriteOutput('');
  FOutputIndent := FOutputIndent + '  ';

  EnterHandler('ClassType');
  inherited;
  ExitHandler('ClassType');

  FOutputIndent := Copy(FOutputIndent, 1, Length(FOutputIndent)-2);
  ReplaceOutput('&classposendline&', IntToStr(Lexer.LineNumber));
  ReplaceOutput('&classposendchar&', IntToStr(Length(Lexer.Line)));
  ReplaceOutput('&classfooterspan&', IntToStr(Lexer.RunPos) + ',' + IntToStr(Lexer.RunPos));
end;

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
begin
  EnterHandler('FunctionMethodName');
  inherited;
  ExitHandler('FunctionMethodName');
end;

procedure TPas2YamlParser.FunctionProcedureBlock;
begin
  EnterHandler('FunctionProcedureBlock');
  inherited;
  ExitHandler('FunctionProcedureBlock');
end;

procedure TPas2YamlParser.FunctionProcedureName;
begin
  EnterHandler('FunctionProcedureName');
  inherited;
  ExitHandler('FunctionProcedureName');
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
begin
  EnterHandler('ImplementationSection');
  inherited;
  ExitHandler('ImplementationSection');
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
begin
  EnterHandler('InterfaceSection');
  inherited;
  ExitHandler('InterfaceSection');
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
begin
  WriteOutput('---');
  WriteOutput('type : file');
  WriteOutput('name : %s', [FUnitName]);
  WriteOutput('locationSpan : {start: [%d,%d], end: [&fileposendline&,&fileposendchar&]}', [Lexer.LineNumber, Lexer.LinePos]);
  //locationSpan : {start: [1,0], end: [52,0]}
  WriteOutput('footerSpan : [&footerspan&]', []);
  //footerSpan : [1752, 1751]
  //parsingErrorsDetected : true
  WriteOutput('children :');
  WriteOutput('');
  FOutputIndent := FOutputIndent + '  ';

  EnterHandler('ParseFile');
  inherited;
  ExitHandler('ParseFile');

  FOutputIndent := Copy(FOutputIndent, 1, Length(FOutputIndent)-2);
  ReplaceOutput('&fileposendline&', IntToStr(Lexer.LineNumber));
  ReplaceOutput('&fileposendchar&', IntToStr(Length(Lexer.Line)));
  ReplaceOutput('&footerspan&', IntToStr(Lexer.RunPos) + ',' + IntToStr(Lexer.RunPos));
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
  EnterHandler('ProcedureMethodDeclaration');
  inherited;
  ExitHandler('ProcedureMethodDeclaration');
end;

procedure TPas2YamlParser.ProcedureMethodName;
begin
(*
     - type : method
       name : preVisit
       locationSpan : {start: [11,4], end: [27,3]}
       span : [251, 865]
*)
  WriteOutput('- type : method');
  WriteOutput('  name : %s', [Lexer.Token]);
  WriteOutput('  locationSpan : {start: [%d,%d], end: [%d,%d]}', [Lexer.LineNumber, 0, Lexer.LineNumber, Length(Lexer.Line)]);
  WriteOutput('  span : [%d,%d]', [Lexer.LinePos, Lexer.LinePos + Length(Lexer.Line)]);
  WriteOutput('');
  FOutputIndent := FOutputIndent + '  ';

  EnterHandler('ProcedureMethodName');
  inherited;
  ExitHandler('ProcedureMethodName');

  FOutputIndent := Copy(FOutputIndent, 1, Length(FOutputIndent)-2);
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
begin
  EnterHandler('PropertyName');
  inherited;
  ExitHandler('PropertyName');
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

procedure TPas2YamlParser.Run(aUnitName: string; SourceStream: TCustomMemoryStream);
begin
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
begin
  EnterHandler('TypeSection');
  inherited;
  ExitHandler('TypeSection');
end;

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

procedure TPas2YamlParser.UsedUnitName;
begin
  EnterHandler('UsedUnitName');
  inherited;
  ExitHandler('UsedUnitName');
end;

procedure TPas2YamlParser.UsedUnitsList;
begin
  EnterHandler('UsedUnitsList');
  inherited;
  ExitHandler('UsedUnitsList');
end;

procedure TPas2YamlParser.UsesClause;
begin
  EnterHandler('UsesClause');
  inherited;
  ExitHandler('UsesClause');
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
  EnterHandler('VisibilityPrivate');
  inherited;
  ExitHandler('VisibilityPrivate');
end;

procedure TPas2YamlParser.VisibilityProtected;
begin
  EnterHandler('VisibilityProtected');
  inherited;
  ExitHandler('VisibilityProtected');
end;

procedure TPas2YamlParser.VisibilityPublic;
begin
  EnterHandler('VisibilityPublic');
  inherited;
  ExitHandler('VisibilityPublic');
end;

procedure TPas2YamlParser.VisibilityPublished;
begin
  EnterHandler('VisibilityPublished');
  inherited;
  ExitHandler('VisibilityPublished');
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

procedure TPas2YamlParser.WriteOutput(const aLine: string);
begin
  if Assigned(OnYamlOutput) then
    OnYamlOutput(FOutputIndent + aLine);
end;

procedure TPas2YamlParser.WriteOutput(const aLine: string; const Args: array of const);
begin
  if Assigned(OnYamlOutput) then
    OnYamlOutput(FOutputIndent + Format(aLine, Args));
end;

procedure TPas2YamlParser.ReplaceOutput(const aSearch, aReplace: string);
begin
  if Assigned(OnReplaceOutput) then
    OnReplaceOutput(aSearch, aReplace);
end;

procedure TPas2YamlParser.EnterHandler(const aName: string);
begin
  if Assigned(OnDebugOutput) then
    OnDebugOutput(Format('%s%s -> %s at %d:%d',
                         [FIndent, aName, Lexer.Token, Lexer.LineNumber, Lexer.LinePos]));
  FIndent := FIndent + '+ ';
end;

procedure TPas2YamlParser.ExitHandler(const aName: string);
begin
  FIndent := Copy(FIndent, 1, Length(FIndent)-2);
  if Assigned(OnDebugOutput) then
    OnDebugOutput(Format('%s%s <- %s at %d:%d',
                         [FIndent, aName, Lexer.Token, Lexer.LineNumber, Lexer.LinePos]));
end;

end.
