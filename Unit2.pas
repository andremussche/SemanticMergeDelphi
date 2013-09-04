unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  CastaliaSimplePasPar;

type
  TForm2 = class(TForm)
    mmoSource: TMemo;
    Button1: TButton;
    mmoResult: TMemo;
    mmoOutput: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TMySimplePasPar = class(TmwSimplePasPar)
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
  public
    procedure Run(aUnitName: string; SourceStream: TCustomMemoryStream); override;
  protected
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

var
  Form2: TForm2;

implementation

{$R *.dfm}

{ TMySimplePasPar }

procedure TMySimplePasPar.AccessSpecifier;
begin
  EnterHandler('AccessSpecifier');
  inherited;
  ExitHandler('AccessSpecifier');
end;

procedure TMySimplePasPar.AdditiveOperator;
begin
  EnterHandler('AdditiveOperator');
  inherited;
  ExitHandler('AdditiveOperator');
end;

procedure TMySimplePasPar.AncestorId;
begin
  EnterHandler('AncestorId');
  inherited;
  ExitHandler('AncestorId');
end;

procedure TMySimplePasPar.AncestorIdList;
begin
  EnterHandler('AncestorIdList');
  inherited;
  ExitHandler('AncestorIdList');
end;

procedure TMySimplePasPar.AnonymousMethod;
begin
  EnterHandler('AnonymousMethod');
  inherited;
  ExitHandler('AnonymousMethod');
end;

procedure TMySimplePasPar.AnonymousMethodType;
begin
  EnterHandler('AnonymousMethodType');
  inherited;
  ExitHandler('AnonymousMethodType');
end;

procedure TMySimplePasPar.ArrayConstant;
begin
  EnterHandler('ArrayConstant');
  inherited;
  ExitHandler('ArrayConstant');
end;

procedure TMySimplePasPar.ArrayType;
begin
  EnterHandler('ArrayType');
  inherited;
  ExitHandler('ArrayType');
end;

procedure TMySimplePasPar.AsmStatement;
begin
  EnterHandler('AsmStatement');
  inherited;
  ExitHandler('AsmStatement');
end;

procedure TMySimplePasPar.Block;
begin
  EnterHandler('Block');
  inherited;
  ExitHandler('Block');
end;

procedure TMySimplePasPar.CaseLabel;
begin
  EnterHandler('CaseLabel');
  inherited;
  ExitHandler('CaseLabel');
end;

procedure TMySimplePasPar.CaseSelector;
begin
  EnterHandler('CaseSelector');
  inherited;
  ExitHandler('CaseSelector');
end;

procedure TMySimplePasPar.CaseStatement;
begin
  EnterHandler('CaseStatement');
  inherited;
  ExitHandler('CaseStatement');
end;

procedure TMySimplePasPar.CharString;
begin
  EnterHandler('CharString');
  inherited;
  ExitHandler('CharString');
end;

procedure TMySimplePasPar.ClassClass;
begin
  EnterHandler('ClassClass');
  inherited;
  ExitHandler('ClassClass');
end;

procedure TMySimplePasPar.ClassField;
begin
  EnterHandler('ClassField');
  inherited;
  ExitHandler('ClassField');
end;

procedure TMySimplePasPar.ClassForward;
begin
  EnterHandler('ClassForward');
  inherited;
  ExitHandler('ClassForward');
end;

procedure TMySimplePasPar.ClassFunctionHeading;
begin
  EnterHandler('ClassFunctionHeading');
  inherited;
  ExitHandler('ClassFunctionHeading');
end;

procedure TMySimplePasPar.ClassHeritage;
begin
  EnterHandler('ClassHeritage');
  inherited;
  ExitHandler('ClassHeritage');
end;

procedure TMySimplePasPar.ClassMemberList;
begin
  EnterHandler('ClassMemberList');
  inherited;
  ExitHandler('ClassMemberList');
end;

procedure TMySimplePasPar.ClassMethodDirective;
begin
  EnterHandler('ClassMethodDirective');
  inherited;
  ExitHandler('ClassMethodDirective');
end;

procedure TMySimplePasPar.ClassMethodHeading;
begin
  EnterHandler('ClassMethodHeading');
  inherited;
  ExitHandler('ClassMethodHeading');
end;

procedure TMySimplePasPar.ClassMethodOrProperty;
begin
  EnterHandler('ClassMethodOrProperty');
  inherited;
  ExitHandler('ClassMethodOrProperty');
end;

procedure TMySimplePasPar.ClassMethodResolution;
begin
  EnterHandler('ClassMethodResolution');
  inherited;
  ExitHandler('ClassMethodResolution');
end;

procedure TMySimplePasPar.ClassProcedureHeading;
begin
  EnterHandler('ClassProcedureHeading');
  inherited;
  ExitHandler('ClassProcedureHeading');
end;

procedure TMySimplePasPar.ClassProperty;
begin
  EnterHandler('ClassProperty');
  inherited;
  ExitHandler('ClassProperty');
end;

procedure TMySimplePasPar.ClassReferenceType;
begin
  EnterHandler('ClassReferenceType');
  inherited;
  ExitHandler('ClassReferenceType');
end;

procedure TMySimplePasPar.ClassType;
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

procedure TMySimplePasPar.ClassTypeEnd;
begin
  EnterHandler('ClassTypeEnd');
  inherited;
  ExitHandler('ClassTypeEnd');
end;

procedure TMySimplePasPar.ClassVisibility;
begin
  EnterHandler('ClassVisibility');
  inherited;
  ExitHandler('ClassVisibility');
end;

procedure TMySimplePasPar.CompoundStatement;
begin
  EnterHandler('CompoundStatement');
  inherited;
  ExitHandler('CompoundStatement');
end;

procedure TMySimplePasPar.ConstantColon;
begin
  EnterHandler('ConstantColon');
  inherited;
  ExitHandler('ConstantColon');
end;

procedure TMySimplePasPar.ConstantDeclaration;
begin
  EnterHandler('ConstantDeclaration');
  inherited;
  ExitHandler('ConstantDeclaration');
end;

procedure TMySimplePasPar.ConstantEqual;
begin
  EnterHandler('ConstantEqual');
  inherited;
  ExitHandler('ConstantEqual');
end;

procedure TMySimplePasPar.ConstantExpression;
begin
  EnterHandler('ConstantExpression');
  inherited;
  ExitHandler('ConstantExpression');
end;

procedure TMySimplePasPar.ConstantName;
begin
  EnterHandler('ConstantName');
  inherited;
  ExitHandler('ConstantName');
end;

procedure TMySimplePasPar.ConstantType;
begin
  EnterHandler('ConstantType');
  inherited;
  ExitHandler('ConstantType');
end;

procedure TMySimplePasPar.ConstantValue;
begin
  EnterHandler('ConstantValue');
  inherited;
  ExitHandler('ConstantValue');
end;

procedure TMySimplePasPar.ConstantValueTyped;
begin
  EnterHandler('ConstantValueTyped');
  inherited;
  ExitHandler('ConstantValueTyped');
end;

procedure TMySimplePasPar.ConstParameter;
begin
  EnterHandler('ConstParameter');
  inherited;
  ExitHandler('ConstParameter');
end;

procedure TMySimplePasPar.Constraint;
begin
  EnterHandler('Constraint');
  inherited;
  ExitHandler('Constraint');
end;

procedure TMySimplePasPar.ConstraintList;
begin
  EnterHandler('ConstraintList');
  inherited;
  ExitHandler('ConstraintList');
end;

procedure TMySimplePasPar.ConstructorHeading;
begin
  EnterHandler('ConstructorHeading');
  inherited;
  ExitHandler('ConstructorHeading');
end;

procedure TMySimplePasPar.ConstructorName;
begin
  EnterHandler('ConstructorName');
  inherited;
  ExitHandler('ConstructorName');
end;

procedure TMySimplePasPar.ConstSection;
begin
  EnterHandler('ConstSection');
  inherited;
  ExitHandler('ConstSection');
end;

procedure TMySimplePasPar.ContainsClause;
begin
  EnterHandler('ContainsClause');
  inherited;
  ExitHandler('ContainsClause');
end;

procedure TMySimplePasPar.ContainsExpression;
begin
  EnterHandler('ContainsExpression');
  inherited;
  ExitHandler('ContainsExpression');
end;

procedure TMySimplePasPar.ContainsIdentifier;
begin
  EnterHandler('ContainsIdentifier');
  inherited;
  ExitHandler('ContainsIdentifier');
end;

procedure TMySimplePasPar.ContainsStatement;
begin
  EnterHandler('ContainsStatement');
  inherited;
  ExitHandler('ContainsStatement');
end;

procedure TMySimplePasPar.DeclarationSection;
begin
  EnterHandler('DeclarationSection');
  inherited;
  ExitHandler('DeclarationSection');
end;

procedure TMySimplePasPar.Designator;
begin
  EnterHandler('Designator');
  inherited;
  ExitHandler('Designator');
end;

procedure TMySimplePasPar.DestructorHeading;
begin
  EnterHandler('DestructorHeading');
  inherited;
  ExitHandler('DestructorHeading');
end;

procedure TMySimplePasPar.DestructorName;
begin
  EnterHandler('DestructorName');
  inherited;
  ExitHandler('DestructorName');
end;

procedure TMySimplePasPar.Directive16Bit;
begin
  EnterHandler('Directive16Bit');
  inherited;
  ExitHandler('Directive16Bit');
end;

procedure TMySimplePasPar.DirectiveBinding;
begin
  EnterHandler('DirectiveBinding');
  inherited;
  ExitHandler('DirectiveBinding');
end;

procedure TMySimplePasPar.DirectiveCalling;
begin
  EnterHandler('DirectiveCalling');
  inherited;
  ExitHandler('DirectiveCalling');
end;

procedure TMySimplePasPar.DirectiveDeprecated;
begin
  EnterHandler('DirectiveDeprecated');
  inherited;
  ExitHandler('DirectiveDeprecated');
end;

procedure TMySimplePasPar.DirectiveLibrary;
begin
  EnterHandler('DirectiveLibrary');
  inherited;
  ExitHandler('DirectiveLibrary');
end;

procedure TMySimplePasPar.DirectiveLocal;
begin
  EnterHandler('DirectiveLocal');
  inherited;
  ExitHandler('DirectiveLocal');
end;

procedure TMySimplePasPar.DirectivePlatform;
begin
  EnterHandler('DirectivePlatform');
  inherited;
  ExitHandler('DirectivePlatform');
end;

procedure TMySimplePasPar.DirectiveVarargs;
begin
  EnterHandler('DirectiveVarargs');
  inherited;
  ExitHandler('DirectiveVarargs');
end;

procedure TMySimplePasPar.DispIDSpecifier;
begin
  EnterHandler('DispIDSpecifier');
  inherited;
  ExitHandler('DispIDSpecifier');
end;

procedure TMySimplePasPar.DispInterfaceForward;
begin
  EnterHandler('DispInterfaceForward');
  inherited;
  ExitHandler('DispInterfaceForward');
end;

procedure TMySimplePasPar.EmptyStatement;
begin
  EnterHandler('EmptyStatement');
  inherited;
  ExitHandler('EmptyStatement');
end;

procedure TMySimplePasPar.EnumeratedType;
begin
  EnterHandler('EnumeratedType');
  inherited;
  ExitHandler('EnumeratedType');
end;

procedure TMySimplePasPar.EnumeratedTypeItem;
begin
  EnterHandler('EnumeratedTypeItem');
  inherited;
  ExitHandler('EnumeratedTypeItem');
end;

procedure TMySimplePasPar.ExceptBlock;
begin
  EnterHandler('ExceptBlock');
  inherited;
  ExitHandler('ExceptBlock');
end;

procedure TMySimplePasPar.ExceptionBlockElseBranch;
begin
  EnterHandler('ExceptionBlockElseBranch');
  inherited;
  ExitHandler('ExceptionBlockElseBranch');
end;

procedure TMySimplePasPar.ExceptionClassTypeIdentifier;
begin
  EnterHandler('ExceptionClassTypeIdentifier');
  inherited;
  ExitHandler('ExceptionClassTypeIdentifier');
end;

procedure TMySimplePasPar.ExceptionHandler;
begin
  EnterHandler('ExceptionHandler');
  inherited;
  ExitHandler('ExceptionHandler');
end;

procedure TMySimplePasPar.ExceptionHandlerList;
begin
  EnterHandler('ExceptionHandlerList');
  inherited;
  ExitHandler('ExceptionHandlerList');
end;

procedure TMySimplePasPar.ExceptionIdentifier;
begin
  EnterHandler('ExceptionIdentifier');
  inherited;
  ExitHandler('ExceptionIdentifier');
end;

procedure TMySimplePasPar.ExceptionVariable;
begin
  EnterHandler('ExceptionVariable');
  inherited;
  ExitHandler('ExceptionVariable');
end;

procedure TMySimplePasPar.ExplicitType;
begin
  EnterHandler('ExplicitType');
  inherited;
  ExitHandler('ExplicitType');
end;

procedure TMySimplePasPar.ExportedHeading;
begin
  EnterHandler('ExportedHeading');
  inherited;
  ExitHandler('ExportedHeading');
end;

procedure TMySimplePasPar.ExportsClause;
begin
  EnterHandler('ExportsClause');
  inherited;
  ExitHandler('ExportsClause');
end;

procedure TMySimplePasPar.ExportsElement;
begin
  EnterHandler('ExportsElement');
  inherited;
  ExitHandler('ExportsElement');
end;

procedure TMySimplePasPar.Expression;
begin
  EnterHandler('Expression');
  inherited;
  ExitHandler('Expression');
end;

procedure TMySimplePasPar.ExpressionList;
begin
  EnterHandler('ExpressionList');
  inherited;
  ExitHandler('ExpressionList');
end;

procedure TMySimplePasPar.ExternalDirective;
begin
  EnterHandler('ExternalDirective');
  inherited;
  ExitHandler('ExternalDirective');
end;

procedure TMySimplePasPar.ExternalDirectiveThree;
begin
  EnterHandler('ExternalDirectiveThree');
  inherited;
  ExitHandler('ExternalDirectiveThree');
end;

procedure TMySimplePasPar.ExternalDirectiveTwo;
begin
  EnterHandler('ExternalDirectiveTwo');
  inherited;
  ExitHandler('ExternalDirectiveTwo');
end;

procedure TMySimplePasPar.Factor;
begin
  EnterHandler('Factor');
  inherited;
  ExitHandler('Factor');
end;

procedure TMySimplePasPar.FieldDeclaration;
begin
  EnterHandler('FieldDeclaration');
  inherited;
  ExitHandler('FieldDeclaration');
end;

procedure TMySimplePasPar.FieldList;
begin
  EnterHandler('FieldList');
  inherited;
  ExitHandler('FieldList');
end;

procedure TMySimplePasPar.FieldName;
begin
  EnterHandler('FieldName');
  inherited;
  ExitHandler('FieldName');
end;

procedure TMySimplePasPar.FieldNameList;
begin
  EnterHandler('FieldNameList');
  inherited;
  ExitHandler('FieldNameList');
end;

procedure TMySimplePasPar.FileType;
begin
  EnterHandler('FileType');
  inherited;
  ExitHandler('FileType');
end;

procedure TMySimplePasPar.FormalParameterList;
begin
  EnterHandler('FormalParameterList');
  inherited;
  ExitHandler('FormalParameterList');
end;

procedure TMySimplePasPar.FormalParameterSection;
begin
  EnterHandler('FormalParameterSection');
  inherited;
  ExitHandler('FormalParameterSection');
end;

procedure TMySimplePasPar.ForStatement;
begin
  EnterHandler('ForStatement');
  inherited;
  ExitHandler('ForStatement');
end;

procedure TMySimplePasPar.ForwardDeclaration;
begin
  EnterHandler('ForwardDeclaration');
  inherited;
  ExitHandler('ForwardDeclaration');
end;

procedure TMySimplePasPar.FunctionHeading;
begin
  EnterHandler('FunctionHeading');
  inherited;
  ExitHandler('FunctionHeading');
end;

procedure TMySimplePasPar.FunctionMethodDeclaration;
begin
  EnterHandler('FunctionMethodDeclaration');
  inherited;
  ExitHandler('FunctionMethodDeclaration');
end;

procedure TMySimplePasPar.FunctionMethodName;
begin
  EnterHandler('FunctionMethodName');
  inherited;
  ExitHandler('FunctionMethodName');
end;

procedure TMySimplePasPar.FunctionProcedureBlock;
begin
  EnterHandler('FunctionProcedureBlock');
  inherited;
  ExitHandler('FunctionProcedureBlock');
end;

procedure TMySimplePasPar.FunctionProcedureName;
begin
  EnterHandler('FunctionProcedureName');
  inherited;
  ExitHandler('FunctionProcedureName');
end;

procedure TMySimplePasPar.Identifier;
begin
  EnterHandler('Identifier');
  inherited;
  ExitHandler('Identifier');
end;

procedure TMySimplePasPar.IdentifierList;
begin
  EnterHandler('IdentifierList');
  inherited;
  ExitHandler('IdentifierList');
end;

procedure TMySimplePasPar.IfStatement;
begin
  EnterHandler('IfStatement');
  inherited;
  ExitHandler('IfStatement');
end;

procedure TMySimplePasPar.ImplementationSection;
begin
  EnterHandler('ImplementationSection');
  inherited;
  ExitHandler('ImplementationSection');
end;

procedure TMySimplePasPar.IncludeFile;
begin
  EnterHandler('IncludeFile');
  inherited;
  ExitHandler('IncludeFile');
end;

procedure TMySimplePasPar.IndexSpecifier;
begin
  EnterHandler('IndexSpecifier');
  inherited;
  ExitHandler('IndexSpecifier');
end;

procedure TMySimplePasPar.InheritedStatement;
begin
  EnterHandler('InheritedStatement');
  inherited;
  ExitHandler('InheritedStatement');
end;

procedure TMySimplePasPar.InitializationSection;
begin
  EnterHandler('InitializationSection');
  inherited;
  ExitHandler('InitializationSection');
end;

procedure TMySimplePasPar.InlineStatement;
begin
  EnterHandler('InlineStatement');
  inherited;
  ExitHandler('InlineStatement');
end;

procedure TMySimplePasPar.InParameter;
begin
  EnterHandler('InParameter');
  inherited;
  ExitHandler('InParameter');
end;

procedure TMySimplePasPar.InterfaceDeclaration;
begin
  EnterHandler('InterfaceDeclaration');
  inherited;
  ExitHandler('InterfaceDeclaration');
end;

procedure TMySimplePasPar.InterfaceForward;
begin
  EnterHandler('InterfaceForward');
  inherited;
  ExitHandler('InterfaceForward');
end;

procedure TMySimplePasPar.InterfaceGUID;
begin
  EnterHandler('InterfaceGUID');
  inherited;
  ExitHandler('InterfaceGUID');
end;

procedure TMySimplePasPar.InterfaceHeritage;
begin
  EnterHandler('InterfaceHeritage');
  inherited;
  ExitHandler('InterfaceHeritage');
end;

procedure TMySimplePasPar.InterfaceMemberList;
begin
  EnterHandler('InterfaceMemberList');
  inherited;
  ExitHandler('InterfaceMemberList');
end;

procedure TMySimplePasPar.InterfaceSection;
begin
  EnterHandler('InterfaceSection');
  inherited;
  ExitHandler('InterfaceSection');
end;

procedure TMySimplePasPar.InterfaceType;
begin
  EnterHandler('InterfaceType');
  inherited;
  ExitHandler('InterfaceType');
end;

procedure TMySimplePasPar.LabelDeclarationSection;
begin
  EnterHandler('LabelDeclarationSection');
  inherited;
  ExitHandler('LabelDeclarationSection');
end;

procedure TMySimplePasPar.LabeledStatement;
begin
  EnterHandler('LabeledStatement');
  inherited;
  ExitHandler('LabeledStatement');
end;

procedure TMySimplePasPar.LabelId;
begin
  EnterHandler('LabelId');
  inherited;
  ExitHandler('LabelId');
end;

procedure TMySimplePasPar.LibraryFile;
begin
  EnterHandler('LibraryFile');
  inherited;
  ExitHandler('LibraryFile');
end;

procedure TMySimplePasPar.MainUsedUnitExpression;
begin
  EnterHandler('MainUsedUnitExpression');
  inherited;
  ExitHandler('MainUsedUnitExpression');
end;

procedure TMySimplePasPar.MainUsedUnitName;
begin
  EnterHandler('MainUsedUnitName');
  inherited;
  ExitHandler('MainUsedUnitName');
end;

procedure TMySimplePasPar.MainUsedUnitStatement;
begin
  EnterHandler('MainUsedUnitStatement');
  inherited;
  ExitHandler('MainUsedUnitStatement');
end;

procedure TMySimplePasPar.MainUsesClause;
begin
  EnterHandler('MainUsesClause');
  inherited;
  ExitHandler('MainUsesClause');
end;

procedure TMySimplePasPar.MultiplicativeOperator;
begin
  EnterHandler('MultiplicativeOperator');
  inherited;
  ExitHandler('MultiplicativeOperator');
end;

procedure TMySimplePasPar.NewFormalParameterType;
begin
  EnterHandler('NewFormalParameterType');
  inherited;
  ExitHandler('NewFormalParameterType');
end;

procedure TMySimplePasPar.Number;
begin
  EnterHandler('Number');
  inherited;
  ExitHandler('Number');
end;

procedure TMySimplePasPar.ObjectConstructorHeading;
begin
  EnterHandler('ObjectConstructorHeading');
  inherited;
  ExitHandler('ObjectConstructorHeading');
end;

procedure TMySimplePasPar.ObjectDestructorHeading;
begin
  EnterHandler('ObjectDestructorHeading');
  inherited;
  ExitHandler('ObjectDestructorHeading');
end;

procedure TMySimplePasPar.ObjectField;
begin
  EnterHandler('ObjectField');
  inherited;
  ExitHandler('ObjectField');
end;

procedure TMySimplePasPar.ObjectForward;
begin
  EnterHandler('ObjectForward');
  inherited;
  ExitHandler('ObjectForward');
end;

procedure TMySimplePasPar.ObjectFunctionHeading;
begin
  EnterHandler('ObjectFunctionHeading');
  inherited;
  ExitHandler('ObjectFunctionHeading');
end;

procedure TMySimplePasPar.ObjectHeritage;
begin
  EnterHandler('ObjectHeritage');
  inherited;
  ExitHandler('ObjectHeritage');
end;

procedure TMySimplePasPar.ObjectMemberList;
begin
  EnterHandler('ObjectMemberList');
  inherited;
  ExitHandler('ObjectMemberList');
end;

procedure TMySimplePasPar.ObjectMethodDirective;
begin
  EnterHandler('ObjectMethodDirective');
  inherited;
  ExitHandler('ObjectMethodDirective');
end;

procedure TMySimplePasPar.ObjectMethodHeading;
begin
  EnterHandler('ObjectMethodHeading');
  inherited;
  ExitHandler('ObjectMethodHeading');
end;

procedure TMySimplePasPar.ObjectNameOfMethod;
begin
  EnterHandler('ObjectNameOfMethod');
  inherited;
  ExitHandler('ObjectNameOfMethod');
end;

procedure TMySimplePasPar.ObjectProcedureHeading;
begin
  EnterHandler('ObjectProcedureHeading');
  inherited;
  ExitHandler('ObjectProcedureHeading');
end;

procedure TMySimplePasPar.ObjectProperty;
begin
  EnterHandler('ObjectProperty');
  inherited;
  ExitHandler('ObjectProperty');
end;

procedure TMySimplePasPar.ObjectPropertySpecifiers;
begin
  EnterHandler('ObjectPropertySpecifiers');
  inherited;
  ExitHandler('ObjectPropertySpecifiers');
end;

procedure TMySimplePasPar.ObjectType;
begin
  EnterHandler('ObjectType');
  inherited;
  ExitHandler('ObjectType');
end;

procedure TMySimplePasPar.ObjectTypeEnd;
begin
  EnterHandler('ObjectTypeEnd');
  inherited;
  ExitHandler('ObjectTypeEnd');
end;

procedure TMySimplePasPar.ObjectVisibility;
begin
  EnterHandler('ObjectVisibility');
  inherited;
  ExitHandler('ObjectVisibility');
end;

procedure TMySimplePasPar.OldFormalParameterType;
begin
  EnterHandler('OldFormalParameterType');
  inherited;
  ExitHandler('OldFormalParameterType');
end;

procedure TMySimplePasPar.OrdinalIdentifier;
begin
  EnterHandler('OrdinalIdentifier');
  inherited;
  ExitHandler('OrdinalIdentifier');
end;

procedure TMySimplePasPar.OrdinalType;
begin
  EnterHandler('OrdinalType');
  inherited;
  ExitHandler('OrdinalType');
end;

procedure TMySimplePasPar.OutParameter;
begin
  EnterHandler('OutParameter');
  inherited;
  ExitHandler('OutParameter');
end;

procedure TMySimplePasPar.PackageFile;
begin
  EnterHandler('PackageFile');
  inherited;
  ExitHandler('PackageFile');
end;

procedure TMySimplePasPar.ParameterFormal;
begin
  EnterHandler('ParameterFormal');
  inherited;
  ExitHandler('ParameterFormal');
end;

procedure TMySimplePasPar.ParameterName;
begin
  EnterHandler('ParameterName');
  inherited;
  ExitHandler('ParameterName');
end;

procedure TMySimplePasPar.ParameterNameList;
begin
  EnterHandler('ParameterNameList');
  inherited;
  ExitHandler('ParameterNameList');
end;

procedure TMySimplePasPar.ParseFile;
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

procedure TMySimplePasPar.PointerType;
begin
  EnterHandler('PointerType');
  inherited;
  ExitHandler('PointerType');
end;

procedure TMySimplePasPar.ProceduralDirective;
begin
  EnterHandler('ProceduralDirective');
  inherited;
  ExitHandler('ProceduralDirective');
end;

procedure TMySimplePasPar.ProceduralType;
begin
  EnterHandler('ProceduralType');
  inherited;
  ExitHandler('ProceduralType');
end;

procedure TMySimplePasPar.ProcedureDeclarationSection;
begin
  EnterHandler('ProcedureDeclarationSection');
  inherited;
  ExitHandler('ProcedureDeclarationSection');
end;

procedure TMySimplePasPar.ProcedureHeading;
begin
  EnterHandler('ProcedureHeading');
  inherited;
  ExitHandler('ProcedureHeading');
end;

procedure TMySimplePasPar.ProcedureMethodDeclaration;
begin
  EnterHandler('ProcedureMethodDeclaration');
  inherited;
  ExitHandler('ProcedureMethodDeclaration');
end;

procedure TMySimplePasPar.ProcedureMethodName;
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

procedure TMySimplePasPar.ProgramBlock;
begin
  EnterHandler('ProgramBlock');
  inherited;
  ExitHandler('ProgramBlock');
end;

procedure TMySimplePasPar.ProgramFile;
begin
  EnterHandler('ProgramFile');
  inherited;
  ExitHandler('ProgramFile');
end;

procedure TMySimplePasPar.PropertyDefault;
begin
  EnterHandler('PropertyDefault');
  inherited;
  ExitHandler('PropertyDefault');
end;

procedure TMySimplePasPar.PropertyInterface;
begin
  EnterHandler('PropertyInterface');
  inherited;
  ExitHandler('PropertyInterface');
end;

procedure TMySimplePasPar.PropertyName;
begin
  EnterHandler('PropertyName');
  inherited;
  ExitHandler('PropertyName');
end;

procedure TMySimplePasPar.PropertyParameterConst;
begin
  EnterHandler('PropertyParameterConst');
  inherited;
  ExitHandler('PropertyParameterConst');
end;

procedure TMySimplePasPar.PropertyParameterList;
begin
  EnterHandler('PropertyParameterList');
  inherited;
  ExitHandler('PropertyParameterList');
end;

procedure TMySimplePasPar.PropertySpecifiers;
begin
  EnterHandler('PropertySpecifiers');
  inherited;
  ExitHandler('PropertySpecifiers');
end;

procedure TMySimplePasPar.QualifiedIdentifier;
begin
  EnterHandler('QualifiedIdentifier');
  inherited;
  ExitHandler('QualifiedIdentifier');
end;

procedure TMySimplePasPar.QualifiedIdentifierList;
begin
  EnterHandler('QualifiedIdentifierList');
  inherited;
  ExitHandler('QualifiedIdentifierList');
end;

procedure TMySimplePasPar.RaiseStatement;
begin
  EnterHandler('RaiseStatement');
  inherited;
  ExitHandler('RaiseStatement');
end;

procedure TMySimplePasPar.ReadAccessIdentifier;
begin
  EnterHandler('ReadAccessIdentifier');
  inherited;
  ExitHandler('ReadAccessIdentifier');
end;

procedure TMySimplePasPar.RealIdentifier;
begin
  EnterHandler('RealIdentifier');
  inherited;
  ExitHandler('RealIdentifier');
end;

procedure TMySimplePasPar.RealType;
begin
  EnterHandler('RealType');
  inherited;
  ExitHandler('RealType');
end;

procedure TMySimplePasPar.RecordConstant;
begin
  EnterHandler('RecordConstant');
  inherited;
  ExitHandler('RecordConstant');
end;

procedure TMySimplePasPar.RecordFieldConstant;
begin
  EnterHandler('RecordFieldConstant');
  inherited;
  ExitHandler('RecordFieldConstant');
end;

procedure TMySimplePasPar.RecordType;
begin
  EnterHandler('RecordType');
  inherited;
  ExitHandler('RecordType');
end;

procedure TMySimplePasPar.RecordVariant;
begin
  EnterHandler('RecordVariant');
  inherited;
  ExitHandler('RecordVariant');
end;

procedure TMySimplePasPar.RelativeOperator;
begin
  EnterHandler('RelativeOperator');
  inherited;
  ExitHandler('RelativeOperator');
end;

procedure TMySimplePasPar.RepeatStatement;
begin
  EnterHandler('RepeatStatement');
  inherited;
  ExitHandler('RepeatStatement');
end;

procedure TMySimplePasPar.RequiresClause;
begin
  EnterHandler('RequiresClause');
  inherited;
  ExitHandler('RequiresClause');
end;

procedure TMySimplePasPar.RequiresIdentifier;
begin
  EnterHandler('RequiresIdentifier');
  inherited;
  ExitHandler('RequiresIdentifier');
end;

procedure TMySimplePasPar.ResolutionInterfaceName;
begin
  EnterHandler('ResolutionInterfaceName');
  inherited;
  ExitHandler('ResolutionInterfaceName');
end;

procedure TMySimplePasPar.ResourceDeclaration;
begin
  EnterHandler('ResourceDeclaration');
  inherited;
  ExitHandler('ResourceDeclaration');
end;

procedure TMySimplePasPar.ReturnType;
begin
  EnterHandler('ReturnType');
  inherited;
  ExitHandler('ReturnType');
end;

procedure TMySimplePasPar.Run(aUnitName: string; SourceStream: TCustomMemoryStream);
begin
  FUnitName := aUnitName;
  inherited;
end;

procedure TMySimplePasPar.SetConstructor;
begin
  EnterHandler('SetConstructor');
  inherited;
  ExitHandler('SetConstructor');
end;

procedure TMySimplePasPar.SetElement;
begin
  EnterHandler('SetElement');
  inherited;
  ExitHandler('SetElement');
end;

procedure TMySimplePasPar.SetType;
begin
  EnterHandler('SetType');
  inherited;
  ExitHandler('SetType');
end;

procedure TMySimplePasPar.SimpleExpression;
begin
  EnterHandler('SimpleExpression');
  inherited;
  ExitHandler('SimpleExpression');
end;

procedure TMySimplePasPar.SimpleStatement;
begin
  EnterHandler('SimpleStatement');
  inherited;
  ExitHandler('SimpleStatement');
end;

procedure TMySimplePasPar.SimpleType;
begin
  EnterHandler('SimpleType');
  inherited;
  ExitHandler('SimpleType');
end;

procedure TMySimplePasPar.SkipAnsiComment;
begin
  EnterHandler('SkipAnsiComment');
  inherited;
  ExitHandler('SkipAnsiComment');
end;

procedure TMySimplePasPar.SkipBorComment;
begin
  EnterHandler('SkipBorComment');
  inherited;
  ExitHandler('SkipBorComment');
end;

procedure TMySimplePasPar.SkipCRLF;
begin
  EnterHandler('SkipCRLF');
  inherited;
  ExitHandler('SkipCRLF');
end;

procedure TMySimplePasPar.SkipCRLFco;
begin
  EnterHandler('SkipCRLFco');
  inherited;
  ExitHandler('SkipCRLFco');
end;

procedure TMySimplePasPar.SkipSlashesComment;
begin
  EnterHandler('SkipSlashesComment');
  inherited;
  ExitHandler('SkipSlashesComment');
end;

procedure TMySimplePasPar.SkipSpace;
begin
  EnterHandler('SkipSpace');
  inherited;
  ExitHandler('SkipSpace');
end;

procedure TMySimplePasPar.Statement;
begin
  EnterHandler('Statement');
  inherited;
  ExitHandler('Statement');
end;

procedure TMySimplePasPar.StatementList;
begin
  EnterHandler('StatementList');
  inherited;
  ExitHandler('StatementList');
end;

procedure TMySimplePasPar.StorageDefault;
begin
  EnterHandler('StorageDefault');
  inherited;
  ExitHandler('StorageDefault');
end;

procedure TMySimplePasPar.StorageExpression;
begin
  EnterHandler('StorageExpression');
  inherited;
  ExitHandler('StorageExpression');
end;

procedure TMySimplePasPar.StorageIdentifier;
begin
  EnterHandler('StorageIdentifier');
  inherited;
  ExitHandler('StorageIdentifier');
end;

procedure TMySimplePasPar.StorageNoDefault;
begin
  EnterHandler('StorageNoDefault');
  inherited;
  ExitHandler('StorageNoDefault');
end;

procedure TMySimplePasPar.StorageSpecifier;
begin
  EnterHandler('StorageSpecifier');
  inherited;
  ExitHandler('StorageSpecifier');
end;

procedure TMySimplePasPar.StorageStored;
begin
  EnterHandler('StorageStored');
  inherited;
  ExitHandler('StorageStored');
end;

procedure TMySimplePasPar.StringIdentifier;
begin
  EnterHandler('StringIdentifier');
  inherited;
  ExitHandler('StringIdentifier');
end;

procedure TMySimplePasPar.StringStatement;
begin
  EnterHandler('StringStatement');
  inherited;
  ExitHandler('StringStatement');
end;

procedure TMySimplePasPar.StringType;
begin
  EnterHandler('StringType');
  inherited;
  ExitHandler('StringType');
end;

procedure TMySimplePasPar.StructuredType;
begin
  EnterHandler('StructuredType');
  inherited;
  ExitHandler('StructuredType');
end;

procedure TMySimplePasPar.SubrangeType;
begin
  EnterHandler('SubrangeType');
  inherited;
  ExitHandler('SubrangeType');
end;

procedure TMySimplePasPar.TagField;
begin
  EnterHandler('TagField');
  inherited;
  ExitHandler('TagField');
end;

procedure TMySimplePasPar.TagFieldName;
begin
  EnterHandler('TagFieldName');
  inherited;
  ExitHandler('TagFieldName');
end;

procedure TMySimplePasPar.TagFieldTypeName;
begin
  EnterHandler('TagFieldTypeName');
  inherited;
  ExitHandler('TagFieldTypeName');
end;

procedure TMySimplePasPar.Term;
begin
  EnterHandler('Term');
  inherited;
  ExitHandler('Term');
end;

procedure TMySimplePasPar.TryStatement;
begin
  EnterHandler('TryStatement');
  inherited;
  ExitHandler('TryStatement');
end;

procedure TMySimplePasPar.TypeArgs;
begin
  EnterHandler('TypeArgs');
  inherited;
  ExitHandler('TypeArgs');
end;

procedure TMySimplePasPar.TypedConstant;
begin
  EnterHandler('TypedConstant');
  inherited;
  ExitHandler('TypedConstant');
end;

procedure TMySimplePasPar.TypeDeclaration;
begin
  EnterHandler('TypeDeclaration');
  FCurrentType := Lexer.Token;
  inherited;
  ExitHandler('TypeDeclaration');
end;

procedure TMySimplePasPar.TypeId;
begin
  EnterHandler('TypeId');
  inherited;
  ExitHandler('TypeId');
end;

procedure TMySimplePasPar.TypeKind;
begin
  EnterHandler('TypeKind');
  inherited;
  ExitHandler('TypeKind');
end;

procedure TMySimplePasPar.TypeName;
begin
  EnterHandler('TypeName');
  inherited;
  ExitHandler('TypeName');
end;

procedure TMySimplePasPar.TypeParamDecl;
begin
  EnterHandler('TypeParamDecl');
  inherited;
  ExitHandler('TypeParamDecl');
end;

procedure TMySimplePasPar.TypeParamDeclList;
begin
  EnterHandler('TypeParamDeclList');
  inherited;
  ExitHandler('TypeParamDeclList');
end;

procedure TMySimplePasPar.TypeParamList;
begin
  EnterHandler('TypeParamList');
  inherited;
  ExitHandler('TypeParamList');
end;

procedure TMySimplePasPar.TypeParams;
begin
  EnterHandler('TypeParams');
  inherited;
  ExitHandler('TypeParams');
end;

procedure TMySimplePasPar.TypeSection;
begin
  EnterHandler('TypeSection');
  inherited;
  ExitHandler('TypeSection');
end;

procedure TMySimplePasPar.UnitFile;
begin
  EnterHandler('UnitFile');
  inherited;
  ExitHandler('UnitFile');
end;

procedure TMySimplePasPar.UnitId;
begin
  EnterHandler('UnitId');
  inherited;
  ExitHandler('UnitId');
end;

procedure TMySimplePasPar.UnitName;
begin
  EnterHandler('UnitName');
  inherited;
  ExitHandler('UnitName');
end;

procedure TMySimplePasPar.UsedUnitName;
begin
  EnterHandler('UsedUnitName');
  inherited;
  ExitHandler('UsedUnitName');
end;

procedure TMySimplePasPar.UsedUnitsList;
begin
  EnterHandler('UsedUnitsList');
  inherited;
  ExitHandler('UsedUnitsList');
end;

procedure TMySimplePasPar.UsesClause;
begin
  EnterHandler('UsesClause');
  inherited;
  ExitHandler('UsesClause');
end;

procedure TMySimplePasPar.VarAbsolute;
begin
  EnterHandler('VarAbsolute');
  inherited;
  ExitHandler('VarAbsolute');
end;

procedure TMySimplePasPar.VarDeclaration;
begin
  EnterHandler('VarDeclaration');
  inherited;
  ExitHandler('VarDeclaration');
end;

procedure TMySimplePasPar.VarEqual;
begin
  EnterHandler('VarEqual');
  inherited;
  ExitHandler('VarEqual');
end;

procedure TMySimplePasPar.Variable;
begin
  EnterHandler('Variable');
  inherited;
  ExitHandler('Variable');
end;

procedure TMySimplePasPar.VariableList;
begin
  EnterHandler('VariableList');
  inherited;
  ExitHandler('VariableList');
end;

procedure TMySimplePasPar.VariableReference;
begin
  EnterHandler('VariableReference');
  inherited;
  ExitHandler('VariableReference');
end;

procedure TMySimplePasPar.VariableTwo;
begin
  EnterHandler('VariableTwo');
  inherited;
  ExitHandler('VariableTwo');
end;

procedure TMySimplePasPar.VariantIdentifier;
begin
  EnterHandler('VariantIdentifier');
  inherited;
  ExitHandler('VariantIdentifier');
end;

procedure TMySimplePasPar.VariantSection;
begin
  EnterHandler('VariantSection');
  inherited;
  ExitHandler('VariantSection');
end;

procedure TMySimplePasPar.VarName;
begin
  EnterHandler('VarName');
  inherited;
  ExitHandler('VarName');
end;

procedure TMySimplePasPar.VarNameList;
begin
  EnterHandler('VarNameList');
  inherited;
  ExitHandler('VarNameList');
end;

procedure TMySimplePasPar.VarParameter;
begin
  EnterHandler('VarParameter');
  inherited;
  ExitHandler('VarParameter');
end;

procedure TMySimplePasPar.VarSection;
begin
  EnterHandler('VarSection');
  inherited;
  ExitHandler('VarSection');
end;

procedure TMySimplePasPar.VisibilityAutomated;
begin
  EnterHandler('VisibilityAutomated');
  inherited;
  ExitHandler('VisibilityAutomated');
end;

procedure TMySimplePasPar.VisibilityPrivate;
begin
  EnterHandler('VisibilityPrivate');
  inherited;
  ExitHandler('VisibilityPrivate');
end;

procedure TMySimplePasPar.VisibilityProtected;
begin
  EnterHandler('VisibilityProtected');
  inherited;
  ExitHandler('VisibilityProtected');
end;

procedure TMySimplePasPar.VisibilityPublic;
begin
  EnterHandler('VisibilityPublic');
  inherited;
  ExitHandler('VisibilityPublic');
end;

procedure TMySimplePasPar.VisibilityPublished;
begin
  EnterHandler('VisibilityPublished');
  inherited;
  ExitHandler('VisibilityPublished');
end;

procedure TMySimplePasPar.VisibilityUnknown;
begin
  EnterHandler('VisibilityUnknown');
  inherited;
  ExitHandler('VisibilityUnknown');
end;

procedure TMySimplePasPar.WhileStatement;
begin
  EnterHandler('WhileStatement');
  inherited;
  ExitHandler('WhileStatement');
end;

procedure TMySimplePasPar.WithStatement;
begin
  EnterHandler('WithStatement');
  inherited;
  ExitHandler('WithStatement');
end;

procedure TMySimplePasPar.WriteAccessIdentifier;
begin
  EnterHandler('WriteAccessIdentifier');
  inherited;
  ExitHandler('WriteAccessIdentifier');
end;

procedure TMySimplePasPar.WriteOutput(const aLine: string);
begin
  Form2.mmoOutput.Lines.Add(FOutputIndent + aLine);
end;

procedure TMySimplePasPar.WriteOutput(const aLine: string; const Args: array of const);
begin
  Form2.mmoOutput.Lines.Add(FOutputIndent + Format(aLine, Args));
end;

procedure TMySimplePasPar.ReplaceOutput(const aSearch, aReplace: string);
begin
  Form2.mmoOutput.Lines.Text := StringReplace(Form2.mmoOutput.Lines.Text, aSearch, aReplace, [rfReplaceAll]);
end;

procedure TMySimplePasPar.EnterHandler(const aName: string);
begin
  Form2.mmoResult.Lines.Add(Format('%s%s -> %s at %d:%d',
                                   [FIndent, aName, Lexer.Token, Lexer.LineNumber, Lexer.LinePos]));
  FIndent := FIndent + '+ ';
end;

procedure TMySimplePasPar.ExitHandler(const aName: string);
begin
  FIndent := Copy(FIndent, 1, Length(FIndent)-2);
  Form2.mmoResult.Lines.Add(Format('%s%s <- %s at %d:%d',
                                   [FIndent, aName, Lexer.Token, Lexer.LineNumber, Lexer.LinePos]));
end;

procedure TForm2.Button1Click(Sender: TObject);
var
  parser: TMySimplePasPar;
  strm: TMemoryStream;
begin
  mmoResult.Clear;
  mmoOutput.Clear;

  parser := TMySimplePasPar.Create;
  strm   := TMemoryStream.Create;
  strm.Position := 0;
  mmoSource.Lines.SaveToStream(strm, TEncoding.Unicode);
  parser.Run('test', strm);
end;

end.

  Form2.mmoResult.Lines.Add(Format('WriteAccessIdentifier -> %s at %d:%d',[Lexer.Token, Lexer.LineNumber, Lexer.LinePos]));
  inherited;
  Form2.mmoResult.Lines.Add(Format('WriteAccessIdentifier <- %s at %d:%d',[Lexer.Token, Lexer.LineNumber, Lexer.LinePos]));

