unit SemanticYaml;

interface
{$IF NOT OXYGENE}
uses
  Generics.Collections;
{$ENDIF}
type
  TSemanticItemYaml = class;
  TSemanticParentYaml = class;

  TBaseYaml = class
  public
    function Generate(const aIndent: String): String; virtual; abstract;
  end;

  TBaseSemanticYaml = class(TBaseYaml)
  public
    type_: String;
    name: String;
    function Generate(const aIndent: String): String; override;
  end;

  TBaseSemanticYamlList = class(TList<TBaseSemanticYaml>)
  public
    function Generate(const aIndent: String): String;

    function AddNewItem: TSemanticItemYaml;
    function AddNewParent: TSemanticParentYaml;
  end;

  //[0, -1]
  TSemanticSpan = class(TBaseYaml)
  public
    a: Integer;
    b: Integer;
    function Generate(const aIndent: String): String; override;
  end;

  //locationSpan : {start: [1,0], end: [19,4]}
  TSemanticLocationSpan = class(TBaseYaml)
  public
    procedure  AfterConstruction; override;
    {$IF NOT OXYGENE}
    destructor Destroy; override;
    {$ENDIF}
  public
    start: TSemanticSpan;
    end_ : TSemanticSpan;
    function Generate(const aIndent: String): String; override;
  end;

  (* ---
     type : file
     name : /path/to/file
     locationSpan : {start: [1,0], end: [19,4]}
     footerSpan : [0, -1]
     parsingErrorsDetected : false
     children :
  *)
  TSemanticMasterYaml = class(TBaseSemanticYaml)
  public
    procedure  AfterConstruction; override;
    {$IF NOT OXYGENE}
    destructor Destroy; override;
    {$ENDIF}
  public
    locationSpan: TSemanticLocationSpan;
    footerSpan: TSemanticSpan;
    parsingErrorsDetected: Boolean;
    children: TBaseSemanticYamlList;
    function Generate(const aIndent: String): String; override;
  end;

  (*  - type : unit
        name : Unit1
        locationSpan : {start: [1,0], end: [1,13]}
        span : [0, 12]
  *)
  TSemanticItemYaml = class(TBaseSemanticYaml)
  public
    procedure  AfterConstruction; override;
    {$IF NOT OXYGENE}
    destructor Destroy; override;
    {$ENDIF}
  public
    locationSpan: TSemanticLocationSpan;
    span: TSemanticSpan;
    function Generate(const aIndent: String): String; override;
  end;

  (*   - type : interface
         name : interface
         locationSpan : {start: [2,0], end: [9,0]}
         headerSpan : [13, 25]
         footerSpan : [0, -1]
         children :
  *)
  TSemanticParentYaml = class(TBaseSemanticYaml)
  public
    procedure  AfterConstruction; override;
    {$IF NOT OXYGENE}
    destructor Destroy; override;
    {$ENDIF}
  public
    locationSpan: TSemanticLocationSpan;
    headerSpan: TSemanticSpan;
    footerSpan: TSemanticSpan;
    children: TBaseSemanticYamlList;
    function Generate(const aIndent: String): String; override;
  end;

implementation
{$IF NOT OXYGENE}
uses
  SysUtils;
{$ENDIF}
{ TSemanticLocationSpan }

procedure TSemanticLocationSpan.AfterConstruction;
begin
  inherited;
  start := TSemanticSpan.Create;
  end_  := TSemanticSpan.Create;
end;
{$IF NOT OXYGENE}
destructor TSemanticLocationSpan.Destroy;
begin
  start.Free;
  end_.Free;
  inherited;
end;
{$ENDIF}
function TSemanticLocationSpan.Generate(const aIndent: string): string;
begin
  //Result := inherited Generate(aIndent);
  //locationSpan : {start: [1,0], end: [19,4]}
  Result := '{' + 'start: ' + start.Generate('') + ', ' +
                  'end: '   + end_ .Generate('') + '}'#13#10;
end;

{ TSemanticItemYaml }

procedure TSemanticItemYaml.AfterConstruction;
begin
  inherited;
  locationSpan := TSemanticLocationSpan.Create;
  span         := TSemanticSpan.Create;
end;
{$IF NOT OXYGENE}
destructor TSemanticItemYaml.Destroy;
begin
  locationSpan.Free;
  span.Free;
  inherited;
end;
{$ENDIF}
function TSemanticItemYaml.Generate(const aIndent: string): string;
begin
  (*  - type : unit
        name : Unit1
        locationSpan : {start: [1,0], end: [1,13]}
        span : [0, 12]
  *)
  Result := inherited Generate(aIndent) +
            aIndent + 'locationSpan : ' + locationSpan.Generate(aIndent) +
            aIndent + 'span : ' + span.Generate('') + #13#10;
end;

{ TSemanticParentYaml }

procedure TSemanticParentYaml.AfterConstruction;
begin
  inherited;
  locationSpan:= TSemanticLocationSpan.Create;
  headerSpan  := TSemanticSpan.Create;
  footerSpan  := TSemanticSpan.Create;
  children    := TBaseSemanticYamlList.Create;
end;
{$IF NOT OXYGENE}
destructor TSemanticParentYaml.Destroy;
begin
  locationSpan.Free;
  headerSpan.Free;
  footerSpan.Free;
  children.Free;
  inherited;
end;
{$ENDIF}
function TSemanticParentYaml.Generate(const aIndent: string): string;
begin
  (*   - type : interface
         name : interface
         locationSpan : {start: [2,0], end: [9,0]}
         headerSpan : [13, 25]
         footerSpan : [0, -1]
         children :
  *)
  Result := inherited Generate(aIndent) +
            aIndent + 'locationSpan : ' + locationSpan.Generate(aIndent) +
            aIndent + 'headerSpan : ' + headerSpan.Generate('') + #13#10 +
            aIndent + 'footerSpan : ' + footerSpan.Generate('') + #13#10;
  if children.Count > 0 then
    Result := Result +
              aIndent + 'children : '#13#10 + children.Generate(aIndent + '    ');
end;

{ TBaseSemanticYaml }

function TBaseSemanticYaml.Generate(const aIndent: string): string;
begin
  if aIndent = '' then
  begin
    Result := 'type : '+ type_ + #13#10 +
              'name : '+ name  + #13#10;
  end
  else
  begin
    Result := Copy(aIndent, 1, Length(aIndent)-2) +
              '- '    + 'type : '+ type_ + #13#10;   //    - type: xyz
    Result := Result +
              aIndent + 'name : '+ name  + #13#10;   //      name: xyz
  end;
end;

{ TSemanticSpan }

function TSemanticSpan.Generate(const aIndent: string): string;
begin
  //[0, -1]
  Result := aIndent + Format('[%d, %d]', [a, b]);
end;

{ TSemanticMasterYaml }

procedure TSemanticMasterYaml.AfterConstruction;
begin
  inherited;
  locationSpan := TSemanticLocationSpan.Create;
  footerSpan   := TSemanticSpan.Create;
  children     := TBaseSemanticYamlList.Create;
end;
{$IF NOT OXYGENE}
destructor TSemanticMasterYaml.Destroy;
begin
  locationSpan.Free;
  footerSpan.Free;
  children.Free;
  inherited;
end;
{$ENDIF}
function TSemanticMasterYaml.Generate(const aIndent: string): string;
begin
  (* ---
     type : file
     name : /path/to/file
     locationSpan : {start: [1,0], end: [19,4]}
     footerSpan : [0, -1]
     parsingErrorsDetected : false
     children :
  *)
  Result := '---'#13#10 +
            inherited Generate('') +
            aIndent + 'locationSpan : ' + locationSpan.Generate(aIndent) +
            aIndent + 'footerSpan : ' + footerSpan.Generate('') + #13#10 +
            aIndent + 'parsingErrorsDetected : ' + LowerCase(BoolToStr(parsingErrorsDetected, True)) + #13#10;
  if children.Count > 0 then
    Result := Result +
              aIndent + 'children : '#13#10 + children.Generate('    ');
end;

{ TBaseSemanticYamlList }

function TBaseSemanticYamlList.AddNewItem: TSemanticItemYaml;
begin
  Result := TSemanticItemYaml.Create;
  Self.Add(Result);
end;

function TBaseSemanticYamlList.AddNewParent: TSemanticParentYaml;
begin
  Result := TSemanticParentYaml.Create;
  Self.Add(Result);
end;

function TBaseSemanticYamlList.Generate(const aIndent: string): string;
var item: TBaseSemanticYaml;
begin
  if Self.Count = 0 then Exit('');

  for item in Self do
    Result := Result +
              #13#10 +  //empty line between
              item.Generate(aIndent);
end;

end.
