unit Unit1;

//unit comment

interface

uses
  Windows, Forms, SysUtils;
  
//interface comment

type
  //type comment

  //class comment
  TTest = class(TObject)
  private
    FTestProp: TObject;
  protected
    procedure Test;
    function TestFunc(const aParam: Integer) string;
  public
    property TestProp: TObject read FTestProp;
  end;
  
implementation

uses
  DateUtils, Classes;
  
{ TTest }

procedure TTest.Test;
begin
  //
end;

end.
