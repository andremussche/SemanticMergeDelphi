namespace ConsoleApplication1;

interface

uses
  System.IO;

type
  ConsoleApp = class
  public
    method Test;
    class method Main(args: array of String);
  end;

implementation

class method ConsoleApp.Main(args: array of String);
begin
  // add your own code here
  Console.WriteLine('Hello World.');
end;

method ConsoleApp.Test;
begin

end;

end.
