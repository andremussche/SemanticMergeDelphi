namespace KeywordHashCalculator;

interface

uses
  System.Collections.Generic,
  System.Linq,
  System.Windows,
  System.Windows.Controls,
  System.Windows.Data,
  System.Windows.Documents,
  System.Windows.Media,
  System.Windows.Navigation,
  System.Windows.Shapes;

type
  Window1 = public partial class(System.Windows.Window)
  private
    method Button_Click(sender: Object; e: System.Windows.RoutedEventArgs);
  public
    constructor;
  end;
  
implementation

constructor Window1;
begin
  InitializeComponent();
end;

method Window1.Button_Click(sender: Object; e: System.Windows.RoutedEventArgs);
begin
  var hash := 0;
  for each c in tbIndentifier.Text do
  begin
    var u := Char.ToUpper(c);
    hash := hash + (ord(u) - 64);
  end;
  self.Title := 'Hash value = ' + hash.ToString();
end;
  
end.
