namespace System;

interface

Uses
  System.Collections.Generic;

method &Copy(aString: String; aStart, aLength: Int32): String;

method &Copy(aString: PChar; aStart, aLength: Int32): String;

method Pos(aSubString: String; aString: String): Int32;

method SetString(var TargetString: String; BufferPointer: PChar; Length: Integer);

method UpperCase(aString: String): String;

method LowerCase(aString: String): String;

method CharInSet(C: Char; CharSet: array of Char): Boolean;

method BoolToStr(aBoolean, UseBoolStrs: Boolean): String;

const MaxInt: Integer = Integer.MaxValue;

type
    TObject = Object;

    PChar = ^Char;

    PString = ^String;

    ByteBool = Boolean;

    TList<T> = class(List<T>);

    TStrings = class(List<String>)
    private
    public
      method Delete(aElement: String);
      method Delete(aElementIndex: Integer);
    end;

    TStringList = class(TStrings);

    TCustomMemoryStream = class(System.IO.MemoryStream)
    private
      method get_Size: Int32;
    public
      property Size: Int32 read get_Size;
    end;

    TStack<T> = class(Stack<T>);

implementation

method Pos(aSubString: String; aString: String): Int32;
begin
  result := aString:IndexOf(aSubString) + 1;
end;

method &Copy(aString: String; aStart: Int32; aLength: Int32): String;
begin
  if not assigned(aString) then exit ''; 
  // Delphi's copy() handels lengths that exceed the string, and returns what's there. 
  // .NET and Sugar's SubString would throw an exception, so we need to account for that.
  var l := aString.Length;
  if (aStart-1)+aLength > l then aLength := l-(aStart+1); 
  result := aString.Substring(aStart-1, aLength);
end;

method SetString(var TargetString: String; BufferPointer: PChar; Length: Integer);
begin
  var sb := new System.Text.StringBuilder(Length);
  for i: Int32 := 1 to Length do
  begin
    sb.Append(BufferPointer^);
    inc(BufferPointer);
  end;
  TargetString := sb.ToString();
end;

method UpperCase(aString: String): String;
begin
  exit aString.ToUpper();
end;

method CharInSet(C: Char; CharSet: array of Char): Boolean;
begin
  for each ch in CharSet do
  begin
    if ch = C then exit true;
  end;
end;

method &Copy(aString: PChar; aStart: Int32; aLength: Int32): String;
begin
  var ptr: PString;
  ptr := PString(aString);
  result := &Copy(ptr^, aStart, aLength);
end;

method LowerCase(aString: String): String;
begin
  exit aString.ToLower();
end;

method BoolToStr(aBoolean, UseBoolStrs: Boolean): String;
begin
  exit aBoolean.ToString();
end;

method TStrings.Delete(aElement: String);
begin
  self.Remove(aElement);
end;

method TStrings.Delete(aElementIndex: Integer);
begin
  self.RemoveAt(aElementIndex);
end;

method TCustomMemoryStream.get_Size: Int32;
begin
  exit self.Length;
end;

end.
