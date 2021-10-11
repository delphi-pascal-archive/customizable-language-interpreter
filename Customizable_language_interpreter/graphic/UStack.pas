unit UStack;

interface


uses types,SysUtils,UValues;


type
 TStack=class
 private
  FCount:integer;
  FValues:array of TValue;
 public
  constructor create;
  destructor free;
  property count:integer read FCount;
  function Pop:TValue;
  procedure Push(value:TValue);
  function read(index:integer):TValue;
  procedure write(index:integer;value:TValue);
 end;

implementation


constructor TStack.create;
begin
 Fcount:=0;
 setlength(FValues,0);
end;

destructor TStack.free;
var
 i:integer;
begin
 for i:=0 to Fcount-1 do FValues[i].Free;
end;

function TStack.Pop:TValue;
begin
 result:=tvalue.create(FValues[Fcount-1]);
 FValues[Fcount-1].free;
 dec(FCount);
 setlength(FValues,FCount);
end;

procedure TStack.Push(value:TValue);
begin
 inc(FCount);
 setlength(FValues,FCount);
 FValues[Fcount-1]:=value;
end;

function TStack.read(index:integer):TValue;
begin
 result:=FValues[Fcount-index-1];
end;

procedure TStack.write(index:integer;value:TValue);
begin
 FValues[Fcount-index-1].Assign(value);
end;

end.
