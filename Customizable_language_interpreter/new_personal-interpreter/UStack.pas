unit UStack;

interface


uses types,SysUtils,UValues;



type
 TObjectType=(Object_Constant,Object_Variable);

type
 TStackObject=class
 private
  FConstantValue:TValue;
  FVarName:string;
 public
  ObjectType:TObjectType;
  property ConstantValue:TValue read FConstantValue;
  property VarName:string read FVarName;
  constructor createConstante(value:tvalue);
  constructor createVariable(VarName:string;Value:TValue);
 end;



type
 TStack=class
 private
  FCount:integer;
  FStack:array of TStackObject;
 public
  constructor create;
  destructor Destroy; override;
  property count:integer read FCount;
  function Pop:TStackObject;
  procedure Push(Obj:TStackObject);
  function getString:string;
 end;

implementation


constructor TStack.create;
begin
 Fcount:=0;
 setlength(FStack,0);
end;

destructor TStack.Destroy;
var
 i:integer;
begin
 for i:=0 to Fcount-1 do FStack[i].Free;
 inherited destroy;
end;

function TStack.Pop:TStackObject;
begin
 if Fcount=0 then begin result:=nil; exit; end;
 result:=FStack[Fcount-1];
 dec(FCount);
 setlength(FStack,FCount);
end;

procedure TStack.Push(Obj:TStackObject);
begin
 inc(FCount);
 setlength(FStack,FCount);
 FStack[Fcount-1]:=Obj;
end;


function TStack.getString:string;
var
 i:integer;
begin
 result:='';
 for i:=Fcount-1 downto 0 do
  begin
   case FStack[i].ObjectType of
   Object_Constant:result:=result+'(const)'+FStack[i].FConstantValue.StringValue;
   Object_Variable:result:=result+'(var  )'+FStack[i].FVarName+'='+FStack[i].FConstantValue.StringValue;
   end;
   result:=result+#13#10;
  end;
end;


// TStackObject TStackObject TStackObject TStackObject TStackObject TStackObject
//==============================================================================
constructor TStackObject.createConstante(value:tvalue);
begin
 create;
 ObjectType:=Object_Constant;
 FConstantValue:=Value;
end;


constructor TStackObject.createVariable(VarName:string;Value:TValue);
begin
 create;
 ObjectType:=Object_Variable;
 FConstantValue:=Value;
 FVarName:=VarName;
end;




end.
