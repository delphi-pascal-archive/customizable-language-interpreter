unit UVariables;

interface


uses types,SysUtils,UValues;


type
 TVariablesList=class
 private
  FCount:integer;
  FNames:array of string;
  FValues:array of TValue;
  function GetNames(index:integer):string;
 public
  constructor create;
  destructor free;
  property count:integer read FCount;
  property Names[index:integer]:string read GetNames;
  function GetValue(name:string):tvalue;
  function Define(name:string;value:tvalue):tvalue;
  function DefineNew(name:string):tvalue;
  function IndexNameOf(name:string):integer;
  function Add(name:string;value:tvalue):TValue;
 end;

 TVariablesStack=class
 private
  FCount:integer;
  FStack:array of TVariablesList;
 public
  constructor create;
  destructor free;
  property count:integer read FCount;
  function GetValue(name:string):tvalue;
  function Define(name:string;value:tvalue):tvalue;
  function DefineNew(name:string):tvalue;
  function IndexNameOf(name:string):integer;
  procedure UpStack;
  procedure DownStack;
 end;

implementation

constructor TVariablesList.create;
begin
 Fcount:=0;
 setlength(Fnames,0);
 setlength(FValues,0);
end;

destructor TVariablesList.free;
var
 i:integer;
begin
 for i:=0 to Fcount-1 do FValues[i].Free;
end;

function TVariablesList.GetValue(name:string):tvalue;
var
 i,n:integer;
begin
 n:=IndexNameOf(name);
 if n=-1 then result:=nil
         else result:=FValues[n];
end;

function TVariablesList.Define(name:string;value:tvalue):tvalue;
var
 i,n:integer;
begin
 n:=IndexNameOf(name);
 if n=-1 then result:=add(name,value)
         else result:=FValues[n].assign(value);
end;

function TVariablesList.DefineNew(name:string):tvalue;
begin
 inc(Fcount);
 setlength(Fnames,fcount);
 setlength(Fvalues,fcount);
 fnames[fcount-1]:=name;
 fvalues[fcount-1]:=TValue.create();
 result:=fvalues[fcount-1];
end;

function TVariablesList.Add(name:string;value:tvalue):TValue;
begin
 inc(Fcount);
 setlength(Fnames,fcount);
 setlength(Fvalues,fcount);
 fnames[fcount-1]:=name;
 fvalues[fcount-1]:=TValue.create(Value);
 result:=fvalues[fcount-1];
end;

function TVariablesList.GetNames(index:integer):string;
begin
 if (index<0) or (index>=Fcount) then result:=''
                                 else result:=Fnames[index];
end;

function TVariablesList.IndexNameOf(name:string):integer;
var
 i:integer;
begin
 result:=-1;
 for i:=0 to Fcount-1 do
  if Fnames[i]=name then result:=i;
end;

// TVariablesStack TVariablesStack TVariablesStack TVariablesStack
//=================================================================

constructor TVariablesStack.create;
begin
 Fcount:=0;
 setlength(FStack,0);
end;

destructor TVariablesStack.free;
var
 i:integer;
begin
 for i:=0 to fcount-1 do FStack[i].free;
 Fcount:=0;
 setlength(FStack,0);
end;

function TVariablesStack.GetValue(name:string):tvalue;
begin
 result:=FStack[FCount-1].GetValue(name);
end;

function TVariablesStack.Define(name:string;value:tvalue):tvalue;
begin
 result:=FStack[FCount-1].define(name,value);
end;

function TVariablesStack.DefineNew(name:string):tvalue;
begin
 result:=FStack[FCount-1].defineNew(name);
end;

function TVariablesStack.IndexNameOf(name:string):integer;
begin
 result:=FStack[FCount-1].IndexNameOf(name);
end;

procedure TVariablesStack.UpStack;
begin
 inc(Fcount);
 setlength(FStack,Fcount);
 FStack[FCount-1]:=TVariablesList.create;
end;

procedure TVariablesStack.DownStack;
begin
 FStack[Fcount-1].Free;
 dec(Fcount);
 setlength(FStack,Fcount);
end;

end.
