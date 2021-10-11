unit UFunctions;

interface


uses types,SysUtils,UValues;

 
type
 TFunctionsList=class
 private
  FCount:integer;
  FNames:array of string;
  FFunctions:array of TObject;
  function GetNames(index:integer):string;
 public
  constructor create;
  destructor free;
  property count:integer read FCount;
  property Names[index:integer]:string read GetNames;
  function GetFunction(name:string):TObject;
  procedure Define(name:string;FunctionNode:TObject);
  function Add(name:string;FunctionNode:TObject):TObject;
  function IndexNameOf(name:string):integer;
 end;

 TFunctionsStack=class
 private
  FCount:integer;
  FStack:array of TFunctionsList;
 public
  constructor create;
  property count:integer read FCount;
  function GetFunction(name:string):TObject;
  procedure Define(name:string;FunctionNode:TObject);
  procedure UpStack;
  procedure DownStack;
 end;


implementation


// TFunctionsList TFunctionsList TFunctionsList TFunctionsList TFunctionsList
//============================================================================
constructor TFunctionsList.create;
begin
 Fcount:=0;
 setlength(Fnames,0);
 setlength(FFunctions,0);
end;

destructor TFunctionsList.free;
begin
 //
end;

function TFunctionsList.GetFunction(name:string):TObject;
begin
 result:=FFunctions[IndexNameOf(name)];
end;

procedure TFunctionsList.Define(name:string;FunctionNode:TObject);
begin
 add(name,FunctionNode);
end;

function TFunctionsList.Add(name:string;FunctionNode:TObject):TObject;
begin
 inc(Fcount);
 setlength(Fnames,fcount);
 setlength(FFunctions,fcount);
 fnames[fcount-1]:=name;
 FFunctions[fcount-1]:=FunctionNode;
 result:=FFunctions[fcount-1];
end;

function TFunctionsList.GetNames(index:integer):string;
begin
 if (index<0) or (index>=Fcount) then result:=''
                                 else result:=Fnames[index];
end;

function TFunctionsList.IndexNameOf(name:string):integer;
var
 i:integer;
begin
 result:=-1;
 for i:=0 to Fcount-1 do
  if Fnames[i]=name then result:=i;
end;

// TVariablesStack TVariablesStack TVariablesStack TVariablesStack
//=================================================================

constructor TFunctionsStack.create;
begin
 Fcount:=0;
 setlength(FStack,0);
end;

function TFunctionsStack.GetFunction(name:string):TObject;
var
 j,s:integer;
begin
 s:=-1;
 result:=nil;
 for j:=0 to Fcount-1 do
  if FStack[j].IndexNameOf(name)<>-1 then s:=j;

 if s<>-1 then result:=FStack[s].GetFunction(name);
end;

procedure TFunctionsStack.Define(name:string;FunctionNode:TObject);
begin
 if FStack[FCount-1].IndexNameOf(name)<>-1 then
  Raise Exception.CreateFmt('''%s'' déjà définie', [name]) ;

 FStack[FCount-1].define(name,FunctionNode);
end;


procedure TFunctionsStack.UpStack;
begin
 inc(Fcount);
 setlength(FStack,Fcount);
 FStack[FCount-1]:=TFunctionsList.create;
end;

procedure TFunctionsStack.DownStack;
begin
 FStack[Fcount-1].Free;
 dec(Fcount);
 setlength(FStack,Fcount);
end;

end.
