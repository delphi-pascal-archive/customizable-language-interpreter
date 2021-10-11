unit UEnvironnement;

interface

uses UVariables,UFunctions,UValues,UStack;


type
  TFunctionCallEvent=function(sender:tobject;FunctionName:string;index:integer;ArgsCount:integer):TValue of object;

type
 TEnvironnement=class
 private
  FVariables:TVariablesStack;
  FGlobals:TVariablesStack;
  FFunctions:TFunctionsStack;
  FStack:TStack;
  FResult:TValue;
  FCount:integer;
  FFunctionCallList:array of string;
  FFunctionCallRef:array of array of boolean;
  FOnFunctionCall:TFunctionCallEvent;
  function GetFunctionCallRef(Funct,Arg:integer):boolean;
 public
  constructor create;
  destructor free;
  property Variables:TVariablesStack read FVariables;
  property Globals:TVariablesStack read FGlobals;
  property Stack:TStack read FStack;
  property Functions:TFunctionsStack read FFunctions;
  property Result:TValue read FResult write FResult;

  property FunctionCallRef[Funct,Arg:integer]:boolean read GetFunctionCallRef;
  function GetFunctionArgCount(Funct:integer):integer;
  function IndexofFunctionCall(name:string):integer;
  procedure AddFunctionCall(name:string;const RefList: array of Boolean);
  property OnFunctionCall:TFunctionCallEvent read FOnFunctionCall write FOnFunctionCall;
 end;


implementation

constructor TEnvironnement.create;
begin
 FVariables:=TVariablesStack.create;
 FFunctions:=TFunctionsStack.create;
 FGlobals:=TVariablesStack.create;
 FStack:=TStack.create;
 FResult:=tvalue.create;
 FOnFunctionCall:=nil;
 FCount:=0;
 setlength(FFunctionCallList,0);
end;

destructor TEnvironnement.free;
begin
 FVariables.free;
 FFunctions.free;
 FGlobals.free;
 FStack.free;
 FResult.free;
end;

procedure TEnvironnement.AddFunctionCall(name:string;const RefList: array of Boolean);
var
 i:integer;
begin
 inc(FCount);
 setlength(FFunctionCallList,FCount);
 setlength(FFunctionCallRef,FCount);
 FFunctionCallList[FCount-1]:=name;
 setlength(FFunctionCallRef[FCount-1],length(RefList));
 for i:=0 to high(RefList) do FFunctionCallRef[FCount-1][i]:=RefList[i];
end;

function TEnvironnement.IndexofFunctionCall(name:string):integer;
var
 i:integer;
begin
 result:=-1;
 for i:=0 to Fcount-1 do if name=FFunctionCallList[i] then begin result:=i;break; end;
end;


function TEnvironnement.GetFunctionCallRef(Funct,Arg:integer):boolean;
begin
 result:=false;
 if (Funct<0) or (Funct>=Fcount) then exit;
 if (Arg<0) or (Arg>=length(FFunctionCallRef[Funct])) then exit;
 result:=FFunctionCallRef[Funct][Arg];
end;

function TEnvironnement.GetFunctionArgCount(Funct:integer):integer;
begin
 result:=0;
 if (Funct<0) or (Funct>=Fcount) then exit;
 result:=length(FFunctionCallRef[Funct]);
end;

end.

