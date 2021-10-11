unit UFunctions;

interface


uses types,SysUtils,UValues;

 
type
 TFunctionsList=record
                       FSubIndex:integer;
                       FPosIndex:integer;
 end;

 TFunctionsStack=class
 private
  FCount:integer;
  FStack:array of TFunctionsList;
  function GetFunctionList:TFunctionsList;
  procedure SetFunctionList(fl:TFunctionsList);
 public
  constructor create;
  property FunctionList:TFunctionsList read GetFunctionList write SetFunctionList;
  property count:integer read FCount;
  procedure UpStack;
  procedure DownStack;
  function GetString:string;
 end;


implementation


// TFunctionsStack TFunctionsStack TFunctionsStack TFunctionsStack
//=================================================================

constructor TFunctionsStack.create;
begin
 Fcount:=0;
 setlength(FStack,0);
end;


procedure TFunctionsStack.UpStack;
begin
 inc(Fcount);
 setlength(FStack,Fcount);
 FStack[FCount-1].FSubIndex:=-1;
 FStack[FCount-1].FPosIndex:=-1;
end;

procedure TFunctionsStack.DownStack;
begin
 dec(Fcount);
 setlength(FStack,Fcount);
end;

function TFunctionsStack.GetFunctionList:TFunctionsList;
begin
 if Fcount>0 then result:=FStack[Fcount-1];
end;

procedure TFunctionsStack.SetFunctionList(fl:TFunctionsList);
begin
 if Fcount<=0 then exit;
 FStack[Fcount-1]:=fl;
end;

function TFunctionsStack.GetString:string;
var
 i:integer;
begin
 result:='';
 if fcount=0 then exit;
 for i:=0 to Fcount-1 do
  result:=result+inttohex(i,4)+':'+inttohex(FStack[i].FSubIndex,4)+','
                                  +inttohex(FStack[i].FPosIndex,4)+#13#10;

end;

end.
