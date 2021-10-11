unit UListing;

interface

uses types,SysUtils,UEnvironnement,UValues,UStack,Math,UFunctions;

type
 TOperator=(LOP_PLUS, LOP_MOINS, LOP_MULTIPLIE, LOP_DIVISE, LOP_MODULO, LOP_PUISSANCE,
 LOP_CONCATENATION, LOP_NON, LOP_ET, LOP_OU, LOP_NEGATIF,
 LOP_INFERIEUR, LOP_INFERIEUREGAL, LOP_EGAL, LOP_SUPERIEUREGAL, LOP_SUPERIEUR, LOP_DIFFERENT);


Type
 TLabelList=record Name:string; Location:integer; end;

Type
 TBaseListing=class
 end;

 TAsmObject=class
 private
 public
  procedure Eval(Env:TEnvironnement;Listing:TBaseListing);virtual; abstract;
  function GetString:string;virtual; abstract;
  function GetValueOf(obj:TStackObject):tvalue;
 end;



Type
 TSubListing=class(TBaseListing)
 private
  FName:string;
  FIndex:integer;
  FCount:integer;
  FList:array of TAsmObject;
  FLabelCount:integer;
  FLabelList:array of TLabelList;
  FLabelBreakCount:integer;
  FLabelBreakList:array of String;
  FLabelContinueList:array of String;
  FResult:TValue;
  FCountRef:integer;
  FListRef:array of boolean;

  FParent:TBaseListing;
  FParentIndex:integer;
  procedure SetCountRef(size:integer);
  function GetListref(index:integer):boolean;
  procedure SetListRef(index:integer;IsRef:boolean);
  function GetBreakLabel:String;
  function GetContinueLabel:String;

 public
  constructor create(Owner:TBaseListing;ParentIndex:integer);
  destructor Destroy; override;

  property Count:integer read FCount;
  property CountRef:integer read FCountRef write SetCountRef;
  property ListRef[index:integer]:boolean read GetListref write SetListRef;

  property LabelBreak:String read GetBreakLabel;
  property LabelContinue:String read GetContinueLabel;

  property Name:string read FName write FName;
  property Index:integer read FIndex write FIndex;
  property ParentIndex:integer read FParentIndex;

  procedure SetLabelBreak(cont,brk:string);
  procedure DetLabelBreak;

  procedure add(obj:TAsmObject);
  function addLabel(name:string;Location:integer):integer;
  function GetLabelLocation(name:string):integer;
  function GetString:string;
 end;


Type
 TListing=class(TBaseListing)
 private
  FCount:integer;
  FList:array of TSubListing;
  FCurrentListingIndex:integer;
  FIndexEval:integer;
  function GetCurrentListing:TSubListing;
  function GetSub(index:integer):TSubListing;
  function GetEvalPos:integer;
 public
  property Count:integer read FCount;
  property CurrentListingIndex:integer read FCurrentListingIndex write FCurrentListingIndex;
  property CurrentListing:TSubListing read GetCurrentListing;
  property SubListing[index:integer]:TSubListing read GetSub;
  property EvalPos:integer read GetEvalPos;
  constructor create;
  destructor Destroy; override;
  function AddListing(ChildOf:integer):integer;
  function Eval(n:integer;Env:TEnvironnement):integer;
  function GetString:string;
  function GetIndexOfSub(from:integer;named:string):integer;
 end;


 TCallSubObject=class(TAsmObject)
 private
 public
  FunctionName:string;
  constructor create(name:string);
  function GetString:string;override;
  procedure Eval(Env:TEnvironnement;Listing:TBaseListing);override;
 end;

 TPushVariableObject=class(TAsmObject)
 private
  FValue:TValue;
 public
  VariableName:string;
  constructor create(name:string);
  function GetString:string;override;
  procedure Eval(Env:TEnvironnement;Listing:TBaseListing); override;
 end;

 TReturnObject=class(TAsmObject)
 private
 public
  constructor create;
  function GetString:string; override;
  procedure Eval(Env:TEnvironnement;Listing:TBaseListing); override;
 end;

 TResultObject=class(TAsmObject)
 private
 public
  constructor create;
  function GetString:string; override;
  procedure Eval(Env:TEnvironnement;Listing:TBaseListing); override;
 end;

 TPushValueObject=class(TAsmObject)
 private
  FValue:TValue;
 public
  constructor create(Value:TValue);
  function GetString:string;override;
  procedure Eval(Env:TEnvironnement;Listing:TBaseListing);override;
 end;

 TDropObject=class(TAsmObject)
 private
 public
  constructor create;
  function GetString:string;override;
  procedure Eval(Env:TEnvironnement;Listing:TBaseListing);override;
 end;

 TEvalObject=class(TAsmObject)
 private
 public
  constructor create;
  function GetString:string;override;
  procedure Eval(Env:TEnvironnement;Listing:TBaseListing);override;
 end;

 TJumpObject=class(TAsmObject)
 private
 public
  LabelName:string;
  constructor create(Name:string);
  function GetString:string; override;
  procedure Eval(Env:TEnvironnement;Listing:TBaseListing); override;
 end;

 TBoolJumpObject=class(TAsmObject)
 private
 public
  LabelName:string;
  constructor create(Name:string);
  function GetString:string; override;
  procedure Eval(Env:TEnvironnement;Listing:TBaseListing); override;
 end;

 TOperatorObject=class(TAsmObject)
 private
 public
  OpName:string;
  constructor create(Name:string);
  function GetString:string; override;
  procedure Eval(Env:TEnvironnement;Listing:TBaseListing);override;
 end;

 TArrayObject=class(TAsmObject)
 private
 public
  constructor create;
  function GetString:string;override;
  procedure Eval(Env:TEnvironnement;Listing:TBaseListing);override;
 end;

 TExtractObject=class(TAsmObject)
 private
 public
  constructor create;
  function GetString:string; override;
  procedure Eval(Env:TEnvironnement;Listing:TBaseListing); override;
 end;

 TStoObject=class(TAsmObject)
 private
 public
  constructor create;
  function GetString:string;override;
  procedure Eval(Env:TEnvironnement;Listing:TBaseListing);override;
 end;

 TGStoObject=class(TAsmObject)
 private
 public
  constructor create;
  function GetString:string;override;
  procedure Eval(Env:TEnvironnement;Listing:TBaseListing);override;
 end;
 

implementation

// TSubListing TSubListing TSubListing TSubListing TSubListing TSubListing
//=========================================================================


procedure TSubListing.SetCountRef(size:integer);
begin
 FCountRef:=size;
 Setlength(FListRef,size);
end;

function TSubListing.GetListref(index:integer):boolean;
begin
 if (index<0) or (index>=FcountRef) then result:=false
                                    else result:=FListRef[index];
end;

procedure TSubListing.SetListRef(index:integer;IsRef:boolean);
begin
 if (index<0) or (index>=FcountRef) then exit;
 FListRef[index]:=IsRef;
end;

function TSubListing.GetBreakLabel:String;
begin
 if FLabelBreakCount<=0 then result:=''
 else result:=FLabelBreakList[FLabelBreakCount-1];
end;

function TSubListing.GetContinueLabel:String;
begin
 if FLabelBreakCount<=0 then result:=''
 else result:=FLabelContinueList[FLabelBreakCount-1];
end;

procedure TSubListing.SetLabelBreak(cont,brk:string);
begin
 inc(FLabelBreakCount);
 setlength(FLabelContinueList, FLabelBreakCount);
 setlength(FLabelBreakList, FLabelBreakCount);
 FLabelContinueList[FLabelBreakCount-1]:=cont;
 FLabelBreakList[FLabelBreakCount-1]:=brk;
end;

procedure TSubListing.DetLabelBreak;
begin
 dec(FLabelBreakCount);
 setlength(FLabelContinueList, FLabelBreakCount);
 setlength(FLabelBreakList, FLabelBreakCount);
end;

constructor TSubListing.create(Owner:TBaseListing;ParentIndex:integer);
begin
 Inherited create;
 FParent:=Owner;
 FResult:=TValue.create;
 Fcount:=0;
 Setlength(FList,0);

 FLabelCount:=0;
 setlength(FLabelList,0);
 FParentIndex:=ParentIndex;
 FCountRef:=0;
 setlength(FListRef,0);
 FLabelBreakCount:=0;
 setlength(FLabelContinueList,0);
 setlength(FLabelBreakList,0);
end;

destructor TSubListing.Destroy;
begin
 Fcount:=0;
 Setlength(FList,0);
 Inherited;
end;

procedure TSubListing.add(obj:TAsmObject);
begin
 inc(Fcount);
 Setlength(FList,Fcount);
 FList[Fcount-1]:=Obj;
end;

function TSubListing.AddLabel(name:string;Location:integer):integer;
var
 n:integer;
begin
 //recherche si name existe déjà
 n:=FLabelCount-1;
 while (n>=0) and (FLabelList[n].Name<>name) do dec(n);

 if n=-1 then
  begin
   inc(FLabelCount);
   n:=FLabelCount-1;
   Setlength(FLabelList,FLabelCount);
   FLabelList[n].Name:=Name;
   FLabelList[n].Location:=-1;
  end;
 result:=n;

 FLabelList[n].Location:=Location;
end;

function TSubListing.GetLabelLocation(name:string):integer;
var
 n:integer;
begin
 //recherche si name existe déjà
 n:=FLabelCount-1;
 while (n>0) and (FLabelList[n].Name<>name) do dec(n);
 if n=-1 then result:=-1 else result:=FLabelList[n].Location;
end;

function TSubListing.GetString:string;
var
 i:integer;
begin
 result:='//========='+inttohex(FIndex,4)+#13#10;
 result:=result+'MACRO '+FName+#13#10;
 result:=result+'Label List-------'+#13#10;
 for i:=0 to FLabelCount-1 do
  result:=result+'addr '+FLabelList[i].Name+':'+inttohex(FLabelList[i].Location,4)+#13#10;
 result:=result+'Code---------'+#13#10;
 for i:=0 to Fcount-1 do
  result:=result+inttohex(i,4)+':'+FList[i].GetString+#13#10;

 result:=result+'//========= fin '+FName;
end;

// TListing TListing TListing TListing TListing TListing TListing TListing
//=========================================================================

constructor TListing.create;
begin
 Inherited;
 FCurrentListingIndex:=-1;
 Fcount:=0;
 Setlength(FList,0);
 FIndexEval:=-1;
end;

destructor TListing.Destroy;
var
 i:integer;
begin
 for i:=0 to FCount-1 do FList[i].Free;
 Fcount:=0;
 Setlength(FList,0);
 Inherited;
end;

function TListing.GetCurrentListing:TSubListing;
begin
 result:=FList[FCurrentListingIndex];
end;

function TListing.GetSub(index:integer):TSubListing;
begin
 if (index<0) or (index>=fcount) then result:=nil
                                 else result:=self.flist[index];
end;

function TListing.GetEvalPos:integer;
var
 i:integer;
begin
 result:=0;
 if FCurrentListingIndex=-1 then exit;
 for i:=0 to FCurrentListingIndex-1 do result:=result+FList[i].FCount+FList[i].FLabelCount+5;
 result:=result+FIndexEval+FList[FCurrentListingIndex].FLabelCount+4;
end;

function TListing.AddListing(ChildOf:integer):integer;
begin
 inc(Fcount);
 Setlength(FList,Fcount);
 FList[Fcount-1]:=TSubListing.create(self,ChildOf);
 FList[Fcount-1].FIndex:=Fcount-1;
 FCurrentListingIndex:=Fcount-1;
 result:=Fcount-1;
end;


function TListing.Eval(n:integer;Env:TEnvironnement):integer;
begin
 // début de l'execution
 if FCurrentListingIndex=-1 then
  begin
   FCurrentListingIndex:=0;
   FIndexEval:=0;
   Env.Variables.UpStack;
   Env.Globals.UpStack;
   Env.Result.UpStack;
   Env.FunctionsCall.UpStack;
  end;

 while (FCurrentListingIndex<>-1) and (n<>0) do
  begin
   FList[FCurrentListingIndex].FList[FIndexEval].Eval(env,self);
   inc(FIndexEval);
   dec(n);
  end;
 result:=n;

  // fin de l'execution
 if FCurrentListingIndex=-1 then
  begin
   FIndexEval:=0;
  end;
end;

function TListing.GetString:string;
var
 i:integer;
begin
 result:='';
 for i:=0 to Fcount-1 do
  result:=result+FList[i].GetString+#13#10;
end;

function TListing.GetIndexOfSub(from:integer;named:string):integer;
var
 i:integer;
begin
 result:=-1;
   while (result=-1) and (from<>-1) do
    begin
     for i:=0 to FCount-1 do
      if (FList[i].ParentIndex=from) and
         (FList[i].FName=named) then begin result:=i; break; end;
     if result=-1 then from:=FList[from].ParentIndex;
    end;
end;


//==============================================================================
//==============================================================================
//==============================================================================
// TAsmObject TAsmObject TAsmObject TAsmObject TAsmObject TAsmObject TAsmObject
//==============================================================================
//==============================================================================
//==============================================================================


function TAsmObject.GetValueOf(obj:TStackObject):tvalue;
var
 name:string;
begin
 case obj.ObjectType of
 Object_Constant :result:=obj.ConstantValue;
 Object_Variable :result:=obj.ConstantValue;
 else
   Raise Exception.Create('Constante ou Variable attendue sur la pile (GetValueOf)');
 end;
 obj.Free;
end;


// TCallSubObject TCallSubObject TCallSubObject TCallSubObject TCallSubObject
//============================================================================
// niveau 0:function à suivre (interne ou externe)

constructor TCallSubObject.create(name:string);
begin
 inherited create;
 FunctionName:=name;
end;

function TCallSubObject.GetString:string;
begin
 result:='CALL '+FunctionName;
end;

procedure TCallSubObject.Eval(Env:TEnvironnement;Listing:TBaseListing);
var
 i:integer;
 indexfunction:integer;
 findparent:integer;
 fl:TFunctionsList;
 Args:array of Tvalue;
 sv:TStackObject;
 NewPos:integer;
 res:Tvalue;
begin
 indexfunction:=env.IndexofFunctionCall(FunctionName);

 // si fonction externe, on transmet le tout
 if indexfunction<>-1 then
  begin
   setlength(Args,Env.GetFunctionArgCount(indexfunction));
   for i:=0 to Env.GetFunctionArgCount(indexfunction)-1 do Args[i]:=GetValueOf(Env.Stack.Pop);
   res:=Tvalue.create;

   if assigned(Env.OnFunctionCall) then Env.OnFunctionCall(env,FunctionName,indexfunction,args,res);

   for i:=Env.GetFunctionArgCount(indexfunction)-1 downto 0 do
   if Env.FunctionCallRef[indexfunction,i] then Env.Stack.Push(TStackObject.createConstante(TValue.create(args[i])));
   for i:=0 to Env.GetFunctionArgCount(indexfunction)-1 do Args[i].Free;
   Env.Stack.Push(TStackObject.createConstante(res));
  end
 else // sinon, on appel la fonction interne
  begin
   // on cherche la fonction interne
   indexfunction:=TListing(listing).GetIndexOfSub(TListing(listing).CurrentListing.Index,FunctionName);

   Env.Variables.UpStack;
   Env.Result.UpStack;

   //sauvegarde la position courante dans cette fonction
   Env.FunctionsCall.UpStack;
   fl.FSubIndex:= TListing(listing).CurrentListing.Index;
   fl.FPosIndex:=TListing(listing).FIndexEval;
   Env.FunctionsCall.FunctionList:=fl;

   // definit la nouvelle position d'execution
   TListing(listing).FCurrentListingIndex:=indexfunction;
   TListing(listing).FIndexEval:=-1;
  end;
end;

// TReturnObject TReturnObject TReturnObject TReturnObject TReturnObject
//=======================================================================
// niveau 0:coordonées de retour

constructor TReturnObject.create;
begin
 inherited create;
end;

function TReturnObject.GetString:string;
begin
 result:='RET';
end;

procedure TReturnObject.Eval(Env:TEnvironnement;Listing:TBaseListing);
var
 res:tvalue;
 fl:TFunctionsList;
begin
 fl:=Env.FunctionsCall.FunctionList;
 TListing(Listing).CurrentListingIndex:=fl.FSubIndex;
 TListing(Listing).FIndexEval:=fl.FPosIndex;

 res:=tvalue.create(Env.Result.GetValue('result'));
 Env.Variables.DownStack;
 Env.Result.DownStack;
 Env.FunctionsCall.DownStack;
 if res<>nil then Env.Stack.Push(TStackObject.createConstante(res))
             else Env.Stack.Push(TStackObject.createConstante(TValue.create));
end;


// TPushVariableObject TPushVariableObject TPushVariableObject
//=============================================================
constructor TPushVariableObject.create(name:string);
begin
 inherited create;
 VariableName:=name;
end;

function TPushVariableObject.GetString:string;
begin
 result:='PUSH '+VariableName;
end;

procedure TPushVariableObject.Eval(Env:TEnvironnement;Listing:TBaseListing);
var
 i1,i2:integer;
 value:tvalue;
begin
 i1:=Env.Variables.IndexNameOf(VariableName);
 i2:=Env.Globals.IndexNameOf(VariableName);
 if (i1=-1) and (i2<>-1) then value:=Env.Globals.GetValue(VariableName)
                         else value:=Env.Variables.GetValue(VariableName);
 Env.Stack.Push(TStackObject.createVariable(VariableName,TValue.create(value)));
end;

// TPushValueObject TPushValueObject TPushValueObject TPushValueObject
//=============================================================
constructor TPushValueObject.create(Value:TValue);
begin
 inherited create;
 FValue:=Value;
end;

function TPushValueObject.GetString:string;
begin
 result:='PUSH '+FValue.StringValue;
end;

procedure TPushValueObject.Eval(Env:TEnvironnement;Listing:TBaseListing);
begin
 Env.Stack.Push(TStackObject.createConstante(TValue.create(FValue)));
end;

// TDropObject TDropObject TDropObject TDropObject TDropObject TDropObject
//=========================================================================
constructor TDropObject.create;
begin
 inherited create;
end;

function TDropObject.GetString:string;
begin
 result:='DROP';
end;

procedure TDropObject.Eval(Env:TEnvironnement;Listing:TBaseListing);
var
 tmp:TStackObject;
begin
 tmp:=Env.Stack.Pop;
 tmp.free;
end;

// TEvalObject TEvalObject TEvalObject TEvalObject TEvalObject TEvalObject
//=========================================================================
constructor TEvalObject.create;
begin
 inherited create;
end;

function TEvalObject.GetString:string;
begin
 result:='EVAL';
end;

procedure TEvalObject.Eval(Env:TEnvironnement;Listing:TBaseListing);
var
 value:tvalue;
 sv:TStackObject;
begin
 sv:=Env.Stack.Pop;
 case sv.ObjectType of
 Object_Constant:
  begin
   value:=sv.ConstantValue;
   if value.IsArray then value.NumberValue:=value.NumberValue;
   sv.free;
  end;
 Object_Variable:
  begin
   value:=GetValueOf(sv);
  end;
 end;
 Env.Stack.Push(TStackObject.createConstante(value));
end;

// TJumpObject TJumpObject TJumpObject TJumpObject TJumpObject TJumpObject
//=========================================================================
// niveau 0:label à suivre

constructor TJumpObject.create(Name:string);
begin
 inherited create;
 LabelName:=name;
end;

function TJumpObject.GetString:string;
begin
 result:='JUMP '+LabelName;
end;

procedure TJumpObject.Eval(Env:TEnvironnement;Listing:TBaseListing);
var
 sv:TStackObject;
 lab:string;
 NewPos:integer;
begin
 //sv:=Env.Stack.Pop;
 //if sv.ObjectType<>Object_Label then  Raise Exception.Create('Label attendue sur la pile (TBoolJumpObject.Eval)');
 //lab:=sv.LabelName;
 lab:=LabelName;
 NewPos:=TListing(listing).CurrentListing.GetLabelLocation(lab);
 if NewPos=-1 then  Raise Exception.Create('Label "'+lab+'" non trouvé dans la fonction "'+TListing(listing).CurrentListing.Name+'"(TBoolJumpObject.Eval)');
 TListing(listing).FIndexEval:=NewPos-1;
 //sv.Free;
end;

// TBoolJumpObject TBoolJumpObject TBoolJumpObject TBoolJumpObject
//=================================================================
// niveau 0:valeur boolean (variable ou constante)
// niveau 1:label à suivre si Niveau 0 vrai

constructor TBoolJumpObject.create(Name:string);
begin
 inherited create;
 LabelName:=name;
end;

function TBoolJumpObject.GetString:string;
begin
 result:='BJUMP '+LabelName;
end;

procedure TBoolJumpObject.Eval(Env:TEnvironnement;Listing:TBaseListing);
var
 bool:tvalue;
 sv:TStackObject;
 lab:string;
 NewPos:integer;
begin
 bool:=GetValueOf(Env.Stack.Pop);
 //sv:=Env.Stack.Pop;
 //if sv.ObjectType<>Object_Label then  Raise Exception.Create('Label attendue sur la pile (TBoolJumpObject.Eval)');
 // on saut à label ?
 if bool.BooleanValue then
  begin
   //lab:=sv.LabelName;
   lab:=LabelName;
   NewPos:=TListing(listing).CurrentListing.GetLabelLocation(lab);
   if NewPos=-1 then  Raise Exception.Create('Label "'+lab+'" non trouvé dans la fonction "'+TListing(listing).CurrentListing.Name+'"(TBoolJumpObject.Eval)');
   TListing(listing).FIndexEval:=NewPos-1;
  end;
 bool.Free;
 //sv.Free;
end;

// TOperatorObject TOperatorObject TOperatorObject TOperatorObject
//=================================================================
// niveau 0:valeur 1 (variable ou constante)
// niveau 1:valeur 2 (sauf opérateur unaire) (variable ou constante)

constructor TOperatorObject.create(Name:string);
begin
 inherited create;
 OpName:=name;
end;

function TOperatorObject.GetString:string;
begin
 result:='OP '+OpName;
end;

function MyMod(a,b:extended):extended;
begin
 result:=0;
 if b<0 then  Raise Exception.Create('le second oppérande d''un modulo ne peut pas être négatif');
 if a>0 then
  begin
   while a>=b do begin result:=result+1;a:=a-b; end;
  end
 else
  begin
   while (a<0) do begin result:=result+1;a:=a+b; end;
  end;
 result:=a;
end;

procedure TOperatorObject.Eval(Env:TEnvironnement;Listing:TBaseListing);
var
 u,v,res:tvalue;
begin
 // cas du 'neg' et du 'not'
 if OpName[1]='n' then
  begin
   u:=GetValueOf(Env.Stack.Pop);
  end
 else
  begin
   u:=GetValueOf(Env.Stack.Pop);
   v:=GetValueOf(Env.Stack.Pop);
  end;

 case OpName[1] of
 '~':res:=TValue.create(u.StringValue+v.StringValue);
 '+':res:=TValue.create(u.NumberValue+v.NumberValue);
 '-':res:=TValue.create(u.NumberValue-v.NumberValue);
 '*':res:=TValue.create(u.NumberValue*v.NumberValue);
 '/':res:=TValue.create(u.NumberValue/v.NumberValue);
 '^':res:=TValue.create(power(u.NumberValue,v.NumberValue));
 '%':res:=TValue.create(MyMod(u.tonumber,v.tonumber));
 '|':res:=TValue.create(u.BooleanValue or v.BooleanValue);
 '&':res:=TValue.create(u.BooleanValue and v.BooleanValue);
 '<':
  begin
   if OpName='<=' then
    begin
     if u.IsString or v.IsString
     then res:=TValue.create(u.StringValue<=v.StringValue)
     else res:=TValue.create(u.NumberValue<=v.NumberValue);
    end
   else
     begin
     if u.IsString or v.IsString
     then res:=TValue.create(u.StringValue<v.StringValue)
     else res:=TValue.create(u.NumberValue<v.NumberValue);
    end
  end;
  '>':
  begin
   if OpName='>=' then
    begin
     if u.IsString or v.IsString
     then res:=TValue.create(u.StringValue>=v.StringValue)
     else res:=TValue.create(u.NumberValue>=v.NumberValue);
    end
   else
     begin
     if u.IsString or v.IsString
     then res:=TValue.create(u.StringValue>v.StringValue)
     else res:=TValue.create(u.NumberValue>v.NumberValue);
    end
  end;
  '!':
  begin
   if OpName='!=' then
    begin
     if u.IsString or v.IsString
     then res:=TValue.create(u.StringValue<>v.StringValue)
     else res:=TValue.create(u.NumberValue<>v.NumberValue);
    end;
  end;
  '=':
  begin
     if u.IsString or v.IsString
     then res:=TValue.create(u.StringValue=v.StringValue)
     else res:=TValue.create(u.NumberValue=v.NumberValue);
  end;
  'n':
  begin
   if OpName='neg' then
     res:=TValue.create(-u.NumberValue)
   else
     res:=TValue.create(not u.BooleanValue);
  end;
 end;

 Env.Stack.Push(TStackObject.createConstante(res));

 if OpName[1]='n' then
  begin
   u.free;
  end
 else
  begin
   u.free;
   v.free
  end;
end;

// TArrayObject TArrayObject TArrayObject TArrayObject TArrayObject TArrayObject
//==============================================================================
// niveau 0:valeur (variable, tableau ou valeur)
// niveau 1:valeur à ajouter

constructor TArrayObject.create;
begin
 inherited create;
end;

function TArrayObject.GetString:string;
begin
 result:='ARRAY';
end;

procedure TArrayObject.Eval(Env:TEnvironnement;Listing:TBaseListing);
var
 u,v,nb:tvalue;
 i,c:integer;
begin
 nb:=GetValueOf(Env.Stack.Pop);
 c:=round(nb.NumberValue);

 if c<0 then exit;
 v:=tvalue.create;
 v.ToArray;
 if c>0 then
   for i:=1 to c do
    begin
     u:=GetValueOf(Env.Stack.Pop);
     v.Add(u);
     u.Free;
    end;
 Env.Stack.Push(TStackObject.createConstante(v));
end;

// TExtractObject TExtractObject TExtractObject TExtractObject TExtractObject
//=============================================================================
// niveau 0:valeur
// niveau 1:index à extraire

constructor TExtractObject.create;
begin
 inherited create;
end;

function TExtractObject.GetString:string;
begin
 result:='EXTRACT';
end;

procedure TExtractObject.Eval(Env:TEnvironnement;Listing:TBaseListing);
var
 i,v:tvalue;
 index:integer;
begin
 i:=GetValueOf(Env.Stack.Pop);
 v:=GetValueOf(Env.Stack.Pop);
 index:=round(i.NumberValue);
 i.Free;
 i:=TValue.create(v.ArrayValue[index]);
 v.Free;
 Env.Stack.Push(TStackObject.createConstante(i));
end;

// TStoObject TStoObject TStoObject TStoObject TStoObject TStoObject TStoObject
//==============================================================================
// niveau 0:nom de la variable
// niveau 1:valeur à sauvegarder

constructor TStoObject.create;
begin
 inherited create;
end;

function TStoObject.GetString:string;
begin
 result:='STO';
end;

procedure TStoObject.Eval(Env:TEnvironnement;Listing:TBaseListing);
var
 val:tvalue;
 sv:TStackObject;
 name:string;
 i1,i2:integer;
begin
 sv:=Env.Stack.Pop;
 if sv.ObjectType<>Object_Variable then  Raise Exception.Create('Nom de Variable attendue sur la pile (TStoObject.Eval)');
 val:=GetValueOf(Env.Stack.Pop);

 name:=sv.VarName;
 i1:=Env.Variables.IndexNameOf(name);
 i2:=Env.Globals.IndexNameOf(name);
 if (i1=-1) and (i2<>-1) then Env.Globals.Define(name,val)
                         else Env.Variables.Define(name,val);
 val.free;
 sv.Free;
end;

// TGStoObject TGStoObject TGStoObject TGStoObject TGStoObject TGStoObject
//=========================================================================
// niveau 0:nom de la variable
// niveau 1:valeur à sauvegarder dans Env.Globals

constructor TGStoObject.create;
begin
 inherited create;
end;

function TGStoObject.GetString:string;
begin
 result:='STOG';
end;

procedure TGStoObject.Eval(Env:TEnvironnement;Listing:TBaseListing);
var
 val:tvalue;
 sv:TStackObject;
 name:string;
begin
 sv:=Env.Stack.Pop;
 if sv.ObjectType<>Object_Variable then  Raise Exception.Create('Nom de Variable attendue sur la pile (TStoObject.Eval)');
 val:=GetValueOf(Env.Stack.Pop);
 name:=sv.VarName;
 Env.Globals.Define(name,val);
 val.free;
 sv.Free;
end;

// TResultObject TResultObject TResultObject TResultObject TResultObject
//=======================================================================
// niveau 0:valeur à sauvegarder dans result

constructor TResultObject.create;
begin
 inherited create;
end;

function TResultObject.GetString:string;
begin
 result:='STOR';
end;

procedure TResultObject.Eval(Env:TEnvironnement;Listing:TBaseListing);
var
 val:tvalue;
begin
 val:=GetValueOf(Env.Stack.Pop);
 Env.Result.Define('result',val);
 val.Free;
end;


end.

