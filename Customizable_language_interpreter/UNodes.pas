unit UNodes;

interface

uses types,SysUtils,UValues,UEnvironnement,Math;


type
 TNode=class
 private
  FPosition:tpoint;
  FName:string;
  FCount:integer;
  FNodesList:array of TNode;
  function GetNode(index:integer):tnode;
 public
  constructor create(position:tpoint);
  procedure Eval(Env:TEnvironnement);virtual; abstract;
  procedure AddNode(n:tnode);
  property Position:tpoint read FPosition;
  property Name:string read FName;
  property NodeCount:integer read Fcount;
  property NodesList[index:integer]:TNode read GetNode;default;
 end;

 TBinaryOpNode=class(TNode)
 private
  FOpName:string;
 public
  constructor create(position:tpoint;opcode:string;n1,n2:tnode);
  procedure Eval(Env:TEnvironnement);override;
 end;

 TUnaryOpNode=class(TNode)
 private
  FopName:string;
 public
  constructor create(position:tpoint;opcode:string;n:tnode);
  procedure Eval(Env:TEnvironnement);override;
 end;

 TVariableNode=class(TNode)
 private
  FVariableName:string;
 public
  constructor create(position:tpoint;varname:string);
  procedure Eval(Env:TEnvironnement);override;
  property VariableName:string read FVariableName;
 end;

 TValueNode=class(TNode)
 private
  FValue:TValue;
 public
  constructor create(position:tpoint;Value:TValue);
  destructor free;
  procedure Eval(Env:TEnvironnement);override;
 end;

 TArrayNode=class(TNode)
 private
 public
  constructor create(position:tpoint);
  procedure Eval(Env:TEnvironnement);override;
 end;

 TAssigneNode=class(TNode)
 private
 public
  constructor create(position:tpoint;v,e:tnode);
  procedure Eval(Env:TEnvironnement);override;
 end;


 TFunctionCallNode=class(TNode)
 private
  FFunctionName:string;
 public
  constructor create(position:tpoint;name:string);
  procedure Eval(Env:TEnvironnement);override;
 end;

 TIfNode=class(tnode)
 private
 public
  constructor create(position:tpoint;condition,thenblock,elseblock:tnode);
  procedure Eval(Env:TEnvironnement);override;
 end;

 TBlockNode=class(tnode)
 private
 public
  constructor create(position:tpoint);
  procedure Eval(Env:TEnvironnement);override;
 end;

 TForNode=class(tnode)
 private
 public
  constructor create(position:tpoint;v:tnode);
  procedure Eval(Env:TEnvironnement);override;
 end;

 TWhileNode=class(TNode)
 private
 public
  constructor create(position:tpoint;c,b:tnode);
  procedure Eval(Env:TEnvironnement);override;
 end;

 TFunctionNode=class(TNode)
 private
 public
  constructor create(position:tpoint;n,p,b:tnode);
  procedure Eval(Env:TEnvironnement);override;
  procedure EvalFunction(Env:TEnvironnement);
 end;

 TGlobalNode=class(TNode)
 private
 public
  constructor create(position:tpoint;v,e:tnode);
  procedure Eval(Env:TEnvironnement);override;
 end;

 TReturnNode=class(TNode)
 private
 public
  constructor create(position:tpoint;e:tnode);
  procedure Eval(Env:TEnvironnement);override;
 end;


 TParamNode=class(TNode)
 private
  FRef:array of boolean;
 public
  procedure Eval(Env:TEnvironnement);override;
  procedure AddNode(n:tnode;ref:boolean);overload;
 end;



implementation


// TNode TNode TNode TNode TNode TNode TNode TNode TNode TNode TNode TNode TNode
//==============================================================================
constructor TNode.create(position:tpoint);
begin
 inherited create;
 FPosition:=position;
 FCount:=0;
 FNodesList:=nil;
end;

function TNode.GetNode(index:integer):tnode;
begin
 if (index<0) or (index>=Fcount) then result:=nil
                                 else result:=FNodesList[index];
end;

procedure TNode.AddNode(n:tnode);
begin
 inc(fcount);
 setlength(FNodesList,Fcount);
 FNodesList[FCount-1]:=n;
end;

// TFunctionCallNode TFunctionCallNode TFunctionCallNode TFunctionCallNode
//=========================================================================
constructor TFunctionCallNode.create(position:tpoint;name:string);
begin
 inherited create(position);
 FFunctionName:=name;
 Fname:='Appel Fonction "'+name+'"';
end;

procedure TFunctionCallNode.Eval(Env:TEnvironnement);
var
 i:integer;
 indexfunction:integer;
 n:TFunctionNode;
 arg:tvalue;
 s:string;
 IsByRef:boolean;
begin
 Env.Result.free;
 Env.Result:=TValue.create;
 // recherche la fonction (interne ou externe) à appeler
 indexfunction:=env.IndexofFunctionCall(FFunctionName);
 if indexfunction<>-1 then
  begin
   if Env.GetFunctionArgCount(indexfunction)<>Fcount then Raise Exception.CreateFmt('appel à ''%s'' avec un nombre incorrect de paramètres (%d,%d)', [FFunctionName,FPosition.X,FPosition.Y]);
  end
 else
  begin
   n:=TFunctionNode(Env.Functions.GetFunction(FFunctionName));
   if n=nil then Raise Exception.CreateFmt('''%s'' n''est pas défini (%d,%d)', [FFunctionName,FPosition.X,FPosition.Y]);
   if n.FNodesList[1].FCount<>Fcount then Raise Exception.CreateFmt('appel à ''%s'' avec un nombre incorrect de paramètres (%d,%d)', [FFunctionName,FPosition.X,FPosition.Y]);
  end;

 // évaluation des arguments (ajout à la pile) du dernier au premier
 // le premier sur la pile sera le premier argument
 for i:=Fcount-1 downto 0 do
  begin
   if indexfunction<>-1 then IsByRef:=Env.FunctionCallRef[indexfunction,i]
                        else IsByRef:=TParamNode(n.FNodesList[1]).FRef[i];
   if IsByRef and not (NodesList[i] is TVariableNode) then
    Raise Exception.CreateFmt('Appel à la fonction ''%s'' avec l''argument %d différent d''une variable (%d,%d)', [FFunctionName,i,FPosition.X,FPosition.Y]);
   NodesList[i].Eval(env);
  end;


 // si fonction externe, on transmet le tout
 if indexfunction<>-1 then
  begin
   if assigned(Env.OnFunctionCall) then Env.OnFunctionCall(env,FFunctionName,indexfunction,FCount);
  end
 else // sinon, on appel la fonction interne
  begin
   n.EvalFunction(Env);
  end;

 // retire de la pile les arguments et enregistre ceux qui ont une réference
 // le premier sur la pile sera le premier argument
 for i:=0 to Fcount-1 do
  begin
   if indexfunction<>-1 then IsByRef:=Env.FunctionCallRef[indexfunction,i]
                        else IsByRef:=TParamNode(n.FNodesList[1]).FRef[i];
   arg:=Env.Stack.Pop;
   if IsByRef then
   Env.Variables.Define(TVariableNode(NodesList[i]).FVariableName,arg);
   arg.Free;
  end;

 // on ajoute à la pile le résultat
 Env.Stack.Push(TValue.create(Env.Result));
end;

// TVariableNode TVariableNode TVariableNode TVariableNode TVariableNode
//=======================================================================

constructor TVariableNode.create(position:tpoint;varname:string);
begin
 inherited create(position);
 FVariableName:=varname;
 Fname:='Variable "'+varname+'"';
end;

procedure TVariableNode.Eval(Env:TEnvironnement);
var
 i:integer;
 index,v:TValue;
begin
 if Env.Variables.IndexNameOf(FVariableName)=-1 then
  begin
   if Env.Globals.IndexNameOf(FVariableName)=-1 then v:=Env.Variables.DefineNew(FVariableName)
                                                else v:=Env.Globals.GetValue(FVariableName);
  end
 else
   v:=Env.Variables.GetValue(FVariableName);
 v:=TValue.create(v);

 for i:=0 to fcount-1 do
  begin
   if not v.IsArray then  Raise Exception.CreateFmt('''%s'' n''est pas un tableau à %d dimensions (%d,%d)', [FVariableName,fcount,FPosition.X,FPosition.Y]);
   FNodesList[i].eval(Env);
   index:=Env.Stack.pop;
   v:=v.ArrayValue[round(Index.NumberValue)];
   index.free;
  end;
 Env.Stack.Push(v);
end;
                     
// TArrayNode TArrayNode TArrayNode TArrayNode TArrayNode TArrayNode TArrayNode
//==============================================================================

constructor TArrayNode.create(position:tpoint);
begin
 inherited create(position);
 Fname:='Tableau';
end;

procedure TArrayNode.Eval(Env:TEnvironnement);
var
 i:integer;
 v,w:TValue;
begin
 v:=tvalue.create;
 for i:=0 to fcount-1 do
  begin
   FNodesList[i].eval(Env);
   w:=Env.Stack.Pop;
   v.Add(w);
   w.free;
  end;
 Env.Stack.Push(v);
end;


// TValueNode TValueNode TValueNode TValueNode TValueNode TValueNode TValueNode
//==============================================================================

constructor TValueNode.create(position:tpoint;Value:TValue);
begin
 inherited create(position);
 case Value.TypeValue of
 TypeNone   : FName:='None';
 TypeNumber : FName:='Number:'+ Value.StringValue;
 TypeString : FName:='String:'+ Value.StringValue;
 TypeBoolean: FName:='Boolean:'+ Value.StringValue;
 TypeArray  : FName:='Array:'+ Value.StringValue;
 end;
 FValue:=Value;
end;

procedure TValueNode.Eval(Env:TEnvironnement);
begin
 Env.Stack.Push(TValue.create(FValue));
end;

destructor TValueNode.free;
begin
 FValue.free;
 inherited free;
end;

// TAssigneNode TAssigneNode TAssigneNode TAssigneNode TAssigneNode TAssigneNode
//==============================================================================

constructor TAssigneNode.create(position:tpoint;v,e:tnode);
begin
 inherited create(position);
 Fname:='Assignation';
 Fcount:=2;
 setlength(FNodesList,2);
 FNodesList[0]:=v;
 FNodesList[1]:=e;
end;

procedure TAssigneNode.Eval(Env:TEnvironnement);
var
 s:string;
 i1,i2:integer;
 u:TValue;
begin
 FNodesList[1].eval(Env);
 s:=TVariableNode(FNodesList[0]).VariableName;
 i1:=Env.Variables.IndexNameOf(s);
 i2:=Env.Globals.IndexNameOf(s);
 u:=Env.Stack.Pop;
 if (i1=-1) and (i2<>-1) then Env.Globals.Define(s,u)
                         else Env.Variables.Define(s,u);
 u.free;
end;


// TBinaryOpNode TBinaryOpNode TBinaryOpNode TBinaryOpNode TBinaryOpNode
//=======================================================================

constructor TBinaryOpNode.create(position:tpoint;opcode:string;n1,n2:tnode);
begin
 inherited create(position);
 Fname:='Opperator '+opcode;
 FOpName:=opcode;
 Fcount:=2;
 setlength(FNodesList,2);
 FNodesList[0]:=n1;
 FNodesList[1]:=n2;
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

procedure TBinaryOpNode.Eval(Env:TEnvironnement);
var
 u,v,res:tvalue;
begin
 FNodesList[0].Eval(Env);
 FNodesList[1].Eval(Env);
 v:=Env.Stack.Pop;
 u:=Env.Stack.Pop;
 case FOpName[1] of
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
   if length(FOpName)=2 then
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
   if length(FOpName)=2 then
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
     if u.IsString or v.IsString
     then res:=TValue.create(u.StringValue<>v.StringValue)
     else res:=TValue.create(u.NumberValue<>v.NumberValue);
  end;
  '=':
  begin
     if u.IsString or v.IsString
     then res:=TValue.create(u.StringValue=v.StringValue)
     else res:=TValue.create(u.NumberValue=v.NumberValue);
  end;
 end;
 Env.Stack.Push(res);
 u.free;
 v.free;
end;

// TUnaryOpNode TUnaryOpNode TUnaryOpNode TUnaryOpNode TUnaryOpNode
//=======================================================================

constructor TUnaryOpNode.create(position:tpoint;opcode:string;n:tnode);
begin
 inherited create(position);
 Fname:='Opperator '+opcode;
 FopName:=opcode;
 Fcount:=1;
 setlength(FNodesList,1);
 FNodesList[0]:=n;
end;

procedure TUnaryOpNode.Eval(Env:TEnvironnement);
var
 u:tvalue;
begin
 FNodesList[0].Eval(Env);
 u:=Env.Stack.Pop;
 case FOpName[1] of
 '-':u:=TValue.create(-u.ToNumber);
 '!':u:=TValue.create(not u.toboolean);
 end;
 Env.Stack.push(u);
end;

// TIfNode TIfNode TIfNode TIfNode TIfNode TIfNode TIfNode TIfNode TIfNode
//=========================================================================

constructor TIfNode.create(position:tpoint;condition,thenblock,elseblock:tnode);
begin
 inherited create(position);
 Fname:='if';
 Fcount:=2;
 setlength(FNodesList,2);
 FNodesList[0]:=condition;
 FNodesList[1]:=thenblock;
 if elseblock<>nil then  addnode(elseblock);
end;

procedure TIfNode.Eval(Env:TEnvironnement);
var
 u:tvalue;
begin
 FNodesList[0].Eval(Env);
 u:=Env.Stack.pop;
 // évaluation du then
 if u.BooleanValue then FNodesList[1].Eval(Env)
 else
 // évaluation du else si il y en a un
 if FCount=3 then FNodesList[2].Eval(Env);
end;

// TBlockNode TBlockNode TBlockNode TBlockNode TBlockNode TBlockNode TBlockNode
//==============================================================================

constructor TBlockNode.create(position:tpoint);
begin
 inherited create(position);
 Fname:='block';
end;

procedure TBlockNode.Eval(Env:TEnvironnement);
var
 i:integer;
begin
 for i:=0 to FCount-1 do
  begin
   FNodesList[i].Eval(Env);
  end;
end;

// TForNode TForNode TForNode TForNode TForNode TForNode TForNode TForNode
//=========================================================================

constructor TForNode.create(position:tpoint;v:tnode);
begin
 inherited create(position);
 Fname:='for';
 Fcount:=1;
 setlength(FNodesList,1);
 FNodesList[0]:=v;
end;

procedure TForNode.Eval(Env:TEnvironnement);
var
 v,w:tvalue;
 varname:string;
 ii:integer;
 i,a,b:extended;
begin
 varname:=TVariableNode(FNodesList[0]).VariableName;
 // FOR...IN...DO...
 if Fcount=3 then
  begin
   FNodesList[1].Eval(Env);
   v:=Env.Stack.Pop;

   for ii:=0 to v.ArrayLenght-1 do
    begin
     Env.Variables.Define(varname,v.ArrayValue[ii]);
     FNodesList[2].Eval(Env);
    end;
   v.free;
  end
 // FOR...FROM...TO...DO
 else
  begin
   FNodesList[1].Eval(Env);
   FNodesList[2].Eval(Env);
   v:=Env.Stack.Pop;
   b:=v.ToNumber;
   v.free;

   v:=Env.Stack.Pop;
   a:=v.ToNumber;
   v.free;
   i:=a;
   v:=TValue.create(i);
   Env.Variables.Define(varname,v);
   if a<b then
    begin
     while i<=b do
      begin
       FNodesList[3].Eval(Env);
       i:=i+1;
       v.NumberValue:=i;
       Env.Variables.Define(varname,v);
      end;
    end
   else
   if a>b then
   while i>=b do
    begin
     FNodesList[3].Eval(Env);
     i:=i-1;
     v.NumberValue:=i;
     Env.Variables.Define(varname,v);
    end;
   v.free;
  end;
end;

// TWhileNode TWhileNode TWhileNode TWhileNode TWhileNode TWhileNode TWhileNode
//==============================================================================

constructor TWhileNode.create(position:tpoint;c,b:tnode);
begin
 inherited create(position);
 Fname:='while';
 Fcount:=2;
 setlength(FNodesList,2);
 FNodesList[0]:=c;
 FNodesList[1]:=b;
end;

procedure TWhileNode.Eval(Env:TEnvironnement);
var
 u:tvalue;
begin
 FNodesList[0].Eval(Env);
 u:=Env.Stack.pop;
 while (u.BooleanValue) do
  begin
   u.free;
   FNodesList[1].Eval(Env);
   FNodesList[0].Eval(Env);
   u:=Env.Stack.pop;
  end;
 u.free;
end;

// TFunctionNode TFunctionNode TFunctionNode TFunctionNode TFunctionNode
//=======================================================================

constructor TFunctionNode.create(position:tpoint;n,p,b:tnode);
begin
 inherited create(position);
 Fname:='function';
 Fcount:=3;
 setlength(FNodesList,3);
 FNodesList[0]:=n;
 FNodesList[1]:=p;
 FNodesList[2]:=b;
end;

procedure TFunctionNode.Eval(Env:TEnvironnement);
begin
 // sauvegarde ce noeud comme début d'une fonction
 Env.Functions.Define(TVariableNode(FNodesList[0]).VariableName,self);
end;


// traite l'appel à cette fonction
procedure TFunctionNode.EvalFunction(Env:TEnvironnement);
var
 i:integer;
 arg:tvalue;
begin
 // on ajout un niveau aux variables
 Env.Variables.UpStack;
 Env.Functions.UpStack;

 // on assigne chaque argument à sa valeur
 for i:=0 to FNodesList[1].Fcount-1 do
  begin
   arg:=Env.Stack.pop;
   Env.Variables.Define(TVariableNode(FNodesList[1].FNodesList[i]).FVariableName,arg);
   arg.free;
  end;

 FNodesList[2].Eval(Env);

 // on renvoi chaque argument sur la pile (pour les réferences)
 for i:=FNodesList[1].Fcount-1 downto 0 do
  begin
   Env.Stack.push(TValue.create(Env.Variables.GetValue(TVariableNode(FNodesList[1].FNodesList[i]).FVariableName)));
  end;

 // on supprime un niveau aux variables
 Env.Variables.DownStack;
 Env.Functions.DownStack;
end;

// TGlobalNode TGlobalNode TGlobalNode TGlobalNode TGlobalNode TGlobalNode
//=========================================================================

constructor TGlobalNode.create(position:tpoint;v,e:tnode);
begin
 inherited create(position);
 Fname:='Global';
 Fcount:=2;
 setlength(FNodesList,2);
 FNodesList[0]:=v;
 FNodesList[1]:=e;
end;

procedure TGlobalNode.Eval(Env:TEnvironnement);
var
 v:tvalue;
begin
 if FNodesList[1]=nil then v:=TValue.create
                      else
                       begin
                        FNodesList[1].eval(Env);
                        v:=Env.Stack.Pop;
                       end;
 Env.Globals.Define(TVariableNode(FNodesList[0]).VariableName,v);
 v.free;
end;

// TReturnNode TReturnNode TReturnNode TReturnNode TReturnNode TReturnNode
//=========================================================================

constructor TReturnNode.create(position:tpoint;e:tnode);
begin
 inherited create(position);
 Fname:='return';
 Fcount:=1;
 setlength(FNodesList,1);
 FNodesList[0]:=e;
end;

procedure TReturnNode.Eval(Env:TEnvironnement);
var
 v:tvalue;
begin
 FNodesList[0].eval(Env);
 v:=Env.Stack.pop;
 Env.Result.Assign(v);
 v.free;
end;


// TParamNode TParamNode TParamNode TParamNode TParamNode TParamNode TParamNode
//==============================================================================

procedure TParamNode.AddNode(n:tnode;ref:boolean);
begin
 inherited AddNode(n);
 setlength(FRef,Fcount);
 FRef[Fcount-1]:=ref;
end;

procedure TParamNode.Eval(Env:TEnvironnement);
begin
 //
end;


end.
