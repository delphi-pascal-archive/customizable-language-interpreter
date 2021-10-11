unit UParser;

interface

uses UToken,types,SysUtils,UNodes,UValues,UEnvironnement;

Type
 TParser=class
 private
  FCount:integer;
  FPosInTokens:integer;
  FTokens:TTokenList;
  FRootNodes:array of TNode;
  FBufferToken:array[1..2] of TToken;
  FEnvironnement:TEnvironnement;
  function GetNode(index:integer):tnode;
  procedure AddNode(n:tnode);
  function match(TokenType:TTokenType):TToken;


  // cascade de fonctions pour gérer l'ordre des priorités des opérateurs, des instructions...
  function Commande:tnode;
  function Getvariable:tnode;
  function Getarray:tnode;

  function Block:tnode;
  function Expression:tnode;
  function StringExpression:tnode;
  function StringTerm:tnode;
  function Condition:tnode;
  function BoolExpression:tnode;
  function BoolTerm:tnode;
  function BoolFactor:tnode;
  function BoolRelation:tnode;
  function SumExpression:tnode;
  function Term:tnode;
  function Factor:tnode;
  function SignExpression:tnode;
  function Value:tnode;
  function atom:tnode;
  function functionCall:tnode;
  function IfBlock:tnode;
  function ForBlock:tnode;
  function WhileBlock:tnode;
  function DefineFunction:tnode;
  function DefineGlobal:tnode;
  function DefineReturn:tnode;
 public
  constructor create(Environnement:TEnvironnement);
  procedure Parse(Tokens:TTokenList);
  function Eval:tvalue;
  property Environnement:TEnvironnement read FEnvironnement write FEnvironnement;
  property RootNodes[index:integer]:TNode read GetNode; default;
  property RootCount:integer read FCount;
 end;


implementation

constructor TParser.create(Environnement:TEnvironnement);
begin
 FEnvironnement:=Environnement;
 FCount:=0;
 FRootNodes:=nil;
end;

function TParser.match(TokenType:TTokenType):TToken;
var
 i:integer;
begin
 result:=FTokens[FPosInTokens];
 if result.TokenType=VIDE then Raise Exception.CreateFmt('''%s'' attendu mais la fin du fichier à été atteint', [CTokenTypeStr[TokenType]])
 else
 if result.TokenType<>TokenType then Raise Exception.CreateFmt('''%s'' attendu mais ''%s'' trouvé (%d,%d)', [CTokenTypeStr[TokenType],CTokenTypeStr[result.TokenType],result.position.X,result.position.Y])
 else
  begin
   inc(FPosInTokens);
   while (FTokens[FPosInTokens].TokenType<>VIDE) and (FTokens[FPosInTokens].TokenType=COMMENTAIRE) do inc(FPosInTokens);
   FBufferToken[1]:=FTokens[FPosInTokens];
   i:=FPosInTokens+1;
   while (FTokens[i].TokenType<>VIDE) and (FTokens[i].TokenType=COMMENTAIRE) do inc(i);
   FBufferToken[2]:=FTokens[i];
  end;
end;

procedure TParser.AddNode(n:tnode);
begin
 inc(FCount);
 setlength(FRootNodes,FCount);
 FRootNodes[FCount-1]:=n;
end;

function TParser.GetNode(index:integer):tnode;
begin
 if (index<0) or (index>=FCount) then result:=nil
                                 else result:=FRootNodes[index];
end;

procedure TParser.Parse(Tokens:TTokenList);
var
 i:integer;
begin
 //cherche les 2 premiers tokens autre que commentaire
 FTokens:=Tokens;
 FPosInTokens:=0;
 while (FTokens[FPosInTokens].TokenType<>VIDE) and (FTokens[FPosInTokens].TokenType=COMMENTAIRE) do inc(FPosInTokens);
 FBufferToken[1]:=FTokens[FPosInTokens];
 i:=FPosInTokens+1;
 while (FTokens[i].TokenType<>VIDE) and (FTokens[i].TokenType=COMMENTAIRE) do inc(i);
 FBufferToken[2]:=FTokens[i];

 // on ajoute chaque commandes
 while FBufferToken[1].TokenType<>VIDE do AddNode(Commande);
end;


function TParser.Commande:tnode;
var
 t1,t2:TTokenType;
 n1,n2:tnode;
 p:tpoint;
begin
 t1:=FBufferToken[1].TokenType;
 t2:=FBufferToken[2].TokenType;
 if (t1=VARIABLE) and (t2=PARGAUCHE) then
   begin
     result := functionCall;
     match(FIN_COMMANDE);
     exit;
   end
 else
 if (t1=VARIABLE) then
   begin
     n1 := Getvariable;
     p:=match(OP_EGAL).position;
     n2:=expression;
     match(FIN_COMMANDE);
     result:=TAssigneNode.create(p,n1,n2);
     exit;
   end
 else
 if (t1=INSTR_IF) then result:=IfBlock
 else
 if (t1=INSTR_FOR) then result:=ForBlock
 else
 if (t1=INSTR_WHILE) then result:=WhileBlock
 else
 if (t1=INSTR_FUNCTION) then result:=DefineFunction
 else
 if (t1=INSTR_RETURN) then result:=DefineReturn
 else
 if (t1=INSTR_GLOBAL) then result:=DefineGlobal
 else
 result:=nil;
end;

function TParser.Getvariable:tnode;
var
 t:TToken;
begin
 t:=match(VARIABLE);
 result:=TVariableNode.create(t.Position,t.Value);
 while FBufferToken[1].TokenType=CROCHETGAUCHE do
   begin
    match(CROCHETGAUCHE);
    result.AddNode(expression);
    match(CROCHETDROITE);
   end;
end;

function TParser.Getarray:tnode;
var
 t:ttoken;
begin
 t:=match(CROCHETGAUCHE);
 result:=TArrayNode.create(t.position);
 while FBufferToken[1].TokenType<>CROCHETDROITE do
  begin
   result.AddNode(expression);
   if FBufferToken[1].TokenType<>CROCHETDROITE then match(VIRGULE);
  end;
 match(CROCHETDROITE);
end;

function TParser.Atom:tnode;
var
 t:TToken;
 v:tvalue;
begin
 case FBufferToken[1].TokenType of
  NOMBRE:
   begin
    t:=match(FBufferToken[1].TokenType);
    v:=TValue.create(t.position,t.value);
    v.ToNumber;
    result:=TValueNode.create(t.position,v);
   end;
  VRAI:
   begin
    t:=match(FBufferToken[1].TokenType);
    result:=TValueNode.create(t.position,TValue.create(t.position,True));
   end;
  FAUX:
   begin
    t:=match(FBufferToken[1].TokenType);
    result:=TValueNode.create(t.position,TValue.create(t.position,False));
   end;
  VARIABLE:result:=getvariable;
 PARGAUCHE:
  begin
   match(PARGAUCHE);
   result:= expression;
   match(PARDROITE);
  end;
 end;
end;


function TParser.Expression:tnode;
begin
 if FBufferToken[1].TokenType=CROCHETGAUCHE then
  result:=Getarray
 else
  result:=stringExpression;
end;

function TParser.StringExpression:tnode;
var
 t:ttoken;
begin
  result:= StringTerm;
  if FBufferToken[1].TokenType = OP_CONCATENATION then
   begin
      t:=match(FBufferToken[1].TokenType);
      result:=TBinaryOpNode.create(t.position, '~', result, stringExpression());
   end;
end;

function TParser.StringTerm:tnode;
var
 t:TToken ;
begin
  if FBufferToken[1].TokenType =CHAINE then
   begin
      t:=match(CHAINE);
      result:=TValueNode.create(t.position,TValue.create(t.position,t.value));
   end
  else
    result:=boolExpression;
end;

function TParser.BoolExpression:tnode;
var
 t:TToken ;
begin
 result:=boolTerm();
 if FBufferToken[1].TokenType=OP_OU then
  begin
   t:=match(OP_OU);
   result:=TBinaryOpNode.create(t.position,'||', result, boolExpression);
  end;
end;

function TParser.BoolTerm:tnode;
var
 t:TToken ;
begin
 result := boolFactor;
 if FBufferToken[1].TokenType=OP_ET then
  begin
   t:=match(OP_ET);
   result:=TBinaryOpNode.create(t.position,'&&', result, boolTerm);
  end;
end;

function TParser.BoolFactor:tnode;
var
 t:TToken ;
begin
 if FBufferToken[1].TokenType=OP_NON then
  begin
   t:=match(OP_NON);
   result:=TUnaryOpNode.create(t.position,'!', BoolRelation);
  end
 else result:=BoolRelation;
end;

function TParser.BoolRelation:tnode;
var
  t:TToken ;
begin
 result := sumExpression;
 if  FBufferToken[1].TokenType in [OP_INFERIEUR,OP_INFERIEUREGAL,OP_EGAL,OP_SUPERIEUREGAL,OP_SUPERIEUR,OP_DIFFERENT] then
  begin
   t:=match(FBufferToken[1].TokenType);
   case t.TokenType of
    OP_INFERIEUR     : result:=TBinaryOpNode.create(t.position,'<',result, sumExpression);
    OP_INFERIEUREGAL : result:=TBinaryOpNode.create(t.position,'<=',result, sumExpression);
    OP_EGAL          : result:=TBinaryOpNode.create(t.position,'=', result,sumExpression);
    OP_SUPERIEUREGAL : result:=TBinaryOpNode.create(t.position,'>=',result, sumExpression);
    OP_SUPERIEUR     : result:=TBinaryOpNode.create(t.position,'>',result, sumExpression);
    OP_DIFFERENT     : result:=TBinaryOpNode.create(t.position,'!=',result, sumExpression);
   end;
  end;
end;

function TParser.SumExpression:tnode;
var
 t:ttoken;
begin
 result := term();
 while FBufferToken[1].TokenType in [OP_PLUS,OP_MOINS] do
  begin
   t:=match(FBufferToken[1].TokenType);
   case t.TokenType of
    OP_PLUS:result:=TBinaryOpNode.create(t.position,'+',result, term);
    OP_MOINS:result:=TBinaryOpNode.create(t.position,'-',result, term);
   end;
  end;
end;

function TParser.Term:tnode;
var
 t:ttoken;
begin
 result := Factor();
 while FBufferToken[1].TokenType in [OP_MULTIPLIE, OP_DIVISE, OP_MODULO] do
  begin
   t:=match(FBufferToken[1].TokenType);
   case t.TokenType of
    OP_MULTIPLIE:result:=TBinaryOpNode.create(t.position,'*',result, Factor);
    OP_DIVISE:result:=TBinaryOpNode.create(t.position,'/',result, Factor);
    OP_MODULO:result:=TBinaryOpNode.create(t.position,'%',result, Factor);
   end;
  end;
end;

function TParser.Factor:tnode;
var
 t:ttoken;
begin
 result := signExpression();
 while FBufferToken[1].TokenType in [OP_PUISSANCE] do
  begin
   t:=match(OP_PUISSANCE);
   result:=TBinaryOpNode.create(t.position,'^',result, signExpression);
  end;
end;

function TParser.SignExpression:tnode;
var
 t:tToken;
begin
 t:=nil;
 if FBufferToken[1].TokenType=OP_MOINS then t:=match(OP_MOINS)
 else
 if FBufferToken[1].TokenType=OP_PLUS then t:=match(OP_PLUS);
 result:=value;
 if (t<>nil) and (t.TokenType=OP_MOINS) then result:=TUnaryOpNode.create(t.position,'-', result);
end;

function TParser.Value:tnode;
begin
 if (FBufferToken[1].TokenType=VARIABLE) and (FBufferToken[2].TokenType=PARGAUCHE) then
  result:=functionCall
 else result:=atom;
end;

function TParser.functionCall:tnode;
var
 t1,t2:tToken;
 n1,n2:tnode;
begin
 t1:=match(VARIABLE);
 t2:=match(PARGAUCHE);
 result:=TFunctionCallNode.create(t1.position,t1.value);
 if FBufferToken[1].TokenType<>PARDROITE then
  begin
    result.AddNode(expression);
    while FBufferToken[1].TokenType=VIRGULE do
     begin
      match(VIRGULE);
      result.AddNode(expression);
     end;
  end;
 match(PARDROITE);
end;

function TParser.Condition:tnode;
begin
 match(PARGAUCHE);
 result := BoolExpression;
 match(PARDROITE);
end;

function TParser.IfBlock:tnode;
var
 t:ttoken;
 n1,n2,n3:tnode;
begin
 t:=match(INSTR_IF);
 n1:=condition;
 n2:=block;
 if FBufferToken[1].TokenType=INSTR_ELSE then
  begin
   match(INSTR_ELSE);
   n3:=block;
  end
 else
  n3:=nil;

 result:=TIfNode.create(t.position,n1,n2,n3);
end;

function TParser.ForBlock:tnode;
var
 t:ttoken;
 n:tnode;
begin
 t:=match(INSTR_FOR);
 n:=Getvariable;
 result:=TFornode.create(t.position,n);
 if FBufferToken[1].TokenType=INSTR_IN then
  begin
   match(INSTR_IN);
   case FBufferToken[1].TokenType of
   VARIABLE: result.AddNode(Getvariable);
   CROCHETGAUCHE: result.AddNode(GetArray);
   end;
   match(INSTR_DO);
   result.AddNode(Block);
  end
 else
  begin
   match(INSTR_FROM);
   result.AddNode(SumExpression);
   match(INSTR_TO);
   result.AddNode(SumExpression);
   match(INSTR_DO);
   result.AddNode(Block);
  end;
end;

function TParser.WhileBlock:tnode;
var
 t:ttoken;
 n1,n2:tnode;
begin
 t:=match(INSTR_WHILE);
 n1:=condition;
 n2:=block;
 result:=TWhileNode.create(t.position,n1,n2);
end;


function TParser.DefineFunction:tnode;
var
 t1,t2:ttoken;
 n,b:tnode;
 p:tparamnode;
 s:string;
 ref:boolean;
begin
 t1:=match(INSTR_FUNCTION);
 n:=getvariable;
 s:=TVariableNode(n).VariableName;
 if FEnvironnement.IndexofFunctionCall(s)<>-1 then Raise Exception.CreateFmt('''%s'' est un nom de fonction externe réservé (%d,%d)', [s,n.Position.X,n.Position.Y]) ;
 t2:=match(PARGAUCHE);
 p:=tparamnode.create(t2.position);
 while FBufferToken[1].TokenType<>PARDROITE do
  begin
    ref:=FBufferToken[1].TokenType=INSTR_REFERENCE;
    if ref then match(INSTR_REFERENCE);
    p.AddNode(GetVariable,ref);

    if FBufferToken[1].TokenType=VIRGULE then match(VIRGULE);
  end;
 match(PARDROITE);
 b:=block;
 result:=TFunctionnode.create(t1.position,n,p,b);
end;

function TParser.DefineGlobal:tnode;
var
 t1:ttoken;
 p:tpoint;
 n1,n2:tnode;
begin
 t1:=match(INSTR_GLOBAL);
 n1:=getvariable;
 if FBufferToken[1].TokenType=OP_EGAL then
  begin
   match(OP_EGAL);
   n2:=expression;
  end
 else
  n2:=nil;
 match(FIN_COMMANDE);
 result:=TGlobalNode.create(p,n1,n2);
end;

function TParser.DefineReturn:tnode;
var
 t:ttoken;
 n:tnode;
begin
 t:=match(INSTR_RETURN);
 n:=expression;
 match(FIN_COMMANDE);
 result:=TReturnNode.create(t.position,n);
end;

function TParser.Block:tnode;
var
 t:ttoken;
 n:tnode;
begin
 t:=FBufferToken[1];
 result:=TBlockNode.create(t.position);
 if t.TokenType=ACCOLADEGAUCHE then
  begin
   t:=match(ACCOLADEGAUCHE);
   result.AddNode(commande);
   while FBufferToken[1].TokenType<>ACCOLADEDROITE do
    result.AddNode(commande);
   match(ACCOLADEDROITE);
  end
 else
  result.AddNode(commande);
end;


function TParser.Eval:tvalue;
var
 i:integer;
begin
 // on initialise les piles l'environnement
 FEnvironnement.Variables.UpStack;
 FEnvironnement.Functions.UpStack;
 FEnvironnement.Globals.UpStack;
 // on effectue chaque commandes
 for i:=0 to FCount-1 do
  begin
   FRootNodes[i].Eval(FEnvironnement);
  end;
 // on récupère l'éventuel resultat
 result:=FEnvironnement.Result;
 FEnvironnement.Globals.DownStack;
 FEnvironnement.Variables.DownStack;
 FEnvironnement.Functions.DownStack;
end;

end.
