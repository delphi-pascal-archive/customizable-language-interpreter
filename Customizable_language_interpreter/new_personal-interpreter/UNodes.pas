unit UNodes;

interface

uses types,SysUtils,UValues,UEnvironnement,Math,UToken,UListing;

type
 TStopLoop=(NEXT_LOOP,STOP_LOOP);

type
 TNode=class
 private
  FToken:TToken;
  FName:string;
  FCount:integer;
  FNodesList:array of TNode;
  function GetNode(index:integer):tnode;
 public
  constructor create(Token:TToken);
  procedure AddToListing(list:TListing;Env:TEnvironnement);virtual; abstract;
  procedure AddNode(n:tnode);
  property Token:TToken read FToken;
  property Name:string read FName;
  property NodeCount:integer read Fcount;
  property NodesList[index:integer]:TNode read GetNode;default;
 end;

 TBinaryOpNode=class(TNode)
 private
  FOpName:string;
 public
  constructor create(Token:TToken;opcode:string;n1,n2:tnode);
  procedure AddToListing(list:TListing;Env:TEnvironnement);override;
 end;

 TUnaryOpNode=class(TNode)
 private
  FopName:string;
 public
  constructor create(Token:TToken;opcode:string;n:tnode);
  procedure AddToListing(list:TListing;Env:TEnvironnement);override;
 end;

 TVariableNode=class(TNode)
 private
  FVariableName:string;
 public
  constructor create(Token:TToken;varname:string);
  property VariableName:string read FVariableName;
  procedure AddToListing(list:TListing;Env:TEnvironnement);override;
 end;

 TLabelNode=class(TNode)
 private
  FLabelName:string;
 public
  constructor create(Token:TToken;LabelName:string);
  property LabelName:string read FLabelName;
  procedure AddToListing(list:TListing;Env:TEnvironnement);override;
 end;

 TGotoNode=class(TNode)
 private
  FLabelName:string;
 public
  constructor create(Token:TToken;LabelName:string);
  property LabelName:string read FLabelName;
  procedure AddToListing(list:TListing;Env:TEnvironnement);override;
 end;

 TStopLoopNode=class(TNode)
 private
  FStopType:TStopLoop;
 public
  constructor create(Token:TToken;StopType:TStopLoop);
  property TStopType:TStopLoop read FStopType;
  procedure AddToListing(list:TListing;Env:TEnvironnement);override;
 end;

 TValueNode=class(TNode)
 private
  FValue:TValue;
 public
  constructor create(Token:TToken;Value:TValue);
  destructor Destroy; override;
  procedure AddToListing(list:TListing;Env:TEnvironnement);override;
 end;

 TArrayNode=class(TNode)
 private
 public
  constructor create(Token:TToken);
  procedure AddToListing(list:TListing;Env:TEnvironnement);override;
 end;

 TAssigneNode=class(TNode)
 private
 public
  constructor create(Token:TToken;v,e:tnode);
  procedure AddToListing(list:TListing;Env:TEnvironnement);override;
 end;


 TFunctionCallNode=class(TNode)
 private
  FFunctionName:string;
  FIsFunct:boolean;
 public
  constructor create(Token:TToken;name:string;IsFunct:boolean);
  procedure AddToListing(list:TListing;Env:TEnvironnement);override;
 end;

 TFunctionNode=class(TNode)
 private
 public
  constructor create(Token:TToken;n,p,b:tnode);
  procedure AddToListing(list:TListing;Env:TEnvironnement);override;
 end;

 TIfNode=class(tnode)
 private
 public
  constructor create(Token:TToken;condition,thenblock,elseblock:tnode);
  procedure AddToListing(list:TListing;Env:TEnvironnement);override;
 end;

 TBlockNode=class(tnode)
 private
 public
  constructor create(Token:TToken);
  procedure AddToListing(list:TListing;Env:TEnvironnement);override;
 end;

 TForNode=class(tnode)
 private
 public
  constructor create(Token:TToken;v:tnode);
  procedure AddToListing(list:TListing;Env:TEnvironnement);override;
 end;

 TWhileNode=class(TNode)
 private
 public
  constructor create(Token:TToken;c,b:tnode);
  procedure AddToListing(list:TListing;Env:TEnvironnement);override;
 end;

 TGlobalNode=class(TNode)
 private
 public
  constructor create(Token:TToken;v,e:tnode);
  procedure AddToListing(list:TListing;Env:TEnvironnement);override;
 end;

 TReturnNode=class(TNode)
 private
 public
  constructor create(Token:TToken;e:tnode);
  procedure AddToListing(list:TListing;Env:TEnvironnement);override;
 end;


 TParamNode=class(TNode)
 private
  FRef:array of boolean;
 public
  procedure AddNode(n:tnode;ref:boolean);overload;
  procedure AddToListing(list:TListing;Env:TEnvironnement);override;
 end;



implementation




// TNode TNode TNode TNode TNode TNode TNode TNode TNode TNode TNode TNode TNode
//==============================================================================
constructor TNode.create(Token:TToken);
begin
 inherited create;
 FToken:=Token;
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
constructor TFunctionCallNode.create(Token:TToken;name:string;IsFunct:boolean);
begin
 inherited create(Token);
 FFunctionName:=name;
 FIsFunct:=IsFunct;
 if FIsFunct then Fname:='Appel Fonction "'+name+'"'
             else Fname:='Appel Procedure "'+name+'"'
end;

procedure TFunctionCallNode.AddToListing(list:TListing;Env:TEnvironnement);
var
 i:integer;
 indexfunction:integer;
begin
 // cherche parmis les fonctions externes
 indexfunction:=Env.IndexofFunctionCall(FFunctionName);
 if indexfunction<>-1 then
  begin
   if Env.GetFunctionArgCount(indexfunction)<>FCount then
     Raise Exception.CreateFmt('appel à ''%s'' avec un nombre incorrect de paramètres (%d,%d)',
                                      [FFunctionName,FToken.position.X,FToken.Position.Y]);

   // place les paramètres sur la pile
   for i:=FCount-1 downto 0 do
    begin
     if Env.FunctionCallRef[indexfunction,i] and
        not (FNodesList[i] is TVariableNode) then
         Raise Exception.CreateFmt('Un paramètre de type Ref doit être une variable (%d,%d)',
                                      [FToken.position.X,FToken.Position.Y]);
     FNodesList[i].AddToListing(list,Env);
    end;

   // appelle la procédure
   List.CurrentListing.add(TCallSubObject.create(FFunctionName));
   // on retire le résultat de la pile si c'est une procédure
   if not FIsFunct then List.CurrentListing.add(TDropObject.create);
   // on sauve les variables par paramètres (les valeurs sont sur la pile)
   for i:=0 to FCount-1 do
   if Env.FunctionCallRef[indexfunction,i] then
    begin
     List.CurrentListing.add(TPushVariableObject.create(TVariableNode(FNodesList[i]).VariableName));
     List.CurrentListing.add(TStoObject.create);
    end;
  end
 // cherche parmis les fonctions interne
 else
  begin
   indexfunction:=list.GetIndexOfSub(list.CurrentListingIndex,FFunctionName);
   if indexfunction=-1 then Raise Exception.CreateFmt('appel de fonction inconnue ''%s'' (%d,%d)',
                                      [FFunctionName,FToken.position.X,FToken.Position.Y]);
   if list.SubListing[indexfunction].CountRef<>FCount then
     Raise Exception.CreateFmt('appel à ''%s'' avec un nombre incorrect de paramètres (%d,%d)',
                                      [FFunctionName,FToken.position.X,FToken.Position.Y]);
   for i:=FCount-1 downto 0 do
    begin
     if list.SubListing[indexfunction].ListRef[i] and
        not (FNodesList[i] is TVariableNode) then
         Raise Exception.CreateFmt('Un paramètre de type Ref doit être une variable (%d,%d)',
                                      [FToken.position.X,FToken.Position.Y]);
     FNodesList[i].AddToListing(list,Env);
    end;

   // appelle la procédure
   List.CurrentListing.add(TCallSubObject.create(FFunctionName));
   // on retire le résultat de la pile si c'est une procédure
   if not FIsFunct then List.CurrentListing.add(TDropObject.create);
   // on sauve les variables par paramètres (les valeurs sont sur la pile)
   for i:=0 to FCount-1 do
   if list.SubListing[indexfunction].ListRef[i] then
    begin
     List.CurrentListing.add(TPushVariableObject.create(TVariableNode(FNodesList[i]).VariableName));
     List.CurrentListing.add(TStoObject.create);
    end;
  end;
end;


// TFunctionNode TFunctionNode TFunctionNode TFunctionNode TFunctionNode
//=======================================================================

constructor TFunctionNode.create(Token:TToken;n,p,b:tnode);
begin
 inherited create(Token);
 Fname:='function';
 Fcount:=3;
 setlength(FNodesList,3);
 FNodesList[0]:=n;
 FNodesList[1]:=p;
 FNodesList[2]:=b;
end;

procedure TFunctionNode.AddToListing(list:TListing;Env:TEnvironnement);
var
 i:integer;
 cur_list:integer;
begin
 cur_list:=list.CurrentListingIndex;
 // crée un nouveau listing
 list.AddListing(cur_list);
 list.CurrentListing.Name:=TVariableNode(FNodesList[0]).VariableName;

 // ajoute la récupération des paramètres depuis la pile
 list.CurrentListing.CountRef:=FNodesList[1].FCount;
 for i:=0 to FNodesList[1].NodeCount-1 do
  begin
   // par ref ou non ?
   list.CurrentListing.ListRef[i]:=TParamNode(FNodesList[1]).FRef[i];
   // on place le nom de la variable et l'instruction STO
   list.CurrentListing.add(TPushVariableObject.create(TVariableNode(FNodesList[1].NodesList[i]).VariableName));
   list.CurrentListing.add(TStoObject.create);
  end;
 // ajoute le listing correspondant (block)
 FNodesList[2].AddToListing(list,Env);
 // replace sur la pile les variables correspondants aux paramètres par Ref
 for i:=FNodesList[1].FCount-1 downto 0 do
  if TParamNode(FNodesList[1]).FRef[i] then
  begin
   list.CurrentListing.add(TPushVariableObject.create(TVariableNode(FNodesList[1].NodesList[i]).VariableName));
   list.CurrentListing.add(TEvalObject.create);
  end;
 // l'instruction de retour
 list.CurrentListing.add(TReturnObject.create);
 list.CurrentListingIndex:=cur_list;
end;


// TVariableNode TVariableNode TVariableNode TVariableNode TVariableNode
//=======================================================================

constructor TVariableNode.create(Token:TToken;varname:string);
begin
 inherited create(Token);
 FVariableName:=varname;
 Fname:='Variable "'+varname+'"';
end;

procedure TVariableNode.AddToListing(list:TListing;Env:TEnvironnement);
begin
 List.CurrentListing.add(TPushVariableObject.create(FVariableName));
end;

// TArrayNode TArrayNode TArrayNode TArrayNode TArrayNode TArrayNode TArrayNode
//==============================================================================

constructor TArrayNode.create(Token:TToken);
begin
 inherited create(Token);
 Fname:='Tableau';
end;

procedure TArrayNode.AddToListing(list:TListing;Env:TEnvironnement);
var
 i:integer;
begin
 for i:=FCount-1 downto 0 do FNodesList[i].AddToListing(list,Env);
 List.CurrentListing.add(TPushValueObject.create(TValue.create(Fcount)));
 List.CurrentListing.add(TArrayObject.create);
end;

// TValueNode TValueNode TValueNode TValueNode TValueNode TValueNode TValueNode
//==============================================================================

constructor TValueNode.create(Token:TToken;Value:TValue);
begin
 inherited create(Token);
 case Value.TypeValue of
 TypeNone   : FName:='None';
 TypeNumber : FName:='Number:'+ Value.StringValue;
 TypeString : FName:='String:'+ Value.StringValue;
 TypeBoolean: FName:='Boolean:'+ Value.StringValue;
 TypeArray  : FName:='Array:'+ Value.StringValue;
 end;
 FValue:=Value;
end;

procedure TValueNode.AddToListing(list:TListing;Env:TEnvironnement);
begin
  List.CurrentListing.add(TPushValueObject.create(TValue.create(FValue)));
end;

destructor TValueNode.Destroy;
begin
 FValue.free;
 inherited destroy;
end;

// TAssigneNode TAssigneNode TAssigneNode TAssigneNode TAssigneNode TAssigneNode
//==============================================================================
constructor TAssigneNode.create(Token:TToken;v,e:tnode);
begin
 inherited create(Token);
 Fname:='Assignation';
 Fcount:=2;
 setlength(FNodesList,2);
 FNodesList[0]:=v;
 FNodesList[1]:=e;
end;

procedure TAssigneNode.AddToListing(list:TListing;Env:TEnvironnement);
begin
 FNodesList[1].AddToListing(list,Env);
 FNodesList[0].AddToListing(list,Env); 
 List.CurrentListing.add(TStoObject.create);
end;

// TBinaryOpNode TBinaryOpNode TBinaryOpNode TBinaryOpNode TBinaryOpNode
//=======================================================================

constructor TBinaryOpNode.create(Token:TToken;opcode:string;n1,n2:tnode);
begin
 inherited create(Token);
 Fname:='Opperator '+opcode;
 FOpName:=opcode;
 Fcount:=2;
 setlength(FNodesList,2);
 FNodesList[0]:=n1;
 FNodesList[1]:=n2;
end;

procedure TBinaryOpNode.AddToListing(list:TListing;Env:TEnvironnement);
begin
 FNodesList[1].AddToListing(list,Env);
 FNodesList[0].AddToListing(list,Env);
 List.CurrentListing.add(TOperatorObject.create(FOpName));
end;

// TUnaryOpNode TUnaryOpNode TUnaryOpNode TUnaryOpNode TUnaryOpNode
//=======================================================================

constructor TUnaryOpNode.create(Token:TToken;opcode:string;n:tnode);
begin
 inherited create(Token);
 Fname:='Opperator '+opcode;
 FopName:=opcode;
 Fcount:=1;
 setlength(FNodesList,1);
 FNodesList[0]:=n;
end;

procedure TUnaryOpNode.AddToListing(list:TListing;Env:TEnvironnement);
begin
 FNodesList[0].AddToListing(list,Env);
 List.CurrentListing.add(TOperatorObject.create(FOpName));
end;


// TIfNode TIfNode TIfNode TIfNode TIfNode TIfNode TIfNode TIfNode TIfNode
//=========================================================================

constructor TIfNode.create(Token:TToken;condition,thenblock,elseblock:tnode);
begin
 inherited create(Token);
 Fname:='if';
 Fcount:=2;
 setlength(FNodesList,2);
 FNodesList[0]:=condition;
 FNodesList[1]:=thenblock;
 if elseblock<>nil then  addnode(elseblock);
end;

procedure TIfNode.AddToListing(list:TListing;Env:TEnvironnement);
var
 label1,label2,label3:string;
begin
 //ajoute la condition sur la pile
 FNodesList[0].AddToListing(list,Env);

 // definition des labels ('then label','else label' et 'end if label')
 label1:='@TL'+inttostr(Env.InterneNameCount);
 label2:='@EL'+inttostr(Env.InterneNameCount+1);
 label3:='@EIL'+inttostr(Env.InterneNameCount+2);
 if Fcount=2 then label2:=label3;
 inc(Env.InterneNameCount,3);

 // on evalue la condition et on saut à label 1 ou label2
 List.CurrentListing.add(TBoolJumpObject.create(label1));
 List.CurrentListing.add(TJumpObject.create(label2));
 List.CurrentListing.addLabel(label1,list.CurrentListing.Count);
 // on ajoute le THEN
 FNodesList[1].AddToListing(list,Env);
 // si un then, on le traite
 if Fcount=3 then
  begin
   List.CurrentListing.add(TJumpObject.create(label3));
   List.CurrentListing.addLabel(label2,list.CurrentListing.Count);
   FNodesList[2].AddToListing(list,Env);
  end;
 List.CurrentListing.addLabel(label3,list.CurrentListing.Count);
end;

// TBlockNode TBlockNode TBlockNode TBlockNode TBlockNode TBlockNode TBlockNode
//==============================================================================

constructor TBlockNode.create(Token:TToken);
begin
 inherited create(Token);
 Fname:='block';
end;

procedure TBlockNode.AddToListing(list:TListing;Env:TEnvironnement);
var
 i:integer;
begin
 for i:=0 to FCount-1 do FNodesList[i].AddToListing(list,Env);
end;


// TForNode TForNode TForNode TForNode TForNode TForNode TForNode TForNode
//=========================================================================

constructor TForNode.create(Token:TToken;v:tnode);
begin
 inherited create(Token);
 Fname:='for';
 Fcount:=1;
 setlength(FNodesList,1);
 FNodesList[0]:=v;
end;

procedure TForNode.AddToListing(list:TListing;Env:TEnvironnement);
var
 c,s,v,t,label1,label2,label3,label4:string;
begin
 v:=TVariableNode(FNodesList[0]).VariableName;

 // FOR.0.IN.1.DO.2.
 if Fcount=3 then
  begin
   t:='@TABLEAU_FOR'+inttostr(Env.InterneNameCount);    // tableau à parcourir
   s:='@TAILLE_FOR'+inttostr(Env.InterneNameCount+1);   // taille du tableau
   c:='@COMPTEUR_FOR'+inttostr(Env.InterneNameCount+2); // compteur pour le parcour du tableau
   label1:='@FL'+inttostr(Env.InterneNameCount+3);      // For Label
   label2:='@EFL'+inttostr(Env.InterneNameCount+4);     // Enf For Label
   inc(Env.InterneNameCount,5);
   list.CurrentListing.SetLabelBreak(label1,label2);

   // on place le tableau sur la pile puis on le sauvegarde dans T
   FNodesList[1].AddToListing(list,Env);
   List.CurrentListing.add(TPushVariableObject.create(t));
   List.CurrentListing.add(TStoObject.create);
   // on replace T sur la pile et on l'évalue 2 fois pour avoir la taille
   List.CurrentListing.add(TPushVariableObject.create(t));
   List.CurrentListing.add(TEvalObject.create);
   List.CurrentListing.add(TEvalObject.create);
   List.CurrentListing.add(TPushVariableObject.create(s));
   List.CurrentListing.add(TStoObject.create);
   // on initialise le compteur C
   List.CurrentListing.add(TPushValueObject.create(TValue.create(0)));
   List.CurrentListing.add(TPushVariableObject.create(c));
   List.CurrentListing.add(TStoObject.create);

   List.CurrentListing.addLabel(label1,list.CurrentListing.Count);
   //condition de la boucle (compteur<taille donc on arrete si compteur>=taille
   List.CurrentListing.add(TPushVariableObject.create(s));
   List.CurrentListing.add(TPushVariableObject.create(c));
   List.CurrentListing.add(TOperatorObject.create('>='));
   // si non Ok
   List.CurrentListing.add(TBoolJumpObject.create(label2));


   //extrait l'élement C du tableau
   List.CurrentListing.add(TPushVariableObject.create(t));
   List.CurrentListing.add(TPushVariableObject.create(c));
   List.CurrentListing.add(TExtractObject.create);
   //sauvegarde dans la variable V
   List.CurrentListing.add(TPushVariableObject.create(v));
   List.CurrentListing.add(TStoObject.create);

   FNodesList[2].AddToListing(list,Env);

   // on augmente le compteur
   List.CurrentListing.add(TPushVariableObject.create(c));
   List.CurrentListing.add(TPushValueObject.create(TValue.create(1)));
   List.CurrentListing.add(TOperatorObject.create('+'));
   List.CurrentListing.add(TPushVariableObject.create(c));
   List.CurrentListing.add(TStoObject.create);
   // on retourne à la boucle
   List.CurrentListing.add(TJumpObject.create(label1));
   List.CurrentListing.addLabel(label2,list.CurrentListing.Count);
   List.CurrentListing.DetLabelBreak;

  end
 // FOR.0.FROM.1.TO.2.DO.3.
 // FOR.0.FROM.1.TO.2.STEP.3.DO.4.
 else
 if (Fcount=4) or (Fcount=5) then
  begin
   s:='@STEP_FOR'+inttostr(Env.InterneNameCount);       // pas d'incrément/décrément
   c:='@COMPTEUR_FOR'+inttostr(Env.InterneNameCount+1); // compteur pour la boucle
   label1:='@FL'+inttostr(Env.InterneNameCount+2);      // For Label
   label2:='@EFL'+inttostr(Env.InterneNameCount+3);     // Enf For Label
   label3:='@SFL'+inttostr(Env.InterneNameCount+4);     // Suite For Label
   label4:='@CFL'+inttostr(Env.InterneNameCount+5);     // compare For Label
   inc(Env.InterneNameCount,6);
   list.CurrentListing.SetLabelBreak(label1,label2);
   // on évalue le pas
   if (Fcount=4) then
    begin
     List.CurrentListing.add(TPushValueObject.create(TValue.create(1)));
     List.CurrentListing.add(TPushVariableObject.create(s));
     List.CurrentListing.add(TStoObject.create);
    end
   else
    begin
     FNodesList[3].AddToListing(list,Env);
     List.CurrentListing.add(TPushVariableObject.create(s));
     List.CurrentListing.add(TStoObject.create);
     // si Pas=0, on arrête la boucle de suite
     List.CurrentListing.add(TPushValueObject.create(TValue.create(0)));
     List.CurrentListing.add(TPushVariableObject.create(s));
     List.CurrentListing.add(TOperatorObject.create('='));
     List.CurrentListing.add(TBoolJumpObject.create(label2));
    end;

   // on évalue le point de départ
   FNodesList[1].AddToListing(list,Env);
   List.CurrentListing.add(TPushVariableObject.create(c));
   List.CurrentListing.add(TStoObject.create);

   // on commence la boucle
   List.CurrentListing.addLabel(label1,list.CurrentListing.Count);
   // on évalue la fin

   // on test si on a fini la boucle (en fonction du pas)
   // SI S>0 ALORS (SI C>Fin ALORS fin SINON continue)
   //        SINON (SI C<Fin ALORS fin SINON continue)
   FNodesList[2].AddToListing(list,Env);                          //= fin
   List.CurrentListing.add(TPushVariableObject.create(c));        //= C
   List.CurrentListing.add(TPushValueObject.create(TValue.create(0)));
   List.CurrentListing.add(TPushVariableObject.create(s));
   List.CurrentListing.add(TOperatorObject.create('<'));
   List.CurrentListing.add(TBoolJumpObject.create(label4));
   List.CurrentListing.add(TOperatorObject.create('>'));
   List.CurrentListing.add(TBoolJumpObject.create(label2));
   List.CurrentListing.add(TJumpObject.create(label3));
   List.CurrentListing.addLabel(label4,list.CurrentListing.Count);
   List.CurrentListing.add(TOperatorObject.create('<'));
   List.CurrentListing.add(TBoolJumpObject.create(label2));
   List.CurrentListing.addLabel(label3,list.CurrentListing.Count);
   //coeur de la boucle
   // on affecte V
   List.CurrentListing.add(TPushVariableObject.create(c));
   List.CurrentListing.add(TPushVariableObject.create(v));
   List.CurrentListing.add(TStoObject.create);
   // on execute les instructions
   if (Fcount=4) then FNodesList[3].AddToListing(list,Env);
   if (Fcount=5) then FNodesList[4].AddToListing(list,Env);
   // on avance d'un pas et on recommence...
   List.CurrentListing.add(TPushVariableObject.create(c));
   List.CurrentListing.add(TPushVariableObject.create(s));
   List.CurrentListing.add(TOperatorObject.create('+'));
   List.CurrentListing.add(TPushVariableObject.create(c));
   List.CurrentListing.add(TStoObject.create);
   List.CurrentListing.add(TJumpObject.create(label1));
   // Pfff.. c'est la fin...
   List.CurrentListing.addLabel(label2,list.CurrentListing.Count);
  end;
end;


// TWhileNode TWhileNode TWhileNode TWhileNode TWhileNode TWhileNode TWhileNode
//==============================================================================

constructor TWhileNode.create(Token:TToken;c,b:tnode);
begin
 inherited create(Token);
 Fname:='while';
 Fcount:=2;
 setlength(FNodesList,2);
 FNodesList[0]:=c;
 FNodesList[1]:=b;
end;

procedure TWhileNode.AddToListing(list:TListing;Env:TEnvironnement);
var
 label1,label2:string;
begin
 // definition des labels
 label1:='@WHILELABEL'+inttostr(Env.InterneNameCount);
 label2:='@ENDWHILELABEL'+inttostr(Env.InterneNameCount+1);
 inc(Env.InterneNameCount,2);
 list.CurrentListing.SetLabelBreak(label1,label2);
 List.CurrentListing.addLabel(label1,list.CurrentListing.Count);
 //ajoute la condition sur la pile
 FNodesList[0].AddToListing(list,Env);
 List.CurrentListing.add(TOperatorObject.create('not'));
 // saut vers la fin si non Ok
 List.CurrentListing.add(TBoolJumpObject.create(label2));
 FNodesList[1].AddToListing(list,Env);
 List.CurrentListing.add(TJumpObject.create(label1));
 List.CurrentListing.addLabel(label2,list.CurrentListing.Count);
 List.CurrentListing.DetLabelBreak;
end;


// TGlobalNode TGlobalNode TGlobalNode TGlobalNode TGlobalNode TGlobalNode
//=========================================================================

constructor TGlobalNode.create(Token:TToken;v,e:tnode);
begin
 inherited create(Token);
 Fname:='Global';
 Fcount:=2;
 setlength(FNodesList,2);
 FNodesList[0]:=v;
 FNodesList[1]:=e;
end;

procedure TGlobalNode.AddToListing(list:TListing;Env:TEnvironnement);
begin
 FNodesList[1].AddToListing(list,Env);
 FNodesList[0].AddToListing(list,Env);
 list.CurrentListing.add(TGStoObject.create);
end;

// TReturnNode TReturnNode TReturnNode TReturnNode TReturnNode TReturnNode
//=========================================================================

constructor TReturnNode.create(Token:TToken;e:tnode);
begin
 inherited create(Token);
 Fname:='return';
 Fcount:=1;
 setlength(FNodesList,1);
 FNodesList[0]:=e;
end;

procedure TReturnNode.AddToListing(list:TListing;Env:TEnvironnement);
var
 i:integer;
begin
 FNodesList[0].AddToListing(list,Env);
 list.CurrentListing.add(TResultObject.create);
end;

// TParamNode TParamNode TParamNode TParamNode TParamNode TParamNode TParamNode
//==============================================================================

procedure TParamNode.AddNode(n:tnode;ref:boolean);
begin
 inherited AddNode(n);
 setlength(FRef,Fcount);
 FRef[Fcount-1]:=ref;
end;

procedure TParamNode.AddToListing(list:TListing;Env:TEnvironnement);
var
 i:integer;
begin
 for i:=FCount-1 downto 0 do FNodesList[i].AddToListing(list,Env);
end;

// TLabelNode TLabelNode TLabelNode TLabelNode TLabelNode TLabelNode TLabelNode
//==============================================================================

constructor TLabelNode.create(Token:TToken;LabelName:string);
begin
 inherited create(Token);
 FLabelName:=LabelName;
end;

procedure TLabelNode.AddToListing(list:TListing;Env:TEnvironnement);
begin
 list.CurrentListing.addLabel(FLabelName,list.CurrentListing.Count);
end;

// TGotoNode TGotoNode TGotoNode TGotoNode TGotoNode TGotoNode TGotoNode
//=======================================================================

constructor TGotoNode.create(Token:TToken;LabelName:string);
begin
 inherited create(Token);
 FLabelName:=LabelName;
end;

procedure TGotoNode.AddToListing(list:TListing;Env:TEnvironnement);
begin
 list.CurrentListing.add(TJumpObject.create(FLabelName));
end;

// TStopLoopNode TStopLoopNode TStopLoopNode TStopLoopNode TStopLoopNode
//=======================================================================

constructor TStopLoopNode.create(Token:TToken;StopType:TStopLoop);
begin
 inherited create(Token);
 FStopType:=StopType;
end;

procedure TStopLoopNode.AddToListing(list:TListing;Env:TEnvironnement);
var
 lab:string;
begin
 case FStopType of
 NEXT_LOOP:lab:=List.CurrentListing.LabelContinue;
 STOP_LOOP:lab:=List.CurrentListing.LabelBreak;
 end;
 if lab='' then Raise Exception.CreateFmt('instuction BREAK ou NEXT en dehors d''une boucle FOR ou WHILE (%d,%d)',
                                      [FToken.position.X,FToken.Position.Y]);
 list.CurrentListing.add(TJumpObject.create(lab));
end;

end.
