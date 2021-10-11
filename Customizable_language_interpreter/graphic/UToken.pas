// transforme la chaine de caractère en token (élément simple du language)

unit UToken;

interface

uses types,SysUtils;

type
 TTokenType=(
 COMMENTAIRE,  FIN_COMMANDE, VIDE, VARIABLE,
 OP_PLUS, OP_MOINS, OP_MULTIPLIE, OP_DIVISE, OP_MODULO, OP_PUISSANCE,OP_CONCATENATION,
 PARGAUCHE, PARDROITE, CROCHETGAUCHE, CROCHETDROITE,
 ACCOLADEGAUCHE, ACCOLADEDROITE,
 VIRGULE, OP_NON, OP_ET, OP_OU,
 OP_INFERIEUR, OP_INFERIEUREGAL, OP_EGAL, OP_SUPERIEUREGAL, OP_SUPERIEUR, OP_DIFFERENT,
 NOMBRE, CHAINE, VRAI, FAUX,
 INSTR_IF, INSTR_ELSE, INSTR_WHILE,
 INSTR_FOR, INSTR_IN, INSTR_FROM, INSTR_TO,INSTR_DO,
 INSTR_FUNCTION, INSTR_RETURN,INSTR_GLOBAL,INSTR_REFERENCE);

const
 CTokenTypeStr:array[TTokenType] of string=(
 'COMMENTAIRE', 'FIN_COMMANDE', 'VIDE', 'VARIABLE',
 'OP_PLUS', 'OP_MOINS', 'OP_MULTIPLIE', 'OP_DIVISE', 'OP_MODULO', 'OP_PUISSANCE', 'OP_CONCATENATION',
 'PARGAUCHE', 'PARDROITE', 'CROCHETGAUCHE', 'CROCHETDROITE',
 'ACCOLADEGAUCHE', 'ACCOLADEDROITE',
 'VIRGULE', 'OP_NON', 'OP_ET', 'OP_OU',
 'OP_INFERIEUR', 'OP_INFERIEUREGAL', 'OP_EGAL', 'OP_SUPERIEUREGAL', 'OP_SUPERIEUR', 'OP_DIFFERENT',
 'NOMBRE', 'CHAINE', 'VRAI', 'FAUX',
 'INSTR_IF', 'INSTR_ELSE', 'INSTR_WHILE',
 'INSTR_FOR', 'INSTR_IN', 'INSTR_FROM', 'INSTR_TO','INSTR_DO',
 'INSTR_FUNCTION', 'INSTR_RETURN', 'INSTR_GLOBAL', 'INSTR_REFERENCE');


type
 TToken=Class
  private
  Fposition:tpoint;
  FTokenType:TTokenType;
  FValue:String;
  public
  property value:string read FValue;
  property position:tpoint read Fposition;
  property TokenType:TTokenType read FTokenType;
  constructor create(pos:tpoint;TokenType:TTokenType;str:string);
  end;

Type
 TTokenList=class
 private
  FCount:integer;
  FSource:string;
  FIndexInSource:integer;
  FpositionInSource:tpoint;
  FBufferChar:array[1..2] of char;
  FList:array of TToken;
  function GetToken(index:integer):TToken;
  function GetLastToken:TToken;

  procedure LitLigneCommentaire;
  procedure LitBlocCommentaire;
  procedure LitChaine;
  procedure LitNombre;
  procedure LitIdentifieur;
  function litChar:char;
  procedure AddToken(TokenType:TTokenType;str:string);
  procedure RaiseExceptionChar(n:integer);
 public
  property token[index:integer]:TToken read GetToken;default;
  property LastToken:TToken read GetLastToken;
  property Count:integer read FCount;
  constructor create;
  destructor free;
  procedure add(pos:tpoint;TokenType:TTokenType;str:string);
  function IsEmpty:boolean;
  procedure Tokenize(codesource:string);
 end;

implementation

//TToken TToken TToken TToken TToken TToken TToken TToken
//========================================================
constructor TToken.create(pos:tpoint;TokenType:TTokenType;str:string);
begin
 Fposition:=pos;
 FTokenType:=TokenType;
 FValue:=str;
end;

//TTokenList TTokenList TTokenList TTokenList TTokenList
//=======================================================

constructor TTokenList.Create;
begin
 setlength(FList,0);
 FCount:=0;
end;

destructor TTokenList.free;
var
 i:integer;
begin
 for i:=0 to Fcount-1 do FList[i].Free;
 FCount:=0;
 setlength(FList,0);
end;

procedure TTokenList.add(pos:tpoint;TokenType:TTokenType;str:string);
begin
 inc(FCount);
 setlength(FList,FCount);
 FList[FCount-1]:=TToken.create(pos,TokenType,str);
end;

function TTokenList.IsEmpty:boolean;
begin
 result:=FCount=0;
end;


function TTokenList.GetToken(index:integer):TToken;
begin
 if (index<0) or (index>=FCount) then result:=TToken.create(point(-1,-1),VIDE,'')
                                 else result:=FList[index];
end;

function TTokenList.GetLastToken:TToken;
begin
 if FCount=0 then result:=nil
 else result:=FList[FCount-1];
end;


function TTokenList.litChar:char;
begin
 FBufferChar[1]:=FBufferChar[2];
 inc(FIndexInSource);
 if FIndexInSource+1>length(FSource) then FBufferChar[2]:=#0 else FBufferChar[2]:=FSource[FIndexInSource+1];
 result:=FBufferChar[1];
 if result=#10 then FpositionInSource:=point(0,FpositionInSource.Y+1)
               else inc(FpositionInSource.x);
end;

procedure TTokenList.AddToken(TokenType:TTokenType;str:string);
var
 i:integer;
begin
 add(FpositionInSource,TokenType,str);
 for i:=1 to length(str) do litChar;
end;

procedure TTokenList.RaiseExceptionChar(n:integer);
begin
 Raise Exception.CreateFmt('Caractère ''%s'' invalide (%d,%d)', [FBufferChar[n],FpositionInSource.X+n-1,FpositionInSource.Y]);
end;

procedure TTokenList.Tokenize(codesource:string);
var
 c,d:word;
 s:string;
begin
 FSource:=codesource;
 if length(FSource)<1 then FBufferChar[1]:=#0 else FBufferChar[1]:=FSource[1];
 if length(FSource)<2 then FBufferChar[2]:=#0 else FBufferChar[2]:=FSource[2];
 FIndexInSource:=1;
 FpositionInSource:=point(1,1);

 while FBufferChar[1]<>#0 do
  begin
   // caractères blancs, on continue
   if FBufferChar[1] in [#32,#9,#13,#10] then begin LitChar;continue; end;

   // sinon, on cherche le token correspondant
   case FBufferChar[1] of
    ';': AddToken(FIN_COMMANDE,';');
    '+': AddToken(OP_PLUS,'+');
    '-': AddToken(OP_MOINS,'-');
    '*': AddToken(OP_MULTIPLIE,'*');
    '%': AddToken(OP_MODULO,'%');
    '^': AddToken(OP_PUISSANCE,'^');
    ',': AddToken(VIRGULE,',');
    '~': AddToken(OP_CONCATENATION,'~');
    '(': AddToken(PARGAUCHE,'(');
    ')': AddToken(PARDROITE,')');
    '{': AddToken(ACCOLADEGAUCHE,'{');
    '}': AddToken(ACCOLADEDROITE,'}');
    '[': AddToken(CROCHETGAUCHE,'[');
    ']': AddToken(CROCHETDROITE,']');
    '|': if FBufferChar[2] = '|' then AddToken(OP_OU,'||')
                                 else RaiseExceptionChar(2);
    '&': if FBufferChar[2] = '&' then AddToken(OP_ET,'&&')
                                 else RaiseExceptionChar(2);
    '/':case FBufferChar[2] of
       '/':LitLigneCommentaire;
       '*':LitBlocCommentaire;
       else AddToken(OP_DIVISE, '/');
       end;
    '=': AddToken(OP_EGAL,'=');
    '!': if FBufferChar[2] = '=' then AddToken(OP_DIFFERENT,'!=')
                                 else AddToken(OP_NON,'!');
    '<': if FBufferChar[2] = '=' then AddToken(OP_INFERIEUREGAL,'<=')
                                 else AddToken(OP_INFERIEUR,'<');
    '>': if FBufferChar[2] = '=' then AddToken(OP_SUPERIEUREGAL,'>=')
                                 else AddToken(OP_SUPERIEUR,'>');
    '''','"':LitChaine;
    '0'..'9':LitNombre;
    'a'..'z','A'..'Z','_':LitIdentifieur;
   else
    RaiseExceptionChar(1);
   end;
 end;
end;

procedure TTokenList.LitLigneCommentaire;
var
 tpos:tpoint;
 s:string;
begin
 tpos:=FpositionInSource;
 while not (FBufferChar[1] in [#13,#10,#0]) do
  begin
   s:=s+FBufferChar[1];
   LitChar;
  end;
 add(tpos,COMMENTAIRE,s);
end;

procedure TTokenList.LitBlocCommentaire;
var
 tpos:tpoint;
 s:string;
begin
 tpos:=FpositionInSource;
 while true do
  begin
   if FBufferChar[1]=#0 then
    Raise Exception.CreateFmt('*/ attendu mais la fin du fichier a été trouvé (%d,%d)', [FpositionInSource.X,FpositionInSource.Y]);
   if FBufferChar[1]+FBufferChar[2]='*/' then break;
   s:=s+FBufferChar[1];
   LitChar;
  end;
 LitChar;LitChar;s:=s+'*/';
 add(tpos,COMMENTAIRE,s);
end;

procedure TTokenList.LitChaine;
var
 tpos:tpoint;
 s:string;
 c:char;
begin
 tpos:=FpositionInSource;
 c:=FBufferChar[1]; LitChar;
 while true do
  begin
   if FBufferChar[1]=#0 then
    Raise Exception.CreateFmt('" attendu mais la fin du fichier a été trouvé (%d,%d)', [FpositionInSource.X,FpositionInSource.Y]);
   if FBufferChar[1]=c then break;
   if FBufferChar[1]='/' then LitChar;
   s:=s+FBufferChar[1];
   LitChar;
  end;
 LitChar;
 add(tpos,CHAINE,s);
end;

procedure TTokenList.LitNombre;
var
 tpos:tpoint;
 s:string;
 drapeau:boolean;
begin
 tpos:=FpositionInSource;
 drapeau:=false;
 while FBufferChar[1] in ['0'..'9','.'] do
  begin
   s:=s+FBufferChar[1];
   LitChar;
   if FBufferChar[1]='.' then
    if not drapeau then drapeau:=true
   else Raise Exception.CreateFmt('un chiffre est attendu mais un point a été trouvé (%d,%d)', [FpositionInSource.X,FpositionInSource.Y]);
  end;
 add(tpos,NOMBRE,s);
end;

procedure TTokenList.LitIdentifieur;
var
 tpos:tpoint;
 s,l:string;
begin
 tpos:=FpositionInSource;
 while FBufferChar[1] in ['a'..'z','A'..'Z','_','0'..'9'] do
  begin
   s:=s+FBufferChar[1];
   LitChar;
  end;
 l:=ansilowercase(s);
      if l='true' then add(tpos,VRAI,l)
 else if l='false' then add(tpos,FAUX,l)
 else if l='if' then add(tpos,INSTR_IF,l)
 else if l='else' then add(tpos,INSTR_ELSE,l)
 else if l='while' then add(tpos,INSTR_WHILE,l)
 else if l='in' then add(tpos,INSTR_IN,l)
 else if l='from' then add(tpos,INSTR_FROM,l)
 else if l='to' then add(tpos,INSTR_TO,l)
 else if l='do' then add(tpos,INSTR_DO,l)
 else if l='for' then add(tpos,INSTR_FOR,l)
 else if l='function' then add(tpos,INSTR_FUNCTION,l)
 else if l='return' then add(tpos,INSTR_RETURN,l)
 else if l='global' then add(tpos,INSTR_GLOBAL,l)
 else if l='ref' then add(tpos,INSTR_REFERENCE,l)
 else add(tpos,VARIABLE,l);

end;



end.
