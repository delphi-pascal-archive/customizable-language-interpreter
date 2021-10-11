unit UInterpreteur;

interface

uses types, Utoken,SysUtils,UParser,UEnvironnement,UValues;

Type
TInterpreteur = Class
  private
   FSource:string;
   FTokenList:TTokenList;
   FParser:TParser;
   FEnvironnement:TEnvironnement;
   procedure SetSource(s:string);
   function GetSource:string;
   procedure SetFunctionCall(f:TFunctionCallEvent);
   function GetFunctionCall:TFunctionCallEvent;
  public
   constructor create;
   destructor free;
   function Run:tvalue;
   property TokenList:TTokenList read FTokenList;
   property Parser:TParser read FParser;
   property Environnement:TEnvironnement read FEnvironnement;
   property CodeSource:string read getsource write setsource;
   property OnFunctionCall:TFunctionCallEvent read GetFunctionCall write SetFunctionCall;
  end;

implementation


constructor TInterpreteur.create;
begin
 FTokenList:=TTokenList.create;
 FEnvironnement:=TEnvironnement.create;
 FParser:=TParser.create(FEnvironnement);
end;

destructor TInterpreteur.free;
begin
 FTokenList.free;
 FEnvironnement.free;
end;

procedure TInterpreteur.SetSource(s:string);
begin
 FSource:=s;
 FTokenList.Tokenize(s);
 FParser.Parse(FTokenList);
end;

function TInterpreteur.GetSource:string;
begin
 result:=FSource;
end;

procedure TInterpreteur.SetFunctionCall(f:TFunctionCallEvent);
begin
 FEnvironnement.OnFunctionCall:=f;
end;

function TInterpreteur.GetFunctionCall:TFunctionCallEvent;
begin
 result:=FEnvironnement.OnFunctionCall;
end;

function TInterpreteur.Run:tvalue;
begin
 result:=FParser.eval;
end;


end.
