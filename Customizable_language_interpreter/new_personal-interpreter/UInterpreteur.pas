unit UInterpreteur;

interface

uses types, Utoken,SysUtils,UParser,UEnvironnement,UValues,UListing;

Type
TInterpreteur = Class
  private
   FSource:string;
   FTokenList:TTokenList;
   FParser:TParser;
   FEnvironnement:TEnvironnement;
   FListing:TListing;
   procedure SetSource(s:string);
   function GetSource:string;
   procedure SetFunctionCall(f:TFunctionCallEvent);
   function GetFunctionCall:TFunctionCallEvent;
  public
   constructor create;
   destructor Destroy; override;
   function Run(n:integer):tvalue;
   procedure Clear;
   property TokenList:TTokenList read FTokenList;
   property Parser:TParser read FParser;
   property Listing:TListing read FListing;
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
 FListing:=TListing.create;
end;

destructor TInterpreteur.Destroy;
begin
 FTokenList.free;
 FEnvironnement.free;
 FListing.Free;
 FParser.Free;
 inherited Destroy;
end;

procedure TInterpreteur.SetSource(s:string);
begin
 FSource:=s;
 FTokenList.Tokenize(s);
 FParser.Parse(FTokenList,FListing,FEnvironnement);
 FListing.CurrentListingIndex:=-1;
 FSource:=FListing.GetString;
end;

function TInterpreteur.GetSource:string;
begin
 result:=FListing.GetString;
end;

procedure TInterpreteur.SetFunctionCall(f:TFunctionCallEvent);
begin
 FEnvironnement.OnFunctionCall:=f;
end;

function TInterpreteur.GetFunctionCall:TFunctionCallEvent;
begin
 result:=FEnvironnement.OnFunctionCall;
end;

function TInterpreteur.Run(n:integer):tvalue;
begin
 FListing.eval(n,FEnvironnement);
end;

procedure TInterpreteur.Clear;
begin
 FTokenList.free;
 FEnvironnement.free;
 FListing.Free;
 FParser.Free;
 
 FTokenList:=TTokenList.create;
 FEnvironnement:=TEnvironnement.create;
 FParser:=TParser.create(FEnvironnement);
 FListing:=TListing.create;
end;


end.
