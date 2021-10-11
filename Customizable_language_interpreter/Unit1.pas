unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls,
  UInterpreteur,UToken,UNodes,UValues;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Label1: TLabel;
    Label4: TLabel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TreeView1: TTreeView;
    Memo2: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    { Déclarations privées }
    procedure AddToTree(parentnode:TTreeNode;n:TNode);
  public
    { Déclarations publiques }
    function FunctionCall(sender:tobject;FunctionName:string;index:integer;ArgsCount:integer):TValue;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses UEnvironnement;

// REMARQUES :
// - les paramètres sont sur la pile de l'environnement
//   le premier est en haut de la pile
// - A la fin, si une valeur doit être retourner, la mettre dans Env.result
// - Tous les paramètres doivent rester sur la pile pour être ensuite traiter
//   pour les paramètres référencés
// - le numéro d'index correspond à l'ordre lors de l'ajout par t.Environnement.AddFunctionCall
// 
function TForm1.FunctionCall(sender:tobject;FunctionName:string;index:integer;ArgsCount:integer):TValue;
var
 Env:TEnvironnement;
 arg:tvalue;
begin
 Env:=TEnvironnement(sender);
 case index of
 0:{ println }
  begin
   arg:=Env.Stack.read(0);
   memo2.Lines.add(arg.StringValue);
  end;
 1:{ len }
  begin
   arg:=Env.Stack.Pop;
   Env.Result.Free;
   Env.Result:=TValue.create(length(Arg.StringValue));
   arg.free;
  end;
 2:{ inc }
  begin
   arg:=Env.Stack.read(0);
   arg.NumberValue:=Arg.NumberValue+1;
  end;
 end;
end;

procedure TForm1.AddToTree(parentnode:TTreeNode;n:TNode);
var
 i:integer;
 t:TTreeNode;
begin
 if n=nil then exit;
 t:=treeview1.Items.AddChild(parentnode,n.ClassName+'('+n.Name+')');
 for i:=0 to n.NodeCount-1 do AddToTree (t,n.NodesList[i]);
end;

procedure TForm1.Button1Click(Sender: TObject);
var
 t:TInterpreteur;
 i:integer;
 n:TTreeNode;
begin
 memo2.Clear;
 try
 try
 t:=TInterpreteur.create;
 t.Environnement.AddFunctionCall('println',[false]);
 t.Environnement.AddFunctionCall('len',[false]);
 t.Environnement.AddFunctionCall('inc',[true]);
 t.OnFunctionCall:=FunctionCall;

 t.CodeSource:=memo1.Text;
 treeview1.Items.Clear;
 for i:=0 to t.Parser.RootCount-1 do
  begin
   AddToTree(nil,t.Parser[i]);
  end;
 t.Run;

 except
   on E: Exception do label4.Caption:=E.Message;
 end;
 finally
 t.free;
 end;
end;

end.

