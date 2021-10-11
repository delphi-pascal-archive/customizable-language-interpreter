unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls,UStack,
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
    TabSheet3: TTabSheet;
    Memo3: TMemo;
    Label2: TLabel;
    Button2: TButton;
    Memo4: TMemo;
    Button3: TButton;
    Label3: TLabel;
    Memo5: TMemo;
    Memo6: TMemo;
    Memo7: TMemo;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure TreeView1Change(Sender: TObject; Node: TTreeNode);
    procedure Memo1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Memo1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Déclarations privées }
    t:TInterpreteur;
    procedure AddToTree(parentnode:TTreeNode;n:TNode);
  public
    { Déclarations publiques }
    procedure FunctionCall(sender:tobject;FunctionName:string;index:integer;var Args:Array Of TValue;var res:Tvalue);
  end;

var
  Form1: TForm1;


implementation

{$R *.dfm}

uses UEnvironnement;

// REMARQUES :
// - les paramètres sont dans Args
// - A la fin, si une valeur doit être retourner, la mettre dans Res
// - Tous les paramètres doivent rester sur la pile pour être ensuite traiter
//   pour les paramètres référencés
// - le numéro d'index correspond à l'ordre lors de l'ajout par t.Environnement.AddFunctionCall
//
procedure TForm1.FunctionCall(sender:tobject;FunctionName:string;index:integer;var Args:Array of TValue;var res:Tvalue);
var
 Env:TEnvironnement;
 arg:TStackObject;
 val:tvalue;
begin
 Env:=TEnvironnement(sender);
 case index of
 0:{ println }
  begin
   memo2.Lines.Add(args[0].StringValue);
  end;
 1:{ len }
  begin
   res.NumberValue:=length(args[0].StringValue);
  end;
 2:{ inc }
  begin
   args[0].NumberValue:=args[0].NumberValue+1;
  end;
 end;
end;


type
 TTokenInfo=record pos:tpoint;size:integer;s:string;end;
 PTokenInfo=^TTokenInfo;

procedure TForm1.AddToTree(parentnode:TTreeNode;n:TNode);
var
 i:integer;
 t:TTreeNode;
 p:PTokenInfo;
begin
 if n=nil then exit;
 t:=treeview1.Items.AddChild(parentnode,n.ClassName+'('+n.Name+')');
 new(p);
 p.pos:=n.Token.position;
 p.s:=n.Token.value;
 t.Data:=p;
 for i:=0 to n.NodeCount-1 do AddToTree (t,n.NodesList[i]);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
 try
 t.Run(-1);
  except
   on E: Exception do label4.Caption:=E.Message;
 end;
end;

procedure TForm1.TreeView1Change(Sender: TObject; Node: TTreeNode);
var
 t:PTokenInfo;
begin
 t:=PTokenInfo(TreeView1.Selected.data);
 memo1.CaretPos:=point(t.pos.X-1,t.pos.Y-1);
 memo1.SelLength:=1;
end;

procedure TForm1.Memo1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 label2.Caption:='Position du curseur :'+inttostr(Memo1.CaretPos.X+1)+','+inttostr(Memo1.CaretPos.Y+1);
end;

procedure TForm1.Memo1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 label2.Caption:='Position du curseur :'+inttostr(Memo1.CaretPos.X+1)+','+inttostr(Memo1.CaretPos.Y+1);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
 t:=TInterpreteur.create;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
 p:tpoint;
begin
 try
  t.Run(1);
  p.x:=0;
  p.Y:=t.Listing.EvalPos;
  memo3.CaretPos:=p;
  memo3.SelLength:=length(memo3.Lines[p.y]);
  memo4.Text:=t.Environnement.Stack.getString;
  memo5.Text:=t.Environnement.Variables.getString;
  memo6.Text:=T.Environnement.FunctionsCall.GetString;
  memo7.Text:=T.Environnement.Globals.GetString;
 except
   on E: Exception do label4.Caption:=E.Message;
 end;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
 i:integer;
begin
 memo2.Clear;
 treeview1.Items.Clear;
 try
 t.clear;
 t.Environnement.AddFunctionCall('println',[false]);
 t.Environnement.AddFunctionCall('len',[false]);
 t.Environnement.AddFunctionCall('inc',[true]);
 t.OnFunctionCall:=FunctionCall;
 t.CodeSource:=memo1.Text;
 Memo3.Lines.Text:=T.Listing.GetString;
 LAbel3.Caption:=inttostr(Memo3.Lines.Count)+' lignes';
 label4.Caption:='Pas d''erreur...';
 for i:=0 to t.Parser.RootCount-1 do AddToTree(nil,t.Parser[i]);
 except
   on E: Exception do label4.Caption:=E.Message;
 end;

end;

end.

