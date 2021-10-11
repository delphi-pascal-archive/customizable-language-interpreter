unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls,Math,
  UInterpreteur,UToken,UNodes,UValues, ExtCtrls;

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
    Image1: TImage;
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
 p:tvalue;
 pt:array[0..3] of tpoint;
 x,y,r,a,c1,c2:extended;
 s:string;
const
 MyColor:array[0..15] of tcolor=($000000,$800000,$008000,$808000,$000080,$800080,$008080,$c0c0c0,
                                 $808080,$FF0000,$00FF00,$FFFF00,$0000FF,$FF00FF,$00FFFF,$FFFFFF);
begin
 Env:=TEnvironnement(sender);
 case index of
 0://clear()
  begin
   image1.Canvas.FillRect(image1.ClientRect);
  end;
 1://circle([x,y],rayon)
  begin
   p:=Env.Stack.read(0);
   x:=p.ArrayValue[0].NumberValue;
   y:=p.ArrayValue[1].NumberValue;
   r:=Env.Stack.read(1).NumberValue;
   image1.Canvas.Ellipse(round(x-r),round(y-r),round(x+r),round(y+r));
  end;
 2://square([x,y],rayon,angle)
  begin
   p:=Env.Stack.read(0);
   x:=p.ArrayValue[0].NumberValue;
   y:=p.ArrayValue[1].NumberValue;
   r:=Env.Stack.read(1).NumberValue;
   a:=Env.Stack.read(2).NumberValue+Pi/4;
   pt[0]:=point(round(x+cos(a)*r),round(y+sin(a)*r));
   pt[1]:=point(round(x-sin(a)*r),round(y+cos(a)*r));
   pt[2]:=point(round(x-cos(a)*r),round(y-sin(a)*r));
   pt[3]:=point(round(x+sin(a)*r),round(y-cos(a)*r));
   image1.Canvas.Polygon(pt);

  end;
 3://color(pen,brush)
  begin
   c1:=Env.Stack.read(0).NumberValue;
   c2:=Env.Stack.read(1).NumberValue;
   image1.Canvas.Pen.color:=MyColor[round(c1)];
   image1.Canvas.Font.Color:=MyColor[round(c1)];
   image1.Canvas.Brush.color:=MyColor[round(c2)];
  end;
 4://text([x,y],string)
  begin
   p:=Env.Stack.read(0);
   x:=p.ArrayValue[0].NumberValue;
   y:=p.ArrayValue[1].NumberValue;
   s:=Env.Stack.read(1).StringValue;
   image1.Canvas.TextOut(round(x),round(y),s);
  end;
 5://cos(x)
  begin
   x:=Env.Stack.read(0).NumberValue;
   Env.Result.NumberValue:=cos(x);
  end;
 6://sin(x)
  begin
   x:=Env.Stack.read(0).NumberValue;
   Env.Result.NumberValue:=sin(x);
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
 image1.Canvas.Font.Size:=15;
 image1.Canvas.Font.Name:='Courier New';
 label4.Caption:='';
 try
 try
 t:=TInterpreteur.create;
 t.Environnement.AddFunctionCall('clear',[]);
 t.Environnement.AddFunctionCall('circle',[false,false]);
 t.Environnement.AddFunctionCall('square',[false,false,false]);
 t.Environnement.AddFunctionCall('color',[false,false]);
 t.Environnement.AddFunctionCall('text',[false,false]);
 t.Environnement.AddFunctionCall('sin',[false]);
 t.Environnement.AddFunctionCall('cos',[false]);
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

