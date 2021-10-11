object Form1: TForm1
  Left = 217
  Top = 125
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Customizable language interpreter'
  ClientHeight = 580
  ClientWidth = 1027
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 120
  TextHeight = 16
  object Label1: TLabel
    Left = 8
    Top = 24
    Width = 82
    Height = 16
    Caption = 'Code Source:'
  end
  object Label4: TLabel
    Left = 496
    Top = 16
    Width = 521
    Height = 25
    AutoSize = False
    Caption = 'Message de l'#39'interpreteur...'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clMaroon
    Font.Height = -18
    Font.Name = 'Times New Roman'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Button1: TButton
    Left = 160
    Top = 16
    Width = 329
    Height = 25
    Caption = 'Start'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 8
    Top = 56
    Width = 481
    Height = 513
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -18
    Font.Name = 'Courier New'
    Font.Style = []
    HideSelection = False
    Lines.Strings = (
      'global pi=3.14159;'
      'global red=12;'
      'global yellow=14;'
      'global blue=9;'
      'global green=10;'
      'global black=0;'
      'global white=15;'
      ''
      '// on efface l'#39'ecran'
      'color(black,white);'
      'clear();'
      ''
      '// on ecrit un petit texte'
      'text([20,240],'#39'Power Flower'#39');'
      ''
      '// on dessine une fleur rouge et jaune'
      'color(0,red);'
      'for i from 0 to 20 do'
      '{'
      '   circle([cos(PI*i/10)*80+100,'
      '           sin(PI*i/10)*80+100],10);'
      '}'
      'color(0,yellow);'
      'circle([100,100],75);'
      ''
      '// et la tige...'
      'color(0,green);'
      'for i from 0 to 20 do'
      '{'
      '   n=(i-10)/10;'
      '   square([200-30*n^2,150+i*10],10,n*5); '
      '}')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 1
    WantTabs = True
  end
  object PageControl1: TPageControl
    Left = 496
    Top = 56
    Width = 521
    Height = 516
    ActivePage = TabSheet2
    TabOrder = 2
    object TabSheet1: TTabSheet
      Caption = 'L'#39'arbre syntaxique'
      object TreeView1: TTreeView
        Left = 10
        Top = 10
        Width = 495
        Height = 463
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -18
        Font.Name = 'Courier New'
        Font.Style = []
        Indent = 19
        ParentFont = False
        TabOrder = 0
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Ecran'
      ImageIndex = 1
      object Image1: TImage
        Left = 10
        Top = 10
        Width = 495
        Height = 463
      end
    end
  end
end
