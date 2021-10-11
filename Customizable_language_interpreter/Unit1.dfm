object Form1: TForm1
  Left = 229
  Top = 130
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Interpreteur'
  ClientHeight = 580
  ClientWidth = 1114
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
    Left = 552
    Top = 16
    Width = 553
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
    Width = 385
    Height = 25
    Caption = 'Start'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 8
    Top = 56
    Width = 537
    Height = 516
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -18
    Font.Name = 'Courier New'
    Font.Style = []
    HideSelection = False
    Lines.Strings = (
      '// definition de variables'
      '//========================'
      'str="2*5//4+(-5%2)*5";'
      'num=2*5/4+(-5%2)*5;'
      ''
      '// affichage'
      '//========================'
      'println("str="~str);'
      'println('#39'num='#39'~num);'
      'println("longueur de str="~len(str));'
      ''
      '// parametres par reference'
      '//========================='
      'function dec(ref n) n=n-1;'
      'a=1;'
      'println('#39'a='#39'~a);'
      'println('#39'Fonction Interne DEC'#39');'
      'dec(a);'
      'println('#39'dec(a)='#39'~a);'
      'println('#39'Fonction Externe INC'#39');'
      'inc(a);'
      'println('#39'inc(a)='#39'~a);'
      ''
      '// calcul de factorielles'
      '//========================'
      'function factorielle(n)'
      '{'
      #9'if (n<1) return 1;'
      #9'else return factorielle(n-1)*n;'
      '}'
      ''
      'println("");'
      'for i from 1 to 10 do'
      '{'
      #9'f=factorielle(i);'
      #9'println(i~"!="~f);'
      '}'
      ''
      '// tableau'
      '//========================'
      ''
      'println("");'
      'tableau=["la","1","delphifr.com",'
      '1,2,'
      '[777],[888,999],'
      'false,true];'
      ''
      'for j in tableau do'
      '{'
      #9'if (j=1) println(j~" est egal a 1");'
      #9'else println(j~" est different de 1");'
      '}')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 1
    WantTabs = True
  end
  object PageControl1: TPageControl
    Left = 552
    Top = 56
    Width = 553
    Height = 516
    ActivePage = TabSheet1
    TabOrder = 2
    object TabSheet1: TTabSheet
      Caption = 'L'#39'arbre syntaxique'
      object TreeView1: TTreeView
        Left = 8
        Top = 10
        Width = 529
        Height = 464
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
      Caption = 'Messages de sortie'
      ImageIndex = 1
      object Memo2: TMemo
        Left = 10
        Top = 10
        Width = 818
        Height = 464
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
  end
end
