object Form1: TForm1
  Left = 263
  Top = 114
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Interpreteur'
  ClientHeight = 491
  ClientWidth = 1099
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 32
    Width = 68
    Height = 13
    Caption = 'Code Source :'
  end
  object Label4: TLabel
    Left = 400
    Top = 8
    Width = 625
    Height = 25
    AutoSize = False
    Caption = 'Message de l'#39'interpreteur...'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clMaroon
    Font.Height = -15
    Font.Name = 'Times New Roman'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 8
    Top = 472
    Width = 96
    Height = 13
    Caption = 'Position du curseur :'
  end
  object Button1: TButton
    Left = 200
    Top = 8
    Width = 105
    Height = 33
    Caption = 'Go'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 8
    Top = 50
    Width = 385
    Height = 415
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Courier New'
    Font.Style = []
    HideSelection = False
    Lines.Strings = (
      '// definition de variables'
      '//========================'
      'str="2*5//4+(-5%2)*5";'
      'num=2*5/4+(-5%2)*5;'
      'goto suite;'
      'println("ligne non evaluee");'
      'label suite;'
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
      'for i from 1 to 15 do'
      '{'
      #9'f=factorielle(i);'
      #9'println(i~"!="~f);'
      #9'if (i=10) break;'
      '}'
      ''
      'println("sortie de la boucle pour i="~i);'
      '// tableau'
      '//========================'
      ''
      'println("");'
      ''
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
    OnKeyDown = Memo1KeyDown
    OnKeyUp = Memo1KeyDown
    OnMouseUp = Memo1MouseUp
  end
  object PageControl1: TPageControl
    Left = 400
    Top = 48
    Width = 689
    Height = 417
    ActivePage = TabSheet3
    TabOrder = 2
    object TabSheet1: TTabSheet
      Caption = 'L'#39'arbre syntaxique'
      object TreeView1: TTreeView
        Left = 8
        Top = 8
        Width = 665
        Height = 377
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'Courier New'
        Font.Style = []
        Indent = 19
        ParentFont = False
        TabOrder = 0
        OnChange = TreeView1Change
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'mon assembleur'
      ImageIndex = 2
      object Label3: TLabel
        Left = 16
        Top = 8
        Width = 32
        Height = 13
        Caption = 'Label3'
      end
      object Label5: TLabel
        Left = 248
        Top = 8
        Width = 17
        Height = 13
        Caption = 'Pile'
      end
      object Label6: TLabel
        Left = 248
        Top = 208
        Width = 79
        Height = 13
        Caption = 'Variables locales'
      end
      object Label7: TLabel
        Left = 464
        Top = 208
        Width = 85
        Height = 13
        Caption = 'Variables globales'
      end
      object Label8: TLabel
        Left = 464
        Top = 8
        Width = 138
        Height = 13
        Caption = 'point de retour d'#39'une fonction'
      end
      object Memo3: TMemo
        Left = 8
        Top = 24
        Width = 233
        Height = 361
        HideSelection = False
        ScrollBars = ssVertical
        TabOrder = 0
      end
      object Memo4: TMemo
        Left = 248
        Top = 24
        Width = 209
        Height = 177
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -10
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        ScrollBars = ssVertical
        TabOrder = 1
      end
      object Memo5: TMemo
        Left = 248
        Top = 224
        Width = 209
        Height = 161
        ScrollBars = ssVertical
        TabOrder = 2
      end
      object Memo6: TMemo
        Left = 464
        Top = 24
        Width = 209
        Height = 177
        ScrollBars = ssVertical
        TabOrder = 3
      end
      object Memo7: TMemo
        Left = 464
        Top = 224
        Width = 209
        Height = 161
        ScrollBars = ssVertical
        TabOrder = 4
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Messages de sortie'
      ImageIndex = 1
      object Memo2: TMemo
        Left = 8
        Top = 8
        Width = 665
        Height = 377
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
  end
  object Button2: TButton
    Left = 312
    Top = 8
    Width = 81
    Height = 33
    Caption = 'Pas a Pas "assembleur"'
    TabOrder = 3
    WordWrap = True
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 88
    Top = 8
    Width = 105
    Height = 33
    Caption = 'Compile'
    TabOrder = 4
    OnClick = Button3Click
  end
end
