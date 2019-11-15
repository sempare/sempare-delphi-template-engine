object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'FormMain'
  ClientHeight = 380
  ClientWidth = 648
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    648
    380)
  PixelsPerInch = 96
  TextHeight = 13
  object lblTemplate: TLabel
    Left = 8
    Top = 8
    Width = 69
    Height = 13
    Caption = 'Template Path'
  end
  object lblData: TLabel
    Left = 8
    Top = 38
    Width = 44
    Height = 13
    Caption = 'CSV Path'
  end
  object lblOutput: TLabel
    Left = 8
    Top = 65
    Width = 59
    Height = 13
    Caption = 'Output Path'
  end
  object Label1: TLabel
    Left = 8
    Top = 89
    Width = 417
    Height = 13
    Caption = 
      'This is just a demo using a template and a csv to  perform '#39'mail' +
      ' merge'#39' type processing.'
  end
  object edtTemplate: TEdit
    Left = 83
    Top = 8
    Width = 476
    Height = 21
    Hint = 'Enter a path to a template file'
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    Text = 'edtTemplate'
    ExplicitWidth = 478
  end
  object edtData: TEdit
    Left = 83
    Top = 35
    Width = 476
    Height = 21
    Hint = 'Enter a path to a CSV file (first line should contain a header)'
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    Text = 'edtData'
    ExplicitWidth = 478
  end
  object edtOutput: TEdit
    Left = 83
    Top = 62
    Width = 476
    Height = 21
    Hint = 'Enter a path to where the output should be writt'
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
    Text = 'edtOutput'
    ExplicitWidth = 478
  end
  object pc: TPageControl
    Left = 8
    Top = 144
    Width = 632
    Height = 228
    ActivePage = tsData
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 3
    ExplicitWidth = 634
    ExplicitHeight = 217
    object tsTemplate: TTabSheet
      Caption = 'Template'
      ExplicitWidth = 281
      ExplicitHeight = 165
      object memTemplate: TMemo
        Left = 0
        Top = 0
        Width = 624
        Height = 200
        Align = alClient
        Lines.Strings = (
          'memTemplate')
        TabOrder = 0
        OnChange = memTemplateChange
        ExplicitLeft = 184
        ExplicitTop = 155
        ExplicitWidth = 185
        ExplicitHeight = 89
      end
    end
    object tsData: TTabSheet
      Caption = 'Data'
      ImageIndex = 1
      ExplicitWidth = 281
      ExplicitHeight = 165
      object DBNavigator1: TDBNavigator
        Left = 0
        Top = 0
        Width = 624
        Height = 25
        DataSource = DataSource
        Align = alTop
        TabOrder = 0
        ExplicitLeft = 336
        ExplicitTop = 136
        ExplicitWidth = 240
      end
      object DBGrid1: TDBGrid
        Left = 0
        Top = 25
        Width = 624
        Height = 175
        Align = alClient
        DataSource = DataSource
        TabOrder = 1
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'Tahoma'
        TitleFont.Style = []
      end
    end
    object tsPreview: TTabSheet
      Caption = 'Preview'
      ImageIndex = 2
      ExplicitWidth = 281
      ExplicitHeight = 165
      object memPreview: TMemo
        Left = 0
        Top = 0
        Width = 624
        Height = 200
        Align = alClient
        Lines.Strings = (
          'memPreview')
        ReadOnly = True
        TabOrder = 0
        ExplicitLeft = 80
        ExplicitTop = 144
        ExplicitWidth = 185
        ExplicitHeight = 89
      end
    end
  end
  object butOpenTemplate: TButton
    Left = 565
    Top = 8
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Open'
    TabOrder = 4
    OnClick = butOpenTemplateClick
    ExplicitLeft = 567
  end
  object butOpenCSV: TButton
    Left = 565
    Top = 33
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Open'
    TabOrder = 5
    OnClick = butOpenCSVClick
    ExplicitLeft = 567
  end
  object butSelectOutput: TButton
    Left = 565
    Top = 60
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Select'
    TabOrder = 6
    OnClick = butSelectOutputClick
    ExplicitLeft = 567
  end
  object butProcess: TButton
    Left = 565
    Top = 91
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Process'
    TabOrder = 7
    OnClick = butProcessClick
  end
  object csv: TJvCsvDataSet
    FileName = 
      'E:\sempare\sempare-boot-git\sempare.boot.velocity\demo\MiniMailM' +
      'erge\data\data.csv'
    AfterScroll = csvAfterScroll
    AutoBackupCount = 0
    StoreDefs = True
    Left = 16
    Top = 96
  end
  object DataSource: TDataSource
    DataSet = csv
    Left = 88
    Top = 96
  end
  object OpenDialog: TOpenDialog
    Left = 152
    Top = 96
  end
  object SelectDirectory: TJvSelectDirectory
    Left = 224
    Top = 96
  end
end
