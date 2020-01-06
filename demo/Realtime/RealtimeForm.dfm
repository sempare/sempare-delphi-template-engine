object FormRealTime: TFormRealTime
  Left = 0
  Top = 0
  Caption = 'FormRealTime'
  ClientHeight = 677
  ClientWidth = 985
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lblTemplate: TLabel
    Left = 18
    Top = 149
    Width = 44
    Height = 13
    Caption = 'Template'
  end
  object lblOutput: TLabel
    Left = 464
    Top = 149
    Width = 34
    Height = 13
    Caption = 'Output'
  end
  object lblPrettyPrint: TLabel
    Left = 24
    Top = 399
    Width = 55
    Height = 13
    Caption = 'Pretty Print'
  end
  object memoOutput: TMemo
    Left = 463
    Top = 168
    Width = 434
    Height = 225
    Lines.Strings = (
      'Memo1')
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object memoTemplate: TMemo
    Left = 18
    Top = 168
    Width = 432
    Height = 225
    ScrollBars = ssBoth
    TabOrder = 1
    WantTabs = True
    OnChange = memoTemplateChange
  end
  object cbStripRecurringSpaces: TCheckBox
    Left = 536
    Top = 38
    Width = 153
    Height = 17
    Caption = 'StripRecurringSpaces'
    TabOrder = 2
    OnClick = cbStripRecurringSpacesClick
  end
  object cbConvertTabsToSpaces: TCheckBox
    Left = 536
    Top = 15
    Width = 153
    Height = 17
    Caption = 'ConvertTabsToSpaces'
    TabOrder = 3
    OnClick = cbConvertTabsToSpacesClick
  end
  object memoPrettyPrint: TMemo
    Left = 18
    Top = 418
    Width = 432
    Height = 257
    Lines.Strings = (
      'memoPrettyPrint')
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 4
  end
  object cbEvalEarly: TCheckBox
    Left = 776
    Top = 107
    Width = 113
    Height = 17
    Caption = 'EvalEarly'
    TabOrder = 5
    OnClick = cbEvalEarlyClick
  end
  object cbEvalVarsEarly: TCheckBox
    Left = 856
    Top = 107
    Width = 97
    Height = 17
    Caption = 'EvalVarsEarly'
    TabOrder = 6
    OnClick = cbEvalVarsEarlyClick
  end
  object cbStripRecurringNewlines: TCheckBox
    Left = 536
    Top = 84
    Width = 153
    Height = 17
    Caption = 'StripRecurringNewlines'
    TabOrder = 7
    OnClick = cbStripRecurringNewlinesClick
  end
  object cbTrimLines: TCheckBox
    Left = 536
    Top = 61
    Width = 97
    Height = 17
    Caption = 'TrimLines'
    TabOrder = 8
    OnClick = cbTrimLinesClick
  end
  object cbRaiseErrorWhenVariableNotFound: TCheckBox
    Left = 536
    Top = 107
    Width = 217
    Height = 17
    Caption = 'RaiseErrorWhenVariableNotFound'
    TabOrder = 9
    OnClick = cbRaiseErrorWhenVariableNotFoundClick
  end
  object cbHtml: TCheckBox
    Left = 776
    Top = 15
    Width = 97
    Height = 17
    Caption = 'Html'
    TabOrder = 10
    OnClick = cbHtmlClick
  end
  object cbSetEncoding: TCheckBox
    Left = 776
    Top = 61
    Width = 74
    Height = 17
    Caption = 'Encoding'
    TabOrder = 11
    OnClick = cbSetEncodingClick
  end
  object cbUseHtmlBR: TCheckBox
    Left = 776
    Top = 38
    Width = 97
    Height = 17
    Caption = 'Use Html <BR>'
    TabOrder = 12
    OnClick = cbUseHtmlBRClick
  end
  object properties: TStringGrid
    Left = 18
    Top = 8
    Width = 432
    Height = 106
    ColCount = 2
    DefaultColWidth = 150
    FixedCols = 0
    RowCount = 50
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goTabs]
    TabOrder = 13
    OnGetEditText = propertiesGetEditText
    OnSetEditText = propertiesSetEditText
  end
  object WebBrowser1: TWebBrowser
    Left = 464
    Top = 418
    Width = 433
    Height = 265
    TabOrder = 14
    ControlData = {
      4C000000C02C0000631B00000000000000000000000000000000000000000000
      000000004C000000000000000000000001000000E0D057007335CF11AE690800
      2B2E126208000000000000004C0000000114020000000000C000000000000046
      8000000000000000000000000000000000000000000000000000000000000000
      00000000000000000100000000000000000000000000000000000000}
  end
  object butClear: TButton
    Left = 88
    Top = 137
    Width = 75
    Height = 25
    Caption = 'Clear'
    TabOrder = 15
    OnClick = butClearClick
  end
  object butSave: TButton
    Left = 184
    Top = 136
    Width = 75
    Height = 25
    Caption = 'Save'
    Enabled = False
    TabOrder = 16
    OnClick = butSaveClick
  end
  object butOpen: TButton
    Left = 375
    Top = 137
    Width = 75
    Height = 25
    Caption = 'Open'
    TabOrder = 17
    OnClick = butOpenClick
  end
  object cmbEncoding: TComboBox
    Left = 792
    Top = 80
    Width = 145
    Height = 21
    AutoDropDown = True
    ItemIndex = 2
    TabOrder = 18
    Text = 'UTF-8 (without BOM)'
    Items.Strings = (
      'ASCII'
      'UTF-8 (with BOM)'
      'UTF-8 (without BOM)')
  end
  object butSaveAs: TButton
    Left = 280
    Top = 137
    Width = 75
    Height = 25
    Caption = 'Save As'
    TabOrder = 19
    OnClick = butSaveAsClick
  end
  object context: TSempareBootVelocityContext
    Options = []
    MaxRunTimeMs = 60000
    StartToken = '<%'
    EndToken = '%>'
    Left = 728
  end
  object template: TSempareBootVelocityTemplate
    Context = context
    TemplateText = #13#10
    Enabled = False
    Left = 728
    Top = 48
  end
  object evalEngine: TSempareBootVelocityEngine
    Enabled = False
    Context = context
    Template = template
    Left = 728
    Top = 96
  end
  object OpenDialog1: TOpenDialog
    Left = 464
    Top = 16
  end
  object SaveDialog1: TSaveDialog
    Left = 472
    Top = 72
  end
end
