object FormTemplateEditor: TFormTemplateEditor
  Left = 0
  Top = 0
  Caption = 'Template'
  ClientHeight = 289
  ClientWidth = 459
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    459
    289)
  PixelsPerInch = 96
  TextHeight = 13
  object memo: TMemo
    Left = 0
    Top = 0
    Width = 459
    Height = 250
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'memo')
    TabOrder = 0
  end
  object butCancel: TButton
    Left = 295
    Top = 256
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object butOk: TButton
    Left = 376
    Top = 256
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
end
