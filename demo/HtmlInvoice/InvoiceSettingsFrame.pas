unit InvoiceSettingsFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.TMSFNCTypes, FMX.TMSFNCUtils, FMX.TMSFNCGraphics,
  FMX.TMSFNCGraphicsTypes, FMX.ListBox, FMX.Controls.Presentation,
  FMX.Edit, FMX.TMSFNCCustomControl, FMX.TMSFNCHTMLText, Model, FMX.Objects;

type
  TSettingsFrame = class(TFrame)
    TMSFNCHTMLText3: TTMSFNCHTMLText;
    edtCompany: TEdit;
    edtCompanyAddr1: TEdit;
    edtCompanyAddr2: TEdit;
    TMSFNCHTMLText1: TTMSFNCHTMLText;
    cmbCurrency: TComboBox;
    butSave: TButton;
    TMSFNCHTMLText2: TTMSFNCHTMLText;
    edtCompanyNo: TEdit;
    TMSFNCHTMLText4: TTMSFNCHTMLText;
    edtBank: TEdit;
    edtBankDetails1: TEdit;
    edtBankDetails2: TEdit;
    TMSFNCHTMLText5: TTMSFNCHTMLText;
    edtLogoPath: TEdit;
    imgLogo: TImage;
    Button1: TButton;
    OpenDialog1: TOpenDialog;
    procedure butSaveClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    FConfig: TConfig;
    FOnSave: TProc;
    procedure SetConfig(const Value: TConfig);
    { Private declarations }
  public
    { Public declarations }
    procedure UpdateConfig;
    property Config: TConfig read FConfig write SetConfig;
    property OnSave: TProc read FOnSave write FOnSave;
  end;

implementation

uses
  System.IOUtils;

{$R *.fmx}
{ TSettingsFrame }

procedure TSettingsFrame.butSaveClick(Sender: TObject);
begin
  if assigned(FOnSave) then
    FOnSave();
end;

procedure TSettingsFrame.Button1Click(Sender: TObject);
begin
  if edtLogoPath.text = '' then
    OpenDialog1.FileName := ''
  else
    OpenDialog1.FileName := TPath.GetDirectoryName(edtLogoPath.text);
  if OpenDialog1.Execute then
  begin
    edtLogoPath.text := OpenDialog1.FileName;
    if (edtLogoPath.text <> '') and ((TPath.GetExtension(edtLogoPath.text).ToLower = '.png') or (TPath.GetExtension(edtLogoPath.text).ToLower = '.jpg') or (TPath.GetExtension(edtLogoPath.text).ToLower = '.gif')) then

      imgLogo.Bitmap.LoadFromFile(edtLogoPath.text);
  end;
end;

procedure TSettingsFrame.SetConfig(const Value: TConfig);
begin
  FConfig := Value;
  edtCompany.text := Value.Company;
  edtCompanyAddr1.text := Value.CompanyAddress1;
  edtCompanyAddr2.text := Value.CompanyAddress2;
  cmbCurrency.ItemIndex := cmbCurrency.items.IndexOf(Value.Currency);
  edtCompanyNo.text := Value.CompanyNo;
  edtBank.text := Value.Bank;
  edtBankDetails1.text := Value.BankDetails1;
  edtBankDetails2.text := Value.BankDetails2;
  edtLogoPath.text := Value.LogoPath;
end;

procedure TSettingsFrame.UpdateConfig;
begin
  FConfig.Company := edtCompany.text;
  FConfig.CompanyAddress1 := edtCompanyAddr1.text;
  FConfig.CompanyAddress2 := edtCompanyAddr2.text;
  FConfig.Currency := cmbCurrency.text;
  FConfig.CompanyNo := edtCompanyNo.text;

  FConfig.Bank := edtBank.text;
  FConfig.BankDetails1 := edtBankDetails1.text;
  FConfig.BankDetails2 := edtBankDetails2.text;

  FConfig.LogoPath := edtLogoPath.text;
end;

end.
