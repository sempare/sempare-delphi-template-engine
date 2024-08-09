unit InvoiceInputFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.TMSFNCTypes, FMX.TMSFNCUtils, FMX.TMSFNCGraphics,
  FMX.TMSFNCGraphicsTypes, FMX.TMSFNCHTMLText, FMX.TMSFNCCustomControl,
  FMX.TMSFNCCustomPicker, FMX.TMSFNCDatePicker, FMX.Controls.Presentation,
  FMX.Edit, FMX.EditBox, FMX.NumberBox, Model, DateUtils;

type
  TInputFrame = class(TFrame)
    edtInvoiceNo: TEdit;
    edtInvoiceDate: TTMSFNCDatePicker;
    TMSFNCHTMLText1: TTMSFNCHTMLText;
    TMSFNCHTMLText2: TTMSFNCHTMLText;
    TMSFNCHTMLText3: TTMSFNCHTMLText;
    edtClient: TEdit;
    edtClientAddr1: TEdit;
    edtClientAddr2: TEdit;
    TMSFNCHTMLText4: TTMSFNCHTMLText;
    TMSFNCHTMLText5: TTMSFNCHTMLText;
    TMSFNCHTMLText6: TTMSFNCHTMLText;
    edtBillingPeriodStart: TTMSFNCDatePicker;
    edtBillingPeriodEnd: TTMSFNCDatePicker;
    edtHoursWorked: TNumberBox;
    edtHourlyRate: TNumberBox;
    TMSFNCHTMLText7: TTMSFNCHTMLText;
    edtExpenses: TNumberBox;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    FOnAdd: TProc<TInvoice>;
    FConfig: TConfig;
    function GetConfig: TConfig;
    function GetOnAdd: TProc<TInvoice>;
    procedure SetConfig(const Value: TConfig);
    procedure SetOnAdd(const Value: TProc<TInvoice>);

  public
    { Public declarations }

    procedure Clear;
    property Config: TConfig read GetConfig write SetConfig;
    property OnAdd: TProc<TInvoice> read GetOnAdd write SetOnAdd;
  end;

implementation

{$R *.fmx}
{ TInputFrame }

procedure TInputFrame.Button1Click(Sender: TObject);
var
  LInv: TInvoice;
begin
  if assigned(FOnAdd) then
  begin
    LInv := TInvoice.Create;
    with LInv do
    begin
      InvoiceNo := edtInvoiceNo.text;
      InvoiceDate := edtInvoiceDate.SelectedDate;
      Client := edtClient.text;
      ClientAddress1 := edtClientAddr1.text;
      ClientAddress2 := edtClientAddr2.text;

      Company := FConfig.Company;
      CompanyAddress1 := FConfig.CompanyAddress1;
      CompanyAddress2 := FConfig.CompanyAddress2;
      CompanyNo := FConfig.CompanyNo;

      Bank := FConfig.Bank;
      BankDetails1 := FConfig.BankDetails1;
      BankDetails2 := FConfig.BankDetails2;

      BillingPeriodStart := edtBillingPeriodStart.SelectedDate;
      BillingPeriodEnd := edtBillingPeriodEnd.SelectedDate;

      HoursWorked := edtHoursWorked.Value;
      HourlyRate := edtHourlyRate.Value;

      Expenses := edtExpenses.Value;

      logoPath := FConfig.logoPath;

      Currency := FConfig.Currency;

      Status := 'draft';
    end;

    FOnAdd(LInv);
  end;
end;

procedure TInputFrame.Clear;
begin
  edtInvoiceNo.text := '';
  edtInvoiceDate.SelectedDate := now();

  edtClient.text := '';
  edtClientAddr1.text := '';
  edtClientAddr2.text := '';

  edtBillingPeriodStart.SelectedDate := IncDay(now, -7);
  edtBillingPeriodEnd.SelectedDate := IncDay(now, -1);
  edtHoursWorked.Value := 5 * 8;
  edtHourlyRate.Value := 50;

  edtExpenses.Value := 0;
end;

function TInputFrame.GetConfig: TConfig;
begin
  exit(FConfig);
end;

function TInputFrame.GetOnAdd: TProc<TInvoice>;
begin
  result := FOnAdd;
end;

procedure TInputFrame.SetConfig(const Value: TConfig);
begin
  FConfig := Value;
end;

procedure TInputFrame.SetOnAdd(const Value: TProc<TInvoice>);
begin
  FOnAdd := Value;
end;

end.
