unit Model;

interface

type
  TConfig = class
    Company: string;
    CompanyAddress1: string;
    CompanyAddress2: string;
    CompanyNo: string;
    Currency: string;

    LogoPath:string;

    Bank: string;
    BankDetails1: string;
    BankDetails2: string;
  end;

  TInvoice = class
  public
    InvoiceNo: string;
    InvoiceDate: TDate;
    Client: string;
    ClientAddress1: string;
    ClientAddress2: string;

    LogoPath:string;

    Company: string;
    CompanyAddress1: string;
    CompanyAddress2: string;
    CompanyNo: string;

    BillingPeriodStart: TDateTime;
    BillingPeriodEnd: TDateTime;

    HoursWorked: double;
    HourlyRate: double;

    Expenses: double;

    Currency: string;

    Status: string;

    Bank: string;
    BankDetails1: string;
    BankDetails2: string;

    function WorkTotal: double;
    function Total: double;

  end;

  TTemplateData = record
    Invoice: TInvoice;
  end;

implementation

{ TInvoice }

function TInvoice.Total: double;
begin
  exit(WorkTotal + Expenses);
end;

function TInvoice.WorkTotal: double;
begin
  exit(HoursWorked * HourlyRate);
end;

end.
