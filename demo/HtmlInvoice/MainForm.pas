unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  SubjectStand, FrameStand, FMX.StdCtrls, FMX.Controls.Presentation,
  Model,
  InvoiceListFrame,
  InvoicePreviewFrame,
  InvoiceInputFrame,
  InvoiceSettingsFrame;

type
  TForm1 = class(TForm)
    FrameStand: TFrameStand;
    body: TPanel;
    Splitter1: TSplitter;
    preview: TPanel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FConfig: TConfig;
    FInvoices: TArray<TInvoice>;
    FListFrame: TFrameInfo<TListInvoicesFrame>;
    FPreviewFrame: TPreviewInvoiceFrame;
    FInputFrame: TFrameInfo<TInputFrame>;
    FSettingsFrame: TFrameInfo<TSettingsFrame>;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.FormCreate(Sender: TObject);
begin
  FConfig := TConfig.Create;
  FConfig.Company := 'Sempare Limited';
  FConfig.CompanyAddress1 := '81 Quarrendon Road, Amersham';
  FConfig.CompanyAddress2 := 'United Kingdoom, HP7 9EH';
  FConfig.Currency := 'GBP';
  FConfig.CompanyNo := '123123';

  FConfig.Bank := 'Starling Bank';
  FConfig.BankDetails1 := 'Account No: 123123, Sort Code: 12-12-12';
  FConfig.BankDetails2 := 'IBAN: 121212123123';
  FConfig.LogoPath := 'Y:\sempare-delphi-sempare-boot-pro\sempare-boot-pro-bootstrap\images\sempare-logo.png';
  FListFrame := FrameStand.NewAndShow<TListInvoicesFrame>(body);
  FPreviewFrame := TPreviewInvoiceFrame.Create(preview);
  FPreviewFrame.parent := preview;
  preview.Visible := false;
  FInputFrame := FrameStand.New<TInputFrame>(body);
  FInputFrame.Frame.Config := FConfig;
  FSettingsFrame := FrameStand.New<TSettingsFrame>(body);

  FSettingsFrame.Frame.OnSave := procedure
    begin
      FSettingsFrame.Hide();
      FSettingsFrame.Frame.UpdateConfig;
      FListFrame.Show();
    end;

  FListFrame.Frame.OnAdd := procedure
    begin
      FListFrame.Hide();
      FInputFrame.Show();
      FInputFrame.Frame.Clear;
    end;

  FListFrame.Frame.OnGetInvoices := function: TArray<TInvoice>
    begin
      exit(FInvoices);
    end;

  FListFrame.Frame.OnSettings := procedure
    begin
      FListFrame.Hide();
      FSettingsFrame.Frame.Config := FConfig;
      FSettingsFrame.Show();
    end;

  FInputFrame.Frame.OnAdd := procedure(AInvoice: TInvoice)
    begin
      FListFrame.Frame.AddItem(AInvoice);
      insert(AInvoice, FInvoices, length(FInvoices));
      FInputFrame.Hide;
      FListFrame.Show();
    end;

  FListFrame.Frame.OnPreview := procedure(AInvoice: TInvoice)
    begin
      preview.Visible := true;
      FPreviewFrame.invoice := AInvoice;
    end;

  FListFrame.Frame.OnPrint := procedure
    begin
      if preview.Visible then
        FPreviewFrame.Print;
    end;
  body.Width := Width * 2 / 3;
end;

end.
