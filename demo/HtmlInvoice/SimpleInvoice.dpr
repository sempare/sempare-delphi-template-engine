program SimpleInvoice;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMX.Styles,
  MainForm in 'MainForm.pas' {Form1} ,
  InvoiceInputFrame in 'InvoiceInputFrame.pas' {InputFrame: TFrame} ,
  InvoicePreviewFrame in 'InvoicePreviewFrame.pas' {PreviewInvoiceFrame: TFrame} ,
  InvoiceListFrame in 'InvoiceListFrame.pas' {ListInvoicesFrame: TFrame} ,
  Model in 'Model.pas',
  InvoiceSettingsFrame in 'InvoiceSettingsFrame.pas' {SettingsFrame: TFrame};

{$R *.res}

begin
  // TStyleManager.SetStyleFromFile('Dark.Style');
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;

end.
