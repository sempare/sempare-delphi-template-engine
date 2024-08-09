unit InvoicePreviewFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.WebBrowser,
  Model;

type
  TPreviewInvoiceFrame = class(TFrame)
    WebBrowser1: TWebBrowser;
  private
    { Private declarations }
    FInvoice: TInvoice;
    procedure SetInvoice(const Value: TInvoice);
  public
    { Public declarations }
    procedure Print;
    property Invoice: TInvoice read FInvoice write SetInvoice;
  end;

implementation

uses
  Sempare.Template;

{$R *.fmx}
{ TPreviewInvoiceFrame }

procedure TPreviewInvoiceFrame.Print;
begin
  // TODO
end;

procedure TPreviewInvoiceFrame.SetInvoice(const Value: TInvoice);
var
  LText: string;
  LData: TTemplateData;
begin
  FInvoice := Value;
  LData.Invoice := FInvoice;
  WebBrowser1.LoadFromStrings(Template.Resolve('invoice', LData), 'text/html');
end;

end.
