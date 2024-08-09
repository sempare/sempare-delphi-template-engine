unit InvoiceListFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.TMSFNCTypes, FMX.TMSFNCUtils, FMX.TMSFNCGraphics,
  FMX.TMSFNCGraphicsTypes, FMX.Controls.Presentation,
  FMX.TMSFNCCustomControl, FMX.TMSFNCTreeViewBase, FMX.TMSFNCTreeViewData,
  FMX.TMSFNCCustomTreeView, FMX.TMSFNCTreeView,
  Model;

type
  TListInvoicesFrame = class(TFrame)
    tv: TTMSFNCTreeView;
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure tvAfterSelectNode(Sender: TObject; ANode: TTMSFNCTreeViewVirtualNode);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
    FInvoices: Tarray<TInvoice>;
    FOnGetInvoices: TFunc<Tarray<TInvoice>>;
    FOnAdd: TProc;
    FOnSettings: TProc;
    FOnPreview: TProc<TInvoice>;
    FOnPrint: TProc;

    procedure populateNodes;
  public
    { Public declarations }
    procedure Init;

    procedure AddItem(const AInvoice: TInvoice);
    property OnGetInvoices: TFunc < Tarray < TInvoice >> read FOnGetInvoices write FOnGetInvoices;
    property OnAdd: TProc read FOnAdd write FOnAdd;
    property OnSettings: TProc read FOnSettings write FOnSettings;
    property OnPreview: TProc<TInvoice> read FOnPreview write FOnPreview;
    property OnPrint: TProc read FOnPrint write FOnPrint;
  end;

implementation

{$R *.fmx}

uses
  DateUtils;

procedure TListInvoicesFrame.AddItem(const AInvoice: TInvoice);
var
  LNode: TTMSFNCTreeViewNode;
begin
  LNode := tv.nodes.add;
  LNode.Text[0] := AInvoice.InvoiceNo;
  LNode.Text[1] := AInvoice.Client;
  LNode.Text[2] := format('%8.2f', [AInvoice.Total]);
  LNode.Text[3] := AInvoice.Status;
  LNode.Text[4] := format('%s - %s', [FormatDateTime('yyyy-mm-dd', AInvoice.BillingPeriodStart), FormatDateTime('yyyy-mm-dd', AInvoice.BillingPeriodEnd)]);
  LNode.Text[5] := format('%8.2f', [AInvoice.HoursWorked]);
  LNode.Text[6] := format('%8.2f', [AInvoice.HourlyRate]);
  LNode.Text[7] := format('%8.2f', [AInvoice.WorkTotal]);
  LNode.Text[8] := format('%8.2f', [AInvoice.Expenses]);
  LNode.DataObject := AInvoice;
end;

procedure TListInvoicesFrame.Button1Click(Sender: TObject);
begin
  if assigned(OnAdd) then
    OnAdd();
end;

procedure TListInvoicesFrame.Button2Click(Sender: TObject);
begin
  if assigned(FOnSettings) then
    FOnSettings();
end;

procedure TListInvoicesFrame.Button3Click(Sender: TObject);
begin
  if assigned(FOnPrint) then
    FOnPrint;
end;

procedure TListInvoicesFrame.Init;
begin
  if not assigned(FInvoices) and assigned(FOnGetInvoices) then
    FInvoices := FOnGetInvoices;
  populateNodes;
end;

procedure TListInvoicesFrame.populateNodes;
var
  LItem: TTMSFNCTreeViewNode;
  LInvoice: TInvoice;
begin
  tv.BeginUpdate;
  tv.nodes.clear;

  for LInvoice in FInvoices do
  begin
    LItem := tv.nodes.add;
  end;
  tv.EndUpdate;
end;

procedure TListInvoicesFrame.tvAfterSelectNode(Sender: TObject; ANode: TTMSFNCTreeViewVirtualNode);
begin
  if assigned(FOnPreview) then
    FOnPreview(ANode.Node.DataObject as TInvoice);
end;

end.
