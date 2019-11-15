unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Data.DB, Vcl.StdCtrls, JvCsvData,
  Vcl.Grids, Vcl.DBGrids, Vcl.ExtCtrls, Vcl.DBCtrls, Vcl.ComCtrls, JvBaseDlg,
  JvSelectDirectory, System.Generics.Collections, System.Rtti,
  Sempare.Boot.Template.Velocity;

type
  TFormMain = class(TForm)
    lblTemplate: TLabel;
    lblData: TLabel;
    lblOutput: TLabel;
    edtTemplate: TEdit;
    edtData: TEdit;
    edtOutput: TEdit;
    pc: TPageControl;
    tsTemplate: TTabSheet;
    tsData: TTabSheet;
    tsPreview: TTabSheet;
    memTemplate: TMemo;
    DBNavigator1: TDBNavigator;
    DBGrid1: TDBGrid;
    csv: TJvCsvDataSet;
    butOpenTemplate: TButton;
    butOpenCSV: TButton;
    butSelectOutput: TButton;
    DataSource: TDataSource;
    OpenDialog: TOpenDialog;
    memPreview: TMemo;
    butProcess: TButton;
    Label1: TLabel;
    SelectDirectory: TJvSelectDirectory;
    procedure butOpenTemplateClick(Sender: TObject);
    procedure butOpenCSVClick(Sender: TObject);
    procedure butSelectOutputClick(Sender: TObject);
    procedure butProcessClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure csvAfterScroll(DataSet: TDataSet);
    procedure memTemplateChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    FTemplate: IVelocityTemplate;
    FData: TDictionary<string, string>;
    function DataSetToDict: TDictionary<string, string>;
    function DictEvalToString(const ADict: TDictionary<string, string>): string;
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

uses
  System.IOUtils;

{$R *.dfm}

procedure TFormMain.butOpenCSVClick(Sender: TObject);
begin
  OpenDialog.InitialDir := tpath.GetDirectoryName(edtData.Text);
  OpenDialog.FileName := edtData.Text;
  if OpenDialog.Execute then
  begin
    csv.LoadFromFile(OpenDialog.FileName);
    csv.active := true;
    pc.ActivePageIndex := 1;
  end;
end;

procedure TFormMain.butOpenTemplateClick(Sender: TObject);
var
  fs: TFileStream;
  ss: TStringStream;
begin
  OpenDialog.InitialDir := tpath.GetDirectoryName(edtTemplate.Text);
  OpenDialog.FileName := edtTemplate.Text;
  if OpenDialog.Execute then
  begin
    fs := TFileStream.Create(OpenDialog.FileName, fmopenread);
    ss := TStringStream.Create;
    ss.CopyFrom(fs, fs.Size);
    fs.Free;
    memTemplate.Text := ss.DataString;
    ss.Free;
  end;
end;

procedure TFormMain.butProcessClick(Sender: TObject);
var
  i: integer;
  fs: TFileStream;
  tw: TStreamWRiter;
begin
  for i := 0 to csv.RecordCount do
  begin
    csv.RecNo := i;
    memPreview.Text := DictEvalToString(DataSetToDict);
    fs := TFileStream.Create(tpath.Combine(edtOutput.Text, format('output.%d.txt', [i])), fmCreate);
    try
      tw := TStreamWRiter.Create(fs);
      tw.Write(memPreview.Text);
    finally
      tw.Free;
      fs.Free;
    end;
  end;
end;

procedure TFormMain.butSelectOutputClick(Sender: TObject);
begin
  SelectDirectory.InitialDir := edtOutput.Text;
  if SelectDirectory.Execute then
  begin
    edtOutput.Text := SelectDirectory.Directory;
  end;
end;

procedure TFormMain.csvAfterScroll(DataSet: TDataSet);
begin
  memPreview.Text := DictEvalToString(DataSetToDict);
  pc.ActivePageIndex := 2;
end;

function TFormMain.DataSetToDict: TDictionary<string, string>;
var
  i: integer;
  names: tarray<string>;
begin
  FData.Clear;
  names := csv.HeaderRow.Split([',']);
  for i := 0 to csv.FieldCount - 1 do
  begin
    FData.AddOrSetValue(names[i], VarToStr(csv.Fields[i].Value));
  end;
  result := FData;
end;

function TFormMain.DictEvalToString(const ADict: TDictionary<string, string>): string;

begin
  result := Velocity.Eval(FTemplate, ADict);
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  pc.ActivePageIndex := 0;
  csv.active := false;
  memTemplate.Text := '';
  memPreview.Text := '';
  edtTemplate.Text := tpath.GetFullPath(tpath.Combine(tpath.GetDirectoryName(paramstr(0)), '..\..\template\hello.txt'));
  edtData.Text := tpath.GetFullPath(tpath.Combine(tpath.GetDirectoryName(paramstr(0)), '..\..\data\data.csv'));
  edtOutput.Text := tpath.GetFullPath(tpath.Combine(tpath.GetDirectoryName(paramstr(0)), '..\..\output'));
  FData := TDictionary<string, string>.Create;

end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FData.Free;
end;

procedure TFormMain.memTemplateChange(Sender: TObject);
begin
  FTemplate := Velocity.Parse(memTemplate.Text);
end;

end.
