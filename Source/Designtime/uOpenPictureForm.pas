unit uOpenPictureForm;

//==============================================================================
{$mode objfpc}{$H+}
{$i ..\bzscene_options.inc}
//==============================================================================

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ExtDlgs, StdCtrls,
  BZColors, BZGraphic, BZBitmap, BZImageViewer, BZBitmapIO;

type

  { TOpenPictureForm }

  TOpenPictureForm = class(TForm)
    Panel1 : TPanel;
    Panel2 : TPanel;
    OPD : TOpenPictureDialog;
    btnOpen : TButton;
    btnCancel : TButton;
    btnOk : TButton;
    ImageViewer : TBZImageViewer;
    btnClear : TButton;
    procedure btnClearClick(Sender : TObject);
    procedure btnOpenClick(Sender : TObject);
  private

  public
    constructor Create(AOwner: TComponent); override;
    function Execute(APicture : TBZPicture) : Boolean;
  end;

function OpenPictureForm : TOpenPictureForm;
procedure ReleaseOpenPictureForm;

implementation

{$R *.lfm}

var
  vOpenPictureForm : TOpenPictureForm;

function OpenPictureForm : TOpenPictureForm;
begin
  if not Assigned(vOpenPictureForm) then
      vOpenPictureForm := TOpenPictureForm.Create(nil);
  Result := vOpenPictureForm;
end;

procedure ReleaseOpenPictureForm;
begin
  if Assigned(vOpenPictureForm) then
  begin
  	 vOpenPictureForm.Free;
     vOpenPictureForm:=nil;
  end;
end;

{ TOpenPictureForm }

constructor TOpenPictureForm.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  OPD.Filter :=  GetBZImageFileFormats.BuildDialogFilters;
end;

procedure TOpenPictureForm.btnClearClick(Sender : TObject);
begin
  ImageViewer.Picture.Bitmap.Clear(clrTransparent);
end;

procedure TOpenPictureForm.btnOpenClick(Sender : TObject);
begin
  if OPD.Execute then
  begin
    ImageViewer.Picture.LoadFromFile(OPD.FileName);
  end;
end;


function TOpenPictureForm.Execute(APicture : TBZPicture) : Boolean;
begin
  Result := (ShowModal = mrOk);
  if Result then
  begin
    APicture.Bitmap.Assign(ImageViewer.Picture.Bitmap);
  end;
end;

initialization


finalization

  ReleaseOpenPictureForm;

end.

