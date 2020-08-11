unit BZPictureDialogs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, GraphType, InterfaceBase, LCLType,
  IntfGraphics, FPimage, LCLIntf, LazFileUtils, FPCanvas,
  Forms, StdCtrls, Controls, Dialogs, ExtDlgs, ComCtrls, ExtCtrls,
  BZClasses, BZColors, BZGraphic, BZBitmap, BZBitmapIO;

type
  { TBZOpenPictureDialog }
  TBZOpenPictureDialog = Class(TOpenDialog)
  private
    FPreviewFilename : string;
  protected
    FPreviewForm : TForm;
    FPanelView : TPanel;
    FGroupBoxInfo : TGroupBox;
    FLabelFileName, FlblFileName : TLabel;
    FLabelPicInfo, FlblPicInfo : TLabel;
    FLabelPicFormat, FlblPicFormat : TLabel;
    FLabelPicEncoding, FlblPicEncoding : TLabel;
    FImageProgress : TProgressBar;

    FBmp : TBZBitmap;
    //class procedure WSRegisterClass; override;
    procedure UpdatePreview; virtual; //override;

    procedure CreatePreviewForm;

    procedure DoPreviewPaint(Sender: TObject);
    Procedure HandleOnImageProgress(Sender : TObject; Stage : TBZProgressStage; PercentDone : Byte; RedrawNow : Boolean; Const R : TRect; Const Msg : String; Var aContinue : Boolean);
  public
    constructor Create(TheOwner: TComponent); override;
    procedure DoClose; override;
    procedure DoShow; override;
    procedure DoSelectionChange; override;
    function Execute: boolean; override;
  end;

implementation

Procedure TBZOpenPictureDialog.HandleOnImageProgress(Sender : TObject; Stage : TBZProgressStage; PercentDone : Byte; RedrawNow : Boolean; Const R : TRect; Const Msg : String; Var aContinue : Boolean);
begin
  Case Stage Of
    opsStarting, opsRunning :
    Begin
      FImageProgress.Position := PercentDone;
      Application.ProcessMessages;
    End;
    opsEnding:
    Begin
      FImageProgress.Position := 0;
    End;
  End;
end;

procedure TBZOpenPictureDialog.UpdatePreview;
var
  CurFilename: string;
  FileIsValid: boolean;
  DlgRect : TRect;
  //NativeBmp : Graphics.TBitmap;
begin

  CurFilename := FileName;

  if CurFilename = FPreviewFilename then exit;
  FPreviewFilename := CurFilename;
  FileIsValid := FileExists(FPreviewFilename); //FileIsReadable(FPreviewFilename);
  if FileIsValid then
  try

    try
      if not(FPreviewForm.Visible) then
      begin
        GetWindowRect(Self.Handle, DlgRect);
        FPreviewForm.Left := DlgRect.Left + DlgRect.Right + 8;
        FPreviewForm.Top := DlgRect.Top;
        FImageProgress.Visible := False;
        FPanelView.Caption := '';
        FPreviewForm.Show;
      end;

      Screen.Cursor := crHourGlass;
      FBmp.SetSize(FPanelView.Width,FPanelView.Height);
      FBmp.Clear(clrGray);
      //FPanelView.Caption := 'Chargement en cours';
      FImageProgress.Visible := True;
      FImageProgress.Position := 0;
      FPanelView.Invalidate;

      FBmp.LoadFromFile(FPreviewFilename);
      Screen.Cursor := crDefault;

      //PictureGroupBox.Caption := Format('(%s - %dx%d BPP:%d)',
      //                           [Bmp.Width, Bmp.Height, Bmp.ImageDescription.BitCount]);
      //Bmp.Transformation.Stretch(ImageCtrl.ClientWidth, ImageCtrl.ClientHeight, True);
      //NativeBmp := Bmp.ExportToBitmap;
      //ImageCtrl.Transparent := False;
      //ImageCtrl.Picture.Bitmap.Assign(NativeBmp);
      //ImageCtrl.Repaint;
      //FreeAndNil(NativeBmp)


      FLblFileName.Caption := ExtractFileName(FPreviewFileName);
      FLblPicInfo.Caption := Format('%dx%d - bpp : %d bits',[FBmp.Width, FBmp.Height, FBmp.ImageDescription.BitCount]); ;
      FLblPicFormat.Caption := FBmp.DataFormatDesc.Desc + ' Version : ' + FBmp.DataFormatDesc.Version;
      FlblPicEncoding.Caption := cBZEncodingTypeStr[FBmp.DataFormatDesc.Encoding];
      FBmp.Transformation.Stretch(FPanelView.ClientWidth, FPanelView.ClientHeight, True);


  Finally
    FImageProgress.Visible := False;
    FPanelView.Caption := '';
    FPreviewForm.Refresh;
  end;
  except
    FileIsValid := False;
  end;
  //if not FileIsValid then
  //begin
  //  //ShowMessage('Not Valid');
  //  FPreviewForm.Close;
  //  //ClearPreview;
  //end;
end;

procedure TBZOpenPictureDialog.DoSelectionChange;
begin
  inherited DoSelectionChange;
  UpdatePreview;
end;

function TBZOpenPictureDialog.Execute: boolean;
begin
  Filter := GetBZImageFileFormats.BuildDialogFilters;
  result := inherited Execute;
end;

constructor TBZOpenPictureDialog.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  CreatePreviewForm;
  FBmp := TBZBitmap.create;
  FBmp.OnProgress := @HandleOnImageProgress;
end;

procedure TBZOpenPictureDialog.DoClose;
begin
  FPreviewForm.Close;
  FreeAndNil(FPreviewForm);
  FreeAndNil(FBmp);
  inherited DoClose;
end;

procedure TBZOpenPictureDialog.DoShow;
begin
  CreatePreviewForm;
  FBmp := TBZBitmap.create;
  FBmp.OnProgress := @HandleOnImageProgress;
  inherited DoShow;
end;

procedure TBZOpenPictureDialog.CreatePreviewForm;
begin
  FPreviewForm := TForm.Create(nil);
  FPreviewForm.Width := 360;
  FPreviewForm.Height := 360;
  FPreviewForm.Constraints.MinWidth := 360;
  FPreviewForm.Constraints.MinHeight := 360;
  FPreviewForm.Position :=  poScreenCenter;
  FPreviewForm.FormStyle := fsStayOnTop;
  FPreviewForm.BorderStyle := bsDialog;
  FPreviewForm.Caption := 'Preview';

  FPanelView := TPanel.Create(FPreviewForm);
  FPanelView.Parent := FPreviewForm;
  FPanelView.Width := 360;
  FPanelView.Height := 240;
  FPanelView.Align := alTop;
  FPanelView.Caption := '';
  FPanelView.Color := clGray;
  FPanelView.OnPaint := @DoPreviewPaint;
  FPanelView.BevelOuter := bvNone;
  FPanelView.BorderSpacing.Around := 4;

  FImageProgress := TProgressBar.Create(FPreviewForm);
  FImageProgress.Parent := FPanelView;
  FImageProgress.Width := 330;
  FImageProgress.Height := 16;
  FImageProgress.Left := 10;
  FImageProgress.Top := 112;
  FImageProgress.Visible := False;
  FImageProgress.Position := 0;


  FGroupBoxInfo := TGroupBox.Create(FPreviewForm);
  FGroupBoxInfo.Parent := FPreviewForm;
  FGroupBoxInfo.Caption := 'Informations';
  FGroupBoxInfo.Align := alClient;
  FGroupBoxInfo.BorderSpacing.Around := 4;

  FLabelFileName := TLabel.Create(FPreviewForm);
  FLabelFileName.Parent := FGroupBoxInfo;
  FLabelFileName.AutoSize := false;
  FLabelFileName.Width := 80;
  FLabelFileName.Top := 8;
  FLabelFileName.Left := 4;
  FLabelFileName.Alignment := taRightJustify;
  FLabelFileName.Caption := 'Nom : ';
  FLabelFileName.Font.Style := [fsBold];

  FLabelPicFormat := TLabel.Create(FPreviewForm);
  FLabelPicFormat.Parent := FGroupBoxInfo;
  FLabelPicFormat.AutoSize := false;
  FLabelPicFormat.Width := 80;
  FLabelPicFormat.Top := 26;
  FLabelPicFormat.Left := 4;
  FLabelPicFormat.Alignment := taRightJustify;
  FLabelPicFormat.Caption := 'Format : ';
  FLabelPicFormat.Font.Style := [fsBold];

  FLabelPicInfo := TLabel.Create(FPreviewForm);
  FLabelPicInfo.Parent := FGroupBoxInfo;
  FLabelPicInfo.AutoSize := false;
  FLabelPicInfo.Width := 80;
  FLabelPicInfo.Top := 44;
  FLabelPicInfo.Left := 4;
  FLabelPicInfo.Alignment := taRightJustify;
  FLabelPicInfo.Caption := 'Dimension : ';
  FLabelPicInfo.Font.Style := [fsBold];

  FLabelPicEncoding := TLabel.Create(FPreviewForm);
  FLabelPicEncoding.Parent := FGroupBoxInfo;
  FLabelPicEncoding.AutoSize := false;
  FLabelPicEncoding.Width := 80;
  FLabelPicEncoding.Top := 62;
  FLabelPicEncoding.Left := 4;
  FLabelPicEncoding.Alignment := taRightJustify;
  FLabelPicEncoding.Caption := 'Compression : ';
  FLabelPicEncoding.Font.Style := [fsBold];

  FlblFileName := TLabel.Create(FPreviewForm);
  FlblFileName.Parent := FGroupBoxInfo;
  FlblFileName.Top := 8;
  FlblFileName.Left := 84;
  FlblFileName.Caption := '-';

  FlblPicInfo := TLabel.Create(FPreviewForm);
  FlblPicInfo.Parent := FGroupBoxInfo;
  FlblPicInfo.Top := FLabelPicInfo.Top;
  FlblPicInfo.Left := 84;
  FlblPicInfo.Caption := '-';

  FlblPicFormat := TLabel.Create(FPreviewForm);
  FlblPicFormat.Parent := FGroupBoxInfo;
  FlblPicFormat.Top := FLabelPicFormat.Top;
  FlblPicFormat.Left := 84;
  FlblPicFormat.Caption := '-';

  FlblPicEncoding := TLabel.Create(FPreviewForm);
  FlblPicEncoding.Parent := FGroupBoxInfo;
  FlblPicEncoding.Top := FLabelPicEncoding.Top;
  FlblPicEncoding.Left := 84;
  FlblPicEncoding.Caption := '-';



end;

procedure TBZOpenPictureDialog.DoPreviewPaint(Sender: TObject);
Var
  DstRect : TRect;
begin
  DstRect := FPanelView.ClientRect;
  DstRect.Left := ((DstRect.Right - DstRect.Left) div 2) - (FBmp.Width div 2);
  DstRect.Top := ((DstRect.Bottom - DstRect.Top) div 2) - (FBmp.Height div 2);
  DstRect.Right := DstRect.Left + FBmp.Width;
  DstRect.Bottom := DstRect.Top + FBmp.Height;

  If Assigned(FBmp) then FBmp.DrawToCanvas(FPanelView.Canvas, DstRect);
end;



end.

