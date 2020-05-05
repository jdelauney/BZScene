unit uBZitmapInfosForm;

{$mode objfpc}{$H+}
{$i ..\..\bzscene_options.inc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons,
  BZClasses,
  BZColors, BZGraphic, BZBitmap, BZImageFileBMP,
  BZTypesHelpers;

type

  { TBZBitmapInfosForm }

  TBZBitmapInfosForm = class(TForm)
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    Bevel4: TBevel;
    Bevel5: TBevel;
    Bevel6: TBevel;
    Bevel7: TBevel;
    Bevel8: TBevel;
    Bevel9: TBevel;
    Button1: TButton;
    Button2 : Tbutton;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    lblAlphaShift: TLabel;
    lblAlphaSize: TLabel;
    lblBlueShift: TLabel;
    lblBlueSize: TLabel;
    lblGreenShift: TLabel;
    lblGreenSize: TLabel;
    lblRedMask: TLabel;
    lblBitsCount: TLabel;
    lblLineOrder: TLabel;
    lblHasAlpha: TLabel;
    lblHasPalette: TLabel;
    lblPaletteCount: TLabel;
    lblDataFormat: TLabel;
    lblDataCompression: TLabel;
    Label2: TLabel;
    lblPixelFormat: TLabel;
    lblGreenMask: TLabel;
    lblBlueMask: TLabel;
    lblAlphaMask: TLabel;
    lblRedShift: TLabel;
    lblRedSize: TLabel;
    lblWidth: TLabel;
    lblHeight: TLabel;
    lblDataSize: TLabel;
    lblRowStrideType: TLabel;
    lblRowSize: TLabel;
    lblRowStrideSize: TLabel;
    lblRowStrideLen: TLabel;
    lblColorFormat: TLabel;
    lblDataVersion: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    label9: TLabel;
    Panel1: TPanel;
    Panel10: TPanel;
    Panel2: TPanel;
    pnlGeneralsInfos: TPanel;
    Panel4: TPanel;
    pnlExtrasInfos: TPanel;
    Panel6: TPanel;
    pnlBFInfos: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    SpeedButton1: TSpeedButton;
    btnShowHideExtrasInfos: TSpeedButton;
    btnShowHideBFInfos: TSpeedButton;
    Label26 : TLabel;
    lblIsInterlaced : TLabel;
    procedure btnShowHideBFInfosClick(Sender: TObject);
    procedure btnShowHideExtrasInfosClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure AlignPanels(Sender: TObject);
  public
   // Description : TBZImageRawDescription;
   procedure FillInfos(Const ABitmap : TBZCustomBitmap);
  end;

var
  BZBitmapInfosForm: TBZBitmapInfosForm;

procedure ShowBitmapInfos(Const ABitmap : TBZCustomBitmap);

implementation

{$R *.lfm}



Const
   cLineEndStr : array[bleNoBoundary..bleDQWordBoundary] of string =('Aucun', '8bits', '16bits', '32bits','64bits','128bits');
   cLineOrderStr : Array[bloTopToBottom..bloBottomToTop] of string =('de haut en bas', 'de bas en haut');


procedure TBZBitmapInfosForm.btnShowHideExtrasInfosClick(Sender: TObject);
begin
  pnlExtrasInfos.Top :=btnShowHideExtrasInfos.top+btnShowHideExtrasInfos.Height;  //pnlExtrasInfos.Top+pnlExtrasInfos.Height;
  pnlExtrasInfos.Visible := not pnlExtrasInfos.Visible;
  AlignPanels(Sender);
end;

procedure TBZBitmapInfosForm.Button1Click(Sender: TObject);
begin
  Close;
end;


procedure TBZBitmapInfosForm.FormShow(Sender: TObject);
begin
  AlignPanels(Sender);
end;

procedure TBZBitmapInfosForm.btnShowHideBFInfosClick(Sender: TObject);
begin
  pnlBFInfos.Top := btnShowHideBFInfos.Top+btnShowHideBFInfos.Height;// pnlBFInfos.Top+pnlBFInfos.Height;
  pnlBFInfos.Visible := not pnlBFInfos.Visible;
  AlignPanels(Sender);
end;

procedure TBZBitmapInfosForm.AlignPanels(Sender: TObject);
var
  H: Integer;
begin
  H := 0;

  Inc(H, pnlGeneralsInfos.Height);
  if pnlExtrasInfos.Visible then Inc(H, pnlExtrasInfos.Height);

  Inc(H, pnlBFInfos.Height);
  if pnlBFInfos.Visible then Inc(H, pnlBFInfos.Height);

//  Inc(H, PostProcessHintPnl.Height);

  //TSpeedButton(Sender).Parent.ClientHeight := H;
  Self.ClientHeight := H;

end;

procedure TBZBitmapInfosForm.FillInfos(Const ABitmap : TBZCustomBitmap);
begin
  With ABitmap do
  begin
   With DataFormatDesc do
   begin
     lblDataFormat.Caption:= Name + Desc.Surround('( ',' )');
     lblDataVersion.Caption:=Version;
   end;

   Case DataFormatDesc.Encoding of
      etNone : lblDataCompression.Caption:='Aucune';
      etRLE : lblDataCompression.Caption:='RLE';
      etJPEG : lblDataCompression.Caption:='JPEG';
      etLZ77 : lblDataCompression.Caption:='LZ 77';
      etHuffman : lblDataCompression.Caption:='Huffman';
     // 2 : lblDataCompression.Caption:='RLE 4Bits';
      etBitFields : lblDataCompression.Caption:='Encodage BitFields';
      etLZW : lblDataCompression.Caption:='LZW';
      //  4 : if FHeaderType=bmpht_Os22x then lblDataCompression.Caption:='RLE 24Bits' else
      //4:  lblDataCompression.Caption:='JPEG';
      //5 : lblDataCompression.Caption:='PNG';
      //6 : lblDataCompression.Caption:='Encodage Alpha Bitfields';
      else lblDataCompression.Caption:='Encodage non supporté';
    end;


   With ImageDescription do
   begin
     // Infos de base
     lblPixelFormat.Caption:=BitsPerPixel.ToString;
     lblWidth.Caption:=Width.ToString();
     lblHeight.Caption:=Height.ToString();
     lblRowSize.Caption:=BytesPerLine.ToString;
     lblDataSize.Caption:=Size.ToString();
     lblRowStrideType.Caption:=cLineEndStr[RowStrideType];
     lblRowStrideSize.Caption:=RowStrideSize.ToString;
     lblRowStrideLen.Caption:=RowStrideLength.ToString;
     lblColorFormat.Caption:=BZColorFormatDesc[ColorFormat].name; //'BGRA';
     // Infos complémentaires
     lblLineOrder.Caption:=cLineOrderStr[LineOrder];
     lblBitsCount.Caption:=BitCount.ToString;
     lblHasAlpha.Caption:= HasAlpha.ToString('Oui','Non');

     // Infos "BitFields"
     With BitFields do
     begin
       lblRedMask.Caption:=RedMask.ToHexString; //IntToHex(RedMask,8);
       lblGreenMask.Caption:=GreenMask.ToHexString; //IntToHex(GreenMask,8);
       lblBlueMask.Caption:=BlueMask.ToHexString;//IntToHex(BlueMask,8);
       lblAlphaMask.Caption:=AlphaMask.ToHexString;//IntToHex(AlphaMask,8);
       lblRedShift.Caption:=RedShift.ToString();
       lblGreenShift.Caption:=GreenShift.ToString();
       lblBlueShift.Caption:=BlueShift.ToString();
       lblAlphaShift.Caption:=AlphaShift.ToString();
       lblRedSize.Caption:=RedSize.ToString;
       lblGreenSize.Caption:=GreenSize.ToString;
       lblBlueSize.Caption:=BlueSize.ToString;
       lblAlphaSize.Caption:=AlphaSize.ToString;
     end;
   end;

   With ImageDescription do
   begin
     lblHasPalette.Caption:= UsePalette.ToString('Oui','Non');
     lblPaletteCount.Caption:= PaletteCount.ToString();
     lblIsInterlaced.Caption:= Interlaced.ToString('Oui','Non');
   end;
  end;
end;
procedure ShowBitmapInfos(Const ABitmap : TBZCustomBitmap);

begin
  Try
    BZBitmapInfosForm:=TBZBitmapInfosForm.Create(nil);
    With BZBitmapInfosForm do
    begin
      FillInfos(ABitmap);
      ShowModal;
    end;
  finally
    BZBitmapInfosForm.Free;
  end;
end;


end.

