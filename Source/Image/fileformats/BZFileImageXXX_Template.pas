(*
  @created(2020-02-06)
  @author(J.Delauney (BeanzMaster))
  Historique : @br
  @unorderedList(
    @item(06/02/2020 : Creation  )
  )
--------------------------------------------------------------------------------@br

     @bold(Description :)@br
     Prise en charge des Images au format XXX proposé XXXXXXX en lecture et écriture.

     Spécifications :
     @unorderedList(       
       @item(Méthode de compression    :  )
       @item(Nombre de couleurs	       : 24 / 32 bits  : (RGB/A, BGR/A, YUV) )
       @item(Supporte plusieurs images : Non )
       @item(Format des nombres	       : - )
       @item(Auteur	                   : )
       @item(Extensons                 : *.xxx; 
     )

  ------------------------------------------------------------------------------@br
  @bold(Notes :)@br

    Basé sur la bibliothèque externe "LiWebp"

    Informations sur le format WEBP :
    @unorderedList(
       @item()
       @item()
       @item()
       @item()
    )

    Autres informations utiles :

         Fichiers test :@br
           - 


  ------------------------------------------------------------------------------
  @bold(BUGS :)@br

  ------------------------------------------------------------------------------
  @bold(TODO :)@br
  @unorderedList(

  )
  ------------------------------------------------------------------------------@br

  @bold(Credits :)
    - Tous les liens au dessus


  ------------------------------------------------------------------------------@br
  @bold(LICENCE :) MPL / GPL @br
  @br
 *============================================================================== *)
unit BZImageFileWEBP;

//==============================================================================
{$mode objfpc}{$H+}
{$i ..\..\bzscene_options.inc}
//==============================================================================

interface

uses
  Classes, SysUtils,
  BZClasses, BZGraphic, BZBitmap, BZImageFileIO;



Type

  { TBZBitmapXXXImage }

  TBZBitmapXXXImage = Class(TBZCustomImageFileIO)
  private
    //FHeader : ???;
    FBmpWidth, FBmpHeight : Integer;
    
  protected

    Procedure LoadFromMemory(); override;
    Procedure SaveToMemory; override;

    Function CheckFormat(): Boolean; override;
    Function ReadImageProperties: Boolean; override;
  public

    Constructor Create(AOwner: TPersistent; AWidth, AHeight: Integer); override;
    Destructor Destroy; override;

    Class Function Capabilities: TBZDataFileCapabilities; override;

    //Function getImagePropertiesAsString: String; override;


  end;

implementation

uses
  BZImageStrConsts,
  BZUtils;



{ TBZBitmapWEBPImage }

Constructor TBZBitmapWEBPImage.Create(AOwner: TPersistent; AWidth, AHeight: Integer);
Begin
  Inherited Create(AOwner, AWidth, AHeight);
  With DataFormatDesc Do
  Begin
    Name := 'XXX';
    Desc := 'xxxxxx';  // , icone, curseur';
    FileMask := '*.xxx'; //*.ico;*.cur;
    Version := '';
    Encoding := etNone;
  End;

  SupportedColorFormat := SupportedColorFormat + [cfBGR, cfRGB, cfRGBA, cfBGRA];
End;

Destructor TBZBitmapWEBPImage.Destroy;
Begin
  SupportedColorFormat := [];
  Inherited Destroy;
End;

Procedure TBZBitmapWEBPImage.LoadFromMemory();
begin
  //if Ok then FreeMem(TmpBuf);
end;

Procedure TBZBitmapWEBPImage.SaveToMemory;
begin
  inherited SaveToMemory;
end;

Function TBZBitmapWEBPImage.CheckFormat() : Boolean;
begin
  Result := False;
  // Lecture de l'entête ici
  if Result then
  begin
    Result := ReadImageProperties;
  end
  else
  begin
    Result := False;
    RaiseInvalidImageFile(Format(rsBadSignature,['XXXX']));
  end;
end;

Function TBZBitmapWEBPImage.ReadImageProperties : Boolean;

begin
  Result := True;

  With DataFormatDesc do
  begin
    Version := '0.0.0';
  end;
  ImageDescription.InitDefault(FBmpWidth, FBmpHeight, 32);

end;

Class Function TBZBitmapWEBPImage.Capabilities : TBZDataFileCapabilities;
begin
  Result := [dfcRead, dfcWrite];
end;

Initialization

  RegisterRasterFormat('xxx', 'xxx Image format', TBZBitmapXXXImage);

Finalization

  if LibWebPIsLoaded then
  begin
    LibWebPUnload;
    LibWebPIsLoaded:= false;
  end;

  UnregisterRasterFormat(TBZBitmapWEBPImage);

end.
