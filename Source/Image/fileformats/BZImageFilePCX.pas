(*
  @abstract(Prise en charge des Images au format PCX ZSoft PaintBrush)

  Spécifications : @br
    @unorderedList(
      @item(Méthode de compression    : Aucune, RLE)
      @item(Nombre de couleurs	      :  1, 2, 4, 8 bits format RGB, 24 bits ou 32 bits RGB(A)
      @item(Supporte plusieurs images : Non, une seule image dans un même fichier, sauf DCX)
      @item(Format des nombres	      : Big-endian )
      @item(Auteur                    : ZSoft PaitBrush.)
      @item(Extensions                : *.pcx, *.pcc, *.scr )
    )

  -------------------------------------------------------------------------------------------------------------

  @created(2017-04-30)
  @author(J.Delauney (BeanzMaster))
  Historique : @br
  @unorderedList(
    @item(30/04/2017 : Creation  )
  )

  -------------------------------------------------------------------------------------------------------------

  @bold(Notes) : @br
    Informations sur le format PCX : @br
      @unorderedList(
        @item(https://fr.wikipedia.org/wiki/PCX)
        @item(https://en.wikipedia.org/wiki/PCX)
        @item(http://www.fileformat.info/format/pcx/corion.htm)
        @item(http://www.fileformat.info/format/pcx/egff.htm)
        @item(http://www.drdobbs.com/pcx-graphics/184402396)
        @item(https://rkwee.home.xs4all.nl/Variousfiles/pcx.format.html)
      )

    Autres informations utiles : @br
      @unorderedList(
        @item(http://fileformats.archiveteam.org/wiki/PCX)
      )

    Fichiers test :@br
      @unorderedList(
        @item(http://www.fileformat.info/format/pcx/sample/index.htm)
        @item(http://www.fileformatcommons.com/pcx-file-format/)
      )

  -------------------------------------------------------------------------------------------------------------

  @bold(Dépendances) : BZClasses, BZColors, BZGraphic, BZBitmap, BZImageFileIO, BZBitmapCompression

  -------------------------------------------------------------------------------------------------------------

  @bold(Credits) : @br
    @unorderedList(
      @item(FPC/Lazarus)
    )

  -------------------------------------------------------------------------------------------------------------

  @bold(Licence) : MPL / GPL

  ------------------------------------------------------------------------------------------------------------- *)
Unit BZImageFilePCX;

{
}
//==============================================================================
{$mode objfpc}{$H+}
{$i ..\..\bzscene_options.inc}
//==============================================================================

//------------------------------------------------------------------------------
//----------------------------[ TODO LIST ]-------------------------------------

{ TODO 3  -oBZBitmap -cSupport_Images_PCX : Ecriture Format PCX }
{ TODO 3  -oBZBitmap -cSupport_Images_PCX : Support format DCX multi-images [*.DCX] }

//------------------------------------------------------------------------------
interface

uses
  Classes, SysUtils, Dialogs,
  BZClasses,
  BZColors, BZGraphic, BZBitmap,
  BZImageFileIO, BZBitmapCompression;


type
  { PBZPCXHeader : En-tête fichier PCX
     - FileID:   byte;   signature $0A fichiers PCX, $CD fichiers SCR
  }
  TBZPCXHeader = packed record
   // FileID:   byte;
    Version       :  byte;            //< 0: version 2.5, 2: 2.8 avec palette, 3: 2.8 sans palette, 5: version 3@br
    Encoding      : byte;             //< Compression
                                      //< 0: non compresser 1: encodage RLE
    BitsPerPixel  : byte;             //< nombre de bits par pixel de l'image: 1, 4, 8, 24
    XMin          : word;             //< abscisse de l'angle supérieur gauche
    YMin          : word;             //< ordonnée de l'angle supérieur gauche
    XMax          : word;             //< abscisse de l'angle inférieur droit
    YMax: word;                       //< ordonnée de l'angle inférieur droit
    HRes: word;                       //< résolution horizontale en dpi
    VRes: word;                       //< résolution verticale en dpi
    ColorMap: array[0..15] of TBZColor24;    //< Palette 16 couleurs
    Reserved : Byte;                   //< Réservé                     NOTE : Ici c'est bizarre car normalement reserved est avant colorplane. Il y a inversion à la lecture, mais je ne sais pas pourquoi
    ColorPlanes: byte;                 //< Nombre de plans de couleur
    BytesPerLine : Word;               //< Nombre de bits par ligne
    PaletteType  : word;               //< Type de palette : 1 = couleur ou N/B, 2 = dégradé de gris, 0 = peux-etre utilisé dans le cas de fichier dont l'en-tête est incorrect
    Fill:     array[0..57] of byte;    //< Réservé
  end;

Type
  { Classe de lecture et d"ecriture des images au format PCX }
  TBZBitmapPCXImage = Class(TBZCustomImageFileIO)
  private

  protected
    Header : TBZPCXHeader;
    ABitCount:Byte;
    bmpWidth, bmpHeight : Integer;

    Function CheckFormat(): Boolean; override;
    Function ReadImageProperties: Boolean; override;
    Procedure LoadFromMemory(); override;
  public

    Constructor Create(AOwner: TPersistent; AWidth, AHeight: Integer); override;
    Destructor Destroy; override;

    Class Function Capabilities: TBZDataFileCapabilities; override;

    Function getImagePropertiesAsString: String; override;
  end;

implementation

Uses
  BZUtils;

{%region%=====[ TBZBitmapXXXImage ]============================================}

Constructor TBZBitmapPCXImage.Create(AOwner: TPersistent; AWidth, AHeight: Integer);
Begin
  Inherited Create(aOwner, AWidth, AHeight);

  With DataFormatDesc Do
  Begin
    Name := 'ZSoft PCX';
    Desc := 'ZSoft Paint Brush image';
    FileMask := '*.pcx; *.pcc; *.scr';
    Version := '0.0';
    Encoding := etNone;
  End;
end;

Destructor TBZBitmapPCXImage.Destroy;
Begin
  SupportedColorFormat := [];
  Inherited Destroy;
End;

Class Function TBZBitmapPCXImage.Capabilities: TBZDataFileCapabilities;
Begin
  Result := [dfcRead]; //[dfcRead, dfcWrite]
End;

Function TBZBitmapPCXImage.getImagePropertiesAsString: String;
Var
  S: String;
Begin
  S := 'na';
  Result:=S;
end;

Function TBZBitmapPCXImage.ReadImageProperties: Boolean;
Begin
  Result := True;
  With Header do
  Begin
    // On Calcul les dimensions et le nombre de bit par pixel
    if (BitsPerPixel=4) and (ColorPlanes=4) then ABitCount := 4 // /!\ Cas spécial
    else if (BitsPerPixel=1) and (ColorPlanes=3) then ABitCount := 4  // /!\ 2eme Cas spécial  (car 1*3 = 3 donc bitcount erroné)
    else ABitCount  := BitsPerPixel * ColorPlanes;

    bmpWidth  := abs(XMax - XMin + 1);
    bmpHeight := abs(YMax - YMin + 1);
  end;

  With ImageDescription do
  begin
    // On initialise la descritption du "ImageDescription"
    InitDefault(bmpWidth, bmpHeight, ABitCount);
  end;
  // On met à jour quelques infos
  With DataFormatDesc Do
  Begin
    Version := Inttostr(Header.Version);
    if  Header.Encoding = 1 then Encoding := etRLE
    else Encoding := etNone;
  End;
End;

Function TBZBitmapPCXImage.CheckFormat(): Boolean;
Var
  Id : Byte;
Begin
  Result :=  False;
  Id := Memory.ReadByte;
  if (Id in [$0A,$0C]) then
  begin
    DataFormatDesc.Name:='Format PCX';

    Memory.Read(Header,Sizeof(TBZPCXHeader));
    With Header do
    begin
     if (Version      in [0, 2, 3, 4, 5]) and
        (Encoding     in [0..1]) and
        (BitsPerPixel in [1, 2, 4, 8]) and
        (ColorPlanes  in [1, 3, 4]) and
        (PaletteType  in [0..2]) then Result:=ReadImageProperties; // Tout est ok on passe à la suite
    end;
  end;
  if Result=False then
    begin
      RaiseInvalidImageFile('Format PCX Invalide');
    end;
End;

Procedure TBZBitmapPCXImage.LoadFromMemory();
Var
  UnpackSize,DataSize, OldPos : Int64;
  LineSize : Longint;
  Marker, Idx: Byte;
  TempBuffer, pR, pG, pB, pA   : PByte;
  RGBEntry : TBZColor24;
  C : TBZColor;
  SrcPtr : PByte;
  DstPtr : PBZColor;
  DstColor : TBZColor;
  I, X,Y, YY, iL2, iL3 :Integer;
  RLEEncoder : TBZDataEncoderRLE_PCX;
  Delta:Single;

 procedure Create16ColorsPalette;
 Var
    I : Byte;
 begin
   ImageDescription.PaletteCount:=16;
   // TODO : if Header.Version = 3 and PaletteType =0 then begin PaletteSystem; exit; end else

   For I:=0 To 15 Do
   Begin
     With ImageDescription.PaletteEntries^[I] do
     begin
       if Header.Version = 3 then
       begin
         Red:=Header.ColorMap[I].Red shl 2;
         Green:=Header.ColorMap[I].Green shl 2;
         Blue:=Header.ColorMap[I].Blue shl 2;
         Alpha:=255;
       end
       else
       begin
         Red:=Header.ColorMap[I].Red;
         Green:=Header.ColorMap[I].Green;
         Blue:=Header.ColorMap[I].Blue;
         Alpha:=255;
       end;
     end;
   end;
  end;

Begin
  SetSize(bmpWidth, bmpHeight);

  InitProgress(bmpWidth,bmpHeight);
  StartProgressSection(0, ''); // On debute une nouvelle section globale
  Delta := 100 / Height;
  StartProgressSection(100 ,'Chargement de l''image');

  // On initiialise le moteur de Compression/Decompression
  RLEEncoder := TBZDataEncoderRLE_PCX.Create(Self);
  // Taille des données brute totale
  DataSize := Memory.Size - Memory.position;
  // Taille des données brute d'UNE ligne
  LineSize := int64(Header.ColorPlanes) * int64(Header.BytesPerLine);
  // On charge ou on initialise la palette de couleur en fonction du nombre de bits de l'image, si besoin.
  Case ImageDescription.PixelFormat of //Header.BitsPerPixel of
    pf1bit : //1 :
    begin
      CreateBWRawPalette;
    end;
    pf2bits, pf4bits :
    begin
      Create16ColorsPalette;
    end;
    pf8Bits :
    begin
      Dec(DataSize,768);
      // Chargement palette
      ImageDescription.PaletteCount:=256;
      if (Header.PaletteType = 2) then  CreateGrayRawPalette  // Image en niveaux de gris
      else
      begin
        // On va chercher la palette à la fin du fichier
        OldPos := Memory.position;
        Memory.Seek(768,soEnd);
        Marker:=Memory.ReadByte;
        if (Marker <> $0C) then  // On verifie le marqueur
        begin
          // Le marqueur est invalide. C'est peux être un image en niveau de gris
          if Header.PaletteType = 2 then CreateGrayRawPalette
          else
          begin
            RaiseInvalidImageFile('Erreur lors du chargement de la palette.'+#13+#10+'Marqueur Palette PCX invalide');
          end;
        end
        else
        begin  // Le marqueur est valide, on charge la palette
          RGBEntry.Red:=0;RGBEntry.Green:=0;RGBEntry.Blue:=0;
          for I := 0 to 255 do
          begin
            Memory.Read(RGBEntry, 3); //SizeOf(RGBEntry));
            with c do
            begin
              Red   := RGBEntry.Red;
              Green := RGBEntry.Green;
              Blue  := RGBEntry.Blue;
              Alpha := 255;
            end;
           ImageDescription.PaletteEntries^[I]:= C;
          end;
          Memory.Seek(OldPos,soBeginning); // On retourne à notre position d'origine
        end;
      end;
    end;
  end;

  // On copie ou on decompresse les données depuis le flux en 1 fois
  UnPackSize := (LineSize *  int64(ImageDescription.Height));;
  TempBuffer:=nil;
  ReAllocMem(TempBuffer, UnPackSize);
  Try
    // Lecture des données
    // Les données sont elles compressées ?
    If Header.Encoding = 1 then
      RLEEncoder.Decode(TempBuffer,UnPackSize)
    else
      Memory.Read(TempBuffer,UnPackSize);

    // On transfert les données vers notre bitmap
    SrcPtr := PByte(TempBuffer); // On se place au debut
    Case ImageDescription.PixelFormat of
      pf1Bit :
      begin

        DstPtr:= GetScanLine(0); // GetBuffer
        X:=0; Y:=0;
        While (Y<MaxHeight) do
        begin
          // Blanc ou Noir ?
          //if ((PByte(SrcPtr+(X div 8))^ shr (7-(X and 7)) ) and 1)<>0 then Idx:=1 else Idx:=0;
          Idx := ExtractPixel1Bit(PByte(SrcPtr+(X div 8))^,X);
          With DstColor do     // on va chercher la couleur dans la palette
          Begin
            Red := ImageDescription.PaletteEntries^[Idx].Red;
            Green := ImageDescription.PaletteEntries^[Idx].Green;
            Blue := ImageDescription.PaletteEntries^[Idx].Blue;
            Alpha := ImageDescription.PaletteEntries^[Idx].Alpha;
          end;

          DstPtr^:= DstColor;
          Inc(X);
          Inc(DstPtr);

          If X>MaxWidth then
          begin
            While X < LineSize do // Padding comme dans le format BMP
            begin
              Inc(X);
            end;
            If X>=LineSize then
            begin
              X:=0;
              Inc(Y);
              AdvanceProgress(Delta,0,1,False);
              SrcPtr := PByte(TempBuffer+(Y*LineSize)); // On se place au debut de la ligne
            end;
          end;
        end;
      end;
      pf2Bits :
      begin
        DstPtr:= GetScanLine(0); // GetBuffer
        X:=0; Y:=0;
        While (Y<MaxHeight) do
        begin
          // Blanc ou Noir ?
          //if ((PByte(SrcPtr+(X div 8))^ shr (7-(X and 7)) ) and 1)<>0 then Idx:=1 else Idx:=0;
          Idx := ExtractPixel2Bits(PByte(SrcPtr+(X div 4))^,X);
          With DstColor do     // on va chercher la couleur dans la palette
          Begin
            Red := ImageDescription.PaletteEntries^[Idx].Red;
            Green := ImageDescription.PaletteEntries^[Idx].Green;
            Blue := ImageDescription.PaletteEntries^[Idx].Blue;
            Alpha := ImageDescription.PaletteEntries^[Idx].Alpha;
          end;

          DstPtr^:= DstColor;
          Inc(X);
          Inc(DstPtr);

          If X>MaxWidth then
          begin
            While X < LineSize do // Padding comme dans le format BMP
            begin
              Inc(X);
            end;
            If X>=LineSize then
            begin
              X:=0;
              Inc(Y);
              AdvanceProgress(Delta,0,1,False);
              SrcPtr := PByte(TempBuffer+(Y*LineSize)); // On se place au debut de la ligne
            end;
          end;
        end;
      end;
      pf4Bits:
      begin
        iL2 := Header.BytesPerLine * 2;
        iL3 := Header.BytesPerLine * 3;
        pR := SrcPtr;
        Inc(pR, Header.BytesPerLine);
        pG := SrcPtr;
        Inc(pG, iL2);
        pb := SrcPtr;
        Inc(pB, iL3);

        DstPtr:= GetScanLine(0); // GetBuffer
        X:=0; Y:=0;
        While (Y<MaxHeight) do
        begin

          Idx := 0;
          Case Header.ColorPlanes of
            1:
            begin
              Idx := ExtractPixel4Bits(PByte(SrcPtr+(X div 2))^,X);
              //if odd(x) then idx:=Lo((PByte(SrcPtr+(X div 2)))^) else idx:=Hi((PByte(SrcPtr+(X div 2)))^);
            end;
            3:
            begin
              if ((PByte(SrcPtr+(X div 8))^ shr (7-(X and 7))) and 1) <> 0 then Inc(Idx, 1);
              if ((PByte(pR+(X div 8))^ shr (7-(X and 7))) and 1) <> 0 then Inc(Idx, 2);
              if ((PByte(pG+(X div 8))^ shr (7-(X and 7))) and 1) <> 0 then Inc(Idx, 4);
            end;
            4:
            begin
              if ((PByte(SrcPtr+(X div 8))^ shr (7-(X and 7))) and 1) <> 0 then Inc(Idx, 1);
              if ((PByte(pR+(X div 8))^ shr (7-(X and 7))) and 1) <> 0 then Inc(Idx, 2);
              if ((PByte(pG+(X div 8))^ shr (7-(X and 7))) and 1) <> 0 then Inc(Idx, 4);
              if ((PByte(pB+(X div 8))^ shr (7-(X and 7))) and 1) <> 0 then Inc(Idx, 8);
            end;
          end;

          With DstColor do     // on va chercher la couleur dans la palette
          Begin
            Red := ImageDescription.PaletteEntries^[Idx].Red;
            Green := ImageDescription.PaletteEntries^[Idx].Green;
            Blue := ImageDescription.PaletteEntries^[Idx].Blue;
            Alpha := ImageDescription.PaletteEntries^[Idx].Alpha;
          end;

          DstPtr^:= DstColor;
          Inc(X);
          Inc(DstPtr);

          If X>MaxWidth then
          begin
            While X < LineSize do // Padding comme dans le format BMP
            begin
              Inc(X);
            end;
            If X>=LineSize then
            begin
              X:=0;
              Inc(Y);
              AdvanceProgress(Delta,0,1,False);
              SrcPtr := PByte(TempBuffer+(Y*LineSize)); // On se place au debut de la ligne
              pR := SrcPtr;
              Inc(pR, Header.BytesPerLine);
              pG := SrcPtr;
              Inc(pG, iL2);
              pb := SrcPtr;
              Inc(pB, iL3);
            end;
          end;
        end;
      end;
      pf8Bits:
      begin
        DstPtr:= GetScanLine(0); // GetBuffer
        SrcPtr := PByte(TempBuffer);

        X:=0; Y:=MaxHeight;
        While (Y>0) do
        begin
          Idx := SrcPtr^;
          With DstColor do     // on va chercher la couleur dans la palette
          Begin
            Red := ImageDescription.PaletteEntries^[Idx].Red;
            Green := ImageDescription.PaletteEntries^[Idx].Green;
            Blue := ImageDescription.PaletteEntries^[Idx].Blue;
            Alpha := ImageDescription.PaletteEntries^[Idx].Alpha;
          end;

          DstPtr^:= DstColor;
          Inc(X);
          Inc(SrcPtr);
          Inc(DstPtr);
          If X>MaxWidth then
          begin
            While X < LineSize do // Padding comme dans le format BMP
            begin
              Inc(X);
              Inc(SrcPtr);
            end;
            If X>=LineSize then
            begin
              X:=0;
              Dec(Y);
              AdvanceProgress(Delta,0,1,False);
            end;
          end;
        end;
      end;
      pf24bits :
      begin
        // Happy Compilo
        pR:=nil;
        pG:=nil;
        pB:=nil;
        X:=0; Y:=MaxHeight;
        YY:=0;

        pR := PByte(TempBuffer);
        pG := PByte(TempBuffer+(Header.BytesPerLine));
        pB := PByte(TempBuffer+(2*Header.BytesPerLine));

        DstPtr:= GetScanLine(0); // GetBuffer

        While (Y>0) do
        begin

          With DstColor do
          Begin
            Red   := pR^;
            Green := pG^;
            Blue  := pB^;
            Alpha := 255;
          end;

          DstPtr^:= DstColor;
          Inc(X);
          Inc(DstPtr);

          If X>MaxWidth then
          begin
            While X < Header.BytesPerLine do // Padding comme dans le format BMP
            begin
              Inc(X);
              Inc(pR);Inc(pG);Inc(pB);
            end;
            If X>=Header.BytesPerLine then
            begin
              X:=0;
              Dec(Y);
              AdvanceProgress(Delta,0,1,False);
              YY :=(MaxHeight-Y)*3;
              pR := PByte(TempBuffer+(YY*Header.BytesPerLine));
              pG := PByte(TempBuffer+((YY+1)*Header.BytesPerLine));
              pB := PByte(TempBuffer+((YY+2)*Header.BytesPerLine));
            end;
          end
          else
          begin
             Inc(pR);Inc(pG);Inc(pB);
          end;
        end;
      end;
       // A l'origine le format PCX ne supporte pas le 32bits, mais....
      pf32bits :
      begin
        // Happy Compilo
         pR:=nil;
         pG:=nil;
         pB:=nil;
         pA:=nil;
         X:=0; Y:=MaxHeight;
         YY:=0;

         pR := PByte(TempBuffer);
         pG := PByte(TempBuffer+(Header.BytesPerLine));
         pB := PByte(TempBuffer+(2*Header.BytesPerLine));
         pA := PByte(TempBuffer+(3*Header.BytesPerLine));

         DstPtr:= GetScanLine(0); // GetBuffer

         While (Y>0) do
         begin

           With DstColor do
           Begin
             Red   := pR^;
             Green := pG^;
             Blue  := pB^;
             Alpha := pA^;
           end;

           DstPtr^:= DstColor;
           Inc(X);
           Inc(DstPtr);

           If X>MaxWidth then
           begin
             While X < Header.BytesPerLine do // Padding comme dans le format BMP
             begin
               Inc(X);
               Inc(pR);Inc(pG);Inc(pB); Inc(pA);
             end;
             If X>=Header.BytesPerLine then
             begin
               X:=0;
               Dec(Y);
               AdvanceProgress(Delta,0,1,False);
               YY :=(MaxHeight-Y)*4;
               pR := PByte(TempBuffer+(YY*Header.BytesPerLine));
               pG := PByte(TempBuffer+((YY+1)*Header.BytesPerLine));
               pB := PByte(TempBuffer+((YY+2)*Header.BytesPerLine));
               pA := PByte(TempBuffer+((YY+3)*Header.BytesPerLine));
             end;
           end
           else
           begin
              Inc(pR);Inc(pG);Inc(pB);Inc(pA);
           end;
         end;
      end;
    else
      begin
        RaiseInvalidImageFile('Format de bits non supporté');
      end;
    end;
  finally
    FreeMem(TempBuffer);
    TempBuffer:=nil;
  end;
  FreeAndNil(RLEEncoder);
  FinishProgressSection(False);
  FinishProgressSection(True);
End;

{%endregion%}

Initialization

  RegisterRasterFormat('pcx', 'ZSoft Paintbrush PCX Image', TBZBitmapPCXImage);
  RegisterRasterFormat('pcc', 'ZSoft Paintbrush PCX Image', TBZBitmapPCXImage);
  RegisterRasterFormat('scr', 'ZSoft Paintbrush PCX Image', TBZBitmapPCXImage);

Finalization
  UnregisterRasterFormat(TBZBitmapPCXImage);

end.

