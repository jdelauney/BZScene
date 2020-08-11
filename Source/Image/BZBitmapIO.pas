(*
  @abstract(Unité de convenance pour la prise en charge de tous les formats image supportés)

  -------------------------------------------------------------------------------------------------------------

  @created(2018-07-11)
  @author(J.Delauney (BeanzMaster))
  Historique : @br
  @unorderedList(
    @item(11/07/2018 : Creation)
    @item(11/07/2018 : Dernière mise à jour)
  )

  -------------------------------------------------------------------------------------------------------------

  @bold(Notes) :

  -------------------------------------------------------------------------------------------------------------

  @bold(Dependances) : BZImageFileBMP, BZImageFileXPM, BZImageFileTGA, BZImageFilePCX,
                       BZImageFilePPM, BZImageFileJPEG, BZImageFilePNG, BZImageFileGIF, BZImageFileWEBP

  -------------------------------------------------------------------------------------------------------------

  @bold(Credits) :

  -------------------------------------------------------------------------------------------------------------

  LICENCE : MPL / GPL

  ------------------------------------------------------------------------------------------------------------- *)
Unit BZBitmapIO;

//==============================================================================
{$mode objfpc}{$H+}
{$i ..\bzscene_options.inc}
//==============================================================================


Interface

Uses
  Classes, Sysutils,

  BZImageFileBMP, BZImageFileXPM, BZImageFileTGA, BZImageFilePCX, BZImageFilePPM,
  BZImageFileJPEG, BZImageFilePNG, BZImageFileGIF, BZImageFileWEBP; {, BZImageFileTIFF;}

 { TODO -oBZBitmap -cIO : Prise en charge de nouveaux formats de fichier image
    - BZImageFileICO
    - BZImageFileTIFF
    - BZImageFilePSD
    - BZImageFileXCF
    - BZImageFileDDS
    - BZImageFileKTX

    - BZImageFileRAW
    - BZImageFilePCD
    - BZImageFileHDR
    - etc...
  }

Implementation

End.

