(*
  @abstract(Regroupe les messages d'erreurs et autres en français.
  Utile pour la translation vers un autre langage. @br  )

  --------------------------------------------------------------------------------

  @created(2018-07-11)
  @author(J.Delauney (BeanzMaster))
  Historique : @br
  @unorderedList(
    @item(11/07/2018 : Creation)
    @item(05/05/2020 : Dernière mise à jour)
  )

  --------------------------------------------------------------------------------

  @bold(Notes) :

  --------------------------------------------------------------------------------

  @bold(Dependances) : -

  --------------------------------------------------------------------------------

  @bold(Credits :)
   @unorderedList(
     @item ()
     @item(J.Delauney (BeanzMaster))
   )

  --------------------------------------------------------------------------------

  @bold(LICENCE) : MPL / GPL

  -------------------------------------------------------------------------------- *)
Unit BZImageStrConsts;

{$mode objfpc}{$H+}

Interface

ResourceString

  // Messages d'erreurs ou de notifications / Error or notification messages
  rsFileNotFound             = 'Le fichier %s est introuvable !';
  rsResourceNotFound         = 'Resource %s not found !';
  rsBadFileSize              = 'Taille du fichier invalide : %s';
  rsFileDecodingError        = 'Erreur de décodage du fichier';
  // Erreurs communes TBZBitmap
  rsBitmapCreateError        = 'Erreur lors de la création du TBitmap';
  rsBitmapScanlineOutOfRange = 'Scanline : Indice hors limite';
  rsCompressionMethodInvalid = 'Méthode de compression invalide';


  // Erreurs communes aux formats d'images
  rsBitmapBitPerPixelNotSupported = 'Bit par pixel : %d non supporté';
  rsBadImageSize             = 'Dimension de l''image Invalides : %dx%d';
  rsTooManyColorInPalette    = 'Trop de couleur dans la palette %d > %d';
  rsBadSignature             = 'Signature invalide : %s';
  rsEmptyColorMap            = 'Erreur aucune palette de couleur disponible pour cette image !';
  rsEmptyImage               = 'L''Image est vide';
  rsUnknownVersion           = 'Version inconnue';
  rsBitmapBadPaletteIndex    = 'Index de palette de couleur invalide : %d ';

  // Messages d'erreur BZImageXPM
  rsInvalidTagXPMExt        = 'Mauvaise définition du Tag XPMEXT';
  // Messages d'erreur BZImageBMP
  rsInvalidHeaderSize          = 'Taille de l''en-tête invalide = %d';
  rsPNGCompressionNotSupported = 'Compression PNG non supportée';
  rsHUFFMAN1DCompressionNotSupported = 'Compression HUFFMAN 1D non supportée';
  rsJPGCompressionNotSupported = 'Compression JPEG non supportée';
  rsBMPRLE8BitInvalidInputBufferSize = 'Fin prématurée des données compressées au format RLE 8 bits';
  rsBMPRLE8BitDataCorrupted    = 'Données compressées au format RLE 8 bits corrompues.';
  rsBMPRLE4BitInvalidInputBufferSize = 'Fin prématurée des données compressées au format RLE 4 bits';
  rsBMPRLE4BitDataCorrupted    = 'Données compressées au format RLE 4 bits corrompues.';
  rsBMPBadHeaderPalcount       = 'Nombre de couleur dans la palette définis dans l''en-tête invalide %d au lieu de %d';

  // Messages d'erreur BZImageGIF
  rsScreenBadColorSize       = 'Nombre de couleur dans la palette globale invalide.';
  rsImageBadColorSize        = 'Nombre de couleur dans la palette locale invalide.';
  rsGIFBufferOverFlow           = 'Image #%d : Le décodeur s''est arrêté pour empêcher un débordement de tampon';
  rsGIFInvalidOutputBufferSize  = 'Image #%d : La taille du tampon de sortie est invalide ( Taille <= 0)';
  rsGIFInvalidInputBufferSize   = 'Image #%d : La taille du tampon d''entrée est invalide ( Taille <= 0)';
  rsGIFInvalidBufferSize        = 'Image #%d : La taille du tampon d''entrée et de sortie sont invalides ( Taille <= 0)';
  rsLZWInternalErrorOutputBufferOverflow = 'Dépassement du buffer de sortie dans le décodeur GIF LZW. Signaler ce bug. C''est un bug sérieux !';
  rsLZWInternalErrorInputBufferOverflow  = 'Dépassement du buffer d''entrée dans le décodeur GIF LZW. Signaler ce bug. C''est un bug sérieux !';
  rsLZWInvalidInput          = 'Image #%d : Le décodeur a rencontré une entrée invalide (données corrompues)';
  rsLZWOutputBufferTooSmall  = 'Image #%d : Le décodeur n''a pas pu décoder toutes les données car le tampon de sortie est trop petit';


  // Message d'etats
  rsLoadingPalette       = 'Chargement de la palette de couleurs';
  rsLoadingBitmapData    = 'Chargement des données';
  rsDecompressFrameData  = 'Décompression des données de l''image #%d';

Implementation

End.

