(*
  Définit et regroupe les systèmes de couleurs les types de filtres
  et autres données utiles à la gestion du graphisme. @br
  Contient les classes de base à la gestion des informations sur les formats d'image

  --------------------------------------------------------------------------------

  @created(2018-07-11)
  @author(J.Delauney (BeanzMaster))
  Historique : @br
  @unorderedList(
    @item(11/07/2018 : Creation)
    @item(11/07/2018 : Dernière mise à jour)
  )

  --------------------------------------------------------------------------------

  @bold(Notes) :

  --------------------------------------------------------------------------------

  @bold(Dependances) : BZSystem, BZUtils, BZClasses, BZMath, BZVectorMath, BZAnimationTool

  --------------------------------------------------------------------------------

  @bold(Credits :)
   @unorderedList(
     @item ()
     @item(J.Delauney (BeanzMaster))
   )

  --------------------------------------------------------------------------------

  @bold(LICENCE) : MPL / GPL

  -------------------------------------------------------------------------------- *)
unit BZGraphic;

//==============================================================================
{$mode objfpc}{$H+}
{$i ..\bzscene_options.inc}

//-----------------------------
{$INLINE ON}
{$MODESWITCH ADVANCEDRECORDS}
//-----------------------------

//==============================================================================

interface

uses
  Classes, SysUtils, Types, GraphType, Graphics, LCLIntf, LCLType,   fpImage, dialogs,

  {$IFDEF WINDOWS}Windows, Win32Extra,{$ENDIF}
  //{$IFDEF LCLGTK3}Cairo,FileUtil,{$ENDIF}
  {$IFDEF LCLGTK2}GDK2, Gtk2Def,FileUtil,{$ENDIF}
  {$IFDEF LCLQT4}QT4, QTWidgets, QTObjects,{$ENDIF}
  //{$IFDEF LCLQT5}QT4, QTWidgets, QTObjects,{$ENDIF}
  {$IFDEF LCLCARBON}MacOSAll, CarbonCanvas,{$ENDIF}

  BZClasses, BZMath, BZVectorMath, BZColors;
  //BZAnimationTool;

//---------------------------------------------------------------------------------------------------

{%region%=====[ Constantes et types utiles ]=========================================}

  {.$i BZGraphicTypes.Inc}

Type
  { Exeption levée relative aux Bitmaps}
  EBZBitmapException = Class(EBZBaseException);
  { Exception lévée pour un format d'image invalide }
  EBZInvalidImageFormatException = Class(EBZBitmapException);

  { Type de convenance pour définir un point 2D de type Integer }
  TBZPoint = TBZVector2i;
  { Pointeur vers un TBZPoint }
  PBZPoint = ^TBZPoint;


{%endregion%}

{%region%=====[ Définitions de Rect personnalisé ]===================================}

  { Tableau pour la représentation d'un rectangle virtuel (Integer) }
  TBZRectVectorType = packed array[0..3] of Integer;

  { Définition d'un rectangle virtuel en valeur entière }
  TBZRect = record
  private
    function GetWidth : LongInt;
    function GetHeight : LongInt;
    procedure setWidth(AValue: Longint);
    procedure setHeight(AValue: Longint);
  public
    { Creation de l'objet TBZRect avec les paramètres "aLeft,aTop,aRight,aBottom" de type LongInt }
    procedure Create(Const aLeft,aTop,aRight,aBottom : Longint ); overload;
    { Creation de l'objet TBZRect avec les paramètres "aLeft,aTop,aRight,aBottom" de type LongInt
      "DoNormalize" indique si les postions Top et Left doivent être interchanger si plus elles sont plus grandes que Bottom et Right }
    procedure Create(Const aLeft,aTop,aRight,aBottom : Longint; Const DoNormalize : Boolean ); overload;
    { Creation de l'objet TBZRect avec les paramètres "aLeftTop,aRightaBottom".
      "DoNormalize" indique si les postions Top et Left doivent être interchanger si plus elles sont plus grandes que Bottom et Right }
    procedure Create(Const aLeftTop,aRightBottom : TBZPoint; Const DoNormalize : Boolean = true ); overload;
    { Creation de l'objet TBZRect depuis un TRect }
    procedure Create(Const ARect : TRect ); overload;
    { Creation de l'objet TBZRect avec les paramètres "Origin de type  TBZPoint et AWidth, AHeight" }
    procedure Create(Origin: TBZPoint; AWidth, AHeight: Longint); overload;
    { Creation de l'objet TBZRect nul }
    procedure CreateEmpty;

    { Ajoute les coordonnées des deux TBZRect }
    class operator +(ConstRef A:TBZRect; ConstRef B:TBZRect):TBZRect; overload;
    { Soustrait les coordonnées des deux TBZRect }
    class operator -(ConstRef A:TBZRect; ConstRef B:TBZRect):TBZRect; overload;
    { Ajoute les cordonnées d'un TBZRect avec une valeur "B" de type Longint }
    class operator +(ConstRef A:TBZRect; ConstRef B : LongInt):TBZRect; overload;
    { Soustrait les cordonnées d'un TBZRect avec une valeur "B" de type Longint }
    class operator -(ConstRef A:TBZRect; ConstRef B : LongInt):TBZRect; overload;

    { Verifie si les deux TBZRct ont des coordonnées égales.
      Renvoie @True si toutes les coordonnée sont identiques }
    class operator =(ConstRef A, B: TBZRect): Boolean;
    { Verifie si les deux TBZRct ont des coordonnées Différents.
      Renvoie @True si une des coordonnée est  différente }
    class operator <>(ConstRef A, B: TBZRect): Boolean;

    { Retourne la représentation du TBZRect sous forme de chaine de caractères }
    function ToString : String;

    { Retourne les coordonées minimum des deux rectangles }
    function Min(ConstRef B : TBZRect):TBZRect; overload;
    { Retourne les coordonées mininum du rectangle en fonction de la valeur B }
    function Min(Constref B : LongInt) : TBZRect; overload;
    { Retourne les coordonées maximum des deux rectangles }
    function Max(ConstRef B : TBZRect):TBZRect; overload;
    { Retourne les coordonées maximum du rectangle en fonction de la valeur B }
    function Max(Constref B : LongInt) : TBZRect; overload;

    { S'assure que le rectangle soit dans l'interval des rectangle [AMin, AMax] }
    function Clamp(Constref AMin, AMax : TBZRect):TBZRect; overload;
    { S'assure que le rectangle soit dans l'interval des valeurs [AMin, AMax] }
    function Clamp(Constref AMin, AMax : LongInt):TBZRect; overload;

    { Retourne @True si "Rect2" se trouve dans le TBZRect }
    function RectInRect(ConstRef Rect2 : TBZRect): Boolean;
    { Retourne @True si "Rect2" chevauche le TBZRect }
    function OverlapRect(ConstRef Rect2 : TBZRect): Boolean;
    { Retourne @True si le point "x,y" se trouve a l'extérieur du TBZRect }
    Function PointOutRect(ConstRef x,y : LongInt):Boolean; overload;
    { Retourne @True si le point "x,y" se trouve a l'intérieur du TBZRect }
    function PointInRect(ConstRef x,y : LongInt): Boolean; overload;
    { Retourne @True si le point "P" se trouve a l'extérieur du TBZRect }
    Function PointOutRect(ConstRef p : TBZPoint):Boolean; overload;
    { Retourne @True si le point "P" se trouve a l'intérieur du TBZRect }
    function PointInRect(ConstRef  p : TBZPoint): Boolean; overload;

    //function IntersectRect(ConstRef Rect2  : TBZRect; Var OutRect:TBZRect):Boolean;

    { Inverse les coordonnées Top, Left avec Bottom, Right, suivant si Top/Left plus grand que Bottom/Right }
    procedure Normalize;

    { Agrandit le TBZRect de "DL" par la  gauche, de "DT" vers le haut, de "DR" vers la droite et de "DB" vers le bas }
    procedure Inflate(DL, DT, DR, DB: Longint); overload;
    { Agrandit le TBZRect de "DX" par la  gauche et la droite, de "DY" vers le haut et vers le bas }
    procedure Inflate(DX, DY: Longint); overload;
    { Réduit le TBZRect de "DL" par la  gauche, de "DT" vers le haut, de "DR" vers la droite et de "DB" vers le bas }
    procedure Contract(DL, DT, DR, DB: Longint); overload;
    { Réduit le TBZRect de "DX" par la  gauche et la droite, de "DY" vers le haut et vers le bas }
    procedure Contract(DX, DY: Longint); overload;
    { Déplace le TBZRect de "dx" et "dy" unité }
    function OffSetRect(ConstRef dx,dy: LongInt): Boolean;overload;
    { Déplace le TBZRect de "x" et "y" unité représenté par le point "P" }
    function OffSetRect(ConstRef P: TBZPoint): Boolean;overload;
    { Déplace le TBZRect à la position P }
    procedure MoveTo(ConstRef P : TBZPoint);

    //{ Déplace le TBZRect de P offsets }
    //procedure Move(ConstRef P : TBZPoint); overload;
    //procedure Move(DX, DY : Integer); overload;

    { Retourne le point central du TBZRect }
    function CenterPoint: TBZPoint;

    { Retourne le TBZRect en tant que TRect}
    function AsRect : TRect;

    { Retourne la hauteur du TBZRect }
    property Height: Longint read getHeight write setHeight;
    { Retourne la Largeur du TBZRect }
    property Width : Longint read getWidth  write setWidth;

    { Access properties }
    case Byte of
      0: (V: TBZRectVectorType);                       //< Acces au valeurs via un tableau "Array"
      1: (Left, Top, Right, Bottom : LongInt);         //< Acces aux valeurs par défaut
      2: (TopLeft : TBZPoint; BottomRight : TBZPoint); //< Position des points TopLeft et BottomRight
  End;
  { Pointeur vers un TBZRect }
  PBZRect = ^TBZRect;

  { Creation rapide d'un TBZRect }
  function BZRect(aLeft, aTop, aRight, aBottom : Longint) : TBZRect;
  //function BZBoundsRect(aLeft, aTop, aWidth, aHeight : Longint) : TBZRect;

Type
  { Tableau pour la représentation d'un rectangle virtuel e,n virgule flottante (Single) }
  TBZFloatRectVectorType = packed array[0..3] of Single;

  { Définition d'un rectangle en valeur flottante }
  TBZFloatRect = record // < Type d'un rectangle virtuel
  private
    function GetWidth : Single;
    function GetHeight : Single;
    procedure setWidth(AValue: Single);
    procedure setHeight(AValue: Single);
    function GetTopRight : TBZFloatPoint;
    function GetBottomLeft : TBZFloatPoint;
  public
    { Creation de l'objet TBZFloatRect avec les paramètres "aLeft,aTop,aRight,aBottom" de type Single}
    procedure Create(Const aLeft,aTop,aRight,aBottom : Single ); overload;
    { Creation de l'objet TBZFloatRect avec les paramètres "aLeftTop,aRightaBottom".
      "DoNormalize" indique si les postions Top et Left doivent être interchanger si plus elles sont plus grandes que Bottom et Right }
    procedure Create(Const aLeftTop,aRightBottom : TBZFloatPoint; Const DoNormalize : Boolean = true ); overload;
    { Creation de l'objet TBZFloatRect depuis un TRect }
    procedure Create(Const ARect : TRect ); overload;
    { Creation de l'objet TBZFloatRect avec les paramètres "Origin de type  TBZPoint et AWidth, AHeight" de type Single }
    procedure Create(Origin: TBZFloatPoint; AWidth, AHeight: Single); overload;
    { Creation de l'objet TBZFloatRect nul }
    procedure CreateEmpty;

    procedure CreateBounds(Const aLeft, aTop, aWidth, aHeight: Single);

    { Operateur Addition : ajoute les coordonnées des deux TBZFloatRect }
    class operator +(ConstRef A:TBZFloatRect; ConstRef B:TBZFloatRect):TBZFloatRect; overload;
    { Operateur Soustraction :  Soustrait les coordonnées des deux TBZFloatRect }
    class operator -(ConstRef A:TBZFloatRect; ConstRef B:TBZFloatRect):TBZFloatRect; overload;
    { Operateur Addition : ajoute les cordonnées d'un TBFloatZRect avec une valeur "B" de type Single }
    class operator +(ConstRef A:TBZFloatRect; ConstRef B : Single):TBZFloatRect; overload;
    { Operateur Soustraction : Soustrait les cordonnées d'un TBFloatZRect avec une valeur "B" de type Single }
    class operator -(ConstRef A:TBZFloatRect; ConstRef B : Single):TBZFloatRect; overload;

    { Operateur Egale : Verifie si les deux TBZFloatRect ont des coordonnées égales
       Renvoie @True si toutes les coordonnée sont identiques }
    class operator =(ConstRef A, B: TBZFloatRect): Boolean;
    { Operateur Différent : Verifie si les deux TBZFloatRect ont des coordonnées Différents.
      Renvoie @True si une des coordonnée est  différente }
    class operator <>(ConstRef A, B: TBZFloatRect): Boolean;

    { Retourne la représentation du TBZFloatZRect sous forme de chaine de caractères }
    function ToString : String;

    { Retourne les coordonées minimum des deux rectangles }
    function Min(ConstRef B : TBZFloatRect):TBZFloatRect; overload;
    { Retourne les coordonées minimum des rectangles en fonction de la valeur B }
    function Min(Constref B : Single) : TBZFloatRect; overload;
    { Retourne les coordonées maximum des rectangles  deux rectangles }
    function Max(ConstRef B : TBZFloatRect):TBZFloatRect; overload;
    { Retourne les coordonées maxmum des rectangles en fonction de la valeur B }
    function Max(Constref B : Single) : TBZFloatRect; overload;

    { S'assure que le rectangle soit dans l'interval des rectangle [AMin, AMax] }
    function Clamp(Constref AMin, AMax : TBZFloatRect):TBZFloatRect; overload;
    { S'assure que le rectangle soit dans l'interval des valeurs [AMin, AMax] }
    function Clamp(Constref AMin, AMax : Single):TBZFloatRect; overload;

    { Retourne @True si "Rect2" se trouve dans le TBZFloatRect }
    function RectInRect(ConstRef Rect2: TBZFloatRect): Boolean;
    { Retourne @True si "Rect2" chevauche le TBFloatZRect }
    function OverlapRect(ConstRef Rect2: TBZFloatRect): Boolean;
    { Retourne @True si le point "x,y" se trouve a l'extérieur du TBZFloatRect }
    Function PointOutRect(ConstRef x,y:Single):Boolean; overload;
    { Retourne @True si le point "x,y" se trouve a l'intérieur du TBZFloatRect }
    function PointInRect(ConstRef x,y:Single): Boolean; overload;
    { Retourne @True si le point "P" se trouve a l'extérieur du TBZFloatRect }
    Function PointOutRect(ConstRef p : TBZFloatPoint):Boolean; overload;
    { Retourne @True si le point "P" se trouve a l'intérieur du TBZFloatRect }
    function PointInRect(ConstRef  p : TBZFloatPoint): Boolean; overload;

    { Retourne @True sui les dexu rectangles se chevauve et retour le rectangle inscrit dans les deux rectangles }
    function IntersectWithRect(ConstRef Rect2  : TBZFloatRect; Var OutRect:TBZFloatRect):Boolean;

    { Inverse les coordonnées Top, Left avec Bottom, Right, suivant si Top/Left plus grand que Bottom/Right }
    procedure Normalize;

    { Agrandit le TBZFloatRect de "DL" par la  gauche, de "DT" vers le haut, de "DR" vers la droite et de "DB" vers le bas }
    procedure Inflate(DL, DT, DR, DB: Single); overload;
    { Agrandit le TBZFloatRect de "DX" par la  gauche et la droite, de "DY" vers le haut et vers le bas }
    procedure Inflate(DX, DY: Single); overload;
    { Réduit le TBZFloatRect de "DL" par la  gauche, de "DT" vers le haut, de "DR" vers la droite et de "DB" vers le bas }
    procedure Contract(DL, DT, DR, DB: Single); overload;
    { Réduit le TBZFloatRect de "DX" par la  gauche et la droite, de "DY" vers le haut et vers le bas }
    procedure Contract(DX, DY: Single); overload;
    { Déplace le TBZFloatRect de "dx" et "dy" unité }
    function OffSetRect(ConstRef dx,dy: Single): Boolean;overload;
    { Déplace le TBZFloatRect de "x" et "y" unité représenté par le point "P" }
    function OffSetRect(ConstRef P: TBZFloatPoint): Boolean;overload;

    { Unie les deux rectangles }
    procedure UnionRect(ConstRef Rect2 : TBZFloatRect); overload;

    { Retourne le point central du TBZFloatRect }
    function CenterPoint: TBZFloatPoint;

    { Retourne le TBZFloatRect en tant que TBZRect}
    function AsRect : TBZRect; overload;
    { Retourne le TBZFloatRect en tant que TRect}
    function AsNativeRect : TRect; overload;

    { Retourne la hauteur du TBZFloatRect }
    property Height: Single read getHeight write setHeight;
    { Retourne la Largeur du TBZFloatZRect }
    property Width : Single read getWidth  write setWidth;
    { Retourne la position haut/droite du TBZFloatRect }
    property TopRight : TBZFloatPoint read GetTopRight;
    { Retourne la position bas/gauche du TBZFloatRect }
    property BottomLeft : TBZFloatPoint read GetBottomLeft;

    { Access properties }
    case Byte of
      0: (V: TBZFloatRectVectorType);                             //< Array access
      1: (Left, Top, Right, Bottom : Single);                     //< Legacy access²
      2: (TopLeft : TBZFloatPoint; BottomRight : TBZFloatPoint);
  End;

  { Classe permettant de gérer un type TBZRect dans une classe TODO : A déplacer dans l'unité BZControlClasses}
  TBZRectItemProperty = class(TBZUpdateAbleObject)
   private
     FRect : TBZRect;

     procedure SetLeft(const AValue : Integer);
     procedure SetTop(const AValue : Integer);
     procedure SetRight(const AValue : Integer);
     procedure SetBottom(const AValue : Integer);
     procedure SetWidth(const AValue : Integer);
     procedure SetHeight(const AValue : Integer);
     function GetLeft : Integer;
     function GetTop : Integer;
     function GetRight : Integer;
     function GetBottom : Integer;
     function GetWidth : Integer;
     function GetHeight : Integer;
   protected
   public

     function CenterX : Integer;
     Function CenterY : Integer;

     property Left : Integer read GetLeft write SetLeft;
     property Top : Integer read GetTop write SetTop;
     property Right : Integer read GetRight write SetRight;
     property Bottom : Integer read GetBottom write SetBottom;
     property Width : Integer read GetWidth write SetWidth;
     property Height : Integer read GetHeight write SetHeight;
     property AsRect : TBZRect Read FRect;
   end;

  // TODO : Faire un Enregistrment Avancer et à passer dans BZGeoTool.pas. A voir.
  TBZQuadrangle2D = Array[0..3] of TBZFloatPoint;

{%endregion%}

{%region%=====[ Descriptions des histogrammes d'une image ]=====================}
Type
  // D'après les codes source de Earl F. Glynn
  { Choix du canal de l'histogramme }
  TBZHistogramChannel = (hcLuminosity, hcRed, hcGreen, hcBlue, hcHue, hcSaturation, hcLuminance, hcValue); //hcCyan, hcMagenta, hcYellow, hcBlack
  { Type de l'histogramme }
  TBZHistorgramDrawMode = (hdmRBGLuminosity, hdmRGB, hdmLuminosity, hdmRed, hdmGreen, hdmBlue, hdmHue, hdmSaturation, hdmLuminance, hdmValue);
  { Tableau de stockage des valeurs pour les histogrammes }
  TBZHistogramArray = array[0..255] of integer;
  { Statistiques d'un histogramme }
  TBZHistogramStatistics = packed record
     Pixels  : Integer;
     Minimum : Integer;
     Maximum : Integer;
     MaxFrequency  : Integer;
     Median  : Integer;
     Mean              : Double;
     StandardDeviation : Double;
     Skewness          : Double;
     ExcessKurtosis    : Double;
     // Entropy
  end;

{%endregion%}

{%region%=====[ Définitions des mode de Blending, et d'affectation des pixels  ]=====}

Type
  { Enumération des méthode de suréchantillionnage }
  TBZGetPixelSampleMethod = (psmDefault, psmNeighbour, psmBilinear, psmBicubic, psmMean, psmMedian, psmMin, psmMax, psmMinMax, psmSubSampling, psmAdaptativeSubSampling, psmCustom);

  { Mode de dessin des pixels }
  TBZBitmapDrawMode = (dmSet, dmSetAlpha, dmCombine, dmCustom);

  { Mode de traitement pour les pixels sur les bords }
  TBZPixelEdgeAction = (peaZero, peaClamp, peaWarp, peaReflect);

  { Type de filtre morphologique de base }
  TBZMorphologicalOperator =(moErode, moDilate);

//const
//  { BZBitmapDrawModeAsString : }
//  BZBitmapDrawModeAsString:Array[TBZBitmapDrawMode] of string=('Set','Alpha','Average','Modulate','Add','Sub','Mul','Div','Or','Xor','And','Blend','Filter','Custom');

  { Définition du mode de composition pour le canal alpha}
  TBZBitmapAlphaMode = (amNone, amOpaque, amAlpha, amAlphaBlend,amAlphaBlendHQ, amBlend, amAlphaSrc, amAlphaDst, amAlphaCheck);

  { @abstract(Définition de convenance, des modes de compositions pour les couleurs RGB (Voir aussi : TBZBitmapCombineMode).) @br
    cf : @br
    @unorderedlist(
      @item(http://www.pegtop.net/delphi/articles/blendmodes/)
      @item(https://en.wikipedia.org/wiki/Blend_modes)) }
  TBZBitmapBlendMode = (bmNormal, bmAdd, bmSub, bmMul, bmDiv, bmOr, bmXor, bmAnd, bmDifference, bmAverage,
                        bmOverlay, bmScreen, bmStamp, bmHeat, bmFreeze, bmGlow, bmReflect, bmExclusion, bmNegate, bmLighten, bmDarken,
                        bmColorBurn, bmSoftColorBurn, bmInverseColorBurn, bmColorDodge, bmSoftColorDodge, bmInverseColorDodgen,
                        bmInterpolation, bmHardLight, bmSoftLight, bmBrightLight, bmLinearLight, bmVividLight, bmPinLight, bmHardMix,
                        bmHue, bmColor, bmValue, bmSaturation, bmLuminosity, bmPhoenix, bmGrainMerge, bmGrainExtract, bmRed, bmGreen, bmBlue,
                        bmDissolve, bmErase, bmCustom);

Const
  { Définition en chaine de caractères des modes de dessin }
  cPixelDrawModeStr   : Array[0..3] of String  = ('Set', 'Set Alpha', 'Combine', 'Custom');
  { Définition en chaine de caractères des modes de prise en charge du canal Alpha }
  cAlphaDrawModeStr   : Array[0..8] of String  = ('Aucun', 'Opaque', 'Alpha', 'Alpha Blend', 'Alpha Blend HQ', 'Blend', 'AlphaSrc', 'AlphaDst', 'AlphaCheck');

{%endregion%}

{%region%=====[ Gestionnaire de couleurs, palettes et dégradés ]=====================}

Type
  { Définitions supplémentaires }
  //TBZGradientType = (gtHorizontal, gtVertical, gtRadial, gtFromTopLeft, gtFromTopRight, gtDoubleHorz, gtDoubleVert);
  TBZTextureMappingMode = (tmmDefault,tmCenter, tmmStretch,tmmStretchProportional, tmmTiled);

  { Définition des modes de remplissage d'un bitmap }
  TBZBitmapFillType = (bftNone, bftFlat, bftGradient, bftTextured); //???????????????? A SUPPRIMER !!!!

  { Définition des modes de remplissage d'une brosse }
  TBZCanvasBrushFillType =(ftNone,ftSolid,ftGradient,ftTexture,ftPattern,ftCustom);
  { Définition des mode d'anticrénelage }
  TBZBitmapAntiAliasMode = (aamNone,aam2x, aam4x, aam8x, aam16x, aam32x, aamCustom);

{%endregion%}

{%region%=====[ Définitions des propriétés des Images ]==============================}

Type
  { Ordre des lignes c-a-d l'origine en haut à gauche ou en bas à gauche }
  TBZBitmapLineOrder = (bloTopToBottom, bloBottomToTop);

(* A rajouter peut-être pour la compatibilité au niveau de l'architecture
TBZBitmapByteOrder = (
   bboLSBFirst, // least significant byte first // Litlle_Indian
   bboMSBFirst  // most significant byte first  // Big_Indian
   ); *)

 { Ordre des bits pour les formats couleurs < 8bits }
  TBZBitmapBitOrder = (
    bboBitsInOrder, //< Bit 0 is pixel 0
    bboReversedBits //< Bit 0 is pixel 7 (Bit 1 is pixel 6, ...)
    );

  { Alignement de la taille d'une ligne suivant l'unité utilisé }
  TBZBitmapLineEnd = (
    bleNoBoundary,    //< no gap at end of lines
    bleByteBoundary,  //< each line starts at byte boundary. For example : If BitsPerPixel=3 and Width=1, each line has a gap of 5 unused bits at the end.
    bleWordBoundary,  //< each line starts at word (16bit) boundary
    bleDWordBoundary, //< each line starts at double word (32bit) boundary
    bleQWordBoundary, //< each line starts at quad word (64bit) boundary
    bleDQWordBoundary //< each line starts at double quad word (128bit) boundary
   );

{%endregion%}

{%region%=====[ Miscs defnitions ]===================================================}

Type

  { Type pour le dessin du conteur }
  TBZStrokeStyle = (ssClear, ssSolid, ssPattern, ssGradient, ssTexture, ssCustom);

  { Définition du mode de dessin des contours }
  TBZStrokeMode = (smInner, smOuter, smAround);

  { Style des differentes brosses pour le Canvas }
  TBZBrushStyle = (bsClear, bsSolid, bsGradient, bsTexture, bsPattern, bsCustom);

  { Style des pattern prédéfinis }
  TBZPatternStyle = (psClear, psSolid, psDash, psDot, psDashDot, psDashDotDot, psCustom);

  { Type de mapping à appliquer pour le remplissage avec une texture. (ssTexture, bsTexture) }
  TBZTextureMappingKind = (tmkDefault, tmkAutoTile, tmkTiled);

  { Tableau type pour definir une "Patterne" }
  TBZCanvasPenStrokePattern = array of boolean;

  { Tableau type pour definir une "Patterne" pour la brosse }
  TBZBrushPattern = array[0..31] of TBZCanvasPenStrokePattern;
  PBZBrushPattern = ^TBZBrushPattern;

  { Style des jointures de contour }
  TBZJoinStyle = (jsMitter, jsRounded, jsBevel);

  { Style de debut et de fin de ligne et ou chemins }
  TBZCapsMode = (cmNone, cmRounded, cmArrow, cCircle, cmSquare, cmCustom);

  { Format des palette utilisés dans les données brutes (provenant d'un fichier par exemple ) (Source : GraphicEx) }
  TBZImageRawPaletteFormat = (
    pfInterlaced8Triple, //< 24bits RGB, 8 bits par canal
    pfInterlaced8Quad, //< 32bits RGB.Reserved, 8 bits par canal (Le 4eme canal est reservé pour windows dans sa gestion des palettes)
    pfPlane8Triple, //< 3 plans séparés, 8 bits par canal
    pfPlane8Quad //< 4 plans séparés, 8 bits par canal
 (*   pfInterlaced16Triple,// 48bits RGB, 16 bits par canal
    pfInterlaced16Quad,  // 64bits RGB, 16 bits par canal
    pfPlane16Triple,     // 3 plans séparés, 16 bits par canal
    pfPlane16Quad        // 4 plans séparés, 16 bits par canal  *)
  );

Type
  { Définit comment la Canal Alpha doit être définit dans un bitmap :
    @unorderedList(
      @item(asmDefault               : Utilise le canal alpha non modifié du bitmap  par defaut)
      @item(asmAlphaFromIntensity    : La valeur du canal alpha est déduite des autres intensité des composants RVB @(plus lumineux, plus opaque@))
      @item(asmSuperBlackTransparent : Les pixels avec une couleur RVB de (0, 0, 0) sont complètement transparent, les autres reste inchangés)
      @item(asmLuminance             : La valeur de luminance est calculée pour chaque pixel)     
      @item(asmLuminanceSqrt         : Identique à bamLuminance mais avec un Sqrt @(Luminance@))
      @item(asmOpaque                : Le canal alpha est uniformément réglé sur 255)
    )@br

    @bold(Note) : Luminance = Intensité , mais plus précis cf : fonction TBZBitmapColorFilters.SetAlpha @br
    TODO asmCustom : Le canal alpha est défini par l'utilisateur Porte ouverte pour une fonction "CallBack" }
   TBZBitmapAlphaSetMode = (
     asmDefault,
     asmAlphaFromIntensity,
     asmSuperBlackTransparent,
     asmLuminance,
     asmLuminanceSqrt,
     asmOpaque,
     asmInverseLuminance,
     asmInverseLuminanceSqrt,
     asmAlphaFromColor
 //     asmTopLeftPixelTransparent,
 //    asmBottomRightPixelTransparent
 //  asmCustom
   );

  //TBZUpSampleMethod =(usHQx, usXBR, usHQXBR);


Type
  { Mode de detection des contours par convolution disponibles }
  TBZDetectEdgeFilterMode = (defPrewitt, defSobel, defRoberts, defKirsch, defScharr, defRobinson, defMDif, defLaplace);

  { Tableau pour stocker des matrices de convolution 3x3 }
  TBZMatrixConvolutionFilter3x3 = Array [0..8] of Integer;
  { Tableau pour stocker des matrices de convolution 5x5 }
  TBZMatrixConvolutionFilter5x5 = Array [0..24] of Integer;
  { Tableau pour stocker des matrices de convolution 7x7 }
  TBZMatrixConvolutionFilter7x7 = Array [0..48] of Integer;

  //TBZMatrixConvolutionFilter9x9 = Array [0..80] of Integer;
  //TBZMatrixConvolutionFilter11x11= Array [0..120] of Integer;
  //TBZMatrixConvolutionFilterNxN= Array of Integer;

  { Type de la matrice de convolution }
  TBZMatrixConvolutionType = (mct3x3,mct5x5,mct7x7);
  TBZConvolutionFilterCategory = (cfcAntialiasing, cfcEdgeDetect, cfcEnhanceEdge, cfcEnhanceColor, cfcArtistic);
  { Conteneur servant à décrire un filtre de convolution }
  TBZConvolutionFilter = packed record
   Name : String;
   Category : TBZConvolutionFilterCategory;
   Bias : Integer;
   Divisor : Integer;
   MatrixType : TBZMatrixConvolutionType;
   MatrixSize : Byte;
   Matrix : packed record case integer of
     1 : ( _3 : array [0..8] of Single);
     2 : ( _5 : array [0..24] of Single);
     3 : ( _7 : array [0..48] of Single);
     //4 : (  _9 : array [-4..4,-4..4] of integer);
     //5 : ( _11 : array [-5..5,-5..5] of integer);
     //6 : (  _N : array of integer); // [0..(N*N)-1]
    end;
  end;


{%endregion%}

{%region%=====[ Filtres de convolution ]=============================================}

Const
  { Filtres de "convolution ou matricielle" prédéfinis. @br
    Sources : @br
    @unorderedList(
      @item(http://xphilipp.developpez.com/articles/filtres/)
      @item(https://www.developpez.net/forums/d328817/general-developpement/algorithme-mathematiques/contribuez/image-gradient-hessienne-convolution/)
      @item(https://homepages.inf.ed.ac.uk/rbf/HIPR2/gsmooth.htm)
      @item(http://lodev.org/cgtutor/filtering.html)
      @item(https://docs.gimp.org/fr/plug-in-convmatrix.html)
      @item(http://www.roborealm.com/help/Convolution.php)
      @item(Graphic32, BGRABitmap, Vampyre/FreeImage Lib)
    )
  }
  BZConvolutionFilterPresets : Array[0..63] of TBZConvolutionFilter = (
    ( // 0
      Name     : 'Neutre';
      Category : cfcAntialiasing;
      Bias    : 0;
      Divisor : 1;
      MatrixType : mct3x3;
      MatrixSize : 3;
      Matrix :(_3 : (0,0,0,
                     0,1,0,
                     0,0,0
                ));
    ),
    ( // 1
      Name    : 'Moyenne 3x3';
      Category : cfcAntialiasing;
      Bias    : 0;
      Divisor : 9;
      MatrixType : mct3x3;
      MatrixSize : 3;
      Matrix :(_3 : (1,1,1,
                     1,1,1,
                     1,1,1
                ));
    ),
    ( // 2
      Name    : 'Moyenne 5x5';
      Category : cfcAntialiasing;
      Bias    : 0;
      Divisor : 25;
      MatrixType : mct5x5;
      MatrixSize : 3;
      Matrix  : (_5 : ( 1, 1, 2, 1, 1,
                        1, 1, 1, 1, 1,
                        1, 1, 1, 1, 1,
                        1, 1, 1, 1, 1,
                        1, 1, 1, 1, 1
                       ));
     ),
     ( // 3
      Name    : 'Flou Gaussien 3x3';
      Category : cfcAntialiasing;
      Bias    : 0;
      Divisor : 16;
      MatrixType : mct3x3;
      MatrixSize : 3;
      Matrix  : (_3 : (1,2,1,
                       2,4,2,
                       1,2,1
                 ));
     ),
     ( // 4
      Name    : 'Flou Gaussien 3x3 Doux';
      Category : cfcAntialiasing;
      Bias    : 0;
      Divisor : 32;
      MatrixType : mct3x3;
      MatrixSize : 3;
      Matrix  : (_3 : (1, 3,1,
                       3,16,3,
                       1, 3,1
                 ));
     ),
    ( // 5
      Name    : 'Flou Gaussien 5x5 Fort';
      Category : cfcAntialiasing;
      Bias    : 0;
      Divisor : 64;
      MatrixType : mct5x5;
      MatrixSize : 3;
      Matrix  : (_5 : ( 0, 1, 2, 1, 0,
                        1, 4, 6, 4, 1,
                        2, 6, 8, 6, 2,
                        1, 4, 6, 4, 1,
                        0, 1, 2, 1, 0
                       ));
     ),
     ( // 6
     //Divisor: 256;
     //Matrix  : (_5 : (1,  4,  6,  4, 1,
     //                 4, 16, 24, 16, 4,
     //                 6, 24, 36, 24, 6,
     //                 4, 16, 24, 16, 4,
     //                 1,  4,  6,  4, 1));

      Name    : 'Flou Gaussien 5x5';
      Category : cfcAntialiasing;
      Bias    : 0;
      Divisor : 159; //273;
      MatrixType : mct5x5;
      MatrixSize : 5;
      //Matrix  : (_5 : ( 1, 4, 7, 4, 1,
      //                  4,16,26,16, 4,
      //                  9,26,41,26, 7,
      //                  4,16,26,16, 4,
      //                  1, 4, 7, 4, 1
      //                 ));
      Matrix  : (_5 : ( 2, 4, 5, 4, 2,
                        4, 9,12, 9, 4,
                        5,12,15,12, 5,
                        4, 9,12, 9, 4,
                        2, 4, 5, 4, 2
                       ));
      ),
      ( // 7
      Name    : 'Flou Gaussien 7x7';
      Category : cfcAntialiasing;
      Bias    : 0;
      Divisor : 999;
      MatrixType : mct7x7;
      MatrixSize : 7;
      Matrix  : (_7 : ( 1, 4, 8,10, 8, 4, 1,
                        4,12,25,29,25,12, 4,
                        8,25,49,58,49,25, 8,
                       10,29,58,67,58,29,10,
                        8,25,49,58,49,25, 8,
                        4,12,25,29,25,12, 4,
                        1, 4, 8,10, 8, 4, 1
                      ));
     ),
     ( // 8
      Name    : 'Flou Bartlett 7x7';
      Category : cfcAntialiasing;
      Bias    : 0;
      Divisor : 256;
      MatrixType : mct7x7;
      MatrixSize : 7;
      Matrix  : (_7 : ( 1, 2, 3, 4, 3, 2, 1,
                        2, 4, 6, 8, 6, 4, 2,
                        3, 6, 9,12, 9, 6, 3,
                        4, 8,12,16,12, 8, 4,
                        3, 6, 9,12, 9, 6, 3,
                        2, 4, 6, 8, 6, 4, 2,
                        1, 2, 3, 4, 3, 2, 1
                       ));
      ),
     ( // 9
      Name    : 'Renforcement des contours'; //Mean removal
      Category : cfcEnhanceEdge;
      Bias    : 0;
      Divisor : 1;
      MatrixType : mct3x3;
      MatrixSize : 3;
      Matrix :(_3 : (-1,-1,-1,
                     -1,9,-1,
                     -1,-1,-1
              ));
      ),
     ( // 10
      Name    : 'Filtre passe bas';
      Category : cfcAntialiasing;
      Bias    : 0;
      Divisor : 12;
      MatrixType : mct3x3;
      MatrixSize : 3;
      Matrix  : (_3 :(1,1,1,
                      1,4,1,
                      1,1,1
                ));
      ),
     ( // 11
      Name    : 'Filtre passe bas doux';
      Category : cfcAntialiasing;
      Bias    : 0;
      Divisor : 20;
      MatrixType : mct3x3;
      MatrixSize : 3;
      Matrix  : (_3 :(1,1,1,
                      1,12,1,
                      1,1,1
                 ));
      ),
     ( // 12
      Name    : 'Filtre passe bas fort';
      Category : cfcAntialiasing;
      Bias    : 0;
      Divisor : 10;
      MatrixType : mct3x3;
      MatrixSize : 3;
      Matrix  : (_3 :(1,1,1,
                      1,2,1,
                      1,1,1
                 ));
      ),
     ( // 13
      Name    : 'Filtre passe haut';
      Category : cfcEnhanceEdge;
      Bias    : 0;
      Divisor : 1;
      MatrixType : mct3x3;
      MatrixSize : 3;
      Matrix  : (_3 :( 0,-1, 0,
                      -1, 5,-1,
                       0,-1, 0
                 ));
      ),
     ( // 14
      Name    : 'Filtre passe haut doux';
      Category : cfcEnhanceEdge;
      Bias    : 0;
      Divisor : 1;
      MatrixType : mct3x3;
      MatrixSize : 3;
      Matrix  : (_3 :(-1 , 1,-1,
                       1,  4, 1,
                      -1 , 1,-1
                 ));
      ),
     ( // 15
      Name    : 'Filtre passe haut moyen';
      Category : cfcEnhanceEdge;
      Bias    : 0;
      Divisor : 1;
      MatrixType : mct3x3;
      MatrixSize : 3;
      Matrix  : (_3 :( 1 , -2,  1,
                      -2,   5, -2,
                       1 , -2,  1
                     ));
      ),
     ( // 16
      Name    : 'Filtre passe haut fort';
      Category : cfcEnhanceEdge;
      Bias    : 0;
      Divisor : 16;
      MatrixType : mct3x3;
      MatrixSize : 3;
      Matrix  : (_3 :( 0 , -1,  0,
                      -1,  20, -1,
                       0 , -1,  0
                     ));
      ),
    ( // 17
     Name    : 'Prewitt horizontal';
     Category : cfcEdgeDetect;
     Bias    : 0;
     Divisor : 3;
     MatrixType : mct3x3;
     MatrixSize : 3;
     Matrix  : (_3 : (-1,-1,-1,
                       0, 0, 0,
                       1, 1, 1
               ));
    ),
    ( // 18
     Name    : 'Prewitt vertictal';
     Category : cfcEdgeDetect;
     Bias    : 0;
     Divisor : 3;
     MatrixType : mct3x3;
     MatrixSize : 3;
     Matrix  : (_3 : (-1, 0, 1,
                      -1, 0, 1,
                      -1, 0, 1
               ));
    ),
    ( // 19
     Name    : 'Roberts -45°';
     Category : cfcEdgeDetect;
     Bias    : 0;
     Divisor : 1;
     MatrixType : mct3x3;
     MatrixSize : 3;
     Matrix  : (_3 : ( 0, 0, 0,
                       0, 0, 1,
                       0,-1, 0
               ));
    ),
    ( // 20
     Name    : 'Roberts +45°';
     Category : cfcEdgeDetect;
     Bias    : 0;
     Divisor : 1;
     MatrixType : mct3x3;
     MatrixSize : 3;
     Matrix  : (_3 : ( 0, 0, 0,
                       0,-1, 0,
                       0, 0, 1
               ));
    ),
    ( // 21
     Name    : 'Sobel horizontal';
     Category : cfcEdgeDetect;
     Bias    : 0;
     Divisor : 4;
     MatrixType : mct3x3;
     MatrixSize : 3;
     Matrix  : (_3 : ( -1,-2,-1,
                        0, 0, 0,
                        1, 2, 1
               ));

    ),
    ( // 22
     Name    : 'Sobel vertical';
     Category : cfcEdgeDetect;
     Bias    : 0;
     Divisor : 4;
     MatrixType : mct3x3;
     MatrixSize : 3;
     Matrix  : (_3 : (-1, 0, 1,
                      -2, 0, 2,
                      -1, 0, 1
               ));

    ),
    ( // 23
     Name    : 'Kirsch horizontal';
     Category : cfcEdgeDetect;
     Bias    : 0;
     Divisor : 15;
     MatrixType : mct3x3;
     MatrixSize : 3;
     Matrix  : (_3 : (-3,-3,-3,
                      -3, 0,-3,
                       5, 5, 5
               ));
    ),
    ( // 24
     Name    : 'Kirsch vertical';
     Category : cfcEdgeDetect;
     Bias    : 0;
     Divisor : 15;
     MatrixType : mct3x3;
     MatrixSize : 3;
     Matrix  : (_3 : ( -3,-3, 5,
                       -3, 0, 5,
                       -3,-3, 5
               ));
    ),
    ( // 25
     Name    : 'Scharr horizontal';
     Category : cfcEdgeDetect;
     Bias    : 0;
     Divisor : 10;
     MatrixType : mct3x3;
     MatrixSize : 3;
     Matrix  : (_3 : (-3, -1, -3,
                       0,  0,  0,
                       3,  1,  3
               ));
    ),
    ( // 26
     Name    : 'Scharr vertical';
     Category : cfcEdgeDetect;
     Bias    : 0;
     Divisor : 10;
     MatrixType : mct3x3;
     MatrixSize : 3;
     Matrix  : (_3 : ( -3,  0,  3,
                       -1,  0,  1,
                       -3,  0,  3
               ));
    ),
    ( // 27
     Name    : 'RobinSon Horizontal';
     Category : cfcEdgeDetect;
     Bias    : 0;
     Divisor : 5;
     MatrixType : mct3x3;
     MatrixSize : 3;
     Matrix  : (_3 : ( 1, 1, 1,
                       1, -2, 1,
                      -1,-1,-1
               ));

    ),
    ( // 28
     Name    : 'RobinSon Vertical';
     Category : cfcEdgeDetect;
     Bias    : 0;
     Divisor : 5;
     MatrixType : mct3x3;
     MatrixSize : 3;
     Matrix  : (_3 : (-1, 1, 1,
                      -1, -2, 1,
                      -1, 1, 1
               ));

    ),
    ( // 29
     Name    : 'MDif horizontal';
     Category : cfcEdgeDetect;
     Bias    : 0;
     Divisor : 12;
     MatrixType : mct5x5;
     MatrixSize : 5;
     Matrix  : (_5 : ( 0,-1,-1,-1, 0,
                      -1,-2,-3,-2,-1,
                       0, 0, 0, 0, 0,
                       1, 2, 3, 2, 1,
                       0, 1, 1, 1, 0
               ));
    ),
    ( // 30
     Name    : 'MDif vertical';
     Category : cfcEdgeDetect;
     Bias    : 0;
     Divisor : 12;
     MatrixType : mct5x5;
     MatrixSize : 5;
     Matrix  : (_5 : ( 0,-1, 0, 1, 0,
                      -1,-2, 0, 2, 1,
                      -1,-3, 0, 3, 1,
                      -1,-2, 0, 2, 1,
                       0,-1, 0, 1, 0
                    ));
    ),
    ( // 31
     Name    : 'Différence Gaussienne';
     Category : cfcEdgeDetect;
     Bias    : 0;
     Divisor : 28;
     MatrixType : mct5x5;
     MatrixSize : 5;
     Matrix  : (_5 : ( 0,-1, 0, 1, 0,
                      -1,-2, 0, 2, 1,
                      -1,-3, 0, 3, 1,
                      -1,-2, 0, 2, 1,
                       0,-1, 0, 1, 0
               ));
    ),
    ( // 32
     Name    : 'Detection ligne horizontale';
     Category : cfcEdgeDetect;
     Bias    : 0;
     Divisor : 1;
     MatrixType : mct3x3;
     MatrixSize : 3;
     Matrix  : (_3 : (-1, -1, -1,
                       2,  2,  2,
                      -1, -1, -1
               ));

    ),
    ( // 33
     Name    : 'Detection ligne verticale';
     Category : cfcEdgeDetect;
     Bias    : 0;
     Divisor : 1;
     MatrixType : mct3x3;
     MatrixSize : 3;
     Matrix  : (_3 : (-1,  2, -1,
                      -1,  2, -1,
                      -1,  2, -1
               ));

    ),
    ( // 34
     Name    : 'Detection ligne +45°';
     Category : cfcEdgeDetect;
     Bias    : 0;
     Divisor : 1;
     MatrixType : mct3x3;
     MatrixSize : 3;
     Matrix  : (_3 : (-1, -1,  2,
                      -1,  2, -1,
                       2, -1, -1
               ));

    ),
    ( // 35
     Name    : 'Detection ligne -45°';
     Category : cfcEdgeDetect;
     Bias    : 0;
     Divisor : 1;
     MatrixType : mct3x3;
     MatrixSize : 3;
     Matrix  : (_3 : ( 2, -1, -1,
                      -1,  2, -1,
                      -1, -1,  2
               ));

    ),
    ( // 36
     Name    : 'Laplace Gaussien connexités 4';
     Category : cfcEdgeDetect;
     Bias    : -1;
     Divisor : 4;
     MatrixType : mct3x3;
     MatrixSize : 3;
     Matrix  : (_3 : ( 0,-1, 0,
                      -1, 4,-1,
                       0,-1, 0
                 ));
    ),
    ( // 37
     Name    : 'Laplace Gaussien connexités 8';
     Category : cfcEnhanceEdge;
     Bias    : -1;
     Divisor : 8;
     MatrixType : mct3x3;
     MatrixSize : 3;
     Matrix  : (_3 : (-1,-1,-1,
                      -1, 8,-1,
                      -1,-1,-1
                 ));
    ),
    ( // 38
     Name    : 'Laplace horizontal';
     Category : cfcEdgeDetect;
     Bias    : -1;
     Divisor : 1;
     MatrixType : mct3x3;
     MatrixSize : 3;
     Matrix  : (_3 : ( 0, -1,  0,
                       0,  2,  0,
                       0, -1,  0
                 ));
    ),
    ( // 39
     Name    : 'Laplace vertical';
     Category : cfcEdgeDetect;
     Bias    : -1;
     Divisor : 1;
     MatrixType : mct3x3;
     MatrixSize : 3;
     Matrix  : (_3 : ( 0,  0,  0,
                      -1,  2, -1,
                       0,  0,  0
                 ));
    ),
    ( // 40
     Name    : 'Laplace diagonal';
     Category : cfcEdgeDetect;
     Bias    : -1;
     Divisor : 1;
     MatrixType : mct3x3;
     MatrixSize : 3;
     Matrix  : (_3 : ( -1,  0, -1,
                        0,  4,  0,
                       -1,  0, -1
                 ));
    ),
    ( // 41
     Name    : 'Laplace discret';
     Category : cfcEdgeDetect;
     Bias    : -1;
     Divisor : 1;
     MatrixType : mct3x3;
     MatrixSize : 3;
     Matrix  : (_3 : ( 1,  1, 1,
                       1, -8, 1,
                       1,  1, 1
                 ));
    ),
    ( // 42
     Name    : 'Laplace Robinson';
     Category : cfcEdgeDetect;
     Bias    : -1;
     Divisor : 1;
     MatrixType : mct3x3;
     MatrixSize : 3;
     Matrix  : (_3 : ( 1,-2, 1,
                      -2, 4,-2,
                       1,-2, 1
                 ));
    ),
    ( // 43
     Name    : 'Laplace 3x3 OmniDir';
     Category : cfcEdgeDetect;
     Bias    : 0;
     Divisor : 1;
     MatrixType : mct3x3;
     MatrixSize : 3;
     Matrix  : (_3 : (-1,-1,-1,
                      -1, 8,-1,
                      -1,-1,-1
               ));
    ),
    ( // 44
     Name    : 'Laplace Gaussien 5x5';
     Category : cfcEdgeDetect;
     Bias    : -4;
     Divisor : 16;
     MatrixType : mct5x5;
     MatrixSize : 5;
     Matrix  : (_5 : ( 0, 0,-1, 0, 0,
                       0,-1,-2,-1, 0,
                      -1,-2,16,-2,-1,
                       0,-1,-2,-1, 0,
                       0, 0,-1, 0, 0
               ));
    ),
    ( // 45
     Name    : 'Laplace 5x5 Omnidir';
     Category : cfcEdgeDetect;
     Bias    : 0;
     Divisor : 1;
     MatrixType : mct5x5;
     MatrixSize : 5;
     Matrix  : (_5 : (-1,-1,-1,-1,-1,
                      -1,-1,-1,-1,-1,
                      -1,-1,24,-1,-1,
                      -1,-1,-1,-1,-1,
                      -1,-1,-1,-1,-1
                ));
    ),
    ( // 46
     Name    : 'Sharpen 3x3';
     Category : cfcEnhanceEdge;
     Bias    : 0;
     Divisor : 1;
     MatrixType : mct3x3;
     MatrixSize : 3;
     Matrix  : (_3 :(-1,-1,-1,
                     -1, 9,-1,
                     -1,-1,-1
                    ));
    ),
    ( // 47
     Name    : 'Sharpen 5x5';
     Category : cfcEnhanceEdge;
     Bias    : 0;
     Divisor : 1;
     MatrixType : mct5x5;
     MatrixSize : 5;
     Matrix  : (_5 : (-1,-1,-1,-1,-1,
                      -1,-1,-1,-1,-1,
                      -1,-1,25,-1,-1,
                      -1,-1,-1,-1,-1,
                      -1,-1,-1,-1,-1
               ));
    ),
    ( // 48
     Name    : 'Amélioration des contour'; //Unsharp mask / EdgeEnhance
     Category : cfcEnhanceEdge;
     Bias    : 0;
     Divisor : 4;
     MatrixType : mct3x3;
     MatrixSize : 3;
     Matrix  : (_3 :(-1,-2,-1,
                     -2,16,-2,
                     -1,-2,-1
                    ));
    ),
    ( // 49
    Name    : 'Contour Epais';
    Category : cfcEdgeDetect;
    Bias    : 0;
    Divisor : 4;
    MatrixType : mct3x3;
    MatrixSize : 3;
    Matrix  : (_3 :( 1,  3, 1,
                     3,-16, 3,
                     1,  3,  1
                   ));
    ),
    ( // 50
    Name    : 'Contour Fin';
    Category : cfcEdgeDetect;
    Bias    : 0;
    Divisor : 4;
    MatrixType : mct3x3;
    MatrixSize : 3;
    Matrix  : (_3 :( 0, 1, 0,
                     1,-4, 1,
                     0, 1, 0
                   ));
    ),
    ( // 51
    Name :'Trace contour';
    Category : cfcEdgeDetect;
    Bias:240; // 240/255;
    Divisor:4; // 4
    MatrixType : mct3x3;
    MatrixSize : 3;
    Matrix  : (_3 :(-6,-2,-6,
                    -1,32,-1,
                    -6,-2,-6 ));


    ),
    ( // 52
    Name :'Glow';
    Category : cfcArtistic;
    Bias:0;
    Divisor:8;
    MatrixType : mct5x5;
     MatrixSize : 5;
     Matrix  : (_5 : (1, 2,   2, 2, 1,
                      2, 0,   0, 0, 2,
                      2, 0, -20, 0, 2,
                      2, 0,   0, 0, 2,
                      1, 2,   2, 2, 1
               ));


    ),
    ( // 53
    Name :'Waggle';
    Category : cfcArtistic;
    Bias:0;
    Divisor:3;
    MatrixType : mct7x7;
    MatrixSize : 7;
    Matrix  : (_7 :( 0, 1, 0, 0, 0, 0, 0,
                     0, 0, 0, 0, 0, 0, 1,
                     0, 0, 0, 0, 0, 0, 0,
                     0, 0, 0, 0, 0, 0, 0,
                     0, 0, 0, 0, 0, 0, 0,
                     0, 0, 0, 0, 0, 0, 0,
                     0, 0, 1, 0, 0, 0, 0
                  ));
    ),
    ( // 54
    Name :'Negate';
    Category : cfcArtistic;
    Bias:255;
    Divisor:1;
    MatrixType : mct3x3;
    MatrixSize : 3;
    Matrix  : (_3 :(0, 0, 0,
                    0,-1, 0,
                    0, 0, 0
                  ));


    ),
    ( // 55
    Name :'Emboss';
    Category : cfcEdgeDetect;
    Bias:1; //0.5
    Divisor:1;
    MatrixType : mct3x3;
    MatrixSize : 3;
    Matrix  : (_3 :(2, 0, 0,
                    0,-1, 0,
                    0, 0,-1
                  ));


    ),
    ( // 56
    Name :'Emboss Doux';
    Category : cfcEdgeDetect;
    Bias:192;
    Divisor:1;
    MatrixType : mct3x3;
    MatrixSize : 3;
    Matrix  : (_3 :(0,-1, 0,
                    0, 0, 0,
                    0, 1, 0
                  ));


    ),
    ( // 57
    Name :'Emboss Moyen';
    Category : cfcEdgeDetect;
    Bias:192;
    Divisor:1;
    MatrixType : mct3x3;
    MatrixSize : 3;
    Matrix  : (_3 :(0,-2, 0,
                    0, 0, 0,
                    0, 2, 0
                  ));
    ),
    ( // 58
    Name :'Emboss Color';
    Category : cfcArtistic;
    Bias:0;
    Divisor:1;
    MatrixType : mct3x3;
    MatrixSize : 3;
    Matrix  : (_3 :(-1,-1,-1,
                     0, 1, 0,
                     1, 1, 1
                   ));
    ),
    ( // 59
    Name :'Différence Gaussienne (DOG) 2nd Degré';
    Category : cfcEdgeDetect;
    Bias:0;
    Divisor:7;
    MatrixType : mct7x7;
    MatrixSize : 7;
    Matrix  : (_7 :( 0, 0, 1, 1, 1, 0, 0,
                     0, 1, 1, 1, 1, 1, 0,
                     1, 1,-1,-4,-1, 1, 1,
                     1, 1,-1,-8,-1, 1, 1,
                     1, 1,-1,-4,-1, 1, 1,
                     0, 1, 1, 1, 1, 1, 0,
                     0, 0, 1, 1, 1, 0, 0
                  ));
    ),
    ( // 60
    Name :'Emboss Color +';
    Category : cfcArtistic;
    Bias:0;
    Divisor:1;
    MatrixType : mct3x3;
    MatrixSize : 3;
    Matrix  : (_3 :( 2,  3, -3,
                     1, -1,  1,
                    -2,  1, -2
                   ));
    ),
    ( // 61
    Name :'Ghost';
    Category : cfcArtistic;
    Bias:0;
    Divisor:-29;
    MatrixType : mct3x3;
    MatrixSize : 3;
    Matrix  : (_3 :( 11,-33, -4,
                     3,  -4, -3,
                    36, -40,  3
                   ));
    ),
    ( // 62
    Name :'Freeze';
    Category : cfcArtistic;
    Bias:0;
    Divisor:-1;
    MatrixType : mct3x3;
    MatrixSize : 3;
    Matrix  : (_3 :( 18,-19, 15,
                    -14,  5, -6,
                      8,-17,  7
                   ));
    ),
    ( // 63
    Name :'Fuzzy glass';
    Category : cfcArtistic;
    Bias:0;
    Divisor:7;
    MatrixType : mct3x3;
    MatrixSize : 3;
    Matrix  : (_3 :( 0, 20, 0,
                    20,-59,20,
                     0, 13, 0
                   ));
    )
  );

{%endregion%}

{%region%-----[ Gestion d'image bitmap ]---------------------------------------------------}

Type
  { TClasse décrivant les informations utiles pour décoder les données brute du bitmap.@br
    On stocke ici toutes les informations relatives à  la bonne marche des importations et exportations des données d'un format à l'autre.}
  TBZRawImageDescription = Class(TBZUpdateAbleObject)
   private
     // Largeur hauteur et taille totale des données
     FWidth  : Integer;
     FHeight : Integer;
     FSize   : Int64;
     FFrameCount : Integer;
     // Format de couleur utilisé par le bitmap
     FColorFormat : TBZColorFormat;

     // Nombre de bits par pixel définit en fonction du format de couleur
     FPixelFormat : TBZPixelFormat;

     { Taille d'un pixel

       Ex BitPerPixel = 24bits = PixelSize = 3, BitCount = 24
          BitPerPixel = 32bits = PixelSize = 4, BitCount = 24
          BitPerPixel = 32bits = PixelSize = 4, BitCount = 32
          BitPerPixel = 16bits = PixelSize = 2, BitCount = 15
          BitPerPixel = 16bits = PixelSize = 2, BitCount = 16

       Nb : La valeur BitCount est définie en fonction des valeurs Alpha de la variable "BitFields" (cf plus bas) }
     FPixelSize : Byte;
     FBitCount : Byte; // Le bitCount peut être différent de BitsParPixel
     FBitsPerPixel : Byte;
     // FPixelDataSize

     { Type de padding de fin de ligne pour aligner les données suivant
       un nombre de bits précis. Par défaut pas de padding = bleDWordBoundary }
     FRowStrideType : TBZBitmapLineEnd;
     FRowStride : byte;


     // Pour définir l'ordre de lecture pour les formats 1bits
     FBitOrder : TBZBitmapBitOrder;

     // Ordre des ligne de haut en bas ou de bas en haut
     FLineOrder : TBZBitmapLineOrder;

     { Juste pour information. Les infos "Swap" sont définies en fonction du format de couleur
       Les Valeurs Rouge et Bleu sont inversée format BGR / BGRA
     FSwapRB : Boolean;

     // La Valeurs Alpha est Inversée ARGB<>RGBA
     FSwapAlpha : Boolean;      }

     { Dans le cas des formats 16bits ou 32bits on determine si le canal alpha est valide.
      Ex on lit un fichier bmp dont toutes les valeurs alpha sont à 0 (transparent) FHasAlpha vaudra False
      Ce paramètre est utilisé principalement dans la procedure d'affichage du bitmap (BZBitmap.pas : procedure DrawBZBitmapToCanvas )
      afin de determiner si l'on doit afficher en tenant compte de la transparence }
     FHasAlpha : Boolean;

     FUsePalette : Boolean;
     FPaletteCount : Integer;
     FPaletteEntries : PBZPaletteEntries;
     { Normalemnt, ces valeur doivent être en adéquation avec un des format de couleurs "ColorFormat" supportés }
     FBitFields : TBZColorBitFields;
     FTransparentColor : TBZColor;
     FInterlaced : Boolean;
     FDPI : Integer;
     FPrintSizeWidthinCm : Integer;
     FPrintSizeHeightinCm : Integer;

     //procedure setColorFormat(const Value:TBZColorFormat);
     procedure setWidth(Const Value : Integer);
     procedure setHeight(Const Value : Integer);
     procedure setPixelFormat(Const Value : TBZPixelFormat);
     procedure setColorFormat(Const Value : TBZColorFormat);
     procedure setRowStrideType(Const Value : TBZBitmapLineEnd);
     procedure setPaletteCount(Const NewSize:Integer);

     function GetBytesPerLine: PtrUInt;
     function GetBitsPerLine: PtrUInt;
     function GetBitCount : Byte;
     function getRowStrideSize:Integer;
     function getRowStrideLength:Byte;
   public
     Constructor Create; override;
     Destructor Destroy; override;

     procedure Assign(Source : TPersistent); Override;
     { UpdateSize : Mise à jour des dimensions }
     procedure UpdateSize(Const NewWidth, NewHeight : Integer);
     { InitDefault : Initialisation par defaut du format en corrélation avec les paramètres de l'OS }
     procedure InitDefault(Const AWidth, AHeight : Integer; Const ADepth: Byte);
     { Initialise certain paramètres de la descriptions
     @groupbegin }
     procedure Init(aColorFormat:TBZColorFormat);
     procedure Init(aColorFormat:TBZColorFormat;aPixelFormat : TBZPixelFormat);overload;
     procedure Init(aColorFormat:TBZColorFormat;aPixelFormat : TBZPixelFormat;const aLineOrder : TBZBitmapLineOrder); overload;
     procedure Init(const aLineOrder : TBZBitmapLineOrder;Const aPixelFormat:TBZPixelFormat;aPixelSize:Byte;Const aSwapRB, aSwapAlpha : Boolean); overload;
     { @groupend }

     { Vérifie et corrige les informations sur la transparence suivant le format. @br
     Cette fonction doit être publique pour être appeler dans certain cas. Lors du chargement d'un fichier par exemple }
     procedure CheckAlphaDescription;
     { Retourne la description complète de l'image sous forme de chaine de caractères }
     function getDescriptionAsString:String;

     // proceudre InitFromDevice;
     { Largeur du l'image }
     property Width:Integer read FWidth Write setWidth;
     { Largeur du l'image }
     property Height:Integer read FHeight Write setHeight;
     { Taille de l'image (Taille = (Largeur * Hauteur) * Taille d'un pixel en octet }
     property Size:Int64 read FSize write FSize;
     { Format couleur de l'image : RGB, BGRA, GrayScale,... }
     property ColorFormat : TBZColorFormat read FColorFormat write FColorFormat; //setColorFormat;
     { Format du pixel : @br - pf1bit ,pf2bits, pf4bits, pf8bits, pf15bits, @br
                                           - pf16bits ,pf32bits, pf48bits, pf64bits, pf96bits ,pf128bits}
     property PixelFormat : TBZPixelFormat read FPixelFormat write setPixelFormat;
     { Format du pixel 1,2,4,8,16,32,48,64,96,128 bits}
     property BitsPerPixel:Byte Read FBitsPerPixel;
     { Taille d'un pixel en octet. @br Exemple : 1bits := 1 div 8, 8bits = 1, 32bits = 4 }
     property PixelSize : Byte Read FPixelSize;
     { Nombre de bits réelement pris en compte. Il peux donc être différent du BitsPerPixel. @br
       Exemple : Le pixel format est pf32bits. Mais tous les pixels de l'image sont opaque (valeur alpha = 255). @br
       On peux alors considérer, vu que l'image est entièrement opaque que le BitCount est 24. On n'utilise que les canaux RGB. }
     property BitCount:Byte Read GetBitCount;
     { Informations contenant les masques et les décalages de bits afin de pouvoir
       décoder n'importe quel format de couleur depuis n'importe quel type et taille de pixel (1,2,..8,15,16,32...bits)}
     property BitFields : TBZColorBitFields read FBitFields write FBitFields;
     { Definit la façon dont les lignes de l'image sont alignées }
     property RowStrideType : TBZBitmapLineEnd read FRowStrideType write setRowStrideType;
     { Taille de l'alignement des données }
     property RowStrideSize : Integer read getRowStrideSize;
     { Longueur de l'alignement des données de l'image. (0,1,2,3...) @br
       Calculer en fonction de RowStrideType et RowStrideSize  }
     property RowStrideLength : Byte read getRowStrideLength;
     { Nombre d'octets d'une ligne de l'image }
     property BytesPerLine : PtrUInt read GetBytesPerLine;
     { Nombre de Bits d'une ligne de l'image }
     property BitsPerLine : PtrUInt read GetBitsPerLine;
     { Agencement des lignes de l'image de bas en haut ou de haut en bas }
     property LineOrder : TBZBitmapLineOrder Read FLineOrder Write FLineOrder;
     { L'image utilise la transparence }
     property HasAlpha : Boolean Read FHasAlpha Write FHasAlpha;
     { Couleur de transparence }
     property TransparentColor : TBZColor Read FTransparentColor Write FTransparentColor;
     { Utilisation d'une palette Oui/Non }
     Property UsePalette : Boolean read FUsePalette write FUsePalette;
     { Nombre de couleur dans la palette }
     Property PaletteCount : Integer read FPaletteCount write SetPaletteCount;
     { Pointeur sur le tableau contenant la palette }
     Property PaletteEntries : PBZPaletteEntries read FPaletteEntries write FPaletteEntries;
     { Nombre de frame. Uniquement pour les formats de fichier multi images (GIF, APNG, MNG, TIFF) }
     property FrameCount : Integer Read FFrameCount Write FFrameCount;
     { Indique si l'image est entrelacée }
     property Interlaced : Boolean Read FInterlaced Write FInterlaced;

     property WidthInCm : Integer read FPrintSizeWidthInCm write FPrintSizeWidthInCm;
     property HeightInCm : Integer read FPrintSizeHeightInCm write FPrintSizeHeightInCm;
     property DPI : Integer read FDPI write FDPI;


   end;

 { Classe "semi-abstraite"  décrivant les informations de données brute d'un bitmap (image).@br
   On stocke ici sa description, ainsi que la palette de couleur "brute" si elle existe.@br
   Les informations sont à mettre à jour manuellement. Lors du chargement d'un fichier par exemple
   ou lors de la création d'une nouvelle image. @br

   Les données des brutes, ne sont pas stockées. Ici, car dans BZScene, lors
   du chargement d'un bitmap celui-ci est automatique convertit vers le format
   RGBA ou BGRA suivant l'OS. (cf : @link(TBZBitmap) ). @br
   Une fois l'image chargée et la description remplie. On peux modifier, certain
   paramètres comme les propriétés PixelFormat, ou ColorFormat pour l'export des
   données. Celles-ci seront alors convertient depuis les données contenue dans TBZBitmap. }
  TBZRawImageData = Class(TBZUpdateAbleObject)
  Private

    //FRawData : PByte;   // N'est pas utilisé pour des raison de performance
    FDescription : TBZRawImageDescription;

    //Palette Infos

    FUsePalette : Boolean;
    // FPalette : TBZImageRawPalette
    FPaletteCount : Integer;
    FPaletteEntries : PBZPaletteEntries;
    // PaletteOrder

    function getDescription: TBZRawImageDescription;

   // procedure setData(Const Value : PByte);
    procedure setPaletteCount(Const NewSize:Integer);
  protected

     procedure FreeData;
  public
    Constructor Create; override;
    Destructor Destroy; override;

    procedure Assign(Source : TPersistent);  override;

    { Init : Initialisation de base de l'image }
    procedure Init(Const aWidth, aHeight:Integer;aColorFormat: TBZColorFormat);
    { UpdateSize : Mise à jour des dimension seulement }
    procedure UpdateSize(const aWidth,aHeight:Integer);

   // procedure ConvertPixelFormat(Const NewPixelFormat:TBZPixelFormat);
   // procedure ConvertColorFormat(Const NewColorFormat:TBZColorFormat);

    { Retourne la description complète de l'image sous forme de chaine de caractères }
    function getDescriptionAsString:String;

    { Fonction générique pour accéder aux valeurs d'un pixel dans un tampon "P" de taille "BPP" (Bit Par Pixel) }
    function GenericGetPixel(P: PByte; BPP: Byte): Cardinal;
    { Fonction générique pour compter le nombre de pixel différent }
    function GenericCountDiffPixels(P: PByte; BPP: Byte; Count: Integer): Integer;
    { Fonction générique pour compter le nombre de pixel identique }
    function GenericCountSamePixels(P: PByte; BPP: Byte; Count: Integer): Integer;

    //property RawData : PByte Read FRawData ;//write setData;

    { Informations sur le format de l'image }
    property Description : TBZRawImageDescription read FDescription ;
    { Utilisation d'une palette Oui/Non }
    Property UsePalette : Boolean read FUsePalette write FUsePalette;
    { Nombre de couleur dans la palette }
    Property PaletteCount : Integer read FPaletteCount write SetPaletteCount;
    { Pointeur sur le tableau contenant la palette }
    Property PaletteEntries : PBZPaletteEntries read FPaletteEntries write FPaletteEntries;
  end;

{%region%=====[ TBZCustomBitmap ]==========================================}

  TBZCustomBitmap = Class;
  { Objet pour stocker des images supplémentaire dans un TBZCustomBitmap.@br
    Utile dans le cas d'images animées (gif, apng) ou de fichier multi-images (tiff, mng, psd etc...)
    Peux également être utilisé dans la gestion de calque comme photoshop ou gimp. }
  TBZBitmapLayerItem = Class(TBZUpdateAbleObject)
   Private
     FImage: TBZCustomBitmap;
     {$IFDEF CPU64}
       {CODEALIGN RECORDMIN = 16}
       FPosition : TBZPoint;
       {CODEALIGN RECORDMIN = 4}
     {$ELSE}
       FPosition : TBZPoint;
     {$ENDIF}
     FDrawMode : Byte;
     FDelayTime : Integer;
     FComment : TStringList;
     FIsTransparent : Boolean;
     FIsCorrupted : Boolean;
     // FOverlayMode
     // FOverlayFactor

     Function GetImage: TBZCustomBitmap;
     Procedure SetImage(AValue: TBZCustomBitmap);

     Function GetLeftPosition: Integer;
     Function GetTopPosition: Integer;


     Function GetWidth: Integer;
     Function GetHeight: Integer;
     Procedure SetLeftPosition(AValue: Integer);
     Procedure SetTopPosition(AValue: Integer);

   Protected
   Public
     Constructor Create; Override;
     Destructor Destroy; override;

     procedure Assign(source: TPersistent); override;

     Procedure WriteToFiler(writer: TVirtualWriter);
     Procedure ReadFromFiler(reader: TVirtualReader);

     { Tampon de stockage de l'image }
     Property Bitmap: TBZCustomBitmap read GetImage write SetImage;
     { Position en x du calque }
     property Left : Integer read GetLeftPosition Write SetLeftPosition;
     { Position en Y du calque }
     property Top : Integer read GetTopPosition Write SetTopPosition;
     { Mode d'affichage du calque }
     property DrawMode : Byte Read FDrawMode write FDrawMode;
     { Temps de pause entre chaque frame (image animée seulement. ex : GIF }
     property DelayTime : Integer Read FDelayTime Write FDelayTime;
     { Commentaire }
     property Comment : TStringList  Read FComment;
     { Le calque utilise-t-il la transparence ? }
     property IsTransparent : Boolean Read FIsTransparent Write FIsTransparent;
     { L'image est-elle corrompue. Utiliser lors du chargement d'images animées tel que le format GIF }
     property IsCorrupted : Boolean Read FIsCorrupted Write FIsCorrupted;
     { Largeur du calque }
     property Width : Integer read GetWidth;
     { Hauteur du calque }
     property Height : Integer read GetHeight;

   End;

  { Gestionnaire de liste conteant des TBZBitmapLayerItem }
  TBZBitmapLayerList = Class(TBZPersistentObjectList)
   Private
     FBackgroundColor : TBZColor;
     FTransparent : Boolean;
   Protected
     Function GetImageItem(index: Integer): TBZBitmapLayerItem;
     Procedure SetImageItem(index: Integer; val: TBZBitmapLayerItem);
   Public
     Constructor Create; Override;
     Destructor Destroy; Override;

     procedure Assign(source: TPersistent); override;

     procedure WriteToFiler(writer: TVirtualWriter); override;
     procedure ReadFromFiler(reader: TVirtualReader); override;

     { Efface la liste de Bitmaps Brute}
     Procedure Clear; Override;
     { Ajoute une image à la liste}
     Function AddImage(anImage : TBZCustomBitmap; Const LeftPos: Integer = 0;Const TopPos : Integer = 0; const DrawMode : Byte = 0; Const DelayTime : Integer = 100): Integer; Overload;
     { Ajoute une image à la liste}
     Function AddImage(Const aLayerItem: TBZBitmapLayerItem): Integer; Overload;
     { Ajoute une nouvelle image vide }
     Function AddNewImage : TBZBitmapLayerItem;

     { Acces aux  éléments de la liste }
     Property Items[Index: Integer]: TBZBitmapLayerItem read GetImageItem write setImageItem;
     { Couleur de fond global }
     property BackgroundColor : TBZColor Read FbackgroundColor Write FBackgroundColor;
     { Utilisation de la transparence }
     property Transparent : Boolean Read FTransparent Write FTransparent;
   End;

  { Tableau de DWord servant a stocker les adresses de début de chaque ligne du bitmap }
  TBZScanLineLUT = Array Of DWord;
  { Classe abstraite de base pour la gestion de bitmap.@br
    Cette classe doit-être hérité pour être utilisée à 100%. (cf TBZBitmap) }

  { TBZCustomBitmap }

  TBZCustomBitmap = Class(TBZCustomDataFile)
  Private
    { FImageDescription est ici pour gérer les informations relative aux données du bitmap
      lui même. c'est à dire que seul Le tampon des données est modifié lorsque
      l'on demande l'accès au "pointer" des donées de celui-ci.
      Exemples : Le tampon est mis à jour lors du chargement ou de la sauvegarde
      du bitmap en synchroniation avec le tampon de travail de "TBZBitmap" (cf en dessous)

      TODO : Lors du chargement le format des données est convertit suivant le
      format et le nombre de bits utilisés en interne (cf juste en dessous) }
    FImageDescription: TBZRawImageDescription;
    FHeight, FWidth: Integer;
    FPixelFormat: TBZPixelFormat;

    { On gère les données en 32bits RGBA ou BGRA suivant le type OS. }
    FSurfaceBuffer:  PDWord; //PBZColor;

    FMultiImage: Boolean;
    FLayerIndex: Integer;
    FLayers: TBZBitmapLayerList;
    FLayerAutoSize: Boolean; // Si vrai les dimensions du TBZCustomBitmap sont mises a jour sinon la frame est transférée suivant les "HotSpots"

    FTransparentColor : TColor; // Couleur transparent  pour l'affichage via TBitmap de la LCL si besoin
    { On va stoquer les adresses de chaque ligne de notre bitmap
      On évite ainsi encore de nombreuse opérations, lors des accès aux données }
    FScanLineLUT: TBZScanLineLUT;


    { Optimisations du traitements des données. On va précalculer quelques constantes
      pour nous épargner de nombreux petits calculs inutiles plus tard, lors de
      la manipulation des données. Tout ceci afin d'optimiser les performances d'acces.}
    FMaxHeight, FMaxWidth: Integer;
    FSize, FMaxSize:    Int64;
    FCenterX, FCenterY: Integer;
    FLineSizeInBytes : Integer;

    { Utilise t-on une palette de couleur ? Si oui
      Pixel format 8bits = codé sur 32Bits
      Le pixel représente un index dans la palette de couleur

      NOTES : Lorsque vous modifier les valeurs des index du bitmap
              les changements sont également répercutés automatiquement dans les
              données principales si le FLAG "FAutoUpdateIndexBuffer" est à TRUE }
    FUsePalette:   Boolean;
    FColorManager: TBZColorsManager; //TBZColorList;

    { Ici on stocke un indice (Integer) si notre bitmap utilise une palette de couleur }
    FImageIndexBuffer:      PDWord;
    //FAutoUpdateIndexBuffer: Boolean;

    FClipping: Boolean;
    FClipRect: TBZRect;

    FIsPremultiply : Boolean;

    Function getImageDescription: TBZRawImageDescription;
    Function GetIsMultiImage: Boolean;

    Procedure setUsePalette(Const aValue: Boolean);
    Function getColorManager: TBZColorsManager;

    // Pre-Calcul des adresses "ScanLine"
    Procedure ComputeScanLineLUT;

    procedure SetLayerIndex(Value:Integer);

  Protected
    Procedure setWidth(Const AValue: Integer);
    Function getWidth: Integer;

    Procedure setHeight(Const AValue: Integer);
    Function getHeight: Integer;

    Function GetInternalPixelColor(x, y: Integer): Integer; Virtual;
    Procedure SetInternalPixelColor(x, y: Integer; Value: Integer); Virtual;

    Function CloneSurfaceBuffer(Const BufferData: PByte; Var Clone: PByte): Boolean;
    procedure AddLayer(anImage:TBZCustomBitmap);
    // Can resize buffer in children. Using in TBZGLBitmap
    //procedure ReAllocBuffer(ANewSize: Longword);virtual;

    procedure Changed; virtual;

  Public
    Constructor Create; override;
    Constructor Create(AOwner: TPersistent; AWidth, AHeight: Integer);Overload; Virtual;
    Constructor Create(aWidth, aHeight: Integer); Overload; Virtual;

    //constructor CreateFromBitmap;
    //constructor CreateFromLazIntfImage;
    //constructor CreateFromImageDescription;
    //constructor CreateFromStream;
    //constructor CreateFromMemory;

    Destructor Destroy; Override;

    { Assigne un autre TBZCustomBitmap  }
    Procedure Assign(Source: TPersistent); Override;
    // function Clone : TBZCustomBitmap; virtual;

    { Copie rapide d'un autre Bitmap. ATTENTION les dimensions doivent être identique }
    procedure FastCopy(Source : TBZCustomBitmap);

    { Charge un bitmap depuis un fichier }
    Procedure LoadFromFile(Const FileName: String); Override;

    Function CheckPixelBound(X, Y: Integer): Boolean; Inline;
    { Modifie la taille du tampon d'affichage, sans préservation des données }
    Procedure SetSize(NewWidth, NewHeight: Integer); overload;
    { Modifie la taille du tampon d'affichage, sans préservation des données }
    procedure SetSize(ANewSize:LongWord); overload;

    { Retourne l'adresse dans le tampon d'affichage de la ligne "Row" }
    Function GetScanLine(Row: LongInt): PBZColor;
    { Retourne la table précalculée des adresses de début des lignes }
    Function GetScanLineLUT : TBZScanLineLUT;

    //Function GetIndexedScanLine(Row: Integer): PByte;

    { Retourne l'adresse dans le tampon d'affichage de la position "X,Y" }
    Function GetPixelPtr(X, Y: Integer): PBZColor;
    //Function GetPixelIndexPtr(X, Y: Integer): PDWord;

    { Retourne le tampon d'affichage }
    Function getSurfaceBuffer: PBZColor;
    { Retourne le tampon d'affichage pour les image indexées }
    Function getSurfaceIndexedBuffer: PDWord;

    { Retourne la couleur (TBZColor) du pixel à la postion "X,Y" }
    Function getPixel(x, y: Integer): TBZColor;
    { Retourne la couleur (TBZColor) du pixel à la postion "X,Y" avec traitement des bords en fonction de "EdgeAction" (TBZPixelEdgeAction) }
    function getPixel(x,y : Integer; EdgeAction : TBZPixelEdgeAction) : TBZColor;
    { Retourne la couleur (TBZColor) du pixel à la postion "Offset" }
    Function getPixelOffset(Offset: Integer): TBZColor;
    { Retourne la couleur (TBZColor) du pixel à la postion "X,Y". Si la position dépasse des limite celle-ci boucle sur elle même }
    Function getPixelCycle(x, y: Integer): TBZColor;
    { Ecrit un pixel à la position "X,Y" de couleur "aValue" }
    Procedure setPixel(x, y: Integer; aValue: TBZColor);
    { Ecrit un pixel à la position "Offset" de couleur "aValue" }
    Procedure setPixelOffset(Offset: DWord; aValue: TBZColor);
    { Ecrit un pixel à la position "x,y" en mixant la couleur "aValue" avec celle du fond }
    Procedure setPixelAlphaBlend(x, y: Integer; aValue: TBZColor);
    { Ecrit un pixel à la position "x,y" de couleur "aValue" seulement si la valeur de Alpha est plus grande que 0}
    Procedure setPixelCheckAlpha(x, y: Integer; aValue: TBZColor);
    { Retourne l'index de la couleur du pixel à la postion "X,Y" présente dans la palette}
    Function GetPixelColorIndex(X, Y: Integer): Integer;
    { Ecrit l'index de la couleur du pixel à la postion "X,Y" présente dans la palette}
    Procedure SetPixelColorIndex(X, Y, Value: Integer);

    { Retourne le pixel suréchantilloner avec la methode "NearestNeighbour" à la position x,y }
    function GetPixelNeighbour(x, y: Integer; Const EdgeAction : TBZPixelEdgeAction = peaClamp): TBZColor;
    { Retourne le pixel suréchantilloner avec la methode "Bilinéaire" à la position x,y }
    function GetPixelBilinear(x, y: Single; Const EdgeAction : TBZPixelEdgeAction = peaClamp): TBZColor;
    { Retourne le pixel suréchantilloner avec la methode "Bicubic" à la position x,y }
    function GetPixelBicubic(x, y: Single; Const EdgeAction : TBZPixelEdgeAction = peaClamp): TBZColor;
    { Retourne le pixel suréchantilloner avec la methode "Mean"(moyenne d'un carré de 3x3 par défaut) à la position x,y }
    function GetPixelMean(x, y: Integer; Const FilterSize : word = 3; Const EdgeAction : TBZPixelEdgeAction = peaClamp): TBZColor;
    { Retourne le pixel suréchantilloner avec la methode "Median"(d'un carré de 3x3 par défaut) à la position x,y }
    function GetPixelMedian(x, y: Integer; Const FilterSize : word = 3; Const EdgeAction : TBZPixelEdgeAction = peaClamp): TBZColor;
    { Retourne le pixel suréchantilloner avec la methode "Min"(d'un carré de 3x3 par défaut) à la position x,y }
    function GetPixelMin(x, y: Integer; Const FilterSize : word = 3; Const EdgeAction : TBZPixelEdgeAction = peaClamp): TBZColor;
    { Retourne le pixel suréchantilloner avec la methode "Max"(d'un carré de 3x3 par défaut) à la position x,y }
    function GetPixelMax(x, y: Integer; Const FilterSize : word = 3; Const EdgeAction : TBZPixelEdgeAction = peaClamp): TBZColor;
    { Retourne le pixel suréchantilloner avec la methode "Max"(d'un carré de 3x3 par défaut) à la position x,y }
    function GetPixelMinMax(x, y: Integer; Const FilterSize : word = 3; Const EdgeAction : TBZPixelEdgeAction = peaClamp): TBZColor;

    function GetPixelSubSample(x, y: Single; Const FilterSize : word = 3; Const EdgeAction : TBZPixelEdgeAction = peaClamp): TBZColor;
    { Retourne le pixel suréchantilloner à la position x, y en fonction de la méthode "SubSampleMode" (TBZGetPixelSampleMethod) choisie @br
      et gestion des bords en fonction de "EdgeAction" (TBZPixelEdgeAction). Par défaut peaRGBClamp }
    function GetSamplePixel(x,y : Single; SubSampleMode : TBZGetPixelSampleMethod;  Const FilterSize : word = 3; Const EdgeAction : TBZPixelEdgeAction = peaClamp) : TBZColor;


    //Procedure DrawPixel(Const x, y: Integer; Const Col: TBZColor; Const ADrawMode: TBZBitmapDrawMode = dmSet;
    //                        Const AAlphaMode: TBZBitmapAlphaMode = amAlpha; Const SrcFactor: TBZBlendingFactor = bfOne;
    //                        Const DstFactor: TBZBlendingFactor = bfDstAlpha); virtual;

    { Retourne l'intensité maximale de lumière globale de l'image }
    function getIntensityMax : Byte;
    { Retourne l'intensité minimal de lumière globale de l'image }
    function getIntensityMin : Byte;
    { Retourne l'intensité moyenne de lumière globale de l'image }
    function getIntensityAverage : Byte;
    { Retourne la variance globale de l'image }
    function GetVariance(IntensityAverage : Byte) : Single;
    { Retourne l'energie globale de l'image }
    function GetEnergy : Single;

    { Affiche un pixel en tenant compte des paramètres }
    procedure DrawPixel(x,y : Integer; Const ForeColor : TBZColor;
                        Const ADrawMode : TBZBitmapDrawMode = dmSet ; Const AAlphaMode : TBZBitmapAlphaMode = amNone;
                        Const MasterAlpha : Byte = 255;
                        Const CombineMode : TBZColorCombineMode = cmNormal;
                        Const BlendSrcFactor: TBZBlendingFactor = bfOne;
                        Const BlendDstFactor: TBZBlendingFactor = bfDstAlpha); Virtual;

    { multiplie les canaux RGB par la valeur de l'Alpha, utile lors de l'affichage des images avec transparence }
    Procedure PreMultiplyAlpha;
    { Divise les canaux RGB par la valeur de l'Alpha }
    Procedure UnPreMultiplyAlpha;
    { Efface le tampon d'affichage par la couleur "aColor" }
    Procedure Clear(aColor: TBZColor); Overload;
    { Efface le tampon d'affichage par la couleur définie dans la palette de couleur d'index "aColorIndex" }
    Procedure Clear(aColorIndex: Integer); Overload;

    { Transfert une partie d'un autre Bitmap suivant les paramètres }
    Procedure PutImage(Const ASrcBmp: TBZCustomBitmap; SrcX, SrcY, cfWidth, cfHeight: Integer; DstX, DstY: Integer;
                       Const ADrawMode : TBZBitmapDrawMode = dmSet ; Const AAlphaMode : TBZBitmapAlphaMode = amNone;
                       Const MasterAlpha : Byte = 255;
                       Const CombineMode : TBZColorCombineMode = cmNormal;
                       Const BlendSrcFactor: TBZBlendingFactor = bfOne;
                       Const BlendDstFactor: TBZBlendingFactor = bfDstAlpha);  Overload;

    procedure PutImage(Const ASrcBmp: TBZCustomBitmap;DstX,DstY:Integer);overload;
    procedure PutImage(Const ASrcBmp: TBZCustomBitmap;DstX,DstY:Integer; MasterAlpha: Byte);overload;
    procedure PutImage(Const ASrcBmp: TBZCustomBitmap;DstX,DstY:Integer; MasterAlpha: Byte; ADrawMode : TBZBitmapDrawMode ; AAlphaMode : TBZBitmapAlphaMode);overload;
    procedure PutImageBlend(Const ASrcBmp: TBZCustomBitmap;DstX,DstY:Integer;Const CombineMode : TBZColorCombineMode = cmNormal;Const MasterAlpha : Byte = 255);

   //function GetImage(SrcX,SrcY,SrcW,SrcH:Integer):TBZBitmap;overload;
   //function GetImage(ARect:TBZRect):TBZBitmap;overload;

    (* procedure CopyTo(Const ADstBmp:TBZCustomBitmap; SrcX, SrcY, cfWidth, cfHeight:integer;DstX, DstY:Integer ;
                        const ADrawMode: TBZBitmapDrawMode=dmSet;
                        Const AAlphaMode: TBZBitmapAlphaMode=amOpaque;
                        const SrcFactor: TBZBlendingFactor = bfOne;
                        const DstFactor: TBZBlendingFactor = bfDstAlpha);
    *)

    { Transfert un autre Bitmap et le redimensionne en suivant les paramètres }
    procedure PutImageStretch(Source : TBZCustomBitmap; DestLeft, DestTop, DestRight, DestBottom : Integer;
                       Const ADrawMode : TBZBitmapDrawMode = dmSet ; Const AAlphaMode : TBZBitmapAlphaMode = amNone;
                       Const MasterAlpha : Byte = 255;
                       Const CombineMode : TBZColorCombineMode = cmNormal  ); overload;
    { Transfert un autre Bitmap et le redimensionne en suivant les paramètres }
    procedure PutImageStretch(Source : TBZCustomBitmap; DestLeft, DestTop, DestRight, DestBottom : Integer;  MasterAlpha : Byte); overload;
    { Transfert un autre Bitmap et le redimensionne en suivant les paramètres, avec prise en charge de la transparence }
    procedure PutImageStretch(Source : TBZCustomBitmap; DestRect : TBZRect;  MasterAlpha : Byte); overload;

    { Transfert un autre Bitmap à la position DstX, DstY en lui faisant subir une rotation de "Angle" et un redimensionnement en fonction de "ZoomFactor".
      Si "MasterAlpha" < 255 alors le bitmap sera dessiné avec transparence
      Si "Warp" est à @True alors l'effet sera un "RotoZoom"}
    procedure PutImageRotateAndScale(Source : TBZCustomBitmap; DstX, DstY : Integer; Angle : Single; ZoomFactor : Single; xOrg, yOrg : Integer; Const MasterAlpha : Byte = 255; Const Warp : Boolean = false);

    { Transfere une bande de la longueur d'un autre bitmap de la position SrcY et de Hauteur aHeight vers la position DstY }
    procedure CopyHorizontalBand(Source : TBZCustomBitmap; SrcY, aHeight , destY : Integer);

    { Transfere rapidement un block à la position "SrcX, SrcY" de dimension "aWidth, aHeight" d'un autre bitmap à la position "DestX, DestY"
      Si CheckAlpha est @True seul les pixels avec un Alpha > 0 sont copiés. (Note : Plus rapide que PutImage }
    procedure CopyBlock(Source : TBZCustomBitmap; SrcX, SrcY, aWidth, aHeight, DestX, destY : Integer; Const CheckAlpha : Boolean =  False); overload;
    { Transfere rapidement un block à la position DstRect d'un autre bitmap à la position "DestX, DestY"
      Si CheckAlpha est @True seul les pixels avec un Alpha > 0 sont copiés. (Note : Plus rapide que PutImage }
    procedure CopyBlock(Source : TBZCustomBitmap; SrcRect : TBZRect; DestX, destY : Integer; Const CheckAlpha : Boolean =  False); overload;

    { Déplace le bitmap sur lui même d'une colone à gauche }
    procedure ShiftLeft;
    { Déplace le bitmap sur lui même d'une colone à droite }
    procedure ShiftRight;
    { Déplace le bitmap sur lui même d'une ligne vers le haut }
    procedure ShiftUp;
    { Déplace le bitmap sur lui même d'une ligne vers le bas }
    procedure ShiftDown;

    procedure TakeDesktopScreenShot; overload;
    procedure TakeDesktopScreenShot(ARect : TBZRect);

    // Procedure NotifyChange(Sender: TObject); Override;
    //    Procedure Notification(Sender: TObject; Operation: TOperation); Virtual;

    { Affiche le bitmap sur un TCanvas de taille ARect.
      Si IsOpaque est à @False alors, affichage avec prise en charge de la transparence.
      Si ClearBK est à @TRUE alors le canvas est effacé avant l'affichage du Bitmap }
    procedure DrawToCanvas(Const ACanvas : TCanvas; Const ARect : TRect; Const IsOpaque:Boolean=True; Const ClearBK : Boolean = False);

    { Importation d'un Bitmap depuis un TRawImage de la LCL}
    function ImportFromRawImage(Const ARawImage:TRawImage):Boolean;
    { Importation d'un Bitmap depuis un TBitmap de la LCL }
    function ImportFromBitmap(Const ABitmap:Graphics.TBitmap):Boolean;
    { Exporte le Bitmap vers un TBitmap de la LCL }
    Function ExportToBitmap: Graphics.TBitmap;

    { Dessine une ligne Horizontale }
    procedure HLine(x,y,x1 : Integer; aColor : TBZColor);
    { Dessine une ligne Verticale }
    procedure VLine(x,y1,y2 : Integer; aColor : TBZColor);
    { Dessine un rectangle }
    procedure Box(x1,y1,x2,y2 : Integer; aColor : TBZColor);
    { Dessine un rectangle plein }
    procedure FillBox(x1,y1,x2,y2 : Integer; aColor : TBZColor);
    { Dessine une ligne }
    procedure Line(x1,y1,x2,y2 : Integer; aColor : TBZColor);
    { Dessine une ligne AntiAliasée }
    procedure LineAA(x1,y1,x2,y2 : Integer; aColor : TBZColor);

    { Largeur du bitmap en pixel }
    Property Width: Integer read getWidth write setWidth;
    { Hauteur du bitmap en pixel }
    Property Height: Integer read getHeight write setHeight;
    { Position du centre en X dans le buffer (ie CenterX = (Width div 2)-1 }
    Property CenterX: Integer read FCenterX;
    { Position du centre en X dans le buffer (ie CenterX = (Height div 2)-1 }
    Property CenterY: Integer read FCenterY;
    { Hauteur maximale dans le buffer = Height-1 }
    Property MaxHeight: Integer read FMaxHeight;
    { Largeur maximale dans le buffer = Width-1 }
    Property MaxWidth: Integer read FMaxWidth;
    { Taille maximum pour le position  relatif au buffer (Dernier Octet du buffer) = Size -1 }
    Property MaxSize: Int64 read FMaxSize;
    { Taille du bitmap : (Width*Height)*4 --- nb : 4 = Taille d'un pixel en octet }
    Property Size: Int64 read FSize;
    {Taille d'une ligne en octet }
    property LineSizeInBytes : Integer read  FLineSizeInBytes;
    { Utilisation de'une palette de couleur (image indexée ) }
    Property UsePalette: Boolean read FUsePalette write setUsePalette;
    { Gestionnaire de couleur. Utile dans l'aide de l'utilisation d'une palette de couleur }
    Property ColorManager: TBZColorsManager read getColorManager;

    { Acces en lecture ou ecriture au pixel à la position "x,y".@br
      Lecture  : @code(aTBZColor := Bmp.Pixels[x,y])@br
      Ecriture : @code(Bmp.Pixels[x,y] := aTBZColor) }
    Property Pixels[x, y: Integer]: TBZColor read GetPixel write SetPixel;

    { Acces en lecture ou ecriture de l'index de couleur  à la position "x,y" (Utilisation de la palette de couleur).@br
      Lecture  : @code(ColorIndexInPalette := Bmp.Colors[x,y])@br
      Ecriture : @code(Bmp.Colors[x,y] := ColorIndexInPalette ) }
    Property Colors[x, y: Integer]: Integer read GetInternalPixelColor write SetInternalPixelColor;
    //property Mask[x,y:integer] : Byte read GetMaskPixel write SetMaskPixel;

    { Toujours a 32bits, n'a aucune incidence sur le bitmap à l'heure actuelle }
    Property PixelFormat: TBZPixelFormat read FPixelFormat write FPixelFormat; // setPixelFormat

    { Objet conteant diverses information sur le bitmap (utiles lors du chargement d'une image) }
    Property ImageDescription: TBZRawImageDescription read getImageDescription;

    { Drapeau indiquant si l'on doit effectuer un "Clipping" lors d'un acces au bitmap }
    Property Clipping: Boolean read FClipping write FClipping;
    { Position et dimension de "clipping" }
    Property ClipRect: TBZRect read FClipRect write FClipRect;

    //property ClippingType : TBZClippingKind Read FClipType Write FClipType;

    { Retourne @True si l'image contient plusieurs images. cf Layers }
    property IsMultiImage : Boolean Read GetIsMultiImage;

    { Liste d'images pour les animations. Peux servir de calques à l'image principale }
    property Layers : TBZBitmapLayerList read FLayers;

    // property LayersCount : Integer read GetLayersCount;
    // Property ExtraInfos : TStrings;

    { Index courrant de l'image séléctionnée dans Layers }
    property LayerIndex : Integer read FLayerIndex write SetLayerIndex;
    { Couleur transparente }
    property TransparentColor : TColor read FTransparentColor write FTransparentColor;
    { Chemin d'accès complet du fichier chargé }
    Property FullFileName;
  End;

  { Classe ajoutant une gestion simple de masque bitmap, et de masque de séléction @br
    Couleur blanche (clrWhite) = Pixel visible @br
    Couleur noir ou transparente (clrBlack, clrTransparent) = Pixel Invisible @br }

  { TBZBaseBitmap }

  TBZBaseBitmap = Class(TBZCustomBitmap)
  private
    procedure SetMask(const AValue : TBZCustomBitmap);
    function GetSelectionMask : TBZCustomBitmap;
    procedure SetSelectionMask(const AValue : TBZCustomBitmap);
  protected
    FMask : TBZCustomBitmap;
    FUseMask : Boolean;
    FMaskApplyAlpha: Boolean;
    FMaskApply : Boolean;
    FSelectionMask : TBZCustomBitmap;
    FUseSelectionMask : Boolean;

    function GetMask : TBZCustomBitmap;
    procedure SetUseMask(Const Value:Boolean); virtual;
    procedure SetUseSelectionMask(Const Value:Boolean); virtual;

    { Définis le masque depuis un bitmap en fonction de la couleur transparente choisie }
    procedure DoSetMaskFromBitmap(Const aBmp, aDest :TBZCustomBitmap; Const aTransparentColor:TBZColor);
    { Définis le masque depuis un bitmap en fonction de la valeur alpha }
    procedure DoSetMaskFromBitmapAlpha(Const aBmp, aDest :TBZCustomBitmap);
    { Définis le masque depuis un bitmap en fonction de l'intensité lumineuse choisie }
    procedure DoSetMaskFromBitmapIntensity(Const aBmp, aDest :TBZCustomBitmap; Const aThresHold : Byte);

  public

    Constructor Create; override;//Overload;

    procedure Assign(Source : TPersistent);  override;
    { Creation d'un clone du bitmap (nouvelle instance) }
    function CreateClone : TBZBaseBitmap; overload;

    { Définis le masque depuis un bitmap en fonction de la couleur transparente choisie }
    procedure SetMaskFromBitmap(Const aBmp:TBZCustomBitmap; Const aTransparentColor:TBZColor);
    { Définis le masque depuis un bitmap en fonction de la valeur alpha }
    procedure SetMaskFromBitmapAlpha(Const aBmp:TBZCustomBitmap);
    { Définis le masque depuis un bitmap en fonction de l'intensité lumineuse choisie }
    procedure SetMaskFromBitmapIntensity(Const aBmp:TBZCustomBitmap; Const aThresHold : Byte);

    { Définis le masque de sélection depuis un bitmap en fonction de la couleur transparente choisie }
    procedure SetSelectionMaskFromBitmap(Const aBmp:TBZCustomBitmap; Const aTransparentColor:TBZColor);
    { Définis le masque de sélection depuis un bitmap en fonction de la valeur alpha }
    procedure SetSelectionMaskFromBitmapAlpha(Const aBmp:TBZCustomBitmap);
    { Définis le masque de sélection depuis un bitmap en fonction de l'intensité lumineuse choisie }
    procedure SetSelectionMaskFromBitmapIntensity(Const aBmp:TBZCustomBitmap; Const aThresHold : Byte);

    property Mask : TBZCustomBitmap read GetMask write SetMask;
    property SelectionMask : TBZCustomBitmap read GetSelectionMask write SetSelectionMask;

    { Si TRUE le masque sera utilisé ????? à supprimer ???? }
    Property UseMask : Boolean read FUseMask Write SetUseMask;
    { Si TRUE le masque sera appliqué }
    property ApplyMask : Boolean Read FMaskApply Write FMaskApply;
    { Si TRUE on applique la transparence, sinon toutes valeurs > 0 sont affichées. }
    property ApplyMaskAlpha : Boolean Read FMaskApplyAlpha Write FMaskApplyAlpha;
    { Si TRUE le masque de selection sera utilisé pour l'application de certains filtres }
    Property UseSelectionMask : Boolean read FUseSelectionMask Write SetUseSelectionMask;
  end;

{%endregion%}

  { Classe de base servant de "conténaire" à  filtres @br
    @bold(Note) : Cette classe est temporaire et en attente d'améliorations futures.
    Et donc par défaut tous ces enfants également. }
  TBZCustomOwnerBitmap = Class(TBZProgressAbleObject)
  Private
    Procedure SetClipping(Const Value: Boolean);
    Function GetClipping: Boolean;

    Procedure SetClipRect(Const Value: TBZRect);
    Function GetClipRect: TBZRect;
  Protected
    OwnerBitmap: TBZBaseBitmap;//TBZCustomBitmap;

    Procedure getClippingBoundInfos(Var x1, y1, x2, y2, ClipWidth, ClipHeight, ClipRectSize, NextLineInc: Integer);

    Property Clipping: Boolean read GetClipping write SetClipping;
    Property ClipRect: TBZRect read GetClipRect write SetClipRect;
  Public
    Constructor Create(Const AOwner: TBZBaseBitmap); virtual; Overload;
    Destructor Destroy; Override;
  End;

  { Type de classe descendante de TBZCustomBitmap }
  TBZBitmapClass = Class Of TBZCustomBitmap;

  { Description pour le support d'un format de fichier "Image" }
  TBZImageFileFormat = Class
  Public
    BaseImageClass: TBZBitmapClass;   //< Class de chargement du format
    Extension:      String;           //< Extension du fichier
    Description:    String;           //< Description du format
    DescResID:      Integer;          //< Numero d'identification
  End;

  { Stock les classes des formats de fichiers supportés }
  TBZImageFileFormatsList = Class(TBZPersistentObjectList)
  Public
    Destructor Destroy; Override;

    { Ajoute la prise en charge d'un format de fichier }
    Procedure Add(Const Ext, Desc: String; DescID: Integer; AClass: TBZBitmapClass);
    { Trouve la classe du format en fonction de l'extension du nom de fichier }
    Function FindExt(ext: String): TBZBitmapClass;
    { Trouve la classe du format en fonction du nom de fichier }
    Function FindFromFileName(Const fileName: String): TBZBitmapClass;
    { Trouve la classe du format depuis un flux }
    Function FindFromStream(Const AStream: TStream): TBZBitmapClass;
    { Efface la classe d'un format de la liste }
    Procedure Remove(AClass: TBZBitmapClass);
    { Construit la liste des extensions supporté en fonction de la liste de classe }
    Procedure BuildFilterStrings(imageFileClass: TBZBitmapClass; Var descriptions, filters: String; formatsThatCanBeOpened: Boolean = True; formatsThatCanBeSaved: Boolean = False);
    { Retourne l'extension du format pris en charge de la classe déclaré à "Index" dans la liste }
    Function FindExtByIndex(index: Integer; formatsThatCanBeOpened: Boolean = True; formatsThatCanBeSaved: Boolean = False): String;
    { Construit la chaine de caractères de filtre pour les boites de dialogue }
    function BuildDialogFilters:String;
    { Construit la chaine de caractères des masques pour les boites de dialogue }
    function BuildFileFilterMask:String;
  End;

  { Lecture de la liste des formats "Image" supportés }
  Function GetBZImageFileFormats: TBZImageFileFormatsList;

  { Retourne l'extension de l'index "index" }
  Function BZImageFileFormatExtensionByIndex(index: Integer): String;

  { Enregistrement d'un type de format }
  Procedure RegisterRasterFormat(Const AExtension, ADescription: String; AClass: TBZBitmapClass);

  { Annulation d'un type de format }
  Procedure UnregisterRasterFormat(AClass: TBZBitmapClass);

{%endregion%}

{%region%=====[ Fonctions utiles ]===================================================}

// 2 fonctions provenant de "Graphics.pp" de la LCL. Renommées et adaptées.

{ Calcul le nombre d'octets par ligne en fonction, du nombre de bits par pixel, de la longueur de la ligne et enfin du nombre de bits. @br
  @bold(nb) : la ligne doit être alignée. }
function Compute_BytesPerLine(AWidth: Cardinal; ABitsPerPixel: Byte; ALineEnd: TBZBitmapLineEnd): PtrUInt;
{ Calcul le nombre de bits par ligne en fonction, du nombre de bits par pixel, de la longueur de la ligne et enfin du nombre de bits. @br
  @bold(nb) : la ligne doit être alignée. }
function Compute_BitsPerLine(AWidth: Cardinal; ABitsPerPixel: Byte; ALineEnd: TBZBitmapLineEnd): PtrUInt;

{ Calcul les nouvelles dimensions d'une image en conservant les proportions }
procedure KeepAspectRatio(Const SrcW, SrcH : Integer; Var NewWidth, NewHeight : Integer);

{ Retourne @TRUE si un point aux coordonées (px,py) se trouve dans le cercle à la postion (cx,cy) et de radius "Radius) }
function PointInCircle(const px,py,cx,cy: Integer; const Radius:Integer): boolean; inline;
{ Retourne @TRUE si deux TBZQuadrangle2D se chevauche }
function OverlapQuadrangle(const Q1, Q2 : TBZQuadrangle2D): Boolean;

//function PointInPolygon(const Pt: TBZVector2f; const Pg: TBZArrayOfFloatPoints): Boolean;

{ Effectue une rotation des points contenus dans le tableaux "Points" d'angle "Angle" en degré autours du point (xOrg, yOrg) }
procedure RotateArrayOfPoints(Var Points : Array of TBZPoint; xOrg, yOrg: Integer; Angle : Single);


{%endregion%}

//===================================================================================================
implementation

uses
  Math, BZSystem, BZUtils, BZLogger ;

{%region%=====[ Constantes et variables globales interne ]======================}



Const
  cLineEndStr : array[bleNoBoundary..bleDQWordBoundary] of string =('Aucun', '8bits', '16bits', '32bits','64bits','128bits');
  cLineOrderStr : Array[bloTopToBottom..bloBottomToTop] of string =('de haut en bas', 'de bas en haut');

Var
  vBZImageFileFormats:  TBZImageFileFormatsList;
  PreMultipliedAlphaLUT: Array[0..255, 0..255] Of Byte;

  //DefaultCombineBlendMapLUT : TBZCustomBlendMap;

{%endregion%}

{%region%====[ Fonctions Utiles ]===============================================}

function GetPixelSize(aPixelFormat:TBZPixelFormat):Byte;
begin
 result:=0;
 Case aPixelFormat of
   pfDefault : result:=4;
   pf1bit, pf2bits, pf4bits, pf8bits : result:=1;
   //pf15bit : result:=2;
   pf16bits : result:=2;
   pf24bits : result:=3;
   pf32bits : result:=4;
   pf48bits : result:=6;
   pf64bits : result:=8;
   pf96bits : result:=12;
   pf128bits : result:=16;
 end;
end;

function GetBitsPerPixel(aPixelFormat:TBZPixelFormat):Byte;
begin
 result:=0;
 Case aPixelFormat of
   pfDefault : result:=32;
   pf1bit    : result:=1;
   pf2bits   : result:=2;
   pf4bits   : result:=4;
   pf8bits   : result:=8;
   pf16bits  : result:=16;
   pf24bits  : result:=24;
   pf32bits  : result:=32;
   pf48bits  : result:=48;
   pf64bits  : result:=64;
   pf96bits  : result:=96;
   pf128bits : result:=128;
 end;
end;

function Compute_BytesPerLine(AWidth: Cardinal; ABitsPerPixel: Byte; ALineEnd: TBZBitmapLineEnd): PtrUInt;
begin
  Result := (Compute_BitsPerLine(AWidth, ABitsPerPixel, ALineEnd) + 7) shr 3;
end;

function Compute_BitsPerLine(AWidth: Cardinal; ABitsPerPixel: Byte; ALineEnd: TBZBitmapLineEnd): PtrUInt;
begin

  Result := AWidth * ABitsPerPixel;

  case ALineEnd of
    bleNoBoundary: ;
    bleByteBoundary:   Result := (Result +  7) and not PtrUInt(7);
    bleWordBoundary:   Result := (Result + 15) and not PtrUInt(15);
    bleDWordBoundary:  Result := (Result + 31) and not PtrUInt(31);
    bleQWordBoundary:  Result := (Result + 63) and not PtrUInt(63);
    bleDQWordBoundary: Result := (Result +127) and not PtrUInt(127);
  end;
end;

procedure KeepAspectRatio(Const SrcW, SrcH : Integer; Var NewWidth, NewHeight : Integer);
Var
  w, h: Integer;
Begin
  W := NewWidth;
  H := newHeight;
  if ( SrcW > 1) and ( SrcH > 1) then // On calcul le ratio par rapport aux dimensions du buffer d'affichage
  begin
    H:=Trunc((SrcH*W) / SrcW);
    if (H>=NewHeight) then
    begin
      H:=NewHeight;
      W:=Trunc((SrcW*H) / SrcH);
    end;
    NewWidth := W;
    NewHeight := H;
  end;
End;

function PointInCircle(const px,py,cx,cy: Integer; const Radius:Integer): boolean; inline;
var x,y: single;
begin
  x := cx - px;
  y := cy - py;
  result := (x * x + y * y) < (radius * radius);
end;


//---------------------------------------------------------------------------


// Importé des la librairie Asphyre 3.10
function OverlapQuadrangle(const Q1, Q2 : TBZQuadrangle2D): Boolean;
var
 d1, d2, d3, d4: Single;

begin

 d1 := (Q1[2].X - Q1[1].X) * (Q2[0].X - Q1[0].X) + (Q1[2].Y - Q1[1].Y) * (Q2[0].Y - Q1[0].Y);
 d2 := (Q1[3].X - Q1[2].X) * (Q2[0].X - Q1[1].X) + (Q1[3].Y - Q1[2].Y) * (Q2[0].Y - Q1[1].Y);
 d3 := (Q1[0].X - Q1[3].X) * (Q2[0].X - Q1[2].X) + (Q1[0].Y - Q1[3].Y) * (Q2[0].Y - Q1[2].Y);
 d4 := (Q1[1].X - Q1[0].X) * (Q2[0].X - Q1[3].X) + (Q1[1].Y - Q1[0].Y) * (Q2[0].Y - Q1[3].Y);

 if (d1 >= 0) and (d2 >= 0) and (d3 >= 0) and (d4 >= 0) then
 begin
  Result := True;
  Exit;
 end;

 d1 := (Q1[2].X - Q1[1].X) * (Q2[1].X - Q1[0].X) + (Q1[2].Y - Q1[1].Y) * (Q2[1].Y - Q1[0].Y);
 d2 := (Q1[3].X - Q1[2].X) * (Q2[1].X - Q1[1].X) + (Q1[3].Y - Q1[2].Y) * (Q2[1].Y - Q1[1].Y);
 d3 := (Q1[0].X - Q1[3].X) * (Q2[1].X - Q1[2].X) + (Q1[0].Y - Q1[3].Y) * (Q2[1].Y - Q1[2].Y);
 d4 := (Q1[1].X - Q1[0].X) * (Q2[1].X - Q1[3].X) + (Q1[1].Y - Q1[0].Y) * (Q2[1].Y - Q1[3].Y);
 if (d1 >= 0) and (d2 >= 0) and (d3 >= 0) and (d4 >= 0) then
 begin
  Result := True;
  Exit;
 end;

 d1:= (Q1[2].X - Q1[1].X) * (Q2[2].X - Q1[0].X) + (Q1[2].Y - Q1[1].Y) * (Q2[2].Y - Q1[0].Y);
 d2:= (Q1[3].X - Q1[2].X) * (Q2[2].X - Q1[1].X) + (Q1[3].Y - Q1[2].Y) * (Q2[2].Y - Q1[1].Y);
 d3:= (Q1[0].X - Q1[3].X) * (Q2[2].X - Q1[2].X) + (Q1[0].Y - Q1[3].Y) * (Q2[2].Y - Q1[2].Y);
 d4:= (Q1[1].X - Q1[0].X) * (Q2[2].X - Q1[3].X) + (Q1[1].Y - Q1[0].Y) * (Q2[2].Y - Q1[3].Y);
 if(d1 >= 0) and (d2 >= 0) and (d3 >= 0) and (d4 >= 0) then
 begin
  Result := True;
  Exit;
 end;

 d1:= (Q1[2].X - Q1[1].X) * (Q2[3].X - Q1[0].X) + (Q1[2].Y - Q1[1].Y) * (Q2[3].Y - Q1[0].Y);
 d2:= (Q1[3].X - Q1[2].X) * (Q2[3].X - Q1[1].X) + (Q1[3].Y - Q1[2].Y) * (Q2[3].Y - Q1[1].Y);
 d3:= (Q1[0].X - Q1[3].X) * (Q2[3].X - Q1[2].X) + (Q1[0].Y - Q1[3].Y) * (Q2[3].Y - Q1[2].Y);
 d4:= (Q1[1].X - Q1[0].X) * (Q2[3].X - Q1[3].X) + (Q1[1].Y - Q1[0].Y) * (Q2[3].Y - Q1[3].Y);
 if (d1 >= 0) and (d2 >= 0) and (d3 >= 0) and (d4 >= 0) then
 begin
  Result := True;
  Exit;
 end;

 Result := False;
end;




Procedure get_ClippingBoundInfos(Const Clipping: Boolean; Const ClipRect: Types.TRect;
  Const aMaxWidth, aMaxHeight: Integer; Var x1, y1, x2, y2, ClipWidth, ClipHeight, ClipRectSize, NextLineInc: Integer); Inline;
Begin
  If Clipping Then
  Begin
    With ClipRect Do
    Begin
      x1 := Left;
      y1 := Top;
      x2 := Right;
      y2 := Bottom;
    End;
  End
  Else
  Begin
    x1 := 0;
    y1 := 0;
    x2 := aMaxWidth;
    y2 := aMaxHeight;
  End;
  ClipHeight := (y2 + 1) - y1;
  ClipWidth := (x2 + 1) - x1;
  ClipRectSize := (ClipWidth * ClipHeight) - 1;
  nextLineinc := aMaxWidth - ClipWidth;
End;


{$IFDEF CPU32}
  {$i bzrect_native_imp.inc}
{$ELSE}
  {$IFDEF USE_ASM}
    {$IFDEF UNIX}
      {$i bzrect_native_imp.inc}
    {$ENDIF}
    {$IFDEF WINDOWS}
      {$i bzrect_native_imp.inc}
    {$ENDIF}
  {$ELSE}
    {$i bzrect_native_imp.inc}
  {$ENDIF}
{$ENDIF}

function BZRect(aLeft, aTop, aRight, aBottom : Longint) : TBZRect;
begin
  Result.Create(aLeft, aTop, aRight, aBottom, False);
end;

Procedure ComputePreMulAlphaLUT;
Var
  i, j: Integer;
Begin
  For i := 0 To 255 Do
    For j := 0 To 255 Do
      PreMultipliedAlphaLUT[I][J] := Byte(Round(i * j / 255));
End;

procedure RotateArrayOfPoints(var Points : array of TBZPoint; xOrg, yOrg: Integer; Angle : Single);
Var
  aSin, aCos: Double;
  xPrime, yPrime: Integer;
  I: Integer;
begin
  aSin := System.Sin(Angle);
  aCos := System.Cos(Angle);
  for I := Low(Points) to High(Points) do
  begin
    xPrime := Points[I].X - xOrg;
    yPrime := Points[I].Y - yOrg;
    Points[I].X := Round(xPrime * aCos - yPrime * aSin) + xOrg;
    Points[I].Y := Round(xPrime * aSin + yPrime * aCos) + yOrg;
  end;
end;

{%endregion%}



{%region%====[ TBZRectItemProperty ]================================================}

procedure TBZRectItemProperty.SetBottom(const AValue : Integer);
begin
  if FRect.Bottom = AValue then Exit;
  FRect.Bottom := AValue;
end;

procedure TBZRectItemProperty.SetHeight(const AValue : Integer);
begin
  if FRect.Height = AValue then Exit;
  FRect.Bottom := FRect.Top + AValue;
end;

function TBZRectItemProperty.GetBottom : Integer;
begin
  Result := FRect.Bottom;
end;

function TBZRectItemProperty.GetHeight : Integer;
begin
  Result := FRect.Height;
end;

function TBZRectItemProperty.CenterX : Integer;
begin
  Result := FRect.CenterPoint.X;
end;

function TBZRectItemProperty.CenterY : Integer;
begin
  Result := FRect.CenterPoint.Y;
end;

function TBZRectItemProperty.GetLeft : Integer;
begin
  Result := FRect.Left;
end;

function TBZRectItemProperty.GetRight : Integer;
begin
  Result := FRect.Right;
end;

function TBZRectItemProperty.GetTop : Integer;
begin
  Result := FRect.Top;
end;

function TBZRectItemProperty.GetWidth : Integer;
begin
  Result := FRect.Width;
end;

procedure TBZRectItemProperty.SetLeft(const AValue : Integer);
begin
  if FRect.Left = AValue then Exit;
  FRect.Left := AValue;
end;

procedure TBZRectItemProperty.SetRight(const AValue : Integer);
begin
  if FRect.Right = AValue then Exit;
  FRect.Right := AValue;
end;

procedure TBZRectItemProperty.SetTop(const AValue : Integer);
begin
  if FRect.Top = AValue then Exit;
  FRect.Top := AValue;
end;

procedure TBZRectItemProperty.SetWidth(const AValue : Integer);
begin
  if FRect.Width = AValue then Exit;
  FRect.Right := FRect.Left + AValue;
end;

{%endregion%}

{%region%====[ TBZRawImageDescription ]=========================================}

Constructor TBZRawImageDescription.Create;
begin
  Inherited Create; //(nil);

  FWidth:=1;
  FHeight:=1;
  FSize:=4;
  FLineOrder:=bloTopToBottom;
  FPixelFormat:=pf32bits;
  FBitsPerPixel:=32;//GetBitsPerPixel(aPixelFormat);
  FBitCount := 32;
  FPixelSize:=4; //GetPixelSize(aPixelFormat);
  FRowStrideType := bleDWordBoundary;
  FRowStride:=0;
  FHasAlpha:=False;
  FBitOrder := bboBitsInOrder;
  With FBitFields do // Format par defaut RGBA
  begin
    Size := 16;
    RedSize := 8;
    RedShift := 0;
    RedMask := CreateBitFieldMask(RedShift,RedSize);
    GreenSize := 8;
    GreenShift := 8;
    GreenMask := CreateBitFieldMask(GreenShift,GreenSize);
    BlueSize := 8;
    BlueShift := 16;
    BlueMask := CreateBitFieldMask(BlueShift,BlueSize);
    AlphaSize := 8;
    AlphaShift:=24;
    AlphaMask:=CreateBitFieldMask(AlphaShift,AlphaSize);
  end;
  FFrameCount := 0;
  FInterlaced := False;
  // Taille minimum 1x1
end;

Destructor TBZRawImageDescription.Destroy;
begin
  If Assigned(FPaletteEntries) then
   begin
     FreeMem(FPaletteEntries);
     FPaletteEntries:=Nil;
   end;
  inherited Destroy;
end;

procedure TBZRawImageDescription.setWidth(Const Value : Integer);
begin
 if Value = FWidth then exit;
 FWidth:= Value;
 FSize:=int64(FHeight)*int64(GetBytesPerLine);
end;

procedure TBZRawImageDescription.setHeight(Const Value : Integer);
begin
 if Value = FHeight then exit;
 FHeight:= Value;
 FSize:=int64(FHeight)*int64(GetBytesPerLine);
end;

procedure TBZRawImageDescription.setPixelFormat(Const Value : TBZPixelFormat);
begin
 if Value = FPixelFormat then exit;
 FPixelFormat:= Value;
 FBitsPerPixel:= GetBitsPerPixel(Value);
 FPixelSize:= GetPixelSize(Value);
 FSize:=int64(FHeight)*int64(GetBytesPerLine);
end;

procedure TBZRawImageDescription.setRowStrideType(Const Value : TBZBitmapLineEnd);
begin
 if Value = FRowStrideType then exit;
 FRowStrideType :=Value;
end;

procedure TBZRawImageDescription.setColorFormat(Const Value : TBZColorFormat);
begin

end;

procedure TBZRawImageDescription.setPaletteCount(Const NewSize:Integer);
Var i:Integer;
begin
 if FPaletteCount = NewSize then exit;
 FPaletteCount:=NewSize;
 memReAlloc(FPaletteEntries,FPaletteCount*4);
 For i:= 0 to FPaletteCount-1 do
 begin
     FPaletteEntries^[I].Red:= 0;
     FPaletteEntries^[I].Green:= 0;
     FPaletteEntries^[I].Blue:= 0;
     FPaletteEntries^[I].Alpha:= 255;
 end;

end;

function TBZRawImageDescription.getRowStrideSize:Integer;
begin
   result := Compute_BytesPerLine(FWidth, FBitsPerPixel,FRowStrideType);
end;

function TBZRawImageDescription.GetBytesPerLine: PtrUInt;  inline;
Var
  B:PtrUInt;
begin
  B:=GetBitsPerLine;
  Result := (B + 7) shr 3;
end;

function TBZRawImageDescription.GetBitsPerLine: PtrUInt; inline;
begin
(* Case FBitsPerPixel of
  1 : Result := FWidth div 8;
  2 : Result := FWidth div 4;
  4 : Result := FWidth div 2;
 else *)
   Result := (FWidth * FBitsPerPixel);
   Result := (Result +  7) and not PtrUInt(7);
end;

function TBZRawImageDescription.GetBitCount:Byte;
begin
  result:=0;
  Case FPixelFormat of
    pf16bits :
    begin
      With FBitFields do
      begin
        Result := RedSize+GreenSize+BlueSize+AlphaSize;
      end;
    end;
    pf32bits :
    begin
      With FBitFields do
      begin
          if (AlphaSize=0) and (Size < 16) then Result := 24
          else if (AlphaSize=0) then Result := 24 else Result := 32;
        end;
    end;
    else
    begin
      result := GetBitsPerPixel(FPixelFormat);
    end;
  End;
end;

function TBZRawImageDescription.getRowStrideLength:Byte;
begin
  result:=0;
  Case FBitsPerPixel of
    4: result := getRowStrideSize - (FWidth shr 1);
    8: result := getRowStrideSize - FWidth;
   16: result := getRowStrideSize - (FWidth*2);
   24: result := getRowStrideSize - (FWidth*3);
   32: result := getRowStrideSize - (FWidth*4);
  end;
end;

procedure TBZRawImageDescription.Assign(Source : TPersistent);
begin
  If (Source Is TBZRawImageDescription) then
  begin
    FWidth        := TBZRawImageDescription(Source).Width;
    FHeight       := TBZRawImageDescription(Source).Height;
    FSize         := TBZRawImageDescription(Source).Size;
    FColorFormat  := TBZRawImageDescription(Source).ColorFormat;
    FPixelFormat  := TBZRawImageDescription(Source).PixelFormat;
    FPixelSize    := TBZRawImageDescription(Source).PixelSize;
    FBitCount     := TBZRawImageDescription(Source).BitCount;
    FBitsPerPixel := TBZRawImageDescription(Source).BitsPerPixel;
    FHasAlpha     := TBZRawImageDescription(Source).HasAlpha;
    FTransparentColor := TBZRawImageDescription(Source).TransparentColor;
    FBitFields    := TBZRawImageDescription(Source).BitFields;
    FLineOrder    := TBZRawImageDescription(Source).LineOrder;
    //FBitOrder:= TBZRawImageDescription(Source).BitOrder;
    FRowStride    := TBZRawImageDescription(Source).RowStrideSize;
    FRowStrideType:= TBZRawImageDescription(Source).RowStrideType;
    FUsePalette   := TBZRawImageDescription(Source).UsePalette;
    FInterlaced   := TBZRawImageDescription(Source).Interlaced;
    FPaletteCount := TBZRawImageDescription(Source).PaletteCount;
    if FPaletteCount>0 then
    begin
      memReAlloc(FPaletteEntries,FPaletteCount*4);
      Move(TBZRawImageDescription(Source).PaletteEntries^,FPaletteEntries^,FPaletteCount*4);
    End;
    FFrameCount := TBZRawImageDescription(Source).FrameCount;
  End;
End;

procedure TBZRawImageDescription.UpdateSize(Const NewWidth, NewHeight : Integer);
begin
 FWidth:=NewWidth;
 FHeight:=NewHeight;

 FSize:=NewHeight*GetBytesPerLine; //(NewWidth*NewHeight)*FPixelSize; //(FWidth*GetBytesPerLine);
end;

procedure TBZRawImageDescription.Init(aColorFormat:TBZColorFormat);
begin
  if aColorFormat = cfCustom then exit;
  FColorFormat:=aColorFormat;
  With BZColorFormatDesc[FColorFormat] do
  begin
    FLineOrder:=LineOrder;

    FPixelFormat:= DefaultPixelFormat;
    FPixelSize:=PixelSize;
    FBitsPerPixel:= BitsPerPixel;
  end;
end;

procedure TBZRawImageDescription.Init(aColorFormat:TBZColorFormat;aPixelFormat : TBZPixelFormat);
begin
  if aColorFormat = cfCustom then exit;
  FColorFormat:=aColorFormat;
  With BZColorFormatDesc[FColorFormat] do
  begin
    FLineOrder:=LineOrder;
    FPixelFormat:= aPixelFormat;
    FBitsPerPixel:= GetBitsPerPixel(aPixelFormat);
    FPixelSize:= GetPixelSize(aPixelFormat);
  end;
end;

procedure TBZRawImageDescription.Init(aColorFormat:TBZColorFormat;aPixelFormat : TBZPixelFormat; const aLineOrder : TBZBitmapLineOrder);
begin
 Init(aColorFormat, aPixelFormat);
 FLineOrder:=aLineOrder;
end;

procedure TBZRawImageDescription.Init(const aLineOrder : TBZBitmapLineOrder;Const aPixelFormat:TBZPixelFormat;aPixelSize:Byte;Const aSwapRB, aSwapAlpha : Boolean);
begin
 FColorFormat:=cfCustom;
 FLineOrder:=aLineOrder;
 FPixelFormat:=aPixelFormat;
 FBitsPerPixel:=GetBitsPerPixel(aPixelFormat);
 FPixelSize:=aPixelSize; //GetPixelSize(aPixelFormat);
end;

procedure TBZRawImageDescription.CheckAlphaDescription;
  Function AddAlphaToDescription: Boolean;
  var
    Mask: Cardinal;
  begin
    Result:=False;
    With BitFields do
    begin
      if AlphaSize >= 8 then Exit;
      //if Self.BitsPerPixel <> 32 then Exit;
      if Self.BitCount <> 24 then Exit;

      Mask := RedMask or GreenMask or BlueMask;
      if (Mask and $FF = 0) then
      begin
        AlphaShift := 0;
        AlphaSize  := 8;
        Result:=True;
      end
      else if (Mask and $FF000000 = 0) then
      begin
        AlphaShift := 24;
        AlphaSize  := 8;
        Result:=True;
      end;
    end;
  end;
Begin
  if not(AddAlphaToDescription) then
  begin
    With BitFields do
    begin
      AlphaSize := 8;
      AlphaShift := 24;
      AlphaMask  := CreateBitFieldMask(AlphaShift,AlphaSize);
    end;
  end;
end;

function TBZRawImageDescription.getDescriptionAsString:String;
Var
  Info : TBZColorFormatInfosRec;
  S,tmp :String;
begin

    Info:=BZColorFormatDesc[ColorFormat];
    S:=Info.name;

    Tmp:=S+#13+#10;
   // Tmp:=Tmp+'________________________________________________________________'+#13+#10+#13+#10;
    Tmp:=Tmp+'___[ CORE ]_____________________________'+#13+#10+#13+#10;
    Tmp:=Tmp+' · Nombre de bits par pixel    : '+inttostr(FBitsPerPixel)+#13+#10;
    Tmp:=Tmp+' · Taille d''un pixel en octet : '+inttostr(FPixelSize)+#13+#10;
    Tmp:=Tmp+' · Largeur                     : '+inttostr(FWidth)+#13+#10;
    Tmp:=Tmp+' · Hauteur                     : '+inttostr(FHeight)+#13+#10;
    Tmp:=Tmp+' · Taille d''une ligne          : '+inttostr(BytesPerLine)+#13+#10;
//    Tmp:=Tmp+' · Type d''alignement des données sur  : '+cLineEndStr[RowStrideType]+#13+#10;
    Tmp:=Tmp+' · Taille d''une ligne avec alignement : '+inttostr(RowStrideSize)+#13+#10;
    Tmp:=Tmp+' · Taille de l''alignement en octet    : '+inttostr(RowStrideLength)+#13+#10;
    Tmp:=Tmp+' · Taille Totale en octet      : '+inttostr(Size)+#13+#10;
    Tmp:=Tmp+'___[ EXTRA ]____________________________'+#13+#10+#13+#10;
    Tmp:=Tmp+' · Nombre de bit utilisés      : '+inttostr(FBitCount)+#13+#10;
 //   Tmp:=Tmp+' · Ordre des lignes            : '+ cLineOrderStr[LineOrder]+#13+#10;
    Tmp:=Tmp+' · Palette                     : '+'Non'+#13+#10;
    Tmp:=Tmp+' · Nombre de couleur           : '+IntToStr(FPaletteCount)+#13+#10;
 // Tmp:=Tmp+'Format couleur             : '+s+#13+#10;
 (*   Tmp:=Tmp+' · Prise en charge de la Transparence : '+Bool2StrFR[HasAlpha]+#13+#10;
    Tmp:=Tmp+'___[ BITFIELDS ]________________________'+#13+#10+#13+#10;
    With BitFields do
    begin
      Tmp:=Tmp+'                  |------------|------------|------------|------------|'+#13+#10;
      Tmp:=Tmp+'                  |     Red    |   Green    |    Blue    |    Alpha   |'+#13+#10;
      Tmp:=Tmp+'      ------------|------------|------------|------------|------------|'+#13+#10;
      Tmp:=Tmp+'           Masque | '+StrOps.PadCenter(IntToHex(RedMask,8),10)+' | '
                                     +StrOps.PadCenter(IntToHex(GreenMask,8),10)+' | '
                                     +StrOps.PadCenter(IntToHex(BlueMask,8),10)+' | '
                                     +StrOps.PadCenter(IntToHex(AlphaMask,8),10)+' |'+#13+#10;
      Tmp:=Tmp+'      ------------|------------|------------|------------|------------|'+#13+#10;
      Tmp:=Tmp+' Décalage en bits | '+StrOps.PadCenter(IntToStr(RedShift),10)+' | '
                                     +StrOps.PadCenter(IntToStr(GreenShift),10)+' | '
                                     +StrOps.PadCenter(IntToStr(BlueShift),10)+' | '
                                     +StrOps.PadCenter(IntToStr(AlphaShift),10)+' | '+#13+#10;
      Tmp:=Tmp+'      ------------|------------|------------|------------|------------|'+#13+#10;
      Tmp:=Tmp+' Taille en bits   | '+StrOps.PadCenter(IntToStr(RedSize),10)+' | '
                                     +StrOps.PadCenter(IntToStr(GreenSize),10)+' | '
                                     +StrOps.PadCenter(IntToStr(BlueSize),10)+' | '
                                     +StrOps.PadCenter(IntToStr(AlphaSize),10)+' | '+#13+#10;
      Tmp:=Tmp+'      ------------|------------|------------|------------|------------|'+#13+#10;
    end;

      Tmp:=Tmp+'______________________________________________________________________'+#13+#10;
  *)
 result:=tmp;
end;

procedure TBZRawImageDescription.InitDefault(Const AWidth, AHeight : Integer; Const ADepth: Byte);
begin
   FColorFormat:=cfRGBA;

    // Taille minimum 1x1
    SetWidth(AWidth);
    SetHeight(AHeight);
    FLineOrder:=bloTopToBottom;

    HasAlpha:=False;
    case ADepth of
      1:
      begin
        //ColorFormat:=
        FPixelFormat := pf1bit;
        FBitCount := 1;
        FBitsPerPixel := 1;

        FRowStrideType := bleWordBoundary;
        FRowStride := 0;

        With FBitFields do
        begin
          Size := 12;
          RedSize := 1;
          RedShift := 0;
          RedMask := CreateBitFieldMask(RedShift,RedSize);
          GreenSize := 1;
          GreenShift := 0;
          GreenMask := CreateBitFieldMask(GreenShift,GreenSize);
          BlueSize := 1;
          BlueShift := 0;
          BlueMask := CreateBitFieldMask(BlueShift,BlueSize);
          AlphaSize:=0;
          AlphaShift:=0;
          AlphaMask := 0;
        end;
        FPixelSize := 1 div 8;
        FSize:=(FWidth*FHeight) div 8;
      end;
      2:
      begin
        FPixelFormat := pf2bits;
        FBitCount := 2;
        FBitsPerPixel := 2;

        FPixelSize := 1 div 4;
        FSize:=(FWidth*FHeight) div 4;
      end;
      4:
      begin
        FPixelFormat := pf4bits;
        FBitCount := 4;
        FBitsPerPixel := 4;

        FPixelSize := 1 shr 1;
        FSize:=(FWidth*FHeight) div 2;
      end;
      5..8:
      begin
        FPixelFormat := pf8bits;
        FBitCount := 8;
        FBitsPerPixel := 8;
        FPixelSize := 1;
        FSize:=(FWidth*FHeight);
      end;
      9..15:
      begin
        //ColorFormat:=cfRGB_1555
        FPixelFormat := pf16bits;
        FBitCount := 15;
        FBitsPerPixel := 16;

        FRowStrideType := bleWordBoundary;
        FRowStride := 0;

        With FBitFields do
        begin
          Size := 16;
          RedSize := 5;
          RedShift := 11;
          RedMask := CreateBitFieldMask(RedShift,RedSize);
          GreenSize := 5;
          GreenShift := 6;
          GreenMask := CreateBitFieldMask(GreenShift,GreenSize);
          BlueSize := 5;
          BlueShift := 1;
          BlueMask := CreateBitFieldMask(BlueShift,BlueSize);
          AlphaSize:=1;
          AlphaShift:=0;
          AlphaMask := 0;
        end;

      end;
      16:     // RGB 565
      begin
        //ColorFormat:=cfRGB_565
        FPixelFormat := pf16bits;
        FBitCount := 16;
        FBitsPerPixel := 16;

        FRowStrideType := bleWordBoundary;
        FRowStride := 0;

        With FBitFields do
        begin
          Size := 12;
          RedSize := 5;
          RedShift := 10;
          RedMask := CreateBitFieldMask(RedShift,RedSize);
          GreenSize := 6;
          GreenShift := 5;
          GreenMask := CreateBitFieldMask(GreenShift,GreenSize);
          BlueSize := 5;
          BlueShift := 0;
          BlueMask := CreateBitFieldMask(BlueShift,BlueSize);
          AlphaSize:=0;
          AlphaShift:=0;
          AlphaMask := 0;
        end;
        FPixelSize := 2;
        FSize:=(FWidth*FHeight)*FPixelSize;
      end;
      17..24:
      begin
        //ColorFormat:=cfRGB_1555
        FPixelFormat := pf24bits;
        FBitCount := 24;
        FBitsPerPixel := 24;

        FRowStrideType := bleDWordBoundary;
        FRowStride := 0;

        With FBitFields do
        begin
          Size := 12;
          RedSize := 8;
          RedShift := 16;
          RedMask := CreateBitFieldMask(RedShift,RedSize);
          GreenSize := 8;
          GreenShift := 8;
          GreenMask := CreateBitFieldMask(GreenShift,GreenSize);
          BlueSize := 8;
          BlueShift := 0;
          BlueMask := CreateBitFieldMask(BlueShift,BlueSize);
          AlphaSize:=0;
          AlphaShift:=0;
          AlphaMask := 0;
        end;

        FPixelSize := 3;
        FSize:=(FWidth*FHeight)*FPixelSize;
      end;
      48:
      begin
        FColorFormat:=cfRGBA;
        FPixelFormat := pf48bits;
        FBitCount := 48;
        FBitsPerPixel := 48;
        FHasAlpha:=true;
        FRowStrideType := bleDWordBoundary;
        FRowStride := 0;
        FPixelSize := 6;
        FSize:=(FWidth*FHeight)*FPixelSize;
      end;
      96:
      begin
        FColorFormat:=cfRGBA;
        FPixelFormat := pf96bits;
        FBitCount := 96;
        FBitsPerPixel := 96;
        FHasAlpha:=true;
        FRowStrideType := bleDWordBoundary;
        FRowStride := 0;
        FPixelSize := 12;
        FSize:=(FWidth*FHeight)*FPixelSize;
      end;
      128:
      begin
        FColorFormat:=cfRGBA;
        FPixelFormat := pf32bits;
        FBitCount := 96;
        FBitsPerPixel := 96;
        FHasAlpha:=true;
        FRowStrideType := bleDWordBoundary;
        FRowStride := 0;
        FPixelSize := 16;
        FSize:=(FWidth*FHeight)*FPixelSize;
      end;
    else
    begin
      FColorFormat:=cfRGBA;
      FPixelFormat := pf32bits;
      FBitCount := 32;
      FBitsPerPixel := 32;
      FHasAlpha:=true;
      FRowStrideType := bleDWordBoundary;
      FRowStride := 0;
      // On initialise notre encodage bitfields
      With FBitFields do
      begin
        Size := 16;
        RedSize := 8;
        RedShift := 16;
        RedMask := CreateBitFieldMask(RedShift,RedSize);
        GreenSize := 8;
        GreenShift := 8;
        GreenMask := CreateBitFieldMask(GreenShift,GreenSize);
        BlueSize := 8;
        BlueShift := 0;
        BlueMask := CreateBitFieldMask(BlueShift,BlueSize);
        AlphaSize := 8;
        AlphaShift:=24;
        AlphaMask:=CreateBitFieldMask(AlphaShift,AlphaSize);
      end;
      FPixelSize := 4;
      FSize:=int64((FWidth*FHeight)*FPixelSize);
    End;
  end;
end;

{%endregion%}

{%region%====[ TBZRawImageData ]================================================}

Constructor TBZRawImageData.Create;
begin
  Inherited Create(nil);

 // FRawData := nil;
  FPaletteEntries:= nil;
  FPaletteCount:=0;

  FDescription := nil;
  FDescription := TBZRawImageDescription.Create;
end;

Destructor TBZRawImageData.Destroy;
begin
 If Assigned(FPaletteEntries) then
 begin
   FreeMem(FPaletteEntries);
   FPaletteEntries:=Nil;
 end;
 FreeAndNil(FDescription);
 Inherited Destroy;
end;

procedure TBZRawImageData.Assign(Source:TPersistent);
begin
  if (Source Is TBZRawImageData) then
  begin
    FDescription.Assign(TBZRawImageData(Source).Description);
    FUsePalette :=  TBZRawImageData(Source).UsePalette;
    FPaletteCount := TBZRawImageData(Source).PaletteCount;
    if(TBZRawImageData(Source).PaletteEntries<>nil) then
    begin
      memReAlloc(FPaletteEntries,int64(FPaletteCount*4));
      Move(TBZRawImageData(Source).PaletteEntries^,FPaletteEntries^,FPaletteCount*4);
    End;
  End;
End;

procedure TBZRawImageData.UpdateSize(const aWidth,aHeight:Integer);
begin
  (* TODO faire une copie pour conserver les données *)
 // assert(aWidth<=0,'Erreur la Largeur du bitmap définit par TBZBitmapRawDataRec doit être superieur à 0');
// assert(aHeight<=0,'Erreur la Hauteur du bitmap définit par TBZBitmapRawDataRec doit être superieur à 0');

   FDescription.UpdateSize(aWidth, aHeight);
 //  If FDescription.Size>0 then memReAlloc(FData,FDescription.Size);
   // assert(Size<=0,'Erreur  la Taille en octet du bitmap définit par TBZBitmapRawDataRec doit être superieur à 0');
  // ShowMessage('TBZImageRawData.UpdateDataSize ' +Inttostr(FDescription.Width)+'x'+inttostr(FDescription.Height)+' - '+inttostr(FDescription.Size));
  //end;
end;

procedure TBZRawImageData.FreeData;
begin
  //FreeAndNil(FRawData);
end;

function TBZRawImageData.GenericGetPixel(P: PByte; BPP: Byte): Cardinal;
// Retrieves a pixel value from a Buffer. The actual size and order of the bytes is not important
// since we are only using the value for comparisons with other pixels.
begin
  Result := P^;
  Inc(P);
  Dec(BPP);
  while BPP > 0 do
  begin
    Result := Result shl 8;
    Result := Result or P^;
    Inc(P);
    Dec(BPP);
  end;
end;

function TBZRawImageData.GenericCountDiffPixels(P: PByte; BPP: Byte; Count: Integer): Integer;

// counts pixels in Buffer until two identical adjacent ones found

var
  N: Integer;
  Pixel,
  NextPixel: Cardinal;

begin
  N := 0;
  NextPixel := 0; // shut up compiler
  if Count = 1 then Result := Count
               else
  begin
    Pixel := GenericGetPixel(P, BPP);
    while Count > 1 do
    begin
      Inc(P, BPP);
      NextPixel := GenericGetPixel(P, BPP);
      if NextPixel = Pixel then Break;
      Pixel := NextPixel;
      Inc(N);
      Dec(Count);
    end;
    if NextPixel = Pixel then
      Result := N
    else
      Result := N + 1;
  end;
end;

function TBZRawImageData.GenericCountSamePixels(P: PByte; BPP: Byte; Count: Integer): Integer;

var
  Pixel,
  NextPixel: Cardinal;

begin
  Result := 1;
  Pixel := GenericGetPixel(P, BPP);
  Dec(Count);
  while Count > 0 do
  begin
    Inc(P, BPP);
    NextPixel := GenericGetPixel(P, BPP);
    if NextPixel <> Pixel then
      Break;
    Inc(Result);
    Dec(Count);
  end;
end;


function TBZRawImageData.getDescription: TBZRawImageDescription;
begin
  result:=FDescription;
end;

procedure TBZRawImageData.setPaletteCount(Const NewSize:Integer);
Var i:Integer;
begin
 if FPaletteCount = NewSize then exit;
 FPaletteCount:=NewSize;
 memReAlloc(FPaletteEntries,int64(FPaletteCount*4));
 For i:= 0 to FPaletteCount-1 do
 begin
     FPaletteEntries^[I].Red:= 0;
     FPaletteEntries^[I].Green:= 0;
     FPaletteEntries^[I].Blue:= 0;
     FPaletteEntries^[I].Alpha:= 255;
 end;

end;

procedure TBZRawImageData.Init(Const aWidth, aHeight:Integer;aColorFormat: TBZColorFormat); //+ aPixelFormat:TBZPixelFormat);
//Var
//  DataSize:Int64;
begin
  FreeData;
 // FDescription.Init(aColorFormat);
  FDescription.InitDefault(aWidth, aHeight,32);
  UpdateSize(aWidth, aHeight);
  FUsePalette:=False;
  FPaletteCount:=0;
//  memReAlloc(FPaletteEntries, Sizeof(TBZPaletteEntries));
 // SetLength(FPaletteEntries,0);
 // FDescription.
 (* Case aColorFormat of
    cfMono :
    begin
      Case aPixelFormat of
        pf1Bit :
        begin

        end;
        pf8Bits :
        begin

        end
        else
        begin
          Raise Exception.Create('Nombre de bits invalide pour le format de couleur : Mono');
        end;
      end;
    end;
    cfIndexed :
    begin
      Case aPixelFormat of
        pf8Bit :
        begin

        end;
        pf16Bits :
        begin

        end;
        pf32Bits :
        begin

        end;
        pf64Bits :
        begin

        end
        else
        begin
          Raise Exception.Create('Nombre de bits invalide pour le format de couleur : Mono');
        end;
      end;
    end;
    cfRGB565,cfBGR565 :
    begin
      // if aPixelFormat<>pf16bits then Raise Exception.Create('Nombre de bits invalide pour le format de couleur : RGB565 / BGR565');
      aPixelFormat:=pf16Bits;

    end;
    cfRGB,cfBGR :
    begin
      Case aPixelFormat of
        pf24Bit :
        begin

        end;
        pf48Bits :
        begin

        end
        else
        begin
          Raise Exception.Create('Nombre de bits invalide pour le format de couleur : Mono');
        end;
      end;
    end;
    cfRGBA, cfBGRA, cfABGR, cfARGB :
    begin
      Case aPixelFormat of
        pf32Bit :
        begin
          DataSize
          Fimage.NewData(aWidth,aHeight,DataSize);
        end;
        pf64Bits :
        begin

        end
        else
        begin
          Raise Exception.Create('Nombre de bits invalide pour le format de couleur : Mono');
        end;
      end;
    end;
  end; *)
end;

function TBZRawImageData.getDescriptionAsString:String;
Var
  Info : TBZColorFormatInfosRec;
  S,tmp :String;
begin

    Info:=BZColorFormatDesc[FDescription.ColorFormat];
    S:=Info.name;

    Tmp:=S+#13+#10;
   // Tmp:=Tmp+'________________________________________________________________'+#13+#10+#13+#10;
    Tmp:=Tmp+'___[ CORE ]_____________________________'+#13+#10+#13+#10;
    Tmp:=Tmp+' · Nombre de bits par pixel    : '+inttostr(FDescription.BitsPerPixel)+#13+#10;
    Tmp:=Tmp+' · Taille d''un pixel en octet : '+inttostr(FDescription.PixelSize)+#13+#10;
    Tmp:=Tmp+' · Largeur                     : '+inttostr(FDescription.Width)+#13+#10;
    Tmp:=Tmp+' · Hauteur                     : '+inttostr(FDescription.Height)+#13+#10;
    Tmp:=Tmp+' · Taille d''une ligne          : '+inttostr(FDescription.BytesPerLine)+#13+#10;
    Tmp:=Tmp+' · Type d''alignement des données sur  : '+cLineEndStr[FDescription.RowStrideType]+#13+#10;
    Tmp:=Tmp+' · Taille d''une ligne avec alignement : '+inttostr(FDescription.RowStrideSize)+#13+#10;
    Tmp:=Tmp+' · Taille de l''alignement en octet    : '+inttostr(FDescription.RowStrideLength)+#13+#10;
    Tmp:=Tmp+' · Taille Totale en octet      : '+inttostr(FDescription.Size)+#13+#10;
    Tmp:=Tmp+'___[ EXTRA ]____________________________'+#13+#10+#13+#10;
    Tmp:=Tmp+' · Nombre de bit utilisés      : '+inttostr(FDescription.BitCount)+#13+#10;
    Tmp:=Tmp+' · Ordre des lignes            : '+ cLineOrderStr[FDescription.LineOrder]+#13+#10;
    Tmp:=Tmp+' · Palette                     : '+'Non'+#13+#10;
    Tmp:=Tmp+' · Nombre de couleur           : '+IntToStr(FPaletteCount)+#13+#10;
 // Tmp:=Tmp+'Format couleur             : '+s+#13+#10;
 (*   Tmp:=Tmp+' · Prise en charge de la Transparence : '+Bool2StrFR[FDescription.HasAlpha]+#13+#10;
    Tmp:=Tmp+'___[ BITFIELDS ]________________________'+#13+#10+#13+#10;
    With FDescription.BitFields do
    begin
      Tmp:=Tmp+'                  |------------|------------|------------|------------|'+#13+#10;
      Tmp:=Tmp+'                  |     Red    |   Green    |    Blue    |    Alpha   |'+#13+#10;
      Tmp:=Tmp+'      ------------|------------|------------|------------|------------|'+#13+#10;
      Tmp:=Tmp+'           Masque | '+StrOps.PadCenter(IntToHex(RedMask,8),10)+' | '
                                     +StrOps.PadCenter(IntToHex(GreenMask,8),10)+' | '
                                     +StrOps.PadCenter(IntToHex(BlueMask,8),10)+' | '
                                     +StrOps.PadCenter(IntToHex(AlphaMask,8),10)+' |'+#13+#10;
      Tmp:=Tmp+'      ------------|------------|------------|------------|------------|'+#13+#10;
      Tmp:=Tmp+' Décalage en bits | '+StrOps.PadCenter(IntToStr(RedShift),10)+' | '
                                     +StrOps.PadCenter(IntToStr(GreenShift),10)+' | '
                                     +StrOps.PadCenter(IntToStr(BlueShift),10)+' | '
                                     +StrOps.PadCenter(IntToStr(AlphaShift),10)+' | '+#13+#10;
      Tmp:=Tmp+'      ------------|------------|------------|------------|------------|'+#13+#10;
      Tmp:=Tmp+' Taille en bits   | '+StrOps.PadCenter(IntToStr(RedSize),10)+' | '
                                     +StrOps.PadCenter(IntToStr(GreenSize),10)+' | '
                                     +StrOps.PadCenter(IntToStr(BlueSize),10)+' | '
                                     +StrOps.PadCenter(IntToStr(AlphaSize),10)+' | '+#13+#10;
      Tmp:=Tmp+'      ------------|------------|------------|------------|------------|'+#13+#10;
    end;

      Tmp:=Tmp+'______________________________________________________________________'+#13+#10;
  *)
 result:=tmp;
end;

{%endregion%}

{%region%====[ TBZBitmapLayerItem ]=============================================}

Constructor TBZBitmapLayerItem.Create;
Begin
  Inherited Create(nil);
  FImage := nil;
  FPosition.Create(0,0);
  FDrawMode :=0;
  FDelayTime := 0;
  FComment := TStringList.Create;
  FImage := TBZCustomBitmap.Create;
  FIsTransparent := False;
End;

Destructor TBZBitmapLayerItem.Destroy;
Begin
  FreeAndNil(FImage);
  FreeAndNil(FComment);
  Inherited Destroy;
End;

Procedure TBZBitmapLayerItem.Assign(source: TPersistent);
Begin
  if (Source Is TBZBitmapLayerItem) then
  begin
    Bitmap.Assign(TBZBitmapLayerItem(Source).Bitmap);
    Left := TBZBitmapLayerItem(Source).Left;
    Top := TBZBitmapLayerItem(Source).Top;
    Comment.Assign(TBZBitmapLayerItem(Source).Comment);
    DelayTime := TBZBitmapLayerItem(Source).DelayTime;
    DrawMode := TBZBitmapLayerItem(Source).DrawMode;
  End
  else
    Inherited Assign(source);
End;

Function TBZBitmapLayerItem.GetImage: TBZCustomBitmap;
Begin
  Result := FImage;
End;

Function TBZBitmapLayerItem.GetLeftPosition: Integer;
Begin
  Result := FPosition.X;
end;

Function TBZBitmapLayerItem.GetTopPosition: Integer;
Begin
  Result := FPosition.Y;
end;

Procedure TBZBitmapLayerItem.SetImage(AValue: TBZCustomBitmap);
Begin
  if FImage = nil then
  begin
    FImage := TBZCustomBitmap.Create;
  End;
  FImage.Assign(AValue);
End;

Function TBZBitmapLayerItem.GetWidth: Integer;
Begin
  Result := FImage.Width;
End;

Function TBZBitmapLayerItem.GetHeight: Integer;
Begin
  Result := FImage.Height;
End;

Procedure TBZBitmapLayerItem.SetLeftPosition(AValue: Integer);
Begin
   FPosition.X := AValue;
end;

Procedure TBZBitmapLayerItem.SetTopPosition(AValue: Integer);
Begin
  FPosition.Y := AValue;
end;

Procedure TBZBitmapLayerItem.WriteToFiler(writer: TVirtualWriter);
Begin
  with writer do
  begin
    WriteInteger(0); // Archive Version 0
   //// FImage.WriteToFiler;
   // FPosition : TBZPoint;
   // WriteInteger(Integer(FDrawMode));;
   // WriteInteger(FDelayTime);
   // //FComment : TStringList;
  end;
End;

Procedure TBZBitmapLayerItem.ReadFromFiler(reader: TVirtualReader);
Begin

End;

{%endregion%}

{%region%====[ TBZBitmapLayerList ]=============================================}

Constructor TBZBitmapLayerList.Create;
Begin
  Inherited Create;
  FTransparent := False;
  FBackgroundColor := clrTransparent;
End;

Destructor TBZBitmapLayerList.Destroy;
Begin
  Clear;
  Inherited Destroy;
End;

Procedure TBZBitmapLayerList.Assign(source: TPersistent);
Var
  I : Integer;
  NewItem : TBZBitmapLayerItem;
Begin
  if (Source Is TBZBitmapLayerList) then
  begin
    If TBZBitmapLayerList(Source).Count > 0 then
    begin
      Clear;
      For I := 0 to TBZBitmapLayerList(Source).Count-1 do
      begin
        NewItem := TBZBitmapLayerItem.Create;
        NewItem.Assign(TBZBitmapLayerList(Source).Items[I]);
        AddImage(NewItem);
        //FreeAndNil(NewItem);
      End;
    end;
    FTransparent := TBZBitmapLayerList(Source).Transparent;
    FBackgroundColor := TBZBitmapLayerList(Source).BackgroundColor;
  End
  else
    Inherited Assign(source);
End;

Procedure TBZBitmapLayerList.WriteToFiler(writer: TVirtualWriter);
Begin
  Inherited WriteToFiler(writer);
  with writer do
  begin
    WriteInteger(0); // Archive Version 0

  end;
End;

Procedure TBZBitmapLayerList.ReadFromFiler(reader: TVirtualReader);
var
  archiveVersion: integer;
Begin
  Inherited ReadFromFiler(reader);
  archiveVersion := reader.ReadInteger;
  if archiveVersion = 0 then
  begin
    with reader do
    begin

    end;
  End
  else
    RaiseFilerException(archiveVersion);
  //for i := 0 to Count - 1 do Items[i].FOwner := Self;
End;

Function TBZBitmapLayerList.GetImageItem(index: Integer): TBZBitmapLayerItem;
Begin
  Result := TBZBitmapLayerItem(Get(Index));
End;

Procedure TBZBitmapLayerList.SetImageItem(index: Integer; val: TBZBitmapLayerItem);
Begin
  Put(Index, Val);
End;

//procedure WriteToFiler(writer: TVirtualWriter); //override;
//procedure ReadFromFiler(reader: TVirtualReader);// override;

Procedure TBZBitmapLayerList.Clear;
Var
  i:  Integer;
  pm: TBZBitmapLayerItem;
Begin
  if Count < 1 then exit;
  For i := Count - 1 downTo 0 Do
  Begin
    pm := GetImageItem(i);
    //If Assigned(pm) Then
    FreeAndNil(pm); //.Free;
  End;
  Inherited Clear;
End;

Function TBZBitmapLayerList.AddImage(anImage: TBZCustomBitmap; Const LeftPos: Integer = 0;Const TopPos : Integer = 0; Const DrawMode: Byte=0; Const DelayTime: Integer=100): Integer;
Var
  anItem: TBZBitmapLayerItem;
Begin
  anitem := TBZBitmapLayerItem.Create;
  anItem.Bitmap := anImage;
  anItem.Left := LeftPos;
  anItem.Top := TopPos;
  anItem.DrawMode := DrawMode;
  anItem.DelayTime := DelayTime;
  Result := Add(anItem);
End;

Function TBZBitmapLayerList.AddImage(Const aLayerItem: TBZBitmapLayerItem): Integer;
Begin
  Result := Add(aLayerItem);
End;

Function TBZBitmapLayerList.AddNewImage: TBZBitmapLayerItem;
Var
  anItem: TBZBitmapLayerItem;
Begin
  anitem := TBZBitmapLayerItem.Create;
  Add(anItem);
  Result:= Items[Self.Count-1];
End;

{%endregion%}

{%region%====[ TBZCustomBitmap ]================================================}

constructor TBZCustomBitmap.Create(AOwner : TPersistent; AWidth, AHeight : Integer);  //Const Indexed : Boolean=false
Begin
  if Assigned(AOwner) then Inherited Create(AOwner)
  else inherited Create;

  {$IFDEF DEBUG}
  GlobalLogger.LogNotice('TBZCustomBitmap.Create : ' + IntToStr(AWidth) + 'x' + IntToStr(AHeight));
  {$ENDIF}

  FImageDescription := TBZRawImageDescription.Create;
  FImageDescription.InitDefault(aWidth, aHeight, 32);

  SetSize(AWidth, AHeight);

  FClipRect.Create(0, 0, FMaxWidth, FMaxHeight);
  FClipping := False;
  FMultiImage := False;
  // Init palette de couleur
  FColorManager := TBZColorsManager.Create(nil);

  // Notre bitmap est une image avec des couleurs indexées dans une palette de couleur
  FUsePalette:=false; //Indexed;
  // Si on utilise la palette on charge les couleurs par defaut commune à tous les systemes
  // FUseDefaultPalette:=False;

  // On Utilise une palette avec un index codé sur 32bits -> Integer
  (* If FUsePalette then
     begin
       memReAlloc(FImageIndexBuffer,  FBufferSize);
     end
     else begin *)
  // Init liste sous-images
  FLayers := TBZBitmapLayerList.Create;
  FIsPremultiply := False;
  //IsMultiImage := False;
End;

constructor TBZCustomBitmap.Create(aWidth, aHeight : Integer);
Begin
  Self.Create(nil, aWidth, aHeight);
End;

constructor TBZCustomBitmap.Create;
begin
//  inherited Create;
  Self.Create(0,0);
end;


destructor TBZCustomBitmap.Destroy;
Begin

  If Assigned(FImageIndexBuffer) Then
  Begin
    memReAlloc(FImageIndexBuffer, 0);
    FreeMem(FImageIndexBuffer);
    FImageIndexBuffer := nil;
  End;
  setLength(FScanLineLUT, 0); // SetLenght ne libere pas le tableaux completement
  FScanLineLUT := nil; // Un simple nil en plus :)
  //setLength(FScanLineIdxLUT, 0);
  //FScanLineIdxLUT := nil;

  //memReAlloc(FSurfaceBuffer, 0);
  if FSurfaceBuffer<>nil then FreeMem(FSurfaceBuffer);
  FSurfaceBuffer := nil;
  FreeAndNil(FLayers);
  FreeAndNil(FColorManager);
  FreeAndNil(FImageDescription);
  //FImageDescription.Free;

  Inherited Destroy;
End;

procedure TBZCustomBitmap.Assign(Source : TPersistent);
Var
  SrcW, SrcH:     Integer;
  SrcPtr, DstPtr: PBZColor;
  SrcIdxPtr, DstIdxPtr: PDWord;
Begin
  If (Source Is TBZCustomBitmap) Then
  Begin
    SrcW := TBZCustomBitmap(Source).Width;
    SrcH := TBZCustomBitmap(Source).Height;
    FUsePalette := TBZCustomBitmap(Source).UsePalette;
    SetSize(SrcW, SrcH);
    SrcPtr := TBZCustomBitmap(Source).getSurfaceBuffer;
    DstPtr := getSurfaceBuffer;
    DstIdxPtr := PDWord(FImageIndexBuffer);
    SrcIdxPtr := TBZCustomBitmap(Source).getSurfaceIndexedBuffer;
    Move(SrcPtr^, DstPtr^, Size);
    if (DstIdxPtr<>nil) and (SrcIdxPtr<>nil) then Move(SrcIdxPtr^, DstIdxPtr^, Size);
    //DstIdxPtr := SrcIdxPtr;
    FClipping := TBZCustomBitmap(Source).Clipping;
    FClipRect := TBZCustomBitmap(Source).ClipRect;
    FImageDescription.Assign(TBZCustomBitmap(Source).ImageDescription);
    FLayers.Assign(TBZCustomBitmap(Source).Layers);
    FLayerIndex := TBZCustomBitmap(Source).LayerIndex;
    Inherited Assign(TBZCustomDataFile(TBZCustomBitmap(Source)));
  End;
End;

procedure TBZCustomBitmap.FastCopy(Source : TBZCustomBitmap);
Var
   SrcPtr, DstPtr: PBZColor;
begin
  SrcPtr := Source.getSurfaceBuffer;
  DstPtr := getSurfaceBuffer;
  if (Self.Width <> Source.Width) or (Self.Height<> Source.Height) then SetSize(Source.Width, Source.Height);
  Move(SrcPtr^, DstPtr^, Self.Size);
end;

procedure TBZCustomBitmap.setWidth(const AValue : Integer);
Begin
  If FWidth = AValue Then exit;
  SetSize(AValue, FHeight);
  FWidth := AValue;
  //  FImageDescription.Image.UpdateSize(AValue, FHeight,(FHeight*AValue)*FImageDescription.Description.PixelSize);
End;

function TBZCustomBitmap.getWidth : Integer;
Begin
  Result := FWidth;
End;

procedure TBZCustomBitmap.setHeight(const AValue : Integer);
Begin
  If FHeight = AValue Then exit;
  SetSize(FWidth, AValue);
  FHeight := AValue;
  // FImageDescription.UpdateSize(FWidth,AValue,(FWidth*AValue)*FImageDescription.Description.PixelSize);
End;

function TBZCustomBitmap.getHeight : Integer;
Begin
  Result := FHeight;
End;

function TBZCustomBitmap.getImageDescription : TBZRawImageDescription;
Begin
  Result := FImageDescription;
End;

function TBZCustomBitmap.GetIsMultiImage : Boolean;
Begin
  Result := (Layers.Count>0);
end;

procedure TBZCustomBitmap.setUsePalette(const aValue : Boolean);
Begin
  If aValue = FUsePalette Then exit;
  FUsePalette := aValue;
  If FUsePalette Then
  Begin
    memReAlloc(FImageIndexBuffer, 0);
    FreeMem(FImageIndexBuffer);
    FImageIndexBuffer := nil;
    memReAlloc(FImageIndexBuffer, int64((FWidth*FHeight)*Sizeof(DWord)));
  End;
  //else
  //begin
  //  memReAlloc(FImageIndexBuffer, 0);
  //  FreeMem(FImageIndexBuffer);
  //  FImageIndexBuffer := nil;
  //end;
End;

procedure TBZCustomBitmap.ComputeScanLineLUT;
Var
  y: Integer;
Begin
  If (FWidth = 0) Or (FHeight = 0) Then Exit;
  For y := 0 To FMaxHeight Do
  Begin
    FScanLineLUT[y] := DWord(y * FWidth);
    //FScanLineIdxLUT[y] := y * FWidth;
  End;
End;

procedure TBZCustomBitmap.SetLayerIndex(Value : Integer);
var
  Src : TBZCustomBitmap;
  //dx,dy : Integer;
  //pTop, pLeft : Integer;
Begin
  if Value = FLayerIndex then exit;
  FLayerIndex := Value;
  Src := Layers.Items[Value].Bitmap;
  //pLeft := Layers.Items[Value].Left;
  //pTop  := Layers.Items[Value].Top;
  //Showmessage('DrawMode : '+Layers.Items[Value].DrawMode.ToString);
  Self.Clear(clrTransparent);
  Self.PutImage(Src,0,0, Src.Width, Src.Height,0,0);
  Changed();
End;

function TBZCustomBitmap.CheckPixelBound(X, Y : Integer) : Boolean;
Begin
  // Algo Clipping Cohen-Sutherland
  if FClipping then
    Result := (Ord(X < FClipRect.Left) + Ord(X > FClipRect.Right) Shl 1 + Ord(Y < FClipRect.Top) Shl 2 + Ord(Y > FClipRect.Bottom) Shl 3) = 0
  else
    Result := (Ord(X < 0) + Ord(X > FMaxWidth) Shl 1 + Ord(Y < 0) Shl 2 + Ord(Y > FMaxHeight) Shl 3) = 0;
    //((x>=0) or (x<=FMaxWidth)) and ((y>=0) or (y<=FMaxHeight));
End;

function TBZCustomBitmap.GetPixelColorIndex(X, Y : Integer) : Integer;
Var
  YY:     Integer;
  IdxPtr: PDWord;
Begin
  Result := 0;
  If Not (CheckPixelBound(x, y)) Then exit;
  YY := FScanLineLUT[y];
  IdxPtr := PDWord(FImageIndexBuffer + YY);
  Inc(IdxPtr, X);
  Result := IdxPtr^; //PInteger(FImageIndexBuffer+(x*4)+FScanLineLUT[y])^;
End;

procedure TBZCustomBitmap.SetPixelColorIndex(X, Y, Value : Integer);
Var
  PixPtr: PBZColor;
  IdxPtr: PDWord;
  yy:     Integer;
  AColor : TBZColor;
Begin
  If Not(CheckPixelBound(x, y)) Then exit;

  YY := FScanLineLUT[y];
  IdxPtr := PDWord(FImageIndexBuffer + YY);
  Inc(IdxPtr, X);
  IdxPtr^ := Value;

  PixPtr := PBZColor(FSurfaceBuffer + YY);
  Inc(PixPtr, X);
  AColor := FColorManager.Palette.Colors[Value].Value;
  PixPtr^ := AColor;

End;

function TBZCustomBitmap.GetPixelNeighbour(x, y : Integer; const EdgeAction : TBZPixelEdgeAction) : TBZColor;
Var
  {$CODEALIGN VARMIN=16}
  c,cL,cT,cR,cB, ColorF : TBZColorVector;
  {$CODEALIGN VARMIN=4}
begin
  c  := Self.GetPixel(x, y, EdgeAction).AsColorVector;
  cT := Self.GetPixel(x, y - 1, EdgeAction).AsColorVector;
  cB := Self.GetPixel(x, y + 1, EdgeAction).AsColorVector;
  cL := Self.GetPixel(x - 1, y, EdgeAction).AsColorVector;
  cR := Self.GetPixel(x + 1, y, EdgeAction).AsColorVector;

  ColorF := cT + cB + cL + cR;
  ColorF := ColorF * 0.25;
 // Result := (Result + c) div 2;
  ColorF.alpha := c.Alpha;
  Result.Create(ColorF);
end;

//function TBZCustomBitmap.GetPixelBilinear(fx, fy : Single; x, y : Integer; Const EdgeAction : TBZPixelEdgeAction) : TBZColor;
//begin
// dx := fx; dy := fy;
function TBZCustomBitmap.GetPixelBilinear(x, y : Single; const EdgeAction : TBZPixelEdgeAction) : TBZColor;
var
  sx, sxFract, sxFractInv,
  sy, syFract, syFractInv,
  wul, wur, wll, wlr, u, v : Single;
  iu, iv, pLeft, pTop, pRight, pBottom, d : Integer;
  cTL, cTR, cBR, cBL, OutColor : TBZColor;
  PixPtr : PBZColor;
begin

  Case EdgeAction of
    peaZero:
    begin
      if ((x>=0) and (x<=Self.MaxWidth)) and ((y>=0) and (y<=Self.MaxHeight)) then
      begin
        u := x;
        v := y;
      end
      else
      begin
        Result := clrTransparent;
        exit;
      end;
    end;
    peaClamp:
    begin
      u := Clamp(x,0,Self.MaxWidth);
      v := Clamp(y,0,Self.MaxHeight);
    end;
    peaWarp:
    begin
      u := modulus(x, Self.MaxWidth);
      v := modulus(y, Self.MaxHeight)
    end;
  end;
  iu := Floor(u);
  iv := Floor(v);
  sxFract := u - iu;
  sxFractInv := 1.0 - sxFract;
  syFract := v - iv;
  syFractInv := 1.0 - syFract;
  wul := (sxFractInv * syFractInv);
  wur := (sxFract * syFractInv);
  wll := (sxFractInv * syFract);
  wlr := (sxFract * syFract);
  pLeft := iu;
  pTop  := iv;
  if (pLeft = Self.MaxWidth) then pRight := pLeft else pRight := pLeft + 1;
  if (pTop = Self.MaxHeight) then pBottom := pTop else pBottom := pTop + 1;
  d := (pRight - pLeft);
  PixPtr := Self.GetPixelPtr(pLeft, pTop);
  cTL := PixPtr^;
  Inc(PixPtr, d);
  cTR := PixPtr^;
  PixPtr := Self.GetPixelPtr(pLeft, pBottom);
  cBL := PixPtr^;
  Inc(PixPtr, d);
  cBR := PixPtr^;
  OutColor := clrTransparent;
  Result := OutColor.BlendFour(cTL, cTR, cBL, cBR, wul, wur, wll, wlr);
end;

function TBZCustomBitmap.GetPixelBicubic(x, y : Single; const EdgeAction : TBZPixelEdgeAction) : TBZColor;
Const
  BiCubicRPrecal: Array[1..16] Of Single = (0.00260416666666667, 0.0208333333333333, 0.0703125, 0.166666666666667,
    0.315104166666667, 0.479166666666667, 0.611979166666667, 0.666666666666667,
    0.611979166666667, 0.479166666666667, 0.315104166666667, 0.166666666666667,
    0.0703125, 0.0208333333333333, 0.00260416666666667, 0.0);
Var
  dx, dy, weight : Single;
  xs, ys, ii, jj, m, n : Integer;
  Ind1, Ind2, pLeft, pTop : Integer;
  DstCol: TBZColor;
  {$CODEALIGN VARMIN=16}
  ColorF, SumColor : TBZColorVector;
  WeightColor : TBZColorVector;
  {$CODEALIGN VARMIN=4}
begin
  xs := Floor(x);
  ys := Floor(y);
  dx := x - xs;
  dy := y - ys;
  pLeft := xs;
  pTop  := ys;
  SumColor.Create(0,0);
  For m := -1 To 2 Do
  Begin
    For n := -1 To 2 Do
    Begin
      ii := pLeft + m;
      jj := pTop  + n;

      Ind1 := round(4 * (m - dx)) + 8; // Indice de correspondance avec la table précalculée
      Ind2 := round(4 * (n - dy)) + 8; // Idem
      // Evite un bug d'indice hors limites
      Ind1 := Max(Min(Ind1, 16), 1);
      Ind2 := Max(Min(Ind2, 16), 1);

      weight := BiCubicRPrecal[ind1] * BiCubicRPrecal[ind2];
      WeightColor.Create(Weight, Weight);
      ColorF := Self.getPixel(ii, jj, EdgeAction).AsColorVector;
      ColorF := ColorF * WeightColor;
      SumColor := SumColor + ColorF;
    End;
  End;
  DstCol.Create(SumColor);
  Result := DstCol;
end;

function TBZCustomBitmap.GetPixelMean(x, y : Integer; const FilterSize : word; const EdgeAction : TBZPixelEdgeAction) : TBZColor;
Var
  ix, iy, nx,ny : Integer;
  d, r, FilterDelta : Integer;
  outColor : TBZColor;
  {$CODEALIGN VARMIN=16}
  Sum : TBZColorVector;
  {$CODEALIGN VARMIN=4}
begin
  r := FilterSize + FilterSize + 1;
  d := r * r;
  Sum.Create(0,0);
  for iy := -FilterSize to FilterSize do
  begin
    For ix := -FilterSize to FilterSize do
    begin
      nx := x + ix;
      ny := y + iy;
      Sum := Sum + Self.getPixel(nx,ny, EdgeAction).AsColorVector;
      //Inc(d);
    end;
  end;
  Sum := Sum / d;
  outColor.Create(Sum);
  Result := outColor;
end;

function CompareColors(const C1, C2: TBZColorVector): Single; inline;
begin
  Result := (C1.Red * cGCM_CIEObserverRef709.Red +
             C1.Green * cGCM_CIEObserverRef709.Green +
             C1.Blue * cGCM_CIEObserverRef709.Blue) -
            (C2.Red * cGCM_CIEObserverRef709.Red +
             C2.Green * cGCM_CIEObserverRef709.Green +
             C2.Blue * cGCM_CIEObserverRef709.Blue);
end;

procedure InsertionSortPixelArray(Var Pixels : TBZDynColorVectorArray; idxL, idxH : Integer);
var
  Cnt, ps, cs : Integer;
  li,hi : Integer;
  SwapBuf : TBZColorVector;
begin
  cnt := (idxH - idxL);
  if cnt <2 then exit;

  if cnt = 2 then
  begin
    if (CompareColors(Pixels[1], Pixels[0]) < 0) then
    begin
      SwapBuf := Pixels[0];
      Pixels[0] := Pixels[1];
      Pixels[1] := SwapBuf;
      Exit;
    end
    else
    begin
       Exit;
    end;
  end;

  li :=idxL + 1;
  hi :=idxH;

  Repeat
   SwapBuf := Pixels[li]; //Move(pb[ls], SwapBuf^, Stride);
   ps := li;
   cs := ps - 1;

   While (ps >= 1) and  (CompareColors(SwapBuf, Pixels[cs]) < 0) do
   begin
     Pixels[ps] := Pixels[cs];
     dec(ps);
     dec(cs);
   end;

   Pixels[ps] := SwapBuf;
   inc(li);
  until li > hi;
end;

procedure QuickSortPixelArray(Var Pixels : TBZDynColorVectorArray; L, R: Integer);
var
  I, J: Integer;
  {$CODEALIGN VARMIN=16}
  P, Temp: TBZColorVector;
  {$CODEALIGN VARMIN=4}
begin
  repeat
    I := L;
    J := R;
    P := Pixels[(L + R) div 2];
    repeat
      while CompareColors(Pixels[I], P) < 0 do Inc(I);
      while CompareColors(Pixels[J], P) > 0 do Dec(J);
      if I <= J then
      begin
        Temp := Pixels[I];
        Pixels[I] := Pixels[J];
        Pixels[J] := Temp;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSortPixelArray(Pixels, L, J);
    L := I;
  until I >= (R-1);
end;


function TBZCustomBitmap.GetPixelMedian(x, y : Integer; const FilterSize : word; const EdgeAction : TBZPixelEdgeAction) : TBZColor;
Var
  ix, iy, nx,ny : Integer;
  FilterDelta, d, dr,r : Integer;
  TabPix : PBZDynColorVectorArray;
  {$CODEALIGN VARMIN=16}
  ColorF : TBZColorVector;
  {$CODEALIGN VARMIN=4}
  outColor : TBZColor;
begin
  d := 0;
  r := (FilterSize + FilterSize) + 1;
  dr := r * r;
  TabPix := nil;
  GetMem(TabPix, dr * Sizeof(TBZColorVector));
  ColorF.Create(0,0);
  for iy := -FilterSize to FilterSize do
  begin
    For ix := -FilterSize to FilterSize do
    begin
      nx := x + ix;
      ny := y + iy;
      ColorF := Self.getPixel(nx,ny, EdgeAction).AsColorVector;
      TabPix^[d] := ColorF;
      inc(d);
    end;
  end;
  QuickSortPixelArray(TabPix^, 0, dr);
  //InsertionSortPixelArray(TabPix, 0, dr);
  OutColor.Create(TabPix^[(dr div 2)+1]);
  FreeMem(TabPix);
  TabPix := nil;
  Result := outColor;
end;

function TBZCustomBitmap.GetPixelMin(x, y : Integer; const FilterSize : word; const EdgeAction : TBZPixelEdgeAction) : TBZColor;
Var
  ix, iy, nx, ny : Integer;
  dr,d, FilterDelta,r : Integer;
  outColor : TBZColor;
  TabPix : PBZDynColorVectorArray;   //TBZDynColorVectorArray;
  {$CODEALIGN VARMIN=16}
  ColorF : TBZColorVector;
  {$CODEALIGN VARMIN=4}
begin
  d := 0;
  r := (FilterSize + FilterSize) + 1;
  dr := r * r;
  TabPix := nil;
  GetMem(TabPix, dr * Sizeof(TBZColorVector));
  ColorF.Create(0,0);
  for iy := -FilterSize to FilterSize do
  begin
    For ix := -FilterSize to FilterSize do
    begin
      nx := x + ix;
      ny := y + iy;
      ColorF := Self.getPixel(nx,ny, EdgeAction).AsColorVector;
      TabPix^[d] := ColorF;
      inc(d);
    end;
  end;
  QuickSortPixelArray(TabPix^, 0, dr);
  OutColor.Create(TabPix^[0]);
  FreeMem(TabPix);
  TabPix := nil;
  Result := outColor;
end;

function TBZCustomBitmap.GetPixelMax(x, y : Integer; const FilterSize : word; const EdgeAction : TBZPixelEdgeAction) : TBZColor;
Var
  ix, iy, nx, ny : Integer;
  FilterDelta, dr, d, r : Integer;
  outColor : TBZColor;
  TabPix : PBZDynColorVectorArray;   //TBZDynColorVectorArray;
  {$CODEALIGN VARMIN=16}
  ColorF : TBZColorVector;
  {$CODEALIGN VARMIN=4}
begin
  d := 0;
  r := (FilterSize + FilterSize) + 1;
  dr := r * r;
  TabPix := nil;
  GetMem(TabPix, dr * Sizeof(TBZColorVector));
  ColorF.Create(0,0);
  for iy := -FilterSize to FilterSize do
  begin
    For ix := -FilterSize to FilterSize do
    begin
      nx := x + ix;
      ny := y + iy;
      ColorF := Self.getPixel(nx,ny, EdgeAction).AsColorVector;
      TabPix^[d] := ColorF;
      inc(d);
    end;
  end;
  QuickSortPixelArray(TabPix^, 0, dr);
  OutColor.Create(TabPix^[dr-1]);
  FreeMem(TabPix);
  TabPix := nil;
  Result := outColor;
end;

function TBZCustomBitmap.GetPixelMinMax(x, y : Integer; const FilterSize : word; const EdgeAction : TBZPixelEdgeAction) : TBZColor;
Var
  ix, iy, nx, ny : Integer;
  dr,d, FilterDelta,r : Integer;
  outColor : TBZColor;
  TabPix : PBZDynColorVectorArray;   //TBZDynColorVectorArray;
  {$CODEALIGN VARMIN=16}
  ColorF : TBZColorVector;
  {$CODEALIGN VARMIN=4}
begin
  d := 0;
  r := (FilterSize + FilterSize) + 1;
  dr := r * r;
  TabPix := nil;
  GetMem(TabPix, dr * Sizeof(TBZColorVector));
  ColorF.Create(0,0);
  for iy := -FilterSize to FilterSize do
  begin
    For ix := -FilterSize to FilterSize do
    begin
      nx := x + ix;
      ny := y + iy;
      ColorF := Self.getPixel(nx,ny, EdgeAction).AsColorVector;
      TabPix^[d] := ColorF;
      inc(d);
    end;
  end;
  QuickSortPixelArray(TabPix^, 0, dr);
  ColorF := TabPix^[0] + TabPix^[dr-1];
  OutColor.Create(ColorF.DivideBy2);
  FreeMem(TabPix);
  TabPix := nil;
  Result := outColor;
end;

function TBZCustomBitmap.GetPixelSubSample(x, y : Single; const FilterSize : word; const EdgeAction : TBZPixelEdgeAction) : TBZColor;
Var
  ix, iy, nx,ny : Integer;
  d, r, nbSample,FilterDelta : Integer;
  outColor : TBZColor;
  xx, yy : Single;
  {$CODEALIGN VARMIN=16}
  Sum, AAWeight, vColor : TBZColorVector;
  v1, v2 : TBZFloatPoint;
  p : TBZPoint ;
  {$CODEALIGN VARMIN=4}
begin
  //r := FilterSize;
  r := Round(FilterSize * (System.Sqrt(5)*0.5)); //Rotation sampling
  if not(Odd(r)) then r := FilterSize + 1;
  //d := r * r;
  d := 0;
  FilterDelta := Floor(r * 0.5);
  //AdaptativeThreshold : = (r - 2) * FilterDelta;
  //nbSample := 0;
  Sum.Create(0,0);
  v2.Create(x,y);
  for iy := -FilterDelta to FilterDelta do
  begin
    yy := y + 1.0 / r * iy + 0.5;
    For ix := -FilterDelta to FilterDelta do
    begin
      xx := x + 1.0 / r * ix + 0.5;
      v1.Create(xx,yy);
      v1.Rotate(26.6,v2);
      p := v1.Round;
      vColor := Self.getPixelBilinear(p.x,p.y, EdgeAction).AsColorVector;
      AAWeight.Create(vColor.Alpha,1.0);
      Sum := Sum + (vColor * AAWeight);
      Inc(d);
    end;
  end;
  AAWeight.Create(Sum.Alpha, d);
  Sum := Sum / AAWeight;
  outColor.Create(Sum);
  Result := outColor;
end;

function TBZCustomBitmap.GetSamplePixel(x, y : Single; SubSampleMode : TBZGetPixelSampleMethod; const FilterSize : word; const EdgeAction : TBZPixelEdgeAction) : TBZColor;
Var
  OutColor : TBZColor;
begin
  Case SubSampleMode of
    psmDefault : OutColor := getPixel(Round(x), Round(y), EdgeAction);
    psmNeighbour : OutColor := GetPixelNeighbour(Round(x), Round(y), EdgeAction);
    psmBilinear : OutColor := GetPixelBilinear(x, y, EdgeAction);
    psmBicubic : OutColor := GetPixelBicubic(x, y, EdgeAction);
    psmMean : OutColor := GetPixelMean(Round(x), Round(y), FilterSize, EdgeAction);
    psmMedian  : OutColor := GetPixelMedian(Round(x), Round(y), FilterSize, EdgeAction);
    psmMin :  OutColor := GetPixelMin(Round(x), Round(y), FilterSize, EdgeAction);
    psmMax : OutColor := GetPixelMax(Round(x), Round(y), FilterSize, EdgeAction);
    psmMinMax : OutColor := GetPixelMinMax(Round(x), Round(y), FilterSize, EdgeAction);
    psmSubSampling : OutColor := GetPixelSubSample(x, y, FilterSize, EdgeAction);
    //psmCustom
  end;
  Result := OutColor;
end;

function TBZCustomBitmap.getIntensityMax : Byte;
Var
  j : Integer;
  i1, i2 : Byte;
  PixPtr : PBZColor;
begin
  j := 0;
  i2 := 0;
  PixPtr := Self.getSurfaceBuffer;
  while (j<Self.MaxSize) do
  begin
    i1 := PixPtr^.Luminance;
    i2 := Max(i1, i2);
    //if i1 > i2 then i2 := i1;
    inc(j);
    inc(PixPtr);
  end;
  Result := i2;
end;

function TBZCustomBitmap.getIntensityMin : Byte;
Var
  j : Integer;
  i1, i2 : Byte;
  PixPtr : PBZColor;
begin
  j := 0;
  i2 := 255;
  PixPtr := Self.getSurfaceBuffer;
  while (j<Self.MaxSize) do
  begin
    i1 := PixPtr^.Luminance;
    i2 := Min(i1, i2);
    //if i1 < i2 then i2 := i1;
    inc(j);
    inc(PixPtr);
  end;
  Result := i2;
end;

function TBZCustomBitmap.getIntensityAverage : Byte;
Var
  j : Integer;
  i1 : Byte;
  i2 : Integer;
  PixPtr : PBZColor;
begin
  j := 0;
  i2 := 0;
  PixPtr := Self.getSurfaceBuffer;
  while (j<Self.MaxSize) do
  begin
    i1 := PixPtr^.Luminance;
    i2 := i2 + i1;
    //if i1 < i2 then i2 := i1;
    inc(j);
    inc(PixPtr);
  end;
  Result := i2 div (Self.MaxSize+1);
end;

function TBZCustomBitmap.GetVariance(IntensityAverage : Byte) : Single;
Var
  j : Integer;
  i1 : Byte;
  TotalCount, Delta : Integer;
  Variance : Single;
  PixPtr : PBZColor;
begin
  j := 0;
  Variance := 0;
  TotalCOunt := Self.MaxSize + 1;
  PixPtr := Self.getSurfaceBuffer;
  while (j<Self.MaxSize) do
  begin
    i1 := PixPtr^.Luminance;
    delta := i1 - IntensityAverage;
    variance := variance + (delta * delta) / TotalCount;
    inc(j);
    inc(PixPtr);
  end;
  Result := Variance;
end;

function TBZCustomBitmap.GetEnergy : Single;
Var
  j : Integer;
  i1 : Single;
  Energy : Single;
  PixPtr : PBZColor;
begin
  j := 0;
  Energy := 0;
  PixPtr := Self.getSurfaceBuffer;
  while (j<Self.MaxSize) do
  begin
    i1 := PixPtr^.Luminance; // /255
    Energy := Energy + (i1 * i1);
    inc(j);
    inc(PixPtr);
  end;
  Result := System.Sqrt(Energy);
end;

procedure TBZCustomBitmap.DrawPixel(x, y : Integer; const ForeColor : TBZColor; const ADrawMode : TBZBitmapDrawMode; const AAlphaMode : TBZBitmapAlphaMode; const MasterAlpha : Byte; const CombineMode : TBZColorCombineMode; const BlendSrcFactor : TBZBlendingFactor; const BlendDstFactor : TBZBlendingFactor);
Var
 SrcColor, DstColor: TBZColor;
begin
  If (Clipping And  ClipRect.PointOutRect(X, Y)) Then exit;

  If (ADrawMode = dmSet) and (AAlphaMode = amNone) Then
  Begin
    SetPixel(x, y, ForeColor);
    Exit;
  End;
  SrcColor := Self.GetPixel(x, y);
  DstColor := ForeColor;

  If (ADrawMode = dmSetAlpha) Then    //dmSetAlphaFromIntensity
  Begin
    SrcColor.alpha := ForeColor.alpha;
    SetPixel(x, y, SrcColor);
    exit;
  End;

  If (ADrawMode = dmCombine) then
  begin
    DstColor := SrcColor.Combine(ForeColor, CombineMode);
  end;

 // if AAlphaMode = amNone then exit;

  Case AALphaMode of
     amNone :  SetPixel(x, y, DstColor);
     amOpaque :
     begin
       DstColor.Alpha := 255;
       SetPixel(x, y, DstColor);
     end;
     amAlpha : if DstColor.Alpha > 0 then SetPixel(x, y,SrcColor.Alphablend(DstColor));
     amAlphaBlend :
     begin

       if (DstColor.Alpha > 0) then Self.SetPixel(x, y,SrcColor.Blend(DstColor, MasterAlpha));

     end;
     amAlphaBlendHQ:
     begin
       if DstColor.Alpha > 0 then SetPixel(x, y,SrcColor.BlendHQ(ForeColor, DstColor, MasterAlpha));
     end;
     amBlend:
     begin
       if DstColor.Alpha > 0 then SetPixel(x, y,SrcColor.Blend(DstColor, BlendSrcFactor, BlendDstFactor));
     end;
     amAlphaSrc :
     Begin
       DstColor.Alpha := ForeColor.alpha;
       SetPixel(x, y, DstColor.AlphaBlend(SrcColor));
     End;
     amAlphaDst :
     Begin
       DstColor.Alpha := SrcColor.alpha;
       SetPixel(x, y, SrcColor.AlphaBlend(DstColor));
     End;
     amAlphaCheck :
     Begin
       If DstColor.Alpha > 0 Then SetPixel(x, y, SrcColor.AlphaBlend(DstColor));
     End;
  end;
end;

//procedure TBZCustomBitmap.DrawPixel(const x, y : Integer; const Col : TBZColor; const ADrawMode : TBZBitmapDrawMode; const AAlphaMode : TBZBitmapAlphaMode; const SrcFactor : TBZBlendingFactor; const DstFactor : TBZBlendingFactor);
//Var
//  SrcColor, DstColor: TBZColor;
//
//Begin
//  If (Clipping And  ClipRect.PointOutRect(X, Y)) Then exit;
//
//  If (ADrawMode = dmSet) And (AAlphaMode = amNone) Then
//  Begin
//    SetPixel(x, y, Col);
//    Exit;
//  End;
//
//  If ADrawMode = dmSetAlpha Then    //dmSetAlphaFromIntensity
//  Begin
//    SrcColor := GetPixel(x, y);
//    SrcColor.alpha := Col.alpha;
//    SetPixel(x, y, SrcColor);
//    exit;
//  End;
//
//  Case ADrawMode Of
//    dmSet:
//    Begin
//      SrcColor := GetPixel(x, y);
//      DstColor := Col;
//    End;
//    dmBlend:
//    Begin
//      SrcColor := GetPixel(x, y);
//      DstColor := SrcColor.Blend(Col, SrcFactor, DstFactor);
//    End;
//    dmAdd:
//    Begin
//      SrcColor := GetPixel(x, y);
//      DstColor := SrcColor + Col;
//    End;
//    dmSub:
//    Begin
//      SrcColor := GetPixel(x, y);
//      DstColor := SrcColor - Col;
//    End;
//    dmMul:
//    Begin
//      SrcColor := GetPixel(x, y);
//      DstColor := SrcColor * Col;
//    End;
//    dmDiv:
//    Begin
//      SrcColor := GetPixel(x, y);
//      DstColor := SrcColor div Col;
//    End;
//    dmOr:
//    Begin
//      SrcColor := GetPixel(x, y);
//      DstColor := SrcColor Or Col;
//    End;
//    dmXor:
//    Begin
//      SrcColor := GetPixel(x, y);
//      DstColor := SrcColor Xor Col;
//    End;
//    dmAnd:
//    Begin
//      SrcColor := GetPixel(x, y);
//      DstColor := SrcColor And Col;
//    End;
//    dmAverage:
//    Begin
//      SrcColor := GetPixel(x, y);
//      DstColor := SrcColor.Average(Col);
//    End;
//    dmModulate:
//    Begin
//      SrcColor := GetPixel(x, y);
//      DstColor := SrcColor.Modulate(Col);
//    End
//    //dmBlendFilter
//    //dmCustom
//    Else
//    Begin
//      SrcColor := GetPixel(x, y);
//      DstColor := Col;
//    End;
//  End;
//
//  Case AAlphamode Of
//    amNone: SetPixel(x, y, DstColor);
//    amAlpha: SetPixel(x, y,SrcColor.Alphablend(DstColor));
//    //DstColor.AlphaMix(SrcColor,DstColor.Alpha/SrcColor.Alpha));
//    //DstColor.AlphaMix(SrcColor, ((DstColor.Alpha/SrcColor.Alpha)*_FloatColorRatio))); // DstPtr^:=DstCol;
//    amAlphaBlend: SetPixel(x, y,SrcColor.AlphaMix(DstColor)); // DstPtr^:=DstCol;
//    amAlphaCheck:
//    Begin
//      If DstColor.Alpha > 0 Then SetPixel(x, y, SrcColor.AlphaBlend(DstColor));
//    End;
//    amOpaque:
//    Begin
//      DstColor.Alpha := clrAlphaOpaque;
//      SetPixel(x, y, DstColor);
//    End;
//    amAlphaSrc:
//    Begin
//      DstColor.Alpha := Col.alpha;
//      SetPixel(x, y, SrcColor.AlphaBlend(DstColor));
//    End;
//    amAlphaDst:
//    Begin
//      DstColor.Alpha := SrcColor.alpha;
//      SetPixel(x, y, SrcColor.AlphaBlend(DstColor));
//    End
//  End;
//End;

function TBZCustomBitmap.getColorManager : TBZColorsManager;
Begin
  Result := FColorManager;
End;

function TBZCustomBitmap.getSurfaceBuffer : PBZColor;
Begin
  Result := PBZColor(FSurfaceBuffer);
End;

function TBZCustomBitmap.getSurfaceIndexedBuffer : PDWord;
Begin
  Result := FImageIndexBuffer;
End;

function TBZCustomBitmap.GetScanLine(Row : LongInt) : PBZColor;
Var
  yy: Integer;
Begin
  yy := FScanLineLUT[Row];
  //result:=PBZColor(FSurfaceBuffer+Row*Width*4);
  Result := PBZColor(FSurfaceBuffer + YY);
End;

function TBZCustomBitmap.GetScanLineLUT : TBZScanLineLUT;
begin
  Result := FScanLineLUT;
end;

function TBZCustomBitmap.GetPixelPtr(X, Y : Integer) : PBZColor;
Var
  yy: Integer;
Begin
  result := nil;
  If Not(CheckPixelBound(x, y)) Then exit;
  yy := FScanLineLUT[Y];
  // xx:=yy+x;
  Result := PBZColor(FSurfaceBuffer + YY + X);// (X * 4));
End;

procedure TBZCustomBitmap.SetSize(NewWidth, NewHeight : Integer);
Begin
  If (FWidth = Abs(NewWidth)) And (FHeight = Abs(NewHeight)) Then exit;
  FWidth := Abs(NewWidth); // abs au cas ou cela serai négatif (lors d'un chargement de fichier incorrecte par exemple)
  FHeight := Abs(NewHeight);

  // Les dimension ne peuvent pas être égale à zero, on ajuste
  If FWidth = 0 Then FWidth := 1;
  If FHeight = 0 Then FHeight := 1;
 // {$IFDEF DEBUG}
  GlobalLogger.LogNotice('TBZCustomBitmap.SetSize : ' + IntToStr(FWidth) + 'x' + IntToStr(FHeight));
 // {$ENDIF}
  FSize := int64((Int64(FWidth) * Int64(FHeight)) * 4); //sizeof(TBZColor); //FImageDescription.Description.PixelSize;
  GlobalLogger.LogNotice('TBZCustomBitmap.SetSize Buffer size : '+FSize.ToString());
  FMaxWidth := FWidth - 1;
  FMaxHeight := FHeight - 1;
  FMaxSize := (Int64(FWidth) * Int64(FHeight)) - 1;
  FCenterX := (FWidth Shr 1) - 1;
  FCenterY := (FHeight Shr 1) - 1;
  FLineSizeInBytes := FWidth * 4;
  FClipRect.Create(0, 0, FMaxWidth, FMaxHeight);
  If FSurfaceBuffer <> nil Then
  Begin
    memReAlloc(FSurfaceBuffer, 0);
    FreeMem(FSurfaceBuffer);
    FSurfaceBuffer := nil;
  End;
  if memReAlloc(FSurfaceBuffer, FSize) < 0 then
  begin
    Raise Exception.Create('Out of Memory');
    exit;
  end;

  setLength(FScanLineLUT, FHeight);
 // setLength(FScanLineIdxLUT, FHeight);

  ComputeScanLineLUT;

  If FUsePalette Then
  Begin
    memReAlloc(FImageIndexBuffer, 0);
    FreeMem(FImageIndexBuffer);
    FImageIndexBuffer := nil;
    memReAlloc(FImageIndexBuffer, int64((FWidth*FHeight)*Sizeof(DWord)));
  End;
  //    updateProperties;
  NotifyChange(self);
End;

procedure TBZCustomBitmap.SetSize(ANewSize : LongWord);
begin
  If FSurfaceBuffer <> nil Then
  Begin
    memReAlloc(FSurfaceBuffer, 0);
    FreeMem(FSurfaceBuffer);
    FSurfaceBuffer := nil;
  End;
  FSize := ANewSize;
  memReAlloc(FSurfaceBuffer, ANewSize);
end;

function TBZCustomBitmap.GetInternalPixelColor(x, y : Integer) : Integer;
Begin
  Result := 0;
  //if not(CheckPixelBound(x,y)) then exit;
  If FUsePalette Then
  Begin
    Result := GetPixelColorIndex(x, y);
  End;
End;

procedure TBZCustomBitmap.SetInternalPixelColor(x, y : Integer; Value : Integer);
Begin
  If FUsePalette Then
  Begin
    setPixelColorIndex(x, y, Value);
  End;
End;

function TBZCustomBitmap.CloneSurfaceBuffer(const BufferData : PByte; var Clone : PByte) : Boolean;
Var
  aSize: Integer;
Begin
  Result := False;
  Try
    aSize := Sizeof(BufferData);
    memReAlloc(Clone, aSize);
    Move(BufferData^, Clone^, aSize);
    Result := True;
  Except
    Raise Exception.Create('Erreur pendant le clonage du buffer');
  End;
End;

procedure TBZCustomBitmap.AddLayer(anImage : TBZCustomBitmap);
Begin

End;

procedure TBZCustomBitmap.Changed;
begin
  NotifyChange(self);
end;


function TBZCustomBitmap.getPixel(x, y : Integer) : TBZColor; //Inline;
Var
  yy: Integer;
  PixelPtr: PBZColor;
Begin
  Result := clrTransparent;
  // On verifie toujours les limites, on pourrait omettre cette ligne
  // mais cela évite d'innombrables erreures de dépassement. Mais attention si vous ne borner pas correctement la position
  // de vos pixels, vous ne le saurez jamais. A decommenter quand on sera sur à 100% de toutes les fonctions de manipulation
  if not(CheckPixelBound(x,y)) then exit;
  yy := FScanLineLUT[y];
  PixelPtr := PBZColor(FSurfaceBuffer + yy); // 1 On va d'abord à la ligne
  Inc(PixelPtr, X); // 2 On se deplace sur les X
  Result := PixelPtr^; // On recupere la valeur
End;

function TBZCustomBitmap.getPixel(x, y : Integer; EdgeAction : TBZPixelEdgeAction) : TBZColor;
  function Reflect(xx : Integer; aMax : Integer) : Integer;
  Var
    r : Boolean;
  begin
    r := False;
    Result := xx;
    if (Result < 0) then
    begin
      While (Result < 0) do
      begin
        Result := Result + aMax;
        r := not(r);
      end;
    end
    else if (Result > aMax) then
    begin
      While (Result > aMax) do
      begin
        Result := Result - aMax;
        r := not(r);
      end;
    end;
    if r then
    begin
      Result := aMax - xx;
    end;
  end;

begin
  Case EdgeAction of
    peaZero: Result := getPixel(x,y);
    peaClamp: Result := getPixel(Clamp(x,0,MaxWidth), Clamp(y,0,MaxHeight));
    peaWarp: Result := getPixel(modulus(x, MaxWidth), modulus(y, MaxHeight));
    peaReflect: Result := getPixel(Reflect(x, Self.Width), Reflect(y, Self.Height));
  end;
end;

function TBZCustomBitmap.getPixelOffset(Offset : Integer) : TBZColor;
begin
  Result := PBZColor(FSurfaceBuffer + Offset)^;
end;

function TBZCustomBitmap.getPixelCycle(x, y : Integer) : TBZColor;
Var
  yy,px,py : Integer;
  PixelPtr: PBZColor;

  //function MyMod(a,b : Integer) : Integer;
  //var
  //  n : integer;
  //begin
  //  n := Round(a/b);
  //	a := a - (n*b);
  //	if (a < 0) then
  //			result := a + b
  //  else
  // 		result := a;
  //end;

Begin
  px := x;
  py := y;
  if px>FMaxWidth then px:= (px Mod FWidth) -1; //MyMod(px,FWidth);
  if py>FMaxHeight then py:= (py Mod FHeight) -1; // MyMod(py,FHeight);
  if px<0 then px := abs(px);
  if py<0 then py := abs(py);
  //GlobalLogger.LogNotice('GetPixelCycle : '+px.ToString()+', ' +py.ToString());
  yy := FScanLineLUT[py];
  PixelPtr := PBZColor(FSurfaceBuffer + yy); // 1 On va d'abord à la ligne
  Inc(PixelPtr, pX); // 2 On se deplace sur les X
  Result := PixelPtr^; // On recupere la valeur
end;

procedure TBZCustomBitmap.setPixel(x, y : Integer; aValue : TBZColor); //Inline;
Var
  yy:     Integer;
  //xx:Integer
  PixPtr: PBZColor;   // On travail toujours sur une copie
Begin
  // Ici faut mieux tourner à gauche que de se prendre un mur avec un gros tag "SIGSEV" dessus,
  // ou un résultat erroné. Dans certaines manipulations la position des pixels calculés sort des limites.
  // Tronquer ces valeurs  ou les "clipper" donnerait des erreurs de rendu.
  If Not(CheckPixelBound(x, y)) Then exit;
  yy := FScanLineLUT[y]; // yy:=yy+(x*4);
  // Note pour obtenir la bonne adresse. on doit le faire en 2 temps et travaillez avec 1 copie
  PixPtr := PBZColor(FSurfaceBuffer + YY); // 1 On va d'abord à la ligne
  Inc(PixPtr, X);  // 2 On se deplace sur les X
  PixPtr^ := aValue; // On écrit la valeur
End;


procedure TBZCustomBitmap.setPixelOffset(Offset : DWord; aValue : TBZColor);
begin
  PBZColor(FSurfaceBuffer + Offset)^ := aValue;
end;

procedure TBZCustomBitmap.setPixelAlphaBlend(x, y : Integer; aValue : TBZColor);
Var
  yy:     Integer;
  PixPtr: PBZColor;
begin
  If Not(CheckPixelBound(x, y)) Then exit;
  yy := FScanLineLUT[y];
  PixPtr := PBZColor(FSurfaceBuffer + YY + X);
  PixPtr^:= PixPtr^.AlphaBlend(aValue);
end;

procedure TBZCustomBitmap.setPixelCheckAlpha(x, y : Integer; aValue : TBZColor);
begin
  if aValue.Alpha > 0 then SetPixel(x,y,aValue);
end;

(* Function  TBZCustomBitmap.getPixelColor(x, y : Integer): Integer;
begin
  //if FClipping then if InternalClipOut(x,y,FClipRect) then exit;
  result:=getInternalPixelColor(x,y);
end;

Procedure TBZCustomBitmap.setPixelColor(x, y : Integer; aValue: Integer);
begin
  //if FClipping then if InternalClipOut(x,y,FClipRect) then exit;
  setInternalPixelColor(x, y,aValue);
end; *)

procedure TBZCustomBitmap.LoadFromFile(const FileName : String);
Var
  BaseImageClass: TBZBitmapClass;
  tempImage:      TBZCustomBitmap;
  //bSize: Longint;
Begin
  //GlobalLogger.LogNotice('TBZBitmap.LoadFromFile');
  If filename = '' Then exit;
  BaseImageClass := GetBZImageFileFormats.FindFromFileName(FixPathDelimiter(filename));
  TempImage := nil;
  tempImage := BaseImageClass.Create(0, 0);
  TempImage.OnProgress := Self.OnProgress;
  TempImage.OnLoadError := Self.OnLoadError;
  Try
    //GlobalLogger.LogNotice('--> Format found : '+TempImage.ClassName);
    if Layers.Count>0 then Layers.Clear;
    tempImage.LoadFromFile(fileName);
    Self.Assign(TempImage);

    if IsMultiImage  then //or (ExtractFileExt(FileName) ='.gif')
    begin
      Clear(Layers.BackgroundColor);
      //On assigne la 1ere image
      PutImage(Layers.Items[0].Bitmap,0,0,Width, Height,0,0);
    End;
  Finally
   // TempImage.NotifyError;
    FreeAndNil(tempImage);
  End;
end;

procedure TBZCustomBitmap.PreMultiplyAlpha;
Var
  i: Integer;
  DstPtr, SrcPtr: PBZColor;
Begin
  if not(FIsPremultiply) then
  begin
    i := 0;
    SrcPtr := GetSurfaceBuffer;
    DstPtr := GetSurfaceBuffer;
    Repeat
      DstPtr^.Blue := PreMultipliedAlphaLUT[SrcPtr^.Blue][SrcPtr^.Alpha];
      DstPtr^.Green := PreMultipliedAlphaLUT[SrcPtr^.Green][SrcPtr^.Alpha];
      DstPtr^.Red := PreMultipliedAlphaLUT[SrcPtr^.Red][SrcPtr^.Alpha];
      DstPtr^.Alpha := SrcPtr^.Alpha;
      Inc(SrcPtr);
      Inc(DstPtr);
      Inc(i);
    Until i > MaxSize;
    FIsPremultiply := True;
  end;
End;

procedure TBZCustomBitmap.UnPreMultiplyAlpha;
Var
  i: Integer;
  DstPtr, SrcPtr: PBZColor;
  d: Single;
Begin
  if (FIsPremultiply) then
  begin
    i := 0;
    SrcPtr := GetSurfaceBuffer;
    DstPtr := GetSurfaceBuffer;
    Repeat
      // DstPtr^.Alpha := SrcPtr^.Alpha;
      If SrcPtr^.Alpha > 0 Then
      Begin
        d := (SrcPtr^.Alpha / 255);
        DstPtr^.Blue := ClampByte(round(SrcPtr^.Blue * d));
        DstPtr^.Green := ClampByte(round(SrcPtr^.Green * d));
        DstPtr^.Red := ClampByte(round(SrcPtr^.Red * d));
        DstPtr^.Alpha := SrcPtr^.Alpha;
      End
      Else
      Begin
        DstPtr^.Blue := 0;
        DstPtr^.Green := 0;
        DstPtr^.Red := 0;
        DstPtr^.Alpha := 0;
      End;
      Inc(SrcPtr);
      Inc(DstPtr);
      Inc(i);
    Until i > MaxSize;
    FIsPremultiply := False;
  end;
End;

procedure TBZCustomBitmap.Clear(aColor : TBZColor);
Begin
  //GlobalLogger.LogStatus('Clear Color : '+aColor.ToString);
  FillLongWord(FSurfaceBuffer^, FWidth * FHeight , DWord(aColor));
End;

procedure TBZCustomBitmap.Clear(aColorIndex : Integer);
Begin
  If FUsePalette Then
  Begin
    //FillDWord(FImageIndexBuffer^, (FWidth*FHeight) , DWord(aColorIndex));
    FillLongWord(FSurfaceBuffer^, FWidth * FHeight , DWord(aColorIndex));
    Clear(FColorManager.Palette.Colors[aColorIndex].Value);
  End;
End;

procedure TBZCustomBitmap.PutImage(
  const ASrcBmp : TBZCustomBitmap; SrcX, SrcY, cfWidth, cfHeight : Integer; DstX, DstY : Integer; const ADrawMode : TBZBitmapDrawMode; const AAlphaMode : TBZBitmapAlphaMode; const MasterAlpha : Byte; const CombineMode : TBZColorCombineMode; const BlendSrcFactor : TBZBlendingFactor; const BlendDstFactor : TBZBlendingFactor);

  Procedure ClipCopyRect(Var SrcX, SrcY, Width, Height, DstX, DstY: Integer; SrcImageWidth, SrcImageHeight: Integer; Const DstClip: Types.TRect);
  Var
    diff, OldDstPosX, OldDstPosY: Integer;
  Begin
    OldDstPosX := 0;
    If (DstX < 0) Then OldDstPosX := DstX;
    OldDstPosY := 0;
    If (DstY < 0) Then OldDstPosY := DstY;

    If DstX < DstClip.Left Then
    Begin
      Diff := DstClip.Left - DstX;
      Width := Width - Diff;
      SrcX := SrcX + Diff;
      DstX := DstClip.Left;
    End;

    If DstY < DstClip.Top Then
    Begin
      Diff := DstClip.Top - DstY;
      Height := Height - Diff;
      SrcY := SrcY + Diff;
      DstY := DstClip.Top;
    End;

    If SrcX < 0 Then
    Begin
      Width := Width + SrcX - OldDstPosX;
      DstX := DstX - SrcX + OldDstPosX;
      SrcX := 0;
    End;

    If SrcY < 0 Then
    Begin
      Height := Height + SrcX - OldDstPosY;
      DstY := DstY - SrcY + OldDstPosY;
      SrcY := 0;
    End;

    If ((SrcX + Width) > SrcImageWidth) Then Width := SrcImageWidth - SrcX;
    If ((SrcY + Height) > SrcImageHeight) Then Height := SrcImageHeight - SrcY;

    if DstX > FWidth then DstX := 0;
    if DstY > FHeight then DstY := 0;

    If ((DstX + Width) > DstClip.Right) Then Width := DstClip.Right - DstX;
    If ((DstY + Height) > DstClip.Bottom) Then Height := DstClip.Bottom - DstY;
  End;

Var
  TotalSize, xx, yy, i, nextSrcLine, nextDstLine: Integer;
  LineSize: Longint;
  SrcPtr, DstPtr: PBZColor;
  BackColor, SrcColor, DstColor: TBZColor;
  CoefAlpha : Single;
Begin
  assert( Assigned(ASrcBmp),'Erreur : Le bitmap source n''est pas assigné !');
  if (cfWidth=0) and (cfHeight=0) then exit;
  if (DstX < -cfWidth ) or (DstY < -cfHeight) or (DstX > Self.MaxWidth) or (DstY > Self.MaxHeight) then exit;

  {%H-}ClipCopyRect(SrcX, SrcY, cfWidth, cfHeight, DstX, DstY, ASrcBmp.Width, ASrcBmp.Height, Types.Rect(0, 0, Self.Width, Self.Height));

  If (cfWidth <= ASrcBmp.Width) Then  nextSrcLine := ASrcBmp.Width
  Else nextSrcLine := SrcX + (ASrcBmp.Width - (SrcX + cfWidth));

  SrcPtr := ASrcBmp.GetPixelPtr(SrcX, SrcY);
  DstPtr := self.GetPixelPtr(DstX, DstY);

  If (ADrawMode = dmSet) And (AAlphamode = amNone) Then
  Begin
    if (((ASrcBmp.Width = FWidth) and (ASrcBmp.Height = FHeight)) and
       ((cfWidth = FWidth) and (cfHeight = FHeight))) //and ((DstX= 0) and (DstY = 0))
        then Move(SrcPtr^,DstPtr^,DWord(ASrcBmp.Size))
    else
    begin
      LineSize := CfWidth * 4;
      For I := 0 to cfHeight-1 do
      begin
        Move(SrcPtr^, DstPtr^, LineSize);
        Inc(SrcPtr, NextSrcLine);
        Inc(DstPtr, FWidth);
      End;
    End;
  End
  Else
  Begin
    //CoefAlpha := MasterAlpha * _FloatColorRatio;
    totalsize := (cfWidth * cfHeight) - 1;
    Dec(cfHeight);
    xx := 0;
    Dec(cfWidth);
    nextSrcLine := SrcX + (ASrcBmp.Width - (SrcX + cfWidth));
    nextDstLine := DstX + (Self.Width - (DstX + cfWidth));
    yy := 0;
    xx := 0;
    //SrcCol := clrTransparent;
    //DstCol := clrTransparent;
    While (yy <= TotalSize) Do
    Begin
      BackColor := DstPtr^;
      SrcColor := SrcPtr^;

      If (ADrawMode = dmSetAlpha) Then    //dmSetAlphaFromIntensity
      Begin
        DstColor := BackColor;
        DstColor.alpha := SrcColor.alpha;
        DstPtr^ := DstColor;
      End
      else
      begin
        If (ADrawMode = dmCombine) then
        begin
           if (CombineMode = cmErase) then
          begin
            If (SrcColor.Alpha > 0) then DstPtr^ := DstColor;
          end
          else DstColor := BackColor.Combine(SrcColor, CombineMode);
        end
        else DstColor := SrcColor;

        Case AAlphaMode of
          amNone :  If (DstColor.Alpha > 0) then DstPtr^ := DstColor;
          amOpaque :
          begin
            DstColor.Alpha := 255;
            DstPtr^ := DstColor;
          end;
          amAlpha :  If (DstColor.Alpha > 0) then DstPtr^ := BackColor.Alphablend(DstColor);
          amAlphaBlend :  If (DstColor.Alpha > 0) then  DstPtr^ := BackColor.blend(DstColor, MasterAlpha);
          amAlphaBlendHQ : If (DstColor.Alpha > 0) then DstPtr^ := BackColor.blendHQ(SrcColor, DstColor, MasterAlpha); //Round(DstColor.Alpha * CoefAlpha));
          amBlend: DstPtr^ := BackColor.Blend(DstColor, BlendSrcFactor, BlendDstFactor);
          amAlphaSrc :
          Begin
            BackColor.Alpha := SrcColor.alpha;
            DstPtr^ := BackColor.Alphablend(DstColor);
          End;
          amAlphaDst :
          Begin
            DstColor.Alpha := BackColor.alpha;
            DstPtr^ := BackColor.Alphablend(DstColor);
          End;
          amAlphaCheck :
          Begin
            If (DstColor.Alpha > 0)  or (ASrcBmp.ImageDescription.HasAlpha and (DstColor <> ASrcBmp.ImageDescription.TransparentColor)) Then
            DstPtr^ := BackColor.Alphablend(DstColor) else DstPtr^:= BackColor;
          End;
        end;
      end;

      Inc(xx);
      Inc(yy);
      If xx > cfWidth Then
      Begin
        xx := 0;
        Inc(DstPtr, NextDstLine);
        Inc(SrcPtr, NextSrcLine);
      End
      Else
      Begin
        Inc(SrcPtr);
        Inc(DstPtr);
      End;
    End;
  End;
End;

procedure TBZCustomBitmap.PutImage(const ASrcBmp : TBZCustomBitmap; DstX, DstY : Integer);
begin
  PutImage(ASrcBmp,0,0,ASrcBmp.Width, ASrcBmp.Height,DstX,DstY);
end;

procedure TBZCustomBitmap.PutImage(const ASrcBmp : TBZCustomBitmap; DstX, DstY : Integer; MasterAlpha : Byte);
begin
  PutImage(ASrcBmp,0,0,ASrcBmp.Width, ASrcBmp.Height,DstX,DstY,dmSet, amAlphaBlend, MasterAlpha);
end;

procedure TBZCustomBitmap.PutImage(
  const ASrcBmp : TBZCustomBitmap; DstX, DstY : Integer; MasterAlpha : Byte; ADrawMode : TBZBitmapDrawMode; AAlphaMode : TBZBitmapAlphaMode);
begin
  PutImage(ASrcBmp,0,0,ASrcBmp.Width, ASrcBmp.Height,DstX,DstY, ADrawMode, AAlphaMode, MasterAlpha);
end;

procedure TBZCustomBitmap.PutImageBlend(
  const ASrcBmp : TBZCustomBitmap; DstX, DstY : Integer; const CombineMode : TBZColorCombineMode; const MasterAlpha : Byte);
begin
  PutImage(ASrcBmp,0,0,ASrcBmp.Width, ASrcBmp.Height,DstX,DstY, dmCombine, amAlphaBlendHQ, MasterAlpha, CombineMode);
end;

procedure TBZCustomBitmap.PutImageStretch(Source : TBZCustomBitmap; DestLeft, DestTop, DestRight, DestBottom : Integer; const ADrawMode : TBZBitmapDrawMode; const AAlphaMode : TBZBitmapAlphaMode; const MasterAlpha : Byte; const CombineMode : TBZColorCombineMode);
var
  SrcPtr, DstPtr : PBZColor;
  SrcStartX, SrcStartY, StartX, EndX, StartY, EndY, W, H, X, Y, XX,YY, px, py : Integer;
  SgnY, SgnX, Dx, Dy, U, V : Single;
  SrcColor, InColor, OutColor, MergeColor : TBZColor;
begin
  //if DestLeft<0 then DestLeft :=0;

  if DestBottom > DestTop then
  begin
    SrcStartY := 0;
    StartY := DestTop;
    EndY := DestBottom;
    SgnY := 1.0;
  end
  else
  begin
    SrcStartY := Source.MaxHeight;
    StartY := DestBottom;
    EndY := DestTop;
    SgnY := -1.0;
  end;

  if DestRight > DestLeft then
  begin
    SrcStartX := 0;
    StartX := DestLeft;
    EndX := DestRight;
    SgnX := 1.0;
  end
  else
  begin
    SrcStartX := Source.MaxWidth;
    StartX := DestRight;
    EndX := DestLeft;
    SgnX := -1.0;
  end;

  W := (EndX - StartX)+1;
  H := (EndY - StartY)+1;

  Dx := (Source.Width / W) * SgnX;
  Dy := (Source.Height / H) * SgnY;

  Dec(H);
  Dec(W);
  V := SrcStartY;
  For Y := 0 to H do
  begin
    py := Clamp(Trunc(V), 0, Source.MaxHeight);
    SrcPtr := Source.GetScanLine(py);
    YY := StartY+Y;

    if (YY >=0) and  (YY <= Self.MaxHeight) then
    begin
      DstPtr := Self.GetScanLine(YY);
      U := SrcStartX;
      For X := 0 to W do
      begin
        XX := StartX + X;
        if (XX >= 0) and (XX <= Self.MaxWidth) then
        begin
          if X = 0 then Inc(DstPtr,StartX);
          px := Clamp(Trunc(U),0, Source.MaxWidth);


          MergeColor := PBZColor(SrcPtr + px)^;
          if (aAlphaMode<>amOpaque) and (aDrawMode = dmSet) then InColor := DstPtr^;
          if (ADrawMode = dmCombine) then
          begin
            InColor := DstPtr^;
            SrcColor := MergeColor;
            //MergeColor := InColor.Combine(SrcColor,CombineMode);
            MergeColor := SrcColor.Combine(InColor,CombineMode);
          end;

          Case aAlphaMode of
            amNone, amOpaque : OutColor := MergeColor;
            amAlpha :
            begin
              If (MergeColor.Alpha > 0) then OutColor := InColor.AlphaBlend(MergeColor) else OutColor := SrcColor;
            end;
            amAlphaBlend :
            Begin
              If (MergeColor.Alpha > 0) then OutColor := InColor.Blend(MergeColor,MasterAlpha); // else OutColor := SrcColor;
            end;
            amAlphaBlendHQ :
            Begin
              If (MergeColor.Alpha > 0) then OutColor := InColor.BlendHQ(SrcColor,MergeColor,MasterAlpha) else OutColor := SrcColor;
            end;
          end;
          If (MergeColor.Alpha > 0) then DstPtr^:= OutColor;
          Inc(DstPtr);
          //U := U + Dx;
        end;
        U := U + Dx;
      end;
    end;
    V := V + Dy
  end;
end;

procedure TBZCustomBitmap.PutImageStretch(Source : TBZCustomBitmap; DestLeft, DestTop, DestRight, DestBottom : Integer; MasterAlpha : Byte);
begin
  Self.PutImageStretch(Source, DestLeft, DestTop, DestRight, DestBottom, dmSet, amAlphaBlend, MasterAlpha)
end;

procedure TBZCustomBitmap.PutImageStretch(Source : TBZCustomBitmap; DestRect : TBZRect;  MasterAlpha : Byte);
begin
   Self.PutImageStretch(Source, DestRect.Left, DestRect.Top, DestRect.Right, DestRect.Bottom,MasterAlpha);
end;

procedure TBZCustomBitmap.PutImageRotateAndScale(Source : TBZCustomBitmap; DstX, DstY : Integer; Angle : Single; ZoomFactor : Single; xOrg, yOrg : Integer; const MasterAlpha : Byte; const Warp : Boolean);
Var
  x,y : Integer;

  Delta,Theta, duCol,duRow, dvcol, dvRow, uStart, vStart,uRow, vRow, u,v : Single;
  DstPtr : PBZColor;
  InColor, OutColor : TBZColor;
begin
  Delta := 1.0 / ZoomFactor;
  Theta := DegToRadian(Angle);
  duCol := Sin(Theta) * Delta;
  dvCol := Cos(Theta) * Delta;
  duRow := dvCol;
  dvRow := -duCol;

  uStart := xOrg - ((DstX * dvCol) + (DstY * duCol));
  vStart := yOrg - ((DstX * dvRow) + (DstY * duRow));

  uRow := uStart;
  vRow:= vStart;

  for y := 0 to Self.MaxHeight do
  begin
    DstPtr := Self.GetScanLine(y);
    u := uRow;
    v := vRow;
    for x := 0 to Self.MaxWidth do
    begin
      if not(Warp) then
      begin
        if (u>=0) and (v>=0) and (u<=Source.MaxWidth) and (v<=Source.MaxHeight) then
        begin
          OutColor := Source.getPixel(Round(u),Round(v));
          if (MasterAlpha<255) then
          begin
            if (MasterAlpha>0) then
            begin
              if OutColor.Alpha>0 then
              begin
                InColor := DstPtr^;
                DstPtr^ := InColor.Blend(OutColor, MasterAlpha);
              end;
            end;
          end
          else if OutColor.Alpha>0 then DstPtr^ := OutColor;
          inc(DstPtr);
        end
        else
        begin
          inc(DstPtr);
        end;
      end
      else
      begin
        OutColor := Source.getPixel(Round(Abs(u)) mod Source.MaxWidth,Round(Abs(v)) mod Source.MaxHeight);
        if (MasterAlpha<255) then
        begin
          if (MasterAlpha>0) then
          begin
            InColor := DstPtr^;
            DstPtr^ := InColor.Blend(OutColor, MasterAlpha);
          end;
        end
        else DstPtr^ := OutColor;
        inc(DstPtr);
      end;
      u := u + duRow;
      v := v + dvRow;
    end;
    uRow := uRow + duCol;
    vRow := vRow + dvCol;
  end;
end;

procedure TBZCustomBitmap.CopyHorizontalBand(Source : TBZCustomBitmap; SrcY, aHeight, destY : Integer);
Var
  SrcPtr, DstPtr: PBZColor;
  I, MaxSourceH, MaxDestH, MaxH, d1, d2, maxLineSize : Longint;
begin
  if (aHeight <= 0) or (SrcY<0) or (SrcY>Source.MaxHeight) or (DestY<0) or (DestY>Self.MaxHeight) then exit;

  MaxLineSize := Source.Width shl 2;

  MaxSourceH := SrcY+aHeight;
  if (MaxSourceH > Source.MaxHeight) then MaxSourceH := Source.MaxHeight;

  MaxDestH := DestY+aHeight;
  if (MaxDestH > Self.MaxHeight) then MaxDestH := Self.MaxHeight;

  d1 := (MaxSourceH - SrcY);
  d2 := (MaxDestH - DestY);
  MaxH := MaxSourceH;
  if (d1 > d2)  then MaxH := SrcY + d2 else MaxH := SrcY + d1;

  SrcPtr := Source.GetScanLine(SrcY);
  DstPtr := Self.GetScanLine(DestY);
  I := SrcY;
  While not(I > MaxH) Do
  Begin
    Move(SrcPtr^, DstPtr^, MaxLineSize);
    Inc(SrcPtr,Source.Width);
    Inc(DstPtr,Self.Width);
    Inc(I);
  End;
end;

procedure TBZCustomBitmap.CopyBlock(Source : TBZCustomBitmap; SrcX, SrcY, aWidth, aHeight, DestX, destY : Integer; const CheckAlpha : Boolean);
Var
  LineSize : DWord;
  SrcPtr, DstPtr : PBZColor;
  y,yy,x,xx, LineSrcInc, LineDstInc, SrcMaxWidth, SrcMaxHeight : Integer;
  SrcCol : TBZColor;
begin
 // if not(Source.CheckPixelBound(SrcX, SrcY)) or not(Self.CheckPixelBound(DestX, DestY)) then exit;

  SrcMaxWidth  := Min(aWidth,(Self.Width - DestX));
  SrcMaxHeight := Min(aHeight,(Self.Height - DestY));

  if not(CheckAlpha) then
  begin
    LineSize := SrcMaxWidth shl 2; //*4
    SrcPtr := Source.GetPixelPtr(SrcX, SrcY);
    DstPtr := Self.GetPixelPtr(DestX, DestY);
    yy := SrcY + SrcMaxHeight - 1;
    For y := SrcY to yy do
    begin
      Move(SrcPtr^, DstPtr^,LineSize);
      Inc(SrcPtr, Source.Width);
      Inc(DstPtr, Self.Width);
    end;
  end
  else
  begin
    SrcPtr := Source.GetPixelPtr(SrcX, SrcY);
    DstPtr := Self.GetPixelPtr(DestX, DestY);
    yy := SrcY + SrcMaxHeight - 1;
    xx := SrcX + SrcMaxWidth - 1;
    LineSrcInc := Source.Width - SrcMaxWidth;
    LineDstInc := Self.Width - SrcMaxWidth;
    For y := SrcY to yy do
    begin
      For x:= SrcX to xx do
      begin
        SrcCol := SrcPtr^;
        if (SrcCol.Alpha > 0) then DstPtr^ := SrcCol;
        inc(DstPtr);
        Inc(SrcPtr);
      end;
      inc(DstPtr, LineDstInc);
      Inc(SrcPtr, LineSrcInc);
    end;
  end;
end;

procedure TBZCustomBitmap.CopyBlock(Source : TBZCustomBitmap; SrcRect : TBZRect; DestX, destY : Integer; const CheckAlpha : Boolean);
begin
  Self.CopyBlock(Source, SrcRect.Left, SrcRect.Top, SrcRect.Width, SrcRect.Height, DestX, DestY, CheckAlpha);
end;

procedure TBZCustomBitmap.ShiftLeft;
Var
  i : Integer;
  ls : DWord;
  OutColor : TBZColor;

  Temp, DstLine, SrcLine : PBZColor;
begin
  ls := FMaxWidth*4;
  for i:= 0 to FMaxHeight do
  begin
    OutColor := Self.GetPixel(0,i);
    Temp := Self.GetPixelPtr(FMaxWidth,i);
    SrcLine := Self.GetPixelPtr(1,i);
    DstLine := Self.GetScanLine(i);
    Move(SrcLine^,DstLine^, ls);
    Temp^:= OutColor;
  end;
end;

procedure TBZCustomBitmap.ShiftRight;
Var
  i : Integer;
  ls : DWord;
  OutColor : TBZColor;

  Temp, DstLine, SrcLine : PBZColor;
begin
  ls := FMaxWidth*4;
  for i:= 0 to FMaxHeight do
  begin
    OutColor := Self.GetPixel(FMaxWidth,i);
    Temp := Self.GetScanLine(i);
    SrcLine := Temp;
    DstLine := SrcLine;
    Inc(DstLine);
    Move(SrcLine^,DstLine^, ls);
    Temp^:= OutColor;
  end;
end;

procedure TBZCustomBitmap.ShiftUp;
Var
  LineTemp,DstLine, SrcLine : PBZColor;
begin
  LineTemp := nil;
  GetMem(LineTemp, FLineSizeInBytes);
  DstLine := Self.GetScanLine(0);
  Move(DstLine^,LineTemp^,DWord(FLineSizeInBytes));
  SrcLine := Self.GetScanLine(1);
  Move(SrcLine^, DstLine^, DWord((FWidth * FMaxHeight)*4));
  DstLine := Self.GetScanLine(FMaxHeight);
  Move(LineTemp^,DstLine^,DWord(FLineSizeInBytes));
  FreeMem(LineTemp);
  LineTemp := nil;
end;

procedure TBZCustomBitmap.ShiftDown;
Var
  LineTemp,DstLine, SrcLine : PBZColor;
begin
  LineTemp := nil;
  GetMem(LineTemp, FLineSizeInBytes);
  DstLine := Self.GetScanLine(FMaxHeight);
  Move(DstLine^,LineTemp^,DWord(FLineSizeInBytes));
  DstLine := Self.GetScanLine(1);
  SrcLine := Self.GetScanLine(0);
  Move(SrcLine^, DstLine^, DWord((FWidth * FMaxHeight)*4));
  DstLine := Self.GetScanLine(0);
  Move(LineTemp^,DstLine^,DWord(FLineSizeInBytes));
  FreeMem(LineTemp);
  LineTemp := nil;
end;

procedure TBZCustomBitmap.TakeDesktopScreenShot;
var
  TmpBmp: Graphics.TBitmap;
  ScreenDC: HDC;
begin
  ScreenDC := GetDC(0);
  try
    TmpBmp := Graphics.TBitmap.Create;
    TmpBmp.LoadFromDevice(ScreenDC);
  finally
    ReleaseDC(0, ScreenDC);
    if (TmpBmp.Width > 0) and (TmpBmp.Height > 0) then
    begin
      Self.ImportFromBitmap(TmpBmp);
    end;
    FreeAndNil(TmpBmp);
  end;
end;

procedure TBZCustomBitmap.TakeDesktopScreenShot(ARect : TBZRect);
Var
  TmpBmp : TBZCustomBitmap;
begin
  Self.TakeDesktopScreenShot();
  TmpBmp := TBZCustomBitmap.Create(ARect.Width, ARect.Height);
  TmpBmp.CopyBlock(Self,ARect,0,0);
  Self.Assign(TmpBmp);
  FreeAndNil(TmpBmp);
end;

procedure TBZCustomBitmap.DrawToCanvas(const ACanvas : TCanvas; const ARect : TRect; const IsOpaque : Boolean; const ClearBK : Boolean);
{$IFDEF WINDOWS}
  // Fast Red-Blue channel color swapping
  procedure SwapRB(Buf: PBZColor; pixelCount: Integer);
  var
    Pixptr: PBZColor;
    AIntColor : Cardinal;
  begin
    PixPtr := Buf;
    while pixelCount > 0 do
    begin
      AIntColor := PixPtr^.AsInteger;
      PixPtr^.AsInteger := AIntColor and $FF00FF00 or (AintColor and $000000FF SHL 16) or (AIntColor and $00FF0000 SHR 16);
      Inc(PixPtr);
      Dec(pixelCount);
    end;
  end;

//https://msdn.microsoft.com/en-us/library/windows/desktop/dd183351(v=vs.85).aspx
//Const
//  SHADEBLENDCAPS = 120;  //WinGDI
Var
  BitsInfo: TBitmapInfo;
  //RW,RH : Integer;
  hBmp: HBitmap;
  memDC: HDC;
  // hOld: HGDIOBJ;
  BitsPtr: Pointer;
  //PixPtr:PBZColor;
  DataSize: Integer;
  bf: TBlendFunction;
  W, H : Longint;
Begin
  //assert(assigned(Self),'Source Bitmap ABMP must be assigned');
  If Self.Size <= 0 Then exit;

  //TmpBmp := TBZBitmap.Create(1,1);
  //TmpBmp.Assign(ABmp);
  W := Self.Width;
  H := Self.Height;

  ZeroMemory(@BitsInfo, sizeof(BITMAPINFO));
  With BitsInfo.bmiHeader Do
  Begin
    biSize := SizeOf(TBitmapInfoHeader);
    biWidth := W;
    biHeight := -H; { Note: Une hauteur negative signifie que la lecture d'une ligne se fait de bas en haut }
    biPlanes := 1;
    biBitCount := 32; //On Affiche en 32bits (normalent prendre donnée depuis "Device"
    biCompression := BI_RGB;
    biSizeImage := Self.Size; //(TmpBmp.Width * TmpBmp.Height) * 4;// Pf32
  End;


  ACanvas.Lock;
  If ClearBk Then ACanvas.Clear;

  // msdn : Device does not support any of these capabilities.
  //    if  (GetDeviceCaps(Canvas.Handle,SHADEBLENDCAPS)<> 0) and ((ABmp.ImageDescription.Description.HasAlpha=true) or not(opaque)) then
  If (Not (IsOpaque)) Then
  Begin
    // Canvas.Clear;  // Il faut effacer le canvas sinon il y a des artefacts
    BitsPtr := nil;
    DataSize := BitsInfo.bmiHeader.biSizeImage;

    memDC := CreateCompatibleDC(ACanvas.Handle);
    hBmp := Windows.CreateDIBSection(ACanvas.Handle, BitsInfo, DIB_RGB_COLORS, BitsPtr, 0, 0);

    SelectObject(memDC, hBMp);
   // rgb
   // SwapRB(Tmpbmp.GetSurfaceBuffer,Tmpbmp.MaxSize);

    if Self.ImageDescription.BitsPerPixel = 32 then Self.PremultiplyAlpha;
    //Move(Self.getScanline(0)^, BitsPtr^, DataSize);
    Move(Self.getSurfaceBuffer^, BitsPtr^, DataSize);

    With bf Do
    Begin
      BlendOp := AC_SRC_OVER;
      BlendFlags := 0;
      SourceConstantAlpha := $FF;
      AlphaFormat := AC_SRC_ALPHA;
    End;

    Win32Extra.AlphaBlend(ACanvas.Handle, ARect.Left, ARect.Top, W, H, memDC, 0, 0, W, H, bf);
  End
  Else
  Begin
    //SwapRB(Tmpbmp.GetSurfaceBuffer,Tmpbmp.MaxSize);// Convert RGBA bitmap to BGRA Bitmap for output
    //Windows.StretchDIBits(Canvas.Handle, R.Left, R.Top, TmpBmp.Width, TmpBmp.Height,
    //  0, 0, TmpBmp.Width, aBmp.Height,
    //  PByte(TmpBmp.GetScanLine(0)), BitsInfo,
    //  DIB_RGB_COLORS, SRCCOPY);
    if Self.ImageDescription.BitsPerPixel = 32 then Self.PremultiplyAlpha;
    Windows.StretchDIBits(ACanvas.Handle, ARect.Left, ARect.Top, W, H,
      0, 0, W, H,
      PByte(Self.getSurfaceBuffer), BitsInfo,        //aBmp.GetScanLine(0)
      DIB_RGB_COLORS, SRCCOPY);
  End;
  //FreeAndNil(TmpBmp);
  ACanvas.Unlock;
End;

{$ENDIF}

{$IFDEF LCLGTK2}
Var
  TmpBmp : Graphics.TBitmap;
Begin
  if IsOpaque then
  begin
    gdk_draw_rgb_32_image(TGTKDeviceContext(ACanvas.Handle).Drawable,
                        TGTKDeviceContext(ACanvas.Handle).GC, ARect.Left, ARect.Top, Self.Width, Self.Height, //(R.Right-R.Left), (R.Bottom-R.Top), //???
                        GDK_RGB_DITHER_NORMAL,
                        PByte(Self.getSurfaceBuffer),Self.Width*4);  //Self.Size
  end
  else
  begin
    Try
      TmpBmp := Self.ExportToBitmap;
      ACanvas.Lock;
      ACanvas.StretchDraw(ARect,TmpBmp);
      ACanvas.UnLock;
    finally
      TmpBmp.Free;
    end;
  end;
End;
{$ENDIF}

{$IFDEF LCLQT}
Var
  QtImage: TQtImage;
  QtContext: TQtDeviceContext;
  TmpBmp : Graphics.TBitmap;
 // Rect: TRect;
Begin
  UpdateData;
  if IsOpaque then
  begin
    QtImage := TQtImage.Create(ImageDescription.Data,Self.Width, Self.Height, QImageFormat_ARGB32);
    //Rect := R; //Types.Rect(0, 0, FCompactImage.Width, FCompactImage.Height);
    QtContext := TQtDeviceContext(ACanvas.Handle);
    QtContext.dImageDescription(@R, QtImage.Handle, @ARect, Nil, @ARect);
    QtImage.Free;
  end
  else
  begin
    Try
      TmpBmp := Self.ExportToBitmap;
      ACanvas.Lock;
      ACanvas.StretchDraw(ARect,TmpBmp);
      ACanvas.UnLock;
    finally
      TmpBmp.Free;
    end;
  end;
End;
{$ENDIF}

{$IFDEF LCLCARBON}
Var
  Image: CGImageRef;
  BitmapReference: CGContextRef;
  TmpBmp : Graphics.TBitmap;
Begin
  UpdateData;
  if IsOpaque then
  begin
    BitmapReference := CGBitmapContextCreate(ImageDescription.Data, Self.Width, Self.Height,
                                             8, 4*Self.Width, //Self.Size
                                             CGColorSpaceCreateWithName(kCGColorSpaceGenericRGB),
                                             kCGImageAlphaPremultipliedLast);
    Image := CGBitmapContextCreateImage(BitmapReference);
   // TCarbonDeviceContext(Canvas.Handle).DrawCGImage(0, 0, FCompactImage.Width, FCompactImage.Height, Image);
    TCarbonDeviceContext(Canvas.Handle).DrawCGImage(ARect.Left, ARect.Top, (ARect.Right-ARect.Left), (ARect.Bottom-ARect.Top), Image);
    CGImageRelease(Image);
    CGContextRelease(BitmapReference);
  end
  else
  begin
    Try
      TmpBmp := Self.ExportToBitmap;
      ACanvas.Lock;
      ACanvas.StretchDraw(ARect,TmpBmp);
      ACanvas.UnLock;
    finally
      TmpBmp.Free;
    end;
  end;
End;
{$ENDIF}
(*{$ELSE}
Begin
  Canvas.Lock;
  if ClearBk then Canvas.Clear;
  Canvas.StretchDraw(R,GetImageBitmap);
  Canvas.UnLock;
  //Canvas.Draw(0,0, ImageBitmap);
  //Canvas.Draw(R.Left, R.Top, ImageBitmap);
End;
{$ENDIF} *)

//------------------------------------------------------------------------------
// Importation des données d'un TBitmap de bit indifferent et le converti en 32bit
//------------------------------------------------------------------------------
function TBZCustomBitmap.ImportFromBitmap(const ABitmap : Graphics.TBitmap) : Boolean;
var
  LTempBitmap: Graphics.TBitmap;
  ok,ResetAlpha:Boolean;
  procedure SetAlpha(Value : Byte);
  var
    i : Integer;
    PixPtr : PBZColor;
  begin
    i:=0;
    PixPtr := Self.GetScanLine(0);
    While i<Self.MaxSize do
    begin
      PixPtr^.Alpha:= Value;
      inc(PixPtr);
      inc(i);
    end;
  end;

begin
  ResetAlpha:=False;
  result:=false;
  if (ABitmap.PixelFormat <> pf32bit)  then
  begin
    LTempBitmap := Graphics.TBitmap.Create;
    try
      ResetAlpha:=True;
      LTempBitmap.PixelFormat := pf32bit;
      LTempBitmap.SetSize(ABitmap.Width, ABitmap.Height);
      LTempBitmap.Canvas.Draw(0, 0, ABitmap);
    finally
      ok:=Self.ImportFromRawImage(LTempBitmap.RawImage);
      if ResetAlpha then SetAlpha(255);
      FreeAndNil(LTempBitmap);
      result:=true and (ok);
    end;
  end
  else
  begin
   ok:=Self.ImportFromRawImage(ABitmap.RawImage);
   //if ResetAlpha then SetAlpha(255);
   result:=true and (ok);
  end;
end;

//------------------------------------------------------------------------------
// Importation des données d'un TRawImage
//------------------------------------------------------------------------------
function TBZCustomBitmap.ImportFromRawImage(const ARawImage : TRawImage) : Boolean;
var
  //BytePerRow:Integer;
  BufferData : PByte;
begin
 // Clear(ccolBlack);
  SetSize(ARawImage.Description.Width,ARawImage.Description.Height);
  result:=false;
  // On verifie si la taille des deux tampons sont identique
  // Si ce n'est pas le cas, cela veut dire que le TRawImage n'est pas au format 32bit
  if (ARawImage.DataSize= Self.Size) then
  begin
    try
      //BytePerRow := RawImage.Description.BytesPerLine;
      BufferData := PByte(Self.getSurfaceBuffer);
      Move(ARawImage.Data^, BufferData^, self.Size);
    finally
      result:=true;
      //Changed();
    end;
  end;
end;

function TBZCustomBitmap.ExportToBitmap : Graphics.TBitmap;
var
  Temp:     Graphics.TBitmap;
  RawImage: TRawImage;
  BitmapHandle, MaskHandle: HBitmap;
  W,H : Integer;
  Buffer : PByte;
Begin
  result := nil;
  BitmapHandle := 0;
  MaskHandle := 0;
  w := Self.Width;
  h:=  Self.Height;
  Buffer := PByte(GetSurfaceBuffer);

  RawImage.Init;
  RawImage.Description.Init_BPP32_B8G8R8A8_BIO_TTB(W,H); //Init_BPP32_R8G8B8A8_BIO_TTB(W,H);
  RawImage.Data := Buffer;
  RawImage.DataSize:= Self.Size;

  if not RawImage_CreateBitmaps(RawImage, BitmapHandle, MaskHandle, False) then
  begin
    raise EBZBitmapException.Create('Failed to create bitmap handle');
  end
  else
  begin
   Temp := Graphics.TBitmap.Create;
   //Temp.PixelFormat := pf32bit;
   //Temp.SetSize(W,H);
   Temp.Handle := BitmapHandle;
   Temp.MaskHandle := MaskHandle;
   Result := Temp;
  end;
  if (Result=nil) then Raise EBZBitmapException.Create('Erreur de conversion vers TBitmap');
End;

procedure TBZCustomBitmap.HLine(x, y, x1 : Integer; aColor : TBZColor);
Var
  PixPtr : PBZColor;
begin
  if x>x1 then Swap(x,x1);
  PixPtr := GetScanLine(y);
  Inc(PixPtr, x);
  FillLongWord(PixPtr^, (x1 - x) , DWord(aColor));
end;

procedure TBZCustomBitmap.VLine(x, y1, y2 : Integer; aColor : TBZColor);
Var
  PixPtr : PBZColor;
  y : Integer;
begin
  if y1>y2 then swap(y1,y2);
  PixPtr := GetScanLine(y1);
  Inc(PixPtr,x);
  y := y1;
  While not(y>y2) do
  begin
     PixPtr^ := aColor;
     Inc(PixPtr, Self.Width);
     Inc(y);
  end;
end;

procedure TBZCustomBitmap.Box(x1, y1, x2, y2 : Integer; aColor : TBZColor);
begin
  HLine(x1,y1,x2,aColor);
  VLine(x1,y1,y2,aColor);
  VLine(x2,y1,y2,aColor);
  HLine(x1,y2,x2,aColor);
end;

procedure TBZCustomBitmap.FillBox(x1, y1, x2, y2 : Integer; aColor : TBZColor);
Var
  PixPtr : PBZColor;
  y,w : Integer;
begin
  if x1>x2 then swap(x1,x2);
  if y1>y2 then swap(y1,y2);
  if x2 > Self.MaxWidth then x2 := Self.MaxWidth;
  if y2 > Self.MaxHeight then y2 := Self.MaxHeight;

  PixPtr := GetScanLine(y1);
  Inc(PixPtr,x1);
  y := y1;
  w := (x2 - x1);
  While (y<=y2) do
  begin
     FillLongWord(PixPtr^, w , DWord(aColor));
     Inc(PixPtr, Self.Width);
     Inc(y);
  end;
end;

procedure TBZCustomBitmap.Line(x1, y1, x2, y2 : Integer; aColor : TBZColor);
begin

end;

procedure TBZCustomBitmap.LineAA(x1, y1, x2, y2 : Integer; aColor : TBZColor);
begin

end;

{%endregion%}

{%region%====[ TBZCustomOwnerBitmap ]===========================================}

Procedure TBZCustomOwnerBitmap.SetClipping(Const Value: Boolean);
Begin
  OwnerBitmap.Clipping := Value;
End;

Function TBZCustomOwnerBitmap.GetClipping: Boolean;
Begin
  Result := OwnerBitmap.Clipping;
End;

Procedure TBZCustomOwnerBitmap.SetClipRect(Const Value: TBZRect);
Begin
  OwnerBitmap.ClipRect := Value;
End;

Function TBZCustomOwnerBitmap.GetClipRect: TBZRect;
Begin
  Result := OwnerBitmap.ClipRect;
End;

Constructor TBZCustomOwnerBitmap.Create(Const AOwner: TBZBaseBitmap);
Begin
  Inherited Create;
  OwnerBitmap := AOwner;
End;

Destructor TBZCustomOwnerBitmap.Destroy;
Begin
  OwnerBitmap := nil;
  Inherited Destroy;
End;

Procedure TBZCustomOwnerBitmap.getClippingBoundInfos(Var x1, y1, x2, y2, ClipWidth, ClipHeight, ClipRectSize, NextLineInc: Integer);
Begin
  get_ClippingBoundInfos(Clipping, ClipRect.AsRect, OwnerBitmap.MaxWidth, OwnerBitmap.MaxHeight, x1, y1, x2, y2, ClipWidth, ClipHeight, ClipRectSize, NextLineInc);
End;

{%endregion%}

{%region%====[ TBZBaseBitmap ]==================================================}

procedure TBZBaseBitmap.SetUseMask(Const Value : Boolean);
begin
  if FUseMask = Value then exit;
  FUseMask := Value;
End;

procedure TBZBaseBitmap.SetUseSelectionMask(Const Value : Boolean);
begin
  if FUseSelectionMask = Value then exit;
  FUseSelectionMask := Value;
end;

procedure TBZBaseBitmap.DoSetMaskFromBitmap(Const aBmp, aDest : TBZCustomBitmap; Const aTransparentColor : TBZColor);
Var
  i : int64;
  SrcPtr, DstPtr : PBZColor;
begin
  I:=0;
  SrcPtr := aBmp.GetSurfaceBuffer;
  DstPtr:= aDest.GetSurfaceBuffer;
  While i<=MaxSize do
  begin
    if SrcPtr^=aTransparentColor then DstPtr^:=clrTransparent else DstPtr^:=clrWhite;
    inc(SrcPtr);
    inc(DstPtr);
    Inc(I);
  End;
end;

procedure TBZBaseBitmap.DoSetMaskFromBitmapAlpha(Const aBmp, aDest : TBZCustomBitmap);
Var
  i : int64;
  SrcPtr, DstPtr : PBZColor;
begin
  I:=0;
  SrcPtr := aBmp.GetSurfaceBuffer;
  DstPtr:= aDest.GetSurfaceBuffer;
  While i<=MaxSize do
  begin
    if SrcPtr^.Alpha>0 then DstPtr^:=clrWhite else DstPtr^:=clrTransparent;
    inc(SrcPtr);
    inc(DstPtr);
    Inc(I);
  End;
end;

procedure TBZBaseBitmap.DoSetMaskFromBitmapIntensity(Const aBmp, aDest : TBZCustomBitmap; Const aThresHold : Byte);
Var
  i : int64;
  Intensity : Byte;
  SrcPtr, DstPtr : PBZColor;
  AColor : TBZColor;
begin
  I:=0;
  SrcPtr := aBmp.GetSurfaceBuffer;
  DstPtr:= aDest.GetSurfaceBuffer;
  While i<=MaxSize do
  begin
    AColor := SrcPtr^;
    Intensity := (AColor.Red + AColor.Green + AColor.Blue) div 3;
    if Intensity>aThresHold then DstPtr^:=clrWhite else DstPtr^:=clrTransparent;
    inc(SrcPtr);
    inc(DstPtr);
    Inc(I);
  End;
end;

Constructor TBZBaseBitmap.Create;
begin
  Inherited Create(nil,0,0);
  FMaskApplyAlpha:=False;
  FMaskApply :=False;
  FUseMask := False;
  FUseSelectionMask := False;
end;

procedure TBZBaseBitmap.Assign(Source : TPersistent);
Begin
  Inherited Assign(Source);
  if Source is TBZBaseBitmap then
  begin
    FMask := TBZBaseBitmap(Source).Mask;
    FUseMask := TBZBaseBitmap(Source).UseMask;
    FMaskApplyAlpha := TBZBaseBitmap(Source).ApplyMask;
    FMaskApply := TBZBaseBitmap(Source).ApplyMaskAlpha;
    FSelectionMask := TBZBaseBitmap(Source).SelectionMask;
    FUseSelectionMask := TBZBaseBitmap(Source).UseSelectionMask;
  End;
End;


function TBZBaseBitmap.CreateClone : TBZBaseBitmap;
Var
  Clone : TBZBaseBitmap;
Begin
  Result:=nil;
  Try
    Clone := TBZBaseBitmap.Create(0,0);
    Clone.Assign(self);
  Finally
    Result:=Clone;
  End;
end;

procedure TBZBaseBitmap.SetMaskFromBitmap(Const aBmp : TBZCustomBitmap; Const aTransparentColor : TBZColor);
begin
  DoSetMaskFromBitmap(aBmp, FMask, aTransparentColor);
end;

procedure TBZBaseBitmap.SetMaskFromBitmapAlpha(Const aBmp : TBZCustomBitmap);
begin
  DoSetMaskFromBitmapAlpha(aBmp, FMask);
end;

procedure TBZBaseBitmap.SetMaskFromBitmapIntensity(Const aBmp : TBZCustomBitmap; Const aThresHold : Byte);
begin
  DoSetMaskFromBitmapIntensity(aBmp, FMask, aThresHold);
end;

procedure TBZBaseBitmap.SetSelectionMaskFromBitmap(Const aBmp : TBZCustomBitmap; Const aTransparentColor : TBZColor);
begin
  DoSetMaskFromBitmap(aBmp, FSelectionMask, aTransparentColor);
end;

procedure TBZBaseBitmap.SetSelectionMaskFromBitmapAlpha(Const aBmp : TBZCustomBitmap);
begin
  DoSetMaskFromBitmapAlpha(aBmp, FSelectionMask);
end;

procedure TBZBaseBitmap.SetSelectionMaskFromBitmapIntensity(Const aBmp : TBZCustomBitmap; Const aThresHold : Byte);
begin
  DoSetMaskFromBitmapIntensity(aBmp, FSelectionMask, aThresHold);
end;

function TBZBaseBitmap.GetSelectionMask : TBZCustomBitmap;
begin
  Result := FSelectionMask;
end;

procedure TBZBaseBitmap.SetMask(const AValue : TBZCustomBitmap);
begin
  FMask := AValue;
end;

procedure TBZBaseBitmap.SetSelectionMask(const AValue : TBZCustomBitmap);
begin
  FSelectionMask := AValue;
end;

function TBZBaseBitmap.GetMask : TBZCustomBitmap;
begin
  Result := nil;
  if FMask<>nil then Result := TBZCustomBitmap(FMask);
end;

{%endregion%}

{%region%====[ Support BZImageFileFormats ]=====================================}

Function GetBZImageFileFormats: TBZImageFileFormatsList;
Begin
  If Not Assigned(vBZImageFileFormats) Then
    vBZImageFileFormats := TBZImageFileFormatsList.Create;
  Result := vBZImageFileFormats;
End;

Procedure RegisterRasterFormat(Const AExtension, ADescription: String; AClass: TBZBitmapClass);
Begin
  Classes.RegisterClass(AClass);
  GetBZImageFileFormats.Add(AExtension, ADescription, 0, AClass);
End;

Procedure UnregisterRasterFormat(AClass: TBZBitmapClass);
Begin
  If Assigned(vBZImageFileFormats) Then vBZImageFileFormats.Remove(AClass);
End;

Function BZImageFileFormatExtensionByIndex(index: Integer): String;
Begin
  Result := GetBZImageFileFormats.FindExtByIndex(index);
End;

Destructor TBZImageFileFormatsList.Destroy;
Begin
  Clean;
  Inherited;
End;

Procedure TBZImageFileFormatsList.Add(Const Ext, Desc: String; DescID: Integer; AClass: TBZBitmapClass);
Var
  newRec: TBZImageFileFormat;
Begin
  newRec := TBZImageFileFormat.Create;
  With newRec Do
  Begin
    Extension := AnsiLowerCase(Ext);
    BaseImageClass := AClass;
    Description := Desc;
    DescResID := DescID;
  End;
  Inherited Add(newRec);
End;

Function TBZImageFileFormatsList.FindExt(ext: String): TBZBitmapClass;
Var
  i: Integer;
Begin
  ext := LowerCase(ext);
  For i := Count - 1 Downto 0 Do
    With TBZImageFileFormat(Items[I]) Do
    Begin
      If Extension = ext Then
      Begin
        Result := BaseImageClass;
        Exit;
      End;
    End;
  Result := nil;
End;

Function TBZImageFileFormatsList.FindFromFileName(Const fileName: String): TBZBitmapClass;
Var
  ext: String;
Begin
  ext := ExtractFileExt(Filename);
  System.Delete(ext, 1, 1);
  Result := FindExt(ext);
  If Not Assigned(Result) Then
    Raise EBZInvalidImageFormatException.CreateFmt('Format de fichier %s non supporté ', [UpperCase(ext)]);

End;

Function TBZImageFileFormatsList.FindFromStream(Const AStream: TStream): TBZBitmapClass;
Var
  ext:   String;
  magic: Array[0..1] Of Longword;
Begin
  magic[0] := 0;
  magic[1] := 1;
  AStream.ReadBuffer(magic, 2 * SizeOf(Longword));
  AStream.Seek(-2 * SizeOf(Longword), 1);
  ext :='';

  If magic[0] = $20534444 Then
    ext := 'DDS'
  Else If magic[1] = $4354334F Then
    ext := 'O3TC'
  Else If (magic[0] And $0000FFFF) = $00003F23 Then
    ext := 'HDR'
  Else If (magic[0] = $474E5089) And (magic[1] = $0A1A0A0D) Then
    ext := 'PNG'
  Else If (magic[0] = $E0FFD8FF) And (magic[1] = $464A1000) Then
    ext := 'JPG';

  Result := FindExt(ext);
  if not Assigned(Result) then
     raise EBZInvalidImageFormatException.CreateFmt('Extension de fichier inconnu : ',[ext, ' Extension = ' + UpperCase(ext)]);
End;

Procedure TBZImageFileFormatsList.Remove(AClass: TBZBitmapClass);
Var
  i: Integer;
Begin
  For i := Count - 1 Downto 0 Do
  Begin
    If TBZImageFileFormat(Items[i]).BaseImageClass.InheritsFrom(AClass) Then
      DeleteAndFree(i);
  End;
End;

Procedure TBZImageFileFormatsList.BuildFilterStrings(imageFileClass: TBZBitmapClass; Var descriptions, filters: String;
  formatsThatCanBeOpened: Boolean = True; formatsThatCanBeSaved: Boolean = False);
Var
  k, i: Integer;
  p:    TBZImageFileFormat;
Begin
  descriptions := '';
  filters := '';
  k := 0;
  For i := 0 To Count - 1 Do
  Begin
    p := TBZImageFileFormat(Items[i]);
    If p.BaseImageClass.InheritsFrom(imageFileClass) And (p.Extension <> '') And
      ((formatsThatCanBeOpened And (dfcRead In p.BaseImageClass.Capabilities)) Or (formatsThatCanBeSaved And
      (dfcWrite In p.BaseImageClass.Capabilities))) Then
    Begin
      With p Do
      Begin
        If k <> 0 Then
        Begin
          descriptions := descriptions + '|';
          filters := filters + ';';
        End;
        If (Description = '') And (DescResID <> 0) Then
          Description := LoadStr(DescResID);
        FmtStr(descriptions, '%s%s (*.%s)|*.%2:s', [descriptions, Description, Extension]);
        filters := filters + '*.' + Extension;
        Inc(k);
      End;
    End;
  End;
  If (k > 1) And (Not formatsThatCanBeSaved) Then
    FmtStr(descriptions, '%s (%s)|%1:s|%s', ['Tous les formats', filters, descriptions]);
End;

function TBZImageFileFormatsList.BuildDialogFilters:String;
Var
   i: Integer;
  p:    TBZImageFileFormat;
  Descriptions, Filters : String;
Begin
  descriptions := '';
  filters := '';
 // k := 0;
  For i := 0 To Count - 1 Do
  Begin
    p := TBZImageFileFormat(Items[i]);
    With p Do
    Begin
     // If k <> 0 Then
     // Begin
        descriptions := descriptions + '|';
        filters := filters + ';';
     // End;
      FmtStr(descriptions, '%s%s (*.%s)|*.%2:s', [descriptions, Description, Extension]);
      filters := filters + '*.' + Extension;
      //Inc(k);
    End;
  End;
  FmtStr(descriptions, '%s (%s)|%1:s|%s', ['Tous les formats', filters, descriptions]);

  Result := descriptions;
End;

function TBZImageFileFormatsList.BuildFileFilterMask:String;
Var
   i: Integer;
  p:    TBZImageFileFormat;
  Filters : String;
Begin

  filters := '';
  For i := 0 To Count - 1 Do
  Begin
    p := TBZImageFileFormat(Items[i]);
    With p Do
    Begin
      if i>0 then filters := filters + ';';
      filters := filters + '*.' + Extension;
    End;
  End;
  Result := Filters;
End;


Function TBZImageFileFormatsList.FindExtByIndex(index: Integer; formatsThatCanBeOpened: Boolean = True; formatsThatCanBeSaved: Boolean = False): String;
Var
  i: Integer;
  p: TBZImageFileFormat;
Begin
  Result := '';
  If index > 0 Then
  Begin
    For i := 0 To Count - 1 Do
    Begin
      p := TBZImageFileFormat(Items[i]);
      If (formatsThatCanBeOpened And (dfcRead In p.BaseImageClass.Capabilities)) Or (formatsThatCanBeSaved And
        (dfcWrite In p.BaseImageClass.Capabilities)) Then
      Begin
        If index = 1 Then
        Begin
          Result := p.Extension;
          Break;
        End
        Else
          Dec(index);
      End;
    End;
  End;
End;

{%endregion%}



//===================================================================================================

Initialization

  RegisterClasses([TBZCustomBitmap,TBZRectItemProperty, TBZBitmapLayerItem, TBZBitmapLayerList]);

  ComputePreMulAlphaLUT;

Finalization

  if Assigned(vBZImageFileFormats) then FreeAndNil(vBZImageFileFormats);

  UnRegisterClasses([TBZCustomBitmap, TBZRectItemProperty, TBZBitmapLayerItem, TBZBitmapLayerList]);
//==============================================================================
End.
