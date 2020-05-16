(*
  @abstract(Définit et regroupe les systèmes de couleurs les types de filtres
  et autres données utiles à la gestion du graphisme. @br
  Contient les classes de base à la gestion des informations sur les formats d'image.)

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

  @bold(Dependances) : BZClasses, BZMath, BZVectorMath, BZAnimationTool

  --------------------------------------------------------------------------------

  @bold(Credits :)
   @unorderedList(
     @item ()
     @item(J.Delauney (BeanzMaster))
   )

  --------------------------------------------------------------------------------

  @bold(LICENCE) : MPL / GPL

  -------------------------------------------------------------------------------- *)
unit BZColors;

//==============================================================================
{$mode objfpc}{$H+}
{$i ..\bzscene_options.inc}


//----------------------- DATA ALIGNMENT ---------------------------------------
{$ALIGN 16}
{$CODEALIGN CONSTMIN=16}
{$CODEALIGN LOCALMIN=16}
{$CODEALIGN VARMIN=16}

//------------------------------------------------------------------------------

{$MODESWITCH ADVANCEDRECORDS}
//-----------------------------

//==============================================================================

interface

uses
  Classes, SysUtils, Math, GraphType, Graphics, fpImage,
  BZClasses, BZMath, BZVectorMath, BZAnimationTool;

Const
  { Rapport Byte/Float d'une couleur }
  _FloatColorRatio : single = 1/255;
  { valeur achromatique de la teinte }
  _HueACH  : byte   = 160;
  { valeur gamma par defaut pour la correction }
  _DefaultGammaFactor   = 2.2;
  { facteur gamma utilisé pour la correction  }
  _GammaExpFactor   = 1.7;

Const
  { Constantes utiles pour le calcul sur les masques de couleur }
  {$IFDEF WINDOWS}
    cBlueOrder = 0;
    cGreenOrder = 1;
    cRedOrder = 2;
    cAlphaOrder = 3;
  {$ELSE}
    cRedOrder = 0;
    cGreenOrder = 1;
    cBlueOrder = 2;
    cAlphaOrder = 3;
  {$ENDIF}

  { Décalage de la composante rouge }
  cRedShift = cRedOrder * 8;
  { Décalage de la composante verte }
  cGreenShift = cGreenOrder * 8;
  { Décalage de la composante bleue }
  cBlueShift = cBlueOrder * 8;
  { Décalage de la composante alpha }
  cAlphaShift = cAlphaOrder * 8;

  { Valeur du masque rouge }
  maskRed = 1;
  { Valeur du masque vert }
  maskGreen = 2;
  { Valeur du masque bleue }
  maskBlue = 4;
  { Valeur du masque alpha }
  maskAlpha = 8;

  { Masque RBG }
  maskRGB = maskRed Or maskGreen Or maskBlue;
  { Masque RBGA }
  maskRGBA = maskRGB Or maskAlpha;


Type
  { @abstract(Facteurs de fusion source et destination pour les fonctions de dessin avec fusion.) @br
    Formule : SrcColor * SrcFactor + DestColor * DestFactor @br
    cf : https://fr.wikipedia.org/wiki/Alpha_blending  }
  TBZBlendingFactor = (
    bfIgnore,           //< Don't care
    bfZero,             //< For Src and Dest, Factor = (0, 0, 0, 0)
    bfOne,              //< For Src and Dest, Factor = (1, 1, 1, 1)
    bfSrcAlpha,         //< For Src and Dest, Factor = (Src.A, Src.A, Src.A, Src.A)
    bfOneMinusSrcAlpha, //< For Src and Dest, Factor = (1 - Src.A, 1 - Src.A, 1 - Src.A, 1 - Src.A)
    bfDstAlpha,         //< For Src and Dest, Factor = (Dest.A, Dest.A, Dest.A, Dest.A)
    bfOneMinusDstAlpha, //< For Src and Dest, Factor = (1 - Dest.A, 1 - Dest.A, 1 - Dest.A, 1 - Dest.A)
    bfSrcColor,         //< For Dest,         Factor = (Src.R, Src.R, Src.B, Src.A)
    bfOneMinusSrcColor, //< For Dest,         Factor = (1 - Src.R, 1 - Src.G, 1 - Src.B, 1 - Src.A)
    bfDstColor,         //< For Src,          Factor = (Dest.R, Dest.G, Dest.B, Dest.A)
    bfOneMinusDstColor  //< For Src,          Factor = (1 - Dest.R, 1 - Dest.G, 1 - Dest.B, 1 - Dest.A)
  );

  { @abstract(Définition des mode de fusion.) @br
    Certains modes de fusion ont été créés à partir des descriptions d'Adobe Photoshop et de Gimp. @br   
    D'autres viennent de Jens Gruschel & Francesco Savastano (et d'autres) de diverses discussions et de groupes de discussion ...}
  TBZColorCombineMode = (cmNormal, cmAdd, cmSub, cmRealSub, cmMul, cmDiv, cmOr, cmXor, cmAnd, cmDifference, cmAverage,
                         cmOverlay, cmScreen, cmStamp, cmHeat, cmFreeze, cmGlow, cmReflect, cmExclusion, cmNegate, cmLightenOnly, cmDarkenOnly,
                         cmColorBurn, cmSoftColorBurn, cmInverseColorBurn, cmColorDodge, cmSoftColorDodge, cmInverseColorDodge,
                         cmInterpolation, cmHardLight, cmSoftLight, cmBrightLight, cmLinearLight, cmVividLight, cmPinLight, cmHardMix,
                         cmHue, cmColor, cmValue, cmSaturation, cmLuminosity, cmPhoenix, cmGrainMerge, cmGrainExtract, cmRed, cmGreen, cmBlue,
                         cmDissolve, cmErase, cmMin, cmMax, cmAmplitude, cmCustomize);

{%region%=====[ Définitions des formats de couleur standards ]=======================}

Type
  { Enumeration des composantes de couleur RGBA }
  TBZColorMaskComponent = (cmcRed, cmcGreen, cmcBlue, cmcAlpha);

  { Tableau pour la représentation d'une couleur en 24 bits }
  TBZColor24Type = packed array[0..2] of byte;
  { Tableau pour la représentation d'une couleur en 32 bits }
  TBZColor32Type = packed array[0..3] of byte;
  { Definition d'une couleur 128 bits en virgule flottante }
  TBZColorVectorType = packed array[0..3] of Single;
  { Definition d'une couleur 96 bits en virgule flottante }
  TBZColorVectorType3f = packed array[0..2] of Single;

  { @abstract(Définition d'une matrice 3x3 de conversion entre espace de couleur. @br
    cf Fonctions mathematiques de conversion des espaces de couleur http://brucelindbloom.com/index.html?UPLab.html.)

    Plus d'informations  :
    @unorderedlist(
      @item(http://brucelindbloom.com/index.html?WorkingSpaceInfo.html)
      @item(http://brucelindbloom.com/index.html?Eqn_RGB_XYZ_Matrix.html)
      @item(http://brucelindbloom.com/index.html?Eqn_ChromAdapt.html)
      @item(https://en.wikipedia.org/wiki/Color_appearance_model)
      @item(https://en.wikipedia.org/wiki/LMS_color_space)
      @item(https://en.wikipedia.org/wiki/YUV)
      @item(http://discoverybiz.net/enu0/faq/faq_YUV_YCbCr_YPbPr.html)
      @item(https://ciechanow.ski/color-spaces/)
    )

    A utiliser avec TBZColorVector et la methode ConvertColorSpace }
  TBZColorSpaceMatrixConversion = array[0..8] of single;

  {Enumération des différents espaces de couleur}
  TBZColorSpace = (csRGBA, csBGRA,
                   csHSL, csHSV,
                   csCIExyz, csLab, csLCH,
                   csCMYK);
  //csYUV, csRGB,  csHSLA,  csCMY,

  { Matrice de conversion des différents espaces de couleur vers leurs sous-expaces. @br
    @bold(Note) : tA=H, tB=S, tC=L ou V @br
    cf : https://en.wikipedia.org/wiki/Wikipedia:WikiProject_Color/Normalized_Color_Coordinates }
  TBZColorSubSpace = record
    tA, tB, tC : Single;
    CS : TBZColorSpace;
  end;

  { Enumération des sous espaces de couleur pour l'espace de couleur HSL (HLS) }
  THSLColorSubSpaceType = (hslNormalize, hslDefault, hslPaintShopPro, hslWindows );
  { Enumération des sous espaces de couleur pour l'espace de couleur HSV }
  THSVColorSubSpaceType = (hsvNormalize, hsvDefault, hsvOpenCV, hsvGimp, hsvPhotopshop, hsvLinuxKDE, hsvGTK, hsvJavaAWT, hsvApple);

  { Définition des type de dégradé }
  TBZGradientKind = (gkHorizontal, gkVertical, gkFreeAngle, gkFromTopLeft, gkFromTopRight, gkRadial, gkReflectHorz, gkReflectVert, gkPyramid, gkPolar);
  //gkRadialEx--> can choose center,

  { Format RGB 15 bits }
  TBZColorRGB15_555 = packed record
    Red: 0..31;   //< Composante Rouge 5 bits
    Green: 0..31; //< Composante Vert 5 bits
    Blue: 0..31;  //< Composante Bleu 5 bits
  end;

  { Format BGR 16 bits }
  TBZColorBGR16_565 = packed record
    Blue: 0..31;  //< 5 bits
    Green: 0..63; //< 6 bits
    Red: 0..31;   //< 5 bits
  end;
  PBZColorBGR16_565 = ^TBZColorBGR16_565;

  { Format RGB 16 bits }
  TBZColorRGB16_565 = packed record
    Red: 0..31;   // 5 bits
    Green: 0..63; // 6 bits
    Blue: 0..31;  // 5 bits
  end;
  PBZColorRGB16_565 = ^TBZColorRGB16_565;

  { Format XRGB 16 bits }
  TBZColorXRGB16_1555 = packed record
    x: 0..0;      // 1 bits
    Red: 0..31;   // 5 bits
    Green: 0..31; // 5 bits
    Blue: 0..31;  // 5 bits
  end;
  PBZColorXRGB16_1555 = ^TBZColorXRGB16_1555;

  { Format XBGR 16 bits }
  TBZColorXBGR16_1555 = packed record
    x: 0..0;      // 1 bits
    Blue: 0..31;  // 5 bits
    Green: 0..31; // 5 bits
    Red: 0..31;   // 5 bits
  end;
  PBZColorXBGR16_1555 = ^TBZColorXBGR16_1555;

  {  Format couleur BGR 24 Bits }
  TBZColorBGR_24 = packed record
    Case Integer of
     0 : (V:TBZColor24Type);        //< Array access
     1 : (Blue, Green, Red : Byte);
  End;
  PBZColorBGR_24 = ^TBZColorBGR_24;

  { Format couleur RGB 24 bits }
  TBZColor24 =  record
    function ToBGR : TBZColorBGR_24;
    Case Integer of
     0 : (V:TBZColor24Type);        //< Array access
     1 : (X, Y, Z : Byte);          //< Legacy vector access
     2 : (Red, Green, Blue:Byte);   //< As Color component
  end;
  PBZColor24 = ^TBZColor24;
  TBZColorRGB_24 = TBZColor24;

  //TBZColor24Helper = record helper for TBZColor24
  //  function ToString : String;
  //  function ToBGR : TBZColorBGR_24;
  //End;

  { Enregisterement pour la représentation d'une couleur au format RGB 48 bits }
  TBZColorRGB48 = packed record
    Red,  Green, Blue : Word;
  end;

  { Enregisterement pour la représentation d'une couleur au format RGBA 64 bits }
  TBZColorRGBA64 = packed record
    Red, Green, Blue, Alpha : word;
  end;
  //PBZColor64 = ^TBZColor64;

  { Definition du format de couleur HSL }
  TBZColorHSL =  Record
    Case byte of
     0 : (Hue : Integer; Saturation : Byte; Luminosity : Byte; ColorSubSpaceType : THSLColorSubSpaceType);
  end;

  {TBZColorHSV : Definition du format de couleur HSV }
  TBZColorHSV =  Record
    Hue : Integer;
    Saturation : Byte;
    Value : Byte;
    ColorSubSpaceType : THSVColorSubSpaceType;
  end;

  { Definition du format de couleur Lab* }
  TBZColorLab = packed record
    l,a,b : Single;
  end;

  {  Definition du format de couleur LCH }
  TBZColorLCH = packed record
    luminance, c, h : Single;
  end;

  { Definition du format de couleur CMYK }
  TBZColorCMYK = packed record
    C,M,Y,K : Single;
  end;

  { Definition du format de couleur YUV }
  TBZColorYUV = packed record
    Y,U,V : Single;
  end;

  { Definition du format de couleur CIE xyz }
  TBZColorCIExyz = packed record
    x,y,z : Single;
  end;

  { Definition du format de couleur YCbCr }
  TBZColorYCbCr = packed record
    Y, Cb, Cr : Single;
  end;

  { Définition du format de couleur RGBA ou BGRA en fonction de l'OS en 32 bits}
  TBZColor32 = Record
  private
    function getColorComponent(Index : Integer): byte;
    procedure SetColorComponent(Index : Integer; aValue:Byte);
  public
    { Creation du TBZColor32 depuis les paramètres x,y,z,w. Valeur par défaut pour W = 255 }
    procedure Create(const aRed, aGreen, aBlue : Byte; const aAlpha : Byte=255); overload;
    { Creation depuis un TBZColor24. Valeur par défaut pour W = 255 }
    procedure Create(Const aValue : TBZColor24 ; const aW : Byte = 255); overload; //TBZColor24
    { Creation depuis un TBZColorBGR_24. Valeur par défaut pour W = 255 }
    procedure Create(Const aValue : TBZColorBGR_24 ; const aW : Byte = 255); overload;
    { Creation depuis un TBZVector3b  . Valeur par défaut pour W = 255 }
    procedure Create(Const aValue : TBZVector3b ; const aW : Byte = 255); overload; //TBZColor24

    { Retourne la représentation du TBZColor32 sous forme de chaine de caractères }
    function ToString : String;
    { Retourne la représentation du TBZColor32 sous forme de chaine de caractères au format hexadécimal }
    function ToHexString : String;

    { Ajoute deux TBZColor32. @br
      La valeur Alpha n'est pas pris en compte. @br
      utilisé AddWithAlpha à la place si vous souhaitez traiter la canal alpha.}
    class operator +(constref A, B: TBZColor32): TBZColor32; overload;
    { Soustrais deux TBZColor32. @br
      La valeur Alpha n'est pas pris en compte.}
    class operator -(constref A, B: TBZColor32): TBZColor32; overload;
    { Multiplie deux TBZColor32. @br
      La valeur Alpha n'est pas pris en compte.}
    class operator *(constref A, B: TBZColor32): TBZColor32; overload;
    { Divise deux TBZColor32. @br
      La valeur Alpha n'est pas pris en compte.}
    class operator Div(constref A, B: TBZColor32): TBZColor32; overload;
    { Ajoute un octet à chaque composant d'un TBZColor32. @br
      La valeur Alpha n'est pas pris en compte.}
    class operator +(constref A: TBZColor32; constref B:Byte): TBZColor32; overload;
    { Sosutrait un octet à chaque composant d'un TBZColor32.  @br
      La valeur Alpha n'est pas pris en compte.}
    class operator -(constref A: TBZColor32; constref B:Byte): TBZColor32; overload;
    { Multiplie chaque composant d'un TBZColor32 par une valeur de type Byte. @br
      La valeur Alpha n'est pas pris en compte.}
    class operator *(constref A: TBZColor32; constref B:Byte): TBZColor32; overload;
    { Multiplie chaque composant d'un TBZColor32 par une valeur en virgule flottante. @br
      La valeur Alpha n'est pas pris en compte.}
    class operator *(constref A: TBZColor32; constref B:Single): TBZColor32; overload;
    { Divise chaque composant d'un TBZColor32 par une valeur de type Byte. @br
      La valeur Alpha n'est pas pris en compte. }
    class operator Div(constref A: TBZColor32; constref B:Byte): TBZColor32; overload;
    { Compare si deux TBZColor32 sont égale. @br
      La valeur Alpha est pris en compte}
    class operator =(constref A, B: TBZColor32): Boolean;
    { Compare si deux TBZColor32 sont différent. @br
      La valeur Alpha est pris en compte}
    class operator <>(constref A, B: TBZColor32): Boolean;
    { Opérateur logique ET @bold(AND) entre deux TBZColor32. @br
      La valeur Alpha n'est pas pris en compte. }
    class operator And(constref A, B: TBZColor32): TBZColor32; overload;
    { Opérateur logique OU @bold(OR) entre deux TBZColor32. @br
      La valeur Alpha n'est pas pris en compte. }
    class operator Or(constref A, B: TBZColor32): TBZColor32; overload;
    { Opérateur logique OU Exclusif @bold(XOR) entre deux TBZColor32. @br
      La valeur Alpha n'est pas pris en compte. }
    class operator Xor(constref A, B: TBZColor32): TBZColor32; overload;
    { Opérateur logique ET @bold(AND) entre un TBZColor32 et une valeur de type Byte. @br
      La valeur Alpha n'est pas pris en compte. }
    class operator And(constref A: TBZColor32; constref B:Byte): TBZColor32; overload;
    { Opérateur logique OU @bold(OR) entre un TBZColor32 et une valeur de type Byte. @br
      La valeur Alpha n'est pas pris en compte. }
    class operator or(constref A: TBZColor32; constref B:Byte): TBZColor32; overload;
    { Opérateur logique OU Exclusif @bold(XOR) entre un TBZColor32 et une valeur de type Byte. @br
      La valeur Alpha n'est pas pris en compte. }
    class operator Xor(constref A: TBZColor32; constref B:Byte): TBZColor32; overload;

    // function AddWithAlpha(constref A, B: TBZColor32): TBZColor32;
    // function SubWithAlpha(constref A, B: TBZColor32): TBZColor32;
    // function MulWithAlpha(constref A, B: TBZColor32): TBZColor32;
    // function DivWithAlpha(constref A, B: TBZColor32): TBZColor32;

    { Division rapide par des des composantes de la couleur. @br
      La valeur Alpha n'est pas pris en compte.}
    function DivideBy2 : TBZColor32;

    //function DivideBy2WithAlpha : TBZColor32;

    { Renvoie les valeurs minimum de chaque composant du TBZColor32 }
    function Min(Constref B : TBZColor32):TBZColor32; overload;
    //function MinWithAlpha(Constref B : TBZColor32):TBZColor32; overload;

    { Renvoie les valeurs minimum de chaque composant du TBZColor32 par rapport à la valeur de type Byte }
    function Min(Constref B : Byte):TBZColor32; overload;
    //function MinWithAlpha(Constref B : Byte):TBZColor32; overload;

    { Renvoie les valeurs maximum de chaque composant du TBZColor32 }
    function Max(Constref B : TBZColor32):TBZColor32; overload;
    //function MaxWithAlpha(Constref B : TBZColor32):TBZColor32; overload;

    { Renvoie les valeurs maximum de chaque composant du TBZColor32 par rapport à la valeur de type Byte }
    function Max(Constref B : Byte):TBZColor32; overload;
    //function MaxWithAlpha(Constref B : Byte):TBZColor32; overload;

    { S'assure chaque composant du TBZColor32 soit dans l'interval TBZColor32 [AMin, AMax] }
    function Clamp(Constref AMin, AMax : TBZColor32):TBZColor32; overload;
    //function ClampWithAlpha(Constref AMin, AMax : TBZColor32):TBZColor32; overload;

    { S'assure chaque composant du TBZColor32 soit dans l'interval des valeurs de type Byte [AMin, AMax] }
    function Clamp(Constref AMin, AMax : Byte):TBZColor32; overload;
    //function ClampWithAlpha(Constref AMin, AMax : Byte):TBZColor32; overload;

    { Retourne la couleur multiplié par un autre TBZColor32 et ajoute un autre TBZColor32. @br
      La valeur Alpha n'est pas pris en compte.}
    function MulAdd(Constref B, C : TBZColor32):TBZColor32;
    //function MulAddWithAlpha(Constref B, C : TBZColor32):TBZColor32;

    { Retourne la couleur multiplié par un autre TBZColor32 et diviser par un autre TBZColor32. @br
      La valeur Alpha n'est pas pris en compte.}
    function MulDiv(Constref B, C : TBZColor32):TBZColor32;
    //function MulDivWithAlpha(Constref B, C : TBZColor32):TBZColor32;

    { Mélange les composantes de la couleur en fonction de l'ordre des paramètres.}
    function Shuffle(const x,y,z,w : Byte):TBZColor32;
    { Retourne les composantes de la couleur dans un ordre spécifique en fonction du paramètre "ASwizzle" }
    function Swizzle(const ASwizzle: TBZVector4SwizzleRef ): TBZColor32;
    { Retourne Combine = Self + (V2 * F1). @br
      La valeur Alpha n'est pas pris en compte. }
    function Combine(constref V2: TBZColor32; constref F1: Single): TBZColor32;
    { Retourne Combine2 = (Self * F1) + (V2 * F2). @br
      La valeur Alpha n'est pas pris en compte. }
    function Combine2(constref V2: TBZColor32; const F1, F2: Single): TBZColor32;
    { Retourne Combine3 = (Self * F1) + (V2 * F2) + (V3 * F3). @br
      La valeur Alpha n'est pas pris en compte. }
    function Combine3(constref V2, V3: TBZColor32; const F1, F2, F3: Single): TBZColor32;

    { Renvoie la couleur ayant subit une interpolation  }
    function Lerp(Constref A : TBZColor32; const Theta : Single; const Distortion : Single;
                  const LerpType : TBZInterPolationType; const LerpAlpha : Boolean) : TBZColor32;

    { Retourne la valeur minimum des composantes XYZ (RGB par defaut) de la couleur }
    function MinXYZComponent : Byte;
    { Retourne la valeur maximum des composantes XYZ (RGB defaut) de la couleur }
    function MaxXYZComponent : Byte;

    { Interchange les composantes Rouge et bleu }
    function FastSwapRedBlue : Integer;

    { Retourne @True si les composantes de la couleur se trouve dans l'ensemble définit par "AminColor" et "aMaxColor" }
    function IsInRange(AMinColor,AMaxColor:TBZColor32):Boolean;

    { Retourne la composante Rouge de la couleur }
    property Red:Byte Index cRedOrder read GetColorComponent Write SetColorComponent;
    { Retourne la composante Verte de la couleur }
    property Green:Byte Index cGreenOrder read GetColorComponent Write SetColorComponent;
    { Retourne la composante Bleu de la couleur }
    property Blue:Byte Index cBlueOrder read GetColorComponent Write SetColorComponent;
    { Retourne la composante Alpha de la couleur }
    property Alpha:Byte Index cAlphaOrder read GetColorComponent Write SetColorComponent;

    { Access properties }
    Case Integer of
     0 : (V:TBZColor32Type);            //< Accès aux valeurs via un tableau
     1 : (X, Y, Z, W : Byte);           //< Accès aux valeurs par défault
     2 : (AsVector3b : TBZVector3b);    //< Accès aux valeurs de type TBZVector3b
     3 : (AsInteger : Cardinal);        //< Représentation de la couleur en type "Cardinal"
  end;
  { Pointeur vers un TBZColor32 }
  PBZColor32 = ^TBZColor32;

  { Type de convenance pour le type TBZColor32 }
  TBZColor = TBZColor32;
  { Pointeur vers un TBZColor }
  PBZColor = ^TBZColor;

  { Définition d'un tableau dynamique de TBZColor }
  TBZDynColorArray = Array of TBZColor;

  { Format générique d'une couleur au format RGBA en virgule flottante }
  TBZColorVector = record
  public
    {  Create as Homogenous from a Float. W value set by default to 0.0 }
    procedure Create(const AValue: single; Const AlphaValue : Single =1.0); overload;
    { Create  as Homogenous from three Float. W value set by default to 1.0}
    procedure Create(Const aX,aY,aZ: single; const aW : Single = 1.0); overload;
    {  Create As Point from three Float. W value set by default to 1.0}
    procedure Create(Const aValue : TBZColor32); overload;
    {  Create As Point from Vector.}
    procedure Create(Const aValue : TBZVector); overload;

    { Return Vector to a formatted string eg "(x, y, z, w)" }
    function ToString : String;

    {  Add two TBZColorVector }
    class operator +(constref A, B: TBZColorVector): TBZColorVector; overload;
    { Sub two TBZColorVector }
    class operator -(constref A, B: TBZColorVector): TBZColorVector; overload;
    { Multiply two TBZColorVector }
    class operator *(constref A, B: TBZColorVector): TBZColorVector; overload;
    { Divide two TBZColorVector }
    class operator /(constref A, B: TBZColorVector): TBZColorVector; overload;
    { Add to each component of a TBZColorVector one Float }
    class operator +(constref A: TBZColorVector; constref B:Single): TBZColorVector; overload;
    { Sub each component of a TBZColorVector by one float }
    class operator -(constref A: TBZColorVector; constref B:Single): TBZColorVector; overload;
    { Multiply one Float to each component of a TBZColorVector) }
    class operator *(constref A: TBZColorVector; constref B:Single): TBZColorVector; overload;
    { Divide each component of a TBZColorVector by one Float }
    class operator /(constref A: TBZColorVector; constref B:Single): TBZColorVector; overload;

    { Compare if two TBZColorVector are equal }
    class operator =(constref A, B: TBZColorVector): Boolean;
    { Compare if two TBZColorVector are greater or equal}
    class operator >=(constref A, B: TBZColorVector): Boolean;
    { Compare if two TBZColorVector are less or equal}
    class operator <=(constref A, B: TBZColorVector): Boolean;
    { Compare if two TBZColorVector are greater }
    class operator >(constref A, B: TBZColorVector): Boolean;
    { Compare if two TBZColorVector are less }
    class operator <(constref A, B: TBZColorVector): Boolean;
    { Compare if two TBZColorVector are not equal }
    class operator <>(constref A, B: TBZColorVector): Boolean;

    { Operateur XOr entre 2 couleur }
    class operator xor(constref A, B: TBZColorVector): TBZColorVector; overload;

    { Return shuffle components of self following params orders }
    function Shuffle(const x,y,z,w : Byte):TBZColorVector;
    { Return swizzle (shuffle) components of self from  TBZVector4SwizzleRef mask }
    function Swizzle(const ASwizzle: TBZVector4SwizzleRef ): TBZColorVector;

    { Return the minimum component value in XYZ }
    function MinXYZComponent : Single;
    { Return the maximum component value in XYZ }
    function MaxXYZComponent : Single;

    { Fast Self Divide by 2 }
    function DivideBy2:TBZColorVector;
    { Return the minimum of each component in TBZColorVector between self and another TBZColorVector }
    function Min(constref B: TBZColorVector): TBZColorVector; overload;
    { Return the minimum of each component in TBZColorVector between self and a float }
    function Min(constref B: Single): TBZColorVector; overload;
    { Return the maximum of each component in TBZColorVector between self and another TBZColorVector }
    function Max(constref B: TBZColorVector): TBZColorVector; overload;
    { Return the maximum of each component in TBZColorVector between self and a float }
    function Max(constref B: Single): TBZColorVector; overload;
    { Clamp Self beetween a min and a max TBZColorVector }
    function Clamp(Constref AMin, AMax: TBZColorVector): TBZColorVector; overload;
    { Clamp each component of Self beatween a min and a max float }
    function Clamp(constref AMin, AMax: Single): TBZColorVector; overload;

    { Multiply Self by a TBZColorVector and Add an another TBZColorVector }
    function MulAdd(Constref B, C: TBZColorVector): TBZColorVector;
    { Multiply Self by a TBZColorVector and Substract an another TBZColorVector }
    function MulSub(Constref B, C: TBZColorVector): TBZColorVector;
    { Multiply Self by a TBZColorVector and Divide by an another TBZColorVector}
    function MulDiv(Constref B, C: TBZColorVector): TBZColorVector;

    { Return linear interpolate value at T between self and B }
    function Lerp(Constref B: TBZColorVector; Constref T:Single): TBZColorVector;
    { Return Combine3 = Self + (V2 * F2) }
    function Combine(constref V2: TBZColorVector; constref F1: Single): TBZColorVector;
    { Return Combine2 = (Self * F1) + (V2 * F2) }
    function Combine2(constref V2: TBZColorVector; const F1, F2: Single): TBZColorVector;
    { Return Combine3 = (Self * F1) + (V2 * F2) + (V3 * F3) }
    function Combine3(constref V2, V3: TBZColorVector; const F1, F2, F3: Single): TBZColorVector;

    { Retourne le produit scalaire de 2 couleurs }
    function DotProduct(constref A: TBZColorVector):Single;

    { Retourne les valeurs absolue }
    function Abs: TBZColorVector;

    { Retourne les valeurs arrondies }
    function Round: TBZVector4i;

    { Convertit une couleur suivant la matrice d'espace de couleur }
    function ConvertColorSpace(ColorMatrixConversion : TBZColorSpaceMatrixConversion): TBZColorVector;

    { Access properties }
    case Byte of
      0: (V: TBZColorVectorType);              //< Array access
      1: (X, Y, Z, W: Single);                 //< Legacy access
      2: (Red, Green, Blue, Alpha: Single);    //< As Color components in RGBA order
  end;
  PBZColorVector = ^TBZColorVector;

  { Tableau dynamique pour stoquer des TBZColorVector }
  TBZDynColorVectorArray  = Array[0..MaxInt shr 8] of TBZColorVector;
  { Pointeur vers TBZDynColorVectorArray }
  PBZDynColorVectorArray = ^TBZDynColorVectorArray;

  { TBZColorFloatHSL : Definition du format de couleur HSL en virgule flottante}
  TBZColorFloatHSL =  record
  public
    procedure Create(H,S,L : Single; Const ColorSubSpace : THSLColorSubSpaceType = hslDefault);

    function ToColorRBGA : TBZColor32;
    function ToColorVector : TBZColorVector;

    function AsColorVector : TBZColorVector;
    function AsColorHSL : TBZColorFloatHSL;

    //function AsColorHSV  : TBZColorFloatHSV;

  Case Byte of
    0 : (V: TBZColorVectorType3f; );
    1 : (Hue, Saturation, Luminosity : Single; ColorSubSpaceType : THSLColorSubSpaceType; );
  end;

  { TBZColorFloatHSV : Definition du format de couleur HSV en virgule flottante}
  TBZColorFloatHSV =  record
  public

    procedure Create(H,S,V : Single; Const ColorSubSpace : THSVColorSubSpaceType = hsvDefault);

    function ToColorRBGA : TBZColor32;
    function ToColorVector : TBZColorVector;

    function AsColorHSV : TBZColorHSV;
    function AsColorVector : TBZColorVector;

    //function AsColorHSL  : TBZColorFloatHSL;

  Case Byte of
    0 : (V: TBZColorVectorType3f; );
    1 : (Hue, Saturation, Value : Single; ColorSubSpaceType : THSVColorSubSpaceType; );
  end;


{%endregion%}

{%region%=====[ Gestion matrice de couleurs ]=======================================}
  { @abstract(Décrit une matrice de conversion de couleur (simplifiée).)

    sous la forme : @br
    __ R G B A O(FFSET) @br
    R  _ _ _ _ _  @br
    G  _ _ _ _ _  @br
    B  _ _ _ _ _  @br
    A  _ _ _ _ _  @br

    @br
    @bold(Note) : Normalement une 5eme ligne intervient pour le "control des blancs"

    TODO : Créer un enregistrement avancé
  }
  TBZColorMatrixType = array[0..19] of single; //5x4

  { @abstract(Gestion d'une matrice de transformation de couleur.)

    Plus d'informations : @br
    @unorderedlist(
      @item(https://code.tutsplus.com/tutorials/manipulate-visual-effects-with-the-colormatrixfilter-and-convolutionfilter--active-3221
      @item(https://alistapart.com/article/finessing-fecolormatrix/
      @item(https://www.imatest.com/docs/colormatrix/
      @item(http://graficaobscura.com/matrix/index.html
      @item(https://www.c-sharpcorner.com/article/color-transformations-and-the-color-matrix/
      @item(https://www.codeproject.com/Articles/75006/Color-Matrix-Image-Drawing-Effects
      @item(https://docs.microsoft.com/en-us/previous-versions/windows/internet-explorer/ie-developer/platform-apis/jj192162(v=vs.85)?redirectedfrom=MSDN ))
  }
  TBZColorMatrix = record

    { Création d'un matrice de couleur }
    procedure Create(RedRow, GreenRow, BlueRow, AlphaRow, OffsetRow : TBZColorVector);

    { Création d'une matrice de couleur d'identité }
    procedure CreateIdentity;

    {@abstract(Création d'un matrice de couleur de luminosité.) @br   
     @bold(Note) : @br
     Un facteur de 0 créera une image entièrement noire. @br
     Un facteur de 1 laissera l'image inchangée. @br
     D'autres valeurs seront des multiplicateurs linéaires. @br
     Les valeurs supérieures à 1 sont autorisées, ce qui donnera des couleurs plus lumineuses.}
    procedure CreateBrightness(Factor : Single);

    {@abstract(Création d'un matrice de couleur de luminosité.) @br   
     @bold(Note) : @br
     Un facteur de 0 laissera l'image inchangée. @br
     Un facteur de -1 ou inférieur créera une image entièrement noire. @br
     Un facteur de 1 ou supérieur créera une image entièrement blanche. @br
     Un facteur supérieur à 0 augmentera la luminosité. @br
     Un facteur inférieur à 0 diminuera la luminosité.}
    procedure CreateLightness(Factor : Single);

    {@abstract(Création d'un matrice de couleur de contraste.) @br   
     @bold(Note) : @br
     Un facteur de 0 créera une image en niveau de gris. @br
     Un facteur de 1 laissera l'image inchangée. @br
     D'autres valeurs seront des multiplicateurs linéaires. @br
     Les valeurs supérieures à 1 sont autorisées, ce qui donnera des couleurs plus contrastés.}
    procedure CreateContrast(Factor : Single);

    {@abstract(Création d'un matrice de couleur de saturation.) @br   
     @bold(Note) : @br
     Un facteur de 0 créera une image désaturée. @br
     Un facteur de 1 laissera l'image inchangée. @br
     D'autres valeurs seront des multiplicateurs linéaires. @br
     Les valeurs supérieures à 1 sont autorisées, ce qui donnera des couleurs plus saturées.}
    procedure CreateSaturation(Factor : Single);

    { Création d'un matrice de couleur de rotation de teinte.}
    procedure CreateHueRotate(AngleInDeg : Single);

    { Création d'un matrice de couleur d'inversion }
    procedure CreateInvert(Factor : Single);

    {@abstract(Création d'un matrice de couleur de transparence)
     @bold(Note) : @br
     Un facteur de 0 créera une image totalement transparente. @br
     Un facteur de 1 laissera l'image inchangée. @br
     D'autres valeurs seront des multiplicateurs linéaires. }
    procedure CreateOpacity(Factor : Single);

    { Création d'une matrice de couleur approximative Noir et blanc }
    procedure CreateBlackWhite;

    { Création d'une matrice de couleur approximative Sepia }
    procedure CreateSepia(Factor : Single);

    { Création d'une matrice de couleur précise Sepia }
    procedure CreateSepiaII(Factor : Single);

    {@abstract(Création d'une matrice de couleur pour la conversion en niveau de gris suivant la spécification BT601.)@br
     https://en.wikipedia.org/wiki/Rec._601 }
    procedure CreateGrayscaleBt601(Factor : Single);

    {@abstract(Création d'une matrice de couleur pour la conversion en niveau de gris suivant la spécification BT709.)@br
     https://en.wikipedia.org/wiki/Rec._709#Luma_coefficients}
    procedure CreateGrayscaleBt709(Factor : Single);

    {@abstract(Création d'une matrice de couleur pour la conversion en niveau de gris suivant la spécification BT709.)@br
     https://en.wikipedia.org/wiki/Rec._2100#Luma_coefficients }
    procedure CreateGrayscaleBt2100(Factor : Single);

    {@abstract(Création d'une matrice de couleur pour la conversion en niveau de gris suivant la spécification BT709.)@br
     https://en.wikipedia.org/wiki/Rec._2020 }
    //procedure CreateGrayscaleBt2020(Factor : Single);

    { Création d'une matrice de couleur pour recréer le daltonisme achromanomalie(désensibilité des couleurs = daltonisme) }
    procedure CreateAchromAnomaly;

    { Création d'une matrice de couleur pour recréer le daltonisme achromatopsie (monochrome).}
    procedure CreateAchromatopsia;

    { Création d'une matrice de couleur pour recréer le daltonisme deutéranomalie (vert-faible) .}
    procedure CreateDeuterAnomaly;

    { Création d'une matrice de couleur pour recréer le datonisme deutéranopsie (vert) }
    procedure CreateDeuteranopia;

    { Création d'une matrice de couleur pour recréer le daltonisme  Protanomalie (rouge-faible) .}
    procedure CreateProtAnomaly;

    { Création d'une matrice de couleur pour recréer le daltonisme  Protanopsie (rouge)}
    procedure CreateProtanopia;

    { Création d'une matrice de couleur pour recréer le daltonisme tritanomalie (bleu-faible).}
    procedure CreateTritAnomaly;

    { Création d'une matrice de couleur pour recréer le daltonisme tritanopsie (bleu).}
    procedure CreateTritanopia;

    { Création d'une matrice de couleur pour recréer un ancien effet d'appareil photo KodaChrome. }
    procedure CreateKodaChrome;

    { Création d'une matrice de couleur pour recréer un ancien effet d'appareil photo Lomo. }
    procedure CreateLomoGraph;

    { Création d'une matrice de couleur pour recréer un ancien effet d'appareil photo polaroide. }
    procedure CreatePolaroid;

    { Création d'une matrice de couleur pour recréer un ancien effet d'appareil photo polaroide. }
    procedure CreatePolaroidII;

    { Création d'une matrice de couleur pour créer un effet de photo ancienne }
    procedure CreateOldPhoto;

    { Retourne @True si la matrice est une matrice d'identité }
    function IsIdentity : Boolean;

    { Retourne la représentation de la matrice sous forme de chaine de caractères }
    function ToString : String;

    { Ajoute deux matrices }
    class operator +(constref A, B : TBZColorMatrix) : TBZColorMatrix;
    { Soustrait une matrice à une autre }
    class operator -(constref A, B : TBZColorMatrix) : TBZColorMatrix;
    { Inverse le signe des composantes de la matrice }
    class operator -(constref A : TBZColorMatrix) : TBZColorMatrix;
    { Multiplie dux matrice }
    class operator *(constref A, B : TBZColorMatrix) : TBZColorMatrix;
    { Multiplie les composantes de la matrice par un facteur }
    class operator *(constref A : TBZColorMatrix; constref B : Single) : TBZColorMatrix;
    { Retourne @True si deux matrices sont identiques }
    class operator =(constref A, B : TBZColorMatrix) : Boolean;

    { Transforme une couleur avec la matrice actuelle }
    function Transform(AColor : TBZColorVector) : TBZColorVector;

    { Access aux propriétés }
    case Byte of
      0: (V: array [0..4] of TBZColorVector);         //< Acces via tableau de TBZVector
      1: (M: array [0..4, 0..3] of Single);           //< Acces par tableau 2D /!\ Cause de l'erreur d'alignement des données ?????? A placé en dernier ????
      2: (R,G,B,A,W : TBZColorVector);                //< Acces aux lignes
      3: (m11, m12, m13, m14: Single;                 //< Acces par défaut
          m21, m22, m23, m24: Single;
          m31, m32, m33, m34: Single;
          m41, m42, m43, m44: Single;
          m51, m52, m53, m54: Single);
  end;

{%region%=====[ Gestion, conversion des espaces de couleurs ]========================}

Type
  { Enumération des Matrices de conversion couleur vers niveaux de gris. Cf : https://fr.wikipedia.org/wiki/Niveau_de_gris }
  TBZGrayConvertMode = (gcmLightness, gcmLuminosity, gcmLuminance, gcmAverage, gcmDecomposeMin, gcmDecomposeMax, gcmColorMask, gcmCustomShades, gcmPowerLaw, gcmLogarithmic);
  { Représentation d'une matrice de conversion couleurs vers niveaux de gris }
  TBZGrayMatrixConv =  packed record
    red, green, blue : single;
  end;

  { Enumération des matrice de conversion couleur vers niveaux de gris disponibles }
  TBZGrayMatrixType = (gmtNone,gmtNTSC, gmtJPEG, gmtAverage, gmtPhotoShop, gmtCIEObserverRef709, gmtRMY, gmtRedMask, gmtGreenMask, gmtBlueMask, gmtAlphaMask);

  { Masques et décalage des bits pour extraire les composantes d'une couleur.}
  TBZColorBitFields = packed record
    Size : Byte;                                            //< Nombre de bits (12 = 16/24bits, 16=32bits)
    RedMask, GreenMask, BlueMask, AlphaMask : LongWord;     //< Masque des composantes de la couleur
    RedShift, GreenShift, BlueShift, AlphaShift : Integer;  //< Masque de décalage en bits des composantes de la couleur
    RedDeltaShift, GreenDeltaShift, BlueDeltaShift, AlphaDeltaShift : Integer; //< Masque de décalage en bits supplémentaires des composantes de la couleur
    RedSize, GreenSize, BlueSize, AlphaSize : Byte;         //< Taille en bits des composantes de la couleur
  end;



{%region%=====[ Descriptions des formats de couleurs supportés ]================}

Type
  { Reflete la taille et l'ordre des valeurs d'un pixel  }
  TBZColorFormat = (cfCustom,
                   cfDevice,
                   cfDefault,
                   // pf1bits,pf8bits
                   cfMono,
                   // pf8bits, pf16bits, pf32bits, pf64bits
                   cfIndexed, //cfGray
                   // pf8bits, pf16bits, pf32bits, pf64bits
                   cfGrayAlpha,
                   // pf16bits
                   cfXRGB_1555, cfXBGR_1555,
                   //cfRGBX_5551,
                   //cfXRGB_4444, cfRGBX_4444
                   cfRGB_565, cfBGR_565,
                   // pf24bits, pf48bits,
                   cfRGB, cfBGR,
                   // pf32bits, pf64bits
                   cfRGBA, cfBGRA,
                   cfABGR, cfARGB,
                   //formats spéciaux  pf64bits , pf96bits, pf128bits
                   cfXRGB, cfXBGR);

                   (* cfRGBAFloat, cfBGRAFloat, cfARGBFloat, cf ABGRFLoat,
                   cfHSL, cfHSLA, cfHSV, cfHSVA,
                   cfYUV, cfCIELab, cfCIExyz, cfCMY, cfCMYK, cfPhotoYCC, cfYCbCr *)

  { Liste des formats de couleur supportés }
  TBZSupportedColorFormat = set of TBZColorFormat;

  { Reflete la taille du pixel en bits }
  TBZPixelFormat = (pfDefault, pf1bit, pf2bits, pf4bits, pf8bits, pf15bits, pf16bits, pf24bits, pf32bits, pf48bits, pf64bits, pf96bits, pf128bits);

  {  Liste des nombres de bits supportés par un formats de couleur  }
  TBZSupportedPixelSize = set of TBZPixelFormat;

  { Conteneur décrivant les formats de couleur supportés }
  TBZColorFormatInfosRec = packed record
   name : string[12]; //< Représentation du format sous chaine de caractères
   SupportedPixelFormat : TBZSupportedPixelSize;        //< Bits par pixel supportés
   DefaultPixelFormat : TBZPixelFormat;                 //< Format du pixel par defaut
   RedPrec, GreenPrec, BluePrec, AlphaPrec : Byte;      //< Precision des composantes de couleurs en bits
   RedMask, GreenMask, BlueMask, AlphaMask : LongWord;  //< Position des composantes de couleurs
   RedShift, GreenShift, BlueShit, AlphaShift : Integer;//< Décalage des composantes de couleurs en bits
   BitsPerPixel : Byte;                                 //< Bits par Pixel : 1,2, 4,8,16,24,32,48,64,96,128
   PixelSize : Byte;                                    //< Taille en octet d'UN Pixel : 1,2,3,4,6,8,12,16
   SwapRB : Boolean;                                    //< Si les valeurs Rouge et bleu sont inversées
   SwapAlpha : Boolean;                                 //< Si la valeur Alpha est en premier (ex : ARGB )
 end;


{%endregion%}

{%region%-----[ Assistants dans la gestion des formats de couleur ]------------------------}

{%region%=====[ TBZColorVectorHelper ]==========================================}

Type
  { Assistant pour le type TBZColorVector}
  TBZColorVectoHelper = record helper for TBZColorVector
  public
    { Retourne les valeurs RGBA en virgules flottante dans le format TBZColor32 }
    function AsRGBA : TBZColor32;

    { Convertis la couleur au format RGBA en virgule flottante vers le modele de couleur HSV en virgule flottante @br
      Le résultat est retourné en tant que TBZColorVector. Les valeurs HSV sont comprises entre 0.0 et 1.0 }
    function ToColorVectorHSV : TBZColorVector;
    function AsColorFloatHSV : TBZColorFloatHSV;

    { Convertis la couleur au format RGBA en virgule flottante vers le modele de couleur HSV en virgule flottante (TBZColorFloatHSV)
      en fonction du sous-expace de couleur (THSVColorSubSpaceType) choisi }
    function ToColorFloatHSV(Const SubSpaceType : THSVColorSubSpaceType) : TBZColorFloatHSV;

    { Convertis la couleur au format RGBA en virgule flottante vers le modele de couleur HSL (HLS) en virgule flottante @br
      Le résultat est retourné en tant que TBZColorVector. Les valeurs HSV sont comprises entre 0.0 et 1.0 }
    function ToColorVectorHSL : TBZColorVector;
    function AsColorFloatHSL : TBZColorFloatHSL;

    { Convertis la couleur au format RGBA en virgule flottante vers le modele de couleur HSL (HSL) en virgule flottante (TBZColorFloatHSL)
      en fonction du sous-expace de couleur (THSLColorSubSpaceType) choisi }
    function ToColorFloatHSL(Const SubSpaceType : THSLColorSubSpaceType) : TBZColorFloatHSL;

    { Applique une matrice de couleur }
    function ApplyColorMatrix(ColorMatrix : TBZColorMatrix): TBZColorVector;
  end;

{%endregion%}

{%region%=====[ TBZColor32Helper ]==============================================}

  { Assistant pour le type TBZColor32 }
  TBZColor32Helper = record Helper for TBZColor
  public
    { Create color from a TColor }
    procedure Create(Const aValue : TColor); overload;
    { Create color from a TFPColor }
    procedure Create(Const aValue : TFPColor); overload;
    { Create color from a TBZColorVector }
    procedure Create(Const aValue : TBZColorVector); overload;

    { Create color from a TBZColorVector }
    procedure Create(Const aValue : TBZColorFloatHSL); overload;
    procedure Create(Const aValue : TBZColorFloatHSV); overload;
    procedure Create(Const aValue : TBZColorHSL); overload;
    procedure Create(Const aValue : TBZColorHSV); overload;
    procedure Create(Const aValue : TBZColorYUV); overload;
    procedure Create(Const aValue : TBZColorCMYK); overload;


    { Create color from a Haxadecimal string (eg : #RRGGBBAA ) }
    procedure Create(Const aHexValue : String); overload;
    { Create color from an Integer and format defined by a TBZColorBitFields }
    procedure Create(Const aBitFields : TBZColorBitFields; SrcColor : Integer);

    { Retourne la couleur negative}
    function Negate: TBZColor32;
    { Transforme la couleur actuelle en negatif }
    procedure DirectNegate;

    { @abstract(Désaturation des couleurs = Conversion en niveau de gris) @br
      @bold(Note) : @br
      @unorderedlist(
        @item(Le paramètre GrayMatrix : Matrice de conversion, uniquement utilisé avec GrayMode = gcmLuminosity
        @item(Le paramètre OptVal seulement utilisé : @br
              @unorderedlist(
               @item(si GrayMode =  gmColorMask alors sa valeur sera comprise entre 0 et 3, ou chaque chiffre représente respectivement le 0 = Canal Rouge, 1 = Vert, 2 = Bleu et 3 = Alpha)
               @item(si GrayMode = gcmCustomShades (réduction de couleurs). Toutes les valeurs comprises entre 2 et 256 sont acceptées ; 2 donne une image en noir et blanc, tandis que 256 vous donne une image identique à la méthode gcmAverage)
               @item(si GrayMode = gcmPowerLaw et gcmLogarithmic))
        @item(Le paramètre GammaFactor est utilisé seulement pour GrayMode = gcmLogarithmic))

        Dans le cas ou GrayMode = gcmLogarithmic, le paramètre OptVal serre de constante qui est généralement utilisée pour mettre à l'échelle
        la plage de la fonction "log" afin qu'elle corresponde au domaine d'entrée. "Constante = OptVal / log (1 + ValeurCouleur)". @br
        Pour une image avec des composantes de couleur en byte OptVal sera compris entre [0..255] @br
        Dans le cas de couleur en virgule flottante OptVal sera compris entre [0..1] @br
        Le paramètre OptVal peut également être utilisé pour augmenter davantage le contraste : @br
          - plus la valeur est élevé, plus l'image apparaît lumineuse..
     }
    function Desaturate(Const GrayMode : TBZGrayConvertMode=gcmLuminosity;Const AGrayMatrixType:TBZGrayMatrixType = gmtJPEG; Const OptVal:Single=0; Const GammaFactor : Single = 1.7): TBZColor32;

    //procedure DirectDesaturate;

    {Retourne la couleur hyper saturée }
    function HyperSat: TBZColor32;
    //procedure DirectHyperSat;

    { Retourne la couleur inverse }
    function Invert: TBZColor32;
    //procedure DirectInvert;

    { Retourne la moyenne de la couleur avec une autre }
    function Average(Constref A : TBZColor32): TBZColor32;
    ////procedure DirectAverage;

    { Retourne la couleur modulée avec une autre}
    function Modulate(Constref A : TBZColor32): TBZColor32;

    { Combine la couleur actuelle avec une autre, suivant le mode de fusion (TBZColorCombineMode) choisi }
    function Combine(ConstRef A : TBZColor32; CombineMode : TBZColorCombineMode = cmNormal) : TBZColor32;

    { Retourne la couleur actuelle mixée avec une autre suivant un facteur compris entre [0..1] }
    function Mix(Constref A : TBZColor32;Factor:Single): TBZColor32;

    { Retourne la couleur actuelle mixée inversée avec une autre suivant un facteur compris entre [0..1] }
    function MixInv(Constref A : TBZColor32; Factor : Single) : TBZColor32;

    { Retourne la couleur (de fond) mélangée avec une une autre couleur en fonction des facteurs Source et Destination
      Simule la fonction OpenGL "glBlendFunc" cf https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBlendFunc.xhtml }
    function Blend(Constref A : TBZColor32;const SrcFactor: TBZBlendingFactor=bfDstAlpha; const DestFactor: TBZBlendingFactor=bfOne): TBZColor32; overload;

    { Retourne la couleur (de fond) mélangée avec une couleur La transparence globale est ajustée en fontion du paramètre "MasterAlpha". Voir aussi AlphaBlend et BlendHQ}
    function Blend(AColor : TBZColor; Const MasterAlpha : Byte = 255) : TBZColor; overload;

    { Retourne la couleur (de fond) mélangée avec une couleur et sa couleur combinée. La transparence globale est ajustée en fontion du paramètre "MasterAlpha".
      ATTENTION : Pour des raison de qualité l'image nécessite que les valeurs de couleurs soit prémultipliées (cf TBZCustomBitmap.PremultiplyAlpha) }
    Function BlendHQ(ForeColor, CombinedColor : TBZColor; Const MasterAlpha : Byte = 255) : TBZColor; overload;

    { Retourne la couleur (de fond) mélangée avec une couleur. Les valeur de transparence sont automatiquement ajusté en fonction de la valeur du canal Alpha de la couleur à mélanger }
    function AlphaBlend(DestPix: TBZColor32):TBZColor32;

    { Retourne la couleur  mélangée de quatre couleurs en fonction des facteurs de poid}
    function BlendFour(c1, c2, c3, c4 : TBZColor; w1, w2, w3, w4 : Single) : TBZColor; overload;
    function BlendFour(c1, c2, c3, c4 : TBZColor; weight : Single) : TBZColor; overload;

    { Mixe la couleur actuelle avec une autre en fonction de la valeur de leur Alpha }
    function AlphaMix( DestPix: TBZColor32):TBZColor32;

    { Retourne la couleur (de fond) mélangée avec une couleur.  sans ajuster automatiquement les valeur de transparence }
    function BlendWithoutAlpha(DestPix: TBZColor):TBZColor;

    { Retourne la luminance de la couleur }
    function Luminance : Byte;
    { Retourne la luminosité de la couleur }
    function Luminosity : Byte;

    { Intervertit les canaux Rouge et Bleu }
    function SwapRBChannels : TBZColor32;

    { Adjust brightness of the color by a factor }
    procedure AdjustBrightness(factor: Single);
    { Adjust brightness of the color by a factor }
    procedure AdjustContrast(factor: Single);
    { Adjust saturation of the color by a factor. If AsGray is true then color is return as gray level }
    procedure AdjustSaturation(Factor: Single; Const AsGray: Boolean = False);

    { GammaCorrection : Correction GAMMA de l'image suivant le facteur "factor" en nombre réel par defaut 2.2}
    //function GammaCorrection(Const Factor: Single = 2.2): TBZColor32;
    //procedure DirectGammaCorrection(Const Factor: Single = 2.2);
    { Posterize }
    //function Posterize(Factor: Single):TBZColor32;
    //procedure DirectPosterize(Factor: Single);
    { Solarize }
    //function Solarize(Factor: Single):TBZColor32;
    //procedure DirectSolarize(Factor: Single);

    { Eclaircie la coueur en fonction du facteur }
    function Lighten(Factor : Single) : TBZColor32;
    { Assombrit la couleur en fonction du facteur }
    function Darken(Factor : Single) : TBZColor32;

    { Diminue le rouge de Delta unité. Par defaut 1 }
    procedure DecRed(const Delta:Byte = 1);
    { Diminue le vert de Delta unité. Par defaut 1 }
    procedure DecGreen(const Delta:Byte = 1);
    { Diminue le bleu de Delta unité. Par defaut 1 }
    procedure DecBlue(const Delta:Byte = 1);
    { Diminue l'alpha de Delta unité. Par defaut 1 }
    procedure DecAlpha(const Delta:Byte = 1);

    { Incrémente le rouge de Delta unité. Par defaut 1 }
    procedure IncRed(const Delta:Byte = 1);
    { Incrémente le vert de Delta unité. Par defaut 1 }
    procedure IncGreen(const Delta:Byte = 1);
    { Incrémente le bleu de Delta unité. Par defaut 1 }
    procedure IncBlue(const Delta:Byte = 1);
    { Incrémente l'alpha de Delta unité. Par defaut 1 }
    procedure IncAlpha(const Delta:Byte = 1);

    { Definie l'alpha de la couleur actuelle en fonction d'une autre couleur }
    procedure SetAlphaFromIntensity(AColor: TBZColor32);
    { Definie l'alpha à 0 de la couleur actuelle en fonction d'une autre couleur seulement si celle est completement noir }
    procedure SetAlphaFromSuperBlackTransparent(AColor: TBZColor32);
    { Definie l'alpha de la couleur actuelle en fonction de la luminance d'une autre couleur }
    procedure SetAlphaFromLuminance(AColor: TBZColor32);
    { Definie l'alpha de la couleur actuelle en fonction la racine carré de la luminance d'une autre couleur }
    procedure SetAlphaFromLuminanceSqrt(AColor: TBZColor32);
    { Definie l'alpha de la couleur actuelle en fonction de la luminance inverse d'une autre couleur }
    procedure SetAlphaFromInverseLuminance(AColor: TBZColor32);
    { Definie l'alpha de la couleur actuelle en fonction la racine carré inverse de la luminance d'une autre couleur }
    procedure SetAlphaFromInverseLuminanceSqrt(AColor: TBZColor32);
    { Definie l'alpha de la couleur actuelle en fonction d'une autre couleur seulement si celle-ci est entièrement opaque }
    procedure SetAlphaOpaque(AColor: TBZColor32);

    { Convertis la couleur actuelle en TColor }
    function AsColor:TColor;
    { Convertis la couleur actuelle en TBZColorVector en tenant en compte l'ordre des composantes (BGRA/RGBA) }
    function AsColorVector : TBZColorVector;
    { Convertis la couleur actuelle en TBZColorVector sans tenir compte de l'ordre des composantes (BGRA/RGBA) }
    function ToColorVector : TBZColorVector;

    { Convertis la couleur actuelle en HSL (TBZColorFloatHSL) }
    function ToColorFloatHSL : TBZColorFloatHSL; overload;
    { Convertis la couleur actuelle en HSL (TBZColorFloatHSL) en fonction du sous espace de couleur choisi THSLColorSubSpaceType}
    function ToColorFloatHSL(const SubSpaceType : THSLColorSubSpaceType) : TBZColorFloatHSL; overload;

    { Convertis la couleur actuelle en HSV (TBZColorFloatHSV) }
    function ToColorFloatHSV : TBZColorFloatHSV; overload;
    { Convertis la couleur actuelle en HSV (TBZColorFloatHSL) en fonction du sous espace de couleur choisi THSVColorSubSpaceType}
    function ToColorFloatHSV(const SubSpaceType : THSVColorSubSpaceType) : TBZColorFloatHSV; overload;

    { Convertis la couleur actuelle en HSL (TBZColorHSL) }
    function ToColorHSL : TBZColorHSL; overload;
    { Convertis la couleur actuelle en HSL (TBZColorHSL) en fonction du sous espace de couleur choisi THSLColorSubSpaceType}
    function ToColorHSL(const SubSpaceType : THSLColorSubSpaceType) : TBZColorHSL; overload;

    { Convertis la couleur actuelle en HSV (TBZColorHSV) }
    function ToColorHSV(Const SubSpaceType : THSVColorSubSpaceType) : TBZColorHSV; overload;
    { Convertis la couleur actuelle en HSV (TBZColorHSL) en fonction du sous espace de couleur choisi THSVColorSubSpaceType}
    function ToColorHSV : TBZColorHSV; overload;

    //function TransformHSV(HueShiftInDeg : Integer;  SaturationFactor, ValueFactor : Single) : TBZColor;
    //function toBGRA : TBZColorBGRA_32;
    //function toRGB : TBZColor24;

    //function HueRotate(Deg : Integer) : TBZColor;


  end;

{%endregion%}

  { Assistant pour le type TBZColorHSL }
  TBZColorHSLHelper =  Record Helper for TBZColorHSL
    { Convertis la couleur HSL actuelle vers un TBZColor }
    function ToColorRGBA : TBZColor;
  end;

{%endregion%}

  { Tableau pour stocker un palette de couleur au format TBZColor32 }
  TBZPaletteEntries  = Array[0..maxInt shr 8] of TBZColor;
  { Pointeur vers un TBZPaletteEntries }
  PBZPaletteEntries  = ^TBZPaletteEntries;

  { Gestionnaire de palette de couleurs }
  TBZColorsManager = Class;

  { Objet englobant une couleur de type TBZColor @A Remplacer par TBZColorProperty de l'unité BZControlClasses }
  TBZColorItemProperty = Class(TBZUpdateAbleObject)
  Private
    FColor: TBZColor;
    FName:  String;
    FTag:   Integer;

    Procedure SetRed(Const AValue: Byte);
    Procedure SetGreen(Const AValue: Byte);
    Procedure SetBlue(Const AValue: Byte);
    Procedure SetAlpha(Const AValue: Byte);
    Procedure SetValue(Const AValue: TBZColor);
    Procedure SetColorName(Const aName: String);

    Function getRed: Byte;
    Function getGreen: Byte;
    Function getBlue: Byte;
    Function getAlpha: Byte;
    Function getValue: TBZColor;

  Protected
  Public
    Constructor Create(AOwner: TPersistent); Override;

    Procedure WriteToFiler(writer: TVirtualWriter); //override;
    Procedure ReadFromFiler(reader: TVirtualReader); //override;

    { Valeur de la couleur }
    Property Value: TBZColor read getValue write setValue;
    { Nom de la couleur eg : clrRed }
    Property Name: String read FName write setColorName;
  Published
    { Valeur du canal rouge }
    Property Red: Byte read getRed write setRed;
    { Valeur du canal vert }
    Property Green: Byte read getRed write setGreen;
    { Valeur du canal Bleu }
    Property Blue: Byte read getRed write setBlue;
    { Valeur du canal alpha pour la transparence }
    Property Alpha: Byte read getRed write setAlpha;
    { Valeur complémentaire personnel }
    Property Tag: Integer read FTag write FTag;
  End;

  { Objet de Classe descendante de TBZColorItemProperty }
  TBZColorItemPropertyClass = Class Of TBZColorItemProperty;

  { Objet héritant de TBZColorItemProperty @br
    Sert à englober une couleur pour la creation de dégradés }
  TBZGradientColorItem = Class(TBZColorItemProperty)
  Private
    FNodeEaseMode: TBZAnimationMode; //TBZShadeNodeType;
    FNodeEaseType: TBZAnimationType; //TBZShadeNodeType;
    FLerpType: TBZInterpolationType;
    FDistorsion: Single; // Uniquement LerpType = itExp, itLn, itPower
    FTheta: Single;

    Procedure setNodeEaseType(Const aValue: TBZAnimationType);
    Procedure setLerpType(Const aValue: TBZInterpolationType);
    Procedure setDistorsion(Const aValue: Single);
    Procedure setTheta(Const aValue: Single);

    Function getNodeEaseType: TBZAnimationType;
    Function getLerpType: TBZInterpolationType;
    Function getDistorsion: Single;
    Function getTheta: Single;
    function getNodeEaseMode : TBZAnimationMode;
    procedure setNodeEaseMode(const AValue : TBZAnimationMode);
  Protected
  Public
    Constructor Create(AOwner: TPersistent); Override;

    Procedure WriteToFiler(writer: TVirtualWriter);
    Procedure ReadFromFiler(reader: TVirtualReader);

  Published
    { Fonction d'interpolation dans le temps entre 2 couleurs }
    Property EaseMode: TBZAnimationMode read getNodeEaseMode write setNodeEaseMode;
    { Fonction d'interpolation dans le temps entre 2 couleurs }
    Property EaseType: TBZAnimationType read getNodeEaseType write setNodeEaseType;
    { Fonction d'interpolation entre 2 couleurs }
    Property LerpType: TBZInterpolationType read getLerpType write setLerpType;
    { Paramètre pour influencer le dégradé }
    Property Distorsion: Single read getDistorsion write setDistorsion;
    { Position de la couleur dans le dégradé. Compris entre 0 et 1. }
    Property Theta: Single read getTheta write setTheta;

    Property Red;
    Property Green;
    Property Blue;
    Property Alpha;
    Property Tag;
  End;

  { Tableau pour stocker des TBZGradientColorItem }
  TBZGradientColorItemArray = Array[0..MaxInt Shr 4] Of TBZGradientColorItem;
  { Pointeur vers un TBZGradientColorItemArray }
  PBZGradientColorItemArray = ^TBZGradientColorItemArray;

  { Tableau pour stocker des TBZColorItemProperty }
  TBZColorItemPropertyArray = Array[0..MaxInt Shr 4] Of TBZColorItemProperty;
  { Pointeur vers un TBZColorItemPropertyArray}
  PBZColorItemArray = ^TBZColorItemPropertyArray;

  { Gestionnaire d'une liste de couleurs de type TBZColorItemProperty }
  TBZColorList = Class;

  { Gestionnaire d'une liste de couleurs de type TBZGradientColorItem pour la génération de dégradés }
  TBZGradientColorList = Class(TBZPersistentObjectList)
  Private
    FOwnColorList:  TBZColorList;

    FTweenGlobal:   Boolean;
    FTweenDuration: Single;
    FTweenNext:     Boolean;
  Protected
    Function getDirectGradientList: PBZColorItemArray;
    Function GetGradientColorItem(index: Integer): TBZGradientColorItem;
    Procedure SetGradientColorItem(index: Integer; val: TBZGradientColorItem);
  Public
    Constructor CreateOwned(AOwner: TBZColorList);
    Destructor Destroy; Override;

    { Efface la liste de couleur}
    Procedure Clear; Override;

    { Ajoute une couleur à la liste}
    Function AddColorStop(Const aColor: TBZColor): Integer; Overload;
    { Ajoute une couleur à la liste}
    Function AddColorStop(Const aColor: TBZColor;  Const aEaseMode : TBZAnimationMode; Const aEaseType: TBZAnimationType; Const aLerpType: TBZInterpolationType;
      Const aTheta: Single; Const aDistorsion: Single = 0): Integer; Overload;
    { Ajoute une couleur à la liste}
    Function AddColorStop(Const aColorItem: TBZGradientColorItem): Integer; Overload;
    { Ajoute une couleur à la liste}
    Function AddColorStop(Const aColor: TBZColor; Const aTheta:Single): Integer;Overload;

    { Retourne la couleur du degradé à la postion "aTheta" avec "aTheta" compris entre 0.0 et 1.0 }
    Function GetGradientColor(Const aTheta: Single): TBZColor;

    { Création du dégradé de "nb" couleurs }
    Procedure CreateGradient(nb: Integer);
    { Creation de la liste de couleurs du degradé ou la retourne si celle-ci existe déja }
    Function GetOrCreateGradientColorList(Const nb: Integer = 256; Const AColorList: TBZColorList = nil): TBZColorList;

    { Colors : Acceder à la couleur "Index" de la liste }
    Property ColorStops[Index: Integer]: TBZGradientColorItem read GetGradientColorItem write setGradientColorItem;
    { GradientList : Retourne  la liste des couleurs du dégradé }
    Property GadientList: PBZColorItemArray read getDirectGradientList;
    { TweenGlobal : Utilisation de l'interpolation global ou local }
    Property TweenGlobal: Boolean read FTweenGlobal write FTweenGlobal;
    { TweenNext : Utilser le type d'interpolation du point suivant pour les calcul du dégradé }
    Property TweenNext: Boolean read FTweenNext write FTweenNext;
    { TweenDuration : Paramètre de durée dans le temps du dégradé }
    Property TweenDuration: Single read FTweenDuration write FTweenDuration;
  End;

  { Classe pour la gestion d'une palette (liste) de couleurs de type TBZColorItemProperty }
  TBZColorList = Class(TBZPersistentObjectList)
  Private
    FOwner:      TBZColorsManager;
  Protected
    Function getDirectList: PBZColorItemArray;
    Function GetColorItem(index: Integer): TBZColorItemProperty;
    Procedure SetColorItem(index: Integer; val: TBZColorItemProperty);
  Public
    Constructor CreateOwned(aOwner: TBZColorsManager);
    Destructor Destroy; Override;

    //procedure WriteToFiler(writer: TVirtualWriter); //override;
    //procedure ReadFromFiler(reader: TVirtualReader);// override;

    { Efface la liste de couleur}
    Procedure Clear; Override;
    { Ajoute une couler à la liste }
    Function AddColor(Const aColor: TBZColor): Integer; Overload;
    { Ajoute une couler à la liste }
    Function AddColor(Const aName: String; Const aColor: TBZColor): Integer; Overload;
    { Ajoute une couler à la liste }
    Function AddColor(Const aColorItem: TBZColorItemProperty): Integer; Overload;
    { Efface une couleur de la liste }
    Procedure RemoveColor(Const aName: String);
    { Recherche une couleur dans la liste }
    Function FindColorByName(Const aName: String; Out Index: Integer): TBZColor; Overload;
    { Recherche une couleur dans la liste }
    Function FindColorByName(Const aName: String): TBZColor; Overload;
    // function  FindColorIndex(const aName: String): integer; overload;
    // function  FindColorIndex(const aColor : TBZColor): integer; overload;
    // function FindColor(aItem: TBZColorItemProperty): Integer; overload;

    { Owner: Liste gérér par un gestionnaire de type TBZColorManager }
    Property Owner: TBZColorsManager read FOwner write FOwner;
    { DirectList : Pointeur sur une liste des couleurs }
    Property DirectList: PBZColorItemArray read getDirectList;
    { Colors : Acceder à la couleur "Index" de la liste }
    Property Colors[Index: Integer]: TBZColorItemProperty read GetColorItem write setColorItem;
  End;

  { @abstract(Objet servant à manipuler une palette de couleur en "temp réel".)

    @unorderedList(
     @item(Avancer/reculer, boucler)
     @item(Chargement de palette spéciales prédéfinies)
     @item(Creation de couleurs dans la palette)
     @item(Creation de degradés depuis une liste de couleurs)
     @item(Lecture, écriture da la liste de couleurs depuis, vers un fichier))
  }
  TBZColorsManager = Class(TBZCadenceAbleComponent)
  Private
    FColors: TBZColorList;
    FGradientColors: TBZGradientColorList;
    FGradientPalette: TBZColorList;

    FFriendlyName: String;

    //FPaletteFileFormat  : TBZPaletteFileFormat(TBZCustomDataFile);
  Protected
    Procedure SetColors(Const aColors: TBZColorList);
    Procedure SetGradientColors(Const aColors: TBZGradientColorList);
  Public
    Constructor Create(aOwner: TComponent); Override;
    Destructor Destroy; Override;

    //procedure Assign(const Source: TBZColorsManager); overload;

    { Notification de changement de valeur dans la classe }
    Procedure NotifyChange(Sender: TObject);

    { Permet d'animer les valeurs }
    Procedure DoProgress(Const progressTime: TBZProgressTimes); Override;

    { Retourne la classe de couleur créé par le Manager }
    Class Function ColorItemClass: TBZColorItemPropertyClass; Virtual;
    { Creation d'une nouvelle couleur }
    Function CreateColor: TBZColorItemProperty; Virtual; Overload;
    { Creation d'une nouvelle couleur }
    Function CreateColor(aName: String; aColor: TBZColor): TBZColorItemProperty; Virtual; Overload;
    { Creation d'une nouvelle couleur }
    Function CreateColor(aColor: TBZColor): TBZColorItemProperty; Virtual; Overload;
    { Creation de plusieurs couleurs }
    Procedure CreateColors(nbColors: Integer);
    { Creation d'une palette de 16 couleurs par defaut }
    Procedure CreateDefaultPalette;
    { Creation d'une palette en 255 niveaux de gris }
    Procedure CreateGrayPalette;
    { Creation d'une palette 2 couleurs Noir et Blanc }
    Procedure CreateBWPalette;

    { Creation d'un identificatuer sous forme de chaine de caractère pour une couleur }
    Function MakeColorName(Const aColor: TBZColor): String;

    { Creation d'une palette de couleur Web }
    //procedure CreateWebPalette;

    { Creation d'une couleur pour la  creation d'un dégradé }
    Function CreateGradientColor: TBZGradientColorItem; Virtual; Overload;
    { Creation d'une couleur pour la  creation d'un dégradé }
    Function CreateGradientColor(aColor: TBZColor): TBZGradientColorItem; Virtual; Overload;
//    Function CreateGradientColor(aColor: TBZColor; aTheta : Single): TBZGradientColorItem; Virtual; Overload;

    { Creation d'une couleur pour la  creation d'un dégradé }
    Function CreateGradientColor(Const aColor: TBZColor; Const aTheta: Single; Const aEaseMode : TBZAnimationMode = amIn; Const aEaseType: TBZAnimationType = atLinear;
      Const aLerpType: TBZInterpolationType = itLinear; Const aDistorsion: Single = 0): TBZGradientColorItem; Virtual; Overload;
    { Creation d'une palette de "nbColors" couleurs depuis la définition des couleurs pour le dégradé }
    Procedure CreateGradientColors(nbColors: Integer);

    //procedure CreateGradientStopsFrom(Cols:Array of TBZColor);
    //procedure CreateGradientStopsFrom(Cols:TBZGradientColorList);

    { Retourne la couleur du dégradé à la postion "aTheta" (Compris entre 0 et 1 ) }
    Function GetGradientColor(aTheta: Single): TBZColor;

    { Retourne le nombre de couleur dans la palette. }
    Function ColorCount: Integer; Virtual;
    { Retourne  le nombre de couleur définis pour la creation d'un dégradé }
    Function GradientColorCount: Integer; Virtual;
    {  Creation d'une palette de couleur en dégradé }
    Function MakeGradientPalette(Const Nb: Integer = 256; Const AColorList: TBZColorList = nil): TBZColorList;
    // Function MakeGradientPalette(FromColor : TBZColor; ToColor : TBZColor; Const Nb:Integer=256;;Const AColorList : TBZColorList=nil);
    // Function MakeGradientPalette(aColors : Array of TBZColor; Const Nb:Integer=256;;Const AColorList : TBZColorList=nil);
    //function ColorExist(Const AColor:TBZColor):Boolean;

    { Recherche d'une couleur dans la palette par son nom }
    Function FindColor(ColorName: String; Var Index: Integer): Boolean;

    { Sauvegarde la palette de couleur dans un flux }
    Procedure SaveToStream(Const Stream: TStream);
    { Charge une palette de couleur depuis un flux }
    Procedure LoadFromStream(Const Stream: TStream);
    { Sauvegarde la palette de couleur dans un fichier }
    Procedure SaveToFile(Const FileName: String);
    { Charge une palette de couleur depuis un fichier }
    Procedure LoadFromFile(Const FileName: String);

    { Palette de couleur. }
    Property Palette: TBZColorList read FColors write SetColors;
    { Liste des couleur pour la génération d'un dégradé }
    Property GradientColors: TBZGradientColorList read FGradientColors write SetGradientColors;
    { Dégradé généré depuis GradientColors }
    Property GradientPalette: TBZColorList read FGradientPalette;

    {  Nom/Description de la palette }
    Property FriendlyName: String read FFriendlyName write FFriendlyName;
  Published
    // property Cadencer;
  End;

  { Classe de stockage des propriétés d'un dégradé. @br
    TODO : a Déplacer dans BZControlClasses }
  TBZGradientProperty = class(TBZUpdateAbleObject)
  private
    FColorSteps: TBZGradientColorList;
    FPalette : TBZColorList;
    FGradientStyle: TBZGradientKind;
    FGradientAngle: Single;
    FAutoAngle : Boolean;
    FMaxDist : Integer;

    // Procedure SetGradientStyle(AValue: TBZGradientStyle);
  public
    constructor Create; override;
    destructor Destroy; override;


    { Creation d'une palette en dégradé de "nbSteps" couleurs }
    procedure MakeGradientPalette(Const nbStep:integer);

    { Liste des couleurs de la peltte }
    property Palette : TBZColorList Read FPalette;
    {Definition de la liste des couleurs pour la génération du dégradé }
    property ColorSteps : TBZGradientColorList Read FColorSteps Write FColorSteps;
    { Type du dégradé : Horizontal, vertical, radial etc... }
    property Kind: TBZGradientKind Read FGradientStyle Write FGradientStyle;
    { Orientation du dégradé }
    property Angle: Single Read FGradientAngle Write FGradientAngle;
    { Choix de l'angle automatique }
    property AutoAngle : Boolean Read FAutoAngle Write FAutoAngle;
  end;

  { Pointeur vers un TBZColorRegisterEntry }
  PBZColorRegisterEntry = ^TBZColorRegisterEntry;
  { Données pour les couleurs enregistrée }
  TBZColorRegisterEntry = record
                  Name  : String[31];
                  Color : TBZColor;
                end;


  { Ordre de trie de la liste de couleurs enregistrée }
  TBZColorRegisterSortType = (crstHue, crstSaturation, crstValue, crstRed, crstGreen, crstBlue, crstIntensity);
  { Liste de couleurs enregistrées disponibles }
  TBZColorRegister = class(TList)
     public
        destructor Destroy; override;

        { Ajoute une couleur }
        procedure AddColor(const aName: String; const aColor: TBZColor);
        { Enumération des couleurs avec fonction Call-Back}
        procedure EnumColors(Proc: TGetStrProc); overload;
        { Enumération des couleurs dans un TStrings }
        procedure EnumColors(AValues: TStrings); overload;

        { Recherche une couleur en fonction de son nom }
        function  FindColor(const aName: String): TBZColor;

        { Retourne une couleur en fonction de son nom}
        function  GetColor(const aName: String): TBZColor;

        { Retourne le nom d'une couleur sous forme de chaine de caractère. @br
          Retourne : '<red green blue alpha>' }
        function  GetColorName(const aColor: TBZColor): String;

        { Retourne la couleur de l'index }
        function GetColorByIndex(Const Index : Integer) : TBZColor;

        { Retourne le nom de la couleur de l'index }
        function GetColorNameByIndex(Const Index : Integer) : String;

        { Enregistre les couleurs par défaut }
        procedure RegisterDefaultColors;

        { Efface une couleur de la liste }
        procedure RemoveColor(const aName: String);

        { Trie la liste ne fonction de TBZColorRegisterSortType }
        procedure Sort(SortType : TBZColorRegisterSortType); overload;
  end;

{ Retourne la liste des couleurs enregistrées }
function ColorRegister: TBZColorRegister;

{ Ajoute une couleur dans la liste des couleurs enregistrée }
procedure RegisterColor(const aName : String; const aColor : TBZColor);
{ Effece une couleur de la liste des couleurs enregistrées }
procedure UnRegisterColor(const aName : String);

{%region%=====[ Consantes de Couleurs ]==============================================}

Const
  { Constantes pour les couleurs du type TBZColor }
  clrAlphaOpaque = 255;
  clrAlphaTransparent = 0;

// color definitions
Const
  {$J+ - allow change of the following typed constants}

  clrScrollBar           : TBZColor = (X : 0; Y : 0; Z : 0; W : 255);
  clrBackground          : TBZColor = (X : 0; Y : 0; Z : 0; W : 255);
  clrActiveCaption       : TBZColor = (X : 0; Y : 0; Z : 0; W : 255);
  clrInactiveCaption     : TBZColor = (X : 0; Y : 0; Z : 0; W : 255);
  clrMenu                : TBZColor = (X : 0; Y : 0; Z : 0; W : 255);
  clrWindow              : TBZColor = (X : 0; Y : 0; Z : 0; W : 255);
  clrWindowFrame         : TBZColor = (X : 0; Y : 0; Z : 0; W : 255);
  clrMenuText            : TBZColor = (X : 0; Y : 0; Z : 0; W : 255);
  clrWindowText          : TBZColor = (X : 0; Y : 0; Z : 0; W : 255);
  clrCaptionText         : TBZColor = (X : 0; Y : 0; Z : 0; W : 255);
  clrActiveBorder        : TBZColor = (X : 0; Y : 0; Z : 0; W : 255);
  clrInactiveBorder      : TBZColor = (X : 0; Y : 0; Z : 0; W : 255);
  clrAppWorkSpace        : TBZColor = (X : 0; Y : 0; Z : 0; W : 255);
  clrHighlight           : TBZColor = (X : 0; Y : 0; Z : 0; W : 255);
  clrHighlightText       : TBZColor = (X : 0; Y : 0; Z : 0; W : 255);
  clrBtnFace             : TBZColor = (X : 0; Y : 0; Z : 0; W : 255);
  clrBtnShadow           : TBZColor = (X : 0; Y : 0; Z : 0; W : 255);
  clrGrayText            : TBZColor = (X : 0; Y : 0; Z : 0; W : 255);
  clrBtnText             : TBZColor = (X : 0; Y : 0; Z : 0; W : 255);
  clrInactiveCaptionText : TBZColor = (X : 0; Y : 0; Z : 0; W : 255);
  clrBtnHighlight        : TBZColor = (X : 0; Y : 0; Z : 0; W : 255);
  clr3DDkShadow          : TBZColor = (X : 0; Y : 0; Z : 0; W : 255);
  clr3DLight             : TBZColor = (X : 0; Y : 0; Z : 0; W : 255);
  clrInfoText            : TBZColor = (X : 0; Y : 0; Z : 0; W : 255);
  clrInfoBk              : TBZColor = (X : 0; Y : 0; Z : 0; W : 255);
  clrHotLight             : TBZColor = (X : 0; Y : 0; Z : 0; W : 255);
  clrGradientActiveCaption   : TBZColor = (X : 0; Y : 0; Z : 0; W : 255);
  clrGradientInactiveCaption : TBZColor = (X : 0; Y : 0; Z : 0; W : 255);
  clrMenuHighlight       : TBZColor = (X : 0; Y : 0; Z : 0; W : 255);
  clrMenuBar             : TBZColor = (X : 0; Y : 0; Z : 0; W : 255);
  clrForm                : TBZColor = (X : 0; Y : 0; Z : 0; W : 255);
  clrColorDesktop        : TBZColor = (X : 0; Y : 0; Z : 0; W : 255);
  clr3DFace              : TBZColor = (X : 0; Y : 0; Z : 0; W : 255);
  clr3DShadow            : TBZColor = (X : 0; Y : 0; Z : 0; W : 255);
  clr3DHiLight           : TBZColor = (X : 0; Y : 0; Z : 0; W : 255);
  clrBtnHiLight          : TBZColor = (X : 0; Y : 0; Z : 0; W : 255);

  clrMask     : TBZColor = (X : 255; Y : 255; Z : 255; W : 255);
  clrDontMask : TBZColor = (X :   0; Y :   0; Z :   0; W : 255);

  // sort of grays
  clrGray05 : TBZColor = (X :  13; Y :  13; Z :  13; W : 255);
  clrGray10 : TBZColor = (X :  26; Y :  26; Z :  26; W : 255);
  clrGray15 : TBZColor = (X :  38; Y :  38; Z :  38; W : 255);
  clrGray20 : TBZColor = (X :  51; Y :  51; Z :  51; W : 255);
  clrGray25 : TBZColor = (X :  64; Y :  64; Z :  64; W : 255);
  clrGray30 : TBZColor = (X :  77; Y :  77; Z :  77; W : 255);
  clrGray35 : TBZColor = (X :  89; Y :  89; Z :  89; W : 255);
  clrGray40 : TBZColor = (X : 102; Y : 102; Z : 102; W : 255);
  clrGray45 : TBZColor = (X : 115; Y : 115; Z : 115; W : 255);
  clrGray50 : TBZColor = (X : 128; Y : 128; Z : 128; W : 255);
  clrGray55 : TBZColor = (X : 140; Y : 140; Z : 140; W : 255);
  clrGray60 : TBZColor = (X : 153; Y : 153; Z : 153; W : 255);
  clrGray65 : TBZColor = (X : 166; Y : 166; Z : 166; W : 255);
  clrGray70 : TBZColor = (X : 179; Y : 179; Z : 179; W : 255);
  clrGray75 : TBZColor = (X : 192; Y : 192; Z : 192; W : 255);
  clrGray80 : TBZColor = (X : 204; Y : 204; Z : 204; W : 255);
  clrGray85 : TBZColor = (X : 217; Y : 217; Z : 217; W : 255);
  clrGray90 : TBZColor = (X : 230; Y : 230; Z : 230; W : 255);
  clrGray95 : TBZColor = (X : 242; Y : 242; Z : 242; W : 255);

  // other grays
  clrDimGray   : TBZColor = (X :  84; Y :  84; Z :  84; W : 255);
  clrLightGray : TBZColor = (X : 168; Y : 168; Z : 168; W : 255);

  {.IFDEF WINDOWS} // BGRA
  clrTransparent  : TBZColor = (X :   0; Y :   0; Z :   0; W : 0);
  clrBlack        : TBZColor = (X :   0; Y :   0; Z :   0; W : 255);
  clrBlue         : TBZColor = (X : 255; Y :   0; Z :   0; W : 255);
  clrRed          : TBZColor = (X :   0; Y :   0; Z : 255; W : 255);
  clrGreen        : TBZColor = (X :   0; Y : 128; Z :   0; W : 255);
  clrCyan         : TBZColor = (X : 255; Y : 255; Z :   0; W : 255);
  clrMagenta      : TBZColor = (X : 255; Y :   0; Z : 255; W : 255);
  clrYellow       : TBZColor = (X :   0; Y : 255; Z : 255; W : 255);
  clrWhite        : TBZColor = (X : 255; Y : 255; Z : 255; W : 255);
  clrGray         : TBZColor = (X : 128; Y : 128; Z : 128; W : 255);
  clrLtGray       : TBZColor = (X : 192; Y : 192; Z : 192; W : 255);
  clrOlive        : TBZColor = (X :   0; Y : 128; Z : 128; W : 255);
  clrNavy         : TBZColor = (X : 128; Y :   0; Z :   0; W : 255);
  clrPurple       : TBZColor = (X : 128; Y :   0; Z : 128; W : 255);
  clrFuchsia      : TBZColor = (X : 255; Y :   0; Z : 255; W : 255);
  clrTeal         : TBZColor = (X : 128; Y : 128; Z :   0; W : 255);
  clrSilver       : TBZColor = (X : 192; Y : 192; Z : 192; W : 255);
  clrMaroon       : TBZColor = (X :   0; Y :   0; Z : 128; W : 255);
  clrLime         : TBZColor = (X :   0; Y : 255; Z :   0; W : 255);
  clrLtGreen      : TBZColor = (X : 192; Y : 220; Z : 192; W : 255);
  clrAqua         : TBZColor = (X : 255; Y : 255; Z :   0; W : 255);

  // WinGraph
  clrAliceBlue            : TBZColor = (X : 255; Y : 248; Z : 240; W : 255);
  clrAlizarinCrimson      : TBZColor = (X :  54; Y :  38; Z : 227; W : 255);
  clrAmber                : TBZColor = (X :   0; Y : 191; Z : 255; W : 255);
  clrAmethyst             : TBZColor = (X : 204; Y : 102; Z : 153; W : 255);
  clrAntiqueWhite         : TBZColor = (X : 215; Y : 235; Z : 250; W : 255);
  clrAquamarine           : TBZColor = (X : 212; Y : 255; Z : 127; W : 255);
  clrAsparagus            : TBZColor = (X :  91; Y : 160; Z : 123; W : 255);
  clrAzure                : TBZColor = (X : 255; Y : 255; Z : 240; W : 255);
  clrBeige                : TBZColor = (X : 220; Y : 245; Z : 245; W : 255);
  clrBisque               : TBZColor = (X : 196; Y : 228; Z : 255; W : 255);
  clrBistre               : TBZColor = (X :  31; Y :  43; Z :  61; W : 255);
  clrBitterLemon          : TBZColor = (X :  13; Y : 224; Z : 202; W : 255);
  clrBlanchedAlmond       : TBZColor = (X : 205; Y : 235; Z : 255; W : 255);
  clrBlueViolet           : TBZColor = (X : 226; Y :  43; Z : 138; W : 255);
  clrBondiBlue            : TBZColor = (X : 182; Y : 149; Z :   0; W : 255);
  clrBrass                : TBZColor = (X :  66; Y : 166; Z : 181; W : 255);
  clrBrightGreen          : TBZColor = (X :   0; Y : 255; Z : 102; W : 255);
  clrBrightViolet         : TBZColor = (X : 205; Y :   0; Z : 205; W : 255);
  clrBronze               : TBZColor = (X :  50; Y : 127; Z : 205; W : 255);
  clrBrown                : TBZColor = (X :  42; Y :  42; Z : 165; W : 255);
  clrBuff                 : TBZColor = (X : 130; Y : 220; Z : 240; W : 255);
  clrBurgundy             : TBZColor = (X :  32; Y :   0; Z : 144; W : 255);
  clrBurlyWood            : TBZColor = (X : 135; Y : 184; Z : 222; W : 255);
  clrBurntOrange          : TBZColor = (X :   0; Y :  85; Z : 204; W : 255);
  clrBurntSienna          : TBZColor = (X :  81; Y : 116; Z : 233; W : 255);
  clrBurntUmber           : TBZColor = (X :  36; Y :  51; Z : 138; W : 255);
  clrCadetBlue            : TBZColor = (X : 160; Y : 158; Z :  95; W : 255);
  clrCamouflageGreen      : TBZColor = (X : 107; Y : 134; Z : 120; W : 255);
  clrCardinal             : TBZColor = (X :  58; Y :  30; Z : 196; W : 255);
  clrCarmine              : TBZColor = (X :  24; Y :   0; Z : 150; W : 255);
  clrCarrot               : TBZColor = (X :  33; Y : 145; Z : 237; W : 255);
  clrCasper               : TBZColor = (X : 209; Y : 190; Z : 173; W : 255);
  clrCerise               : TBZColor = (X :  99; Y :  49; Z : 222; W : 255);
  clrCerulean             : TBZColor = (X : 167; Y : 123; Z :   0; W : 255);
  clrCeruleanBlue         : TBZColor = (X : 190; Y :  82; Z :  42; W : 255);
  clrChartreuse           : TBZColor = (X :   0; Y : 255; Z : 127; W : 255);
  clrChocolate            : TBZColor = (X :  30; Y : 105; Z : 210; W : 255);
  clrCinnamon             : TBZColor = (X :   0; Y :  63; Z : 123; W : 255);
  clrCobalt               : TBZColor = (X : 171; Y :  71; Z :   0; W : 255);
  clrCopper               : TBZColor = (X :  51; Y : 115; Z : 184; W : 255);
  clrCoral                : TBZColor = (X :  80; Y : 127; Z : 255; W : 255);
  clrCorn                 : TBZColor = (X :  93; Y : 236; Z : 251; W : 255);
  clrCornflowerBlue       : TBZColor = (X : 237; Y : 149; Z : 100; W : 255);
  clrCornsilk             : TBZColor = (X : 220; Y : 248; Z : 255; W : 255);
  clrCream                : TBZColor = (X : 208; Y : 253; Z : 255; W : 255);
  clrCrimson              : TBZColor = (X :  60; Y :  20; Z : 220; W : 255);
  clrDarkBrown            : TBZColor = (X :  33; Y :  67; Z : 101; W : 255);
  clrDarkCerulean         : TBZColor = (X : 126; Y :  69; Z :   8; W : 255);
  clrDarkChestnut         : TBZColor = (X :  96; Y : 105; Z : 152; W : 255);
  clrDarkCoral            : TBZColor = (X :  69; Y :  91; Z : 205; W : 255);
  clrDarkCyan             : TBZColor = (X : 139; Y : 139; Z :   0; W : 255);
  clrDarkGoldenrod        : TBZColor = (X :  11; Y : 134; Z : 184; W : 255);
  clrDarkGray             : TBZColor = (X :  84; Y :  84; Z :  84; W : 255);
  clrDarkGreen            : TBZColor = (X :   0; Y : 100; Z :   0; W : 255);
  clrDarkIndigo           : TBZColor = (X :  98; Y :   0; Z :  49; W : 255);
  clrDarkKhaki            : TBZColor = (X : 107; Y : 183; Z : 189; W : 255);
  clrDarkOlive            : TBZColor = (X :  50; Y : 104; Z :  85; W : 255);
  clrDarkOliveGreen       : TBZColor = (X :  47; Y : 107; Z :  85; W : 255);
  clrDarkOrange           : TBZColor = (X :   0; Y : 140; Z : 255; W : 255);
  clrDarkOrchid           : TBZColor = (X : 204; Y :  50; Z : 153; W : 255);
  clrDarkPastelGreen      : TBZColor = (X :  60; Y : 192; Z :   3; W : 255);
  clrDarkPink             : TBZColor = (X : 128; Y :  84; Z : 231; W : 255);
  clrDarkRed              : TBZColor = (X :   0; Y :   0; Z : 139; W : 255);
  clrDarkSalmon           : TBZColor = (X : 122; Y : 150; Z : 233; W : 255);
  clrDarkScarlet          : TBZColor = (X :  25; Y :   3; Z :  86; W : 255);
  clrDarkSeaGreen         : TBZColor = (X : 143; Y : 188; Z : 143; W : 255);
  clrDarkSlateBlue        : TBZColor = (X : 139; Y :  61; Z :  72; W : 255);
  clrDarkSlateGray        : TBZColor = (X :  79; Y :  79; Z :  47; W : 255);
  clrDarkSpringGreen      : TBZColor = (X :  69; Y : 114; Z :  23; W : 255);
  clrDarkTan              : TBZColor = (X :  81; Y : 129; Z : 145; W : 255);
  clrDarkTeaGreen         : TBZColor = (X : 173; Y : 219; Z : 186; W : 255);
  clrDarkTerraCotta       : TBZColor = (X :  92; Y :  78; Z : 204; W : 255);
  clrDarkTurquoise        : TBZColor = (X : 209; Y : 206; Z :   0; W : 255);
  clrDarkViolet           : TBZColor = (X : 211; Y :   0; Z : 148; W : 255);
  clrDeepPink             : TBZColor = (X : 147; Y :  20; Z : 255; W : 255);
  clrDeepSkyBlue          : TBZColor = (X : 255; Y : 191; Z :   0; W : 255);
  clrDenim                : TBZColor = (X : 189; Y :  96; Z :  21; W : 255);
  clrDodgerBlue           : TBZColor = (X : 255; Y : 144; Z :  30; W : 255);
  clrEmerald              : TBZColor = (X : 120; Y : 200; Z :  80; W : 255);
  clrEggplant             : TBZColor = (X : 102; Y :   0; Z : 153; W : 255);
  clrFernGreen            : TBZColor = (X :  66; Y : 121; Z :  79; W : 255);
  clrFireBrick            : TBZColor = (X :  34; Y :  34; Z : 178; W : 255);
  clrFlax                 : TBZColor = (X : 130; Y : 220; Z : 238; W : 255);
  clrFloralWhite          : TBZColor = (X : 240; Y : 250; Z : 255; W : 255);
  clrForestGreen          : TBZColor = (X :  34; Y : 139; Z :  34; W : 255);
  clrFractal              : TBZColor = (X : 128; Y : 128; Z : 128; W : 255);
  clrGainsboro            : TBZColor = (X : 220; Y : 220; Z : 220; W : 255);
  clrGamboge              : TBZColor = (X :  15; Y : 155; Z : 228; W : 255);
  clrGhostWhite           : TBZColor = (X : 255; Y : 248; Z : 248; W : 255);
  clrGold                 : TBZColor = (X :   0; Y : 215; Z : 255; W : 255);
  clrGoldenrod            : TBZColor = (X :  32; Y : 165; Z : 218; W : 255);
  clrGrayAsparagus        : TBZColor = (X :  69; Y :  89; Z :  70; W : 255);
  clrGrayTeaGreen         : TBZColor = (X : 186; Y : 218; Z : 202; W : 255);
  clrGreenYellow          : TBZColor = (X :  47; Y : 255; Z : 173; W : 255);
  clrHeliotrope           : TBZColor = (X : 255; Y : 115; Z : 223; W : 255);
  clrHoneydew             : TBZColor = (X : 240; Y : 255; Z : 240; W : 255);
  clrHotPink              : TBZColor = (X : 180; Y : 105; Z : 255; W : 255);
  clrIndianRed            : TBZColor = (X :  92; Y :  92; Z : 205; W : 255);
  clrIndigo               : TBZColor = (X : 130; Y :   0; Z :  75; W : 255);
  clrInternationalKleinBlue: TBZColor = (X : 167; Y :  47; Z :   0; W : 255);
  clrInternationalOrange  : TBZColor = (X :   0; Y :  79; Z : 255; W : 255);
  clrIvory                : TBZColor = (X : 240; Y : 255; Z : 255; W : 255);
  clrJade                 : TBZColor = (X : 107; Y : 168; Z :   0; W : 255);
  clrKhaki                : TBZColor = (X : 140; Y : 230; Z : 240; W : 255);
  clrLavender             : TBZColor = (X : 250; Y : 230; Z : 230; W : 255);
  clrLavenderBlush        : TBZColor = (X : 245; Y : 240; Z : 255; W : 255);
  clrLawnGreen            : TBZColor = (X :   0; Y : 252; Z : 124; W : 255);
  clrLemon                : TBZColor = (X :  16; Y : 233; Z : 253; W : 255);
  clrLemonChiffon         : TBZColor = (X : 205; Y : 250; Z : 255; W : 255);
  clrLightBlue            : TBZColor = (X : 230; Y : 216; Z : 173; W : 255);
  clrLightBrown           : TBZColor = (X : 140; Y : 180; Z : 210; W : 255);
  clrLightCoral           : TBZColor = (X : 128; Y : 128; Z : 240; W : 255);
  clrLightCyan            : TBZColor = (X : 255; Y : 255; Z : 224; W : 255);
  clrLightGoldenrodYellow : TBZColor = (X : 210; Y : 250; Z : 250; W : 255);
  clrLightMagenta         : TBZColor = (X : 255; Y : 128; Z : 255; W : 255);
  clrLightRed             : TBZColor = (X : 128; Y : 128; Z : 255; W : 255);
  clrLightSalmon          : TBZColor = (X : 122; Y : 160; Z : 255; W : 255);
  clrLightSeaGreen        : TBZColor = (X : 170; Y : 178; Z :  32; W : 255);
  clrLightSkyBlue         : TBZColor = (X : 250; Y : 206; Z : 135; W : 255);
  clrLightSlateGray       : TBZColor = (X : 153; Y : 136; Z : 119; W : 255);
  clrLightSteelBlue       : TBZColor = (X : 222; Y : 196; Z : 176; W : 255);
  clrLightYellow          : TBZColor = (X : 224; Y : 255; Z : 255; W : 255);
  clrLilac                : TBZColor = (X : 200; Y : 162; Z : 200; W : 255);
  clrLimeGreen            : TBZColor = (X :  50; Y : 205; Z :  50; W : 255);
  clrLinen                : TBZColor = (X : 230; Y : 240; Z : 250; W : 255);
  clrMalachite            : TBZColor = (X :  81; Y : 218; Z :  11; W : 255);
  clrMauve                : TBZColor = (X : 255; Y : 176; Z : 224; W : 255);
  clrMediumAquamarine     : TBZColor = (X : 170; Y : 205; Z : 102; W : 255);
  clrMediumBlue           : TBZColor = (X : 205; Y :   0; Z :   0; W : 255);
  clrMediumOrchid         : TBZColor = (X : 211; Y :  85; Z : 186; W : 255);
  clrMediumSeaGreen       : TBZColor = (X : 113; Y : 179; Z :  60; W : 255);
  clrMediumSlateBlue      : TBZColor = (X : 238; Y : 104; Z : 123; W : 255);
  clrMediumSpringGreen    : TBZColor = (X : 154; Y : 250; Z :   0; W : 255);
  clrMediumTurquoise      : TBZColor = (X : 204; Y : 209; Z :  72; W : 255);
  clrMediumVioletRed      : TBZColor = (X : 133; Y :  21; Z : 199; W : 255);
  clrMidnightBlue         : TBZColor = (X : 112; Y :  25; Z :  25; W : 255);
  clrMintCream            : TBZColor = (X : 250; Y : 255; Z : 245; W : 255);
  clrMistyRose            : TBZColor = (X : 225; Y : 228; Z : 255; W : 255);
  clrMoccasin             : TBZColor = (X : 181; Y : 228; Z : 255; W : 255);
  clrMoneyGreen           : TBZColor = (X : 192; Y : 220; Z : 192; W : 255);
  clrMonza                : TBZColor = (X :  30; Y :   3; Z : 199; W : 255);
  clrMossGreen            : TBZColor = (X : 173; Y : 223; Z : 173; W : 255);
  clrMountbattenPink      : TBZColor = (X : 141; Y : 122; Z : 153; W : 255);
  clrMustard              : TBZColor = (X :  88; Y : 219; Z : 255; W : 255);
  clrNavajoWhite          : TBZColor = (X : 173; Y : 222; Z : 255; W : 255);
  clrOchre                : TBZColor = (X :  34; Y : 119; Z : 204; W : 255);
  clrOldGold              : TBZColor = (X :  59; Y : 181; Z : 207; W : 255);
  clrOldLace              : TBZColor = (X : 230; Y : 245; Z : 253; W : 255);
  clrOliveDrab            : TBZColor = (X :  35; Y : 142; Z : 107; W : 255);
  clrOrange               : TBZColor = (X :   0; Y : 165; Z : 255; W : 255);
  clrOrangeRed            : TBZColor = (X :   0; Y :  69; Z : 255; W : 255);
  clrOrchid               : TBZColor = (X : 214; Y : 112; Z : 218; W : 255);
  clrPaleBrown            : TBZColor = (X :  84; Y : 118; Z : 152; W : 255);
  clrPaleCarmine          : TBZColor = (X :  53; Y :  64; Z : 175; W : 255);
  clrPaleChestnut         : TBZColor = (X : 175; Y : 173; Z : 221; W : 255);
  clrPaleCornflowerBlue   : TBZColor = (X : 239; Y : 205; Z : 171; W : 255);
  clrPaleGoldenrod        : TBZColor = (X : 170; Y : 232; Z : 238; W : 255);
  clrPaleGreen            : TBZColor = (X : 152; Y : 251; Z : 152; W : 255);
  clrPaleMagenta          : TBZColor = (X : 229; Y : 132; Z : 249; W : 255);
  clrPaleMauve            : TBZColor = (X : 102; Y : 102; Z : 153; W : 255);
  clrPalePink             : TBZColor = (X : 221; Y : 218; Z : 250; W : 255);
  clrPaleSandyBrown       : TBZColor = (X : 171; Y : 189; Z : 218; W : 255);
  clrPaleTurquoise        : TBZColor = (X : 238; Y : 238; Z : 175; W : 255);
  clrPaleVioletRed        : TBZColor = (X : 147; Y : 112; Z : 219; W : 255);
  clrPapayaWhip           : TBZColor = (X : 213; Y : 239; Z : 255; W : 255);
  clrPastelGreen          : TBZColor = (X : 119; Y : 221; Z : 119; W : 255);
  clrPastelPink           : TBZColor = (X : 220; Y : 209; Z : 255; W : 255);
  clrPeach                : TBZColor = (X : 180; Y : 229; Z : 255; W : 255);
  clrPeachOrange          : TBZColor = (X : 153; Y : 204; Z : 255; W : 255);
  clrPeachPuff            : TBZColor = (X : 185; Y : 218; Z : 255; W : 255);
  clrPeachYellow          : TBZColor = (X : 173; Y : 223; Z : 250; W : 255);
  clrPear                 : TBZColor = (X :  49; Y : 226; Z : 209; W : 255);
  clrPeriwinkle           : TBZColor = (X : 255; Y : 204; Z : 204; W : 255);
  clrPersianBlue          : TBZColor = (X : 255; Y :   0; Z : 102; W : 255);
  clrPeru                 : TBZColor = (X :  63; Y : 133; Z : 205; W : 255);
  clrPineGreen            : TBZColor = (X : 111; Y : 121; Z :   1; W : 255);
  clrPink                 : TBZColor = (X : 203; Y : 192; Z : 255; W : 255);
  clrPinkOrange           : TBZColor = (X : 102; Y : 153; Z : 255; W : 255);
  clrPlum                 : TBZColor = (X : 221; Y : 160; Z : 221; W : 255);
  clrPowderBlue           : TBZColor = (X : 230; Y : 224; Z : 176; W : 255);
  clrPrussianBlue         : TBZColor = (X :  83; Y :  49; Z :   0; W : 255);
  clrPuce                 : TBZColor = (X : 153; Y : 136; Z : 204; W : 255);
  clrPumpkin              : TBZColor = (X :  24; Y : 117; Z : 255; W : 255);
  clrRawUmber             : TBZColor = (X :  18; Y :  74; Z : 115; W : 255);
  clrReef                 : TBZColor = (X : 162; Y : 255; Z : 201; W : 255);
  clrRobinEggBlue         : TBZColor = (X : 204; Y : 204; Z :   0; W : 255);
  clrRosyBrown            : TBZColor = (X : 143; Y : 143; Z : 188; W : 255);
  clrRoyalBlue            : TBZColor = (X : 225; Y : 105; Z :  65; W : 255);
  clrRusset               : TBZColor = (X :  27; Y :  70; Z : 128; W : 255);
  clrRust                 : TBZColor = (X :  14; Y :  65; Z : 183; W : 255);
  clrSaddleBrown          : TBZColor = (X :  19; Y :  69; Z : 139; W : 255);
  clrSaffron              : TBZColor = (X :  48; Y : 196; Z : 244; W : 255);
  clrSalmon               : TBZColor = (X : 114; Y : 128; Z : 250; W : 255);
  clrSandyBrown           : TBZColor = (X :  96; Y : 164; Z : 244; W : 255);
  clrSangria              : TBZColor = (X :  10; Y :   0; Z : 146; W : 255);
  clrSapphire             : TBZColor = (X : 103; Y :  37; Z :   8; W : 255);
  clrScarlet              : TBZColor = (X :   0; Y :  36; Z : 255; W : 255);
  clrSeaGreen             : TBZColor = (X :  87; Y : 139; Z :  46; W : 255);
  clrSeaShell             : TBZColor = (X : 238; Y : 245; Z : 255; W : 255);
  clrSepia                : TBZColor = (X :  20; Y :  66; Z : 112; W : 255);
  clrSienna               : TBZColor = (X :  45; Y :  82; Z : 160; W : 255);
  clrSkyBlue              : TBZColor = (X : 235; Y : 206; Z : 135; W : 255);
  clrSlateBlue            : TBZColor = (X : 205; Y :  90; Z : 106; W : 255);
  clrSlateGray            : TBZColor = (X : 144; Y : 128; Z : 112; W : 255);
  clrSnow                 : TBZColor = (X : 250; Y : 250; Z : 255; W : 255);
  clrSpringGreen          : TBZColor = (X : 127; Y : 255; Z :   0; W : 255);
  clrSteelBlue            : TBZColor = (X : 180; Y : 130; Z :  70; W : 255);
  clrSwampGreen           : TBZColor = (X : 142; Y : 183; Z : 172; W : 255);
  clrTaupe                : TBZColor = (X : 126; Y : 152; Z : 188; W : 255);
  clrTangerine            : TBZColor = (X :   0; Y : 204; Z : 255; W : 255);
  clrTeaGreen             : TBZColor = (X : 192; Y : 240; Z : 208; W : 255);
  clrTenne                : TBZColor = (X :   0; Y :  87; Z : 205; W : 255);
  clrTerraCotta           : TBZColor = (X :  91; Y : 114; Z : 226; W : 255);
  clrThistle              : TBZColor = (X : 216; Y : 191; Z : 216; W : 255);
  clrTomato               : TBZColor = (X :  71; Y :  99; Z : 255; W : 255);
  clrTurquoise            : TBZColor = (X : 208; Y : 224; Z :  64; W : 255);
  clrUltramarine          : TBZColor = (X : 143; Y :  10; Z :  18; W : 255);
  clrVermilion            : TBZColor = (X :   0; Y :  77; Z : 255; W : 255);
  clrViolet               : TBZColor = (X : 238; Y : 130; Z : 238; W : 255);
  clrVioletEggplant       : TBZColor = (X : 153; Y :  17; Z : 153; W : 255);
  clrViridian             : TBZColor = (X : 109; Y : 130; Z :  64; W : 255);
  clrWheat                : TBZColor = (X : 179; Y : 222; Z : 245; W : 255);
  clrWhiteSmoke           : TBZColor = (X : 245; Y : 245; Z : 245; W : 255);
  clrWisteria             : TBZColor = (X : 220; Y : 160; Z : 201; W : 255);
  clrYellowGreen          : TBZColor = (X :  50; Y : 205; Z : 154; W : 255);
  clrZinnwaldite          : TBZColor = (X : 175; Y : 194; Z : 235; W : 255);

  // colors en masse
  clrCoral2             : TBZColor = (X :   0; Y : 127; Z : 255; W : 255);
  clrCornflowerBlue2    : TBZColor = (X : 111; Y :  66; Z :  66; W : 255);
  clrDarkGreen2         : TBZColor = (X :  47; Y :  79; Z :  47; W : 255);
  clrDarkOliveGreen2    : TBZColor = (X :  47; Y :  79; Z :  79; W : 255);
  clrDarkSlateBlue2     : TBZColor = (X : 142; Y :  35; Z : 107; W : 255);
  clrDarkSlateGray2     : TBZColor = (X :  79; Y :  79; Z :  47; W : 255);
  clrDarkSlateGrey     : TBZColor = (X :  79; Y :  79; Z :  47; W : 255);
  clrDarkTurquoise2     : TBZColor = (X : 219; Y : 147; Z : 112; W : 255);
  clrFirebrick2         : TBZColor = (X :  35; Y :  35; Z : 142; W : 255);
  clrGold2              : TBZColor = (X :  50; Y : 127; Z : 204; W : 255);
  clrGoldenrod2         : TBZColor = (X : 112; Y : 219; Z : 219; W : 255);
  clrGreenYellow2       : TBZColor = (X : 112; Y : 219; Z : 147; W : 255);
  clrIndian            : TBZColor = (X :  47; Y :  47; Z :  79; W : 255);
  clrKhaki2             : TBZColor = (X :  95; Y : 159; Z : 159; W : 255);
  clrLightBlue2         : TBZColor = (X : 216; Y : 216; Z : 191; W : 255);
  clrLightSteelBlue2    : TBZColor = (X : 188; Y : 143; Z : 143; W : 255);
  clrLimeGreen2         : TBZColor = (X :  50; Y : 204; Z :  50; W : 255);
  clrMediumAquamarine2  : TBZColor = (X : 153; Y : 204; Z :  50; W : 255);
  clrMediumBlue2        : TBZColor = (X : 204; Y :  50; Z :  50; W : 255);
  clrMediumForestGreen : TBZColor = (X :  35; Y : 142; Z : 107; W : 255);
  clrMediumGoldenrod   : TBZColor = (X : 173; Y : 234; Z : 234; W : 255);
  clrMediumOrchid2      : TBZColor = (X : 219; Y : 112; Z : 147; W : 255);
  clrMediumSeaGreen2    : TBZColor = (X :  66; Y : 111; Z :  66; W : 255);
  clrMediumSlateBlue2   : TBZColor = (X : 255; Y :   0; Z : 127; W : 255);
  clrMediumSpringGreen2 : TBZColor = (X :   0; Y : 255; Z : 127; W : 255);
  clrMediumTurquoise2   : TBZColor = (X : 219; Y : 219; Z : 112; W : 255);
  clrMediumViolet      : TBZColor = (X : 147; Y : 112; Z : 219; W : 255);
  clrMidnightBlue2      : TBZColor = (X :  79; Y :  47; Z :  47; W : 255);
  clrNavyBlue          : TBZColor = (X : 142; Y :  35; Z :  35; W : 255);
  clrOrange2            : TBZColor = (X :   0; Y : 128; Z : 255; W : 255);
  clrPink2              : TBZColor = (X : 143; Y : 143; Z : 188; W : 255);
  clrSalmon2            : TBZColor = (X :  66; Y :  66; Z : 111; W : 255);
  clrSeaGreen2          : TBZColor = (X : 107; Y : 142; Z :  35; W : 255);
  clrSienna2            : TBZColor = (X :  35; Y : 107; Z : 142; W : 255);
  clrSkyBlue2           : TBZColor = (X : 204; Y : 153; Z :  50; W : 255);
  clrSlateBlue2         : TBZColor = (X : 255; Y : 127; Z :   0; W : 255);
  clrSteelBlue2         : TBZColor = (X : 142; Y : 107; Z :  35; W : 255);
  clrTan               : TBZColor = (X : 112; Y : 147; Z : 219; W : 255);
  clrThistle2           : TBZColor = (X : 216; Y : 191; Z : 216; W : 255);
  clrTurquoise2         : TBZColor = (X : 234; Y : 234; Z : 173; W : 255);
  clrViolet2            : TBZColor = (X :  79; Y :  47; Z :  79; W : 255);
  clrVioletRed         : TBZColor = (X : 153; Y :  50; Z : 204; W : 255);
  clrWheat2             : TBZColor = (X : 191; Y : 216; Z : 216; W : 255);
  clrYellowGreen2       : TBZColor = (X :  50; Y : 204; Z : 153; W : 255);
  clrSummerSky         : TBZColor = (X : 222; Y : 176; Z :  56; W : 255);
  clrRichBlue          : TBZColor = (X : 171; Y :  89; Z :  89; W : 255);
  clrBrass2             : TBZColor = (X :  66; Y : 166; Z : 181; W : 255);
  clrCopper2            : TBZColor = (X :  51; Y : 115; Z : 184; W : 255);
  clrBronze2            : TBZColor = (X :  36; Y : 120; Z : 140; W : 255);
  clrBronze3           : TBZColor = (X :  61; Y : 125; Z : 166; W : 255);
  clrBrightGold        : TBZColor = (X :  26; Y : 217; Z : 217; W : 255);
  clrOldGold2           : TBZColor = (X :  59; Y : 181; Z : 207; W : 255);
  clrFeldspar          : TBZColor = (X : 117; Y : 145; Z : 209; W : 255);
  clrQuartz            : TBZColor = (X : 242; Y : 217; Z : 217; W : 255);
  clrNeonPink          : TBZColor = (X : 199; Y : 110; Z : 255; W : 255);
  clrDarkPurple        : TBZColor = (X : 120; Y :  31; Z : 135; W : 255);
  clrNeonBlue          : TBZColor = (X : 255; Y :  77; Z :  77; W : 255);
  clrCoolCopper        : TBZColor = (X :  26; Y : 135; Z : 217; W : 255);
  clrMandarinOrange    : TBZColor = (X :  51; Y : 120; Z : 227; W : 255);
  clrLightWood         : TBZColor = (X : 166; Y : 194; Z : 232; W : 255);
  clrMediumWood        : TBZColor = (X :  99; Y : 128; Z : 166; W : 255);
  clrDarkWood          : TBZColor = (X :  66; Y :  94; Z : 133; W : 255);
  clrSpicyPink         : TBZColor = (X : 173; Y :  28; Z : 255; W : 255);
  clrSemiSweetChoc     : TBZColor = (X :  38; Y :  66; Z : 107; W : 255);
  clrBakersChoc        : TBZColor = (X :  23; Y :  51; Z :  92; W : 255);
  clrFlesh             : TBZColor = (X : 176; Y : 204; Z : 245; W : 255);
  clrNewTan            : TBZColor = (X : 158; Y : 199; Z : 235; W : 255);
  clrNewMidnightBlue   : TBZColor = (X : 156; Y :   0; Z :   0; W : 255);
  clrVeryDarkBrown     : TBZColor = (X :  36; Y :  41; Z :  89; W : 255);
  clrDarkBrown2         : TBZColor = (X :  51; Y :  64; Z :  92; W : 255);
  clrDarkTan2           : TBZColor = (X :  79; Y : 105; Z : 150; W : 255);
  clrGreenCopper       : TBZColor = (X : 117; Y : 125; Z :  82; W : 255);
  clrDkGreenCopper     : TBZColor = (X : 110; Y : 117; Z :  74; W : 255);
  clrDustyRose         : TBZColor = (X :  99; Y :  99; Z : 133; W : 255);
  clrHuntersGreen      : TBZColor = (X :  79; Y :  94; Z :  33; W : 255);
  clrScarlet2           : TBZColor = (X :  23; Y :  23; Z : 140; W : 255);
  clrMediumPurple2      : TBZColor = (X : 245; Y :  41; Z : 186; W : 255);
  clrLightPurple       : TBZColor = (X : 250; Y : 148; Z : 222; W : 255);
  clrVeryLightPurple   : TBZColor = (X : 252; Y : 207; Z : 240; W : 255);

  {.$ELSE}
  {.$ENDIF}

  {$J- - disallow change of the following typed constants}

  { Définition des facteurs de fusion source et destination pour les fonctions de dessin avec fusion en chaine de caractères. }
  cBlendingFactorStr : array[0..10] of string = ('Ignore','Zero','One', 'Source Alpha', 'Source Alpha Inverse', 'Destination Alpha', 'Destination Alpha Inverse',
                                                 'Source Couleur', 'Source Couleur Inverse', 'Destination Couleur', 'Destination Couleur Inverse');

  { Définition en chaine de caractères des modes de combinaison }
  cCombineDrawModeStr : Array[0..52] of String = ('Normal', 'Addition', 'Substract', 'RealSubstract', 'Multiply', 'Divide', 'Or', 'Xor', 'And', 'Difference', 'Average',
                                                  'Overlay', 'Screen', 'Stamp', 'Heat', 'Freeze', 'Glow', 'Reflect', 'Exclusion', 'Negate', 'Lighten', 'Darken',
                                                  'ColorBurn', 'SoftColorBurn', 'InverseColorBurn', 'ColorDodge', 'SoftColorDodge', 'InverseColorDodge',
                                                  'Interpolation', 'HardLight', 'SoftLight', 'BrightLight', 'Linear Light', 'Vivid Light', 'Pin Light', 'Hard Mix',
                                                  'Teinte', 'Color', 'Value', 'Saturation', 'Luminosity', 'Phoenix', 'GrainMerge', 'GrainExtract',
                                                  'Red', 'Green', 'Blue', 'Dissolve', 'Erase', 'Min', 'Max', 'Amplitude', 'Custom');

  { Matrice de conversion du sous-espace de couleur HSL par défaut }
  cssHSL_Default      : TBZColorSubSpace = (tA:360; tB:100; tC:100; CS : csHSL);
  { Matrice de conversion du sous-espace de couleur HSL utilisé par PaintShopPro }
  cssHSL_PaintShopPro : TBZColorSubSpace = (tA:255; tB:255; tC:255; CS : csHSL);
  { Matrice de conversion du sous-espace de couleur HSL utilisé par Windows }
  cssHSL_Windows      : TBZColorSubSpace = (tA:240; tB:240; tC:240; CS : csHSL);

  { Matrice de conversion du sous-espace de couleur HSV par défaut }
  cssHSV_Default      : TBZColorSubSpace = (tA:360; tB:100; tC:100; CS : csHSV);
  { Matrice de conversion du sous-espace de couleur HSV utilisé par OpenCV }
  cssHSV_OpenCV       : TBZColorSubSpace = (tA:180; tB:255; tC:255; CS : csHSV);
  { Matrice de conversion du sous-espace de couleur HSV utilisé par Gimp }
  cssHSV_Gimp         : TBZColorSubSpace = (tA:360; tB:100; tC:100; CS : csHSV);
  { Matrice de conversion du sous-espace de couleur HSV utilisé par Photoshop }
  cssHSV_Photoshop    : TBZColorSubSpace = (tA:360; tB:100; tC:100; CS : csHSV);
  { Matrice de conversion du sous-espace de couleur HSV utilisé par KDE }
  cssHSV_LinuxKDE     : TBZColorSubSpace = (tA:360; tB:255; tC:255; CS : csHSV);
  { Matrice de conversion du sous-espace de couleur HSV utilisé par GTK }
  cssHSV_GTK          : TBZColorSubSpace = (tA:360; tB:1;   tC:1;   CS : csHSV);
  { Matrice de conversion du sous-espace de couleur HSV utilisé par Java AWT }
  cssHSV_JavaAWT      : TBZColorSubSpace = (tA:1;   tB:1;   tC:1;   CS : csHSV);
  { Matrice de conversion du sous-espace de couleur HSV utilisé par Apple }
  cssHSV_Apple        : TBZColorSubSpace = (tA:360; tB:100; tC:100; CS : csHSV);

  { Matrice de conversion couleur vers niveaux de gris NTSC }
  cGCM_NTSC              : TBZGrayMatrixConv = (red:0.299; green:0.587; blue:0.114);
  { Matrice de conversion couleur vers niveaux de gris JPEG }
  cGCM_JPEG              : TBZGrayMatrixConv = (red:0.299; green:0.587; blue:0.114);
  { Matrice de conversion couleur vers niveaux "moyen" de gris }
  cGCM_Average           : TBZGrayMatrixConv = (red:0.334; green:0.333; blue:0.333);
  { Matrice de conversion couleur vers niveaux de gris Photoshop }
  cGCM_Photoshop         : TBZGrayMatrixConv = (red:0.213; green:0.715; blue:0.072);
  { Matrice de conversion couleur vers niveaux de gris CIEObserverRef709 }
  cGCM_CIEObserverRef709 : TBZGrayMatrixConv = (red:0.2126; green:0.7152;blue:0.0722);
  { Matrice de conversion couleur vers niveaux de gris RMY }
  cGCM_RMY               : TBZGrayMatrixConv = (Red: 0.5; Green: 0.419; Blue: 0.081);
  { Matrice de conversion couleur vers niveaux de gris en fonction de la composante Rouge }
  cGCM_RedMask           : TBZGrayMatrixConv = (Red: 1.0; Green: 0.0; Blue: 0.0);
  { Matrice de conversion couleur vers niveaux de gris en fonction de la composante Vert }
  cGCM_GreenMask         : TBZGrayMatrixConv = (Red: 0.0; Green: 1.0; Blue: 0.0);
  { Matrice de conversion couleur vers niveaux de gris en fonction de la composante Bleu }
  cGCM_BlueMask          : TBZGrayMatrixConv = (Red: 0.0; Green: 0.0; Blue: 1.0);
  { Matrice de conversion couleur vers niveaux de gris en fonction de la composante Alpha }
  cGCM_AlphaMask         : TBZGrayMatrixConv = (Red: 0.334; Green: 0.333; Blue: 0.0333);

  { Type de conversion _ Espace de Couleur _ Reference Point Blanc
   Les matrices ci-dessous sont les matrices adaptés par Bradford. Et autres tel que YIQ, YUV }

  { Matrice de conversion d'un format RGB vers le profil Adobe RGB D50 }
  RGBtoXYZ_AdobeRGB_D50 : TBZColorSpaceMatrixConversion = ( 0.6097559, 0.2052401,  0.1492240,
                                                            0.3111242, 0.6256560,  0.0632197,
                                                            0.0194811, 0.0608902,  0.7448387);
  { Matrice de conversion d'un profil Adobe RGB D50 vers le format RGB  }
  XYZtoRGB_AdobeRGB_D50 : TBZColorSpaceMatrixConversion = (  1.9624274, -0.6105343, -0.3413404,
                                                            -0.9787684,  1.9161415,  0.0334540,
                                                             0.0286869, -0.1406752,  1.3487655);

  { Matrice de conversion d'un format RGB vers le profil Apple RGB D50 }
  RGBtoXYZ_AppleRGB_D50 : TBZColorSpaceMatrixConversion = (0.4755678,  0.3396722,  0.1489800,
                                                           0.2551812,  0.6725693,  0.0722496,
                                                           0.0184697,  0.1133771,  0.6933632);
  { Matrice de conversion d'un profil Apple RGB D50 vers le format RGB  }
  XYZtoRGB_AppleRGB_D50 : TBZColorSpaceMatrixConversion = (2.8510695, -1.3605261, -0.4708281,
                                                          -1.0927680,  2.0348871,  0.0227598,
                                                           0.1027403, -0.2964984,  1.4510659);

  { Matrice de conversion d'un format RGB vers le profil Bruce RGB D50 }
  RGBToXYZ_BruceRGB_D50 : TBZColorSpaceMatrixConversion = (0.4941816, 0.3204834, 0.1495550,
                                                           0.2521531, 0.6844869, 0.0633600,
                                                           0.0157886, 0.0629304, 0.7464909);
  { Matrice de conversion d'un profil Bruce RGB D50 vers le format RGB  }
  XYZtoRGB_BruceRGB_D50 : TBZColorSpaceMatrixConversion = (2.6502856, -1.2014485, -0.4289936,
                                                          -0.9787684,  1.9161415,  0.0334540,
                                                           0.0264570, -0.1361227,  1.3458542);

  { Matrice de conversion d'un format RGB vers le profil CIE RGB D50 }
  RGBtoXYZ_CIERGB_D50 : TBZColorSpaceMatrixConversion = (0.4868870, 0.3062984, 0.1710347,
                                                         0.1746583, 0.8247541, 0.0005877,
                                                        -0.0012563, 0.0169832, 0.8094831);

  { Matrice de conversion d'un profil CIE RGB D50 vers le format RGB  }
  XYZtoRGB_CIERGB_D50 : TBZColorSpaceMatrixConversion = (2.3638081, -0.8676030, -0.4988161,
                                                        -0.5005940,  1.3962369,  0.1047562,
                                                         0.0141712, -0.0306400,  1.2323842);

  { Matrice de conversion d'un format RGB vers le profil NTSC RGB D50 }
  RGBtoXYZ_NTSCRGB_D50 : TBZColorSpaceMatrixConversion = (0.6343706, 0.1852204, 0.1446290,
                                                          0.3109496, 0.5915984, 0.0974520,
                                                         -0.0011817, 0.0555518, 0.7708399);
  { Matrice de conversion d'un profil NTSC RGB D50 vers le format RGB  }
  XYZtoRGB_NTSCRGB_D50 : TBZColorSpaceMatrixConversion = (1.8464881, -0.5521299, -0.2766458,
                                                         -0.9826630,  2.0044755, -0.0690396,
                                                          0.0736477, -0.1453020,  1.3018376);

  { Matrice de conversion d'un format RGB vers le profil PAL/SECAM D50 }
  RGBtoXYZ_PALSECAMRGB_D50 : TBZColorSpaceMatrixConversion = (0.4552773, 0.3675500, 0.1413926,
                                                              0.2323025, 0.7077956, 0.0599019,
                                                              0.0145457, 0.1049154, 0.7057489);
  { Matrice de conversion d'un profil PAL/SECAM RGB D50 vers le format RGB  }
  XYZtoRGB_PALSECAMRGB_D50 : TBZColorSpaceMatrixConversion = (2.9603944, -1.4678519, -0.4685105,
                                                             -0.9787684,  1.9161415,  0.0334540,
                                                              0.0844874, -0.2545973,  1.4216174);

  { Matrice de conversion d'un format RGB vers le profil SMPTE-C RGB D50 }
  RGBtoXYZ_SMPTECRGB_D50 : TBZColorSpaceMatrixConversion = (0.4163290, 0.3931464, 0.1547446,
                                                            0.2216999, 0.7032549, 0.0750452,
                                                            0.0136576, 0.0913604, 0.7201920);
  { Matrice de conversion d'un profil SMPTE-C RGB D50 vers le format RGB  }
  XYZtoRGB_SMPTECRGB_D50 : TBZColorSpaceMatrixConversion = (3.3921940, -1.8264027, -0.5385522,
                                                           -1.0770996,  2.0213975,  0.0207989,
                                                            0.0723073, -0.2217902,  1.3960932);

  { Matrice de conversion d'un format RGB vers le profil sRGB D50 }
  RGBtoXYZ_sRGB_D50 : TBZColorSpaceMatrixConversion = (0.4360747, 0.3850649, 0.1430804,
                                                       0.2225045, 0.7168786, 0.0606169,
                                                       0.0139322, 0.0971045, 0.7141733);
  { Matrice de conversion d'un profil sRGB D50 vers le format RGB  }
  XYZtoRGB_sRGB_D50 : TBZColorSpaceMatrixConversion = (3.1338561, -1.6168667, -0.4906146,
                                                      -0.9787684,  1.9161415,  0.0334540,
                                                       0.0719453, -0.2289914,  1.4052427);

  { Matrice de conversion d'un format RGB vers le profil sRGB D65
    https://en.wikipedia.org/wiki/SRGB#Specification_of_the_transformation}
  RGBtoXYZ_sRGB_D65 : TBZColorSpaceMatrixConversion = (0.4124,  0.3576,  0.1805,
                                                       0.2126,  0.7152,  0.0722,
                                                       0.0193,  0.1192,  0.9504);
  { Matrice de conversion d'un profil sRGB D65 vers le format RGB  }
  XYZtoRGB_sRGB_D65 : TBZColorSpaceMatrixConversion = (3.2406, -1.5372, -0.4986,
                                                      -0.9689,  1.8758,  0.0415,
                                                       0.0557, -0.2040,  1.0570);

  { Matrice de conversion d'un format RGB vers le profil YIQ }
  RGBtoXYZ_YIQ : TBZColorSpaceMatrixConversion = (0.299,  0.587,  0.114,
                                                  0.596, -0.274, -0.322,
                                                  0.211, -0.523,  0.312);
  { Matrice de conversion d'un profil YIQ vers le format RGB  }
  XYZtoRGB_YIQ : TBZColorSpaceMatrixConversion = (1.0,  0.956,  0.621,
                                                  1.0, -0.272, -0.647,
                                                  1.0, -1.106,  1.703);

  { Matrice de conversion d'un format RGB vers le profil SDTV YUV }
  RGBtoXYZ_SDTV_YUV : TBZColorSpaceMatrixConversion = (0.299,    0.587,    0.114,
                                                      -0.14713, -0.28886,  0.436,
                                                       0.615,   -0.51499, -0.10001);
  { Matrice de conversion d'un profil SDTV YUV vers le format RGB  }
  XYZtoRGB_SDTV_YUV : TBZColorSpaceMatrixConversion = (1.0,  0.0,      1.13983,
                                                       1.0, -0.39465, -0.58060,
                                                       1.0,  2.03211,  0.0);

  { Matrice de conversion d'un format RGB vers le profil HDTV YUV }
  RGBtoXYZ_HDTV_YUV : TBZColorSpaceMatrixConversion = (0.2126,   0.7152,   0.0722,
                                                      -0.09991, -0.33906,  0.436,
                                                       0.615,   -0.55861, -0.05639);
  { Matrice de conversion d'un profil HDTV YUV vers le format RGB  }
  XYZtoRGB_HDTV_YUV : TBZColorSpaceMatrixConversion = (1.0,  0.0,      1.28033,
                                                       1.0, -0.21482, -0.38059,
                                                       1.0,  2.12798,  0.0);

  { Descriptions et définitions des information sur les formats de couleurs supportés }
  BZColorFormatDesc:array[cfMono..cfXBGR] of TBZColorFormatInfosRec = (
   ( name : 'Mono';
     SupportedPixelFormat : [pf1Bit, pf8bits];
     DefaultPixelFormat : pf8bits;
     RedPrec : 1; GreenPrec:1; BluePrec:1; AlphaPrec:1;
     RedMask:$FF000000; GreenMask:$FF000000; BlueMask:$FF000000; AlphaMask:$FF000000;
     RedShift:0; GreenShift:8; BlueShit:16; AlphaShift:24;
     BitsPerPixel : 1;
     PixelSize : 1;
     SwapRB : False;
     SwapAlpha : False;
   ),
   ( name : 'Indexed';
     SupportedPixelFormat : [pf8Bits, pf16bits, pf32bits, pf64bits];
     DefaultPixelFormat : pf8bits;
     RedPrec : 1; GreenPrec:1; BluePrec:1; AlphaPrec:1;
     RedMask:$FFFFFFFF; GreenMask:$FFFFFFFF; BlueMask:$FFFFFFFF; AlphaMask:$FFFFFFFF;
     RedShift:0; GreenShift:0; BlueShit:0; AlphaShift:0;
     BitsPerPixel : 8;
     PixelSize : 1;
     SwapRB : False;
     SwapAlpha : False;
   ),
   ( name : 'GrayAlpha';
     SupportedPixelFormat : [pf8Bits, pf16bits, pf32bits, pf64bits];
     DefaultPixelFormat : pf16bits;
     RedPrec : 1; GreenPrec:1; BluePrec:1; AlphaPrec:1;
     RedMask:$FF000000; GreenMask:$FF000000; BlueMask:$FF000000; AlphaMask:$FF000000;
     RedShift:0; GreenShift:8; BlueShit:16; AlphaShift:24;
     BitsPerPixel : 16;
     PixelSize : 2;
     SwapRB : False;
     SwapAlpha : False;
   ),
   ( name : 'XRGB 1555';
     SupportedPixelFormat : [pf16Bits];
     DefaultPixelFormat : pf16bits;
     RedPrec : 5; GreenPrec:5; BluePrec:5; AlphaPrec:1;
     RedMask:$0F00; GreenMask:$00F0; BlueMask:$000F; AlphaMask:$F000;
     RedShift:2; GreenShift:7; BlueShit:12; AlphaShift:0;
     BitsPerPixel : 16;
     PixelSize : 2;
     SwapRB : False;
     SwapAlpha : False;
   ),
   ( name : 'RGBX 5551';
     SupportedPixelFormat : [pf16Bits];
     DefaultPixelFormat : pf16bits;
     RedPrec : 5; GreenPrec:5; BluePrec:5; AlphaPrec:1;
     RedMask:$F000; GreenMask:$0F00; BlueMask:$00F0; AlphaMask:$000F;
     RedShift:0; GreenShift:4; BlueShit:8; AlphaShift:12;
     BitsPerPixel : 16;
     PixelSize : 2;
     SwapRB : False;
     SwapAlpha : False;
   ),
   ( name : 'RGB 565';
     SupportedPixelFormat : [pf16Bits];
     DefaultPixelFormat : pf16bits;
     RedPrec : 5; GreenPrec:6; BluePrec:5; AlphaPrec:0;
     RedMask:$F000; GreenMask:$0F00; BlueMask:$00F0; AlphaMask:$0000;
     RedShift:0; GreenShift:4; BlueShit:8; AlphaShift:0;
     BitsPerPixel : 16;
     PixelSize : 2;
     SwapRB : False;
     SwapAlpha : False;
   ),
    ( name : 'BGR 565';
     SupportedPixelFormat : [pf16Bits];
     DefaultPixelFormat : pf16bits;
     RedPrec : 5; GreenPrec:6; BluePrec:5; AlphaPrec:0;
     RedMask:$00F0; GreenMask:$0F00; BlueMask:$F000; AlphaMask:$0000;
     RedShift:8; GreenShift:4; BlueShit:0; AlphaShift:0;
     BitsPerPixel : 16;
     PixelSize : 2;
     SwapRB : True;
     SwapAlpha : False;
   ),

   ( name : 'RGB';
     SupportedPixelFormat : [pf24Bits, pf48bits];
     DefaultPixelFormat : pf24bits;
     RedPrec : 8; GreenPrec:8; BluePrec:8; AlphaPrec:0;
     RedMask:$FF0000; GreenMask:$00FF00; BlueMask:$0000FF; AlphaMask:$000000;
     RedShift:0; GreenShift:8; BlueShit:16; AlphaShift:0;
     BitsPerPixel : 24;
     PixelSize : 3;
     SwapRB : False;
     SwapAlpha : False;
   ),
   ( name : 'BGR';
     SupportedPixelFormat : [pf24Bits, pf48bits];
     DefaultPixelFormat : pf24bits;
     RedPrec : 8; GreenPrec:8; BluePrec:8; AlphaPrec:0;
     RedMask:$0000FF; GreenMask:$00FF00; BlueMask:$FF0000; AlphaMask:$000000;
     RedShift:16; GreenShift:8; BlueShit:0; AlphaShift:0;
     BitsPerPixel : 24;
     PixelSize : 3;
     SwapRB : True;
     SwapAlpha : False;
   ),
   ( name : 'RGBA';
     SupportedPixelFormat : [pf32Bits, pf64bits];
     DefaultPixelFormat : pf32bits;
     RedPrec : 8; GreenPrec:8; BluePrec:8; AlphaPrec:8;
     RedMask:$FF000000; GreenMask:$00FF0000; BlueMask:$0000FF00; AlphaMask:$000000FF;
     RedShift:0; GreenShift:8; BlueShit:16; AlphaShift:24;
     BitsPerPixel : 32;
     PixelSize : 4;
     SwapRB : False;
     SwapAlpha : False;
   ),
   ( name : 'BGRA';
     SupportedPixelFormat : [pf32Bits, pf64bits];
     DefaultPixelFormat : pf32bits;
     RedPrec : 8; GreenPrec:8; BluePrec:8; AlphaPrec:8;
     RedMask:$0000FF00; GreenMask:$00FF0000; BlueMask:$FF000000; AlphaMask:$000000FF;
     RedShift:16; GreenShift:8; BlueShit:0; AlphaShift:24;
     BitsPerPixel : 32;
     PixelSize : 4;
     SwapRB : True;
     SwapAlpha : False;
   ),
   ( name : 'ARGB';
     SupportedPixelFormat : [pf32Bits, pf64bits];
     DefaultPixelFormat : pf32bits;
     RedPrec : 8; GreenPrec:8; BluePrec:8; AlphaPrec:8;
     RedMask:$00FF0000; GreenMask:$0000FF00; BlueMask:$000000FF; AlphaMask:$FF000000;
     RedShift:8; GreenShift:16; BlueShit:24; AlphaShift:0;
     BitsPerPixel : 32;
     PixelSize : 4;
     SwapRB : False;
     SwapAlpha : True
   ),
   ( name : 'ABGR';
     SupportedPixelFormat : [pf32Bits, pf64bits];
     DefaultPixelFormat : pf32bits;
     RedPrec : 8; GreenPrec:8; BluePrec:8; AlphaPrec:8;
     RedMask:$000000FF; GreenMask:$0000FF00; BlueMask:$00FF0000; AlphaMask:$FF000000;
     RedShift:24; GreenShift:16; BlueShit:8; AlphaShift:0;
     BitsPerPixel : 32;
     PixelSize : 4;
     SwapRB : True;
     SwapAlpha : True;
   ),
   ( name : 'XRGB';
     SupportedPixelFormat : [pf32Bits, pf64bits];
     DefaultPixelFormat : pf32bits;
     RedPrec : 8; GreenPrec:8; BluePrec:8; AlphaPrec:0;
     RedMask:$00FF0000; GreenMask:$0000FF00; BlueMask:$000000FF; AlphaMask:$00000000;
     RedShift:8; GreenShift:16; BlueShit:24; AlphaShift:0;
     BitsPerPixel : 32;
     PixelSize : 4;
     SwapRB : False;
     SwapAlpha : True
   ),
   ( name : 'XBGR';
     SupportedPixelFormat : [pf32Bits, pf64bits];
     DefaultPixelFormat : pf32bits;
     RedPrec : 8; GreenPrec:8; BluePrec:8; AlphaPrec:0;
     RedMask:$000000FF; GreenMask:$0000FF00; BlueMask:$00FF0000; AlphaMask:$00000000;
     RedShift:24; GreenShift:16; BlueShit:8; AlphaShift:0;
     BitsPerPixel : 32;
     PixelSize : 4;
     SwapRB : True;
     SwapAlpha : True;
   )
   );

{%endregion%}

{ Creation d'un masque de couleur }
function CreateBitFieldMask(AShift, APrec: Byte): Cardinal;
{ Retourne le decalge d'un masque de couleur}
function GetMaskShift(AMask: LongWord): ShortInt;
{ Retourne la taille d'un massque de couleur }
function GetMaskSize(AMask: LongWord; AShift:ShortInt): Byte;
{ Retourne l'incrément de déclage d'un masque de couleur }
function GetMaskDeltaShift(ASize:Byte): Integer;
{ Convertir une couleur en fonction d'un masque de couleur }
function ConvertBitFieldsToBZColor(Const aBitFields : TBZColorBitFields; SrcColor : Integer):TBZColor;
{ Creation d'un TBZColor }
function BZColor(Const aX,aY,aZ: Byte; const aW : Byte = 255):TBZColor;

implementation

uses Dialogs;


Const
  cHalfOneVector4f    : TBZColorVector  = (X:0.5; Y:0.5; Z:0.5; W:0.5);

{$IFDEF USE_ASM}
Const
  cSSE_MASK_ABS    : array [0..3] of UInt32 = ($7FFFFFFF, $7FFFFFFF, $7FFFFFFF, $7FFFFFFF);
  cSSE_MASK_NO_W   : array [0..3] of UInt32 = ($FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $00000000);
  {%H-}cSSE_MASK_ONLY_W : array [0..3] of UInt32 = ($00000000, $00000000, $00000000, $FFFFFFFF);
  //cSSE_MASK_NO_XYZ : array [0..3] of UInt32 = ($00000000, $00000000, $00000000, $FFFFFFFF);

  cSSE_OPERATOR_EQUAL             = 0;
  cSSE_OPERATOR_LESS              = 1;
  cSSE_OPERATOR_LESS_OR_EQUAL     = 2;

  cSSE_OPERATOR_NOT_EQUAL         = 4;
  cSSE_OPERATOR_NOT_LESS          = 5;
  cSSE_OPERATOR_NOT_LESS_OR_EQUAL = 6;
{$ENDIF}

var
	vColorRegister : TBZColorRegister;
  SqrtCombineLUT : array [0 .. 65535] of Byte;
  LerpCombineCosineLUT : array [0 .. 255] of Integer;
  ProbCombineLUT : array [0..100, 0..99] of Boolean;

function CreateBitFieldMask(AShift, APrec: Byte): Cardinal; inline;
begin
  Result := ($FFFFFFFF shr (32 - APrec)) shl AShift;
end;

function GetMaskShift(AMask: LongWord): ShortInt; inline;
begin
  Result := 0;
  while ((AMask and (1 shl Result)) = 0) and (Result < 32) do inc(Result);
  if result = 32 then result:=0;
end;

function GetMaskSize(AMask: LongWord; AShift:ShortInt): Byte; inline;

begin
  Result := 0;
  while (AShift + Result < 32) and ((AMask and (1 shl (AShift + Result))) <> 0) do Inc(Result);
end;

function GetMaskDeltaShift(ASize:Byte): Integer; inline;
begin
  result:=8-ASize;
end;

function ConvertBitFieldsToBZColor(Const aBitFields : TBZColorBitFields; SrcColor : Integer):TBZColor;

  function GetBitFieldValue(Const Value : LongWord; Mask:LongWord; BitShift, BitSize : Byte):Byte; inline;//; DeltaShift:Integer;const IsAlphaMask:Boolean=false): Byte; inline;
  var
    I,v,v2 : Integer;
  begin
      V:=(Value and Mask) Shr BitShift;
      V2:=0;
      I:=8-BitSize; //DeltaShift;
      While I>0 do
      begin
        V2 := V2 or (V Shl I);
        I:=I-BitSize;
      end;
      if I<0 then
        V2 := V2 or (V Shr (-I))
      else
        V2 := V2 or (V Shl I);
      V:= (V2 shl 8) + V2;
      Result:= V; //(V and  255); // ClampByte(V);
    end;

begin
  With aBitFields do
  begin
    if RedSize>0 then Result.Red:= GetBitFieldValue(SrcColor,RedMask, RedShift, RedSize) else Result.Red := 0;//, RedDeltaShift);
    if GreenSize>0 then Result.Green:= GetBitFieldValue(SrcColor,GreenMask, GreenShift, GreenSize) else Result.Green := 0;//, GreenDeltaShift);
    if BlueSize>0 then Result.Blue:= GetBitFieldValue(SrcColor,BlueMask, BlueShift, BlueSize) else Result.Blue := 0;//, BlueDeltaShift);

    if (AlphaSize=0) then Result.Alpha:=255
    else Result.Alpha:=GetBitFieldValue(SrcColor,AlphaMask, AlphaShift, AlphaSize);//, AlphaDeltaShift, true);

  end;
end;

function BZColor(Const aX,aY,aZ: Byte; const aW : Byte = 255):TBZColor;
Begin
  Result.Create(aX,aY,AZ,aW);
End;

{%region=====[ TBZColorMatrix ]======================================================}

procedure TBZColorMatrix.Create(RedRow, GreenRow, BlueRow, AlphaRow, OffsetRow : TBZColorVector);
begin
  Self.R := RedRow;
  Self.G := GreenRow;
  Self.B := BlueRow;
  Self.A := AlphaRow;
  Self.W := OffsetRow;
end;

procedure TBZColorMatrix.CreateIdentity;
begin
  Self.R.Create(1.0, 0.0, 0.0, 0.0);
  Self.G.Create(0.0, 1.0, 0.0, 0.0);
  Self.B.Create(0.0, 0.0, 1.0, 0.0);
  Self.A.Create(0.0, 0.0, 0.0, 1.0);
  Self.W.Create(0.0, 0.0, 0.0, 0.0);
end;

procedure TBZColorMatrix.CreateBrightness(Factor : Single);
begin
  Self.CreateIdentity;
  if Factor < 0 then Factor := Abs(Factor);
  Self.M11 := Factor;
  Self.M22 := Factor;
  Self.M33 := Factor;
  Self.M44 := 1.0;
end;

procedure TBZColorMatrix.CreateLightness(Factor : Single);
begin
  Self.CreateIdentity;
  if Factor < -1.0 then Factor := -1.0;
  if Factor > 1.0 then Factor := 1.0;
  //Self.M11 := 1.0;
  //Self.M22 := 1.0;
  //Self.M33 := 1.0;
  //Self.M44 := 1.0;
  Self.M51 := Factor;
  Self.M52 := Factor;
  Self.M53 := Factor;
end;

procedure TBZColorMatrix.CreateContrast(Factor : Single);
Var
  ContrastFactor : Single;
begin
  Self.CreateIdentity;
  if Factor < 0 then Factor := Abs(Factor);
  ContrastFactor := (-0.5 * Factor) + 0.5;
  Self.M11 := Factor;
  Self.M22 := Factor;
  Self.M33 := Factor;
  Self.M44 := 1.0;
  Self.M51 := ContrastFactor;
  Self.M52 := ContrastFactor;
  Self.M53 := ContrastFactor;
end;

procedure TBZColorMatrix.CreateSaturation(Factor : Single);
begin
  Self.CreateIdentity;
  if Factor < 0 then Factor := Abs(Factor);

  Self.M11 := 0.213 + (0.787 * Factor);
  Self.M12 := 0.213 - (0.213 * Factor);
  Self.M13 := 0.213 - (0.213 * Factor);

  Self.M21 := 0.715 - (0.715 * Factor);
  Self.M22 := 0.715 + (0.285 * Factor);
  Self.M23 := 0.715 - (0.715 * Factor);

  Self.M31 := 1.0 - (Self.M11 + Self.M21);
  Self.M32 := 1.0 - (Self.M12 + Self.M22);
  Self.M33 := 1.0 - (Self.M13 + Self.M23);

  Self.M44 := 1.0;
end;

procedure TBZColorMatrix.CreateHueRotate(AngleInDeg : Single);
var
  ra : Single;
  CosA, SinA : Single;
begin
  Self.CreateIdentity;
  AngleInDeg := fmod(AngleInDeg, 360);
  While AngleInDeg < 0 do
  begin
    AngleInDeg := AngleInDeg + 360;
  end;
  ra := DegToradian(AngleInDeg);
  CosA := System.Cos(ra);
  SinA := System.Cos(ra);

  Self.M11 := 0.213 + (CosA * 0.787) - (SinA * 0.213);
  Self.M21 := 0.715 - (CosA * 0.715) - (SinA * 0.715);
  Self.M31 := 0.072 - (CosA * 0.072) + (SinA * 0.928);

  Self.M12 := 0.213 - (CosA * 0.213) + (SinA * 0.143);
  Self.M22 := 0.715 + (CosA * 0.285) + (SinA * 0.140);
  Self.M32 := 0.072 - (CosA * 0.072) - (SinA * 0.283);

  Self.M13 := 0.213 - (CosA * 0.213) - (SinA * 0.787);
  Self.M23 := 0.715 - (CosA * 0.715) + (SinA * 0.715);
  Self.M33 := 0.072 + (CosA * 0.928) + (SinA * 0.072);

  Self.M44 := 1.0;
end;

procedure TBZColorMatrix.CreateInvert(Factor : Single);
Var
  invertFactor : Single;
begin
  Self.CreateIdentity;
  if Factor < 0 then Factor := Abs(Factor);
  invertFactor := 1.0 - (Factor + Factor);

  Self.M11 := invertFactor;
  Self.M22 := invertFactor;
  Self.M33 := invertFactor;

  Self.M44 := 1.0;

  Self.M51 := Factor;
  Self.M52 := Factor;
  Self.M53 := Factor;
end;

procedure TBZColorMatrix.CreateOpacity(Factor : Single);
begin
  Self.CreateIdentity;
  if Factor < 0 then Factor := Abs(Factor);
  if Factor > 1.0 then Factor := 1.0;
  Self.M44 := Factor;
end;

procedure TBZColorMatrix.CreateBlackWhite;
begin
  Self.CreateIdentity;
  Self.R.Create(1.5,1.5,1.5,0.0);
  //Self.M11 := 1.5;
  //Self.M12 := 1.5;
  //Self.M13 := 1.5;

  Self.G.Create(1.5,1.5,1.5,0.0);
  //Self.M21 := 1.5;
  //Self.M22 := 1.5;
  //Self.M23 := 1.5;

  Self.B.Create(1.5,1.5,1.5,0.0);
  //Self.M31 := 1.5;
  //Self.M32 := 1.5;
  //Self.M33 := 1.5;

  Self.A.Create(0.0,0.0,0.0,1.0);
  //Self.M44 := 1.0;

  Self.W.Create(-1.0,-1.0,-1.0,0.0);
  //Self.M51 := -1.0;
  //Self.M52 := -1.0;
  //Self.M53 := -1.0;
end;

procedure TBZColorMatrix.CreateSepia(Factor : Single);
begin
  Self.CreateIdentity;
  if Factor < 0 then Factor := Abs(Factor);
  Factor := 1.0 - Factor;

  Self.M11 := 0.393 + (0.607 * Factor);
  Self.M12 := 0.349 - (0.349 * Factor);
  Self.M13 := 0.272 - (0.272 * Factor);

  Self.M21 := 0.769 - (0.769 * Factor);
  Self.M22 := 0.686 + (0.314 * Factor);
  Self.M23 := 0.534 - (0.534 * Factor);

  Self.M33 := 0.131 + (0.869 * Factor);
  Self.M31 := 0.189 - (0.189 * Factor);
  Self.M32 := 0.168 - (0.168 * Factor);

  Self.M44 := 1.0;
end;

procedure TBZColorMatrix.CreateSepiaII(Factor : Single);
begin
  Self.CreateIdentity;
  if Factor < 0 then Factor := Abs(Factor);
  Factor := 1.0 - Factor;

  Self.M11 := (0.3930000066757202 + 0.6069999933242798 * Factor); //0.3930000066757202
  Self.M12 := (0.3490000069141388 - 0.3490000069141388 * Factor);
  Self.M13 := (0.2720000147819519 - 0.2720000147819519 * Factor); //0.7279999852180481

  Self.M21 := (0.7689999938011169 - 0.7689999938011169 * Factor);
  Self.M22 := (0.6859999895095825 + 0.3140000104904175 * Factor);
  Self.M23 := (0.5339999794960022 - 0.5339999794960022 * Factor);


  Self.M31 := (0.1309999972581863 + 0.8690000027418137 * Factor); //0.1309999972581863
  Self.M32 := (0.1889999955892563 - 0.1889999955892563 * Factor); //0.8110000044107437
  Self.M33 := (0.1679999977350235 - 0.1679999977350235 * Factor);



  Self.M44 := 1.0;
end;

procedure TBZColorMatrix.CreateGrayscaleBt601(Factor : Single);
begin
  Self.CreateIdentity;

  if Factor < 0 then Factor := Abs(Factor);
  Factor := 1.0 - Factor;

  Self.M11 := 0.299 + (0.701 * Factor);
  Self.M12 := 0.299 - (0.299 * Factor);
  Self.M13 := 0.299 - (0.299 * Factor);

  Self.M21 := 0.587 - (0.587 * Factor);
  Self.M22 := 0.587 + (0.2848 * Factor);
  Self.M23 := 0.587 - (0.587 * Factor);

  Self.M31 := 1.0 - (Self.M11 + Self.M21);
  Self.M32 := 1.0 - (Self.M12 + Self.M22);
  Self.M33 := 1.0 - (Self.M13 + Self.M23);

  Self.M44 := 1.0;
end;

procedure TBZColorMatrix.CreateGrayscaleBt709(Factor : Single);
begin
  Self.CreateIdentity;

  if Factor < 0 then Factor := Abs(Factor);
  Factor := 1.0 - Factor;

  // 0.2126, 0.7152, and 0.0722
  Self.M11 := 0.2126 + (0.7874 * Factor);
  Self.M12 := 0.2126 - (0.2126 * Factor);
  Self.M13 := 0.2126 - (0.2126 * Factor);

  Self.M21 := 0.7152 - (0.7152 * Factor);
  Self.M22 := 0.7152 + (0.2848 * Factor);
  Self.M23 := 0.7152 - (0.7152 * Factor);

  //Self.M31 := 0.0722 + (0.0722 * Factor);
  //Self.M32 := 0.0722 - (0.9278 + Self.M22);
  //Self.M33 := 0.0722 + (0.0722 * Factor);

  Self.M31 := 1.0 - (Self.M11 + Self.M21);
  Self.M32 := 1.0 - (Self.M12 + Self.M22);
  Self.M33 := 1.0 - (Self.M13 + Self.M23);

  Self.M44 := 1.0;
end;

procedure TBZColorMatrix.CreateGrayscaleBt2100(Factor : Single);
begin
  Self.CreateIdentity;

  if Factor < 0 then Factor := Abs(Factor);
  Factor := 1.0 - Factor;

  //0.2627 for red, 0.678 for green, and 0.0593 for blue

  Self.M11 := 0.2627 + (0.7373 * Factor);
  Self.M12 := 0.2627 - (0.2627 * Factor);
  Self.M13 := 0.2627 - (0.2627 * Factor);

  Self.M21 := 0.678 - (0.678 * Factor);
  Self.M22 := 0.678 + (0.322 * Factor);
  Self.M23 := 0.678 - (0.678 * Factor);

  //Self.M31 := 0.0722 + (0.0722 * Factor);
  //Self.M32 := 0.0722 - (0.9278 + Self.M22);
  //Self.M33 := 0.0722 + (0.0722 * Factor);

  Self.M31 := 1.0 - (Self.M11 + Self.M21);
  Self.M32 := 1.0 - (Self.M12 + Self.M22);
  Self.M33 := 1.0 - (Self.M13 + Self.M23);

  Self.M44 := 1.0;

end;

//procedure TBZColorMatrix.CreateGrayscaleBt2020(Factor : Single);
//begin
//  Self.CreateIdentity;
//
//  if Factor < 0 then Factor := Abs(Factor);
//  Factor := 1.0 - Factor;
//  // KR = 0.2627, KG = 1−KR−KB = 0.678 and KB = 0.0593
//end;

procedure TBZColorMatrix.CreateAchromAnomaly;
begin
  Self.CreateIdentity;
  Self.M11 := 0.618;
  Self.M12 := 0.163;
  Self.M13 := 0.163;
  Self.M21 := 0.320;
  Self.M22 := 0.775;
  Self.M23 := 0.320;
  Self.M31 := 0.062;
  Self.M32 := 0.062;
  Self.M33 := 0.516;
  Self.M44 := 1.0;
end;

procedure TBZColorMatrix.CreateAchromatopsia;
begin
  Self.CreateIdentity;
  Self.M11 := 0.299;
  Self.M12 := 0.299;
  Self.M13 := 0.299;
  Self.M21 := 0.587;
  Self.M22 := 0.587;
  Self.M23 := 0.587;
  Self.M31 := 0.114;
  Self.M32 := 0.114;
  Self.M33 := 0.114;
  Self.M44 := 1.0;
end;

procedure TBZColorMatrix.CreateDeuterAnomaly;
begin
  Self.CreateIdentity;
  Self.M11 := 0.8;
  Self.M12 := 0.258;
  Self.M21 := 0.2;
  Self.M22 := 0.742;
  Self.M23 := 0.142;
  Self.M33 := 0.858;
  Self.M44 := 1.0;
end;

procedure TBZColorMatrix.CreateDeuteranopia;
begin
  Self.CreateIdentity;
  Self.M11 := 0.625;
  Self.M12 := 0.7;
  Self.M21 := 0.375;
  Self.M22 := 0.3;
  Self.M23 := 0.3;
  Self.M33 := 0.7;
  Self.M44 := 1.0;
end;

procedure TBZColorMatrix.CreateProtAnomaly;
begin
  Self.CreateIdentity;
  Self.M11 := 0.817;
  Self.M12 := 0.333;
  Self.M21 := 0.183;
  Self.M22 := 0.667;
  Self.M23 := 0.125;
  Self.M33 := 0.875;
  Self.M44 := 1.0;
end;

procedure TBZColorMatrix.CreateProtanopia;
begin
  Self.CreateIdentity;
  Self.M11 := 0.567;
  Self.M12 := 0.558;
  Self.M21 := 0.433;
  Self.M22 := 0.442;
  Self.M23 := 0.242;
  Self.M33 := 0.758;
  Self.M44 := 1.0;
end;

procedure TBZColorMatrix.CreateTritAnomaly;
begin
  Self.CreateIdentity;
  Self.M11 := 0.967;
  Self.M21 := 0.33;
  Self.M22 := 0.733;
  Self.M23 := 0.183;
  Self.M32 := 0.267;
  Self.M33 := 0.817;
  Self.M44 := 1.0;
end;

procedure TBZColorMatrix.CreateTritanopia;
begin
  Self.CreateIdentity;
  Self.M11 := 0.95;
  Self.M21 := 0.05;
  Self.M22 := 0.433;
  Self.M23 := 0.475;
  Self.M32 := 0.567;
  Self.M33 := 0.525;
  Self.M44 := 1.0;
end;

procedure TBZColorMatrix.CreateKodaChrome;
Var
  ContrastMatrix, SaturateMatrix : TBZColorMatrix;
begin
  Self.CreateIdentity;
  Self.M11 := 0.7297023;
  Self.M22 := 0.6109577;
  Self.M33 := 0.597218;
  Self.M44 := 1.0;
  Self.M51 := 0.105;
  Self.M52 := 0.145;
  Self.M53 := 0.155;
  SaturateMatrix.CreateSaturation(1.20);
  ContrastMatrix.CreateContrast(1.35);
  Self := Self * SaturateMatrix * ContrastMatrix;
end;

procedure TBZColorMatrix.CreateLomoGraph;
Var
  ContrastMatrix, SaturateMatrix : TBZColorMatrix;
begin
  Self.CreateIdentity;
  Self.M11 := 1.5;
  Self.M22 := 1.45;
  Self.M33 := 1.16;
  Self.M44 := 1.0;
  Self.M51 := -0.1;
  Self.M52 := -0.02;
  Self.M53 := -0.07;
  SaturateMatrix.CreateSaturation(1.10);
  ContrastMatrix.CreateContrast(1.33);
  Self := Self * SaturateMatrix * ContrastMatrix;
end;

procedure TBZColorMatrix.CreatePolaroid;
begin
  Self.CreateIdentity;
  Self.M11 := 1.538;
  Self.M12 := -0.062;
  Self.M13 := -0.262;
  Self.M21 := -0.022;
  Self.M22 := 1.578;
  Self.M23 := -0.022;
  Self.M31 := 0.216;
  Self.M32 := -0.16;
  Self.M33 := 1.5831;
  Self.M44 := 1;
  Self.M51 := 0.02;
  Self.M52 := -0.05;
  Self.M53 := -0.05;
end;

procedure TBZColorMatrix.CreatePolaroidII;
begin
  Self.CreateIdentity;

  Self.M11 := 1.438;
  Self.M12 := 0.122;
  Self.M13 := 0.016;

  Self.M21 := -0.062;
  Self.M22 := 1.378;
  Self.M23 := -0.016;

  Self.M31 := -0.062;
  Self.M32 := 0.122;
  Self.M33 := 1.483;

  Self.M41 := 0.000;
  Self.M42 := 0.000;
  Self.M43 := 0.000;
  Self.M44 := 1.0;

  Self.M51 := 0.03;
  Self.M52 := 0.05;
  Self.M53 := -0.02;
  Self.M54 := 0.0;
end;

procedure TBZColorMatrix.CreateOldPhoto;
begin
  Self.CreateIdentity;

  Self.M11 := 0.250;
  Self.M12 := 0.250;
  Self.M13 := 0.250;

  Self.M21 := 0.500;
  Self.M22 := 0.500;
  Self.M23 := 0.500;

  Self.M31 := 0.125;
  Self.M32 := 0.125;
  Self.M33 := 0.125;

  Self.M41 := 0.000;
  Self.M42 := 0.000;
  Self.M43 := 0.000;
  Self.M44 := 1.0;

  Self.M51 := 0.2;
  Self.M52 := 0.2;
  Self.M53 := 0.2;
  Self.M54 := 0.0;
end;

function TBZColorMatrix.IsIdentity : Boolean;
begin
  Self.CreateIdentity;
end;

function TBZColorMatrix.ToString : String;
begin
  Self.CreateIdentity;
end;

class operator TBZColorMatrix.+ (constref A, B : TBZColorMatrix) : TBZColorMatrix;
begin
  Result.R := A.R + B.R;
  Result.G := A.G + B.G;
  Result.B := A.B + B.B;
  Result.A := A.A + B.A;
  Result.W := A.W + B.W;
end;

class operator TBZColorMatrix.- (constref A, B : TBZColorMatrix) : TBZColorMatrix;
begin
  Result.R := A.R - B.R;
  Result.G := A.G - B.G;
  Result.B := A.B - B.B;
  Result.A := A.A - B.A;
  Result.W := A.W - B.W;
end;

class operator TBZColorMatrix.- (constref A : TBZColorMatrix) : TBZColorMatrix;
begin
  Result.M11 := -Result.M11;
  Result.M12 := -Result.M12;
  Result.M13 := -Result.M13;
  Result.M14 := -Result.M14;

  Result.M21 := -Result.M21;
  Result.M22 := -Result.M22;
  Result.M23 := -Result.M23;
  Result.M24 := -Result.M24;

  Result.M31 := -Result.M31;
  Result.M32 := -Result.M32;
  Result.M33 := -Result.M33;
  Result.M34 := -Result.M34;

  Result.M41 := -Result.M41;
  Result.M42 := -Result.M42;
  Result.M43 := -Result.M43;
  Result.M44 := -Result.M44;

  Result.M51 := -Result.M51;
  Result.M52 := -Result.M52;
  Result.M53 := -Result.M53;
  Result.M54 := -Result.M54;
end;

class operator TBZColorMatrix.*(constref A, B : TBZColorMatrix) : TBZColorMatrix;
//Var
//  c1, c2, c3, c4 : TBZColorVector;
//
begin
  // 1ere Ligne
  //c1.Create(A.R.X, A.R.X, A.R.X, A.R.X);
  //c2.Create(A.R.Y, A.R.Y, A.R.Y, A.R.Y);
  //c3.Create(A.R.Z, A.R.Z, A.R.Z, A.R.Z);
  //c4.Create(A.R.W, A.R.W, A.R.W, A.R.W);
  //Result.R := (c1 * B.R) + (c2 * B.G) + (c3 * B.B) + (c4 * B.A);
  Result.M11 := (A.M11 * B.M11) + (A.M12 * B.M21) + (A.M13 * B.M31) + (A.M14 * B.M41);
  Result.M12 := (A.M11 * B.M12) + (A.M12 * B.M22) + (A.M13 * B.M32) + (A.M14 * B.M42);
  Result.M13 := (A.M11 * B.M13) + (A.M12 * B.M23) + (A.M13 * B.M33) + (A.M14 * B.M43);
  Result.M14 := (A.M11 * B.M14) + (A.M12 * B.M24) + (A.M13 * B.M34) + (A.M14 * B.M44);

  // 2eme Ligne
  //c1.Create(A.G.X, A.G.X, A.G.X, A.G.X);
  //c2.Create(A.G.Y, A.G.Y, A.G.Y, A.G.Y);
  //c3.Create(A.G.Z, A.G.Z, A.G.Z, A.G.Z);
  //c4.Create(A.G.W, A.G.W, A.G.W, A.G.W);
  //Result.G := (c1 * B.R) + (c2 * B.G) + (c3 * B.B) + (c4 * B.A);
  Result.M21 := (A.M21 * B.M11) + (A.M22 * B.M21) + (A.M23 * B.M31) + (A.M24 * B.M41);
  Result.M22 := (A.M21 * B.M12) + (A.M22 * B.M22) + (A.M23 * B.M32) + (A.M24 * B.M42);
  Result.M23 := (A.M21 * B.M13) + (A.M22 * B.M23) + (A.M23 * B.M33) + (A.M24 * B.M43);
  Result.M24 := (A.M21 * B.M14) + (A.M22 * B.M24) + (A.M23 * B.M34) + (A.M24 * B.M44);

  // 3eme Ligne
  //c1.Create(A.B.X, A.B.X, A.B.X, A.B.X);
  //c2.Create(A.B.Y, A.B.Y, A.B.Y, A.B.Y);
  //c3.Create(A.B.Z, A.B.Z, A.B.Z, A.B.Z);
  //c4.Create(A.B.W, A.B.W, A.B.W, A.B.W);
  //Result.B := (c1 * B.R) + (c2 * B.G) + (c3 * B.B) + (c4 * B.A);
  Result.M31 := (A.M31 * B.M11) + (A.M32 * B.M21) + (A.M33 * B.M31) + (A.M34 * B.M41);
  Result.M32 := (A.M31 * B.M12) + (A.M32 * B.M22) + (A.M33 * B.M32) + (A.M34 * B.M42);
  Result.M33 := (A.M31 * B.M13) + (A.M32 * B.M23) + (A.M33 * B.M33) + (A.M34 * B.M43);
  Result.M34 := (A.M31 * B.M14) + (A.M32 * B.M24) + (A.M33 * B.M34) + (A.M34 * B.M44);

  // 4eme Ligne
  //c1.Create(A.A.X, A.A.X, A.A.X, A.A.X);
  //c2.Create(A.A.Y, A.A.Y, A.A.Y, A.A.Y);
  //c3.Create(A.A.Z, A.A.Z, A.A.Z, A.A.Z);
  //c4.Create(A.A.W, A.A.W, A.A.W, A.A.W);
  //Result.A := (c1 * B.R) + (c2 * B.G) + (c3 * B.B) + (c4 * B.A);
  Result.M41 := (A.M41 * B.M11) + (A.M42 * B.M21) + (A.M43 * B.M31) + (A.M44 * B.M41);
  Result.M42 := (A.M41 * B.M12) + (A.M42 * B.M22) + (A.M43 * B.M32) + (A.M44 * B.M42);
  Result.M43 := (A.M41 * B.M13) + (A.M42 * B.M23) + (A.M43 * B.M33) + (A.M44 * B.M43);
  Result.M44 := (A.M41 * B.M14) + (A.M42 * B.M24) + (A.M43 * B.M34) + (A.M44 * B.M44);

  // 5eme Ligne
  //c1.Create(A.W.X, A.W.X, A.W.X, A.W.X);
  //c2.Create(A.W.Y, A.W.Y, A.W.Y, A.W.Y);
  //c3.Create(A.W.Z, A.W.Z, A.W.Z, A.W.Z);
  //c4.Create(A.W.W, A.W.W, A.W.W, A.W.W);
  //Result.W := ((c1 * B.R) + (c2 * B.G) + (c3 * B.B) + (c4 * B.A)) + B.W;
  Result.M51 := (A.M51 * B.M11) + (A.M52 * B.M21) + (A.M53 * B.M31) + (A.M54 * B.M41) + B.M51;
  Result.M52 := (A.M51 * B.M12) + (A.M52 * B.M22) + (A.M53 * B.M32) + (A.M54 * B.M52) + B.M52;
  Result.M53 := (A.M51 * B.M13) + (A.M52 * B.M23) + (A.M53 * B.M33) + (A.M54 * B.M53) + B.M53;
  Result.M54 := (A.M51 * B.M14) + (A.M52 * B.M24) + (A.M53 * B.M34) + (A.M54 * B.M54) + B.M54;
end;

class operator TBZColorMatrix.*(constref A : TBZColorMatrix; constref B : Single) : TBZColorMatrix;
begin
  Result.R := A.R * B;
  Result.G := A.G * B;
  Result.B := A.B * B;
  Result.A := A.A * B;
  Result.W := A.W * B;
end;

class operator TBZColorMatrix.=(constref A, B : TBZColorMatrix) : Boolean;
begin
  Result := ((A.R = B.R) and (A.G = B.G) and (A.B = B.B) and (A.A = B.A) and (A.W = B.W));
end;

function TBZColorMatrix.Transform(AColor : TBZColorVector) : TBZColorVector;
//Var
//  {$CODEALIGN VARMIN=16}
//  c1, c2, c3, c4 : TBZColorVector; //, c2, c3, c4
//  {$CODEALIGN VARMIN=4}
begin
  // /!\ L'utilisation des vecteurs avec l'ASM provoque un SIGSEGV avec FPC 3.2
  // il semblerait que les données des vecteurs soient mal alignées en mémoire

  //c1.Create(AColor.X, AColor.X, AColor.X, AColor.X);
  //c1 := (c1 * Self.R);
  ////Result := c1;
  //
  //c2.Create(AColor.Y, AColor.Y, AColor.Y, AColor.Y);
  //c2 := (c2 * Self.G);
  ////Result := Result + c1;
  //
  //c3.Create(AColor.Z, AColor.Z, AColor.Z, AColor.Z);
  //c3 := (c3 * Self.B);
  ////Result := Result + c1;
  //
  //c4.Create(AColor.W, AColor.W, AColor.W, AColor.W);
  //c4 := (c4 * Self.A);
  ////Result := Result + c1;
  //
  //Result :=  c1 +  c2 + c3 + c4;
  //
  //Result := Result + Self.W;

  Result.X := (AColor.X * Self.M11) + (AColor.Y * Self.M21) + (AColor.Z * Self.M31) + (AColor.W * Self.M41) + Self.M51;
  Result.Y := (AColor.X * Self.M12) + (AColor.Y * Self.M22) + (AColor.Z * Self.M32) + (AColor.W * Self.M42) + Self.M52;
  Result.Z := (AColor.X * Self.M13) + (AColor.Y * Self.M23) + (AColor.Z * Self.M33) + (AColor.W * Self.M43) + Self.M53;
  Result.W := (AColor.X * Self.M14) + (AColor.Y * Self.M24) + (AColor.Z * Self.M34) + (AColor.W * Self.M44) + Self.M54;
end;

{%endregion%}

{ TBZColorHSLHelper }

function TBZColorHSLHelper.ToColorRGBA : TBZColor;
Var
  vCol, HSLFactors : TBZColorVector;
  HSLColor : TBZColorFloatHSL;
begin
  vCol.x := Self.Hue;
  vCol.y := Self.Saturation;
  vCol.z := Self.Luminosity;
  vCol.w := 255;
  Case Self.ColorSubSpaceType of
    hslNormalize:
    begin
      HSLFactors.Create(255, 255, 255, 255);
    end;
    hslDefault :
    begin
      HSLFactors.Create(cssHSL_Default.tA, cssHSL_Default.tB, cssHSL_Default.tC, 255);
    end;
    hslPaintShopPro :
    begin
      HSLFactors.Create(cssHSL_PaintShopPro.tA, cssHSL_PaintShopPro.tB, cssHSL_PaintShopPro.tC, 255);
    end;
    hslWindows :
    begin
      HSLFactors.Create(cssHSL_Windows.tA, cssHSL_Windows.tB, cssHSL_Windows.tC, 255);
    end;
  end;
  vCol := vCol / HSLFactors;
  HSLColor := vCol.AsColorFloatHSL;
  HSLColor.ColorSubSpaceType := hslNormalize;
  Result := HSLColor.ToColorRBGA;
end;


{%region%=====[ TBZColor32 and TBZColorVector ]=================================}



{$IFDEF CPU32}
  {$i color32_native_imp.inc}
  {$i colorvector_native_imp.inc}
  {.$i color32Helper_native_imp.inc}
{$ELSE}
  {$IFDEF USE_ASM}
    {$IFDEF UNIX}
      {$i color32_native_imp.inc}
      {.$i color32_unix64_sse_imp.inc}
      {$i colorvector_native_imp.inc}
      {$i colorvector_unix64_sse_imp.inc}
      {.$i color32Helper_native_imp.inc}
      {.$i color32Helper_unix64_sse_imp.inc}
    {$ENDIF}
    {$IFDEF WINDOWS}
      {$i color32_native_imp.inc}
      {.$i color32_win64_sse_imp.inc}
      {$i colorvector_native_imp.inc}
      {$i colorvector_win64_sse_imp.inc}
      {.$i color32Helper_native_imp.inc}
      {.$i color32Helper_win64_sse_imp.inc}
    {$ENDIF}
  {$ELSE}
    {$i color32_native_imp.inc}
    {$i colorvector_native_imp.inc}
    {.$i color32Helper_native_imp.inc}
  {$ENDIF}
{$ENDIF}

{%endregion%}

{%region%=====[ TBZColorFloatHSL ]==============================================}

procedure TBZColorFloatHSL.Create(H, S, L : Single; const ColorSubSpace : THSLColorSubSpaceType);
begin
  Self.Hue := H;
  Self.Saturation := S;
  Self.Luminosity := L;
  Self.ColorSubSpaceType := ColorSubSpace;
end;

function TBZColorFloatHSL.ToColorRBGA : TBZColor32;
begin
  Result.Create(Self.ToColorVector);
end;

function TBZColorFloatHSL.ToColorVector : TBZColorVector;
  function Hue2RGB(v1, v2, vH: Single): Single;
  begin
    if vH < 0 then vH := vH + 1;
    if vH > 1 then vH := vH - 1;

    if (6*vH) < 1 then exit( v1 + (v2-v1) * 6 * vH);
    if (2*vH) < 1 then exit( v2 );
    if (3*vH) < 2 then exit( v1 + (v2-v1) * (((2/3) - vH) * 6));
    result := v1;
  end;
var
   var_1, var_2: single;
begin
  if Self.Saturation = 0 then
  begin
    result.Red := Self.Luminosity;
    result.Green := Self.Luminosity;
    result.Blue := Self.Luminosity;
  end
  else
  begin
    if Self.Luminosity < 0.5 then
      var_2 := Self.Luminosity * ( 1+Self.Saturation )
    else
      var_2 := (Self.Luminosity+Self.Saturation) - (Self.Saturation*Self.Luminosity);

    var_1 := 2 * Self.Luminosity - var_2;

    result.Red := hue2RGB(var_1, var_2, Self.Hue+(1/3));
    result.Green := hue2RGB(var_1, var_2, Self.Hue);
    result.Blue := hue2RGB(var_1, var_2, Self.Hue-(1/3));
    result.Alpha := 1.0;
  end;
end;


function TBZColorFloatHSL.AsColorVector : TBZColorVector;
begin
  Result.Create(Self.Hue, Self.Saturation, Self.Luminosity);
end;

function TBZColorFloatHSL.AsColorHSL : TBZColorFloatHSL;
begin
  Case Self.ColorSubSpaceType of
   hslNormalize :;
   hslDefault,  hslWindows :;
   hslPaintShopPro :;
  end;
  Result.ColorSubSpaceType := Self.ColorSubSpaceType;
end;

{%endregion%}

{%region%=====[ TBZColorFloatHSV ]==============================================}

procedure TBZColorFloatHSV.Create(H, S, V : Single; const ColorSubSpace : THSVColorSubSpaceType);
begin
  Self.Hue := H;
  Self.Saturation := S;
  Self.Value := V;
  Self.ColorSubSpaceType := ColorSubSpace;
end;

function TBZColorFloatHSV.ToColorRBGA : TBZColor32;
begin
  Result.Create(Self.ToColorVector);
end;

function TBZColorFloatHSV.ToColorVector : TBZColorVector;
var
   h, p,q,t: Single;
   i : integer;
begin
  result.Alpha := 1.0;
  if Self.Saturation = 0 then
  begin
    result.Red := Self.Value;
    result.Green := Self.Value;
    result.Blue:= Self.Value;
  end
  else
  begin
    h := Self.Hue * 6;
    if h = 6 then  h := 0;  // H must be < 1

    i := floor(h);
    p := Self.Value * (1-Self.Saturation);
    q := Self.Value * (1-Self.Saturation * (h - i));
    t := Self.Value * (1-Self.Saturation * (1 - (h - i)));

    case i of
      0 :
      begin
        Result.Red := Self.Value;
        Result.Green := t;
        Result.Blue := p;
      end;
      1 :
      begin
        Result.Red := q;
        Result.Green := Self.Value;
        Result.Blue := p;
      end;
      2 :
      begin
        Result.Red := p;
        Result.Green := Self.Value;
        Result.Blue := t;
      end;
      3 :
      begin
        Result.Red := p;
        Result.Green := q;
        Result.Blue := Self.Value;
      end;
      4 :
      begin
        Result.Red := q;
        Result.Green := p;
        Result.Blue := Self.Value;
      end;
      else //Return(aColor.Value,     var_1, var_2, R, G, B);
      begin
        Result.Red := Self.Value;
        Result.Green := p;
        Result.Blue := q;
      end;
    end;
  end;
end;

function TBZColorFloatHSV.AsColorHSV : TBZColorHSV;
begin
  //Case Self.ColorSubSpaceType of
  //  hsvNormalize :
  //  hsvDefault :
  //  hsvOpenCV :
  //  hsvGimp :
  //  hsvPhotopshop :
  //  hsvLinuxKDE :
  //  hsvGTK :
  //  hsvJavaAWT :
  //  hsvApple: ;
  //end;
end;

function TBZColorFloatHSV.AsColorVector : TBZColorVector;
begin
  Result.Create(Self.Hue, Self.Saturation, Self.Value);
end;

{%endregion%}

{%region=====[ TBZColorVectoHelper ]============================================}

function TBZColorVectoHelper.AsRGBA : TBZColor32;
begin
  Result.Create(Self);
end;

function TBZColorVectoHelper.ToColorVectorHSV : TBZColorVector;
Const
  _Normalized60deg : Single = 60/360;
  _Normalized120deg : Single = 120/360;
  _Normalized240deg : Single = 240/360;

var
  Delta, Min, H1, S1, V1: Single;
begin
  h1 := 0;
  Min :=  Self.MinXYZComponent;
  V1 :=   Self.MaxXYZComponent;
  Delta := V1 - min;
  if V1 =  0.0 then S1 := 0 else S1 := Delta / V1;
  if S1  = 0.0 then H1 := 0
  else
  begin
    if Self.x = V1 then H1 := _Normalized60deg * (Self.y - Self.z) / Delta
    else if Self.y = V1 then H1 := _Normalized120deg + _Normalized60deg * (Self.z - Self.x) / Delta
    else if Self.z = V1 then H1 := _Normalized240deg + _Normalized60deg * (Self.x - Self.y) / Delta;
    if H1 < 0.0 then H1 := H1 + 1.0;
  end;
  Result.x := h1; // 0..1
  Result.y := s1; // 0..1
  Result.z := v1; // 0..1

end;

function TBZColorVectoHelper.AsColorFloatHSV : TBZColorFloatHSV;
begin
  Result.hue := Self.x;
  Result.saturation := Self.y;
  Result.Value := Self.z;
  Result.ColorSubSpaceType := hsvNormalize;
end;

function TBZColorVectoHelper.ToColorFloatHSV(const SubSpaceType : THSVColorSubSpaceType) : TBZColorFloatHSV;
Var
  HSVColor, HSVFactors : TBZColorVector;
begin
  HSVColor := Self.ToColorVectorHSV;
  if SubSpaceType <> hsvNormalize then
  begin
    Case SubSpaceType of
      hsvDefault : HSVFactors.Create(cssHSV_Default.tA, cssHSV_Default.tB, cssHSV_Default.tC, Self.w);
      hsvOpenCV : HSVFactors.Create(cssHSV_OpenCV.tA, cssHSV_OpenCV.tB, cssHSV_OpenCV.tC, Self.w);
      hsvGimp : HSVFactors.Create(cssHSV_Gimp.tA, cssHSV_Gimp.tB, cssHSV_Gimp.tC, Self.w);
      hsvPhotopshop : HSVFactors.Create(cssHSV_Photoshop.tA, cssHSV_Photoshop.tB, cssHSV_Photoshop.tC, Self.w);
      hsvLinuxKDE : HSVFactors.Create(cssHSV_LinuxKDE.tA, cssHSV_LinuxKDE.tB, cssHSV_LinuxKDE.tC, Self.w);
      hsvGTK : HSVFactors.Create(cssHSV_GTK.tA, cssHSV_GTK.tB, cssHSV_GTK.tC, Self.w);
      hsvJavaAWT : HSVFactors.Create(cssHSV_JavaAWT.tA, cssHSV_JavaAWT.tB, cssHSV_JavaAWT.tC, Self.w);
      hsvApple : HSVFactors.Create(cssHSV_Apple.tA, cssHSV_Apple.tB, cssHSV_Apple.tC, Self.w);
    end;
    HSVColor := HSVColor * HSVFactors;
    Result.hue := HSVColor.x;
    Result.saturation := HSVColor.y;
    Result.value := HSVColor.z;
  end
  else
  begin
    Result.hue := HSVColor.x;
    Result.saturation := HSVColor.y;
    Result.value := HSVColor.z;
  end;
  Result.ColorSubSpaceType := SubSpaceType;
end;

function TBZColorVectoHelper.ToColorVectorHSL : TBZColorVector;
var
  Delta,
  Maxi,
  Mini:  Single;

begin
  Mini :=  Self.MinXYZComponent;
  Maxi :=   Self.MaxXYZComponent;

  Result.Z := (Maxi + Mini) * 0.5; //L

  if Maxi = Mini then
  begin
    // Achromatic case
    Result.Y := 0; // S
    Result.X := 0; // H undefined
  end
  else
  begin
    Delta := Maxi - Mini;

    if Result.Z < 0.5 then Result.Y := Delta / (Maxi + Mini)
    else Result.Y := Delta / (2 - (Maxi + Mini));

    if Self.x = Maxi then  Result.X := (Self.y - Self.x) / Delta
    else
      if Self.y = Maxi then Result.X := 2 + (Self.z - Self.x) / Delta
      else
        if Self.z = Maxi then Result.X := 4 + (Self.x - Self.y) / Delta;

    Result.X := Result.X / 6;
    if Result.X < 0 then  Result.X := Result.X + 1;
  end;
end;

function TBZColorVectoHelper.AsColorFloatHSL : TBZColorFloatHSL;
begin
  Result.hue := Self.x;
  Result.saturation := Self.y;
  Result.Luminosity := Self.z;
  Result.ColorSubSpaceType := hslNormalize;
end;

function TBZColorVectoHelper.ToColorFloatHSL(const SubSpaceType : THSLColorSubSpaceType) : TBZColorFloatHSL;
Var
  HSLColor, HSLFactors : TBZColorVector;
begin
  HSLColor := Self.ToColorVectorHSL;
  if SubSpaceType <> hslNormalize then
  begin
    Case SubSpaceType of
      hslDefault  : HSLFactors.Create(cssHSL_Default.tA, cssHSL_Default.tB, cssHSL_Default.tC, Self.w);
      hslWindows : HSLFactors.Create(cssHSL_Windows.tA, cssHSL_Windows.tB, cssHSL_Windows.tC, Self.w);
      hslPaintShopPro : HSLFactors.Create(cssHSL_PaintShopPro.tA, cssHSL_PaintShopPro.tB, cssHSL_PaintShopPro.tC, Self.w);
    end;
    HSLColor := HSLColor * HSLFactors;
    Result.hue := HSLColor.x;
    Result.saturation := HSLColor.y;
    Result.Luminosity := HSLColor.z;
  end
  else
  begin
    Result.hue := HSLColor.x;
    Result.saturation := HSLColor.y;
    Result.Luminosity := HSLColor.z;
  end;
  Result.ColorSubSpaceType := SubSpaceType;
end;

function TBZColorVectoHelper.ApplyColorMatrix(ColorMatrix : TBZColorMatrix) : TBZColorVector;
begin
  Result := ColorMatrix.Transform(Self);
end;

{%endregion%}

{%region%=====[ TBZColor32Helper ]==============================================}

procedure TBZColor32Helper.Create(Const aValue : TColor);
Var
  c : Integer;
begin
  C := ColorToRGB(aValue);
  Self.Red   := C and $000000ff;
  Self.Green := (C shr 8) and $000000ff;
  Self.Blue  := (C shr 16) and $000000ff;

 //Self.Red   := GetRValue(aValue); // aValue and $ff;
 //Self.Green := GetGValue(aValue); //(aValue and $ff00) shr 8;
 //Self.Blue  := GetBValue(aValue); //(aValue and $ff0000) shr 16;
 Self.Alpha := 255;
end;

procedure TBZColor32Helper.Create(Const aValue : TFPColor);
begin
  Self.Red := aValue.Red ShR 8;
  Self.Green := aValue.Green ShR 8;
  Self.Blue := aValue.Blue ShR 8;
  Self.Alpha := aValue.Alpha ShR 8;
end;

procedure TBZColor32Helper.Create(Const aValue : TBZColorVector);
var
 c : TBZColorVector;
 ci : TBZVector4i;
begin
 c := aValue * 255;
 ci := c.Round;
 ci:= ci.Clamp(0,255);

 Self.Red := ci.x;
 Self.Green := ci.y;
 Self.Blue := ci.z;
 Self.Alpha := ci.w;

end;

procedure TBZColor32Helper.Create(Const aValue : TBZColorFloatHSL);
begin

end;

procedure TBZColor32Helper.Create(Const aValue : TBZColorFloatHSV);
begin

end;

procedure TBZColor32Helper.Create(Const aValue : TBZColorHSL);
begin
  Self := aValue.ToColorRGBA;
end;

procedure TBZColor32Helper.Create(Const aValue : TBZColorHSV);
begin

end;

procedure TBZColor32Helper.Create(Const aValue : TBZColorYUV);
begin

end;

procedure TBZColor32Helper.Create(Const aValue : TBZColorCMYK);
begin

end;

procedure TBZColor32Helper.Create(Const aHexValue : String);
var
  l : integer;
  s : String;
  function CharConv (c : char) : longword;
  begin
    if (c >= 'A') and (c <= 'F') then
      result := ord (c) - ord('A') + 10
    else if (c >= '0') and (c <= '9') then
      result := ord (c) - ord('0')
    else
      raise exception.CreateFmt ('Wrong character (%s) in hexadecimal number', [c]);
  end;
  function convert (n : string) : word;
  var t,r: integer;
  begin
    result := 0;
    t := System.length(n);
    if t > 4 then
      raise exception.CreateFmt ('Too many bytes for color (%s)',[s]);
    for r := 1 to length(n) do
      result := (result shl 4) or CharConv(n[r]);
    // fill missing bits
    case t of
      1: result:=result or (result shl 4) or (result shl 8) or (result shl 12);
      2: result:=result or (result shl 8);
      3: result:=result or (result shl 12);
    end;
  end;
begin
  s := uppercase (aHexValue);
  l := System.length(s) div 3;
  Self.red   := (Convert(copy(s,1,l)));
  Self.green := (Convert(copy(s,l+1,l)));
  Self.blue  :=  Convert(copy(s,l+l+1,l));
  Self.alpha := 255;//AlphaOpaque;
end;

procedure TBZColor32Helper.Create(Const aBitFields : TBZColorBitFields; SrcColor : Integer);
begin

end;

function TBZColor32Helper.Negate : TBZColor32;
Const
   cVectorNegate : TBZColor32 = (X:255;Y:255;Z:255;W:255);
//Var
//  vc : TBZColor32;
begin
  //vc :=
  Result := cVectorNegate - Self;
  Result.Alpha:= Self.Alpha;
  //Result.Create(vc.Red,vc.Green,vc.Blue, Self.Alpha);
end;

procedure TBZColor32Helper.DirectNegate;
Const
   cVector3bNegate : TBZVector3b = (X:255;Y:255;Z:255);
Var
  vt3 : TBZVector3b;
begin
  vt3 := Self.AsVector3b;
  vt3 := cVector3bNegate-vt3;
  Self.Red   := vt3.Red;
  Self.Green := vt3.Green;
  Self.Blue  := vt3.Blue;
end;

{ Conversion en niveaux de gris
  - https://en.wikipedia.org/wiki/Grayscale
  - http://www.cs.uregina.ca/Links/class-info/425/Lab3/
  - https://digitalpadm.com/gray-level-transformation-image-enhancement-techniques-matlab-code/
  - https://www.giassa.net/?page_id=477
  - http://www.tannerhelland.com/3643/grayscale-image-algorithm-vb6/
}
function TBZColor32Helper.Desaturate(Const GrayMode : TBZGrayConvertMode; Const AGrayMatrixType : TBZGrayMatrixType; Const OptVal : Single; Const GammaFactor : Single) : TBZColor32;
var
  Lum:Integer;
  //GrayLUT : Array[0..255] of byte;
  ConversionFactor : Single;
  AGrayMatrix : TBZGrayMatrixConv;
  J : Integer;
  //i, GrayTempCalc, GrayValue : Integer;
begin
  result.alpha:= Self.alpha;
  Case GrayMode of
    gcmLightness:
    begin
      Lum:= ( BZMath.Max(Self.Red,Self.Green,Self.Blue) + BZMath.Min(Self.Red,Self.Green,Self.Blue)) div 2;
    end;

    gcmAverage :
    begin
     Lum:= Self.Luminosity; //Round((Self.red+Self.Green+Self.Blue)/3);
    end;

    gcmLuminance :
    begin
       Lum := Self.Luminance;
    end;

    gcmLuminosity :
    begin
     Case AGrayMatrixType of
      gmtNTSC : AGrayMatrix := cGCM_NTSC;
      gmtJPEG : AGrayMatrix := cGCM_JPEG;
      gmtAverage : AGrayMatrix := cGCM_Average;
      gmtPhotoShop : AGrayMatrix := cGCM_PhotoShop;
      gmtCIEObserverRef709 : AGrayMatrix := cGCM_CIEObserverRef709;
      gmtRMY : AGrayMatrix := cGCM_RMY;
      gmtRedMask : AGrayMatrix := cGCM_RedMask;
      gmtGreenMask : AGrayMatrix := cGCM_GreenMask;
      gmtBlueMask : AGrayMatrix := cGCM_BlueMask;
      gmtAlphaMask : AGrayMatrix := cGCM_AlphaMask;
     end;

      with AGrayMatrix do
      begin
        Lum := ClampByte(round(red*Self.Red + green*Self.green + blue*Self.blue));
      end;
    end;

    gcmDecomposeMin :
    begin
      Lum :=BZMath.Min(Self.Red,Self.Green,Self.Blue);
    end;

    gcmDecomposeMax :
    begin
      Lum :=BZMath.Max(Self.Red,Self.Green,Self.Blue);
    end;

    gcmColorMask :
    begin
      J:= Round(OptVal);
      Case J of
        0: Lum := Self.Red;
        1: Lum := Self.Green;
        2: Lum := Self.Blue;
        3: Lum := Self.Alpha;
      end;
    end;

     gcmCustomShades :
     begin
       J:= BZMath.Clamp(Round(OptVal),2,256);
       ConversionFactor:= 255 / (J-1);

        // On précalcul nos niveau de luminosité
        //For i := 0 To 255 do
        //begin
        //  grayTempCalc = Round((I / conversionFactor) + 0.5) * conversionFactor);
        //  GrayLUT[i]:= ClampByte(grayTempCalc);
        //end;
       Lum := Self.Luminosity;
       Lum:= ClampByte(Round(Round((Lum / ConversionFactor) + 0.5)  * ConversionFactor));
     end;

    gcmPowerLaw :
    begin
      AGrayMatrix := cGCM_CIEObserverRef709;
      with AGrayMatrix do
      begin
        Lum := ClampByte(round(red*Self.Red + green*Self.green + blue*Self.blue));
      end;
       If GammaFactor < 0.1 Then
        ConversionFactor := 10
      Else
        ConversionFactor  := 1 / GammaFactor;

      Lum := ClampByte(Round(255 * Math.Power((OptVal*Lum) * cColorFloatRatio, ConversionFactor)));
    end;

    gcmLogarithmic :
    begin
      AGrayMatrix := cGCM_CIEObserverRef709;
      with AGrayMatrix do
      begin
        Lum := ClampByte(round(red*Self.Red + green*Self.green + blue*Self.blue));
      end;
      Lum := ClampByte(Round(OptVal*BZMath.ln((1+Lum))));  // ou Log2 ????
    end;
   end;
   result.red := Lum;
   result.green := Lum;
   result.blue := Lum;
end;

function TBZColor32Helper.HyperSat : TBZColor32;
var
  S : Single;
begin
  result.alpha:= Self.alpha;
  S := (BZMath.Max(Self.Red,Self.Green,Self.Blue) - BZMath.Min(Self.Red,Self.Green,Self.Blue)) / 255;
  result.red := Math.min(Self.Red + round(Self.Red * S),255);
  result.green := Math.min(Self.Green + round(Self.Green * S),255);
  result.blue := Math.min(Self.Blue + round(Self.Blue * S),255);
end;

function TBZColor32Helper.Invert : TBZColor32;
Const
   cVector4bNegate : TBZColor32 = (X:255;Y:255;Z:255;W:255);
begin
  Result := cVector4bNegate - Self;
  Result.Alpha := Self.Alpha;
end;

function TBZColor32Helper.Average(Constref A : TBZColor32) : TBZColor32;
begin
  Result := (Self And A) + ((Self Xor A) * 0.5);
  //Result := Self;
  //Result.Red := (Self.Red + A.Red) div 2;
  //Result.Green := (Self.Green + A.Green) div 2;
  //Result.Blue := (Self.Blue + A.Blue) div 2;
end;

function TBZColor32Helper.Modulate(Constref A : TBZColor32) : TBZColor32;
begin
  Result.Alpha := Self.Alpha; //(Self.Alpha * A.Alpha) shr 8;
  Result.Red := (Self.Red * A.Red) shr 8;
  Result.Green := (Self.Green * A.Green) shr 8;
  Result.Blue := (Self.Blue * A.Blue) shr 8;
end;

function TBZColor32Helper.Combine(ConstRef A : TBZColor32; CombineMode : TBZColorCombineMode) : TBZColor32;
Var
  t, temp : Integer;
  b : byte;
  LProbIndex  : Cardinal;
  LRandomIndex: Integer;
  NewColorHSL, BackHSL, SrcHSL : TBZColorFloatHSL;
  NewColorHSV, BackHSV, SrcHSV : TBZColorFloatHSV;

begin
  Result := A;
  if A.Alpha = 0 then exit;
  Case CombineMode of
    cmNormal :;
    cmAdd :
    begin
      Result := (Self + A).Min(255);
    end;
    cmSub :
    begin
      //Result := (Self + A).Max(0);
      if ((A.Red+Self.Red) < 255) then Result.Red := 0 else Result.Red := (A.Red + Self.Red - 255);
      if ((A.Green+Self.Green) < 255) then Result.Green := 0 else Result.Green := (A.Green + Self.Green - 255);
      if ((A.Blue+Self.Blue) < 255) then Result.Blue := 0 else Result.Blue := (A.Blue + Self.Blue - 255);
    end;
    cmRealSub :
    begin
      Result.Red := ClampByte((Self.Red - A.Red));
      Result.Green := ClampByte((Self.Green - A.Green));
      Result.Blue := ClampByte((Self.Blue - A.Blue));
    end;
    cmMul :
    begin
      Result.Red :=ClampByte((A.Red * Self.Red) div 255);
      Result.Green :=ClampByte((A.Green * Self.Green) div 255);
      Result.Blue :=ClampByte((A.Blue * Self.Blue) div 255);
    end;
    cmDiv :
    begin
      //Result := Self div A;
      Result.Red := ClampByte( (Self.Red shr 8) div (A.Red + 1));
      Result.Green := ClampByte( (Self.Green shr 8) div (A.Green + 1));
      Result.Blue := ClampByte( (Self.Blue shr 8) div (A.Blue + 1));
    end;
    cmOr  : Result := Self or A;
    cmXor : Result := Self xor A;
    cmAnd : Result := Self and A;
    cmDifference :
    begin
      Result.Red   := Abs(Self.Red - A.Red);
      Result.Green := Abs(Self.Green - A.Green);
      Result.Blue  := Abs(Self.Blue - A.Blue);
    end;
    cmAverage : Result := (Self And A) + ((Self Xor A) * 0.5);  //(Self+A) div 2
    cmOverlay :
    begin
      if Self.Red < 128 then
      begin
        Result.Red := Self.Red * A.Red div 128;
      end
      else
      begin
        Result.Red := 255 - (255 - Self.Red) * (255 - A.Red) div 128;
      end;

      if Self.Green < 128 then
      begin
        Result.Green := Self.Green * A.Green div 128;
      end
      else
      begin
        Result.Green := 255 - (255 - Self.Green) * (255 - A.Green) div 128;
      end;

      if Self.Blue < 128 then
      begin
        Result.Blue := Self.Blue * A.Blue div 128;
      end
      else
      begin
        Result.Blue := 255 - (255 - Self.Blue) * (255 - A.Blue) div 128;
      end;
    end;
    cmScreen :
    begin
      //result := 255 - ((255-a) * (255-b) SHR 8);
      Result.Red := 255 - ((255 - A.Red) * (255 - Self.Red) div 255);
      Result.Green := 255 - ((255 - A.Green) * (255 - Self.Green) div 255);
      Result.Blue := 255 - ((255 - A.Blue) * (255 - Self.Blue) div 255);
    end;
    cmStamp :
    begin
      Result.Red := ClampByte( ((Self.Red + (A.Red + A.Red )) - 255));
      Result.Green := ClampByte(((Self.Green + (A.Green + A.Green )) - 255));
      Result.Blue := ClampByte(((Self.Blue + (A.Blue + A.Blue )) - 255));
    end;
    cmHeat :
    begin
      if Self.red = 0 then Result.Red := 0
      else
      begin
       t := (255 - A.Red);
       Temp := 255 - (t*t) div Self.Red;
       if Temp < 0 then Result.Red := 0 else Result.Red := ClampByte(Temp);
      end;

      if Self.Green = 0 then Result.Green := 0
      else
      begin
        t := (255 - A.Green);
        Temp := 255 - (t*t) div Self.Green;
        if Temp < 0 then Result.Green := 0 else Result.Green := ClampByte(Temp);
      end;

      if Self.Blue = 0 then Result.Blue := 0
      else
      begin
        t := (255 - A.Blue);
        Temp := 255 - (t*t) div Self.Blue;
        if Temp < 0 then Result.Blue := 0 else Result.Blue := ClampByte(Temp);
      end;
    end;
    cmFreeze :
    begin
      if A.red > 0 then
      begin
       t := (255 - Self.Red);
       Temp := 255 - (t*t) div A.Red;
       if Temp < 0 then Result.Red := 0 else Result.Red := ClampByte(Temp);
      end;

      if A.Green > 0 then
      begin
        t := (255 - Self.Green);
        Temp := 255 - (t*t) div A.Green;
        if Temp < 0 then Result.Green := 0 else Result.Green := ClampByte(Temp);
      end;

      if A.Blue > 0 then
      begin
        t := (255 - Self.Blue);
        Temp := 255 - (t*t) div A.Blue;
        if Temp < 0 then Result.Blue := 0 else Result.Blue := ClampByte(Temp);
      end;
    end;
    cmGlow :
    begin
      if Self.Red < 255 then
      begin
        Result.Red := ClampByte( (A.Red * A.Red) div (255 - Self.Red));
      end
      else
      begin
        Result.Red := 255;
      end;

      if Self.Green < 255 then
      begin
        Result.Green := ClampByte((A.Green * A.Green) div (255 - Self.Green));
      end
      else
      begin
        Result.Green := 255;
      end;

      if Self.Blue < 255 then
      begin
        Result.Blue := ClampByte((A.Blue * A.Blue) div (255 - Self.Blue));
      end
      else
      begin
        Result.Blue := 255;
      end;
    end;
    cmReflect :
    begin
      if A.Red <> 255 then
      begin
        Result.Red := ClampByte( (Self.Red * Self.Red) div (255 - A.Red));
      end;

      if A.Green <> 255 then
      begin
        Result.Green := ClampByte((Self.Green * Self.Green) div (255 - A.Green));
      end;

      if A.Blue <> 255 then
      begin
        Result.Blue := ClampByte((Self.Blue * Self.Blue) div (255 - A.Blue));
      end;
    end;
    cmExclusion :
    begin
      Result.Red := A.Red + Self.Red - ((A.Red + A.Red) * Self.Red) div 255;
      Result.Green := A.Green + Self.Green - ((A.Green + A.Green) * Self.Green) div 255;
      Result.Blue := A.Blue + Self.Blue - ((A.Blue + A.Blue) * Self.Blue) div 255;
    end;
    cmNegate :
    begin
      Result.Red := 255 - Abs(255 - Self.Red - A.Red);
      Result.Green := 255 - Abs(255 - Self.Green - A.Green);
      Result.Blue := 255 - Abs(255 - Self.Blue - A.Blue);
    end;
    cmLightenOnly :
    begin

      if A.Red < Self.Red then
      begin
        Result.Red := Self.Red;
      end;

      if A.Green < Self.Green then
      begin
        Result.Green := Self.Green;
      end;

      if A.Blue < Self.Blue then
      begin
        Result.Blue := Self.Blue;
      end;
    end;
    cmDarkenOnly :
    begin

      if A.Red > Self.Red then
      begin
        Result.Red := Self.Red;
      end;

      if A.Green > Self.Green then
      begin
        Result.Green := Self.Green;
      end;

      if A.Blue > Self.Blue then
      begin
        Result.Blue := Self.Blue;
      end;
    end;
    cmColorBurn :
    begin
      if A.Red > 0 then Result.Red :=ClampByte(255 - (255 - Self.Red) * 255 div A.Red);
      if A.Green > 0 then Result.Green :=ClampByte(255 - (255 - Self.Green) * 255 div A.Green);
      if A.Blue > 0 then Result.Blue :=ClampByte(255 - (255 - Self.Blue) * 255 div A.Blue);
      //if A.Red > 0 then Result.Red :=ClampByte(255 - ((255 - Self.Red)  div (A.Red) ));
      //if A.Green > 0 then Result.Green :=ClampByte(255 - ((255 - Self.Green)  div (A.Green) ));
      //if A.Blue > 0 then Result.Blue :=ClampByte(255 - ((255 - Self.Blue) div (A.Blue) ));
    end;
    cmInverseColorBurn :
    begin
      if Self.Red > 0 then Result.Red :=ClampByte(255 - (255 - A.Red) * 255 div Self.Red);
      if Self.Green > 0 then Result.Green :=ClampByte(255 - (255 - A.Green) * 255 div Self.Green);
      if Self.Blue > 0 then Result.Blue :=ClampByte(255 - (255 - A.Blue) * 255 div Self.Blue);
      //if Self.Red = 0 then
      //begin
      //  Result.Red :=0;
      //end
      //else
      //begin
      //  Temp := 255 - (255 - A.Red) * 255 div Self.Red;
      //  if Temp < 0 then
      //  begin
      //    Result.Red := 0;
      //  end
      //  else
      //  begin
      //    Result.Red := ClampByte(Temp);
      //  end;
      //end;
      //
      //if Self.Green = 0 then
      //begin
      //  Result.Green :=0;
      //end
      //else
      //begin
      //  Temp := 255 - (255 - A.Green) * 255 div Self.Green;
      //  if Temp < 0 then
      //  begin
      //    Result.Green := 0;
      //  end
      //  else
      //  begin
      //    Result.Green := ClampByte(Temp);
      //  end;
      //end;
      //
      //if Self.Blue = 0 then
      //begin
      //  Result.Blue :=0;
      //end
      //else
      //begin
      //  Temp := 255 - (255 - A.Blue) * 255 div Self.Blue;
      //  if Temp < 0 then
      //  begin
      //    Result.Blue := 0;
      //  end
      //  else
      //  begin
      //    Result.Blue := ClampByte(Temp);
      //  end;
      //end;
    end;
    cmSoftColorBurn :
    begin
      if (Self.Red + A.Red) < 256 then
      begin
        if Self.Red = 255 then
        begin
          Result.Red := 255;
        end
        else
        begin
          Result.Red := ClampByte( A.Red * 128 div (255 - Self.Red));
        end;
      end
      else
      begin
        Result.Red :=ClampByte( 255 - (255 - Self.Red) * 128 div A.Red);
      end;

      if (Self.Green + A.Green) < 256 then
      begin
        if Self.Green = 255 then
        begin
          Result.Green := 255;
        end
        else
        begin
          Result.Green := ClampByte( A.Green * 128 div (255 - Self.Green));
        end;
      end
      else
      begin
        Result.Green :=ClampByte( 255 - (255 - Self.Green) * 128 div A.Green);
      end;

      if (Self.Blue + A.Blue) < 256 then
      begin
        if Self.Blue = 255 then
        begin
          Result.Blue := 255;
        end
        else
        begin
          Result.Blue := ClampByte( A.Blue * 128 div (255 - Self.Blue));
        end;
      end
      else
      begin
        Result.Blue :=ClampByte( 255 - (255 - Self.Blue) * 128 div A.Blue);
      end;
    end;
    cmColorDodge :
    begin
      if A.Red < 255 then Result.Red := ClampByte( (Self.Red div 255) div ((255 - A.Red)));
      if A.Green < 255 then Result.Green := ClampByte( (Self.Green div 255) div ((255 - A.Green)));
      if A.Blue < 255 then Result.Blue := ClampByte( (Self.Blue Div 255) div ((255 - A.Blue)));
    end;
    cmInverseColorDodge :
    begin
      if Self.Red = 255 then
      begin
        Result.Red := 255;
      end
      else
      begin
        Result.Red := ClampByte(A.Red * 255 div (255 - Self.Red));
      end;

      if Self.Green = 255 then
      begin
        Result.Green := 255;
      end
      else
      begin
        Result.Green := ClampByte(A.Green * 255 div (255 - Self.Green));
      end;

      if Self.Blue = 255 then
      begin
        Result.Blue := 255;
      end
      else
      begin
        Result.Blue := ClampByte(A.Blue * 255 div (255 - Self.Blue));
      end;
    end;
    cmSoftColorDodge :
    begin
      if (Self.Red + A.Red) < 256 then
      begin
        if A.Red <> 255 then
        begin
          Result.Red := ClampByte(Self.Red * 128 div (255 - A.Red));
        end;
      end
      else
      begin
        Result.Red :=ClampByte( 255 - (255 - A.Red) * 128 div Self.Red );
      end;

      if (Self.Green + A.Green) < 256 then
      begin
        if A.Green <> 255 then
        begin
          Result.Green := ClampByte(Self.Green * 128 div (255 - A.Green));
        end;
      end
      else
      begin
        Result.Green :=ClampByte( 255 - (255 - A.Green) * 128 div Self.Green );
      end;

      if (Self.Blue + A.Blue) < 256 then
      begin
        if A.Blue <> 255 then
        begin
          Result.Blue := ClampByte(Self.Blue * 128 div (255 - A.Blue));
        end;
      end
      else
      begin
        Result.Blue :=ClampByte( 255 - (255 - A.Blue) * 128 div Self.Blue );
      end;
    end;
    cmHardLight :
    begin
      if A.Red <= 128 then
      begin
        Result.Red := ClampByte(Self.Red * A.Red div 128);
      end
      else
      begin
        // 255 - (A -128)  * 255 - B) / 256
        Result.Red :=ClampByte(255 - (255 - Self.Red) * (255 - A.Red) div 128);
      end;

      if A.Green <= 128 then
      begin
        Result.Green := ClampByte(Self.Green * A.Green div 128);
      end
      else
      begin
        Result.Green :=ClampByte(255 - (255 - Self.Green) * (255 - A.Green) div 128);
      end;

      if A.Blue <= 128 then
      begin
        Result.Blue := ClampByte(Self.Blue * A.Blue div 128);
      end
      else
      begin
        Result.Blue :=ClampByte(255 - (255 - Self.Blue) * (255 - A.Blue) div 128);
      end;
    end;
    cmSoftLight :
    begin
      b := Self.Red * A.Red div 255;
      Result.Red := ClampByte( b + Self.Red * (255 - ((255 - Self.Red) * (255 - A.Red) div 255 ) - b  ) div 255);
      b := Self.Green * A.Green div 255;
      Result.Green := ClampByte( b + Self.Green * (255 - ((255 - Self.Green) * (255 - A.Green) div 255 ) - b  ) div 255);
      b := Self.Blue * A.Blue div 255;
      Result.Blue := ClampByte( b + Self.Blue * (255 - ((255 - Self.Blue) * (255 - A.Blue) div 255 ) - b  ) div 255);
    end;
    cmBrightLight :
    begin
      Result.Red := SqrtCombineLUT[A.Red * Self.Red];
      Result.Green := SqrtCombineLUT[A.Green * Self.Green];
      Result.Blue := SqrtCombineLUT[A.Blue * Self.Blue];
    end;
    cmLinearLight :
    begin
      if Self.Red < 128 then
      begin
        t := (Self.Red+Self.Red);
        if ((A.Red+t) < 255) then Result.Red := 0 else Result.Red := (A.Red + t) - 255;
      end
      else
      begin
        Result.Red := Math.Min(255,(A.Red + (Self.Red-128)*2));
      end;

      if Self.Green < 128 then
      begin
        t := (Self.Green+Self.Green);
        if ((A.Green+t) < 255) then Result.Green := 0 else Result.Green := (A.Green + t) - 255;
      end
      else
      begin
        Result.Green := Math.Min(255,(A.Green + (Self.Green-128)*2));
      end;

      if Self.Blue < 128 then
      begin
        t := (Self.Blue+Self.Blue);
        if ((A.Blue+t) < 255) then Result.Blue := 0 else Result.Blue := (A.Blue + t) - 255;
      end
      else
      begin
        Result.Blue := Math.Min(255,(A.Blue + (Self.Blue-128)*2));
      end;

    end;
    cmVividLight :
    Begin
      // (B < 128)?ChannelBlend_ColorBurn(A,(2 * B)):ChannelBlend_ColorDodge(A,(2 * (B - 128))))
      if Self.Red < 128 then
      begin
        if A.Red > 0 then Result.Red :=ClampByte(255 - (((255 - (Self.Red + Self.Red)) div 255) div A.Red ));
      end
      else
      begin
        if A.Red < 255 then Result.Red := ClampByte((((Self.Red-128)*2) div 255) div (255 - A.Red));
      end;

      if Self.Green < 128 then
      begin
        if A.Green > 0 then Result.Green :=ClampByte(255 - (((255 - (Self.Green + Self.Green)) div 255) div A.Green ));
      end
      else
      begin
        if A.Green < 255 then Result.Green := ClampByte((((Self.Green-128)*2) div 255) div (255 - A.Green));
      end;

      if Self.Blue < 128 then
      begin
        if A.Blue > 0 then Result.Blue :=ClampByte(255 - (((255 - (Self.Blue + Self.Blue)) div 255) div A.Blue ));
      end
      else
      begin
        if A.Blue < 255 then Result.Blue := ClampByte((((Self.Blue-128)*2) div 255) div (255 - A.Blue));
      end;
    end;
    cmPinLight :
    Begin
      if Self.Red < 128 then
      begin
        t := Self.Red + Self.Red;
        if A.Red > t then Result.Red := t;
      end
      else
      begin
        t := (Self.Red - 128) * 2;
        if A.Red < t then Result.Red := t;
      end;

      if Self.Green < 128 then
      begin
        t := Self.Green + Self.Green;
        if A.Green > t then Result.Green := t;
      end
      else
      begin
        t := (Self.Green - 128) * 2;
        if A.Green < t then Result.Green := t;
      end;

      if Self.Blue < 128 then
      begin
        t := Self.Blue + Self.Blue;
        if A.Blue > t then Result.Blue := t;
      end
      else
      begin
        t := (Self.Blue - 128) * 2;
        if A.Blue < t then Result.Blue := t;
      end;
    end;
    cmHardMix :
    Begin
      t := Self.Red;
      if Self.Red < 128 then
      begin
        if A.Red > 0 then t :=ClampByte(255 - (((255 - (Self.Red + Self.Red)) div 255) div A.Red ));
      end
      else
      begin
        if A.Red < 255 then t := ClampByte((((Self.Red-128)*2) div 255) div (255 - A.Red));
      end;
      if t < 128 then Result.red := 0 else Result.Red := 255;

      t := Self.Green;
      if Self.Green < 128 then
      begin
        if A.Green > 0 then t :=ClampByte(255 - (((255 - (Self.Green + Self.Green)) div 255) div A.Green ));
      end
      else
      begin
        if A.Green < 255 then t := ClampByte((((Self.Green-128)*2) div 255) div (255 - A.Green));
      end;
      if t < 128 then Result.Green := 0 else Result.Green := 255;

      t := Self.Blue;
      if Self.Blue < 128 then
      begin
        if A.Blue > 0 then t :=ClampByte(255 - (((255 - (Self.Blue + Self.Blue)) div 255) div A.Blue ));
      end
      else
      begin
        if A.Blue < 255 then t := ClampByte((((Self.Blue-128)*2) div 255) div (255 - A.Blue));
      end;
      if t < 128 then Result.Blue := 0 else Result.Blue := 255;

    end;
    cmPhoenix :
    Begin
      Result.Red := ClampByte(Math.Min(A.Red, Self.Red) - Math.Max(A.Red, Self.Red) + 255);
      Result.Green := ClampByte(Math.Min(A.Green, Self.Green) - Math.Max(A.Green, Self.Green) + 255);
      Result.Blue := ClampByte(Math.Min(A.Blue, Self.Blue) - Math.Max(A.Blue, Self.Blue) + 255);
    end;
    cmInterpolation :
    begin
      Result.Red :=  LerpCombineCosineLUT[A.Red] +  LerpCombineCosineLUT[Self.Red];
      Result.Green :=  LerpCombineCosineLUT[A.Green] +  LerpCombineCosineLUT[Self.Green];
      Result.Blue :=  LerpCombineCosineLUT[A.Blue] +  LerpCombineCosineLUT[Self.Blue];
    end;
    cmLuminosity :
    begin
      BackHSL:= Self.AsColorVector.ToColorVectorHSL.AsColorFloatHSL;
      SrcHSL := A.AsColorVector.ToColorVectorHSL.AsColorFloatHSL;
      NewColorHSL.Create(BackHSL.Hue, BackHSL.Saturation, SrcHSL.Luminosity, hslNormalize);
      Result := NewColorHSL.ToColorRBGA;
      Result.Alpha := A.Alpha;
    end;
    cmColor :
    begin
      BackHSL:= Self.AsColorVector.ToColorVectorHSL.AsColorFloatHSL;
      SrcHSL := A.AsColorVector.ToColorVectorHSL.AsColorFloatHSL;
      NewColorHSL.Create(SrcHSL.Hue, SrcHSL.Saturation, BackHSL.Luminosity, hslNormalize);
      Result := NewColorHSL.ToColorRBGA;
      Result.Alpha := A.Alpha;
    end;
    cmValue :
    begin
      BackHSV:= Self.AsColorVector.ToColorVectorHSV.AsColorFloatHSV;
      SrcHSV := A.AsColorVector.ToColorVectorHSV.AsColorFloatHSV;
      NewColorHSV.Create(BackHSV.Value, BackHSV.Value, SrcHSV.Value, hsvNormalize);
      Result := NewColorHSV.ToColorRBGA;
      Result.Alpha := A.Alpha;
    end;
    cmSaturation :
    begin
      BackHSL:= Self.AsColorVector.ToColorVectorHSL.AsColorFloatHSL;
      SrcHSL := A.AsColorVector.ToColorVectorHSL.AsColorFloatHSL;
      NewColorHSL.Create(BackHSL.Hue, SrcHSL.Saturation, BackHSL.Luminosity, hslNormalize);
      Result := NewColorHSL.ToColorRBGA;
      Result.Alpha := A.Alpha;
    end;
    cmHue :
    begin
      BackHSL:= Self.AsColorVector.ToColorVectorHSL.AsColorFloatHSL;
      SrcHSL := A.AsColorVector.ToColorVectorHSL.AsColorFloatHSL;
      NewColorHSL.Create(SrcHSL.Hue, BackHSL.Saturation, BackHSL.Luminosity, hslNormalize);
      Result := NewColorHSL.ToColorRBGA;
      Result.Alpha := A.Alpha;
    end;
    cmGrainMerge :
    begin
      Result.Red := ClampByte(A.Red + Self.Red - 128);
      Result.Green := ClampByte(A.Green + Self.Green - 128);
      Result.Blue := ClampByte(A.Blue + Self.Blue - 128);
    end;
    cmGrainExtract :
    begin
      Temp := Self.Red - A.Red + 128;
      if Temp < 0 then
      begin
        Result.Red := 0;
      end
      else
      begin
        Result.Red := ClampByte(Temp);
      end;

      Temp := Self.Green - A.Green + 128;
      if Temp < 0 then
      begin
        Result.Green := 0;
      end
      else
      begin
        Result.Green := ClampByte(Temp);
      end;

      Temp := Self.Blue - A.Blue + 128;
      if Temp < 0 then
      begin
        Result.Blue := 0;
      end
      else
      begin
        Result.Blue := ClampByte(Temp);
      end;
    end;
    cmRed :
    begin
      Result.Red := A.Red;
      Result.Green := Self.Green;
      Result.Blue := Self.Blue;
    end;
    cmGreen :
    begin
      Result.Red := Self.Red;
      Result.Green := A.Green;
      Result.Blue := Self.Blue;
    end;
    cmBlue :
    begin
      Result.Red := Self.Red;
      Result.Green := Self.Green;
      Result.Blue := A.Blue;
    end;
    cmDissolve :
    begin
      LProbIndex   := Round( (A.Alpha * 100) / 255 );
      LRandomIndex := Random(100);
      if not  ProbCombineLUT[LProbIndex, LRandomIndex] then
      begin
        Result.AsInteger := A.AsInteger and $00FFFFFF;
      end;
    end;
    cmErase :
    Begin
      if Self.Alpha > 0 then Result := clrTransparent;
    End;
    cmMin :
    begin
      Result := Self.Min(A);
    end;
    cmMax :
    begin
      Result := Self.Max(A);
    end;
    cmAmplitude :
    begin
      Result.Red := ClampByte(Round(System.Sqrt((Self.Red * Self.Red) + (A.Red * A.Red)) * cInvSqrt2));
      Result.Green := ClampByte(Round(System.Sqrt((Self.Green * Self.Green) + (A.Green * A.Green)) * cInvSqrt2));
      Result.Blue := ClampByte(Round(System.Sqrt((Self.Blue * Self.Blue) + (A.Blue * A.Blue)) * cInvSqrt2));
    end;
  end;
end;

function TBZColor32Helper.Mix(Constref A : TBZColor32; Factor : Single) : TBZColor32;
var
  pct:single;
begin
  pct:=abs(1.0-Factor);
  Result :=((Self * pct ) + (A * Factor));
  Result.Alpha :=Self.Alpha;
end;

function TBZColor32Helper.MixInv(Constref A : TBZColor32; Factor : Single) : TBZColor32;
var
  pct:single;
begin
 pct:=abs(1.0-Factor);
 Result :=((Self * pct ) - (A * Factor));
 Result.Alpha :=Self.Alpha;
end;


{ https://fr.wikipedia.org/wiki/Alpha_blending
  If pre-multiplied alpha is used, the above equations are simplified to :
  https://en.wikipedia.org/wiki/Alpha_compositing
  https://developer.nvidia.com/content/alpha-blending-pre-or-not-pre
}
function TBZColor32Helper.AlphaBlend(DestPix : TBZColor32) : TBZColor32;
// alpha := pix.alpha / 255
// color = alpha * (src - dest) + dest
// ou ((alpha*(src-dest))/255)+dest
var
 // c12 : Integer;
//  alphaFactor,factor, factor2:single;
  fct, fct2,r,g,b,a : Integer;
begin
  if DestPix.Alpha = 255 then Result := DestPix
  else  if DestPix.Alpha = 0 then Result:= Self
  else
  begin
    //factor := DestPix.Alpha *_FloatColorRatio;
    //factor2 := 1 - Factor;
//    AlphaFactor := 1/(factor * (Self.Alpha *_FloatColorRatio) * factor2);
    //Result.Red   := ClampByte(Round((Self.Red*Factor2)+(DestPix.Red*factor))); // * AlphaFactor));
    //Result.Green := ClampByte(Round((Self.Green*Factor2)+(DestPix.Green*Factor))); //* AlphaFactor));
    //Result.Blue  := ClampByte(Round((Self.Blue*Factor2)+(DestPix.Blue*Factor))); //* AlphaFactor));
    //Result.alpha := ClampByte(Round((Self.Alpha*Factor2)+(DestPix.Alpha*Factor))); //* AlphaFactor));

    // Normal
    //fct := DestPix.Alpha;
    //fct2 := 255 - fct;
    //Result.Red   := ClampByte(((Self.Red*Fct2)+(DestPix.Red*fct)) div 255); // * AlphaFactor));
    //Result.Green := ClampByte(((Self.Green*Fct2)+(DestPix.Green*Fct) )div 255); //* AlphaFactor));
    //Result.Blue  := ClampByte(((Self.Blue*Fct2)+(DestPix.Blue*Fct)) div 255); //* AlphaFactor));
    //Result.alpha := ClampByte(((Self.Alpha*Fct2)+(DestPix.Alpha*Fct)  )div 255); //* AlphaFactor));

    fct := DestPix.Alpha;
    fct2 := 255 - fct;
    r := ((Self.Red*Fct2)+(DestPix.Red*fct));
    g := ((Self.Green*Fct2)+(DestPix.Green*Fct));
    b := ((Self.Blue*Fct2)+(DestPix.Blue*Fct));
    a := ((Self.Alpha*Fct2)+(DestPix.Alpha*Fct));
    Result.Red   := (r+1 + (r shr 8)) shr 8;    // Methode rapide pour diviser par 255
    Result.Green := (g+1 + (g shr 8)) shr 8;
    Result.Blue  := (b+1 + (b shr 8)) shr 8;
    Result.alpha := (a+1 + (a shr 8)) shr 8; //fct;


  End;
  (*Pixel AlphaBlendPixels(Pixel p1, Pixel p2)

      static const int AMASK = 0xFF000000;
      static const int RBMASK = 0x00FF00FF;
      static const int GMASK = 0x0000FF00;
      static const int AGMASK = AMASK | GMASK;
      static const int ONEALPHA = 0x01000000;
      unsigned int a = (p2 & AMASK) >> 24;
      unsigned int na = 255 - a;
      unsigned int rb = ((na * (p1 & RBMASK)) + (a * (p2 & RBMASK))) >> 8;
      unsigned int ag = (na * ((p1 & AGMASK) >> 8)) + (a * (ONEALPHA | ((p2 & GMASK) >> 8)));
      return ((rb & RBMASK) | (ag & AGMASK));
 *)
{ In\   EAX = background color (ZRBG) 32bit (Z mean zero, always is zero)
; In\   EDX = foreground color (RBGA) 32bit
; Out\  EAX = new color
alpha_blend_sse:
    xor r14, r14
    xor r12, r12
    xor r13, r13

    movzx r15, dl               ; av: alpha number (0x00--->0xFF)
    movzx ecx, dl
    not ecx                     ; faster than 255 - dl
    mov r14b, cl                ; rem

    shr edx, 8
    and edx, 0x00FFFFFF
    mov r12d, edx
    mov r13d, eax               ; RBGA ---> ZRGB

    mov rax, 0x0000FF
    movq xmm3, rax

    ; s: eax
    ; d: edx

    ;=============================red = ((s >> 16) * rem + (d >> 16) * av) >> 8;
    movq xmm0, r12
    psrld xmm0, 0x10
    movq xmm1, r14
    pmuludq xmm1, xmm0
    movq xmm0, r13
    psrld xmm0, 0x10
    movq xmm2, r15
    pmuludq xmm2, xmm0
    addps xmm2, xmm1
    psrld xmm2, 0x8
    movq rax, xmm2
    mov r9b, al
    shl r9d, 8

    ;=============================green = (((s >> 8) & 0x0000ff) * rem + ((d >> 8) & 0x0000ff) * av) >> 8;
    movq xmm0, r12
    psrld xmm0, 0x8
    andps xmm0, xmm3
    movq xmm1, r14
    pmuludq xmm1, xmm0
    movq xmm0, r13
    psrld xmm0, 0x8
    andps xmm0, xmm3
    movq xmm2, r15
    pmuludq xmm2, xmm0
    addps xmm2, xmm1
    psrld xmm2, 0x8
    movq rax, xmm2
    mov r9b, al
    shl r9d, 8

    ;=============================blue = ((s & 0x0000ff) * rem + (d & 0x0000ff) * av) >> 8;
    movq xmm0, r12
    andps xmm0, xmm3
    movq xmm1, r14
    andps xmm1, xmm3
    pmuludq xmm1, xmm0
    movq xmm0, r13
    andps xmm0, xmm3
    movq xmm2, r15
    andps xmm2, xmm3
    pmuludq xmm2, xmm0
    addps xmm2, xmm1
    psrld xmm2, 0x8
    movq rax, xmm2
    mov r9b, al

    mov eax, r9d
    ret }
End;

function TBZColor32Helper.BlendFour(c1, c2, c3, c4 : TBZColor; w1, w2, w3, w4 : Single) : TBZColor;
Var
  af : Single;
  {$CODEALIGN VARMIN=16}
  cv1, cv2, cv3, cv4, outcolor : TBZColorVector;
  {$CODEALIGN VARMIN=4}

begin
  cv1.Create(c1);
  cv2.Create(c2);
  cv3.Create(c3);
  cv4.Create(c4);
  af := (cv1.Alpha * w1) + (cv2.Alpha * w2) + (cv3.Alpha * w3) + (cv4.Alpha * w4);
  if af = 0 then
  begin
    Result := clrTransparent;
    Exit;
  end
  else
  begin
    outColor.Alpha := af;
    af := 1 / af;
    outColor.Red := ((cv1.Alpha * cv1.Red * w1) + (cv2.Alpha * cv2.Red * w2) + (cv3.Alpha * cv3.Red * w3) + (cv4.Alpha * cv4.Red * w4)) * af;
    outColor.Green := ((cv1.Alpha * cv1.Green * w1) + (cv2.Alpha * cv2.Green * w2) + (cv3.Alpha * cv3.Green * w3) + (cv4.Alpha * cv4.Green * w4)) * af;
    outColor.Blue := ((cv1.Alpha * cv1.Blue * w1) + (cv2.Alpha * cv2.Blue * w2) + (cv3.Alpha * cv3.Blue * w3) + (cv4.Alpha * cv4.Blue * w4)) * af;
    Result.Create(outColor);
  end;
end;

function TBZColor32Helper.BlendFour(c1, c2, c3, c4 : TBZColor; weight : Single) : TBZColor;
begin
  Result := Self.BlendFour(c1,c2,c3,c4,weight,weight,weight,weight);
end;

function TBZColor32Helper.BlendWithoutAlpha(DestPix: TBZColor):TBZColor;
// alpha := pix.alpha / 255
// color = alpha * (src - dest) + dest
// ou ((alpha*(src-dest))/255)+dest
var
 // c12 : Integer;
  factor, factor2:single;
begin
  if DestPix.Alpha = 255 then Result := DestPix
  else  if DestPix.Alpha = 0 then Result:= Self
  else
  begin
    factor := DestPix.Alpha / 255;
    factor2 := 1 - Factor;
    Result.Red  := ClampByte(Round((Self.Red*Factor2)+(DestPix.Red*factor)));
    Result.Green  := ClampByte(Round((Self.Green*Factor2)+(DestPix.Green*Factor)));
    Result.Blue  := ClampByte(Round((Self.Blue*Factor2)+(DestPix.Blue*Factor)));
    Result.alpha := Self.Alpha; //ClampByte(Round((Self.Alpha*Factor2)+(DestPix.Alpha*Factor)));
  End;

End;

function TBZColor32Helper.AlphaMix(DestPix : TBZColor32) : TBZColor32;
var
  pct, factor:single;
begin
 if Self.Alpha >0 then
 begin
   factor := DestPix.Alpha / Self.Alpha ;
   pct:=(1.0-Factor);
   Result :=((Self * pct ) + (DestPix * Factor));
   Result.Alpha := 255; //DestPix.Alpha;
 end
 else
 Result := DestPix;

end;

(*var
  //c12 : Integer;
  factor, factor2:single;
begin
  if DestPix.Alpha = 255 then Result := DestPix
  else  if DestPix.Alpha = 0 then Result:= Self
  else
  begin
    factor := DestPix.Alpha / 255;
    factor2 := 1 - Factor;
    //c12 := Self.alpha + DestPix.alpha;
    Result.Red  := Round((Self.Red*Factor)+(DestPix.Red*factor2));
    Result.Green  := Round((Self.Green*Factor)+(DestPix.Green*Factor2));
    Result.Blue  := Round((Self.Blue*Factor)+(DestPix.Blue*Factor2));
    Result.alpha := Round((Self.Alpha*Factor)+(DestPix.Alpha*Factor2));
  End;
end; *)

function TBZColor32Helper.Blend(Constref A : TBZColor32; const SrcFactor : TBZBlendingFactor; const DestFactor : TBZBlendingFactor) : TBZColor32;
Const
  cColorVectorONE : TBZColorVector = (x:1.0;y:1.0;z:1.0;w:1.0);
 var
   FactorSrc, FactorDst, FS, FD, RS, RD, R : TBZColorVector;
   t : Single;
 begin
   // On premultiplie
   FS := Self.AsColorVector;
   FD := A.AsColorVector;
   t := FS.Alpha;
   FS := FS * t;
   FS.Alpha := t;
   t := FD.Alpha;
   FD := FD * t;
   FD.Alpha := t;


   // Determine current blending factors
   case SrcFactor of
     bfZero              : FactorSrc.Create(0, 0, 0, 0);
     bfOne               : FactorSrc := cColorVectorONE;
     bfSrcAlpha          : FactorSrc.Create(FS.Alpha, FS.Alpha, FS.Alpha, FS.Alpha);
     bfOneMinusSrcAlpha  :
     begin
       t := 1 - FS.Alpha;
       FactorSrc.Create(t,t,t,t);
     end;
     bfDstAlpha          : FactorSrc.Create(FD.Alpha, FD.Alpha, FD.Alpha, FD.Alpha);
     bfOneMinusDstAlpha  :
     begin
       t := 1 - FD.Alpha;
       FactorSrc.Create(t,t,t,t);
     end;
     bfSrcColor         : FactorSrc := FS;
     bfOneMinusSrcColor : FactorSrc := cColorVectorONE - FS;
     bfDstColor         : FactorSrc := FD;
     bfOneMinusDstColor : FactorSrc := cColorVectorONE - FD;
   end;

   case DestFactor of
     bfZero              : FactorDst.Create(0, 0, 0, 0);
     bfOne               : FactorDst := cColorVectorONE;
     bfSrcAlpha          : FactorDst.Create(FS.Alpha, FS.Alpha, FS.Alpha, FS.Alpha);
     bfOneMinusSrcAlpha  :
     begin
       t := 1 - FS.Alpha;
       FactorDst.Create(t,t,t,t);
     end;
     bfDstAlpha          : FactorDst.Create(FD.Alpha, FD.Alpha, FD.Alpha, FD.Alpha);
     bfOneMinusDstAlpha  :
     begin
       t := 1 - FD.Alpha;
       FactorDst.Create(t,t,t,t);
     end;
     bfSrcColor         : FactorDst := FS;
     bfOneMinusSrcColor : FactorDst := cColorVectorONE - FS;
     bfDstColor         : FactorDst := FD;
     bfOneMinusDstColor : FactorDst := cColorVectorONE - FD;
   end;

   // Compute blending formula
   //RS := ((FS * FS.Alpha) * FactorSrc).Min(1.0);
   //RD := ((FD * FD.Alpha) * FactorDst).Min(1.0);
   RS := (FS * FactorSrc).Min(1.0);
   RD := (FD * FactorDst).Min(1.0);
   R := RS + RD;
   //r.Red   := Math.min(1.0,(Self.Red * FactorSrc.Red)) + Math.min(1.0,(A.Red * FactorDstRed));
   //r.Green := Math.min(1.0,(Self.Green * FactorSrc.Green)) + Math.min(1.0,(A.Green * FactorDstGreen));
   //r.Blue  := Math.min(1.0,(Self.Blue * FactorSrc.Blue)) + Math.min(1.0,(A.Blue * FactorDstBlue));
   //r.Alpha := Math.min(1.0,(Self.Alpha * FactorSrc.Alpha)) + Math.min(1.0,(A.Alpha * FactorDstAlpha));

   Result.Create(R);
end;

function TBZColor32Helper.Blend(AColor : TBZColor; Const MasterAlpha : Byte) : TBZColor;
var
  mAlpha, AlphaFactor, factor, factor2 : single;
begin
  AlphaFactor := MasterAlpha * _FloatColorRatio;
  mAlpha := AColor.Alpha * AlphaFactor;

 if (mAlpha = 0) then
  begin
    Result := Self; //BackColor
    Exit;
  end;

  if (mAlpha < 255) and (Self.Alpha > 0)  then
  begin
    factor := (mAlpha *_FloatColorRatio);
    factor2 := 1 - Factor;

    Result.Red   := ClampByte(Round(((Self.Red*Factor2)+(AColor.Red*factor))));
    Result.Green := ClampByte(Round(((Self.Green*Factor2)+(AColor.Green*Factor))));
    Result.Blue  := ClampByte(Round(((Self.Blue*Factor2)+(AColor.Blue*Factor))));
    Result.alpha := ClampByte(Round(((Self.Alpha*Factor2)+(AColor.Alpha*Factor))));
  end
  else
  begin
    Result := AColor;
    Exit;
  end;
end;

Function TBZColor32Helper.BlendHQ(ForeColor, CombinedColor : TBZColor; Const MasterAlpha : Byte) : TBZColor;
var
  ForeAlpha  : Single;
  rA, Delta : Single;
  ColorFore, ColorBack, ColorBlend, ResultColor,
  AlphaFore, AlphaBack, rAlpha, c1,c2: TBZColorVector;
begin
  Result := Self;
  //fct = Self.A; //MasterAlpha
  //fct2 := 255 - fct;

  {Combine la valeur Alpha de la couleur avec l'alpha maitre}
  Delta := MasterAlpha * _FloatColorRatio;
  if Delta = 0 then exit; // Opcité à zero on quitte, rien à afficher

  // On tavail avec des couleurs en virgule flottante pour plus de précision
  ColorFore := ForeColor.AsColorVector;
  ColorBack := Self.AsColorVector;
  ColorBlend := CombinedColor.AsColorVector;
  //GlobalLogger.LogStatus('ColorFore = ' +ColorFore.ToString);
  // La couleur est transparente, le fond reste donc inchanger
  if (ColorFore.Alpha = 0) or (ColorBlend.Alpha = 0) then exit;

  // Premultiplication des valeurs de couleur par le canal Alpha
  //ta := ColorFore.Alpha;
  //ColorFore := ColorFore * ta;
  //ColorFore.Alpha := ta;
  //
  //ta := ColorBack.Alpha;
  //ColorBack := ColorBack * ta;
  //ColorBack.Alpha := ta;
  //
  //ta := ColorBlend.Alpha;
  //ColorBlend := ColorBlend * ta;
  //ColorBlend.Alpha := ta;

  { Blend }

  ForeAlpha := ColorFore.Alpha * Delta;

  //rA := fA + bA - ba * fa div 255;
  rA := (ForeAlpha + ColorBack.Alpha) - (ColorBack.Alpha * ForeAlpha);

  rAlpha.Create(rA,1);
  AlphaFore.Create(ForeAlpha,1);
  AlphaBack.Create(ColorBack.Alpha,1);

  //-------------------------------------------------------------
  // tR := bR - bR * fA div rA + (fR - (fR - BlendR) * bA div 255) * fA div rA;
  // tR := c1 + c2 * fA div rA;

  // C1 =  (bR - ((bR * fA) div rA))
  c1 := (ColorBack -  ((ColorBack * AlphaFore) / rAlpha));
  // C2 = (fR - ((fR - BlendR) * bA))
  c2 := (ColorFore - ((ColorFore - ColorBlend) * AlphaBack));

  // tR := - C1r + ((C2r * fA) / rA);
  ResultColor := c1 + ((c2 * AlphaFore) / rAlpha);
  ResultColor.Alpha := rA;
  //-------------------------------------------------------------
  //GlobalLogger.LogStatus('Blend = ' +ResultColor.ToString);
  Result.Create(ResultColor);
  //GlobalLogger.LogStatus('After Blend = ' +Result.ToString);
end;

function TBZColor32Helper.Luminance : Byte;
begin
//  Gray = (Red * 0.299 + Green * 0.587 + Blue * 0.114) = recommendation (BT.601)
 Result := ClampByte(round(Self.Red*0.2126 + Self.Green*0.7152 + Self.Blue*0.0722));
end;

function TBZColor32Helper.Luminosity : Byte;
begin
 result :=ClampByte(Floor((Self.Red + Self.Green + Self.Blue) / 3));
end;

function TBZColor32Helper.SwapRBChannels : TBZColor32;
Var
  AIntColor : Cardinal;
begin
 AIntColor := Self.AsInteger;
 Result.AsInteger := AIntColor and $FF00FF00 or (AintColor and $000000FF SHL 16) or (AIntColor and $00FF0000 SHR 16);

end;

procedure TBZColor32Helper.AdjustBrightness(factor : Single);
begin

end;

procedure TBZColor32Helper.AdjustContrast(factor : Single);
begin

end;

procedure TBZColor32Helper.AdjustSaturation(Factor : Single; Const AsGray : Boolean);
begin

end;

//procedure TBZColor32Helper.GammaCorrection(const Factor : Single);
//begin
//
//end;

//function TBZColor32Helper.Posterize(Factor : Single): TBZColor32;
//begin
//
//end;
//
//function TBZColor32Helper.Solarize(Factor: Single):TBZColor32;
//begin
//
//end;

procedure TBZColor32Helper.DecRed(const Delta : Byte);
begin
 if Self.Red > 0 then Self.Red := Self.Red - Delta;
end;

procedure TBZColor32Helper.DecGreen(const Delta : Byte);
begin
 if Self.Green > 0 then Self.Green := Self.Green - Delta;
end;

procedure TBZColor32Helper.DecBlue(const Delta : Byte);
begin
 if Self.Blue > 0 then Self.Blue := Self.Blue - Delta;
end;

procedure TBZColor32Helper.DecAlpha(const Delta : Byte);
begin
 if Self.Alpha > 0 then Self.Alpha := Self.Alpha - Delta;
end;

procedure TBZColor32Helper.IncRed(const Delta : Byte);
begin
 if Self.V[cRedOrder] < 255 then Self.V[cRedOrder] := Self.V[cRedOrder] + Delta;
end;

procedure TBZColor32Helper.IncGreen(const Delta : Byte);
begin
 if Self.Green < 255 then Self.Green := Self.Green + Delta;
end;

procedure TBZColor32Helper.IncBlue(const Delta : Byte);
begin
 if Self.Blue < 255 then Self.Blue := Self.Blue + Delta;
end;

procedure TBZColor32Helper.IncAlpha(const Delta : Byte);
begin
 if Self.Alpha < 255 then Self.Alpha := Self.Alpha + Delta;
end;

procedure TBZColor32Helper.SetAlphaFromIntensity(AColor : TBZColor32);
begin
 Self.Alpha := AColor.Luminosity; //ClampByte(Round((AColor.Red + AColor.Blue + AColor.Green) / 3));
end;

procedure TBZColor32Helper.SetAlphaFromSuperBlackTransparent(AColor : TBZColor32);
begin
 if ((AColor.Red = 0) and (AColor.Blue = 0) and (AColor.Green = 0)) then Self.Alpha := 0
end;

procedure TBZColor32Helper.SetAlphaFromLuminance(AColor : TBZColor32);
begin
 Self.Alpha := AColor.Luminance;
end;

procedure TBZColor32Helper.SetAlphaFromLuminanceSqrt(AColor : TBZColor32);
begin
 Self.Alpha := ClampByte(Round(Sqrt(AColor.Luminance)));
end;

procedure TBZColor32Helper.SetAlphaFromInverseLuminance(AColor : TBZColor32);
begin
 Self.Alpha := ClampByte(255 - round((AColor.Red*0.2126 + AColor.Green*0.7152 + AColor.Blue*0.0722)));
end;

procedure TBZColor32Helper.SetAlphaFromInverseLuminanceSqrt(AColor : TBZColor32);
begin
 Self.Alpha := ClampByte(255 - round(Sqrt((AColor.Red*0.2126 + AColor.Green*0.7152 + AColor.Blue*0.0722))));
end;

procedure TBZColor32Helper.SetAlphaOpaque(AColor : TBZColor32);
begin
  if AColor.Alpha>0 then Self.Alpha := 255;
end;

function TBZColor32Helper.AsColor : TColor;
begin
  result:=(Self.Red or (Self.Green shl 8) or (Self.Blue shl 16)) and $ffffff;
end;

function TBZColor32Helper.AsColorVector : TBZColorVector;
Var
  vc : TBZColorVector;
begin
  vc.Create(Self.Red,Self.Green,Self.Blue,Self.Alpha);
  Result := vc * cColorFloatRatio;
end;

function TBZColor32Helper.ToColorVector : TBZColorVector;
begin
  Result.Create(Self.Red,Self.Green,Self.Blue,Self.Alpha);
end;

function TBZColor32Helper.Lighten(Factor:Single):TBZColor32;
var
  d : Integer;
begin
 d:= Self.Luminosity;
 if (d = 0) then d:= round(255*factor) else d := round(d*factor) ;
 Result.Red := Math.Min(Self.Red + d, 255);
 Result.Green := Math.Min(Self.Green + d, 255);
 Result.Blue := Math.Min(Self.Blue + d, 255);
 Result.Alpha := Self.Alpha;
end;

function TBZColor32Helper.Darken(Factor:Single):TBZColor32;
var
  d : Integer;
begin
 d:= Self.Luminosity;
 d := Round(d*factor) ;
 Result.Red := Math.Max(Self.Red - d, 0);
 Result.Green := Math.Max(Self.Green - d, 0);
 Result.Blue := Math.Max(Self.Blue - d, 0);
 Result.Alpha := Self.Alpha;
end;

function TBZColor32Helper.ToColorHSV(Const SubSpaceType : THSVColorSubSpaceType) : TBZColorHSV;
var
  Temp : TBZColorFloatHSV;
begin
  Temp := Self.ToColorFloatHSV(SubSpaceType);
  Result.ColorSubSpaceType :=  SubSpaceType;
  Result.Hue := Round(Temp.Hue);
  Result.Saturation := Round(Temp.Saturation);
  Result.Value := Round(Temp.Value);
end;

function TBZColor32Helper.ToColorHSV : TBZColorHSV;
begin
  Result := Self.ToColorHSV(hsvDefault);
end;

//function TBZColor32Helper.HueRotate(Deg : Integer) : TBZColor;
//Const _Coef : Single = 1/3;
//Var
//  ColorMatrix : TBZColorMatrix;
//  CosA, SinA, d, s : Single;
//  {$CODEALIGN VARMIN=16}
//  cv : TBZColorVector;
//  {$CODEALIGN VARMIN=4}
//begin
//  cosA := cos(DegToRadian(deg));
//  sinA := sin(DegToRadian(deg));
//  d := 1.0 - CosA;
//  s := Sqrt(_Coef);
//  //Red
//  ColorMatrix[0] := cosA + d / 3.0;
//  ColorMatrix[1] := _Coef * d - s * sinA;
//  ColorMatrix[2] := _Coef * d + s * sinA;
//  ColorMatrix[3] := 1.0;
//  ColorMatrix[4] := 0.0;
//  //Green
//  ColorMatrix[5] := _Coef * d + s * sinA;
//  ColorMatrix[6] := cosA + _Coef * d;
//  ColorMatrix[7] := _Coef * d - s * sinA;
//  ColorMatrix[8] := 1.0;
//  ColorMatrix[9] := 0.0;
//  //Blue
//  ColorMatrix[10] := _Coef * d - s * sinA;
//  ColorMatrix[11] := _Coef * d + s * sinA;
//  ColorMatrix[12] := cosA + _Coef * d;
//  ColorMatrix[13] := 1.0;
//  ColorMatrix[14] := 0.0;
//  //Alpha
//  ColorMatrix[15] := 1.0;
//  ColorMatrix[16] := 1.0;
//  ColorMatrix[17] := 1.0;
//  ColorMatrix[18] := 1.0;
//  ColorMatrix[19] := 0.0;
//
//  cv.Create(Self);
//
//  Result.Create(cv.ApplyColorMatrix(ColorMatrix));
//end;

function TBZColor32Helper.ToColorFloatHSL : TBZColorFloatHSL;
begin
  Result := Self.ToColorFloatHSL(hslDefault);
end;

function TBZColor32Helper.ToColorFloatHSL(const SubSpaceType : THSLColorSubSpaceType) : TBZColorFloatHSL;
begin
   Result := Self.AsColorVector.ToColorFloatHSL(SubSpaceType);
end;

function TBZColor32Helper.ToColorFloatHSV : TBZColorFloatHSV;
begin
  Result := Self.ToColorFloatHSV(hsvDefault);
end;

function TBZColor32Helper.ToColorFloatHSV(const SubSpaceType : THSVColorSubSpaceType) : TBZColorFloatHSV;
begin
  Result := Self.AsColorVector.ToColorFloatHSV(SubSpaceType);
end;

function TBZColor32Helper.ToColorHSL(const SubSpaceType : THSLColorSubSpaceType) : TBZColorHSL;
var
  Temp : TBZColorFloatHSL;
begin
  Temp := Self.ToColorFloatHSL(SubSpaceType);
  Result.ColorSubSpaceType :=  SubSpaceType;
  Result.Hue := Round(Temp.Hue);
  Result.Saturation := Round(Temp.Saturation);
  Result.Luminosity := Round(Temp.Luminosity);
end;

function TBZColor32Helper.ToColorHSL : TBZColorHSL;
begin
  Result := Self.ToColorHSL(hslDefault);
end;

{%endregion%}

{%region%====[ TBZColorItemProperty ]===================================================}

Constructor TBZColorItemProperty.Create(AOwner: TPersistent);
Begin
  Inherited Create(AOwner);
  FName := 'Black';
  FColor := clrBlack;
  FTag := 0;
End;

Procedure TBZColorItemProperty.SetRed(Const AValue: Byte);
Begin
  If AValue = FColor.red Then exit;
  FColor.Red := AValue;
End;

Procedure TBZColorItemProperty.SetGreen(Const AValue: Byte);
Begin
  If AValue = FColor.Green Then exit;
  FColor.Green := AValue;
End;

Procedure TBZColorItemProperty.SetBlue(Const AValue: Byte);
Begin
  If AValue = FColor.Blue Then exit;
  FColor.Blue := AValue;
End;

Procedure TBZColorItemProperty.SetAlpha(Const AValue: Byte);
Begin
  If AValue = FColor.Alpha Then exit;
  FColor.Alpha := AValue;
End;

Procedure TBZColorItemProperty.SetValue(Const AValue: TBZColor);
Begin
  If AValue = FColor Then exit;
  FColor := AValue;
End;

Function TBZColorItemProperty.getRed: Byte;
Begin
  Result := FColor.Red;
End;

Function TBZColorItemProperty.getGreen: Byte;
Begin
  Result := FColor.Green;
End;

Function TBZColorItemProperty.getBlue: Byte;
Begin
  Result := FColor.Blue;
End;

Function TBZColorItemProperty.getAlpha: Byte;
Begin
  Result := FColor.Alpha;
End;

Function TBZColorItemProperty.getValue: TBZColor;
Begin
  Result := FColor;
End;

Procedure TBZColorItemProperty.setColorName(Const aName: String);
Begin
  If FName = aName Then exit;
  FName := aName;
End;

Procedure TBZColorItemProperty.WriteToFiler(writer: TVirtualWriter);
Begin
  With Writer Do
  Begin
    WriteString(FName);
  End;
End;

Procedure TBZColorItemProperty.ReadFromFiler(reader: TVirtualReader);
Begin
  With Reader Do
  Begin
    setColorName(ReadString);
  End;
End;

{%endregion%}

{%region%====[ TBZGradientColorItem ]===========================================}

Constructor TBZGradientColorItem.Create(AOwner: TPersistent);
Begin
  Inherited Create(AOwner);
  FNodeEaseMode := amIn;
  FNodeEaseType := atLinear;
  FLerpType := itLinear;
  FDistorsion := 0;
  FTheta := 0;
End;

Procedure TBZGradientColorItem.setNodeEaseType(Const aValue : TBZAnimationType);
Begin
  If aValue = FNodeEaseType Then exit;
  FNodeEaseType := aValue;
End;

Procedure TBZGradientColorItem.setLerpType(Const aValue: TBZInterpolationType);
Begin
  If aValue = FLerpType Then exit;
  FLerpType := aValue;
End;

Procedure TBZGradientColorItem.setDistorsion(Const aValue: Single);
Begin
  If aValue = FDistorsion Then exit;
  FDistorsion := aValue;
End;

Procedure TBZGradientColorItem.setTheta(Const aValue: Single);
Begin
  If aValue = FTheta Then exit;
  FTheta := aValue;
End;

Function TBZGradientColorItem.getNodeEaseType : TBZAnimationType;
Begin
  Result := FNodeEaseType;
End;

Function TBZGradientColorItem.getLerpType: TBZInterpolationType;
Begin
  Result := FLerpType;
End;

Function TBZGradientColorItem.getDistorsion: Single;
Begin
  Result := FDistorsion;
End;

Function TBZGradientColorItem.getTheta: Single;
Begin
  Result := FTheta;
End;

function TBZGradientColorItem.getNodeEaseMode : TBZAnimationMode;
begin
 Result := FNodeEaseMode;
end;

procedure TBZGradientColorItem.setNodeEaseMode(const AValue : TBZAnimationMode);
begin
  FNodeEaseMode := AValue;
end;

Procedure TBZGradientColorItem.WriteToFiler(writer: TVirtualWriter);
Begin
  With Writer Do
  Begin

  End;
End;

Procedure TBZGradientColorItem.ReadFromFiler(reader: TVirtualReader);
Begin
  With Reader Do
  Begin

  End;
End;

{%endregion%}

{%region%====[ TBZGradientColorList ]===========================================}

Constructor TBZGradientColorList.CreateOwned(AOwner: TBZColorList);
Begin
  FOwnColorList := AOwner;
  FTweenGlobal := False;
  FTweenDuration := 1.0;
  FTweenNext := False;
  Create;
End;

Destructor TBZGradientColorList.Destroy;
Begin
  Clear;
  Inherited Destroy;
End;

Function TBZGradientColorList.getDirectGradientList: PBZColorItemArray;
Begin
  Result := nil;
End;

Function TBZGradientColorList.GetGradientColorItem(index: Integer): TBZGradientColorItem;
Begin
  Result := TBZGradientColorItem(Get(Index));
End;

Procedure TBZGradientColorList.SetGradientColorItem(index: Integer; val: TBZGradientColorItem);
Begin
  Put(Index, Val);
End;

Procedure TBZGradientColorList.Clear;
Var
  i:   Integer;
  Col: TBZGradientColorItem;
Begin
  if Count < 1 then exit;
  For i := 0 To Count - 1 Do
  Begin
    Col := GetGradientColorItem(i);
    If Assigned(Col) Then Col.Free;
  End;
  Inherited Clear;
End;

Function TBZGradientColorList.AddColorStop(Const aColor: TBZColor): Integer;
Var
  aColorItem: TBZGradientColorItem;
Begin
  aColorItem := TBZGradientColorItem.Create(self);
  aColorItem.Value := aColor;
  Result := Add(aColorItem);
End;

Function TBZGradientColorList.AddColorStop(Const aColor: TBZColor; Const aTheta:Single): Integer;
Var
  aColorItem: TBZGradientColorItem;
Begin
  aColorItem := TBZGradientColorItem.Create(self);
  aColorItem.Value := aColor;
  aColorItem.Theta := aTheta;
  Result := Add(aColorItem);
End;

Function TBZGradientColorList.AddColorStop(Const aColor : TBZColor; Const aEaseMode : TBZAnimationMode; Const aEaseType : TBZAnimationType; Const aLerpType : TBZInterpolationType; Const aTheta : Single; Const aDistorsion : Single) : Integer;
Var
  aColorItem: TBZGradientColorItem;
Begin
  aColorItem := TBZGradientColorItem.Create(self);
  aColorItem.Value := aColor;
  aColorItem.EaseMode := aEaseMode;
  aColorItem.EaseType := aEaseType;
  aColorItem.LerpType := aLerpType;
  aColorItem.Theta := aTheta;
  aColorItem.Distorsion := aDistorsion;
  Result := Add(aColorItem);
End;

Function TBZGradientColorList.AddColorStop(Const aColorItem: TBZGradientColorItem): Integer;
Begin
  Result := Add(aColorItem);
End;

Function TBZGradientColorList.GetGradientColor(Const aTheta: Single): TBZColor;
Var
  aFirst, aNext: TBZGradientColorItem;
  Kappa: Single;

  Function GetFirstColor(Const aTheta: Single): TBZGradientColorItem;
  Var
    I, Frame, WorstIndex: Integer;
    Delta, NewDelta, WorstDelta: Single;
    CurrentItem: TBZGradientColorItem;
  Begin
    Result := nil;
    Delta := 65535;
    Frame := -1;
    WorstIndex := -1;
    WorstDelta := 65535;

    For I := 0 To pred(Self.Count) Do
    Begin
      CurrentItem := TBZGradientColorItem(Get(I));
      NewDelta := Abs(aTheta - CurrentItem.Theta);

      If (CurrentItem.Theta <= aTheta) And (NewDelta < Delta) Then
      Begin
        Frame := I;
        Delta := NewDelta;
      End;

      If NewDelta < WorstDelta Then
      Begin
        WorstIndex := I;
        WorstDelta := NewDelta;
      End;
    End;

    If Frame = -1 Then
    Begin
      If WorstIndex <> -1 Then
        Result := TBZGradientColorItem(Get(WorstIndex));
    End
    Else
      Result := TBZGradientColorItem(Get(Frame));
  End;

  Function GetNextColor(Const aTheta: Single): TBZGradientColorItem;
  Var
    I, Frame, WorstIndex: Integer;
    Delta, NewDelta, WorstDelta: Single;
    CurrentItem: TBZGradientColorItem;
  Begin
    Result := nil;
    Delta := 65535;
    Frame := -1;
    WorstIndex := -1;
    WorstDelta := 65535;

    For I := 0 To pred(Count) Do
    Begin
      CurrentItem := TBZGradientColorItem(Get(I));
      NewDelta := Abs(CurrentItem.Theta - aTheta);
      If (CurrentItem.Theta > aTheta) And (NewDelta < Delta) Then
      Begin
        Frame := I;
        Delta := NewDelta;
      End;

      If WorstDelta > NewDelta Then
      Begin
        WorstDelta := NewDelta;
        WorstIndex := I;
      End;
    End;

    If Frame = -1 Then
    Begin
      If WorstIndex <> -1 Then
        Result := TBZGradientColorItem(Get(WorstIndex));
    End
    Else
      Result := TBZGradientColorItem(Get(Frame));
  End;

Begin
  Result := clrBlack;
  // Aucune données, on s'en va

  If Count < 2 Then Exit;

  // On cherche la 1ere couleur
  aFirst := GetFirstColor(aTheta);
  // Aucune données, on sort
  If aFirst = nil Then Exit;

  { On utilise les informations de la première couleur directement si l'une des conditions suivantes est remplie:
      1) Le  Theta est identique
      2) Le Theta est supérieur à la 1ere couleur  }
  If (aFirst.Theta = aTheta) Or (aFirst.Theta > aTheta) Then Exit(aFirst.Value);

  // On va cherchez la 2eme couleur
  aNext := GetNextColor(aTheta);
  If aNext = nil Then Exit(aFirst.Value);

  // S'il n'y a pas de différence de temps entre les 2 couleurs, on retourne la prochaine
  If (aNext.Theta = aFirst.Theta) Or (aNext.Theta = aTheta) Or (aNext = aFirst) Then Exit(aNext.Value);

  // Calcul de la valeur d'interpolation
  If FTweenGlobal Then
    Kappa := aTheta
  Else
    Kappa := (aTheta - aFirst.Theta) / (aNext.Theta - aFirst.Theta);


  If FTweenNext Then
    Kappa := Tweener(0.0, 1.0, Kappa, FTweenDuration, aNext.EaseMode, aNext.EaseType)
  Else
    Kappa := Tweener(0.0, 1.0, Kappa, FTweenDuration, aFirst.EaseMode, aFirst.EaseType);

  // On interpole les couleurs suivant la valeur d'interpolation "Kappa"
  If FTweenNext Then
    Result := aFirst.Value.Lerp(aNext.Value, Kappa, 0.0, aNext.LerpType,True)
  Else
    Result := aFirst.Value.Lerp(aNext.Value, Kappa, 0.0, aFirst.LerpType,True);
End;

Procedure TBZGradientColorList.CreateGradient(nb: Integer);
Var
  I:     Integer;
  //AColorItem : TBZColorItemProperty;
  Delta: Single;
Begin
  If (Self.Count <= 1) Or (FOwnColorList = nil) Then exit;
  Delta := 1.0 / nb;
  For i := 0 To pred(nb) Do
  Begin
    // aColorItem := TBZColorItemProperty.Create(FOwnColorList);
    // aColorItem.Value := GetGradientColor(I*Delta);
    // FOwnColorList.AddColor(aColorItem);
    FOwnColorList.AddColor(GetGradientColor(I * Delta));
  End;
End;

Function TBZGradientColorList.GetOrCreateGradientColorList(Const nb: Integer = 256; Const AColorList: TBZColorList = nil): TBZColorList;
Var
  I:     Integer;
  Delta: Single;
  Cols:  TBZColorList;
Begin
  Result := nil;
  If (Self.Count <= 1) Then Exit;

  If FOwnColorList = nil Then
  Begin
    If AColorList = nil Then
      Cols := TBZColorList.CreateOwned(nil)
    Else
      Cols := AColorList;
  End
  Else
  Begin
    If AColorList = nil Then
      Cols := FOwnColorList
    Else
      Cols := AColorList;
  End;

  Delta := 1.0 / nb;
  For i := 0 To pred(nb) Do
  Begin
    Cols.AddColor(GetGradientColor(I * Delta));
  End;
  Result := Cols;
End;

{%endregion%}

{%region%====[ TBZColorList ]===================================================}

Constructor TBZColorList.CreateOwned(aOwner: TBZColorsManager);    //(AOwner: TPersistent)
Begin
  FOwner := aOwner;
  Create;
  //FGrowDelta := 256;
End;

Destructor TBZColorList.Destroy;
Begin
  Clear;
  Inherited Destroy;
End;

Function TBZColorList.getDirectList: PBZColorItemArray;
Begin
  Result := nil;
End;

Function TBZColorList.GetColorItem(index: Integer): TBZColorItemProperty;
Begin
  Result := TBZColorItemProperty(Get(Index));
End;

Procedure TBZColorList.SetColorItem(index: Integer; val: TBZColorItemProperty);
Begin
  Put(Index, Val);
End;

Procedure TBZColorList.Clear;
Var
  i:   Integer;
  Col: TBZColorItemProperty;
Begin
  if Count < 1 then exit;
  For i := 0 To Count - 1 Do
  Begin
    Col := GetColorItem(i);
    If Assigned(Col) Then
      Col.Free;
  End;
  Inherited Clear;
End;

//procedure WriteToFiler(writer: TVirtualWriter); //override;
//procedure ReadFromFiler(reader: TVirtualReader);// override;

Function TBZColorList.AddColor(Const aColor: TBZColor): Integer;
Var
  aColorItem: TBZColorItemProperty;
Begin
  aColorItem := TBZColorItemProperty.Create(self);
  aColorItem.Value := aColor;
  //GlobalLogger.LogStatus('Add Color To Palette : ' + aColor.ToString);
  Result := Add(aColorItem);
End;

Function TBZColorList.AddColor(Const aName: String; Const aColor: TBZColor): Integer;
Var
  aColorItem: TBZColorItemProperty;
Begin
  aColorItem := TBZColorItemProperty.Create(self);
  aColorItem.Value := aColor;
  aColorItem.Name := aName;
  Result := Add(aColorItem);
End;

(* procedure TBZColorList.AddNoDUpColor(const aName: String; const aColor: TBZColor);
var
   workCopy  : String;
   delimiter : Integer;

begin
   if aName='' then FColor:=clrBlack
   else
   begin
      workCopy:=Trim(AName);
      if CharInSet(AName[1], ['(','[','<']) then workCopy:=Copy(workCopy, 2, Length(AName)-2);
      if CompareText(Copy(workCopy,1,3),'clr')=0 then  FColor:=FindColor(workCopy))
      else
        try
         // initialize FColor
         FColor:=clrBlack;
         workCopy:=Trim(workCopy);
         delimiter:=Pos(' ', workCopy);
         if (Length(workCopy)>0) and (delimiter>0) then
         begin
            FColor.Red:=StrToFloat(Copy(workCopy, 1, delimiter-1));
            System.Delete(workCopy, 1, delimiter);
            workCopy:=TrimLeft(workCopy);
            delimiter:=Pos(' ',workCopy);
            if (Length(workCopy)>0) and (delimiter>0) then
            begin
               FColor.Green:=StrToFloat(Copy(workCopy, 1, delimiter-1));
               System.Delete(workCopy, 1, delimiter);
               workCopy:=TrimLeft(workCopy);
               delimiter:=Pos(' ', workCopy);
               if (Length(workCopy)>0) and (delimiter>0) then
               begin
                  FColor.Blue:=StrToFloat(Copy(workCopy, 1, delimiter-1));
                  System.Delete(workCopy, 1, delimiter);
                  workCopy:=TrimLeft(workCopy);
                  FColor.Alpha:=StrToFloat(workCopy);
               end else FColor.Blue:=StrToFloat(workCopy);
            end else FColor.Green:=StrToFloat(workCopy);
         end else FColor.Red:=StrToFloat(workCopy);
      except
//         ShowMessage('Mauvais formatage utilisez : ''<red green blue alpha>'');
         Abort;
      end;
   end;
end;

procedure TBZColorList.EnumColors(AValues: TStrings);
var
   i : Integer;
begin
   for i:=0 to Count-1 do
      AValues.Add(string(TColorEntry(Items[i]^).Name));
end;
*)

Function TBZColorList.AddColor(Const aColorItem: TBZColorItemProperty): Integer;
Begin
  Result := Add(aColorItem);
End;

Procedure TBZColorList.RemoveColor(Const aName: String);
Var
  I:   Integer;
  Col: TBZColorItemProperty;
Begin
  FindColorByName(aName, I);
  If I > -1 Then
  Begin
    Col := GetColorItem(I);
    If Assigned(Col) Then
      Col.Free;
    Delete(I);
  End;
End;

Function TBZColorList.FindColorByName(Const aName: String; Out Index: Integer): TBZColor;
Var
  i: Integer;
Begin
  Result := clrTransparent;
  Index := -1;
  For i := 0 To Count - 1 Do
    If TBZColorItemProperty(Items[i]).Name = aName Then
    Begin
      Index := I;
      Result := TBZColorItemProperty(Items[i]).Value;
      break;
    End;
End;

Function TBZColorList.FindColorByName(Const aName: String): TBZColor;
Var
  i: Integer;
Begin
  Result := FindColorByName(aName, I);
End;

{%endregion%}

{%region%====[ TBZColorsManager ]===============================================}

Constructor TBZColorsManager.Create(aOwner: TComponent);
Begin
  Inherited Create(AOwner);
  FColors := TBZColorList.CreateOwned(Self);
  FGradientColors := TBZGradientColorList.CreateOwned(FColors);
  FFriendlyName := 'New Palette';
End;

Destructor TBZColorsManager.Destroy;
Begin
  FreeAndNil(FColors);
  FreeAndNil(FGradientColors);
  if Assigned(FGradientPalette) then FreeAndNil(FGradientPalette);
  Inherited Destroy;
End;

Procedure TBZColorsManager.NotifyChange(Sender: TObject);
Begin
  //  if Assigned(FRenderer) then
  //   Renderer.StructureChanged;
End;

Procedure TBZColorsManager.DoProgress(Const progressTime: TBZProgressTimes);
Begin
  Inherited;
  //  if FAutoFreeWhenEmpty and (FColors.ItemCount = 0) then Free;
End;

Class Function TBZColorsManager.ColorItemClass: TBZColorItemPropertyClass;
Begin
  Result := TBZColorItemProperty;
End;

Function TBZColorsManager.MakeColorName(Const aColor: TBZColor): String;
Begin
  Result := Format('<%d %d %d %d>', [AColor.red, AColor.green, AColor.Blue, AColor.Alpha]);
End;

Function TBZColorsManager.CreateColor: TBZColorItemProperty;
Begin
  Result := ColorItemClass.Create(Self);
  // Result.FID := FNextID;
  // if Assigned(cadencer) then
  //    Result.FCreationTime := Cadencer.CurrentTime;
  // Inc(FNextID);

  FColors.AddColor(Result);
  // if Assigned(FOnCreateColor) then FOnCreateColor(Self, Result);
End;

Function TBZColorsManager.CreateColor(aName: String; aColor: TBZColor): TBZColorItemProperty;
Begin
  Result := ColorItemClass.Create(Self);
  Result.Name := aName;
  Result.Value := aColor;
  FColors.AddColor(Result);
End;

Function TBZColorsManager.CreateColor(aColor: TBZColor): TBZColorItemProperty;
Begin
  Result := ColorItemClass.Create(Self);
  Result.Value := aColor;
  FColors.AddColor(Result);
End;

Procedure TBZColorsManager.CreateColors(nbColors: Integer);
Var
  i: Integer;
Begin
  // FColors.FItemList.RequiredCapacity(FColors.ItemCount + nbColors);
  For i := 1 To nbColors Do CreateColor;
End;

Function TBZColorsManager.CreateGradientColor: TBZGradientColorItem;
Begin
  Result := TBZGradientColorItem.Create(self);
  FGradientColors.AddColorStop(Result);
End;

Function TBZColorsManager.CreateGradientColor(aColor: TBZColor): TBZGradientColorItem;
Begin
  Result := TBZGradientColorItem.Create(self);
  Result.Value := aColor;
  FGradientColors.AddColorStop(Result);
End;

(* Function TBZColorsManager.CreateGradientColor(aColor: TBZColor; aTheta:Single): TBZGradientColorItem;
Begin
  Result := TBZGradientColorItem.Create(self);
  Result.Value := aColor;
  Result.Theta := aTheta;
  FGradientColors.AddColorStop(Result);
End; *)

Function TBZColorsManager.CreateGradientColor(Const aColor : TBZColor; Const aTheta : Single; Const aEaseMode : TBZAnimationMode; Const aEaseType : TBZAnimationType; Const aLerpType : TBZInterpolationType; Const aDistorsion : Single) : TBZGradientColorItem;
Begin
  Result := TBZGradientColorItem.Create(self);
  Result.Value := aColor;
  Result.EaseType := aEaseType;
  Result.LerpType := aLerpType;
  Result.Distorsion := aDistorsion;
  Result.Theta := aTheta;
  FGradientColors.AddColorStop(Result);
End;

Procedure TBZColorsManager.CreateGradientColors(nbColors: Integer);
Var
  i: Integer;
Begin
  For i := 1 To nbColors Do CreateGradientColor;
End;

Function TBZColorsManager.GetGradientColor(aTheta: Single): TBZColor;
Begin
  Result := FGradientColors.GetGradientColor(aTheta);
End;

Function TBZColorsManager.GradientColorCount: Integer;
Begin
  Result := FGradientColors.Count;
End;

Function TBZColorsManager.MakeGradientPalette(Const Nb: Integer = 256; Const AColorList: TBZColorList = nil): TBZColorList;
Begin

  If AColorList = nil Then
  Begin
    //FGradientPalette := FGradientColors.GetOrCreateGradientColorList(nb, );
    FColors := FGradientColors.GetOrCreateGradientColorList(nb,nil);
    Result := FColors; //FGradientPalette;
  End
  Else
  Begin
    Result := TBZColorList.Create;
    Result := FGradientColors.GetOrCreateGradientColorList(nb, Result);
  End;
End;

Procedure TBZColorsManager.CreateDefaultPalette;
Begin
  CreateColor('transparent', clrTransparent);
  CreateColor('none', clrTransparent);
  CreateColor('black', clrBlack);
  CreateColor('blue', clrBlue);
  CreateColor('green', clrGreen);
  CreateColor('cyan', clrCyan);
  CreateColor('red', clrRed);
  CreateColor('magenta', clrMagenta);
  CreateColor('yellow', clrYellow);
  CreateColor('white', clrWhite);
  CreateColor('gray', clrGray);
  CreateColor('ltgray', clrLtGray);
  //CreateColor('dkblue', clrDkBlue);
  //CreateColor('dkgreen', clrDkGreen);
  //CreateColor('dkcyan', clrDkCyan);
  //CreateColor('dkred', clrDkRed);
  //CreateColor('dkmagenta', clrDkMagenta);
  //CreateColor('dkyellow', clrDkYellow);
  CreateColor('ltgreen', clrLtGreen);
  CreateColor('olive', clrOlive);
  CreateColor('navy', clrNavy);
  CreateColor('purple', clrPurple);
  CreateColor('teal', clrTeal);
  CreateColor('silver', clrSilver);
  CreateColor('lime', clrLime);
  CreateColor('fuchsia', clrFuchsia);
  CreateColor('aqua', clrAqua);
End;

Procedure TBZColorsManager.CreateGrayPalette;
Var
  I:      Byte;
  aColor: TBZColor;
Begin
  For I := 0 To 255 Do
  Begin
    With aColor Do
    Begin
      Red := I;
      Green := I;
      Blue := I;
      Alpha := 255;
    End;
    CreateColor(aColor);
  End;
End;

Procedure TBZColorsManager.CreateBWPalette;
Begin
  CreateColor('black', clrBlack);
  CreateColor('white', clrWhite);
End;

Procedure TBZColorsManager.SetColors(Const aColors: TBZColorList);
Begin
  FColors.Assign(aColors);
End;

Procedure TBZColorsManager.SetGradientColors(Const aColors: TBZGradientColorList);
Begin
  FGradientColors.Assign(aColors);
End;

Function TBZColorsManager.ColorCount: Integer;
Begin
  Result := FColors.Count;
End;

Function TBZColorsManager.FindColor(ColorName: String; Var Index: Integer): Boolean;
Begin
  Result := False;
  Index := -1;
  FColors.FindColorByName(ColorName, Index);
  If Index > -1 Then
    Result := True;
End;

Procedure TBZColorsManager.LoadFromFile(Const FileName: String);
Var
  Stream: TStream;
Begin
  Stream := TFileStream.Create(FileName, fmOpenRead Or fmShareDenyWrite);
  Try
    LoadFromStream(Stream);
  Finally
    Stream.Free;
  End;
End;

Procedure TBZColorsManager.SaveToFile(Const FileName: String);
Var
  Stream: TStream;
Begin
  Stream := TFileStream.Create(FileName, fmCreate Or fmShareExclusive);
  Try
    SaveToStream(Stream);
  Finally
    Stream.Free;
  End;
End;

Procedure TBZColorsManager.SaveToStream(Const Stream: TStream);
//Var
//  I: Integer;
Begin
  // Stream.WriteBuffer(FFriendlyName,64);
  //  Stream.WriteDouble(FTime);
  //Stream.WriteQWord(Length(FData));

 (* for I := 0 to Length(FData) - 1 do
  begin
    Stream.WriteByte(FData[I].Color.Red);
    Stream.WriteByte(FData[I].Color.Green);
    Stream.WriteByte(FData[I].Color.Blue);
    Stream.WriteByte(FData[I].Color.Alpha);
   // Stream.WriteByte(Integer(FData[I].NodeType));
   // Stream.WriteDouble(FData[I].Theta);
  end; *)
End;

Procedure TBZColorsManager.LoadFromStream(Const Stream: TStream);
//Var
//  I, ElemCount: Integer;
Begin
(*  FName := Stream.ReadAnsiString;
//  FTime := Stream.ReadDouble;
  ElemCount := Stream.ReadQWord;

  if ElemCount <= 0 then
  begin
    SetLength(FData, 0);
    Exit;
  end;

  SetLength(FData, ElemCount);

  for I := 0 to ElemCount - 1 do
  begin
    FData[I].Color.Red := Stream.ReadByte;
    FData[I].Color.Green := Stream.ReadByte;
    FData[I].Color.Blue := Stream.ReadByte;
    FData[I].Color.Alpha := Stream.ReadByte;
//    FData[I].NodeType := TFloatNodeType(Stream.ReadByte);
//    FData[I].Theta := Stream.ReadDouble;
  end; *)
End;

 (*procedure TBZColorsManager.Assign(const Source: TBZColorsManager);
var
  I: Integer;
begin
 FName := Source.Name;

  Clear;

  for I := 0 to Source.Count - 1 do
    Add(Source.Items[I]^);
end;  *)

(* procedure TBZColorsManager.registerUser(obj: TBZColorFXEffect);
begin
  if FUsers.IndexOf(obj) = -1 then
    FUsers.Add(obj);
end;

procedure TBZColorManager.unregisterUser(obj: TBZColorFXEffect);
begin
  FUsers.Remove(obj);
end; *)
{%endregion%}

{%region%====[ TBZGradientProperty ]============================================}

constructor TBZGradientProperty.Create;
begin
  Inherited Create;
  FColorSteps := TBZGradientColorList.CreateOwned(nil);
  FGradientStyle := gkHorizontal;
  FGradientAngle := 0;
  FAutoAngle := False;
end;

destructor TBZGradientProperty.Destroy;
begin
  FreeAndNil(FColorSteps);
  inherited Destroy;
end;

procedure TBZGradientProperty.MakeGradientPalette(const nbStep : integer);
begin
  FPalette := FColorSteps.GetOrCreateGradientColorList(nbStep);
end;


(*Procedure TBZGradientProperty.Assign(Source: TPersistent);
Begin
  If (Source Is TBZCanvasBrush) Then
  Begin

    FColorSteps.Assign(TBZCanvasBrush(Source).GradientColors);
    FGradientStyle := TBZCanvasBrush(Source).GradientStyle;
    FGradientAngle := TBZCanvasBrush(Source).GradientAngle;

  End;
End;

Procedure TBZGradientProperty.SetGradientStyle(AValue: TBZGradientStyle);
Begin
  If FGradientStyle = AValue Then exit;
  FGradientStyle := AValue;
End;

Procedure TBZGradientProperty.SetGradientBoundRect(BoundRect : TRect);
begin
  if GradientBoundsRect  =  BoundRect then exit;
  GradientBoundsRect := BoundRect;
End;

*)

{%endregion%}

{%region%====[ Fonctions Internes ]=============================================}

procedure ComputeCombineLUT;
var
  i, j: Integer;
  x: Integer;
  LTmp: Integer;
begin
  { Init SqrtTable }
  for x := 0 to 65535 do
  begin
    LTmp := Round(System.Sqrt(x));

    if LTmp <= 255 then
    begin
      SqrtCombineLUT[x] := LTmp;
    end
    else
    begin
      SqrtCombineLUT[x] := 255;
    end;
  end;

  { Init Custom Blendmap - like normal blend }
  //for x := 0 to 255 do
  //begin
  //  for y := 0 to 255 do
  //  begin
  //    FillChar(DefaultCombineBlendMapLUT[y + x shl 8], 3, x);
  //  end;
  //end;

  { Init CosineTable }
  for i := 0 to 255 do
  begin
    LerpCombineCosineLUT[i] := Round( 64 - Cos(i * Pi / 255) * 64 );
  end;

  { Init ProbTable -- Probability Table }
  for i := 0 to 100 do
  begin
    for j := 0 to 99 do
    begin
      if j < i then
      begin
        ProbCombineLUT[i, j] := True;
      end
      else
      begin
        ProbCombineLUT[i, j] := False;
      end;
    end;
  end;
end;

{%endregion%}


destructor TBZColorRegister.Destroy;
var
   i : Integer;
begin
   for i:=0 to Count-1 do
   begin
      FreeMem(Items[i], SizeOf(TBZColorRegisterEntry));
   end;
   inherited Destroy;
end;

function TBZColorRegister.FindColor(const aName: String): TBZColor;
var
   i : Integer;
begin
   Result:=clrBlack;
   for i:=0 to Count-1 do
   begin
      if CompareText(string(TBZColorRegisterEntry(Items[i]^).Name), AName)=0 then
      begin
         Result := TBZColorRegisterEntry(Items[i]^).Color;
         Break;
      end;
   end;
end;

function TBZColorRegister.GetColor(const aName : String): TBZColor;
var
   workCopy  : String;
   delimiter : Integer;
begin
   if aName='' then
      Result:=clrBlack
   else
   begin
      workCopy:=Trim(AName);
      if CharInSet(AName[1], ['(','[','<']) then workCopy:=Copy(workCopy, 2, Length(AName)-2);
      if CompareText(Copy(workCopy,1,3),'clr')=0 then Result := FindColor(workCopy)
      else
      begin
        try
          // initialize result
          Result:=clrBlack;
          workCopy:=Trim(workCopy);
          delimiter:=Pos(' ', workCopy);
          if (Length(workCopy)>0) and (delimiter>0) then
          begin
            Result.Red:=StrToInt(Copy(workCopy, 1, delimiter-1));
            System.Delete(workCopy, 1, delimiter);
            workCopy:=TrimLeft(workCopy);
            delimiter:=Pos(' ',workCopy);
            if (Length(workCopy)>0) and (delimiter>0) then
            begin
              Result.Green:=StrToInt(Copy(workCopy, 1, delimiter-1));
              System.Delete(workCopy, 1, delimiter);
              workCopy:=TrimLeft(workCopy);
              delimiter:=Pos(' ', workCopy);
              if (Length(workCopy)>0) and (delimiter>0) then
              begin
                Result.Blue:=StrToInt(Copy(workCopy, 1, delimiter-1));
                System.Delete(workCopy, 1, delimiter);
                workCopy:=TrimLeft(workCopy);
                Result.Alpha:=StrToInt(workCopy);
              end
              else Result.Blue:=StrToInt(workCopy);
            end
            else Result.Green:=StrToInt(workCopy);
          end
          else Result.Red:=StrToInt(workCopy);
        except
         {$IFDEF WINDOWS}
           ShowMessage('Wrong vector format. Use: ''<blue green red alpha>''!');
         {$ELSE}
           ShowMessage('Wrong vector format. Use: ''<red green blue alpha>''!');
         {$ENDIF}
         Abort;
        end;
     end;
   end;
end;

//------------------------------------------------------------------------------

function TBZColorRegister.GetColorName(const aColor: TBZColor): String;
//const MinDiff = 1e-6;
var
  I : Integer;
begin
  for I:=0 to Count-1 do
  begin
    with TBZColorRegisterEntry(Items[I]^) do
    begin
      if (Abs(Color.Red-AColor.Red) < 1) and
         (Abs(Color.Green-AColor.Green) < 1) and
         (Abs(Color.Blue-AColor.Blue) < 1) and
         (Abs(Color.Alpha-AColor.Alpha) < 1) then Break;
    end;
  end;
  if I < Count then
    Result:=string(TBZColorRegisterEntry(Items[I]^).Name)
  else
    Result:=Format('<%d %d %d %d>',[AColor.Red,AColor.Green,
                   AColor.Blue,AColor.Alpha]);
end;

function TBZColorRegister.GetColorByIndex(Const Index : Integer) : TBZColor;
begin
  Result := TBZColorRegisterEntry(Items[Index]^).Color;
end;

function TBZColorRegister.GetColorNameByIndex(Const Index : Integer) : String;
begin
  Result := TBZColorRegisterEntry(Items[Index]^).Name;
end;

procedure TBZColorRegister.AddColor(const aName: String; const aColor: TBZColor);
var
   newEntry : PBZColorRegisterEntry;
begin
   New(newEntry);
   if newEntry = nil then
      raise Exception.Create('Could not allocate memory for color registration!');
   with newEntry^ do
   begin
     Name:=shortstring(AName);
     Color := aColor;
   end;
   Add(newEntry);
end;

procedure TBZColorRegister.EnumColors(Proc: TGetStrProc);
var
   i : Integer;
begin
   for i:=0 to Count-1 do
      Proc(string(TBZColorRegisterEntry(Items[i]^).Name));
end;

procedure TBZColorRegister.EnumColors(AValues: TStrings);
var
   i : Integer;
begin
   for i:=0 to Count-1 do
      AValues.Add(string(TBZColorRegisterEntry(Items[i]^).Name));
end;


procedure TBZColorRegister.RegisterDefaultColors;
begin
  Capacity:=150;
  AddColor('clrTransparent',clrTransparent);
  AddColor('clrBlack',clrBlack);
  AddColor('clrGray05',clrGray05);
  AddColor('clrGray10',clrGray10);
  AddColor('clrGray15',clrGray15);
  AddColor('clrGray20',clrGray20);
  AddColor('clrGray25',clrGray25);
  AddColor('clrGray30',clrGray30);
  AddColor('clrGray35',clrGray35);
  AddColor('clrGray40',clrGray40);
  AddColor('clrGray45',clrGray45);
  AddColor('clrGray50',clrGray50);
  AddColor('clrGray55',clrGray55);
  AddColor('clrGray60',clrGray60);
  AddColor('clrGray65',clrGray65);
  AddColor('clrGray70',clrGray70);
  AddColor('clrGray75',clrGray75);
  AddColor('clrGray80',clrGray80);
  AddColor('clrGray85',clrGray85);
  AddColor('clrGray90',clrGray90);
  AddColor('clrGray95',clrGray95);
  AddColor('clrWhite',clrWhite);

  AddColor('clrBlue',clrBlue);
  AddColor('clrRed',clrRed);
  AddColor('clrGreen',clrGreen);
  AddColor('clrCyan',clrCyan);
  AddColor('clrMagenta',clrMagenta);
  AddColor('clrYellow',clrYellow);
  AddColor('clrWhite',clrWhite);
  AddColor('clrGray',clrGray);
  AddColor('clrLtGray',clrLtGray);
  AddColor('clrOlive',clrOlive);
  AddColor('clrNavy',clrNavy);
  AddColor('clrPurple',clrPurple);
  AddColor('clrFuchsia',clrFuchsia);
  AddColor('clrTeal',clrTeal);
  AddColor('clrSilver',clrSilver);
  AddColor('clrMaroon',clrMaroon);
  AddColor('clrLime',clrLime);
  AddColor('clrLtGreen',clrLtGreen);
  AddColor('clrAqua',clrAqua);

  AddColor('clrAliceBlue',clrAliceBlue);
  AddColor('clrAlizarinCrimson',clrAlizarinCrimson);
  AddColor('clrAmber',clrAmber);
  AddColor('clrAmethyst',clrAmethyst);
  AddColor('clrAntiqueWhite',clrAntiqueWhite);
  AddColor('clrAquamarine',clrAquamarine);
  AddColor('clrAsparagus',clrAsparagus);
  AddColor('clrAzure',clrAzure);
  AddColor('clrBeige',clrBeige);
  AddColor('clrBisque',clrBisque);
  AddColor('clrBistre',clrBistre);
  AddColor('clrBitterLemon',clrBitterLemon);
  AddColor('clrBlanchedAlmond',clrBlanchedAlmond);
  AddColor('clrBlueViolet',clrBlueViolet);
  AddColor('clrBondiBlue',clrBondiBlue);
  AddColor('clrBrass',clrBrass);
  AddColor('clrBrightGreen',clrBrightGreen);
  AddColor('clrBrightViolet',clrBrightViolet);
  AddColor('clrBronze',clrBronze);
  AddColor('clrBrown',clrBrown);
  AddColor('clrBuff',clrBuff);
  AddColor('clrBurgundy',clrBurgundy);
  AddColor('clrBurlyWood',clrBurlyWood);
  AddColor('clrBurntOrange',clrBurntOrange);
  AddColor('clrBurntSienna',clrBurntSienna);
  AddColor('clrBurntUmber',clrBurntUmber);
  AddColor('clrCadetBlue',clrCadetBlue);
  AddColor('clrCamouflageGreen',clrCamouflageGreen);
  AddColor('clrCardinal',clrCardinal);
  AddColor('clrCarmine',clrCarmine);
  AddColor('clrCarrot',clrCarrot);
  AddColor('clrCasper',clrCasper);
  AddColor('clrCerise',clrCerise);
  AddColor('clrCerulean',clrCerulean);
  AddColor('clrCeruleanBlue',clrCeruleanBlue);
  AddColor('clrChartreuse',clrChartreuse);
  AddColor('clrChocolate',clrChocolate);
  AddColor('clrCinnamon',clrCinnamon);
  AddColor('clrCobalt',clrCobalt);
  AddColor('clrCopper',clrCopper);
  AddColor('clrCoral',clrCoral);
  AddColor('clrCorn',clrCorn);
  AddColor('clrCornflowerBlue',clrCornflowerBlue);
  AddColor('clrCornsilk',clrCornsilk);
  AddColor('clrCream',clrCream);
  AddColor('clrCrimson',clrCrimson);
  AddColor('clrDarkBrown',clrDarkBrown);
  AddColor('clrDarkCerulean',clrDarkCerulean);
  AddColor('clrDarkChestnut',clrDarkChestnut);
  AddColor('clrDarkCoral',clrDarkCoral);
  AddColor('clrDarkCyan',clrDarkCyan);
  AddColor('clrDarkGoldenrod',clrDarkGoldenrod);
  AddColor('clrDarkGray',clrDarkGray);
  AddColor('clrDarkGreen',clrDarkGreen);
  AddColor('clrDarkIndigo',clrDarkIndigo);
  AddColor('clrDarkKhaki',clrDarkKhaki);
  AddColor('clrDarkOlive',clrDarkOlive);
  AddColor('clrDarkOliveGreen',clrDarkOliveGreen);
  AddColor('clrDarkOrange',clrDarkOrange);
  AddColor('clrDarkOrchid',clrDarkOrchid);
  AddColor('clrDarkPastelGreen',clrDarkPastelGreen);
  AddColor('clrDarkPink',clrDarkPink);
  AddColor('clrDarkRed',clrDarkRed);
  AddColor('clrDarkSalmon',clrDarkSalmon);
  AddColor('clrDarkScarlet',clrDarkScarlet);
  AddColor('clrDarkSeaGreen',clrDarkSeaGreen);
  AddColor('clrDarkSlateBlue',clrDarkSlateBlue);
  AddColor('clrDarkSlateGray',clrDarkSlateGray);
  AddColor('clrDarkSpringGreen',clrDarkSpringGreen);
  AddColor('clrDarkTan',clrDarkTan);
  AddColor('clrDarkTeaGreen',clrDarkTeaGreen);
  AddColor('clrDarkTerraCotta',clrDarkTerraCotta);
  AddColor('clrDarkTurquoise',clrDarkTurquoise);
  AddColor('clrDarkViolet',clrDarkViolet);
  AddColor('clrDeepPink',clrDeepPink);
  AddColor('clrDeepSkyBlue',clrDeepSkyBlue);
  AddColor('clrDenim',clrDenim);
  AddColor('clrDodgerBlue',clrDodgerBlue);
  AddColor('clrEmerald',clrEmerald);
  AddColor('clrEggplant',clrEggplant);
  AddColor('clrFernGreen',clrFernGreen);
  AddColor('clrFireBrick',clrFireBrick);
  AddColor('clrFlax',clrFlax);
  AddColor('clrFloralWhite',clrFloralWhite);
  AddColor('clrForestGreen',clrForestGreen);
  AddColor('clrFractal',clrFractal);
  AddColor('clrGainsboro',clrGainsboro);
  AddColor('clrGamboge',clrGamboge);
  AddColor('clrGhostWhite',clrGhostWhite);
  AddColor('clrGold',clrGold);
  AddColor('clrGoldenrod',clrGoldenrod);
  AddColor('clrGrayAsparagus',clrGrayAsparagus);
  AddColor('clrGrayTeaGreen',clrGrayTeaGreen);
  AddColor('clrGreenYellow',clrGreenYellow);
  AddColor('clrHeliotrope',clrHeliotrope);
  AddColor('clrHoneydew',clrHoneydew);
  AddColor('clrHotPink',clrHotPink);
  AddColor('clrIndianRed',clrIndianRed);
  AddColor('clrIndigo',clrIndigo);
  AddColor('clrInternationalKleinBlue',clrInternationalKleinBlue);
  AddColor('clrInternationalOrange',clrInternationalOrange);
  AddColor('clrIvory',clrIvory);
  AddColor('clrJade',clrJade);
  AddColor('clrKhaki',clrKhaki);
  AddColor('clrLavender',clrLavender);
  AddColor('clrLavenderBlush',clrLavenderBlush);
  AddColor('clrLawnGreen',clrLawnGreen);
  AddColor('clrLemon',clrLemon);
  AddColor('clrLemonChiffon',clrLemonChiffon);
  AddColor('clrLightBlue',clrLightBlue);
  AddColor('clrLightBrown',clrLightBrown);
  AddColor('clrLightCoral',clrLightCoral);
  AddColor('clrLightCyan',clrLightCyan);
  AddColor('clrLightGoldenrodYellow',clrLightGoldenrodYellow);
  AddColor('clrLightMagenta',clrLightMagenta);
  AddColor('clrLightRed',clrLightRed);
  AddColor('clrLightSalmon',clrLightSalmon);
  AddColor('clrLightSeaGreen',clrLightSeaGreen);
  AddColor('clrLightSkyBlue',clrLightSkyBlue);
  AddColor('clrLightSlateGray',clrLightSlateGray);
  AddColor('clrLightSteelBlue',clrLightSteelBlue);
  AddColor('clrLightYellow',clrLightYellow);
  AddColor('clrLilac',clrLilac);
  AddColor('clrLimeGreen',clrLimeGreen);
  AddColor('clrLinen',clrLinen);
  AddColor('clrMalachite',clrMalachite);
  AddColor('clrMauve',clrMauve);
  AddColor('clrMediumAquamarine',clrMediumAquamarine);
  AddColor('clrMediumBlue',clrMediumBlue);
  AddColor('clrMediumOrchid',clrMediumOrchid);
  AddColor('clrMediumSeaGreen',clrMediumSeaGreen);
  AddColor('clrMediumSlateBlue',clrMediumSlateBlue);
  AddColor('clrMediumSpringGreen',clrMediumSpringGreen);
  AddColor('clrMediumTurquoise',clrMediumTurquoise);
  AddColor('clrMediumVioletRed',clrMediumVioletRed);
  AddColor('clrMidnightBlue',clrMidnightBlue);
  AddColor('clrMintCream',clrMintCream);
  AddColor('clrMistyRose',clrMistyRose);
  AddColor('clrMoccasin',clrMoccasin);
  AddColor('clrMoneyGreen',clrMoneyGreen);
  AddColor('clrMonza',clrMonza);
  AddColor('clrMossGreen',clrMossGreen);
  AddColor('clrMountbattenPink',clrMountbattenPink);
  AddColor('clrMustard',clrMustard);
  AddColor('clrNavajoWhite',clrNavajoWhite);
  AddColor('clrOchre',clrOchre);
  AddColor('clrOldGold',clrOldGold);
  AddColor('clrOldLace',clrOldLace);
  AddColor('clrOliveDrab',clrOliveDrab);
  AddColor('clrOrange',clrOrange);
  AddColor('clrOrangeRed',clrOrangeRed);
  AddColor('clrOrchid',clrOrchid);
  AddColor('clrPaleBrown',clrPaleBrown);
  AddColor('clrPaleCarmine',clrPaleCarmine);
  AddColor('clrPaleChestnut',clrPaleChestnut);
  AddColor('clrPaleCornflowerBlue',clrPaleCornflowerBlue);
  AddColor('clrPaleGoldenrod',clrPaleGoldenrod);
  AddColor('clrPaleGreen',clrPaleGreen);
  AddColor('clrPaleMagenta',clrPaleMagenta);
  AddColor('clrPaleMauve',clrPaleMauve);
  AddColor('clrPalePink',clrPalePink);
  AddColor('clrPaleSandyBrown',clrPaleSandyBrown);
  AddColor('clrPaleTurquoise',clrPaleTurquoise);
  AddColor('clrPaleVioletRed',clrPaleVioletRed);
  AddColor('clrPapayaWhip',clrPapayaWhip);
  AddColor('clrPastelGreen',clrPastelGreen);
  AddColor('clrPastelPink',clrPastelPink);
  AddColor('clrPeach',clrPeach);
  AddColor('clrPeachOrange',clrPeachOrange);
  AddColor('clrPeachPuff',clrPeachPuff);
  AddColor('clrPeachYellow',clrPeachYellow);
  AddColor('clrPear',clrPear);
  AddColor('clrPeriwinkle',clrPeriwinkle);
  AddColor('clrPersianBlue',clrPersianBlue);
  AddColor('clrPeru',clrPeru);
  AddColor('clrPineGreen',clrPineGreen);
  AddColor('clrPink',clrPink);
  AddColor('clrPinkOrange',clrPinkOrange);
  AddColor('clrPlum',clrPlum);
  AddColor('clrPowderBlue',clrPowderBlue);
  AddColor('clrPrussianBlue',clrPrussianBlue);
  AddColor('clrPuce',clrPuce);
  AddColor('clrPumpkin',clrPumpkin);
  AddColor('clrRawUmber',clrRawUmber);
  AddColor('clrReef',clrReef);
  AddColor('clrRobinEggBlue',clrRobinEggBlue);
  AddColor('clrRosyBrown',clrRosyBrown);
  AddColor('clrRoyalBlue',clrRoyalBlue);
  AddColor('clrRusset',clrRusset);
  AddColor('clrRust',clrRust);
  AddColor('clrSaddleBrown',clrSaddleBrown);
  AddColor('clrSaffron',clrSaffron);
  AddColor('clrSalmon',clrSalmon);
  AddColor('clrSandyBrown',clrSandyBrown);
  AddColor('clrSangria',clrSangria);
  AddColor('clrSapphire',clrSapphire);
  AddColor('clrScarlet',clrScarlet);
  AddColor('clrSeaGreen',clrSeaGreen);
  AddColor('clrSeaShell',clrSeaShell);
  AddColor('clrSepia',clrSepia);
  AddColor('clrSienna',clrSienna);
  AddColor('clrSkyBlue',clrSkyBlue);
  AddColor('clrSlateBlue',clrSlateBlue);
  AddColor('clrSlateGray',clrSlateGray);
  AddColor('clrSnow',clrSnow);
  AddColor('clrSpringGreen',clrSpringGreen);
  AddColor('clrSteelBlue',clrSteelBlue);
  AddColor('clrSwampGreen',clrSwampGreen);
  AddColor('clrTaupe',clrTaupe);
  AddColor('clrTangerine',clrTangerine);
  AddColor('clrTeaGreen',clrTeaGreen);
  AddColor('clrTenne',clrTenne);
  AddColor('clrTerraCotta',clrTerraCotta);
  AddColor('clrThistle',clrThistle);
  AddColor('clrTomato',clrTomato);
  AddColor('clrTurquoise',clrTurquoise);
  AddColor('clrUltramarine',clrUltramarine);
  AddColor('clrVermilion',clrVermilion);
  AddColor('clrViolet',clrViolet);
  AddColor('clrVioletEggplant',clrVioletEggplant);
  AddColor('clrViridian',clrViridian);
  AddColor('clrWheat',clrWheat);
  AddColor('clrWhiteSmoke',clrWhiteSmoke);
  AddColor('clrWisteria',clrWisteria);
  AddColor('clrYellowGreen',clrYellowGreen);
  AddColor('clrZinnwaldite',clrZinnwaldite);

  AddColor('clrCoral2',clrCoral2);
  AddColor('clrCornflowerBlue2',clrCornflowerBlue2);
  AddColor('clrDarkGreen2',clrDarkGreen2);
  AddColor('clrDarkOliveGreen2',clrDarkOliveGreen2);
  AddColor('clrDarkSlateBlue2',clrDarkSlateBlue2);
  AddColor('clrDarkTurquoise2',clrDarkTurquoise2);
  AddColor('clrFirebrick2',clrFirebrick2);
  AddColor('clrGold2',clrGold2);
  AddColor('clrGoldenrod2',clrGoldenrod2);
  AddColor('clrGreenYellow2',clrGreenYellow2);
  AddColor('clrIndian',clrIndian);
  AddColor('clrKhaki2',clrKhaki2);
  AddColor('clrLightBlue2',clrLightBlue2);
  AddColor('clrLightSteelBlue2',clrLightSteelBlue2);
  AddColor('clrLimeGreen2',clrLimeGreen2);
  AddColor('clrMediumAquamarine2',clrMediumAquamarine2);
  AddColor('clrMediumBlue2',clrMediumBlue2);
  AddColor('clrMediumForestGreen',clrMediumForestGreen);
  AddColor('clrMediumGoldenrod',clrMediumGoldenrod);
  AddColor('clrMediumOrchid2',clrMediumOrchid2);
  AddColor('clrMediumSeaGreen2',clrMediumSeaGreen2);
  AddColor('clrMediumSlateBlue2',clrMediumSlateBlue2);
  AddColor('clrMediumSpringGreen2',clrMediumSpringGreen2);
  AddColor('clrMediumTurquoise2',clrMediumTurquoise2);
  AddColor('clrMediumViolet',clrMediumViolet);
  AddColor('clrMidnightBlue2',clrMidnightBlue2);
  AddColor('clrNavyBlue',clrNavyBlue);
  AddColor('clrOrange2',clrOrange2);
  AddColor('clrPink2',clrPink2);
  AddColor('clrSalmon2',clrSalmon2);
  AddColor('clrSeaGreen2',clrSeaGreen2);
  AddColor('clrSienna2',clrSienna2);
  AddColor('clrSkyBlue2',clrSkyBlue2);
  AddColor('clrSlateBlue2',clrSlateBlue2);
  AddColor('clrSteelBlue2',clrSteelBlue2);
  AddColor('clrTan',clrTan);
  AddColor('clrThistle2',clrThistle2);
  AddColor('clrTurquoise2',clrTurquoise2);
  AddColor('clrViolet2',clrViolet2);
  AddColor('clrVioletRed',clrVioletRed);
  AddColor('clrWheat2',clrWheat2);
  AddColor('clrYellowGreen2',clrYellowGreen2);
  AddColor('clrSummerSky',clrSummerSky);
  AddColor('clrRichBlue',clrRichBlue);
  AddColor('clrBrass2',clrBrass2);
  AddColor('clrCopper2',clrCopper2);
  AddColor('clrBronze2',clrBronze2);
  AddColor('clrBronze3',clrBronze3);
  AddColor('clrBrightGold',clrBrightGold);
  AddColor('clrOldGold2',clrOldGold2);
  AddColor('clrFeldspar',clrFeldspar);
  AddColor('clrQuartz',clrQuartz);
  AddColor('clrNeonPink',clrNeonPink);
  AddColor('clrDarkPurple',clrDarkPurple);
  AddColor('clrNeonBlue',clrNeonBlue);
  AddColor('clrCoolCopper',clrCoolCopper);
  AddColor('clrMandarinOrange',clrMandarinOrange);
  AddColor('clrLightWood',clrLightWood);
  AddColor('clrMediumWood',clrMediumWood);
  AddColor('clrDarkWood',clrDarkWood);
  AddColor('clrSpicyPink',clrSpicyPink);
  AddColor('clrSemiSweetChoc',clrSemiSweetChoc);
  AddColor('clrBakersChoc',clrBakersChoc);
  AddColor('clrFlesh',clrFlesh);
  AddColor('clrNewTan',clrNewTan);
  AddColor('clrNewMidnightBlue',clrNewMidnightBlue);
  AddColor('clrVeryDarkBrown',clrVeryDarkBrown);
  AddColor('clrDarkBrown2',clrDarkBrown2);
  AddColor('clrDarkTan2',clrDarkTan2);
  AddColor('clrGreenCopper',clrGreenCopper);
  AddColor('clrDkGreenCopper',clrDkGreenCopper);
  AddColor('clrDustyRose',clrDustyRose);
  AddColor('clrHuntersGreen',clrHuntersGreen);
  AddColor('clrScarlet2',clrScarlet2);
  AddColor('clrMediumPurple2',clrMediumPurple2);
  AddColor('clrLightPurple',clrLightPurple);
  AddColor('clrVeryLightPurple',clrVeryLightPurple);

  AddColor('clrScrollBar',clrScrollBar);
  AddColor('clrBackground',clrBackground);
  AddColor('clrActiveCaption',clrActiveCaption);
  AddColor('clrInactiveCaption',clrInactiveCaption);
  AddColor('clrMenu',clrMenu);
  AddColor('clrWindow',clrWindow);
  AddColor('clrWindowFrame',clrWindowFrame);
  AddColor('clrMenuText',clrMenuText);
  AddColor('clrWindowText',clrWindowText);
  AddColor('clrCaptionText',clrCaptionText);
  AddColor('clrActiveBorder',clrActiveBorder);
  AddColor('clrInactiveBorder',clrInactiveBorder);
  AddColor('clrAppWorkSpace',clrAppWorkSpace);
  AddColor('clrHighlight',clrHighlight);
  AddColor('clrHighlightText',clrHighlightText);
  AddColor('clrBtnFace',clrBtnFace);
  AddColor('clrBtnShadow',clrBtnShadow);
  AddColor('clrGrayText',clrGrayText);
  AddColor('clrBtnText',clrBtnText);
  AddColor('clrInactiveCaptionText',clrInactiveCaptionText);
  AddColor('clrBtnHighlight',clrBtnHighlight);
  AddColor('clr3DDkShadow',clr3DDkShadow);
  AddColor('clr3DLight',clr3DLight);
  AddColor('clrInfoText',clrInfoText);
  AddColor('clrInfoBk',clrInfoBk);
  AddColor('clrHotLight',clrHotLight);
  AddColor('clrGradientActiveCaption',clrGradientActiveCaption);
  AddColor('clrGradientInactiveCaption',clrGradientInactiveCaption);
  AddColor('clrMenuHighlight',clrMenuHighlight);
  AddColor('clrMenuBar', clrMenuBar);
  AddColor('clrForm', clrForm);
  AddColor('clrColorDesktop',clrColorDesktop);
  AddColor('clr3DFace',clr3DFace);
  AddColor('clr3DShadow',clr3DShadow);
  AddColor('clr3DHiLight',clr3DHiLight);
  AddColor('clrBtnHiLight',clrBtnHiLight);
  AddColor('clrMask',clrMask);
  AddColor('clrDontMask', clrDontMask);

  Self.Sort(crstHue);
end;

procedure TBZColorRegister.RemoveColor(const aName: String);
var
   i : Integer;
begin
   for i:=0 to Count-1 do
   begin
     if CompareText(string(TBZColorRegisterEntry(Items[i]^).Name), aName)=0 then
     begin
       Delete(i);
       Break;
	   end;
   end;
end;

function CompareColorHue( A, B : Pointer) : Integer;
Var
  v1 : PBZColorRegisterEntry Absolute A;
  v2 : PBZColorRegisterEntry Absolute B;
  hsv1, hsv2 : TBZColorHSL;
begin
  hsv1 := v1^.Color.ToColorHSL(hslDefault);
  hsv2 := v2^.Color.ToColorHSL(hslDefault);
  //Result := Round(hsv2.Hue - hsv1.Hue);
  Result := hsv2.Hue - hsv1.Hue;
end;

function CompareColorSaturation( A, B : Pointer) : Integer;
Var
  v1 : PBZColorRegisterEntry Absolute A;
  v2 : PBZColorRegisterEntry Absolute B;
  hsv1, hsv2 : TBZColorFloatHSV;
begin
  hsv1 := v1^.Color.ToColorFloatHSV;
  hsv2 := v2^.Color.ToColorFloatHSV;
  Result := Round(hsv2.Saturation - hsv1.Saturation);
end;

function CompareColorValue( A, B : Pointer) : Integer;
Var
  v1 : PBZColorRegisterEntry Absolute A;
  v2 : PBZColorRegisterEntry Absolute B;
  hsv1, hsv2 : TBZColorFloatHSV;
begin
  hsv1 := v1^.Color.ToColorFloatHSV;
  hsv2 := v2^.Color.ToColorFloatHSV;
  Result := Round(hsv2.Value - hsv1.Value);
end;

function CompareColorRed( A, B : Pointer) : Integer;
Var
  v1 : PBZColorRegisterEntry Absolute A;
  v2 : PBZColorRegisterEntry Absolute B;
begin
  Result := v2^.Color.Red - v1^.Color.Red;
end;

function CompareColorGreen( A, B : Pointer) : Integer;
Var
  v1 : PBZColorRegisterEntry Absolute A;
  v2 : PBZColorRegisterEntry Absolute B;
begin
  Result := v2^.Color.Green - v1^.Color.Green;
end;

function CompareColorBlue( A, B : Pointer) : Integer;
Var
  v1 : PBZColorRegisterEntry Absolute A;
  v2 : PBZColorRegisterEntry Absolute B;
begin
  Result := v2^.Color.Blue - v1^.Color.Blue;
end;

function CompareColorIntensity( A, B : Pointer) : Integer;
Var
  v1 : PBZColorRegisterEntry Absolute A;
  v2 : PBZColorRegisterEntry Absolute B;
begin
  Result := (v2^.Color.Red + v2^.Color.Green + v2^.Color.Blue + v2^.Color.Alpha) - (v1^.Color.Red + v1^.Color.Green + v1^.Color.Blue + v1^.Color.Alpha);
end;

procedure TBZColorRegister.Sort(SortType : TBZColorRegisterSortType);
begin
  Case SortType of
    crstHue: Inherited Sort(@CompareColorHue);
    crstSaturation: Inherited Sort(@CompareColorSaturation);
    crstValue: Inherited Sort(@CompareColorValue);
    crstRed: Inherited Sort(@CompareColorRed);
    crstGreen: Inherited Sort(@CompareColorGreen);
    crstBlue: Inherited Sort(@CompareColorBlue);
    crstIntensity: Inherited Sort(@CompareColorIntensity);
  end;
end;

function ColorRegister : TBZColorRegister;
begin
	if not Assigned(vColorRegister) then
  begin
		vColorRegister := TBZColorRegister.Create;
		vColorRegister.RegisterDefaultColors;
	end;
	Result := vColorRegister;
end;

procedure RegisterColor(const aName : String; const aColor : TBZColor);
begin
   ColorRegister.AddColor(AName, AColor);
end;

procedure UnregisterColor(const aName : String);
begin
   ColorRegister.RemoveColor(AName);
end;

procedure InitSystemColors;
begin
  clrScrollBar.Create(clScrollBar);
  clrActiveCaption.Create(clActiveCaption);
  clrInactiveCaption.Create(clInactiveCaption);
  clrMenu.Create(clMenu);
  clrWindow.Create(clWindow);
  clrWindowFrame.Create(clWindowFrame);
  clrMenuText.Create(clMenuText);
  clrWindowText.Create(clWindowText);
  clrCaptionText.Create(clCaptionText);
  clrActiveBorder.Create(clActiveBorder);
  clrInactiveBorder.Create(clInactiveBorder);
  clrAppWorkSpace.Create(clAppWorkSpace);
  clrHighlightText.Create(clHighlightText);
  clrBtnFace.Create(clBtnFace);
  clrBtnShadow.Create(clBtnShadow);
  clrGrayText.Create(clGrayText);
  clrBtnText.Create(clBtnText);
  clrInactiveCaptionText.Create(clInactiveCaptionText);
  clrBtnHighlight.Create(clBtnHighlight);
  clr3DDkShadow.Create(cl3DDkShadow);
  clr3DLight.Create(cl3DLight);
  clrInfoText.Create(clInfoText);
  clrInfoBk.Create(clInfoBk);
  {$ifndef LCLGTK2}
  clrHighlight.Create(clHighlight);
  clrBackground.Create(clBackground);
  {$ENDIF}

  clrHotLight.Create(clHotLight);
  clrGradientActiveCaption.Create(clGradientActiveCaption);
  clrGradientInactiveCaption.Create(clGradientInactiveCaption);
  clrMenuHighlight.Create(clMenuHighlight);
  clrMenuBar.Create(clMenuBar);
  clrForm.Create(clForm);
  clrColorDesktop.Create(clColorDesktop);
  clr3DFace.Create(cl3DFace);
  clr3DShadow.Create(cl3DShadow);
  clr3DHiLight.Create(cl3DHiLight);
  clrBtnHiLight.Create(clBtnHiLight);

  {$IFDEF LINUX}
  clrBlue                 := clrBlue.SwapRBChannels;
  clrRed                  := clrRed.SwapRBChannels;
  clrGreen                := clrGreen.SwapRBChannels;
  clrCyan                 := clrCyan.SwapRBChannels;
  clrMagenta              := clrMagenta.SwapRBChannels;
  clrYellow               := clrYellow.SwapRBChannels;
  clrWhite                := clrWhite.SwapRBChannels;
  clrGray                 := clrGray.SwapRBChannels;
  clrLtGray               := clrLtGray.SwapRBChannels;
  clrOlive                := clrOlive.SwapRBChannels;
  clrNavy                 := clrNavy.SwapRBChannels;
  clrPurple               := clrPurple.SwapRBChannels;
  clrFuchsia              := clrFuchsia.SwapRBChannels;
  clrTeal                 := clrTeal.SwapRBChannels;
  clrSilver               := clrSilver.SwapRBChannels;
  clrMaroon               := clrMaroon.SwapRBChannels;
  clrLime                 := clrLime.SwapRBChannels;
  clrLtGreen              := clrLtGreen.SwapRBChannels;
  clrAqua                 := clrAqua.SwapRBChannels;

  // WinGraph
  clrAliceBlue            := clrAliceBlue.SwapRBChannels;
  clrAlizarinCrimson      := clrAlizarinCrimson.SwapRBChannels;
  clrAmber                := clrAmber.SwapRBChannels;
  clrAmethyst             := clrAmethyst.SwapRBChannels;
  clrAntiqueWhite         := clrAntiqueWhite.SwapRBChannels;
  clrAquamarine           := clrAquamarine.SwapRBChannels;
  clrAsparagus            := clrAsparagus.SwapRBChannels;
  clrAzure                := clrAzure.SwapRBChannels;
  clrBeige                := clrBeige.SwapRBChannels;
  clrBisque               := clrBisque.SwapRBChannels;
  clrBistre               := clrBistre.SwapRBChannels;
  clrBitterLemon          := clrBitterLemon.SwapRBChannels;
  clrBlanchedAlmond       := clrBlanchedAlmond.SwapRBChannels;
  clrBlueViolet           := clrBlueViolet.SwapRBChannels;
  clrBondiBlue            := clrBondiBlue.SwapRBChannels;
  clrBrass                := clrBrass.SwapRBChannels;
  clrBrightGreen          := clrBrightGreen.SwapRBChannels;
  clrBrightViolet         := clrBrightViolet.SwapRBChannels;
  clrBronze               := clrBronze.SwapRBChannels;
  clrBrown                := clrBrown.SwapRBChannels;
  clrBuff                 := clrBuff.SwapRBChannels;
  clrBurgundy             := clrBurgundy.SwapRBChannels;
  clrBurlyWood            := clrBurlyWood.SwapRBChannels;
  clrBurntOrange          := clrBurntOrange.SwapRBChannels;
  clrBurntSienna          := clrBurntSienna.SwapRBChannels;
  clrBurntUmber           := clrBurntUmber.SwapRBChannels;
  clrCadetBlue            := clrCadetBlue.SwapRBChannels;
  clrCamouflageGreen      := clrCamouflageGreen.SwapRBChannels;
  clrCardinal             := clrCardinal.SwapRBChannels;
  clrCarmine              := clrCarmine.SwapRBChannels;
  clrCarrot               := clrCarrot.SwapRBChannels;
  clrCasper               := clrCasper.SwapRBChannels;
  clrCerise               := clrCerise.SwapRBChannels;
  clrCerulean             := clrCerulean.SwapRBChannels;
  clrCeruleanBlue         := clrCeruleanBlue.SwapRBChannels;
  clrChartreuse           := clrChartreuse.SwapRBChannels;
  clrChocolate            := clrChocolate.SwapRBChannels;
  clrCinnamon             := clrCinnamon.SwapRBChannels;
  clrCobalt               := clrCobalt.SwapRBChannels;
  clrCopper               := clrCopper.SwapRBChannels;
  clrCoral                := clrCoral.SwapRBChannels;
  clrCorn                 := clrCorn.SwapRBChannels;
  clrCornflowerBlue       := clrCornflowerBlue.SwapRBChannels;
  clrCornsilk             := clrCornsilk.SwapRBChannels;
  clrCream                := clrCream.SwapRBChannels;
  clrCrimson              := clrCrimson.SwapRBChannels;
  clrDarkBrown            := clrDarkBrown.SwapRBChannels;
  clrDarkCerulean         := clrDarkCerulean.SwapRBChannels;
  clrDarkChestnut         := clrDarkChestnut.SwapRBChannels;
  clrDarkCoral            := clrDarkCoral.SwapRBChannels;
  clrDarkCyan             := clrDarkCyan.SwapRBChannels;
  clrDarkGoldenrod        := clrDarkGoldenrod.SwapRBChannels;
  clrDarkGray             := clrDarkGray.SwapRBChannels;
  clrDarkGreen            := clrDarkGreen.SwapRBChannels;
  clrDarkIndigo           := clrDarkIndigo.SwapRBChannels;
  clrDarkKhaki            := clrDarkKhaki.SwapRBChannels;
  clrDarkOlive            := clrDarkOlive.SwapRBChannels;
  clrDarkOliveGreen       := clrDarkOliveGreen.SwapRBChannels;
  clrDarkOrange           := clrDarkOrange.SwapRBChannels;
  clrDarkOrchid           := clrDarkOrchid.SwapRBChannels;
  clrDarkPastelGreen      := clrDarkPastelGreen.SwapRBChannels;
  clrDarkPink             := clrDarkPink.SwapRBChannels;
  clrDarkRed              := clrDarkRed.SwapRBChannels;
  clrDarkSalmon           := clrDarkSalmon.SwapRBChannels;
  clrDarkScarlet          := clrDarkScarlet.SwapRBChannels;
  clrDarkSeaGreen         := clrDarkSeaGreen.SwapRBChannels;
  clrDarkSlateBlue        := clrDarkSlateBlue.SwapRBChannels;
  clrDarkSlateGray        := clrDarkSlateGray.SwapRBChannels;
  clrDarkSpringGreen      := clrDarkSpringGreen.SwapRBChannels;
  clrDarkTan              := clrDarkTan.SwapRBChannels;
  clrDarkTeaGreen         := clrDarkTeaGreen.SwapRBChannels;
  clrDarkTerraCotta       := clrDarkTerraCotta.SwapRBChannels;
  clrDarkTurquoise        := clrDarkTurquoise.SwapRBChannels;
  clrDarkViolet           := clrDarkViolet.SwapRBChannels;
  clrDeepPink             := clrDeepPink.SwapRBChannels;
  clrDeepSkyBlue          := clrDeepSkyBlue.SwapRBChannels;
  clrDenim                := clrDenim.SwapRBChannels;
  clrDodgerBlue           := clrDodgerBlue.SwapRBChannels;
  clrEmerald              := clrEmerald.SwapRBChannels;
  clrEggplant             := clrEggplant.SwapRBChannels;
  clrFernGreen            := clrFernGreen.SwapRBChannels;
  clrFireBrick            := clrFireBrick.SwapRBChannels;
  clrFlax                 := clrFlax.SwapRBChannels;
  clrFloralWhite          := clrFloralWhite.SwapRBChannels;
  clrForestGreen          := clrForestGreen.SwapRBChannels;
  clrFractal              := clrFractal.SwapRBChannels;
  clrGainsboro            := clrGainsboro.SwapRBChannels;
  clrGamboge              := clrGamboge.SwapRBChannels;
  clrGhostWhite           := clrGhostWhite.SwapRBChannels;
  clrGold                 := clrGold.SwapRBChannels;
  clrGoldenrod            := clrGoldenrod.SwapRBChannels;
  clrGrayAsparagus        := clrGrayAsparagus.SwapRBChannels;
  clrGrayTeaGreen         := clrGrayTeaGreen.SwapRBChannels;
  clrGreenYellow          := clrGreenYellow.SwapRBChannels;
  clrHeliotrope           := clrHeliotrope.SwapRBChannels;
  clrHoneydew             := clrHoneydew.SwapRBChannels;
  clrHotPink              := clrHotPink.SwapRBChannels;
  clrIndianRed            := clrIndianRed.SwapRBChannels;
  clrIndigo               := clrIndigo.SwapRBChannels;
  clrInternationalKleinBlue:= clrInternationalKleinBlue.SwapRBChannels;
  clrInternationalOrange  := clrInternationalOrange.SwapRBChannels;
  clrIvory                := clrIvory.SwapRBChannels;
  clrJade                 := clrJade.SwapRBChannels;
  clrKhaki                := clrKhaki.SwapRBChannels;
  clrLavender             := clrLavender.SwapRBChannels;
  clrLavenderBlush        := clrLavenderBlush.SwapRBChannels;
  clrLawnGreen            := clrLawnGreen.SwapRBChannels;
  clrLemon                := clrLemon.SwapRBChannels;
  clrLemonChiffon         := clrLemonChiffon.SwapRBChannels;
  clrLightBlue            := clrLightBlue.SwapRBChannels;
  clrLightBrown           := clrLightBrown.SwapRBChannels;
  clrLightCoral           := clrLightCoral.SwapRBChannels;
  clrLightCyan            := clrLightCyan.SwapRBChannels;
  clrLightGoldenrodYellow := clrLightGoldenrodYellow.SwapRBChannels;
  clrLightMagenta         := clrLightMagenta.SwapRBChannels;
  clrLightRed             := clrLightRed.SwapRBChannels;
  clrLightSalmon          := clrLightSalmon.SwapRBChannels;
  clrLightSeaGreen        := clrLightSeaGreen.SwapRBChannels;
  clrLightSkyBlue         := clrLightSkyBlue.SwapRBChannels;
  clrLightSlateGray       := clrLightSlateGray.SwapRBChannels;
  clrLightSteelBlue       := clrLightSteelBlue.SwapRBChannels;
  clrLightYellow          := clrLightYellow.SwapRBChannels;
  clrLilac                := clrLilac.SwapRBChannels;
  clrLimeGreen            := clrLimeGreen.SwapRBChannels;
  clrLinen                := clrLinen.SwapRBChannels;
  clrMalachite            := clrMalachite.SwapRBChannels;
  clrMauve                := clrMauve.SwapRBChannels;
  clrMediumAquamarine     := clrMediumAquamarine.SwapRBChannels;
  clrMediumBlue           := clrMediumBlue.SwapRBChannels;
  clrMediumOrchid         := clrMediumOrchid.SwapRBChannels;
  clrMediumSeaGreen       := clrMediumSeaGreen.SwapRBChannels;
  clrMediumSlateBlue      := clrMediumSlateBlue.SwapRBChannels;
  clrMediumSpringGreen    := clrMediumSpringGreen.SwapRBChannels;
  clrMediumTurquoise      := clrMediumTurquoise.SwapRBChannels;
  clrMediumVioletRed      := clrMediumVioletRed.SwapRBChannels;
  clrMidnightBlue         := clrMidnightBlue.SwapRBChannels;
  clrMintCream            := clrMintCream.SwapRBChannels;
  clrMistyRose            := clrMistyRose.SwapRBChannels;
  clrMoccasin             := clrMoccasin.SwapRBChannels;
  clrMoneyGreen           := clrMoneyGreen.SwapRBChannels;
  clrMonza                := clrMonza.SwapRBChannels;
  clrMossGreen            := clrMossGreen.SwapRBChannels;
  clrMountbattenPink      := clrMountbattenPink.SwapRBChannels;
  clrMustard              := clrMustard.SwapRBChannels;
  clrNavajoWhite          := clrNavajoWhite.SwapRBChannels;
  clrOchre                := clrOchre.SwapRBChannels;
  clrOldGold              := clrOldGold.SwapRBChannels;
  clrOldLace              := clrOldLace.SwapRBChannels;
  clrOliveDrab            := clrOliveDrab.SwapRBChannels;
  clrOrange               := clrOrange.SwapRBChannels;
  clrOrangeRed            := clrOrangeRed.SwapRBChannels;
  clrOrchid               := clrOrchid.SwapRBChannels;
  clrPaleBrown            := clrPaleBrown.SwapRBChannels;
  clrPaleCarmine          := clrPaleCarmine.SwapRBChannels;
  clrPaleChestnut         := clrPaleChestnut.SwapRBChannels;
  clrPaleCornflowerBlue   := clrPaleCornflowerBlue.SwapRBChannels;
  clrPaleGoldenrod        := clrPaleGoldenrod.SwapRBChannels;
  clrPaleGreen            := clrPaleGreen.SwapRBChannels;
  clrPaleMagenta          := clrPaleMagenta.SwapRBChannels;
  clrPaleMauve            := clrPaleMauve.SwapRBChannels;
  clrPalePink             := clrPalePink.SwapRBChannels;
  clrPaleSandyBrown       := clrPaleSandyBrown.SwapRBChannels;
  clrPaleTurquoise        := clrPaleTurquoise.SwapRBChannels;
  clrPaleVioletRed        := clrPaleVioletRed.SwapRBChannels;
  clrPapayaWhip           := clrPapayaWhip.SwapRBChannels;
  clrPastelGreen          := clrPastelGreen.SwapRBChannels;
  clrPastelPink           := clrPastelPink.SwapRBChannels;
  clrPeach                := clrPeach.SwapRBChannels;
  clrPeachOrange          := clrPeachOrange.SwapRBChannels;
  clrPeachPuff            := clrPeachPuff.SwapRBChannels;
  clrPeachYellow          := clrPeachYellow.SwapRBChannels;
  clrPear                 := clrPear.SwapRBChannels;
  clrPeriwinkle           := clrPeriwinkle.SwapRBChannels;
  clrPersianBlue          := clrPersianBlue.SwapRBChannels;
  clrPeru                 := clrPeru.SwapRBChannels;
  clrPineGreen            := clrPineGreen.SwapRBChannels;
  clrPink                 := clrPink.SwapRBChannels;
  clrPinkOrange           := clrPinkOrange.SwapRBChannels;
  clrPlum                 := clrPlum.SwapRBChannels;
  clrPowderBlue           := clrPowderBlue.SwapRBChannels;
  clrPrussianBlue         := clrPrussianBlue.SwapRBChannels;
  clrPuce                 := clrPuce.SwapRBChannels;
  clrPumpkin              := clrPumpkin.SwapRBChannels;
  clrRawUmber             := clrRawUmber.SwapRBChannels;
  clrReef                 := clrReef.SwapRBChannels;
  clrRobinEggBlue         := clrRobinEggBlue.SwapRBChannels;
  clrRosyBrown            := clrRosyBrown.SwapRBChannels;
  clrRoyalBlue            := clrRoyalBlue.SwapRBChannels;
  clrRusset               := clrRusset.SwapRBChannels;
  clrRust                 := clrRust.SwapRBChannels;
  clrSaddleBrown          := clrSaddleBrown.SwapRBChannels;
  clrSaffron              := clrSaffron.SwapRBChannels;
  clrSalmon               := clrSalmon.SwapRBChannels;
  clrSandyBrown           := clrSandyBrown.SwapRBChannels;
  clrSangria              := clrSangria.SwapRBChannels;
  clrSapphire             := clrSapphire.SwapRBChannels;
  clrScarlet              := clrScarlet.SwapRBChannels;
  clrSeaGreen             := clrSeaGreen.SwapRBChannels;
  clrSeaShell             := clrSeaShell.SwapRBChannels;
  clrSepia                := clrSepia.SwapRBChannels;
  clrSienna               := clrSienna.SwapRBChannels;
  clrSkyBlue              := clrSkyBlue.SwapRBChannels;
  clrSlateBlue            := clrSlateBlue.SwapRBChannels;
  clrSlateGray            := clrSlateGray.SwapRBChannels;
  clrSnow                 := clrSnow.SwapRBChannels;
  clrSpringGreen          := clrSpringGreen.SwapRBChannels;
  clrSteelBlue            := clrSteelBlue.SwapRBChannels;
  clrSwampGreen           := clrSwampGreen.SwapRBChannels;
  clrTaupe                := clrTaupe.SwapRBChannels;
  clrTangerine            := clrTangerine.SwapRBChannels;
  clrTeaGreen             := clrTeaGreen.SwapRBChannels;
  clrTenne                := clrTenne.SwapRBChannels;
  clrTerraCotta           := clrTerraCotta.SwapRBChannels;
  clrThistle              := clrThistle.SwapRBChannels;
  clrTomato               := clrTomato.SwapRBChannels;
  clrTurquoise            := clrTurquoise.SwapRBChannels;
  clrUltramarine          := clrUltramarine.SwapRBChannels;
  clrVermilion            := clrVermilion.SwapRBChannels;
  clrViolet               := clrViolet.SwapRBChannels;
  clrVioletEggplant       := clrVioletEggplant.SwapRBChannels;
  clrViridian             := clrViridian.SwapRBChannels;
  clrWheat                := clrWheat.SwapRBChannels;
  clrWhiteSmoke           := clrWhiteSmoke.SwapRBChannels;
  clrWisteria             := clrWisteria.SwapRBChannels;
  clrYellowGreen          := clrYellowGreen.SwapRBChannels;
  clrZinnwaldite          := clrZinnwaldite.SwapRBChannels;

  // colors en masse
  clrCoral2               := clrCoral2.SwapRBChannels;
  clrCornflowerBlue2      := clrCornflowerBlue2.SwapRBChannels;
  clrDarkGreen2           := clrDarkGreen2.SwapRBChannels;
  clrDarkOliveGreen2      := clrDarkOliveGreen2.SwapRBChannels;
  clrDarkSlateBlue2       := clrDarkSlateBlue2.SwapRBChannels;
  clrDarkSlateGray2       := clrDarkSlateGray2.SwapRBChannels;
  clrDarkSlateGrey        := clrDarkSlateGrey.SwapRBChannels;
  clrDarkTurquoise2       := clrDarkTurquoise2.SwapRBChannels;
  clrFirebrick2           := clrFirebrick2.SwapRBChannels;
  clrGold2                := clrGold2.SwapRBChannels;
  clrGoldenrod2           := clrGoldenrod2.SwapRBChannels;
  clrGreenYellow2         := clrGreenYellow2.SwapRBChannels;
  clrIndian               := clrIndian.SwapRBChannels;
  clrKhaki2               := clrKhaki2.SwapRBChannels;
  clrLightBlue2           := clrLightBlue2.SwapRBChannels;
  clrLightSteelBlue2      := clrLightSteelBlue2.SwapRBChannels;
  clrLimeGreen2           := clrLimeGreen2.SwapRBChannels;
  clrMediumAquamarine2    := clrMediumAquamarine2.SwapRBChannels;
  clrMediumBlue2          := clrMediumBlue2.SwapRBChannels;
  clrMediumForestGreen    := clrMediumForestGreen.SwapRBChannels;
  clrMediumGoldenrod      := clrMediumGoldenrod.SwapRBChannels;
  clrMediumOrchid2        := clrMediumOrchid2.SwapRBChannels;
  clrMediumSeaGreen2      := clrMediumSeaGreen2.SwapRBChannels;
  clrMediumSlateBlue2     := clrMediumSlateBlue2.SwapRBChannels;
  clrMediumSpringGreen2   := clrMediumSpringGreen2.SwapRBChannels;
  clrMediumTurquoise2     := clrMediumTurquoise2.SwapRBChannels;
  clrMediumViolet         := clrMediumViolet.SwapRBChannels;
  clrMidnightBlue2        := clrMidnightBlue2.SwapRBChannels;
  clrNavyBlue             := clrNavyBlue.SwapRBChannels;
  clrOrange2              := clrOrange2.SwapRBChannels;
  clrPink2                := clrPink2.SwapRBChannels;
  clrSalmon2              := clrSalmon2.SwapRBChannels;
  clrSeaGreen2            := clrSeaGreen2.SwapRBChannels;
  clrSienna2              := clrSienna2.SwapRBChannels;
  clrSkyBlue2             := clrSkyBlue2.SwapRBChannels;
  clrSlateBlue2           := clrSlateBlue2.SwapRBChannels;
  clrSteelBlue2           := clrSteelBlue2.SwapRBChannels;
  clrTan                  := clrTan.SwapRBChannels;
  clrThistle2             := clrThistle2.SwapRBChannels;
  clrTurquoise2           := clrTurquoise2.SwapRBChannels;
  clrViolet2              := clrViolet2.SwapRBChannels;
  clrVioletRed            := clrVioletRed.SwapRBChannels;
  clrWheat2               := clrWheat2.SwapRBChannels;
  clrYellowGreen2         := clrYellowGreen2.SwapRBChannels;
  clrSummerSky            := clrSummerSky.SwapRBChannels;
  clrRichBlue             := clrRichBlue.SwapRBChannels;
  clrBrass2               := clrBrass2.SwapRBChannels;
  clrCopper2              := clrCopper2.SwapRBChannels;
  clrBronze2              := clrBronze2.SwapRBChannels;
  clrBronze3              := clrBronze3.SwapRBChannels;
  clrBrightGold           := clrBrightGold.SwapRBChannels;
  clrOldGold2             := clrOldGold2.SwapRBChannels;
  clrFeldspar             := clrFeldspar.SwapRBChannels;
  clrQuartz               := clrQuartz.SwapRBChannels;
  clrNeonPink             := clrNeonPink.SwapRBChannels;
  clrDarkPurple           := clrDarkPurple.SwapRBChannels;
  clrNeonBlue             := clrNeonBlue.SwapRBChannels;
  clrCoolCopper           := clrCoolCopper.SwapRBChannels;
  clrMandarinOrange       := clrMandarinOrange.SwapRBChannels;
  clrLightWood            := clrLightWood.SwapRBChannels;
  clrMediumWood           := clrMediumWood.SwapRBChannels;
  clrDarkWood             := clrDarkWood.SwapRBChannels;
  clrSpicyPink            := clrSpicyPink.SwapRBChannels;
  clrSemiSweetChoc        := clrSemiSweetChoc.SwapRBChannels;
  clrBakersChoc           := clrBakersChoc.SwapRBChannels;
  clrFlesh                := clrFlesh.SwapRBChannels;
  clrNewTan               := clrNewTan.SwapRBChannels;
  clrNewMidnightBlue      := clrNewMidnightBlue.SwapRBChannels;
  clrVeryDarkBrown        := clrVeryDarkBrown.SwapRBChannels;
  clrDarkBrown2           := clrDarkBrown2.SwapRBChannels;
  clrDarkTan2             := clrDarkTan2.SwapRBChannels;
  clrGreenCopper          := clrGreenCopper.SwapRBChannels;
  clrDkGreenCopper        := clrDkGreenCopper.SwapRBChannels;
  clrDustyRose            := clrDustyRose.SwapRBChannels;
  clrHuntersGreen         := clrHuntersGreen.SwapRBChannels;
  clrScarlet2             := clrScarlet2.SwapRBChannels;
  clrMediumPurple2        := clrMediumPurple2.SwapRBChannels;
  clrLightPurple          := clrLightPurple.SwapRBChannels;
  clrVeryLightPurple      := clrVeryLightPurple.SwapRBChannels;

  {$ENDIF}

end;

Initialization

  RegisterClasses([TBZColorItemProperty, TBZColorList, TBZGradientColorItem, TBZGradientColorList, TBZColorsManager, TBZGradientProperty]);

  ComputeCombineLUT;

  InitSystemColors;

Finalization

  if Assigned(vColorRegister) then FreeAndNil(vColorRegister);

  UnRegisterClasses([TBZColorItemProperty, TBZColorList, TBZGradientColorItem, TBZGradientColorList, TBZColorsManager, TBZGradientProperty]);
//==============================================================================
End.
