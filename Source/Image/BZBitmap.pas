(*
  @abstract(Contient un objet dont les performances d'acces et d'affichage
  ont étés optimisés pour un affichage en 32bits avec prise en charge de la
  transparence en mode RGBA ou BGRA suivant l'OS.)

  -------------------------------------------------------------------------------------------------------------

  @created(2016-06-08)
  @author(J.Delauney (BeanzMaster))
  Historique : @br
  @unorderedList(
    @item(08/06/2016 : Creation  )
  )

  -------------------------------------------------------------------------------------------------------------

  @bold(Petite Histoire :) @br
  L'acces aux bitmaps par GLscene est confu et il subsiste des bugs (conversion Delphi=>Lazarus) @br
  De plus la gestion des bitmaps avec FPC et la LCL est quelques peu "capricieuse" @br
  Mixer les possibilités de GLScene en lui rajoutant une gestion des bitmaps optimisée. Tout était bénef ! @br
  Il m'a donc également semblé évident  de rajouter les fonctionnalité de la librairie "GraphiEx"
  de Mike  Lischke (http://www.soft-gems.net/) qui est le créateur initial de GLScene.

  Au final cela donne les capacités suivante : @br
  @unorderedList(
   @item(Travailler avec format de donnée brute 32Bits au format de couleur RGBA ou BGRA  (Linux / Windows) afin d'optimiser les performances d'affichage)
   @item(Affichage 32bit avec ou sans transparence en fonction du sytsteme et de l'interface.)
   @item(Accéder aux données graphique indépendemment des routines de FPC et de la LCL)
   @item(Acces aux pixels optimisé)
   @item(Capaciter dessiner des lignes, rectangles, cercles, polygones, textes, arc, courbes,...)
   @item(Appliquer de nombreux filtres sur les couleurs)
   @item(Appliquer des effets spéciaux)
   @item(Remplissage avec des dégradés,  des textures, patterns)
   @item(Plusieurs modes de redimensionnement (Stretch, StretSmooth, DownSample, Resample))
   @unorderedList(
     @item(Plus de 30 filtres d'interpolation disponibles pour "Resample"  : Box, Hermit, Cubic, Lanscoz, Mitchell, Hagraam, Kayser,...)
    )
   @item(Convertion des couleurs dans plusieurs formats (rgba, bgra, hsl, hsv, cmyk, cie, yuv, lab, lch) et bit par pixel (de 1 à 128bits))
   @item(Prise en charge des differents formats de couleurs dans les fichiers)
   @item(Prise en charge de différents formats d'image (independant des unités de  FPC et Lazarus))
  )

  Liste des formats supportés + variantes : @br
  En lecture et ecriture : @br
  @unorderedList(
    @item(BMP : TBZImageFileBMP)
    @item(JPEG : TBZImageFileJPEG)
    @item(TGA : TBZImageFileTGA)
  )

  En lecture seule : @br
  @unorderedList(
    @item(PNG : TBZImageFilePNG)
    @item(PIXMAP : TBZImageFilePPM)
    @item(PCX : TBZImageFilePCX)
    @item(XPM : TBZImageFileXPM)

  )

  -------------------------------------------------------------------------------------------------------------

  @bold(Notes) : @br
  La base de tout ce "beanz" entre autres : @br
  Discussions intéressantes, n'est t-il pas ? @br
  @unorderedList(
    @item(https://www.developpez.net/forums/d1610633/autres-langages/pascal/lazarus/info-font-graphisme-pointu/)
    @item(https://www.developpez.net/forums/d1677547/autres-langages/pascal/lazarus/copie-d-image-resultat-mauvais-selon-type-d-image/)
  )

  C'est bizarre des fois de voir comment le temps passe et de se rendre compte que certaines
  choses ; même en informatique ne change pas. @br La methode ici pour gérer et
  manipuler une image le plus rapidement possible est la même qu'il y a plus
  de 20 ans sous DOS avec les Modes X/Y. La methode reste quasi identique :D @br
  De quoi se refaire des demoscenes, des jeux 2D à l'ancienne facilement... ;)
  avec de très bonnes performances. @br
  Couplé à OpenGL/Vulkan voir même DirectX, ça peux faire mal.

  @unorderedList(
    @item(http://wiki.lazarus.freepascal.org/Fast_direct_pixel_access)
    @item(Plein de documents sur  des sujets  mathematique : http://www.nrbook.com/a/bookcpdf.php)
    @item(Parlons couleur : http://www.brucelindbloom.com/index.html?ColorCheckerRGB.html et http://www.easyrgb.com/en/math.php)
  )

  Vous prendrez bien un peu de rééchantillonage pour la route ?
  @unorderedList(
   @item(https://clouard.users.greyc.fr/Pantheon/experiments/rescaling/index-fr.html#convolution)
   @item(http://www.drdobbs.com/image-scaling-with-bresenham/184405045)
  )

  @unorderedList(
   @item(Tout ce deforme ! : http://edn.embarcadero.com/bg/article/40052
   @item(Questions de "bit" et de logique : http://lab.polygonal.de/2007/05/10/bitwise-gems-fast-integer-math/
  )

  Quelques liens qui me servent de références : @br
  @unorderedList(
    @item(http://www.efg2.com/Lab/)
    @item(http://delphi-kb.blogspot.ch)
    @item(http://www.davdata.nl)
    @item(http://patrick-bonnin.developpez.com/cours/vision/apprendre-bases-traitement-image/partie-2-visualisation-images-operateurs-simples/#LIII-D-1)
    @item(http://en.wikipedia.org/wiki/Pixel_art_scaling_algorithms)
    @item(https://en.wikipedia.org/wiki/Cohen–Sutherland_algorithm)
    @item(https://www.gamedev.net/articles/programming/graphics/graphics-programming-black-book-r1698)
    @item(http://www.easyrgb.com/en/math.php)
    @item(http://www.brucelindbloom.com/index.html?ColorCheckerRGB.html)
    @item(http://www.jcolibri.com/articles/graphique/correction_couleur/correction_couleur.html)
    @item(https://fr.wikipedia.org/wiki/Filtre_médian)
    @item(http://benjlaiel.hebergratuit.net/filtrage_Cours6.ppt)
    @item(http://www.helixsoft.nl/articles/circle/sincos.htm)
    @item(http://www.dailyfreecode.com/Code/draw-circular-arc-trigonometric-method-669.aspx)
    @item(http://answers.google.com/answers/threadview/id/769733.html)
    @item(http://www.cambridgeincolour.com/tutorials/image-resize-for-web.htm)
    @item(http://www.cambridgeincolour.com/tutorials/digital-photo-enlargement.htm)
    @item(https://homepages.inf.ed.ac.uk/rbf/HIPR2/gsmooth.htm)
    @item(https://www.eriksmistad.no/gaussian-blur-using-opencl-and-the-built-in-images-textures/)
    @item(http://blog.ivank.net/fastest-gaussian-blur.html)
    @item(http://xphilipp.developpez.com/articles/filtres/)
    @item(https://www.developpez.net/forums/d328817/general-developpement/algorithme-mathematiques/contribuez/image-gradient-hessienne-convolution/)
    @item(http://lodev.org/cgtutor/filtering.html)
    @item(http://www.roborealm.com/help/Convolution.php)
    @item(http://www.sunshine2k.de/coding/java/TriangleRasterization/TriangleRasterization.html#sunbresenhamarticle)
    @item(https://www.mikekohn.net/stuff/image_processing.php)
    @item(https://www.youtube.com/watch?v=-X49VQgi86E)
    @item(http://crteknologies.fr/projets/tpe_compression_graphique/)
    @item(http://www.optique-ingenieur.org/fr/cours/OPI_fr_M04_C05/co/Contenu_02.html)
    @item(http://www.nrbook.com/a/bookcpdf.php)
    @item(http://www.hugi.scene.org/online/coding/hugi%20se%204%20-%20index%20sorted%20by%20author.htm)
    @item(http://www.jagregory.com/abrash-black-book/)
    @item(http://www.tsi.enst.fr/pages/enseignement/ressources/mti/)
    @item(https://perso.esiee.fr/~perretb/I5FM/TAI/histogramme/index.html)
    @item(http://www.imageprocessingplace.com/root_files_V3/tutorials.htm)
  )

  -------------------------------------------------------------------------------------------------------------

  @bold(Bugs) :@br
  @unorderedList(
    @item(/!\ pour l'application de certain filtre (blur notemment) il faudra rajouter@br
           l'action PreMultiplyAlpha AVANT l'application du filtre afin de preserver au mieux le@br
           canal ALPHA original)
    @item()
  )

  -------------------------------------------------------------------------------------------------------------

  @bold(Credits) : Tous les liens au dessus, dessous, dedans, et au-delà...... @br
  @unorderedList(
     @item( FPC/Lazarus, GLScene, Vampire / FreeImage Lib, Graphic32, BGRABitmap...)
   )

  -------------------------------------------------------------------------------------------------------------

  @bold(LICENCE) : MPL / GPL

  ------------------------------------------------------------------------------------------------------------- *)
Unit BZBitmap;
//------------------------------------------------------------------------------

//----------------------------[ TODO LIST ]-------------------------------------

{ TODO 1 -oBZBitmap -cGeneral : Modifier le canvas vers un canvas vectoriel type HTML5/SVG
  - Changer le nom de la propriété Pen par Stroke
  - Changer le nom de la propriété Brush par Fill
  - Ajouter un objet "Path"
  - Modifier l'algorithme de génération des contours des polylines et polygones
  - Améliorer le rendu des textes
}

//------------------------------------------------------------------------------

//==============================================================================
{$mode objfpc}{$H+}
{$i ..\bzscene_options.inc}
//==============================================================================

Interface

Uses
  Classes, SysUtils, {Types,} GraphType, Graphics,
  LCLIntf, LCLType,
  LazFileUtils, ExtCtrls,
  BZClasses, BZArrayClasses,                  //< ---> Unités de bases
  BZMath, BZVectorMath,                       //< ---> Unités mathématique et vectorielle
  //BZCoordinates,                            //< ---> Objet persistant pour la gestion de coordonnées (2D,3D et ou 4D)
  BZColors,
  BZGraphic,                                  //< ---> Unité de base pour le Graphisme
  BZFontClasses,                              //< ---> Unité de base pour la gestion et le rendu des polices de caractères dans un "Canvas"
  BZCanvasClasses,                            //< ---> Unité de base pour l'objet "Canvas" et
  BZInterpolationFilters,                     //< ---> Filtres de rééchantillonage
  {BZTweener,                                 //< ---> Fonctions d'interpolations }
  BZLibFreeType, BZLibFreeTypehDyn,           //< ---> Support FreeType, pour le rendu de texte
  BZBitmapColorFilters,
  BZBitmapBlurFilters,
  BZBitmapDeformationFilters,
  BZBitmapMorphologicalFilters,
  BZBitmapThresholdFilters;

// Constantes de "temps" pour animer un ensemble de bitmap et/ou le support d'animation au format GIF, APNG ou autre
Const
  cAnimation_DelayRatio : integer = 12;     //< Multiplicateur pour le temps d'attente entre chaque image de l'animation en ms

     { Les valeurs d'attente suivantes, doivent toutes
       être multiplié par "cAnimation_DelayRatio" qui calcule le temps effectif (en mS).
       Selon les spécifications GIF, cette valeur devrait être 10.
       Puisque nos routines de dessin sont rapide on peux avoir besoin
       d'augmenter cette valeur si les boucles d'animations sont trop rapides.
       La valeur optimale est impossible à déterminer car cela dépend de la
       vitesse du CPU, la carte video, de la mémoire et de nombreux autres facteurs. }

  cAnimation_DefaultDelayRatio : integer = 10;   //< Temps d'attente par defaut
  cAnimation_MinimumDelayRatio : integer = 3;    //< Temps d'attente minimum
  cAnimation_MaximumDelayRatio : integer = 1000; //< Temps d'attente maximum. Cette valeur garantit que de longue, ou lente animation ne bloque pas le systeme. (1000 = 10000 mS = 10 Seconds)

Type
  TBZBitmap = class;
  TBZBitmapCanvas = class;

  TBZCustomBitmapFontDrawer = Class(TBZCustomFontDrawer)
  private
     FSurfaceCanvas : TBZBitmapCanvas;

   protected
     procedure DrawLastText (atX,atY:integer);
     procedure DrawChar (x,y:integer; data:PByteArray; pitch, width, height:integer); virtual;
     procedure DrawCharBW (x,y:integer; data:PByteArray; pitch, width, height:integer); virtual;
     // procedure DoCopyProps (From:TFPCanvasHelper); override;

     procedure Internal_DrawText (atx,aty:integer; atext:string); override;
     procedure Internal_DrawText (atx,aty:integer; atext: unicodestring); override;

   public
     Constructor Create(ASurface: TBZBitmapCanvas); Virtual;
     Destructor Destroy; Override;
   end;

  { Classe de rendu de texte par defaut
    @unorderedlist(
      @item(Rendu des contours)
      @item(Remplissage, couleur unie, texture, dégradé)
      @item(Rendu d'une Ombre)
    ) }
  TBZBitmapFontDrawer = Class(TBZCustomBitmapFontDrawer)
  Private
    FMask:     TBZCustomBitmap;
    //FShadowed: Boolean;
    //FOutLined: Boolean;
  Protected

  Public
    Constructor Create(ASurface: TBZBitmapCanvas); Override;
    Destructor Destroy; Override;
  End;

  { Enumération des algorithme de remplissage de zone disponibles pour le Canvas }
  TBZFloodFillMode = (ffmBoundary4, ffmBoundary8, ffmScanLine);

  { Regroupe les fonctions pour dessiner dans un TBZBitmap.
     - point, ligne, cercke, éllipse, rectangle, triangle, polygone, arc, chord,
       bezier, spline, remplissage, texte....@br
     Avec ou sans adoucissement, remplissage uniforme, patterne, degradé, texture,
     options pour l'aspect des contours... }
  TBZBitmapCanvas = Class(TBZCustomCanvas)
  private
    FOwnerBitmap : TBZCustomBitmap;
  protected
    function Internal_CreateDefaultFont : TBZCustomFont; override;

    procedure Internal_TextOut (x,y:integer;text:string); override;
    procedure Internal_GetTextSize (text:string; var w,h:integer); override;
    function  Internal_GetTextHeight (text:string) : integer; override;
    function  Internal_GetTextWidth (text:string) : integer; override;

    function GetPen : TBZCustomCanvasPen;
    function GetBrush : TBZCustomCanvasBrush;

    procedure Internal_HLine(x1, y1, x2 : Single); // Pour le remplissage couleur unique
    procedure Internal_HLinePen(x1, y1, x2 : Single); // Pour le remplissage couleur unique des contours
    Procedure Internal_HLineGradient(x1, y1, x2: Integer); // Pour le remplissage avec dégradé
    Procedure Internal_HLineMap(x1, y1, x2: Integer);

    Procedure DrawLineBrush(x1, y1, x2: Integer);  // Pour le remplissage
    Procedure DrawLinePen(x1, y1, x2: Integer);  // Pour le remplissage des contours
    //procedure Internal_BuilLineStroke;

    procedure DrawVLine(x1,y1,y2 : Single); override;
    procedure DrawHLine(x1,y1,x2 : Single); override;
    procedure DrawLine(x1,y1,x2,y2 : Single); override;

    procedure DrawAntiAliasLine(x1,y1,x2,y2 : Single); override;

    procedure DrawPolyLine(Pts : TBZArrayOfFloatPoints; Const Closed : Boolean = False); override;
    procedure DrawAntiaAliasPolyLine(Pts : TBZArrayOfFloatPoints; Const Closed : Boolean = False); override;
    // procedure DrawBezierLine; virtual; abstract;
    // procedure DrawSplineLine; virtual; abstract;

    procedure DrawPolygon(Pts : TBZArrayOfFloatPoints);override;

    procedure DrawRectangle(x1,y1,x2,y2 : Single); override;
    procedure DrawRoundedRect(x1,y1,x2,y2 : Single;  Const Rx : Single = 3; Const Ry : Single = 3); override;

    procedure DrawArc(cx, cy, rx, ry, StartAngle, EndAngle: Single; Const ClockWise : Boolean = True);  override;

    procedure DrawCircle(cx,cy, Radius : Single); override;
    Procedure DrawAntiAliasCircle(cx, cy, Radius : Single); override;

    procedure DrawEllipse(cx, cy, Rx, Ry: Single);   override;
    Procedure DrawAntiAliasEllipse(cx, cy, Rx, Ry: Single); override;

    procedure DrawTriangle(x1,y1,x2,y2,x3,y3 : Single); override;
    //procedure DrawPath; virtual; abstract;
    //procedure DrawText; virtual; abstract;

    procedure FillStrokePolygon(Pts : TBZArrayOfFloatPoints);
    procedure FillPolygon(Pts : TBZArrayOfFloatPoints); override;
    procedure FillTriangle(x1, y1, x2, y2, x3, y3 : Single); override;

    procedure FillRectangle(x1,y1,x2,y2 : Single); override;
    procedure FillRoundedRectangle(x1,y1,x2,y2 : Single; Const Rx : Single = 3; Const Ry : Single = 3); override;

    procedure FillArc(StartX, StartY, EndX, EndY, StartAngle, EndAngle : Single); override;
    procedure FillCircle(cx,cy, Radius : Single); override;
    procedure FillEllipse(cx, cy, Rx, Ry : Single);  override;

    procedure FloodFill_Boundary4(px,py : Single; Const SearchColor, NewColor: TBZColor);
    procedure FloodFill_Boundary8(px,py : Single; Const SearchColor, NewColor: TBZColor);
    procedure FloodFill_ScanLine(px, py : Single; const SearchColor, NewColor : TBZColor);
    //procedure FillPath; virtual; abstract;


  public
    constructor Create(AOwner : TBZCustomBitmap); overload;
    Destructor Destroy; override;

    procedure FreeBuffer;  override;
    procedure ResizeBuffer(const AWidth, AHeight: integer); override;

    procedure FloodFill(px,py : Single; Const SearchColor, NewColor: TBZColor);  override;
    procedure FloodFill(px,py : Single; Const SearchColor, NewColor: TBZColor; Const FloodFillMethod : TBZFloodFillMode);  overload;

    procedure Line(PtFrom, PtTo : TBZPoint); overload;

    Procedure PutPixel(X, Y: Integer; AColor: TBZColor);
    Procedure PutAlphaBlendPixel(X, Y: Integer; AColor: TBZColor; Ratio : Single);

    property Pen : TBZCustomCanvasPen read GetPen;
    property Brush : TBZCustomCanvasBrush read GetBrush;
    property Surface : TBZCustomBitmap Read FOwnerBitmap;
  end;

  { Objet permettant le calcul et la manipulation de l'histogramme d'une image.@br
    D'après les codes source de Earl F. Glynn entre autres.

    Plus d'informations : @br
    @unorderedlist(
      @item(http://www.tsi.enst.fr/pages/enseignement/ressources/mti/egal-histo/rapport.htm)
      @item(https://en.wikipedia.org/wiki/Adaptive_histogram_equalization)
      @item(https://en.wikipedia.org/wiki/Normalization_(image_processing))
      @item(https://stackoverflow.com/questions/56905592/automatic-contrast-and-brightness-adjustment-of-a-color-photo-of-a-sheet-of-pape)
      @item(http://www.eyemaginary.com/Rendering/AutomaticHistogramWarping.pdf)
      @item(https://openclassrooms.com/fr/courses/1490316-introduction-a-la-vision-par-ordinateur/1491061-etirement-et-egalisation-dhistogramme)
    ) }
  TBZHistogram =  Class(TBZCustomOwnerBitmap)
  private
    FChannels: array[TBZHistogramChannel] of TBZHistogramArray;
    //FChannelsNormalize: array[TBZHistogramChannel] of Array[0..255] of Single;  //TBZHistogramArray;
    FChannelsMaximum : Array[TBZHistogramChannel] of Integer;
    FComputed : Boolean;


    FStatistics : TBZHistogramStatistics;

    function GetChannel(Index : TBZHistogramChannel) : TBZHistogramArray;
    function getStatistics : TBZHistogramStatistics;

  protected
    procedure ComputeStatistics;
    //function GetChannelIndex : Byte;
    //procedure ComputeHistogramByColorSpace(ColorSpace : TBZColorSpace);
  public
    procedure ComputeHistogram;

    procedure GetAutoLevels(Const Channel : TBZHistogramChannel; var OptimalFactorBrightness, OptimalFactorContrast: single);

    procedure AutoAdjustBrightnessContrast;

    procedure AdjustLevels(LowInputLevel, HighInputLevel, LowOutputLevel, HighOutputLevel : Single);

    procedure AutoAdjustLevels(Const Tolerance : Integer = 0);

    //procedure Adjust
    procedure Equalize;
    //procedure Homogenize;
    //procedure Stretch
    //procedure DrawTo(Dst : TBZBitmap);
    procedure DrawTo(Dst : TBZBitmap; HistoChannel : TBZHistorgramDrawMode; Const Logarithmic, ShowAxis, ShowGrid, ShowGradient : Boolean );

    property Channel[Index: TBZHistogramChannel]: TBZHistogramArray read GetChannel; default;
    property Statistics : TBZHistogramStatistics read getStatistics;
  end;

  { Regroupe les fonctions pour effectuer les opérations suivantes : @br
    @unorderedlist(
      @item(Inversion Horizontale et verticale)
      @item(Rotation)
      @item(Redimensionner (plusieurs methodes disponible))
    )

    @TODO : cisaillement, rotation par cisaillement, Deformation affine }
  TBZBitmapTransformations = Class(TBZCustomOwnerBitmap)
  Private
  Protected
    Procedure KeepAspectRatio(Const SrcW, SrcH: Integer; Var NewWidth, NewHeight: Integer);
  Public
    //Constructor Create(Const AOwner: TBZCustomBitmap); Overload;
    //Destructor Destroy; Override;
    { Retournement horizontal (effet mirroir) }
    Procedure FlipX;
    { Retournement vertical (effet mirroir) }
    Procedure FlipY;
    { Rotation de l'image de "Degrees") }
    Procedure Rotate(Const Degrees: Integer; Const Wrap: Boolean = False; Const RotCX: Integer = -1; Const RotCY: Integer = -1);

    // Const Wrap: Boolean = True;
    //procedure RotateTo(DstBmp : TBZBitmap; Const Degrees: Integer; Const RotCX: Integer = -1; Const RotCY: Integer = -1);

    //procedure RotateSmooth
    //procedure RotateShear

    { Redimensionne le bitmap sans adoucissement }
    Procedure Stretch(NW, NH: Integer; out NewWidth, NewHeight : Integer; Const KeepRatio: Boolean = False ) overload;
    //Procedure Stretch(NW, NH: Integer; Const KeepRatio: Boolean = False; out NewWidth, NewHeight : Integer; ) overload; // BUG COMPILATION FPC
    Procedure Stretch(NW, NH: Integer; Const KeepRatio: Boolean = False); overload;

    procedure StretchTo(DstBmp : TBZBitmap; Const KeepRatio: Boolean = False);

    { Redimensionne le bitmap avec adoucissement Bilineaire) }
    Procedure StretchSmooth(NW, NH: Integer; Const KeepRatio: Boolean = False);

    procedure StretchSmoothTo(DstBmp : TBZBitmap; Const KeepRatio: Boolean = False);

    { Redimensionne le bitmap avec adoucissement Bicubique

      Code adapté et modifié d'après un source de :
       http://codes-sources.commentcamarche.net/source/54486-reechantillonnage-bicubique-vs-stretchblt
         lui même adapté d'un code Paul Toth en C }
    Procedure StretchBicubic(NW, NH: Integer; Const KeepRatio: Boolean = False);

    procedure StretchBicubicTo(DstBmp : TBZBitmap; Const KeepRatio: Boolean = False);

    { Methode Experimentale
      Redimensionne en utilisant un "effet de cercle concentrique" (comme quand on jette une pierre dans une marre)
      Cette fonction est lente, mais donne de bon résultats dans l'ensemble.
      Sinon elle peux produire, suivant l'image et le facteur de redimensionnement
      des parasites.

      StretchSmart utilise une fonction de rééchantillognage "fenêtré" a base de SinH/SinC et Bessel

      Note : J'aimerai ajouté une dimension dans l'equation pour controler le lissage. Afin de supprimer
             ces "erreurs de diffusion" et obtenir de meilleurs résultats }
    Procedure StretchSmart(Var NW, NH: Integer; Const RayLength: Byte = 2; Const KeepRatio: Boolean = False);

    { Réduction des dimensions en largeur et hauteur du bitmap sans perte de qualité suivant un facteur.
      Ex "Factor = 1" pas de changements, si "Factor" = 2 : largeur = largeur*2 et hauteur = hauteur*2, ect...  }
    Procedure DownSample(Const Factor: Integer);
    //procedure DownSampleTo(DstBmp : TBZBitmap);

    { Réduction ou augmentation des dimensions du bitmap. Avec plusieurs méthode de rééchantillonnage possible :
       - imBox, imTriangle, imSpline, imCubic, imBell,imHermit, imMitchell, imLanscoz3, imBlackman,
         imBlackmanSinC, imBlackmanBessel, imQuadratic, imCatrom, imHanning, imHamming, imKaiser.... }
    Procedure Resample(NewWidth, NewHeight: Integer; ResampleMode: TBZInterpolationFilterMethod; Const KeepRatio: Boolean = False);

    Procedure ResampleTo(DstBmp : TBZBitmap; ResampleMode : TBZInterpolationFilterMethod; Const KeepRatio : Boolean = False);

    //procedure Resize(Const NewWidth, NewHeight : Integer; Const ResizeMode: TBZBitmapResizeMode = brmResample; Const ResampleMode:TBZResampleFilterMethod = rfmLanczos3 ;Const KeepRatio:Boolean=false);

    Procedure Crop(x, y, w, h: Integer); Overload;
    Procedure CropRect(ARect: TBZRect); Overload;
    Procedure Crop; Overload;
    // procedure CropTo(var DstBmp : TBZBitmap; x, y, w, h: Integer);

    //procedure Crop(Pixels:Integer;Const CropMode : TBZBitmapCropMode = cmAll);

    { AddBorders : }
    // Procedure ResizeSurface(bl,bt,br,bd:Integer;Const CenterBitmap : Boolean = false);

    //    procedure Skew;
    //    procedure Perspective

    procedure PolarToCartesian;
    procedure CartesianToPolar;

  End;

  TBZBitmapColorFilters  = Class;
  TBZBitmapEffectFilters=class;
  TBZBitmapBlurFilters   = Class;
  TBZBitmapConvolutionFilters = Class;
  TBZBitmapRenderFilters = Class;
  TBZBitmapDeformationFilters = Class;
  TBZBitmapSegmentationFilters = Class;
  TBZBitmapMorphologicalFilters = Class;
  TBZBitmapThresholdFilters = Class;

  //TBZBitmapMask = Class;
  //TBZBitmapSelectionMask = Class;

  { Classe spécialisée et optimisée dans la gestion d'images. Avec les capacités suivantes : @br
    @unorderedlist(
      @item(Importation / exportation des divers formats de fichier définis ou depuis un autre BZBitmap)
      @item(Redimensionnement (DownSample, Upsample, StrecthSmooth, Resample))
      @item(Transformations (Mirroir Horizontal et vertical, rotation, deformation perspective))
      @item(Filtres Couleurs + de 50 par defaut)
      @item(Lissage (Rapide, Gaussien, Radial, Zoomm, Wind))
      @item(Filtres de convolutions + de 40)
      @item(Filtres de déformations et autres + de 10 (Twirl, FishEye,... )
      @item(Dessiner des points, lignes, cercles, ellipses, rectangle, rectangle plein etc...)
    ) }
  TBZBitmap = Class(TBZBaseBitmap)  //TBZCustomBitmap
  Private

    FCanvas:      TBZBitmapCanvas;
    FTransformations: TBZBitmapTransformations;
    FColorFilters: TBZBitmapColorFilters;
    FEffectFilters:  TBZBitmapEffectFilters;
    FBlurFilters: TBZBitmapBlurFilters;
    FConvolutionFilters: TBZBitmapConvolutionFilters;
    FRenderFilters: TBZBitmapRenderFilters;
    FDeformationFilters: TBZBitmapDeformationFilters;
    FSegmentationFilters : TBZBitmapSegmentationFilters;
    FMorphologicalFilters : TBZBitmapMorphologicalFilters;
    FThresHoldFilters : TBZBitmapThresholdFilters;

    FHistogram : TBZHistogram;

    // Pour l'animation
    FFrameIndex : Integer;
    FAnimateTimer : TTimer;
    FAnimateBuffer : TBZBitmap;
    FRestoreBitmap : TBZBitmap;

    FIsAnimate : Boolean;
    FIsAnimatePaused : Boolean;
    FAnimateLoop : Integer;
    FCurrentLoopIndex: Integer;
    FAnimationSpeed : Integer;
    FLastDrawMode : Byte;

    (* FFrameLoop : Boolean;
    FFramePingpong : Boolean;
    FFrameStart : Integer;
    FFrameEnd   : Integer;
   *)
    FIsUpdated : Boolean;
    //FOnApplyMask

    FOnStartAnimate : TNotifyEvent;
    FOnStopAnimate : TNotifyEvent;
    FOnPauseAnimate : TNotifyEvent;
    FOnFrameChanged : TNotifyEvent;

  Protected
    //procedure SetUseMask(Const Value:Boolean); override;



    procedure DoOnAnimateTime(Sender : TObject);
  Public
    Constructor Create(AOwner: TPersistent; AWidth, AHeight: Integer);override;
    Constructor Create(aWidth, aHeight: Integer); Override;
    Constructor Create(aWidth, aHeight: Integer; FillColor : TBZColor); Overload;

    Constructor Create;override;
    Destructor Destroy; Override;

    { Assigne les propriété d'un autre bitmap }
    Procedure Assign(Source: TPersistent); Override;

    { Creation d'un clone du bitmap (nouvelle instance) }
    function CreateClone : TBZBitmap; overload;

    { Modifie les dimensions du bitmap }
    Procedure SetSize(NewWidth, NewHeight: Integer); //override;

    //{ Notifie au objet parent un changement de propriété }
    //procedure Changed();

    { Enregistre le bitmap dans un fichier }
    Procedure SaveToFile(Const FileName: String); Override;

    { Applique un masque à l'image }
    procedure ApplyMask(Const FreeAfter:Boolean = True); overload;
    procedure ApplyMask(MaskBmp : TBZBitmap); overload;

    { Applique une operation arithmetique avec un autre image. (cf : PutImageBlend) }
    procedure ArithmeticBlend(BlendMap : TBZBitmap; ColorOperator :  TBZColorCombineMode);

    procedure RenderFrame(Index : Integer);
    { Joue l'animation depuis les images contenue dans Layers }
    procedure StartAnimate;virtual;
    { Stoppe l'animation }
    procedure StopAnimate;virtual;
    { Pause l'animation }
    procedure PauseAnimate;virtual;

    //procedure ViewFrame(FrameIndex : Integer);

    { Accès au canvas }
    Property Canvas: TBZBitmapCanvas read FCanvas;
    { Accès aux méthodes de transformations applicables à l'image }
    Property Transformation: TBZBitmapTransformations read FTransformations;
    { Accès aux méthodes pour appliquer des filtres sur les couleurs }
    Property ColorFilter: TBZBitmapColorFilters read FColorFilters;
    { Accès aux méthodes pour appliquer des effets spéciaux }
    property EffectFilter : TBZBitmapEffectFilters read FEffectFilters;
    { Accès aux méthodes pour appliquer les filtres d'adoucissement et de réduction de bruit }
    Property BlurFilter: TBZBitmapBlurFilters read FBlurFilters;
    { Accès aux méthodes pour appliquer des filtres de convolution }
    Property ConvolutionFilter: TBZBitmapConvolutionFilters read FConvolutionFilters;
    { Accès aux méthodes de rendu }
    Property RenderFilter: TBZBitmapRenderFilters read FRenderFilters;
    { Accès aux méthodes pour appliquer des filtres de déformation }
    Property DeformationFilter: TBZBitmapDeformationFilters read FDeformationFilters;
    { Accès aux méthodes pour appliquer des filtres de segmentation et de détection des contours }
    Property SegmentationFilter : TBZBitmapSegmentationFilters read FSegmentationFilters;
    { Accès aux méthodes pour appliquer des filtres morphologique }
    Property MorphologicalFilter : TBZBitmapMorphologicalFilters read FMorphologicalFilters;
    { Accès aux méthodes de seuillage d'une image }
    Property ThresholdFilter : TBZBitmapThresholdFilters read FThresholdFilters;

    { Acces à l'histogramme et ses méthodes }
    property Histogram : TBZHistogram read FHistogram;

    //Property Mask : TBZBitmapMask Read FMask;
    //Property UseMask : Boolean read FUseMask Write SetUseMask;

    { Retourne @true si une animation est jouée }
    property IsAnimate : Boolean Read FIsAnimate;
    { Mettre l'animation en pause }
    property IsAnimatedPaused : Boolean Read FIsAnimatePaused;
    { Nombre de boucle à jouée pour l'animation }
    property AnimateLoop : Integer Read FAnimateLoop Write FAnimateLoop;
    { Vitesse de l'animation }
    property AnimationSpeed : Integer Read FAnimationSpeed Write FAnimationSpeed;
    { Image actuelle de l'animation }
    property CurrentFrame : Integer Read FFrameIndex;

    { Evènement déclenché au démarrage de l'animation }
    property OnStartAnimate : TNotifyEvent Read FOnStartAnimate Write FOnStartAnimate;
    { Evènement déclenché à l'arrêt de l'animation }
    property OnStopAnimate : TNotifyEvent Read FOnStopAnimate Write FOnStopAnimate;
    { Evènement déclenché lorsque l'animation est mise en pause }
    property OnPauseAnimate : TNotifyEvent Read FOnPauseAnimate Write FOnPauseAnimate;
    { Evènement déclenché a chaque changement d'image dans l'animation }
    property OnFrameChanged : TNotifyEvent Read FOnFrameChanged Write FOnFrameChanged;

    // Propriétés héritées
    property Pixels;
    Property Colors;

    Property FullFileName;
    property ErrorCount;
    property Errors;

    property OnLoadError;
    Property OnChange;
  End;


  //TBZBitmapMaskMode = (mmRed,mmGreen,mmBlue,mmAlpha,mmAverage);

  { Regroupe les filtres basés sur les couleurs dans une image.}

  { TBZBitmapColorFilters }

  TBZBitmapColorFilters = Class(TBZCustomOwnerBitmap)
  Private
  Protected
  Public
    { SwapChannel : Intervertit les canaux de couleurs entre eux.@br
      Avec "Mode" : @br
      @unorderedlist(
        @item(0 : Red   <-> Blue)
        @item(1 : Red   <-> Green)
        @item(2 : Green <-> Blue)
      ) }
    Procedure SwapChannel(Mode: TBZColorFilterSwapChannelMode);
    { Inverser les composantes RGB }
    Procedure Negate(Const PreserveLuminosity : Boolean = False; Const ApplyOnShadowTone : Boolean = True;  Const ApplyOnMidTone : Boolean = True; Const ApplyOnHighlightTone : Boolean = True);
    { Ajuste la saturation des couleurs à leur "maximum" }
    Procedure HyperSat;
    { Mixer l'image avec la couleur "HoverColor" de "pct" pourcent }
    Procedure Mix(aHoverColor: TBZColor; pct: Single);
    { Mixer l'image avec l'inverse de couleur "HoverColor" de "pct" pourcent }
    Procedure MixInv(aHoverColor: TBZColor; pct: Single);
    { Mixer l'image en prenant la couleur moyenne avec "ACOlor" }
    Procedure Average(AColor: TBZColor);
    { Modulation des couleur de l'image avec "AColor" }
    Procedure Modulate(AColor: TBZColor);
    { Teinte l'image avec la couleur "ByAColor" }
    Procedure Colorize(ByAColor: TBZColor);

    { Convertit l'image en niveau de gris, suivant l'algorithme appliqué :@br
      @bold(Note) : @br
      @unorderedlist(
        @item(Le paramètre GrayMatrix : Matrice de conversion, uniquement utilisé avec GrayMode = gcmLuminosity)
        @item(Le paramètre OptVal seulement utilisé :)
        @item(si GrayMode =  gmColorMask alors sa valeur sera comprise entre 0 et 3, ou chaque chiffre représente respectivement le 0 = Canal Rouge, 1 = Vert, 2 = Bleu et 3 = Alpha)
        @item(si GrayMode = gcmCustomShades (réduction de couleurs), alors valeur comprise entre 1 et 255)
        @item(si GrayMode = gcmPowerLaw et gcmLogarithmic)
        @item(Le paramètre GammaFactor est utilisé seulement pour GrayMode = gcmLogarithmic)
      )

      Dans le cas ou GrayMode = gcmLogarithmic : @br
      La paramètre OptVal serre de constante qui est généralement utilisée pour mettre à l'échelle
      la plage de la fonction "log" afin qu'elle corresponde au domaine d'entrée. "Constante = OptVal / log (1 + ValeurCouleur)" . @br
      Pour une image avec des composantes de couleur en byte OptVal sera compris entre [0..255] @br
      Dans le cas de couleur en virgule flottante OptVal sera compris entre [0..1] @br
      Le paramètre OptVal peut également être utilisé pour augmenter davantage le contraste : @br
        - plus la valeur est élevé, plus l'image apparaît lumineuse. }
    Procedure GrayScale(Const GrayMode: TBZGrayConvertMode = gcmLuminosity;Const GrayMatrix: TBZGrayMatrixType=gmtJPEG;  Const OptVal: Single = 0; Const GammaFactor : Single = 1.7);
    { Modifiie la luminosité de l'image suivant le facteur "factor" en nombre réel (0..1.0 = 0%..100%) }
    Procedure AdjustBrightness(factor: Single; Const ApplyOnShadowTone : Boolean = True;  Const ApplyOnMidTone : Boolean = True; Const ApplyOnHighlightTone : Boolean = True);
    { Modifiie la luminosité de l'image suivant le facteur "factor" en nombre réel }
    Procedure AdjustContrast(factor: Single; Const PreserveLuminosity : Boolean = False; Const ApplyOnShadowTone : Boolean = True;  Const ApplyOnMidTone : Boolean = True; Const ApplyOnHighlightTone : Boolean = True);
    { Ajustement automatique du contraste }
    procedure AutoAdjustContrast;

    { Modifiie la saturation des couleurs de l'image suivant le facteur "factor" en nombre réel.@br
                   Si LimitLuminosity = True alors le résultat retourné sera en fonction du niveau de gris }
    Procedure AdjustSaturation(Factor: Single; Const LimitLuminosity: Boolean = False;  Const PreserveLuminosity : Boolean = False; Const ApplyOnShadowTone : Boolean = True;  Const ApplyOnMidTone : Boolean = True; Const ApplyOnHighlightTone : Boolean = True);
    { Correction GAMMA de l'image suivant le facteur "factor" en nombre réel par defaut 2.2}
    Procedure GammaCorrection(Const Factor: Single = 2.2; Const PreserveLuminosity : Boolean = False; Const ApplyOnShadowTone : Boolean = True;  Const ApplyOnMidTone : Boolean = True; Const ApplyOnHighlightTone : Boolean = True);
    { Méthode de postérisation }
    Procedure Posterize(Factor: Single; Const PreserveLuminosity : Boolean = False; Const ApplyOnShadowTone : Boolean = True;  Const ApplyOnMidTone : Boolean = True; Const ApplyOnHighlightTone : Boolean = True);
    { Méthode de solarisation }
    Procedure Solarize(Factor: Single; Const PreserveLuminosity : Boolean = False; Const ApplyOnShadowTone : Boolean = True;  Const ApplyOnMidTone : Boolean = True; Const ApplyOnHighlightTone : Boolean = True);
    { Découpage de la lumière. Amélioration du contraste et de la luminosité }
    Procedure SplitLight(Amount : Integer);


    procedure KeepRed(Factor : Single);
    procedure KeepGreen(Factor : Single);
    procedure KeepBlue(Factor : Single);
    procedure KeepRedInRange(mini,maxi:Byte);
    procedure KeepGreenInRange(mini,maxi:Byte);
    procedure KeepBlueInRange(mini,maxi:Byte);

    procedure FilterOnlyRedInRange(mini,maxi:Byte);
    procedure FilterOnlyGreenInRange(mini,maxi:Byte);
    procedure FilterOnlyBlueInRange(mini,maxi:Byte);

    { Exclusion de la couleur définie }
    procedure ExcludeColor(AColor:TBZColor);
    { Extraction de la couleur définie }
    procedure ExtractColor(AColor:TBZColor);
    { Exclusion des couleurs dans l'interval définis }
    procedure ExcludeColorInRange(AMinColor,AMaxColor:TBZColor);
    { Extraction des couleurs dans l'interval définis }
    procedure ExtractColorInRange(AMinColor,AMaxColor:TBZColor);
    { SetAlpha : Modifie le canal Alpha du bitmap en fonction de "mode" }
    Procedure SetAlpha(Mode: TBZBitmapAlphaSetMode; const aTransparentColor : TBZColor);
    { Conserve les valeurs minimum du bitmap }
    procedure Minimum;
    { Conserve les valeurs maximum du bitmap }
    procedure Maximum;
    { filtre qui 'grise' une image en faisant la moyenne de chaque pixel avec du blanc }
    procedure GrayOut;
    { Ajustement de l'exposition }
    Procedure AdjustExposure(Factor: Single; Const PreserveLuminosity : Boolean = False; Const ApplyOnShadowTone : Boolean = True;  Const ApplyOnMidTone : Boolean = True; Const ApplyOnHighlightTone : Boolean = True);
    { Un filtre qui modifie le gain et la polarisation d'une image - similaire au filtre Contrast }
    procedure AdjustGain(FactorGain, FactorBias : Single; Const PreserveLuminosity : Boolean = False; Const ApplyOnShadowTone : Boolean = True;  Const ApplyOnMidTone : Boolean = True; Const ApplyOnHighlightTone : Boolean = True);
    { Ajustement des valeurs RGB }
    procedure AdjustRGB(FactorRed, FactorGreen, FactorBlue : Single; Const PreserveLuminosity : Boolean = False; Const ApplyOnShadowTone : Boolean = True;  Const ApplyOnMidTone : Boolean = True; Const ApplyOnHighlightTone : Boolean = True);
    { Ajustement des valeurs HSV }
    procedure AdjustHSV(FactorHue, FactorSaturation, FactorValue : Single; Const ApplyOnShadowTone : Boolean = True;  Const ApplyOnMidTone : Boolean = True; Const ApplyOnHighlightTone : Boolean = True);
    //procedure AdjustHSL(FactorHue, FactorSat, FactorLevel : Single);
    //procedure AdjustHSV(FactorHue, FactorSat, FactorValue : Single);

    procedure Boost(Factor : Single);
    //procedure ReplaceColor( ACokor, ByAColor:TBZColor);
    //procedure ReduceColor
    { Supression des yeux rouge }
    procedure RemoveRedEye;

    { Filtre sépia }
    procedure Sepia(Factor : Single);

    { Ajustement de la balance des couleurs }
    procedure ColorBalance(FactorCyanRed, FactorMagentaGreen, FactorYellowBlue : Single; Const PreserveLuminosity : Boolean = False; Const ApplyOnShadowTone : Boolean = True;  Const ApplyOnMidTone : Boolean = True; Const ApplyOnHighlightTone : Boolean = True);

    { Applique une matrice de couleur }
    procedure ApplyColorMatrix(aColorMatrix : TBZColorMatrix);

    { Rotation de la teinte }
    procedure HueRotate(ShiftDeg : Integer);
  End;

  { Regroupe les filtres d'effets spéciaux }

  { TBZBitmapEffectFilters }

  TBZBitmapEffectFilters = Class(TBZCustomOwnerBitmap)
  private
  protected
  public
    //procedure Twist;
    { Agrandissement des pixels }
    procedure Pixelate(xAmount,yAmount:Integer);

    procedure InstagramFilter(aFilter : TBZInstagramFilterType);

    procedure Bloom(RadiusBlur : Single; BlendFactor : Single);
    procedure Boost(Factor : Single);
    procedure Gloss(RadiusBlur : Single; ThresholdFactor : Single; Const BlendFactor : byte = 255);

  end;

  { Regroupe les filtres d'adoucissement ("Blur") et de réduction du bruit d'une image }
  TBZBitmapBlurFilters = Class(TBZCustomOwnerBitmap)
  Private
  Protected
  Public
    { Adoucissement linéaire }
    procedure LinearBlur;
    { Adoucissement rapide }
    Procedure FastBlur;
    { Adoucissement encadré "box" }
    Procedure BoxBlur(Radius:Integer);
    { Adoucissement par découpage }
    Procedure SplitBlur(Amount: Integer);
    { Adoucissement gaussien }
    Procedure GaussianBlur(Const Radius : Single = 3.0; Const Sigma : Single = 1.0); //;theta:single);
    { Adoucissement gaussien par découpage }
    Procedure GaussianSplitBlur(Amount: Integer);
    { Adoucissement gaussien rapide "box" }
    Procedure GaussianBoxBlur(Radius : Single);

    //Procedure MotionBlur(direction, amount: Integer); //

    { Adoucissement de mouvement }
    procedure MotionBlur(Direction : Single; Distance : Integer; ZoomFactor, Rotation : Single );
    { Adoucissement radial }
    procedure RadialBlur(Radius:Integer);
    { Adoucissement circulaire }
    procedure CircularBlur(Radius:Integer);
    { Adoucissement zoom }
    procedure ZoomBlur(cx, cy : Integer; Radius, FactorPrecisionX, FactorPrecisionY :Integer);
    { Adouccisment Radial Zoom @br
      Quality : 1 = Best | 2 = Good | 4 = Medium | 16 = Bad }
    procedure RadialZoomBlur(cx, cy, Force : Integer; Const Quality : Byte = 2);
    { Adoucissement FXAA }
    procedure FXAABlur;
    { Adoucissment adaptatif par seuillage }
    procedure ThresholdBlur(Radius : Single; ThresholdValue : Byte);

    procedure AverageBlur(Radius : Byte);

    //procedure SmartBlur;
    //procedure VariableBlur
    //procedure LensBlur;

  End;

  TBZConvolutionFilterMode = (cfmNormal, cfmDilate, cfmErode, cfmContract, cfmExpand);
  { Regroupe les méthodes pour l'application de filtres de convolution.

    Plus d'informations : @br
    @unorderedlist(
      @item(https://en.wikipedia.org/wiki/Convolution)
      @item(https://en.wikipedia.org/wiki/Mathematical_morphology)
    ) }

  { Regroupe les méthodes de convolution }
  TBZBitmapConvolutionFilters = Class(TBZCustomOwnerBitmap)
  Private
    FCurrentFilter : String;
  Protected

  Public
    Constructor Create(Const AOwner: TBZBaseBitmap); Override;
    //procedure Convolve(afilter: TBZConvolutionFilterType);

    { Filtre de convolution. @br
      La convolution 3x3, 5x5, 7x7..NxN utilise les pixels environnants pour le calcul. }
    Procedure Convolve(Const aMatrix: TBZDynSingleArray; Const Size: Byte; Const Divisor: Single = -1; Const Bias: Single = 0; Const Mode : TBZConvolutionFilterMode = cfmNormal; Const ApplyRed : Boolean = true; Const ApplyGreen : Boolean = true; Const ApplyBlue : Boolean = true); Overload;
    Procedure Convolve(Const aMatrix: TBZDynSingleArray; Const Size: Byte; Const FriendlyName : String=''; Const Divisor: Single = -1; Const Bias: Single = 0; Const Mode : TBZConvolutionFilterMode = cfmNormal; Const ApplyRed : Boolean = true; Const ApplyGreen : Boolean = true; Const ApplyBlue : Boolean = true); Overload;

    { Convolution depuis un filtre prédéfini }
    Procedure Convolve(filter: TBZConvolutionFilter; Const CustomDivisor: Integer = -1; Const CustomBias: Integer = 0; Const Mode : TBZConvolutionFilterMode = cfmNormal; Const ApplyRed : Boolean = true; Const ApplyGreen : Boolean = true; Const ApplyBlue : Boolean = true); Overload;

    { Détection des contours en fonction du mode TBZDetectEdgeFilterMode }
    Procedure DetectEdge(aMode: TBZDetectEdgeFilterMode);

  End;

  { Regroupe les filtres de seuillage }
  TBZBitmapThresholdFilters = Class(TBZCustomOwnerBitmap)
  Private

  Protected

  Public
    Constructor Create(Const AOwner: TBZBaseBitmap); Override;

    { Méthode de seuillage simple }
    procedure ThresholdSimple(Level : Byte; Const MinColor : TBZColor; Const MaxColor : TBZColor;Const AsGray : Boolean = True); overload;
    { Méthode de seuillage double avec possibilité d'interpoler les valeurs}
    procedure ThresholdDual(MinLevel, MaxLevel : Byte;Const MinColor : TBZColor; Const MaxColor : TBZColor;Const AsGray : Boolean = True; Const Interpolated : Boolean = False); overload;
    { Méthode de seuillage par canal de couleur }
    procedure ThresholdSimple(rLevel,gLevel,bLevel : Byte; Const MinColor : TBZColor; Const MaxColor : TBZColor); overload;
    { Méthode de seuillage double par canal de couleur}
    procedure ThresholdDual(rMinLevel,gMinLevel,bMinLevel,rMaxLevel,gMaxLevel,bMaxLevel : Byte; Const MinColor : TBZColor; Const MaxColor : TBZColor; Const Interpolated : Boolean = False); overload;
    { Méthode de seuillage suivant l'algorithme Otsu }
    procedure Otsu(Const MinColor : TBZColor; Const MaxColor : TBZColor; Const AsGray : Boolean = False ; Const Interpolated : Boolean = False);

    { TODO 2 -oBZBitmap -cFiltre : Ajouter d'autres methodes de seuillage cf : TBZBitmapThresholdFilter }

  end;

  //TBZGradientEdgeDetectMode = (gedDefault, gedPrewitt, gedSobel, gedRoberts, gedScharr, gedRobinson, gedMDif, gedLaplace);

  { Type d'adoucissement à appliquer pour la detection de conteur avec Canny }
  TBZBitmapEdgeDetctionBlurMode = (edbmLowPass, edbmSoftLowPass, edbmHardLowPass,
                                  edbmMean3x3, edbmMean5x5,
                                  edbmGaussian, edbmSoftGaussian, edbmHardGaussian,
                                  edbmGaussian5x5, edbmGaussian7x7,
                                  edbmBartlett7x7,
                                  edbmRealGaussian);
  { Type d'operateur morphologique à appliquer pour la detection de contour }
  TBZMorphologicalEdgeDetectionMode =(medErode, medDilate, medOpen, medClose);
  { Classe spécialsisée dans la segmentation d'image (détection des contours) }

  { Regroupe les filtres de segmentation (Détection de contour) }
  TBZBitmapSegmentationFilters = Class(TBZCustomOwnerBitmap)
  Private
    FCurrentFilter : String;
  Protected

  Public
    Constructor Create(Const AOwner: TBZBaseBitmap); Override;

    //function ComputeGradientX
    //function ComputeGradientY
    //function ComputeGradient

    function ComputeGradientX : TBZInteger2DMap;
    function ComputeGradientY : TBZInteger2DMap;

    function ComputeConvolutionGradientX(aMode: TBZDetectEdgeFilterMode) : TBZSingle2DMap;
    function ComputeConvolutionGradientY(aMode: TBZDetectEdgeFilterMode) : TBZSingle2DMap;

    procedure GradientX;
    procedure GradientY;
    procedure Gradient;
    procedure GradientLength;

    procedure GradientConvolution(aMode: TBZDetectEdgeFilterMode);


    procedure GradientDir;

   { Retourne la déviation standard de l'image }
    procedure StandardDeviation(Const StdDevFactor : Single = 2.5; Const BinaryOutput : Boolean = False; Const ShowOnlyROI : Boolean = False);

    { Retourne l'integration de l'image. @br
      @bold(Note) : Paramètres recommandés : @br
      - ScaleFactor : [-10,10] @br
      - absorbtionFactor : [0, 1]
      - Offset : [-200, 200]
    }
    procedure Integration(Const ScaleFactor : Single = 0.5; Const absorbtionFactor : Single = 1.0;Const Offset : Integer = 64);
    // procedure KernelGradient;
    // procedure Convolution

    { Retourne la Transformée en cosinus discrète de type I
      https://fr.wikipedia.org/wiki/Transformée_en_cosinus_discrète}
    procedure DCT(Const ScaleFactor : Single = 2.5);

    { Detection des contours avec la methode de Canny }
    procedure CannyEdgeDetection(const NumStdDev : Single = 1; Const MinThresholdFactor : Single = 0.2;
                    const MinThreshold : Byte = 30; const MaxThresHold : Byte = 60;
                    Const GrayMode : TBZGrayConvertMode=gcmLuminosity;Const GrayMatrix : TBZGrayMatrixType = gmtJPEG;
                    Const BlurMode : TBZBitmapEdgeDetctionBlurMode = edbmGaussian5x5;
                    Const GaussianRadius : Single = 5.0; Const GaussianSigma : Single = 2.0;
                    Const GradientKernel : TBZDetectEdgeFilterMode = defSobel);

    { Detection des contours avec la methode de Marr–Hildreth. @br
      @bold(Note) : Plus la taille du filtre est grande et plus le poid est faible, plus il y a de contours }
    procedure MarrHildrethEdgeDetection(Const GrayMode : TBZGrayConvertMode=gcmLuminosity;Const GrayMatrix : TBZGrayMatrixType = gmtJPEG; Const KernelSize : Integer = 7; Const Weight : Single = 1.5);


    { Detection des contour par seuil, variance locale et fonction binaire
      - https://www.academia.edu/17699371/Local_threshold_and_Boolean_function_based_edge_detection @br
      - https://pdfs.semanticscholar.org/5aa0/35d4dd5823a2e0c12dfddae875eb4cccd8eb.pdf}
    procedure BooleanEdgeDetection(LocalConst, GlobalThreshold : Byte);


    //procedure Dog(Bias : Single; Invert : Boolean);
    //procedure DogWeighted(FilterSize : Byte; Weight1, Weight2, Bias : Single; Invert : Boolean);
    //procedure StandarddDeviationEdgeDetection(FilterSize : Byte; VarianceFactor : Single);
    //procedure SharpenEdgeDetection(FilterSize : Byte; FilterType : TBZSharpenFilterType; Threshold : Byte; GrayScale, BinaryOut : Boolean);

    { Detection des contours par morphologie. @br
      @bold(Note) : Pour obtenir une accentuation des contours, il suffit d'appliquer par dessus un filtre arithmétique d'addition de l'image originale. @br
      @bold(Attention) : @br
      L'image retournée est en couleur. Pour obtenir une image binaire, il faudra convertir la source ou l'image retournée en niveau de gris,
      puis appliquer un filtre de seuillage }
    procedure MorphologicalEdgeDetection(FilterSize : Byte; FilterType : TBZMorphologicalEdgeDetectionMode);

    { Detection des contours par la méthode "MinMax" }
    procedure MinMaxEdgeDetection(MinThresHold, MaxThreshold, GlobalThreshold : Byte);

    //procedure BoundaryEdgeDetection(FilterSize : Byte; StructuralElements : Array of Boolean; Threshold : Byte);

    // procedure KangWangEdgeDetection
    // procedure DericheEdgeDetection
    // procedure FreiShenEdgeDetection
    // procedure ShenCastanEdgeDetection;
    // procedure HoughLineEdgeDetection
    // procedure HoughCircleEdgeDetection

  end;

  { Regroupe les filtres de rendu de texture }
  TBZBitmapRenderFilters = Class(TBZCustomOwnerBitmap)
  Private
  Protected
  Public
    { Rendu d'un damier }
    Procedure DrawChecker(Const X, Y, W, H: Integer; Const C1, C2: TBZColor; Const Factor: Byte = 8);

    procedure DrawGrid(Const BackgroundColor, MainLineColor, SecondLineColor, AxisColorX, AxisColorY : TBZColor; Const CellSize : Byte = 10);
    { Rendu d'une "tilemap" }
    procedure TileTexture(Const TileWidth, TileHeight : Integer; Const ATexture : TBZBitmap;const ADrawMode: TBZBitmapDrawMode=dmSet;Const aAlphaMode :TBZBitmapAlphaMode=amOpaque);
  End;

  { Regroupe les filtres de déformations }
  TBZBitmapDeformationFilters = Class(TBZCustomOwnerBitmap)
  private
  protected
  public
    Constructor Create(Const AOwner: TBZBaseBitmap); Override;
    //procedure Twist;
    { Agrandissement des pixels }
    procedure PinchAndTwirl(ACenterX, ACenterY, ARadius, AnAngle : Integer; AnAmount  : Single; Const EdgeAction : TBZPixelEdgeAction = peaClamp; Const SamplerMode : TBZGetPixelSampleMethod = psmMean; Const SampleLevel : Byte = 2);
    procedure Twirl(ACenterX, ACenterY, ARadius, AnAngle : Integer; Const EdgeAction : TBZPixelEdgeAction = peaClamp; Const SamplerMode : TBZGetPixelSampleMethod = psmMean);
    procedure FishEye(ACenterX, ACenterY, ARadius, AnAngle : Integer; Const EdgeAction : TBZPixelEdgeAction = peaClamp; Const SamplerMode : TBZGetPixelSampleMethod = psmMean);
    procedure WaveDistorsion(aMode : TBZWaveDistorsionMode; aWaveLengthX, AmpX, aWaveLengthY, AmpY : Single;  Const EdgeAction : TBZPixelEdgeAction = peaClamp; Const SamplerMode : TBZGetPixelSampleMethod = psmMean; Const SampleLevel : Byte = 2);
    procedure WaterRipple(ACenterX, ACenterY, ARadius : Integer; APeriod, AAmplitude, APhase : Single; Const EdgeAction : TBZPixelEdgeAction = peaClamp; Const SamplerMode : TBZGetPixelSampleMethod = psmMean; Const SampleLevel : Byte = 2);
    procedure Diffusion(aHorizontal, aVertical, aMinDistance, aMaxDistance : Single; Const EdgeAction : TBZPixelEdgeAction = peaClamp; Const SamplerMode : TBZGetPixelSampleMethod = psmMean; Const SampleLevel : Byte = 2);
    procedure Polar(aMode : TBZPolarTransformationMode; ACenterX, ACenterY : Integer; Const EdgeAction : TBZPixelEdgeAction = peaClamp; Const SamplerMode : TBZGetPixelSampleMethod = psmMean; Const SampleLevel : Byte = 2);
  end;

  { Classe spécialisée dans l'application des filtres morphologique }
  TBZBitmapMorphologicalFilters = Class(TBZCustomOwnerBitmap)
  Private
  Protected
  Public
    procedure Erode(FilterSize : Byte);
    procedure Dilate(FilterSize : Byte);
    procedure Open(FilterSize : Byte);
    procedure Close(FilterSize : Byte);
  end;

  { Classe d'aide surtout utile pour l'integration d'un bitmap dans un composant (non) visuel.@br
    Elle permet également de définir le format les options adequates pour la sauvegarde d'une image. }

  { TBZPicture }

  TBZPicture = Class(TBZPersistentObject, IBZNotifyAble)
  Private
    FBitmap: TBZBitmap;
    FOnChange: TNotifyEvent;

    Procedure SetBitmap(Const aBitmap: TBZBitmap);
  Protected
     procedure DefineProperties(Filer: TFiler); override;
     procedure ReadData(Stream: TStream);
     procedure WriteData(Stream: TStream);
  Public
    Constructor Create; Override;
    Destructor Destroy; Override;

    procedure WriteToFiler(writer: TVirtualWriter); override;
    procedure ReadFromFiler(reader: TVirtualReader); override;

    { Notifie un changement dans l'objet. L'énènement se propagera dans les objets propriétaires (Owner) de celui-ci }
    Procedure NotifyChange(Sender: TObject); Virtual;

    { Charge un fichier image }
    Procedure LoadFromFile(Const aFileName: String); Overload;
    { Enregistre un fichier image }
    Procedure SaveToFile(Const aFileName: String); Overload;//aFormat : TBZImageFileFormats; Options : Pointer);
  Public
    { Bitmap }
    Property Bitmap: TBZBitmap read FBitmap write SetBitmap;
    { Evènement levé lors d'une changement d'état de l'objet }
    Property OnChange: TNotifyEvent read FOnChange write FOnChange;
  End;


{ Calcul une matrice de convolution gaussienne }
function ComputeGaussianKernel(KernelSize : Integer; Weight : Single) : TBZDynSingleArray;

{ Calcul une matrice de convolution laplacienne de gausse }
function ComputeLOGKernel(KernelSize : Integer; Weight : Single) : TBZDynSingleArray;

Implementation

Uses
  // pour les définitions de types et constantes surtout
  math, Lazutf8,{ fpImage, fpCanvas, }
  // Complements
  BZSystem,
  BZUtils,
  //BZVectorMathEx,
  BZVectorMathUtils,
  BZGeoTools,
  BZTypesHelpers,
  //Debug
  {.$IFDEF DEBUG}
  Dialogs, BZLogger
  {.$ENDIF};

{ TBZBitmapThresholdFilters }

constructor TBZBitmapThresholdFilters.Create(const AOwner : TBZBaseBitmap);
begin
  inherited Create(AOwner);
end;

procedure TBZBitmapThresholdFilters.ThresholdSimple(Level : Byte; const MinColor : TBZColor; const MaxColor : TBZColor; const AsGray : Boolean);
Var
  Filter : TBZBitmapThresholdBaseFilter;
Begin
  Filter := TBZBitmapThresholdBaseFilter.Create(OwnerBitmap);
  Filter.OnProgress := Self.OnProgress;
  Filter.Multi := False;
  Filter.AsGray := AsGray;
  Filter.Interpolate := False; //Interpolated;
  Filter.MinThreshold := Level;
  Filter.MaxThreshold := Level;
  Filter.MinColor := MinColor;
  Filter.MaxColor := MaxColor;
  Filter.Render;
  FreeAndNil(Filter);
end;

procedure TBZBitmapThresholdFilters.ThresholdDual(MinLevel, MaxLevel : Byte; const MinColor : TBZColor; const MaxColor : TBZColor; const AsGray : Boolean; const Interpolated : Boolean);
Var
  Filter : TBZBitmapThresholdBaseFilter;
Begin
  Filter := TBZBitmapThresholdBaseFilter.Create(OwnerBitmap);
  Filter.OnProgress := Self.OnProgress;
  Filter.Multi := False;
  Filter.AsGray := AsGray;
  Filter.Interpolate := Interpolated;
  Filter.MinThreshold := MinLevel;
  Filter.MaxThreshold := MaxLevel;
  Filter.MinColor := MinColor;
  Filter.MaxColor := MaxColor;
  Filter.Render;
  FreeAndNil(Filter);
end;

procedure TBZBitmapThresholdFilters.ThresholdSimple(rLevel, gLevel, bLevel : Byte; const MinColor : TBZColor; const MaxColor : TBZColor);
Var
  Filter : TBZBitmapThresholdBaseFilter;
Begin
  Filter := TBZBitmapThresholdBaseFilter.Create(OwnerBitmap);
  Filter.OnProgress := Self.OnProgress;
  Filter.Multi := True;
  Filter.Interpolate := False;
  Filter.AsGray := False;
  Filter.RedMinThreshold := rLevel;
  Filter.RedMaxThreshold := rLevel;
  Filter.GreenMinThreshold := gLevel;
  Filter.GreenMaxThreshold := gLevel;
  Filter.BlueMinThreshold := bLevel;
  Filter.BlueMaxThreshold := bLevel;
  Filter.MinColor := MinColor;
  Filter.MaxColor := MaxColor;
  Filter.Render;
  FreeAndNil(Filter);
end;

procedure TBZBitmapThresholdFilters.ThresholdDual(rMinLevel, gMinLevel, bMinLevel, rMaxLevel, gMaxLevel, bMaxLevel : Byte; const MinColor : TBZColor; const MaxColor : TBZColor; const Interpolated : Boolean);
Var
  Filter : TBZBitmapThresholdBaseFilter;
Begin
  Filter := TBZBitmapThresholdBaseFilter.Create(OwnerBitmap);
  Filter.OnProgress := Self.OnProgress;
  Filter.Multi := True;
  Filter.Interpolate := Interpolated;
  Filter.AsGray := False;
  Filter.RedMinThreshold := rMinLevel;
  Filter.RedMaxThreshold := rMaxLevel;
  Filter.GreenMinThreshold := gMinLevel;
  Filter.GreenMaxThreshold := gMaxLevel;
  Filter.BlueMinThreshold := bMinLevel;
  Filter.BlueMaxThreshold := bMaxLevel;
  Filter.MinColor := MinColor;
  Filter.MaxColor := MaxColor;
  Filter.Render;
  FreeAndNil(Filter);
end;

procedure TBZBitmapThresholdFilters.Otsu(const MinColor : TBZColor; const MaxColor : TBZColor; const AsGray : Boolean; const Interpolated : Boolean);
Var
  Filter : TBZBitmapThresholdAutomaticFilter;
Begin
  Filter := TBZBitmapThresholdAutomaticFilter.Create(OwnerBitmap);
  Filter.OnProgress := Self.OnProgress;
  Filter.AsGray := AsGray;
  Filter.Interpolate := Interpolated;
  Filter.AutomaticThresholdMode := atmOtsu;
  Filter.MinColor := MinColor;
  Filter.MaxColor := MaxColor;
  Filter.Render;
  FreeAndNil(Filter);
end;

{ TBZBitmapMorphologicalFilters }

procedure TBZBitmapMorphologicalFilters.Erode(FilterSize : Byte);
Var
  Filter : TBZBitmapMorphologicalFilter;
begin
  Filter := TBZBitmapMorphologicalFilter.Create(OwnerBitmap);
  Filter.OnProgress := Self.OnProgress;
  Filter.FilterSize := FilterSize;
  Filter.PixelEdgeAction := peaClamp;
  Filter.OperatorType := moErode;
  Filter.Render;
  FreeAndNil(Filter);
end;

procedure TBZBitmapMorphologicalFilters.Dilate(FilterSize : Byte);
Var
  Filter : TBZBitmapMorphologicalFilter;
begin
  Filter := TBZBitmapMorphologicalFilter.Create(OwnerBitmap);
  Filter.OnProgress := Self.OnProgress;
  Filter.FilterSize := FilterSize;
  Filter.PixelEdgeAction := peaClamp;
  Filter.OperatorType := moDilate;
  Filter.Render;
  FreeAndNil(Filter);
end;

procedure TBZBitmapMorphologicalFilters.Open(FilterSize : Byte);
//Var
//  Filter : TBZBitmapMorphologicalFilter;
//begin
//  Filter := TBZBitmapMorphologicalFilter.Create(OwnerBitmap);
//  Filter.OnProgress := Self.OnProgress;
//  Filter.FilterSize := FilterSize;
//  Filter.PixelEdgeAction := peaClamp;
//  Filter.OperatorType := moErode;
//  Filter.Render;
//
//  Filter.OperatorType := moDilate;
//  Filter.Render;
//
//
//  FreeAndNil(Filter);
begin
  Self.Erode(FilterSize);
  Self.Dilate(FilterSize);
end;

procedure TBZBitmapMorphologicalFilters.Close(FilterSize : Byte);
//Var
//  Filter : TBZBitmapMorphologicalFilter;
//begin
//  Filter := TBZBitmapMorphologicalFilter.Create(OwnerBitmap);
//  Filter.OnProgress := Self.OnProgress;
//  Filter.FilterSize := FilterSize;
//  Filter.PixelEdgeAction := peaClamp;
//  Filter.OperatorType := moDilate;
//  Filter.Render;
//  Filter.OperatorType := moErode;
//  Filter.Render;
//  FreeAndNil(Filter);
//end;
begin
  Self.Dilate(FilterSize);
  Self.Erode(FilterSize);
end;

{ TBZBitmapEdgeDetctionFilters }

constructor TBZBitmapSegmentationFilters.Create(const AOwner : TBZBaseBitmap);
begin
  inherited Create(AOwner);
  FCurrentFilter := '';
end;

function TBZBitmapSegmentationFilters.ComputeGradientX : TBZInteger2DMap;
Var
  grad, x, y : Integer;
begin
  Result := TBZInteger2DMap.Create(OwnerBitmap.Width, OwnerBitmap.Height);
  for y := 1 to OwnerBitmap.MaxHeight - 1 do
  begin
    for x := 1 to OwnerBitmap.MaxWidth - 1 do
    begin
      grad := abs(OwnerBitmap.Getpixel(x, y + 1).Red - OwnerBitmap.Getpixel(x, y - 1).Red);
      Result.Add(Grad);
    end;
  end;
end;

function TBZBitmapSegmentationFilters.ComputeGradientY : TBZInteger2DMap;
Var
  grad, x, y : Integer;
begin
  Result := TBZInteger2DMap.Create(OwnerBitmap.Width, OwnerBitmap.Height);
  for y := 1 to OwnerBitmap.MaxHeight - 1 do
  begin
    for x := 1 to OwnerBitmap.MaxWidth - 1 do
    begin
      grad := abs(OwnerBitmap.Getpixel(x + 1, y).Red - OwnerBitmap.Getpixel(x - 1, y).Red);
      Result.Add(Grad);
    end;
  end;
end;

function TBZBitmapSegmentationFilters.ComputeConvolutionGradientX(aMode : TBZDetectEdgeFilterMode) : TBZSingle2DMap;//TBZSingle2DMap;
Var
  size, moffset, idx,  x, y, i, j, pX, pY : Integer;
  KS, KSDiv2: Byte;
  KernelValue: Single;
  Sum, Value : Single;
  //Sum, Value : Integer;
  //vC : Byte;
  aMatrix : Array of Single;
begin
  Case aMode of
    defPrewitt: idx := 18;
    defSobel: idx := 22;
    defRoberts: idx := 20;
    defKirsch: idx := 24;
    defScharr: idx := 26;
    defRobinson: idx := 28;
    defMDif: idx := 30;
    defLaplace: idx := 39;
  end;

  Size := BZConvolutionFilterPresets[idx].MatrixSize;
  Case size of
    3: aMatrix := BZConvolutionFilterPresets[idx].Matrix._3;
    5: aMatrix := BZConvolutionFilterPresets[idx].Matrix._5;
    7: aMatrix := BZConvolutionFilterPresets[idx].Matrix._7;
  end;

  KS := Size - 1;
  KSDiv2 := KS Div 2;

  Result := TBZSingle2DMap.Create(OwnerBitmap.Width, OwnerBitmap.Height);
  //Result := TBZInteger2DMap.Create(OwnerBitmap.Width, OwnerBitmap.Height);
  For Y := 0 To OwnerBitmap.MaxHeight Do
  Begin
    For X := 0 To OwnerBitmap.MaxWidth Do
    Begin
      Sum := 0;
      For J := 0 To KS Do
      Begin
        pY := Clamp(Y + J - KSDiv2, 0, OwnerBitmap.MaxHeight);
        moffset := J * Size;
        For I := 0 To KS Do
        Begin
          pX := Clamp(X + I - KSDiv2, 0, OwnerBitmap.MaxWidth);
          KernelValue := aMatrix[moffset + I]; // Valeur dans la matrice
          Value := OwnerBitmap.GetPixel(pX, pY).Red  * _FloatColorRatio;
          //Value := Round(Value * KernelValue);
          Value := (Value * KernelValue);
          Sum := Sum + Value;
        end;
      end;
      Result.Add(Sum);
      //Sum:= Clamp(Sum, 0.0, 1.0);
      //Sum := Sum * 255;
      //vC := Round(Sum);
      //Result.Add(vC);
    end;
  end;
end;

function TBZBitmapSegmentationFilters.ComputeConvolutionGradientY(aMode : TBZDetectEdgeFilterMode) : TBZSingle2DMap;
Var
  size, moffset, idx,  x, y, i, j, pX, pY : Integer;
  KS, KSDiv2: Byte;
  KernelValue : Single;
  Sum, Value : Single;
  aMatrix : Array of Single;
begin
  Case aMode of
    defPrewitt: idx := 17;
    defSobel: idx := 21;
    defRoberts: idx := 19;
    defKirsch: idx := 23;
    defScharr: idx := 25;
    defRobinson: idx := 27;
    defMDif: idx := 29;
    defLaplace: idx := 38;
  end;

  Size := BZConvolutionFilterPresets[idx].MatrixSize;
  Case size of
    3: aMatrix := BZConvolutionFilterPresets[idx].Matrix._3;
    5: aMatrix := BZConvolutionFilterPresets[idx].Matrix._5;
    7: aMatrix := BZConvolutionFilterPresets[idx].Matrix._7;
  end;

  KS := Size - 1;
  KSDiv2 := KS Div 2;

  Result := TBZSingle2DMap.Create(OwnerBitmap.Width, OwnerBitmap.Height);
  For Y := 0 To OwnerBitmap.MaxHeight Do
  Begin
    For X := 0 To OwnerBitmap.MaxWidth Do
    Begin
      Sum := 0;
      For J := 0 To KS Do
      Begin
        pY := Clamp(Y + J - KSDiv2, 0, OwnerBitmap.MaxHeight);
        moffset := J * Size;
        For I := 0 To KS Do
        Begin
          pX := Clamp(X + I - KSDiv2, 0, OwnerBitmap.MaxWidth);
          KernelValue := aMatrix[moffset + I]; // Valeur dans la matrice
          Value := OwnerBitmap.GetPixel(pX, pY).Red * _FloatColorRatio;
          Value := (Value * KernelValue);
          Sum := Sum + Value;
        end;
      end;
      Result.Add(Sum);
    end;
  end;
end;

procedure TBZBitmapSegmentationFilters.GradientX;
Var
  tmpBmp : TBZCustomBitmap;
  grad, x, y : Integer;
  PixPtr : PBZColor;
begin
  tmpBmp := TBZCustomBitmap.Create(OwnerBitmap.Width, OwnerBitmap.Height);
  tmpBmp.Clear(clrTransparent);
  PixPtr := tmpBmp.GetScanLine(0);
  for y := 0 to OwnerBitmap.MaxHeight do
  begin
    for x := 0 to OwnerBitmap.MaxWidth do
    begin
      grad := abs(OwnerBitmap.Getpixel(x, y + 1).Red - OwnerBitmap.Getpixel(x, y - 1).Red);
      PixPtr^ := BZColor(Grad, grad, grad);
      Inc(PixPtr);
    end;
  end;
  OwnerBitmap.FastCopy(tmpBmp);
  FreeAndNil(tmpBmp);
end;

procedure TBZBitmapSegmentationFilters.GradientY;
Var
  tmpBmp : TBZCustomBitmap;
  grad, x, y : Integer;
  PixPtr : PBZColor;
begin
  tmpBmp := TBZCustomBitmap.Create(OwnerBitmap.Width, OwnerBitmap.Height);
  tmpBmp.Clear(clrTransparent);
  PixPtr := tmpBmp.GetScanLine(0);
  for y := 0 to OwnerBitmap.MaxHeight do
  begin
    for x := 0 to OwnerBitmap.MaxWidth do
    begin
      grad := abs(OwnerBitmap.Getpixel(x + 1, y).Red - OwnerBitmap.Getpixel(x - 1, y).Red);
      PixPtr^ := BZColor(Grad, grad, grad);
      Inc(PixPtr);
    end;
  end;
  OwnerBitmap.FastCopy(tmpBmp);
  FreeAndNil(tmpBmp);
end;

procedure TBZBitmapSegmentationFilters.Gradient;
Var
  tmpBmp : TBZCustomBitmap;
  gradx, grady, x, y  : Integer;
  PixPtr : PBZColor;
begin
  tmpBmp := TBZCustomBitmap.Create(OwnerBitmap.Width, OwnerBitmap.Height);
  tmpBmp.Clear(clrTransparent);
  PixPtr := tmpBmp.GetScanLine(1);
  for y := 1 to OwnerBitmap.MaxHeight - 1 do
  begin
    for x := 0 to OwnerBitmap.MaxWidth do
    begin
      gradX := abs(OwnerBitmap.Getpixel(x, y + 1).Red - OwnerBitmap.Getpixel(x, y - 1).Red);
      gradY := abs(OwnerBitmap.Getpixel(x + 1, y).Red - OwnerBitmap.Getpixel(x - 1, y).Red);
      PixPtr^ := BZColor(GradX, gradX, gradX);
      PixPtr^ := BZColor(GradY, gradY, gradY);
      Inc(PixPtr);
    end;
  end;
  OwnerBitmap.FastCopy(tmpBmp);
  FreeAndNil(tmpBmp);
end;

procedure TBZBitmapSegmentationFilters.GradientLength;
Var
  tmpBmp : TBZCustomBitmap;
  gradx, grady, mag, x, y : Integer;
  PixPtr : PBZColor;
begin
  tmpBmp := TBZCustomBitmap.Create(OwnerBitmap.Width, OwnerBitmap.Height);
  tmpBmp.Clear(clrTransparent);
  PixPtr := tmpBmp.GetScanLine(0);
  for y := 0 to OwnerBitmap.MaxHeight  do
  begin
    for x := 0 to OwnerBitmap.MaxWidth do
    begin
      gradX := abs(OwnerBitmap.Getpixel(x, y + 1).Red - OwnerBitmap.Getpixel(x, y - 1).Red);
      gradY := abs(OwnerBitmap.Getpixel(x + 1, y).Red - OwnerBitmap.Getpixel(x - 1, y).Red);
      mag := Round(Clamp(System.Sqrt(gradX * gradx + gradY * gradY),0,255));
      PixPtr^ := BZColor(mag, mag, mag);
      Inc(PixPtr);
    end;
  end;
  OwnerBitmap.FastCopy(tmpBmp);
  FreeAndNil(tmpBmp);
end;

procedure TBZBitmapSegmentationFilters.GradientConvolution(aMode : TBZDetectEdgeFilterMode);
Var
  x, y : Integer;
  GradX, GradY : TBZSingle2DMap;
  gX, gY : Single;
  c  : Integer;
begin
  GradX := Self.ComputeConvolutionGradientX(aMode);
  GradY := Self.ComputeConvolutionGradientX(aMode);
  For y := 0 to OwnerBitmap.MaxHeight do
  begin
    for x := 0 to OwnerBitmap.MaxWidth do
    begin
      gX := GradX.Items[x,y];
      gY := GradY.Items[x,y];
      c := Round(Clamp(System.Sqrt(gX * gX + gY * gY),0,1.0) * 255);
      OwnerBitmap.setPixel(x,y, BZColor(c,c,c));
    end;
  end;
end;

procedure TBZBitmapSegmentationFilters.GradientDir;
Var
  tmpBmp : TBZCustomBitmap;
  a, gradx, grady,  x, y : Integer;
  Angle : Single;
  PixPtr : PBZColor;
begin
  tmpBmp := TBZCustomBitmap.Create(OwnerBitmap.Width, OwnerBitmap.Height);
  tmpBmp.Clear(clrTransparent);
  PixPtr := tmpBmp.GetScanLine(0);
  for y := 0 to OwnerBitmap.MaxHeight do
  begin
    for x := 0 to OwnerBitmap.MaxWidth do
    begin
      gradX := abs(OwnerBitmap.Getpixel(x, y + 1).Red - OwnerBitmap.Getpixel(x, y - 1).Red);
      gradY := abs(OwnerBitmap.Getpixel(x + 1, y).Red - OwnerBitmap.Getpixel(x - 1, y).Red);
      //mag := Round(Clamp(System.Sqrt(gradX * gradx + gradY * gradY),0,255));
      angle:= RadianToDeg(Math.ArcTan2(GradY, GradX));
      if (angle < 0) then Angle := Angle + 360;
      A := 2 * Round((angle +  202.5) / 45);

      PixPtr^ := BZColor(a, a, a);
      Inc(PixPtr);
    end;
  end;
  OwnerBitmap.FastCopy(tmpBmp);
  FreeAndNil(tmpBmp);

end;

procedure TBZBitmapSegmentationFilters.StandardDeviation(
  const StdDevFactor : Single; const BinaryOutput : Boolean; const ShowOnlyROI : Boolean);
Var
  IntensityAverage, Intensity: Byte;
  Delta : Single;
  i, vMin, vMax : Integer;
  PixPtr : PBZColor;
  InColor : TBZColor;
begin
  IntensityAverage := OwnerBitmap.getIntensityAverage;
  Delta := System.Sqrt(OwnerBitmap.getVariance(IntensityAverage));
  vMin:=Round(IntensityAverage - StdDevFactor * Delta);
  vMax:=Round(IntensityAverage + StdDevFactor * Delta);

  if ShowOnlyROI then
  begin
    i := 0;
    PixPtr := OwnerBitmap.GetScanLine(0);
    while i <= OwnerBitmap.MaxSize do
    begin
      InColor := PixPtr^;
      Intensity := InColor.Luminance;
      if (Intensity<vMin) or (Intensity>vMax) then
      begin
        if BinaryOutPut then PixPtr^:= clrWhite else PixPtr^.Create(Intensity,Intensity,Intensity, InColor.Alpha);
      end
      else PixPtr^:=clrTransparent;
      inc(i);
      inc(PixPtr);
    end;
  end
  else
  begin
    i := 0;
    PixPtr := OwnerBitmap.GetScanLine(0);
    while i <= OwnerBitmap.MaxSize do
    begin
      InColor := PixPtr^;
      Intensity := InColor.Luminance;
      if (Intensity<vMin) or (Intensity>vMax) then
      begin
        if BinaryOutPut then PixPtr^:= clrWhite else PixPtr^.Create(Intensity,Intensity,Intensity, InColor.Alpha);
      end;
      inc(i);
      inc(PixPtr);
    end;
  end;
end;

procedure TBZBitmapSegmentationFilters.Integration(const ScaleFactor : Single; const absorbtionFactor : Single; const Offset : Integer);
Var
  x,y : Integer;
  OutGray : Byte;
  Sum : Single ;
  Level : Single;
  PixPtr : PBZColor;
  inColor : TBZColor;
begin
  Sum := 0;
  PixPtr := OwnerBitmap.getSurfaceBuffer;
  For y := 0 to OwnerBitmap.MaxHeight do
  begin
    Sum := 0;
    For x := 0 to OwnerBitmap.MaxWidth do
    begin
      inColor := PixPtr^;
      Level := inColor.Luminance;
      sum := (sum + (Level - offset) * scaleFactor) * AbsorbtionFactor ;
      OutGray := ClampByte(Trunc(sum / 10) + 127);
      PixPtr^:= BZColor(OutGray, OutGray, OutGray, inColor.Alpha);
      inc(PixPtr);
    end;
  end;
end;

procedure TBZBitmapSegmentationFilters.DCT(const ScaleFactor : Single);
Var
  x, y, i, j : Integer;
  Sum : Single ;
  f : Single;
  inColor : TBZColor;
  outGray : Byte;
  TmpBmp : TBZBitmap;

  function ComputeFrequence(r,g,b : Byte) : Integer;
  begin
    Result := Round((r * 780.0 / 768.0) + (g * 570.0 / 768.0) + (b * 400.0 /768.0))
  end;

begin
  TmpBmp := TBZBitmap.Create(OwnerBitmap.Width, OwnerBitmap.Height);
  TmpBmp.Clear(clrTransparent);
  //PixPtr := OwnerBitmap.getSurfaceBuffer;
  For y := 0 to OwnerBitmap.MaxHeight do
  begin
    For x := 0 to OwnerBitmap.MaxWidth do
    begin
      Sum := 0;
      for j := -1 to 1 do
      begin
        for i := -1 to 1 do
        begin
          inColor := OwnerBitmap.getPixel(x + i, y + j, peaClamp);
          f := ComputeFrequence(inColor.Red, inColor.Green, inColor.Blue);
          sum := Sum + System.Exp(-1.0/f)*(System.sin(c2PI*f)+cos(c2PI*f))/(c2PI*f);
        end;
      end;
      Sum := Sum * ScaleFactor;
      Sum := Sum * 255;
      outGray := ClampByte(Round(Sum));
      TmpBmp.SetPixel(x,y,BZColor(OutGray, OutGray, OutGray, inColor.Alpha));
    end;
  end;
  OwnerBitmap.FastCopy(TmpBmp);
  FreeAndNil(TmpBmp);
end;

procedure TBZBitmapSegmentationFilters.CannyEdgeDetection(const NumStdDev : Single; const MinThresholdFactor : Single; const MinThreshold : Byte; const MaxThresHold : Byte; const GrayMode : TBZGrayConvertMode; const GrayMatrix : TBZGrayMatrixType; const BlurMode : TBZBitmapEdgeDetctionBlurMode; const GaussianRadius : Single; const GaussianSigma : Single; const GradientKernel : TBZDetectEdgeFilterMode);
Var
  x, y, Total : Integer;
  i, j, Dir : Integer;
  GradX, GradY, GradientMap, CleanGradientMap : TBZSingle2DMap;
  DirMap : TBZInteger2DMap;
  TmpBmp : TBZBitmap;
  Angle, diff, sum, xx, yy,
  Mag, Variance, MagMean, StdDev,
  tHi, tLo: Single;
  Connected : Boolean;

begin
  TmpBmp := TBZBitmap.Create;
  TmpBmp.Assign(OwnerBitmap);

  // Etape 1 : Conversion en niveau de gris
  TmpBmp.ColorFilter.GrayScale(GrayMode, GrayMatrix);

  // Etape 2 : Adoucissement gaussien
  Case BlurMode of
    edbmLowPass: TmpBmp.ConvolutionFilter.Convolve(BZConvolutionFilterPresets[10]);
    edbmSoftLowPass: TmpBmp.ConvolutionFilter.Convolve(BZConvolutionFilterPresets[11]);
    edbmHardLowPass: TmpBmp.ConvolutionFilter.Convolve(BZConvolutionFilterPresets[12]);
    edbmMean3x3: TmpBmp.ConvolutionFilter.Convolve(BZConvolutionFilterPresets[1]);
    edbmMean5x5: TmpBmp.ConvolutionFilter.Convolve(BZConvolutionFilterPresets[2]);
    edbmGaussian: TmpBmp.ConvolutionFilter.Convolve(BZConvolutionFilterPresets[3]);
    edbmSoftGaussian: TmpBmp.ConvolutionFilter.Convolve(BZConvolutionFilterPresets[4]);
    edbmHardGaussian: TmpBmp.ConvolutionFilter.Convolve(BZConvolutionFilterPresets[5]);
    edbmGaussian5x5: TmpBmp.ConvolutionFilter.Convolve(BZConvolutionFilterPresets[6]);
    edbmGaussian7x7: TmpBmp.ConvolutionFilter.Convolve(BZConvolutionFilterPresets[7]);
    edbmBartlett7x7: TmpBmp.ConvolutionFilter.Convolve(BZConvolutionFilterPresets[8]);
    edbmRealGaussian :  TmpBmp.BlurFilter.GaussianBlur(GaussianRadius, GaussianSigma);
  end;

  // Etape 3 : Calcul des gradients et directions
  GradX := TmpBmp.SegmentationFilter.ComputeConvolutionGradientX(GradientKernel);
  GradY := TmpBmp.SegmentationFilter.ComputeConvolutionGradientY(GradientKernel);

  GradientMap := TBZSingle2DMap.Create(TmpBmp.Width, TmpBmp.Height);
  Total := TmpBmp.Width * TmpBmp.Height;
  Sum := 0;

  for y := 0 to TmpBmp.MaxHeight do
  begin
    for x := 0 to TmpBmp.MaxWidth do
    begin
      xx := GradX.Items[x,y];
      yy := GradY.Items[x,y];
      Mag := System.Sqrt(xx * xx + yy * yy);
      GradientMap.Add(Mag);
      Sum := Sum + Mag;
    end;
  end;

  if (NumStdDev > 0) and (MinThresholdFactor > 0) then
  begin
    MagMean := Sum / Total;

    Variance := 0;
    for y := 0 to TmpBmp.MaxHeight do
    begin
      for x := 0 to TmpBmp.MaxWidth do
      begin
        Diff := GradientMap.Items[x,y] - MagMean;
        Variance := Variance + (Diff * Diff);
      end;
    end;

    StdDev := System.Sqrt(Variance / Total);
  end;

  // Etape 4 : Calcul des directions
  DirMap := TBZInteger2DMap.Create(TmpBmp.Width, TmpBmp.Height);

  for y := 0 to TmpBmp.MaxHeight do
  begin
    for x := 0 to TmpBmp.MaxWidth do
    begin
       Angle := RadianToDeg(Math.ArcTan2(GradY.Items[x,y], GradX.Items[x,y]));

       if Angle < 0 then Angle := Angle + 180;

       if (angle < 22.5) or ((angle >= 157.5) and (angle <= 180)) then Dir := 0
       else if ((angle >= 22.5) and (angle < 67.5)) then Dir := 45
       else if ((angle >= 67.5) and (angle < 112.5)) then Dir := 90
       else  if ((angle >= 112.5) and (angle < 157.5)) then Dir := 135;

      DirMap.Add(Dir);
    end;
  end;

  // Etape 5 : Suppression des non maxima
  CleanGradientMap := TBZSingle2DMap.Create(TmpBmp.Width, TmpBmp.Height);

  for y := 0 to TmpBmp.MaxHeight do
  begin
    for x := 0 to TmpBmp.MaxWidth do
    begin
      CleanGradientMap.Add(GradientMap.Items[x,y]);
    end
  end;

  for y := 1 to TmpBmp.MaxHeight-1 do
  begin
    for x := 1 to TmpBmp.MaxWidth-1 do
    begin
      mag := GradientMap.Items[x,y];
      Dir := DirMap.Items[x,y];
      Case Dir of
        0 :
        begin
          if (mag < GradientMap.Items[x - 1, y]) or (mag < GradientMap.Items[x + 1, y]) then CleanGradientMap.Items[x - 1, y + 1] := 0;
        end;
        45 :
        begin
          if (mag < GradientMap.Items[x + 1, y - 1]) or (mag < GradientMap.Items[x - 1, y + 1]) then CleanGradientMap.Items[x - 1, y + 1] := 0;
        end;
        90 :
        begin
          if (mag < GradientMap.Items[x, y - 1]) or (mag < GradientMap.Items[x, y + 1]) then CleanGradientMap.Items[x - 1, y + 1] := 0;
        end;
        135 :
        begin
          if (mag < GradientMap.Items[x - 1, y - 1]) or (mag < GradientMap.Items[x + 1, y + 1]) then CleanGradientMap.Items[x - 1, y + 1] := 0;
        end;
      end;
    end;
  end;

  // Etape 6 : Double seuillage et suivi des contour par hysteresis
  if (NumStdDev > 0) and (MinThresholdFactor > 0) then
  begin
    tHi := MagMean + (NumStdDev * StdDev);
    tLo := tHi * MinThresHoldFactor;
  end
  else
  begin
    tHi := MaxThreshold * _FloatColorRatio;
    tLo := MinThresHold * _FloatColorRatio;
  end;

  TmpBmp.Clear(clrTransparent);
  for y := 1 to TmpBmp.MaxHeight-1 do
  begin
    for x := 1 to TmpBmp.MaxWidth-1 do
    begin
      mag := CleanGradientMap.Items[x,y];
      if (mag > tHi) then
      begin
        TmpBmp.setPixel(x, y, clrWhite);
        CleanGradientMap.Items[x,y] := 1.0;
      end
      else if (mag < tLo) then
      begin
        TmpBmp.setPixel(x, y, clrTransparent);
        CleanGradientMap.Items[x,y] := 0;
      end
      else
      begin // suivi des contours
        Connected := False;
        for j := -1 to 1 do
        begin
          for i := -1 to 1 do
          begin
            if (CleanGradientMap.Items[x + i, y + j] = 1.0) then connected := true;
          end;
        end;
        if Connected then
        begin
          TmpBmp.setPixel(x, y, clrWhite);
          CleanGradientMap.Items[x,y] := 1.0;
        end;

      end;
    end;
  end;

  OwnerBitmap.FastCopy(TmpBmp);
  FreeAndNil(CleanGradientMap);
  FreeAndNil(GradientMap);
  FreeAndNil(DirMap);
  FreeAndNil(GradX);
  FreeAndNil(GradY);
  FreeAndNil(TmpBmp);
end;

procedure TBZBitmapSegmentationFilters.MarrHildrethEdgeDetection(
  const GrayMode : TBZGrayConvertMode; const GrayMatrix : TBZGrayMatrixType; const KernelSize : Integer; const Weight : Single);
var
  TmpBmp : TBZBitmap;
  LoGKernel : TBZDynSingleArray;
  LMask : TBZSingle2DMap;

  procedure MaskConvolution(Kernel : TBZDynSingleArray; kSize : Integer; out mask : TBZSingle2DMap);
  var
    x,y, i, j, m, px, py : Integer;
    v : Single;
  begin
    m := (KSize div 2);
    Mask := TBZSingle2DMap.Create(TmpBmp.Width, TmpBmp.Height);
    for y := 0 to TmpBmp.MaxHeight do
    begin

      for x := 0 to TmpBmp.MaxWidth do
      begin
        v := 0;

        for j := 0 to KSize-1 do
        begin
          py := Clamp(y + j - m,0,TmpBmp.MaxHeight);
          for i := 0 to KSize-1 do
          begin
            px := Clamp(x + i - m,0,TmpBmp.MaxWidth);
            v := v + (Kernel[j * kSize + i] * (TmpBmp.getPixel(px, py).AsColorVector.x));
          end;
        end;
        Mask.Add(v);
      end;
    end;
  end;

  procedure ZeroCrossing(Mask : TBZSingle2DMap);
  Var
    x, y : Integer;
  begin
    for y := 1 to (TmpBmp.MaxHeight - 1) do
    begin
      for x := 1 to (TmpBmp.MaxWidth - 1) do
      begin
        OwnerBitmap.setPixel(x,y, clrTransparent);

        if ((Mask.Items[x - 1, y] * Mask.Items[x + 1, y]) < 0) or
           ((Mask.Items[x, y - 1] * Mask.Items[x, y + 1]) < 0) or
           ((Mask.Items[x - 1, y + 1] * Mask.Items[x + 1, y - 1]) < 0) or
           ((Mask.Items[x - 1, y - 1] * Mask.Items[x + 1, y + 1]) < 0) then
        begin
         OwnerBitmap.setPixel(x,y, clrWhite);
        end;
      end;
    end;
  end;

begin
  TmpBmp := TBZBitmap.Create;
  TmpBmp.Assign(OwnerBitmap);

  // Etape 1 : Conversion en niveau de gris
  TmpBmp.ColorFilter.GrayScale(GrayMode, GrayMatrix);

  // Etape 2 : Creation et application du masque de convolution "LoG"
  LoGKernel := ComputeLoGKernel(KernelSize, Weight); //TmpBmp.ConvolutionFilter.
  MaskConvolution(LoGKernel, KernelSize, LMask);

  // Etape 3 : Recherche des contours par Zero-crossing
  OwnerBitmap.Clear(clrTransparent);
  ZeroCrossing(LMask);

  FreeAndNil(TmpBmp);
  FreeAndNil(LMask);
  SetLength(LoGKernel, 0);
end;

procedure TBZBitmapSegmentationFilters.BooleanEdgeDetection(LocalConst, GlobalThreshold : Byte);
Const
  cEdgeMask : Array[0..15] of String = (
            '011011011',
            '000111111',
            '110110110',
            '111111000',

            '011011001',
            '100110110',
            '111011000',
            '111110000',

            '111011001',
            '100110111',
            '001011111',
            '111110100',

            '000011111',
            '000110111',
            '001011011',
            '110110100');

Var
  x, y, i, j : Integer;
  Mean : Integer;
  t, k  : integer;
  Variance : Single;
  PatternMaskA, PatternMask : String;
  inColor : TBZColor;
  TmpBmp : TBZBitmap;

  function checkMaskPattern(Const Pattern : String) : Boolean;
  var
    li : byte;
  begin
    result := False;
    for li := 0 to 15 do
    begin
      if (cEdgeMask[li] = Pattern) then
      begin
        Result := True;
        Break;
      end;
    end;
  end;

begin
  TmpBmp := TBZBitmap.Create(OwnerBitmap.Width, OwnerBitmap.Height);
  for y := 0 to OwnerBitmap.MaxHeight do
  begin
    for x := 0 to OwnerBitmap.MaxWidth do
    begin
      Mean := 0;
      Variance := 0;
      PatternMask := '';
      PatternMaskA := '';
      // Calcul de la moyenne locale
      for j := -1 to 1 do
      begin
        for i := -1 to 1 do
        begin
           inColor := OwnerBitmap.getPixel(x + i, y + j, peaClamp);
           Mean := Mean + inColor.Luminance;
        end;
      end;
      Mean := (Mean div 9) - LocalConst;
      //Mean := Median;
      //Mean := (min + max)/2;
      //Mean := (max - min)/2;

      for j := -1 to 1 do
      begin
        for i := -1 to 1 do
        begin
           inColor := OwnerBitmap.getPixel(x + i, y + j, peaClamp);
           if (inColor.Luminance > Mean) then PatternMaskA := PatternMaskA + '1' else PatternMaskA := PatternMaskA + '0';
        end;
      end;

      // Calcul de la variance locale
      for j := -1 to 1 do
      begin
        for i := -1 to 1 do
        begin
          inColor := OwnerBitmap.getPixel(x + i, y + j, peaClamp);
          t := inColor.Luminance;
          Variance := Variance + Math.Power((t - Mean), 2.0);
       end;
      end;
      Variance := Variance / 9;

      k:=1;
      for j := -1 to 1 do
      begin
        for i := -1 to 1 do
        begin
          inColor := OwnerBitmap.getPixel(x + i, y + j, peaClamp);
          t := inColor.Luminance;
          if (Variance > t) and (PatternMaskA[k] = '1') then PatternMask := PatternMask + '1' else PatternMask := PatternMask + '0';
          inc(k);
        end;
      end;

      if ((Variance > GlobalThreshold) and CheckMaskPattern(PatternMask)) then
      begin
        TmpBmp.setPixel(x,y, clrWhite);
      end
      else
      begin
        TmpBmp.setPixel(x,y, clrTransparent);
      end;
    end;
  end;
  OwnerBitmap.FastCopy(TmpBmp);
  FreeAndNil(TmpBmp);
end;

procedure TBZBitmapSegmentationFilters.MorphologicalEdgeDetection(FilterSize : Byte; FilterType : TBZMorphologicalEdgeDetectionMode);
Var
  TmpBmp : TBZBitmap;
begin
  TmpBmp := TBZBitmap.Create();
  TmpBmp.Assign(OwnerBitmap);
  Case FilterType of
    medErode :
    begin
      TmpBmp.MorphologicalFilter.Erode(FilterSize);
      TBZBitmap(OwnerBitmap).ArithmeticBlend(TmpBmp, cmRealSub);
    end;
    medDilate :
    begin
      TBZBitmap(OwnerBitmap).MorphologicalFilter.Dilate(FilterSize);
      TBZBitmap(OwnerBitmap).ArithmeticBlend(TmpBmp, cmRealSub);
    end;
    medOpen :
    begin
      TmpBmp.MorphologicalFilter.Open(FilterSize);
      TBZBitmap(OwnerBitmap).ArithmeticBlend(TmpBmp, cmRealSub);
    end;
    medClose :
    begin
      TBZBitmap(OwnerBitmap).MorphologicalFilter.Close(FilterSize);
      TBZBitmap(OwnerBitmap).ArithmeticBlend(TmpBmp, cmRealSub);
    end;
  end;
  FreeAndNil(TmpBmp);
end;

procedure TBZBitmapSegmentationFilters.MinMaxEdgeDetection(MinThresHold, MaxThreshold, GlobalThreshold : Byte);
Var
  x, y, i, j : Integer;
  luma, minValue, maxValue  : integer;
  TmpBmp : TBZBitmap;
begin
  TmpBmp := TBZBitmap.Create(OwnerBitmap.Width, OwnerBitmap.Height);
  for y := 0 to OwnerBitmap.MaxHeight do
  begin
    for x := 0 to OwnerBitmap.MaxWidth do
    begin
      minValue := 255;
      maxValue := 0;
      for j := -1 to 1 do
      begin
        for i := -1 to 1 do
        begin
          luma := OwnerBitmap.getPixel(x + i, y + j, peaClamp).Luminance;
          minValue := Min(luma, minValue);
          maxValue := Max(luma, maxValue);
        end;
      end;
      minValue := Max(minValue,MinThreshold);
      maxValue := Min(maxValue,MaxThreshold);
      luma := maxValue - minValue;
      if luma > GlobalThreshold then TmpBmp.SetPixel(x,y, clrWhite)
      else TmpBmp.SetPixel(x,y, clrTransparent)
    end;
  end;
  OwnerBitmap.FastCopy(TmpBmp);
  FreeAndNil(TmpBmp);
end;



{%region ====[ TBZCustomBitmapCanvas ]==========================================}

(*

//Procedure TBZCustomBitmapCanvas.InitTextureMapping;
//Begin
//  FBB.TopLeft := Brush.LeftTop.AsVector2f.Abs;
//  FBB.BottomRight := Brush.RightBottom.AsVector2f.Abs;
//  FBBSize := FBB.BottomRight - FBB.TopLeft;
//  FBBCenter := (FBBSize * 0.5);
//  FBBCenter := FBBCenter + FBB.TopLeft;
//  FMaxDist := Round(FBBSize.Height);
//end;

Procedure TBZCustomBitmapCanvas.Internal_BezierLine(x1, y1, x2, y2, c1x, c1y, c2x, c2y, segments : integer);
var
  i, x, y : integer;
  delta, time : single;
  lastx, lasty : integer;
  t1_1, t1_2, t1_3, t2, t3 : single;
  { TODO 1 -oBZBitmap -cBitmapCanvas : BezierLine : Use TBZVector2f for speedup }
  function ComputeBezier(p1, p2, c1, c2, time : single) : single;
  begin
    t1_1:= 1- time;
    t1_2:= t1_1* t1_1;
    t1_3:= t1_1* t1_2;
    t2:= time* time;
    t3:= t2* time;
    result:= t1_3* p1+ 3* time* t1_2* c1+ 3* t2* t1_1* c2+ t3* p2;
  end;

begin
  If Pen.GetStyle = ssClear Then exit;
  time:= 0;
  if segments= 0 then exit;
  delta:= 1/ segments;
  lastx:= round(ComputeBezier(x1, x2, c1x, c2x, time));
  lasty:= round(ComputeBezier(y1, y2, c1y, c2y, time));
  for i:= 0 to segments do
  begin
    x:= round(ComputeBezier(x1, x2, c1x, c2x, time));
    y:= round(ComputeBezier(y1, y2, c1y, c2y, time));
    time:= time+ delta;
    Line(x, y, lastx, lasty);
    lastx:= x;
    lasty:= y;
  end;
end;

Procedure TBZCustomBitmapCanvas.Internal_Arc(Cx, Cy, Rx, Ry, StartAngle, EndAngle: Integer);
Var
  Angle, Precision, Range, d, s,c: Single;
  Xs, Ys: Single;
  StrokePatternSize, delta: Integer;
  AColor: TBZColor;
Begin
  AColor := Pen.GetColor;
  StrokePatternSize := 0;
  Case Pen.GetPatternStyle Of
    psDot: StrokePatternSize := Length(DotPenStrokePattern);
    psDash: StrokePatternSize := Length(DashPenStrokePattern);
    psDashDot: StrokePatternSize := Length(DashDotPenStrokePattern);
    psDashDotDot: StrokePatternSize := Length(DashDotDotPenStrokePattern);
  End;

  precision := 0.01;

  If (startangle <= endangle) Then
    Angle := StartAngle * cPIdiv180
  Else
    Angle := EndAngle * cPIdiv180;
  If (endangle > startangle) Then
    Range := endangle * cPIdiv180
  Else
    Range := startangle * cPIdiv180;

  Math.SinCos(Angle,s,c);
  Xs := (rx * s);
  Ys := (ry * c);

  //L'Angle 0 est le point le plus gauche par rapport au centre (à 9 heure)
  // On tourne dans le sens des aiguille d'une montre
  Delta := 0;
  D := 0;
  Repeat
    Case Pen.GetPatternStyle Of
      psSolid: PutPixel(Round((Cx - Xs)), Round((Cy - Ys)), AColor);
      psDot: If DotPenStrokePattern[round(Angle - StartAngle) Mod StrokePatternSize] Then PutPixel(Round((Cx - Xs)), Round((Cy - Ys)), AColor);
      psDash: If DashPenStrokePattern[Delta Mod StrokePatternSize] Then PutPixel(Round((Cx - Xs)), Round((Cy - Ys)), AColor);
      psDashDot: If DashDotPenStrokePattern[round(Angle - StartAngle) Mod StrokePatternSize] Then PutPixel(Round((Cx - Xs)), Round((Cy - Ys)), AColor);
      psDashDotDot: If DashDotDotPenStrokePattern[round(Angle - StartAngle) Mod StrokePatternSize] Then PutPixel(Round((Cx - Xs)), Round((Cy - Ys)), AColor);
    End;

    Angle := Angle + Precision;
    If Pen.GetStyle <> ssSolid Then
    Begin
      D := D + Precision;
      If D >= 1 Then
      Begin
        Inc(Delta);
        D := 0;
      End;
    End;

    Math.SinCos(Angle,s,c);
    Xs := (rx * s);
    Ys := (ry * c);
  Until (angle > range);

End;

Procedure TBZCustomBitmapCanvas.Internal_Chord(Cx, Cy, Rx, Ry, StartAngle, EndAngle: Integer);
Var
  Angle, Precision, Range, d: Single;
  Xs, Ys:   Single;
  lsx, lsy: Integer;
  StrokePatternSize, delta: Integer;
  AColor:   TBZColor;

  Procedure DrawArcChord;
  Begin
    //L'Angle 0 est le point le plus gauche par rapport au centre (à 9 heure)
    // On tourne dans le sens des aiguille d'une montre
    Delta := 0;
    D := 0;

    Repeat

      Case Pen.GetPatternStyle Of
        psSolid: PutPixel(Round((Cx - Xs)), Round((Cy - Ys)), AColor);
        psDot: If DotPenStrokePattern[round(Angle - StartAngle) Mod StrokePatternSize] Then PutPixel(Round((Cx - Xs)), Round((Cy - Ys)), AColor);
        psDash: If DashPenStrokePattern[Delta Mod StrokePatternSize] Then PutPixel(Round((Cx - Xs)), Round((Cy - Ys)), AColor);
        psDashDot: If DashDotPenStrokePattern[round(Angle - StartAngle) Mod StrokePatternSize] Then PutPixel(Round((Cx - Xs)), Round((Cy - Ys)), AColor);
        psDashDotDot: If DashDotDotPenStrokePattern[round(Angle - StartAngle) Mod StrokePatternSize] Then PutPixel(Round((Cx - Xs)), Round((Cy - Ys)), AColor);
      End;

      Angle := Angle + Precision;
      If Pen.GetStyle <> ssSolid Then
      Begin
        D := D + Precision;
        If D >= 1 Then
        Begin
          Inc(Delta);
          D := 0;
        End;
      End;
      Xs := (rx * cos(angle));
      Ys := (ry * sin(angle));

    Until (angle > range);
  End;

  Procedure DrawThickArcChord;
  Var
    w1, w2, r2: Integer;
  Begin
    w1 := Pen.GetWidth Div 2;
    w2 := w1;
    If w1 + w2 = Pen.GetWidth Then
      Dec(w1);
    For r2 := 1 To w1 Do
      Internal_Arc(cx, cy, rx + r2, ry + r2, StartAngle, EndAngle);
    For r2 := 1 To w2 Do
      Internal_Arc(cx, cy, rx - r2, ry - r2, StartAngle, EndAngle);
  End;

Begin

  StrokePatternSize := 0;
  Case Pen.GetPatternStyle Of
    psDot: StrokePatternSize := Length(DotPenStrokePattern);
    psDash: StrokePatternSize := Length(DashPenStrokePattern);
    psDashDot: StrokePatternSize := Length(DashDotPenStrokePattern);
    psDashDotDot: StrokePatternSize := Length(DashDotDotPenStrokePattern);
  End;

  precision := 0.01;
  AColor := Pen.GetColor;

  If (startangle <= endangle) Then
    Angle := StartAngle * cPIdiv180
  Else
    Angle := EndAngle * cPIdiv180;
  If (endangle > startangle) Then
    Range := endangle * cPIdiv180
  Else
    Range := startangle * cPIdiv180;

  Xs := (rx * cos(angle));
  Ys := (ry * sin(angle));
  lsx := Round(Xs);
  lsy := Round(Ys);
  Line(Cx, Cy, Cx - lsx, Cy - Lsy);
  DrawArcChord;
  lsx := Round(Xs);
  lsy := Round(Ys);
  Line(Cx, Cy, Cx - lsx, Cy - Lsy);
  If Pen.GetWidth > 1 Then DrawThickArcChord;
End;

Procedure TBZCustomBitmapCanvas.Internal_RoundBox(x1, y1, x2, y2, RadiusTopLeft, RadiusTopRight, RadiusBottomLeft, RadiusBottomRight : Integer);
Var
  ATLX, ATLY, ATRX, ATRY, ABLX, ABLY, ABRX, ABRY : Integer;
  HLTX1, HLTX2, HLTY1, HLTY2 : Integer; // Haut
  HLBX1, HLBX2, HLBY1, HLBY2 : Integer; // Bas
  VLLX1, VLLX2, VLLY1, VLLY2 : Integer; // Gauche
  VLRX1, VLRX2, VLRY1, VLRY2 : Integer; // Droite
Begin


  If Pen.GetStyle = ssClear Then exit;
  HLTX1 := x1;
  HLTY1 := y1;
  HLTX2 := x2;
  HLTY2 := y1;

  HLBX1 := x1;
  HLBY1 := y2;
  HLBX2 := x2;
  HLBY2 := y2;

  VLLX1 := x1;
  VLLY1 := y1;
  VLLX2 := x1;
  VLLY2 := y2;

  VLRX1 := x2;
  VLRY1 := y1;
  VLRX2 := x2;
  VLRY2 := y2;

  RadiusTopLeft :=15;
  // Coin Haut Gauche
  if RadiusTopLeft > 0 then
  begin
    HLTX1 := x1 + RadiusTopLeft;
    VLLY1 := y1 + RadiusTopLeft;
    Internal_Arc(HLTX1, VLLY1, RadiusTopLeft, RadiusTopLeft, 0, 90);   //0,90
  End;

  RadiusBottomLeft :=15;
    // Coin Bas Gauche
  if RadiusBottomLeft > 0 then
  begin
    HLBX1 := x1 + RadiusBottomLeft;
    VLLY2 := y2 - RadiusBottomLeft;
    Internal_Arc(HLBX1, VLLY2, RadiusBottomLeft, RadiusBottomLeft, 90, 180);
  End;
  RadiusTopRight := 15;
  // Coin Haut Droite
   if RadiusTopRight > 0 then
  begin
    HLTX2 := x2 - RadiusTopRight;
    VLRY1 := y1 + RadiusTopRight;
    Internal_Arc(HLTX2, VLRY1,RadiusTopLeft, RadiusTopLeft, 270, 360);
  End;
  RadiusBottomRight := 15;
  // Coin Bas Droit
  if RadiusBottomRight > 0 then
  begin
    HLBX2 := x2 - RadiusBottomRight;
    VLRY2 := y2 - RadiusBottomRight;
    Internal_Arc(HLBX2, VLRY2, RadiusBottomLeft, RadiusBottomLeft, 180, 270);
  End;

  // Bordure Gauche
  Internal_Line(VLLX1, VLLY1, VLLX2, VLLY2);
  // Bordure Droite
  Internal_Line(VLRX1, VLRY1, VLRX2, VLRY2);
  // Bordure Haut
  Internal_Line(HLTX1, HLTY1, HLTX2, HLTY2);
  // Bordure Bas
  Internal_Line(HLBX1, HLBY1, HLBX2, HLBY2);

End;

*)

{%endregion%}

{%region ====[ TBZHistogram ]===================================================}

function TBZHistogram.GetChannel(Index : TBZHistogramChannel) : TBZHistogramArray;
begin
  if Not(FComputed) then ComputeHistogram;
  Result := FChannels[Index];
end;

function TBZHistogram.getStatistics : TBZHistogramStatistics;
begin
  if Not(FComputed) then ComputeHistogram;
  ComputeStatistics;
  Result := FStatistics;
end;

procedure TBZHistogram.ComputeHistogram;
var
  PixPtr : PBZColor;
  X, Y, M : Integer;
  HSV : TBZColorHSV;
  Hue : Byte;
begin
  FillChar(FChannels, SizeOf(FChannels), #0);

  for Y := 0 to OwnerBitmap.MaxHeight do
  begin
    PixPtr := OwnerBitmap.GetScanLine(Y);
    for X := 0 to OwnerBitmap.MaxWidth do
    begin
      //hcLuminosity, hcRed, hcGreen, hcBlue, hcHue, hcSaturation, hcLuminance, hcValue
      Inc(FChannels[hcLuminosity ][PixPtr^.Luminosity]);
      Inc(FChannels[hcRed][PixPtr^.Red]);
      Inc(FChannels[hcGreen][PixPtr^.Green]);
      Inc(FChannels[hcBlue][PixPtr^.Blue]);
      HSV := PixPtr^.ToColorHSV;
      Hue := (255 * HSV.Hue) div 359;
      Inc(FChannels[hcHue][Hue]);
      Inc(FChannels[hcSaturation][HSV.Saturation]);
      Inc(FChannels[hcLuminance][PixPtr^.Luminance]);
      Inc(FChannels[hcValue][HSV.Value]);
      Inc(PixPtr);
    end;
  end;
  // On cherche le maximum
  For x:=0 to 7 do
  begin
    FChannelsMaximum[TBZHistogramChannel(x)] := 0;
    For y := 0 to 255 do
    begin
      M := FChannels[TBZHistogramChannel(x)][y];
      if M > FChannelsMaximum[TBZHistogramChannel(x)] then FChannelsMaximum[TBZHistogramChannel(x)] := M;
    end;
  end;
  // Valeurs normalisées entre [0,1]
  //For x:=hcLuminosity to hcValue do
  //begin
  //  For y := 0 to 255 do
  //  begin
  //    FChannelsNormalize[x][y] := FChannels[x][y] / FChannelsMaximums[x];
  //  end;
  //end;

  ComputeStatistics;
  FComputed := True;
end;

procedure TBZHistogram.DrawTo(Dst : TBZBitmap; HistoChannel : TBZHistorgramDrawMode; Const Logarithmic, ShowAxis , ShowGrid, ShowGradient : Boolean );
const
  cGradientBarHeight = 8;
  cAxisBarHeight = 8;
  cMargin = 0;
  //cAxisMargin = 16;

  cBaseLog = 2.71828;  // the base of the logarithm
Var
  GradientRect, GridRect, HistoRect, AxisHorizRect : TBZRect;
  StartX, StartY,EndX, EndY,x1,y1,x2,y2 : Integer;
  xFactor : Single;
  DrawChannel : TBZHistogramChannel;


  procedure DrawGradientBar(Channel : TBZHistogramChannel);
  var
    aColor : TBZColor;
    HSLColor : TBZColorFloatHSL;
    f : Single;
    x : Integer;
  begin
    case Channel of
      hcSaturation, hcLuminosity, hcLuminance, hcValue : aColor := clrWhite;
      hcRed : aColor := clrRed;
      hcGreen : aColor := clrGreen;
      hcBlue : aColor := clrBlue;
    end;

    for x := 0 to GradientRect.Width-1 do
    begin
      if Channel = hcHue then
      begin
        f := Lerp(0.0,1.0,x/GradientRect.Width);
        HSLColor.Create(f,1.0,0.5,hslNormalize);
        aColor.Create(HSLColor.ToColorVector);

        Dst.VLine(GradientRect.Left + x,GradientRect.Top,GradientRect.Bottom,aColor);
      end
      else
        Dst.VLine(GradientRect.Left + x,GradientRect.Top,GradientRect.Bottom,clrBlack.Lerp(aColor,x/GradientRect.Width,0,itLinear, False));
    end;
  end;

  procedure DrawHistoGrid;
  Var
    y : Integer;
  begin
    Dst.Canvas.Brush.Style := bsClear;
    Dst.Canvas.Pen.Style := ssSolid;
    Dst.Canvas.Pen.Color := clrGray;
    Dst.Canvas.Pen.Width := 1;

    // Horizontal Lines
    for y := 0 TO 4 DO
    begin
      Dst.Canvas.MoveTo(GridRect.Left, GridRect.Top + (y*GridRect.Height) div 5);
      Dst.Canvas.LineTo(GridRect.Right, GridRect.Top + (y*GridRect.Height) div 5);
    end;
    // Vertical Lines
    for y := 0 TO 4 DO
    begin
      Dst.Canvas.MoveTo(GridRect.Left + (y*GridRect.Width) div 5, GridRect.Top);
      Dst.Canvas.LineTo(GridRect.Left + (y*GridRect.Width) div 5, GridRect.Bottom);
    end;
    Dst.Canvas.Rectangle(GridRect);
  end;

  procedure DrawHistoAxis;
  Var
    x, Dy : Integer;
    Dx : Integer;
    l : Single;
  begin
    Dst.Canvas.Brush.Style := bsClear;
    Dst.Canvas.Pen.Style := ssSolid;
    Dst.Canvas.Pen.Color := clrGray;
    Dst.Canvas.Pen.Width := 1;
    Dy :=  cAxisBarHeight;
    // Vertical Lines for visual estimation Axis
    l := 255 / 25;
    for x := 0 TO 25 DO
    begin
      Dx := Round(l*x*xFactor);
      Dst.Canvas.MoveTo(AxisHorizRect.Left + Dx, AxisHorizRect.Top);
      if x mod 5 = 0 then Dy :=  cAxisBarHeight
      else Dy :=  cAxisBarHeight div 2;
      Dst.Canvas.LineTo(AxisHorizRect.Left + Dx, AxisHorizRect.Top+Dy);
    end;
     Dst.Canvas.Line(AxisHorizRect.Left,AxisHorizRect.Top, AxisHorizRect.Right, AxisHorizRect.Top);
  end;

  procedure DrawHistoChannel(Channel : TBZHistogramChannel; Const AlphaFactor : Byte = 192);
  Var
    aColor, aLightColor : TBZColor;
    idx, hl,x : Integer;
    HSLColor : TBZColorFloatHSL;
    MaxValue, f : Single;
  begin
    case Channel of
      hcSaturation, hcLuminosity, hcLuminance, hcValue : aColor := clrGray;
      hcRed : aColor := clrRed;
      hcGreen : aColor := clrGreen;
      hcBlue : aColor := clrBlue;
    end;
    aColor.Alpha := AlphaFactor;
    aLightColor := aColor.Lighten(0.5);
    MaxValue := FChannelsMaximum[Channel];
    if Logarithmic then
    begin
      if MaxValue > 0.0 then
      begin
        MaxValue := Math.LogN(cBaseLog, MaxValue);
      end
      else
      begin
        MaxValue := 1.0;
      end;
    end;

    Dst.Canvas.DrawMode.AlphaMode := amBlend;
    Dst.Canvas.Brush.Style := bsClear;
    Dst.Canvas.Pen.Style := ssSolid;
    Dst.Canvas.Pen.Width := 1;
    xFactor :=  HistoRect.Width / 255;
    Dst.Canvas.Pen.Color := aLightColor;
    if MaxValue > 0 then
    begin
      For x := 0 to HistoRect.Width-1 do
      begin
        idx := Round(x / xFactor);
        idx := min(idx,255);
        if Channel = hcHue then
        begin
          f := Lerp(0.0,1.0,x/HistoRect.Width);
          HSLColor.Create(f,1.0,0.5,hslNormalize);
          aColor.Create(HSLColor.ToColorVector);
          aColor.Alpha := AlphaFactor;
          Dst.Canvas.Pen.Color := aColor.Lighten(0.3);
        end;

        if Logarithmic then
         hl := Trunc(HistoRect.Height * Math.LogN(cBaseLog, FChannels[Channel][idx]) / MaxValue )
        else
         hl := Trunc(HistoRect.Height * FChannels[Channel][idx] / MaxValue);

        With Dst.Canvas do
        begin
          MoveTo(HistoRect.Left + x, HistoRect.Bottom-hl);
          LineTo(HistoRect.Left + x, HistoRect.Bottom);
        end;
      end;
    end;
  end;

  procedure DrawHistoChannelCurve(Channel : TBZHistogramChannel);
  Var
    aColor  : TBZColor;
    idx, hl,x : Integer;
    HSLColor : TBZColorFloatHSL;
    MaxValue, f : Single;
  begin
    case Channel of
      hcLuminosity, hcLuminance, hcValue, hcSaturation : aColor := clrWhite;
      hcRed : aColor := clrRed;
      hcGreen : aColor := clrLime;
      hcBlue : aColor := clrBlue;
    end;

    MaxValue := FChannelsMaximum[Channel];
    if Logarithmic then
    begin
      if MaxValue > 0.0 then
      begin
        MaxValue := Math.LogN(cBaseLog, MaxValue);
      end
      else
      begin
        MaxValue := 1.0;
      end;
    end;

    Dst.Canvas.DrawMode.AlphaMode := amOpaque;
    Dst.Canvas.Brush.Style := bsClear;
    Dst.Canvas.Pen.Style := ssSolid;
    Dst.Canvas.Pen.Width := 1;
    Dst.Canvas.MoveTo(HistoRect.Left, HistoRect.Bottom);
    Dst.Canvas.Pen.Color := aColor;
    xFactor :=  HistoRect.Width / 255;
    if MaxValue > 0 then
    begin
      For x := 0 to HistoRect.Width-1 do
      begin
        idx := Round(x / xFactor);
        idx := min(idx,255);

         if Channel = hcHue then
        begin
          f := Lerp(0.0,1.0,x/HistoRect.Width);
          HSLColor.Create(f,1.0,0.5,hslNormalize);
          aColor.Create(HSLColor.ToColorVector);
          Dst.Canvas.Pen.Color := aColor;
        end;

        if Logarithmic then
         hl := Trunc(HistoRect.Height * Math.LogN(cBaseLog, FChannels[Channel][idx]) / MaxValue )
        else
         hl := Trunc(HistoRect.Height * FChannels[Channel][idx] / MaxValue);

        With Dst.Canvas do
        begin
          LineTo(HistoRect.Left + x, HistoRect.Bottom-hl);
        end;
      end;
    end;
  end;

begin
  if Not(FComputed) then
  begin
    ComputeHistogram;
    ComputeStatistics;
  end;

  //Dst.Clear(clrGray);

  //w := Dst.Width - (cMargin * 2);
  //h := Dst.Height - (cMargin * 2);
  StartX := 0;//cMargin + 1;
  StartY := 0;//cMargin + 1;
  EndX := Dst.MaxWidth; //- cMargin - 1;
  EndY := Dst.MaxHeight; //- cMargin - 1;
  x1 := StartX;
  x2 := EndX;
  y1 := StartY;
  y2 := EndY;

  xFactor :=  (x2 - x1) / 255;

  if ShowAxis then
  begin
    //x1:=  x1 + cAxisMargin;
    //x2 := x2 - (cAxisMargin div 2);
    //y1 := y2 - cAxisBarHeight;
    AxisHorizRect.Create(x1, (y2 - cAxisBarHeight), x2, y2);
    y2 := y2 - cAxisBarHeight;
    DrawHistoAxis;
  end;

  if ShowGradient then
  begin
    if ShowAxis then
    begin
      GradientRect.Create(x1+1,(y2 - cGradientBarHeight), x2-1, y2-1);
      y2 := y2 - cGradientBarHeight - 1;
    end
    else
    begin
      GradientRect.Create(x1+1,(y2 - cGradientBarHeight), x2-1, y2-1);
    end;
  end;

  if ShowGrid then
  begin
//    x1 := x1 - 4;
    //y2 := y1;
    //y1 := StartY;
    if ShowGradient then GridRect.Create(x1,y1,x2,y2 + cGradientBarHeight)
    else GridRect.Create(x1,y1,x2,y2);
    DrawHistoGrid;
  end;

  x1 := StartX;
  HistoRect.Create(x1+1,y1+1,x2-1,y2-1);
  // Paint histogram

  if (HistoChannel = hdmRBGLuminosity) or (HistoChannel = hdmRGB) then
  begin
    DrawChannel := hcRed;
    DrawHistoChannel(DrawChannel,192);
    DrawHistoChannelCurve(DrawChannel);
    DrawChannel := hcGreen;
    DrawHistoChannel(DrawChannel,160);
    DrawHistoChannelCurve(DrawChannel);
    DrawChannel := hcBlue;
    DrawHistoChannel(DrawChannel,128);
    DrawHistoChannelCurve(DrawChannel);
    if (HistoChannel = hdmRBGLuminosity) then
    begin
      DrawChannel := hcLuminosity;
      DrawHistoChannel(DrawChannel,96);
      DrawHistoChannelCurve(DrawChannel);
    end;
    if ShowGradient then DrawGradientBar(hcLuminosity);
  end
  else
  begin
    Case HistoChannel of
      hdmLuminosity : DrawChannel := hcLuminosity;
      hdmRed : DrawChannel := hcRed;
      hdmGreen : DrawChannel := hcGreen;
      hdmBlue : DrawChannel := hcBlue;
      hdmHue : DrawChannel := hcHue;
      hdmSaturation : DrawChannel := hcSaturation;
      hdmLuminance : DrawChannel := hcLuminance;
      hdmValue : DrawChannel := hcValue;
    end;
    DrawHistoChannel(DrawChannel);
    DrawHistoChannelCurve(DrawChannel);
    if ShowGradient then DrawGradientBar(DrawChannel);
  end;

  FComputed := False;
  //c1:= Blend(c, clWhite, 30);
  //Canvas.Pen.Color:= c;
  //j:= MulDiv(nHist[ChnIdx, 0], 230, maxHist[ChnIdx]);
  //Canvas.MoveTo(Offset, MaxVal);
  //Canvas.LineTo(Offset, MaxVal - j);
  //k:= (MaxVal) - j;
  //for i := 1 to 255 do begin
  //  j:= (MaxVal) - MulDiv(nHist[ChnIdx, i], 230, maxHist[ChnIdx]);
  //  Canvas.Pen.Color:= c1;
  //  Canvas.MoveTo(i + Offset, MaxVal);
  //  Canvas.LineTo(i + Offset, j);
  //  WuLine(imgCurve.Picture.Bitmap, Offset + (i - 1), k, Offset + i, j, c);
  //  k:= j;
  //end;
end;

procedure TBZHistogram.ComputeStatistics;
Var
  N,Cumulative  :  Integer;
  i           :  Byte;
  MaxFrequency:  Integer;
  M2          :  Double;
  M3          :  Double;
  M3Sum       :  Double;
  M4          :  Double;
  M4Sum       :  Double;
  x           :  Double;
  xSum        :  Double;  // Use floats to avoid integer overflow
  xSqrSum     :  Double;
begin
  With FStatistics do
  begin
    Pixels := 0;
    Minimum := 0;
    Maximum := 255;
    MaxFrequency := 0;
    Mean := 0;
    StandardDeviation := 0.0;
    Skewness := 0.0;
    ExcessKurtosis := -3.0;
  end;

  While (FChannels[hcLuminosity][FStatistics.Minimum] = 0) And (FStatistics.Minimum < 255) do Inc(FStatistics.Minimum);
  While (FChannels[hcLuminosity][FStatistics.Maximum] = 0) And (FStatistics.Maximum > 0) do Dec(FStatistics.Maximum);

  // Mode is value with highest frequency.
  // For now, don't worry about a "tie".
  FStatistics.MaxFrequency := FStatistics.Minimum;

  MaxFrequency := FChannels[hcLuminosity][FStatistics.Minimum];
  For i := Succ(FStatistics.Minimum) to FStatistics.Maximum do
  Begin                                  // runtime problem
    if (FChannels[hcLuminosity][i] > MaxFrequency) then
    Begin
      FStatistics.MaxFrequency := i;
      MaxFrequency := FChannels[hcLuminosity][i];
    End;
  End;

  // Calculate Mean and Standard Deviation
  xSum    := 0.0;
  xSqrSum := 0.0;
  For i := FStatistics.Minimum to FStatistics.Maximum do
  Begin
    Inc(FStatistics.Pixels, FChannels[hcLuminosity][i]);
    x := i;
    xSum    := xSum    + FChannels[hcLuminosity][i]*x;
    xSqrSum := xSqrSum + FChannels[hcLuminosity][i]*(x*x)
  END;

  if FStatistics.Pixels = 0 then FStatistics.Mean := NaN else FStatistics.Mean := xSum / FStatistics.Pixels;

  If FStatistics.Pixels < 2 Then
  Begin
    FStatistics.StandardDeviation := NaN;
    FStatistics.Skewness := 0.0;
    FStatistics.ExcessKurtosis := -3.0;
  End
  Else
  Begin
    N := FStatistics.Pixels - 1;
    FStatistics.StandardDeviation := System.Sqrt( (xSqrSum - FStatistics.Pixels*Sqr(FStatistics.Mean)) / N );
    // Standard Deviation is related to moment M2
    M2 := Sqr(FStatistics.StandardDeviation) * N / FStatistics.Pixels;
    // Calculate third and fourth moments
    M3Sum := 0.0;
    M4Sum := 0.0;
    For i := FStatistics.Minimum to FStatistics.Maximum do
    Begin
      x := i;
      M3Sum := M3Sum + FChannels[hcLuminosity][i]*Math.IntPower(x - FStatistics.Mean, 3);
      M4Sum := M4Sum + FChannels[hcLuminosity][i]*Math.IntPower(x - FStatistics.Mean, 4);
    End;

    M3 := M3Sum / FStatistics.Pixels;
    M4 := M4Sum / FStatistics.Pixels;

    If M2 = 0.0 Then
    Begin
      FStatistics.Skewness := NaN;
      FStatistics.ExcessKurtosis := -3.0;
    End
    Else
    Begin
      FStatistics.Skewness := M3 / Math.Power(M2, 1.5);
      FStatistics.ExcessKurtosis := M4 / (M2*M2) -3.0;
    End
  End;

  // Median is value with half of values above and below.
  Cumulative := 0;
  i := FStatistics.Minimum;
  N := FStatistics.Pixels div 2;
  While (Cumulative < N) And (i < 255) do
  Begin
    Inc(Cumulative, FChannels[hcLuminosity][i]);
    If (Cumulative < N) Then Inc(i);    // fix for when all 0s
  End;
  FStatistics.Median := i;
end;

{ https://stackoverflow.com/questions/9744255/instagram-lux-effect/9761841#9761841 }
procedure TBZHistogram.GetAutoLevels(Const Channel : TBZHistogramChannel; var OptimalFactorBrightness, OptimalFactorContrast: single);
var
  i, iLow: integer;
  MaxH, Prev: int64;
  MinLim, MaxLim, MinPos, MaxPos: Single;
  MinBest, MaxBest: Single;
begin
  if Not(FComputed) then
  begin
    ComputeHistogram;
    ComputeStatistics;
  end;

  MinBest := 0.02;
  MaxBest := 0.98;
  MinPos := 0;
  MaxPos := 1;

  // Valeur maximale dans l'histogramme
  MaxH := FChannelsMaximum[hcLuminosity];

  // limite basse et haute
  MinLim := MaxH * MinBest;
  MaxLim := MaxH * MaxBest;

  // Determination des pourcentages bas et haut
  MaxH := 0;
  Prev := 0;
  iLow := 0;
  for i := 0 to 255 do
  begin
    inc(MaxH, FChannels[hcLuminosity][i]);
    if (MaxH >= MinLim) and (Prev <= MinLim) and not (MaxH = Prev) then MinPos := iLow + (i - iLow) * (MinLim - Prev)/(MaxH - Prev);
    if (MaxH >= MaxLim) and (Prev <= MaxLim) and not (MaxH = Prev) then MaxPos := iLow + (i - iLow) * (MaxLim - Prev)/(MaxH - Prev);

    if Prev < MaxH then
    begin
      iLow := i;
      Prev := MaxH;
    end;
  end;

  MaxPos := MaxPos/255;
  MinPos := MinPos/255;

  // pas d'ajustement excessif, seulement 85% de décalage au lieu de 100%
  MaxBest := MaxBest + 0.15 * (MaxPos - MaxBest);
  MinBest := MinBest + 0.15 * (MinPos - MinBest);

  // Ajustement du contrast
  OptimalFactorContrast := ((MaxBest - MinBest) * (MaxPos - MinPos));
  // Ajustement de la luminosité
  OptimalFactorBrightness := (MaxBest - (0.5 + (MaxPos - 0.5)) * OptimalFactorContrast);

end;

procedure TBZHistogram.AutoAdjustBrightnessContrast;
Var
  BrightnessFactor, ContrastFactor : Single;
begin
  BrightnessFactor := 1.0;
  ContrastFactor := 1.0;
  GetAutoLevels(hcLuminosity, BrightnessFactor, ContrastFactor);
  TBZBitmap(OwnerBitmap).ColorFilter.AdjustBrightness(BrightnessFactor);
  TBZBitmap(OwnerBitmap).ColorFilter.AdjustContrast(ContrastFactor);
end;

procedure TBZHistogram.AdjustLevels(LowInputLevel, HighInputLevel, LowOutputLevel, HighOutputLevel : Single);
var
  Diff,l,h : Byte;
  RGBLut : Array[0..2,0..255] of byte;
  i, j : Integer;
  PixPtr : PBZColor;
  inColor, outColor : TBZColor;
begin
  if Not(FComputed) then
  begin
    ComputeHistogram;
    ComputeStatistics;
  end;
  l := Round(LowInputLevel * 255);
  h := Round(HighInputLevel * 255);
  Diff := h-l;
  if l=h then h:=h+1;
  if (l=255) and (h=255) then l := l - 1;
  for i := 0 to 2 do
  begin
    for j := 0 to 255 do
    begin
      RGBLut[i,j] := ClampByte(Round(255 * (LowOutputLevel + (HighOutputLevel-LowOutputLevel) * ((j-l)/Diff))));
    end;
  end;
//  if not(Assigned(OwnerBitmap)) then GlobalLogger.Log('ERROR !!!!!!!!!!!!!!!!!!');
  PixPtr := OwnerBitmap.GetScanLine(0);
  i:=0;
  While i <= OwnerBitmap.MaxSize do
  begin
    inColor := PixPtr^;
    outColor.Red := RGBLut[0,inColor.Red];
    outColor.Green := RGBLut[1,inColor.Green];
    outColor.Blue := RGBLut[2,inColor.Blue];
    outColor.Alpha := inColor.Alpha;
    PixPtr^:= outColor;
    inc(i);
    inc(PixPtr);
  end;

  //For y := 0 to OwnerBitmap.MaxHeight do
  //begin
  //  For x := 0 to OwnerBitmap.MaxWidth do
  //  begin
  //    inColor := OwnerBitmap.getPixel(x,y);
  //    outColor.Red := RGBLut[0,inColor.Red];
  //    outColor.Green := RGBLut[1,inColor.Green];
  //    outColor.Blue := RGBLut[2,inColor.Blue];
  //    outColor.Alpha := inColor.Alpha;
  //    OwnerBitmap.setPixel(x,y,outColor);
  //  end;
  //end;

  // Mise à jour
  ComputeHistogram;
  ComputeStatistics;
end;

procedure TBZHistogram.AutoAdjustLevels(Const Tolerance : Integer = 0);
var
  i, L, H: Byte;
  T: Integer;
begin
  T := Tolerance;
  L := 0;
  for i := 0 to 255 do
  begin
    if FChannels[hcLuminosity][i] > T then Break
    else Inc(L);
  end;
  H := 255;
  for i := 255 downto 0 do
  begin
    if FChannels[hcLuminosity][i] > T then Break
    else Dec(H);
  end;
  AdjustLevels(L/255,H/255,0.0,1.0);
end;

procedure TBZHistogram.Equalize;
Var
  MapRed, MapGreen, MapBlue : Array[0..255] of Byte;
  i : Integer;
  SumR, SumG, SumB : Integer;
  ScaleFactor : Single;
  PixPtr : PBZColor;
  inColor, outColor : TBZColor;
begin
  if Not(FComputed) then
  begin
    ComputeHistogram;
    ComputeStatistics;
  end;

  SumR := 0;
  SumG := 0;
  SumB := 0;
  ScaleFactor := 255 / FStatistics.Pixels;
  For i := 0 to 255 do
  begin
    Inc(SumR, FChannels[hcRed][i]);
    MapRed[i] := Round(SumR * ScaleFactor);
    Inc(SumG, FChannels[hcGreen][i]);
    MapGreen[i] := Round(SumG * ScaleFactor);
    Inc(SumB, FChannels[hcBlue][i]);
    MapBlue[i] := Round(SumB * ScaleFactor);
  end;

  PixPtr := OwnerBitmap.GetScanLine(0);
  i:=0;
  While i <= OwnerBitmap.MaxSize do
  begin
    inColor := PixPtr^;
    outColor.Red := MapRed[inColor.Red];
    outColor.Green := MapGreen[inColor.Green];
    outColor.Blue := MapBlue[inColor.Blue];
    outColor.Alpha := inColor.Alpha;
    PixPtr^:= outColor;
    inc(i);
    inc(PixPtr);
  end;
  // Mise à jour
  ComputeHistogram;
  ComputeStatistics;
end;

{%endregion%}

{%region ====[ TBZBitmapTransformations ]=======================================}

{ TODO 1  -oBZBitmap -cFilter : Ajouter d'autres filtres à TBZBitmapTransformationFilters (Shear, perspective, HQx, XBR, SeamCarving,...) }

//Constructor TBZBitmapTransformations.Create(Const AOwner: TBZCustomBitmap);
//Begin
//  Inherited Create;
//  Owner := AOwner;
//End;
//
//Destructor TBZBitmapTransformations.Destroy;
//Begin
//  Owner := nil;
//  Inherited Destroy;
//End;

Procedure TBZBitmapTransformations.FlipX;
Var
  pLine1, pLine2: PBZColor;
  c2, c1: TBZColor;
  Y, X:   Longint;
Begin
  For Y := 0 To OwnerBitmap.MaxHeight Do
  Begin
    pLine1 := OwnerBitmap.GetScanLine(Y);
    pLine2 := OwnerBitmap.GetPixelPtr(OwnerBitmap.MaxWidth, Y);
    For X := 0 To OwnerBitmap.CenterX - 1 Do
    Begin
      c1 := pLine1^;
      c2 := pLine2^;
      pLine1^ := c2;
      pLine2^ := c1;
      Inc(pLine1);
      Dec(pLine2);
    End;
  End;
End;

Procedure TBZBitmapTransformations.FlipY;
Var
  P1, P2, Buff:  PBZColor;
  I: Longint;
Begin
  GetMem(Buff, OwnerBitmap.Width);
  Try
    // Swap all scanlines of image
    For I := 0 To OwnerBitmap.CenterY Do
    Begin
      P1 := OwnerBitmap.GetScanLine(I);
      P2 := OwnerBitmap.GetScanLine(OwnerBitmap.MaxHeight - I);
      Move(P1^, Buff^, OwnerBitmap.Width);
      Move(P2^, P1^, OwnerBitmap.Width);
      Move(Buff^, P2^, OwnerBitmap.Width);
    End;
  Finally
    FreeMem(Buff);
  End;
End;

Procedure TBZBitmapTransformations.Rotate(Const Degrees : Integer; Const Wrap : Boolean; Const RotCX : Integer; Const RotCY : Integer);
Var
  cosTheta, sinTheta, Theta: Single;
  //Delta:  Integer;
  ecX1, ecY1: Integer;
  ecX2, ecY2: Integer;
  ecX3, ecY3: Integer;
  ecX4, ecY4: Integer;
  cX, cY: Integer;
  xDiff, yDiff: Integer;
  minX, maxX: Integer;
  minY, maxY: Integer;
  i, j:   Integer;
  iSrc, jSrc: Integer;
  iSrcPrime, iDestPrime: Integer;
  jSrcPrime, jDestPrime: Integer;
  TmpBmp: TBZBitmap;
  SrcRow, DestRow: PBZColor;
  SrcW, SrcH: Integer;

  Function GetRotatedY(OrgX, OrgY: Integer; SinTheta, CosTheta: Double): Integer;
  Begin
    Result := (round((2 * (OrgX) + 1) * sinTheta + (2 * (OrgY) + 1) * cosTheta) - 1) Div 2;
  End;

  Function GetRotatedX(OrgX, OrgY: Integer; SinTheta, CosTheta: Double): Integer;
  Begin
    Result := (round((2 * (OrgX) + 1) * CosTheta - (2 * (OrgY) + 1) * sinTheta) - 1) Div 2;
  End;

Begin
  SrcW := OwnerBitmap.Width;
  SrcH := OwnerBitmap.Height;
  Theta := DegToRadian(-Degrees);
  sinTheta := SIN(Theta);
  cosTheta := COS(Theta);
  If Not (Wrap) Then
  Begin
    //GlobalLogger.LogNotice('Rotate Wrap');
    ecX1 := GetRotatedX(0, 0, SinTheta, CosTheta);
    ecY1 := GetRotatedY(0, 0, SinTheta, CosTheta);

    ecX2 := GetRotatedX(SrcW, 0, SinTheta, CosTheta);
    ecY2 := GetRotatedY(SrcW, 0, SinTheta, CosTheta);

    ecX3 := GetRotatedX(SrcW, SrcH, SinTheta, CosTheta);
    ecY3 := GetRotatedY(SrcW, SrcH, SinTheta, CosTheta);

    ecX4 := GetRotatedX(0, SrcH, SinTheta, CosTheta);
    ecY4 := GetRotatedY(0, SrcH, SinTheta, CosTheta);

    If ecX1 >= ecX2 Then
    Begin
      maxX := ecX1;
      minX := ecX2;
    End
    Else
    Begin
      maxX := ecX2;
      minX := ecX1;
    End;
    If ecY1 >= ecY2 Then
    Begin
      maxY := ecY1;
      minY := ecY2;
    End
    Else
    Begin
      maxY := ecY2;
      minY := ecY1;
    End;
    If ecX3 >= maxX Then maxX := ecX3
    Else If ecX3 <= minX Then minX := ecX3;
    If ecY3 >= maxY Then maxY := ecY3
    Else If ecY3 <= minY Then minY := ecY3;
    If ecX4 >= maxX Then maxX := ecX4
    Else If ecX4 <= minX Then minX := ecX4;
    If ecY4 >= maxY Then maxY := ecY4
    Else If ecY4 <= minY Then minY := ecY4;
    TmpBmp := TBZBitmap.Create(Abs(MaxX - MinX), Abs(MaxY - MinY));
    //   DestBmp.SetSize(Abs(MaxX-MinX),Abs(MaxY-MinY));
    XDiff := (TmpBmp.Width - SrcW) Div 2;
    YDiff := (TmpBmp.Height - SrcH) Div 2;

  End
  Else
  Begin
    //GlobalLogger.LogNotice('Rotate Normal');
    TmpBmp := TBZBitmap.Create(SrcW, SrcH);

    //DestBmp.SetSize(Owner.Width,Owner.Height);
    yDiff := 0;
    xDiff := 0;
  End;

  If RotCX < 0 Then cX := OwnerBitmap.CenterX
  Else cX := RotCX;
  If RotCY < 0 Then cY := OwnerBitmap.CenterY
  Else cY := RotCY;

  For j := TmpBmp.MaxHeight Downto 0 Do
  Begin
    DestRow := TmpBmp.GetScanline(j);
    jSrcPrime := 2 * (j - (YDiff + cY)) + 1;
    //if Assigned(ResampleCallBack) then ResampleCallBack(0,100,Round(((DestBitmap.Height-j)/DestBitmap.Height)*100));

    For i := 0 To TmpBmp.MaxWidth Do
    Begin
      iSrcPrime := 2 * (i - (XDiff + cX)) + 1;
      iDestPrime := round(iSrcPrime * CosTheta - jSrcPrime * sinTheta);
      jDestPrime := round(iSrcPrime * sinTheta + jSrcPrime * cosTheta);

      iSrc := (iDestPrime - 1) Div 2 + OwnerBitmap.CenterX;
      jSrc := (jDestPrime - 1) Div 2 + OwnerBitmap.CenterY;

      If (iSrc >= 0) And (iSrc <= OwnerBitmap.MaxWidth) And (jSrc >= 0) And (jSrc <= OwnerBitmap.MaxHeight) Then
      Begin
        SrcRow := OwnerBitmap.GetScanLine(jSrc);
        DestRow^ := PBZColor(SrcRow + iSrc)^;
      End
      Else
      Begin
        DestRow^ := clrTransparent;
      End;
      Inc(DestRow);
    End;
  End;
  If Not (Wrap) Then
  Begin
    //GlobalLogger.LogStatus('Rotate New Size :' + IntToStr(TmpBmp.Width) + 'x' + IntToStr(TmpBmp.Height));
    OwnerBitmap.SetSize(TmpBmp.Width, TmpBmp.Height);

  End;
  OwnerBitmap.PutImage(TmpBmp, 0, 0, TmpBmp.Width, TmpBmp.Height, 0, 0);
  FreeAndNil(TmpBmp);
End;

Procedure TBZBitmapTransformations.KeepAspectRatio(Const SrcW, SrcH : Integer; Var NewWidth, NewHeight : Integer);
begin
  BZGraphic.KeepAspectRatio(SrcW, SrcH, NewWidth, NewHeight);
End;

Procedure TBZBitmapTransformations.Stretch(NW, NH : Integer; Const KeepRatio : Boolean);
Var
  nwr, nhr : Integer;
begin
  Self.Stretch(NW,NH, nwr, nhr, KeepRatio);
End;

procedure TBZBitmapTransformations.StretchTo(DstBmp : TBZBitmap; Const KeepRatio : Boolean);
Var
  NW, NH, SrcW, SrcH, DstW, DstH, X, Y, UX, UY: Integer;
  Dx, Dy, uvx, uvy: Single;
  DstPtr, SrcPtr: PBZColor;
  AColor: TBZColor;
Begin
  SrcW := OwnerBitmap.Width;
  SrcH := OwnerBitmap.Height;
  NW := DstBmp.Width;
  NH := DstBmp.Height;
  If KeepRatio Then KeepAspectRatio(SrcW, SrcH, NW, NH);
  If (NW = SrcW) And (NH = SrcH) Then
  begin
    DstBmp.FastCopy(OwnerBitmap);
    exit;
  end;
  DstW := NW - 1;
  DstH := NH - 1;
  Dx := SrcW / DstW;
  Dy := SrcH / DstH;
  DstPtr := DstBmp.GetScanLine(0);
  UvY :=0;
  For y := 0 To DstH Do
  Begin
    UY := Clamp(Ceil(UvY), 0, OwnerBitmap.MaxHeight);
    SrcPtr := OwnerBitmap.GetScanLine(UY);
    UvX := 0;
    For X := 0 To DstW Do
    Begin
      UX := Clamp(Ceil(UvX), 0, OwnerBitmap.MaxWidth);
      AColor := PBZColor(SrcPtr + UX)^;
      DstPtr^ := AColor;
      Inc(DstPtr);
      UvX := UvX + DX;
    End;
    UvY := UvY + DY;
  End;
end;

Procedure TBZBitmapTransformations.Stretch(NW, NH : Integer; out NewWidth, NewHeight : Integer; Const KeepRatio : Boolean);
Var
  TmpBmp: TBZBitmap;
  SrcW, SrcH, DstW, DstH, X, Y, UX, UY: Integer;
  Dx, Dy, uvx, uvy: Single;
  DstPtr, SrcPtr: PBZColor;
  AColor: TBZColor;
Begin
  SrcW := OwnerBitmap.Width;
  SrcH := OwnerBitmap.Height;
  NewWidth := nw;
  NewHeight := nh;
  If (NW = SrcW) And (NH = SrcH) Then exit;
  If KeepRatio Then KeepAspectRatio(SrcW, SrcH, NewWidth, NewHeight);
  TmpBmp := TBZBitmap.Create(NewWidth, NewHeight);
  DstW := NewWidth - 1;
  DstH := NewHeight - 1;
  Dx := SrcW / DstW;
  Dy := SrcH / DstH;
  DstPtr := TmpBmp.GetScanLine(0);
  UvY :=0;
  For y := 0 To DstH Do
  Begin
    UY := Clamp(Trunc(UvY), 0, OwnerBitmap.MaxHeight);
    SrcPtr := OwnerBitmap.GetScanLine(UY);
    UvX := 0;
    For X := 0 To DstW Do
    Begin
      UX := Clamp(Trunc(UvX), 0, OwnerBitmap.MaxWidth);
      AColor := PBZColor(SrcPtr + UX)^;
      DstPtr^ := AColor;
      Inc(DstPtr);
      UvX := UvX + DX;
    End;
    UvY := UvY + DY;
  End;
  OwnerBitmap.Assign(TmpBmp);
  //Owner.FastCopy(TmpBmp);
  FreeAndNil(TmpBmp);
end;

{ Modifié, adpapté et optimisé depuis : http://delphi-kb.blogspot.ch/2011/05/resize-tbitmap-quickly.html
  --> "This is a modification of Answer 4 and is about three times faster:"
  ==> Stretch Bilineaire
}
Procedure TBZBitmapTransformations.StretchSmooth(NW, NH : Integer; Const KeepRatio : Boolean);
Var
  TmpBmp:   TBZBitmap;
  SrcW, SrcH, X, Y, ix, iy: Integer;
  Dx, Dy:   Single;
  uvx, uvy: Integer;

  sfrom_y, sfrom_x: Single;
  ifrom_y, ifrom_x: Integer;

  weight_x, weight_y:     Array[0..1] Of Single;
  weight, tr, tg, tb, ta: Single;

  DstPtr, SrcPtr: PBZColor;
  AColor: TBZColor;

  NewWidth, NewHeight : Integer;

Begin
  SrcW := OwnerBitmap.Width;
  SrcH := OwnerBitmap.Height;
  NewWidth := NW;
  NewHeight := NH;

  If KeepRatio Then KeepAspectRatio(SrcW, SrcH, NewWidth, NewHeight);

  If (NewWidth = SrcW) And (NewHeight = SrcH) Then exit;

  TmpBmp := TBZBitmap.Create(nil, NewWidth, NewHeight);
  //TmpBmp.PutImage(Owner, 0, 0, NewWidth, NewHeight, 0, 0);
  Dx := 1 / (NewWidth / SrcW);
  Dy := 1 / (NewHeight / SrcH);

  uvx := 0;
  uvy := 0;
  sFrom_y := Dy;
  sFrom_x := Dx;

  For Y := 0 To TmpBmp.MaxHeight Do
  Begin
    sfrom_y := Y * Dy;
    ifrom_y := Trunc(sfrom_y);
    // Calcul du poid en y
    weight_y[1] := sfrom_y - ifrom_y;
    weight_y[0] := 1 - weight_y[1];
    DstPtr := TmpBmp.GetScanLine(Y);
    For X := 0 To TmpBmp.MaxWidth Do
    Begin
      sfrom_x := X * Dx;
      ifrom_x := Trunc(sfrom_x);
      // Calcul du poid en x
      weight_x[1] := sfrom_x - ifrom_x;
      weight_x[0] := 1 - weight_x[1];
      tr := 0.0;
      tg := 0.0;
      tb := 0.0;
      ta := 0.0;
      // rééchantillonnage rapide. On prend un carre de 2x2
      For ix := 0 To 1 Do
      Begin
        For iy := 0 To 1 Do
        Begin
          uvy := (ifrom_y + iy);
          If uvy > OwnerBitmap.MaxHeight Then uvy := OwnerBitmap.MaxHeight;
          SrcPtr := OwnerBitmap.GetScanLine(uvy);
          uvx := (ifrom_x + ix);
          If uvx > OwnerBitmap.MaxWidth Then uvx := OwnerBitmap.MaxWidth;
          If uvx > 0 Then Inc(SrcPtr, uvx);
          AColor := SrcPtr^;
          weight := weight_x[ix] * weight_y[iy];
          tr := tr + AColor.Red * weight;
          tg := tg + AColor.Green * weight;
          tb := tb + AColor.Blue * weight;
          ta := ta + AColor.Alpha * weight;
        End;
      End;
      DstPtr^.Create(Round(tr), Round(tg), Round(tb), Round(ta));
      Inc(DstPtr);
    End;
  End;
//  Owner.SetSize(NW, NH);
//  Owner.PutImage(TmpBmp, 0, 0, NW, NH, 0, 0);
  OwnerBitmap.Assign(TmpBmp);
  FreeAndNil(TmpBmp);
End;

procedure TBZBitmapTransformations.StretchSmoothTo(DstBmp : TBZBitmap; Const KeepRatio : Boolean);
Var
  NH, NW, SrcW, SrcH, X, Y, ix, iy: Integer;
  Dx, Dy:   Single;
  uvx, uvy: Integer;

  sfrom_y, sfrom_x: Single;
  ifrom_y, ifrom_x: Integer;

  weight_x, weight_y:     Array[0..1] Of Single;
  weight, tr, tg, tb, ta: Single;  //ColorVector, WeightVector : TBZColorFloat;

  DstPtr, SrcPtr: PBZColor;
  AColor: TBZColor;

Begin
  SrcW := OwnerBitmap.Width;
  SrcH := OwnerBitmap.Height;
  NW := DstBmp.Width;
  NH := DstBmp.Height;

  If KeepRatio Then KeepAspectRatio(SrcW, SrcH, NW, NH);
  If (NW = SrcW) And (NH = SrcH) Then
  begin
    DstBmp.FastCopy(OwnerBitmap);
    exit;
  end;
  Dx := 1 / (NW / SrcW);
  Dy := 1 / (NH / SrcH);

  uvx := 0;
  uvy := 0;
  sFrom_y := Dy;
  sFrom_x := Dx;

  For Y := 0 To DstBmp.MaxHeight Do
  Begin
    sfrom_y := Y * Dy;
    ifrom_y := Trunc(sfrom_y);
    // Calcul du poid en y
    weight_y[1] := sfrom_y - ifrom_y;
    weight_y[0] := 1 - weight_y[1];
    DstPtr := DstBmp.GetScanLine(Y);
    For X := 0 To DstBmp.MaxWidth Do
    Begin
      sfrom_x := X * Dx;
      ifrom_x := Trunc(sfrom_x);
      // Calcul du poid en x
      weight_x[1] := sfrom_x - ifrom_x;
      weight_x[0] := 1 - weight_x[1];
      tr := 0.0;
      tg := 0.0;
      tb := 0.0;
      ta := 0.0;
      // rééchantillonnage rapide. On prend un carre de 2x2
      For ix := 0 To 1 Do
      Begin
        For iy := 0 To 1 Do
        Begin
          uvy := (ifrom_y + iy);
          If uvy > OwnerBitmap.MaxHeight Then uvy := OwnerBitmap.MaxHeight;
          SrcPtr := OwnerBitmap.GetScanLine(uvy);
          uvx := (ifrom_x + ix);
          If uvx > OwnerBitmap.MaxWidth Then uvx := OwnerBitmap.MaxWidth;
          If uvx > 0 Then Inc(SrcPtr, uvx);
          AColor := SrcPtr^;
          weight := weight_x[ix] * weight_y[iy];
          tr := tr + AColor.Red * weight;
          tg := tg + AColor.Green * weight;
          tb := tb + AColor.Blue * weight;
          ta := ta + AColor.Alpha * weight;
        End;
      End;
      DstPtr^.Create(Round(tr), Round(tg), Round(tb), Round(ta));
      Inc(DstPtr);
    End;
  End;
end;

{ https://en.wikipedia.org/wiki/Bicubic_interpolation du chinois pour moi toutes ces formules }

{ Fonction adapté du code source de  ??
  d'après le code source en C de Paul Toth
}
procedure TBZBitmapTransformations.StretchBicubicTo(DstBmp : TBZBitmap; Const KeepRatio : Boolean);
Const
  BiCubicRPrecal: Array[1..16] Of Single = (0.00260416666666667, 0.0208333333333333, 0.0703125, 0.166666666666667,
    0.315104166666667, 0.479166666666667, 0.611979166666667, 0.666666666666667,
    0.611979166666667, 0.479166666666667, 0.315104166666667, 0.166666666666667,
    0.0703125, 0.0208333333333333, 0.00260416666666667, 0.0);
Var
  NH, NW, SrcH, SrcW: Integer;

  xd, yd, xs, ys, ii, jj, n, m: Integer;

  fx,fy,cx, cy, dx, dy,weight : Single;

  //red, green, blue, alpha: Single;
  ColorF, SumColor : TBZColorVector;
  WeightColor : TBZColorVector;

  //AColor,
  DstCol: TBZColor;
  DstPtr:     PBZColor;
  Ind1, Ind2: Integer;

Begin

  SrcW := OwnerBitmap.Width;
  SrcH := OwnerBitmap.Height;
  NW := DstBmp.Width;
  NH := DstBmp.Height;

  If KeepRatio Then KeepAspectRatio(SrcW, SrcH, NW, NH);
  If (NW = SrcW) And (NH = SrcH) Then
  begin
    DstBmp.FastCopy(OwnerBitmap);
    exit;
  end;

  fx := SrcW / NW;
  fy := SrcH / NH;
  DstPtr := DstBmp.GetScanLine(0);
  For yd := 0 To DstBmp.MaxHeight Do
  Begin
    For xd := 0 To DstBmp.MaxWidth Do
    Begin
      //xs := (xd * SrcW) Div NW;
      //ys := (yd * SrcH) Div NH;
      cx := xd * fx; //SrcW / NW;
      cy := yd * fy;//SrcH / NH;
      xs := Trunc(cx);
      ys := Trunc(cy);

      dx := cx - xs;
      dy := cy - ys;
      //red := 0;
      //green := 0;
      //blue := 0;
      //alpha := 0;
      SumColor.Create(0,0);

      For m := -1 To 2 Do
      Begin
        For n := -1 To 2 Do
        Begin
          ii := Clamp(xs + m, 0, OwnerBitmap.MaxWidth);
          jj := Clamp(ys + n, 0, OwnerBitmap.MaxHeight);

          Ind1 := round(4 * (m - dx)) + 8; // Indice de correspondance avec la table précalculée
          Ind2 := round(4 * (n - dy)) + 8; // Idem
          // Evite un bug d'indice hors limites
          Ind1 := Max(Min(Ind1, 16), 1);
          Ind2 := Max(Min(Ind2, 16), 1);

          weight := BiCubicRPrecal[ind1] * BiCubicRPrecal[ind2];
          WeightColor.Create(Weight, Weight);
          //AColor := OwnerBitmap.getPixel(ii, jj);
          ColorF := OwnerBitmap.getPixel(ii, jj).AsColorVector;
          ColorF := ColorF * WeightColor;
          SumColor := SumColor + ColorF
          //red := red + weight * AColor.Red;
          //green := green + weight * AColor.Green;
          //blue := blue + weight * AColor.Blue;
          //alpha := alpha + weight * AColor.Alpha;

        End;
      End;
       DstCol.Create(SumColor);
      //DstCol.Red := ClampByte(Round(red));
      //DstCol.Green := ClampByte(Round(green));
      //DstCol.Blue := ClampByte(Round(blue));
      //DstCol.Alpha := ClampByte(Round(alpha));

      DstPtr^ := DstCol;
      Inc(DstPtr);
    End;
  End;
End;

Procedure TBZBitmapTransformations.StretchBicubic(NW, NH : Integer; Const KeepRatio : Boolean);
Var
  SrcH, SrcW: Integer;

  TmpBmp: TBZBitmap;

  xd, yd, xs, ys, ii, jj, n, m,ww,hh: Integer;

  cx, cy, dx, dy, weight, red, green, blue, alpha: Single;
  AColor, DstCol: TBZColor;
  DstPtr:     PBZColor;
  Ind1, Ind2: Integer;
Const
  BiCubicRPrecal: Array[1..16] Of Single = (0.00260416666666667, 0.0208333333333333, 0.0703125, 0.166666666666667,
    0.315104166666667, 0.479166666666667, 0.611979166666667, 0.666666666666667,
    0.611979166666667, 0.479166666666667, 0.315104166666667, 0.166666666666667,
    0.0703125, 0.0208333333333333, 0.00260416666666667, 0.0);
Begin
  ww := NW;
  hh := NH;
  SrcW := OwnerBitmap.Width;
  SrcH := OwnerBitmap.Height;

  If KeepRatio Then KeepAspectRatio(SrcW, SrcH, ww, hh);

  If (ww = SrcW) And (hh = SrcH) Then exit;

  TmpBmp := TBZBitmap.Create(nil, ww, hh);
  //TmpBmp.PutImage(Owner, 0, 0, NW, NH, 0, 0);

  DstPtr := TmpBmp.GetScanLine(0);
  For yd := 0 To TmpBmp.MaxHeight Do
  Begin
    For xd := 0 To TmpBmp.MaxWidth Do
    Begin
      xs := (xd * SrcW) Div ww;
      ys := (yd * SrcH) Div hh;
      cx := xd * SrcW / ww;
      cy := yd * SrcH / hh;

      dx := cx - xs;
      dy := cy - ys;
      red := 0;
      green := 0;
      blue := 0;
      alpha := 0;

      For m := -1 To 2 Do
      Begin
        For n := -1 To 2 Do
        Begin
          ii := Clamp(xs + m, 0, OwnerBitmap.MaxWidth);
          jj := Clamp(ys + n, 0, OwnerBitmap.MaxHeight);

          Ind1 := round(4 * (m - dx)) + 8; // Indice de correspondance avec la table précalculée
          Ind2 := round(4 * (n - dy)) + 8; // Idem
          // Evite un bug d'indice hors limites
          Ind1 := Max(Min(Ind1, 16), 1);
          Ind2 := Max(Min(Ind2, 16), 1);

          weight := BiCubicRPrecal[ind1] * BiCubicRPrecal[ind2];
          AColor := OwnerBitmap.getPixel(ii, jj);

          red := red + weight * AColor.Red;
          green := green + weight * AColor.Green;
          blue := blue + weight * AColor.Blue;
          alpha := alpha + weight * AColor.Alpha;

        End;
      End;
      DstCol.Create(0,0,0,0);
      DstCol.Red := ClampByte(Round(red));
      DstCol.Green := ClampByte(Round(green));
      DstCol.Blue := ClampByte(Round(blue));
      DstCol.Alpha := ClampByte(Round(alpha));

      DstPtr^ := DstCol;
      Inc(DstPtr);
    End;
  End;
 // Owner.SetSize(NW, NH);
 // Owner.PutImage(TmpBmp, 0, 0, NW, NH, 0, 0);
  OwnerBitmap.Assign(TmpBmp);
  FreeAndNil(TmpBmp);
end;

{ Methode de redimensionnement experimentale }
Procedure TBZBitmapTransformations.StretchSmart(Var NW, NH : Integer; Const RayLength : Byte; Const KeepRatio : Boolean);
Var
  SrcH, SrcW, I, J, XX, YY, kx, ky: Integer;
  a, b, s1, s11, s22, s33, s44, vs, vt, va1, va2, va3, va4: Double;
  t, x, y, xr, yr, ScaleFactorX, ScaleFactorY: Double;

  TmpBmp: TBZBitmap;
  DstPtr: PBZColor;
  AColor, DstColor: TBZColor;
  xStart, xEnd, yStart, yEnd: Integer;

  Function AccurateSinc(x: Single): Single;
  var xx:single;
  Begin
      If x = 0.0 Then
        Result := 1.0
      Else
      Begin
        xx := cPI * x;
        Result := System.Sin(xx) / (xx);
      End;
  End;

  Function ComputeWeight(Value: Double): Double;
  Var
    w, r: Double;
  Begin
    r := ComputeReciprocal(BesselIO(6.33));
    w := Bessel(6.33 * (0.5 - (Value))) * r;
    //w:=Bessel(6.33 * (1.0 - (Value/3) * (Value/3))) * r;
    // Value := SinC(cPI*Value);
    Result := abs(AccurateSinC(Value) * w);
  End;

Begin
  SrcW := OwnerBitmap.Width;
  SrcH := OwnerBitmap.Height;

  If KeepRatio Then KeepAspectRatio(SrcW, SrcH, NW, NH);

  If (NW = SrcW) And (NH = SrcH) Then exit;

  TmpBmp := TBZBitmap.Create(nil, NW, NH);
  //TmpBmp.PutImage(Owner, 0, 0, NW, NH, 0, 0);
  ScaleFactorX := 1 / (NW / SrcW);
  ScaleFactorY := 1 / (NH / SrcH);

  For  J := 0 To TmpBmp.MaxHeight Do
  Begin
    y := J * ScaleFactorY;
    Yr := y;
    yy := Clamp(Round(y), 0, OwnerBitmap.MaxHeight);
    DstPtr := TmpBmp.GetScanLine(J);
    // SrcPtr := Owner.GetScanLine(yy);
    yStart := yy - RayLength;
    yEnd := yy + RayLength;
    For  I := 0 To TmpBmp.MaxWidth Do
    Begin
      x := I * ScaleFactorX;
      Xr := x;
      xx := Clamp(Round(x), 0, OwnerBitmap.MaxWidth); // On va chercher le plus proche
      s1 := 0;
      s11 := 0;
      s22 := 0;
      s33 := 0;
      s44 := 0;
      xStart := xx - RayLength;
      xEnd := xx + RayLength;
      For kx := xStart To xEnd Do
      Begin
        If Not ((kx >= 0) And (kx < OwnerBitmap.MaxWidth)) Then continue;
        For ky := yStart To yEnd Do
        Begin
          If Not ((ky >= 0) And (ky < OwnerBitmap.MaxHeight)) Then continue;
          a := (xr - kx);
          b := (yr - ky);
          // Le coeur de la bete
          t := (cPI * sqrt(a * a * 0.5 + b * b)); // Restons concentrique !

          vt := ComputeWeight(t);

          // t:=a;//(cPI*a*a*a);//(cPI*sqrt(a*a));
          // v:=b;//(cPI*b*b*b);//(cPI*sqrt(b*b)); //(0.005*cPIdiv4)*b;

          //wd:=Bessel(t/(RayLength*4)); // *4 = quantité de diffusion flou d'erreur du tramage RayLength = MaxSupport = 3.0
          (*wd:=Bessel(t/12);
          t := Sinh(cPI*t);  //SinC(t)
          vs:=abs(SinC(t)*wd); *)

          vs := vt;//ComputeWeight(t); //Blur Horizontal
          //  vs2:=ComputeWeight(v);  //Vertical
          //  vs:=(vt*(1.0-vs2))+(vt*(1.0-vs)); //(vs+vs2);
          If vs < 0 Then vs := -vs;

          s1 := s1 + vs;

          If (vs > 0) Then
          Begin
            AColor := OwnerBitmap.GetPixel(kx, ky);
            va1 := AColor.Red * vs;
            va2 := AColor.Blue * vs;
            va3 := AColor.Green * vs;
            va4 := AColor.Alpha * vs;

            s11 := s11 + va1;
            s22 := s22 + va2;
            s33 := s33 + va3;
            s44 := s44 + va4;
          End;
        End;
      End;
      s1 := s1 + cEpsilon; //Evite division par zero
      s11 := s11 / s1;
      s22 := s22 / s1;
      s33 := s33 / s1;
      s44 := s44 / s1;

      DstColor.Create(ClampByte(Round(s11)), ClampByte(Round(s33)), ClampByte(Round(s22)), ClampByte(Round(s44)));
      //TmpBmp.SetPixel(i,j,DstColor);
      DstPtr^ := DstColor;
      Inc(DstPtr);
    End;
  End;
  //Owner.SetSize(NW, NH);
  //Owner.PutImage(TmpBmp, 0, 0, NW, NH, 0, 0);
  OwnerBitmap.Assign(TmpBmp);
  FreeAndNil(TmpBmp);
End;

Procedure TBZBitmapTransformations.DownSample(Const Factor : Integer);
Var
  S1, S2:   PBZColor;
  PixelPtr: PBZColor;
  N, X, Y, W, H, LW, hh, ww, NW, NH: Integer;
  AColor, C1, C2, C3, C4: TBZColor;
  TmpBmp:   TBZBitmap;
Begin

  With OwnerBitmap Do
  Begin
    NW := Width;
    NH := Height;
    If (factor <= 0) Then
      Exit;
    // La largeur n'est pas multiple de 2, on ajuste
    If (Width And 1 = 1) Then
    Begin
      W := MaxWidth Shr 1;
      LW := MaxWidth;
      NW := LW;
    End
    Else
    Begin
      W := Width Shr 1;
      LW := Width;
      NW := LW;
    End;

    // La Hauteur n'est pas multiple de 2, on ajuste
    If (Height And 1 = 1) Then
    Begin
      H := MaxHeight Shr 1;
      NH := MaxHeight;
    End
    Else
    Begin
      H := Height Shr 1;
      NH := Height;
    End;
  End;

  ww := w - 1;
  hh := h - 1;

  TmpBmp := TBZBitmap.Create(nil, NW, NH);
  TmpBmp.PutImage(OwnerBitmap, 0, 0, NW, NH, 0, 0);

  With OwnerBitmap Do
  Begin
    SetSize(W, H);
    Clear(clrBlack);
  End;

  For N := 1 To Factor Do
  Begin
    If N > 1 Then
    Begin
      LW := W;
      TmpBmp.SetSize(W, H);
      TmpBmp.PutImage(OwnerBitmap, 0, 0, W, H, 0, 0);
      W := W Shr 1;
      H := H Shr 1;

      ww := w - 1;
      hh := h - 1;

      OwnerBitmap.SetSize(W, H);
      OwnerBitmap.Clear(clrBlack);
    End;
    S1 := TmpBmp.GetScanLine(0);
    S2 := TmpBmp.GetScanLine(1);

    PixelPtr := OwnerBitmap.GetScanLine(0);

    { Pour chaque pixel du bitmap réduit }
    For Y := 0 To hh Do
    Begin
      For X := 0 To ww Do
      Begin
        { On prend le carré de 2x2 pixels de l'image originale et l'on calcule le pixel final en
          prenant la moyenne des quatre pixels du carré }
        C1 := S1^;
        C2 := PBZColor(S1 + 1)^;
        C3 := S2^;
        C4 := PBZColor(S2 + 1)^;

        AColor.Red := (C1.Red + C2.Red + C3.Red + C4.Red) Shr 2;
        AColor.Green := (C1.Green + C2.Green + C3.Green + C4.Green) Shr 2;
        AColor.Blue := (C1.Blue + C2.Blue + C3.Blue + C4.Blue) Shr 2;
        AColor.Alpha := (C1.Alpha + C2.Alpha + C3.Alpha + C4.Alpha) Shr 2;
        PixelPtr^ := AColor;

        Inc(S1, 2);
        Inc(S2, 2);
        Inc(PixelPtr);
      End;
      Inc(S1, LW);
      Inc(S2, LW);
    End;
  End;
  TmpBmp.Free;
End;

{ Resample : Méthode adapté de fpc (fpcIntepolation.pp) et de graphic32 (GR32_Resample.pas) }
Type
  TWeightPointRec = Record
    Pos:    Integer;
    Weight: Single;
  End;
  //PWeightPointRec = ^TWeightPointRec;

  TWeightPoints = Array Of TWeightPointRec;
  TWeightable   = Array Of TWeightPoints;

Procedure TBZBitmapTransformations.Resample(NewWidth, NewHeight : Integer; ResampleMode : TBZInterpolationFilterMethod; Const KeepRatio : Boolean);
Var
  TmpBmp: TBZBitmap;
begin
  TmpBmp := TBZBitmap.Create(NewWidth, NewHeight);
  Self.ResampleTo(TmpBmp, ResampleMode, KeepRatio);
  //NewWidth := TmpBmp.Width;
  //NewHeight := TmpBmp.Height;
  OwnerBitmap.Assign(TmpBmp);
  FreeAndNil(TmpBmp);
end;

Procedure TBZBitmapTransformations.ResampleTo(DstBmp : TBZBitmap; ResampleMode : TBZInterpolationFilterMethod; Const KeepRatio : Boolean);
Var
  ResampleFilterClass: TBZInterpolationFilterClass;
  ResampleFilter: TBZCustomInterpolationFilter;
  VerticalWeightTable, HorizontalWeightTable: TWeightable; // Ou Pointer data = (integer+single);???
  VWTableSize, HWTableSize: Integer;
  HorzBuffer: Array Of TBZColor;
  ClusterX, ClusterY: TWeightPoints;
  //Cr, Cg, Cb, Ca: Single; //TBZColorVector

  C:      TBZColor;
  ClustYW, Wt: Single;

  DstPtr: PBZColor;
  I, J, X, Y: Integer;
  NewWidth, NewHeight : Integer;

  {$CODEALIGN VARMIN=16}
  cv, ClusterVec, TempVec : TBZColorVector;
  ColorOut : TBZVector4i;
  {$CODEALIGN VARMIN=4}

  Procedure BuilWeightTable(DestSize, SrcSize: Integer; out WeightTable: TWeightable; out TableSize: Integer);
  Var
    ScaleFactor, InvScaleFactor: Single;
    FilterWidth, Center: Single;
    I, J, K, N:  Integer;
    Count, t:    Single;
    Left, Right: Integer;
    Weight:      Single;
  Begin
    ScaleFactor := DestSize / SrcSize;
    TableSize := 0;
    K := 0;
    WeightTable := nil;

    If (SrcSize = 0) Then exit
    Else If SrcSize = 1 Then
    Begin
      SetLength(WeightTable, 1);
      SetLength(WeightTable[0], 1);
      WeightTable[0][0].Pos := 0;
      WeightTable[0][0].Weight := 1.0;
      TableSize := 1;
      exit;
    End;

    SetLength(WeightTable, DestSize);
    TableSize := DestSize;

    If (ScaleFactor = 1) Or (DestSize = SrcSize) Then // Pas de changement
    Begin
      SetLength(WeightTable, DestSize);
      For i := 0 To Pred(DestSize) Do
      Begin
        SetLength(WeightTable[I], 1);
        WeightTable[I][K].Pos := I;
        WeightTable[I][K].Weight := 1.0;
      End;
    End
    Else If ScaleFactor < 1 Then // Réduction
    Begin
      InvScaleFactor := ScaleFactor.Reciprocal; //1 / ScaleFactor;
      FilterWidth := InvScaleFactor * ResampleFilter.MaxSupport;
      For I := 0 To Pred(DestSize) Do
      Begin
        Center := -0.5 + (I + 0.5) * InvScaleFactor;
        Left := Floor(Center - FilterWidth);
        Right := Ceil(Center + FilterWidth);
        Count := -1.0;
        For J := Left To Right Do
        Begin
          Weight := ResampleFilter.Filter((Center - J) * ScaleFactor) * ScaleFactor;
          If Weight = 0.0 Then Continue;
          Count := Count + Weight;
          K := Length(WeightTable[I]);
          SetLength(WeightTable[I], K + 1);
          WeightTable[I][K].Pos := Clamp(J, 0, Pred(SrcSize));
          WeightTable[I][K].Weight := Weight;
        End;
        If Length(WeightTable[I]) = 0 Then
        Begin
          SetLength(WeightTable[I], 1);
          WeightTable[I][0].Pos := Floor(Center);
          WeightTable[I][0].Weight := 1.0;
        End;
        If Count <> 0 Then
        Begin
          K := Length(WeightTable[I]);
          t := WeightTable[I][K Div 2].Weight;
          WeightTable[I][K Div 2].Weight := t - Count;
        End;
      End;
    End
    Else If ScaleFactor > 1 Then /// Augmentation
    Begin
      InvScaleFactor := ComputeReciprocal(ScaleFactor);
      FilterWidth := ResampleFilter.MaxSupport * InvScaleFactor;
      For I := 0 To Pred(DestSize) Do
      Begin
        Center := -0.5 + (I + 0.5) * InvScaleFactor;
        Left := Floor(Center - FilterWidth);
        Right := Ceil(Center + FilterWidth);
        Count := -1.0;
        N := 0;
        For J := Left To Right Do
        Begin
          Weight := ResampleFilter.Filter(Center - J);
          If Weight = 0.0 Then Continue;
          Count := Count + Weight;
          K := Length(WeightTable[I]);
          SetLength(WeightTable[I], K + 1);
          WeightTable[I][N].Pos := Clamp(J, 0, Pred(SrcSize));
          WeightTable[I][N].Weight := Weight;
          Inc(N);
        End;
        // On ajuste le pixel central. Car la somme de tous les poids doit être égale à 1.0.
        If (Count <> 0.0) Then
        Begin
          K := Length(WeightTable[I]);
          J := K Div 2;
          t := WeightTable[I][J].Weight;
          WeightTable[I][J].Weight := t - Count;
        End;
      End;
    End;
  End;

Begin
  ResampleFilter := nil;

  ClusterX := nil;
  ClusterY := nil;
  NewWidth := DstBmp.Width;
  NewHeight := DstBmp.Height;
  If KeepRatio Then
  begin
    KeepAspectRatio(OwnerBitmap.Width, OwnerBitmap.Height, NewWidth, NewHeight);
    if (DstBmp.Width <> NewWidth) or (DstBmp.Height <> NewHeight) then DstBmp.SetSize(NewWidth, NewHeight);
  end;

  // On va chercher le filtre
  ResampleFilterClass := GetBZInterpolationFilter(ResampleMode);
  ResampleFilter := ResampleFilterClass.Create;
  Try
    // Precalcul des "poids" a l'horizontal et en vertical
    BuilWeightTable(NewHeight, OwnerBitmap.Height, VerticalWeightTable, VWTableSize);
    BuilWeightTable(NewWidth, OwnerBitmap.Width, HorizontalWeightTable, HWTableSize);

    // Rien à faire, on sort
    If (VerticalWeightTable = nil) Or (HorizontalWeightTable = nil) Then Exit;

    // On initialise notre buffer temporaire pour stocker les valeurs des pixels
    // horizontal, après rééchantillonage.
    SetLength(HorzBuffer{%H-}, OwnerBitmap.Width);

    For J := 0 To DstBmp.MaxHeight Do
    Begin
      ClusterY := VerticalWeightTable[J];
      // Rééchantillonage Vertical
      //for X := MapXLoPos to MapXHiPos do
      For X := 0 To OwnerBitmap.MaxWidth Do
      Begin
        //Ca := 0;
        //Cr := 0;
        //Cg := 0;
        //Cb := 0;
        cv.Create(0,0);
        For Y := 0 To Length(ClusterY) - 1 Do
        Begin
          C := OwnerBitmap.getPixel(X, ClusterY[Y].Pos);
          TempVec := C.ToColorVector;
          ClustYW := ClusterY[Y].Weight;
          ClusterVec.Create(ClustYW,ClustYW);
          TempVec := TempVec * ClusterVec;
          cv := cv + TempVec;
          //Ca := Ca + (C.Alpha * ClustYW);
          //Cr := Cr + (C.Red * ClustYW);
          //Cg := Cg + (C.Green * ClustYW);
          //Cb := Cb + (C.Blue * ClustYW);
        End;
        // On transfert le résultat dans notre buffer temporaire "HorzBuffer"
        ColorOut := cv.Round;
        With HorzBuffer[X] Do     //-MapxLoPos
        Begin
          //Red := ClampByte(Round(Cr));
          //Green := ClampByte(Round(Cg));
          //Blue := ClampByte(Round(Cb));
          //Alpha := ClampByte(Round(Ca));
          Red := ClampByte(ColorOut.Red);
          Green := ClampByte(ColorOut.Green);
          Blue := ClampByte(ColorOut.Blue);
          Alpha := ClampByte(ColorOut.Alpha);
        End;
      End;

      DstPtr := DstBmp.GetScanLine(J);

      // Rééchantillonage horizontal
      For I := 0 To DstBmp.MaxWidth Do
      Begin
        ClusterX := HorizontalWeightTable[I];
        //Ca := 0;
        //Cr := 0;
        //Cg := 0;
        //Cb := 0;
        cv.Create(0,0);
        For X := 0 To Length(ClusterX) - 1 Do
        Begin
          Wt := ClusterX[X].Weight;
          ClusterVec.Create(Wt, Wt);
          // On prend en compte les valeurs calculé dans notre buffer temporaire "HorzBuffer"
          C := HorzBuffer[ClusterX[X].Pos];
          TempVec := C.ToColorVector;
          TempVec := TempVec * ClusterVec;
          cv := cv + TempVec;
          //With C Do //- MapXLoPos
          //Begin
          //  Ca := Ca + (Alpha * Wt);
          //  Cr := Cr + (Red * Wt);
          //  Cg := Cg + (Green * Wt);
          //  Cb := Cb + (Blue * Wt);
          //End;
        End;
        // On ecrit le pixel
        ColorOut := cv.Round;
        With C Do
        Begin
          Red := ClampByte(ColorOut.Red);
          Green := ClampByte(ColorOut.Green);
          Blue := ClampByte(ColorOut.Blue);
          Alpha := ClampByte(ColorOut.Alpha);
          //Alpha := ClampByte(round(Ca));
          //Red := ClampByte(round(Cr));
          //Green := ClampByte(round(Cg));
          //Blue := ClampByte(round(Cb));
        End;
        DstPtr^ := C;
        Inc(DstPtr);
      End;
    End;
  Finally
    // On met à jour notre bitmap et on recopie le résultat obtenu depuis "TmpBmp"
    //Owner.SetSize(NewWidth, NewHeight);
    //Owner.PutImage(TmpBmp, 0, 0, NewWidth, NewHeight, 0, 0);


    // On vide et on libère les tableaux
    SetLength(HorzBuffer, 0);
    HorzBuffer := nil;
    SetLength(HorizontalWeightTable , 0);
    HorizontalWeightTable := nil;
    SetLength(VerticalWeightTable, 0);
    VerticalWeightTable := nil;

    // On libère les objets temporaire
    FreeAndNil(ResampleFilter);
  End;
End;

Procedure TBZBitmapTransformations.Crop(x, y, w, h : Integer);
Var
  TmpBmp: TBZBitmap;
Begin
  If (w <= 0) Or (h <= 0) Then exit;
  TmpBmp := TBZBitmap.Create(OwnerBitmap.Width, OwnerBitmap.Height);
  Try
    TmpBmp.PutImage(OwnerBitmap, 0, 0, OwnerBitmap.Width, OwnerBitmap.Height, 0, 0);
    OwnerBitmap.SetSize(W, H);
    OwnerBitmap.Clear(clrBlack);
    OwnerBitmap.PutImage(TmpBmp, X, Y, W, H, 0, 0);
  Finally
    FreeAndNil(TmpBmp);
  End;
End;

Procedure TBZBitmapTransformations.CropRect(ARect : TBZRect);
//Var
//  w, h: Integer;
Begin
  //w := (ARect.Right - ARect.Left) + 1;
  //h := (ARect.Top - ARect.Bottom) + 1;
  Crop(ARect.Left, ARect.Top, ARect.Width, ARect.Height);
End;

Procedure TBZBitmapTransformations.Crop;
Begin
  CropRect(OwnerBitmap.ClipRect);
End;

// NEW buffer of dimensions ceil(sqrt(W^2+H^2)/2) and 2·(W+H-2).

// cf : https://fr.mathworks.com/matlabcentral/fileexchange/17933-polar-to-from-rectangular-transform-of-images
procedure TBZBitmapTransformations.CartesianToPolar;
var
  x,y,dx,dy : Integer;
  dr, dt, r, theta, dtheta, SinAngle, CosAngle: Single;
  InColor : TBZColor;
  TmpBmp : TBZBitmap;
begin
  dr := Sqrt(OwnerBitmap.CenterX * OwnerBitmap.CenterX + OwnerBitmap.CenterY * OwnerBitmap.CenterY) / OwnerBitmap.Height;
  dTheta  := 360 / OwnerBitmap.Width;
  dt := DegToRadian(dTheta);
  r := dr;
  TmpBmp := TBZBitmap.Create(OwnerBitmap.Width, OwnerBitmap.Height);


  InColor := OwnerBitmap.GetPixel(OwnerBitmap.CenterX,OwnerBitmap.CenterY);
  for x := 0 to OwnerBitmap.MaxWidth do TmpBmp.setPixel(x,0, InColor);

  for y := 0 to OwnerBitmap.MaxHeight do
  begin
    theta := 0.0;
    for x := 0 to OwnerBitmap.MaxWidth do
    begin
      CosAngle := System.Cos(Theta);
      SinAngle := System.Sin(Theta);

      dx := round(OwnerBitmap.CenterX + r * CosAngle);
      dy := round(OwnerBitmap.CenterY + r * SinAngle);

      if OwnerBitmap.CheckPixelBound(dx, dy) then
      begin
        InColor := OwnerBitmap.getPixel(dx,dy)
      end
      else InColor := clrTransparent;

      TmpBmp.setPixel(x,y,InColor);

      Theta := Theta + dt;
    end;
    r := r + dr;
  end;
  OwnerBitmap.FastCopy(TmpBmp);
  FreeAndNil(TmpBmp);
end;

procedure TBZBitmapTransformations.PolarToCartesian;
var
  ii,jj,Ind1, Ind2, m,n,x,y,px,py : Integer;
  weight, dr, dt, r, theta, dtheta, SinAngle, CosAngle, dx, dy  : Single;
  AColor: TBZColor;
  xs,ys,ddx,ddy,cx,cy : Integer;
  TmpBmp : TBZBitmap;
Const
  BiCubicRPrecal: Array[1..16] Of Single = (0.00260416666666667, 0.0208333333333333, 0.0703125, 0.166666666666667,
    0.315104166666667, 0.479166666666667, 0.611979166666667, 0.666666666666667,
    0.611979166666667, 0.479166666666667, 0.315104166666667, 0.166666666666667,
    0.0703125, 0.0208333333333333, 0.00260416666666667, 0.0);

begin
  dr := System.Sqrt(OwnerBitmap.CenterX * OwnerBitmap.CenterX + OwnerBitmap.CenterY * OwnerBitmap.CenterY) / OwnerBitmap.Height;
  dTheta  := 360 / OwnerBitmap.Width;
  dt := DegToRadian(dTheta);
  r := dr;
  TmpBmp := TBZBitmap.Create(OwnerBitmap.Width, OwnerBitmap.Height);
  for y := 0 to OwnerBitmap.MaxHeight do
  begin
    theta := 0.0;
    for x := 0 to OwnerBitmap.MaxWidth do
    begin

      CosAngle := System.Cos(Theta);
      SinAngle := System.Sin(Theta);

      dx := (OwnerBitmap.CenterX + r * CosAngle);
      dy := (OwnerBitmap.CenterY + r * SinAngle);
      px := Round(dx);
      py := round(dy);

      if TmpBmp.CheckPixelBound(px, py) then
      begin
        AColor := OwnerBitmap.getPixel(x, y);

        xs := Ceil(dx - dt);
        ys := Ceil(dy - dr);

        cx := Floor(dx + dt);
        cy := Floor(dy + dr);

        ddx := cx - xs;
        ddy := cy - ys;

        For m := -1 To 2 Do
        Begin
          For n := -1 To 2 Do
          Begin
            Ind1 := round(4 * (m - ddx)) + 8; // Indice de correspondance avec la table précalculée
            Ind2 := round(4 * (n - ddy)) + 8; // Idem
            // Evite un bug d'indice hors limites
            Ind1 := Max(Min(Ind1, 16), 1);
            Ind2 := Max(Min(Ind2, 16), 1);

            weight := BiCubicRPrecal[ind1]*BiCubicRPrecal[ind2];

            ii := Clamp((xs + m), 0, OwnerBitmap.MaxWidth);
            jj := Clamp((ys + n), 0, OwnerBitmap.MaxHeight);

            TmpBmp.setPixel(Floor(ii+m*weight),Floor(jj+n*weight),AColor);
          End;
        End;
      end;

      Theta := Theta + dt;
    end;
    r := r + dr;
  end;
  OwnerBitmap.FastCopy(TmpBmp);
  FreeAndNil(TmpBmp);
end;

{%endregion%}

{%region ====[ TBZBitmapColorFilters ]==========================================}

procedure TBZBitmapColorFilters.SwapChannel(Mode : TBZColorFilterSwapChannelMode);
Var
  Filter : TBZBitmapColorFilterSwapChannel;
Begin
  Filter := TBZBitmapColorFilterSwapChannel.Create(OwnerBitmap);
  Filter.OnProgress := Self.OnProgress;
  Filter.SwapMode := Mode;
  Filter.Render;
  FreeAndNil(Filter);
End;

procedure TBZBitmapColorFilters.Negate(
  const PreserveLuminosity : Boolean; const ApplyOnShadowTone : Boolean; const ApplyOnMidTone : Boolean; const ApplyOnHighlightTone : Boolean);
Var
  Filter : TBZBitmapColorFilterNegate;
Begin
  Filter := TBZBitmapColorFilterNegate.Create(OwnerBitmap);
  Filter.OnProgress := Self.OnProgress;
  Filter.ApplyOnShadowColor := ApplyOnShadowTone;
  Filter.ApplyOnMidColor := ApplyOnMidTone;
  Filter.ApplyOnHighlightColor := ApplyOnHighlightTone;
  Filter.PreserveLuminosity := PreserveLuminosity;
  Filter.Render;
  FreeAndNil(Filter);
End;

procedure TBZBitmapColorFilters.HyperSat;
Var
  Filter : TBZBitmapColorFilterHyperSat;
Begin
  Filter := TBZBitmapColorFilterHyperSat.Create(OwnerBitmap);
  Filter.OnProgress := Self.OnProgress;
  Filter.Render;
  FreeAndNil(Filter);
End;

procedure TBZBitmapColorFilters.Mix(aHoverColor : TBZColor; pct : Single);
Var
  Filter : TBZBitmapColorFilterMix;
Begin
  Filter := TBZBitmapColorFilterMix.Create(OwnerBitmap);
  Filter.OnProgress := Self.OnProgress;
  Filter.HoverColor := aHoverColor;
  Filter.Factor := pct;
  Filter.Render;
  FreeAndNil(Filter);
End;

procedure TBZBitmapColorFilters.MixInv(aHoverColor : TBZColor; pct : Single);
Var
  Filter : TBZBitmapColorFilterMixInv;
Begin
  Filter := TBZBitmapColorFilterMixInv.Create(OwnerBitmap);
  Filter.OnProgress := Self.OnProgress;
  Filter.HoverColor := aHoverColor;
  Filter.Factor := pct;
  Filter.Render;
  FreeAndNil(Filter);
End;

procedure TBZBitmapColorFilters.Average(AColor : TBZColor);
Var
  Filter : TBZBitmapColorFilterAverage;
Begin
  Filter := TBZBitmapColorFilterAverage.Create(OwnerBitmap);
  Filter.OnProgress := Self.OnProgress;
  Filter.HoverColor := AColor;
  Filter.Render;
  FreeAndNil(Filter);
End;

procedure TBZBitmapColorFilters.Modulate(AColor : TBZColor);
Var
  Filter : TBZBitmapColorFilterModulate;
Begin
  Filter := TBZBitmapColorFilterModulate.Create(OwnerBitmap);
  Filter.OnProgress := Self.OnProgress;
  Filter.HoverColor := AColor;
  Filter.Render;
  FreeAndNil(Filter);
End;

procedure TBZBitmapColorFilters.Colorize(ByAColor : TBZColor);
Var
  Table:    Array[0..255] Of TBZColor;
  DstColor: TBZColor;
  x, y, x1, y1, l, h: Integer;
  totalsize, nextLineinc: Integer;
  PixelPtr: PBZColor;
  AColor:   TBZColor;
  i, j:     Integer;
Begin
  For i := 0 To 255 Do
  Begin
    {%H-}Table[i].red := clampbyte(max(i + ByAColor.Red, ByAColor.Red));
    {%H-}Table[i].green := clampbyte(max(i + ByAColor.Green, ByAColor.Green));
    {%H-}Table[i].blue := clampbyte(max(i + ByAColor.Blue, ByAColor.Blue));
  End;
  nextlineInc := 0;
  TotalSize := 0;
  x := 0;
  y := 0;
  x1 := 0;
  y1 := 0;
  l := 0;
  h := 0;
  getClippingBoundInfos(x, y, x1, y1, l, h, totalsize, nextlineinc);

  i := 0;
  j := 0;

  PixelPtr := OwnerBitmap.GetPixelPtr(X, Y);
  Repeat
    AColor := PBZColor(PixelPtr)^;
    DstColor.Red := Table[AColor.Red].Red;
    DstColor.Green := Table[AColor.Green].Green;
    DstColor.Blue := Table[AColor.Blue].Blue;
    DstColor.Alpha := AColor.Alpha;
    PixelPtr^ := DstColor;
    Inc(PixelPtr);
    Inc(j);
    If j > l Then
    Begin
      j := 0;
      If NextLineInc > 0 Then
        Inc(PixelPtr, NextLineInc);
    End;
    Inc(i);
  Until i > totalsize;

End;

procedure TBZBitmapColorFilters.GrayScale(
  const GrayMode : TBZGrayConvertMode; const GrayMatrix : TBZGrayMatrixType; const OptVal : Single; const GammaFactor : Single);
Var
  Filter : TBZBitmapColorFilterGrayScale;
Begin

  Filter := TBZBitmapColorFilterGrayScale.Create(OwnerBitmap);
  Filter.OnProgress := Self.OnProgress;
  Filter.Mode := GrayMode;
  Filter.Matrix := GrayMatrix;
  Filter.OptionalValue := OptVal;
  Filter.Render;
  FreeAndNil(Filter);
End;

procedure TBZBitmapColorFilters.AdjustBrightness(factor : Single; const ApplyOnShadowTone : Boolean; const ApplyOnMidTone : Boolean; const ApplyOnHighlightTone : Boolean);
Var
  Filter : TBZBitmapColorFilterAdjustBrightness;
Begin
  Filter := TBZBitmapColorFilterAdjustBrightness.Create(OwnerBitmap);
  Filter.OnProgress := Self.OnProgress;
  Filter.Factor := factor;
  Filter.ApplyOnShadowColor := ApplyOnShadowTone;
  Filter.ApplyOnMidColor := ApplyOnMidTone;
  Filter.ApplyOnHighlightColor := ApplyOnHighlightTone;
  Filter.PreserveLuminosity := False;
  Filter.Render;
  FreeAndNil(Filter);
End;

procedure TBZBitmapColorFilters.AdjustContrast(factor : Single; const PreserveLuminosity : Boolean; const ApplyOnShadowTone : Boolean; const ApplyOnMidTone : Boolean; const ApplyOnHighlightTone : Boolean);
Var
  Filter : TBZBitmapColorFilterAdjustContrast;
Begin
  Filter := TBZBitmapColorFilterAdjustContrast.Create(OwnerBitmap);
  Filter.OnProgress := Self.OnProgress;
  Filter.ApplyOnShadowColor := ApplyOnShadowTone;
  Filter.ApplyOnMidColor := ApplyOnMidTone;
  Filter.ApplyOnHighlightColor := ApplyOnHighlightTone;
  Filter.PreserveLuminosity := PreserveLuminosity;
  Filter.Factor := factor;
  Filter.Render;
  FreeAndNil(Filter);
End;

procedure TBZBitmapColorFilters.AutoAdjustContrast;
Var
  Filter : TBZBitmapColorFilterAutoAdjustContrast;
Begin
  Filter := TBZBitmapColorFilterAutoAdjustContrast.Create(OwnerBitmap);
  Filter.OnProgress := Self.OnProgress;
  Filter.Render;
  FreeAndNil(Filter);
end;

procedure TBZBitmapColorFilters.AdjustSaturation(Factor : Single; const LimitLuminosity : Boolean; const PreserveLuminosity : Boolean; const ApplyOnShadowTone : Boolean; const ApplyOnMidTone : Boolean; const ApplyOnHighlightTone : Boolean);
Var
  Filter : TBZBitmapColorFilterAdjustSaturation;
Begin
  Filter := TBZBitmapColorFilterAdjustSaturation.Create(OwnerBitmap);
  Filter.OnProgress := Self.OnProgress;
  Filter.Factor := factor;
  Filter.LimitLuminosity := LimitLuminosity;
  Filter.ApplyOnShadowColor := ApplyOnShadowTone;
  Filter.ApplyOnMidColor := ApplyOnMidTone;
  Filter.ApplyOnHighlightColor := ApplyOnHighlightTone;
  Filter.PreserveLuminosity := PreserveLuminosity;
  Filter.Render;
  FreeAndNil(Filter);
End;

procedure TBZBitmapColorFilters.GammaCorrection(
  const Factor : Single; const PreserveLuminosity : Boolean; const ApplyOnShadowTone : Boolean; const ApplyOnMidTone : Boolean; const ApplyOnHighlightTone : Boolean);
Var
  Filter : TBZBitmapColorFilterGammaCorrection;
Begin
  Filter := TBZBitmapColorFilterGammaCorrection.Create(OwnerBitmap);
  Filter.OnProgress := Self.OnProgress;
  Filter.Factor := factor;
  Filter.ApplyOnShadowColor := ApplyOnShadowTone;
  Filter.ApplyOnMidColor := ApplyOnMidTone;
  Filter.ApplyOnHighlightColor := ApplyOnHighlightTone;
  Filter.PreserveLuminosity := PreserveLuminosity;
  Filter.Render;
  FreeAndNil(Filter);
End;

procedure TBZBitmapColorFilters.Solarize(Factor : Single; const PreserveLuminosity : Boolean; const ApplyOnShadowTone : Boolean; const ApplyOnMidTone : Boolean; const ApplyOnHighlightTone : Boolean);
Var
  Filter : TBZBitmapColorFilterSolarize;
Begin
  Filter := TBZBitmapColorFilterSolarize.Create(OwnerBitmap);
  Filter.OnProgress := Self.OnProgress;
  Filter.Factor := factor;
  Filter.ApplyOnShadowColor := ApplyOnShadowTone;
  Filter.ApplyOnMidColor := ApplyOnMidTone;
  Filter.ApplyOnHighlightColor := ApplyOnHighlightTone;
  Filter.PreserveLuminosity := PreserveLuminosity;
  Filter.Render;
  FreeAndNil(Filter);
End;

procedure TBZBitmapColorFilters.SplitLight(Amount : Integer);
Var
  Filter : TBZBitmapColorFilterSplitLight;
Begin
  Filter := TBZBitmapColorFilterSplitLight.Create(OwnerBitmap);
  Filter.OnProgress := Self.OnProgress;
  Filter.Amount := Amount;
  Filter.Render;
  FreeAndNil(Filter);
End;

procedure TBZBitmapColorFilters.KeepRed(Factor : Single);
Var
  Filter : TBZBitmapColorFilterFilterColor;
Begin
  Filter := TBZBitmapColorFilterFilterColor.Create(OwnerBitmap);
  Filter.OnProgress := Self.OnProgress;
  Filter.InRangeMode := False;
  Filter.Factor := Factor;
  Filter.ColorMask := cmcRed;
  Filter.KeepMode := True;
  Filter.Render;
  FreeAndNil(Filter);
End;

procedure TBZBitmapColorFilters.KeepGreen(Factor : Single);
Var
  Filter : TBZBitmapColorFilterFilterColor;
Begin
  Filter := TBZBitmapColorFilterFilterColor.Create(OwnerBitmap);
  Filter.OnProgress := Self.OnProgress;
  Filter.InRangeMode := False;
  Filter.Factor := Factor;
  Filter.ColorMask := cmcGreen;
  Filter.KeepMode := True;
  Filter.Render;
  FreeAndNil(Filter);
End;

procedure TBZBitmapColorFilters.KeepBlue(Factor : Single);
Var
  Filter : TBZBitmapColorFilterFilterColor;
Begin
  Filter := TBZBitmapColorFilterFilterColor.Create(OwnerBitmap);
  Filter.OnProgress := Self.OnProgress;
  Filter.InRangeMode := False;
  Filter.Factor := Factor;
  Filter.ColorMask := cmcBlue;
  Filter.KeepMode := True;
  Filter.Render;
  FreeAndNil(Filter);
End;

procedure TBZBitmapColorFilters.KeepRedInRange(mini, maxi : Byte);
Var
  Filter : TBZBitmapColorFilterFilterColor;
Begin
  Filter := TBZBitmapColorFilterFilterColor.Create(OwnerBitmap);
  Filter.OnProgress := Self.OnProgress;
  Filter.InRangeMode := True;
  Filter.ColorMask := cmcRed;
  Filter.KeepMode := True;
  Filter.MinRange := mini;
  Filter.MaxRange := maxi;
  Filter.Render;
  FreeAndNil(Filter);
End;

procedure TBZBitmapColorFilters.KeepGreenInRange(mini, maxi : Byte);
Var
  Filter : TBZBitmapColorFilterFilterColor;
Begin
  Filter := TBZBitmapColorFilterFilterColor.Create(OwnerBitmap);
  Filter.OnProgress := Self.OnProgress;
  Filter.InRangeMode := True;
  Filter.ColorMask := cmcGreen;
  Filter.KeepMode := True;
  Filter.MinRange := mini;
  Filter.MaxRange := maxi;
  Filter.Render;
  FreeAndNil(Filter);
End;

procedure TBZBitmapColorFilters.KeepBlueInRange(mini, maxi : Byte);
Var
  Filter : TBZBitmapColorFilterFilterColor;
Begin
  Filter := TBZBitmapColorFilterFilterColor.Create(OwnerBitmap);
  Filter.OnProgress := Self.OnProgress;
  Filter.InRangeMode := True;
  Filter.ColorMask := cmcBlue;
  Filter.KeepMode := True;
  Filter.MinRange := mini;
  Filter.MaxRange := maxi;
  Filter.Render;
  FreeAndNil(Filter);
End;

procedure TBZBitmapColorFilters.FilterOnlyRedInRange(mini, maxi : Byte);
Var
  Filter : TBZBitmapColorFilterFilterColor;
Begin
  Filter := TBZBitmapColorFilterFilterColor.Create(OwnerBitmap);
  Filter.OnProgress := Self.OnProgress;
  Filter.InRangeMode := True;
  Filter.ColorMask := cmcRed;
  Filter.KeepMode := False;
  Filter.MinRange := mini;
  Filter.MaxRange := maxi;
  Filter.Render;
  FreeAndNil(Filter);
End;

procedure TBZBitmapColorFilters.FilterOnlyGreenInRange(mini, maxi : Byte);
Var
  Filter : TBZBitmapColorFilterFilterColor;
Begin
  Filter := TBZBitmapColorFilterFilterColor.Create(OwnerBitmap);
  Filter.OnProgress := Self.OnProgress;
  Filter.InRangeMode := True;
  Filter.ColorMask := cmcGreen;
  Filter.KeepMode := False;
  Filter.MinRange := mini;
  Filter.MaxRange := maxi;
  Filter.Render;
  FreeAndNil(Filter);
End;

procedure TBZBitmapColorFilters.FilterOnlyBlueInRange(mini, maxi : Byte);
Var
  Filter : TBZBitmapColorFilterFilterColor;
Begin
  Filter := TBZBitmapColorFilterFilterColor.Create(OwnerBitmap);
  Filter.OnProgress := Self.OnProgress;
  Filter.InRangeMode := True;
  Filter.ColorMask := cmcBlue;
  Filter.KeepMode := False;
  Filter.MinRange := mini;
  Filter.MaxRange := maxi;
  Filter.Render;
  FreeAndNil(Filter);
End;

procedure TBZBitmapColorFilters.ExcludeColor(AColor : TBZColor);
Var
  Filter : TBZBitmapColorFilterExcludeColor;
Begin
  Filter := TBZBitmapColorFilterExcludeColor.Create(OwnerBitmap);
  Filter.OnProgress := Self.OnProgress;
  Filter.InRangeMode := False;
  Filter.MinRange := AColor;
  Filter.MaxRange := AColor;
  Filter.Render;
  FreeAndNil(Filter);
End;

procedure TBZBitmapColorFilters.ExtractColor(AColor : TBZColor);
Var
  Filter : TBZBitmapColorFilterExtractColor;
Begin
  Filter := TBZBitmapColorFilterExtractColor.Create(OwnerBitmap);
  Filter.OnProgress := Self.OnProgress;
  Filter.InRangeMode := False;
  Filter.MinRange := AColor;
  Filter.MaxRange := AColor;
  Filter.Render;
  FreeAndNil(Filter);
End;

procedure TBZBitmapColorFilters.ExcludeColorInRange(AMinColor, AMaxColor : TBZColor);
Var
  Filter : TBZBitmapColorFilterExcludeColor;
Begin
  Filter := TBZBitmapColorFilterExcludeColor.Create(OwnerBitmap);
  Filter.OnProgress := Self.OnProgress;
  Filter.InRangeMode := True;
  Filter.MinRange := AMinColor;
  Filter.MaxRange := AMaxColor;
  Filter.Render;
  FreeAndNil(Filter);
End;

procedure TBZBitmapColorFilters.ExtractColorInRange(AMinColor, AMaxColor : TBZColor);
Var
  Filter : TBZBitmapColorFilterExtractColor;
Begin
  Filter := TBZBitmapColorFilterExtractColor.Create(OwnerBitmap);
  Filter.OnProgress := Self.OnProgress;
  Filter.InRangeMode := True;
  Filter.MinRange := AMinColor;
  Filter.MaxRange := AMaxColor;
  Filter.Render;
  FreeAndNil(Filter);
End;

procedure TBZBitmapColorFilters.Posterize(Factor : Single; const PreserveLuminosity : Boolean; const ApplyOnShadowTone : Boolean; const ApplyOnMidTone : Boolean; const ApplyOnHighlightTone : Boolean);
Var
  Filter : TBZBitmapColorFilterPosterize;
Begin
  Filter := TBZBitmapColorFilterPosterize.Create(OwnerBitmap);
  Filter.OnProgress := Self.OnProgress;
  Filter.Factor := factor;
  Filter.ApplyOnShadowColor := ApplyOnShadowTone;
  Filter.ApplyOnMidColor := ApplyOnMidTone;
  Filter.ApplyOnHighlightColor := ApplyOnHighlightTone;
  Filter.PreserveLuminosity := PreserveLuminosity;
  Filter.Render;
  FreeAndNil(Filter);
End;

procedure TBZBitmapColorFilters.SetAlpha(Mode : TBZBitmapAlphaSetMode; const aTransparentColor : TBZColor);
Var
  Filter : TBZBitmapColorFilterModifyAlpha;
Begin
  Filter := TBZBitmapColorFilterModifyAlpha.Create(OwnerBitmap);
  Filter.OnProgress := Self.OnProgress;
  Filter.Mode := mode;
  Filter.TransparentColor := aTransparentColor;
  Filter.Render;
  FreeAndNil(Filter);
End;

procedure TBZBitmapColorFilters.Minimum;
Var
  Filter : TBZBitmapColorFilterMinimum;
Begin
  Filter := TBZBitmapColorFilterMinimum.Create(OwnerBitmap);
  Filter.OnProgress := Self.OnProgress;
  Filter.Render;
  FreeAndNil(Filter);
end;

procedure TBZBitmapColorFilters.Maximum;
Var
  Filter : TBZBitmapColorFilterMaximum;
Begin
  Filter := TBZBitmapColorFilterMaximum.Create(OwnerBitmap);
  Filter.OnProgress := Self.OnProgress;
  Filter.Render;
  FreeAndNil(Filter);
end;

procedure TBZBitmapColorFilters.GrayOut;
Var
  Filter : TBZBitmapColorFilterGrayOut;
Begin
  Filter := TBZBitmapColorFilterGrayOut.Create(OwnerBitmap);
  Filter.OnProgress := Self.OnProgress;
  Filter.Render;
  FreeAndNil(Filter);
end;

procedure TBZBitmapColorFilters.AdjustExposure(Factor : Single; const PreserveLuminosity : Boolean; const ApplyOnShadowTone : Boolean; const ApplyOnMidTone : Boolean; const ApplyOnHighlightTone : Boolean);
Var
  Filter : TBZBitmapColorFilterExposure;
Begin
  Filter := TBZBitmapColorFilterExposure.Create(OwnerBitmap);
  Filter.OnProgress := Self.OnProgress;
  Filter.Factor := Factor;
  Filter.ApplyOnShadowColor := ApplyOnShadowTone;
  Filter.ApplyOnMidColor := ApplyOnMidTone;
  Filter.ApplyOnHighlightColor := ApplyOnHighlightTone;
  Filter.PreserveLuminosity := PreserveLuminosity;
  Filter.Render;
  FreeAndNil(Filter);
end;

procedure TBZBitmapColorFilters.AdjustGain(FactorGain, FactorBias : Single; const PreserveLuminosity : Boolean; const ApplyOnShadowTone : Boolean; const ApplyOnMidTone : Boolean; const ApplyOnHighlightTone : Boolean);
Var
  Filter : TBZBitmapColorFilterGain;
Begin
  Filter := TBZBitmapColorFilterGain.Create(OwnerBitmap);
  Filter.OnProgress := Self.OnProgress;
  Filter.FactorGain := FactorGain;
  Filter.FactorBias := FactorBias;
  Filter.ApplyOnShadowColor := ApplyOnShadowTone;
  Filter.ApplyOnMidColor := ApplyOnMidTone;
  Filter.ApplyOnHighlightColor := ApplyOnHighlightTone;
  Filter.PreserveLuminosity := PreserveLuminosity;
  Filter.Render;
  FreeAndNil(Filter);
end;

procedure TBZBitmapColorFilters.AdjustRGB(FactorRed, FactorGreen, FactorBlue : Single; const PreserveLuminosity : Boolean; const ApplyOnShadowTone : Boolean; const ApplyOnMidTone : Boolean; const ApplyOnHighlightTone : Boolean);
Var
  Filter : TBZBitmapColorFilterAdjustRGB;
Begin
  Filter := TBZBitmapColorFilterAdjustRGB.Create(OwnerBitmap);
  Filter.OnProgress := Self.OnProgress;
  Filter.FactorRed := FactorRed;
  Filter.FactorGreen := FactorGreen;
  Filter.FactorBlue := FactorBlue;
  Filter.ApplyOnShadowColor := ApplyOnShadowTone;
  Filter.ApplyOnMidColor := ApplyOnMidTone;
  Filter.ApplyOnHighlightColor := ApplyOnHighlightTone;
  Filter.PreserveLuminosity := PreserveLuminosity;
  Filter.Render;
  FreeAndNil(Filter);
end;

procedure TBZBitmapColorFilters.AdjustHSV(FactorHue, FactorSaturation, FactorValue : Single; const ApplyOnShadowTone : Boolean; const ApplyOnMidTone : Boolean; const ApplyOnHighlightTone : Boolean);
Var
  Filter : TBZBitmapColorFilterAdjustHSV;
Begin
  Filter := TBZBitmapColorFilterAdjustHSV.Create(OwnerBitmap);
  Filter.OnProgress := Self.OnProgress;
  Filter.HueFactor := FactorHue;
  Filter.SaturationFactor := FactorSaturation;
  Filter.ValueFactor := FactorValue;
  Filter.ApplyOnShadowColor := ApplyOnShadowTone;
  Filter.ApplyOnMidColor := ApplyOnMidTone;
  Filter.ApplyOnHighlightColor := ApplyOnHighlightTone;
  Filter.PreserveLuminosity := False;
  Filter.Render;
  FreeAndNil(Filter);
end;

procedure TBZBitmapColorFilters.Boost(Factor : Single);
Var
  Filter : TBZBitmapColorFilterBoost;
Begin
  Filter := TBZBitmapColorFilterBoost.Create(OwnerBitmap);
  Filter.OnProgress := Self.OnProgress;
  Filter.Factor := Factor;
  Filter.Render;
  FreeAndNil(Filter);
end;

procedure TBZBitmapColorFilters.RemoveRedEye;
Var
  Filter : TBZBitmapColorFilterRemoveRedEye;
Begin
  Filter := TBZBitmapColorFilterRemoveRedEye.Create(OwnerBitmap);
  Filter.OnProgress := Self.OnProgress;
  Filter.Render;
  FreeAndNil(Filter);
end;


procedure TBZBitmapColorFilters.Sepia(Factor : Single);
Var
  Filter : TBZBitmapColorFilterSepia;
Begin
  Filter := TBZBitmapColorFilterSepia.Create(OwnerBitmap);
  Filter.OnProgress := Self.OnProgress;
  Filter.Scale := Factor;
  Filter.Render;
  FreeAndNil(Filter);
end;

procedure TBZBitmapColorFilters.ColorBalance(FactorCyanRed, FactorMagentaGreen, FactorYellowBlue : Single; const PreserveLuminosity : Boolean; const ApplyOnShadowTone : Boolean; const ApplyOnMidTone : Boolean; const ApplyOnHighlightTone : Boolean);
Var
  Filter : TBZBitmapColorFilterBalance;
Begin
  Filter := TBZBitmapColorFilterBalance.Create(OwnerBitmap);
  Filter.OnProgress := Self.OnProgress;
  Filter.CyanToRedFactor := FactorCyanRed;
  Filter.MagentaToGreenFactor := FactorMagentaGreen;
  Filter.YellowToBlueFactor := FactorYellowBlue;
  Filter.ApplyOnShadowColor := ApplyOnShadowTone;
  Filter.ApplyOnMidColor := ApplyOnMidTone;
  Filter.ApplyOnHighlightColor := ApplyOnHighlightTone;
  Filter.PreserveLuminosity := PreserveLuminosity;
  Filter.Render;
  FreeAndNil(Filter);
end;

procedure TBZBitmapColorFilters.ApplyColorMatrix(aColorMatrix : TBZColorMatrix);
Var
  Filter : TBZBitmapColorFilterMatrix;
Begin
  Filter := TBZBitmapColorFilterMatrix.Create(OwnerBitmap);
  Filter.OnProgress := Self.OnProgress;
  Filter.ColorMatrix := aColorMatrix;
  Filter.Render;
  FreeAndNil(Filter);
end;

procedure TBZBitmapColorFilters.HueRotate(ShiftDeg : Integer);
Var
  Filter : TBZBitmapColorFilterHueRotate;
Begin
  Filter := TBZBitmapColorFilterHueRotate.Create(OwnerBitmap);
  Filter.OnProgress := Self.OnProgress;
  Filter.Shift := ShiftDeg;
  Filter.Render;
  FreeAndNil(Filter);
end;

{%endregion%}

{%region ====[ TBZBitmapEffectFilters ]=========================================}

//procedure TBZBitmapEffectFilters.Mosaic(Src : TBZBitmap;xAmount,yAmount:Integer);
procedure TBZBitmapEffectFilters.Pixelate(xAmount,yAmount:Integer);
var
  Delta,
  tx,ty,
  cx,cy,ccy,
  ix,iy,
  x,y:   Integer;
  AColor:   TBZColor;
  DstPtr:  PBZColor;
  SrcBmp : TBZBitmap;
begin


  if(xAmount<1)or(yAmount<1)then Exit;
  ix:=(xAmount shr 1)+(xAmount and 1);
  iy:=(yAmount shr 1)+(yAmount and 1);

  SrcBmp:=TBZBitmap(OwnerBitmap).CreateClone;

  y:=0;
  while y<SrcBmp.Height do
  begin
    x:=0;
    cy:=y+iy;
    ccy:=cy;
    if cy>SrcBmp.MaxHeight then ccy:= SrcBmp.MaxHeight;
    //SrcPtr:=SrcBmp.GetScanLine(cy);

    if y+yAmount-1>SrcBmp.MaxHeight then ty:=SrcBmp.MaxHeight-y
    else ty:=yAmount;

    while x<SrcBmp.Width do
    begin
      cx:=x+ix;
     // ccx:=cx;
      if cx>SrcBmp.MaxWidth then AColor:=SrcBmp.GetPixel(SrcBmp.MaxWidth,ccy)
      else AColor:=SrcBmp.GetPixel(cx,ccy);

      if x+xAmount-1>SrcBmp.MaxWidth then tx:=SrcBmp.MaxWidth-x
      else tx:=xAmount;

      Delta:=SrcBmp.Width-tx;
      DstPtr:= OwnerBitmap.GetPixelPtr(x,y);
      for cy:=1 to ty do
      begin
        for cx:=1 to tx do
        begin
          DstPtr^:=AColor;
          Inc(DstPtr);
        end;
        Inc(DstPtr,Delta);
      end;
      Inc(x,xAmount);
    end;
    Inc(y,yAmount);
  end;
  FreeAndNil(SrcBmp);
end;

procedure TBZBitmapEffectFilters.InstagramFilter(aFilter : TBZInstagramFilterType);
Var
  //ContrastFilter : TBZBitmapColorFilterAdjustContrast;
  //BrighnessFilter : TBZBitmapColorFilterAdjustBrightness;
  //SaturationFilter : TBZBitmapColorFilterAdjustSaturation;
  //SepiaFilter : TBZBitmapColorFilterSepia;
  //GrayScaleFilter : TBZBitmapColorFilterGrayScale;
  TmpBmp1, TmpBmp2 : TBZBitmap;
  AColorMatrix : TBZColorMatrix;
  radiusX, radiusY : Integer;
  {$CODEALIGN VARMIN=16}
  pt1, pt2 : TBZFloatPoint;
  {$CODEALIGN VARMIN=4}
Begin
  Case aFilter of
    ift1977 :
    begin
      TmpBmp1 := TBZBitmap.Create(OwnerBitmap.Width, OwnerBitmap.Height);
      TmpBmp1.Clear(BZColor(243, 106, 188,77));

      TBZBitmap(OwnerBitmap).ColorFilter.OnProgress := Self.OnProgress;
      TBZBitmap(OwnerBitmap).ColorFilter.AdjustContrast(0.10);
      TBZBitmap(OwnerBitmap).ColorFilter.AdjustBrightness(0.10);
      TBZBitmap(OwnerBitmap).ColorFilter.AdjustSaturation(0.3);

      OwnerBitmap.PutImage(TmpBmp1,0,0,OwnerBitmap.Width, OwnerBitmap.Height,0,0,dmCombine,amAlphaBlendHQ,255,cmScreen);

      FreeAndNil(TmpBmp1);
    end;
    iftAden :
    begin
      TmpBmp1 := TBZBitmap.Create(OwnerBitmap.Width, OwnerBitmap.Height);

      TmpBmp1.Clear(clrTransparent);
      with TBZBitmap(TmpBmp1).Canvas do
      begin
        Pen.Style := ssClear;
        Brush.Style := bsGradient;
        Brush.Gradient.Kind := gkHorizontal;
        Brush.Gradient.ColorSteps.AddColorStop(BZColor(66, 10, 14, 51),0.0);
        Brush.Gradient.ColorSteps.AddColorStop(clrTransparent,1.0);
        Rectangle(0,0,OwnerBitmap.Width, OwnerBitmap.Height);
      end;

      TBZBitmap(OwnerBitmap).ColorFilter.OnProgress := Self.OnProgress;
      TBZBitmap(OwnerBitmap).ColorFilter.HueRotate(-20);
      TBZBitmap(OwnerBitmap).ColorFilter.AdjustContrast(-0.10);
      TBZBitmap(OwnerBitmap).ColorFilter.AdjustSaturation(-0.15);
      TBZBitmap(OwnerBitmap).ColorFilter.AdjustBrightness(0.20);

      OwnerBitmap.PutImage(TmpBmp1,0,0,OwnerBitmap.Width, OwnerBitmap.Height,0,0,dmCombine,amAlphaBlendHQ,255,cmDarkenOnly);

      FreeAndNil(TmpBmp1);
    end;
    iftAmaro :
    begin
      TBZBitmap(OwnerBitmap).ColorFilter.OnProgress := Self.OnProgress;
      TBZBitmap(OwnerBitmap).ColorFilter.AdjustContrast(-0.10);
      TBZBitmap(OwnerBitmap).ColorFilter.AdjustBrightness(0.10);
      TBZBitmap(OwnerBitmap).ColorFilter.AdjustSaturation(0.5);
      TBZBitmap(OwnerBitmap).ColorFilter.HueRotate(-10);
    end;
    iftAntique :
    begin
      TBZBitmap(OwnerBitmap).ColorFilter.OnProgress := Self.OnProgress;
      TBZBitmap(OwnerBitmap).ColorFilter.AdjustBrightness(-0.20);
      TBZBitmap(OwnerBitmap).ColorFilter.AdjustContrast(-0.20);
      TBZBitmap(OwnerBitmap).ColorFilter.Colorize(BZColor(75,50,25));
    end;
    iftBlackAndWhite :
    begin
      AColorMatrix.CreateBlackWhite;
      TBZBitmap(OwnerBitmap).ColorFilter.OnProgress := Self.OnProgress;
      TBZBitmap(OwnerBitmap).ColorFilter.GrayScale(gcmLuminance);
      TBZBitmap(OwnerBitmap).ColorFilter.ApplyColorMatrix(AColorMatrix);
    end;
    iftBrannan :
    begin
      TmpBmp1 := TBZBitmap.Create(OwnerBitmap.Width, OwnerBitmap.Height);
      TmpBmp1.Clear(BZColor(161, 44, 199,79));

      TBZBitmap(OwnerBitmap).ColorFilter.OnProgress := Self.OnProgress;
      TBZBitmap(OwnerBitmap).ColorFilter.Sepia(0.5);
      TBZBitmap(OwnerBitmap).ColorFilter.AdjustContrast(0.40);

      OwnerBitmap.PutImage(TmpBmp1,0,0,OwnerBitmap.Width, OwnerBitmap.Height,0,0,dmCombine,amAlphaBlendHQ,255,cmLightenOnly);

      FreeAndNil(TmpBmp1);
    end;
    iftBrooklyn :
    begin
      TmpBmp1 := TBZBitmap.Create(OwnerBitmap.Width, OwnerBitmap.Height);

      TmpBmp1.Clear(BZColor(196, 183, 200));
      with TBZBitmap(TmpBmp1).Canvas do
      begin
        Pen.Style := ssClear;
        Brush.Style := bsGradient;
        Brush.Gradient.Kind := gkRadial;
        Brush.Gradient.ColorSteps.AddColorStop(BZColor(168,223,193,102),0.0);
        Brush.Gradient.ColorSteps.AddColorStop(BZColor(168,223,193,102),0.7);
        Brush.Gradient.ColorSteps.AddColorStop(BZColor(196, 183, 200),1.0);
        pt2.Create(OwnerBitmap.Width,OwnerBitmap.Height);
        pt1.Create(OwnerBitmap.CenterX, OwnerBitmap.CenterY);
        RadiusX := Ceil(Abs(pt1.Distance(pt2)));
        Circle(OwnerBitmap.CenterX, OwnerBitmap.CenterY, RadiusX);
      end;

      TBZBitmap(OwnerBitmap).ColorFilter.OnProgress := Self.OnProgress;
      TBZBitmap(OwnerBitmap).ColorFilter.AdjustContrast(-0.10);
      TBZBitmap(OwnerBitmap).ColorFilter.AdjustBrightness(0.10);

      OwnerBitmap.PutImage(TmpBmp1,0,0,OwnerBitmap.Width, OwnerBitmap.Height,0,0,dmCombine,amAlphaBlendHQ,255,cmOverlay);

      FreeAndNil(TmpBmp1);
    end;
    iftClarendon :
    begin
      TmpBmp1 := TBZBitmap.Create(OwnerBitmap.Width, OwnerBitmap.Height);
      TmpBmp1.Clear(BZColor(127,187,227,51));

      OwnerBitmap.PutImage(TmpBmp1,0,0,OwnerBitmap.Width, OwnerBitmap.Height,0,0,dmCombine,amAlphaBlendHQ,51,cmOverlay);

      TBZBitmap(OwnerBitmap).ColorFilter.OnProgress := Self.OnProgress;
      TBZBitmap(OwnerBitmap).ColorFilter.AdjustContrast(0.20);
      TBZBitmap(OwnerBitmap).ColorFilter.AdjustSaturation(0.35);

      FreeAndNil(TmpBmp1);
    end;
    iftDream :
    begin
      TBZBitmap(OwnerBitmap).ColorFilter.OnProgress := Self.OnProgress;
      TBZBitmap(OwnerBitmap).ColorFilter.Colorize(BZColor(20, 0, 80, 155));
      TBZBitmap(OwnerBitmap).ColorFilter.Negate();
      TBZBitmap(OwnerBitmap).ColorFilter.Colorize(BZColor(0, 50, 0, 55));
      TBZBitmap(OwnerBitmap).ColorFilter.Negate();
      TBZBitmap(OwnerBitmap).BlurFilter.OnProgress := Self.OnProgress;
      TBZBitmap(OwnerBitmap).BlurFilter.GaussianBoxBlur(1.0);
    end;
    iftEasyBird :
    begin
      TmpBmp1 := TBZBitmap.Create(OwnerBitmap.Width, OwnerBitmap.Height);

      TmpBmp1.Clear(BZColor(29, 2, 16));
      with TBZBitmap(TmpBmp1).Canvas do
      begin
        Pen.Style := ssClear;
        Brush.Style := bsGradient;
        Brush.Gradient.Kind := gkRadial;
        Brush.Gradient.ColorSteps.AddColorStop(BZColor(208, 186, 142),0.0);
        Brush.Gradient.ColorSteps.AddColorStop(BZColor(208, 186, 142),0.2);
        Brush.Gradient.ColorSteps.AddColorStop(BZColor(54, 3, 9),0.80);
        Brush.Gradient.ColorSteps.AddColorStop(BZColor(29, 2, 16),1.0);
        pt2.Create(OwnerBitmap.Width,OwnerBitmap.Height);
        pt1.Create(OwnerBitmap.CenterX, OwnerBitmap.CenterY);
        RadiusX := Ceil(Abs(pt1.Distance(pt2)));
        Circle(OwnerBitmap.CenterX, OwnerBitmap.CenterY, RadiusX);
      end;

      TBZBitmap(OwnerBitmap).ColorFilter.OnProgress := Self.OnProgress;
      TBZBitmap(OwnerBitmap).ColorFilter.AdjustContrast(-0.10);
      TBZBitmap(OwnerBitmap).ColorFilter.Sepia(0.2);

      OwnerBitmap.PutImage(TmpBmp1,0,0,OwnerBitmap.Width, OwnerBitmap.Height,0,0,dmCombine,amAlphaBlendHQ,255,cmOverlay);

      FreeAndNil(TmpBmp1);
    end;
    iftEverGlow :
    begin
      TBZBitmap(OwnerBitmap).ColorFilter.OnProgress := Self.OnProgress;
      TBZBitmap(OwnerBitmap).ColorFilter.AdjustBrightness(-0.30);
      TBZBitmap(OwnerBitmap).ColorFilter.AdjustContrast(-0.10);
      TBZBitmap(OwnerBitmap).ColorFilter.Colorize(BZColor(30, 30, 0));
    end;
    iftFreshAqua :
    begin
      TBZBitmap(OwnerBitmap).ColorFilter.OnProgress := Self.OnProgress;
      TBZBitmap(OwnerBitmap).ColorFilter.AdjustContrast(-0.10);
      TBZBitmap(OwnerBitmap).ColorFilter.Colorize(BZColor(20, 0, 80,135));
    end;
    iftForest :
    begin
      TBZBitmap(OwnerBitmap).ColorFilter.OnProgress := Self.OnProgress;
      TBZBitmap(OwnerBitmap).ColorFilter.Colorize(BZColor(0, 0, 180, 165));
      TBZBitmap(OwnerBitmap).ColorFilter.Negate();
      TBZBitmap(OwnerBitmap).ColorFilter.Colorize(BZColor(0, 0, 180, 165));
      TBZBitmap(OwnerBitmap).ColorFilter.Negate();
      TBZBitmap(OwnerBitmap).BlurFilter.OnProgress := Self.OnProgress;
      TBZBitmap(OwnerBitmap).BlurFilter.GaussianBoxBlur(0.1);
    end;
    iftGingham :
    begin
      TmpBmp1 := TBZBitmap.Create(OwnerBitmap.Width, OwnerBitmap.Height);
      TmpBmp1.Clear(BZColor(230, 230, 250));

      TBZBitmap(OwnerBitmap).ColorFilter.OnProgress := Self.OnProgress;
      TBZBitmap(OwnerBitmap).ColorFilter.AdjustBrightness(0.05);
      TBZBitmap(OwnerBitmap).ColorFilter.HueRotate(-10);

      OwnerBitmap.PutImage(TmpBmp1,0,0,OwnerBitmap.Width, OwnerBitmap.Height,0,0,dmCombine,amAlphaBlendHQ,255,cmSoftLight);

      FreeAndNil(TmpBmp1);
    end;
    iftHudson :
    begin
      TmpBmp1 := TBZBitmap.Create(OwnerBitmap.Width, OwnerBitmap.Height);

      TmpBmp1.Clear(clrTransparent);
      with TBZBitmap(TmpBmp1).Canvas do
      begin
        Pen.Style := ssClear;
        Brush.Style := bsGradient;
        Brush.Gradient.Kind := gkRadial;
        Brush.Gradient.ColorSteps.AddColorStop(BZColor(166, 177, 255),0.0);
        Brush.Gradient.ColorSteps.AddColorStop(BZColor(166, 177, 255),0.5);
        Brush.Gradient.ColorSteps.AddColorStop(BZColor(52, 33, 52),1.0);
        pt2.Create(OwnerBitmap.Width,OwnerBitmap.Height);
        pt1.Create(OwnerBitmap.CenterX, OwnerBitmap.CenterY);
        RadiusX := Ceil(Abs(pt1.Distance(pt2)));
        Circle(OwnerBitmap.CenterX, OwnerBitmap.CenterY, RadiusX);
      end;

      TBZBitmap(OwnerBitmap).ColorFilter.OnProgress := Self.OnProgress;
      TBZBitmap(OwnerBitmap).ColorFilter.AdjustBrightness(0.20);
      TBZBitmap(OwnerBitmap).ColorFilter.AdjustContrast(-0.10);
      TBZBitmap(OwnerBitmap).ColorFilter.AdjustSaturation(0.10);

      OwnerBitmap.PutImage(TmpBmp1,0,0,OwnerBitmap.Width, OwnerBitmap.Height,0,0,dmCombine,amAlphaBlendHQ,128,cmMul);

      FreeAndNil(TmpBmp1);
    end;
    iftInkWell :
    begin
      TBZBitmap(OwnerBitmap).ColorFilter.OnProgress := Self.OnProgress;
      TBZBitmap(OwnerBitmap).ColorFilter.Sepia(0.3);
      TBZBitmap(OwnerBitmap).ColorFilter.AdjustContrast(0.10);
      TBZBitmap(OwnerBitmap).ColorFilter.AdjustBrightness(-0.10);
      TBZBitmap(OwnerBitmap).ColorFilter.GrayScale(gcmLuminance);
    end;
    iftJuno :
    begin
      TBZBitmap(OwnerBitmap).ColorFilter.OnProgress := Self.OnProgress;
      TBZBitmap(OwnerBitmap).ColorFilter.AdjustSaturation(0.8);
      TBZBitmap(OwnerBitmap).ColorFilter.AdjustBrightness(0.44);
      TBZBitmap(OwnerBitmap).ColorFilter.AdjustContrast(0.15);
      TBZBitmap(OwnerBitmap).ColorFilter.Sepia(0.35);
    end;
    iftKelvin :
    begin
       TmpBmp1 := TBZBitmap.Create(OwnerBitmap.Width, OwnerBitmap.Height);
       TmpBmp1.FastCopy(OwnerBitmap);

       TmpBmp2 := TBZBitmap.Create(OwnerBitmap.Width, OwnerBitmap.Height);
       TmpBmp2.Clear(BZColor(183,125,33));

       OwnerBitmap.Clear(BZColor(56,44,52));

       OwnerBitmap.PutImage(TmpBmp1,0,0,OwnerBitmap.Width, OwnerBitmap.Height,0,0,dmCombine,amAlphaBlendHQ,255,cmInverseColorDodge);
       OwnerBitmap.PutImage(TmpBmp2,0,0,OwnerBitmap.Width, OwnerBitmap.Height,0,0,dmCombine,amAlphaBlendHQ,255,cmOverlay);

       FreeAndNil(TmpBmp1);
       FreeAndNil(TmpBmp2);
     end;
    iftLark :
    begin
       TmpBmp1 := TBZBitmap.Create(OwnerBitmap.Width, OwnerBitmap.Height);
       TmpBmp1.FastCopy(OwnerBitmap);

       TmpBmp2 := TBZBitmap.Create(OwnerBitmap.Width, OwnerBitmap.Height);
       TmpBmp2.Clear(BZColor(242,242,242));

       OwnerBitmap.Clear(BZColor(34,37,63));

       OwnerBitmap.PutImage(TmpBmp1,0,0,OwnerBitmap.Width, OwnerBitmap.Height,0,0,dmCombine,amAlphaBlendHQ,255,cmInverseColorDodge);

       TBZBitmap(OwnerBitmap).ColorFilter.OnProgress := Self.OnProgress;
       TBZBitmap(OwnerBitmap).ColorFilter.AdjustContrast(-0.10);

       OwnerBitmap.PutImage(TmpBmp2,0,0,OwnerBitmap.Width, OwnerBitmap.Height,0,0,dmCombine,amAlphaBlendHQ,204,cmDarkenOnly);

       FreeAndNil(TmpBmp1);
       FreeAndNil(TmpBmp2);
     end;
    iftLight :
    begin
      TBZBitmap(OwnerBitmap).ColorFilter.OnProgress := Self.OnProgress;
      TBZBitmap(OwnerBitmap).ColorFilter.AdjustBrightness(0.10);
      TBZBitmap(OwnerBitmap).ColorFilter.Colorize(BZColor(100, 50, 0, 235));
    end;
    iftLime :
    begin
       AColorMatrix.m11 := 1.0;  AColorMatrix.m12 := 0.0; AColorMatrix.m13 := 0.0; AColorMatrix.m14 := 0.0; AColorMatrix.m51 := 0.0;
       AColorMatrix.m21 := 0.0;  AColorMatrix.m22 := 2.0; AColorMatrix.m23 := 0.0; AColorMatrix.m24 := 0.0; AColorMatrix.m52 := 0.0;
       AColorMatrix.m31 := 0.0;  AColorMatrix.m32 := 0.0; AColorMatrix.m33 := 0.0; AColorMatrix.m34 := 0.0; AColorMatrix.m53 := 0.0;
       AColorMatrix.m41 := 0.0;  AColorMatrix.m42 := 0.0; AColorMatrix.m43 := 0.0; AColorMatrix.m44 := 1.0; AColorMatrix.m54 := 0.0;
      //
      TBZBitmap(OwnerBitmap).ColorFilter.OnProgress := Self.OnProgress;
      TBZBitmap(OwnerBitmap).ColorFilter.ApplyColorMatrix(AColorMatrix);
    end;
    iftLofi :
    begin
      TmpBmp1 := TBZBitmap.Create(OwnerBitmap.Width, OwnerBitmap.Height);

      TmpBmp1.Clear(clrBlack);
      with TBZBitmap(TmpBmp1).Canvas do
      begin
        Pen.Style := ssClear;
        Brush.Style := bsGradient;
        Brush.Gradient.Kind := gkRadial;
        Brush.Gradient.ColorSteps.AddColorStop(clrTransparent,0.0);
        Brush.Gradient.ColorSteps.AddColorStop(clrTransparent,0.7);
        Brush.Gradient.ColorSteps.AddColorStop(BZColor(17,17,17),1.0);
        pt2.Create(OwnerBitmap.Width,OwnerBitmap.Height);
        pt1.Create(OwnerBitmap.CenterX, OwnerBitmap.CenterY);
        RadiusX := Ceil(Abs(pt1.Distance(pt2)));
        Circle(OwnerBitmap.CenterX, OwnerBitmap.CenterY, RadiusX);
      end;

      TBZBitmap(OwnerBitmap).ColorFilter.OnProgress := Self.OnProgress;
      TBZBitmap(OwnerBitmap).ColorFilter.AdjustSaturation(0.1);
      TBZBitmap(OwnerBitmap).ColorFilter.AdjustContrast(0.5);

      OwnerBitmap.PutImage(TmpBmp1,0,0,OwnerBitmap.Width, OwnerBitmap.Height,0,0,dmCombine,amAlphaBlendHQ,255,cmMul);

      FreeAndNil(TmpBmp1);
    end;
    iftLudwig :
    begin
      TBZBitmap(OwnerBitmap).ColorFilter.OnProgress := Self.OnProgress;
      TBZBitmap(OwnerBitmap).ColorFilter.AdjustSaturation(1.0);
      TBZBitmap(OwnerBitmap).ColorFilter.Sepia(0.25);
      TBZBitmap(OwnerBitmap).ColorFilter.AdjustBrightness(0.44);
      TBZBitmap(OwnerBitmap).ColorFilter.AdjustContrast(0.05);
    end;
    iftMajesty :
    begin
      TBZBitmap(OwnerBitmap).ColorFilter.OnProgress := Self.OnProgress;
      TBZBitmap(OwnerBitmap).ColorFilter.AdjustBrightness(-0.20);
      TBZBitmap(OwnerBitmap).ColorFilter.AdjustContrast(-0.15);
      TBZBitmap(OwnerBitmap).ColorFilter.Colorize(BZColor(80,0,60));
    end;
    iftMaven:
    begin
      TmpBmp1 := TBZBitmap.Create(OwnerBitmap.Width, OwnerBitmap.Height);
      TmpBmp1.Clear(BZColor(3, 230, 26, 51));

      TBZBitmap(OwnerBitmap).ColorFilter.OnProgress := Self.OnProgress;

      TBZBitmap(OwnerBitmap).ColorFilter.Sepia(0.25);
      TBZBitmap(OwnerBitmap).ColorFilter.AdjustBrightness(-0.05);
      TBZBitmap(OwnerBitmap).ColorFilter.AdjustContrast(-0.05);
      TBZBitmap(OwnerBitmap).ColorFilter.AdjustSaturation(0.5);

      OwnerBitmap.PutImage(TmpBmp1,0,0,OwnerBitmap.Width, OwnerBitmap.Height,0,0,dmCombine,amAlphaBlendHQ,255,cmHue);

      FreeAndNil(TmpBmp1);
    end;
    iftMayfair:
    begin
      TmpBmp1 := TBZBitmap.Create(OwnerBitmap.Width, OwnerBitmap.Height);

      TmpBmp1.Clear(clrTransparent);
      with TBZBitmap(TmpBmp1).Canvas do
      begin
        Pen.Style := ssClear;
        Brush.Style := bsGradient;
        Brush.Gradient.Kind := gkRadial;
        Brush.Gradient.ColorSteps.AddColorStop(BZColor(255,255,255,204),0.0);
        Brush.Gradient.ColorSteps.AddColorStop(BZColor(255, 200, 200,153),0.30);
        Brush.Gradient.ColorSteps.AddColorStop(BZColor(17, 17, 17),0.60);
        Brush.Gradient.ColorSteps.AddColorStop(clrTransparent,1.00);
        pt2.Create(OwnerBitmap.Width,0);
        pt1.Create(OwnerBitmap.Width * 0.4, OwnerBitmap.Height * 0.4);
        RadiusX := Ceil(Abs(pt1.Distance(pt2)));
        pt2.Create(OwnerBitmap.Width, OwnerBitmap.Height);
        RadiusY := Ceil(Abs(pt1.Distance(pt2)));
        if RadiusY > RadiusX then
          Circle(Round(pt1.x), Round(pt1.y), RadiusY)
        else
          Circle(Round(pt1.x), Round(pt1.y), RadiusX);
      end;

      TBZBitmap(OwnerBitmap).ColorFilter.OnProgress := Self.OnProgress;
      TBZBitmap(OwnerBitmap).ColorFilter.AdjustContrast(0.10);
      TBZBitmap(OwnerBitmap).ColorFilter.AdjustSaturation(0.10);

      OwnerBitmap.PutImage(TmpBmp1,0,0,OwnerBitmap.Width, OwnerBitmap.Height,0,0,dmCombine,amAlphaBlendHQ,255,cmOverlay);

      FreeAndNil(TmpBmp1);
    end;
    iftMoon:
    begin
      TmpBmp1 := TBZBitmap.Create(OwnerBitmap.Width, OwnerBitmap.Height);

      TmpBmp2 := TBZBitmap.Create(OwnerBitmap.Width, OwnerBitmap.Height);
      TmpBmp2.Clear(BZColor(56,56,56));
      TmpBmp1.Clear(BZColor(160,160,160));

      OwnerBitmap.PutImage(TmpBmp1,0,0,OwnerBitmap.Width, OwnerBitmap.Height,0,0,dmCombine,amAlphaBlendHQ,255,cmSoftLight);

      TBZBitmap(OwnerBitmap).ColorFilter.OnProgress := Self.OnProgress;
      TBZBitmap(OwnerBitmap).ColorFilter.GrayScale(gcmLuminance);
      TBZBitmap(OwnerBitmap).ColorFilter.AdjustContrast(0.10);
      TBZBitmap(OwnerBitmap).ColorFilter.AdjustBrightness(0.10);

      OwnerBitmap.PutImage(TmpBmp2,0,0,OwnerBitmap.Width, OwnerBitmap.Height,0,0,dmCombine,amAlphaBlendHQ,255,cmLightenOnly);

      FreeAndNil(TmpBmp1);
      FreeAndNil(TmpBmp2);
    end;
    iftNashville:
    begin
      TmpBmp1 := TBZBitmap.Create(OwnerBitmap.Width, OwnerBitmap.Height);
      TmpBmp1.FastCopy(OwnerBitmap);

      TmpBmp2 := TBZBitmap.Create(OwnerBitmap.Width, OwnerBitmap.Height);
      TmpBmp2.Clear(BZColor(0,70,150));
      TmpBmp1.Clear(BZColor(247,176,153));

      OwnerBitmap.PutImage(TmpBmp1,0,0,OwnerBitmap.Width, OwnerBitmap.Height,0,0,dmCombine,amAlphaBlendHQ,142,cmDarkenOnly);

      TBZBitmap(OwnerBitmap).ColorFilter.OnProgress := Self.OnProgress;

      TBZBitmap(OwnerBitmap).ColorFilter.Sepia(0.2);
      TBZBitmap(OwnerBitmap).ColorFilter.AdjustContrast(0.2);
      TBZBitmap(OwnerBitmap).ColorFilter.AdjustBrightness(0.05);
      TBZBitmap(OwnerBitmap).ColorFilter.AdjustSaturation(0.2);

      OwnerBitmap.PutImage(TmpBmp2,0,0,OwnerBitmap.Width, OwnerBitmap.Height,0,0,dmCombine,amAlphaBlendHQ,102,cmLightenOnly);

      FreeAndNil(TmpBmp1);
      FreeAndNil(TmpBmp2);
    end;
    iftOldPhoto :
    begin
      AColorMatrix.CreateOldPhoto;
      TBZBitmap(OwnerBitmap).ColorFilter.OnProgress := Self.OnProgress;
      TBZBitmap(OwnerBitmap).ColorFilter.GrayScale(gcmLuminance);
      TBZBitmap(OwnerBitmap).ColorFilter.ApplyColorMatrix(AColorMatrix);
    end;
    iftPeachy :
    begin
      AColorMatrix.m11 := 1.0;  AColorMatrix.m12 := 0.0; AColorMatrix.m13 := 0.0; AColorMatrix.m14 := 0.0; AColorMatrix.m51 := 0.0;
      AColorMatrix.m21 := 0.0;  AColorMatrix.m22 := 0.5; AColorMatrix.m23 := 0.0; AColorMatrix.m24 := 0.0; AColorMatrix.m52 := 0.0;
      AColorMatrix.m31 := 0.0;  AColorMatrix.m32 := 0.0; AColorMatrix.m33 := 0.0; AColorMatrix.m34 := 0.0; AColorMatrix.m53 := 0.0;
      AColorMatrix.m41 := 0.0;  AColorMatrix.m42 := 0.0; AColorMatrix.m43 := 0.0; AColorMatrix.m44 := 1.0; AColorMatrix.m54 := 0.0;

      //
      TBZBitmap(OwnerBitmap).ColorFilter.OnProgress := Self.OnProgress;
      TBZBitmap(OwnerBitmap).ColorFilter.ApplyColorMatrix(AColorMatrix);
    end;
    iftPerpetua :
    begin
      TmpBmp1 := TBZBitmap.Create(OwnerBitmap.Width, OwnerBitmap.Height);

      TmpBmp1.Clear(BZColor(230, 193, 61));
      with TBZBitmap(TmpBmp1).Canvas do
      begin
        Pen.Style := ssClear;
        Brush.Style := bsGradient;
        Brush.Gradient.Kind := gkVertical;
        Brush.Gradient.ColorSteps.AddColorStop(BZColor(0, 91, 154),0.0);
        Brush.Gradient.ColorSteps.AddColorStop(BZColor(230, 193, 61),1.0);
        Rectangle(0,0,OwnerBitmap.Width, OwnerBitmap.Height);
      end;

      OwnerBitmap.PutImage(TmpBmp1,0,0,OwnerBitmap.Width, OwnerBitmap.Height,0,0,dmCombine,amAlphaBlendHQ,128,cmSoftLight);

      FreeAndNil(TmpBmp1);
    end;
    iftPolaroid :
    begin
      AColorMatrix.CreatePolaroid;
      TBZBitmap(OwnerBitmap).ColorFilter.OnProgress := Self.OnProgress;
      TBZBitmap(OwnerBitmap).ColorFilter.ApplyColorMatrix(AColorMatrix);
    end;
    iftPolaroidII :
    begin
      AColorMatrix.CreatePolaroidII;
      TBZBitmap(OwnerBitmap).ColorFilter.OnProgress := Self.OnProgress;
      TBZBitmap(OwnerBitmap).ColorFilter.ApplyColorMatrix(AColorMatrix);
    end;
    iftRise :
    begin
      TmpBmp1 := TBZBitmap.Create(OwnerBitmap.Width, OwnerBitmap.Height);
      TmpBmp2 := TBZBitmap.Create(OwnerBitmap.Width, OwnerBitmap.Height);

      TmpBmp1.Clear(clrTransparent);
      with TBZBitmap(TmpBmp1).Canvas do
      begin
        Pen.Style := ssClear;
        Brush.Style := bsGradient;
        Brush.Gradient.Kind := gkRadial;
        Brush.Gradient.ColorSteps.AddColorStop(BZColor(236, 205, 169,38), 0.0);
        Brush.Gradient.ColorSteps.AddColorStop(BZColor(236, 205, 169,38), 0.55);
        Brush.Gradient.ColorSteps.AddColorStop(BZColor(50, 30, 74,102), 1.0);
        pt2.Create(OwnerBitmap.Width,OwnerBitmap.Height);
        pt1.Create(OwnerBitmap.CenterX, OwnerBitmap.CenterY);
        RadiusX := Ceil(Abs(pt1.Distance(pt2)));
        Circle(OwnerBitmap.CenterX, OwnerBitmap.CenterY, RadiusX);
      end;

      TmpBmp2.Clear(clrTransparent);
      with TBZBitmap(TmpBmp2).Canvas do
      begin
        Pen.Style := ssClear;
        Brush.Style := bsGradient;
        Brush.Gradient.Kind := gkRadial;
        Brush.Gradient.ColorSteps.AddColorStop(BZColor(232, 197, 152,204),0.0);
        Brush.Gradient.ColorSteps.AddColorStop(clrTransparent,0.9);
        Brush.Gradient.ColorSteps.AddColorStop(clrTransparent,1.0);
        pt2.Create(OwnerBitmap.Width,OwnerBitmap.Height);
        pt1.Create(OwnerBitmap.CenterX, OwnerBitmap.CenterY);
        RadiusX := Ceil(Abs(pt1.Distance(pt2)));
        Circle(OwnerBitmap.CenterX, OwnerBitmap.CenterY, RadiusX);
      end;

      OwnerBitmap.PutImage(TmpBmp1,0,0,OwnerBitmap.Width, OwnerBitmap.Height,0,0,dmCombine,amAlphaBlendHQ,153,cmOverlay);

      TBZBitmap(OwnerBitmap).ColorFilter.OnProgress := Self.OnProgress;
      TBZBitmap(OwnerBitmap).ColorFilter.AdjustBrightness(0.05);
      TBZBitmap(OwnerBitmap).ColorFilter.Sepia(0.2);
      TBZBitmap(OwnerBitmap).ColorFilter.AdjustContrast(-0.10);
      TBZBitmap(OwnerBitmap).ColorFilter.AdjustSaturation(-0.10);

      OwnerBitmap.PutImage(TmpBmp2,0,0,OwnerBitmap.Width, OwnerBitmap.Height,0,0,dmCombine,amAlphaBlendHQ,204,cmMul);

      FreeAndNil(TmpBmp1);
      FreeAndNil(TmpBmp2);
    end;
    iftRetro :
    begin
      TBZBitmap(OwnerBitmap).ColorFilter.OnProgress := Self.OnProgress;
      TBZBitmap(OwnerBitmap).ColorFilter.GrayScale(gcmLuminance);
      TBZBitmap(OwnerBitmap).ColorFilter.Colorize(BZColor(100, 25, 25, 155));
    end;
    iftReyes:
    begin
      TmpBmp1 := TBZBitmap.Create(OwnerBitmap.Width, OwnerBitmap.Height);
      TmpBmp1.Clear(BZColor(239, 205, 173));

      TBZBitmap(OwnerBitmap).ColorFilter.OnProgress := Self.OnProgress;
      TBZBitmap(OwnerBitmap).ColorFilter.Sepia(0.22);
      TBZBitmap(OwnerBitmap).ColorFilter.AdjustBrightness(0.10);
      TBZBitmap(OwnerBitmap).ColorFilter.AdjustContrast(-0.15);
      TBZBitmap(OwnerBitmap).ColorFilter.AdjustSaturation(-0.25);

      OwnerBitmap.PutImage(TmpBmp1,0,0,OwnerBitmap.Width, OwnerBitmap.Height,0,0,dmCombine,amAlphaBlendHQ,127,cmSoftLight);

      FreeAndNil(TmpBmp1);
    end;
    iftSlumber:
    begin
      TmpBmp1 := TBZBitmap.Create(OwnerBitmap.Width, OwnerBitmap.Height);
      TmpBmp1.FastCopy(OwnerBitmap);

      TmpBmp2 := TBZBitmap.Create(OwnerBitmap.Width, OwnerBitmap.Height);
      TmpBmp2.Clear(BZColor(125,105,24,127));
      TmpBmp1.Clear(BZColor(69,41,12,102));

      OwnerBitmap.PutImage(TmpBmp1,0,0,OwnerBitmap.Width, OwnerBitmap.Height,0,0,dmCombine,amAlphaBlendHQ,255,cmLightenOnly);

      TBZBitmap(OwnerBitmap).ColorFilter.OnProgress := Self.OnProgress;
      TBZBitmap(OwnerBitmap).ColorFilter.AdjustSaturation(-0.44);
      TBZBitmap(OwnerBitmap).ColorFilter.AdjustBrightness(0.05);

      OwnerBitmap.PutImage(TmpBmp2,0,0,OwnerBitmap.Width, OwnerBitmap.Height,0,0,dmCombine,amAlphaBlendHQ,255,cmSoftLight);

      FreeAndNil(TmpBmp1);
      FreeAndNil(TmpBmp2);
    end;
    iftStinson:
    begin
      TmpBmp1 := TBZBitmap.Create(OwnerBitmap.Width, OwnerBitmap.Height);
      TmpBmp1.Clear(BZColor(240, 149, 128));

      OwnerBitmap.PutImage(TmpBmp1,0,0,OwnerBitmap.Width, OwnerBitmap.Height,0,0,dmCombine,amAlphaBlendHQ,50,cmSoftLight);

      TBZBitmap(OwnerBitmap).ColorFilter.OnProgress := Self.OnProgress;
      TBZBitmap(OwnerBitmap).ColorFilter.AdjustContrast(-0.25);
      TBZBitmap(OwnerBitmap).ColorFilter.AdjustSaturation(-0.15);
      TBZBitmap(OwnerBitmap).ColorFilter.AdjustBrightness(0.15);

      FreeAndNil(TmpBmp1);
    end;
    iftToaster :
    begin
      TmpBmp1 := TBZBitmap.Create(OwnerBitmap.Width, OwnerBitmap.Height);

      TmpBmp1.Clear(BZColor(59, 0, 59));
      with TBZBitmap(TmpBmp1).Canvas do
      begin
        Pen.Style := ssClear;
        Brush.Style := bsGradient;
        Brush.Gradient.Kind := gkRadial;
        Brush.Gradient.ColorSteps.AddColorStop(BZColor(128, 78, 15),0.0);
        Brush.Gradient.ColorSteps.AddColorStop(BZColor(59, 0, 59),0.90);
        Brush.Gradient.ColorSteps.AddColorStop(BZColor(59, 0, 59),1.0);
        pt2.Create(OwnerBitmap.Width,OwnerBitmap.Height);
        pt1.Create(OwnerBitmap.CenterX, OwnerBitmap.CenterY);

        RadiusX := Ceil(Abs(pt1.Distance(pt2)));
        Circle(OwnerBitmap.CenterX, OwnerBitmap.CenterY, RadiusX);
      end;

      OwnerBitmap.PutImage(TmpBmp1,0,0,OwnerBitmap.Width, OwnerBitmap.Height,0,0,dmCombine,amAlphaBlendHQ,255,cmScreen);

      TBZBitmap(OwnerBitmap).ColorFilter.OnProgress := Self.OnProgress;
      TBZBitmap(OwnerBitmap).ColorFilter.AdjustContrast(0.25);
      TBZBitmap(OwnerBitmap).ColorFilter.AdjustBrightness(-0.10);

      FreeAndNil(TmpBmp1);
    end;
    iftValencia:
    begin
      TmpBmp1 := TBZBitmap.Create(OwnerBitmap.Width, OwnerBitmap.Height);
      TmpBmp1.Clear(BZColor(58, 3, 57));

      TBZBitmap(OwnerBitmap).ColorFilter.OnProgress := Self.OnProgress;
      TBZBitmap(OwnerBitmap).ColorFilter.AdjustContrast(0.08);
      TBZBitmap(OwnerBitmap).ColorFilter.AdjustBrightness(0.08);
      TBZBitmap(OwnerBitmap).ColorFilter.Sepia(0.08);

      OwnerBitmap.PutImage(TmpBmp1,0,0,OwnerBitmap.Width, OwnerBitmap.Height,0,0,dmCombine,amAlphaBlendHQ,127,cmExclusion);

      FreeAndNil(TmpBmp1);
    end;
    iftVintage :
    begin
      TBZBitmap(OwnerBitmap).ColorFilter.OnProgress := Self.OnProgress;
      TBZBitmap(OwnerBitmap).ColorFilter.AdjustBrightness(0.10);
      TBZBitmap(OwnerBitmap).ColorFilter.GrayScale(gcmLuminance);
      TBZBitmap(OwnerBitmap).ColorFilter.Colorize(BZColor(40,10,15));
    end;
    iftWalden:
    begin
      TmpBmp1 := TBZBitmap.Create(OwnerBitmap.Width, OwnerBitmap.Height);
      TmpBmp1.Clear(BZColor(0, 68, 204));

      TBZBitmap(OwnerBitmap).ColorFilter.OnProgress := Self.OnProgress;
      TBZBitmap(OwnerBitmap).ColorFilter.AdjustBrightness(0.10);
      TBZBitmap(OwnerBitmap).ColorFilter.HueRotate(-10);
      TBZBitmap(OwnerBitmap).ColorFilter.Sepia(0.3);
      TBZBitmap(OwnerBitmap).ColorFilter.AdjustSaturation(0.6);

      OwnerBitmap.PutImage(TmpBmp1,0,0,OwnerBitmap.Width, OwnerBitmap.Height,0,0,dmCombine,amAlphaBlendHQ,77,cmScreen);

      FreeAndNil(TmpBmp1);
    end;
    iftWillow :
    begin
      TmpBmp1 := TBZBitmap.Create(OwnerBitmap.Width, OwnerBitmap.Height);

      TmpBmp2 := TBZBitmap.Create(OwnerBitmap.Width, OwnerBitmap.Height);
      TmpBmp2.Clear(BZColor(203,205,216));

      TmpBmp1.Clear(clrTransparent);
      with TBZBitmap(TmpBmp1).Canvas do
      begin
        Pen.Style := ssClear;
        Brush.Style := bsGradient;
        Brush.Gradient.Kind := gkRadial;
        Brush.Gradient.ColorSteps.AddColorStop(BZColor(212, 169, 175),0.0);
        Brush.Gradient.ColorSteps.AddColorStop(BZColor(212, 169, 175),0.55);
        Brush.Gradient.ColorSteps.AddColorStop(clrBlack,1.0);
        pt2.Create(OwnerBitmap.Width,0);
        pt1.Create(OwnerBitmap.Width * 0.4, OwnerBitmap.Height * 0.4);
        RadiusX := Ceil(Abs(pt1.Distance(pt2)));
        pt2.Create(OwnerBitmap.Width, OwnerBitmap.Height);
        RadiusY := Ceil(Abs(pt1.Distance(pt2)));
        if RadiusY > RadiusX then
          Circle(Round(pt1.x), Round(pt1.y), RadiusY)
        else
          Circle(Round(pt1.x), Round(pt1.y), RadiusX);
      end;

      TBZBitmap(OwnerBitmap).ColorFilter.OnProgress := Self.OnProgress;
      TBZBitmap(OwnerBitmap).ColorFilter.GrayScale(gcmLuminance);
      TBZBitmap(OwnerBitmap).ColorFilter.AdjustContrast(-0.05);
      TBZBitmap(OwnerBitmap).ColorFilter.AdjustBrightness(-0.10);

      OwnerBitmap.PutImage(TmpBmp1,0,0,OwnerBitmap.Width, OwnerBitmap.Height,0,0,dmCombine,amAlphaBlendHQ,255,cmOverlay);

      OwnerBitmap.PutImage(TmpBmp2,0,0,OwnerBitmap.Width, OwnerBitmap.Height,0,0,dmCombine,amAlphaBlendHQ,255,cmColor);

      FreeAndNil(TmpBmp1);
      FreeAndNil(TmpBmp2);
    end;
    iftXpro2 :
    begin
      TmpBmp1 := TBZBitmap.Create(OwnerBitmap.Width, OwnerBitmap.Height);

      TmpBmp1.Clear(clrTransparent);
      with TBZBitmap(TmpBmp1).Canvas do
      begin
        Pen.Style := ssClear;
        Brush.Style := bsGradient;
        Brush.Gradient.Kind := gkRadial;
        Brush.Gradient.ColorSteps.AddColorStop(BZColor(230, 231, 224),0.0);
        Brush.Gradient.ColorSteps.AddColorStop(BZColor(230, 231, 224),0.4);
        Brush.Gradient.ColorSteps.AddColorStop(BZColor(43,42,161,168),1.0);
        pt2.Create(OwnerBitmap.Width,OwnerBitmap.Height);
        pt1.Create(OwnerBitmap.CenterX, OwnerBitmap.CenterY);
        RadiusX := Ceil(Abs(pt1.Distance(pt2)));
        Circle(OwnerBitmap.CenterX, OwnerBitmap.CenterY, RadiusX);
      end;

      TBZBitmap(OwnerBitmap).ColorFilter.OnProgress := Self.OnProgress;
      TBZBitmap(OwnerBitmap).ColorFilter.Sepia(0.3);

      OwnerBitmap.PutImage(TmpBmp1,0,0,OwnerBitmap.Width, OwnerBitmap.Height,0,0,dmCombine,amAlphaBlendHQ,255,cmColorBurn);

      FreeAndNil(TmpBmp1);
    end;
  end;
end;

procedure TBZBitmapEffectFilters.Bloom(RadiusBlur : Single; BlendFactor : Single);
Var
  TmpBmp : TBZBitmap;
begin
  TmpBmp := TBZBitmap.Create;
  TmpBmp.Assign(OwnerBitmap);
  TmpBmp.BlurFilter.GaussianBoxBlur(RadiusBlur);
  OwnerBitmap.PutImageBlend(TmpBmp,0,0, cmScreen, Round(255 * BlendFactor));
end;

procedure TBZBitmapEffectFilters.Boost(Factor : Single);
Var
  Filter : TBZBitmapColorFilterBoost;
begin
  Filter := TBZBitmapColorFilterBoost.Create(OwnerBitmap);
  Filter.Factor := Factor;
  Filter.Render;
  FreeAndNil(Filter);
end;

procedure TBZBitmapEffectFilters.Gloss(RadiusBlur : Single; ThresholdFactor : Single; const BlendFactor : byte);
Var
  e1, e2, Ratio : Single;
  Luma, Norm, Graymax  : Byte;
  TmpMask, GradientMap : TBZBitmap;
  i, GrayThres  : Integer;
  PixPtr, DestPtr, GradPtr : PBZColor;
  inColor : TBZColor;
begin
  TmpMask := TBZBitmap.Create(OwnerBitmap.Width, OwnerBitmap.Height);
  TmpMask.Clear(clrTransparent);
  GradientMap := TBZBitmap.Create;
  GradientMap.Assign(OwnerBitmap);
  GradientMap.SegmentationFilter.GradientConvolution(defSobel);

  GrayMax := OwnerBitmap.getIntensityMax;
  GrayThres := ClampByte(Round(GrayMax * ThresHoldFactor));
  GrayThres := GrayThres * GrayThres;
  i := 0;
  PixPtr := OwnerBitmap.GetScanLine(0);
  DestPtr := TmpMask.GetScanLine(0);
  GradPtr := GradientMap.GetScanLine(0);
  //Delta := 1 / (GrayMax - GrayThres);
  While  (i <= TmpMask.MaxSize) do
  begin
    inColor := PixPtr^;
    Luma := inColor.Luminance;
    Norm := GradPtr^.Red;
    if ((luma * Norm) > GrayThres) then DestPtr^:= clrWhite;
    inc(i);
    inc(PixPtr);
    inc(DestPtr);
    inc(GradPtr);
  end;

  e1 := TmpMask.GetEnergy;
  TmpMask.BlurFilter.GaussianBoxBlur(RadiusBlur);
  e2 := TmpMask.GetEnergy;
  if e1 = 0 then ratio := 1.0 else ratio := 1.0 + (e2 / e1);
  TmpMask.ColorFilter.Boost(ratio);

  OwnerBitmap.PutImageBlend(TmpMask,0,0,cmScreen, BlendFactor);
  FreeAndNil(TmpMask);
  FreeAndNil(GradientMap);
end;

{%endregion}

{%region ====[ TBZBitmapBlurFilters ]===========================================}

{ TODO 1  -oBZBitmap -cFilter : Ajouter d'autres filtres à TBZBitmapBlurFilters (Split, Radial, Movement, Zoom ) }

procedure TBZBitmapBlurFilters.LinearBlur;
Var
  x1, y1, x2, y2, XSteps, YSteps, w, h: Integer;
  DstColor: TBZColor;
  xx, yy, totalsize, nextLineInc: Integer;
  Line0, Line2, PixelPtr, MaskPtr : PBZColor;
  AColor, AColor1, AColor0, AColor2: TBZColor;
  Delta : Single;
Begin
  OwnerBitmap.BeginUpdate;
  nextlineInc := 0;
  TotalSize := 0;
  x1 := 0;
  y1 := 0;
  x2 := 0;
  y2 := 0;
  w := 0;
  h := 0;
  getClippingBoundInfos(x1, y1, x2, y2, w, h, totalsize, nextLineInc);
  //Line0 := Owner.GetPixelPtr(X1, Y1);
  PixelPtr := OwnerBitmap.GetPixelPtr(X1, Y1); //Owner.GetPixelPtr(X1, Y1 + 1);
  //Line2 := Owner.GetPixelPtr(X1, Y1 + 2);
  XSteps := (x2 - x1) - 2;
  YSteps := (y2 - y1) - 2;
  totalsize := (XSteps * YSteps);
  Inc(XSteps);
  xx := 0;
  yy := 0;
  if (OwnerBitmap.UseSelectionMask) then //and (OwnerBitmap.ApplyMask)
  begin
    MaskPtr := OwnerBitmap.SelectionMask.GetPixelPtr(X1, Y1);
    While yy < TotalSize Do
    Begin
      if (MaskPtr^.Red > 0) then
      begin
        AColor1 := PBZColor(PixelPtr + 1)^;
        AColor := PBZColor(PixelPtr - 1)^;
        AColor0 := PixelPtr^; //PBZColor(Line0)^;
        //AColor2 := PBZColor(Line2)^;

        DstColor.Red := (AColor0.Red +  AColor.Red + AColor1.Red) Div 3;
        DstColor.Green := (AColor0.Green + AColor.Green + AColor1.Green) Div 3;
        DstColor.Blue := (AColor0.Blue + AColor.Blue + AColor1.Blue) Div 3;
        // Alpha
        DstColor.Alpha := ((MaskPtr^.Red + AColor0.Alpha) div 2); //AColor0.Alpha;

        PixelPtr^ := DstColor;
      End;

      Inc(PixelPtr);
      inc(MaskPtr);
      //Inc(Line0);
      //Inc(Line2);
      Inc(xx);
      If xx > XSteps Then
      Begin
        xx := 0;
        Inc(PixelPtr, NextLineInc);
        Inc(MaskPtr, NextLineInc);
        //Inc(Line0, NextLineInc);
        //Inc(Line2, NextLineInc);
      End;
      Inc(yy);
    End;
  End
  else
  begin
    InitProgress(w,h);
    StartProgressSection(0,'');
    StartProgressSection(100,'Flou linéaire');
    Delta := 100 / YSteps;
    While yy < TotalSize Do
    Begin
      AColor0 := PixelPtr^; // PBZColor(Line0)^;
      AColor1 := PBZColor(PixelPtr + 1)^;
      AColor := PBZColor(PixelPtr - 1)^;


      DstColor.Red := (AColor0.Red +  AColor.Red + AColor1.Red) div 3;
      DstColor.Green := (AColor0.Green + AColor.Green + AColor1.Green) div 3;
      DstColor.Blue := (AColor0.Blue +  AColor.Blue + AColor1.Blue) div 3;
      // Alpha
      DstColor.Alpha := AColor0.Alpha;

      PixelPtr^ := DstColor;
      Inc(PixelPtr);
      //Inc(Line0);
      //Inc(Line2);
      Inc(xx);
      If xx > XSteps Then
      Begin
        xx := 0;
        Inc(PixelPtr, NextLineInc);
        AdvanceProgress(Delta,0,1,True);
        //Inc(Line0, NextLineInc);
        //Inc(Line2, NextLineInc);
      End;
      Inc(yy);
    End;
    FinishProgressSection(False);
    FinishProgressSection(True);
  End;
  OwnerBitmap.EndUpdate;
end;

procedure TBZBitmapBlurFilters.FastBlur;
Var
  x1, y1, x2, y2, XSteps, YSteps, w, h: Integer;
  DstColor: TBZColor;
  xx, yy, totalsize, nextLineInc: Integer;
  Line0, Line2, PixelPtr, MaskPtr : PBZColor;
  AColor, AColor1, AColor0, AColor2: TBZColor;
  Delta : Single;
Begin
  OwnerBitmap.BeginUpdate;
  nextlineInc := 0;
  TotalSize := 0;
  x1 := 0;
  y1 := 0;
  x2 := 0;
  y2 := 0;
  w := 0;
  h := 0;
  getClippingBoundInfos(x1, y1, x2, y2, w, h, totalsize, nextLineInc);
  Line0 := OwnerBitmap.GetPixelPtr(X1, Y1);
  PixelPtr := OwnerBitmap.GetPixelPtr(X1, Y1+1);
  Line2 := OwnerBitmap.GetPixelPtr(X1, Y1 + 2);
  XSteps := (x2 - x1) ;
  YSteps := (y2 - y1) ;
  totalsize := (XSteps * YSteps);
  Inc(XSteps);
  xx := 0;
  yy := 0;
  if (OwnerBitmap.UseSelectionMask) then // and (OwnerBitmap.ApplyMask)
  begin
    MaskPtr := OwnerBitmap.SelectionMask.GetPixelPtr(X1, Y1);
    While yy < TotalSize Do
    Begin
      if (MaskPtr^.Red > 0) then
      begin
        AColor1 := PBZColor(PixelPtr + 1)^;
        AColor := PBZColor(PixelPtr - 1)^;
        AColor0 := PBZColor(Line0)^;
        AColor2 := PBZColor(Line2)^;

        DstColor.Red := (AColor0.Red + AColor2.Red + AColor.Red + AColor1.Red) Shr 2;
        DstColor.Green := (AColor0.Green + AColor2.Green + AColor.Green + AColor1.Green) Shr 2;
        DstColor.Blue := (AColor0.Blue + AColor2.Blue + AColor.Blue + AColor1.Blue) Shr 2;
        // Alpha
        DstColor.Alpha := ((MaskPtr^.Red + AColor0.Alpha) div 2);

        PixelPtr^ := DstColor;
      End;

      Inc(PixelPtr);
      inc(MaskPtr);
      Inc(Line0);
      Inc(Line2);
      Inc(xx);
      If xx > XSteps Then
      Begin
        xx := 0;
        Inc(PixelPtr, NextLineInc);
        Inc(MaskPtr, NextLineInc);
        Inc(Line0, NextLineInc);
        Inc(Line2, NextLineInc);
      End;
      Inc(yy);
    End;
  End
  else
  begin
    InitProgress(w,h);
    StartProgressSection(0,'');
    StartProgressSection(100,'Flou rapide');
    Delta := 100 / YSteps;
    While yy < TotalSize Do
    Begin
      AColor1 := PBZColor(PixelPtr + 1)^;
      AColor := PBZColor(PixelPtr - 1)^;
      AColor0 := PBZColor(Line0)^;
      AColor2 := PBZColor(Line2)^;

      DstColor.Red := (AColor0.Red + AColor2.Red + AColor.Red + AColor1.Red) Shr 2;
      DstColor.Green := (AColor0.Green + AColor2.Green + AColor.Green + AColor1.Green) Shr 2;
      DstColor.Blue := (AColor0.Blue + AColor2.Blue + AColor.Blue + AColor1.Blue) Shr 2;
      // Alpha
      DstColor.Alpha := AColor0.Alpha;

      //PixelPtr^
      PixelPtr^:=DstColor;
      Inc(PixelPtr);
      Inc(Line0);
      Inc(Line2);
      Inc(xx);
      If xx > XSteps Then
      Begin
        xx := 0;
        Inc(PixelPtr, NextLineInc);
        Inc(Line0, NextLineInc);
        Inc(Line2, NextLineInc);
        AdvanceProgress(Delta, 0,1,True);
      End;
      Inc(yy);
    End;
  End;
  FinishProgressSection(False);
  FinishProgressSection(True);
  OwnerBitmap.EndUpdate;
End;

procedure TBZBitmapBlurFilters.BoxBlur(Radius : Integer);
Var
  FilterBoxBlur : TBZBitmapFilterBoxBlur;
begin
  FilterBoxBlur := TBZBitmapFilterBoxBlur.Create(OwnerBitmap);
  FilterBoxBlur.OnProgress := Self.OnProgress;
  FilterBoxBlur.Radius := Radius;
  FilterBoxBlur.Render;
  FreeAndNil(FilterBoxBlur);
End;

procedure TBZBitmapBlurFilters.GaussianBlur(const Radius : Single; const Sigma : Single);
Var
  FilterGaussianBlur : TBZBitmapFilterGaussianBlur;
begin
  FilterGaussianBlur := TBZBitmapFilterGaussianBlur.Create(OwnerBitmap);
  FilterGaussianBlur.OnProgress := Self.OnProgress;
  FilterGaussianBlur.Radius := Radius;
  FilterGaussianBlur.Sigma := Sigma;
  FilterGaussianBlur.Render;
  FreeAndNil(FilterGaussianBlur);
End;

procedure TBZBitmapBlurFilters.SplitBlur(Amount : Integer);
Var
  P, Lin1, Lin2: PBZColor;
  C1, C2, C3, C4, MColor : TBZColor;
  x, y, cx: Integer;
  Delta : Single;
Begin
  OwnerBitmap.BeginUpdate;
  P := OwnerBitmap.getScanLine(0);
  InitProgress(OwnerBitmap.Width,OwnerBitmap.Height);
  StartProgressSection(0,'');
  StartProgressSection(100,'Flou découpé');
  Delta := 100 / OwnerBitmap.Height;
  For y := 0 To OwnerBitmap.MaxHeight Do
  Begin
    Lin1 := OwnerBitmap.GetScanLine(Clamp(y + Amount, 0, OwnerBitmap.MaxHeight));
    Lin2 := OwnerBitmap.GetScanLine(Clamp(y - Amount, 0, OwnerBitmap.MaxHeight));

    For x := 0 To OwnerBitmap.MaxWidth Do
    Begin
      if (OwnerBitmap.UseSelectionMask) then // and (OwnerBitmap.ApplyMask)
      begin
        MColor := OwnerBitmap.SelectionMask.GetPixel(X,Y);
        if (MColor.Red > 0) then
        begin
          cx := Clamp(x + Amount, 0, OwnerBitmap.MaxWidth);
          C1 := PBZColor(Lin1 + cx)^;
          C2 := PBZColor(Lin2 + cx)^;
          cx := Clamp(x - Amount, 0, OwnerBitmap.MaxWidth);
          C3 := PBZColor(Lin1 + cx)^;
          C4 := PBZColor(Lin2 + cx)^;

          P^.Red := (C1.Red + C2.Red + C3.Red + C4.Red) Shr 2; // ) * MColor.Red) div 255);
          P^.Green := (C1.Green + C2.Green + C3.Green + C4.Green) Shr 2;
          P^.Blue := (C1.Blue + C2.Blue + C3.Blue + C4.Blue) Shr 2;
          P^.Alpha := (C1.Alpha + C2.Alpha + C3.Alpha + C4.Alpha) Shr 2;
        End;
      End
      else
      begin
        cx := Clamp(x + Amount, 0, OwnerBitmap.MaxWidth);
        C1 := PBZColor(Lin1 + cx)^;
        C2 := PBZColor(Lin2 + cx)^;
        cx := Clamp(x - Amount, 0, OwnerBitmap.MaxWidth);
        C3 := PBZColor(Lin1 + cx)^;
        C4 := PBZColor(Lin2 + cx)^;
        P^.Red := (C1.Red + C2.Red + C3.Red + C4.Red) Shr 2;
        P^.Green := (C1.Green + C2.Green + C3.Green + C4.Green) Shr 2;
        P^.Blue := (C1.Blue + C2.Blue + C3.Blue + C4.Blue) Shr 2;
        P^.Alpha := (C1.Alpha + C2.Alpha + C3.Alpha + C4.Alpha) Shr 2;
      End;
      Inc(P);
    End;

    AdvanceProgress(Delta, 0,1,True);
  End;
  FinishProgressSection(False);
  FinishProgressSection(True);
  OwnerBitmap.EndUpdate;
End;

procedure TBZBitmapBlurFilters.GaussianSplitBlur(Amount : Integer);
Var
  i: Integer;
Begin
  OwnerBitmap.BeginUpdate;
  For i := Amount Downto 1 Do SplitBlur(i);
  OwnerBitmap.EndUpdate;
End;

procedure TBZBitmapBlurFilters.GaussianBoxBlur(Radius : Single);
Type
  TArrayOfInt = Array of Integer;
  Function ComputeGaussianBox(Sigma:Single;n : Integer): TArrayOfInt;
  var
    wIdeal : Single;
    w1,w2 : Integer;
    mIdeal : Single;
    m : Integer;
    i : Integer;

  begin
    wIdeal := System.sqrt((12*sigma*sigma/n)+1);  // Ideal averaging filter width
    w1 := Floor(wIdeal);
    if ((w1 mod 2) = 0) then dec(w1);
    w2 := w1 + 2;
    mIdeal := (12*sigma*sigma - n*w1*w1 - 4*n*w1 - 3*n)/(-4*w1 - 4);
    m := Round(mIdeal);
    SetLength(Result{%H-}, n);
    For i := 0 to n-1 do
    begin
      if (i < m) then Result[i] := w1 else Result[i] := w2;
    End;
  End;

Var
  Boxes : TArrayOfInt;
Begin
  Boxes := ComputeGaussianBox(Radius,3);
  TBZBitmap(OwnerBitmap).BlurFilter.BoxBlur(Boxes[0]);
  TBZBitmap(OwnerBitmap).BlurFilter.BoxBlur(Boxes[1]);
  TBZBitmap(OwnerBitmap).BlurFilter.BoxBlur(Boxes[2]);
  SetLength(Boxes, 0);
  Boxes := nil;
End;

procedure TBZBitmapBlurFilters.RadialBlur(Radius : Integer);
begin
  TBZBitmap(OwnerBitmap).Transformation.CartesianToPolar;
  TBZBitmap(OwnerBitmap).BlurFilter.MotionBlur(90,Radius,0,0);
  TBZBitmap(OwnerBitmap).Transformation.PolarToCartesian;
End;

procedure TBZBitmapBlurFilters.CircularBlur(Radius : Integer);
begin
  TBZBitmap(OwnerBitmap).Transformation.CartesianToPolar;
  TBZBitmap(OwnerBitmap).BlurFilter.MotionBlur(180,Radius,0,0);
  TBZBitmap(OwnerBitmap).Transformation.PolarToCartesian;
end;

procedure TBZBitmapBlurFilters.ZoomBlur(cx, cy : Integer; Radius, FactorPrecisionX, FactorPrecisionY : Integer);
Var
  FilterZoomBlur : TBZBitmapFilterZoomBlur;
begin
  FilterZoomBlur := TBZBitmapFilterZoomBlur.Create(OwnerBitmap);
  FilterZoomBlur.OnProgress := Self.OnProgress;
  FilterZoomBlur.Radius := Radius;
  FilterZoomBlur.CenterX := cx;
  FilterZoomBlur.CenterY := cy;
  FilterZoomBlur.FactorPrecisionX := FactorPrecisionX;
  FilterZoomBlur.FactorPrecisionY := FactorPrecisionY;
  FilterZoomBlur.Render;
  FreeAndNil(FilterZoomBlur);
end;

procedure TBZBitmapBlurFilters.RadialZoomBlur(cx, cy, Force : Integer; const Quality : Byte);
Var
  FilterRadialZoomBlur : TBZBitmapFilterRadialBlur;
begin
  FilterRadialZoomBlur := TBZBitmapFilterRadialBlur.Create(OwnerBitmap);
  FilterRadialZoomBlur.OnProgress := Self.OnProgress;
  FilterRadialZoomBlur.Radius := Force;
  FilterRadialZoomBlur.CenterX := cx;
  FilterRadialZoomBlur.CenterY := cy;
  FilterRadialZoomBlur.Quality := Quality;
  FilterRadialZoomBlur.Render;
  FreeAndNil(FilterRadialZoomBlur);
end;

procedure TBZBitmapBlurFilters.FXAABlur;
const
  	cFXAA_SPAN_MAX   : Single = 8.0;
    cFXAA_REDUCE_MUL : Single = 1.0 / 8.0;
    cFXAA_REDUCE_MIN : Single = 1.0 / 128.0;
Var
  c1 : TBZColor;
 {$CODEALIGN VARMIN=16}
 rgbNW, rgbNE, rgbSW, rgbSE, rgbM,cv,vc  : TBZVector4f; //TBZColor;
 luma, rgbA, rgbB, OutColor : TBZVector4f;
 pt : TBZVector2i;
 p,p1,p2, dir : TBZVector2f;
 ci : TBZVector4i;
 {$CODEALIGN VARMIN=4}

 x, y : LongWord;

 lumaSum, lumaNW, lumaNE, lumaSW, lumaSE, lumaM, lumaB, lumaMin, lumaMax : Single;
 dirReduce, rcpDirMin : Single;
 Delta : Single;

begin
  InitProgress(OwnerBitmap.Width,OwnerBitmap.Height);
  StartProgressSection(0,'');
  StartProgressSection(100,'Flou FXAA');
  Delta := 100 / OwnerBitmap.Height;
  luma.Create(0.299, 0.587, 0.114, 1.0);
  cv.Create( _FloatColorRatio, _FloatColorRatio, _FloatColorRatio, _FloatColorRatio);
  vc.Create(255,255,255,255);
  For y := 0 to OwnerBitmap.MaxHeight do
  begin
    For x := 0 to OwnerBitmap.MaxWidth do
    begin
       p.Create(x,y);
       // 1st stage - Find edge

       c1 := OwnerBitmap.getPixel(x-1,y-1);
       rgbNW.Create(c1.Red,c1.Green,c1.Blue,c1.Alpha);
       rgbNW := rgbNW * cv;
       c1 := OwnerBitmap.getPixel(x,y-1);
       rgbNE.Create(c1.Red,c1.Green,c1.Blue,c1.Alpha);
       rgbNE := rgbNE * cv;
       c1 := OwnerBitmap.getPixel(x-1,y+1);
       rgbSW.Create(c1.Red,c1.Green,c1.Blue,c1.Alpha);
       rgbSW := rgbSW * cv;
       c1 := OwnerBitmap.getPixel(x+1,y+1);
       rgbSE.Create(c1.Red,c1.Green,c1.Blue,c1.Alpha);
       rgbSE := rgbSE * cv;
       c1 := OwnerBitmap.getPixel(x,y);
       rgbM.Create(c1.Red,c1.Green,c1.Blue,c1.Alpha);
       rgbM := rgbM * cv;



       lumaNW := rgbNW.DotProduct(luma);
       lumaNE := rgbNE.DotProduct(luma);
       lumaSW := rgbSW.DotProduct(luma);
       lumaSE := rgbSE.DotProduct(luma);
       lumaM  := rgbM.DotProduct(luma);

       dir.x := -((lumaNW + lumaNE) - (lumaSW + lumaSE));
       dir.y :=  ((lumaNW + lumaSW) - (lumaNE + lumaSE));

       lumaSum := lumaNW + lumaNE + lumaSW + lumaSE;
       dirReduce := (max(lumaSum * (0.25 * cFXAA_REDUCE_MUL), cFXAA_REDUCE_MIN));
       rcpDirMin := 1.0 / (min(abs(dir.x), abs(dir.y)) + dirReduce);

       p1.Create(cFXAA_SPAN_MAX,cFXAA_SPAN_MAX);
       p2.Create(-cFXAA_SPAN_MAX,-cFXAA_SPAN_MAX);
       dir := dir * rcpDirMin;
       dir := (Dir.Max(p2).Min(p1)); // / RES //; min(vec2(FXAA_SPAN_MAX), max(vec2(-FXAA_SPAN_MAX), dir * rcpDirMin)) / RES;

       // 2nd stage - Blur
       p  := p + Dir;
       p1 := p * (-1);
       p2 := p * (1);

       pt := p1.Round;
       c1 := OwnerBitmap.getPixel(pt.x,pt.y);
       rgbNW.Create(c1.Red,c1.Green,c1.Blue,c1.Alpha);
       rgbNW := rgbNW * cv;
       pt := p2.Round;
       c1:= OwnerBitmap.getPixel(pt.x,pt.y);
       rgbNE.Create(c1.Red,c1.Green,c1.Blue,c1.Alpha);
       rgbNE := rgbNE * cv;

       p1 := p1 * (1.0);
       p2 := p2 * (-1.0);
       pt := p1.Round;
       c1 := OwnerBitmap.getPixel(pt.x,pt.y);
       rgbSW.Create(c1.Red,c1.Green,c1.Blue,c1.Alpha);
       rgbSW := rgbSW * cv;
       pt := p2.Round;
       c1 := OwnerBitmap.getPixel(pt.x,pt.y);
       rgbSE.Create(c1.Red,c1.Green,c1.Blue,c1.Alpha);
       rgbSE := rgbSE * cv;

       //rgbM  =  Owner.getPixel(x,y).AsColorVector;

       rgbA := (rgbNW + rgbNE);
       rgbB := rgbA + (rgbSW + rgbSE) * 0.25;
       rgbA := rgbA * 0.5;

       lumaB := rgbB.dotProduct(luma);

       OutColor := rgbB;
       lumaMin := min(lumaM, min(min(lumaNW, lumaNE), min(lumaSW, lumaSE)));
       lumaMax := max(lumaM, max(max(lumaNW, lumaNE), max(lumaSW, lumaSE)));
       if ((lumaB < lumaMin) and (lumaB > lumaMax)) then OutColor := rgbA ;

       OutColor := OutColor * vc;
       ci := OutColor.Round;
       c1.Red := ci.x; //(Round(OutColor.x * 255));
       c1.Green := ci.y; //(Round(OutColor.y * 255));
       c1.Blue := ci.z; // (Round(OutColor.z * 255));
       c1.Alpha := ci.w; //(Round(OutColor.w * 255));
       pt := p.Round;
       OwnerBitmap.SetPixel(pt.x,pt.y,c1);
    end;
    AdvanceProgress(Delta, 0,1,True);
  end;
  FinishProgressSection(False);
  FinishProgressSection(True);
end;

procedure TBZBitmapBlurFilters.ThresholdBlur(Radius : Single; ThresholdValue : Byte);
Var
  FilterThresholdBlur : TBZBitmapFilterThresholdBlur;
begin
  FilterThresholdBlur := TBZBitmapFilterThresholdBlur.Create(OwnerBitmap);
  FilterThresholdBlur.OnProgress := Self.OnProgress;
  FilterThresholdBlur.Radius := Radius;
  FilterThresholdBlur.Threshold := ThresholdValue;
  FilterThresholdBlur.Render;
  FreeAndNil(FilterThresholdBlur);
end;

procedure TBZBitmapBlurFilters.AverageBlur(Radius : Byte);
Var
  FilterAverageBlur : TBZBitmapFilterNonLinearBlur;
begin
  FilterAverageBlur := TBZBitmapFilterNonLinearBlur.Create(OwnerBitmap);
  FilterAverageBlur.OnProgress := Self.OnProgress;
  FilterAverageBlur.FilterSize := Radius;
  FilterAverageBlur.GetPixelSampleMethod := psmMean;
  FilterAverageBlur.PixelEdgeAction := peaClamp;
  FilterAverageBlur.Render;
  FreeAndNil(FilterAverageBlur);
end;

procedure TBZBitmapBlurFilters.MotionBlur(Direction : Single; Distance : Integer; ZoomFactor, Rotation : Single );
Var
  FilterMotionBlur : TBZBitmapFilterMotionBlur;
begin
  FilterMotionBlur := TBZBitmapFilterMotionBlur.Create(OwnerBitmap);
  FilterMotionBlur.OnProgress := Self.OnProgress;
  FilterMotionBlur.Direction := Direction;
  FilterMotionBlur.Distance := Distance;
  FilterMotionBlur.Zoom := ZoomFactor;
  FilterMotionBlur.Rotation := Rotation;
  FilterMotionBlur.Render;
  FreeAndNil(FilterMotionBlur);
end;


//procedure TBZBitmapBlurFilters.RadialBlur
//procedure TBZBitmapBlurFilters.WindBlur
//procedure TBZBitmapBlurFilters.ResampleBlur

{%endregion%}

{%region ====[ TBZBitmapConvolutionFilters ]====================================}

{ TODO 1  -oBZBitmap -cFilter : Ajouter d'autres filtres à TBZConvolutionBlurFilters (Kuwasara, Dilate, Erode, Contract, Expand ) }

constructor TBZBitmapConvolutionFilters.Create(const AOwner : TBZBaseBitmap);
begin
  inherited Create(AOwner);
  FCurrentFilter := '';
end;

procedure TBZBitmapConvolutionFilters.Convolve(
  const aMatrix : TBZDynSingleArray; const Size : Byte; const Divisor : Single; const Bias : Single; const Mode : TBZConvolutionFilterMode; const ApplyRed : Boolean; const ApplyGreen : Boolean; const ApplyBlue : Boolean);
Var
  DstPtr: PBZColor;
  moffset: Integer;
  X, Y, I, J, pX, pY: Integer;
  KernelValue, K: Single;
  KS, KSDiv2: Byte;
  RDiv, MaxWeight :   Single;
  SumRec, FColor, MaxWeightColor, OldColor : TBZColorVector; //TSumRec;
  MColor,AColor: TBZColor;

  TmpBmp : TBZBitmap;
  ComputePixel : Boolean;
  Delta:Single;
Begin
  KS := Size - 1;
  KSDiv2 := KS Div 2;
  RDiv := Divisor;
  TmpBmp := TBZBitmap.Create;
  TmpBmp.Assign(OwnerBitmap);

  InitProgress(OwnerBitmap.ClipRect.Width, OwnerBitmap.ClipRect.Height);
  StartProgressSection(0, ''); // On debute une nouvelle section globale
  Delta := 100 / OwnerBitmap.ClipRect.Height;
  StartProgressSection(100 ,'Filtre : ' + FCurrentFilter);
  //Owner.BeginUpdate;
    // Doit on calculer le Diviseur ?
  If RDiv = -1 Then
  Begin
    K := 0;
    For J := 0 To KS Do
      For I := 0 To KS Do
        K := K + (aMatrix[J * Size + I]); //abs
    RDiv := K;
    If RDiv = 0 Then RDiv := 1;
  End
  Else If RDiv = 0 Then RDiv := 1;
  RDiv := RDiv.Reciprocal;
  ComputePixel := True;
  For Y := OwnerBitmap.ClipRect.Top To OwnerBitmap.ClipRect.Bottom Do
  Begin
    DstPtr := OwnerBitmap.GetScanLine(Y);
    For X := OwnerBitmap.ClipRect.Left To OwnerBitmap.ClipRect.Right Do
    Begin
      if (OwnerBitmap.UseSelectionMask) then // and (OwnerBitmap.ApplyMask)
      begin
        MColor := OwnerBitmap.SelectionMask.GetPixel(X,Y);
        if (MColor.Red > 0) then ComputePixel := True else ComputePixel := False;
      end;
      if ComputePixel then
      begin
        // Remise à zero
        if (Mode = cfmErode) then SumRec.Create(1.0,1.0)
        else SumRec.Create(0,0);
        if (mode = cfmContract) then
        begin
          MaxWeight := 0;
          for J := 0 to KS do
          begin
            moffset := J * Size;
            for I := 0 to KS do
            begin
              MaxWeight := Max(MaxWeight,  aMatrix[moffset + I]);
            end;
          end;
          if MaxWeight > 255 then MaxWeight := 255;
          MaxWeight := MaxWeight * _FloatColorRatio;
          MaxWeightColor.Create(MaxWeight, MaxWeight);
        end;
        moffset := 0;
        For J := 0 To KS Do
        Begin
          // Position Source Y
          pY := Clamp(Y + J - KSDiv2, OwnerBitmap.ClipRect.Top, OwnerBitmap.ClipRect.Bottom);
          //moffset := J * Size;
          For I := 0 To KS Do
          Begin
            // Position Source X
            pX := Clamp(X + I - KSDiv2, OwnerBitmap.ClipRect.Left, OwnerBitmap.ClipRect.Right);
            KernelValue := aMatrix[moffset]; // + I Valeur dans la matrice
            FColor := TmpBmp.GetPixel(pX, pY).AsColorVector;
            OldColor := FColor;
            // Convolution standard
            Case mode of
              cfmNormal:
              begin
                FColor := FColor * KernelValue;
                SumRec := SumRec + FColor;
              end;
              cfmDilate:
              begin
                FColor := FColor + (KernelValue * _FloatColorRatio);
                SumRec := SumRec.Max(FColor);
                //SumRec.Alpha := AlphaTemp;
              end;
              cfmErode:
              begin
                FColor := FColor - (KernelValue * _FloatColorRatio);
                SumRec := SumRec.Min(FColor);
                //SumRec.Alpha := AlphaTemp;
              end;
              cfmExpand :
              begin
                FColor := FColor * KernelValue;
                SumRec := SumRec.Max(FColor);
              end;
              cfmContract :
              begin
                //OldColor := FColor;
                FColor := (MaxWeightColor - FColor); //(FColor - MaxWeightColor); //.abs;
                FColor := FColor xor OldColor; //clrFloatWhite;
                FColor := FColor * KernelValue;
                SumRec := SumRec.Max(FColor);
              end;
            end;
            inc(moffset);
          End;
        End;
        // Morphologie Erode/Dilate/Contract
        if (Mode = cfmNormal) then SumRec := SumRec * RDiv
        else SumRec := SumRec.Clamp(0.0,1.0);

        SumRec := SumRec + (Bias * _FloatColorRatio);
        if ApplyRed then FColor.x := SumRec.x else FColor.x := OldColor.x;
        if ApplyGreen then FColor.y := SumRec.y else FColor.y := OldColor.y;
        if ApplyBlue then FColor.z := SumRec.z else FColor.z := OldColor.z;
        FColor.w := OldColor.w;
        AColor.Create(FColor);

        DstPtr^ := AColor;
      End;
      Inc(DstPtr);
    End;
    AdvanceProgress(Delta,0,1,True);
  End;
  FinishProgressSection(False);
  FinishProgressSection(True);
  FreeAndNil(TmpBmp);
  //  Owner.EndUpdate;
End;

procedure TBZBitmapConvolutionFilters.Convolve(
  const aMatrix : TBZDynSingleArray; const Size : Byte; const FriendlyName : String; const Divisor : Single; const Bias : Single; const Mode : TBZConvolutionFilterMode; const ApplyRed : Boolean; const ApplyGreen : Boolean; const ApplyBlue : Boolean);
begin
  FCurrentFilter := FriendlyName;
  Convolve(aMatrix,Size,Divisor,Bias, Mode, ApplyRed, ApplyGreen, ApplyBlue);
end;

procedure TBZBitmapConvolutionFilters.Convolve(filter : TBZConvolutionFilter; const CustomDivisor : Integer; const CustomBias : Integer; const Mode : TBZConvolutionFilterMode; const ApplyRed : Boolean; const ApplyGreen : Boolean; const ApplyBlue : Boolean);
var
  bias, divisor : integer;
Begin
  Divisor := CustomDivisor;
  Bias := CustomBias;
  FCurrentFilter := Filter.Name;
  if CustomDivisor=-1 then divisor := filter.Divisor;
  if CustomBias = 0 then bias := filter.Bias;
  Case Filter.MatrixType Of
    mct3x3: Convolve(Filter.Matrix._3, 3, Divisor, Bias, Mode, ApplyRed, ApplyGreen, ApplyBlue);
    mct5x5: Convolve(Filter.Matrix._5, 5, Divisor, Bias, Mode, ApplyRed, ApplyGreen, ApplyBlue);
    mct7x7: Convolve(Filter.Matrix._7, 7, Divisor, Bias, Mode, ApplyRed, ApplyGreen, ApplyBlue);
  End;
End;

// Version modifié et adapté de Graphic32 :
// procedure SobelFilter(Dst, Src: TBitmap32; const UsePrewitt: Boolean = False); dans pqConvolution.pas
procedure TBZBitmapConvolutionFilters.DetectEdge(aMode : TBZDetectEdgeFilterMode);
Var
  FilterV, FilterH: TBZBitmap;
  I, TotalSize: Integer;
  DstPtr, FilterVPtr, FilterHPtr: PBZColor;
  VColor, HColor, DstColor: TBZColor;
  Delta : Single;

  Function ProcessColor(Const HVal, VVal: Cardinal): Cardinal;
  Var
    i: Integer;
  Begin
    i := Round(System.Sqrt((HVal * HVal) + (VVal * VVal)));
   // If i < 0 Then  i := 0
   // Else If i > 255 Then i := 255;
    Result := i;
  End;

Begin
 // SrcW := Owner.Width;
 // SrcH := Owner.Height;
  TotalSize := OwnerBitmap.MaxSize;
  I := 0;
 // Owner.BeginUpdate;
  FilterV := TBZBitmap.Create;
  FilterV.ConvolutionFilter.OnProgress := Self.OnProgress;
  FilterH := TBZBitmap.Create;
  FilterH.ConvolutionFilter.OnProgress := Self.OnProgress;

    FilterV.Assign(OwnerBitmap);
    FilterH.Assign(OwnerBitmap);
    Case aMode Of
      defPrewitt:
      Begin
        With BZConvolutionFilterPresets[17] Do
        Begin
          FilterH.ConvolutionFilter.Convolve(Matrix._3, MatrixSize, 'Prewitt horizontal', Divisor, Bias);
        End;
        With BZConvolutionFilterPresets[18] Do
        Begin
          FilterV.ConvolutionFilter.Convolve(Matrix._3, MatrixSize, 'Prewitt vertical', Divisor, Bias);
        End;
      End;
      defRoberts:
      Begin
        With BZConvolutionFilterPresets[19] Do
        Begin
          FilterH.ConvolutionFilter.Convolve(Matrix._3, MatrixSize, 'Roberts horizontal', Divisor, Bias);
        End;
        With BZConvolutionFilterPresets[20] Do
        Begin
          FilterV.ConvolutionFilter.Convolve(Matrix._3, MatrixSize, 'Roberts vertical', Divisor, Bias);
        End;
      End;
      defSobel:
      Begin
        With BZConvolutionFilterPresets[21] Do
        Begin
          FilterH.ConvolutionFilter.Convolve(Matrix._3, MatrixSize, 'Sobel horizontal', Divisor, Bias);
        End;
        With BZConvolutionFilterPresets[22] Do
        Begin
          FilterV.ConvolutionFilter.Convolve(Matrix._3, MatrixSize, 'Sobel vertical', Divisor, Bias);
        End;
      End;
      defKirsch:
      Begin
        With BZConvolutionFilterPresets[23] Do
        Begin
          FilterH.ConvolutionFilter.Convolve(Matrix._3, MatrixSize, 'Kirsch horizontal', Divisor, Bias);
        End;
        With BZConvolutionFilterPresets[24] Do
        Begin
          FilterV.ConvolutionFilter.Convolve(Matrix._3, MatrixSize, 'Kirsch vertical', Divisor, Bias);
        End;
      End;
      defScharr:
      Begin
        With BZConvolutionFilterPresets[25] Do
        Begin
          FilterH.ConvolutionFilter.Convolve(Matrix._3, MatrixSize, 'Scharr horizontal', Divisor, Bias);
        End;
        With BZConvolutionFilterPresets[26] Do
        Begin
          FilterV.ConvolutionFilter.Convolve(Matrix._3, MatrixSize, 'Scharr vertical', Divisor, Bias);
        End;
      End;
      defRobinson:
      Begin
        With BZConvolutionFilterPresets[27] Do
        Begin
          FilterH.ConvolutionFilter.Convolve(Matrix._3, MatrixSize, 'Robinson horizontal', Divisor, Bias);
        End;
        With BZConvolutionFilterPresets[28] Do
        Begin
          FilterV.ConvolutionFilter.Convolve(Matrix._3, MatrixSize, 'Robinson vertical', Divisor, Bias);
        End;
      End;
      defMDif:
      Begin
        With BZConvolutionFilterPresets[29] Do
        Begin
          FilterH.ConvolutionFilter.Convolve(Matrix._5, MatrixSize, 'MDif horizontal', Divisor, Bias);
        End;
        With BZConvolutionFilterPresets[30] Do
        Begin
          FilterV.ConvolutionFilter.Convolve(Matrix._5, MatrixSize, 'MDif vertical', Divisor, Bias);
        End;
      End;
      defLaplace:
      Begin
        With BZConvolutionFilterPresets[38] Do
        Begin
          FilterH.ConvolutionFilter.Convolve(Matrix._5, MatrixSize, 'Laplace horizontal', Divisor, Bias);
        End;
        With BZConvolutionFilterPresets[39] Do
        Begin
          FilterV.ConvolutionFilter.Convolve(Matrix._5, MatrixSize, 'Laplace vertical', Divisor, Bias);
        End;
      End;
    End;
    DstPtr := OwnerBitmap.GetScanLine(0);
    FilterHPtr := FilterH.GetScanLine(0);
    FilterVPtr := FilterV.GetScanLine(0);

    InitProgress(OwnerBitmap.ClipRect.Width, OwnerBitmap.ClipRect.Height);
    StartProgressSection(0, ''); // On debute une nouvelle section globale
    Delta := 100 / TotalSize;
    StartProgressSection(100 ,'Détection des contours');

    While I < TotalSize Do
    Begin
      VColor := FilterVPtr^;
      HColor := FilterHPtr^;
      DstColor.Red := processColor(VColor.Red, HColor.Red);
      DstColor.Green := processColor(VColor.Green, HColor.Green);
      DstColor.Blue := processColor(VColor.Blue, HColor.Blue);
      DstColor.Alpha := processColor(VColor.Alpha, HColor.Alpha);
      DstPtr^ := DstColor;
      Inc(FilterVPtr);
      Inc(FilterHPtr);
      Inc(DstPtr);
      Inc(I);
      AdvanceProgress(Delta,0,1,False);
    End;
 // Finally
    FreeAndNil(FilterH);
    FreeAndNil(FilterV);
    FinishProgressSection(False);
    FinishProgressSection(True);
 // End;
 // Owner.EndUpdate;
End;

{%endregion%}

{%region ====[ TBZBitmapDeformationFilters ]====================================}

Constructor TBZBitmapDeformationFilters.Create(Const AOwner : TBZBaseBitmap);
begin
  inherited Create(AOwner);

end;

procedure TBZBitmapDeformationFilters.PinchAndTwirl(ACenterX, ACenterY, ARadius, AnAngle : Integer; AnAmount : Single; Const EdgeAction : TBZPixelEdgeAction; Const SamplerMode : TBZGetPixelSampleMethod; Const SampleLevel : Byte);
Var
  Filter : TBZBitmapPinchFilter;
Begin
  Filter := TBZBitmapPinchFilter.Create(OwnerBitmap);
  Filter.FilterSize := SampleLevel;
  Filter.OnProgress := Self.OnProgress;
  Filter.CenterX := ACenterX;
  Filter.CenterY := ACenterY;
  Filter.Angle := AnAngle;
  Filter.Radius := ARadius;
  Filter.Amount := AnAmount;
  Filter.PixelEdgeAction := EdgeAction;
  Filter.GetPixelSampleMethod := SamplerMode;
  Filter.Render;
  FreeAndNil(Filter);
end;

procedure TBZBitmapDeformationFilters.Twirl(ACenterX, ACenterY, ARadius, AnAngle : Integer; Const EdgeAction : TBZPixelEdgeAction; Const SamplerMode : TBZGetPixelSampleMethod);
Var
  Filter : TBZBitmapTwirlFilter;
Begin
  Filter := TBZBitmapTwirlFilter.Create(OwnerBitmap);
  Filter.OnProgress := Self.OnProgress;
  Filter.CenterX := ACenterX;
  Filter.CenterY := ACenterY;
  Filter.Angle := AnAngle;
  Filter.Radius := ARadius;
  Filter.PixelEdgeAction := EdgeAction;
  Filter.GetPixelSampleMethod := SamplerMode;
  Filter.Render;
  FreeAndNil(Filter);
end;

procedure TBZBitmapDeformationFilters.FishEye(ACenterX, ACenterY, ARadius, AnAngle : Integer; Const EdgeAction : TBZPixelEdgeAction; Const SamplerMode : TBZGetPixelSampleMethod);
Var
  Filter : TBZBitmapFishEyeFilter;
Begin
  Filter := TBZBitmapFishEyeFilter.Create(OwnerBitmap);
  Filter.OnProgress := Self.OnProgress;
  Filter.CenterX := ACenterX;
  Filter.CenterY := ACenterY;
  Filter.Amount := AnAngle;
  Filter.Radius := ARadius;
  Filter.PixelEdgeAction := EdgeAction;
  Filter.GetPixelSampleMethod := SamplerMode;
  Filter.Render;
  FreeAndNil(Filter);
end;

procedure TBZBitmapDeformationFilters.WaveDistorsion(aMode : TBZWaveDistorsionMode; aWaveLengthX, AmpX, aWaveLengthY, AmpY : Single; Const EdgeAction : TBZPixelEdgeAction; Const SamplerMode : TBZGetPixelSampleMethod; Const SampleLevel : Byte);
Var
  Filter : TBZBitmapWaveDistorsionFilter;
Begin
  Filter := TBZBitmapWaveDistorsionFilter.Create(OwnerBitmap);
  Filter.OnProgress := Self.OnProgress;
  Filter.Mode := aMode;
  Filter.WaveLengthX := aWaveLengthX;
  Filter.WaveLengthY := aWaveLengthY;
  Filter.AmplitudeX := AmpX;
  Filter.AmplitudeY := AmpY;
  Filter.PixelEdgeAction := EdgeAction;
  Filter.GetPixelSampleMethod := SamplerMode;
  Filter.FilterSize := SampleLevel;
  Filter.Render;
  FreeAndNil(Filter);
end;

procedure TBZBitmapDeformationFilters.WaterRipple(ACenterX, ACenterY, ARadius : Integer; APeriod, AAmplitude, APhase : Single; Const EdgeAction : TBZPixelEdgeAction; Const SamplerMode : TBZGetPixelSampleMethod; Const SampleLevel : Byte);
Var
  Filter : TBZBitmapWaterRippleFilter;
Begin
  Filter := TBZBitmapWaterRippleFilter.Create(OwnerBitmap);
  Filter.OnProgress := Self.OnProgress;
  Filter.CenterX := ACenterX;
  Filter.CenterY := ACenterY;
  Filter.Radius := ARadius;
  Filter.WaveLength := APeriod;
  Filter.Amplitude := AAmplitude;
  Filter.Phase := APhase;
  Filter.PixelEdgeAction := EdgeAction;
  Filter.GetPixelSampleMethod := SamplerMode;
  Filter.FilterSize := SampleLevel;
  Filter.Render;
  FreeAndNil(Filter);
end;

procedure TBZBitmapDeformationFilters.Diffusion(aHorizontal, aVertical, aMinDistance, aMaxDistance : Single; Const EdgeAction : TBZPixelEdgeAction; Const SamplerMode : TBZGetPixelSampleMethod; Const SampleLevel : Byte);
Var
  Filter : TBZBitmapDiffusionFilter;
Begin
  Filter := TBZBitmapDiffusionFilter.Create(OwnerBitmap);
  Filter.OnProgress := Self.OnProgress;
  Filter.Horizontal := aHorizontal;
  Filter.Vertical := aVertical;
  Filter.MinDistance := aMinDistance;
  Filter.MaxDistance := aMaxDistance;
  //Filter.Phase := APhase;
  Filter.PixelEdgeAction := EdgeAction;
  Filter.GetPixelSampleMethod := SamplerMode;
  Filter.FilterSize := SampleLevel;
  Filter.Render;
  FreeAndNil(Filter);
end;

procedure TBZBitmapDeformationFilters.Polar(aMode : TBZPolarTransformationMode; ACenterX, ACenterY : Integer; Const EdgeAction : TBZPixelEdgeAction; Const SamplerMode : TBZGetPixelSampleMethod; Const SampleLevel : Byte);
Var
  Filter : TBZBitmapPolarTransformationFilter;
Begin
  Filter := TBZBitmapPolarTransformationFilter.Create(OwnerBitmap);
  Filter.OnProgress := Self.OnProgress;
  Filter.CenterX := ACenterX;
  Filter.CenterY := ACenterY;
  Filter.Mode := AMode;
  Filter.PixelEdgeAction := EdgeAction;
  Filter.GetPixelSampleMethod := SamplerMode;
  Filter.FilterSize := SampleLevel;
  Filter.Render;
  FreeAndNil(Filter);
end;

{%endregion%}

{%region ====[ TBZBitmapRenderFilters ]=========================================}

Procedure TBZBitmapRenderFilters.DrawChecker(Const X, Y, W, H : Integer; Const C1, C2 : TBZColor; Const Factor : Byte);
Var
  xb, yb, xdest, ydest, nbx, nby: Integer;
Begin
  OwnerBitmap.BeginUpdate;
  xdest := X;
  nbx := (W + Factor - 1) Div Factor;
  nby := (H + Factor - 1) Div Factor;
  With TBZBitmap(OwnerBitmap).Canvas Do
  Begin
    DrawMode.PixelMode := dmSet;
    Pen.Style := ssClear;
    Brush.Style := bsSolid;
  End;
  For xb := 0 To nbx - 1 Do
  Begin
    ydest := Y;
    For yb := 0 To nby - 1 Do
    Begin
      With TBZBitmap(OwnerBitmap).Canvas Do
      Begin
        If odd(xb + yb) Then
          Brush.Color := C1
        Else
          Brush.Color := C2;

        Rectangle(xdest, ydest, xdest + Factor, yDest + Factor);
      End;
      Inc(ydest, Factor);
    End;
    Inc(xdest, Factor);
  End;
  OwnerBitmap.EndUpdate;
End;

procedure TBZBitmapRenderFilters.DrawGrid(Const BackgroundColor, MainLineColor, SecondLineColor, AxisColorX, AxisColorY : TBZColor; Const CellSize : Byte);
Var
  MidCellSize : Byte;
  i, j, cx, cy : Integer;
  mx, my : Integer;
begin
  OwnerBitmap.Clear(BackgroundColor);
  mx := OwnerBitmap.Width div CellSize;
  my := OwnerBitmap.Height div CellSize;
  MidCellSize := CellSize div 2;
  with TBZBitmap(OwnerBitmap).canvas do
  begin
    pen.Style := ssSolid;
    cx := OwnerBitmap.CenterX;
    cy := OwnerBitmap.CenterY;
    for i := 1 to mx do
    begin
      if i mod 5 = 0 then pen.color := MainLineColor else pen.color := SecondLineColor;
      j := i * MidCellSize;
      moveto(cx+j, 0);
      lineto(cx+j, OwnerBitmap.MaxHeight);
      moveto(cx-j, 0);
      lineto(cx-j, OwnerBitmap.MaxHeight);
    end;
    for i := 1 to my do
    begin
      if i mod 5 = 0 then pen.color := MainLineColor else pen.color := SecondLineColor;
      j := i * MidCellSize;
      moveto(0,cy+j);
      lineto(OwnerBitmap.MaxWidth,cy+j);
      moveto(0,cy-j);
      lineto(OwnerBitmap.MaxWidth,cy-j);
    end;
    pen.color:= AxisColorX;
    moveto(0,cy);
    lineto(OwnerBitmap.MaxWidth,cy);
    pen.Color := AxisColorY;
    moveto(cx,0);
    lineto(cx, OwnerBitmap.MaxHeight);
  end;
end;

procedure TBZBitmapRenderFilters.TileTexture(Const TileWidth, TileHeight : Integer; Const ATexture : TBZBitmap;const ADrawMode: TBZBitmapDrawMode=dmSet;Const aAlphaMode :TBZBitmapAlphaMode=amOpaque);
Var

  NbTileCols, nbTileRows : Integer;
  I, J : Integer;


  procedure LineMapH(x1,ys,x2,xd,yd,ll:integer;ASrcBmp:TBZBitmap);
  Var
    ii,li :integer;
    v1,xx : single;
    col : TBZColor;
  begin

    v1:= (x2-x1) / ll;
    xx := x1;
    li:=ll-1;

    if (xd+ll)>(OwnerBitmap.MaxWidth) then li:=ll-(xd+ll-OwnerBitmap.MaxWidth);

    for ii:=0 to li do
    begin
      xx:=xx+v1;
      col := ASrcBmp.GetPixel(round(xx),ys);
      OwnerBitmap.DrawPixel(xd,yd,col,ADrawMode,aAlphaMode);
      inc(xd);
    end;
  end;

  Procedure TextureBox(xs,ys,L,H:Integer);
  var
    maxTexWidth,  maxBoxHeight : integer;
    yts : integer;
    yt,yd:Single;
  begin
    maxTexWidth := ATexture.MaxWidth;
    yd:=(ATexture.Height / h);
    maxBoxHeight :=(ys + h) -1;
    if MaxBoxHeight > OwnerBitmap.MaxHeight then MaxBoxHeight := OwnerBitmap.MaxHeight;
    yt := 0;
    for yts := ys to maxBoxHeight do
    begin
      LineMapH(0,round(yt),maxTexWidth,xs,yts,l,ATexture);
      yt:=yt+yd;
    end;
  end;

begin
  NbTileRows := OwnerBitmap.Height div TileHeight;
  NbTileCols := OwnerBitmap.Width div TileWidth;

  For i:=0 to NbTileRows do
  begin
    For J := 0 to NbTileCols do
    begin
       TextureBox(J * TileWidth, I * TileHeight, TileWidth, TileHeight);
    end;
  end;
end;

{%endregion%}

{%region ====[ TBZBitmapNoiseFilters ]==========================================}
//procedure UniformNoise;
//procedure RandomNoise;
//procedure PerlinNoise;
//procedure AnkhNoise;
//procedure PlasmaNoise;
{%endregion%}

{%region ====[ TBZCustomBitmapFontDrawer ]======================================}

Constructor TBZCustomBitmapFontDrawer.Create(ASurface: TBZBitmapCanvas);
Begin
  Inherited Create;
  FSurfaceCanvas := ASurface;
End;

Destructor TBZCustomBitmapFontDrawer.Destroy;
Begin
  FSurfaceCanvas := nil;
  Inherited Destroy;
End;


procedure TBZCustomBitmapFontDrawer.Internal_DrawText(atX,atY:integer; atext:unicodestring);
begin
  GetText(atext);
  DrawLastText(atX,atY);
end;

procedure TBZCustomBitmapFontDrawer.Internal_DrawText(atX,atY:integer; atext:string);
begin
  GetText(atext);
  DrawLastText(atX,atY+Size);
end;

procedure TBZCustomBitmapFontDrawer.DrawLastText (atX,atY:integer);
var r : integer;
begin
  with FLastText do
  begin
    for r := 0 to count-1 do
    begin
      with Bitmaps[r]^ do
      begin
        if mode = btBlackWhite then
          DrawCharBW(atX+x, atY+y, data, pitch, width, height)
        else
          DrawChar(atX+x, atY+y, data, pitch, width, height);
      end;
    End;
  End;
end;

const
  //bits : array[0..7] of byte = (1,2,4,8,16,32,64,128);
  bits : array[0..7] of byte = (128,64,32,16,8,4,2,1);

//procedure TBZCustomBitmapFontDrawer.DrawCharClearType(x,y:integer; data:PByteArray; pitch, width, height:integer);
//begin
//
//End;

procedure TBZCustomBitmapFontDrawer.DrawChar(x,y:integer; data:PByteArray; pitch, width, height:integer);
var
  TmpColor,FontColor,
  SrcColor, DstColor: TBZColor;
  b,rx,ry,xx,yy : integer;
begin
  b := 0;

  FontColor := Self.Color;
  for ry := 0 to height-1 do
    begin
      yy:=y+ry;
      for rx := 0 to width-1 do
      begin
        xx:=x+rx;
        //c2.Alpha :=
        //if ] <> 0 then
        //begin

        if data^[b+rx] > 0 then
        begin
          TmpColor := FontColor;
          TmpColor.Alpha := data^[b+rx];
          SrcColor := FSurfaceCanvas.Surface.GetPixel(xx,yy);
          DstColor := SrcColor.AlphaBlend(TmpColor); //, Ratio);
          DstColor.Alpha := FontColor.Alpha;
          FSurfaceCanvas.PutPixel(xx,yy,DstColor); //.AlphaBlend(SrcColor)
        end;
      end;
      inc (b, pitch);
    end;
end;

procedure TBZCustomBitmapFontDrawer.DrawCharBW(x,y:integer; data:PByteArray; pitch, width, height:integer);
var
  rb : byte;
  rx,ry,b,l : integer;
begin
  if Data = nil then raise Exception.Create('Données TTF Vide');
  b := 0;
  for ry := 0 to height-1 do
  begin
    l := 0;
    for rx := 0 to width-1 do
    begin
      rb := rx mod 8;
      if (data^[b+l] and bits[rb]) <> 0 then FSurfaceCanvas.PutPixel(x+rx,y+ry,Color);
      if rb = 7 then inc (l);
    end;
    inc(b, pitch);
  end;
end;

(*

procedure TextRect(Canvas: TFPImageCanvas; const ARect: TRect; const Text: string);
type
  IntArray = array of integer;

var
  LSpaces: IntArray;
  LStart: Integer;
  aLine: String;
  Lines: TStringList;
  aLineWidth: Integer;
  w, h, y, dy, i: Integer;

  procedure LocateSpaces(const s: string; var a: IntArray);
  const
    RTL = True;

  var
    i,c: Integer;
  begin
    setLength(a, length(s));
    c := 1;
    //for i := 1 to length(s) do   { LTR }
    for i := length(s) downto 1 do { RTL }
    begin
      if s[i]=' ' then
      begin
        a[c] := i;
        inc(c);
      end;
    end;

    { Add two imaginary spaces before and after the text }
    if RTL then
    begin
      a[0] := length(s) + 1;
      a[c] := 0;
    end
    else
    begin
      a[0] := 0;
      a[c] := length(s) + 1;
    end;

    inc(c);
    SetLength(a, c);
  end;

  function BSearch: integer;
  var
    left, right: integer;  { names are not true }
    idx: integer;
    tmpS: String;
    tmpW: integer;
  begin
    { Find the number of words that fit using binary search }
    { Not optimal, just wanted to try it! }
    left := LStart;
    right := Length(LSpaces)-1;

    BSearch := -1;
    while left<=right do
    begin
      idx := (left+right) div 2;
      //tmpS := copy(Text, LSpaces[LStart]+1, LSpaces[idx]-LSpaces[LStart]);//LTR
      tmpS := copy(Text, LSpaces[idx]+1, LSpaces[LStart]-LSpaces[idx]);//RTL
      tmpW := Canvas.TextWidth(tmpS);
      if tmpW<w then
      begin
        aLine := tmpS;
        aLineWidth := tmpW;
        Result := idx;
        left := idx+1
      end
      else if tmpW>w then
        right := idx-1
      else
        exit(idx);
    end;
  end;

  procedure NextLine;
  var
    idx: integer;
  begin
    idx := BSearch();
    if idx=-1 then
      exit;
    LStart := idx;
  end;

begin
  { Find location of space characters where we could break a line }
  LocateSpaces(Text, LSpaces);

  { Width and height }
  with ARect do
  begin
    w := Right-Left;
    h := Bottom-Top;
  end;

  { Height of one line }
  dy := Canvas.TextHeight(Text)+1;

  { Break the text into lines that fit in the width w }
  Lines := TStringList.Create;
  LStart := 0;
  repeat
    NextLine;
    Lines.AddObject(aLine, TObject(aLinewidth)); {  Add each line and its width }
  until Length(aLine)=0;

  { Draw lines aligned to the right side }
  y := ARect.Top;
  for i := 0 to Lines.Count -1 do
  begin
    aLinewidth := Integer(Lines.Objects[i]);
    aLine := Lines[i];
    Canvas.TextOut(ARect.Left+w-aLinewidth, y, aLine);
    inc(y, dy);
  end;

  { Clean up }
  SetLength(LSpaces, 0);
  Lines.Free;
end;

Procedure TBZCustomBitmapFontDrawer.RenderDraft(x, y, tx: Integer; Data: pointer);
Var
  SrcPtr: PByte;
  DstPtr: PBZColor;
  AColor: TBZColor;
  Alpha:  Byte;
Begin
  If (x >= 0) And (x <= FSurface.MaxWidth) And (y >= 0) And (y <= FSurface.MaxHeight) Then
  Begin
    SrcPtr := PByte(Data);
    DstPtr := FSurface.GetPixelPtr(x, y);
    While (tx > 0) Do
    Begin
      AColor := FColor;
      Alpha := AColor.Alpha * (SrcPtr^ + 1) Shr 8;
      If Alpha > 0 Then
        If Alpha = 255 Then
          DstPtr^ := AColor
        Else
          DstPtr^ := DstPtr^.AlphaMix(AColor);
      Inc(SrcPtr);
      Inc(DstPtr);
      Dec(tx);
    End;
  End;
End;

Procedure TBZCustomBitmapFontDrawer.RenderClearType(x, y, tx: Integer; Data: pointer);
Begin

End; *)

(*Procedure TBZCustomBitmapFontDrawer.DrawText(AText: String; AFont: TFreeTypeRenderableFont; x, y: Single; AColor: TBZColor);
Begin
  FColor := AColor;
  If AFont.ClearType Then
    AFont.RenderText(AText, x, y, FSurface.ClipRect.AsRect, @RenderClearType)
  Else
    AFont.RenderText(AText, x, y, FSurface.ClipRect.AsRect, @RenderDraft);
End;

Procedure TBZCustomBitmapFontDrawer.DrawText(AText: String; AFont: TFreeTypeRenderableFont; x, y: Single; AColor: TBZColor; AAlign: TFreeTypeAlignments);
Var
  idx:   Integer;
  delta: Single;
Begin
  If Not (ftaBaseline In AAlign) Then
  Begin
    If ftaTop In AAlign Then y := y + AFont.Ascent
    Else If ftaBottom In AAlign Then y := y + AFont.Ascent - AFont.TextHeight(AText)
    Else If ftaVerticalCenter In AAlign Then y := y+ AFont.Ascent - AFont.TextHeight(AText) * 0.5;
  End;
  AAlign := AAlign - [ftaTop, ftaBaseline, ftaBottom, ftaVerticalCenter];

  idx := pos(LineEnding, AText);
  While idx <> 0 Do
  Begin
    DrawText(copy(AText, 1, idx - 1), AFont, x, y, AColor, AAlign);
    Delete(AText, 1, idx + length(LineEnding) - 1);
    idx := pos(LineEnding, AText);
    y := y + AFont.LineFullHeight;
  End;

  If Not (ftaLeft In AAlign) Then
  Begin
    delta := 0;
    If ftaCenter In AAlign Then delta := -AFont.TextWidth(AText) / 2
    Else If ftaRight In AAlign Then delta := -AFont.TextWidth(AText);
    If AFont.Hinted Then delta := round(delta);
    x := x +delta;
  End;
  DrawText(AText, AFont, x, y, AColor);
End;

Procedure TBZCustomBitmapFontDrawer.DrawTextWordBreak(AText: String; AFont: TFreeTypeRenderableFont; x, y, AMaxWidth: Single;
  AColor: TBZColor; AAlign: TFreeTypeAlignments);
Var
  ARemains: String;
  stepX, stepY: Single;
  Lines: TStringList;
  i:     Integer;
  lineShift: Single;
  lineAlignment: TFreeTypeAlignments;
  p, step : TBZVector2f; //TBZPoint;
Begin
  If (AText = '') Or (AMaxWidth <= 0) Then exit;

  Step.Create(0,AFont.LineFullHeight);
  P.Create(X,Y);

  //stepX := 0;
  //stepY := AFont.LineFullHeight;

  AAlign := AAlign -[ftaBaseline]; //ignored
  If AAlign * [ftaTop, ftaVerticalCenter, ftaBottom] = [] Then AAlign :=AAlign + [ftaTop]; //top by default
  lineAlignment := AAlign * [ftaLeft, ftaCenter, ftaRight] + [ftaVerticalCenter];

  If ftaTop In AAlign Then
  Begin
    lineShift := 0.5;
    P := P + (Step * lineShift);
    //X := X + stepX * lineShift;
    //Y := Y + stepY * lineShift;
    Repeat
      AFont.SplitText(AText, AMaxWidth, ARemains);
      DrawText(AText, AFont, P.X, P.Y, AColor, lineAlignment);
      AText := ARemains;
      P := P + Step;
      //X := X + stepX;
      //Y := Y + stepY;
    Until ARemains = '';
  End
  Else
  Begin
    Lines := TStringList.Create;
    Repeat
      AFont.SplitText(AText, AMaxWidth, ARemains);
      Lines.Add(AText);
      AText := ARemains;
    Until ARemains = '';
    If ftaVerticalCenter In AAlign Then lineShift := Lines.Count / 2 - 0.5
    Else If ftaBottom In AAlign Then lineShift := Lines.Count - 0.5
    Else lineShift := -0.5;
    P := P + (Step * lineShift);
    //X := X - stepX * lineShift;
    //Y := Y - stepY * lineShift;
    For i := 0 To Lines.Count - 1 Do
    Begin
      DrawText(Lines[i], AFont, P.X, P.Y, AColor, lineAlignment);
      P := P + Step;
      //X := X + stepX;
      //Y := Y + stepY;
    End;
    Lines.Free;
  End;
End;

Procedure TBZCustomBitmapFontDrawer.DrawTextRect(AText: String; AFont: TFreeTypeRenderableFont; X1, Y1, X2, Y2: Single; AColor: TBZColor; AAlign: TFreeTypeAlignments);
Var
  X, Y: Single;
Begin
  If X2 <= X1 Then exit;
  If ftaVerticalCenter In AAlign Then Y := (Y1 + Y2) / 2
  Else If ftaBottom In AAlign Then Y := Y2
  Else Y := Y1;

  If ftaCenter In AAlign Then X := (X1 + X2) / 2
  Else If ftaRight In AAlign Then X := X2
  Else  X := X1;

  If FWordBreak Then
    DrawTextWordBreak(AText, AFont, X, Y, X2 - X1, AColor, AAlign)
  Else
    DrawText(AText, AFont, X, Y, AColor, AAlign);
End;

Procedure TBZCustomBitmapFontDrawer.DrawTextRect(AText: String; AFont: TFreeTypeRenderableFont; ARect: TRect; AColor: TBZColor; AAlign: TFreeTypeAlignments);
Begin
  DrawTextRect(AText, AFont, ARect.Left, ARect.Top, ARect.Right, ARect.Bottom, AColor, AALign);
End;  *)

{%endregion%}

{%region ====[ TBZBitmapFontDrawer ]============================================}

Constructor TBZBitmapFontDrawer.Create(ASurface: TBZBitmapCanvas);
Begin
  Inherited Create(ASurface);
  FMask := nil;

  //FShadowed := False;
  //FOutLined := False;
End;

Destructor TBZBitmapFontDrawer.Destroy;
Begin
  if Assigned(FMask) then FreeAndNil(FMask);
  Inherited Destroy;
End;

{%endregion%}

{%region ====[ TBZBitmapCanvas ]================================================}

constructor TBZBitmapCanvas.Create(AOwner : TBZCustomBitmap);
begin
  Inherited Create(AOwner.Width, aOwner.Height);
  FOwnerBitmap := AOwner;
  FPen := TBZCustomCanvasPen.Create;
  FBrush := TBZCustomCanvasBrush.Create;
  FFont := CreateDefaultFont;
  FFont.Name := DefautFontName;
end;

destructor TBZBitmapCanvas.Destroy;
begin
  FreeAndNil(FFont);
  FreeAndNil(FBrush);
  FreeAndNil(FPen);
  inherited Destroy;
end;

procedure TBZBitmapCanvas.FreeBuffer;
begin
  //
end;

procedure TBZBitmapCanvas.ResizeBuffer(const AWidth, AHeight : integer);
begin
  //
end;

function TBZBitmapCanvas.Internal_CreateDefaultFont : TBZCustomFont;
begin
  result := TBZBitmapFontDrawer.Create(Self);     //TBZEmptyFont.Create;
  with result do
  begin
    Size := 10;
    Color := clrBlack;
  end;
  //FFontDrawer := TBZBitmapFontDrawer.Create(Self);
  //Result := FFontDrawer;
end;

procedure TBZBitmapCanvas.Internal_TextOut (x,y:integer;text:string);
begin
  TBZBitmapFontDrawer(FFont).DrawText(x,y,Text);
end;

procedure TBZBitmapCanvas.Internal_GetTextSize (text:string; var w,h:integer);
begin
 TBZBitmapFontDrawer(FFont).GetTextSize(Text,w,h);
end;

function TBZBitmapCanvas.Internal_GetTextHeight (text:string) : integer;
begin
  Result := TBZBitmapFontDrawer(FFont).GetTextHeight(Text);
end;

function TBZBitmapCanvas.Internal_GetTextWidth (text:string) : integer;
begin
  Result := TBZBitmapFontDrawer(FFont).GetTextWidth(Text);
end;

procedure TBZBitmapCanvas.FloodFill(px, py : Single; const SearchColor, NewColor : TBZColor);
begin
  FloodFill(px,py,SearchColor,NewColor,ffmScanLine);
end;

procedure TBZBitmapCanvas.FloodFill(px, py : Single; const SearchColor, NewColor : TBZColor; const FloodFillMethod : TBZFloodFillMode);
begin
  Case FloodFillMethod of
    ffmBoundary4 : FloodFill_Boundary4(px,py,SearchColor,NewColor);
    ffmBoundary8 : FloodFill_Boundary8(px,py,SearchColor,NewColor);
    ffmScanLine  : FloodFill_ScanLine(px,py,SearchColor,NewColor);
  end;
end;

procedure TBZBitmapCanvas.Line(PtFrom, PtTo : TBZPoint);
begin
  Line(PtFrom.AsVector2f, PtTo.AsVector2f);
end;

function TBZBitmapCanvas.GetPen : TBZCustomCanvasPen;
begin
  Result := FPen;
end;

function TBZBitmapCanvas.GetBrush : TBZCustomCanvasBrush;
begin
  Result := FBrush;
end;

procedure TBZBitmapCanvas.PutPixel(X, Y : Integer; AColor : TBZColor);
begin
  With Self.DrawMode Do
  Begin
    FOwnerBitmap.DrawPixel(Round(X), Round(Y), AColor, PixelMode, AlphaMode, MasterAlpha, CombineMode, BlendSrcFactor, BlendDstFactor);
  End;
end;

procedure TBZBitmapCanvas.PutAlphaBlendPixel(X, Y : Integer; AColor : TBZColor; Ratio : Single);
Var
  DstColor, SrcColor : TBZColor;

Begin
  SrcColor := FOwnerBitmap.getPixel(X,Y);
  DstColor := SrcColor.mix(AColor, Ratio);
  //DstColor.Alpha := AColor.Alpha;
  FOwnerBitmap.setPixel(X,Y,SrcColor.AlphaBlend(DstColor));
  //FOwner.setPixel(X,Y,DstColor);
End;

procedure TBZBitmapCanvas.DrawVLine(x1, y1, y2 : Single);
Var
  Y, yi2, xi1 :      Integer;
  StrokePatternSize: Integer;
  AColor: TBZColor;
  //PolyLineTool : TBZ2DPolyLineTool;
  PolyPoints : TBZArrayOfFloatPoints;
  vXLeft, vXRight : Single;
Begin

  xi1 := Round(x1);
  Y := Round(Y1);
  yi2 := Round(y2);
  AColor := FPen.Color;
  if FPen.width > 1 then
  begin
    PolyPoints := TBZArrayOfFloatPoints.Create(4);
    //MidWidth := (FPen.Width * 0.5) - 0.5 ;
    vXLeft := x1-(FPen.Width-1);// MidWidth;
    vXRight := x1; //+MidWidth;

    //if not(Odd(FPen.Width)) then vXRight := x1+MidWidth-1;
    // jsMiter
    PolyPoints.Add(Vec2(vXLeft,y1));
    PolyPoints.Add(Vec2(vXRight,y1));
    PolyPoints.Add(Vec2(vXRight,y2));
    PolyPoints.Add(Vec2(vXLeft,y2));
    FillStrokePolygon(PolyPoints);
    FreeAndNil(PolyPoints);
  end
  else
  begin
    if FPen.Style = ssPattern then
    begin
      StrokePatternSize := 0;
      Case FPen.PatternStyle Of
        psDot: StrokePatternSize := Length(DotPenStrokePattern);
        psDash: StrokePatternSize := Length(DashPenStrokePattern);
        psDashDot: StrokePatternSize := Length(DashDotPenStrokePattern);
        psDashDotDot: StrokePatternSize := Length(DashDotDotPenStrokePattern);
      End;
      Repeat
        Case FPen.PatternStyle Of
          psDot: If DotPenStrokePattern[Y Mod StrokePatternSize] Then PutPixel(xi1, Y, AColor);
          psDash: If DashPenStrokePattern[Y Mod StrokePatternSize] Then PutPixel(xi1, Y, AColor);
          psDashDot: If DashDotPenStrokePattern[Y Mod StrokePatternSize] Then PutPixel(xi1, Y, AColor);
          psDashDotDot: If DashDotDotPenStrokePattern[Y Mod StrokePatternSize] Then PutPixel(xi1, Y, AColor);
        End;
        Inc(Y);
      Until Y > Y2;
    end
    else
    begin
      Repeat
        PutPixel(xi1, Y, AColor);
        Inc(Y);
      Until Y > Yi2;
    end;

    if FPen.StartCap <> cmNone then
    begin

    end;
    if FPen.EndCap <> cmNone then
    begin

    end;
  end;
end;

procedure TBZBitmapCanvas.DrawHLine(x1, y1, x2 : Single);
Var
  x, xi2, xi1, y :     Integer;
  StrokePatternSize: Integer;
  AColor: TBZColor;
  PolyPoints : TBZArrayOfFloatPoints;
  vYUp, vYDown : Single;
Begin
  xi1 := Round(x1);
  xi2 := Round(x2); //xi1 + Round(L) -1;
  y := Round(y1);
  AColor := FPen.Color;
  if FPen.width > 1 then
  begin
    PolyPoints := TBZArrayOfFloatPoints.Create(4);
    //MidWidth := (FPen.Width * 0.5) - 0.5 ;
    vYUp := y1 - (FPen.Width - 1); //MidWidth;
    //if Odd(FPen.Width) then vYUp := y1-MidWidth+1;
    vYDown := y1; //+MidWidth;
    // jsMiter
    PolyPoints.Add(Vec2(x1,vYUp));
    PolyPoints.Add(Vec2(x2,vYUp));
    PolyPoints.Add(Vec2(x2,vYDown));
    PolyPoints.Add(Vec2(x1,vYDown));
    FillStrokePolygon(PolyPoints);
    FreeAndNil(PolyPoints);
  end
  else
  begin
    x := xi1;
    if FPen.Style = ssPattern then
    begin
      StrokePatternSize := 0;
      Case FPen.PatternStyle Of
        psDot: StrokePatternSize := Length(DotPenStrokePattern);
        psDash: StrokePatternSize := Length(DashPenStrokePattern);
        psDashDot: StrokePatternSize := Length(DashDotPenStrokePattern);
        psDashDotDot: StrokePatternSize := Length(DashDotDotPenStrokePattern);
      End;
      Repeat
        Case FPen.PatternStyle Of
          psDot: If DotPenStrokePattern[Y Mod StrokePatternSize] Then PutPixel(x, Y, AColor);
          psDash: If DashPenStrokePattern[Y Mod StrokePatternSize] Then PutPixel(x, Y, AColor);
          psDashDot: If DashDotPenStrokePattern[Y Mod StrokePatternSize] Then PutPixel(x, Y, AColor);
          psDashDotDot: If DashDotDotPenStrokePattern[Y Mod StrokePatternSize] Then PutPixel(x, Y, AColor);
        End;
        Inc(x);
      Until x > xi2;
    end
    else
    begin
      Repeat
        PutPixel(x, Y, AColor);
        Inc(x);
      Until x > xi2;
    end;

    if FPen.StartCap <> cmNone then
    begin

    end;
    if FPen.EndCap <> cmNone then
    begin

    end;
  end;
end;

{ Trace une ligne antialiasée (algorithme de Xiaolin Wu)
https://stackoverflow.com/questions/3613130/simple-anti-aliasing-function-for-delphi-7
https://www.codeproject.com/Articles/13360/Antialiasing-Wu-Algorithm
https://rosettacode.org/wiki/Xiaolin_Wu%27s_line_algorithm#Pascal }
procedure TBZBitmapCanvas.DrawAntiAliasLine(x1,y1,x2,y2 : Single);
var
  loop, start, finish : integer;
  deltax, deltay, dx, dy, dydx : single; // fractional parts
  AColor : TBZColor;
  xi1, xi2, yi1, yi2 : integer;
  Factor : Single;
  C1, C2: TBZColor;
begin

  AColor := FPen.Color;


  xi1 := Round(x1);
  yi1 := Round(y1);
  xi2 := Round(x2);
  yi2 := Round(y2);

  deltax := abs(x2 - x1); // Calculate deltax and deltay for initialisation
  deltay := abs(y2 - y1);

  if (deltax  = 0) then
  begin
//    GlobalLogger.LogNotice('Vertical AA Line');
    if yi1 > yi2 then Swap(yi1,yi2);
    Loop := yi1;
    repeat
      FOwnerBitmap.setPixel(xi1,Loop,AColor);
      inc(Loop);
    until Loop>yi2;
    Exit;
  end;
  if (deltay = 0) then
  begin
//    GlobalLogger.LogNotice('Horizontal AA Line');
    if xi1 > xi2 then Swap(xi1,xi2);
    Loop := xi1;
    repeat
      FOwnerBitmap.setPixel(Loop,yi1,AColor);
      inc(Loop);
    until Loop>xi2;
    Exit;
  end;

  C1 := AColor;
  C2 := C1;

  if deltax > deltay then
  begin // horizontal or vertical
    if y2 > y1 then // determine rise and run
      dydx := -(deltay / deltax)
    else
      dydx := deltay / deltax;
    if x2 < x1 then
    begin
      start := xi2; // right to left
      finish := xi1;
      dy := y2;
    end else
    begin
      start := xi1; // left to right
      finish := xi2;
      dy := y1;
      dydx := -dydx; // inverse slope
    end;

    for loop := start to finish do
    begin
      Factor := frac(dy);
      //tr := ClampByte(Round(Factor * 255));
      //C1.Alpha := Tr;
      //C2.Alpha := 255 - tr;
      PutAlphaBlendPixel(loop, Trunc(dy), C1, 1 - Factor );
      PutAlphaBlendPixel(loop, Trunc(dy) + 1, C2, Factor);
      dy := dy + dydx; // next point
    end;
  end else
  begin
    if x2 > x1 then // determine rise and run
      dydx := -(deltax / deltay)
    else
      dydx := deltax / deltay;
    if y2 < y1 then
    begin
      start := yi2; // right to left
      finish := yi1;
      dx := x2;
    end else
    begin
      start := yi1; // left to right
      finish := yi2;
      dx := x1;
      dydx := -dydx; // inverse slope
    end;
    for loop := start to finish do
    begin
      Factor := frac(dx);
      //tr := ClampByte(Round(Factor * 255));
      //C1.Alpha := Tr;
      //C2.Alpha := 255 - tr;
      PutAlphaBlendPixel(trunc(dx), loop, C1,1 - Factor);
      PutAlphaBlendPixel(trunc(dx) + 1, loop, C2,Factor);
      dx := dx + dydx; // next point
    end;
  end;
end;

procedure TBZBitmapCanvas.DrawLine(x1, y1, x2, y2 : Single);
var
  xi1, yi1, xi2, yi2 : Integer;
  Y, X:   Integer;
  DX, DY, SX, SY, E, C: Integer;
  StrokePatternSize: Integer;
  AColor: TBZColor;
  PolyLineTool : TBZ2DPolyLineTool;
  StrokePoints, LinePoints : TBZArrayOfFloatPoints;
begin
  xi1 := Round(x1);
  yi1 := Round(y1);
  xi2 := Round(x2);
  yi2 := Round(y2);

  If Xi2 = Xi1 Then
  Begin
    if y1>y2 then swap(y1,y2);
    DrawVLine(X1, Y1, Y2);
    Exit;
  End;
  If Yi2 = Yi1 Then
  Begin
    if x1>x2 then swap(x1,x2);
    DrawHLine(X1, Y1, X2);
    Exit;
  End;

  if FPen.Width > 1 then
  begin
    LinePoints := TBZArrayOfFloatPoints.Create(2);
    LinePoints.add(vec2(x1,y1));
    LinePoints.add(vec2(x2,y2));
    PolyLineTool := TBZ2DPolyLineTool.Create(LinePoints);
    PolyLineTool.StrokeWidth := FPen.Width;
    StrokePoints := PolyLineTool.BuildStroke;
    FillStrokePolygon(StrokePoints);
    FreeAndNil(StrokePoints);
    FreeAndNil(PolyLineTool);
    FreeAndNil(LinePoints);
  end
  else
  begin
    if Antialias then
    begin
      DrawAntiAliasLine(x1, y1, x2, y2);
      Exit;
    end;
    AColor := FPen.Color;
    StrokePatternSize := 0;

    If FPen.Style = ssSolid Then PutPixel(xi1, yi1, AColor)
    Else
    Begin
      Case FPen.PatternStyle Of
        psDot: StrokePatternSize := Length(DotPenStrokePattern);
        psDash: StrokePatternSize := Length(DashPenStrokePattern);
        psDashDot: StrokePatternSize := Length(DashDotPenStrokePattern);
        psDashDotDot: StrokePatternSize := Length(DashDotDotPenStrokePattern);
      End;

      Case FPen.PatternStyle Of
        psDot: If DotPenStrokePattern[0] Then PutPixel(xi1, yi1, AColor);
        psDash: If DashPenStrokePattern[0] Then PutPixel(xi1, yi1, AColor);
        psDashDot: If DashDotPenStrokePattern[0] Then PutPixel(xi1, yi1, AColor);
        psDashDotDot: If DashDotDotPenStrokePattern[0] Then PutPixel(xi1, yi1, AColor);
      End;
    End;

    DX := Xi2 - Xi1;
    DY := Yi2 - Yi1;
    C := 1;

    If DX < 0 Then
    Begin
      SX := -1;
      DX := -DX;
    End
    Else
      SX := 1;

    If DY < 0 Then
    Begin
      SY := -1;
      DY := -DY;
    End
    Else
      SY := 1;

    DX := DX Shl 1;
    DY := DY Shl 1;
    C := 0;
    X := Xi1;
    Y := Yi1;
    If DX > DY Then
    Begin
      E := DY - DX Shr 1;

      While X <> Xi2 Do
      Begin
        If E >= 0 Then
        Begin
          Inc(Y, SY);
          Dec(E, DX);
        End;
        Inc(X, SX);
        Inc(E, DY);
        If FPen.Style = ssSolid Then PutPixel(x, y, AColor)
        Else
        begin
          Case FPen.PatternStyle Of
            psSolid: PutPixel(x, y, AColor);
            psDot: If DotPenStrokePattern[C Mod StrokePatternSize] Then PutPixel(x, y, AColor);
            psDash: If DashPenStrokePattern[C Mod StrokePatternSize] Then PutPixel(x, y, AColor);
            psDashDot: If DashDotPenStrokePattern[C Mod StrokePatternSize] Then PutPixel(x, y, AColor);
            psDashDotDot: If DashDotDotPenStrokePattern[C Mod StrokePatternSize] Then PutPixel(x, y, AColor);
            //psGradient
            //psTexture
          End;
        end;
        Inc(C);
      End;
    End
    Else
    Begin
      E := DX - DY Shr 1;
      While Y <> Yi2 Do
      Begin
        If E >= 0 Then
        Begin
          Inc(X, SX);
          Dec(E, DY);
        End;
        Inc(Y, SY);
        Inc(E, DX);

        If FPen.Style = ssSolid Then PutPixel(x, y, AColor)
        Else
        begin
          Case FPen.PatternStyle Of
            psSolid: PutPixel(x, y, AColor);
            psDot: If DotPenStrokePattern[C Mod StrokePatternSize] Then PutPixel(x, y, AColor);
            psDash: If DashPenStrokePattern[C Mod StrokePatternSize] Then PutPixel(x, y, AColor);
            psDashDot: If DashDotPenStrokePattern[C Mod StrokePatternSize] Then PutPixel(x, y, AColor);
            psDashDotDot: If DashDotDotPenStrokePattern[C Mod StrokePatternSize] Then PutPixel(x, y, AColor);
            //psGradient
            //psTexture
          End;
        end;
        Inc(C);
      End;
    End;

    If FPen.Style = ssSolid Then PutPixel(xi2, yi2, AColor)
    Else
    begin
      Case FPen.PatternStyle Of
        psSolid: PutPixel(xi2, yi2, AColor);
        psDot: If DotPenStrokePattern[C Mod StrokePatternSize] Then PutPixel(xi2, yi2, AColor);
        psDash: If DashPenStrokePattern[C Mod StrokePatternSize] Then PutPixel(xi2, yi2, AColor);
        psDashDot: If DashDotPenStrokePattern[C Mod StrokePatternSize] Then PutPixel(xi2, yi2, AColor);
        psDashDotDot: If DashDotDotPenStrokePattern[C Mod StrokePatternSize] Then PutPixel(xi2, yi2, AColor);
      End;
    end;
  end;
end;

procedure TBZBitmapCanvas.DrawPolyLine(Pts : TBZArrayOfFloatPoints; const Closed : Boolean);
Var
 n, i, k : Integer;
 PolyLineTool : TBZ2DPolyLineTool;
 StrokePoints : TBZArrayOfFloatPoints;
begin
  n := pts.Count;
//  GlobalLogger.LogNotice('PolyLine Count Pts : '+n.ToString);
  if FPen.Width > 1 then
  begin
    PolyLineTool := TBZ2DPolyLineTool.Create(Pts);
    PolyLineTool.StrokeWidth := FPen.Width;
    PolyLineTool.ClosePath := Closed;
    PolyLineTool.StrokeMode := smOuter;
    //GlobalLogger.LogNotice('=============================================');
    //GlobalLogger.LogNotice('DrawPolyline');
    //GlobalLogger.LogNotice('=============================================');
    //For i := 0 to Pts.Count - 1 do
    //begin
    //  GlobalLogger.LogStatus('Pts '+i.ToString  + pts.Items[i].ToString);
    //end;
    //GlobalLogger.LogNotice('=============================================');
    StrokePoints := PolyLineTool.BuildStroke;

//    GlobalLogger.LogNotice('Fill Stroke PolyLine');

    FillStrokePolygon(StrokePoints);
    FreeAndNil(StrokePoints);
    FreeAndNil(PolyLineTool);
  end
  else
  begin
    if Antialias then
    begin
      DrawAntiaAliasPolyLine(Pts,Closed);
      Exit;
    end;

    For i := 0 To n-1 Do
    Begin
      k := (i + 1) Mod n;
      if not(Closed) then if k=0 then Continue;
      //if not(Closed) then if k=0 then k:=n-1;

      //GlobalLogger.LogNotice('Draw Line : '+i.ToString+','+ k.ToString + ' --> Coords = ('+Pts.items[i].ToString+'), ('+ Pts.items[k].ToString+')');
      Line(Pts.items[i], Pts.items[k]);
    end;
  end;
end;

procedure TBZBitmapCanvas.DrawAntiaAliasPolyLine(Pts : TBZArrayOfFloatPoints; const Closed : Boolean);
Var
 n, i, k : Integer;
begin
  n := pts.Count;
  For i := 0 To n-1 Do
  Begin
    k := (i + 1) Mod n;
    if not(Closed) then if k=0 then Continue;
    //if not(Closed) then if k=0 then k:=n-1;
    DrawAntiAliasLine(Pts.items[i].x, Pts.items[i].y, Pts.items[k].x,Pts.items[k].y);
  end;
end;

procedure TBZBitmapCanvas.DrawPolygon(Pts : TBZArrayOfFloatPoints);
begin
  DrawPolyLine(Pts, True);
end;

procedure TBZBitmapCanvas.DrawRectangle(x1, y1, x2, y2 : Single);
Var
 Pts : TBZArrayOfFloatPoints;
begin
  Pts := TBZArrayOfFloatPoints.Create(4);
  Pts.Add(Vec2(x1,y1));
  Pts.Add(Vec2(x2,y1));
  Pts.Add(Vec2(x2,y2));
  Pts.Add(Vec2(x1,y2));
  PolyLine(Pts, True);
  FreeAndNil(Pts);
  //HLine(x1,y1,x2);
  //Hline(x1,y2,x2);
  //VLine(x1,y1,y2);
  //Vline(x2,y1,y2);
end;


procedure TBZBitmapCanvas.DrawRoundedRect(x1, y1, x2, y2 : Single; const Rx : Single; const Ry : Single);
Var
  ArcPoints : TBZArrayOfFloatPoints;
  PolyPoints : TBZArrayOfFloatPoints;
  i : integer;
  //ATLX, ATLY, ATRX, ATRY, ABLX, ABLY, ABRX, ABRY : Single;
  HLTX1, HLTX2{, HLTY1, HLTY2} : Single; // Haut
  HLBX1, HLBX2{, HLBY1, HLBY2} : Single; // Bas
  {VLLX1, VLLX2,} VLLY1, VLLY2 : Single; // Gauche
  {VLRX1, VLRX2,} VLRY1, VLRY2 : Single; // Droite
  Radius, RadiusTopLeft, RadiusTopRight, RadiusBottomLeft, RadiusBottomRight : Single;

begin
    PolyPoints := TBZArrayOfFloatPoints.Create(64);

    Radius := Max(Rx, Ry);
    HLTX1 := x1;
    //HLTY1 := y1;
    HLTX2 := x2;
    //HLTY2 := y1;

    HLBX1 := x1;
    //HLBY1 := y2;
    HLBX2 := x2;
    //HLBY2 := y2;

    //VLLX1 := x1;
    VLLY1 := y1;
    //VLLX2 := x1;
    VLLY2 := y2;

    //VLRX1 := x2;
    VLRY1 := y1;
    //VLRX2 := x2;
    VLRY2 := y2;

    RadiusTopLeft := Radius;
    RadiusBottomLeft := Radius;
    RadiusTopRight := Radius;
    RadiusBottomRight := Radius;

    // Coin Haut Gauche
    if RadiusTopLeft > 0 then
    begin
      HLTX1 := x1 + RadiusTopLeft;
      VLLY1 := y1 + RadiusTopLeft;
      BuildPolyArc(HLTX1, VLLY1, RadiusTopLeft, RadiusTopLeft, 180, 270, True, ArcPoints);   //0,90
      For i:=0 to ArcPoints.Count-1 do PolyPoints.Add(ArcPoints.Items[i]);
    End;

    // Coin Bas Gauche
    if RadiusBottomLeft > 0 then
    begin
      HLBX1 := x1 + RadiusBottomLeft;
      VLLY2 := y2 - RadiusBottomLeft;
      if Assigned(ArcPoints) then ArcPoints.Clear;
      BuildPolyArc(HLBX1, VLLY2,  RadiusBottomLeft, RadiusBottomLeft, 90, 180, true, ArcPoints);
      For i:=0 to ArcPoints.Count-1 do PolyPoints.Add(ArcPoints.Items[i]);
    End;

    // Coin Bas Droit
    if RadiusBottomRight > 0 then
    begin
      HLBX2 := x2 - RadiusBottomRight;
      VLRY2 := y2 - RadiusBottomRight;
      if Assigned(ArcPoints) then ArcPoints.Clear;
      BuildPolyArc(HLBX2, VLRY2, RadiusBottomLeft, RadiusBottomLeft, 0, 90, true, ArcPoints);
      For i:=0 to ArcPoints.Count-1 do PolyPoints.Add(ArcPoints.Items[i]);
    End;

    // Coin Haut Droite
    if RadiusTopRight > 0 then
    begin
      HLTX2 := x2 - RadiusTopRight;
      VLRY1 := y1 + RadiusTopRight;
      if Assigned(ArcPoints) then ArcPoints.Clear;
      BuildPolyArc(HLTX2, VLRY1,RadiusTopLeft, RadiusTopLeft, 270, 360, true, ArcPoints);
      For i:=0 to ArcPoints.Count-1 do PolyPoints.Add(ArcPoints.Items[i]);
    End;

    DrawPolyLine(PolyPoints, True);
    FreeAndNil(PolyPoints);
    FreeAndNil(ArcPoints);
end;

procedure TBZBitmapCanvas.DrawArc(cx, cy, rx, ry, StartAngle, EndAngle : Single; const ClockWise : Boolean);
{  cf : https://www.w3schools.com/tags/canvas_arc.asp
               270° (1.5*PI)
                |
(1*PI)  180° ---X--- 0° (0)
                |
               90° (0.5*PI)

 | = Ry
 --- = Rx
 X = (Cx, Cy)
}
Var
  Range, s,c: Single;
  Xs, Ys: Single;
  AColor: TBZColor;
  AngleCurrent, AngleDiff, AngleStep, AngleBegin, AngleEnd: Single;
Begin
  AColor := Pen.Color;
  //StrokePatternSize := 0;

  if (StartAngle = EndAngle) then exit;
  //if (StartAngle > EndAngle) then Swap(StartAngle, EndAngle);

  if ClockWise then  // Sens aiguille d'une montre (CW)
  begin
    AngleBegin := DegToRad(EndAngle);
    AngleEnd := DegToRad(StartAngle);
  end
  else
  begin  // Sens inverse aiguille d'une montre (CCW)
    AngleBegin := DegToRad(StartAngle);
    AngleEnd := DegToRad(EndAngle);
  end;

  if (AngleEnd >= AngleBegin) then
  begin // if end sup to begin, remove 2*Pi (360°)
    AngleEnd := AngleEnd - c2PI;
  end;

  AngleDiff := Abs(AngleEnd - AngleBegin); // the amount radian to travel
  //AngleStep := AngleDiff / Round(Max(Rx, Ry) * 0.1 + 5); // granulity of drawing, not too much, not too less
  AngleStep := 0.01;

  AngleCurrent := AngleBegin;
  Range := AngleCurrent - AngleDiff;

  while AngleCurrent >= Range do
  begin

    //Math.
    SinCos(AngleCurrent,s,c);
    xs := cX + (Rx * c);
    ys := cY + (Ry * s);

    If FPen.Style = ssSolid Then PutPixel(Round(xs), Round(ys), AColor);

    AngleCurrent := AngleCurrent - AngleStep; // always step down, rotate only one way to draw it
  end;

  //Dernier Point
  //Math.SinCos(AngleEnd,s,c);
  //xs := cX + (Rx * c);
  //ys := cY + (Ry * s);
  //
  //If FPen.Style = ssSolid Then PutPixel(Round(xs), Round(ys), AColor)

End;

{ Trace un cercle avec l'algorithme d'Andres }
//procedure TBZBitmapCanvas.DrawCircle(cx, cy, Radius : Single);
//Var
//  cix, ciy, X, Y, D, R2: Integer;
//  x1, x2, x3, x4, y1, y2, y3, y4: Integer;
//  AColor:      TBZColor;
//  //{$CODEALIGN VARMIN=16}
//  //pc,p1,p2,p3,p4,pp,ppi : TBZVector2i;
//  //{$CODEALIGN VARMIN=4}
//begin
//  if FPen.Width >1 then
//  begin
//
//  end
//  else
//  begin
//    If Radius <= 0 Then Exit;
//    if Antialias then
//    begin
//      DrawAntiAliasCircle(cx,cy,Radius);
//      Exit;
//    end;
//
//    AColor := FPen.Color;
//    cix := Round(cx);
//    ciy := Round(cy);
//    If Radius = 1 Then
//    Begin
//      PutPixel(cix, ciy, AColor);
//      PutPixel(cix - 1, ciy + 1, AColor);
//      PutPixel(cix - 1, ciy - 1, AColor);
//      PutPixel(cix + 1, ciy + 1, AColor);
//      PutPixel(cix + 1, ciy - 1, AColor);
//      exit;
//    End;
//
//    X := 0;
//    Y := Round(Radius);
//    D := Y - 1;
//    R2 := Y + Y;
//
//    While Y >= X Do
//    Begin
//      x1 := Cix + X;
//      y1 := Ciy + Y;
//
//
//      x2 := Cix + Y;
//      y2 := Ciy + X;
//
//
//      x3 := Cix - X;
//      y3 := Ciy - Y;
//
//
//      x4 := Cix - Y;
//      y4 := Ciy - X;
//
//      PutPixel(x1, y1, AColor);
//      PutPixel(x2, y2, AColor);
//      PutPixel(x3, y1, AColor);
//      PutPixel(x4, y2, AColor);
//      PutPixel(x1, y3, AColor);
//      PutPixel(x2, y4, AColor);
//      PutPixel(x3, y3, AColor);
//      PutPixel(x4, y4, AColor);
//
//      If (D >= (X + X)) Then
//      Begin
//        D := D - X - X - 1;
//        X := X + 1;
//      End
//      Else
//      If (D <= R2 - (Y - Y)) Then
//      Begin
//        D := D + Y + Y - 1;
//        Y := y -1;
//      End
//      Else
//      Begin
//        D := D + Y + Y - X - X - 2;
//        Y := y - 1;
//        X := X + 1;
//      End;
//    End;
//  end;
//end;

procedure TBZBitmapCanvas.DrawCircle(cx, cy, Radius : Single);
Var
  pts : TBZArrayOfFloatPoints;
begin
  BuildPolyCircle(vec2(cx, cy), Radius, pts);
  Polygon(pts);
  FreeAndNil(pts);
end;

{ Trace un cercle antialiasée (algorithme de Xiaolin Wu)  }
procedure TBZBitmapCanvas.DrawAntiAliasCircle(cx, cy, Radius : Single);

  Procedure Put4Pixels(ccx, ccy, dx, dy: Integer; AColor: TBZColor; fact : single); Inline;
  Begin
    If (dx > 0) And (dy > 0) Then
    Begin
      PutAlphaBlendPixel(ccX + dx, ccY + dY, AColor,fact);
      PutAlphaBlendPixel(ccX - dx, ccY + dY, AColor,fact);
      PutAlphaBlendPixel(ccX + dx, ccY - dy, AColor,fact);
      PutAlphaBlendPixel(ccX - dx, ccY - dy, AColor,fact);
    End
    Else If (dx = 0) Then
    Begin
      PutAlphaBlendPixel(ccX, ccY + dY, AColor,fact);
      PutAlphaBlendPixel(ccX, ccY - dY, AColor,fact);
    End
    Else If (dy = 0) Then
    Begin
      PutAlphaBlendPixel(ccX + Dx, ccY, AColor,fact);
      PutAlphaBlendPixel(ccX - dX, ccY, AColor,fact);
    End;

  End;

Var
  ciX, ciY,X, Y, D, R2, NX, NY: Integer;
  tr: Byte;
  Factor, xx, yy: Single;
  //Rsq,
  AColor, C1, C2: TBZColor;

Begin
  If Radius <= 0 Then Exit;
  ciX := Round(cx);
  ciY := Round(cy);
  AColor := FPen.Color;
  C1 := AColor;
  C2 := C1;

  If Radius = 1 Then
  Begin
    PutPixel(cix, ciy, AColor);
    PutAlphaBlendPixel(cix - 1, ciy + 1, AColor,1);
    PutAlphaBlendPixel(cix - 1, ciy - 1, AColor,1);
    PutAlphaBlendPixel(cix + 1, ciy + 1, AColor,1);
    PutAlphaBlendPixel(cix + 1, ciy - 1, AColor,1);
    exit;
  End;

  R2 := Round(Radius *Radius);
  D := round(Radius / System.Sqrt(2));  // On prend le quart (pas le bus !)

  For X := 0 To D Do
  Begin
    yy := System.Sqrt(R2 - X * X);// R*Sqrt(1-x*x/R2);
    NY := Floor(yy); // + 0.5);
    //Error := NY- yy; // - NY;
    Factor := Frac(YY);
    tr := ClampByte(Round(Factor * 255));
    C1.Alpha := Tr;
    C2.Alpha := 255 - tr;


    Put4Pixels(ciX, ciY, X, NY, C1,1-Factor);
    Put4Pixels(ciX, ciY, X, NY + 1, C2,Factor);
  End;

  For Y := 0 To D Do
  Begin
    XX := System.Sqrt(R2 - Y * Y); // R*Sqrt(1-Y*Y/R2);
    NX := Floor(XX);// + 0.5);
    //Error := NX - XX;
    Factor := Frac(XX);
    tr := ClampByte(Round(Factor * 255));
    C1.Alpha := Tr;
    C2.Alpha := 255 - tr;
    Put4Pixels(ciX, ciY, NX, Y, C1,1-Factor);
    Put4Pixels(ciX, ciY, NX + 1, Y, C2,Factor);
  End;
End;

{ Tracé d'une ellipse
  http://www.eazynotes.com/notes/computer-graphics/algorithms/mid-point-elliplse-algorithm.pdf
  https://www.experts-exchange.com/questions/20310389/Draw-ellipse.html
  http://www.angusj.com/delphitips/ellipses.php
  https://dai.fmph.uniba.sk/upload/0/01/Ellipse.pdf
  https://stackoverflow.com/questions/4650635/ellipse-thickness-algorithm

  }
//procedure TBZBitmapCanvas.DrawEllipse(cx, cy, Rx, Ry: Single);
//var
//  X, Y, cix, ciy, riy,rix,
//  XChange, YChange,
//  EllipseError,
//  TwoASquare, TwoBSquare,
//  StoppingX, StoppingY,
//  Rx2, Ry2 : Integer;
//  AColor : TBZColor;
//
//  procedure Plot4EllipsePoints(px,py:Integer);
//  var cyy,cyy1,cxx,cxx1 : Integer;
//  begin
//    cyy := ciy + py;
//    cyy1 := ciy - py;
//    cxx := cix + px;
//    cxx1 := cix - px;
//    PutPixel(cxx, cyy, AColor);  //Right-Bottom
//    PutPixel(cxx1, cyy, AColor); //Left-Bottom
//    PutPixel(cxx, cyy1, AColor); //Right-Top
//    PutPixel(cxx1, cyy1, AColor);//Left-Top
//  End;
//
//begin
//  cix := Round(cx);
//  ciy := Round(cy);
//  rix := Round(Rx);
//  riy := Round(Ry);
//  AColor:= FPen.Color;
//  if FPen.Width  > 1 then
//  begin
//  end
//  else
//  begin
//    if Antialias then
//    begin
//      DrawAntiAliasEllipse(cx,cy,rx,ry);
//      Exit;
//    end;
//    Rx2 := Round(Rx*Rx);
//    Ry2 := Round(Ry*Ry);
//    TwoASquare := 2*Rx2;
//    TwoBSquare := 2*Ry2;
//    X := Rix;
//    Y := 0;
//    XChange := Ry2*(1-2*Rix);
//    YChange := Rx2;
//    EllipseError := 0;
//    StoppingX := TwoBSquare*Rix;
//    StoppingY := 0;
//    { Partie inférieure }
//    while ( StoppingX>= StoppingY ) do
//    begin
//      Plot4EllipsePoints(X,Y);
//      inc(Y);
//      inc(StoppingY, TwoASquare);
//      inc(EllipseError, YChange);
//      inc(YChange,TwoASquare);
//      if (((EllipseError+EllipseError) + XChange) > 0 ) then
//      begin
//       dec(X);
//       dec(StoppingX, TwoBSquare);
//       inc(EllipseError, XChange);
//       inc(XChange,TwoBSquare)
//      end;
//    end;
//
//    X := 0;
//    Y := Riy;
//    XChange := Ry2;
//    YChange := Rx2*(1 -2*Riy);
//    EllipseError := 0;
//    StoppingX := 0;
//    StoppingY := TwoASquare*Riy;
//    { Partie supérieure }
//    while ( StoppingX <=StoppingY ) do
//    begin
//      Plot4EllipsePoints(X,Y);
//      inc(X);
//      inc(StoppingX, TwoBSquare);
//      inc(EllipseError, XChange);
//      inc(XChange,TwoBSquare);
//      if (((EllipseError+EllipseError) + YChange) > 0 ) then
//      begin
//       dec(Y);
//       dec(StoppingY, TwoASquare);
//       inc(EllipseError, YChange);
//       inc(YChange,TwoASquare)
//      end;
//    end;
//  end;
//End;

procedure TBZBitmapCanvas.DrawEllipse(cx, cy, Rx, Ry: Single);
var
  pts : TBZArrayOfFloatPoints;
begin
  BuildPolyEllipse(vec2(cx,cy),Rx, Ry, pts);
  Polygon(pts);
  //PolyLine(pts, False);
  FreeAndNil(pts);
end;

{ Trace une ellipse antialiasée (algorithme de Xiaolin Wu) }
procedure TBZBitmapCanvas.DrawAntiAliasEllipse(cx, cy, Rx, Ry : Single);

   Procedure Put4Pixels(ccx, ccy, dx, dy: Integer; AColor: TBZColor; fact : single); Inline;
   Begin
     If (dx > 0) And (dy > 0) Then
     Begin
       PutAlphaBlendPixel(ccX + dx, ccY + dY, AColor,fact);
       PutAlphaBlendPixel(ccX - dx, ccY + dY, AColor,fact);
       PutAlphaBlendPixel(ccX + dx, ccY - dy, AColor,fact);
       PutAlphaBlendPixel(ccX - dx, ccY - dy, AColor,fact);
     End
     Else If (dx = 0) Then
     Begin
       PutAlphaBlendPixel(ccX, ccY + dY, AColor,fact);
       PutAlphaBlendPixel(ccX, ccY - dY, AColor,fact);
     End
     Else If (dy = 0) Then
     Begin
       PutAlphaBlendPixel(ccX + Dx, ccY, AColor,fact);
       PutAlphaBlendPixel(ccX - dX, ccY, AColor,fact);
     End;

   End;

Var
  ciX, ciY, X, Y, D, Rx2, Ry2, NX, NY: Integer;
  tr: Byte;
  Rsq, Factor, xx, yy: Single;
  AColor, C1, C2: TBZColor;

Begin
  If (Rx <= 0) or (Ry <= 0) Then Exit;
  ciX := Round(cx);
  ciY := Round(cy);
  AColor := Pen.Color;
  C1 := AColor;
  C2 := C1;

  If (Rx = 1) And (Ry = 1) Then
  Begin
    PutPixel(cix, ciy, AColor);
    PutAlphaBlendPixel(cix - 1, ciy + 1, AColor,1);
    PutAlphaBlendPixel(cix - 1, ciy - 1, AColor,1);
    PutAlphaBlendPixel(cix + 1, ciy + 1, AColor,1);
    PutAlphaBlendPixel(cix + 1, ciy - 1, AColor,1);
    exit;
  End;

  Rx2 := Round(Rx * Rx);
  Ry2 := Round(Ry * Ry);
  Rsq := System.sqrt(Rx2 + Ry2);
  D := round(Rx2 / Rsq);  // On prend le quart (pas le bus !)
  For X := 0 To D Do
  Begin
    yy := Ry * System.Sqrt(1 - x * x / Rx2);
    NY := Floor(yy);
    Factor := Frac(YY);
    tr := ClampByte(Round(Factor * 255));
    C1.Alpha := Tr;
    C2.Alpha := 255 - tr;

    Put4Pixels(ciX, ciY, X, NY, C1,1-Factor);
    Put4Pixels(ciX, ciY, X, NY + 1, C2,Factor);
  End;

  D := round(Ry2 / Rsq);
  For Y := 0 To D Do
  Begin
    XX := Rx * System.Sqrt(1 - Y * Y / Ry2);
    NX := Floor(XX);
    Factor := Frac(XX);
    tr := ClampByte(Round(Factor * 255));
    C1.Alpha := Tr;
    C2.Alpha := 255 - tr;
    Put4Pixels(ciX, ciY, NX, Y, C1,1-Factor);
    Put4Pixels(ciX, ciY, NX + 1, Y, C2,Factor);
  End;
End;

procedure TBZBitmapCanvas.DrawTriangle(x1, y1, x2, y2, x3, y3 : Single);
Var
 Pts : TBZArrayOfFloatPoints;
begin
  Pts := TBZArrayOfFloatPoints.Create(3);
  Pts.Add(Vec2(x1,y1));
  Pts.Add(Vec2(x2,y2));
  Pts.Add(Vec2(x3,y3));
  DrawPolyLine(Pts, True);
  FreeAndNil(Pts);
  //Line(x1,y1,x2,y2);
  //Line(x2,y2,x3,y3);
  //Line(x3,y3,x1,y1);
end;

procedure TBZBitmapCanvas.Internal_HLine(x1, y1, x2 : Single);
Var
  x, xi2, xi1, y :     Integer;
  //StrokePatternSize: Integer;
  AColor: TBZColor;
Begin
  xi1 := Round(x1);
  xi2 := Round(x2); //xi1 + Round(L) -1;
  y := Round(y1);
  AColor := FBrush.Color;
  If xi1 > xi2 Then Swap(xi1,xi2);
  x := xi1;
  //if FPen.Style = ssPattern then
  //begin
  //  StrokePatternSize := 0;
  //  Case FPen.PatternStyle Of
  //    psDot: StrokePatternSize := Length(DotPenStrokePattern);
  //    psDash: StrokePatternSize := Length(DashPenStrokePattern);
  //    psDashDot: StrokePatternSize := Length(DashDotPenStrokePattern);
  //    psDashDotDot: StrokePatternSize := Length(DashDotDotPenStrokePattern);
  //  End;
  //  Repeat
  //    Case FPen.PatternStyle Of
  //      psDot: If DotPenStrokePattern[Y Mod StrokePatternSize] Then PutPixel(x, Y, AColor);
  //      psDash: If DashPenStrokePattern[Y Mod StrokePatternSize] Then PutPixel(x, Y, AColor);
  //      psDashDot: If DashDotPenStrokePattern[Y Mod StrokePatternSize] Then PutPixel(x, Y, AColor);
  //      psDashDotDot: If DashDotDotPenStrokePattern[Y Mod StrokePatternSize] Then PutPixel(x, Y, AColor);
  //    End;
  //    Inc(x);
  //  Until x > xi2;
  //end
  //else
  //begin
    Repeat
      PutPixel(x, Y, AColor);
      Inc(x);
    Until x > xi2;
//  end;
end;

procedure TBZBitmapCanvas.Internal_HLinePen(x1, y1, x2 : Single);
Var
  x, xi2, xi1, y :     Integer;
  //StrokePatternSize: Integer;
  AColor: TBZColor;
Begin
  xi1 := Round(x1);
  xi2 := Round(x2); //xi1 + Round(L) -1;
  y := Round(y1);
  AColor := FPen.Color;
  If xi1 > xi2 Then Swap(xi1,xi2);
  x := xi1;
  //if FPen.Style = ssPattern then
  //begin
  //  StrokePatternSize := 0;
  //  Case FPen.PatternStyle Of
  //    psDot: StrokePatternSize := Length(DotPenStrokePattern);
  //    psDash: StrokePatternSize := Length(DashPenStrokePattern);
  //    psDashDot: StrokePatternSize := Length(DashDotPenStrokePattern);
  //    psDashDotDot: StrokePatternSize := Length(DashDotDotPenStrokePattern);
  //  End;
  //  Repeat
  //    Case FPen.PatternStyle Of
  //      psDot: If DotPenStrokePattern[Y Mod StrokePatternSize] Then PutPixel(x, Y, AColor);
  //      psDash: If DashPenStrokePattern[Y Mod StrokePatternSize] Then PutPixel(x, Y, AColor);
  //      psDashDot: If DashDotPenStrokePattern[Y Mod StrokePatternSize] Then PutPixel(x, Y, AColor);
  //      psDashDotDot: If DashDotDotPenStrokePattern[Y Mod StrokePatternSize] Then PutPixel(x, Y, AColor);
  //    End;
  //    Inc(x);
  //  Until x > xi2;
  //end
  //else
  //begin
    Repeat
      //PutPixel(x, Y, AColor);
      FOwnerBitmap.setPixelAlphaBlend(x,y, AColor);
      Inc(x);
    Until x > xi2;
//  end;
end;

procedure TBZBitmapCanvas.Internal_HLineGradient(x1, y1, x2 : Integer);
var
  CurColor : TBZColor;
  pAngle,Coef, inv : Single;
  x : Longint;
  {$CODEALIGN VARMIN=16}
  cp, t, d :TBZVector2f;
  Corner :TBZVector2f;
  {$CODEALIGN VARMIN=4}

  NormalAngle: Single;
  CosTheta, SinTheta: Single;
  iCosTheta, iSinTheta: Integer;
  xSrc, ySrc: Integer;
  xDst, yDst: Integer;
  xODst, yODst: Integer;
  xOSrc, yOSrc: Integer;
  xPrime, yPrime: Integer;
  xDelta,yDelta:Integer;
  dstWidth, dstHeight: Integer;
  yPrimeSinTheta, yPrimeCosTheta: Integer;

Begin
  Coef :=0;

  // Calcul des cosine et sinus de l'angle
  if (Brush.Gradient.Kind = gkFreeAngle) then
  begin
    pAngle := Brush.Gradient.Angle; //90;
    NormalAngle := Frac(pAngle / 360.0) * 360.0;
    NormalAngle := DegToRadian(NormalAngle);
    Math.SinCos(-NormalAngle, SinTheta, CosTheta);
    iSinTheta := Trunc(SinTheta * (1 shl 16));
    iCosTheta := Trunc(CosTheta * (1 shl 16));
    { Prepararations des données }

    xOSrc := Brush.Bounds.CenterX;
    yOSrc := Brush.Bounds.CenterY;
    yPrime := yOSrc + (y1 - Brush.Bounds.Top);
    yPrimeSinTheta := yPrime * iSinTheta;
    //yPrimeCosTheta := yPrime * iCosTheta;
    xPrime := xOSrc;
  end;

  For x:=x1 to x2 do
  begin
    //cp.Create(x,y1,x,y1);
    cp.Create(x,y1);
    t.Create(Brush.Bounds.Left,Brush.Bounds.Top);
    inv := Brush.GetMaxDist.Reciprocal; //1/FMaxDist  ;

    Case Brush.Gradient.Kind of
      gkHorizontal,gkVertical :
      begin
        cp := cp - t;
        if Brush.Gradient.Kind = gkHorizontal then  Coef := cp.X
        else if Brush.Gradient.Kind = gkReflectHorz then  Coef := abs(cp.X-Brush.GetMaxDist)
        else Coef :=cp.Y;
        Coef := Coef * Inv;
        if coef>1.0 then coef := 1.0;//coef-1.0;
      end;
      gkFromTopLeft :
      begin
        d.Create(Brush.Bounds.Left, Brush.Bounds.Top);
        Coef := cp.Distance(d);
        Coef := Coef * Inv;
        if coef>1.0 then coef := 1.0;//coef-1.0;
      end;
      gkFromTopRight:
      begin
        Corner.Create(Brush.Bounds.Right, Brush.Bounds.Top);
        Coef :=(cp.Distance(Corner));
        Coef := Coef * Inv;
        if coef>1.0 then coef := 1.0;//coef-1.0;
      end;
      gkFreeAngle :
      begin
        xSrc := SmallInt((xPrime * iCosTheta - yPrimeSinTheta) shr 16) + xOSrc;
        //ySrc := SmallInt((xPrime * iSinTheta + yPrimeCosTheta) shr 16) + yOSrc;
        Coef:= XSrc;
        if Coef>Brush.GetMaxDist then Coef:=Brush.GetMaxDist;
        Coef:= (Coef * inv); // XSrc * Inv;
        if Coef < 0.0 then Coef := 0;
      end;
      gkRadial:
      begin
         Coef := System.Sqrt(Sqr( Brush.CenterPoint.X - cp.x) + Sqr( Brush.CenterPoint.Y - cp.y)); // Sqr(cp.y - Brush.CenterPoint.y));
         Coef := Coef * Inv;
         if coef>1.0 then coef := 1.0;//coef-1.0;
      end;
      gkReflectHorz,gkReflectVert :
      begin
        cp := cp - t;
        if Brush.Gradient.Kind = gkReflectVert then Coef := abs(cp.Y-Brush.GetMaxDist)
        else Coef := abs(cp.X-Brush.GetMaxDist);
        Coef := Coef * Inv; // Reflect first
        Coef:= 1.0 - Coef; //Reflect Last
      end;
      gkPolar :
      begin
        Corner.Create(Brush.CenterPoint.X, Brush.CenterPoint.Y);
        pAngle := Cp.AngleBetweenPointsInDeg(Corner);
        Coef:= pAngle * Inv;
      end;
      gkPyramid :
      begin
        cp := cp - t;
        Corner.Create(Brush.CenterPoint.X-x, Brush.CenterPoint.Y-cp.y);
        Coef :=(cp.Distance(Corner));
        Coef := Coef * Inv;
        if coef>1.0 then coef := 1.0
        //mw:=FMaxDist-(x+x);
        //Line(cp.x,cp.y,cp.x+mw,cp.y);
      end;
    End;

    CurColor := Brush.Gradient.ColorSteps.GetGradientColor(Coef);
    //Brush.Gradient.Palette.Items[Coef];
    PutPixel(X,y1,CurColor);
    if (Brush.Gradient.Kind = gkFreeAngle) then Dec(xPrime);

    // Case Pen.Style Of
      //   psSolid: PutPixel(X, y1, CurColor);
      //   psDot: If DotPenStrokePattern[X Mod StrokePatternSize] Then PutPixel(X, y1, CurColor);
      //   psDash: If DashPenStrokePattern[X Mod StrokePatternSize] Then PutPixel(X, y1, CurColor);
      //   psDashDot: If DashDotPenStrokePattern[X Mod StrokePatternSize] Then PutPixel(X, y1, CurColor);
      //   psDashDotDot: If DashDotDotPenStrokePattern[X Mod StrokePatternSize] Then PutPixel(X, y1, CurColor);

  end;
End;

procedure TBZBitmapCanvas.Internal_HLineMap(x1, y1, x2 : Integer);
var
  col : TBZColor;
  h, l,i,pXi, pYi : Integer;
  dy,dx,px : Single;
  //UV = 0,0
  //ST = 1,1

Begin
  if x1<0 then exit;
  if y1>FOwnerBitmap.MaxHeight then exit;
  if x2>FOwnerBitmap.MaxWidth then x2:=FOwnerBitmap.MaxWidth;

  L := Brush.Bounds.Width;//abs(x2-x1);
  H := Brush.Bounds.Height;
  Case Brush.Texture.MappingKind of
    tmkDefault :
    begin
      dx:= (Brush.Texture.Bitmap.Width / L);
      dy:= (Brush.Texture.Bitmap.Height / H);
    end;
    tmkAutoTile :
    begin
      dx:= (Brush.Texture.Bitmap.Width / Brush.GetMaxDist);
      dy:= (Brush.Texture.Bitmap.Height / Brush.GetMaxDist);
    end;
    tmkTiled :
    begin
      dx:= (Brush.Texture.Bitmap.Width / (L / Brush.Texture.TileX));
      dy:= (Brush.Texture.Bitmap.Height / (H / Brush.Texture.TileY));    //FTextureVertRatio
    end
  end;
  //GlobalLogger.LogNotice('MaxDist  = '+Brush.GetMaxDist.ToString);
  //GlobalLogger.LogNotice('Texture = '+Brush.Texture.Bitmap.Width.ToString+', '+Brush.Texture.Bitmap.Height.ToString+' --> '+FBrush.Bounds.AsRect.ToString);
  //GlobalLogger.LogNotice('Delta UV  = '+dx.ToString+', '+dy.ToString);
  pYi:=Round((y1-FBrush.Bounds.Top)*dy);
  //GlobalLogger.LogNotice('pYi  = '+pYi.ToString+' ==== '+y1.ToString);
  px:=0;
  for i:=x1 to x2 do
  begin
   pXi := Round(px);
   //GlobalLogger.LogNotice('UV = '+pXi.ToString+', '+pYi.ToString);
   Col := Brush.Texture.Bitmap.getPixelCycle(pXi, pYi);
   PutPixel(i,y1,Col);
   px:=px+dx;
  end;
End;

procedure TBZBitmapCanvas.DrawLineBrush(x1, y1, x2 : Integer);
Begin
  Case FBrush.Style Of
    bsSolid: Internal_HLine(x1, y1, x2);
    bsTexture: Internal_HLineMap(x1, y1, x2);
    bsPattern:
    Begin

    End;
    bsGradient : Internal_HLineGradient(x1,y1,x2);
  End;

End;

procedure TBZBitmapCanvas.DrawLinePen(x1, y1, x2 : Integer);
Begin
  Case FPen.Style Of
    ssSolid: Internal_HLinePen(x1, y1, x2);
    //ssTexture: ; //Internal_HLineMap(x1, y1, x2);
    ssPattern:
    Begin

    End;
    ssGradient :
    begin
      //Internal_HLineGradient(x1,y1,x2);
    End;
  End;

End;

procedure TBZBitmapCanvas.FillPolygon(Pts : TBZArrayOfFloatPoints);
Var
  i, j, c: Integer;
  PolygonRasterizer: TBZ2DPolygonTool;
  Buckets: TBZRastersList;
  OldWidth : word;
  OldColor : TBZColor;
Begin
  PolygonRasterizer := TBZ2DPolygonTool.Create;
  Try
    PolygonRasterizer.AssignPoints(Pts);
    Buckets := PolygonRasterizer.Rasters;
    For i := 0 To PolygonRasterizer.RastersLineCount-1 Do
    Begin
      c := High(Buckets[i]);
      {On parcours la liste}
      For j := 0 To c Do
      Begin
        DrawLineBrush(Buckets[i][J].xStart, I + PolygonRasterizer.StartY , Buckets[i][J].xEnd);
      End;
    End;
    // On Dessine le contour avec la couleur de fond
    if FPen.Style = ssClear then
    begin
      OldWidth := FPen.Width;
      OldColor := FPen.Color;
      FPen.Width := 1;
      FPen.Color := FBrush.Color;
      if Antialias  then DrawAntiaAliasPolyLine(Pts, True) else DrawPolyLine(Pts, True);
      FPen.Width := OldWidth;
      FPen.Color := OldColor;
    end;
  Finally
    FreeAndNil(PolygonRasterizer);
  End;
end;

procedure TBZBitmapCanvas.FillStrokePolygon(Pts : TBZArrayOfFloatPoints);
Var
  i, j, c: Integer;
  PolygonRasterizer: TBZ2DPolygonTool;
  Buckets: TBZRastersList;
  OldWidth : word;
Begin
  PolygonRasterizer := TBZ2DPolygonTool.Create;
  Try
    GlobalLogger.LogNotice('=============================================');
    GlobalLogger.LogNotice('FillStrokePolygon');
    GlobalLogger.LogNotice('=============================================');
    PolygonRasterizer.AssignPoints(Pts);
    For i := 0 to Pts.Count - 1 do
    begin
      GlobalLogger.LogStatus('Pts '+i.ToString  + pts.Items[i].ToString);
    end;
    GlobalLogger.LogNotice('====[ PolygonRasterizer.Rasters ]=========================================');

    Buckets := PolygonRasterizer.Rasters;

    GlobalLogger.LogNotice('PolygonRasterizer.StartY = '+PolygonRasterizer.StartY.toString);
    For i := 0 To PolygonRasterizer.RastersLineCount-1 Do
    Begin
      c := High(Buckets[i]);
      {On parcours la liste}
      For j := 0 To c Do
      Begin
        DrawLinePen(Buckets[i][J].xStart, I + PolygonRasterizer.StartY , Buckets[i][J].xEnd);
        //GlobalLogger.LogNotice('Pos Stoke Bucket  at : '+Inttostr(I + PolygonRasterizer.StartY)+' == ' +Buckets[i][J].xStart.ToString +' --> '+Buckets[i][J].xEnd.ToString);
        //Internal_HLinePen(Buckets[i][J].xStart, I + PolygonRasterizer.StartY , Buckets[i][J].xEnd);
      End;
    End;
    OldWidth := FPen.Width;
    FPen.Width := 1;
    if Antialias  then
      if (pts.Items[0].x <> pts.items[1] .x) or (pts.Items[0].y <> pts.items[1].y) or
         (pts.Items[1].x <> pts.items[2].x) or (pts.Items[1].y <> pts.items[2].y) or
         (pts.Items[0].x <> pts.items[3].x) or (pts.Items[0].y <> pts.items[3].y) then
      begin
        DrawAntiaAliasPolyLine(Pts, True);
      end
      else DrawPolyLine(Pts, True);

    FPen.Width := OldWidth;
  Finally
    FreeAndNil(PolygonRasterizer);
  End;
end;

procedure TBZBitmapCanvas.FillTriangle(x1, y1, x2, y2, x3, y3 : Single);
Var
  //YBuckets : Array of TPoint; //TBZRasterHLineList;  Je sais pas pourquoi mais ce genre de tableau dynamique ralentie la bloucle de remplissage lors de l'acces v := MonTableu[I].x;

  MXS, MXE:   Array[0..2047] Of Integer; // Avec 2 tableaux plus de problèmes de performances. BUG avec des tableaux dynamique lorsque l'on dessiner beaucoup de triangle dans une boucle par exemple
  MinX, MaxX, MinY, MaxY: Integer;
  MaxBuckets, I, xe, xs: Integer;
  OldColor:   TBZColor;
  xi1,yi1, xi2, yi2, xi3, yi3 : Integer;

  Procedure ComputeBucketLine(X1, Y1, X2, Y2: Integer);//; var buckets : array of TPoint );
  Var
    CurrentX, XIncr: Single;
    mw, Y, YY, Temp, cx: Integer;

  Begin
    If Y1 = Y2 Then exit;
    mw := FOwnerBitmap.MaxWidth; //SurfaceViewPort.Right-1;
    {On va toujours de bas en haut }
    If Y2 < Y1 Then
    Begin
      Temp := Y1;
      Y1 := Y2;
      Y2 := Temp;
      Temp := X1;
      X1 := X2;
      X2 := Temp;
    End;

    XIncr := ((X2 - X1) / (Y2 - Y1));
    CurrentX := X1;
    YY := Y1 - MinY;  // Nos Tableaux commencent a 0

    For Y := Y1 To Y2 Do
    Begin
      If (Y < 0) Or (Y > FOwnerBitmap.MaxHeight) Then //
      Begin
        CurrentX := CurrentX + XIncr;
        continue;
      End;

      cx := Round(CurrentX);

      If MXS[YY] = 16000 Then   //Init
      Begin
        MXS[YY] := cx;
        If (cx > mw) Then
          MXE[YY] := mw
        Else If cx > MXE[YY] Then
          MXE[YY] := cx;
      End
      Else
      Begin
        If cx > 0 Then
        Begin
          If cx < MXS[YY] Then  //Start
          Begin
            If (cx > mw) Then
            Begin
              MXS[YY] := mw;
              MXE[YY] := mw;
              CurrentX := CurrentX + XIncr;
              Inc(YY);
              continue;
            End
            Else
              MXS[YY] := cx;
          End;

          If (cx > MXE[YY]) Then  //End
          Begin
            If (cx > mw) Then
              MXE[YY] := mw
            Else
              MXE[YY] := cx;
          End;
        End;
      End;
      CurrentX := CurrentX + XIncr;
      Inc(YY);
    End;
  End;

Begin
  xi1 := Round(x1);
  yi1 := Round(y1);
  xi2 := Round(x2);
  yi2 := Round(y2);
  xi3 := Round(x3);
  yi3 := Round(y3);
  // On clippe notre "Buckets" au maximum des dimensions de l'image
  MinY := Max(0, BZMath.min(yi1, yi2, yi3));
  MaxY := Min(FOwnerBitmap.MaxHeight, BZMath.max(yi1, yi2, yi3));
  MaxBuckets := (MaxY - MinY);

  If MaxBuckets <= 1 Then exit; // Rien a dessiner on sort

  //else if (Brush.Style = bsGradient) or (Brush.Style = bsTexture) then
  //begin
  //  MinX := Max(0, BZMath.min(x1, x2, x3));
  //  MaxX := Min(Owner.MaxWidth, BZMath.max(x1, x2, x3));
  //  Brush.LeftTop.Create(MinX,MinY);
  //  Brush.RightBottom.Create(MaxX,MaxY);
  //  if (Brush.Style = bsGradient) then initGradient else initTextureMapping;
  //
  //End;
  //  SetLength(YBuckets,MaxBuckets);

  //MXS := Nil;
  //MXE := Nil;
  //SetLength(MXS, MaxBuckets);
  //SetLength(MXE, MaxBuckets);

  //ClearBuckets;
  For i := 0 To MaxBuckets - 1 Do
  Begin
    MXS[I] := 16000;
    MXE[I] := 0;
  End;
  ComputeBucketLine(xi1, yi1, xi2, yi2);//, YBuckets);
  ComputeBucketLine(xi2, yi2, xi3, yi3);//, YBuckets);
  ComputeBucketLine(xi3, yi3, xi1, yi1);//, YBuckets);
  //GlobalLogger.LogNotice('FILL TRIANGLE --> MaxBuckets = '+MaxBuckets.ToString);
  For I := 0 To MaxBuckets-1 Do
  Begin
   { L'acces a ce type ralenti les performances je sais pas pourquoi ?????
     xs := YBuckets[I].x;
     xe := YBuckets[I].y; }
    xs := MXS[I];
    xe := MXE[I];
    //GlobalLogger.LogNotice('FILL TRIANGLE --> XS = '+XS.ToString+' ==> XE = '+XE.ToString);
    If ((xe - xs) > 0) Then  DrawLineBrush(xs, MinY + I, xe);
  End;

  //SetLength(MXS, 0);
  //SetLength(MXE, 0);
  //MXS := Nil;
  //MXE := Nil;
End;

procedure TBZBitmapCanvas.FillRectangle(x1, y1, x2, y2 : Single);
Var
  y, yi1,yi2, xi1,xi2 : Integer;
begin
  xi1:= Round(x1);
  xi2:= Round(x2);
  yi1:= Round(y1);
  yi2:= Round(y2);
  For y:= yi1 to yi2 do
  begin
    DrawLineBrush(xi1,y,xi2);
  end;
end;

procedure TBZBitmapCanvas.FillRoundedRectangle(x1, y1, x2, y2 : Single; const Rx : Single; const Ry : Single);
Var
 ArcPoints : TBZArrayOfFloatPoints;
 PolyPoints : TBZArrayOfFloatPoints;
 i : integer;

  //ATLX, ATLY, ATRX, ATRY, ABLX, ABLY, ABRX, ABRY : Single;
  HLTX1, HLTX2 : Single;
  //HLTY1, HLTY2 : Single; // Haut
  HLBX1, HLBX2  : Single;
  //HLBY1, HLBY2 : Single; // Bas
  //VLLX1, VLLX2  : Single;
  VLLY1, VLLY2 : Single; // Gauche
  //VLRX1, VLRX2 : Single;
  VLRY1, VLRY2 : Single; // Droite
  Radius, RadiusTopLeft, RadiusTopRight, RadiusBottomLeft, RadiusBottomRight : Single;
  //P1, P2 : TBZFloatPoint;

begin
    PolyPoints := TBZArrayOfFloatPoints.Create(64);

    Radius := Max(Rx, Ry);
    HLTX1 := x1;
    //HLTY1 := y1;
    HLTX2 := x2;
    //HLTY2 := y1;

    HLBX1 := x1;
    //HLBY1 := y2;
    HLBX2 := x2;
    //HLBY2 := y2;

    //VLLX1 := x1;
    VLLY1 := y1;
    //VLLX2 := x1;
    VLLY2 := y2;

    //VLRX1 := x2;
    VLRY1 := y1;
    //VLRX2 := x2;
    VLRY2 := y2;

    RadiusTopLeft := Radius;
    RadiusBottomLeft := Radius;
    RadiusTopRight := Radius;
    RadiusBottomRight := Radius;

    // Coin Haut Gauche
    if RadiusTopLeft > 0 then
    begin
      HLTX1 := x1 + RadiusTopLeft;
      VLLY1 := y1 + RadiusTopLeft;
      BuildPolyArc(HLTX1, VLLY1, RadiusTopLeft, RadiusTopLeft, 180, 270, True, ArcPoints);   //0,90
      For i:=0 to ArcPoints.Count-1 do PolyPoints.Add(ArcPoints.Items[i]);
    End;

    // Coin Bas Gauche
    if RadiusBottomLeft > 0 then
    begin
      HLBX1 := x1 + RadiusBottomLeft;
      VLLY2 := y2 - RadiusBottomLeft;
      ArcPoints.Clear;
      BuildPolyArc(HLBX1, VLLY2,  RadiusBottomLeft, RadiusBottomLeft, 90, 180, true, ArcPoints);
      For i:=0 to ArcPoints.Count-1 do PolyPoints.Add(ArcPoints.Items[i]);
    End;

    // Coin Bas Droit
    if RadiusBottomRight > 0 then
    begin
      HLBX2 := x2 - RadiusBottomRight;
      VLRY2 := y2 - RadiusBottomRight;
      ArcPoints.Clear;
      BuildPolyArc(HLBX2, VLRY2, RadiusBottomLeft, RadiusBottomLeft, 0, 90, true, ArcPoints);
      For i:=0 to ArcPoints.Count-1 do PolyPoints.Add(ArcPoints.Items[i]);
    End;

    // Coin Haut Droite
    if RadiusTopRight > 0 then
    begin
      HLTX2 := x2 - RadiusTopRight;
      VLRY1 := y1 + RadiusTopRight;
      ArcPoints.Clear;
      BuildPolyArc(HLTX2, VLRY1,RadiusTopLeft, RadiusTopLeft, 270, 360, true, ArcPoints);
      For i:=0 to ArcPoints.Count-1 do PolyPoints.Add(ArcPoints.Items[i]);
    End;

    FillPolygon(PolyPoints);
    FreeAndNil(PolyPoints);
end;

procedure TBZBitmapCanvas.FillArc(StartX, StartY, EndX, EndY, StartAngle, EndAngle : Single);
begin
  // TODO
end;

procedure TBZBitmapCanvas.FillCircle(cx, cy, Radius : Single);
Var
  cix,ciy, X, Y, D, R2: Integer;
  x1, x2, x3, x4, y1, y2, y3, y4: Integer;
  OldColor : TBZColor;
Begin
  If Radius <= 0 Then Exit;
  //if (Brush.GetStyle = bsGradient) or (Brush.GetStyle = bsTexture) then
  //begin
  //  rv.Create(R,R);
  //  pt.Create(Cx,Cy);
  //
  //  Brush.LeftTop := TBZVector2f(pt - r).Round;
  //  Brush.RightBottom := TBZVector2f(pt + r).Round;
  //  if (Brush.Style = bsGradient) then InitGradient else initTextureMapping;
  //end;
  X := 0;
  Y := Round(Radius);
  D := Y - 1;
  R2 := Y + Y;
  cix := Round(cx);
  ciy := Round(cy);
  While Y >= X Do
  Begin
    x1 := Cix + X;
    y1 := Ciy + Y;
    x2 := Cix + Y;
    y2 := Ciy + X;
    x3 := Cix - X;
    y3 := Ciy - Y;
    x4 := Cix - Y;
    y4 := Ciy - X;

   // l := x1 - x3;
    DrawLineBrush(x3, y1, x1);
    DrawLineBrush(x3, y3, x1);

   // l := x2 - x4;
    DrawLineBrush(x4, y2, x2);
    DrawLineBrush(x4, y4, x2);

    If D >= X + X Then
    Begin
      D := D - X - X - 1;
      Inc(X);
    End
    Else
    If D <= R2 - Y - Y Then
    Begin
      D := D + Y + Y - 1;
      Dec(Y);
    End
    Else
    Begin
      D := D + Y + Y - X - X - 2;
      Dec(Y);
      Inc(X);
    End;
  End;

  if Antialias and (FPen.Style = ssClear) then
  begin
    OldColor := FPen.Color;
    FPen.Color := FBrush.Color;
    DrawAntiAliasCircle(cx, cy, Radius);
    FPen.Color := OldColor;
  end;
end;

procedure TBZBitmapCanvas.FillEllipse(cx, cy, Rx, Ry : Single);
var
  X, Y, cix, ciy, riy,rix,
  XChange, YChange,
  EllipseError,
  TwoASquare, TwoBSquare,
  StoppingX, StoppingY,
  Rx2, Ry2 : Integer;
  OldColor : TBZColor;

  procedure FillLineEllipse(px,py:Integer);
  var cyy,cyy1,cxx,cxx1 : Integer;
  begin
    cyy := ciy + py;
    cyy1 := ciy - py;
    cxx := cix + px;
    cxx1 := cix - px;
    DrawLineBrush(cxx1, cyy, cxx);
    DrawLineBrush(cxx1, cyy1, cxx);
    //PutPixel(cxx, cyy, AColor);  //Right-Bottom
    //PutPixel(cxx1, cyy, AColor); //Left-Bottom
    //PutPixel(cxx, cyy1, AColor); //Right-Top
    //PutPixel(cxx1, cyy1, AColor);//Left-Top
  End;

begin
  cix := Round(cx);
  ciy := Round(cy);
  rix := Round(Rx);
  riy := Round(Ry);

  if (Rx  <= 1) and (Ry<=1) then
  begin
  end
  else
  begin
    Rx2 := Round(Rx*Rx);
    Ry2 := Round(Ry*Ry);
    TwoASquare := 2*Rx2;
    TwoBSquare := 2*Ry2;
    X := Rix;
    Y := 0;
    XChange := Ry2*(1-2*Rix);
    YChange := Rx2;
    EllipseError := 0;
    StoppingX := TwoBSquare*Rix;
    StoppingY := 0;
    { Partie inférieure }
    while ( StoppingX>= StoppingY ) do
    begin
      FillLineEllipse(X,Y);
      inc(Y);
      inc(StoppingY, TwoASquare);
      inc(EllipseError, YChange);
      inc(YChange,TwoASquare);
      if (((EllipseError+EllipseError) + XChange) > 0 ) then
      begin
       dec(X);
       dec(StoppingX, TwoBSquare);
       inc(EllipseError, XChange);
       inc(XChange,TwoBSquare)
      end;
    end;

    X := 0;
    Y := Riy;
    XChange := Ry2;
    YChange := Rx2*(1 -2*Riy);
    EllipseError := 0;
    StoppingX := 0;
    StoppingY := TwoASquare*Riy;
    { Partie supérieure }
    while ( StoppingX <=StoppingY ) do
    begin
      FillLineEllipse(X,Y);
      inc(X);
      inc(StoppingX, TwoBSquare);
      inc(EllipseError, XChange);
      inc(XChange,TwoBSquare);
      if (((EllipseError+EllipseError) + YChange) > 0 ) then
      begin
       dec(Y);
       dec(StoppingY, TwoASquare);
       inc(EllipseError, YChange);
       inc(YChange,TwoASquare)
      end;
    end;

    if Antialias and (FPen.Style = ssClear) then
    begin
      OldColor := FPen.Color;
      FPen.Color := FBrush.Color;
      DrawAntiAliasEllipse(cx, cy, Rx, Ry);
      FPen.Color := OldColor;
    end;
  end;

End;

{ Références :
  - https://en.wikipedia.org/wiki/Flood_fill
  - https://fr.wikipedia.org/wiki/Algorithme_de_remplissage_par_diffusion
  - http://www.ques10.com/p/18403/compare-flood-fill-and-boundary-fill-algorithm-i-1

  Note : Remplissage d'une région. Algorithme récursif "Boundary FloodFill"
         Ces fonctions sont très rapide, mais la récursion pose problème car la pile d'appel devient très vite surchargé
         et provoque une exception de type : "Stack Overflow"
         La solution est donc de gérer nous même une "pile" (liste LIFO) de points.

Autres Algorithmes de remplissage possibles :
 - http://www.ibiblio.org/e-notes/MSet/TheAlmondBreadHomepage.htm#boundary
 - https://www.codeproject.com/Tips/461694/Boundary-Trace-FloodFill
}

Var
  StackPoint : Array of TBZVector2i;  //Pile de points globale
  StackIndex : Integer;               //Index dans la pile de points

procedure InitStack; // Initialisation de la pile de point
begin
  StackIndex := 0;
  SetLength(StackPoint, 1024);
end;

procedure FreeStack; // Libération de la pile de point
begin
  StackIndex := 0;
  SetLength(StackPoint, 0);
  StackPoint := nil;
end;

Procedure StackPush(Point : TBZVector2i); // Empilement d'un point dans la pile
Begin
  Inc(StackIndex);
  If StackIndex > high(StackPoint) Then
  Begin
    SetLength(StackPoint, high(StackPoint) + 256);
  End;
  StackPoint[StackIndex] := Point;
End;

Procedure StackPush(X,Y : Integer);
Begin
  Inc(StackIndex);
  If StackIndex > high(StackPoint) Then
  Begin
    SetLength(StackPoint, high(StackPoint) + 256);
  End;
  StackPoint[StackIndex].x := X;
  StackPoint[StackIndex].y := Y;
End;

Function StackPop(Var Point : TBZVector2i) : Boolean; // Récupération du dernier point empilé dans la pile
Begin
  Result := True;
  Point := StackPoint[StackIndex];
  Dec(StackIndex);
  if StackIndex < 0 then result := False;
End;

procedure TBZBitmapCanvas.FloodFill_Boundary4(px, py : Single; const SearchColor, NewColor : TBZColor);
var
  pxi, pyi, MaxH, MaxW : Integer;
  P: TBZVector2i;
begin
  if (SearchColor = NewColor) then exit;
  pxi := Round(px);
  pyi := Round(py);
  if (FOwnerBitmap.getPixel(pxi, pyi)<>SearchColor) then exit;
  InitStack;
  p.Create(pxi,pyi);
  MaxH := FOwnerBitmap.MaxHeight;
  MaxW := FOwnerBitmap.MaxWidth;
  StackPush(p);
  While (StackPop(p)) do
  begin
    PutPixel(P.x, P.y, NewColor);

    if ((P.x + 1) <= MaxW) then
      if (FOwnerBitmap.getPixel(P.x + 1, P.y) = SearchColor) then StackPush(P.x + 1, P.y);

    if ((P.x - 1) >= 0) then
      if (FOwnerBitmap.getPixel(P.x - 1, P.y) = SearchColor) then StackPush(P.x - 1, P.y);

    if ((P.y - 1) >= 0) then
      if (FOwnerBitmap.getPixel(P.x, P.y - 1) = SearchColor) then StackPush(P.x, P.y - 1);

    if ((P.y + 1) <= MaxH ) then
      if (FOwnerBitmap.getPixel(P.x, P.y + 1) = SearchColor) then StackPush(P.x, P.y + 1);

  end;
  FreeStack;
end;

procedure TBZBitmapCanvas.FloodFill_Boundary8(px, py : Single; const SearchColor, NewColor : TBZColor);
var
  pxi, pyi, MaxH, MaxW : Integer;
  {$CODEALIGN VARMIN=16}
  P : TBZVector2i;
  {$CODEALIGN VARMIN=4}
  //SrcColor : TBZColor;
begin
  if (SearchColor = NewColor) then exit;
  InitStack;
  pxi := Round(px);
  pyi := Round(py);
  p.Create(pxi,pyi);
  MaxH := FOwnerBitmap.MaxHeight;
  MaxW := FOwnerBitmap.MaxWidth;
  StackPush(p);
  While (StackPop(p)) do
  begin
    PutPixel(P.x, P.y, NewColor);

    if ((P.x + 1) <= MaxW) then
      if (FOwnerBitmap.getPixel(P.x + 1, P.y) = SearchColor) then StackPush(P.x + 1, P.y);

    if ((P.x - 1) >= 0) then
      if (FOwnerBitmap.getPixel(P.x - 1, P.y) = SearchColor) then StackPush(P.x - 1, P.y);

    if ((P.y - 1) >= 0) then
      if (FOwnerBitmap.getPixel(P.x, P.y - 1) = SearchColor) then StackPush(P.x, P.y - 1);

    if ((P.y + 1) <= MaxH ) then
      if (FOwnerBitmap.getPixel(P.x, P.y + 1) = SearchColor) then StackPush(P.x, P.y + 1);

    if ((P.x + 1)<=MaxW) and ((P.y + 1)<=MaxH) then
      if (FOwnerBitmap.getPixel(P.x - 1, P.y + 1) = SearchColor) then StackPush(P.x + 1, P.y + 1);

    if ((P.x + 1)<=MaxW) and ((P.y - 1)>=0) then
      if (FOwnerBitmap.getPixel(P.x + 1, P.y - 1) = SearchColor) then StackPush(P.x + 1, P.y - 1);

    if ((P.x - 1)>=0) and ((P.y + 1)<=MaxH) then
      if (FOwnerBitmap.getPixel(P.x - 1, P.y + 1) = SearchColor) then StackPush(P.x - 1, P.y + 1);

    if ((P.y - 1)>= 0) and ((P.y - 1)>=0) then
      if (FOwnerBitmap.getPixel(P.x - 1, P.y - 1) = SearchColor) then StackPush(P.x - 1, P.y - 1);
  end;
  FreeStack;
end;

procedure TBZBitmapCanvas.FloodFill_ScanLine(px, py : Single; const SearchColor, NewColor : TBZColor);
Var
  MaxH, MaxW, pxi, pyi : Integer;
  GoUp, GoDown : Boolean;
  OriginPoint, CurrentPoint, TempPoint : TBZVector2i;
  SrcColor : TBZColor;
begin
  if (SearchColor = NewColor) then exit;
  InitStack;
  pxi := Round(px);
  pyi := Round(py);
  MaxW := FOwnerBitmap.MaxWidth;
  MaxH := FOwnerBitmap.MaxHeight;
  OriginPoint.Create(pxi,pyi);
  GoUp := False;
  GoDown := False;
  StackPush(OriginPoint);
  While (StackPop(CurrentPoint{%H-})) do
  begin
    TempPoint := CurrentPoint;

    SrcColor := FOwnerBitmap.getPixel(TempPoint.x, TempPoint.y);
    While (TempPoint.x >=0) and  (SrcColor = SearchColor) do
    begin
      Dec(TempPoint.x);
      SrcColor := FOwnerBitmap.getPixel(TempPoint.x, TempPoint.y);
    end;

    GoUp := False;
    GoDown := False;
    inc(TempPoint.x);

    SrcColor := FOwnerBitmap.getPixel(TempPoint.x, TempPoint.y);
    While ((TempPoint.x<=MaxW) and (SrcColor = SearchColor)) do
    begin
      PutPixel(TempPoint.X, TempPoint.y, NewColor);
      if (Not(GoUp) and (TempPoint.y>0)) and (FOwnerBitmap.getPixel(TempPoint.x, TempPoint.y - 1) = SearchColor) then
      begin
        StackPush(TempPoint.x, TempPoint.y - 1);
        GoUp := True;
      end
      else if ((GoUp and (TempPoint.y>0)) and ((FOwnerBitmap.getPixel(TempPoint.x, TempPoint.y - 1) <> SearchColor))) then GoUp := False;

      if (Not(GoDown) and (TempPoint.y<MaxH)) and (FOwnerBitmap.getPixel(TempPoint.x, TempPoint.y + 1) = SearchColor) then
      begin
        StackPush(TempPoint.x, TempPoint.y + 1);
        GoDown := True;
      end
      else if ((GoDown and (TempPoint.y<MaxH)) and ((FOwnerBitmap.getPixel(TempPoint.x, TempPoint.y + 1) <> SearchColor))) then GoDown := False;

      Inc(TempPoint.x);
      SrcColor := FOwnerBitmap.getPixel(TempPoint.x, TempPoint.y);
    end;
  end;
end;



{%endregion}

{%region ====[ TBZBitmap ]======================================================}

Constructor TBZBitmap.Create(AOwner: TPersistent; AWidth, AHeight: Integer);  //Const Indexed : Boolean=false
Begin
  Inherited Create(AOwner, aWidth, aHeight);
  FMask := nil;
  FUseMask := False;
  FCanvas := TBZBitmapCanvas.Create(Self);
  //Canvas.SurfaceViewPort.Right := AWidth-1;
  //Canvas.SurfaceViewPort.Bottom := AHeight-1;
  FTransformations := TBZBitmapTransformations.Create(Self);
  FColorFilters := TBZBitmapColorFilters.Create(Self);
  FBlurFilters := TBZBitmapBlurFilters.Create(Self);
  FConvolutionFilters := TBZBitmapConvolutionFilters.Create(Self);
  FRenderFilters := TBZBitmapRenderFilters.Create(Self);
  FEffectFilters := TBZBitmapEffectFilters.Create(Self);
  FDeformationFilters := TBZBitmapDeformationFilters.Create(Self);
  FSegmentationFilters := TBZBitmapSegmentationFilters.Create(Self);
  FMorphologicalFilters := TBZBitmapMorphologicalFilters.Create(Self);
  FThresHoldFilters  := TBZBitmapThresholdFilters.Create(Self);

  FHistogram := TBZHistogram.Create(Self);
  FAnimateTimer := TTimer.Create(nil);
  With FAnimateTimer do
  begin
    Enabled := False;
    Interval := 1000;
    OnTimer := @DoOnAnimateTime
  End;

  FAnimationSpeed := 1;
  FIsAnimate := False;
  FIsAnimatePaused := False;
  FFrameIndex := 0;
  FCurrentLoopIndex := -1;
  FAnimateLoop := 0;
  FAnimateBuffer := nil;
  FLastDrawMode := 0;
End;

Constructor TBZBitmap.Create(aWidth, aHeight: Integer);
Begin
  Create(nil, aWidth, aHeight);
End;

Constructor TBZBitmap.Create;
Begin
  Create(nil, 1, 1);
End;

Constructor TBZBitmap.Create(aWidth, aHeight: Integer; FillColor: TBZColor);
begin
  Create(aWidth, aHeight);
  Clear(FillColor);
end;

Destructor TBZBitmap.Destroy;
Begin
  FreeAndNil(FAnimateTimer);
  FreeAndNil(FHistogram);
  if (FMask<>nil) then FreeAndNil(FMask);
  FreeAndNil(FThresHoldFilters);
  FreeAndNil(FMorphologicalFilters);
  FreeAndNil(FSegmentationFilters);
  FreeAndNil(FDeformationFilters);
  FreeAndNil(FEffectFilters);
  FreeAndNil(FRenderFilters);
  FreeAndNil(FConvolutionFilters);
  FreeAndNil(FBlurFilters);
  FreeAndNil(FColorFilters);
  FreeAndNil(FTransformations);
  FreeAndNil(FCanvas);
  Inherited Destroy;
End;

Procedure TBZBitmap.Assign(Source: TPersistent);
Begin
  Inherited Assign(Source);
End;

function TBZBitmap.CreateClone : TBZBitmap;
Var
  Clone : TBZBitmap;
Begin
  Result:=nil;
  Try
    Clone := TBZBitmap.Create(0,0);
    Clone.Assign(self);
  Finally
    Result:=Clone;
  End;
End;

//procedure TBZBitmap.SetUseMask(const Value : Boolean);
//begin
//  if FUseMask = Value then exit;
//  If (Value = true) and (FMask = nil) then
//  begin
//    FMask := TBZBitmapMask.Create;
//    FMask.SetSize(Self.Width, Self.Height);
//  End;
//  FUseMask := Value;
//End;

Procedure TBZBitmap.SetSize(NewWidth, NewHeight: Integer);
Begin
  Inherited SetSize(NewWidth, NewHeight);
  if FUseMask then if (FMask<>nil) then FMask.SetSize(NewWidth, NewHeight);
  //Canvas.SurfaceViewPort.Right := NewWidth-1;
  //Canvas.SurfaceViewPort.Bottom := NewHeight-1;
End;

//procedure TBZBitmap.Changed();
//Begin
//  NotifyChange(self);
//End;


Procedure TBZBitmap.SaveToFile(Const FileName: String);
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
  Try
    //GlobalLogger.LogNotice('--> Format found : '+TempImage.ClassName);
    //tempImage.Assign(Self);
    tempImage.AssignBitmapAsRef(Self);
    tempImage.SaveToFile(FileName);
  Finally
    FreeAndNil(tempImage);
  End;
End;

procedure TBZBitmap.ApplyMask(Const FreeAfter: Boolean);
var
  SrcPtr, DstPtr : PBZColor;
  I : Integer;
  AColor: TBZColor;
  ADelta : Single;
begin
  if Not(Assigned(FMask)) then exit;
  Assert((FMask.Width<>Width) or (FMask.Height<>Height),'Le masque doit avoir les même dimensions que le bitmap');
  SrcPtr := FMask.getSurfaceBuffer;
  DstPtr := GetSurfaceBuffer;
  I:=0;


    While (I<=FMask.MaxSize) do
    begin
     (* if not(FMask.ApplyAlpha) then
      begin
        if SrcPtr^.Red < 255 then DstPtr^:=clrTransparent; //On prend la canal Red comme référence
      end
      else
      begin*)
        if SrcPtr^.Red>0 then
        begin
          ADelta := SrcPtr^.Red*_FloatColorRatio;
          AColor.Red := Round(DstPtr^.Red*ADelta);
          AColor.Green := Round(DstPtr^.Green*ADelta);
          AColor.Blue := Round(DstPtr^.Blue*ADelta);
          AColor.Alpha := SrcPtr^.Red;// DstPtr^.Alpha;
        End
        else AColor := clrTransparent;
      DstPtr^:=AColor;
      inc(I);
      Inc(SrcPtr);
      Inc(DstPtr);
    End;


  if FreeAfter then
  begin
    FreeAndNil(FMask);
    FUseMask := False;
  end;
End;

procedure TBZBitmap.ApplyMask(MaskBmp: TBZBitmap);
var
  SrcPtr, DstPtr : PBZColor;
  I : Integer;
  SrcColor, DstColor: TBZColor;
begin
  Assert((MaskBmp.Width<>Width) and (MaskBmp.Height<>Height),'Le masque doit avoir les même dimensions que le bitmap');
  SrcPtr := MaskBmp.getSurfaceBuffer;
  DstPtr := GetSurfaceBuffer;
  I:=0;
  While (I<=FMask.MaxSize) do
  begin
    if SrcPtr^.Red>0 then
    begin
      DstColor := DstPtr^;
      DstColor.Alpha := SrcPtr^.Red;// DstPtr^.Alpha;
    End
    else DstColor := clrTransparent;

    DstPtr^ := DstColor;
    inc(I);
    Inc(SrcPtr);
    Inc(DstPtr);
  End;
end;

procedure TBZBitmap.ArithmeticBlend(BlendMap : TBZBitmap; ColorOperator : TBZColorCombineMode);
begin
  Self.PutImageBlend(BlendMap,0, 0, ColorOperator, 255);
end;

procedure TBZBitmap.RenderFrame(Index : Integer);
var
  Src : TBZBitmap;
  pTop, pLeft : Integer;
  AlphaMode : TBZBitmapAlphaMode;
  CurrentDrawMode : Byte;
Begin
  Src := TBZBitmap(Layers.Items[Index].Bitmap);
  pLeft := Layers.Items[Index].Left;
  pTop  := Layers.Items[Index].Top;
  AlphaMode := amNone;
  if FAnimateBuffer = nil then FAnimateBuffer := TBZBitmap.Create(Self.Width,Self.Height);
  GlobalLogger.LogStatus('Current Frame = ' + Index.ToString);
    if Src.ImageDescription.HasAlpha  then
    begin
      GlobalLogger.LogStatus('HAS ALPHA');
      GlobalLogger.LogStatus('LAYER IS TRANSPARENT = '  + Layers.Transparent.ToString());
      if Index = 0 then clear(clrTransparent);
      AlphaMode := amAlphaCheck;
    End
    else
    begin
     GlobalLogger.LogStatus('NO ALPHA');
     AlphaMode := amNone;
     if Index = 0 then Clear(Layers.BackgroundColor);
    End;

    //PutImage(Src, 0, 0, Src.Width, Src.Height, pLeft, pTop, dmSet);
    //if Layers.Items[Index].DrawMode = 1 then FAnimateBuffer.Assign(Self);
    //FRestoreBitmap := Self.CreateClone;

    with Layers.Items[Index] do
    begin
     CurrentDrawMode := DrawMode;

      Case CurrentDrawMode of
        0:  //None
        begin
          GlobalLogger.LogStatus('Current Draw Mode = NONE');
          PutImage(Src,0,0, Src.Width, Src.Height,pLeft,pTop,dmSet,AlphaMode); //PutImage(Src,0,0, Src.Width, Src.Height,pLeft,pTop);
        end;
        1: // Keep
        begin
          GlobalLogger.LogStatus('Current Draw Mode = KEEP');
          GlobalLogger.LogStatus('Last Draw Mode = ' + FLastDrawMode.ToString);
          if FLastDrawMode = 3 then // Erase
          begin
            If (Src.ImageDescription.HasAlpha) then //and Layers.Transparent) Then
              Self.Clear(clrTransparent) //And FTransparent
            Else
              Self.Clear(Layers.BackgroundColor);
          end
          else if FLastDrawMode = 1 then
          begin
            GlobalLogger.LogStatus('Copy Last frame');
            if Index > 0 then PutImage(FAnimateBuffer,0,0, FAnimateBuffer.Width, FAnimateBuffer.Height,0,0,dmSet);
          end;
          GlobalLogger.LogStatus('Copy Current frame');
          Self.PutImage(Src,0,0, Src.Width, Src.Height,pLeft,pTop,dmSet,AlphaMode);

    //        PutImage(Src,0,0, Src.Width, Src.Height,pLeft,pTop,dmSet,amAlphaCheck);
          GlobalLogger.LogStatus('Save frame');
          FAnimateBuffer.Assign(Self);
          //If Assigned(FRestoreBitmap) Then FreeAndNil(FRestoreBitmap);
          //FRestoreBitmap := Self.CreateClone;
        End;
        2:  // Erase
          begin
            GlobalLogger.LogStatus('Current Draw Mode = ERASE');
            GlobalLogger.LogStatus('Last Draw Mode = ' + FLastDrawMode.ToString);

            If (Src.ImageDescription.HasAlpha) then //and (Layers.Transparent) then
              Clear(clrTransparent)
            Else
              Clear(Layers.BackgroundColor);

            PutImage(Src,0,0, Src.Width, Src.Height,pLeft,pTop,dmSet,AlphaMode);
          End;
        3:  // Restore
          begin
            GlobalLogger.LogStatus('Current Draw Mode = RESTORE');
            GlobalLogger.LogStatus('Last Draw Mode = ' + FLastDrawMode.ToString);
            if FLastDrawMode = 2 then
            begin
              If (Src.ImageDescription.HasAlpha) then //and (Layers.Transparent) then
                Clear(clrTransparent)
              Else
                Clear(Layers.BackgroundColor);
            end
            else if FLastDrawMode = 1 then
            begin
              PutImage(FAnimateBuffer,0,0, FAnimateBuffer.Width, FAnimateBuffer.Height,0,0,dmSet);
            end
            else
            begin
              If (Src.ImageDescription.HasAlpha) then  //and (Layers.Transparent) then
                Clear(clrTransparent)
              Else
                Clear(Layers.BackgroundColor);
            end;
            PutImage(Src,0,0, Src.Width, Src.Height,pLeft,pTop,dmSet,AlphaMode);
          End;
        else
          PutImage(Src,0,0, Src.Width, Src.Height,pLeft,pTop,dmSet);
      end;
      if DelayTime <> 0 then FAnimateTimer.Interval := DelayTime * FAnimationSpeed;
    End;

    FLastDrawMode := CurrentDrawMode;

  //If Assigned(FRestoreBitmap) Then FreeAndNil(FRestoreBitmap);
  Changed;
End;

procedure TBZBitmap.DoOnAnimateTime(Sender : TObject);
Begin
  RenderFrame(FFrameIndex);
  Inc(FFrameIndex);
  if FFrameIndex > Layers.Count - 1 then FFrameIndex := 0;
  if assigned(FOnFrameChanged) then FOnFrameChanged(self);
End;

procedure TBZBitmap.StartAnimate;
Begin
  if not(FIsAnimatePaused) then
    if FAnimateBuffer = nil then FAnimateBuffer := TBZBitmap.Create(Self.Width,Self.Height);

  if  not(FIsAnimatePaused) then FFrameIndex := 0;
  FIsAnimatePaused := false;
  FIsAnimate := True;
  FAnimateTimer.Enabled := True;
  If Assigned(FOnStartAnimate) then FOnStartAnimate(Self);
End;

procedure TBZBitmap.StopAnimate;
Begin
  FAnimateTimer.Enabled := False;
  If Assigned(FOnStopAnimate) then FOnStopAnimate(Self);
  FFrameIndex := 0;
  FIsAnimate := False;
  FreeAndNil(FAnimateBuffer);
  If Assigned(FRestoreBitmap) Then FreeAndNil(FRestoreBitmap);
End;

procedure TBZBitmap.PauseAnimate;
Begin
  FIsAnimatePaused := True;
  FAnimateTimer.Enabled := False;
  If Assigned(FOnPauseAnimate) then FOnPauseAnimate(Self);
End;






{%endregion%}

{%region ====[ TBZPicture ]=====================================================}

Constructor TBZPicture.Create;
Begin
  //GlobalLogger.LogNotice('TBZPicture.Create');
  Inherited Create;
  FBitmap := TBZBitmap.Create(Self, 0, 0);
  FBitmap.Clear(clrTransparent);
End;

Destructor TBZPicture.Destroy;
Begin
  FreeAndNil(FBitmap);
  Inherited Destroy;
End;

procedure TBZPicture.WriteToFiler(writer : TVirtualWriter);
{$IFDEF WINDOWS}
Var
  TmpBmp : TBZBitmap;
{$ENDIF}

begin
  inherited WriteToFiler(writer);
  with writer do
  begin
    WriteInteger(0); // Archive Version 0
    WriteInteger(FBitmap.Width);
    WriteInteger(FBitmap.Height);
    if Assigned(FBitmap) then
    begin
      {$IFDEF WINDOWS}
      // On sauvegarde en RGBA
      TmpBmp := FBitmap.CreateClone;
      TmpBmp.ColorFilter.SwapChannel(scmRedBlue);
      write(TmpBmp.getSurfaceBuffer^, FBitmap.Size);
      FreeAndNil(TmpBmp);
      {$ELSE}
      write(FBitmap.getSurfaceBuffer^, FBitmap.Size);
      {$ENDIF}
    end;
  end;
end;

procedure TBZPicture.ReadFromFiler(reader : TVirtualReader);
var
  archiveVersion: Integer;
  BmpWidth, BmpHeight : Integer;
  BmpBuf : PBZColor;
begin
  inherited ReadFromFiler(reader);
  archiveVersion := reader.ReadInteger;
  if archiveVersion = 0 then
  begin
    with reader do
    begin
      BmpWidth := ReadInteger;
      BmpHeight := ReadInteger;
      if (BmpWidth > 0) and (BmpHeight>0) then
      begin
        if Assigned(FBitmap) then FBitmap.SetSize(BmpWidth, BmpHeight)
        else FBitmap := TBZBitmap.Create(BmpWidth, BmpHeight);
        BmpBuf := FBitmap.getSurfaceBuffer;
        read(BmpBuf^,FBitmap.Size);
        {$IFDEF WINDOWS}
        FBitmap.ColorFilter.SwapChannel(scmRedBlue);
        {$ENDIF}
      end;
    end;
  end
  else
    RaiseFilerException(archiveVersion);
end;

Procedure TBZPicture.NotifyChange(Sender: TObject);
begin
  If Assigned(FOnChange) Then FOnChange(Self);
end;

procedure TBZPicture.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty('data', @ReadData, @WriteData,
    ((FBitmap.Width>=1) and (FBitmap.Height>=1)));
end;

procedure TBZPicture.ReadData(Stream: TStream);
var
  virt: TBinaryReader;
begin
  virt := TBinaryReader.Create(Stream);
  ReadFromFiler(virt);
  FreeAndNil(virt);
//var
//  BmpWidth, BmpHeight  : Integer;
////  aSize : Integer;
//  bmpBuffer : PBZColor;
//begin
//  Stream.Read(bmpWidth, SizeOf(Integer));
//  Stream.Read(bmpHeight, SizeOf(Integer));
//  if Not(Assigned(FBitmap)) then FBitmap.Create(bmpWidth, bmpHeight);
////  aSize := (BmpWidth * BmpHeight) * 4;
//  bmpBuffer := FBitmap.getSurfaceBuffer;
//  Stream.Read(bmpBuffer^,FBitmap.Size);
end;

procedure TBZPicture.WriteData(Stream: TStream);
//Var
//  bmpBuffer : PBZColor;
var
  virt: TBinaryWriter;
begin
  virt := TBinaryWriter.Create(Stream);
  WriteToFiler(virt);
  FreeAndNil(virt);
  //Stream.Write(FBitmap.Width, SizeOf(Integer));
  //Stream.Write(FBitmap.Height, SizeOf(Integer));
  //bmpBuffer := FBitmap.getSurfaceBuffer;
  //Stream.Write(bmpBuffer^,FBitmap.Size);
end;

Procedure TBZPicture.SetBitmap(Const aBitmap: TBZBitmap);
Begin
  //GlobalLogger.LogNotice('TBZPicture.SetBitmap');
  With FBItmap Do
  Begin
    SetSize(aBitmap.Width, aBitmap.Height);
    Clear(clrTransparent);
    //PutImage(aBitmap, 0, 0, aBitmap.Width, aBitmap.Height, 0, 0);
    assign(aBitmap);
    NotifyChange(Self);
  End;
End;

//procedure ReadFiler
//procedure WriteFiler

Procedure TBZPicture.LoadFromFile(Const aFileName: String);
Begin
  //GlobalLogger.LogNotice('TBZPicture.LoadFromFile');
  FBitmap.LoadFromFile(aFileName);
  NotifyChange(Self);
  //GlobalLogger.LogStatus('TBZPicture.FBitmap.FullFileName : ' + FBitmap.FullFileName);
End;

Procedure TBZPicture.SaveToFile(Const aFileName: String); //aFormat : TBZImageFileFormats; Options : Pointer);
Begin
  FBitmap.SaveToFile(aFileName);
End;

{%endregion%}


function ComputeGaussianKernel(KernelSize : Integer; Weight : Single) : TBZDynSingleArray;
Var
  KernelRadius : Integer;
  x, y , i, j : Integer;
  Sum, Dist, Theta, f : Single;
  KSize : Integer;
begin
  KernelRadius := KernelSize div 2;
  KSize := KernelSize  * KernelSize;
  SetLength(Result{%H-},  KSize );
  //Theta := 1.0 / (2 * cPi * (Weight * Weight));
  Theta := 1.0 / (2 * (Weight * Weight));
  Sum := 0;
  Dist := 0;
  for y := -KernelRadius to KernelRadius do
  begin
    j := (y + KernelRadius) * KernelSize;
    for x := -KernelRadius to KernelRadius do
    begin
      //Dist := -((X * X) + (Y * Y)) * Theta;
      //f := System.Exp(Dist);

      Dist := ((X * X) + (Y * Y)) * Theta;
      f := ComputeEuler(Weight) * System.Exp(-Dist);

      //Dist := System.Sqrt((X * X) + (Y * Y));
      //f := System.Exp(-(Dist * Dist) / Theta ) / (cPI * Theta);

      i := x + KernelRadius;
      Result[j + i] := f;
      Sum := Sum + f;
    end;
  end;
  Theta := 1.0 / Sum;
  for x := 0 to (KSize-1) do Result[x] := Result[x] * Theta;
end;

function ComputeLOGKernel(KernelSize : Integer; Weight : Single) : TBZDynSingleArray;
Var
  KernelRadius : Integer;
  x, y , i, j : Integer;
  s, Sum, Dist, Theta, f, xx,yy : Single;
  KSize : Integer;
begin
  s := weight; // - 0.8;
  //t := Round(3.35 * s + 0.33);
	//KernelSize := 2 * t + 1;
  KernelRadius := KernelSize div 2;
  KSize := KernelSize  * KernelSize;
  SetLength(Result{%H-},  KSize );
  Sum := 0;
  Dist := 0;
  for y := -KernelRadius to KernelRadius do
  begin
    j := (y + KernelRadius);
    yy := y; //j - t;
    yy := yy * yy;
    j := j * KernelSize;
    for x := -KernelRadius to KernelRadius do
    begin
      i := x + KernelRadius;
      xx := x; //i - t;
      xx := xx * xx;
      Dist := (xx + yy);

      if Abs(Dist) > KSize then f := 0
      else f := ComputeLOGCenter(System.Sqrt(Dist), s);

      Result[j + i] := f;
      Sum := Sum + f;
    end;
  end;
  Theta := 1.0 / Sum;
  for x := 0 to (KSize-1) do Result[x] := Result[x] * Theta;
end;

//==============================================================================

Initialization
  // On enregistre nos classes pour la persitence des objets
  RegisterClasses([TBZBitmap, TBZBitmapCanvas, TBZPicture]);

Finalization

  UnRegisterClasses([TBZBitmap, TBZBitmapCanvas, TBZPicture]);

//==============================================================================
End.
