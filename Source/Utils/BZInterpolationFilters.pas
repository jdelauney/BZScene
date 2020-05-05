(*
  @abstract(Regroupe plusieurs filtres (48) à utiliser pour le rééchantillonage et l'interpolation de valeur)

  Ces filtres sont definis dans TBZInterpolationFilterMethod
   voici la liste complète : @br
   Filtres FIR (Finite impulse Response) :
   @unorderedList(
     @item(ifmBox, ifmTriangle, ifmSystem.Cosine)
     @item(ifmSpline, ifmCatrom, ifmCubic, ifmQuadratic, ifmQuintic )
     @item(ifmBell, ifmHermit, ifmWelch,  ifmMitchell
     @item(ifmHann, ifmHamming, ifmSinsh, ifmBlackman, ifmLagrange)
   )
   Filtres IIR (Infinite impulse Response) :
   @unorderedList(
     @item(ifmBlackmanBessel, ifmBlackmanSinC, ifmGaussian
     @item(ifmSinc, ifmJinc ( Jinc aussi appelé Bessel), ifmBartlett
     @item(ifmSystem.CosineWindowed, ifmWelchWindowed, ifmHannWindowed, ifmHammingWindowed)
     @item(ifmBlackmanWindowed, ifmGaussianWindowed)
     @item(ifmLanczos3, ifmBohman, ifmAlbrecht, ifmKaiser, ifmBeanz)
   )

  -------------------------------------------------------------------------------------------------------------

  @created(2017-07-21)
  @author(J.Delauney (BeanzMaster))
  Historique : @br
  @unorderedList(
    @item(21/07/2017 : Creation  )
  )

  -------------------------------------------------------------------------------------------------------------

  @bold(Notes) : @br
    Les filtres avec le préfixe "WD" (Windowed) donnent en général de meilleurs résultats. @br
    Les filtres de type "windowed" WD peuvent être implémenter de 2 manières différente. @br
    @br
    . 1ere Méthode :@br
      Type@br
        TWD_MonFiltre = Class(TBZCustomInterpolationFilter)

      Dans ce cas, le calcul du  résultat de la fonction "filter" devra être "réfactoré"/"factorisé" @br
      (cf filtres BlackManBessel/SinC, Bohman par exemple)

    . 2eme Méthode@br
      Type@br
        TWD_MonFiltre = Class(TBZCustomWindowInterpolationFilter)@br

     Ici la fonction "Window" devra être surchargée.@br
     On peux egalement choisir la fonction de fenetrage dans le constructor Create
     propriété "WindowMode". Pour l'instant seule les fonctions de fenetrages suivante sont disponible : @br
     cf SinC et Blackman.

    Informations sur le rééchantillonage et autres fonctions mathématiques bien tordues.@br
    Préparez vos tubes d'aspirines !! :

    @unorderedList(
      @item(https://clouard.users.greyc.fr/Pantheon/experiments/rescaling/index-fr.html)
      @item(https://en.wikipedia.org/wiki/Finite_impulse_response)
      @item(http://en.wikipedia.org/wiki/Window_function)
      @item(https://en.wikipedia.org/wiki/Lanczos_resampling)
      @item(http://reference.wolfram.com/language/ref/Resampling.html)
      @item(http://www.resampling.narod.ru)
      @item(http://www.xtremevbtalk.com/archive/index.php/t-228878.html)
      @item(https://docs.scipy.org/doc/scipy/reference/signal.html)
      @item(https://en.wikipedia.org/wiki/Sinc_function)
      @item(http://resampy.readthedocs.io/en/latest/index.html)
      @item(http://dspace.thapar.edu:8080/jspui/bitstream/10266/2052/3/2052.pdf)
      @item(https://cadxfem.org/inf/ResamplingFilters.pdf)
      @item(http://www-cs.ccny.cuny.edu/~wolberg/pub/crc04.pdf)
      @item(https://www.ldv.ei.tum.de/fileadmin/w00bfa/www/content_uploads/Vorlesung_3.4_Resampling.pdf)
      @item(https://cran.r-project.org/web/packages/signal/signal.pdf)
      @item(https://en.wikipedia.org/wiki/Bessel_function)
      @item(https://en.wikipedia.org/wiki/Kaiser_window)
     )
     Autres infos diverses :
     @unorderedList(
      @item(https://en.wikipedia.org/wiki/Elliptic_filter)
      @item(https://fr.wikipedia.org/wiki/Filtre_particulaire)
      @item(http://perso.telecom-paristech.fr/~chollet/Biblio/Articles/Auteurs/Blouet/particleFilter-2.pdf)
      @item(https://www.rocq.inria.fr/clime/adoqa/meetings/grenoble181006/adoqa-irisa.pdf)
      @item(http://cas.ensmp.fr/~chaplais/SeminarSlides/MUSSO_10_12_02.pdf)
      @item(http://graphicon.ru/oldgr/en/research/scaling/index.html)
      @item(http://bigwww.epfl.ch/publications/thevenaz9901.pdf)
      @item(http://dsp-book.narod.ru/HFTSP/8579ch07.pdf)
      @item(https://www.dsprelated.com/freebooks/sasp/)
     )

  -------------------------------------------------------------------------------------------------------------

  @bold(TODO :)@br
     - Ajouter d'autres filtres : @br
     @unorderedList(
      @item(https://en.wikipedia.org/wiki/Chebyshev_filter)
      @item(https://en.wikipedia.org/wiki/Kolmogorov–Zurbenko_filter)
      @item(https://en.wikipedia.org/wiki/Butterworth_filter)
      @item(etc...)
     )
     @br
     - Ajouter d'autres methode de calcul pour les filtres "Windowed"
       @unorderedList(
        @item(Planck-Taper, chebyshev, Burgess, Lawrey, ...)
       )
     @br
     - Ajouter fonction de rééchantillonage à base de FFT (Fast Fourrier Transformation)
     @br
     - @bold(Corriger) les methodes : Nutttal, BlackmanHarris, BlackmanNuttal, BartlettHann, FlatTop, Poisson, HannPoisson

  -------------------------------------------------------------------------------------------------------------

  @bold(Credits :)
    @unorderedList(
      @item(J.Delauney aka BeanzMaster)
      @item(FPC/Lazarus)
      @item(GLScene)
      @item(Graphic32)
    )

  -------------------------------------------------------------------------------------------------------------

  @bold(LICENCE :) MPL / GPL

  ------------------------------------------------------------------------------------------------------------- *)
unit BZInterpolationFilters;

//==============================================================================
{$mode objfpc}{$H+}
{$i ..\bzscene_options.inc}
//==============================================================================

interface

uses
  Classes, SysUtils,
  BZClasses, BZMath, BZTypesHelpers;

type
  { Classe à hériter pour ajouter un filtre d'interpolations. }
  TBZCustomInterpolationFilter = class(TBZPersistentObject)
  private
  protected
    FWindowed : Boolean;
  public
    Constructor Create; override;
    { Calcul du poid par le filtre pour la valeur "X" }
    function Filter(x: double): double; virtual;
    { Résultat maximum retounée pas la fonction de calcul "Filter" }
    function MaxSupport: double; virtual;
    { Drapeau indiquant si le filtre utilise la methode de "fenetrage" }
    property Windowed : Boolean Read FWindowed;
  end;


  { Enumération des modes de fenetrage pour les filtres de type "Windowed" @br
    TODO : Ajouter d'autres modes : Welch, Bartlett, Tukey ect...}
  TBZInterpolationFilterWindowedMode = (wmDefault, wmSinC, wmBlackman);

  { @abstract(Classe abstraite pour les filtres "Windowed") @br
    @bold(Note) : Adapté de graphics32 (TWindowedSincKernel)) }
  TBZCustomWindowedInterpolationFilter = class(TBZCustomInterpolationFilter)
  private
    FWidth : Double;                   //FRadius
    FWidthReciprocal : Double;         //FInvRadius
    FWindowMode : TBZInterpolationFilterWindowedMode;
  protected

    function Window(Value: Double): Double; virtual; abstract;
  public
    constructor Create; override;
    function Filter(Value: Double): Double;  override;
    procedure SetWidth(Value: Double);
   // function GetWidth: Double;
    property WidthReciprocal : Double read FWidthReciprocal;
  published
    { Largeur de la fenêtre }
    property Width: Double read FWidth write SetWidth;
    { Mode de fenêtrage }
    property WindowMode: TBZInterpolationFilterWindowedMode read FWindowMode write FWindowMode;
  end;

{%region =====[ Filtres FIR (Finit Impulsion Response) ]========================}

  { Type de classe descendante de BZCustomInterpolationFilter }
  TBZInterpolationFilterClass = class of TBZCustomInterpolationFilter;

  { Filtre de rééchantillonage FIR (Finit Impulsion Response) Box}
  TBZBoxInterpolationFilter = class(TBZCustomInterpolationFilter)
  private
  protected
  public
    function Filter(x: double): double; override;
    function MaxSupport: double; override;
  end;

  { Filtre de rééchantillonage FIR (Finit Impulsion Response) Triangle }
  TBZTriangleInterpolationFilter = class(TBZCustomInterpolationFilter)
  private
  protected
  public
    function Filter(x: double): double; override;
    function MaxSupport: double; override;
  end;

  { Filtre de rééchantillonage FIR (Finit Impulsion Response) Cosine }
  TBZCosineInterpolationFilter = class(TBZCustomInterpolationFilter)
  private
  protected
  public
    function Filter(x: double): double; override;
    function MaxSupport: double; override;
  end;

  { Filtre de rééchantillonage FIR (Finit Impulsion Response) Spline }
  TBZSplineInterpolationFilter = class(TBZCustomInterpolationFilter)
  private
  protected
  public
    function Filter(x: double): double; override;
    function MaxSupport: double; override;
  end;

  { Filtre de rééchantillonage FIR (Finit Impulsion Response) Catrom}
  TBZCatromInterpolationFilter = class(TBZCustomInterpolationFilter)
  private
  protected
  public
    function Filter(x: double): double; override;
    function MaxSupport: double; override;

  end;

  { Filtre de rééchantillonage FIR (Finit Impulsion Response) Cubic}
  TBZCubicInterpolationFilter = class(TBZCustomInterpolationFilter)
  private
  protected
  public
    function Filter(x: double): double; override;
    function MaxSupport: double; override;
  end;

  (*  TBZCubicExInterpolationFilter = class(TBZCustomInterpolationFilter)
  private
    FCoefB, FCoefC : Double;
    procedure SetCoefB(aValue:Double);
    procedure SetCoefC(aValue:Double);
  protected
  public
    function Filter(x: double): double; override;
    function MaxSupport: double; override;

    property CoefB : double read FCoefB Write SetCoefB;
    property CoefC : double read FCoefC Write SetCoefC;
  end; *)

  { Filtre de rééchantillonage FIR (Finit Impulsion Response) Quadratic }
  TBZQuadraticInterpolationFilter = class(TBZCustomInterpolationFilter)
  private
  protected
  public
    function Filter(x: double): double; override;
    function MaxSupport: double; override;
  end;

  { Filtre de rééchantillonage FIR (Finit Impulsion Response) BSpline Quintic }
  TBZQuinticInterpolationFilter = class(TBZCustomInterpolationFilter)
  private
  protected
  public
    function Filter(x: double): double; override;
    function MaxSupport: double; override;
  end;

  { Filtre de rééchantillonage FIR (Finit Impulsion Response) Bell}
  TBZBellInterpolationFilter = class(TBZCustomInterpolationFilter)
  private
  protected
  public
    function Filter(x: double): double; override;
    function MaxSupport: double; override;
  end;

  { Filtre de rééchantillonage FIR (Finit Impulsion Response) Hermit}
  TBZHermitInterpolationFilter = class(TBZCustomInterpolationFilter)
  private
  protected
  public
    function Filter(x: double): double; override;
    function MaxSupport: double; override;
  end;

  { Filtre de rééchantillonage FIR (Finit Impulsion Response) Welch}
  TBZWelchInterpolationFilter= class(TBZCustomInterpolationFilter)
  private
  protected
  public
    function Filter(x: double): double; override;
    function MaxSupport: double; override;
  end;

  { Filtre de rééchantillonage FIR (Finit Impulsion Response) Mitchell}
  TBZMitchellInterpolationFilter = class(TBZCustomInterpolationFilter)
  private
  protected
  public
    function Filter(x: double): double; override;
    function MaxSupport: double; override;
  end;

  { Filtre de rééchantillonage FIR (Finit Impulsion Response) Hann}
  TBZHannInterpolationFilter = class(TBZCustomInterpolationFilter)
  private
  protected
  public
    function Filter(x: double): double; override;
    function MaxSupport: double; override;
  end;

  { Filtre de rééchantillonage FIR (Finit Impulsion Response) Hamming }
  TBZHammingInterpolationFilter = class(TBZCustomInterpolationFilter)
  private
  protected
  public
    function Filter(x: double): double; override;
    function MaxSupport: double; override;
  end;

  { Filtre de rééchantillonage FIR (Finit Impulsion Response) SinH }
  TBZSinshInterpolationFilter = class(TBZCustomInterpolationFilter)
  private
  protected
  public
    function Filter(x: double): double; override;
    function MaxSupport: double; override;
  end;

  { Filtre de rééchantillonage FIR (Finit Impulsion Response) Blackman}
  TBZBlackmanInterpolationFilter = class(TBZCustomInterpolationFilter)
  private
  protected
  public
    function Filter(x: double): double; override;
    function MaxSupport: double; override;
  end;

  { Filtre de rééchantillonage FIR (Finit Impulsion Response) Lagrange}
  TBZLagrangeInterpolationFilter = class(TBZCustomInterpolationFilter)
  private
  protected
  public
    function Filter(x: double): double; override;
    function MaxSupport: double; override;
  end;

  { Filtre de rééchantillonage FIR (Finit Impulsion Response) Schaum 2. @br
    @bold(Note) : Ce noyau est discontinu. Aux points discontinus, il prend la valeur moyenne des limites gauche et droite. }
  TBZQuadraticSchaumInterpolationFilter = class(TBZCustomInterpolationFilter)
  private
  protected
  public
    function Filter(x: double): double; override;
    function MaxSupport: double; override;
  end;

  { TBZCubicSchaumInterpolationFilter }

  TBZCubicSchaumInterpolationFilter = class(TBZCustomInterpolationFilter)
  private
  protected
  public
    function Filter(x: double): double; override;
    function MaxSupport: double; override;
  end;

  { Filtre de rééchantillonage FIR (Finit Impulsion Response) O-Moms x3 }
  TBZOMomsx3InterpolationFilter = class(TBZCustomInterpolationFilter)
  private
  protected
  public
    function Filter(x: double): double; override;
    function MaxSupport: double; override;
  end;

  { Filtre de rééchantillonage FIR (Finit Impulsion Response) O-Moms x5 }
  TBZOMomsx5InterpolationFilter = class(TBZCustomInterpolationFilter)
  private
  protected
  public
    function Filter(x: double): double; override;
    function MaxSupport: double; override;
  end;

  { Filtre de rééchantillonage FIR (Finit Impulsion Response) O-Moms x7 }
  TBZOMomsx7InterpolationFilter = class(TBZCustomInterpolationFilter)
  private
  protected
  public
    function Filter(x: double): double; override;
    function MaxSupport: double; override;
  end;

{%endregion%}

{%region =====[ Filtres IIR (Infinit Impulsion Response) ]=====================}

  { Filtre de rééchantillonage IIR (Infinit Impulsion Response) Blackman/Bessel}
  TBZBlackmanBesselInterpolationFilter = class(TBZCustomInterpolationFilter)
  private
  protected
  public
    function Filter(x: double): double; override;
    function MaxSupport: double; override;
  end;

  { Filtre de rééchantillonage IIR (Infinit Impulsion Response) Blackman/SinC}
  TBZBlackmanSinCInterpolationFilter = class(TBZCustomInterpolationFilter) // == Windowed ???
  private
  protected
  public
    function Filter(x: double): double; override;
    function MaxSupport: double; override;
  end;

  { Filtre de rééchantillonage IIR (Infinit Impulsion Response) Gaussian}
  TBZGaussianInterpolationFilter = class(TBZCustomInterpolationFilter)
  private
  protected
  public
    function Filter(x: double): double; override;
    function MaxSupport: double; override;
  end;

  { Filtre de rééchantillonage IIR (Infinit Impulsion Response) SinC }
  TBZSinCInterpolationFilter = class(TBZCustomInterpolationFilter)
  private
  protected
  public
    function Filter(x: double): double; override;
    function MaxSupport: double; override;
  end;

  { Filtre de rééchantillonage IIR (Infinit Impulsion Response) JinC appelé également Bessel }
  TBZJinCInterpolationFilter = class(TBZCustomInterpolationFilter)
  private
  protected
  public
    function Filter(x: double): double; override;
    function MaxSupport: double; override;
  end;

  { Filtre de rééchantillonage IIR (Infinit Impulsion Response) BeanZ }
  TBZBeanzInterpolationFilter = class(TBZCustomInterpolationFilter)
  private
  protected
  public
    function Filter(x: double): double; override;
    function MaxSupport: double; override;
  end;

{%endregion%}

{%region =====[ Filtres Windowed ]=============================================}

  { Filtre de rééchantillonage Windowed Barlett }
  TBZBartlettWindowedInterpolationFilter = Class(TBZCustomWindowedInterpolationFilter)
  protected
     function Window(Value: Double): Double; override;
   public
     function MaxSupport: double; override;
  end;

  { Filtre de rééchantillonage Windowed System.Cosine }
  TBZCosineWindowedInterpolationFilter = Class(TBZCustomWindowedInterpolationFilter)
  protected
     function Window(Value: Double): Double; override;
   public
     function MaxSupport: double; override;
  end;

  { Filtre de rééchantillonage Windowed Welch }
  TBZWelchWindowedInterpolationFilter = Class(TBZCustomWindowedInterpolationFilter)
  protected
     function Window(Value: Double): Double; override;
   public
     function MaxSupport: double; override;
  end;

  { Filtre de rééchantillonage Windowed Hann }
  TBZHannWindowedInterpolationFilter = Class(TBZCustomWindowedInterpolationFilter)
  protected
     function Window(Value: Double): Double; override;
   public
     function MaxSupport: double; override;
  end;

  { Filtre de rééchantillonage Windowed Hamming }
  TBZHammingWindowedInterpolationFilter = Class(TBZCustomWindowedInterpolationFilter)
  protected
     function Window(Value: Double): Double; override;
   public
     function MaxSupport: double; override;
  end;

  { Filtre de rééchantillonage Windowed Blackman }
  TBZBlackmanWindowedInterpolationFilter = Class(TBZCustomWindowedInterpolationFilter)
  protected
     function Window(Value: Double): Double; override;
   public
     function MaxSupport: double; override;
  end;

  { Filtre de rééchantillonage Windowed Lanczos3 }
  TBZLanczos3InterpolationFilter = class(TBZCustomInterpolationFilter)
  protected
  public
    function Filter(x: double): double; override;
    function MaxSupport: double; override;
  end;

  { Filtre de rééchantillonage Windowed Bohman }
  TBZBohmanInterpolationFilter = class(TBZCustomInterpolationFilter)
  private
  public
    function Filter(x: double): double; override;
    function MaxSupport: double; override;
  end;

  { Filtre de rééchantillonage Windowed Albrecht }
  TBZAlbrechtWindowedInterpolationFilter = Class(TBZCustomWindowedInterpolationFilter)
  private
    FTerms: Integer;
    FCoefPointer : Array [0..11] of Double;
    procedure SetTerms(Value : Integer);
  protected
     function Window(Value: Double): Double; override;
   public
     constructor Create; override;
     function MaxSupport: double; override;

     property Terms: Integer read FTerms write SetTerms;
  end;

  { Filtre de rééchantillonage Windowed Kaiser }
  TBZKaiserWindowedInterpolationFilter = Class(TBZCustomWindowedInterpolationFilter)
  private
    FTerms: Single; // Compris entre 5.0 et 8.0 par defaut 6.33
    FInvI0Terms : Double;
    procedure SetTerms(Value : Single);
  protected
     function Window(Value: Double): Double; override;
   public
     constructor Create; override;

     function MaxSupport: double; override;
     property Terms: Single read FTerms write SetTerms;
  end;

  { Filtre de rééchantillonage Windowed Gaussian }
  TBZGaussianWindowedInterpolationFilter = Class(TBZCustomWindowedInterpolationFilter)
  private

  protected
     function Window(Value: Double): Double; override;
   public
     constructor Create; override;

     function MaxSupport: double; override;
  end;


  { TBZPoissonWindowedInterpolationFilter }
  TBZPoissonWindowedInterpolationFilter = Class(TBZCustomWindowedInterpolationFilter)
  private
     FN : Double;
     FTerm : Single;
  protected
     function Window(Value: Double): Double; override;
   public
     constructor Create; override;

     function MaxSupport: double; override;
     property Term: Single read FTerm write FTerm;
  end;

  { TBZHannPoissonWindowedInterpolationFilter }

  TBZHannPoissonWindowedInterpolationFilter = Class(TBZCustomWindowedInterpolationFilter)
  private
     FN : Double;
     FTerm : Single;
  protected
     function Window(Value: Double): Double; override;
   public
     constructor Create; override;

     function MaxSupport: double; override;
     property Term: Single read FTerm write FTerm;
  end;

  { Filtre de rééchantillonage Windowed Nuttall }
  TBZNuttallWindowedInterpolationFilter = Class(TBZCustomWindowedInterpolationFilter)
  private
     FN : Double;
  protected
     function Window(Value: Double): Double; override;
   public
     constructor Create; override;

     function MaxSupport: double; override;
  end;

  { TBZBlackmanNuttallWindowedInterpolationFilter }
  TBZBlackmanNuttallWindowedInterpolationFilter = Class(TBZCustomWindowedInterpolationFilter)
  private
     FN : Double;
  protected
     function Window(Value: Double): Double; override;
   public
     constructor Create; override;

     function MaxSupport: double; override;
  end;

  { TBZBlackmanHarrisWindowedInterpolationFilter }

  TBZBlackmanHarrisWindowedInterpolationFilter = Class(TBZCustomWindowedInterpolationFilter)
  private
     FN : Double;
  protected
     function Window(Value: Double): Double; override;
   public
     constructor Create; override;

     function MaxSupport: double; override;
  end;

  { TBZFlatTopWindowedInterpolationFilter }
  TBZFlatTopWindowedInterpolationFilter = Class(TBZCustomWindowedInterpolationFilter)
  private
     FN : Double;
  protected
     function Window(Value: Double): Double; override;
   public
     constructor Create; override;

     function MaxSupport: double; override;
  end;

  { TBZTukeyWindowedInterpolationFilter }

  TBZTukeyWindowedInterpolationFilter = Class(TBZCustomWindowedInterpolationFilter)
  private
     FN : Double;
     FTerm : Single;
  protected
     function Window(Value: Double): Double; override;
   public
     constructor Create; override;

     function MaxSupport: double; override;
     property Term: Single read FTerm write FTerm;
  end;

  { TBZBarlettHannWindowedInterpolationFilter }

  TBZBartlettHannWindowedInterpolationFilter = Class(TBZCustomWindowedInterpolationFilter)
  private
     FN : Double;
  protected
     function Window(Value: Double): Double; override;
   public
     constructor Create; override;

     function MaxSupport: double; override;
  end;

  { TBZCauchyWindowedInterpolationFilter }

  TBZCauchyWindowedInterpolationFilter = Class(TBZCustomWindowedInterpolationFilter)
  private
     FN : Double;
     FTerm : Double;
  protected
     function Window(Value: Double): Double; override;
   public
     constructor Create; override;

     function MaxSupport: double; override;
  end;


{%endregion%}

  { @abstract(Enumération des filtres de rééchantillonage supportés.)

    @unorderedlist(
      @item(FIR : Finite impulse Response)
      @item(IIR : Infinite impulse Response)
    )

    @bold(Note) :  Jinc est aussi appelé Bessel   }
  TBZInterpolationFilterMethod = (ifmBox,
                                  ifmTriangle,
                                  ifmCosine,
                                  ifmSpline,
                                  ifmCatrom,
                                  ifmCubic,
                                  ifmQuadratic,
                                  ifmQuintic,
                                  ifmBell,
                                  ifmHermit,
                                  ifmWelch,
                                  ifmMitchell,
                                  ifmHann,
                                  ifmHamming,
                                  ifmSinsh,
                                  ifmBlackman,
                                  ifmLagrange,
                                  ifmQuadraticSchaum,
                                  ifmCubicSchaum,
                                  ifmOMomsx3,
                                  ifmOMomsx5,
                                  ifmOMomsx7,
                                  ifmBlackmanBessel,
                                  ifmBlackmanSinC,
                                  ifmGaussian,
                                  ifmSinC,
                                  ifmJinC,
                                  ifmNuttall,
                                  ifmTukey,
                                  ifmPoisson,
                                  ifmCauchy,
                                  ifmFlatTop,
                                  ifmBartlett,
                                  ifmBartlettHann,
                                  ifmCosineWindowed,
                                  ifmWelchWindowed,
                                  ifmHannWindowed,
                                  ifmHannPoisson,
                                  ifmHammingWindowed,
                                  //Sinsh
                                  ifmBlackmanWindowed,
                                  ifmBlackmanHarris,
                                  ifmBlackmanNuttall,
                                  ifmGaussianWindowed,
                                  // SinC, JinC,
                                  ifmLanczos3,
                                  ifmBohman,
                                  ifmAlbrecht,
                                  ifmKaiser,
                                  ifmBeanz
                                  );
                              // Autres Méthodes
                              //imCubicSharpened,
                              //imSinCFast
                              //imLanczosSharpened,
                              //imLanczos2,
                              //imLanczos2Sharpened,
                              //imParzen,

  { Informations d'un filtre de rééchantillonnage }
  TBZInterpolationFilterRec = class
  public
    InterpolationFilterClass: TBZInterpolationFilterClass;
    Name: string;
    Description: string;
    DescResID: Integer;
  end;

  { Stock les classes des filtres  de rééchantillonnage supportés }
  TBZInterpolationFiltersList = class(TBZPersistentObjectList)
  public
    { Destruction }
    destructor Destroy; override;

    { Ajoute une classe de filtre à la liste }
    procedure Add(const AName, Desc: string; DescID: Integer; AClass:TBZInterpolationFilterClass);
    { Cherche un filtre à partir de son nom }
    function FindFromName(aName: string): TBZInterpolationFilterClass;
    { Efface une classe de filtre à la liste }
    procedure Remove(AClass: TBZInterpolationFilterClass);
    { Retourne la liste des filtres supporté dans une TStringList }
    procedure BuildStringList(var filters: TStringList);
    { Retourne la classe d'un filtre à la position "Index }
    function getByIndex(index: Integer): TBZInterpolationFilterClass;
    { Retounre le TBZInterpolationFilterMethod de la classe se trouvant à l'"index" de la liste }
    function getFilterByIndex(index: Integer): TBZInterpolationFilterMethod;
  end;

{  @abstract(Retourne la classe du filtre de rééchantillonnage "aType") @br

   Exemple : @br
      @longCode(#
      Var
        FilterClass : TBZInterpolationFilterClass;
        MonFiltre : TBZCustomInterpolationFilter;
      begin
        FilterClass := GetBZInterpolationFilter(imBox);
        MonFiltre := FilterClass.Create;
        // ...
        weight := MonFiltre.Filter(x);
        // ...
        FreeAndNil(MonFiltre);
      end;
      #) }
function GetBZInterpolationFilter( aType: TBZInterpolationFilterMethod): TBZInterpolationFilterClass;
{ Retourne la liste des filtres de rééchantillognage supportés }
function GetBZInterpolationFilters: TBZInterpolationFiltersList;
{ Enregistrement d'un filtre de rééchantillonage dans la liste des filtres supportés}
procedure RegisterBZInterpolationFilter(const AName, ADescription: string;AClass: TBZInterpolationFilterClass);
{ Libération de l'enregistrement d'un filtre de rééchantillonage  dans la liste des filtres supportés}
procedure UnregisterBZInterpolationFilter(AClass: TBZInterpolationFilterClass);

implementation

uses Math; //, BZLogger;

var
  vInterpolationFilters: TBZInterpolationFiltersList;

const
  CAlbrecht2 : array [0..1] of Double = (5.383553946707251E-1,
    4.616446053292749E-1);
  CAlbrecht3 : array [0..2] of Double = (3.46100822018625E-1,
    4.97340635096738E-1, 1.56558542884637E-1);
  CAlbrecht4 : array [0..3] of Double = (2.26982412792069E-1,
    4.57254070828427E-1, 2.73199027957384E-1, 4.25644884221201E-2);
  CAlbrecht5 : array [0..4] of Double = (1.48942606015830E-1,
    3.86001173639176E-1, 3.40977403214053E-1, 1.139879604246E-1,
    1.00908567063414E-2);
  CAlbrecht6 : array [0..5] of Double = (9.71676200107429E-2,
    3.08845222524055E-1, 3.62623371437917E-1, 1.88953325525116E-1,
    4.02095714148751E-2, 2.20088908729420E-3);
  CAlbrecht7 : array [0..6] of Double = (6.39644241143904E-2,
    2.39938645993528E-1, 3.50159563238205E-1, 2.47741118970808E-1,
    8.54382560558580E-2, 1.23202033692932E-2, 4.37788257917735E-4);
  CAlbrecht8 : array [0..7] of Double = (4.21072107042137E-2,
    1.82076226633776E-1, 3.17713781059942E-1, 2.84438001373442E-1,
    1.36762237777383E-1, 3.34038053504025E-2, 3.41677216705768E-3,
    8.19649337831348E-5);
  CAlbrecht9 : array [0..8] of Double = (2.76143731612611E-2,
    1.35382228758844E-1, 2.75287234472237E-1, 2.98843335317801E-1,
    1.85319330279284E-1, 6.48884482549063E-2, 1.17641910285655E-2,
    8.85987580106899E-4, 1.48711469943406E-5);
  CAlbrecht10: array [0..9] of Double = (1.79908225352538E-2,
    9.87959586065210E-2, 2.29883817001211E-1, 2.94113019095183E-1,
    2.24338977814325E-1, 1.03248806248099E-1, 2.75674109448523E-2,
    3.83958622947123E-3, 2.18971708430106E-4, 2.62981665347889E-6);
  CAlbrecht11: array [0..10] of Double = (1.18717127796602E-2,
    7.19533651951142E-2, 1.87887160922585E-1, 2.75808174097291E-1,
    2.48904243244464E-1, 1.41729867200712E-1, 5.02002976228256E-2,
    1.04589649084984E-2, 1.13615112741660E-3, 4.96285981703436E-5,
    4.34303262685720E-7);



function GetBZInterpolationFilter(aType: TBZInterpolationFilterMethod): TBZInterpolationFilterClass;
var
  aFilter: TBZInterpolationFilterClass;
begin
  Result := nil;
  aFilter := TBZBoxInterpolationFilter;
  case aType of
    ifmBox: aFilter := TBZBoxInterpolationFilter;
    ifmTriangle: aFilter := TBZTriangleInterpolationFilter;
    ifmCosine: aFilter := TBZCosineInterpolationFilter;
    ifmSpline: aFilter := TBZSplineInterpolationFilter;
    ifmCatrom: aFilter := TBZCatromInterpolationFilter;
    ifmCubic: aFilter := TBZCubicInterpolationFilter;
    ifmQuadratic: aFilter := TBZQuadraticInterpolationFilter;
    ifmBell: aFilter := TBZBellInterpolationFilter;
    ifmHermit: aFilter := TBZHermitInterpolationFilter;
    ifmWelch: aFilter := TBZWelchInterpolationFilter;
    ifmMitchell: aFilter := TBZMitchellInterpolationFilter;
    ifmHann: aFilter := TBZHannInterpolationFilter;
    ifmHamming: aFilter := TBZHammingInterpolationFilter;
    ifmSinsh: aFilter := TBZSinshInterpolationFilter;
    ifmblackman: aFilter := TBZBlackmanInterpolationFilter;
    ifmLagrange: aFilter := TBZLagrangeInterpolationFilter;
    ifmQuadraticSchaum: aFilter := TBZQuadraticSchaumInterpolationFilter;
    ifmCubicSchaum: aFilter := TBZCubicSchaumInterpolationFilter;
    ifmOMomsx3: aFilter := TBZOMomsx3InterpolationFilter;
    ifmOMomsx5: aFilter := TBZOMomsx5InterpolationFilter;
    ifmOMomsx7: aFilter := TBZOMomsx7InterpolationFilter;
    ifmblackmanBessel: aFilter := TBZBlackmanBesselInterpolationFilter;
    ifmblackmanSinC: aFilter := TBZBlackmanSinCInterpolationFilter;
    ifmGaussian: aFilter := TBZGaussianInterpolationFilter;
    ifmSinc: aFilter := TBZSincInterpolationFilter;
    ifmJinc: aFilter := TBZJincInterpolationFilter;
    ifmNuttall: aFilter := TBZNuttallWindowedInterpolationFilter;
    ifmTukey: aFilter := TBZTukeyWindowedInterpolationFilter;
    ifmPoisson: aFilter := TBZPoissonWindowedInterpolationFilter;
    ifmCauchy: aFilter := TBZCauchyWindowedInterpolationFilter;
    ifmFlatTop: aFilter := TBZFlatTopWindowedInterpolationFilter;
    ifmBartlett : aFilter := TBZBartlettWindowedInterpolationFilter;
    ifmBartlettHann : aFilter := TBZBartlettHannWindowedInterpolationFilter;
    ifmCosineWindowed : aFilter := TBZCosineWindowedInterpolationFilter;
    ifmWelchWindowed : aFilter := TBZWelchWindowedInterpolationFilter;
    ifmHannWindowed : aFilter := TBZHannWindowedInterpolationFilter;
    ifmHannPoisson : aFilter := TBZHannPoissonWindowedInterpolationFilter;
    ifmHammingWindowed : aFilter := TBZHammingWindowedInterpolationFilter;
    ifmblackmanWindowed: aFilter := TBZBlackmanWindowedInterpolationFilter;
    ifmblackmanNuttall: aFilter := TBZBlackmanNuttallWindowedInterpolationFilter;
    ifmblackmanHarris: aFilter := TBZBlackmanHarrisWindowedInterpolationFilter;
    ifmGaussianWindowed: aFilter := TBZGaussianWindowedInterpolationFilter;
    ifmLanczos3: aFilter := TBZLanczos3InterpolationFilter;
    ifmBohman : aFilter := TBZBohmanInterpolationFilter;
    ifmAlbrecht : aFilter := TBZAlbrechtWindowedInterpolationFilter;
    ifmKaiser : aFilter := TBZKaiserWindowedInterpolationFilter;
    ifmBeanz : aFilter := TBZBeanzInterpolationFilter;
   // else aFilter := TBZLanczos3InterpolationFilter;  // Lanczos3 par defaut
  end;
  Result := aFilter;
end;

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

function ComputeBlackman(x:Double):Double;inline;
//var xpi : double;
begin
  //xpi := System.Cos(cPI * x);
  //result:=(0.34+xpi*(0.5+xpi*0.16));
  result:= 0.42659071 + 0.49656062 * System.Cos(cPI*x) + 0.07684867 * System.Cos(c2PI*x);
end;

{%region%====[ Base des Filtres de rééchantillonnage ]=========================}

Constructor TBZCustomInterpolationFilter.Create;
begin
  Inherited Create;
  FWindowed:=False;
end;

function TBZCustomInterpolationFilter.Filter(x: double): double;
begin
  Result := x;
end;

function TBZCustomInterpolationFilter.MaxSupport: double;
begin
  Result := 1.0;
end;

constructor TBZCustomWindowedInterpolationFilter.Create;
begin
  Inherited Create;
  FWidth := 3.0;
  FWindowed:=True;
  FWindowMode :=wmSinC;
  FWidthReciprocal := ComputeReciprocal(FWidth);  // 1 / FWidth;    //FWidth.Reciprocal;//
end;

function TBZCustomWindowedInterpolationFilter.Filter(Value: Double): Double;
begin
  Result := 0;
  if (WindowMode = wmDefault) then Result := Window(Value)
  else
  begin
    Value := Abs(Value);
    if Value <= FWidth then
    begin
      Case WindowMode of
        wmSinC : Result := AccurateSinc(Value) * Window(Value);
        wmBlackMan : Result := ComputeBlackMan(Value/MaxSupport) * Window(Value);
      end;
    end;
  end;
end;

procedure TBZCustomWindowedInterpolationFilter.SetWidth(Value: Double);
begin
  Value := Min(MaxSupport, Value);
  if Value <> FWidth then
  begin
    FWidth := Value;
    FWidthReciprocal := ComputeReciprocal(FWidth);  // FWidth.Reciprocal;
  end;
end;


{%endregion%}

{%region ====[ Filtres de rééchantillonnage TBZxxxInterpolationFilter ]============}

// Filtres FIR
{%region ====[ TBZBoxInterpolationFilter ]=========================================}

function TBZBoxInterpolationFilter.Filter(x: double): double;// Aka nearest neighbour
begin
  if (x > -0.5) and (x <= 0.5) then
    Result := 1
  else
    Result := 0;
end;

function TBZBoxInterpolationFilter.MaxSupport: double;
begin
  Result := 0.5;
end;
{%endregion}

{%region ====[ TBZTriangleInterpolationFilter ]====================================}

function TBZTriangleInterpolationFilter.Filter(x: double): double; //aka Linear ou BiLinear
begin
  if x < -1.0 then
    Result := 0.0
  else if x < 0.0 then
    Result := 1 + x
  else if x < 1.0 then
    Result := 1 - x
  else
    Result := 0.0;
end;

function TBZTriangleInterpolationFilter.MaxSupport: double;
begin
  Result := 1.0;
end;
{%endregion}

{%region ====[ TBZSystem.CosineInterpolationFilter ]======================================}

function TBZCosineInterpolationFilter.Filter(x: Double): Double;
begin
  Result := 0;
  if Abs(x) < 1 then
    Result := (System.Cos(x * cPI) + 1) * 0.5;
end;

function TBZCosineInterpolationFilter.MaxSupport: double;
begin
  Result := 1;
end;
{%endregion}

{%region ====[ TBZSplineInterpolationFilter ]======================================}

function TBZSplineInterpolationFilter.Filter(x: double): double; // B-spline --> GraphicEx
var   // Graphic32
  tt: Single;
const
  TwoThirds = 2 / 3;
  OneSixth = 1 / 6;
begin
  x := Abs(x);
  if x < 1 then
  begin
    tt := x*x;
    Result := 0.5 * tt * x - tt + TwoThirds;
  end
  else if x < 2 then
  begin
    x := 2 - x;
    Result := OneSixth * (x*x) * x;
  end
  else Result := 0;
end;

function TBZSplineInterpolationFilter.MaxSupport: double;
begin
  Result := 1.0;
end;
{%endregion}

{%region ====[ TBZCatromInterpolationFilter ]======================================}

function TBZCatromInterpolationFilter.Filter(x: double): double;
begin
  if x < -2.0 then
    Result := 0.0
  else if x < -1.0 then
    Result := 0.5 * (4.0 + x * (8.0 + x * (5.0 + x)))
  else if x < 0.0 then
    Result := 0.5 * (2.0 + x * x * (-5.0 - 3.0 * x))
  else if x < 1.0 then
    Result := 0.5 * (2.0 + x * x * (-5.0 + 3.0 * x))
  else if x < 2.0 then
    Result := 0.5 * (4.0 + x * (-8.0 + x * (5.0 - x)))
  else
    Result := 0.0;
end;

function TBZCatromInterpolationFilter.MaxSupport: double;
begin
  Result := 2.0;
end;
{%endregion%}

{%region ====[ TBZCubicInterpolationFilter ]=======================================}
function TBZCubicInterpolationFilter.Filter(x: double): double;
var // Graphic32
  tt, ttt, FCoeff: Single;
begin

 {
     Coefficents are determined from B,C values:
       P0 = (  6 - 2*B       )/6 = coeff[0]
       P1 =         0
       P2 = (-18 +12*B + 6*C )/6 = coeff[1]
       P3 = ( 12 - 9*B - 6*C )/6 = coeff[2]
       Q0 = (      8*B +24*C )/6 = coeff[3]
       Q1 = (    -12*B -48*C )/6 = coeff[4]
       Q2 = (      6*B +30*C )/6 = coeff[5]
       Q3 = (    - 1*B - 6*C )/6 = coeff[6]

    which are used to define the filter:

       P0 + P1*x + P2*x^2 + P3*x^3      0 <= x < 1

       Q0 + Q1*x + Q2*x^2 + Q3*x^3      1 <= x < 2
 }
  FCoeff := -0.5;
  x := Abs(x);
  tt := x*x;
  ttt := tt * x;
  if x < 1 then
    Result := (FCoeff + 2) * ttt - (FCoeff + 3) * tt + 1
  else if x < 2 then
    Result := FCoeff * (ttt - 5 * tt + 8 * x - 4)
  else
    Result := 0;
end;

function TBZCubicInterpolationFilter.MaxSupport: double;
begin
  Result := 2.0;
end;
{%endregion}

{%region ====[ TBZQuinticInterpolationFilter ]======================================}

function TBZQuinticInterpolationFilter.Filter(x : double) : double;
Var
   xSqr : Double;
begin
 x := Abs(x);
 Result := 0;
 if (x <= 1) then
 begin
   xSqr := x * x;
   Result := (((-10 * x + 30) * xSqr - 60) * xSqr + 66) / 120;
   Exit;
 end
 else if (x < 2) then
 begin
   x := 2.0 - x;
   Result := (1 + (5 + (10 + (10 + (5 - 5 * x) * x) * x) * x) * x) / 120;
 end
 else if (x < 3) then
 begin
   x := 3.0 - x;
   xSqr := x * x;
   Result := xSqr * xSqr * x / 120;
 end;
 //Result := x * x * x * (x * (x * 6 - 15) + 10);
end;

function TBZQuinticInterpolationFilter.MaxSupport : double;
begin
  Result := 1.0;
end;
{%endregion}

{%region ====[ TBZQuadraticInterpolationFilter ]===================================}

function TBZQuadraticInterpolationFilter.Filter(x: double): double;
begin
  if x < -1.5 then
    Result := 0.0
  else if x < -0.5 then
  begin
    x := x + 1.5;
    Result := 0.5 * x * x;
  end
  else if x < 0.5 then
    Result := 0.75 - x * x
  else if x < 1.5 then
  begin
    x := x - 1.5;
    Result := 0.5 * x * x;
  end
  else
    Result := 0.0;
end;

function TBZQuadraticInterpolationFilter.MaxSupport: double;
begin
  Result := 1.5;
end;
{%endregion%}

{%region ====[ TBZBellInterpolationFilter ]========================================}

function TBZBellInterpolationFilter.Filter(x: double): double;  // GraphicEx
begin
  if x < 0 then x := -x;
  if x < 0.5 then
    Result := 0.75 - (x*x)
  else
  if x < 1.5 then
  begin
    x := x - 1.5;
    Result := 0.5 * (x*x);
  end
  else
    Result := 0;
end;

function TBZBellInterpolationFilter.MaxSupport: double;
begin
  Result := 1.0;
end;
{%endregion}

{%region ====[ TBZHermitInterpolationFilter ]======================================}

function TBZHermitInterpolationFilter.Filter(x: double): double;
var  // Source : Graphic32, http://paulbourke.net/miscellaneous/interpolation/
  Z: Integer;
  t, t2, t3, m0, m1, a0, a1, a2, a3, FBias, FTension: Single;
begin
  FBias:=0;
  FTension :=0;
  t := (1 - FTension) * 0.5;
  m0 := (1 + FBias) * t;
  m1 := (1 - FBias) * t;

  Z := Floor(x);
  t := Abs(Z - x);
  t2 := t * t;
  t3 := t2 * t;

  a1 := t3 - 2 * t2 + t;
  a2 := t3 - t2;
  a3 := -2 * t3 + 3 * t2;
  a0 := -a3 + 1;

  case Z of
    -2: Result := a2 * m1;
    -1: Result := a3 + a1 * m1 + a2 * (m0 - m1);
     0: Result := a0 + a1 * (m0 - m1) - a2 * m0;
     1: Result := -a1 * m0;
  else
    Result := 0;
  end;
//begin
//  Result := x * x * (3 - 2 * x);
end;

function TBZHermitInterpolationFilter.MaxSupport: double;
begin
  Result := 1.0;
end;
{%endregion}

{%region ====[ TBZWelchInterpolationFilter ]=======================================}

function TBZWelchInterpolationFilter.Filter(x: Double): Double;
begin
  Result := 0.0;

  if (x < 1.0) then
  begin
    Result:=(1.0-x*x);
  end;
end;

function TBZWelchInterpolationFilter.MaxSupport: double;
begin
  Result := 0.5;
end;
{%endregion}

{%region ====[ TBZMitchellInterpolationFilter ]====================================}

function TBZMitchellInterpolationFilter.Filter(x: double): double;
var
  tt, ttt: double;
const OneEighteenth = 1 / 18;
begin
  x := Abs(x);
  tt := Sqr(x);
  ttt := tt * x;
  if x < 1 then Result := (21 * ttt - 36 * tt + 16 ) * OneEighteenth  // get rid of divisions
  else if x < MaxSupport then Result := (- 7 * ttt + 36 * tt - 60 * x + 32) * OneEighteenth // "
  else Result := 0;
end;

(*
// Mitchell-Netravali
const
  B = 1 / 3;
  C = 1 / 3;
var Temp: Single;
begin
 Result := 0.0;
 //if x = 0.0 then Exit;
// if x < 0.0 then x := -x;

  Temp := Sqr(x);
  if x < 1.0 then
  begin
    x := (((12 - 9 * B - 6 * C) * (x * Temp))
             + ((-18 + 12 * B + 6 * C) * Temp)
             + (6 - 2 * B));
    Result := (x) / 6;
  end
  else
    if x < MaxSupport then
    begin
      x := (((-B - 6 * C) * (x * Temp))
               + ((6 * B + 30 * C) * Temp)
               + ((-12 * B - 48 * C) * x)
               + (8 * B + 24 * C));
      Result := (x) / 6;
    end
    else Result := 0.0;

 // if result<>0.0 then
 //   Result := MaxSupport * System.Sin(result) * System.Sin(result / MaxSupport) / (result * result);
end;  *)

function TBZMitchellInterpolationFilter.MaxSupport: double;
begin
  Result := 2.0;
end;
{%endregion}

{%region ====[ TBZHannInterpolationFilter ]========================================}

function TBZHannInterpolationFilter.Filter(x: double): double;
begin
  if x < -1.0 then
    Result := 0.0
  else if x <= 1.0 then
    Result := 0.5 + 0.5 * System.Cos(PI * x)
  else
    Result := 0.0;
end;

function TBZHannInterpolationFilter.MaxSupport: double;
begin
  Result := 1.0;
end;
{%endregion%}

{%region ====[ TBZHammingInterpolationFilter ]=====================================}

function TBZHammingInterpolationFilter.Filter(x: double): double;
begin
  if x < -1.0 then
    Result := 0.0
  else if x <= 1.0 then
    Result := 0.54 + 0.46 * System.Cos(cPI * x)
  else
    Result := 0.0;
end;

function TBZHammingInterpolationFilter.MaxSupport: double;
begin
  Result := 1.0;
end;
{%endregion%}

{%region ====[ TBZSinshInterpolationFilter ]=======================================}

function TBZSinshInterpolationFilter.Filter(x: double): double;
begin
  if x = 0 then Result := 1.0
   else
     Result := 0.5 * System.Sin(cPI * x) / Math.Sinh(cPIdiv2 * x);
end;

function TBZSinshInterpolationFilter.MaxSupport: double;
begin
  Result := 3.0;
end;
{%endregion%}

{%region ====[ TBZBlackmanInterpolationFilter ]====================================}

function TBZBlackmanInterpolationFilter.Filter(x: double): double;
begin
  result:=0;
  if x<0 then x:=-x;
  if x<1.0 then
  begin
    result:=ComputeBlackMan(x);
  end;
//  result := 0.42 + 0.5 * System.Cos(xpi) + 0.08 * System.Cos(2*xpi);   xpi:= Pi*x;
end;

function TBZBlackmanInterpolationFilter.MaxSupport: double;
begin
  Result := 3.0;
end;
{%endregion%}

{%region ====[ TBZLagrangeInterpolationFilter ]====================================}

function TBZLagrangeInterpolationFilter.Filter(x: double): double;
var
 xx : double;
 i, j, k, n : integer;
begin
  result:=0;
  if (x > maxSupport) then exit;
  j:=Round(2*MaxSupport);
  k:=Round(MaxSupport+x);
  xx:=1.0;
  for i:=0 to pred(j) do
  begin
    if (i <> k) then
    begin
      n:=k-i;
      xx:=xx*(n-x)/n;
    end;
  end;
  result:=xx;
end;

function TBZLagrangeInterpolationFilter.MaxSupport: double;
begin
  Result := 2.0;
end;
{%endregion%}

{%region ====[ TBZQuadraticSchaumInterpolationFilter ]=======================================}

function TBZQuadraticSchaumInterpolationFilter.Filter(x : double) : double;
begin
  x := Abs(x);
  Result := 0;

  if (x < 0.5) then
   Result := 1 - x * x
  else if (x = 0.5) then
   Result := 0.5625
  else if (x < 1.5) then
   Result := (x - 3) * x / 2 + 1
  else if (x = 1.5) then
   Result := -0.0625;
end;

function TBZQuadraticSchaumInterpolationFilter.MaxSupport : double;
begin
  Result := 1.5;
end;

{%endregion%}

{%region ====[ TBZCubicSchaumInterpolationFilter ]=======================================}

function TBZCubicSchaumInterpolationFilter.Filter(x : double) : double;
begin
  x := Abs(x);
  Result := 0;
  if (x <= 1) then
    Result := ((x - 2) * x - 1) * x / 2 + 1
  else if (x < 2) then
    Result := ((-x + 6) * x - 11) * x / 6 + 1;
end;

function TBZCubicSchaumInterpolationFilter.MaxSupport : double;
begin
  Result := 2.0;
end;

{%endregion%}

{%region ====[ TBZOMomsx3InterpolationFilter ]=======================================}

function TBZOMomsx3InterpolationFilter.Filter(x : double) : double;
begin
 x := Abs(x);
 Result := 0;

 if (x < 1) then
   Result := ((x / 2 - 1) * x + 1 / 14.0) * x + 13 / 21.0
 else if (x < 2) then
   Result := ((-x / 6 + 1) * x - 85 / 42.0) * x + 29 / 21.0;
end;

function TBZOMomsx3InterpolationFilter.MaxSupport : double;
begin
  Result := 2.0;
end;

{%endregion%}

{%region ====[ TBZOMomsx5InterpolationFilter ]=======================================}

function TBZOMomsx5InterpolationFilter.Filter(x : double) : double;
Var
  xSqr : Double;
begin
  x := Abs(x);
  Result := 0;
  if (x <= 1) then
    Result := (((((-10 * x + 30) * x - (200 / 33.0)) * x - (540 / 11.0)) * x - (5 / 33.0)) * x + (687 / 11.0)) / 120
  else if (x < 2) then
    Result := (((((330 * x - 2970) * x + 10100) * x - 14940) * x + 6755) * x + 2517) / 7920
  else if (x < 3) then
  begin
    x := 3 - x;
    xSqr := x * x;
    Result := ((xSqr + (20 / 33.0)) * xSqr + (1 / 66.0)) * x / 120;
  end;
end;

function TBZOMomsx5InterpolationFilter.MaxSupport : double;
begin
  Result := 3.0;
end;

{%endregion%}

{%region ====[ TBZOMomsx7InterpolationFilter ]=======================================}

function TBZOMomsx7InterpolationFilter.Filter(x : double) : double;
Var
  xSqr : Double;
begin
  x := Abs(x);
  Result := 0;
  if (x <= 1) then
    Result := (((((((15015 * x - 60060) * x + 21021) * x + 180180) * x + 2695) * x - 629244) * x + 21) * x + 989636) / 2162160
  else if (x <= 2) then
  begin
    x := 2 - x;
    Result := (x * (x * (x * (x * (x * (x * (5005 * x - 10010) - 13013) - 10010) + 54285) + 119350) + 106267) + 36606) / 1201200;
  end
  else if (x <= 3) then
  begin
   x := 3 - x;
   Result := (x * (x * (x * (x * (x * (x * (-15015 * x + 15015) + 24024) + 90090) + 102410) + 76230) + 31164) + 5536) / 10810800;
  end
  else if (x < 4) then
  begin
    x := 4 - x;
    xSqr := x * x;
    Result := (x * (xSqr * (xSqr * (2145 * xSqr + 3003) + 385) + 3)) / 10810800;
  end;
end;

function TBZOMomsx7InterpolationFilter.MaxSupport : double;
begin
  Result := 4.0;
end;

{%endregion%}

// ---- Filtres IIR

{%region ====[ TBZBlackmanBesselInterpolationFilter ]==============================}

function TBZBlackmanBesselInterpolationFilter.Filter(x: double): double;
begin
  result:=0;
  if x<0 then x:=-x;
  if x<1.0 then
  begin
    result:=ComputeBlackMan(x/MaxSupport)*Bessel(x);
  end;
end;

function TBZBlackmanBesselInterpolationFilter.MaxSupport: double;
begin
  Result := 3.2383;
end;
{%endregion%}

{%region ====[ TBZBlackmanSinCInterpolationFilter ]================================}

function TBZBlackmanSinCInterpolationFilter.Filter(x: double): double;
begin
  result:=0;
  if x<0 then x:=-x;
  if x<1.0 then
  begin
    result:=ComputeBlackMan(x/MaxSupport)*AccurateSinC(x);
  end;
end;

function TBZBlackmanSinCInterpolationFilter.MaxSupport: double;
begin
  Result := 4.0;
end;
{%endregion%}

{%region ====[ TBZGaussianInterpolationFilter ]====================================}

function TBZGaussianInterpolationFilter.Filter(x: double): double;
//Const
  //Sigma : Double = 0.5;
Var
  Coef1 : Double;
begin
  {
   Gaussian Formula (1D) ...
       exp( -(x^2)/((2.0*sigma^2) ) / (sqrt(2*PI)*sigma^2))

   Gaussian Formula (2D) ...
       exp( -(x^2+y^2)/(2.0*sigma^2) ) / (PI*sigma^2) )
   or for radius
       exp( -(r^2)/(2.0*sigma^2) ) / (PI*sigma^2) )

   Note that it is only a change from 1-d to radial form is in the
   normalization multiplier which is not needed or used when Gaussian is used
   as a filter.

   The constants are pre-calculated...
   Sigma := MaxSupport / 4; //Width / 4;
   Coef1 := 1.0/(2.0*(sigma*sigma));
   coef2:=1.0/(sqrt(c2PI)*(sigma*sigma));
   exp( -coef1*(x*x)) ) * coef2;
  }

  Coef1 := 2.0;   //Sigma = 0.5 par defaut
  result:=System.exp(-Coef1*x*x);
end;

function TBZGaussianInterpolationFilter.MaxSupport: double;
begin
  Result := 1.25;
end;
{%endregion%}

{%region ====[ TBZSinCInterpolationFilter ]========================================}

function TBZSinCInterpolationFilter.Filter(x: double): double;
begin
  if x=0 then result:=cPiDiv2
  else
    result:=AccurateSinC(x);
end;

function TBZSinCInterpolationFilter.MaxSupport: double;
begin
  Result := 1.2196698912665045;
end;
{%endregion%}

{%region ====[ TBZJinCInterpolationFilter (Bessel) ]===============================}

function TBZJinCInterpolationFilter.Filter(x: double): double;
begin
  if x=0 then result:=cPiDiv2
  else
    result:=BesselOrderOne(cPI*x)/x;
end;

function TBZJinCInterpolationFilter.MaxSupport: double;
begin
  Result := 1.2196698912665045;
end;
{%endregion%}

{%region ====[ TBZBeanzInterpolationFilter ]=======================================}

function TBZBeanzInterpolationFilter.Filter(x: double): double;
var
  a,b:double;
begin
   a:=cPI*x*x*x;
   b := AccurateSinC(a); //Math.Sinh(a);
   result:=abs(System.Sin(cPIdiv2 * b)*Bessel(a/(MaxSupport*4)));
end;

function TBZBeanzInterpolationFilter.MaxSupport: double;
begin
  Result := 3.0
end;
{%endregion%}

// ---- Filtres "Windowed"

{%region ====[ TBZBartlettWindowedInterpolationFilter ]============================}

function TBZBartlettWindowedInterpolationFilter.Window(Value: Double): Double;
begin
  result:=(Width - Value)*WidthReciprocal;
end;

function TBZBartlettWindowedInterpolationFilter.MaxSupport: double;
begin
  Result := 3.0;
end;
{%endregion%}

{%region ====[ TBZCosineWindowedInterpolationFilter ]==============================}

function TBZCosineWindowedInterpolationFilter.Window(Value: Double): Double;
begin
   Result :=System.Cos((cPIdiv2)*(Value*WidthReciprocal));
end;

function TBZCosineWindowedInterpolationFilter.MaxSupport: double;
begin
  Result := 3.0;
end;
{%endregion%}

{%region ====[ TBZWelchWindowedInterpolationFilter ]=============================}

function TBZWelchWindowedInterpolationFilter.Window(Value: Double): Double;
Var
  x : Double;
begin
  Result := 0;
  x := abs(Value);
  if x < Width then
    Result := 1.0 - power(WidthReciprocal, 2);
   //Result := (1.0 - (Value * Value / 9));
end;

function TBZWelchWindowedInterpolationFilter.MaxSupport: double;
begin
  Result := 3.0;
end;
{%endregion%}

{%region ====[ TBZHannWindowedInterpolationFilter ]================================}

function TBZHannWindowedInterpolationFilter.Window(Value: Double): Double;
Var
  x : Double;
begin
  x := Abs(Value);
  Result := 0;
  if (x < Width) Then
    Result :=  0.5 * (1 + System.Cos(cPI * (x*WidthReciprocal)));
//    Result := 0.5 + 0.5 * System.Cos(cPI * (Value*WidthReciprocal));
end;

function TBZHannWindowedInterpolationFilter.MaxSupport: double;
begin
  Result := 3.0;
end;
{%endregion%}

{%region ====[ TBZHammingWindowedInterpolationFilter ]=============================}

function TBZHammingWindowedInterpolationFilter.Window(Value: Double): Double;
Var
  x : Double;
begin
  x := Abs(Value);
  Result := 0;
  if (x < Width) Then
     Result := 0.54 + 0.46 * System.Cos(cPI * (x*WidthReciprocal));
end;

function TBZHammingWindowedInterpolationFilter.MaxSupport: double;
begin
  Result := 3.0;
end;
{%endregion%}

{%region ====[ TBZBlackmanWindowedInterpolationFilter ]=============================}

function TBZBlackmanWindowedInterpolationFilter.Window(Value: Double): Double;
begin
   Result := (0.42 - 0.5*System.Cos((cPi*Value)*WidthReciprocal +cPi) + 0.08*System.Cos(c2Pi*(Value*WidthReciprocal)))
end;

function TBZBlackmanWindowedInterpolationFilter.MaxSupport: double;
begin
  Result := 3.0;
end;
{%endregion%}

{%region ====[ TBZGaussianWindowedInterpolationFilter ]============================}

constructor TBZGaussianWindowedInterpolationFilter.Create;
begin
  inherited Create;
  Width:=2.0;
  WindowMode := wmBlackman;
//  FWidthReciprocal := ComputeReciprocal(FWidth);
end;

function TBZGaussianWindowedInterpolationFilter.Window(Value: Double): Double;
begin
 Result:=0.0;
 if (Value < 0) then Value := -Value;
 if (Value < MaxSupport) then
   result:=(System.exp(-2.0 * Value * Value) * System.sqrt(c2DivPI));
end;

function TBZGaussianWindowedInterpolationFilter.MaxSupport: double;
begin
  Result := 1.25;
end;
{%endregion%}

{%region ====[ TBZLanczos3InterpolationFilter ]====================================}

function TBZLanczos3InterpolationFilter.Filter(x: double): double; // GLScene
begin
  Result := 1.0;
  if x = 0.0 then Exit;
  if x < 0.0 then x := -x;
  if x < MaxSupport then
  begin
    x := x * cPI;
    Result := MaxSupport * System.Sin(x) * System.Sin(x / MaxSupport) / (x * x);
  end
  else
    Result := 0.0;
end;

function TBZLanczos3InterpolationFilter.MaxSupport: double;
begin
  Result := 3.0;
end;
{%endregion%}

{%region ====[ TBZBohmanInterpolationFilter ]======================================}

function TBZBohmanInterpolationFilter.Filter(x: double): double; // ImageMagick
Var
  C,S : Double;
begin
 if x < -1.0 then
    Result := 0.0
 else if x <= 1.0 then
 begin
{  Bohman: 2rd Order System.Cosine windowing function:
        (1-x) System.Cos(pi x) + System.Sinpi x) / pi.

      Refactored by Nicolas Robidoux to one trig call, one sqrt call, and 7 flops,
      taking advantage of the fact that the support of Bohman is 1.0 (so that we
      know that System.Sin pi x) >= 0).
 }
   C:=System.Cos(cPI*x);
   S:=System.sqrt(1.0-C*C);
   Result:=(1.0-x) * C + (1/cPI) * S ;
 end
 else result:=0;
end;

function TBZBohmanInterpolationFilter.MaxSupport: double;
begin
  Result := 1.0;
end;
{%endregion%}

{%region ====[ TBZAlbrechtWindowedInterpolationFilter ]============================}

// Adapté de Graphic32
constructor TBZAlbrechtWindowedInterpolationFilter.Create;
begin
  inherited Create;
  Terms := 7;
end;

procedure TBZAlbrechtWindowedInterpolationFilter.SetTerms(Value: Integer);
begin
  if (Value < 2) then Value := 2;
  if (Value > 11) then Value := 11;
  if FTerms <> Value then
  begin
    FTerms := Value;
    case Value of
      2 : Move(CAlbrecht2 [0], FCoefPointer[0], Value * SizeOf(Double));
      3 : Move(CAlbrecht3 [0], FCoefPointer[0], Value * SizeOf(Double));
      4 : Move(CAlbrecht4 [0], FCoefPointer[0], Value * SizeOf(Double));
      5 : Move(CAlbrecht5 [0], FCoefPointer[0], Value * SizeOf(Double));
      6 : Move(CAlbrecht6 [0], FCoefPointer[0], Value * SizeOf(Double));
      7 : Move(CAlbrecht7 [0], FCoefPointer[0], Value * SizeOf(Double));
      8 : Move(CAlbrecht8 [0], FCoefPointer[0], Value * SizeOf(Double));
      9 : Move(CAlbrecht9 [0], FCoefPointer[0], Value * SizeOf(Double));
     10 : Move(CAlbrecht10[0], FCoefPointer[0], Value * SizeOf(Double));
     11 : Move(CAlbrecht11[0], FCoefPointer[0], Value * SizeOf(Double));
    end;
  end;
end;

function TBZAlbrechtWindowedInterpolationFilter.Window(Value: Double): Double;
var
  cs : Double;
  i  : Integer;
begin
  cs := System.Cos(Pi * Value * FWidthReciprocal);
  i := FTerms - 1;
  Result := FCoefPointer[i];
  while i > 0 do
  begin
    Dec(i);
    Result := Result * cs + FCoefPointer[i];
  end;
end;

function TBZAlbrechtWindowedInterpolationFilter.MaxSupport: double;
begin
  Result := 3.0;
end;
{%endregion%}

{%region ====[ TBZKaiserWindowedInterpolationFilter ]==============================}

constructor TBZKaiserWindowedInterpolationFilter.Create;
//Var att:Double;
begin
  inherited Create;
  FTerms := 6.33;  // Compris entre 5 et 8 --> attenuation
 // Att:=40.0;
  //FTerms := exp(log2((0.58417 * (att - 20.96)) * 0.4) + 0.07886 * (att - 20.96));
  Width:=1.0;
  FInvI0Terms := ComputeReciprocal(BesselIO(FTerms));
end;

procedure TBZKaiserWindowedInterpolationFilter.SetTerms(Value: Single);
begin
  if (Value < 5.0) then Value := 5.0;
  if (Value > 8.0) then Value := 8.0;
  if FTerms <> Value then
  begin
    FTerms := Value;
    FInvI0Terms := BesselIO(FTerms).Reciprocal; //ComputeReciprocal(BesselIO(FTerms));
  end;
end;

function TBZKaiserWindowedInterpolationFilter.Window(Value: Double): Double;
begin
  Result := BesselIO(FTerms * System.Sqrt(1.0 - Value * Value)) * FInvI0Terms;
end;

function TBZKaiserWindowedInterpolationFilter.MaxSupport: double;
begin
  Result := 3.0;
end;
{%endregion%}

{%region ====[ TBZPoissonWindowedInterpolationFilter ]=======================================}

function TBZPoissonWindowedInterpolationFilter.Window(Value : Double) : Double;
var
  s, n : Double;
begin
  Result := 0;
  //Value := abs(Value);
  if (Value < -Width) or (Value > Width) then exit;
  if Value <= Width then
  begin
    n := Value + (Width * 0.5);
    s := (FN / 2.0) / (FTerm / 8.69);
    Result := System.Exp(-Abs(n - (FN - 1.0) / 2.0) * (1.0 / s));
  end;
end;

constructor TBZPoissonWindowedInterpolationFilter.Create;
begin
  inherited Create;
  FTerm := 60;
  Width := 1.0;
  WindowMode := wmDefault;
  FN := 2 * Width + 1;
end;

function TBZPoissonWindowedInterpolationFilter.MaxSupport : double;
begin
  Result := 1.0;
end;

{%endregion%}

{%region ====[ TBZHannPoissonWindowedInterpolationFilter ]=======================================}

function TBZHannPoissonWindowedInterpolationFilter.Window(Value : Double) : Double;
var
  n : Double;
begin
  Result := 0;
  if (Value < -Width) or (Value > Width) then exit;
  n := Value + (Width * 0.5);
  Result := 0.5 * (1.0 - System.Cos(c2PI * n / (FN - 1.0))) * System.Exp((-FTerm * Abs(N - 1.0 - 2.0 * n)) / (FN - 1.0));
end;

constructor TBZHannPoissonWindowedInterpolationFilter.Create;
begin
  inherited Create;
  FTerm := 2.0;
  Width := 1.0;
  WindowMode := wmDefault;
  FN := 2 * Width + 1;
end;

function TBZHannPoissonWindowedInterpolationFilter.MaxSupport : double;
begin
  Result := 1.0;
end;

{%endregion%}

{%region ====[ TBZBlackmanHarrisWindowedInterpolationFilter ]=======================================}

function TBZBlackmanHarrisWindowedInterpolationFilter.Window(Value : Double) : Double;
const
  a0 : double = 0.35875;
  a1 : double = 0.48829;
  a2 : double = 0.14128;
  a3 : double = 0.01168;
var
  s, n : Double;
begin
  Result := 0;
  if (Value < -Width) or (Value > Width) then exit;
  n := Value + (Width * 0.5);
  s := c2PI * n / (FN - 1);
  Result := a0 - a1 * System.Cos(s) + a2 * System.Cos(2 * s) - a3 * System.Cos(3 * s);
end;

constructor TBZBlackmanHarrisWindowedInterpolationFilter.Create;
begin
  inherited Create;
  Width := 1.0;
  WindowMode := wmDefault;
  FN := 2 * Width + 1;
end;

function TBZBlackmanHarrisWindowedInterpolationFilter.MaxSupport : double;
begin
  Result := 1.0;
end;

{%endregion%}

{%region ====[ TBZFlatTopWindowedInterpolationFilter ]=======================================}

function TBZFlatTopWindowedInterpolationFilter.Window(Value : Double) : Double;
const
  a0 : double = 1.0;
  a1 : double = 1.93;
  a2 : double = 1.29;
  a3 : double = 0.388;
  a4 : double = 0.028;
var
  s, n : Double;
begin
  Result := 0;
  if (Value < -Width) or (Value > Width) then exit;
  n := Value + Width;//(Width * 0.5);
  s := c2PI * n / (FN - 1);
  Result := a0 - a1 * System.Cos(s) + a2 * System.Cos(2 * s) - a3 * System.Cos(3 * s) + a4 * System.Cos(4 * s);
end;

constructor TBZFlatTopWindowedInterpolationFilter.Create;
begin
  inherited Create;
  Width := 0.5;
  WindowMode := wmDefault;
  FN := 2 * Width + 1;
end;

function TBZFlatTopWindowedInterpolationFilter.MaxSupport : double;
begin
  Result := 1.50;
end;

{%endregion%}

{%region ====[ TBZTukeyWindowedInterpolationFilter ]=======================================}

function TBZTukeyWindowedInterpolationFilter.Window(Value : Double) : Double;
var
  s, n : Double;
begin
  Result := 0;
  if (Value < -Width) or (Value > Width) then exit;
  n := Value + (Width * 0.5);

  if (n <= (FTerm * (FN - 1.0) / 2.0)) then
   Result := 0.5 * (1.0 + System.Cos(cPI * (2.0 * n / (FTerm * (FN - 1.0)) - 1.0)))
  else if (n <= ((FN - 1.0) * (1.0 - FTerm / 2.0))) then Result := 1.0
  else
    Result := 0.5 * (1.0 + System.Cos(cPI * (2.0 * n / (FTerm * (FN - 1.0)) - 2.0 / FTerm - 1.0)));
end;

constructor TBZTukeyWindowedInterpolationFilter.Create;
begin
  inherited Create;
  FTerm := 0.5;
  Width := 1.0;
  //WindowMode := wmDefault;
  FN := 2 * Width + 1;
end;

function TBZTukeyWindowedInterpolationFilter.MaxSupport : double;
begin
  Result := 1.0;
end;

{%endregion%}

{%region ====[ TBZNuttallWindowedInterpolationFilter ]=======================================}

function TBZNuttallWindowedInterpolationFilter.Window(Value : Double) : Double;
const
  a0 : double = 0.355768;
  a1 : double = 0.487396;
  a2 : double = 0.144232;
  a3 : double = 0.012604;
var
  s, n : Double;
begin
  Result := 0;
  if (Value < -Width) or (Value > Width) then exit;
  n := Value + (Width * 0.5);
  s := c2PI * n / (FN - 1);
  Result := a0 - a1 * System.Cos(s) + a2 * System.Cos(2 * s) - a3 * System.Cos(3 * s);
end;

constructor TBZNuttallWindowedInterpolationFilter.Create;
begin
  inherited Create;
  Width := 1.0;
  WindowMode := wmDefault;
  FN := 2 * Width + 1;
end;

function TBZNuttallWindowedInterpolationFilter.MaxSupport : double;
begin
  Result := 2.0;
end;

{%endregion%}

{%region ====[ TBZBlackmanNuttallWindowedInterpolationFilter ]=======================================}

function TBZBlackmanNuttallWindowedInterpolationFilter.Window(Value : Double) : Double;
const
  a0 : double = 0.3635819;
  a1 : double = 0.4891775;
  a2 : double = 0.1365995;
  a3 : double = 0.0106411;
var
  s, n : Double;
begin
  Result := 0;
  if (Value < -Width) or (Value > Width) then exit;
  n := Value + (Width * 0.5);
  s := c2PI * n / (FN - 1);
  Result := a0 - a1 * System.Cos(s) + a2 * System.Cos(2 * s) - a3 * System.Cos(3 * s);
end;

constructor TBZBlackmanNuttallWindowedInterpolationFilter.Create;
begin
 inherited Create;
 Width := 1.0;
 WindowMode := wmDefault;
 FN := 2 * Width + 1;
end;

function TBZBlackmanNuttallWindowedInterpolationFilter.MaxSupport : double;
begin
  Result := 2.0;
end;

{%endregion%}

{%region ====[ TBZBarlettHannWindowedInterpolationFilter ]=======================================}

function TBZBartlettHannWindowedInterpolationFilter.Window(Value : Double) : Double;
const
  a0 : double = 0.62;
  a1 : double = 0.48;
  a2 : double = 0.38;
var
  s, n : Double;
begin
  Result := 0;
  if (Value < -Width) or (Value > Width) then exit;
  n := Value + (Width * 0.5);
  Result := a0 - a1 * Abs(n / (FN - 1.0) - 0.5) - a2 * System.Cos(c2PI * n / (FN - 1.0));
end;

constructor TBZBartlettHannWindowedInterpolationFilter.Create;
begin
  inherited Create;
  Width := 1.0;
  WindowMode := wmDefault;
  FN := 2 * Width + 1;
end;

function TBZBartlettHannWindowedInterpolationFilter.MaxSupport : double;
begin
  Result := 2.0;
end;

{%endregion%}

{%region ====[ TBZCauchyWindowedInterpolationFilter ]=======================================}

function TBZCauchyWindowedInterpolationFilter.Window(Value : Double) : Double;
begin
  Result := 0;
  if (Value < -Width) or (Value > Width) then exit;
  //n := Value + Width;
  Result := 1.0 / (1.0 + Math.Power(FTerm * Abs(Value) * WidthReciprocal, 2.0));
end;

constructor TBZCauchyWindowedInterpolationFilter.Create;
begin
  inherited Create;
  Width := 1.0;
  WindowMode := wmDefault;
  FTerm := 3.0;
  FN := 2 * Width + 1;
end;

function TBZCauchyWindowedInterpolationFilter.MaxSupport : double;
begin
  Result := 1.0;
end;

{%endregion%}

{%endregion%}

{%region ====[ Support InterpolationFilters ]=======================================}

function GetBZInterpolationFilters: TBZInterpolationFiltersList;
begin
 if not Assigned(vInterpolationFilters) then vInterpolationFilters := TBZInterpolationFiltersList.Create;
 Result := vInterpolationFilters;
end;

procedure RegisterBZInterpolationFilter(const AName, ADescription: string; AClass: TBZInterpolationFilterClass);
begin
 Classes.RegisterClass(AClass);
 GetBZInterpolationFilters.Add(AName, ADescription, 0, AClass);
end;

procedure UnregisterBZInterpolationFilter(AClass: TBZInterpolationFilterClass);
begin
 if Assigned(vInterpolationFilters) then vInterpolationFilters.Remove(AClass);
end;

destructor  TBZInterpolationFiltersList.Destroy;
begin
 Clean;
 inherited;
end;

procedure  TBZInterpolationFiltersList.Add(const aName, Desc: string; DescID: Integer; AClass: TBZInterpolationFilterClass);
var
 newRec: TBZInterpolationFilterRec;
begin
 newRec := TBZInterpolationFilterRec.Create;
 with newRec do
 begin
   Name := AnsiLowerCase(aName);
   InterpolationFilterClass := AClass;
   Description := Desc;
   DescResID := DescID;
 end;
 inherited Add(newRec);
end;

function TBZInterpolationFiltersList.FindFromName(AName: string): TBZInterpolationFilterClass;
var
 i: Integer;
begin
 aName := LowerCase(aName);
 for i := Count - 1 downto 0 do
   with TBZInterpolationFilterRec(Items[I]) do
   begin
     if Name = aName then
     begin
       Result := InterpolationFilterClass;
       Exit;
     end;
   end;
 Result := nil;
end;


procedure TBZInterpolationFiltersList.Remove(AClass: TBZInterpolationFilterClass);
var
 i: Integer;
begin
 for i := 0 to pred(Count) do
 begin
   if TBZInterpolationFilterRec(Items[i]).InterpolationFilterClass.InheritsFrom(AClass) then DeleteAndFree(i);
 end;
end;

procedure TBZInterpolationFiltersList.BuildStringList(var filters: TStringList);
var
 i: Integer;
 p: TBZInterpolationFilterRec;
begin
 for i := 0 to Count - 1 do
 begin
   p := TBZInterpolationFilterRec(Items[i]);
   if (p.Name <> '') then
   begin
     with p do
     begin
       filters.Add(Name+' '+description);
     end;
   end;
 end;
end;

function TBZInterpolationFiltersList.GetByIndex(index: Integer): TBZInterpolationFilterClass;
begin
 Result := nil;
 if index > 0 then
   result := TBZInterpolationFilterRec(Items[index]).InterpolationFilterClass;
end;

function TBZInterpolationFiltersList.GetFilterByIndex(index: Integer): TBZInterpolationFilterMethod;
begin
// Result := 0;
 if index > 0 then
   result := TBZInterpolationFilterMethod(index)
 else result:= ifmLanczos3; // Filtre par defaut
end;

{%endregion%}

Initialization
  { Enregistrement des filtres
    Attention ordre de la liste identique à  TBZInterpolationFilterMethod definit dans BZGraphicTypes.inc
  }
  RegisterBZInterpolationFilter('Box', 'FIR', TBZBoxInterpolationFilter);
  RegisterBZInterpolationFilter('Triangle', 'FIR', TBZTriangleInterpolationFilter);
  RegisterBZInterpolationFilter('Cosine', 'FIR', TBZCosineInterpolationFilter);
  RegisterBZInterpolationFilter('Spline', 'FIR', TBZSplineInterpolationFilter);
  RegisterBZInterpolationFilter('Catrom', 'FIR', TBZCatromInterpolationFilter);
  RegisterBZInterpolationFilter('Cubic', 'FIR', TBZCubicInterpolationFilter);
  RegisterBZInterpolationFilter('Quadratic', 'FIR', TBZQuadraticInterpolationFilter);
  RegisterBZInterpolationFilter('Quintic', 'FIR', TBZQuinticInterpolationFilter);
  RegisterBZInterpolationFilter('Bell', 'FIR', TBZBellInterpolationFilter);
  RegisterBZInterpolationFilter('Hermit', 'FIR', TBZHermitInterpolationFilter);
  RegisterBZInterpolationFilter('Welch', 'FIR', TBZWelchInterpolationFilter);
  RegisterBZInterpolationFilter('Mitchell', 'FIR', TBZMitchellInterpolationFilter);
  RegisterBZInterpolationFilter('Hann', 'FIR', TBZHannInterpolationFilter);
  RegisterBZInterpolationFilter('Hamming', 'FIR', TBZHammingInterpolationFilter);
  RegisterBZInterpolationFilter('Sinsh', 'FIR', TBZSinshInterpolationFilter);
  RegisterBZInterpolationFilter('Blackman', 'FIR', TBZBlackmanInterpolationFilter);
  RegisterBZInterpolationFilter('Lagrange', 'FIR', TBZLagrangeInterpolationFilter);
  RegisterBZInterpolationFilter('Quadratic Schaum', 'FIR', TBZQuadraticSchaumInterpolationFilter);
  RegisterBZInterpolationFilter('Cubic Schaum', 'FIR', TBZQuadraticSchaumInterpolationFilter);
  RegisterBZInterpolationFilter('O-Moms x3', 'FIR', TBZOMomsx3InterpolationFilter);
  RegisterBZInterpolationFilter('O-Moms x5', 'FIR', TBZOMomsx5InterpolationFilter);
  RegisterBZInterpolationFilter('O-Moms x7', 'FIR', TBZOMomsx7InterpolationFilter);

  RegisterBZInterpolationFilter('BlackmanBessel', 'IIR', TBZBlackmanBesselInterpolationFilter);
  RegisterBZInterpolationFilter('BlackmanSinC', 'IIR', TBZBlackmanSinCInterpolationFilter);
  RegisterBZInterpolationFilter('Gaussian', 'IIR', TBZGaussianInterpolationFilter);
  RegisterBZInterpolationFilter('SinC', 'IIR', TBZSinCInterpolationFilter);
  RegisterBZInterpolationFilter('JinC', 'IIR', TBZJinCInterpolationFilter);

  RegisterBZInterpolationFilter('Nuttall', 'WND', TBZNuttallWindowedInterpolationFilter);
  RegisterBZInterpolationFilter('Tukey', 'WND', TBZTukeyWindowedInterpolationFilter);
  RegisterBZInterpolationFilter('Poisson', 'WND', TBZPoissonWindowedInterpolationFilter);
  RegisterBZInterpolationFilter('Cauchy', 'WND', TBZCauchyWindowedInterpolationFilter);
  RegisterBZInterpolationFilter('Flat Top', 'WND', TBZFlatTopWindowedInterpolationFilter);
  RegisterBZInterpolationFilter('Bartlett', 'WND', TBZBartlettWindowedInterpolationFilter);
  RegisterBZInterpolationFilter('Bartlett Hann', 'WND', TBZBartlettHannWindowedInterpolationFilter);
  RegisterBZInterpolationFilter('Cosine', 'WND', TBZCosineWindowedInterpolationFilter);
  RegisterBZInterpolationFilter('Welch', 'WND', TBZWelchWindowedInterpolationFilter);
  RegisterBZInterpolationFilter('Hann', 'WND',  TBZHannWindowedInterpolationFilter);
  RegisterBZInterpolationFilter('Hann Poisson', 'WND',  TBZHannPoissonWindowedInterpolationFilter);
  RegisterBZInterpolationFilter('Hamming', 'WND', TBZHammingWindowedInterpolationFilter);
  RegisterBZInterpolationFilter('Blackman', 'WND', TBZBlackmanWindowedInterpolationFilter);
  RegisterBZInterpolationFilter('Blackman Nuttall', 'WND', TBZBlackmanNuttallWindowedInterpolationFilter);
  RegisterBZInterpolationFilter('Blackman Harris', 'WND', TBZBlackmanHarrisWindowedInterpolationFilter);
  RegisterBZInterpolationFilter('Gaussian', 'WND', TBZGaussianWindowedInterpolationFilter);
  RegisterBZInterpolationFilter('Lanczos3', 'WND', TBZLanczos3InterpolationFilter);
  RegisterBZInterpolationFilter('Bohman', 'WND', TBZBohmanInterpolationFilter);
  RegisterBZInterpolationFilter('Albrecht', 'WND', TBZAlbrechtWindowedInterpolationFilter);
  RegisterBZInterpolationFilter('Kaiser', 'WND', TBZKaiserWindowedInterpolationFilter);
  RegisterBZInterpolationFilter('Beanz', 'WND', TBZBeanzInterpolationFilter);

Finalization
  UnRegisterBZInterpolationFilter(TBZBoxInterpolationFilter);
  UnRegisterBZInterpolationFilter(TBZTriangleInterpolationFilter);
  UnRegisterBZInterpolationFilter(TBZCosineInterpolationFilter);
  UnRegisterBZInterpolationFilter(TBZSplineInterpolationFilter);
  UnRegisterBZInterpolationFilter(TBZCatromInterpolationFilter);
  UnRegisterBZInterpolationFilter(TBZCubicInterpolationFilter);
  UnRegisterBZInterpolationFilter(TBZQuadraticInterpolationFilter);
  UnRegisterBZInterpolationFilter(TBZQuinticInterpolationFilter);
  UnRegisterBZInterpolationFilter(TBZBellInterpolationFilter);
  UnRegisterBZInterpolationFilter(TBZHermitInterpolationFilter);
  UnRegisterBZInterpolationFilter(TBZWelchInterpolationFilter);
  UnRegisterBZInterpolationFilter(TBZMitchellInterpolationFilter);
  UnRegisterBZInterpolationFilter(TBZHannInterpolationFilter);
  UnRegisterBZInterpolationFilter(TBZHammingInterpolationFilter);
  UnRegisterBZInterpolationFilter(TBZSinshInterpolationFilter);
  UnRegisterBZInterpolationFilter(TBZBlackmanInterpolationFilter);
  UnRegisterBZInterpolationFilter(TBZLagrangeInterpolationFilter);
  UnRegisterBZInterpolationFilter(TBZQuadraticSchaumInterpolationFilter);
  UnRegisterBZInterpolationFilter(TBZQuadraticSchaumInterpolationFilter);
  UnRegisterBZInterpolationFilter(TBZOMomsx3InterpolationFilter);
  UnRegisterBZInterpolationFilter(TBZOMomsx5InterpolationFilter);
  UnRegisterBZInterpolationFilter(TBZOMomsx7InterpolationFilter);
  UnRegisterBZInterpolationFilter(TBZBlackmanBesselInterpolationFilter);
  UnRegisterBZInterpolationFilter(TBZBlackmanSinCInterpolationFilter);
  UnRegisterBZInterpolationFilter(TBZGaussianInterpolationFilter);
  UnRegisterBZInterpolationFilter(TBZSinCInterpolationFilter);
  UnRegisterBZInterpolationFilter(TBZJinCInterpolationFilter);
  UnRegisterBZInterpolationFilter(TBZNuttallWindowedInterpolationFilter);
  UnRegisterBZInterpolationFilter(TBZTukeyWindowedInterpolationFilter);
  UnRegisterBZInterpolationFilter(TBZPoissonWindowedInterpolationFilter);
  UnRegisterBZInterpolationFilter(TBZCauchyWindowedInterpolationFilter);
  UnRegisterBZInterpolationFilter(TBZFlatTopWindowedInterpolationFilter);
  UnRegisterBZInterpolationFilter(TBZBartlettWindowedInterpolationFilter);
  UnRegisterBZInterpolationFilter(TBZBartlettHannWindowedInterpolationFilter);
  UnRegisterBZInterpolationFilter(TBZCosineWindowedInterpolationFilter);
  UnRegisterBZInterpolationFilter(TBZWelchWindowedInterpolationFilter);
  UnRegisterBZInterpolationFilter(TBZHannWindowedInterpolationFilter);
  UnRegisterBZInterpolationFilter(TBZHannPoissonWindowedInterpolationFilter);
  UnRegisterBZInterpolationFilter(TBZHammingWindowedInterpolationFilter);
  UnRegisterBZInterpolationFilter(TBZBlackmanWindowedInterpolationFilter);
  UnRegisterBZInterpolationFilter(TBZBlackmanNuttallWindowedInterpolationFilter);
  UnRegisterBZInterpolationFilter(TBZBlackmanHarrisWindowedInterpolationFilter);
  UnRegisterBZInterpolationFilter(TBZGaussianWindowedInterpolationFilter);
  UnRegisterBZInterpolationFilter(TBZLanczos3InterpolationFilter);
  UnRegisterBZInterpolationFilter(TBZBohmanInterpolationFilter);
  UnRegisterBZInterpolationFilter(TBZAlbrechtWindowedInterpolationFilter);
  UnRegisterBZInterpolationFilter(TBZKaiserWindowedInterpolationFilter);
  UnRegisterBZInterpolationFilter(TBZBeanzInterpolationFilter);

  FreeAndNil(vInterpolationFilters);
end.
