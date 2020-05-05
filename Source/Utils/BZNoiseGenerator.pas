(*
  @abstract(Contient des classes d'aide à la génération de bruit.)

  tel que bruit blanc, bruit blanc additif gaussien, bruit de perlin, bruit fractal,
  bruit cellulaire etc... @br
  Ces classe permettent la creation de divers signaux ainsi que la génération de texture.

  -------------------------------------------------------------------------------------------------------------

  @created(2019-11-10)
  @author(J.Delauney (BeanzMaster))
  Historique : @br
  @unorderedList(
    @item(10/11/2019 : Creation  )
  )

  -------------------------------------------------------------------------------------------------------------

  @bold(Notes) : @br
  
  -------------------------------------------------------------------------------------------------------------

  @bold(Credits :)
    @unorderedList(
      @item(J.Delauney (BeanzMaster))
    )

  -------------------------------------------------------------------------------------------------------------

  @bold(LICENCE) : MPL / GPL

  ------------------------------------------------------------------------------------------------------------- *)
unit BZNoiseGenerator;

//==============================================================================
{$mode objfpc}{$H+}
{$i ..\bzscene_options.inc}
//==============================================================================

Interface

{ TODO -oBZBitmap -cFiltre : Unité à compléter pour être à 100% fonctionnelle dans la génération de texture cohérente }

uses
  Classes, SysUtils,
  BZClasses, BZInterpolationFilters;
  
Const
  { Nombre aléatoire premier pour l'axe des X }
  cX_PRIME = 1619;
  { Nombre aléatoire premier pour l'axe des Y }
  cY_PRIME = 31337;
  { Nombre aléatoire premier pour l'axe des Z }
  cZ_PRIME = 6971;
  { Nombre aléatoire premier pour l'axe des W }
  cW_PRIME = 1013;
  { Nombre aléatoire }
  cNOISE_SEED = 4679;

const
  { Taille de la table de Perlin }
  cPERLIN_TABLE_SIZE = 256;
  { Masque de la table de Perlin }
  cPERLIN_TABLE_MASK = 255;

Type
  { Enumération des types de pertubation du bruit }
  TBZNoisePerturbation = (npNone, npGradient, npGradientFractal, npGradientNormalize, npGradientFractalNormalize);
  { Enumération des types d'interpolation du bruit }
  TBZNoiseInterpolationType = (nitLinear, nitCosine, nitCubic);
  { Enumération des types de bruit }
  TBZNoiseSupport = (ns1D, ns2D, ns3D, ns4D, nsInterpolation, nsSmooth);
  { Support des bruits }
  TBZNoiseSupports = set of TBZNoiseSupport;

  { Classe de base à hériter pour la génération de bruit "simple" }
  TBZCustomNoiseGenerator = class(TBZUpdateAbleObject)
  private  

    FSupport : TBZNoiseSupports;

  protected
    FSeed : Int64;

    procedure SetSeed(const AValue : Int64); virtual;

    //function RandomSeed;
    Function RandUniform1D(x : Double) : Double;
    Function RandUniform2D(x, y : Double) : Double;
    Function RandUniform3D(x, y, z : Double) : Double;
    Function RandUniform4D(x, y, z, w : Double) : Double;

    Function RandAdditiveGaussian1D(x : Double) : Double;
    Function RandAdditiveGaussian2D(x, y : Double) : Double;
    Function RandAdditiveGaussian3D(x, y, z : Double) : Double;
    Function RandAdditiveGaussian4D(x, y, z, w : Double) : Double;


  public
    { Creation }
    Constructor Create; override;
    //Destructor Destroy; override;

    { Retourne la valeur aléatoire d'un bruit 1D }
    Function Noise1D(x : Double) : Double; virtual;
    { Retourne la valeur aléatoire d'un bruit 2D }
    Function Noise2D(x, y : Double) : Double; virtual;
    { Retourne la valeur aléatoire d'un bruit 3D }
    Function Noise3D(x, y, z : Double) : Double; virtual;
    { Retourne la valeur aléatoire d'un bruit 4D }
    Function Noise4D(x, y, z, w : Double) : Double; virtual;

    { Retourne la valeur du bruit en fonction de sa coordonnée 1D et en prenant en compte la fréquence }
    function GetNoise(x : Double) : Double; virtual; abstract; overload;
    { Retourne la valeur du bruit en fonction de sa coordonnée 2D et en prenant en compte la fréquence }
    function GetNoise(x, y : Double) : Double; virtual; abstract; overload;
    { Retourne la valeur du bruit en fonction de sa coordonnée 3D et en prenant en compte la fréquence }
    function GetNoise(x, y, z : Double) : Double; virtual; abstract; overload;
    { Retourne la valeur du bruit en fonction de sa coordonnée 4D et en prenant en compte la fréquence }
    function GetNoise(x, y, z, w : Double) : Double; virtual; abstract; overload;

    { Nombre aléatoire, pour la génération du bruit }
    property Seed : Int64 read FSeed write SetSeed;
    { Types des bruits supportés }
    property Support : TBZNoiseSupports read FSupport write FSupport;
  end;

  { Classe de type TBZCustomNoiseGenerator }
  TBZNoiseGeneratorClass = Class of TBZCustomNoiseGenerator;

  { Enumération des générateurs aléatoire pour la génération du bruit }
  TBZNoiseRandomGeneratorType = (ngtWhite, ngtAdditiveGaussian, ngtCustom);  //
  { Méthode personalisée pour la génération d'un bruit }
  TBZOnCustomRandomNoise = procedure(Dimension : Byte; x,y,z,w : Double; var value : Double) of object;

  //TBZNoiseGeneratorType = (ngtWhite, ngtValue, ngtCubic, ngtSimplex, ngtPerlin, ngtCustom);
  
  { @abstract(Générateur de bruit monochrome)

    Génère également des échantillons de bruit gaussien blanc additif avec une moyenne nulle et un écart-type de 1.0 . @br
    cf : https://en.wikipedia.org/wiki/White_noise  }
  TBZBaseNoiseGenerator = Class(TBZCustomNoiseGenerator)
  private
    FFrequency : Double;

    FSmooth : Boolean;
    FNoiseInterpolationType : TBZNoiseInterpolationType;
    FSmoothInterpolationType : TBZInterpolationFilterMethod;

    FRandomNoiseGenerator : TBZNoiseRandomGeneratorType;
    FOnCustomRandomNoise : TBZOnCustomRandomNoise;
    FInterpolationMode : TBZInterpolationFilterMethod;

    FPerturbation : TBZNoisePerturbation;
    FPerturbationFrequency : Double;
    FPerturbationAmplitude : Double;

    procedure SetFrequency(const AValue : Double);

  protected
    // Retourne un entier aléatoire
    function Hash1D(x : Integer):Integer;
    function Hash2D(x, y : Integer):Integer;
    function Hash3D(x, y, z : Integer):Integer;
    function Hash4D(x, y, z, w : Integer):Integer;

    // Retourne les coordonées perturbées du bruit (à utiliser avant les méthodes NoiseXD)
    procedure GradientPerturb1D(Var x : Double); virtual;
    procedure GradientPerturb2D(Var x, y : Double); virtual;
    procedure GradientPerturb3D(Var x, y, z : Double); virtual;
    procedure GradientPerturb4D(Var x, y, z, w : Double); virtual;

  public

    Constructor Create; override;

    { Retourne la valeur aléatoire d'un bruit 1D }
    Function Noise1D(x : Double) : Double; override;
    { Retourne la valeur aléatoire d'un bruit 2D }
    Function Noise2D(x, y : Double) : Double; override;
    { Retourne la valeur aléatoire d'un bruit 3D }
    Function Noise3D(x, y, z : Double) : Double; override;
    { Retourne la valeur aléatoire d'un bruit 4D }
    Function Noise4D(x, y, z, w : Double) : Double; override;

    { Retourne la valeur du bruit en fonction de sa coordonnée 1D et en prenant en compte la fréquence }
    function GetNoise(x : Double) : Double;  override;
    { Retourne la valeur du bruit en fonction de sa coordonnée 2D et en prenant en compte la fréquence }
    function GetNoise(x, y : Double) : Double;  override;
    { Retourne la valeur du bruit en fonction de sa coordonnée 3D et en prenant en compte la fréquence }
    function GetNoise(x, y, z : Double) : Double; override;
    { Retourne la valeur du bruit en fonction de sa coordonnée 4D et en prenant en compte la fréquence }
    function GetNoise(x, y, z, w : Double) : Double; override;

    { Type du générateur de bruit aléatoire }
    property RandomNoiseGenerator : TBZNoiseRandomGeneratorType read FRandomNoiseGenerator write FRandomNoiseGenerator;
    { Type d'interpolation }
    property NoiseInterpolation : TBZNoiseInterpolationType read FNoiseInterpolationType write FNoiseInterpolationType;
    { Fréquence pour la génération du bruit }
    property Frequency : Double read FFrequency write SetFrequency;
    { Active / désactive le lissage du bruit }
    property Smooth : Boolean read FSmooth write FSmooth;
    { Type du filtre pour le lissage }
    property SmoothInterpolation : TBZInterpolationFilterMethod read FInterpolationMode write FInterpolationMode;
    { Type de pertubation du bruit }
    property Perturbation : TBZNoisePerturbation read FPerturbation write FPerturbation;
    { Fréquence de la perturbation }
    property PerturbationFrequency : Double read FPerturbationFrequency write FPerturbationFrequency;
    { Amplitude de la perturbation }
    property PerturbationAmplitude : Double read FPerturbationAmplitude write FPerturbationAmplitude;

    { Methode personnelle de la génération du bruit }
    property OnCustomRandomNoise : TBZOnCustomRandomNoise read FOnCustomRandomNoise write FOnCustomRandomNoise;
  end;

  
  { @abstract(Générateur de bruit multi valeurs.)

    cf : https://en.wikipedia.org/wiki/Value_noise}
  TBZValueNoiseGenerator = Class(TBZBaseNoiseGenerator)
  public
    Constructor Create; override;

    Function Noise1D(x : Double) : Double; override;
    Function Noise2D(x, y : Double) : Double; override;
    Function Noise3D(x, y, z : Double) : Double; override;
    Function Noise4D(x, y, z, w : Double) : Double; override;
  end;

  { Générateur de bruit "Gradient" appelé aussi "Perlin" suivant l'aglorithme de Ken Perlin }
  TBZGradientNoiseGenerator = Class(TBZBaseNoiseGenerator)
  private

  protected
    function GradientNoise1D(x : Double) : Double;
    function GradientNoise2D(x, y : Double) : Double;
    //function GradientNoise3D(x, y, z : Double) : Double;
    //function GradientNoise4D(x, y, z, w : Double) : Double;
  public
    Constructor Create; override;

    Function Noise1D(x : Double) : Double; override;
    Function Noise2D(x, y : Double) : Double; override;
    Function Noise3D(x, y, z : Double) : Double; override;
    Function Noise4D(x, y, z, w : Double) : Double; override;
  end;
  { Classe de type TBZGradientNoiseGenerator }
  TBZPerlinNoiseGenerator = Class(TBZGradientNoiseGenerator);

  { Générateur de bruit "Perlin" amélioré suivant l'aglorithme de Ken Perlin  }
  TBZImprovedPerlinNoiseGenerator = Class(TBZBaseNoiseGenerator)
  private

  protected
    FPermutations : packed array [0..cPERLIN_TABLE_SIZE-1] of Integer;
    FGradients    : packed array [0..cPERLIN_TABLE_SIZE*3-1] of Single;

    procedure SetSeed(const AValue : Int64); override;

    function Lattice1D(ix: Integer; fx : Single) : Single;
    function Lattice2D(ix, iy : Integer; fx, fy : Single) : Single;
    function Lattice3D(ix, iy, iz : Integer; fx, fy, fz : Single) : Single;
    //function Lattice4D(ix, iy, iz, iw : Integer; fx, fy, fz, fw : Single) : Single;
  public
    Constructor Create; override;

    procedure Initialize(aSeed : Int64); Virtual;

    Function Noise1D(x : Double) : Double; override;
    Function Noise2D(x, y : Double) : Double; override;
    Function Noise3D(x, y, z : Double) : Double; override;
    Function Noise4D(x, y, z, w : Double) : Double; override;
  end;

  { @abstract(Générateur de bruit suivant l'algorithme Simplex)

    cf : https://en.wikipedia.org/wiki/Simplex_noise @br
    @bold(Note) : Attention l'utilisation de cette algorithme est soumis à licence }
  TBZSimplexNoiseGenerator = Class(TBZImprovedPerlinNoiseGenerator)
  private
    function Grad1D(Hash : Integer; x : Double) : Double;
    function Grad2D(Hash : Integer; x, y : Double) : Double;
    function Grad3D(Hash : Integer; x, y, z : Double) : Double;
    function Grad4D(Hash : Integer; x, y, z, w : Double) : Double;
  public
    Constructor Create; override;

    Function Noise1D(x : Double) : Double; override;
    Function Noise2D(x, y : Double) : Double; override;
    Function Noise3D(x, y, z : Double) : Double; override;
    Function Noise4D(x, y, z, w : Double) : Double; override;
  end;

  // OpenSimpleXNoise :  https://en.wikipedia.org/wiki/OpenSimplex_noise
  // Convertis depuis : https://gist.github.com/KdotJPG/b1270127455a94ac5d19

  { TBZOpenSimplexNoiseGenerator }

  TBZOpenSimplexNoiseGenerator = Class(TBZImprovedPerlinNoiseGenerator)
  private
    //FPermutations : packed array [0..cPERLIN_TABLE_SIZE-1] of Integer;
    FGradientPermutationsIndex3D : Array[0..255] of byte;

    function Extrapolate2D(xsb, ysb : Integer; dx, dy : Double) : Double;
    function Extrapolate3D(xsb, ysb, zsb : Integer; dx, dy, dz : Double) : Double;
    function Extrapolate4D(xsb, ysb, zsb, wsb : Integer; dx, dy, dz, dw : Double) : Double;

  protected
    //procedure SetSeed(const AValue : Int64); override;

  public
    Constructor Create; override;

    procedure Initialize(aSeed : Int64); override;

    Function Noise1D(x : Double) : Double; override;
    Function Noise2D(x, y : Double) : Double; override;
    Function Noise3D(x, y, z : Double) : Double; override;
    Function Noise4D(x, y, z, w : Double) : Double; override;
  end;

  // WorleyNoise : https://en.wikipedia.org/wiki/Worley_noise 
  
  //TBZFractalNoiseType = (fntStandard, fntFBM, fntBillow, fntTurbulence, fntDistored, fntMulti, fntHybrid, fnrHetero, fntRidged);
  { TBZCustomFractalNoiseGenerator : Classe de base à hériter pour la génération de bruit "fractal" }
  //TBZCustomFractalNoiseGenerator = Class(TBZCustomNoiseGenerator)
  //private
  //  FNoiseGenerator : TBZBaseNoiseGenerator;
  //  FFractalNoiseType : TBZFractalNoiseType;
  //  FLacunarity : Double;
  //  FGain : Double;
  //  FOctaves : LongWord;
  //  FFractalBounding : Double;
  //protected
  //  procedure ComputeFractalBounding;
  //public
  //
  //  Constructor Create; override;
  //  Destructor Destroy; override;
  //  
  //  //Function Noise1D(x:Double):Double; override; 
  //  Function Noise2D(x,y:Double):Double; override;
  //  Function Noise3D(x,y,z : Double): Double; override;
  //  
  //  property FractalNoiseType : TBZFractalNoiseType read FFractalNoiseType write FFractalNoiseType;
  //  //property NoiseGenerator :TBZCustomNoiseGenerator read FNoiseGenerator write SetNoiseGenerator;
  //end;
  
  //TBZCellularDistanceType = (cdtEuclidian, cdtManhatan, cdtNatural);
  //TBZCellularDistanceReturnTypz = (cdrtValue, cdrtNoiseLookup, cdrtDistance, cdrtDistance2, cdrtDistance2Add, cdrtDistance2Sub, cdrtDistance2Mul, cdrtDistance2Div, cdrtDistanceCave);
  { TBZCustomCellularNoiseGenerator : Classe de base à hériter pour la génération de bruit "cellulaire" }
  //TBZCustomCellularNoiseGenerator = Class(TBZCustomNoiseGenerator)
  //private
  //  FCellularDistanceType : TBZCellularDistanceType;
  //  FCellularDistanceReturnKind : TBZCellularDistanceReturnKind;
  //  FNoiseLookUpType : TBZNoiseGeneratorType;
  //  FJitter : Double;
  //  FDistanceIndiceA, FDistanceIndiceB : Double;
  //protected
  //public
  //  property CellularDistanceType : TBZCellularDistanceType;
  //  property CellularDistanceReturnKind : TBZCellularDistanceReturnKind;
  //  property Jitter : Double;
  //  property DistanceIndiceA : Double; 
  //  property DistanceIndiceB : Double;
  //end;  


  //TBZSimpleCellularNoiseGenerator  = Class(TBZCustomCellularNoiseGenerator); 
  //TBZVoronoiCellularNoiseGenerator  = Class(TBZCustomCellularNoiseGenerator); 
  
  //TBZTextureGenerator

Implementation

uses Math, BZMath;

{%region=====[ Open Simplex Noise constantes ]==================================}
Const
  cOSN_STRETCH_2D      : Double = -0.211324865405187;      // (1 / sqrt(2 + 1) - 1 ) / 2;
  cOSN_SQUISH_2D       : Double = 0.366025403784439;       // (sqrt(2 + 1) -1) / 2; */

  cOSN_STRETCH_3D      : Double = -1.0 / 6.0;              // (1 / sqrt(3 + 1) - 1) / 3; */
  cOSN_SQUISH_3D       : Double = 1.0 / 3.0;               // (sqrt(3+1)-1)/3; */

  cOSN_STRETCH_4D      : Double = -0.138196601125011;      // (1 / sqrt(4 + 1) - 1) / 4; */
  cOSN_SQUISH_4D       : Double = 0.309016994374947;       // (sqrt(4 + 1) - 1) / 4; */

  cOSN_NORM_2D  : Double =  1.0 / 47.0;
  cOSN_NORM_3D  : Double =  1.0 / 103.0;
  cOSN_NORM_4D  : Double =  1.0 / 30.0;

  //Gradients for 2D. They approximate the directions to the
  //vertices of an octagon from the center.
  cOSN_gradients2D : Array[0..15] of ShortInt = (
           5,  2,    2,  5,
          -5,  2,   -2,  5,
           5, -2,    2, -5,
          -5, -2,   -2, -5
  );

  //Gradients for 3D. They approximate the directions to the
  //vertices of a rhombicuboctahedron from the center, skewed so
  //that the triangular and square facets can be inscribed inside
  //circles of the same radius.
  cOSN_gradients3D : Array[0..71] of ShortInt = (
          -11,  4,  4,     -4,  11,  4,    -4,  4,  11,
           11,  4,  4,      4,  11,  4,     4,  4,  11,
          -11, -4,  4,     -4, -11,  4,    -4, -4,  11,
           11, -4,  4,      4, -11,  4,     4, -4,  11,
          -11,  4, -4,     -4,  11, -4,    -4,  4, -11,
           11,  4, -4,      4,  11, -4,     4,  4, -11,
          -11, -4, -4,     -4, -11, -4,    -4, -4, -11,
           11, -4, -4,      4, -11, -4,     4, -4, -11
  );

  //Gradients for 4D. They approximate the directions to the
  //vertices of a disprismatotesseractihexadecachoron from the center,
  //skewed so that the tetrahedral and cubic facets can be inscribed inside
  //spheres of the same radius.
  cOSN_gradients4D : Array[0..255] of ShortInt = (
           3,  1,  1,  1,      1,  3,  1,  1,      1,  1,  3,  1,      1,  1,  1,  3,
          -3,  1,  1,  1,     -1,  3,  1,  1,     -1,  1,  3,  1,     -1,  1,  1,  3,
           3, -1,  1,  1,      1, -3,  1,  1,      1, -1,  3,  1,      1, -1,  1,  3,
          -3, -1,  1,  1,     -1, -3,  1,  1,     -1, -1,  3,  1,     -1, -1,  1,  3,
           3,  1, -1,  1,      1,  3, -1,  1,      1,  1, -3,  1,      1,  1, -1,  3,
          -3,  1, -1,  1,     -1,  3, -1,  1,     -1,  1, -3,  1,     -1,  1, -1,  3,
           3, -1, -1,  1,      1, -3, -1,  1,      1, -1, -3,  1,      1, -1, -1,  3,
          -3, -1, -1,  1,     -1, -3, -1,  1,     -1, -1, -3,  1,     -1, -1, -1,  3,
           3,  1,  1, -1,      1,  3,  1, -1,      1,  1,  3, -1,      1,  1,  1, -3,
          -3,  1,  1, -1,     -1,  3,  1, -1,     -1,  1,  3, -1,     -1,  1,  1, -3,
           3, -1,  1, -1,      1, -3,  1, -1,      1, -1,  3, -1,      1, -1,  1, -3,
          -3, -1,  1, -1,     -1, -3,  1, -1,     -1, -1,  3, -1,     -1, -1,  1, -3,
           3,  1, -1, -1,      1,  3, -1, -1,      1,  1, -3, -1,      1,  1, -1, -3,
          -3,  1, -1, -1,     -1,  3, -1, -1,     -1,  1, -3, -1,     -1,  1, -1, -3,
           3, -1, -1, -1,      1, -3, -1, -1,      1, -1, -3, -1,      1, -1, -1, -3,
          -3, -1, -1, -1,     -1, -3, -1, -1,     -1, -1, -3, -1,     -1, -1, -1, -3
  );

function CastDoubleToInt64(value : double) : int64;
var
  i : int64 absolute Value;
begin
  result := i;
end;

function CastInt64ToDouble(value : Int64) : Double;
var
  d : double absolute Value;
begin
  result := d;
end;

{%region=====[ TBZOpenSimplexNoiseGenerator ]===================================}

constructor TBZOpenSimplexNoiseGenerator.Create;
Var
  i, r : Integer;
  Source : array [0..cPERLIN_TABLE_SIZE-1] of Integer;
begin
  inherited Create;
  Support := [ns2D, ns3D, ns4D];
  Initialize(Seed);
end;

procedure TBZOpenSimplexNoiseGenerator.Initialize(aSeed : Int64);
Var
  i, r : Integer;
  Source : array [0..cPERLIN_TABLE_SIZE-1] of Integer;
  NewSeed : Int64;
begin

  // Initialize permutations table
  for i:=0 to cPERLIN_TABLE_SIZE-1 do Source[i]:=i;

  NewSeed := Hash1D(aSeed);
  NewSeed := NewSeed * 6364136223846793005 + 1442695040888963407;
  NewSeed := NewSeed * 6364136223846793005 + 1442695040888963407;
  NewSeed := NewSeed * 6364136223846793005 + 1442695040888963407;

  //Since 3D has 24 gradients, simple bitmask won't work, so precompute modulo array.
  for i:=0 to cPERLIN_TABLE_SIZE-1 do
  begin
    NewSeed := NewSeed * 6364136223846793005 + 1442695040888963407;
    r := (NewSeed + 31) mod (i + 1);
    if (r < 0) then r := r + (i + 1);
    FPermutations[i] := Source[r];
    FGradientPermutationsIndex3D[i] := (FPermutations[i] mod 85) * 3; // 85 = 256 / 3
    Source[r] := Source[i];
  end;

end;

function TBZOpenSimplexNoiseGenerator.Extrapolate2D(xsb, ysb : Integer; dx, dy : Double) : Double;
Var
  Index : Integer;
begin
  Index := FPermutations[(FPermutations[xsb and cPERLIN_TABLE_MASK] + ysb) and cPERLIN_TABLE_MASK] and $0E;
  Result := (cOSN_Gradients2D[Index] * dx) + (cOSN_Gradients2D[Index + 1] * dy);
end;

function TBZOpenSimplexNoiseGenerator.Extrapolate3D(xsb, ysb, zsb : Integer; dx, dy, dz : Double) : Double;
Var
  Index : Integer;
begin
  Index := FGradientPermutationsIndex3D[(FPermutations[(FPermutations[xsb and cPERLIN_TABLE_MASK] + ysb) and cPERLIN_TABLE_MASK] + zsb) and cPERLIN_TABLE_MASK];
  Result := (cOSN_Gradients3D[Index] * dx) +
            (cOSN_Gradients3D[Index + 1] * dy) +
            (cOSN_Gradients3D[Index + 2] * dz);
end;

function TBZOpenSimplexNoiseGenerator.Extrapolate4D(xsb, ysb, zsb, wsb : Integer; dx, dy, dz, dw : Double) : Double;
Var
  Index : Integer;
begin
  Index :=  FPermutations[(FPermutations[(FPermutations[(FPermutations[xsb and cPERLIN_TABLE_MASK] + ysb) and cPERLIN_TABLE_MASK] + zsb) and cPERLIN_TABLE_MASK] + wsb) and cPERLIN_TABLE_MASK] and 252;
  Result := (cOSN_Gradients4D[Index] * dx) +
            (cOSN_Gradients4D[Index + 1] * dy) +
            (cOSN_Gradients4D[Index + 2] * dz) +
            (cOSN_Gradients4D[Index + 3] * dw);
end;

//procedure TBZOpenSimplexNoiseGenerator.SetSeed(const AValue : Int64);
//begin
//  inherited SetSeed(AValue);
//  Initialize(Seed);
//end;

function TBZOpenSimplexNoiseGenerator.Noise1D(x : Double) : Double;
begin
  Result := Noise2D(x,0);
end;

function TBZOpenSimplexNoiseGenerator.Noise2D(x, y : Double) : Double;
Var
  StretchOffset, SquishOffset, xins, yins, zins, dx_ext, dy_ext, value, xs, ys : Double;
  xb, yb, dx0, dy0, dx1, dy1, dx2, dy2, inSum, attn0, attn1, attn2, attn_ext : Double;
  xsb, ysb, xsv_ext, ysv_ext : Integer;
begin
  //Place input coordinates onto grid.
  StretchOffset := (x + y) * cOSN_STRETCH_2D;
  xs := x + StretchOffset;
  ys := y + StretchOffset;

  //Floor to get grid coordinates of rhombus (stretched square) super-cell origin.
  xsb := Floor(xs);
  ysb := Floor(ys);

  //Skew out to get actual coordinates of rhombus origin. We'll need these later.
  SquishOffset := (xsb + ysb) * cOSN_SQUISH_2D;
  xb := xsb + SquishOffset;
  yb := ysb + SquishOffset;

  //Compute grid coordinates relative to rhombus origin.
  xins := xs - xsb;
  yins := ys - ysb;

  //Sum those together to get a value that determines which region we're in.
  inSum := xins + yins;

  //Positions relative to origin point.
  dx0 := x - xb;
  dy0 := y - yb;

  value := 0.0;

  //Contribution (1,0)
  dx1 := dx0 - 1 - cOSN_SQUISH_2D; //1 - cOSN_SQUISH_2D; //
  dy1 := dy0 - 0 - cOSN_SQUISH_2D;
  attn1 := 2.0 - dx1 * dx1 - dy1 * dy1;
  if (attn1 > 0) then
  begin
    attn1 := attn1 * attn1;
    value := Value + (attn1 * attn1 * Extrapolate2D(xsb + 1, ysb, dx1, dy1));
  end;

  //Contribution (0,1)
  dx2 := dx0 - 0 - cOSN_SQUISH_2D;
  dy2 := dy0 - 1 - cOSN_SQUISH_2D;//1 - cOSN_SQUISH_2D; //
  attn2 := 2.0 - dx2 * dx2 - dy2 * dy2;
  if (attn2 > 0) then
  begin
    attn2 := attn2 * attn2;
    value := value + (attn2 * attn2 * Extrapolate2D(xsb, ysb + 1, dx2, dy2));
  end;

  if (inSum <= 1.0) then //We're inside the triangle (2-Simplex) at (0,0)
  begin
    zins := 1.0 - inSum;
    if (zins > xins) or (zins > yins) then  //(0,0) is one of the closest two triangular vertices
    begin
      if (xins > yins) then
      begin
        xsv_ext := xsb + 1;
        ysv_ext := ysb - 1;
        dx_ext  := dx0 - 1;
        dy_ext  := dy0 + 1;
      end
      else
      begin
        xsv_ext := xsb - 1;
        ysv_ext := ysb + 1;
        dx_ext  := dx0 + 1;
        dy_ext  := dy0 - 1;
      end;
    end
    else  //(1,0) and (0,1) are the closest two vertices.
    begin
      xsv_ext := xsb + 1;
      ysv_ext := ysb + 1;
      dx_ext  := dx0 - 1 - 2 * cOSN_SQUISH_2D;
      dy_ext  := dy0 - 1 - 2 * cOSN_SQUISH_2D;
    end;
  end
  else  //We're inside the triangle (2-Simplex) at (1,1)
  begin
    zins := 2.0 - inSum;
    if (zins < xins) or (zins < yins) then //(0,0) is one of the closest two triangular vertices
    begin
      if (xins > yins) then
      begin
        xsv_ext := xsb + 2;
        ysv_ext := ysb;
        dx_ext  := dx0 - 2 - 2 * cOSN_SQUISH_2D;
        dy_ext  := dy0 - 2 * cOSN_SQUISH_2D;
      end
      else
      begin
        xsv_ext := xsb;
        ysv_ext := ysb + 2;
        dx_ext  := dx0 - 2 * cOSN_SQUISH_2D;
        dy_ext  := dy0 - 2 - 2 * cOSN_SQUISH_2D;
      end;
    end
    else  //(1,0) and (0,1) are the closest two vertices.
    begin
      dx_ext  := dx0;
      dy_ext  := dy0;
      xsv_ext := xsb;
      ysv_ext := ysb;
    end;

    xsb := xsb + 1;
    ysb := ysb + 1;
    dx0 := dx0 - 1 - 2 * cOSN_SQUISH_2D;
    dy0 := dy0 - 1 - 2 * cOSN_SQUISH_2D;
  end;

  //Contribution (0,0) or (1,1)
  attn0 := 2.0 - dx0 * dx0 - dy0 * dy0;
  if (attn0 > 0) then
  begin
    attn0 := attn0 * attn0;
    value := value + (attn0 * attn0 * Extrapolate2D(xsb, ysb, dx0, dy0));
  end;

  //Extra Vertex
  attn_ext := 2.0 - dx_ext * dx_ext - dy_ext * dy_ext;
  if (attn_ext > 0) then
  begin
    attn_ext := attn_ext * attn_ext;
    value := value + (attn_ext * attn_ext * Extrapolate2D(xsv_ext, ysv_ext, dx_ext, dy_ext));
  end;

  Result := value * cOSN_NORM_2D;

end;

function TBZOpenSimplexNoiseGenerator.Noise3D(x, y, z : Double) : Double;
Var
  StretchOffset, SquishOffset,
  xins, yins, zins,
  dx_ext0, dy_ext0, dz_ext0,
  dx_ext1, dy_ext1, dz_ext1,
  value, xs, ys, zs : Double;
  xb, yb, zb,
  dx0, dy0, dz0,
  dx1, dy1, dz1,
  dx2, dy2, dz2,
  dx3, dy3, dz3,
  dx4, dy4, dz4,
  dx5, dy5, dz5,
  dx6, dy6, dz6,
  inSum,
  aScore, bScore, wins, score, p1, p2, p3,
  attn0, attn1, attn2, attn3, attn4, attn5, attn6,
  attn_ext0, attn_ext1 : Double;
  aPoint, bPoint,
  c, c1, c2 : Byte;
  aIsFurtherSide, bIsFurtherSide : Boolean;
  xsb, ysb, zsb,
  xsv_ext0, ysv_ext0, zsv_ext0,
  xsv_ext1, ysv_ext1, zsv_ext1 : Integer;

begin
	// Place input coordinates on simplectic honeycomb.
	StretchOffset := (x + y + z) * cOSN_STRETCH_3D;
	xs := x + StretchOffset;
	ys := y + StretchOffset;
	zs := z + StretchOffset;

	// Floor to get simplectic honeycomb coordinates of rhombohedron (stretched cube) super-cell origin.
	xsb := Floor(xs);
	ysb := Floor(ys);
	zsb := Floor(zs);

	// Skew out to get actual coordinates of rhombohedron origin. We'll need these later.
	SquishOffset := (xsb + ysb + zsb) * cOSN_SQUISH_3D;
	xb := xsb + SquishOffset;
	yb := ysb + SquishOffset;
	zb := zsb + SquishOffset;

	// Compute simplectic honeycomb coordinates relative to rhombohedral origin.
	xins := xs - xsb;
	yins := ys - ysb;
	zins := zs - zsb;

	// Sum those together to get a value that determines which region we're in.
	inSum := xins + yins + zins;

	// Positions relative to origin point.
	dx0 := x - xb;
	dy0 := y - yb;
	dz0 := z - zb;

  value := 0.0;
 	if (inSum <= 1) then // We're inside the tetrahedron (3-Simplex) at (0,0,0)
  begin
 		// Determine which two of (0,0,1), (0,1,0), (1,0,0) are closest. */
 		aPoint := $01;
 		aScore := xins;
 		bPoint := $02;
 		bScore := yins;
 		if (aScore >= bScore) and (zins > bScore) then
    begin
 			bScore := zins;
 			bPoint := $04;
    end
    else if (aScore < bScore) and (zins > aScore) then
    begin
 			aScore := zins;
 			aPoint := $04;
    end;

		// Now we determine the two lattice points not part of the tetrahedron that may contribute.
		// This depends on the closest two tetrahedral vertices, including (0,0,0)
		wins := 1.0 - inSum;
		if (wins > aScore) or (wins > bScore) then // (0,0,0) is one of the closest two tetrahedral vertices.
    begin
      if (bScore > aScore) then c := bPoint else c := aPoint;
			if ((c and $01) = 0) then
      begin
				xsv_ext0 := xsb - 1;
				xsv_ext1 := xsb;
				dx_ext0  := dx0 + 1;
				dx_ext1  := dx0;
      end
      else
      begin
				xsv_ext0 := xsb + 1;
        xsv_ext1 := xsv_ext0;
				dx_ext0  := dx0 - 1;
        dx_ext1  := dx_ext0;
      end;

			if ((c and $02) = 0) then
      begin
				ysv_ext0 := ysb;
        ysv_ext1 := ysb;
				dy_ext0  := dy0;
        dy_ext1  := dy0;
				if ((c and $01) = 0) then
        begin
					ysv_ext1 := ysv_ext1 - 1;
					dy_ext1  := dy_ext1 + 1;
        end
        else
        begin
			    ysv_ext0 := ysv_ext0 - 1;
				  dy_ext0  := dy_ext0 + 1;
			  end;
		  end
      else
      begin
		    ysv_ext0 := ysb + 1;
        ysv_ext1 := ysv_ext0;
			  dy_ext0  := dy0 - 1;
        dy_ext1  := dy_ext0;
		  end;

		  if ((c and $04) = 0) then
      begin
		    zsv_ext0 := zsb;
			  zsv_ext1 := zsb - 1;
			  dz_ext0  := dz0;
			  dz_ext1  := dz0 + 1;
		  end
      else
      begin
		    zsv_ext0 := zsb + 1;
        zsv_ext1 := zsv_ext0;
			  dz_ext0  := dz0 - 1;
        dz_ext1  := dz_ext0;
		  end;

    end
    else  // (0,0,0) is not one of the closest two tetrahedral vertices.
    begin
		  c := (aPoint or bPoint); // Our two extra vertices are determined by the closest two.

		  if ((c and $01) = 0) then
      begin
		    xsv_ext0 := xsb;
			  xsv_ext1 := xsb - 1;
			  dx_ext0 := dx0 - 2 * cOSN_SQUISH_3D;
			  dx_ext1 := dx0 + 1 - cOSN_SQUISH_3D;
		  end
      else
      begin
		    xsv_ext0 := xsb + 1;
        xsv_ext1 := xsv_ext0;
			  dx_ext0 := dx0 - 1 - 2 * cOSN_SQUISH_3D;
			  dx_ext1 := dx0 - 1 - cOSN_SQUISH_3D;
		  end;

		  if ((c and $02) = 0) then
      begin
		    ysv_ext0 := ysb;
			  ysv_ext1 := ysb - 1;
			  dy_ext0  := dy0 - 2 * cOSN_SQUISH_3D;
			  dy_ext1  := dy0 + 1 - cOSN_SQUISH_3D;
		  end
      else
      begin
		    ysv_ext0 :=  ysb + 1;
        ysv_ext1 := ysv_ext0;
			  dy_ext0  := dy0 - 1 - 2 * cOSN_SQUISH_3D;
			  dy_ext1  := dy0 - 1 - cOSN_SQUISH_3D;
		  end;

		  if ((c and $04) = 0) then
      begin
		    zsv_ext0 := zsb;
			  zsv_ext1 := zsb - 1;
			  dz_ext0  := dz0 - 2 * cOSN_SQUISH_3D;
			  dz_ext1  := dz0 + 1 - cOSN_SQUISH_3D;
		  end
      else
      begin
		    zsv_ext0 := zsb + 1;
        zsv_ext1 := zsv_ext0;
			  dz_ext0  := dz0 - 1 - 2 * cOSN_SQUISH_3D;
			  dz_ext1  := dz0 - 1 - cOSN_SQUISH_3D;
		  end;
    end;

    //Contribution (0,0,0)
    attn0 := 2 - dx0 * dx0 - dy0 * dy0 - dz0 * dz0;
    if (attn0 > 0) then
    begin
      attn0 := attn0 * attn0;
      value := value + (attn0 * attn0 * Extrapolate3D(xsb, ysb, zsb, dx0, dy0, dz0));
    end;

		// Contribution (1,0,0)
		dx1 := dx0 - 1 - cOSN_SQUISH_3D;
		dy1 := dy0 - 0 - cOSN_SQUISH_3D;
		dz1 := dz0 - 0 - cOSN_SQUISH_3D;
		attn1 := 2 - dx1 * dx1 - dy1 * dy1 - dz1 * dz1;
		if (attn1 > 0) then
    begin
			attn1 := attn1 * attn1;
			value := value + (attn1 * attn1 * Extrapolate3D(xsb + 1, ysb, zsb, dx1, dy1, dz1));
		end;

		// Contribution (0,1,0)
		dx2 := dx0 - 0 - cOSN_SQUISH_3D;
		dy2 := dy0 - 1 - cOSN_SQUISH_3D;
		dz2 := dz1;
		attn2 := 2 - dx2 * dx2 - dy2 * dy2 - dz2 * dz2;
		if (attn2 > 0) then
    begin
			attn2 := attn2 * attn2;
			value := value + (attn2 * attn2 * Extrapolate3D(xsb, ysb + 1, zsb, dx2, dy2, dz2));
		end;

		// Contribution (0,0,1)
		dx3 := dx2;
		dy3 := dy1;
		dz3 := dz0 - 1 - cOSN_SQUISH_3D;
		attn3 := 2 - dx3 * dx3 - dy3 * dy3 - dz3 * dz3;
		if (attn3 > 0) then
    begin
			attn3 := attn3 * attn3;
			value := value + (attn3 * attn3 * Extrapolate3D(xsb, ysb, zsb + 1, dx3, dy3, dz3));
    end;

  end
  else if (inSum >= 2) then // We're inside the tetrahedron (3-Simplex) at (1,1,1)
  begin
   	// Determine which two tetrahedral vertices are the closest, out of (1,1,0), (1,0,1), (0,1,1) but not (1,1,1).
   	aPoint := $06;
   	aScore := xins;
   	bPoint := $05;
   	bScore := yins;
   	if (aScore <= bScore) and (zins < bScore) then
    begin
   		bScore := zins;
   		bPoint := $03;
    end
    else if (aScore > bScore) and (zins < aScore) then
    begin
   		aScore := zins;
   		aPoint := $03;
   	end;

   	// Now we determine the two lattice points not part of the tetrahedron that may contribute.
   	//   This depends on the closest two tetrahedral vertices, including (1,1,1)
   	wins := 3 - inSum;
   	if (wins < aScore) or (wins < bScore) then // (1,1,1) is one of the closest two tetrahedral vertices. */
    begin
      if (bScore < aScore) then c := bPoint else c := aPoint; // Our other closest vertex is the closest out of a and b. */

   		if ((c and $01) <> 0) then
      begin
   			xsv_ext0 := xsb + 2;
   			xsv_ext1 := xsb + 1;
   			dx_ext0  := dx0 - 2 - 3 * cOSN_SQUISH_3D;
   			dx_ext1  := dx0 - 1 - 3 * cOSN_SQUISH_3D;
      end
      else
      begin
   			xsv_ext0 := xsb;
        xsv_ext1 := xsb;
   			dx_ext0  := dx0 - 3 * cOSN_SQUISH_3D;
        dx_ext1  := dx_ext0;
   		end;

   		if ((c and $02) <> 0) then
      begin
   			ysv_ext0 := ysb + 1;
        ysv_ext1 := ysv_ext0;
   			dy_ext0  := dy0 - 1 - 3 * cOSN_SQUISH_3D;
        dy_ext1  := dy_ext0;

   			if ((c and $01) <> 0) then
        begin
   				ysv_ext1 := ysv_ext1 + 1;
   				dy_ext1  := dy_ext1 - 1;
        end
        else
        begin
   				ysv_ext0 := ysv_ext0 + 1;
   				dy_ext0 := dy_ext0 - 1;
        end;
   		end
      else
      begin
   			ysv_ext0 := ysb;
        ysv_ext1 := ysb;
   			dy_ext0  := dy0 - 3 * cOSN_SQUISH_3D;
        dy_ext1  := dy_ext0;
   		end;

   		if ((c and $04) <> 0) then
      begin
   			zsv_ext0 := zsb + 1;
   			zsv_ext1 := zsb + 2;
   			dz_ext0  := dz0 - 1 - 3 * cOSN_SQUISH_3D;
   			dz_ext1  := dz0 - 2 - 3 * cOSN_SQUISH_3D;
      end
      else
      begin
   			zsv_ext0 := zsb;
        zsv_ext1 := zsb;
   			dz_ext0  := dz0 - 3 * cOSN_SQUISH_3D;
        dz_ext1  := dz_ext0;
      end;
    end
   	else  // (1,1,1) is not one of the closest two tetrahedral vertices.
    begin
   		c := (aPoint and bPoint); // Our two extra vertices are determined by the closest two. */

   		if ((c and $01) <> 0) then
      begin
   			xsv_ext0 := xsb + 1;
   			xsv_ext1 := xsb + 2;
   			dx_ext0  := dx0 - 1 - cOSN_SQUISH_3D;
   			dx_ext1  := dx0 - 2 - 2 * cOSN_SQUISH_3D;
      end
      else
      begin
   			xsv_ext0 := xsb;
        xsv_ext1 := xsb;
   			dx_ext0  := dx0 - cOSN_SQUISH_3D;
   			dx_ext1  := dx0 - 2 * cOSN_SQUISH_3D;
   		end;

   		if ((c and $02) <> 0) then
      begin
   			ysv_ext0 := ysb + 1;
   			ysv_ext1 := ysb + 2;
   			dy_ext0  := dy0 - 1 - cOSN_SQUISH_3D;
   			dy_ext1  := dy0 - 2 - 2 * cOSN_SQUISH_3D;
      end
      else
      begin
   			ysv_ext0 := ysb;
        ysv_ext1 := ysb;
   			dy_ext0  := dy0 - cOSN_SQUISH_3D;
   			dy_ext1  := dy0 - 2 * cOSN_SQUISH_3D;
   		end;

   		if ((c and $04) <> 0) then
      begin
   			zsv_ext0 := zsb + 1;
   			zsv_ext1 := zsb + 2;
   			dz_ext0  := dz0 - 1 - cOSN_SQUISH_3D;
   			dz_ext1  := dz0 - 2 - 2 * cOSN_SQUISH_3D;
      end
      else
      begin
   			zsv_ext0 := zsb;
        zsv_ext1 := zsb;
   			dz_ext0  := dz0 - cOSN_SQUISH_3D;
   			dz_ext1  := dz0 - 2 * cOSN_SQUISH_3D;
   		end;
   	end;

   	// Contribution (1,1,0)
   	dx3 := dx0 - 1 - 2 * cOSN_SQUISH_3D;
   	dy3 := dy0 - 1 - 2 * cOSN_SQUISH_3D;
   	dz3 := dz0 - 0 - 2 * cOSN_SQUISH_3D;
   	attn3 := 2 - dx3 * dx3 - dy3 * dy3 - dz3 * dz3;
   	if (attn3 > 0) then
    begin
   		attn3 := attn3 * attn3;
   		value := value + (attn3 * attn3 * Extrapolate3D(xsb + 1, ysb + 1, zsb, dx3, dy3, dz3));
   	end;

   	// Contribution (1,0,1)
   	dx2 := dx3;
   	dy2 := dy0 - 0 - 2 * cOSN_SQUISH_3D;
   	dz2 := dz0 - 1 - 2 * cOSN_SQUISH_3D;
   	attn2 := 2 - dx2 * dx2 - dy2 * dy2 - dz2 * dz2;
   	if (attn2 > 0) then
    begin
   		attn2 := attn2 * attn2;
   		value := value + (attn2 * attn2 * Extrapolate3D(xsb + 1, ysb, zsb + 1, dx2, dy2, dz2));
   	end;

   	// Contribution (0,1,1)
   	dx1 := dx0 - 0 - 2 * cOSN_SQUISH_3D;
   	dy1 := dy3;
   	dz1 := dz2;
   	attn1 := 2 - dx1 * dx1 - dy1 * dy1 - dz1 * dz1;
   	if (attn1 > 0) then
    begin
   		attn1 := attn1 * attn1;
   		value := value + (attn1 * attn1 * Extrapolate3D(xsb, ysb + 1, zsb + 1, dx1, dy1, dz1));
   	end;

   	// Contribution (1,1,1)
   	dx0 := dx0 - 1 - 3 * cOSN_SQUISH_3D;
   	dy0 := dy0 - 1 - 3 * cOSN_SQUISH_3D;
   	dz0 := dz0 - 1 - 3 * cOSN_SQUISH_3D;
   	attn0 := 2 - dx0 * dx0 - dy0 * dy0 - dz0 * dz0;
   	if (attn0 > 0) then
    begin
   		attn0 := attn0 * attn0;
   		value := value + (attn0 * attn0 * Extrapolate3D(xsb + 1, ysb + 1, zsb + 1, dx0, dy0, dz0));
    end;
  end
  else
  begin // We're inside the octahedron (Rectified 3-Simplex) in between.

    //Decide between point (0,0,1) and (1,1,0) as closest
    p1 := xins + yins;
    if (p1 > 1) then
    begin
    	aScore := p1 - 1;
    	aPoint := $03;
    	aIsFurtherSide := True;
    end
    else
    begin
    	aScore := 1 - p1;
    	aPoint := $04;
    	aIsFurtherSide := False;
    end;

    // Decide between point (0,1,0) and (1,0,1) as closest
    p2 := xins + zins;
    if (p2 > 1) then
    begin
    	bScore := p2 - 1;
    	bPoint := $05;
    	bIsFurtherSide := True;
    end
    else
    begin
    	bScore := 1 - p2;
    	bPoint := $02;
    	bIsFurtherSide := False;
    end;

    // The closest out of the two (1,0,0) and (0,1,1) will replace the furthest out of the two decided above, if closer.
    p3 := yins + zins;
    if (p3 > 1) then
    begin
    	score := p3 - 1;
    	if (aScore <= bScore) and (aScore < score) then
      begin
    		aScore := score;
    		aPoint := $06;
    		aIsFurtherSide := True;
    	end
      else if (aScore > bScore) and (bScore < score) then
      begin
    		bScore := score;
    		bPoint := $06;
    		bIsFurtherSide := True;
    	end;
    end
    else
    begin
    	score := 1 - p3;
    	if (aScore <= bScore) and (aScore < score) then
      begin
    		aScore := score;
    		aPoint := $01;
    		aIsFurtherSide := False;
    	end
      else if (aScore > bScore) and (bScore < score) then
      begin
    		bScore := score;
    		bPoint := $01;
    		bIsFurtherSide := False;
    	end;
    end;

    // Where each of the two closest points are determines how the extra two vertices are calculated. */
    if (aIsFurtherSide = bIsFurtherSide) then
    begin
    	if (aIsFurtherSide) then // Both closest points on (1,1,1) side
      begin
    		// One of the two extra points is (1,1,1)
    		dx_ext0 := dx0 - 1 - 3 * cOSN_SQUISH_3D;
    		dy_ext0 := dy0 - 1 - 3 * cOSN_SQUISH_3D;
    		dz_ext0 := dz0 - 1 - 3 * cOSN_SQUISH_3D;
    		xsv_ext0 := xsb + 1;
    		ysv_ext0 := ysb + 1;
    		zsv_ext0 := zsb + 1;

    		// Other extra point is based on the shared axis.
    		c := (aPoint and bPoint);
    		if ((c and $01) <> 0) then
        begin
    			dx_ext1 := dx0 - 2 - 2 * cOSN_SQUISH_3D;
    			dy_ext1 := dy0 - 2 * cOSN_SQUISH_3D;
    			dz_ext1 := dz0 - 2 * cOSN_SQUISH_3D;
    			xsv_ext1 := xsb + 2;
    			ysv_ext1 := ysb;
    			zsv_ext1 := zsb;
    		end
        else if ((c and $02) <> 0) then
        begin
    			dx_ext1 := dx0 - 2 * cOSN_SQUISH_3D;
    			dy_ext1 := dy0 - 2 - 2 * cOSN_SQUISH_3D;
    			dz_ext1 := dz0 - 2 * cOSN_SQUISH_3D;
    			xsv_ext1 := xsb;
    			ysv_ext1 := ysb + 2;
    			zsv_ext1 := zsb;
    		end
        else
        begin
    			dx_ext1 := dx0 - 2 * cOSN_SQUISH_3D;
    			dy_ext1 := dy0 - 2 * cOSN_SQUISH_3D;
    			dz_ext1 := dz0 - 2 - 2 * cOSN_SQUISH_3D;
    			xsv_ext1 := xsb;
    			ysv_ext1 := ysb;
    			zsv_ext1 := zsb + 2;
    		end;
    	end
      else  // Both closest points on (0,0,0) side
      begin
    		// One of the two extra points is (0,0,0)
    		dx_ext0 := dx0;
    		dy_ext0 := dy0;
    		dz_ext0 := dz0;
    		xsv_ext0 := xsb;
    		ysv_ext0 := ysb;
    		zsv_ext0 := zsb;

    		// Other extra point is based on the omitted axis. */
    		c := (aPoint or bPoint);
    		if ((c and $01) = 0) then
        begin
    			dx_ext1 := dx0 + 1 - cOSN_SQUISH_3D;
    			dy_ext1 := dy0 - 1 - cOSN_SQUISH_3D;
    			dz_ext1 := dz0 - 1 - cOSN_SQUISH_3D;
    			xsv_ext1 := xsb - 1;
    			ysv_ext1 := ysb + 1;
    			zsv_ext1 := zsb + 1;
    		end
        else if ((c and $02) = 0) then
        begin
    			dx_ext1 := dx0 - 1 - cOSN_SQUISH_3D;
    			dy_ext1 := dy0 + 1 - cOSN_SQUISH_3D;
    			dz_ext1 := dz0 - 1 - cOSN_SQUISH_3D;
    			xsv_ext1 := xsb + 1;
    			ysv_ext1 := ysb - 1;
    			zsv_ext1 := zsb + 1;
    		end
        else
        begin
    			dx_ext1 := dx0 - 1 - cOSN_SQUISH_3D;
    			dy_ext1 := dy0 - 1 - cOSN_SQUISH_3D;
    			dz_ext1 := dz0 + 1 - cOSN_SQUISH_3D;
    			xsv_ext1 := xsb + 1;
    			ysv_ext1 := ysb + 1;
    			zsv_ext1 := zsb - 1;
    		end;
    	end;
    end
    else  // One point on (0,0,0) side, one point on (1,1,1) side
    begin
    	if (aIsFurtherSide) then
      begin
    		c1 := aPoint;
    		c2 := bPoint;
    	end
      else
      begin
    		c1 := bPoint;
    		c2 := aPoint;
    	end;

    	// One contribution is a permutation of (1,1,-1)
    	if ((c1 and $01) = 0) then
      begin
    		dx_ext0 := dx0 + 1 - cOSN_SQUISH_3D;
    		dy_ext0 := dy0 - 1 - cOSN_SQUISH_3D;
    		dz_ext0 := dz0 - 1 - cOSN_SQUISH_3D;
    		xsv_ext0 := xsb - 1;
    		ysv_ext0 := ysb + 1;
    		zsv_ext0 := zsb + 1;
    	end
      else if ((c1 and $02) = 0) then
      begin
    		dx_ext0 := dx0 - 1 - cOSN_SQUISH_3D;
    		dy_ext0 := dy0 + 1 - cOSN_SQUISH_3D;
    		dz_ext0 := dz0 - 1 - cOSN_SQUISH_3D;
    		xsv_ext0 := xsb + 1;
    		ysv_ext0 := ysb - 1;
    		zsv_ext0 := zsb + 1;
    	end
      else
      begin
    		dx_ext0 := dx0 - 1 - cOSN_SQUISH_3D;
    		dy_ext0 := dy0 - 1 - cOSN_SQUISH_3D;
    		dz_ext0 := dz0 + 1 - cOSN_SQUISH_3D;
    		xsv_ext0 := xsb + 1;
    		ysv_ext0 := ysb + 1;
    		zsv_ext0 := zsb - 1;
    	end;

    	// One contribution is a permutation of (0,0,2)
    	dx_ext1 := dx0 - 2 * cOSN_SQUISH_3D;
    	dy_ext1 := dy0 - 2 * cOSN_SQUISH_3D;
    	dz_ext1 := dz0 - 2 * cOSN_SQUISH_3D;
    	xsv_ext1 := xsb;
    	ysv_ext1 := ysb;
    	zsv_ext1 := zsb;
    	if ((c2 and $01) <> 0) then
      begin
    		dx_ext1 := dx_ext1 - 2;
    		xsv_ext1 := xsv_ext1 + 2;
    	end
      else if ((c2 and $02) <> 0) then
      begin
    		dy_ext1 := dy_ext1 - 2;
    		ysv_ext1 := ysv_ext1 + 2;
    	end
      else
      begin
    		dz_ext1 := dz_ext1 - 2;
    		zsv_ext1 := zsv_ext1 + 2;
    	end;
    end;

    // Contribution (1,0,0)
    dx1 := dx0 - 1 - cOSN_SQUISH_3D;
    dy1 := dy0 - 0 - cOSN_SQUISH_3D;
    dz1 := dz0 - 0 - cOSN_SQUISH_3D;
    attn1 := 2 - dx1 * dx1 - dy1 * dy1 - dz1 * dz1;
    if (attn1 > 0) then
    begin
    	attn1 := attn1 * attn1;
    	value := value + (attn1 * attn1 * Extrapolate3D(xsb + 1, ysb, zsb, dx1, dy1, dz1));
    end;

    // Contribution (0,1,0)
    dx2 := dx0 - 0 - cOSN_SQUISH_3D;
    dy2 := dy0 - 1 - cOSN_SQUISH_3D;
    dz2 := dz1;
    attn2 := 2 - dx2 * dx2 - dy2 * dy2 - dz2 * dz2;
    if (attn2 > 0) then
    begin
    	attn2 := attn2 * attn2;
    	value := value + (attn2 * attn2 * Extrapolate3D(xsb, ysb + 1, zsb, dx2, dy2, dz2));
    end;

    // Contribution (0,0,1)
    dx3 := dx2;
    dy3 := dy1;
    dz3 := dz0 - 1 - cOSN_SQUISH_3D;
    attn3 := 2 - dx3 * dx3 - dy3 * dy3 - dz3 * dz3;
    if (attn3 > 0) then
    begin
    	attn3 := attn3 * attn3;
    	value := value + (attn3 * attn3 * Extrapolate3D(xsb, ysb, zsb + 1, dx3, dy3, dz3));
    end;

    // Contribution (1,1,0)
    dx4 := dx0 - 1 - 2 * cOSN_SQUISH_3D;
    dy4 := dy0 - 1 - 2 * cOSN_SQUISH_3D;
    dz4 := dz0 - 0 - 2 * cOSN_SQUISH_3D;
    attn4 := 2 - dx4 * dx4 - dy4 * dy4 - dz4 * dz4;
    if (attn4 > 0) then
    begin
    	attn4 := attn4 * attn4;
    	value := value + (attn4 * attn4 * Extrapolate3D(xsb + 1, ysb + 1, zsb, dx4, dy4, dz4));
    end;

    // Contribution (1,0,1)
    dx5 := dx4;
    dy5 := dy0 - 0 - 2 * cOSN_SQUISH_3D;
    dz5 := dz0 - 1 - 2 * cOSN_SQUISH_3D;
    attn5 := 2 - dx5 * dx5 - dy5 * dy5 - dz5 * dz5;
    if (attn5 > 0) then
    begin
    	attn5 := attn5 * attn5;
    	value := value + (attn5 * attn5 * Extrapolate3D(xsb + 1, ysb, zsb + 1, dx5, dy5, dz5));
    end;

    // Contribution (0,1,1)
    dx6 := dx0 - 0 - 2 * cOSN_SQUISH_3D;
    dy6 := dy4;
    dz6 := dz5;
    attn6 := 2 - dx6 * dx6 - dy6 * dy6 - dz6 * dz6;
    if (attn6 > 0) then
    begin
    	attn6 := attn6 * attn6;
    	value := value + (attn6 * attn6 * Extrapolate3D(xsb, ysb + 1, zsb + 1, dx6, dy6, dz6));
    end;
  end;

  // First extra vertex
  attn_ext0 := 2 - dx_ext0 * dx_ext0 - dy_ext0 * dy_ext0 - dz_ext0 * dz_ext0;
  if (attn_ext0 > 0) then
  begin
	  attn_ext0 := attn_ext0 * attn_ext0;
	  value := value + (attn_ext0 * attn_ext0 * Extrapolate3D(xsv_ext0, ysv_ext0, zsv_ext0, dx_ext0, dy_ext0, dz_ext0));
  end;

  // Second extra vertex
  attn_ext1 := 2 - dx_ext1 * dx_ext1 - dy_ext1 * dy_ext1 - dz_ext1 * dz_ext1;
  if (attn_ext1 > 0) then
  begin
	  attn_ext1 := attn_ext1 * attn_ext1;
	  value := value + (attn_ext1 * attn_ext1 * Extrapolate3D(xsv_ext1, ysv_ext1, zsv_ext1, dx_ext1, dy_ext1, dz_ext1));
  end;

  Result := value * cOSN_NORM_3D;
end;

function TBZOpenSimplexNoiseGenerator.Noise4D(x, y, z, w : Double) : Double;
Var
  StretchOffset, SquishOffset,
  xins, yins, zins, wins,
  dx_ext0, dy_ext0, dz_ext0, dw_ext0,
  dx_ext1, dy_ext1, dz_ext1, dw_ext1,
  dx_ext2, dy_ext2, dz_ext2, dw_ext2,
  value, xs, ys, zs, ws : Double;
  xb, yb, zb, wb,
  dx0, dy0, dz0, dw0,
  dx1, dy1, dz1, dw1,
  dx2, dy2, dz2, dw2,
  dx3, dy3, dz3, dw3,
  dx4, dy4, dz4, dw4,
  dx5, dy5, dz5, dw5,
  dx6, dy6, dz6, dw6,
  dx7, dy7, dz7, dw7,
  dx8, dy8, dz8, dw8,
  dx9, dy9, dz9, dw9,
  dx10, dy10, dz10, dw10,
  inSum,
  aScore, bScore, score, p1, p2, p3, p4,
  attn0, attn1, attn2, attn3, attn4, attn5, attn6, attn7, attn8, attn9, attn10,
  attn_ext0, attn_ext1, attn_ext2, uins : Double;
  aPoint, bPoint,
  c, c1, c2 : Byte;
  aIsBiggerSide, bIsBiggerSide : Boolean;
  xsb, ysb, zsb, wsb,
  xsv_ext0, ysv_ext0, zsv_ext0, wsv_ext0,
  xsv_ext1, ysv_ext1, zsv_ext1, wsv_ext1,
  xsv_ext2, ysv_ext2, zsv_ext2, wsv_ext2 : Integer;
begin
	// Place input coordinates on simplectic honeycomb.
	StretchOffset := (x + y + z + w) * cOSN_STRETCH_4D;
	xs := x + StretchOffset;
	ys := y + StretchOffset;
	zs := z + StretchOffset;
	ws := w + StretchOffset;

	// Floor to get simplectic honeycomb coordinates of rhombo-hypercube super-cell origin.
	xsb := Floor(xs);
	ysb := Floor(ys);
	zsb := Floor(zs);
	wsb := Floor(ws);

	// Skew out to get actual coordinates of stretched rhombo-hypercube origin. We'll need these later.
	SquishOffset := (xsb + ysb + zsb + wsb) * cOSN_SQUISH_4D;
	xb := xsb + SquishOffset;
	yb := ysb + SquishOffset;
	zb := zsb + SquishOffset;
	wb := wsb + SquishOffset;

	// Compute simplectic honeycomb coordinates relative to rhombo-hypercube origin.
	xins := xs - xsb;
	yins := ys - ysb;
	zins := zs - zsb;
	wins := ws - wsb;

	// Sum those together to get a value that determines which region we're in.
	inSum := xins + yins + zins + wins;

	// Positions relative to origin point.
	dx0 := x - xb;
	dy0 := y - yb;
	dz0 := z - zb;
	dw0 := w - wb;

	value := 0;

	if (inSum <= 1) then // We're inside the pentachoron (4-Simplex) at (0,0,0,0)
  begin
		// Determine which two of (0,0,0,1), (0,0,1,0), (0,1,0,0), (1,0,0,0) are closest. */
		aPoint := $01;
		aScore := xins;
		bPoint := $02;
		bScore := yins;
		if (aScore >= bScore) and (zins > bScore) then
    begin
			bScore := zins;
			bPoint := $04;
		end
    else if (aScore < bScore) and (zins > aScore) then
    begin
			aScore := zins;
			aPoint := $04;
		end;
		if (aScore >= bScore) and (wins > bScore) then
    begin
			bScore := wins;
			bPoint := $08;
		end
    else if (aScore < bScore) and (wins > aScore) then
    begin
			aScore := wins;
			aPoint := $08;
		end;

		// Now we determine the three lattice points not part of the pentachoron that may contribute.
		// This depends on the closest two pentachoron vertices, including (0,0,0,0)
		uins := 1 - inSum;
		if (uins > aScore) or (uins > bScore) then // (0,0,0,0) is one of the closest two pentachoron vertices.
    begin
      if (bScore > aScore) then	c :=  bPoint else c := aPoint; // Our other closest vertex is the closest out of a and b. */
			if ((c and $01) = 0) then
      begin
				xsv_ext0 := xsb - 1;
				xsv_ext1 := xsb;
        xsv_ext2 := xsb;
				dx_ext0  := dx0 + 1;
				dx_ext1  := dx0;
        dx_ext2  := dx0;
			end
      else
      begin
				xsv_ext0 := xsb + 1;
        xsv_ext1 := xsv_ext0;
        xsv_ext2 := xsv_ext0;
				dx_ext0  := dx0 - 1;
        dx_ext1  := dx_ext0;
        dx_ext2  := dx_ext0;
			end;

			if ((c and $02) = 0) then
      begin
				ysv_ext0 := ysb;
        ysv_ext1 := ysb;
        ysv_ext2 := ysb;
				dy_ext0 := dy0;
        dy_ext1 := dy0;
        dy_ext2 := dy0;
				if ((c and $01) = $01) then
        begin
					ysv_ext0 := ysv_ext0 - 1;
					dy_ext0 := dy_ext0 + 1;
				end
        else
        begin
					ysv_ext1 := ysv_ext1 - 1;
					dy_ext1 := dy_ext1 + 1;
				end;
			end
      else
      begin
				ysv_ext0 := ysb + 1;
        ysv_ext1 := ysv_ext0;
        ysv_ext2 := ysv_ext0;
				dy_ext0 := dy0 - 1;
        dy_ext1 := dy_ext0;
        dy_ext2 := dy_ext0;
			end;

			if ((c and $04) = 0) then
      begin
				zsv_ext0 := zsb;
        zsv_ext1 := zsb;
        zsv_ext2 := zsb;
				dz_ext0  := dz0;
        dz_ext1  := dz0;
        dz_ext2  := dz0;
				if ((c and $03) <> 0) then
        begin
					if ((c and $03) = $03) then
          begin
						zsv_ext0 := zsv_ext0 - 1;
						dz_ext0  := dz_ext0 + 1;
					end
          else
          begin
						zsv_ext1 := zsv_ext1 - 1;
						dz_ext1  := dz_ext1 + 1;
					end;
				end
        else
        begin
					zsv_ext2 := zsv_ext2 - 1;
					dz_ext2 := dz_ext2 + 1;
				end;
			end
      else
      begin
				zsv_ext0 := zsb + 1;
        zsv_ext1 := zsv_ext0;
        zsv_ext2 := zsv_ext0;
				dz_ext0 := dz0 - 1;
        dz_ext1 := dz_ext0;
        dz_ext2 := dz_ext0;
			end;

			if ((c and $08) = 0) then
      begin
				wsv_ext0 := wsb;
        wsv_ext1 := wsb;
				wsv_ext2 := wsb - 1;
				dw_ext0 := dw0;
        dw_ext1 := dw0;
				dw_ext2 := dw0 + 1;
			end
      else
      begin
				wsv_ext0 := wsb + 1;
        wsv_ext1 := wsv_ext0;
        wsv_ext2 := wsv_ext0;
				dw_ext0 := dw0 - 1;
        dw_ext1 := dw_ext0;
        dw_ext2 := dw_ext0;
			end;
		end
    else // (0,0,0,0) is not one of the closest two pentachoron vertices.
    begin
			c := (aPoint or bPoint); // Our three extra vertices are determined by the closest two. */

			if ((c and $01) = 0) then
      begin
				xsv_ext0 := xsb;
        xsv_ext2 := xsb;
				xsv_ext1 := xsb - 1;
				dx_ext0 := dx0 - 2 * cOSN_SQUISH_4D;
				dx_ext1 := dx0 + 1 -  cOSN_SQUISH_4D;
				dx_ext2 := dx0 - cOSN_SQUISH_4D;
			end
      else
      begin
				xsv_ext0 := xsb + 1;
        xsv_ext1 := xsv_ext0;
        xsv_ext2 := xsv_ext0;
				dx_ext0 := dx0 - 1 - 2 * cOSN_SQUISH_4D;
				dx_ext1 := dx0 - 1 -  cOSN_SQUISH_4D;
        dx_ext2 := dx_ext1;
			end;

			if ((c and $02) = 0) then
      begin
				ysv_ext0 := ysb;
        ysv_ext1 := ysb;
        ysv_ext2 := ysb;
				dy_ext0 := dy0 - 2 * cOSN_SQUISH_4D;
				dy_ext1 := dy0 - cOSN_SQUISH_4D;
        dy_ext2 := dy_ext1;
				if ((c and $01) = $01) then
        begin
					ysv_ext1 := ysv_ext1 - 1;
					dy_ext1 := dy_ext1 + 1;
				end
        else
        begin
					ysv_ext2 := ysv_ext2 - 1;
					dy_ext2 := dy_ext2 + 1;
				end;
			end
      else
      begin
				ysv_ext0 := ysb + 1;
        ysv_ext1 := ysv_ext0;
        ysv_ext2 := ysv_ext0;
				dy_ext0 := dy0 - 1 - 2 * cOSN_SQUISH_4D;
				dy_ext1 := dy0 - 1 -  cOSN_SQUISH_4D;
        dy_ext2 := dy_ext1;
			end;

			if ((c and $04) = 0) then
      begin
				zsv_ext0 := zsb;
        zsv_ext1 := zsb;
        zsv_ext2 := zsb;
				dz_ext0 := dz0 - 2 * cOSN_SQUISH_4D;
				dz_ext1 := dz0 - cOSN_SQUISH_4D;
        dz_ext2 := dz_ext1;
				if ((c and $03) = $03) then
        begin
					zsv_ext1 := zsv_ext1 - 1;
					dz_ext1 := dz_ext1 + 1;
				end
        else
        begin
					zsv_ext2 := zsv_ext2 - 1;
					dz_ext2 := dz_ext2 + 1;
				end;
			end
      else
      begin
				zsv_ext0 := zsb + 1;
        zsv_ext1 := zsv_ext0;
        zsv_ext2 := zsv_ext0;
				dz_ext0 := dz0 - 1 - 2 * cOSN_SQUISH_4D;
				dz_ext1 := dz0 - 1 -  cOSN_SQUISH_4D;
        dz_ext2 := dz_ext1;
			end;

			if ((c and $08) = 0) then
      begin
				wsv_ext0 := wsb;
        wsv_ext1 := wsb;
				wsv_ext2 := wsb - 1;
				dw_ext0 := dw0 - 2 * cOSN_SQUISH_4D;
				dw_ext1 := dw0 - cOSN_SQUISH_4D;
				dw_ext2 := dw0 + 1 -  cOSN_SQUISH_4D;
			end
      else
      begin
				wsv_ext0 := wsb + 1;
        wsv_ext1 := wsv_ext0;
        wsv_ext2 := wsv_ext0;
				dw_ext0 := dw0 - 1 - 2 * cOSN_SQUISH_4D;
				dw_ext1 := dw0 - 1 -  cOSN_SQUISH_4D;
        dw_ext2 := dw_ext1;
			end;
		end;

		// Contribution (0,0,0,0)
		attn0 := 2 - dx0 * dx0 - dy0 * dy0 - dz0 * dz0 - dw0 * dw0;
		if (attn0 > 0) then
    begin
			attn0 := attn0 * attn0;
			value := value + (attn0 * attn0 * Extrapolate4D(xsb, ysb, zsb, wsb, dx0, dy0, dz0, dw0));
		end;

		// Contribution (1,0,0,0)
		dx1 := dx0 - 1 - cOSN_SQUISH_4D;
		dy1 := dy0 - 0 - cOSN_SQUISH_4D;
		dz1 := dz0 - 0 - cOSN_SQUISH_4D;
		dw1 := dw0 - 0 - cOSN_SQUISH_4D;
		attn1 := 2 - dx1 * dx1 - dy1 * dy1 - dz1 * dz1 - dw1 * dw1;
		if (attn1 > 0) then
    begin
			attn1 := attn1 * attn1;
			value := value + (attn1 * attn1 * Extrapolate4D(xsb + 1, ysb, zsb, wsb, dx1, dy1, dz1, dw1));
		end;

		// Contribution (0,1,0,0)
		dx2 := dx0 - 0 - cOSN_SQUISH_4D;
		dy2 := dy0 - 1 - cOSN_SQUISH_4D;
		dz2 := dz1;
		dw2 := dw1;
		attn2 := 2 - dx2 * dx2 - dy2 * dy2 - dz2 * dz2 - dw2 * dw2;
		if (attn2 > 0) then
    begin
			attn2 := attn2 * attn2;
			value := value + (attn2 * attn2 * Extrapolate4D(xsb, ysb + 1, zsb, wsb, dx2, dy2, dz2, dw2));
		end;

		// Contribution (0,0,1,0)
		dx3 := dx2;
		dy3 := dy1;
		dz3 := dz0 - 1 - cOSN_SQUISH_4D;
		dw3 := dw1;
		attn3 := 2 - dx3 * dx3 - dy3 * dy3 - dz3 * dz3 - dw3 * dw3;
		if (attn3 > 0) then
    begin
			attn3 := attn3 * attn3;
			value := value + (attn3 * attn3 * Extrapolate4D(xsb, ysb, zsb + 1, wsb, dx3, dy3, dz3, dw3));
		end;

		// Contribution (0,0,0,1)
		dx4 := dx2;
		dy4 := dy1;
		dz4 := dz1;
		dw4 := dw0 - 1 - cOSN_SQUISH_4D;
		attn4 := 2 - dx4 * dx4 - dy4 * dy4 - dz4 * dz4 - dw4 * dw4;
		if (attn4 > 0) then
    begin
			attn4 := attn4 * attn4;
			value := value + (attn4 * attn4 * Extrapolate4D(xsb, ysb, zsb, wsb + 1, dx4, dy4, dz4, dw4));
		end;
	end
  else if (inSum >= 3) then
  begin // We're inside the pentachoron (4-Simplex) at (1,1,1,1)
    //Determine which two of (1,1,1,0), (1,1,0,1), (1,0,1,1), (0,1,1,1) are closest. */
    aPoint := $0E;
    aScore := xins;
    bPoint := $0D;
    bScore := yins;
    if (aScore <= bScore) and (zins < bScore) then
    begin
    	bScore := zins;
    	bPoint := $0B;
    end
    else if (aScore > bScore) and (zins < aScore) then
    begin
    	aScore := zins;
    	aPoint := $0B;
    end;
    if (aScore <= bScore) and (wins < bScore) then
    begin
    	bScore := wins;
    	bPoint := $07;
    end
    else if (aScore > bScore) and (wins < aScore) then
    begin
    	aScore := wins;
    	aPoint := $07;
    end;

    // Now we determine the three lattice points not part of the pentachoron that may contribute.
    // This depends on the closest two pentachoron vertices, including (0,0,0,0)
    uins := 4 - inSum;
    if (uins < aScore) or (uins < bScore) then // (1,1,1,1) is one of the closest two pentachoron vertices.
    begin
      if (bScore < aScore) then	c := bPoint else c := aPoint; // Our other closest vertex is the closest out of a and b.

    	if ((c and $01) <> 0) then
      begin
    		xsv_ext0 := xsb + 2;
    		xsv_ext1 := xsb + 1;
        xsv_ext2 := xsv_ext1;
    		dx_ext0 := dx0 - 2 - 4 * cOSN_SQUISH_4D;
    		dx_ext1 := dx0 - 1 - 4 * cOSN_SQUISH_4D;
        dx_ext2 := dx_ext1;
    	end
      else
      begin
    		xsv_ext0 := xsb;
        xsv_ext1 := xsb;
        xsv_ext2 := xsb;
    		dx_ext0 := dx0 - 4 * cOSN_SQUISH_4D;
        dx_ext1 := dx_ext0;
        dx_ext2 := dx_ext0;
    	end;

    	if ((c and $02) <> 0) then
      begin
    		ysv_ext0 := ysb + 1;
        ysv_ext1 := ysv_ext0;
        ysv_ext2 := ysv_ext0;
    		dy_ext0 := dy0 - 1 - 4 * cOSN_SQUISH_4D;
        dy_ext1 := dy_ext0;
        dy_ext2 := dy_ext0;
    		if ((c and $01) <> 0) then
        begin
    			ysv_ext1 := ysv_ext1 + 1;
    			dy_ext1 := dy_ext1 -1;
    		end
        else
        begin
    			ysv_ext0 := ysv_ext0 + 1;
    			dy_ext0 := dy_ext0 - 1;
    		end;
    	end
      else
      begin
    		ysv_ext0 := ysb;
        ysv_ext1 := ysb;
        ysv_ext2 := ysb;
    		dy_ext0 := dy0 - 4  *  cOSN_SQUISH_4D;
        dy_ext1 := dy_ext0;
        dy_ext2 := dy_ext0;
    	end;

    	if ((c and $04) <> 0) then
      begin
    		zsv_ext0 := zsb + 1;
        zsv_ext1 := zsv_ext0;
        zsv_ext2 := zsv_ext0;
    		dz_ext0 := dz0 - 1 - 4 * cOSN_SQUISH_4D;
        dz_ext1 := dz_ext0;
        dz_ext2 := dz_ext0;
    		if ((c and $03) <> $03) then
        begin
    			if ((c and $03) = 0) then
          begin
    				zsv_ext0 := zsv_ext0 + 1;
    				dz_ext0 := dz_ext0 - 1;
    			end
          else
          begin
    				zsv_ext1 := zsv_ext1 + 1;
    				dz_ext1 := dz_ext1 - 1;
    			end;
    		end
        else
        begin
    			zsv_ext2 := zsv_ext2 + 1;
    			dz_ext2 := dz_ext2 - 1;
    		end;
    	end
      else
      begin
    		zsv_ext0 := zsb;
        zsv_ext1 := zsb;
        zsv_ext2 := zsb;
    		dz_ext0 := dz0 - 4 * cOSN_SQUISH_4D;
        dz_ext1 := dz_ext0;
        dz_ext2 := dz_ext0;
    	end;

    	if ((c and $08) <> 0) then
      begin
    		wsv_ext0 := wsb + 1;
        wsv_ext1 := wsv_ext0;
    		wsv_ext2 := wsb + 2;
    		dw_ext0  := dw0 - 1 - 4 * cOSN_SQUISH_4D;
        dw_ext1  := dw_ext0;
    		dw_ext2  := dw0 - 2 - 4 * cOSN_SQUISH_4D;
    	end
      else
      begin
    		wsv_ext0 := wsb;
        wsv_ext1 := wsb;
        wsv_ext2 := wsb;
    		dw_ext0 :=  dw0 - 4 * cOSN_SQUISH_4D;
        dw_ext1 := dw_ext0;
        dw_ext2 := dw_ext0;
    	end;
    end
    else  // (1,1,1,1) is not one of the closest two pentachoron vertices.
    begin
    	c := (aPoint and bPoint); // Our three extra vertices are determined by the closest two. */

    	if ((c and $01) <> 0) then
      begin
    		xsv_ext0 := xsb + 1;
        xsv_ext2 := xsv_ext0;
    		xsv_ext1 := xsb + 2;
    		dx_ext0 := dx0 - 1 - 3 * cOSN_SQUISH_4D;
    		dx_ext1 := dx0 - 2 - 3 * cOSN_SQUISH_4D;
    		dx_ext2 := dx0 - 1 - 3 * cOSN_SQUISH_4D;
    	end
      else
      begin
    		xsv_ext0 := xsb;
        xsv_ext1 := xsb;
        xsv_ext2 := xsb;
    		dx_ext0 := dx0 - 2 * cOSN_SQUISH_4D;
    		dx_ext1 := dx0 - 3 * cOSN_SQUISH_4D;
        dx_ext2 := dx_ext1;
    	end;

    	if ((c and $02) <> 0) then
      begin
    		ysv_ext0 :=  ysb + 1;
        ysv_ext1 := ysv_ext0;
        ysv_ext2 := ysv_ext0;
    		dy_ext0  := dy0 - 1 - 2 * cOSN_SQUISH_4D;
    		dy_ext1  :=  dy0 - 1 - 3 * cOSN_SQUISH_4D;
        dy_ext2  := dy_ext1;
    		if ((c and $01) <> 0) then
        begin
    			ysv_ext2 := ysv_ext2 + 1;
    			dy_ext2 := dy_ext2 - 1;
    		end
        else
        begin
    			ysv_ext1 := ysv_ext1 + 1;
    			dy_ext1 := dy_ext1 - 1;
    		end;
    	end
      else
      begin
    		ysv_ext0 := ysb;
        ysv_ext1 := ysb;
        ysv_ext2 := ysb;
    		dy_ext0 := dy0 - 2 * cOSN_SQUISH_4D;
    		dy_ext1 := dy0 - 3 * cOSN_SQUISH_4D;
        dy_ext2 := dy_ext1;
    	end;

    	if ((c and $04) <> 0) then
      begin
    		zsv_ext0 := zsb + 1;
        zsv_ext1 := zsv_ext0;
        zsv_ext2 := zsv_ext0;
    		dz_ext0  := dz0 - 1 - 2 * cOSN_SQUISH_4D;
    		dz_ext1  := dz0 - 1 - 3 * cOSN_SQUISH_4D;
        dz_ext2  := dz_ext1;
    		if ((c and $03) <> 0) then
        begin
    			zsv_ext2 := zsv_ext2 + 1;
    			dz_ext2  := dz_ext2 - 1;
    		end
        else
        begin
    			zsv_ext1 := zsv_ext1 + 1;
    			dz_ext1  := dz_ext1 - 1;
    		end;
    	end
      else
      begin
    		zsv_ext0 := zsb;
        zsv_ext1 := zsb;
        zsv_ext2 := zsb;
    		dz_ext0 := dz0 - 2 * cOSN_SQUISH_4D;
    		dz_ext1 := dz0 - 3 * cOSN_SQUISH_4D;
        dz_ext2 := dz_ext1;
    	end;

    	if ((c and $08) <> 0) then
      begin
    		wsv_ext0 := wsb + 1;
        wsv_ext1 := wsv_ext0;
    		wsv_ext2 := wsb + 2;
    		dw_ext0 := dw0 - 1 - 2 * cOSN_SQUISH_4D;
    		dw_ext1 := dw0 - 1 - 3 * cOSN_SQUISH_4D;
    		dw_ext2 := dw0 - 2 - 3 * cOSN_SQUISH_4D;
    	end
      else
      begin
    		wsv_ext0 := wsb;
        wsv_ext1 := wsb;
        wsv_ext2 := wsb;
    		dw_ext0 := dw0 - 2 * cOSN_SQUISH_4D;
    		dw_ext1 := dw0 - 3 * cOSN_SQUISH_4D;
        dw_ext2 := dw_ext1;
    	end;
    end;

    // Contribution (1,1,1,0)
    dx4 := dx0 - 1 - 3 * cOSN_SQUISH_4D;
    dy4 := dy0 - 1 - 3 * cOSN_SQUISH_4D;
    dz4 := dz0 - 1 - 3 * cOSN_SQUISH_4D;
    dw4 := dw0 - 3 * cOSN_SQUISH_4D;
    attn4 := 2 - dx4 * dx4 - dy4 * dy4 - dz4 * dz4 - dw4 * dw4;
    if (attn4 > 0) then
    begin
    	attn4 := attn4 * attn4;
    	value := value + (attn4 * attn4 * Extrapolate4D(xsb + 1, ysb + 1, zsb + 1, wsb, dx4, dy4, dz4, dw4));
    end;

    // Contribution (1,1,0,1)
    dx3 := dx4;
    dy3 := dy4;
    dz3 := dz0 - 3 * cOSN_SQUISH_4D;
    dw3 := dw0 - 1 - 3 * cOSN_SQUISH_4D;
    attn3 := 2 - dx3 * dx3 - dy3 * dy3 - dz3 * dz3 - dw3 * dw3;
    if (attn3 > 0) then
    begin
    	attn3 := attn3 * attn3;
    	value := value + (attn3 * attn3 * Extrapolate4D(xsb + 1, ysb + 1, zsb, wsb + 1, dx3, dy3, dz3, dw3));
    end;

    // Contribution (1,0,1,1)
    dx2 := dx4;
    dy2 := dy0 - 3 * cOSN_SQUISH_4D;
    dz2 := dz4;
    dw2 := dw3;
    attn2 := 2 - dx2 * dx2 - dy2 * dy2 - dz2 * dz2 - dw2 * dw2;
    if (attn2 > 0) then
    begin
    	attn2 := attn2 * attn2;
    	value := value + (attn2 * attn2 * Extrapolate4D(xsb + 1, ysb, zsb + 1, wsb + 1, dx2, dy2, dz2, dw2));
    end;

    // Contribution (0,1,1,1)
    dx1 := dx0 - 3 * cOSN_SQUISH_4D;
    dz1 := dz4;
    dy1 := dy4;
    dw1 := dw3;
    attn1 := 2 - dx1 * dx1 - dy1 * dy1 - dz1 * dz1 - dw1 * dw1;
    if (attn1 > 0) then
    begin
    	attn1 := attn1 * attn1;
    	value := value + (attn1 * attn1 * Extrapolate4D(xsb, ysb + 1, zsb + 1, wsb + 1, dx1, dy1, dz1, dw1));
    end;

    // Contribution (1,1,1,1)
    dx0 := dx0 - 1 - 4 * cOSN_SQUISH_4D;
    dy0 := dy0 - 1 - 4 * cOSN_SQUISH_4D;
    dz0 := dz0 - 1 - 4 * cOSN_SQUISH_4D;
    dw0 := dw0 - 1 - 4 * cOSN_SQUISH_4D;
    attn0 := 2 - dx0 * dx0 - dy0 * dy0 - dz0 * dz0 - dw0 * dw0;
    if (attn0 > 0) then
    begin
    	attn0 := attn0 * attn0;
    	value := value + (attn0 * attn0 * Extrapolate4D(xsb + 1, ysb + 1, zsb + 1, wsb + 1, dx0, dy0, dz0, dw0));
    end;
  end
  else if (inSum <= 2) then
  begin // We're inside the first dispentachoron (Rectified 4-Simplex)
    aIsBiggerSide := True;
    bIsBiggerSide := True;

    // Decide between (1,1,0,0) and (0,0,1,1)
    if (xins + yins) > (zins + wins) then
    begin
    	aScore := xins + yins;
    	aPoint := $03;
    end
    else
    begin
    	aScore := zins + wins;
    	aPoint := $0C;
    end;

    // Decide between (1,0,1,0) and (0,1,0,1)
    if (xins + zins) > (yins + wins) then
    begin
    	bScore := xins + zins;
    	bPoint := $05;
    end
    else
    begin
    	bScore := yins + wins;
    	bPoint := $0A;
    end;

    // Closer between (1,0,0,1) and (0,1,1,0) will replace the further of a and b, if closer.
    if (xins + wins) > (yins + zins) then
    begin
    	score := xins + wins;
    	if (aScore >= bScore) and (score > bScore) then
      begin
    		bScore := score;
    		bPoint := $09;
    	end
      else if (aScore < bScore) and (score > aScore) then
      begin
    		aScore := score;
    		aPoint := $09;
    	end;
    end
    else
    begin
    	score := yins + zins;
    	if (aScore >= bScore) and (score > bScore) then
      begin
    		bScore := score;
    		bPoint := $06;
    	end
      else if (aScore < bScore) and (score > aScore) then
      begin
    		aScore := score;
    		aPoint := $06;
    	end;
    end;

    // Decide if (1,0,0,0) is closer.
    p1 := 2 - inSum + xins;
    if (aScore >= bScore) and (p1 > bScore) then
    begin
    	bScore := p1;
    	bPoint := $01;
    	bIsBiggerSide := False;
    end
    else if (aScore < bScore) and (p1 > aScore) then
    begin
    	aScore := p1;
    	aPoint := $01;
    	aIsBiggerSide := False;
    end;

    // Decide if (0,1,0,0) is closer.
    p2 := 2 - inSum + yins;
    if (aScore >= bScore) and (p2 > bScore) then
    begin
    	bScore := p2;
    	bPoint := $02;
    	bIsBiggerSide := False;
    end
    else if (aScore < bScore) and (p2 > aScore) then
    begin
    	aScore := p2;
    	aPoint := $02;
    	aIsBiggerSide := False;
    end;

    // Decide if (0,0,1,0) is closer.
    p3 := 2 - inSum + zins;
    if (aScore >= bScore) and (p3 > bScore) then
    begin
    	bScore := p3;
    	bPoint := $04;
    	bIsBiggerSide := False;
    end
    else if (aScore < bScore) and (p3 > aScore) then
    begin
    	aScore := p3;
    	aPoint := $04;
    	aIsBiggerSide := False;
    end;

    // Decide if (0,0,0,1) is closer.
    p4 := 2 - inSum + wins;
    if (aScore >= bScore) and (p4 > bScore) then
    begin
    	bScore := p4;
    	bPoint := $08;
    	bIsBiggerSide := False;
    end
    else if (aScore < bScore) and (p4 > aScore) then
    begin
    	aScore := p4;
    	aPoint := $08;
    	aIsBiggerSide := False;
    end;

    // Where each of the two closest points are determines how the extra three vertices are calculated.
    if (aIsBiggerSide = bIsBiggerSide) then
    begin
    	if (aIsBiggerSide) then // Both closest points on the bigger side
      begin
    		c1 := (aPoint or bPoint);
    		c2 := (aPoint and bPoint);
    		if ((c1 and $01) = 0) then
        begin
    			xsv_ext0 := xsb;
    			xsv_ext1 := xsb - 1;
    			dx_ext0 := dx0 - 3 * cOSN_SQUISH_4D;
    			dx_ext1 := dx0 + 1 - 2 * cOSN_SQUISH_4D;
    		end
        else
        begin
    			xsv_ext0 := xsb + 1;
          xsv_ext1 := xsv_ext0;
    			dx_ext0 := dx0 - 1 - 3 * cOSN_SQUISH_4D;
    			dx_ext1 := dx0 - 1 - 2 * cOSN_SQUISH_4D;
    		end;

    		if ((c1 and $02) = 0) then
        begin
    			ysv_ext0 := ysb;
    			ysv_ext1 := ysb - 1;
    			dy_ext0 := dy0 - 3 * cOSN_SQUISH_4D;
    			dy_ext1 := dy0 + 1 - 2 * cOSN_SQUISH_4D;
    		end
        else
        begin
    			ysv_ext0 := ysb + 1;
          ysv_ext1 := ysb + 1;
    			dy_ext0 := dy0 - 1 - 3 * cOSN_SQUISH_4D;
    			dy_ext1 := dy0 - 1 - 2 * cOSN_SQUISH_4D;
    		end;

    		if ((c1 and $04) = 0) then
        begin
    			zsv_ext0 := zsb;
    			zsv_ext1 := zsb - 1;
    			dz_ext0 := dz0 - 3 * cOSN_SQUISH_4D;
    			dz_ext1 := dz0 + 1 - 2 * cOSN_SQUISH_4D;
    		end
        else
        begin
    			zsv_ext0 := zsb + 1;
          zsv_ext1 := zsv_ext0;
    			dz_ext0 := dz0 - 1 - 3 * cOSN_SQUISH_4D;
    			dz_ext1 := dz0 - 1 - 2 * cOSN_SQUISH_4D;
    		end;

    		if ((c1 and $08) = 0) then
        begin
    			wsv_ext0 := wsb;
    			wsv_ext1 := wsb - 1;
    			dw_ext0 := dw0 - 3 * cOSN_SQUISH_4D;
    			dw_ext1 := dw0 + 1 - 2 * cOSN_SQUISH_4D;
    		end
        else
        begin
    			wsv_ext0 := wsb + 1;
          wsv_ext1 := wsv_ext0;
    			dw_ext0 := dw0 - 1 - 3 * cOSN_SQUISH_4D;
    			dw_ext1 := dw0 - 1 - 2 * cOSN_SQUISH_4D;
    		end;

    		// One combination is a permutation of (0,0,0,2) based on c2
    		xsv_ext2 := xsb;
    		ysv_ext2 := ysb;
    		zsv_ext2 := zsb;
    		wsv_ext2 := wsb;
    		dx_ext2 := dx0 - 2 * cOSN_SQUISH_4D;
    		dy_ext2 := dy0 - 2 * cOSN_SQUISH_4D;
    		dz_ext2 := dz0 - 2 * cOSN_SQUISH_4D;
    		dw_ext2 := dw0 - 2 * cOSN_SQUISH_4D;
    		if ((c2 and $01) <> 0) then
        begin
    			xsv_ext2 := xsv_ext2 + 2;
    			dx_ext2 := dx_ext2 - 2;
    		end
        else if ((c2 and $02) <> 0) then
        begin
    			ysv_ext2 := ysv_ext2 + 2;
    			dy_ext2 := dy_ext2 - 2;
    		end
        else if ((c2 and $04) <> 0) then
        begin
    			zsv_ext2 := zsv_ext2 + 2;
    			dz_ext2 := dz_ext2 - 2;
    		end
        else
        begin
    			wsv_ext2 := wsv_ext2 + 2;
    			dw_ext2 := dw_ext2 - 2;
    		end;
      end
    	else // Both closest points on the smaller side
      begin
    		// One of the two extra points is (0,0,0,0)
    		xsv_ext2 := xsb;
    		ysv_ext2 := ysb;
    		zsv_ext2 := zsb;
    		wsv_ext2 := wsb;
    		dx_ext2 := dx0;
    		dy_ext2 := dy0;
    		dz_ext2 := dz0;
    		dw_ext2 := dw0;

    		// Other two points are based on the omitted axes.
    		c := (aPoint or bPoint);

    		if ((c and $01) = 0) then
        begin
    			xsv_ext0 := xsb - 1;
    			xsv_ext1 := xsb;
    			dx_ext0 := dx0 + 1 -  cOSN_SQUISH_4D;
    			dx_ext1 := dx0 - cOSN_SQUISH_4D;
    		end
        else
        begin
    			xsv_ext0 := xsb + 1;
          xsv_ext1 := xsv_ext0;
    			dx_ext0 := dx0 - 1 -  cOSN_SQUISH_4D;
          dx_ext1 := dx_ext0;
    		end;

    		if ((c and $02) = 0) then
        begin
    			ysv_ext0 := ysb;
          ysv_ext1 := ysb;
    			dy_ext0 := dy0 - cOSN_SQUISH_4D;
          dy_ext1 := dy_ext0;
    			if ((c and $01) = $01) then
    			begin
    				ysv_ext0 := ysv_ext0 - 1;
    				dy_ext0 := dy_ext0 + 1;
    			end
          else
          begin
    				ysv_ext1 := ysv_ext1 - 1;
    				dy_ext1 := dy_ext1 + 1;
    			end;
    		end
        else
        begin
    			ysv_ext0 := ysb + 1;
          ysv_ext1 := ysv_ext0;
    			dy_ext0 := dy0 - 1 - cOSN_SQUISH_4D;
          dy_ext1 := dy_ext0;
    		end;

    		if ((c and $04) = 0) then
        begin
    			zsv_ext0 := zsb;
          zsv_ext1 := zsb;
    			dz_ext0  := dz0 - cOSN_SQUISH_4D;
          dz_ext1  := dz_ext0;
    			if ((c and $03) = $03) then
    			begin
    				zsv_ext0 := zsv_ext0 - 1;
    				dz_ext0 := dz_ext0 + 1;
    			end
          else
          begin
    				zsv_ext1 := zsv_ext1 - 1;
    				dz_ext1 := dz_ext1 + 1;
    			end;
    		end
        else
        begin
    			zsv_ext0 := zsb + 1;
          zsv_ext1 := zsv_ext0;
    			dz_ext0 := dz0 - 1 - cOSN_SQUISH_4D;
          dz_ext1 := dz_ext0;
    		end;

    		if ((c and $08) = 0) then
    		begin
    			wsv_ext0 := wsb;
    			wsv_ext1 := wsb - 1;
    			dw_ext0 := dw0 - cOSN_SQUISH_4D;
    			dw_ext1 := dw0 + 1 -  cOSN_SQUISH_4D;
    		end
        else
        begin
    			wsv_ext0 := wsb + 1;
          wsv_ext1 := wsv_ext0;
    			dw_ext0 :=  dw0 - 1 -  cOSN_SQUISH_4D;
          dw_ext1 := dw_ext0;
    		end;
    	end;
    end
    else // One point on each "side"
    begin
    	if (aIsBiggerSide) then
      begin
    		c1 := aPoint;
    		c2 := bPoint;
    	end
      else
      begin
    		c1 := bPoint;
    		c2 := aPoint;
    	end;

    	// Two contributions are the bigger-sided point with each 0 replaced with -1.
    	if ((c1 and $01) = 0) then
      begin
    		xsv_ext0 := xsb - 1;
    		xsv_ext1 := xsb;
    		dx_ext0 := dx0 + 1 -  cOSN_SQUISH_4D;
    		dx_ext1 := dx0 - cOSN_SQUISH_4D;
    	end
      else
      begin
    		xsv_ext0 := xsb + 1;
        xsv_ext1 := xsv_ext0;
    		dx_ext0 := dx0 - 1 -  cOSN_SQUISH_4D;
        dx_ext1 := dx_ext0;
    	end;

    	if ((c1 and $02) = 0) then
      begin
    		ysv_ext0 := ysb;
        ysv_ext1 := ysb;
    		dy_ext0 := dy0 - cOSN_SQUISH_4D;
        dy_ext1 := dy_ext0;
    		if ((c1 and $01) = $01) then
        begin
    			ysv_ext0 := ysv_ext0 - 1;
    			dy_ext0 := dy_ext0 + 1;
    		end
        else
        begin
    			ysv_ext1 := ysv_ext1 - 1;
    			dy_ext1 := dy_ext1 + 1;
    		end;
    	end
      else
      begin
    		ysv_ext0 := ysb + 1;
        ysv_ext1 := ysv_ext0;
    		dy_ext0 := dy0 - 1 -  cOSN_SQUISH_4D;
        dy_ext1 := dy_ext0;
    	end;

    	if ((c1 and $04) = 0) then
      begin
    		zsv_ext0 := zsb;
        zsv_ext1 := zsb;
    		dz_ext0 := dz0 - cOSN_SQUISH_4D;
        dz_ext1 :=  dz_ext0;
    		if ((c1 and $03) = $03) then
        begin
    			zsv_ext0 := zsv_ext0 - 1;
    			dz_ext0 := dz_ext0 + 1;
    		end
        else
        begin
    			zsv_ext1 := zsv_ext1 - 1;
    			dz_ext1 := dz_ext1 + 1;
    		end;
    	end
      else
      begin
    		zsv_ext0 := zsb + 1;
        zsv_ext1 := zsv_ext0;
    		dz_ext0 := dz0 - 1 -  cOSN_SQUISH_4D;
        dz_ext1 := dz_ext0;
    	end;

    	if ((c1 and $08) = 0) then
      begin
    		wsv_ext0 := wsb;
    		wsv_ext1 := wsb - 1;
    		dw_ext0 := dw0 - cOSN_SQUISH_4D;
    		dw_ext1 := dw0 + 1 - cOSN_SQUISH_4D;
    	end
      else
      begin
    		wsv_ext0 := wsb + 1;
        wsv_ext1 := wsv_ext0;
    		dw_ext0 := dw0 - 1 -  cOSN_SQUISH_4D;
        dw_ext1 := dw_ext0;
    	end;

    	// One contribution is a permutation of (0,0,0,2) based on the smaller-sided point
    	xsv_ext2 := xsb;
    	ysv_ext2 := ysb;
    	zsv_ext2 := zsb;
    	wsv_ext2 := wsb;
    	dx_ext2 := dx0 - 2 * cOSN_SQUISH_4D;
    	dy_ext2 := dy0 - 2 * cOSN_SQUISH_4D;
    	dz_ext2 := dz0 - 2 * cOSN_SQUISH_4D;
    	dw_ext2 := dw0 - 2 * cOSN_SQUISH_4D;
    	if ((c2 and $01) <> 0) then
      begin
    		xsv_ext2 := xsv_ext2 + 2;
    		dx_ext2 := dx_ext2 - 2;
    	end
      else if ((c2 and $02) <> 0) then
      begin
    		ysv_ext2 := ysv_ext2 + 2;
    		dy_ext2 := dy_ext2 - 2;
    	end
      else if ((c2 and $04) <> 0) then
      begin
    		zsv_ext2 := zsv_ext2 + 2;
    		dz_ext2 := dz_ext2 - 2;
    	end
      else
      begin
    		wsv_ext2 := wsv_ext2 + 2;
    		dw_ext2 := dw_ext2 - 2;
    	end;
    end;

    // Contribution (1,0,0,0)
    dx1 := dx0 - 1 - cOSN_SQUISH_4D;
    dy1 := dy0 - 0 - cOSN_SQUISH_4D;
    dz1 := dz0 - 0 - cOSN_SQUISH_4D;
    dw1 := dw0 - 0 - cOSN_SQUISH_4D;
    attn1 := 2 - dx1 * dx1 - dy1 * dy1 - dz1 * dz1 - dw1 * dw1;
    if (attn1 > 0) then
    begin
    	attn1 := attn1 * attn1;
    	value := value + (attn1 * attn1 * Extrapolate4D(xsb + 1, ysb, zsb, wsb, dx1, dy1, dz1, dw1));
    end;

    // Contribution (0,1,0,0)
    dx2 := dx0 - 0 - cOSN_SQUISH_4D;
    dy2 := dy0 - 1 - cOSN_SQUISH_4D;
    dz2 := dz1;
    dw2 := dw1;
    attn2 := 2 - dx2 * dx2 - dy2 * dy2 - dz2 * dz2 - dw2 * dw2;
    if (attn2 > 0) then
    begin
    	attn2 := attn2 * attn2;
    	value := value + (attn2 * attn2 * Extrapolate4D(xsb, ysb + 1, zsb, wsb, dx2, dy2, dz2, dw2));
    end;

    // Contribution (0,0,1,0)
    dx3 := dx2;
    dy3 := dy1;
    dz3 := dz0 - 1 - cOSN_SQUISH_4D;
    dw3 := dw1;
    attn3 := 2 - dx3 * dx3 - dy3 * dy3 - dz3 * dz3 - dw3 * dw3;
    if (attn3 > 0) then
    begin
    	attn3 := attn3 * attn3;
    	value := value + (attn3 * attn3 * Extrapolate4D(xsb, ysb, zsb + 1, wsb, dx3, dy3, dz3, dw3));
    end;

    // Contribution (0,0,0,1)
    dx4 := dx2;
    dy4 := dy1;
    dz4 := dz1;
    dw4 := dw0 - 1 -  cOSN_SQUISH_4D;
    attn4 := 2 - dx4 * dx4 - dy4 * dy4 - dz4 * dz4 - dw4 * dw4;
    if (attn4 > 0) then
    begin
    	attn4 := attn4 * attn4;
    	value := value + (attn4 * attn4 * Extrapolate4D(xsb, ysb, zsb, wsb + 1, dx4, dy4, dz4, dw4));
    end;

    // Contribution (1,1,0,0)
    dx5 := dx0 - 1 - 2 * cOSN_SQUISH_4D;
    dy5 := dy0 - 1 - 2 * cOSN_SQUISH_4D;
    dz5 := dz0 - 0 - 2 * cOSN_SQUISH_4D;
    dw5 := dw0 - 0 - 2 * cOSN_SQUISH_4D;
    attn5 := 2 - dx5 * dx5 - dy5 * dy5 - dz5 * dz5 - dw5 * dw5;
    if (attn5 > 0) then
    begin
    	attn5 := attn5 * attn5;
    	value := value + (attn5 * attn5 * Extrapolate4D(xsb + 1, ysb + 1, zsb, wsb, dx5, dy5, dz5, dw5));
    end;

    // Contribution (1,0,1,0)
    dx6 := dx0 - 1 - 2 * cOSN_SQUISH_4D;
    dy6 := dy0 - 0 - 2 * cOSN_SQUISH_4D;
    dz6 := dz0 - 1 - 2 * cOSN_SQUISH_4D;
    dw6 := dw0 - 0 - 2 * cOSN_SQUISH_4D;
    attn6 := 2 - dx6 * dx6 - dy6 * dy6 - dz6 * dz6 - dw6 * dw6;
    if (attn6 > 0) then
    begin
    	attn6 := attn6 * attn6;
    	value := value + (attn6 * attn6 * Extrapolate4D(xsb + 1, ysb, zsb + 1, wsb, dx6, dy6, dz6, dw6));
    end;

    // Contribution (1,0,0,1)
    dx7 := dx0 - 1 - 2 * cOSN_SQUISH_4D;
    dy7 := dy0 - 0 - 2 * cOSN_SQUISH_4D;
    dz7 := dz0 - 0 - 2 * cOSN_SQUISH_4D;
    dw7 := dw0 - 1 - 2 * cOSN_SQUISH_4D;
    attn7 := 2 - dx7 * dx7 - dy7 * dy7 - dz7 * dz7 - dw7 * dw7;
    if (attn7 > 0) then
    begin
    	attn7 := attn7 * attn7;
    	value := value + (attn7 * attn7 * Extrapolate4D(xsb + 1, ysb, zsb, wsb + 1, dx7, dy7, dz7, dw7));
    end;

    // Contribution (0,1,1,0)
    dx8 := dx0 - 0 - 2 * cOSN_SQUISH_4D;
    dy8 := dy0 - 1 - 2 * cOSN_SQUISH_4D;
    dz8 := dz0 - 1 - 2 * cOSN_SQUISH_4D;
    dw8 := dw0 - 0 - 2 * cOSN_SQUISH_4D;
    attn8 := 2 - dx8 * dx8 - dy8 * dy8 - dz8 * dz8 - dw8 * dw8;
    if (attn8 > 0) then
    begin
    	attn8 := attn8 * attn8;
    	value := value + (attn8 * attn8 * Extrapolate4D(xsb, ysb + 1, zsb + 1, wsb, dx8, dy8, dz8, dw8));
    end;

    // Contribution (0,1,0,1)
    dx9 := dx0 - 0 - 2 * cOSN_SQUISH_4D;
    dy9 := dy0 - 1 - 2 * cOSN_SQUISH_4D;
    dz9 := dz0 - 0 - 2 * cOSN_SQUISH_4D;
    dw9 := dw0 - 1 - 2 * cOSN_SQUISH_4D;
    attn9 := 2 - dx9 * dx9 - dy9 * dy9 - dz9 * dz9 - dw9 * dw9;
    if (attn9 > 0) then
    begin
    	attn9 := attn9 * attn9;
    	value := value + (attn9 * attn9 * Extrapolate4D(xsb, ysb + 1, zsb, wsb + 1, dx9, dy9, dz9, dw9));
    end;

    // Contribution (0,0,1,1)
    dx10 := dx0 - 0 - 2 * cOSN_SQUISH_4D;
    dy10 := dy0 - 0 - 2 * cOSN_SQUISH_4D;
    dz10 := dz0 - 1 - 2 * cOSN_SQUISH_4D;
    dw10 := dw0 - 1 - 2 * cOSN_SQUISH_4D;
    attn10 := 2 - dx10 * dx10 - dy10 * dy10 - dz10 * dz10 - dw10 * dw10;
    if (attn10 > 0) then
    begin
    	attn10 := attn10 * attn10;
    	value := value + (attn10 * attn10 * Extrapolate4D(xsb, ysb, zsb + 1, wsb + 1, dx10, dy10, dz10, dw10));
    end;
  end
  else
  begin // We're inside the second dispentachoron (Rectified 4-Simplex)

    aIsBiggerSide := True;
    bIsBiggerSide := True;

    // Decide between (0,0,1,1) and (1,1,0,0)
    if (xins + yins) < (zins + wins) then
    begin
    	aScore := xins + yins;
    	aPoint := $0C;
    end
    else
    begin
    	aScore := zins + wins;
    	aPoint := $03;
    end;

    // Decide between (0,1,0,1) and (1,0,1,0)
    if (xins + zins) < (yins + wins) then
    begin
    	bScore := xins + zins;
    	bPoint := $0A;
    end
    else
    begin
    	bScore := yins + wins;
    	bPoint := $05;
    end;

    // Closer between (0,1,1,0) and (1,0,0,1) will replace the further of a and b, if closer.
    if (xins + wins) < (yins + zins) then
    begin
    	score := xins + wins;
    	if (aScore <= bScore) and (score < bScore) then
      begin
    		bScore := score;
    		bPoint := $06;
    	end
      else if (aScore > bScore) and (score < aScore) then
      begin
    		aScore := score;
    		aPoint := $06;
    	end;
    end
    else
    begin
    	score := yins + zins;
    	if (aScore <= bScore) and (score < bScore) then
      begin
    		bScore := score;
    		bPoint := $09;
    	end
      else if (aScore > bScore) and (score < aScore) then
      begin
    		aScore := score;
    		aPoint := $09;
    	end;
    end;

    // Decide if (0,1,1,1) is closer.
    p1 := 3 - inSum + xins;
    if (aScore <= bScore) and (p1 < bScore) then
    begin
    	bScore := p1;
    	bPoint := $0E;
    	bIsBiggerSide := False;
    end
    else if (aScore > bScore) and (p1 < aScore) then
    begin
    	aScore := p1;
    	aPoint := $0E;
    	aIsBiggerSide := False;
    end;

    // Decide if (1,0,1,1) is closer.
    p2 := 3 - inSum + yins;
    if (aScore <= bScore) and (p2 < bScore) then
    begin
    	bScore := p2;
    	bPoint := $0D;
    	bIsBiggerSide := False;
    end
    else if (aScore > bScore) and (p2 < aScore) then
    begin
    	aScore := p2;
    	aPoint := $0D;
    	aIsBiggerSide := False;
    end;

    // Decide if (1,1,0,1) is closer.
    p3 := 3 - inSum + zins;
    if (aScore <= bScore) and (p3 < bScore) then
    begin
    	bScore := p3;
    	bPoint := $0B;
    	bIsBiggerSide := False;
    end
    else if (aScore > bScore) and (p3 < aScore) then
    begin
    	aScore := p3;
    	aPoint := $0B;
    	aIsBiggerSide := False;
    end;

    // Decide if (1,1,1,0) is closer.
    p4 := 3 - inSum + wins;
    if (aScore <= bScore) and (p4 < bScore) then
    begin
    	bScore := p4;
    	bPoint := $07;
    	bIsBiggerSide := False;
    end
    else if (aScore > bScore) and (p4 < aScore) then
    begin
    	aScore := p4;
    	aPoint := $07;
    	aIsBiggerSide := False;
    end;

    // Where each of the two closest points are determines how the extra three vertices are calculated.
    if (aIsBiggerSide = bIsBiggerSide) then
    begin
    	if (aIsBiggerSide) then // Both closest points on the bigger side
      begin
    		c1 := (aPoint and bPoint);
    		c2 := (aPoint or bPoint);

    		// Two contributions are permutations of (0,0,0,1) and (0,0,0,2) based on c1
    		xsv_ext0 := xsb;
        xsv_ext1 := xsb;
    		ysv_ext0 := ysb;
        ysv_ext1 := ysb;
    		zsv_ext0 := zsb;
        zsv_ext1 := zsb;
    		wsv_ext0 := wsb;
        wsv_ext1 := wsb;
    		dx_ext0 := dx0 - cOSN_SQUISH_4D;
    		dy_ext0 := dy0 - cOSN_SQUISH_4D;
    		dz_ext0 := dz0 - cOSN_SQUISH_4D;
    		dw_ext0 := dw0 - cOSN_SQUISH_4D;
    		dx_ext1 := dx0 - 2 * cOSN_SQUISH_4D;
    		dy_ext1 := dy0 - 2 * cOSN_SQUISH_4D;
    		dz_ext1 := dz0 - 2 * cOSN_SQUISH_4D;
    		dw_ext1 := dw0 - 2 * cOSN_SQUISH_4D;
    		if ((c1 and $01) <> 0) then
        begin
    			xsv_ext0 := xsv_ext0 + 1;
    			dx_ext0 := dx_ext0 - 1;
    			xsv_ext1 := xsv_ext1 + 2;
    			dx_ext1 := dx_ext1 - 2;
    		end
        else if ((c1 and $02) <> 0) then
        begin
    			ysv_ext0 := ysv_ext0 + 1;
    			dy_ext0  := dy_ext0 - 1;
    			ysv_ext1 := ysv_ext1 + 2;
    			dy_ext1  := dy_ext1 - 2;
    		end
        else if ((c1 and $04) <> 0) then
        begin
    			zsv_ext0 := zsv_ext0 + 1;
    			dz_ext0  := dz_ext0 - 1;
    			zsv_ext1 := zsv_ext1 + 2;
    			dz_ext1  := dz_ext1 - 2;
    		end
        else
        begin
    			wsv_ext0 := wsv_ext0 + 1;
    			dw_ext0  := dw_ext0 - 1;
    			wsv_ext1 := wsv_ext1 + 2;
    			dw_ext1  := dw_ext1 - 2;
    		end;

    		// One contribution is a permutation of (1,1,1,-1) based on c2
    		xsv_ext2 := xsb + 1;
    		ysv_ext2 := ysb + 1;
    		zsv_ext2 := zsb + 1;
    		wsv_ext2 := wsb + 1;
    		dx_ext2 := dx0 - 1 - 2 * cOSN_SQUISH_4D;
    		dy_ext2 := dy0 - 1 - 2 * cOSN_SQUISH_4D;
    		dz_ext2 := dz0 - 1 - 2 * cOSN_SQUISH_4D;
    		dw_ext2 := dw0 - 1 - 2 * cOSN_SQUISH_4D;
    		if ((c2 and $01) = 0) then
        begin
    			xsv_ext2 := xsv_ext2 - 2;
    			dx_ext2  := dx_ext2 + 2;
    		end
        else if ((c2 and $02) = 0) then
        begin
    			ysv_ext2 := ysv_ext2 - 2;
    			dy_ext2  := dy_ext2 + 2;
    		end
        else if ((c2 and $04) = 0) then
        begin
    			zsv_ext2 := zsv_ext2 - 2;
    			dz_ext2  := dz_ext2 + 2;
    		end
        else
        begin
    			wsv_ext2 := wsv_ext2 - 2;
    			dw_ext2  := dw_ext2 + 2;
    		end;
    	end
      else  // Both closest points on the smaller side
      begin
    		// One of the two extra points is (1,1,1,1)
    		xsv_ext2 := xsb + 1;
    		ysv_ext2 := ysb + 1;
    		zsv_ext2 := zsb + 1;
    		wsv_ext2 := wsb + 1;
    		dx_ext2  := dx0 - 1 - 4 * cOSN_SQUISH_4D;
    		dy_ext2  := dy0 - 1 - 4 * cOSN_SQUISH_4D;
    		dz_ext2  := dz0 - 1 - 4 * cOSN_SQUISH_4D;
    		dw_ext2  := dw0 - 1 - 4 * cOSN_SQUISH_4D;

    		// Other two points are based on the shared axes.
    		c := (aPoint and bPoint);

    		if ((c and $01) <> 0) then
        begin
    			xsv_ext0 := xsb + 2;
    			xsv_ext1 := xsb + 1;
    			dx_ext0  := dx0 - 2 - 3 * cOSN_SQUISH_4D;
    			dx_ext1  := dx0 - 1 - 3 * cOSN_SQUISH_4D;
    		end
        else
        begin
    			xsv_ext0 := xsb;
          xsv_ext1 := xsb;
    			dx_ext0  := dx0 - 3 * cOSN_SQUISH_4D;
          dx_ext1  := dx_ext0;
    		end;

    		if ((c and $02) <> 0) then
        begin
    			ysv_ext0 := ysb + 1;
          ysv_ext1 := ysv_ext0;
    			dy_ext0  := dy0 - 1 - 3 * cOSN_SQUISH_4D;
          dy_ext1  := dy_ext0;
    			if ((c and $01) = 0) then
    			begin
    				ysv_ext0 := ysv_ext0 + 1;
    				dy_ext0  := dy_ext0 - 1;
    			end
          else
          begin
    				ysv_ext1 := ysv_ext1 + 1;
    				dy_ext1  := dy_ext1 - 1;
    			end;
    		end
        else
        begin
    			ysv_ext0 := ysb;
          ysv_ext1 := ysb;
    			dy_ext0  :=  dy0 - 3 * cOSN_SQUISH_4D;
          dy_ext1  := dy_ext0;
    		end;

    		if ((c and $04) <> 0) then
        begin
    			zsv_ext0 := zsb + 1;
          zsv_ext1 := zsv_ext0;
    			dz_ext0  := dz0 - 1 - 3 * cOSN_SQUISH_4D;
          dz_ext1  := dz_ext0;
    			if ((c and $03) = 0) then
    			begin
    				zsv_ext0 := zsv_ext0 + 1;
    				dz_ext0  := dz_ext0 - 1;
    			end
          else
          begin
    				zsv_ext1 := zsv_ext1 + 1;
    				dz_ext1  := dz_ext1 - 1;
    			end;
    		end
        else
        begin
    			zsv_ext0 := zsb;
          zsv_ext1 := zsb;
    			dz_ext0  := dz0 - 3 * cOSN_SQUISH_4D;
          dz_ext1  := dz_ext0;
    		end;

    		if ((c and $08) <> 0) then
    		begin
    			wsv_ext0 := wsb + 1;
    			wsv_ext1 := wsb + 2;
    			dw_ext0  := dw0 - 1 - 3 * cOSN_SQUISH_4D;
    			dw_ext1  := dw0 - 2 - 3 * cOSN_SQUISH_4D;
    		end
        else
        begin
    			wsv_ext0 := wsb;
          wsv_ext1 := wsb;
    			dw_ext0  := dw0 - 3 * cOSN_SQUISH_4D;
          dw_ext1  := dw_ext0;
    		end;
    	end;
    end
    else  // One point on each "side"
    begin
    	if (aIsBiggerSide) then
      begin
    		c1 := aPoint;
    		c2 := bPoint;
    	end
      else
      begin
    		c1 := bPoint;
    		c2 := aPoint;
    	end;

    	// Two contributions are the bigger-sided point with each 1 replaced with 2.
    	if ((c1 and $01) <> 0) then
      begin
    		xsv_ext0 := xsb + 2;
    		xsv_ext1 := xsb + 1;
    		dx_ext0  := dx0 - 2 - 3 * cOSN_SQUISH_4D;
    		dx_ext1  := dx0 - 1 - 3 * cOSN_SQUISH_4D;
    	end
      else
      begin
    		xsv_ext0 := xsb;
        xsv_ext1 := xsb;
    		dx_ext0  := dx0 - 3 * cOSN_SQUISH_4D;
        dx_ext1  := dx_ext0;
    	end;

    	if ((c1 and $02) <> 0) then
      begin
    		ysv_ext0 := ysb + 1;
        ysv_ext1 := ysv_ext0;
    		dy_ext0  := dy0 - 1 - 3 * cOSN_SQUISH_4D;
        dy_ext1  := dy_ext0;
    		if ((c1 and $01) = 0) then
        begin
    			ysv_ext0 := ysv_ext0 + 1;
    			dy_ext0  := dy_ext0 - 1;
    		end
        else
        begin
    			ysv_ext1 := ysv_ext1 + 1;
    			dy_ext1  := dy_ext1 - 1;
    		end;
    	end
      else
      begin
    		ysv_ext0 := ysb;
        ysv_ext1 := ysb;
    		dy_ext0  := dy0 - 3 * cOSN_SQUISH_4D;
        dy_ext1  := dy_ext0;
    	end;

    	if ((c1 and $04) <> 0) then
      begin
    		zsv_ext0 := zsb + 1;
        zsv_ext1 := zsv_ext0;
    		dz_ext0  := dz0 - 1 - 3 * cOSN_SQUISH_4D;
        dz_ext1  := dz_ext0;
    		if ((c1 and $03) = 0) then
        begin
    			zsv_ext0 := zsv_ext0 + 1;
    			dz_ext0  := dz_ext0 - 1;
    		end
        else
        begin
    			zsv_ext1 := zsv_ext1 + 1;
    			dz_ext1  := dz_ext1 - 1;
    		end;
    	end
      else
      begin
    		zsv_ext0 := zsb;
        zsv_ext1 := zsb;
    		dz_ext0  := dz0 - 3 * cOSN_SQUISH_4D;
        dz_ext1  := dz_ext0;
    	end;

    	if ((c1 and $08) <> 0) then
      begin
    		wsv_ext0 := wsb + 1;
    		wsv_ext1 := wsb + 2;
    		dw_ext0  := dw0 - 1 - 3 * cOSN_SQUISH_4D;
    		dw_ext1  := dw0 - 2 - 3 * cOSN_SQUISH_4D;
    	end
      else
      begin
    		wsv_ext0 := wsb;
        wsv_ext1 := wsb;
    		dw_ext0  := dw0 - 3 * cOSN_SQUISH_4D;
        dw_ext1  := dw_ext0;
    	end;

    	// One contribution is a permutation of (1,1,1,-1) based on the smaller-sided point
    	xsv_ext2 := xsb + 1;
    	ysv_ext2 := ysb + 1;
    	zsv_ext2 := zsb + 1;
    	wsv_ext2 := wsb + 1;
    	dx_ext2  := dx0 - 1 - 2 * cOSN_SQUISH_4D;
    	dy_ext2  := dy0 - 1 - 2 * cOSN_SQUISH_4D;
    	dz_ext2  := dz0 - 1 - 2 * cOSN_SQUISH_4D;
    	dw_ext2  := dw0 - 1 - 2 * cOSN_SQUISH_4D;
    	if ((c2 and $01) = 0) then
      begin
    		xsv_ext2 := xsv_ext2 - 2;
    		dx_ext2  := dx_ext2 + 2;
    	end
      else if ((c2 and $02) = 0) then
      begin
    		ysv_ext2 := ysv_ext2 - 2;
    		dy_ext2  := dy_ext2 + 2;
    	end
      else if ((c2 and $04) = 0) then
      begin
    		zsv_ext2 := zsv_ext2 - 2;
    		dz_ext2  := dz_ext2 + 2;
    	end
      else
      begin
    		wsv_ext2 := wsv_ext2 - 2;
    		dw_ext2  := dw_ext2 + 2;
    	end;
    end;

    // Contribution (1,1,1,0)
    dx4 := dx0 - 1 - 3 * cOSN_SQUISH_4D;
    dy4 := dy0 - 1 - 3 * cOSN_SQUISH_4D;
    dz4 := dz0 - 1 - 3 * cOSN_SQUISH_4D;
    dw4 := dw0 - 3 * cOSN_SQUISH_4D;
    attn4 := 2 - dx4 * dx4 - dy4 * dy4 - dz4 * dz4 - dw4 * dw4;
    if (attn4 > 0) then
    begin
    	attn4 := attn4 * attn4;
    	value := value + (attn4 * attn4 * Extrapolate4D(xsb + 1, ysb + 1, zsb + 1, wsb, dx4, dy4, dz4, dw4));
    end;

    // Contribution (1,1,0,1)
    dx3 := dx4;
    dy3 := dy4;
    dz3 := dz0 - 3 * cOSN_SQUISH_4D;
    dw3 := dw0 - 1 - 3 * cOSN_SQUISH_4D;
    attn3 := 2 - dx3 * dx3 - dy3 * dy3 - dz3 * dz3 - dw3 * dw3;
    if (attn3 > 0) then
    begin
    	attn3 := attn3 * attn3;
    	value := value + (attn3 * attn3 * Extrapolate4D(xsb + 1, ysb + 1, zsb, wsb + 1, dx3, dy3, dz3, dw3));
    end;

    // Contribution (1,0,1,1)
    dx2 := dx4;
    dy2 := dy0 - 3 * cOSN_SQUISH_4D;
    dz2 := dz4;
    dw2 := dw3;
    attn2 := 2 - dx2 * dx2 - dy2 * dy2 - dz2 * dz2 - dw2 * dw2;
    if (attn2 > 0) then
    begin
    	attn2 := attn2 * attn2;
    	value := value + (attn2 * attn2 * Extrapolate4D(xsb + 1, ysb, zsb + 1, wsb + 1, dx2, dy2, dz2, dw2));
    end;

    // Contribution (0,1,1,1)
    dx1 := dx0 - 3 * cOSN_SQUISH_4D;
    dz1 := dz4;
    dy1 := dy4;
    dw1 := dw3;
    attn1 := 2 - dx1 * dx1 - dy1 * dy1 - dz1 * dz1 - dw1 * dw1;
    if (attn1 > 0) then
    begin
    	attn1 := attn1 * attn1;
    	value := value + (attn1 * attn1 * Extrapolate4D(xsb, ysb + 1, zsb + 1, wsb + 1, dx1, dy1, dz1, dw1));
    end;

    // Contribution (1,1,0,0)
    dx5 := dx0 - 1 - 2 * cOSN_SQUISH_4D;
    dy5 := dy0 - 1 - 2 * cOSN_SQUISH_4D;
    dz5 := dz0 - 0 - 2 * cOSN_SQUISH_4D;
    dw5 := dw0 - 0 - 2 * cOSN_SQUISH_4D;
    attn5 := 2 - dx5 * dx5 - dy5 * dy5 - dz5 * dz5 - dw5 * dw5;
    if (attn5 > 0) then
    begin
    	attn5 := attn5 * attn5;
    	value := value + (attn5 * attn5 * Extrapolate4D(xsb + 1, ysb + 1, zsb, wsb, dx5, dy5, dz5, dw5));
    end;

    // Contribution (1,0,1,0)
    dx6 := dx0 - 1 - 2 * cOSN_SQUISH_4D;
    dy6 := dy0 - 0 - 2 * cOSN_SQUISH_4D;
    dz6 := dz0 - 1 - 2 * cOSN_SQUISH_4D;
    dw6 := dw0 - 0 - 2 * cOSN_SQUISH_4D;
    attn6 := 2 - dx6 * dx6 - dy6 * dy6 - dz6 * dz6 - dw6 * dw6;
    if (attn6 > 0) then
    begin
    	attn6 := attn6 * attn6;
    	value := value + (attn6 * attn6 * Extrapolate4D(xsb + 1, ysb, zsb + 1, wsb, dx6, dy6, dz6, dw6));
    end;

    // Contribution (1,0,0,1)
    dx7 := dx0 - 1 - 2 * cOSN_SQUISH_4D;
    dy7 := dy0 - 0 - 2 * cOSN_SQUISH_4D;
    dz7 := dz0 - 0 - 2 * cOSN_SQUISH_4D;
    dw7 := dw0 - 1 - 2 * cOSN_SQUISH_4D;
    attn7 := 2 - dx7 * dx7 - dy7 * dy7 - dz7 * dz7 - dw7 * dw7;
    if (attn7 > 0) then
    begin
    	attn7 := attn7 * attn7;
    	value := value + (attn7 * attn7 * Extrapolate4D(xsb + 1, ysb, zsb, wsb + 1, dx7, dy7, dz7, dw7));
    end;

    // Contribution (0,1,1,0)
    dx8 := dx0 - 0 - 2 * cOSN_SQUISH_4D;
    dy8 := dy0 - 1 - 2 * cOSN_SQUISH_4D;
    dz8 := dz0 - 1 - 2 * cOSN_SQUISH_4D;
    dw8 := dw0 - 0 - 2 * cOSN_SQUISH_4D;
    attn8 := 2 - dx8 * dx8 - dy8 * dy8 - dz8 * dz8 - dw8 * dw8;
    if (attn8 > 0) then
    begin
    	attn8 := attn8 * attn8;
    	value := value + (attn8 * attn8 * Extrapolate4D(xsb, ysb + 1, zsb + 1, wsb, dx8, dy8, dz8, dw8));
    end;

    // Contribution (0,1,0,1)
    dx9 := dx0 - 0 - 2 * cOSN_SQUISH_4D;
    dy9 := dy0 - 1 - 2 * cOSN_SQUISH_4D;
    dz9 := dz0 - 0 - 2 * cOSN_SQUISH_4D;
    dw9 := dw0 - 1 - 2 * cOSN_SQUISH_4D;
    attn9 := 2 - dx9 * dx9 - dy9 * dy9 - dz9 * dz9 - dw9 * dw9;
    if (attn9 > 0) then
    begin
    	attn9 := attn9 * attn9;
    	value := value + (attn9 * attn9 * Extrapolate4D(xsb, ysb + 1, zsb, wsb + 1, dx9, dy9, dz9, dw9));
    end;

    // Contribution (0,0,1,1)
    dx10 := dx0 - 0 - 2 * cOSN_SQUISH_4D;
    dy10 := dy0 - 0 - 2 * cOSN_SQUISH_4D;
    dz10 := dz0 - 1 - 2 * cOSN_SQUISH_4D;
    dw10 := dw0 - 1 - 2 * cOSN_SQUISH_4D;
    attn10 := 2 - dx10 * dx10 - dy10 * dy10 - dz10 * dz10 - dw10 * dw10;
    if (attn10 > 0) then
    begin
    	attn10 := attn10 * attn10;
    	value := value + (attn10 * attn10 * Extrapolate4D(xsb, ysb, zsb + 1, wsb + 1, dx10, dy10, dz10, dw10));
    end;
  end;

	// First extra vertex
	attn_ext0 := 2 - dx_ext0 * dx_ext0 - dy_ext0 * dy_ext0 - dz_ext0 * dz_ext0 - dw_ext0 * dw_ext0;
	if (attn_ext0 > 0) then
	begin
		attn_ext0 := attn_ext0 * attn_ext0;
		value := value + (attn_ext0 * attn_ext0 * Extrapolate4D(xsv_ext0, ysv_ext0, zsv_ext0, wsv_ext0, dx_ext0, dy_ext0, dz_ext0, dw_ext0));
	end;

	// Second extra vertex
	attn_ext1 := 2 - dx_ext1 * dx_ext1 - dy_ext1 * dy_ext1 - dz_ext1 * dz_ext1 - dw_ext1 * dw_ext1;
	if (attn_ext1 > 0) then
	begin
		attn_ext1 := attn_ext1 * attn_ext1;
		value := value + (attn_ext1 * attn_ext1 * Extrapolate4D(xsv_ext1, ysv_ext1, zsv_ext1, wsv_ext1, dx_ext1, dy_ext1, dz_ext1, dw_ext1));
	end;

	// Third extra vertex
	attn_ext2 := 2 - dx_ext2 * dx_ext2 - dy_ext2 * dy_ext2 - dz_ext2 * dz_ext2 - dw_ext2 * dw_ext2;
	if (attn_ext2 > 0) then
	begin
		attn_ext2 := attn_ext2 * attn_ext2;
		value := value + (attn_ext2 * attn_ext2 * Extrapolate4D(xsv_ext2, ysv_ext2, zsv_ext2, wsv_ext2, dx_ext2, dy_ext2, dz_ext2, dw_ext2));
	end;

	Result := value * cOSN_NORM_4D;
end;

{%endregion%}


{%region=====[ TBZCustomNoiseGenerator ]=========================================================}

Constructor TBZCustomNoiseGenerator.Create;
begin
  inherited Create;
  FSeed := 1337;
end;

procedure TBZCustomNoiseGenerator.SetSeed(const AValue : Int64);
begin
  if FSeed = AValue then Exit;
  FSeed := AValue;
end;

Function TBZCustomNoiseGenerator.RandUniform1D(x : Double) : Double;
Var
  n : Integer;
begin
  n := Round(cX_PRIME * x + cNOISE_SEED * Seed) and $7fffffff;
  n := (n shr 13) xor n;
  Result := 1.0 - (((n * (n * n * 60493 + 19990303) + 1376312589) and $7fffffff) / 1073741824.0);
end;

Function TBZCustomNoiseGenerator.RandUniform2D(x,y:Double):Double;
Var
  n : Integer;
begin
  n := Round(cX_PRIME * x + cY_PRIME * y + cNOISE_SEED * Seed) and $7fffffff;
  n := (n shr 13) xor n;
  Result := 1.0 - (((n * (n * n * 60493 + 19990303) + 1376312589) and $7fffffff) / 1073741824.0);
end;

Function TBZCustomNoiseGenerator.RandUniform3D(x,y,z : Double): Double;
Var
  n : Integer;
begin
  n := Round(cX_PRIME * x + cY_PRIME * y + cZ_PRIME * z + cNOISE_SEED * Seed) and $7fffffff;
  n := (n shr 13) xor n;
  Result := 1.0 - (((n * (n * n * 60493 + 19990303) + 1376312589) and $7fffffff) / 1073741824.0);
end;

Function TBZCustomNoiseGenerator.RandUniform4D(x, y, z, w : Double) : Double;
Var
  n : Integer;
begin
  n := Round(cX_PRIME * x + cY_PRIME * y + cZ_PRIME * z + cW_PRIME * w + cNOISE_SEED * Seed) and $7fffffff;
  n := (n shr 13) xor n;
  Result := 1.0 - (((n * (n * n * 60493 + 19990303) + 1376312589) and $7fffffff) / 1073741824.0);
end;

Function TBZCustomNoiseGenerator.RandAdditiveGaussian1D(x : Double) : Double;
var
  r, t1, t2 : Double;
  p : Integer;
begin
  p := 1;

  While p>0 do
  begin
    t2 := 0.5 + RandUniform1D(x) * 0.5;
    if t2=0 then
    begin
      P:=1;
    end
    else
    begin
      p := -1;
    end;
  end;
  r := 0.5 + RandUniform1D(x) * 0.5;
  t1 := cos(c2PI * r);
  result := sqrt( -2.0 * ln( t2 ) ) * t1;
end;

Function TBZCustomNoiseGenerator.RandAdditiveGaussian2D(x, y : Double) : Double;
var
  r, t0, t1, t2 : Double;
  p : Integer;
begin
  p := 1;

  While p>0 do
  begin
    t2 := 0.5 + RandUniform2D(x,y) * 0.5;
    if t2=0 then
    begin
      P:=1;
    end
    else
    begin
      p := -1;
    end;
  end;
  r := 0.5 + RandUniform2D(x,y) * 0.5;  // * c2PI
  t1 := Cos(c2PI * r);
  t0 := sqrt( -2.0 * ln( t2 ));
  result :=  t0 * t1;
end;

Function TBZCustomNoiseGenerator.RandAdditiveGaussian3D(x, y, z : Double) : Double;
var
  r, t0, t1, t2 : Double;
  p : Integer;
begin
  p := 1;

  While p>0 do
  begin
    t2 := 0.5 + RandUniform3D(x,y,z) * 0.5;
    if t2=0 then
    begin
      P:=1;
    end
    else
    begin
      p := -1;
    end;
  end;
  r := 0.5 + RandUniform3D(x,y,z) * 0.5;
  t1 := Cos(c2PI * r);
  t0 := sqrt( -2.0 * ln( t2 ));
  result :=  t0 * t1;
end;

Function TBZCustomNoiseGenerator.RandAdditiveGaussian4D(x, y, z, w : Double) : Double;
var
  r, t0, t1, t2,t3, t4, t5 : Double;
  p : Integer;
begin
  p := 1;

  While p>0 do
  begin
    t2 := 0.5 + RandUniform4D(x,y,z,w) * 0.5;
    if t2=0 then
    begin
      P:=1;
    end
    else
    begin
      p := -1;
    end;
  end;
  r := 0.5 + RandUniform4D(x,y,z,w) * 0.5;  // * c2PI
  t1 := Cos(c2PI * r );
  t0 := sqrt( -2.0 * ln( t2 ));
  result :=  t0 * t1;
end;

Function TBZCustomNoiseGenerator.Noise1D(x : Double) : Double;
begin
  result := 0.0;
end;

Function TBZCustomNoiseGenerator.Noise2D(x,y:Double):Double;
begin
  result := 0.0;
end;

Function TBZCustomNoiseGenerator.Noise3D(x,y,z : Double): Double;  
begin
  result := 0.0;
end;

Function TBZCustomNoiseGenerator.Noise4D(x, y, z, w : Double) : Double;
begin
  result := 0.0;
end;

{%endregion%}

{%region=====[ TBZBaseNoiseGenerator ]===========================================================}

Constructor TBZBaseNoiseGenerator.Create;
begin
  inherited Create;
  FFrequency := 0.02;
  FRandomNoiseGenerator := ngtWhite;
  FNoiseInterpolationType := nitLinear;
  FSmooth := True;
  FSmoothInterpolationType := ifmCosine;
  FSupport := [ns1D, ns2D, ns3D, ns4D];
end;

procedure TBZBaseNoiseGenerator.SetFrequency(const AValue : Double);
begin
  if FFrequency = AValue then Exit;
  FFrequency := AValue;
end;

function TBZBaseNoiseGenerator.Hash1D(x : Integer) : Integer;
var
  hash : Integer;
begin
	hash := Seed;
	hash := hash xor (cX_PRIME * x);
	hash := hash xor (cNOISE_SEED * Seed);

	hash := hash * hash * hash * 60493;
	hash := (hash shr 13) xor hash;

	Result:= hash;
end;

function TBZBaseNoiseGenerator.Hash2D(x, y : Integer) : Integer;
var
  hash : Integer;
begin
	hash := Seed;
	hash := hash xor (cX_PRIME * x);
  hash := hash xor (cY_PRIME * y);
	hash := hash xor (cNOISE_SEED * Seed);

	hash := hash * hash * hash * 60493;
	hash := (hash shr 13) xor hash;

	Result:= hash;
end;

function TBZBaseNoiseGenerator.Hash3D(x, y, z : Integer) : Integer;
var
  hash : Integer;
begin
	hash := Seed;
	hash := hash xor (cX_PRIME * x);
  hash := hash xor (cY_PRIME * y);
  hash := hash xor (cZ_PRIME * z);
	hash := hash xor (cNOISE_SEED * Seed);

	hash := hash * hash * hash * 60493;
	hash := (hash shr 13) xor hash;

	Result:= hash;
end;

function TBZBaseNoiseGenerator.Hash4D(x, y, z, w : Integer) : Integer;
var
  hash : Integer;
begin
	hash := Seed;
	hash := hash xor (cX_PRIME * x);
  hash := hash xor (cY_PRIME * y);
  hash := hash xor (cZ_PRIME * z);
  hash := hash xor (cW_PRIME * w);
	hash := hash xor (cNOISE_SEED * Seed);

	hash := hash * hash * hash * 60493;
	hash := (hash shr 13) xor hash;

	Result:= hash;
end;

procedure TBZBaseNoiseGenerator.GradientPerturb1D(Var x : Double);
begin

end;

procedure TBZBaseNoiseGenerator.GradientPerturb2D(Var x, y : Double);
begin

end;

procedure TBZBaseNoiseGenerator.GradientPerturb3D(Var x, y, z : Double);
begin

end;

procedure TBZBaseNoiseGenerator.GradientPerturb4D(Var x, y, z, w : Double);
begin

end;

Function TBZBaseNoiseGenerator.Noise1D(x : Double) : Double;
begin
  //Result := Frac(Sin(x * ((Random * 2.0) - 1.0)) * 143758.5453);
  //Result := Frequency + ( 1 * (Result + Frequency)); // Pink noise ?????
  Case FRandomNoiseGenerator of
    ngtWhite : Result := RandUniform1D(x);
    ngtAdditiveGaussian : Result := RandAdditiveGaussian1D(x);
    else
      Result := RandUniform1D(x);
  end;
end;

Function TBZBaseNoiseGenerator.Noise2D(x,y:Double):Double;
begin
  Case FRandomNoiseGenerator of
    ngtWhite : Result := RandUniform2D(x,y);
    ngtAdditiveGaussian : Result := RandAdditiveGaussian2D(x,y);
    else
      Result := RandUniform2D(x,y);
  end;
end;

Function TBZBaseNoiseGenerator.Noise3D(x,y,z : Double): Double;
begin
  Case FRandomNoiseGenerator of
    ngtWhite : Result := RandUniform3D(x,y,z);
    ngtAdditiveGaussian : Result := RandAdditiveGaussian3D(x,y,z);
    else
      Result := RandUniform3D(x,y,z);
  end;
end;

Function TBZBaseNoiseGenerator.Noise4D(x, y, z, w : Double) : Double;
begin
  Case FRandomNoiseGenerator of
    ngtWhite : Result := RandUniform4D(x,y,z,w);
    ngtAdditiveGaussian : Result := RandAdditiveGaussian4D(x,y,z,w);
    else
      Result := RandUniform4D(x,y,z,w);
  end;
end;

function TBZBaseNoiseGenerator.GetNoise(x : Double) : Double;
begin
  result := noise1D(x * Frequency);
end;

function TBZBaseNoiseGenerator.GetNoise(x, y : Double) : Double;
begin
  result := noise2D(x * Frequency, y * Frequency);
end;

function TBZBaseNoiseGenerator.GetNoise(x, y, z : Double) : Double;
begin
  result := noise3D(x * Frequency, y * Frequency, z * Frequency);
end;

function TBZBaseNoiseGenerator.GetNoise(x, y, z, w : Double) : Double;
begin
  result := noise4D(x * Frequency, y * Frequency, z * Frequency, w * Frequency);
end;

{%endregion%}

{%region=====[ TBZValueNoiseGenerator ]==========================================================}

Constructor TBZValueNoiseGenerator.Create;
begin
  inherited Create;
  Support := [ns1D, ns2D, ns3D, ns4D, nsInterpolation, nsSmooth];
end;

Function TBZValueNoiseGenerator.Noise1D(x : Double) : Double;
var
  PreviousCellNoise,
  NextCellNoise,
  Factor : Double;
  InterpolationFilterClass : TBZInterpolationFilterClass;
  InterpolationFilter : TBZCustomInterpolationFilter;
  fx, cx : Integer;
begin
  InterpolationFilter := nil;
  fx := Math.Floor(x);
  cx := fx + 1;

  PreviousCellNoise := inherited Noise1D(fx);
  NextCellNoise:= inherited Noise1D(cx);

  // Interpolation linéaire standard
  Factor := Frac(x);

  // Lissage
  if FSmooth then
  begin
    InterpolationFilterClass := GetBZInterpolationFilter(SmoothInterpolation);
    InterpolationFilter := InterpolationFilterClass.Create;
    Factor := InterpolationFilter.Filter(Factor);
    FreeAndNil(InterpolationFilter);
  end;
  //Interpolation
  Case NoiseInterpolation of
    nitLinear : Result := Lerp(PreviousCellNoise, NextCellNoise, Factor);
    nitCosine : Result := CosineLerp(PreviousCellNoise, NextCellNoise, Factor);
  end;
end;

Function TBZValueNoiseGenerator.Noise2D(x, y : Double) : Double;
var
  fx, cx, fy, cy : Integer;
  upperLeftCell,
  upperRightCell,
  lowerLeftCell,
  lowerRightCell,
  interpolatorX,
  interpolatorY,
  upperCells,
  lowerCells : Double;
  InterpolationFilterClass : TBZInterpolationFilterClass;
  InterpolationFilter : TBZCustomInterpolationFilter;

begin

  fx := Math.Floor(x);
  fy := Math.Floor(y);
  cx := fx + 1;  // Math.Ceil(x);
  cy := fy + 1; //Math.Ceil(y);

  upperLeftCell  := inherited Noise2D(fx, cy);
  upperRightCell := inherited Noise2D(cx, cy);
  lowerLeftCell  := inherited Noise2D(fx, fy);
  lowerRightCell := inherited Noise2D(cx, fy);

  // Interpolation linéaire standard
  interpolatorX :=  frac(x); //x - fx;
  interpolatorY :=  frac(y); //y - fy;

  if FSmooth then
  begin
    InterpolationFilterClass := GetBZInterpolationFilter(SmoothInterpolation);
    InterpolationFilter := InterpolationFilterClass.Create;
    interpolatorX := 1-InterpolationFilter.Filter(interpolatorX);
    interpolatorY := 1-InterpolationFilter.Filter(interpolatorY);
    FreeAndNil(InterpolationFilter);
  end;

  Case NoiseInterpolation of
    nitLinear :
    begin
      upperCells := Lerp(upperLeftCell, upperRightCell, interpolatorX);
      lowerCells := Lerp(lowerLeftCell, lowerRightCell, interpolatorX);
      Result := Lerp(lowerCells, upperCells, interpolatorY);
    end;
    nitCosine :
    begin
      upperCells := CosineLerp(upperLeftCell, upperRightCell, interpolatorX);
      lowerCells := CosineLerp(lowerLeftCell, lowerRightCell, interpolatorX);
      Result := CosineLerp(lowerCells, upperCells, interpolatorY);
    end;
  end;
end;

Function TBZValueNoiseGenerator.Noise3D(x, y, z : Double) : Double;
begin
  Result := inherited Noise3D(x, y, z);
end;

Function TBZValueNoiseGenerator.Noise4D(x, y, z, w : Double) : Double;
begin
  Result := inherited Noise4D(x, y, z, w);
end;

{%endregion%}

{%region=====[ TBZGradientNoiseGenerator ]=======================================================}

Constructor TBZGradientNoiseGenerator.Create;
begin
  inherited Create;
  Support := [ns1D, ns2D, ns3D, ns4D, nsInterpolation, nsSmooth];
end;

function TBZGradientNoiseGenerator.GradientNoise1D(x : Double) : Double;
begin
  Result := inherited Noise1D(x)*0.5  +  inherited Noise1D(x-1)*0.25  +  inherited Noise1D(x+1)*0.25;
end;

function TBZGradientNoiseGenerator.GradientNoise2D(x, y: Double) : Double;
var
  Corners, Sides, Center : Double;
begin
  Corners := ( inherited Noise2D(x-1, y-1) + inherited Noise2D(x+1, y-1) + inherited Noise2D(x-1, y+1) + inherited Noise2D(x+1, y+1) ) / 16;
  Sides   := ( inherited Noise2D(x-1, y)   + inherited Noise2D(x+1, y)   + inherited Noise2D(x, y-1)   + inherited Noise2D(x, y+1) ) /  8 ;
  Center  :=   inherited Noise2D(x, y) * 0.25;
  Result := corners + sides + center;
end;

Function TBZGradientNoiseGenerator.Noise1D(x : Double) : Double;
var
  PreviousCellNoise,
  NextCellNoise,
  Factor : Double;
  InterpolationFilterClass : TBZInterpolationFilterClass;
  InterpolationFilter : TBZCustomInterpolationFilter;
  fx,cx : Integer;

begin
  InterpolationFilter := nil;
  fx := Math.Floor(x);
  cx := fx + 1;

  PreviousCellNoise := GradientNoise1D(fx);
  NextCellNoise:= GradientNoise1D(cx);

  // Interpolation linéaire standard
  Factor := Frac(x);

  // Lissage du bruit
  if FSmooth then
  begin
    InterpolationFilterClass := GetBZInterpolationFilter(SmoothInterpolation);
    InterpolationFilter := InterpolationFilterClass.Create;
    Factor := InterpolationFilter.Filter(Factor);
    FreeAndNil(InterpolationFilter);
  end;

  // Interpolation du bruit
  Case NoiseInterpolation of
    nitLinear : Result := Lerp(PreviousCellNoise, NextCellNoise, Factor);
    nitCosine : Result := CosineLerp(PreviousCellNoise, NextCellNoise, Factor);
  end;
end;

Function TBZGradientNoiseGenerator.Noise2D(x, y : Double) : Double;
var
  fx, cx, fy, cy : Integer;
  fx0,fx1,fy0,fy1 : Double;
  upperLeftCell,
  upperRightCell,
  lowerLeftCell,
  lowerRightCell,
  interpolatorX,
  interpolatorY,
  upperCells,
  lowerCells : Double;
  InterpolationFilterClass : TBZInterpolationFilterClass;
  InterpolationFilter : TBZCustomInterpolationFilter;

begin

  fx:= Math.Floor(x);
  fy:= Math.Floor(y);

  cx := fx + 1;
  cy := fy + 1;

  upperLeftCell  := GradientNoise2D(fX, cY);
  upperRightCell := GradientNoise2D(cX, cY);
  lowerLeftCell  := GradientNoise2D(fX, fY);
  lowerRightCell := GradientNoise2D(cX, fY);

  // Interpolation linéaire standard
  interpolatorX :=  frac(x);
  interpolatorY :=  frac(y);

  // Lissage
  if FSmooth then
  begin
    InterpolationFilterClass := GetBZInterpolationFilter(SmoothInterpolation);
    InterpolationFilter := InterpolationFilterClass.Create;
    interpolatorX := 1-InterpolationFilter.Filter(interpolatorX);
    interpolatorY := 1-InterpolationFilter.Filter(interpolatorY);
    FreeAndNil(InterpolationFilter);
  end;

  Case NoiseInterpolation of
    nitLinear :
    begin
      upperCells := Lerp(upperLeftCell, upperRightCell, interpolatorX);
      lowerCells := Lerp(lowerLeftCell, lowerRightCell, interpolatorX);
      Result := Lerp(lowerCells, upperCells, interpolatorY);
    end;
    nitCosine :
    begin
      upperCells := CosineLerp(upperLeftCell, upperRightCell, interpolatorX);
      lowerCells := CosineLerp(lowerLeftCell, lowerRightCell, interpolatorX);
      Result := CosineLerp(lowerCells, upperCells, interpolatorY);
    end;
  end;
end;

Function TBZGradientNoiseGenerator.Noise3D(x, y, z : Double) : Double;
begin
  Result := inherited Noise3D(x, y, z);
end;

Function TBZGradientNoiseGenerator.Noise4D(x, y, z, w : Double) : Double;
begin
  Result := inherited Noise4D(x, y, z, w);
end;

{%endregion%}

{%region=====[ TBZImprovedPerlinNoiseGenerator ]=================================================}

Constructor TBZImprovedPerlinNoiseGenerator.Create;
begin
  inherited Create;
  Support := [ns1D, ns2D, ns3D, nsInterpolation, nsSmooth];
  Initialize(Seed);
end;

procedure TBZImprovedPerlinNoiseGenerator.SetSeed(const AValue : Int64);
begin
  inherited SetSeed(AValue);
  Initialize(Seed);
end;

function TBZImprovedPerlinNoiseGenerator.Lattice1D(ix : Integer; fx : Single) : Single;
var
  g : Integer;
begin
  g:=FPermutations[(ix and cPERLIN_TABLE_MASK)]*3;
  Result:=FGradients[g]*fx;
end;

function TBZImprovedPerlinNoiseGenerator.Lattice2D(ix, iy : Integer; fx, fy : Single) : Single;
var
  g : Integer;
begin
  g:=FPermutations[(ix+FPermutations[(iy+FPermutations[0]) and cPERLIN_TABLE_MASK]) and cPERLIN_TABLE_MASK]*3;
  Result:=FGradients[g]*fx+FGradients[g+1]*fy;
end;

function TBZImprovedPerlinNoiseGenerator.Lattice3D(ix, iy, iz : Integer; fx, fy, fz : Single) : Single;
var
  g : Integer;
begin
  g:=FPermutations[(ix+FPermutations[(iy+FPermutations[iz and cPERLIN_TABLE_MASK]) and cPERLIN_TABLE_MASK]) and cPERLIN_TABLE_MASK]*3;
  Result:=FGradients[g]*fx+FGradients[g+1]*fy+FGradients[g+2]*fz;
end;

procedure TBZImprovedPerlinNoiseGenerator.Initialize(aSeed : Int64);
var
   i, t, j : Integer;
   z, r : Single;
begin
   RandSeed := aSeed;

   // Generate random gradient vectors.
   for i:=0 to cPERLIN_TABLE_SIZE-1 do
   begin
     z:=1-2*Random;
     r:=Sqrt(1-z*z);
     SinCos(c2PI*Random, r, FGradients[i*3], FGradients[i*3+1]);
     FGradients[i*3+2]:=z;
   end;

   // Initialize permutations table
   for i:=0 to cPERLIN_TABLE_SIZE-1 do FPermutations[i]:=i;

   // Shake up
   for i:=0 to cPERLIN_TABLE_SIZE-1 do
   begin
     j:=Random(cPERLIN_TABLE_SIZE);
     t:=FPermutations[i];
     FPermutations[i]:=FPermutations[j];
     FPermutations[j]:=t;
   end;

end;

Function TBZImprovedPerlinNoiseGenerator.Noise1D(x : Double) : Double;
var
  ix, cx : Integer;
  fx0, fx1 : Single;
  LeftCell,
  RightCell,
  InterpolatorX : Double;
  InterpolationFilterClass : TBZInterpolationFilterClass;
  InterpolationFilter : TBZCustomInterpolationFilter;
begin
  ix := Math.Floor(x);
  cx := ix + 1;
  fx0 := frac(x);// x-ix;
  fx1 := fx0 - 1.0;

  interpolatorX := fx0;

  if FSmooth then
  begin
    InterpolationFilterClass := GetBZInterpolationFilter(SmoothInterpolation);
    InterpolationFilter := InterpolationFilterClass.Create;
    interpolatorX := 1.0 - InterpolationFilter.Filter(interpolatorX);
    FreeAndNil(InterpolationFilter);
  end;

  LeftCell  := Lattice1D(ix, fx0);
  RightCell := Lattice1D(cx, fx1);

  Case NoiseInterpolation of
     nitLinear :
     begin
       Result := Lerp(LeftCell, RightCell, interpolatorX);
     end;
     nitCosine :
     begin
       Result := CosineLerp(LeftCell, RightCell, interpolatorX);
     end;
   end;
end;

Function TBZImprovedPerlinNoiseGenerator.Noise2D(x, y : Double) : Double;
var
  ix, iy,cx,cy : Integer;
  fx0, fx1, fy0, fy1 : Single;
  upperLeftCell,
  upperRightCell,
  lowerLeftCell,
  lowerRightCell,
  interpolatorX,
  interpolatorY,
  upperCells,
  lowerCells : Double;
  vy0, vy1 : Single;
  InterpolationFilterClass : TBZInterpolationFilterClass;
  InterpolationFilter : TBZCustomInterpolationFilter;
begin

  ix := Math.Floor(x);
  cx := ix + 1;
  fx0 := frac(x);// x-ix;
  fx1 := fx0 - 1.0;

  iy := Math.Floor(y);
  cy := iy + 1;
  fy0 := frac(y); // y-iy;
  fy1 := fy0 - 1.0;

  interpolatorX := fx0;
  interpolatorY := fy0;

  upperLeftCell  := Lattice2D(ix, cy, fx0, fy1);
  upperRightCell := Lattice2D(cx, cy, fx1, fy1);
  lowerLeftCell  := Lattice2D(ix, iy, fx0, fy0);
  lowerRightCell := Lattice2D(cx, iy, fx1, fy0);
  // Lissage
  if FSmooth then
  begin
    InterpolationFilterClass := GetBZInterpolationFilter(SmoothInterpolation);
    InterpolationFilter := InterpolationFilterClass.Create;
    interpolatorX := 1-InterpolationFilter.Filter(interpolatorX);
    interpolatorY := 1-InterpolationFilter.Filter(interpolatorY);
    FreeAndNil(InterpolationFilter);
  end;

  Case NoiseInterpolation of
     nitLinear :
     begin
       upperCells := Lerp(upperLeftCell, upperRightCell, interpolatorX);
       lowerCells := Lerp(lowerLeftCell, lowerRightCell, interpolatorX);
       Result := Lerp(lowerCells, upperCells, interpolatorY);
     end;
     nitCosine :
     begin
       upperCells := CosineLerp(upperLeftCell, upperRightCell, interpolatorX);
       lowerCells := CosineLerp(lowerLeftCell, lowerRightCell, interpolatorX);
       Result := CosineLerp(lowerCells, upperCells, interpolatorY);
     end;
   end;
end;

Function TBZImprovedPerlinNoiseGenerator.Noise3D(x, y, z : Double) : Double;
var
   ix, iy, iz, cx,cy, cz : Integer;
   fx0, fx1, fy0, fy1, fz0, fz1  : Single;

   upperLeftCell,
   upperRightCell,
   lowerLeftCell,
   lowerRightCell,
   UpperCells,
   LowerCells,

   interpolatorX,
   interpolatorY,
   interpolatorZ  : Double;

   vy0, vy1, vz0, vz1 : Single;

   InterpolationFilterClass : TBZInterpolationFilterClass;
   InterpolationFilter : TBZCustomInterpolationFilter;
begin

   ix := Math.Floor(x);
   cx := ix + 1;
   fx0 := x - ix; //frac(x);
   fx1 := fx0 - 1.0;

   iy := Math.Floor(y);
   cy := iy + 1;
   fy0 := y - iy; //frac(y);
   fy1 := fy0 - 1.0;

   iz := Math.Floor(z);
   cz := iz + 1;
   fz0 := z - iz; //frac(z)
   fz1 := fz0 - 1.0;

   interpolatorX := fx0;
   interpolatorY := fy0;
   interpolatorZ := fz0;


   if FSmooth then
   begin
     InterpolationFilterClass := GetBZInterpolationFilter(SmoothInterpolation);
     InterpolationFilter := InterpolationFilterClass.Create;
     interpolatorX := InterpolationFilter.Filter(interpolatorX);
     interpolatorY := InterpolationFilter.Filter(interpolatorY);
     interpolatorZ := InterpolationFilter.Filter(interpolatorZ);
     FreeAndNil(InterpolationFilter);
   end;

   upperLeftCell  := Lattice3D(ix, cy, iz, fx0, fy1, fz0);
   upperRightCell := Lattice3D(cx, cy, iz, fx1, fy1, fz0);
   lowerLeftCell  := Lattice3D(ix, iy, iz, fx0, fy0, fz0);
   lowerRightCell := Lattice3D(cx, iy, iz, fx1, fy0, fz0);

   Case NoiseInterpolation of
     nitLinear :
     begin
       upperCells := Lerp(upperLeftCell, upperRightCell, interpolatorX);
       lowerCells := Lerp(lowerLeftCell, lowerRightCell, interpolatorX);
       vz0 := Lerp(lowerCells, upperCells, interpolatorY);
     end;
     nitCosine :
     begin
       upperCells := CosineLerp(upperLeftCell, upperRightCell, interpolatorX);
       lowerCells := CosineLerp(lowerLeftCell, lowerRightCell, interpolatorX);
       vz0 := CosineLerp(lowerCells, upperCells, interpolatorY);
     end;
   end;

   upperLeftCell  := Lattice3D(ix, cy, cz, fx0, fy1, fz1);
   upperRightCell := Lattice3D(cx, cy, cz, fx1, fy1, fz1);
   lowerLeftCell  := Lattice3D(ix, iy, cz, fx0, fy0, fz1);
   lowerRightCell := Lattice3D(cx, iy, cz, fx1, fy0, fz1);

   Case NoiseInterpolation of
     nitLinear :
     begin
       upperCells := Lerp(upperLeftCell, upperRightCell, interpolatorX);
       lowerCells := Lerp(lowerLeftCell, lowerRightCell, interpolatorX);
       vz1 := Lerp(lowerCells, upperCells, interpolatorY);
     end;
     nitCosine :
     begin
       upperCells := CosineLerp(upperLeftCell, upperRightCell, interpolatorX);
       lowerCells := CosineLerp(lowerLeftCell, lowerRightCell, interpolatorX);
       vz1 := CosineLerp(lowerCells, upperCells, interpolatorY);
     end;
   end;

   Case NoiseInterpolation of
     nitLinear :
     begin
       Result := Lerp(vz0, vz1, interpolatorZ);
     end;
     nitCosine :
     begin
       Result := CosineLerp(vz0, vz1, interpolatorZ);
     end;
   end;
end;

Function TBZImprovedPerlinNoiseGenerator.Noise4D(x, y, z, w : Double) : Double;
begin
  Result := inherited Noise4D(x, y, z, w);
end;

{%endregion%}

{%region=====[ TBZSimpleXNoiseGenerator ]========================================================}

function TBZSimplexNoiseGenerator.Grad1D(Hash : Integer; x : Double) : Double;
Var
  h : Integer;
  grad : Double;
begin
  h := hash and 15;
  grad := 1.0 + (h and 7);  // Valeur du Gradient 1.0, 2.0, ..., 8.0
  if ((h and 8) <> 0) then grad := -grad; // Signe aleatoire
  Result := (grad * x); // Multiplithe le gradient avec la distance
end;

function TBZSimplexNoiseGenerator.Grad2D(Hash : Integer; x, y : Double) : Double;
Var
  h : Integer;
  u,v : Double;
begin
  // Convertis les 3 bits de poid faible du hash code
	h := hash and 7;
  // Vers les 8 directions du "Gradient"
  if (h<4) then
  begin
    u:=x;
    v:=y;
  end
  else
  begin
    u:=y;
    v:=x;
  end;

  // Calcul le "Dot product" avec x,y
	if ((h and 1) <> 0 ) then
  begin
    result := -u;
  end
  else
  begin
    Result := u;
  end;
  if ((h and 2) <> 0 ) then
  begin
    Result := Result - (v+v);
  end
  else
  begin
    Result := Result + (v+v);
  end;
end;

function TBZSimplexNoiseGenerator.Grad3D(Hash : Integer; x, y, z : Double) : Double;
var
  h : Integer;
  u, v : Double;
begin
	h := hash and 15;     // Convert low 4 bits of hash code into 12 simple
	//float u =
  if (h < 8) then u := x
  else u := y; // gradient directions, and compute dot product.

	if (h < 4) then
    v := y
  else
    if (h = 12) or (h = 14) then v := x
    else v := z; // Fix repeats at h = 12 to 15

  if ((h and 1) <> 0) then result := -u
  else result := u;
  if ((h and 2) <> 0) then Result := Result - v
  else Result := Result + v;

end;

function TBZSimplexNoiseGenerator.Grad4D(Hash : Integer; x, y, z, w : Double) : Double;
begin
	//int h = hash & 31;      // Convert low 5 bits of hash code into 32 simple
	//float u = h < 24 ? x : y; // gradient directions, and compute dot product.
	//float v = h < 16 ? y : z;
	//float w = h < 8 ? z : t;
	//return ((h & 1) != 0 ? -u : u) + ((h & 2) != 0 ? -v : v) + ((h & 4) != 0 ? -w : w);
end;

Constructor TBZSimplexNoiseGenerator.Create;
begin
  inherited Create;
  Support := [ns1D, ns2D, ns3D, ns4D];
end;

Function TBZSimplexNoiseGenerator.Noise1D(x : Double) : Double;
var
  i0, i1 : Integer;
  vx, x0, x1, n0, n1, t0, t1 : Double;
begin
	i0 := Math.Floor(x);
	i1 := i0 + 1;
	x0 := x - i0;
	x1 := x0 - 1.0;

 	t0 := 1.0 - x0 * x0;
	t0 := t0 * t0;
	n0 := t0 * t0 * Grad1D(FPermutations[i0 and 255], x0);

	t1 := 1.0 - x1 * x1;
	t1 := t1 * t1;
	n1 := t1 * t1 * Grad1D(FPermutations[i1 and 255], x1);

	// The maximum value of this noise is 8*(3/4)^4 = 2.53125
	// A factor of 0.395 scales to fit exactly within [-1,1]
	Result := 0.395 * (n0 + n1);
end;

Function TBZSimplexNoiseGenerator.Noise2D(x, y : Double) : Double;
Const
  F2 : Double = 0.366025403; // F2 = 0.5*(sqrt(3.0)-1.0)  >> trianglesToSquares
  G2 : Double = 0.211324865; // G2 = (3.0-Math.sqrt(3.0))/6.0 >> squaresToTriangles
Var
  n0, n1, n2,
  s, xs, ys : Double;
  t, x0,y0 : Double;
  x1, y1, x2, y2 : Double;
  t0, t1, t2, tmp : Double;
  i, j,
  i1, j1,  // Offsets pour le second (milieu) coin du simplex à la coordonée (i,j)
  ii, jj : Integer;
begin

  // Incline l'espace d'entrée pour déterminer dans quelle cellule du simplex nous sommes
	s := (x + y) * F2;
	xs := x + s;
	ys := y + s;
	i := Math.Floor(xs);
	j := Math.Floor(ys);

	t := (i + j) * G2;
	X0 := i - t; // Ré-incline l’origine de la cellule dans l’espace (x, y)
	Y0 := j - t;
	x0 := x - X0; // distances depuis la cellule d'origine
	y0 := y - Y0;

	// Pour le cas 2D, la forme du simplexe est un triangle équilatéral.
  // On Détermine dans quel simplex nous sommes.
	if (x0 > y0) then // Bas du triangle, ordre XY : (0,0)->(1,0)->(1,1)
  begin
    i1 := 1;
    j1 := 0;
  end
  else
  begin   // Haut du triangle, ordre YX : (0,0)->(0,1)->(1,1)
    i1 := 0;
    j1 := 1;
  end;

	// Un pas de (1,0) dans (i, j) signifie un pas de (1-c, -c) dans (x, y), et
  // un pas de (0,1) dans (i, j) signifie un pas de (-c, 1-c) dans (x, y), où
	// c = (3-sqrt(3))/6
  //  = (Mathf.Sqrt(3f) - 1f) / 2f;

  // Offsets pour le coin du milieu pour les coordonnées (x, y) non inclinées
	x1 := x0 - i1 + G2;
	y1 := y0 - j1 + G2;
  // Offsets pour le dernier coin pour les coordonnées (x, y) non inclinées
  tmp := 2.0 * G2;
	x2 := x0 - 1.0 + tmp;
	y2 := y0 - 1.0 + tmp;

	// S'assure que les indices ne dépassent pas 255, pour éviter un index hors limites dans le tableau FPermutations
	ii := i mod 255;
	jj := j mod 255;

	// Calculer la contribution des trois coins
	t0 := 0.5 - x0 * x0 - y0 * y0;
	if (t0 < 0.0) then
  begin
    n0 := 0.0;
  end
  else
	begin
		t0 := t0 * t0;
		n0 := t0 * t0 * Grad2D(FPermutations[ii + FPermutations[jj]], x0, y0);
  end;

	t1 := 0.5 - x1 * x1 - y1 * y1;
	if (t1 < 0.0) then
  begin
    n1 := 0.0;
  end
  else
	begin
		t1 := t1 * t1;
		n1 := t1 * t1 * Grad2D(FPermutations[ii + i1 + FPermutations[jj + j1]], x1, y1);
  end;

	t2 := 0.5 - x2 * x2 - y2 * y2;
	if (t2 < 0.0) then
  begin
    n2 := 0.0;
  end
  else
	begin
		t2 := t2 * t2;
		n2 := t2 * t2 * Grad2D(FPermutations[ii + 1 + FPermutations[jj + 1]], x2, y2);
  end;

	// Ajoutes les contributions de chaque coin pour obtenir la valeur de bruit finale.
  // Le résultat est mis à l'échelle pour renvoyer des valeurs dans l'intervalle [-1,1].
	Result := 40.0 * (n0 + n1 + n2); // Le facteur d'échelle est préliminaire!
end;

Function TBZSimplexNoiseGenerator.Noise3D(x, y, z : Double) : Double;
const
  F3 : Double = 0.333333333;
	G3 : Double = 0.166666667;
  G32 : Double = 0.166666667 * 2.0;
  G33 : Double = 0.166666667 * 3.0;

Var
  n0, n1, n2, n3,
  s, xs, ys, zs : Double;
  t, x0, y0, z0 : Double;
  x1, y1, z1, x2, y2, z2, x3, y3, z3 : Double;
  t0, t1, t2, t3, tmp : Double;
  i, j, k,
  i1, j1, k1, i2, j2, k2,
  ii, jj, kk : Integer;
begin

  s := (x + y + z) * F3;
	xs := x + s;
	ys := y + s;
	zs := z + s;

  i := Math.Floor(xs);
	j := Math.Floor(ys);
	k := Math.Floor(zs);

	t := (i + j + k) * G3;
	X0 := i - t;
	Y0 := j - t;
	Z0 := k - t;

	x0 := x - X0;
	y0 := y - Y0;
	z0 := z - Z0;

	// For the 3D case, the simplex shape is a slightly irregular tetrahedron.
	// Determine which simplex we are in.

	if (x0 >= y0) then
  begin
	 	if (y0 >= z0) then
    begin
	    i1 := 1;
      j1 := 0;
      k1 := 0;
      i2 := 1;
      j2 := 1;
      k2 := 0;
    end
		else
    begin
      if (x0 >= z0) then
      begin
        i1 := 1;
        j1 := 0;
        k1 := 0;
        i2 := 1;
        j2 := 0;
        k2 := 1;
      end
	  	else
      begin
        i1 := 0;
        j1 := 0;
        k1 := 1;
        i2 := 1;
        j2 := 0;
        k2 := 1;
      end;
	  end;
  end
	else
  begin
		if (y0 < z0) then
    begin
      i1 := 0;
      j1 := 0;
      k1 := 1;
      i2 := 0;
      j2 := 1;
      k2 := 1;
    end
		else
    begin
      if (x0 < z0) then
      begin
        i1 := 0;
        j1 := 1;
        k1 := 0;
        i2 := 0;
        j2 := 1;
        k2 := 1;
      end
	  	else
      begin
        i1 := 0;
        j1 := 1;
        k1 := 0;
        i2 := 1;
        j2 := 1;
        k2 := 0;
      end;
	  end;
  end;


	// A step of (1,0,0) in (i,j,k) means a step of (1-c,-c,-c) in (x,y,z),
	// a step of (0,1,0) in (i,j,k) means a step of (-c,1-c,-c) in (x,y,z), and
	// a step of (0,0,1) in (i,j,k) means a step of (-c,-c,1-c) in (x,y,z), where
	// c = 1/6.

  x1 := x0 - i1 + G3;
	y1 := y0 - j1 + G3;
	z1 := z0 - k1 + G3;
	x2 := x0 - i2 + G32;
	y2 := y0 - j2 + G32;
	z2 := z0 - k2 + G32;
	x3 := x0 - 1.0 + G33;
	y3 := y0 - 1.0 + G33;
	z3 := z0 - 1.0 + G33;

	ii := i mod 255;
	jj := j mod 255;
	kk := k mod 255;

	// Calcul de la contribution des quatres coins
	t0 := 0.6 - x0 * x0 - y0 * y0 - z0 * z0;
	if (t0 < 0.0) then n0 := 0.0
	else
	begin
		t0 := t0 * t0;
		n0 := t0 * t0 * Grad3D(FPermutations[ii + FPermutations[jj + FPermutations[kk]]], x0, y0, z0);
	end;

	t1 := 0.6 - x1 * x1 - y1 * y1 - z1 * z1;
	if (t1 < 0.0) then n1 := 0.0
  else
  begin
		t1 := t1 * t1;
		n1 := t1 * t1 * Grad3D(FPermutations[ii + i1 + FPermutations[jj + j1 + FPermutations[kk + k1]]], x1, y1, z1);
	end;

	t2 := 0.6 - x2 * x2 - y2 * y2 - z2 * z2;
	if (t2 < 0.0) then n2 := 0.0
	else
	begin
		t2 := t2 * t2;
		n2 := t2 * t2 * Grad3D(FPermutations[ii + i2 + FPermutations[jj + j2 + FPermutations[kk + k2]]], x2, y2, z2);
	end;

	t3 := 0.6 - x3 * x3 - y3 * y3 - z3 * z3;
	if (t3 < 0.0) then n3 := 0.0
	else
	begin
		t3 := t3 * t3;
		n3 := t3 * t3 * Grad3D(FPermutations[ii + 1 + FPermutations[jj + 1 + FPermutations[kk + 1]]], x3, y3, z3);
	end;

	Result := 32.0 * (n0 + n1 + n2 + n3);
end;

Function TBZSimplexNoiseGenerator.Noise4D(x, y, z, w : Double) : Double;
Const
  F4 : Double = ((2.23606797 - 1.0) / 4.0);
	G4 : Double = ((5.0 - 2.23606797) / 20.0);
  cSimplex4D : Array[0..63,0..3] of byte = (
    (0,1,2,3),(0,1,3,2),(0,0,0,0),(0,2,3,1),(0,0,0,0),(0,0,0,0),(0,0,0,0),(1,2,3,0),
    (0,2,1,3),(0,0,0,0),(0,3,1,2),(0,3,2,1),(0,0,0,0),(0,0,0,0),(0,0,0,0),(1,3,2,0),
    (0,0,0,0),(0,0,0,0),(0,0,0,0),(0,0,0,0),(0,0,0,0),(0,0,0,0),(0,0,0,0),(0,0,0,0),
    (1,2,0,3),(0,0,0,0),(1,3,0,2),(0,0,0,0),(0,0,0,0),(0,0,0,0),(2,3,0,1),(2,3,1,0),
    (1,0,2,3),(1,0,3,2),(0,0,0,0),(0,0,0,0),(0,0,0,0),(2,0,3,1),(0,0,0,0),(2,1,3,0),
    (0,0,0,0),(0,0,0,0),(0,0,0,0),(0,0,0,0),(0,0,0,0),(0,0,0,0),(0,0,0,0),(0,0,0,0),
    (2,0,1,3),(0,0,0,0),(0,0,0,0),(0,0,0,0),(3,0,1,2),(3,0,2,1),(0,0,0,0),(3,1,2,0),
    (2,1,0,3),(0,0,0,0),(0,0,0,0),(0,0,0,0),(3,1,0,2),(0,0,0,0),(3,2,0,1),(3,2,1,0));
begin
	//FN_DECIMAL n0, n1, n2, n3, n4;
	//FN_DECIMAL t = (x + y + z + w) * F4;
	//int i = FastFloor(x + t);
	//int j = FastFloor(y + t);
	//int k = FastFloor(z + t);
	//int l = FastFloor(w + t);
	//t = (i + j + k + l) * G4;
	//FN_DECIMAL X0 = i - t;
	//FN_DECIMAL Y0 = j - t;
	//FN_DECIMAL Z0 = k - t;
	//FN_DECIMAL W0 = l - t;
	//FN_DECIMAL x0 = x - X0;
	//FN_DECIMAL y0 = y - Y0;
	//FN_DECIMAL z0 = z - Z0;
	//FN_DECIMAL w0 = w - W0;
 //
	//int c = (x0 > y0) ? 32 : 0;
	//c += (x0 > z0) ? 16 : 0;
	//c += (y0 > z0) ? 8 : 0;
	//c += (x0 > w0) ? 4 : 0;
	//c += (y0 > w0) ? 2 : 0;
	//c += (z0 > w0) ? 1 : 0;
	//c <<= 2;
 //
	//int i1 = SIMPLEX_4D[c] >= 3 ? 1 : 0;
	//int i2 = SIMPLEX_4D[c] >= 2 ? 1 : 0;
	//int i3 = SIMPLEX_4D[c++] >= 1 ? 1 : 0;
	//int j1 = SIMPLEX_4D[c] >= 3 ? 1 : 0;
	//int j2 = SIMPLEX_4D[c] >= 2 ? 1 : 0;
	//int j3 = SIMPLEX_4D[c++] >= 1 ? 1 : 0;
	//int k1 = SIMPLEX_4D[c] >= 3 ? 1 : 0;
	//int k2 = SIMPLEX_4D[c] >= 2 ? 1 : 0;
	//int k3 = SIMPLEX_4D[c++] >= 1 ? 1 : 0;
	//int l1 = SIMPLEX_4D[c] >= 3 ? 1 : 0;
	//int l2 = SIMPLEX_4D[c] >= 2 ? 1 : 0;
	//int l3 = SIMPLEX_4D[c] >= 1 ? 1 : 0;
 //
	//FN_DECIMAL x1 = x0 - i1 + G4;
	//FN_DECIMAL y1 = y0 - j1 + G4;
	//FN_DECIMAL z1 = z0 - k1 + G4;
	//FN_DECIMAL w1 = w0 - l1 + G4;
	//FN_DECIMAL x2 = x0 - i2 + 2 * G4;
	//FN_DECIMAL y2 = y0 - j2 + 2 * G4;
	//FN_DECIMAL z2 = z0 - k2 + 2 * G4;
	//FN_DECIMAL w2 = w0 - l2 + 2 * G4;
	//FN_DECIMAL x3 = x0 - i3 + 3 * G4;
	//FN_DECIMAL y3 = y0 - j3 + 3 * G4;
	//FN_DECIMAL z3 = z0 - k3 + 3 * G4;
	//FN_DECIMAL w3 = w0 - l3 + 3 * G4;
	//FN_DECIMAL x4 = x0 - 1 + 4 * G4;
	//FN_DECIMAL y4 = y0 - 1 + 4 * G4;
	//FN_DECIMAL z4 = z0 - 1 + 4 * G4;
	//FN_DECIMAL w4 = w0 - 1 + 4 * G4;
 //
	//t = (FN_DECIMAL)0.6 - x0 * x0 - y0 * y0 - z0 * z0 - w0 * w0;
	//if (t < 0) n0 = 0;
	//else
	//{
	//	t *= t;
	//	n0 = t * t * GradCoord4D(seed, i, j, k, l, x0, y0, z0, w0);
	//}
	//t = (FN_DECIMAL)0.6 - x1 * x1 - y1 * y1 - z1 * z1 - w1 * w1;
	//if (t < 0) n1 = 0;
	//else
	//{
	//	t *= t;
	//	n1 = t * t * GradCoord4D(seed, i + i1, j + j1, k + k1, l + l1, x1, y1, z1, w1);
	//}
	//t = (FN_DECIMAL)0.6 - x2 * x2 - y2 * y2 - z2 * z2 - w2 * w2;
	//if (t < 0) n2 = 0;
	//else
	//{
	//	t *= t;
	//	n2 = t * t * GradCoord4D(seed, i + i2, j + j2, k + k2, l + l2, x2, y2, z2, w2);
	//}
	//t = (FN_DECIMAL)0.6 - x3 * x3 - y3 * y3 - z3 * z3 - w3 * w3;
	//if (t < 0) n3 = 0;
	//else
	//{
	//	t *= t;
	//	n3 = t * t * GradCoord4D(seed, i + i3, j + j3, k + k3, l + l3, x3, y3, z3, w3);
	//}
	//t = (FN_DECIMAL)0.6 - x4 * x4 - y4 * y4 - z4 * z4 - w4 * w4;
	//if (t < 0) n4 = 0;
	//else
	//{
	//	t *= t;
	//	n4 = t * t * GradCoord4D(seed, i + 1, j + 1, k + 1, l + 1, x4, y4, z4, w4);
	//}
 //
	//return 27 * (n0 + n1 + n2 + n3 + n4);
end;

{%endregion%}



{%region=====[ TBZCustomFractalNoiseGenerator ]==================================================}
//Constructor  TBZCustomFractalNoiseGenerator.Create;
//begin
//  inherited Create;
//  FLacunarity := 2.0;
//  FGain := 0.5;
//  FOctaves := 3;
//  FAmplitude := 2;
//  FFractalNoiseType := fntFBM;
//  FNoiseType := bntPerlin; //bntValue, bntSimpleX, bntOpenSimpleX, bntCellular
//  FCellularNoiseType := cntWorley; //cntVoronoi, cntStandard
//end;
{%endregion%}

End.
