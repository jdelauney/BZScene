(*

  L'unité BZTypesHelper surcharge les assistants originaux de FPC pour les types. @Br
  Elle ajoute des fonctions supplémentaires pour tous les types d’assistants. @br
  Elle inclut également un assistant spécialisée pour le type TDateTime.

  -------------------------------------------------------------------------------------------------------------

  @created(2018-04-13)
  @author(J.Delauney (BeanzMaster))

  Historique : @br
  @unorderedList(
    @item(Last Update : 21/04/2018  )
    @item(25/11/2017 : Creation  )
  )

  -------------------------------------------------------------------------------------------------------------

  @bold(Notes :)@br
    L'assistant pour les chaines de caractères traite directement avec UTF8

  -------------------------------------------------------------------------------------------------------------

  @bold(Dépendances) : None

  -------------------------------------------------------------------------------------------------------------

  @bold(Credits :)@br
    @unorderedList(
      @item(FPC/Lazarus)
      @item(J.Delauney)
    )

  -------------------------------------------------------------------------------------------------------------

  LICENCE : MPL/GPL

  ------------------------------------------------------------------------------------------------------------- *)
Unit BZTypesHelpers;

//==============================================================================
// /!\ Dont' Work with trunk versions
{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$modeswitch typehelpers}
{.$mode delphi} // Work with trunk version only if Delphi Mode is defined
{$i ..\bzscene_options.inc}
//==============================================================================

Interface

Uses
  Classes, SysUtils
  {$IFDEF WINDOWS}
  , Windows
  {$ENDIF};

//==============================================================================

Const
  { Nombre aléatoire pour le calcul du 'hash' d'une chaine de caractères}
  cHash1 = 130469;
  { Constante pour le calcul du 'hash' d'une chaine de caractères. Plus la taille est grande, plus le risque de collision est faible }
  cHash2 = MaxInt Shr 4;

Type
  { Assistant pour le type Boolean}
  TBooleanHelper = type helper for Boolean
  public
    { Convertis le boolean en chaîne de caractères }
    function ToString(Const DefTrue : String = 'True'; Const defFalse : String = 'False'): String; 
  end;

  { Assistant pour le type Char }
  TCharHelper = type helper for Char
  public
    { Vérifie si le caractère est un Alpha }
    function IsAlpha: Boolean;  
    { Vérifie si le caractère est un Numérique }
    function IsNumeric: Boolean;  
    { Retourne le code du caractère }
    function ToCharCode : Integer;  
    { Convertie le caractère en majuscule }
    function ToUpper : Char;  
    { Convertie le caractère en minuscule }
    function ToLower : Char;  

  end;

  { Assistant pour le type Byte }
  TByteHelper =  type helper for Byte
  public
    const
      MaxValue = 255;  // < Valeur maximum pour le type Byte
      MinValue = 0;    // < Valeur minimum pour le type Byte
      NormalizedRatio : Single = 1/255;   // < Ratio normalisé pour le type Byte
  public
    { Convertir une chaîne de caractères en sa représentation en Byte@br
      Retourne True an cas de succes sinon retourne False et définie la valeur avec la variable DefValue }
    function Parse(Num : String; Const defValue:Byte=0):Boolean;  overload;
    { Convertir la valeur de la variable Byte en chaîne de caractères }
    function ToString: string; 
    { Retourne la plus petite valeur entre la valeur de la variable Byte et le paramètre "vMin" }
    function Min(vMin:Byte):Byte; 
    { Retourne la plus grande valeur entre la valeur de la variable Byte et le paramètre "vMax" }
    function Max(vMax:Byte):Byte; 
    { Vérifie si la valeur de la variable Byte est comprise et ou égale à l'interval définit par les paramètres "Low et High"}
    function IsInRange(Low, High: Byte): Boolean; 
    { Retourne un nombre aléatoire dans l'interval définit par les constantes "MinValue et MaxValue" }
    function Random:Byte;  
    { Retourne un nombre aléatoire dans l'interval definit par les paramètres "vMin et vMax" }
    function RandomRange(vMin, vMax :Byte):Byte;
    { S'assure que la valeur de la variable Byte est comprise entre les paramètres "vMin et vMax" et renvoie la valeur}
    function Clamp(vMin,vMax:Byte):Byte; 
    { Retourne la représentation Booleenne de la variable Byte. Retourne TRUE si la valeur est plus grande que zero. Sinon retourne FALSE }
    function ToBoolean : Boolean; 
    { Retourne la représentation hexadécimal sous forme de chaine de caractères }
    Function ToHexString: string; 
    { Retourne la valeur normalisée de la variable Byte. Dans l'interval [0..1] }
    function Normalized : Single; 
    { Retourne la valeur réciproque de la variable Byte (1/Value) }
    function Reciprocal: Single; 

  end;

  { Assistant pour le type ShortInt }
  TShortIntHelper = type helper for ShortInt
  public
    const
      MaxValue = 127;   // < Valeur maximum pour le type ShortInt
      MinValue = -128;  // < Valeur minimum pour le type ShortInt
      NormalizedRatio : single = 1 / 128; // < Ratio normalisé pour le type ShortInt
  public
    { Convertir une chaîne de caractères en sa représentation en ShortInt@br
      Retourne True an cas de succes sinon retourne False et définie la valeur avec la variable DefValue }
    function Parse(Num : String; Const defValue:ShortInt=0):Boolean; 
    { Convertir la valeur de la variable ShortInt en chaîne de caractères }
    function ToString(Const Formatted : Boolean = False): string; 
    { Retourne  la plus petite valeur entre la valeur de la variable ShortInt et le paramètre "vMin" }
    function Min(vMin:ShortInt):ShortInt; 
    { Retourne la plus grande valeur entre la valeur de la variable ShortInt et le paramètre "vMax" }
    function Max(vMax:ShortInt):ShortInt; 
    { Vérifie si la valeur de la variable ShortInt est comprise et ou égale à l'interval définit par les paramètres "Low et High"}
    function IsInRange(Low, High: ShortInt): Boolean; 
    { Retourne un nombre aléatoire dans l'interval définit par les constantes "MinValue et MaxValue" }
    function Random:ShortInt; 
    { Retourne un nombre aléatoire dans l'interval definit par les paramètres "vMin et vMax" }
    function RandomRange(vMin, vMax :ShortInt):ShortInt; 
    { S'assure que la valeur de la variable ShortInt est comprise entre les paramètres "vMin et vMax" et renvoie la valeur}
    function Clamp(vMin,vMax:ShortInt):ShortInt; 
    { Renvois le signe de la valeur avec la convention (-1, 0, +1) }
    function Sign:ShortInt; 
    { Retourne la représentation Byte de la variable Int64 comprise dans l'interval [0..255] }
    function ToByte : Byte; 
    { Retourne la représentation Booleenne de la variable ShortInt. Retourne TRUE si la valeur est plus grande que zero. Sinon retourne FALSE }
    function ToBoolean : Boolean; 
    { Retourne la representation hexadécimale de la valeur ShortInt sous forme de chaine de caractères }
    function ToHexString: string; 
    { Retourne la valeur normalisée du ShortInt comprise dans l'interval [0..1] }
    function Normalized : Single; 
    { Retourne la valeur réciproque de la variable ShortInt (1/Value) }
    function Reciprocal : Single; 

  end;

  { Assistant pour le type SmallInt }
  TSmallIntHelper =  type helper for SmallInt
  public
    const
      MaxValue = 32767;
      MinValue = -32768;
      NormalizedRatio : single = 1 / 32767;
  public
    { Convertir une chaîne de caractères en sa représentation en SmallInt@br
      Retourne True an cas de succes sinon retourne False et définie la valeur avec la variable DefValue }
    function Parse(Num : String; Const defValue:SmallInt=0):Boolean; 
    { Convertir la valeur de la variable SmallInt en chaîne de caractères }
    function ToString(Const Formatted : Boolean = False): string; 
    { Retourne la plus petite valeur entre la valeur de la variable SmallInt et le paramètre "vMin" }
    function Min(vMin:SmallInt):SmallInt; 
    { Retourne la plus grande valeur entre la valeur de la variable SmallInt et le paramètre "vMax" }
    function Max(vMax:SmallInt):SmallInt; 
    { Vérifie si la valeur de la variable SmallInt est comprise et ou égale à l'interval définit par les paramètres "Low et High"}
    function IsInRange(Low, High: SmallInt): Boolean; 
    { Retourne un nombre aléatoire dans l'interval définit par les constantes "MinValue et MaxValue" }
    function Random:SmallInt; 
    { Retourne un nombre aléatoire dans l'interval definit par les paramètres "vMin et vMax" }
    function RandomRange(vMin, vMax :SmallInt):SmallInt; 
    { S'assure que la valeur de la variable SmallInt est comprise entre les paramètres "vMin et vMax" et renvoie la valeur}
    function Clamp(vMin,vMax:SmallInt):SmallInt; 
    { Renvois le signe de la valeur avec la convention (-1, 0, +1) }
    function Sign:SmallInt; 
    { Retourne la représentation Byte de la variable Int64 comprise dans l'interval [0..255] }
    function ToByte : Byte; 
    { Retourne la représentation Booleenne de la variable SmallInt. Retourne TRUE si la valeur est plus grande que zero. Sinon retourne FALSE }
    function ToBoolean : Boolean; 
    { Retourne la representation hexadécimale de la valeur SmallInt sous forme de chaine de caractères }
    function ToHexString: string; 
    { Retourne la valeur normalisée du SmallInt comprise dans l'interval [0..1] }
    function Normalized : Single; 
    { Retourne la valeur réciproque de la variable SmallInt (1/value) }
    function Reciprocal : Single; 
    { Retourne @True si la valeur est une puissance de deux. Si non retourne @False }
    function IsPowerOfTwo : Boolean;
    { Retourne la puissance de deux suivante }
    function NextPowerOfTwo:SmallInt;
    { Retourne la puissance de deux précédente }
    function PreviousPowerOfTwo:SmallInt;
  end;

  { Assistant pour le type Word }
  TWordHelper =  type helper for Word
  public
    const
      MaxValue = 65535;
      MinValue = 0;
      NormalizedRatio : single = 1 / 65535;
  public
    { Convertir une chaîne de caractères en sa représentation en Word@br
      Retourne True an cas de succes sinon retourne False et définie la valeur avec la variable DefValue }
    function Parse(Num : String; Const defValue:Word=0):Boolean; 
    { Convertir la valeur de la variable Word en chaîne de caractères }
    function ToString(Const Formatted : Boolean = False): string;
    { Retourne la plus petite valeur entre la valeur de la variable Word et le paramètre "vMin" }
    function Min(vMin:Word):Word; 
    { Retourne la plus grande valeur entre la valeur de la variable Word et le paramètre "vMax" }
    function Max(vMax:Word):Word; 
    { Vérifie si la valeur de la variable Word est comprise et ou égale à l'interval définit par les paramètres "Low et High"}
    function IsInRange(Low, High: Word): Boolean; 
    { Retourne un nombre aléatoire dans l'interval définit par les constantes "MinValue et MaxValue" }
    function Random:Word; 
    { Retourne un nombre aléatoire dans l'interval definit par les paramètres "vMin et vMax" }
    function RandomRange(vMin, vMax : Word):Word; 
    { S'assure que la valeur de la variable Word est comprise entre les paramètres "vMin et vMax" et renvoie la valeur}
    function Clamp(vMin,vMax:Word):Word; 
    { Retourne la représentation Byte de la variable Int64 comprise dans l'interval [0..255] }
    function ToByte : Byte; 
    { Retourne la représentation Booleenne de la variable Word. Retourne TRUE si la valeur est plus grande que zero. Sinon retourne FALSE }
    function ToBoolean : Boolean; 
    { Retourne la representation hexadécimale de la valeur Word sous forme de chaine de caractères }
    function ToHexString: string; 
    { Retourne la valeur normalisée du Word comprise dans l'interval [0..1] }
    function Normalized : Single; 
    { Retourne la valeur réciproque de la variable Word (1/value) }
    function Reciprocal : Single; 
    { Retourne @True si la valeur est une puissance de deux. Si non retourne @False }
    function IsPowerOfTwo : Boolean;
    { Retourne la puissance de deux suivante }
    function NextPowerOfTwo:Word;
    { Retourne la puissance de deux précédente }
    function PreviousPowerOfTwo:Word;
  end;

  { Assistant pour le type Cardinal (Note : Cardinal = DWord = LongWord ) }
  TCardinalHelper =  type helper for Cardinal
  public
    const
      MaxValue = 4294967295;
      MinValue = 0;
      NormalizedRatio : single = 1 / 4294967295;
  public
    { Convertir une chaîne de caractères en sa représentation en Cardinal@br
      Retourne True an cas de succes sinon retourne False et définie la valeur avec la variable DefValue }
    function Parse(Num : String; Const defValue:Cardinal=0):Boolean; 
    { Convertir la valeur de la variable Cardinal en chaîne de caractères }
    function ToString(Const Formatted : Boolean = False): string; 
    { Retourne la plus petite valeur entre la valeur de la variable Cardinal et le paramètre "vMin" }
    function Min(vMin:Cardinal):Cardinal; 
    { Retourne la plus grande valeur entre la valeur de la variable Cardinal et le paramètre "vMax" }
    function Max(vMax:Cardinal):Cardinal; 
    { Vérifie si la valeur de la variable Cardinal est comprise et ou égale à l'interval définit par les paramètres "Low et High"}
    function IsInRange(Low, High: Cardinal): Boolean; 
    { Retourne un nombre aléatoire dans l'interval définit par les constantes "MinValue et MaxValue" }
    function Random:Cardinal; 
    { Retourne un nombre aléatoire dans l'interval definit par les paramètres "vMin et vMax" }
    function RandomRange(vMin, vMax :Cardinal):Cardinal; 
    { S'assure que la valeur de la variable Cardinal est comprise entre les paramètres "vMin et vMax" et renvoie la valeur}
    function Clamp(vMin,vMax:Cardinal):Cardinal; 
    { Retourne la représentation Byte de la variable Int64 comprise dans l'interval [0..255] }
    function ToByte : Byte; 
    { Retourne la représentation Booleenne de la variable Cardinal. Retourne TRUE si la valeur est plus grande que zero. Sinon retourne FALSE }
    function ToBoolean : Boolean; 
    { Retourne la representation hexadécimale de la valeur Cardinal sous forme de chaine de caractères }
    function ToHexString: string; 
    { Retourne la valeur normalisée du Cardinal comprise dans l'interval [0..1] }
    function Normalized : Single; 
    { Retourne la valeur réciproque de la variable Cardinal (1/value) }
    function Reciprocal : Single; 
    { Retourne @True si la valeur est une puissance de deux. Si non retourne @False }
    function IsPowerOfTwo : Boolean;
    { Renvois la puissance de deux suivante }
    function NextPowerOfTwo:Cardinal;
    { Renvois la puissance de deux précédente }
    function PreviousPowerOfTwo:Cardinal;
  end;

  { Assistant pour le type Integer}
  TIntegerHelper =  type helper for Integer
  public
    const
      MaxValue = 2147483647;
      MinValue = -2147483648;
      NormalizedRatio : single = 1 / 2147483647;
  public
    { Convertir une chaîne de caractères en sa représentation en Integer @br
      Retourne True an cas de succes sinon retourne False et définie la valeur avec la variable DefValue }
    function Parse(Num : String; Const defValue:Integer=0):Boolean; 
    { Convertir la valeur de la variable Integer en chaîne de caractères }
    function ToString(Const Formatted : Boolean = False): string; 
    { Retourne la plus petite valeur entre la valeur de la variable Integer et le paramètre "vMin" }
    function Min(vMin:Integer):Integer; 
    { Retourne la plus grande valeur entre la valeur de la variable Integer et le paramètre "vMax" }
    function Max(vMax:Integer):Integer; 
    { Vérifie si la valeur de la variable Integer est comprise et ou égale à l'interval définit par les paramètres "Low et High"}
    function IsInRange(Low, High: Integer): Boolean; 
    { Retourne un nombre aléatoire dans l'interval définit par les constantes "MinValue et MaxValue" }
    function Random:Integer;
    { Retourne un nombre aléatoire dans l'interval definit par les paramètres "vMin et vMax" }
    function RandomRange(vMin, vMax :Integer):Integer;
    { S'assure que la valeur de la variable Integer est comprise entre les paramètres "vMin et vMax" et renvoie la valeur}
    function Clamp(vMin,vMax:Integer):Integer;
    { Renvois le signe de la valeur avec la convention (-1, 0, +1) }
    function Sign:Integer;
    { Retourne la représentation Byte de la variable Int64 comprise dans l'interval [0..255] }
    function ToByte : Byte;
    { Retourne la représentation Booleenne de la variable Integer. Retourne TRUE si la valeur est plus grande que zero. Sinon retourne FALSE }
    function ToBoolean : Boolean;
    { Retourne la representation hexadécimale de la valeur Integer sous forme de chaine de caractèresn }
    function ToHexString: string;
    { Retourne la valeur normalisée de l'integer comprise dans l'interval [0..1] }
    function Normalized : Single;
    { Retourne la valeur réciproque de la variable Integer (1/value) }
    function Reciprocal : Single;
    { Retourne @True si la valeur est une puissance de deux. Si non retourne @False }
    function IsPowerOfTwo : Boolean;
    { Renvois la puissance de deux suivante }
    function NextPowerOfTwo:Integer;
    { Renvois la puissance de deux précédente }
    function PreviousPowerOfTwo:Integer;
  end;

  { Assistant pour le type Int64 }
  TInt64Helper =  type helper for Int64
  public
    const
      MaxValue = 9223372036854775807;
      MinValue = -9223372036854775808;
  public
    { Convertir une chaîne de caractères en sa représentation en Int64@br
      Retourne @True an cas de succes sinon retourne False et définie la valeur avec la variable DefValue }
    function Parse(Num : String; Const defValue:Int64=0):Boolean;
    { Convertir la valeur de la variable Int64 en chaîne de caractères }
    function ToString(Const Formatted : Boolean = False): string;
    { Retourne la plus petite valeur entre la valeur de la variable Int64 et le paramètre "vMin" }
    function Min(vMin:Int64):Int64;
    { Retourne la plus grande valeur entre la valeur de la variable Int64 et le paramètre "vMax" }
    function Max(vMax:Int64):Int64;
    { Vérifie si la valeur de la variable Int64 est comprise et ou égale à l'interval définit par les paramètres "Low et High"}
    function IsInRange(Low, High: Int64): Boolean;
    { Retourne un nombre aléatoire dans l'interval définit par les constantes "MinValue et MaxValue" }
    function Random:Int64;
    { Retourne un nombre aléatoire dans l'interval definit par les paramètres "vMin et vMax" }
    function RandomRange(vMin, vMax :Int64):Int64;
    { S'assure que la valeur de la variable Int64 est comprise entre les paramètres "vMin et vMax" et renvoie la valeur}
    function Clamp(vMin,vMax:Int64):Int64;
    { Renvois le signe de la valeur avec la convention (-1, 0, +1) }
    function Sign:Int64;
    { Retourne la représentation Byte de la variable Int64 comprise dans l'interval [0..255] }
    function ToByte : Byte;
    { Retourne la représentation Booleenne de la variable Int64. Retourne TRUE si la valeur est plus grande que zero. Sinon retourne FALSE }
    function ToBoolean : Boolean;
    { Retourne la representation hexadécimale de la valeur Int64 sous forme de chaine de caractères }
    function ToHexString: string;
  end;

  { Assistant pour le type QWord }
  TQWordHelper =  type helper for QWord
  public
    const
      MaxValue = 18446744073709551615;
      MinValue = 0;
  public
    { Convertir une chaîne de caractères en sa représentation en QWord@br
      Retourne True an cas de succes sinon retourne False et définie la valeur avec la variable DefValue }
    function Parse(Num : String; Const defValue:QWord=0):Boolean;
    { Convertir la valeur de la variable QWord en chaîne de caractères }
    function ToString(Const Formatted : Boolean = False): string;
    { Retourne la plus petite valeur entre la valeur de la variable QWord et le paramètre "vMin" }
    function Min(vMin:QWord):QWord;
    { Retourne la plus grande valeur entre la valeur de la variable QWord et le paramètre "vMax" }
    function Max(vMax:QWord):QWord;
    { Vérifie si la valeur de la variable QWord est comprise et ou égale à l'interval définit par les paramètres "Low et High"}
    function IsInRange(Low, High: QWord): Boolean;
    { Retourne un nombre aléatoire dans l'interval définit par les constantes "MinValue et MaxValue" }
    function Random:QWord;
    { Retourne un nombre aléatoire dans l'interval definit par les paramètres "vMin et vMax" }
    function RandomRange(vMin, vMax :QWord):QWord;
    { S'assure que la valeur de la variable QWord est comprise entre les paramètres "vMin et vMax" et renvoie la valeur}
    function Clamp(vMin,vMax:QWord):QWord;
    { Retourne la représentation Booleenne de la variable QWord. Retourne TRUE si la valeur est plus grande que zero. Sinon retourne FALSE }
    function ToBoolean : Boolean;
    { Retourne la representation hexadécimale de la valeur DWord sous forme de chaine de caractères }
    function ToHexString: string;
  end;

  { Assistant pour le type Single }
  TSingleHelper =  type helper for Single
  public
    {$push}
    {$R-}
    {$Q-}
    const
      Epsilon          : Single = 1.4012984643248170709e-45;
      MaxValue         : Single = 340282346638528859811704183484516925440.0;
      MinValue         : Single = -340282346638528859811704183484516925440.0;
      PositiveInfinity : Single = 1.0/0.0;
      NegativeInfinity : Single = -1.0/0.0;
      NaN              : Single = 0.0/0.0;
    {$POP}
  public
    { Convertir une chaîne de caractères en sa représentation en Single@br
      Retourne True an cas de succes sinon retourne False et définie la valeur avec la variable DefValue }
    function Parse(Num : String; Const defValue:Single=0):Boolean;
    { Convertir la valeur de la variable Single avec nombre "Decimals" derrière la virgule en chaîne de caractères }
    function ToString(Const Decimals:Integer = 5): string;
    { Retourne la plus petite valeur entre la valeur de la variable Single et le paramètre "vMin" }
    function Min(vMin:Single):Single;
    { Retourne la plus grande valeur entre la valeur de la variable Single et le paramètre "vMax" }
    function Max(vMax:Single):Single;
    { Vérifie si la valeur de la variable Single est comprise et ou égale à l'interval définit par les paramètres "Low et High"}
    function IsInRange(Low, High: Single): Boolean;
    { Retourne un nombre aléatoire dans l'interval définit par les constantes "MinValue et MaxValue" }
    function Random:Single;
    { Retourne un nombre aléatoire dans l'interval definit par les paramètres "vMin et vMax" }
    function RandomRange(vMin, vMax :Single):Single;
    { S'assure que la valeur de la variable Single est comprise entre les paramètres "vMin et vMax" et renvoie la valeur}
    function Clamp(vMin,vMax:Single):Single;
    { Arrondit une valeur vers zéro }
    function Trunc : Integer;
    { Arrondit une valeur vers son entier le plus proche }
    function Round : Integer;
    { Arrondit la valeur vers l'infini négatif }
    function Floor : Integer;
    { Arrondit la valeur vers l'infini positif }
    function Ceil : Integer;
    {Arrondit la valeur en virgule flottante à l'entier le plus proche.
     Se comporte comme Round mais renvoie une valeur à virgule flottante comme Int.}
    function RoundInt : Single;
    { Renvoie la partie fractionnelle de la valeur }
    function Fract : Single;
    { Renvois le signe de la valeur avec la convention (-1, 0, +1) }
    function Sign:Single;
    { Retourne la valeur réciproques (1/value) }
    function Reciprocal : Single;
    { Retourne @True si la valeur tend vers l'infini positif ou négatif. Sinon retourne @False.}
    Function IsInfinity: Boolean;
    { Retuourne @True si la valeur est égale à NaN. Si non retourne @False.}
    Function IsNan: Boolean;
    { Retourne @True si la valeur tend vers l'infini négatif. Si non retourne @False.}
    Function IsNegativeInfinity: Boolean;
    { Retourne @True si la valeur tend vers l'infini positif. Si non retourne @False.}
    Function IsPositiveInfinity: Boolean;
  end;

  { Assistant pour le type Double }
  TDoubleHelper =  type helper for Double
  public
    const
    {$push}
    {$R-}
    {$Q-}
      Epsilon          : Double = 4.9406564584124654418e-324;
      MaxValue         : Double = 1.7976931348623157081e+308;
      MinValue         : Double = -1.7976931348623157081e+308;
      PositiveInfinity : Double = 1.0/0.0;
      NegativeInfinity : Double = -1.0/0.0;
      NaN              : Double = 0.0/0.0;
    {$POP}
  public
    { Convertir une chaîne de caractères en sa représentation en Double@br
      Retourne True an cas de succes sinon retourne False et définie la valeur avec la variable DefValue }
    function Parse(Num : String; Const defValue:Double=0):Boolean;
    { Convertir la valeur de la variable Double avec nombre "Decimals" derrière la virgule en chaîne de caractères }
    function ToString(Const Decimals:Integer = 5): string;
    { Retourne la plus petite valeur entre la valeur de la variable Double et le paramètre "vMin" }
    function Min(vMin:Double):Double;
    { Retourne la plus grande valeur entre la valeur de la variable Double et le paramètre "vMax" }
    function Max(vMax:Double):Double;
    { Vérifie si la valeur de la variable Double est comprise et ou égale à l'interval définit par les paramètres "Low et High"}
    function IsInRange(Low, High: Double): Boolean;
    { Retourne un nombre aléatoire dans l'interval définit par les constantes "MinValue et MaxValue" }
    function Random:Double;
    { Retourne un nombre aléatoire dans l'interval definit par les paramètres "vMin et vMax" }
    function RandomRange(vMin, vMax :Double):Double;
    { S'assure que la valeur de la variable Double est comprise entre les paramètres "vMin et vMax" et renvoie la valeur}
    function Clamp(vMin,vMax:Double):Double;
    { Arrondit une valeur vers zéro }
    function Trunc : Integer;
    { Arrondit une valeur vers son entier le plus proche }
    function Round : Integer;
    { Arrondit la valeur vers l'infini négatif }
    function Floor : Integer;
    { Arrondit la valeur vers l'infini positif }
    function Ceil : Integer;
    {Arrondit la valeur en virgule flottante à l'entier le plus proche. @br
     Se comporte comme Round mais renvoie une valeur à virgule flottante comme Int.}
    function RoundInt : Double;
    { Renvoie la partie fractionnelle de la valeur }
    function Fract : Double;
    { Renvois le signe de la valeur avec la convention (-1, 0, +1) }
    function Sign:Double;
    { Retours la valeur réciproques (1/value) }
    function Reciprocal : Double;
    { Retourne @True si la valeur tend vers l'infini positif ou négatif. Si non retourne @False.}
    Function IsInfinity: Boolean;
    { Returns @True si égale à NaN. Si non retourne @False.}
    Function IsNan: Boolean;
    { Retourne @True si la valeur tend vers l'infini négatif. Si non retourne @False.}
    Function IsNegativeInfinity: Boolean;
    { Retourne @True si la valeur tend vers l'infini positif. Si non retourne @False.}
    Function IsPositiveInfinity: Boolean;
  end;

  {$IFDEF FPC_HAS_TYPE_EXTENDED}
    { TODO : Add Helper for Extended }
  {$ENDIF}

  { Assistant pour le Type String - Utilise la convention UTF-8 }
  TStringHelper =  type helper for String
  Private
    Function GetChar(AIndex : Integer) : Char;
    Function GetLength : Integer;
  public
    Const
      Empty = '';
  public
    { Définit la chaine de caractère depuis le format System }
    procedure SetFromSys(aString : String);
    { Définit la chaine de caractère depuis le format Console }
    procedure SetFromConsole(aString:String);
    { Convertit et définit une chaine de caractères vers UTF-8 en utilisant les conventions pascal }
    procedure SetToUTF8(aString:String);
    { Convertit la chaine de caractères vers sa représentation au format "System" }
    function ToSys : String;
    { Convertit la chaine de caractères vers sa représentation au format "Console" }
    function ToConsole : String;
    { Convertit la chaine de caractères vers sa représentation au format "Ansi" }
    //function ToAnsi : AnsiString;
    { Supprime les espaces de début / fin et  les caractères de contrôle.}
    function  Trim: String; overload;
    { Supprime les espaces de début et  les caractères de contrôle.}
    function  TrimLeft : String;
    { Supprime les espaces de fin et  les caractères de contrôle. }
    function  TrimRight : String;
    { Renvoie @True si une chaîne n'a pas de caractères au-dessus de ord ('') eg = #32.
      Fondamentalement, cela signifie qu'il retourne @True si la chaîne est vide ou contient seulement
      des caractères de contrôle ou des espaces.
      Note : Beaucoup plus rapide que @code (if Trim(s) = '' Then ...)}
    function  IsEmpty : Boolean;

    //{ Same as the standard Length function }
    //function GetLength: Integer;

    { Convertit la chaine de caractère ne majuscule. }
    function  ToUpper : String;
    { Convertit la chaine de caractère ne minuscule. }
    function  ToLower : String;
    { Retourne @True si deux chaine de caractères sont égale. Paramètre si on prend en charge la casse ou pas. }
    function IsEquals(const Value: string; Const IgnoreCase: Boolean = False): Boolean;
    { Compare deux chaînes de caractère en ignorant éventuellement la casse en fonction du paramètre "IgnoreCase" @br
      Retourne :
      @unorderedlist(
        @item(-1 si A vient avant B)
        @item(1 si A vient après B)
        @item(0 si A et B sont égaux)
      ) }
    function Compare(const Value: string; Const IgnoreCase: Boolean = False): Integer;
    { Répète "count" fois le caractère "C". }
    procedure RepeatChar(C : Char; Count : Integer);
    { Répète "count" fois la chaine de caractères "s". }
    procedure RepeatStr(const s : String; Count : Integer);
    { Renvois une chaine de caractères centrée par rapport à "TotalLen" et ajoute le caractère "PadChar".}
    function PadCenter(TotalLen: Integer; Const PadChar : Char = ' '): String;
    { Ajoute un caractère à la fin de la chaîne jusqu'à ce que la longueur soit égale à "PadLen". @br
      Si "PadLen" est négatif, le caractère sera inséré à gauche.}
    function PadCharRight(PadLen : Integer;Const PadChar : Char = ' ') : String;
    { Ajoute un caractère au débute tant que sa longueur est plus petite que "PadLen".}
    function PadCharLeft(PadLen : Integer;Const PadChar : Char = ' ') : String;
    { Retourne la position d'une chaine de caractère. Identique à la fonction originel "Pos" }
    function Pos(const SubStr : String;StartPos :integer = 1) : Integer;
    { Renvoie @True si "Substr1" et "Substr2" sont trouvés et renvoie la position de "Substr1" et "Substr2". @br
      Si non retourne @False et -1 pour les positions }
    function PosBetween(const SubStr1, SubStr2 : String; var StartPos, EndPos : Integer):Boolean;
    { Retourne la position d'une chaine de caractère. Identique à la fonction originel "Pos" mais avec la prise en charge de la casse en plus }
    function IndexOf(const SubStr: string; Start: Integer=0; IgnoreCase: Boolean = False): Integer;
    { Recherche "SubStr", si trouvé, retourne les caractères situé après "SubStr". Si non une chaîne vide est renvoyée.}
    function After(const SubStr : String; Position : Integer = 1) : String;
    { Recherche "SubStr", si trouvé, retourne les caractères situé avant "SubStr". Si non la chaîne de caractère complète est renvoyée. }
    function Before(const SubStr : String; Position : Integer = 1) : String;
    { Analyse la chaine de caractères pour trouver la combinaison de début "SubStr1" et de fin "SubStr2". @
      Retourne le texte entre les deux. @Br
      Si SubStr2 est vide, SubStr2 sera considéré comme identique à SubStr1. @Br
      Si SubStr2 n'est pas trouvé, une chaîne vide est retournée (par opposition à la fonction Mid)..}
    function Between(const SubStr1, SubStr2 : String) : String;
    { Analyse la chaine de caractère et retourne le texte après "SubStr1" et avant "SubStr2" si ces deux chaines de caractères sont trouvées.
      Si "SubStr2" est vide alors "SubStr2" sera considéré comme identique à "SubStr1".
      Si "SubStr2" n'est pas trouvé alors la chaine de caractères complète après "SubStr1" sera retourné (à l'opposé de la méthode Between).
      Note : Cette méthode est la combinaison des méthode Before/After mais en plus rapide.}
    function Mid(const SubStr1, SubStr2 : String; Position : Integer = 1) : String;
    { Identique à la fonction "copy" standard, sauf que la limite est par défaut de 2 Go.}
    function Copy(aStart, aLength : Integer) : string;
    { Retourne la chaine de caractères entre la position "StartPos" et "EndPos" incluses }
    function CopyPos(StartPos, EndPos: Integer): String;
    { Insert un chaine de caractères "SubStr" à la position "Position"}
    function Insert(const SubStr: string; Position: Integer):String;
    { Renvoie tous les caractères à gauche d'une position spécifique (y compris)}
    function LeftOf(Position : Integer) : String;
    {Renvoie tous les caractères à droite d'une position spécifique (y compris)}
    function RightOf(Position : Integer) : String;
    { Convertit le type String vers le type WideString }
    function ToWideString : WideString;
    { Définit la chaine de caractères depuis une chaine de caractères de type WideString }
    procedure SetWideString(S : WideString);
    { Convertit une valeur de type Interger en sa représentation en chaine de caractères }
    procedure Parse(Num : Integer); overload;
    { Convertit une valeur de type Single avec "Decimal" chiffre après la virgule en sa représentation en chaine de caractères }
    procedure Parse(Num : Single; Const Decimals:Integer = 5); overload;
    {Convertit une valeur de type Double avec "Decimal" chiffre après la virgule en sa représentation en chaine de caractères }
    procedure Parse(Num : Double; Const Decimals:Integer = 5); overload;
    { Encadre la chaine de caractères avec le texte "chs"}
    function Surround(chs: string): string;  overload;
    { Encadre la chaine de caractères à gauche avec "chsL" et à droite avec "chsR"}
    function Surround(chsL, chsR: string): string; overload;
    { Joint les éléments d'une StringList séparé avec "Sep"}
    procedure Implode(lst:TStringList;sep : string =';');
    { Découpe les éléments de la chaine de caractères séparé par "Sep" vers une StringList}
    function Explode(sep: string = ';'):TStringList;
    { Retourne @True si "Substr" est présent dans la chaine de caractères. En ignorant éventuellement la casse. }
    function Contains(const SubStr: string; IgnoreCase: Boolean = False): Boolean;
    { Encadre la chaine de caractères avec des guillemets }
    function Quote : String;
    { Remplace la chaine de caractère "OldPattern" si présente par "NewPattern", en ignorant éventuellement la casse. }
    function Replace(const OldPattern, NewPattern: string; IgnoreCase: Boolean = False) : string;
    { Supprime tous les caractères à gauche à partir de la position "StartIndex" }
    function RemoveLeft(StartIndex : Integer):String; overload;
    { Supprime "aCount" caractères à gauche a partir de la position "StartIndex" }
    function RemoveLeft(StartIndex, aCount : Integer):String; overload;
    { Supprime tous les caractères à droite à partir de la position "StartIndex" }
    function RemoveRight(StartIndex : Integer):String; overload;
    { Supprime "aCount" caractères à droite a partir de la position "StartIndex" }
    function RemoveRight(StartIndex, aCount : Integer):String; overload;
    { Supprime tous les caractères correspondant à "aChar", en ignorant éventuellement la casse. }
    function RemoveChar(aChar : Char; Const IgnoreCase : Boolean = False):String;
    { Efface toutes les cahines de caractères correspond à "SubStr", en ignorant éventuellement la casse. }
    function Remove(SubStr:String; Const IgnoreCase : Boolean = False):String;
    { Retourne la chaine des caractères inversée }
    function Reverse : String;
    { Limite la longeur de la chaine de caractère maximale à 'MaxCol'}
    function Wrap(MaxCol : Integer):String;
    { Retourne le nombre de fois que le caractère "C" est présent dans la chaine de caractères }
    function CountChars(C: Char):Integer;
    { Retourne @True si la chaine de caractère contient "DefTrue".  @br
      Retourne @False si la chaine de caractère contient "DefFalse".
      Si non retourne @True }
    function ToBoolean(Const DefTrue : String = 'True'; Const defFalse : String = 'False'; Const IgnoreCase : Boolean = false):Boolean;
    { Convertit la chaine de caractères vers le type Integer. @br
      Retourne zero par défaut en cas d'erreur. }
    function ToInteger : Integer;
    { Convertit la chaine de caractères vers le type Int64. @br
      Retourne zero par défaut en cas d'erreur. }
    function ToInt64 : Int64;
    { Convertit la chaine de caractères vers le type Single. @br
      Retourne zero par défaut en cas d'erreur. }
    function ToSingle : Single;
    { Convertit la chaine de caractères vers le type Double. @br
      Retourne zero par défaut en cas d'erreur. }
    function ToDouble : Double;
    //function ToInt64 : Int64;
    { Convertit la chaine de caractères vers le type Byte. @br
      Retourne zero par défaut en cas d'erreur }
    function ToByte : Byte;
    { Calcul un "Hash" unique de la chaine de caractères, en ignorant éventuellement la casse}
    function ComputeHash(Const IgnoreCase : Boolean = False) : Integer;
    { Retourne le caractère à la position "AIndex" }
    property Chars[AIndex: Integer]: Char read GetChar;
    { Retourne la longueur de la chaine de caractères }
    property Length: Integer read GetLength;
  End;

  { Définition du type de format d'un TDateTime pour sa representation en chaine de caractères }
  TDateFormat =(dfUTC, dfGMT, dfCustom);

  { @abstract(Assistant pour le type TDateTime)

    Voir aussi : https://www.freepascal.org/docs-html/rtl/sysutils/formatchars.html

    @table(
      @rowHead( @cell(Caractères) @cell(Description))
      @row( @cell(c)     @cell(Format court de la date (uniquement pour FormatDateTime) ))
      @row( @cell(d)     @cell(Jour du mois ))
      @row( @cell(dd)    @cell(Jour du mois (zero en tête) ))
      @row( @cell(ddd)   @cell(Jour de la semaine (abbreviation) ))
      @row( @cell(dddd)  @cell(Jour de la semaine (Complet) ))
      @row( @cell(ddddd) @cell(Format long de la date (uniquement pour FormatDateTime) ))
      @row( @cell(m)     @cell(Mois ou minutes si précédé de h ))
      @row( @cell(mm)    @cell(Mois (zero en tête) ))
      @row( @cell(mmm)   @cell(Mois (abbreviation) ))
      @row( @cell(mmmm)  @cell(Mois (Complet) ))
      @row( @cell(y)     @cell(Année (deux chiffres) ))
      @row( @cell(yy)    @cell(Année (deux chiffres) ))
      @row( @cell(yyyy)  @cell(Année (quatre chiffre, avec siècle) ))
      @row( @cell(h)     @cell(Heure ))
      @row( @cell(hh)    @cell(Heure (zero en tête) ))
      @row( @cell(n)     @cell(Minute ))
      @row( @cell(nn)    @cell(Minute (zero en tête) ))
      @row( @cell(s)     @cell(Secondes ))
      @row( @cell(ss)    @cell(Secondes (zero en tête) ))
      @row( @cell(t)     @cell(Format court de l'heure (uniquement pour FormatDateTime) ))
      @row( @cell(tt)    @cell(Format long de l'heure (uniquement pour FormatDateTime) ))
      @row( @cell(am/pm) @cell(Utilises une horloge sur 12 heures et affiche am ou pm ))
      @row( @cell(a/p)   @cell(Utilises une horloge sur 12 heures et affiche a ou p ))
      @row( @cell(/)     @cell(Insert un séparateur de date ))
      @row( @cell(:)     @cell(Insert un séparateur de temps ))
      @row( @cell("xx")  @cell(Texte literal ))
      @row( @cell(’xx’)  @cell(Texte literal ))
    )

    ----------------------------------------------------------------------------

    @bold(Note) : pour l'utilisation du caractère : '/' comme séparateur vous devrez utiliser un double guillemet dans votre chaîne de modèle ( eg : 'YYYY"/"MM"/"DD' ) }
  TDateTimeHelper = type helper for TDateTime
  private
    function getYear: Word;
    function getMonth: Word;
    function getDay: Word;
    function getHour: Word;
    function getMinute: Word;
    function getSecond: Word;
    Function getMillisecond : Word;

  public
   Const
     fmtFullDate : String = 'dddd dd mmmm yyyy '; // < Format long pour la représentation d'une date en chaine de caractères
     fmtShortDate : String = 'dd"/"mm"/"yyyy'; // < Format cours pour la représentation d'une date en chaine de caractères
     fmtShortTime : String = 'hh":"nn":"ss'; // < Format  pour la représentation d'une "heure" en chaine de caractères
  public
    { Définit la valeur à la date du jour }
    procedure SetToDay;
    { Définit la valeur du temps à l'heure en cours }
    Procedure SetToTime;
    { Définit la valeur à la date et l'heure actuelle }
    procedure SetToNow;
    { Définit l'heure en fonction des paramètres H, M, S }
    procedure SetTime(H,M,S : Integer);
    { Définit la date en fonction des paramètres Y, M, D }
    procedure SetDate(Y,M,D : Integer);
    { Définit la date et l'heure depuis une chaine de caractères d'après le modèle "APattern" }
    procedure SetFromString(ADateTimeStr : String ;Const APAttern : String = 'YYYY"/"MM"/"DD hh":"nn":"ss');
    { Définit la date et l'heure à partir de l'horodatage Unix }
    procedure SetFromUnixTimeStamp(ADate : Int64);
    { Définit la date et l'heure à partir de l'horodatage Mac  }
    procedure SetFromMacTimeStamp(ADate : Int64);
    { Définit la date et l'heure à partir d'une date et heure UTC }
    procedure SetFromUTC(UT: TDateTime); overload;
    { Définit la date et l'heure à partir d'une date et heure UTC  et un écart }
    procedure SetFromUTC(UT: TDateTime; ZOffset : Integer); overload;
    { Définit la date et l'heure à partir d'une date au format "Julian" }
    procedure SetFromJulian(JulianDate : Double);
    { Définit la date et l'heure à partir d'une chaine de caractères provenant d'une représentation au format SQL }
    procedure SetFromSQL(ADataTimeStr : String);
    { Définit une date et heure aléatoire }
    procedure SetRandomDate;  overload;
    { Définit une date et heure aléatoire comprise entre les paramètres "StartDate" et "EndDate" }
    procedure SetRandomDate(StartDate, EndDate : TDateTime); overload;
    { Définit la date et l'heure depuis la date et heure d'un fichier }
    procedure SetFromFile(aFileName:String);
    { Retourne la représentation du TDateTime en chaine de caractères formaté suivant le modèle }
    function ToString(Const Format: TDateFormat = dfCustom ; Const CustomFormat: string = 'yyyy-mm-dd hh:nn:ss'): string;
    { Retourne la représentation au format SQL du TDateTime }
    function ToSQL : String;
    { Retourne la représentation de la date en chaine de caractères formaté suivant le modèle. eg : YYYY/MM/DD @br
      @bold(Note) : pour utiliser le séparateur / il faut le "Double Quoter"}
    function DateToString(Const FormatStr :string = 'dd"/"mm"/"yyyy'): String;
    { Retourne la représentation de l'heure en chaine de caractères formaté suivant le modèle. eg : H:M:S}
    function TimeToString(Const FormatStr : string = 'hh:nn:ss') : String;
    { Compare deux TDateTime. Retourne 0 Si égale. Si non retourne -1 or 1 si elle est plus petite ou plus grande }
    function Compare(SecondDateTime : TDatetime):Integer;
    { Compare deux Date. Retourne 0 Si égale. Si non retourne -1 or 1 si elle est plus petite ou plus grande}
    function CompareDate(SecondDate : TDateTime):Integer;
    { Compare deux Heure. Retourne 0 Si égale. Si non retourne -1 or 1 si elle est plus petite ou plus grande}
    function CompareTime(SecondTime : TDateTime):Integer;
    { Ajoute une année à la date en cours }
    function AddYear(const A: Integer=1): TDateTime;
    { Ajoute un mois à la date en cours }
    function AddMonth(const A: Integer=1): TDateTime;
    { Ajoute un jour à la date en cours }
    function AddDay(const A: Integer=1): TDateTime;
    { Ajoute un semaine à la date en cours }
    function AddWeek(const A: Integer=1): TDateTime;
    { Ajoute des heures à l'heure en cours }
    function AddHour(const A: Integer=1): TDateTime;
    { Ajoute des minutes à l'heure en cours }
    function AddMinute(const A: Integer=1): TDateTime;
    { Ajoute des secondes à l'heure en cours }
    function AddSecond(const A: Integer=1): TDateTime;
    { Ajoute des millisecondes à l'heure en cours}
    function AddMilliSecond(const A: Integer=1): TDateTime;
    { Convertit la date en cours vers le format "Julian" }
    function ToJulian : Double;
    { Convert la date vers la représentation horodatage Unix }
    function ToUnixTimeStamp : Int64;
    { Convert la date vers la représentation horodatage Mac }
    function ToMacTimeStamp : Int64;
    { Convert la date vers la représentation UTC }
    function ToUTC : TDateTime; overload;
    { Convert la date vers la représentation UTC avec un décalage }
    function ToUTC(ZOffset : Integer) : TDateTime;
    { Retourne @True si l'anné est bixestile. Si non retourne @False }
    function IsLeapYear : Boolean;
    { Retourne le nombre de jour de l'année }
    function GetDaysInYear : Word;
    { Retourne le nombre de jour du mois }
    function GetDaysInMonth : Word;
    { Retourne le nombre de semaine de l'année }
    function GetWeeksInYear : Word;
    { Retourne le numéro du jour de la semaine }
    function GetDayOfTheWeek : Word;
    { Retourne le nuémro de la semaine de l'année }
    function GetWeekOfTheYear : Word;
    { Retourne les années écoulées entre deux dates }
    function GetElapsedYears(ATo : TDateTime): Integer;
    { Renvoie les mois écoulées entre deux dates }
    function GetElapsedMonths(ATo : TDateTime): Integer;
    { Renvoie les semaines écoulées entre deux dates }
    function GetElapsedWeeks(ATo : TDateTime): Integer;
    { Renvoie les jours écoulées entre deux dates }
    function GetElapsedDays(ATo : TDateTime): Integer;
    { Renvois les heures écoulées entre deux heures }
    function GetElapsedHours(ATo : TDateTime): Int64;
    { Renvois les minutes écoulées entre deux heures }
    function GetElapsedMinutes(ATo : TDateTime): Int64;
    { Renvois les secondes écoulées entre deux heures }
    function GetElapsedSeconds(ATo : TDateTime): Int64;
    { Renvois les millisecondes écoulées entre deux heures }
    function GetElapsedMilliSeconds(ATo : TDateTime): Int64;
    { Renvois le temps écoulés entre deux TDateTime }
    function GetElapsedPeriod(ATo : TDateTime): TDateTime;
    {$IFDEF WINDOWS}
    { Convertis le FileTime de Windows vers un TDateTime }
    procedure FromFileTime(const FileTime: TFileTime);
    {$ENDIF}

    { Retourne l'année de la date }
    property Year : Word read getYear;
    { Retourne le mois de la date }
    property Month : Word read getMonth;
    { Retourne le jour de la date }
    property Day : Word read getDay;
    { Retourne l'heure de l'heure }
    property Hour : Word read getHour;
    { Retourne la minute de l'heure }
    property Minute : Word read getMinute;
    { Retourne les secondes de l'heure }
    property Second : Word read getSecond;
    {  Retourne les millisecondes de l'heure }
    property MilliSecond : Word read getMilliSecond;
  end;

//==============================================================================

var
  { Variable de formatage pour le choix du séparateur de la décimale des nombres en virgule flottante point --> virgule }
  vPointSeparator,
  { Variable de formatage pour le choix du séparateur de la décimale des nombres en virgule flottante virgule --> point }
  vCommaSeparator: TFormatSettings;

//==============================================================================

Implementation

Uses
  LazUTF8, Math, DateUtils, LazFileUtils;
  //{$IFDEF WINDOWS} ,windows{$ENDIF};

//==============================================================================

{%region%=====[ Internal tools ]================================================}

const
  cHexTbl : array[0..15] of char='0123456789ABCDEF';

function _hexstr(val : longint;cnt : byte) : shortstring;
var
  i : Integer;
begin
  Result[0]:=char(cnt);
  for i:=cnt downto 1 do
   begin
     result[i]:=chextbl[val and $f];
     val:=val shr 4;
   end;
end;

{$IFNDEF NO_ASM_OPTIMIZATIONS}
// pour éviter l'inclusion de l'unité BZMath
function ClampByte(const Value: Integer): Byte; assembler; nostackframe;
asm
{$IFDEF CPU64}
  {$IFDEF UNIX}
     MOV     EAX,EDI
  {$ELSE}
     MOV     EAX,ECX
  {$ENDIF}
{$ENDIF}
   TEST    EAX,$FFFFFF00
   JNZ     @above
   RET
@above:
   JS      @below
   MOV     EAX,$FF
   RET
@Below:
   XOR     EAX,EAX
end;
{$ELSE}
function ClampByte(const Value: Integer): Byte;
begin
 Result := Value;
 if Value > 255 then Result := 255
 else if Value < 0 then Result := 0;
end;
{$ENDIF}

{%endregion%}

{%region%=====[ TBooleanHelper ]================================================}

Function TBooleanHelper.Tostring(Const Deftrue : String; Const Deffalse : String) : String;
Begin
  if (Self = True) then result := DefTrue else result := DefFalse;
End;

{%endregion%}

{%region%=====[ TCharHelper ]===================================================}

Function TCharHelper.IsAlpha : Boolean;
Begin
 Result := ((Self in ['A'..'Z']) or (Self in ['a'..'z']));
End;

Function TCharHelper.IsNumeric : Boolean;
Begin
  Result := (Self in ['0'..'9']);
End;

Function TCharHelper.ToCharCode : Integer;
Begin
  Result := ord(Self);
End;

Function TCharHelper.ToUpper : Char;
begin
  result :=UpCase(Self);
End;

Function TCharHelper.ToLower : Char;
Begin
  Result := LowerCase(Self);
End;

{%endregion%}

{%region%=====[ TByteHelper ]===================================================}

function TByteHelper.Parse(Num: String; const defValue: Byte): Boolean;
Var
  I: Integer;
Begin
  Result := false;
  Self := defValue;
  if TryStrToInt(Num,I) then
  begin
    Self := ClampByte(I);
    result := true;
  End;
End;

function TByteHelper.ToString: string;
Begin
  Result := IntToStr(Self);
End;

function TByteHelper.Min(vMin: Byte): Byte;
Begin
  if Self<vMin then result := vMin else result := Self;
End;

function TByteHelper.Max(vMax: Byte): Byte;
Begin
  if Self>vMax then result := vMax else result := Self;
End;

function TByteHelper.IsInRange(Low, High: Byte): Boolean;
Begin
  result := ((Self >= Low) and (Self <= High));
End;

Function TByteHelper.Random:Byte;
Begin
  Result := Self.RandomRange(Self.MinValue,Self.MaxValue);
End;

Function TByteHelper.RandomRange(vMin, vMax :Byte):Byte;
Begin
 Result:=System.Random(Abs(vMin-vMax)) + Math.Min(vMin,vMax);
End;

function TByteHelper.Clamp(vMin, vMax: Byte): Byte;
begin
  Result := Self;
  if Result > vMax then begin Result := vMax; exit; end;
  if Result < vMin then Result := vMin;
end;

function TByteHelper.ToBoolean: Boolean;
begin
  result := (self<>0);
end;

function TByteHelper.ToHexString: string;
begin
 result :=_hexstr(Self,2)
end;

function TByteHelper.Normalized: Single;
begin
  result := self * Self.NormalizedRatio;
end;

function TByteHelper.Reciprocal: Single;
Begin
  Result := 0;
  If Self = 0 Then exit;
  Result := 1.0 / Self
End;

{%endregion%}

{%region%=====[ TShortIntHelper ]===============================================}

function TShortIntHelper.Parse(Num: String; const defValue: ShortInt): Boolean;
Var
  I: Integer;
Begin
  Result := false;
  Self := defValue;
  if TryStrToInt(Num,I) then
  begin
    Self := I.Clamp(Shortint.MinValue,ShortInt.MaxValue);
    result := true;
  End;
End;

function TShortIntHelper.ToString(const Formatted: Boolean): string;
Begin
  if not(Formatted) then
    Result := IntToStr(Self)
  else
    Result := Format('%.0n',[Self+0.0],vPointSeparator);
End;

function TShortIntHelper.Min(vMin: ShortInt): ShortInt;
Begin
  if Self<vMin then result := vMin else result := Self;
End;

function TShortIntHelper.Max(vMax: ShortInt): ShortInt;
Begin
  if Self>vMax then result := vMax else result := Self;
End;

function TShortIntHelper.IsInRange(Low, High: ShortInt): Boolean;
Begin
  result := ((Self >= Low) and (Self <= High));
End;

Function TShortIntHelper.Random:ShortInt;
Begin
  Result := Self.RandomRange(Self.MinValue,Self.MaxValue);
End;

Function TShortIntHelper.RandomRange(vMin, vMax :ShortInt):ShortInt;
Begin
 Result:=System.Random(Abs(vMin-vMax))+ Math.Min(vMin,vMax);
End;

function TShortIntHelper.Clamp(vMin, vMax: ShortInt): ShortInt;
begin
  Result := Self;
  if Result > vMax then begin Result := vMax; exit; end;
  if Result < vMin then Result := vMin;
end;

function TShortIntHelper.Sign: ShortInt;
begin
  If Self < 0 Then
    Result := -1
  Else If Self > 0 Then
    Result := 1
  Else
    Result := 0;
end;

function TShortIntHelper.ToByte: Byte;
Begin
  Result := ClampByte(Self);
End;

function TShortIntHelper.ToBoolean: Boolean;
begin
  Result := (Self>0);
end;

function TShortIntHelper.ToHexString: string;
begin
  Result:=_hexstr(Self,8)
end;

function TShortIntHelper.Normalized: Single;
begin
  Result := Self * Self.NormalizedRatio;
end;

function TShortIntHelper.Reciprocal: Single;
Const
  cEpsilon : Single = 1.4012984643248170709e-45;
Var
  a: ShortInt;
Begin
  Result := 0;
  If Self = 0 Then exit;
  a := Self.Sign;
  If ((a * Self) >= cEpsilon) Then
    Result := 1.0 / Self
  Else
    Result := a * (1.0 / cEpsilon);
End;

{%endregion%}

{%region%=====[ TSmallIntHelper ]===============================================}

function TSmallIntHelper.Parse(Num: String; const defValue: SmallInt): Boolean;
Var
  I: Integer;
Begin
  Result := false;
  Self := defValue;
  if TryStrToInt(Num,I) then
  begin
    Self := I.Clamp(SmallInt.MinValue,SmallInt.MaxValue);
    result := true;
  End;
End;

function TSmallIntHelper.ToString(const Formatted: Boolean): string;
Begin
  if not(Formatted) then
    Result := IntToStr(Self)
  else
    Result := Format('%.0n',[Self+0.0],vPointSeparator);
End;

function TSmallIntHelper.Min(vMin: SmallInt): SmallInt;
Begin
  if Self<vMin then result := vMin else result := Self;
End;

function TSmallIntHelper.Max(vMax: SmallInt): SmallInt;
Begin
  if Self>vMax then result := vMax else result := Self;
End;

function TSmallIntHelper.IsInRange(Low, High: SmallInt): Boolean;
Begin
  result := ((Self >= Low) and (Self <= High));
End;

Function TSmallIntHelper.Random:SmallInt;
Begin
  Result := Self.RandomRange(Self.MinValue,Self.MaxValue);
End;

Function TSmallIntHelper.RandomRange(vMin, vMax :SmallInt):SmallInt;
Begin
 Result:=System.Random(Abs(vMin-vMax))+ Math.Min(vMin,vMax);
End;

function TSmallIntHelper.Clamp(vMin, vMax: SmallInt): SmallInt;
begin
  Result := Self;
  if Result > vMax then begin Result := vMax; exit; end;
  if Result < vMin then Result := vMin;
end;

function TSmallIntHelper.Sign: SmallInt;
begin
  If Self < 0 Then
    Result := -1
  Else If Self > 0 Then
    Result := 1
  Else
    Result := 0;
end;

function TSmallIntHelper.ToByte: Byte;
Begin
  Result := ClampByte(Self);
End;

function TSmallIntHelper.ToBoolean: Boolean;
begin
  Result := (Self>0);
end;

function TSmallIntHelper.ToHexString: string;
begin
  Result:=_hexstr(Self,8)
end;

function TSmallIntHelper.Normalized: Single;
begin
  Result := Self * Self.NormalizedRatio;
end;

function TSmallIntHelper.Reciprocal: Single;
Const
  cEpsilon : Single = 1.4012984643248170709e-45;
Var
  a: SmallInt;
Begin
  Result := 0;
  If Self = 0 Then exit;
  a := Self.Sign;
  If ((a * Self) >= cEpsilon) Then
    Result := 1.0 / Self
  Else
    Result := a * (1.0 / cEpsilon);
End;

function TSmallIntHelper.IsPowerOfTwo: Boolean;
Const
  BitCountTable: Array[0..255] Of Byte =
    (0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4,
    1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
    1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
    1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
    3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
    1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
    3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
    3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
    3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
    4, 5, 5, 6, 5, 6, 6, 7, 5, 6, 6, 7, 6, 7, 7, 8);

  Function BitCount(Value: Longword): Longword; 
  Var
    V: Array[0..3] Of Byte absolute Value;
  Begin
    Result := BitCountTable[V[0]] + BitCountTable[V[1]] + BitCountTable[V[2]] + BitCountTable[V[3]];
  End;
Begin
  Result := BitCount(abs(Self)) = 1;
End;

function TSmallIntHelper.NextPowerOfTwo: SmallInt;
var
  value : SmallInt;
begin
  Value := Self;
  If (Value > 0) Then
  Begin
    Dec(Value);
    Value := Value Or (Value Shr 1);
    Value := Value Or (Value Shr 2);
    Value := Value Or (Value Shr 4);
    Value := Value Or (Value Shr 8);
    Value := Value Or (Value Shr 16);
  End;

  Result := Value + 1;
end;

function TSmallIntHelper.PreviousPowerOfTwo: SmallInt;
Var
  I, N: SmallInt;
Begin
  Result := 0;
  For I := 14 Downto 2 Do
  Begin
    N := (1 Shl I);
    If N < Self Then
      Break
    Else
      Result := N;
  End;
end;

{%endregion%}

{%region%=====[ TWordHelper ]===================================================}

function TWordHelper.Parse(Num: String; const defValue: Word): Boolean;
Var
  I: Integer;
Begin
  Result := false;
  Self := defValue;
  if TryStrToInt(Num,I) then
  begin
    Self := I.Clamp(0,Self.MaxValue);
    result := true;
  End;
End;

function TWordHelper.ToString(const Formatted: Boolean): string;
Begin
  if not(Formatted) then
    Result := IntToStr(Self)
  else
    Result := Format('%.0n',[Self+0.0],vPointSeparator);
End;

function TWordHelper.Min(vMin: Word): Word;
Begin
  if Self<vMin then result := vMin else result := Self;
End;

function TWordHelper.Max(vMax: Word): Word;
Begin
  if Self>vMax then result := vMax else result := Self;
End;

function TWordHelper.IsInRange(Low, High: Word): Boolean;
Begin
  result := ((Self >= Low) and (Self <= High));
End;

Function TWordHelper.Random:Word;
Begin
  Result := Self.RandomRange(Self.MinValue,Self.MaxValue);
End;

Function TWordHelper.RandomRange(vMin, vMax :Word):Word;
Begin
 Result:=System.Random(Abs(vMin-vMax))+ Math.Min(vMin,vMax);
End;

function TWordHelper.Clamp(vMin, vMax: Word): Word;
begin
  Result := Self;
  if Result > vMax then begin Result := vMax; exit; end;
  if Result < vMin then Result := vMin;
end;

function TWordHelper.ToByte: Byte;
Begin
  Result := ClampByte(Self);
End;

function TWordHelper.ToBoolean: Boolean;
begin
  Result := (Self>0);
end;

function TWordHelper.ToHexString: string;
begin
  Result:=_hexstr(Self,4)
end;

function TWordHelper.Normalized: Single;
begin
  Result := Self * Self.NormalizedRatio;
end;

function TWordHelper.Reciprocal: Single;
Const
  cEpsilon : Single = 1.4012984643248170709e-45;
Begin
  Result := 0;
  If Self = 0 Then exit;
  If (Self >= cEpsilon) Then
    Result := 1.0 / Self
  Else
    Result :=  (1.0 / cEpsilon);
End;

function TWordHelper.IsPowerOfTwo: Boolean;
Const
  BitCountTable: Array[0..255] Of Byte =
    (0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4,
    1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
    1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
    1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
    3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
    1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
    3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
    3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
    3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
    4, 5, 5, 6, 5, 6, 6, 7, 5, 6, 6, 7, 6, 7, 7, 8);

  Function BitCount(Value: Longword): Longword;  
  Var
    V: Array[0..3] Of Byte absolute Value;
  Begin
    Result := BitCountTable[V[0]] + BitCountTable[V[1]] + BitCountTable[V[2]] + BitCountTable[V[3]];
  End;
Begin
  Result := BitCount(abs(Self)) = 1;
End;

function TWordHelper.NextPowerOfTwo: Word;
var
  value : Word;
begin
  Value := Self;
  If (Value > 0) Then
  Begin
    Dec(Value);
    Value := Value Or (Value Shr 1);
    Value := Value Or (Value Shr 2);
    Value := Value Or (Value Shr 4);
    Value := Value Or (Value Shr 8);
    Value := Value Or (Value Shr 16);
  End;

  Result := Value + 1;
end;

function TWordHelper.PreviousPowerOfTwo: Word;
Var
  I, N: Word;
Begin
  Result := 0;
  For I := 14 Downto 2 Do
  Begin
    N := (1 Shl I);
    If N < Self Then
      Break
    Else
      Result := N;
  End;
end;

{%endregion%}

{%region%=====[ TCardinalHelper ]===============================================}

function TCardinalHelper.Parse(Num: String; const defValue: Cardinal): Boolean;
Var
  I: Int64;
Begin
  Result := false;
  Self := defValue;
  if TryStrToInt64(Num,I) then
  begin
    Self := I.Clamp(Cardinal.MinValue,Cardinal.MaxValue);
    result := true;
  End;
End;

function TCardinalHelper.ToString(const Formatted: Boolean): string;
Begin
  if not(Formatted) then
    Result := IntToStr(Self)
  else
    Result := Format('%.0n',[Self+0.0],vPointSeparator);
End;

function TCardinalHelper.Min(vMin: Cardinal): Cardinal;
Begin
  if Self<vMin then result := vMin else result := Self;
End;

function TCardinalHelper.Max(vMax: Cardinal): Cardinal;
Begin
  if Self>vMax then result := vMax else result := Self;
End;

function TCardinalHelper.IsInRange(Low, High: Cardinal): Boolean;
Begin
  result := ((Self >= Low) and (Self <= High));
End;

Function TCardinalHelper.Random:Cardinal;
Begin
  Result := Self.RandomRange(Self.MinValue,Self.MaxValue);
End;

Function TCardinalHelper.RandomRange(vMin, vMax :Cardinal):Cardinal;
Begin
 Result:=System.Random(Abs(vMin-vMax))+Math.Min(vMin,vMax);
End;

function TCardinalHelper.Clamp(vMin, vMax: Cardinal): Cardinal;
begin
  Result := Self;
  if Result > vMax then begin Result := vMax; exit; end;
  if Result < vMin then Result := vMin;
end;

function TCardinalHelper.ToByte: Byte;
Begin
  Result := ClampByte(Self);
End;

function TCardinalHelper.ToBoolean: Boolean;
begin
  Result := (Self>0);
end;

function TCardinalHelper.ToHexString: string;
begin
  Result:=_hexstr(Self,8)
end;

function TCardinalHelper.Normalized: Single;
begin
  Result := Self * Self.NormalizedRatio;
end;

function TCardinalHelper.Reciprocal: Single;
Const
  cEpsilon : Single = 1.4012984643248170709e-45;
Begin
  Result := 0;
  If Self = 0 Then exit;
  If (Self >= cEpsilon) Then
    Result := 1.0 / Self
  Else
    Result :=  (1.0 / cEpsilon);
End;

function TCardinalHelper.IsPowerOfTwo: Boolean;
Const
  BitCountTable: Array[0..255] Of Byte =
    (0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4,
    1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
    1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
    1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
    3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
    1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
    3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
    3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
    3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
    4, 5, 5, 6, 5, 6, 6, 7, 5, 6, 6, 7, 6, 7, 7, 8);

  Function BitCount(Value: Longword): Longword; 
  Var
    V: Array[0..3] Of Byte absolute Value;
  Begin
    Result := BitCountTable[V[0]] + BitCountTable[V[1]] + BitCountTable[V[2]] + BitCountTable[V[3]];
  End;
Begin
  Result := BitCount(abs(Self)) = 1;
End;

function TCardinalHelper.NextPowerOfTwo: Cardinal;
var
  value : Cardinal;
begin
  Value := Self;
  If (Value > 0) Then
  Begin
    Dec(Value);
    Value := Value Or (Value Shr 1);
    Value := Value Or (Value Shr 2);
    Value := Value Or (Value Shr 4);
    Value := Value Or (Value Shr 8);
    Value := Value Or (Value Shr 16);
  End;

  Result := Value + 1;
end;

function TCardinalHelper.PreviousPowerOfTwo: Cardinal;
Var
  I, N: Cardinal;
Begin
  Result := 0;
  For I := 14 Downto 2 Do
  Begin
    N := (1 Shl I);
    If N < Self Then
      Break
    Else
      Result := N;
  End;
end;

{%endregion%}

{%region%=====[ TIntegerHelper ]================================================}

function TIntegerHelper.Parse(Num: String; const defValue: Integer): Boolean;
Var
  I: Integer;
Begin
  Result := false;
  Self := defValue;
  if TryStrToInt(Num,I) then
  begin
    Self := I;
    result := true;
  End;
End;

function TIntegerHelper.ToString(const Formatted: Boolean): string;
Begin
  if not(Formatted) then
    Result := IntToStr(Self)
  else
    Result := Format('%.0n',[Self+0.0],vPointSeparator);
End;

function TIntegerHelper.Min(vMin: Integer): Integer;
Begin
  if Self<vMin then result := vMin else result := Self;
End;

function TIntegerHelper.Max(vMax: Integer): Integer;
Begin
  if Self>vMax then result := vMax else result := Self;
End;

function TIntegerHelper.IsInRange(Low, High: Integer): Boolean;
Begin
  result := ((Self >= Low) and (Self <= High));
End;

Function TIntegerHelper.Random:Integer;
Begin
  Result := Self.RandomRange(Self.MinValue,Self.MaxValue);
End;

Function TIntegerHelper.RandomRange(vMin, vMax :Integer):Integer;
Begin
 Result:=System.Random(Abs(vMin-vMax))+ Math.Min(vMin,vMax);
End;

function TIntegerHelper.Clamp(vMin, vMax: Integer): Integer;
begin
  Result := Self;
  if Result > vMax then begin Result := vMax; exit; end;
  if Result < vMin then Result := vMin;
end;

function TIntegerHelper.Sign: Integer;
begin
  If Self < 0 Then
    Result := -1
  Else If Self > 0 Then
    Result := 1
  Else
    Result := 0;
end;

function TIntegerHelper.ToByte: Byte;
Begin
  Result := ClampByte(Self);
End;

function TIntegerHelper.ToBoolean: Boolean;
begin
  Result := (Self>0);
end;

function TIntegerHelper.ToHexString: string;
begin
  Result:=_hexstr(Self,8)
end;

function TIntegerHelper.Normalized: Single;
begin
  Result := Self * Self.NormalizedRatio;
end;

function TIntegerHelper.Reciprocal: Single;
Const
  cEpsilon : Single = 1.4012984643248170709e-45;
Var
  a: Integer;
Begin
  Result := 0;
  If Self = 0 Then exit;
  a := Self.Sign;
  If ((a * Self) >= cEpsilon) Then
    Result := 1.0 / Self
  Else
    Result := a * (1.0 / cEpsilon);
End;

function TIntegerHelper.IsPowerOfTwo: Boolean;
Const
  BitCountTable: Array[0..255] Of Byte =
    (0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4,
    1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
    1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
    1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
    3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
    1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
    3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
    3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
    3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
    4, 5, 5, 6, 5, 6, 6, 7, 5, 6, 6, 7, 6, 7, 7, 8);

  Function BitCount(Value: Longword): Longword; 
  Var
    V: Array[0..3] Of Byte absolute Value;
  Begin
    Result := BitCountTable[V[0]] + BitCountTable[V[1]] + BitCountTable[V[2]] + BitCountTable[V[3]];
  End;
Begin
  Result := BitCount(abs(Self)) = 1;
End;

function TIntegerHelper.NextPowerOfTwo: Integer;
var
  value : Integer;
begin
  Value := Self;
  If (Value > 0) Then
  Begin
    Dec(Value);
    Value := Value Or (Value Shr 1);
    Value := Value Or (Value Shr 2);
    Value := Value Or (Value Shr 4);
    Value := Value Or (Value Shr 8);
    Value := Value Or (Value Shr 16);
  End;

  Result := Value + 1;
end;

function TIntegerHelper.PreviousPowerOfTwo: Integer;
Var
  I, N: Integer;
Begin
  Result := 0;
  For I := 14 Downto 2 Do
  Begin
    N := (1 Shl I);
    If N < Self Then
      Break
    Else
      Result := N;
  End;
end;

{%endregion%}

{%region%=====[ TInt64Helper ]==================================================}

function TInt64Helper.Parse(Num: String; const defValue: Int64): Boolean;
Var
  I: Int64;
Begin
  Result := false;
  Self := defValue;
  if TryStrToInt64(Num,I) then
  begin
    Self := I;
    result := true;
  End;
End;

function TInt64Helper.ToString(const Formatted: Boolean): string;
Begin
  if not(Formatted) then
    Result := IntToStr(Self)
  else
    Result := Format('%.0n',[Self+0.0],vPointSeparator);
End;

function TInt64Helper.Min(vMin: Int64): Int64;
Begin
  if Self<vMin then result := vMin else result := Self;
End;

function TInt64Helper.Max(vMax: Int64): Int64;
Begin
  if Self>vMax then result := vMax else result := Self;
End;

function TInt64Helper.IsInRange(Low, High: Int64): Boolean;
Begin
  result := ((Self >= Low) and (Self <= High));
End;

Function TInt64Helper.Random:Int64;
Begin
  Result := Self.RandomRange(Self.MinValue,Self.MaxValue);
End;

{ Source : https://oroboro.com/large-random-in-range/ }
Function TInt64Helper.RandomRange(vMin, vMax :Int64):Int64;
Var
  Diff : QWord;
  i, rLow, rHigh, vLow, vHigh :Int64;
Begin
  Diff := vMax - vMin;
  if (Diff<=Int64.MaxValue) then
  begin
    I := Diff;
    result := System.Random(I)+vMin;
  end
  else
  begin
    rLow := System.Random(Int64.MaxValue);
    rHigh := System.Random(Int64.MaxValue);
    vLow := Diff and $FFFFFFFF;
    vHigh := Diff shr 32;
    result  :=  (( rHigh * vLow ) shr 32 )
              + (( rLow * vHigh ) shr 32 )
              + ( rHigh * vHigh )
              + vMin;
  end;
End;


function TInt64Helper.Clamp(vMin, vMax: Int64): Int64;
begin
  Result := Self;
  if Result > vMax then begin Result := vMax; exit; end;
  if Result < vMin then Result := vMin;
end;

function TInt64Helper.Sign: Int64;
begin
  If Self < 0 Then
    Result := -1
  Else If Self > 0 Then
    Result := 1
  Else
    Result := 0;
end;

function TInt64Helper.ToByte: Byte;
Begin
  Result := ClampByte(Self);
End;

function TInt64Helper.ToBoolean: Boolean;
begin
  Result := (Self>0);
end;

function TInt64Helper.ToHexString: string;
begin
  Result:=IntToHex(Self,16)
end;

{%endregion%}

{%region%=====[ TQWordHelper ]==================================================}

function TQWordHelper.Parse(Num: String; const defValue: QWord): Boolean;
Var
  I: QWord;
Begin
  Result := false;
  Self := defValue;
  if TryStrToQWord(Num,I) then
  begin
    Self := I;
    result := true;
  End;
End;

function TQWordHelper.ToString(const Formatted: Boolean): string;
Begin
  if not(Formatted) then
    Result := IntToStr(Self)
  else
    Result := Format('%.0n',[Self+0.0],vPointSeparator);
End;

function TQWordHelper.Min(vMin: QWord): QWord;
Begin
  if Self<vMin then result := vMin else result := Self;
End;

function TQWordHelper.Max(vMax: QWord): QWord;
Begin
  if Self>vMax then result := vMax else result := Self;
End;

function TQWordHelper.IsInRange(Low, High: QWord): Boolean;
Begin
  result := ((Self >= Low) and (Self <= High));
End;

Function TQWordHelper.Random:QWord;
Begin
  Result := Self.RandomRange(Self.MinValue,Self.MaxValue);
End;

Function TQWordHelper.RandomRange(vMin, vMax :QWord):QWord;
Var
  Diff : QWord;
  i, rLow, rHigh, vLow, vHigh :Int64;
Begin
  Diff := vMax - vMin;
  if (Diff<=Int64.MaxValue) then
  begin
    I := Diff;
    result := System.Random(I)+vMin;
  end
  else
  begin
    rLow := System.Random(Int64.MaxValue);
    rHigh := System.Random(Int64.MaxValue);
    vLow := Diff and $FFFFFFFF;
    vHigh := Diff shr 32;
    result  :=  (( rHigh * vLow ) shr 32 )
              + (( rLow * vHigh ) shr 32 )
              + ( rHigh * vHigh )
              + vMin;
  end;
End;

function TQWordHelper.Clamp(vMin, vMax: QWord): QWord;
begin
  Result := Self;
  if Result > vMax then begin Result := vMax; exit; end;
  if Result < vMin then Result := vMin;
end;

function TQWordHelper.ToBoolean: Boolean;
begin
  Result := (Self>0);
end;

function TQWordHelper.ToHexString: string;
begin
  Result:=IntToHex(Self,15);
end;

{%endregion%}

{%region%=====[ TSingleHelper ]=================================================}

function TSingleHelper.ToString(const Decimals: Integer ): string; //Const FormatSetting :TFormatSettings
Begin
  result := FloatToStrF(Self, ffNumber, 15 , Decimals, vPointSeparator);
End;

function TSingleHelper.Parse(Num: String; const defValue: Single): Boolean;
var
  I: Single;
  Ok : Boolean;
Begin
  result := False;
  Self := DefValue;
  if {%H-}Num.Contains('.') then Ok := TryStrToFloat(Num,I,vPointSeparator)
  else if Num.Contains(',') then Ok := TryStrToFloat(Num,I,vCommaSeparator)
  else Ok := TryStrToFloat(Num,I);
  if Ok then
  begin
    Result := True;
    Self := I;
  End;
End;

function TSingleHelper.Min(vMin: Single): Single;
Begin
  if Self<vMin then result := vMin else result := Self;
End;

function TSingleHelper.Max(vMax: Single): Single;
Begin
  if Self>vMax then result := vMax else result := Self;
End;

function TSingleHelper.IsInRange(Low, High: Single): Boolean;
begin
  result := ((Self >= Low) and (Self <= High));
end;

function TSingleHelper.Random : Single;
Begin
   Result :=Self.RandomRange(Self.MinValue, Self.MaxValue);
End;

function TSingleHelper.RandomRange(vMin,vMax : Single): Single;
Begin
   Result := Random * (vMax - vMin) + vMin;
End;

function TSingleHelper.Clamp(vMin, vMax: Single): Single;
begin
  Result := Self;
  if Result > vMax then begin Result := vMax; exit; end;
  if Result < vMin then Result := vMin;
end;

function TSingleHelper.Trunc: Integer;
begin
  result := System.Trunc(Self);
end;

function TSingleHelper.Round: Integer;
begin
  result := System.Round(Self);
end;

function TSingleHelper.Floor: Integer;
Begin
   {$HINTS OFF}
  result :=0;
  if (Self=0.0) then exit
  else If (Self > 0) Then
    Result := System.Trunc(Self)
  Else
    Result := System.Trunc(Self-0.999999999);
  {$HINTS ON}
End;

function TSingleHelper.Ceil: Integer;
begin
 {$HINTS OFF}
 Result := System.Trunc(Self);
 if (Self - Result) > 0 then Inc(Result);
 {$HINTS ON}
end;

function TSingleHelper.RoundInt: Single;
begin
 {$HINTS OFF}
 //Result := system.int
 Result := System.Round(Self + 0.5);
 {$HINTS ON}
end;

function TSingleHelper.Fract: Single;
begin
  result := Self - System.trunc(Self);
end;

function TSingleHelper.Sign: Single;
begin
 If Self < 0.0 Then
   Result := -1.0
 Else If Self > 0.0 Then
   Result := 1.0
 Else
   Result := 0.0;
end;

function TSingleHelper.Reciprocal: Single;
Const
  cEpsilon : Single = 1.4012984643248170709e-45;
Var
  a: Single;
Begin
  Result := 0;
  If Self = 0 Then exit;
  a := Self.Sign;
  If ((a * Self) >= cEpsilon) Then
    Result := 1.0 / Self
  Else
    Result := a * (1.0 / cEpsilon);
End;

function TSingleHelper.IsInfinity: Boolean;
begin
  result := (Self = Self.NegativeInfinity) or (Self = Self.PositiveInfinity);
end;

function TSingleHelper.IsNan: Boolean;
begin
  result := (Self = Self.NaN);
end;

function TSingleHelper.IsNegativeInfinity: Boolean;
begin
  result := (Self = Self.NegativeInfinity)
end;

function TSingleHelper.IsPositiveInfinity: Boolean;
begin
  Result := (Self = Self.PositiveInfinity);
end;

{%endregion%}

{%region%=====[ TDoubleHelper ]=================================================}

function TDoubleHelper.ToString(const Decimals: Integer): string;
Begin
  result := FloatToStrF(Self, ffNumber, 15 , Decimals);
End;

function TDoubleHelper.Parse(Num: String; const defValue: Double): Boolean;
var
  I:Double;
Begin
  result := False;
  Self := DefValue;
  if TryStrToFloat(Num,I) then
  begin
    Result := True;
    Self := I;
  End;
End;

function TDoubleHelper.Min(vMin: Double): Double;
Begin
  if Self<vMin then result := vMin else result := Self;
End;

function TDoubleHelper.Max(vMax: Double): Double;
Begin
  if Self>vMax then result := vMax else result := Self;
End;

function TDoubleHelper.IsInRange(Low, High: Double): Boolean;
begin
  result := ((Self >= Low) and (Self <= High));
end;

function TDoubleHelper.Random : Double;
Begin
   Result :=Self.RandomRange(Self.MinValue, Self.MaxValue);
End;

function TDoubleHelper.RandomRange(vMin,vMax : Double): Double;
Begin
   Result := Random * (vMax - vMin) + vMin;
End;

function TDoubleHelper.Clamp(vMin, vMax: Double): Double;
begin
  Result := Self;
  if Result > vMax then begin Result := vMax; exit; end;
  if Result < vMin then Result := vMin;
end;

function TDoubleHelper.Trunc: Integer;
begin
  result := System.Trunc(Self);
end;

function TDoubleHelper.Round: Integer;
begin
  result := System.Round(Self);
end;

function TDoubleHelper.Floor: Integer;
Begin
   {$HINTS OFF}
  result :=0;
  if (Self=0.0) then exit
  else If (Self > 0) Then
    Result := System.Trunc(Self)
  Else
    Result := System.Trunc(Self-0.99999999999999999);
  {$HINTS ON}
End;

function TDoubleHelper.Ceil: Integer;
begin
 {$HINTS OFF}
 Result := System.Trunc(Self);
 if (Self - Result) > 0 then Inc(Result);
 {$HINTS ON}
end;

function TDoubleHelper.RoundInt: Double;
begin
 {$HINTS OFF}
 //Result := system.int
 Result := System.Round(Self + 0.5);
 {$HINTS ON}
end;

function TDoubleHelper.Fract: Double;
begin
  result := Self - System.trunc(Self);
end;

function TDoubleHelper.Sign: Double;
begin
  If Self < 0.0 Then
    Result := -1.0
  Else If Self > 0.0 Then
    Result := 1.0
  Else
    Result := 0.0;
end;

function TDoubleHelper.Reciprocal: Double;
Const
  cEpsilon : Single = 1.4012984643248170709e-45;
Var
  a: Double;
Begin
  Result := 0;
  If Self = 0 Then exit;
  a := Self.Sign;
  If ((a * Self) >= cEpsilon) Then
    Result := 1.0 / Self
  Else
    Result := a * (1.0 / cEpsilon);
End;

function TDoubleHelper.IsInfinity: Boolean;
begin
 Result := (Self = Self.NegativeInfinity) or (Self = Self.PositiveInfinity);
end;

function TDoubleHelper.IsNan: Boolean;
begin
 Result := (Self = Self.NaN);
end;

function TDoubleHelper.IsNegativeInfinity: Boolean;
begin
 Result := (Self = Self.NegativeInfinity);
end;

function TDoubleHelper.IsPositiveInfinity: Boolean;
begin
 Result := (Self = Self.PositiveInfinity);
end;

{%endregion%}

{%region%=====[ TStringHelper ]=================================================}

procedure TStringHelper.SetFromSys(aString : String);
Begin
  Self := SysToUTF8(aString);
End;

procedure TStringHelper.SetFromConsole(aString : String);
Begin
  Self := ConsoleToUTF8(aString);
End;

procedure TStringHelper.SetToUTF8(aString : String);
begin
  Self := Utf8EscapeControlChars(AString);
End;

function TStringHelper.ToSys : String;
Begin
  result := UTF8ToSys(Self);
End;

function TStringHelper.ToConsole : String;
Begin
  result := UTF8ToConsole(Self); //UTF8ToWinCP(Self);
End;


function TStringHelper.Trim: String;
var i, l: Integer;
begin
  l := UTF8Length(Self);
  i := 1;
  while (i <= l) and (Self[i] <= ' ') do Inc(i);
  if i > l then Result := '' else
  begin
    while Self[l] <= ' ' do Dec(l);
    Result := UTF8Copy(Self, i, l - i + 1);
  end;
End;

function TStringHelper.TrimLeft: String;
var i, l : Integer;
begin
  l := UTF8Length(Self);
  i := 1;
  while (i <= l) and (Self[i] <= ' ') do Inc(i);
  Result := UTF8Copy(Self, i, Maxint);
End;

function TStringHelper.TrimRight: String;
var i : Integer;
begin
  i := UTF8Length(Self);
  while (i > 0) and (Self[i] <= ' ') do Dec(i);
  Result := UTF8Copy(Self, 1, i);
End;

function TStringHelper.IsEmpty: Boolean;
var i,l : Integer;
begin
  l := UTF8Length(Self);
  Result := False;
  for i := 1 to l do if Self[i]>=' ' then Exit;
  Result := True;
End;

Function TStringHelper.GetChar(AIndex : Integer) : Char;
begin
  Result := Self[1];
  if AIndex>0 then
    if (AIndex<=Length) then result := Self[AIndex] else result := Self[Length];
end;

Function TStringHelper.GetLength : Integer;
Begin
  Result := UTF8Length(Self);
End;

function TStringHelper.ToUpper: String;
Begin
  Result := UTF8UpperString(Self);
End;

function TStringHelper.ToLower: String;
Begin
  Result := UTF8LowerString(Self);
End;

function TStringHelper.IsEquals(const Value : string; Const IgnoreCase : Boolean) : Boolean;
Begin
  Result := (Self.Compare(Value,IgnoreCase) = 0);
End;

function TStringHelper.Compare(const Value : string; Const IgnoreCase : Boolean) : Integer;
Var
  S1,S2 : String;
Begin

  if IgnoreCase then
  Begin
    S1 := Self.ToUpper;
    S2 := {%H-}Value.ToUpper;
  End
  else
  begin
    S1 := Self;
    S2 := Value;
  End;
  Result := UTF8CompareText(S1,S2);
End;

procedure TStringHelper.RepeatChar(C: Char; Count: Integer);
begin
  Self:='';
  SetLength(Self, Count);
  if Count>0
    then FillChar(Self[1], Count, c);
end;

procedure TStringHelper.RepeatStr(const s: String; Count: Integer);
var  p : PChar;
  Slen : Integer;
begin
  SLen := UTF8Length(s);
  SetLength(Self, Count*SLen);
  p := PChar(Self);
  while Count > 0 do
  begin
    Move(PChar(Self)^, p^, SLen);
    Inc(p, SLen);
    Dec(Count);
  end;
end;

function TStringHelper.PadCenter(TotalLen : Integer; Const PadChar : Char) : String;
Var
  l:Integer;
  S:String;
Begin
  l := UTF8Length(Self);
  if l < TotalLen then
  begin
    S:='';
    S.RepeatChar(PadChar, (TotalLen div 2) - (L div 2));
    Result := S + Self;
    S.RepeatChar(PadChar, TotalLen - UTF8Length(Result));
    Result := Result + S;
  end else Result := Self;
End;

function TStringHelper.PadCharRight(PadLen : Integer; Const PadChar : Char) : String;
var  i : Integer;
  More : Integer;
  Slen : Integer;
begin
  SLen := UTF8Length(Self);
  More := Abs(PadLen) - Slen;
  if More>0 then
  begin
    if PadLen<0 then
    begin
      SetLength(Result{%H-}, Abs(PadLen));
      Move(Self[1], Result[More+1], Slen);
      for i := 1 to More do Result[i] := PadChar;
    end else
    begin
      Result := Self;
      SetLength(Result, Abs(PadLen));
      for i := SLen+1 to Slen+More do Result[i] := PadChar;
    end;
  end else Result := Self;
End;

function TStringHelper.PadCharLeft(PadLen : Integer; Const PadChar : Char) : String;
Begin
  Result := Self.PadCharRight(-PadLen, PadChar);
End;

function TStringHelper.Pos(const SubStr: String; StartPos: integer): Integer;
Begin
  Result := UTF8Pos(SubStr,Self,StartPos);
End;

function TStringHelper.PosBetween(const SubStr1, SubStr2: String; var StartPos, EndPos: Integer): Boolean;
var REndPos, RStartPos :Integer;
Begin
  Result:=False;
  StartPos:=-1;
  EndPos:=-1;
  RStartPos:= UTF8Pos(Substr1,Self);
  REndPos:= UTF8Pos(Substr2,Self);
  if (RStartPos>0) And (REndPos>0) then
  begin
    result:=True;
    StartPos:=RStartPos+UTF8Length(Substr1);
    EndPos:=REndpos-1;
  end;
End;

function TStringHelper.IndexOf(const SubStr: string; Start: Integer;IgnoreCase: Boolean): Integer;
var
  S1, S2 : String;
Begin
  S1 := Self;
  S2 := SubStr;
  if IgnoreCase then
  Begin
    S1 := Self.ToUpper;
    S2 := {%H-}SubStr.ToUpper;
  End;
  Result := S1.Pos(S2, Start);
End;

function TStringHelper.After(const SubStr: String; Position: Integer): String;
var p,L,Start : Integer;
begin
  p := UTF8Pos(SubStr, Self,Position);
  Result := '';
  if p>=0 then
  begin
    Start:=p+UTF8Length(SubStr);
    L:=UTF8Length(Self)-(Start-1);
    Result := UTF8Copy(Self, Start, L);
  end;
End;

function TStringHelper.Before(const SubStr: String; Position: Integer): String;
var p , L: Integer;
begin
  p := UTF8Pos(SubStr, Self, Position);
  Result := '';
  if p>=0 then
  begin
    L:=p-1;
    Result := Self.Copy(position, L);
  end;
End;

function TStringHelper.Between(const SubStr1, SubStr2: String): String;
var StartPos,EndPos : Integer;
begin
  StartPos:=0;
  EndPos:=0;
  if Self.PosBetween(SubStr1, SubStr2, StartPos, EndPos) then
  begin
    EndPos := EndPos + 1;
    Result := UTF8Copy(Self, StartPos, (EndPos-StartPos));
  end
  else Result := '';
End;

function TStringHelper.Mid(const SubStr1, SubStr2: String; Position: Integer): String;
var p1,p2, ps,pe : Integer;
begin
  p1 := UTF8Pos(SubStr1, Self, Position);
  if p1<=0 then Result := ''
  else
  begin
    if SubStr2='' then p2 := UTF8Pos(SubStr1, Self, p1+UTF8Length(SubStr1))
    else p2 := UTF8Pos(SubStr2, Self, p1+UTF8Length(SubStr1));
    if p2<=0 then
    begin
      ps :=  p1+UTF8Length(SubStr1);
      pe := UTF8Length(Self);
      Result := UTF8Copy(Self,ps, pe)
    end
    else
    begin
      ps :=  p1+UTF8Length(SubStr1);
      pe :=  p2-ps;
      Result := UTF8Copy(Self,ps ,pe);
    end;
  end;
End;

function TStringHelper.Copy(aStart, aLength: Integer): string;
var
  L : Integer;
begin
  L := UTF8Length(Self);
  result := Self;
  if (L=0)  or (aStart < 1) or (aLength < 1) then Exit;
  if ((aStart + aLength) > L) then aLength := (L - aStart)+1;
  SetLength(Result,aLength);
  Move(Self[aStart], Result[1], aLength);
 //Result := UTF8Copy(Self,AStart,ALength);
End;

function TStringHelper.CopyPos(StartPos, EndPos: Integer): String;
var Len:integer;
begin
  if EndPos<StartPos then
  begin
    Len:=StartPos+EndPos;
  end
  else
  begin
    Len:=EndPos-StartPos;
  end;
  result:=Self.Copy(StartPos, Len+1);
End;

function TStringHelper.Insert(const SubStr: string; Position: Integer): String;
Begin
  Result := Self;
  UTF8Insert(Substr,Result,Position);
End;

function TStringHelper.LeftOf(Position: Integer): String;
Begin
  Result := UTF8Copy(Self, 1, Position-1);
End;

function TStringHelper.RightOf(Position: Integer): String;
Begin
  Result := Self.Copy(Position+1, UTF8Length(Self));
End;

function TStringHelper.ToWideString: WideString;
Begin
 result := UTF8ToUTF16(Utf8EscapeControlChars(Self));
End;

procedure TStringHelper.SetWideString(S: WideString);
begin
  Self :='';
  if System.Length(S) < 1 then Exit;
  WideCharToStrVar(PWideChar(S), Self);
  Utf8EscapeControlChars(Self)
End;

procedure TStringHelper.Parse(Num : Integer);
Begin
  Self := IntToStr(Num);
End;

procedure TStringHelper.Parse(Num : Single; Const Decimals : Integer);
Begin
  Self := FloatToStrF(Num, ffNumber, 15 , Decimals, vPointSeparator);
End;

procedure TStringHelper.Parse(Num : Double; Const Decimals : Integer);
Begin
  Self := FloatToStrF(Num, ffNumber, 15 , Decimals, vPointSeparator);
End;

function TStringHelper.Surround(chs: string): string;
Begin
  Result := chs + Self + chs;
End;

function TStringHelper.Surround(chsL, chsR: string): string;
Begin
  Result := chsL + Self + chsR
End;

procedure TStringHelper.Implode(lst: TStringList; sep: string);
var
  i,j : integer;
  s : string;
begin
 S:='';
 j:= lst.Count - 1;
 for i:=0 to j do
 begin
   if i < j then s := s + lst[i] + sep
   else s := s + lst[i];  // don't add last separator
 end;
 Self := s;
End;

function TStringHelper.Explode(sep: string): TStringList;
var
 p : integer;
begin
  p := Self.Pos(sep);
  Result := TStringList.Create;
  while p > 0 do
  begin
    Result.Add(Self.Copy(1,p-1));
    if p <= Self.GetLength then Self := Self.Copy(p+ UTF8length(sep),Self.GetLength);
    p := Self.Pos(sep);
  end;
  Result.Add(Self);
End;

function TStringHelper.Contains(const SubStr: string; IgnoreCase: Boolean): Boolean;
Begin
  if IgnoreCase then
  begin
    Result := (Self.ToUpper.Pos({%H-}Substr.ToUpper) > 0);
  end
  else Result := (Self.Pos(Substr) > 0);
End;

function TStringHelper.Quote: String;
Begin
  Result := Self.Surround('"');
End;

function TStringHelper.Replace(const OldPattern, NewPattern: string; IgnoreCase: Boolean): string;
var rFlag : TReplaceFlags;
Begin
  rFlag := [rfReplaceAll];
  if IgnoreCase then rflag := rFlag + [rfIgnoreCase];
  Result := UTF8StringReplace(Self, OldPattern, NewPattern,rFlag);
End;

function TStringHelper.RemoveLeft(StartIndex : Integer) : String;
Var
  L : Integer;
Begin
 L := UTF8Length(Self)-StartIndex;
 Result := Self.Copy(StartIndex,L);
End;

function TStringHelper.RemoveLeft(StartIndex, aCount : Integer) : String;
Var
  L : Integer;
Begin
 L := UTF8Length(Self)-StartIndex;
 Result := Self.Copy(0,StartIndex-aCount)+Self.Copy(StartIndex,L);
End;

function TStringHelper.RemoveRight(StartIndex : Integer) : String;
Begin
 Result := Self.Copy(0,StartIndex);
End;

function TStringHelper.RemoveRight(StartIndex, aCount : Integer) : String;
Var
  L : Integer;
Begin
  L := UTF8Length(Self)-StartIndex-aCount;
  Result := Self.Copy(0,StartIndex)+Self.Copy(StartIndex+aCount,L);
End;

function TStringHelper.RemoveChar(aChar : Char; Const IgnoreCase : Boolean) : String;
var
  I,L : Integer;
  c, c1 : Char;
  c2 : Char;
Begin
  L := UTF8Length(Self);
  Result := '';
  if IgnoreCase then c2 := {%H-}aChar.ToUpper else c2 := aChar;
  For I:=1 to L do
  begin
    c := Self[I];
    if IgnoreCase then c1 := c.ToUpper else c1 := c;
    if (c1<>c2) then result := result + c;
  End;
End;

function TStringHelper.Remove(SubStr : String; Const IgnoreCase : Boolean) : String;
Begin
  Result := Self.Replace(SubStr,'',IgnoreCase);
End;

function TStringHelper.Reverse: String;
Begin
  Result := Utf8ReverseString(Self);
End;

function TStringHelper.Wrap(MaxCol: Integer): String;
Begin
  Result := UTF8WrapText(Self,MaxCol);
End;

function TStringHelper.CountChars(C: Char): Integer;
Var i,j,L:Integer;
begin
 L:= UTF8Length(Self);
 J:=0;
 For i := 1 to L do
 begin
   if (Self[i] = C) then inc(J);
 end;
 Result := J;
end;

function TStringHelper.ToBoolean(Const DefTrue : String; Const defFalse : String; Const IgnoreCase : Boolean) : Boolean;
begin
  if Self.Contains(DefTrue,IgnoreCase) then result := True
  else if Self.Contains(DefFalse,IgnoreCase) then result := False else result := True;
end;

function TStringHelper.ToInteger: Integer;
Var
  I: Integer;
Begin
  Result := 0;
  if TryStrToInt(Self,I) then
  begin
    result := I;
  End;
end;

function TStringHelper.ToSingle: Single;
var
  I: Single;
  Ok : Boolean;
Begin
  result := 0.0;
  if Self.Contains('.') then Ok := TryStrToFloat(Self,I,vPointSeparator)
  else if Self.Contains(',') then Ok := TryStrToFloat(Self,I,vCommaSeparator)
  else Ok := TryStrToFloat(Self,I);
  if Ok then
  begin
    Result := I;
  End;
end;

function TStringHelper.ToDouble: Double;
var
  I: Double;
  Ok : Boolean;
Begin
  result := 0.0;
  if Self.Contains('.') then Ok := TryStrToFloat(Self,I,vPointSeparator)
  else if Self.Contains(',') then Ok := TryStrToFloat(Self,I,vCommaSeparator)
  else Ok := TryStrToFloat(Self,I);
  if Ok then
  begin
    Result := I;
  End;
end;

function TStringHelper.ToInt64 : Int64;
Var
  I: Int64;
Begin
  Result := 0;
  if TryStrToInt64(Self,I) then
  begin
    result := I;
  End;
end;

function TStringHelper.ToByte: Byte;
Var
  I: Integer;
Begin
  Result := 0;
  if TryStrToInt(Self,I) then
  begin
    result := ClampByte(I);
  End;
end;

function TStringHelper.ComputeHash(Const IgnoreCase : Boolean = False) : Integer;
Var
  i, j: Integer;
Begin
  Result :=-1;
  I := UTF8Length(Self);
  J := Ord(Self[I]);
  Repeat
    if IgnoreCase then J := (J * cHash1 + Ord(UpCase(Self[I])))
    else J := (J * cHash1 + Ord(Self[I]));
    Dec(I);
  Until I = 0;
  if IgnoreCase then Result := (J Mod cHash2) else Result := abs(J Mod cHash2);
End;

{%endregion%}

{%region%=====[ TDateTimeHelper ]===============================================}

function TDateTimeHelper.getYear: Word;
var
  Y, M, D: Word;
begin
  DecodeDate(Self, Y, M, D);
  Result := Y;
end;

function TDateTimeHelper.getMonth: Word;
var
  Y, M, D: Word;
begin
  DecodeDate(Self, Y, M, D);
  Result := M;
end;

function TDateTimeHelper.getDay: Word;
var
  Y, M, D: Word;
begin
  DecodeDate(Self, Y, M, D);
  Result := D;
end;

function TDateTimeHelper.getHour: Word;
var
  H,M,S,MS: Word;
begin
  DecodeTime(Self,H,M,S,MS);
  Result := H;
end;

function TDateTimeHelper.getMinute: Word;
var
  H,M,S,MS: Word;
begin
  DecodeTime(Self,H,M,S,MS);
  Result := M;
end;

function TDateTimeHelper.getSecond : Word;
var
  H,M,S,MS: Word;
begin
  DecodeTime(Self,H,M,S,MS);
  Result := S;
end;

Function TDateTimeHelper.getMillisecond : Word;
var
  H,M,S,MS: Word;
begin
  DecodeTime(Self,H,M,S,MS);
  Result := MS;
end;

procedure TDateTimeHelper.SetToDay;
Begin
  Self := Date();
End;

procedure TDateTimeHelper.SetToNow;
Begin
  Self := Now;
End;

Procedure TDateTimeHelper.SetToTime;
Begin
  Self := Time();
End;

procedure TDateTimeHelper.SetTime(H, M, S : Integer);
Begin
 if not(TryEncodeDateTime(Year,Month,Day,H,M,S,0, Self)) then Self.SetToNow;
End;

procedure TDateTimeHelper.SetDate(Y, M, D : Integer);
Begin
 if not(TryEncodeDateTime(Y,M,D,Hour,Minute,Second,0, Self)) then Self.SetToNow;
End;

procedure TDateTimeHelper.SetFromString(ADateTimeStr : String; Const APAttern : String);
Begin
  Self := ScanDateTime(APattern, ADateTimeStr);
End;

procedure TDateTimeHelper.SetFromUnixTimeStamp(ADate : Int64);
Begin
  Self := UnixToDateTime(ADate);
End;

procedure TDateTimeHelper.SetFromMacTimeStamp(ADate : Int64);
Begin
  Self := MacToDateTime(ADate);
End;

procedure TDateTimeHelper.SetFromUTC(UT : TDateTime);
Begin
  Self :=  UniversalTimeToLocal(UT);
End;

procedure TDateTimeHelper.SetFromUTC(UT : TDateTime; ZOffset : Integer);
Begin
  Self :=  UniversalTimeToLocal(UT,ZOffset);
End;

procedure TDateTimeHelper.SetFromJulian(JulianDate : Double);
Begin
 Self := JulianDateToDateTime(JulianDate);
End;

procedure TDateTimeHelper.SetFromSQL(ADataTimeStr : String);
var
  YYYY, MM, DD: word;
  S:String;
begin
  try
    S := ADataTimeStr.Copy(1, 4);
    YYYY := S.ToInteger;
    // années sur deux chiffres
    if (YYYY < 100) then
    begin
      if (YYYY < 50) then  YYYY := 2000 + YYYY
                     else  YYYY := 1900 + YYYY;
    end;
    MM   := ADataTimeStr.Copy(6, 2).ToInteger;
    DD   := ADataTimeStr.Copy(9, 2).ToInteger;
    Self := EncodeDate(YYYY, MM, DD);
  except
    Self := TDateTime(0.0);
  end;
End;

{ Source : http://codes-sources.commentcamarche.net/source/36519-unite-randdate-fonction-randrangedate-et-randomdate-generatrices-de-dates-aleatoires }
procedure TDateTimeHelper.SetRandomDate;
var aD,
    MaxD,D,
    MaxM,M,
    MaxY,Y : Integer;
begin
  MaxD := 31;
  MaxM := 12;
  MaxY := 9999;

  Y := RandomRange(1,MaxY+1);
  Y:=Y.Clamp(1,MaxY);
  if Y = MaxY then
  begin
     M := RandomRange(1,MaxM+1);//.Clamp(1,MaxM);
     M := M.Clamp(1,MaxM);
  end
  else
  begin
     M := RandomRange(1,12+1).Clamp(1,12);
     //M:= M.Clamp(1,12);
  end;
  aD := 0;
  case M of
     2               : if SysUtils.IsLeapYear(Y) then aD := 29 else aD := 28;
     4,6,9,11        : aD := 30;
     1,3,5,7,8,10,12 : aD := 31;
  end;
  if M = MaxM then
  Begin
     D := RandomRange(1,MaxD+1);
     D := D.Clamp(1,MaxD)
  end
  else
  begin
     D := RandomRange(1,aD+1);
     D := D.Clamp(1,aD);
  end;
  Self := EncodeDate(Y,M,D);
End;

procedure TDateTimeHelper.SetRandomDate(StartDate, EndDate : TDateTime);
var aD,
    MinD,MaxD,D,
    MinM,MaxM,M,
    MinY,MaxY,Y : Word;
begin
  DecodeDate(StartDate,MinY,MinM,MinD);
  DecodeDate(EndDate,MaxY,MaxM,MaxD);
  Y := RandomRange(MinY,MaxY+1);
  Y := Y.Clamp(MinY,MaxY);
  if Y = MinY then
  begin
     M := RandomRange(MinM,12+1);
     M := M.Clamp(MinM,12);
  end
  else if Y = MaxY then
  begin
     M := RandomRange(1,MaxM+1);
     M := M.Clamp(1,MaxM);
  end
  else
  begin
     M := RandomRange(1,12+1).Clamp(1,12);
    // M := M.Clamp(1,12);
  end;
  aD := 0;
  case M of
    2  : if SysUtils.IsLeapYear(Y) then aD := 29 else aD := 28;
    4,6,9,11  : aD := 30;
    1,3,5,7,8,10,12 : aD := 31;
  end;

  if M = MinM then
  Begin
     D := RandomRange(MinD,aD+1);
     D := D.Clamp(MinD,aD);
  end
  else
  if M = MaxM then
  Begin
     D := RandomRange(1,MaxD+1);
     D := D.Clamp(1,MaxD);
  end
  else
  Begin
     D := RandomRange(1,aD+1);
     D := D.Clamp(1,aD);
  end;

  Self := EncodeDate(Y,M,D);
end;

procedure TDateTimeHelper.SetFromFile(aFileName : String);
Var
  fa : Longint;
Begin
  fa:=FileAgeUTF8(aFileName);
  If Fa<>-1 then
  begin
    Self :=FileDateToDateTime(fa);
  end;
End;

function TDateTimeHelper.ToString(Const Format : TDateFormat; Const CustomFormat : string) : string;
begin
  if Format = dfGMT then
    Result := FormatDateTime('ddd, d mmm yyyy hh:nn:ss', Self) + ' GMT'
  else if Format = dfUTC then
    Result := FormatDateTime('ddd, d mmm yyyy hh:nn:ss', Self) + ' UTC'
  else
    result := FormatDateTime(CustomFormat, Self)
end;

function TDateTimeHelper.ToSQL : String;
var
  YYYY, MM, DD: word;
begin
  DecodeDate(Self, YYYY, MM, DD);
  Result := Format('%.4d-%.2d-%.2d', [YYYY, MM, DD]);
End;

function TDateTimeHelper.DateToString(Const FormatStr : string) : String;
Begin
  Result :=FormatDateTime(FormatStr, Self);
End;

function TDateTimeHelper.TimeToString(Const FormatStr : string) : String;
Begin
  Result :=FormatDateTime(FormatStr, Self);
End;

function TDateTimeHelper.Compare(SecondDateTime : TDatetime) : Integer;
Begin
  Result := DateUtils.CompareTime(Self,SecondDateTime);
End;

function TDateTimeHelper.CompareDate(SecondDate : TDateTime) : Integer;
Begin
  Result := DateUtils.CompareDate(Self,SecondDate);
End;

function TDateTimeHelper.CompareTime(SecondTime : TDateTime) : Integer;
Begin
  Result := DateUtils.CompareTime(Self,SecondTime);
End;

function TDateTimeHelper.AddYear(const A : Integer) : TDateTime;
Begin
  Result := IncYear(Self,A)
End;

function TDateTimeHelper.AddMonth(const A : Integer) : TDateTime;
Begin
  Result := IncMonth(Self,A)
End;

function TDateTimeHelper.AddDay(const A : Integer) : TDateTime;
Begin
  Result := IncDay(Self,A)
End;

function TDateTimeHelper.AddWeek(const A : Integer) : TDateTime;
Begin
  Result := IncWeek(Self,A)
End;

function TDateTimeHelper.AddHour(const A : Integer) : TDateTime;
Begin
  Result := IncHour(Self,A)
End;

function TDateTimeHelper.AddMinute(const A : Integer) : TDateTime;
begin
  Result := IncMinute(Self,A)
end;

function TDateTimeHelper.AddSecond(const A : Integer) : TDateTime;
Begin
  Result := IncSecond(Self,A)
End;

function TDateTimeHelper.AddMilliSecond(const A : Integer) : TDateTime;
Begin
  Result := IncMilliSecond(Self,A)
End;

function TDateTimeHelper.ToJulian : Double;
Begin
   result := DateTimeToJulianDate(Self);
End;

function TDateTimeHelper.ToUnixTimeStamp : Int64;
Begin
  result := DateTimeToUnix(Self);
End;

function TDateTimeHelper.ToMacTimeStamp : Int64;
Begin
   Result := DateTimeToMac(Self);
End;

function TDateTimeHelper.ToUTC : TDateTime;
Begin
  result := LocalTimeToUniversal(Self);
End;

function TDateTimeHelper.ToUTC(ZOffset : Integer) : TDateTime;
Begin
 result := LocalTimeToUniversal(Self,ZOffset);
End;

function TDateTimeHelper.IsLeapYear : Boolean;
Begin
  Result := IsInLeapYear(Self);
End;

function TDateTimeHelper.GetDaysInYear : Word;
Begin
  result := DaysInYear(Self);
End;

function TDateTimeHelper.GetDaysInMonth : Word;
Begin
 result := DaysInMonth(Self);
End;

function TDateTimeHelper.GetWeeksInYear : Word;
Begin
 result := WeeksInYear(Self);
End;

function TDateTimeHelper.GetDayOfTheWeek : Word;
Begin
  Result := DayOfTheWeek(Self);
End;

function TDateTimeHelper.GetWeekOfTheYear : Word;
Begin
  Result := WeekOfTheYear(Self);
End;

function TDateTimeHelper.GetElapsedYears(ATo : TDateTime) : Integer;
Begin
   Result :=  YearsBetween(Self,ATo);
End;

function TDateTimeHelper.GetElapsedMonths(ATo : TDateTime) : Integer;
Begin
   Result :=  MonthsBetween(Self,ATo);
End;

function TDateTimeHelper.GetElapsedWeeks(ATo : TDateTime) : Integer;
Begin
  Result :=  WeeksBetween(Self,ATo);
End;

function TDateTimeHelper.GetElapsedDays(ATo : TDateTime) : Integer;
Begin
  Result :=  DaysBetween(Self,ATo);
End;

function TDateTimeHelper.GetElapsedHours(ATo : TDateTime) : Int64;
Begin
  Result :=  HoursBetween(Self,ATo);
End;

function TDateTimeHelper.GetElapsedMinutes(ATo : TDateTime) : Int64;
Begin
  Result :=  MinutesBetween(Self,ATo);
End;

function TDateTimeHelper.GetElapsedSeconds(ATo : TDateTime) : Int64;
Begin
  Result :=  SecondsBetween(Self,ATo);
End;

function TDateTimeHelper.GetElapsedMilliSeconds(ATo : TDateTime) : Int64;
Begin
  Result :=  MilliSecondsBetween(Self,ATo);
End;

function TDateTimeHelper.GetElapsedPeriod(ATo : TDateTime) : TDateTime;
Var
  Y,M,D : Word;
Begin
  PeriodBetween(Self, ATo, Y, M, D);
  Result := EncodeDate(Y,M,D);
End;

{$IFDEF WINDOWS}
procedure TDateTimeHelper.FromFileTime(const FileTime : TFileTime);
const
  FileTimeBase = -109205.0;
  FileTimeStep: Extended = 24.0 * 60.0 * 60.0 * 1000.0 * 1000.0 * 10.0; // 100 nSek per Day
Var
  Tmp : TDateTime;
begin
  Tmp := Int64(FileTime) / FileTimeStep;
  Self := Tmp + FileTimeBase;
end;
{$ENDIF}

{%endregion%}

//==============================================================================

Initialization

  // Format settings to convert a string to a float
  vPointSeparator := DefaultFormatSettings;
  vPointSeparator.DecimalSeparator := '.';
  vPointSeparator.ThousandSeparator := ' ';// disable the thousand separator
  vCommaSeparator := DefaultFormatSettings;
  vCommaSeparator.DecimalSeparator := ',';
  vCommaSeparator.ThousandSeparator := ' ';// disable the thousand separator

Finalization

//==============================================================================
End.

