(*
  @abstract(Contient une classe "TBZRandomGenerator". Cette classe permet de générer
  des nombres aléatoires rapides, personnalisés et sécurisé pour une utilisation avec des "Threads")
  
  La génération des nombres aléatoire est basé sur l'algorithme XorShift.
  (Ce code source est basé sur une fonctionnalité de Castle Engine': https://castle-engine.sourceforge.io)
  Dans certains cas, elle fonctionne 2 à 3 fois plus vite que la fonction aléatoire native de FPC.

  -------------------------------------------------------------------------------------------------------------

  @created(24/02/2019)
  @author(J.Delauney (BeanzMaster))
  Historique : @br
  @unorderedList(
    @item(Creation : 24/02/2019)
    @item(Mise à jour : )
  )

  -------------------------------------------------------------------------------------------------------------

  @bold(Notes :)@br
  @unorderedList(
    @item(https://en.wikipedia.org/wiki/Linear_congruential_generator)
    @item(https://en.wikipedia.org/wiki/Xorshift)
    @item(https://en.wikipedia.org/wiki/Multiply-with-carry)
  )

 -------------------------------------------------------------------------------------------------------------

 @bold(Dependances) : Aucune

 -------------------------------------------------------------------------------------------------------------

 @bold(Credits :)@br
   @unorderedList(
     @item(J.Delauney (BeanzMaster))
   )

 -------------------------------------------------------------------------------------------------------------

 @bold(LICENCE) : MPL / GPL

 ------------------------------------------------------------------------------------------------------------- *)
Unit BZRandomGenerator;

//==============================================================================
{$mode objfpc}{$H+}
{$i ..\bzscene_options.inc}
//==============================================================================

Interface

Uses
  Classes, Sysutils;

//==============================================================================
Type
   { Methode aléatoire : @br
     @unorderedlist(
       @item(rngtXorShif64 : Très rapide)
       @item(rngtMWC256 : Moins rapide mais plus précis))
   }
   TBZRandomNumGeneratorType = (rngtXorShift64, rngtMWC256); //rngtXorShift128, rngtXorShift1024,
   { Générateur de nombres aléatoires }
   TBZRandomNumGenerator = class
    strict private
//      cSeed, iSeed : DWord;
//      QSeed : Array[0..$FF] of DWord;
      FRandSeed:Longint;
      FConstantSeed:Longint;
      FUseConstantSeed : Boolean;
      FRNGType : TBZRandomNumGeneratorType;

      function GetSeed: Longint; inline;
      procedure SetSeed(AValue: Longint);inline;
      procedure Initialize(const ASeed:Cardinal);inline;

      procedure XorShift;inline;
      //class function GetRandomSeed: LongInt;

    public
      { Creation du composant }
      constructor create;
      { Initialisation du générateur aleatoire }
      procedure Randomize;inline;
      { Retourne un nombre aleatoire en nombre flottant }
      function Random: extended; overload;inline;
      { Retourne un nombre aleatoire entier non signé }
      function RandomInt: LongWord; inline;
      { Retourne un nombre aleatoire entier non signé sur 32bits compris entre 0 et le paramètre "Range" }
      function Random(range:longint):longint;overload;inline;
      { Retourne un nombre aleatoire entier non signé sur 64bits compris entre 0 et le paramètre "Range" }
      function Random(range:int64):int64;overload;inline;
      { Réinitialise la graine de génération aleatoire }
      procedure ResetSeed;
      { Retourne la graine générée utilisée pour la génération des nombres aléatoire }
      property RandSeed:Longint Read GetSeed write SetSeed;
      //property UseConstantSeed : Boolean read FUseConstantSeed write FUseConstantSeed;
    end;

//==============================================================================
Var
  { Variable globale auto-initialisée à utiliser directement dans les applications }
  GlobalRandomGen : TBZRandomNumGenerator;

//==============================================================================

Implementation

//==============================================================================

Const
  //CONVERT_MWC256_TO_FLOAT =  2.32830643653869E-10; //1/MaxInt; // Extended (1.0/int64(1 shl 32)); //  0..1
  CONVERT_TO_FLOAT = 1/MaxInt;

//  CONVERT_SIGNED   = Extended (2.0/int64(1 shl 32)); // -1..1
//Threadvar
//  GLRNG_RandSeed:Longint;
 // GLRNG_OldRandSeed:Cardinal;
var
   store_64bit_seed: QWord = 0; //this variable stores 64 bit seed for reusing
   wait_for_seed: boolean = false;

function TBZRandomNumGenerator.GetSeed: Longint;
begin
  Result := FConstantSeed; //GLRNG_RandSeed;
end;

procedure TBZRandomNumGenerator.SetSeed(AValue: Longint);
begin
  FConstantSeed := AValue;
  FRandseed := AValue;
  //Initialize(GLRNG_Randseed);
end;

procedure TBZRandomNumGenerator.Initialize(const ASeed: Cardinal);
Var I:Integer;
begin
  if ASeed = 0 then
  begin
    //For I:=0 to $FF do
    //begin
      Randomize;
      //QSeed[i]:=GLRNG_randseed; //RandSeed;
    //End;
   // GLRNG_RandSeed := GetRandomSeed;
   // Randomize;
  end
  else
  begin
    FRandSeed := LongInt(ASeed);
    FConstantSeed := FRandSeed;
  end;
end;

procedure TBZRandomNumGenerator.ResetSeed;
begin
  FRandSeed := FConstantSeed;
End;

constructor TBZRandomNumGenerator.create;
begin
  initialize(0);
//  iSeed := 0;
//  cSeed:=0;
  //GLRNG_OldRandSeed := 0;
end;

(*RandSeed := ((RandSeed shl 8) or GetCurrentProcessID) xor GetTickCount; *)
procedure TBZRandomNumGenerator.Randomize;
const
  date_multiplier: QWord = 30000000;   //  approximation de la date
  date_order: QWord = 80000 * 30000000; // ordre : "now*date_multiplier" variable
  {p.s. date_order sera juste jusqu'à l'année ~ 2119}

var c64: QWord; // graine actuelle;
    b64: QWord; // graine supplémentaire pour la sécurité multi-threading
    a64: QWord; // une autre graine supplémentaire

    hours, mins, secs, msecs : Word;
  procedure xorshift64;
  begin
    c64:=c64 xor (c64 shl 12);
    c64:=c64 xor (c64 shr 25);
    c64:=c64 xor (c64 shl 27);
  end;
begin
  {Nous ajoutons une variable semi-aléatoire supplémentaire basée sur la variable locale c64 :
   son adresse 64 bits. Le seul bénéfice que nous avons ici est que cette adresse sera
   différente pour les chaque différents threads, donc  2 threads ne pourront être initialisés
   avec des graines égales, même si elles sont absolument simultanées }

  c64 := {%H-}QWORD(@(c64));
  DecodeTime(Now,Hours,mins,Secs,msecs);
  {$R-}
  Secs := Secs * 100;
  mins := mins * 60;
  Hours := Hours * 3600;
  {$R+}
  a64:=Hours+ Mins + Secs + msecs;

  while wait_for_seed do
  begin
    //DecodeTime(Now,Hour,mins,Secs,msecs);
    //Secs := Secs * 100;
    //mins := mins * 60;
   // Hour := Hour * 3600;
    a64:=a64+QWord(now);
    xorshift64; //En attendant, on fait quelque chose
  End;

  wait_for_seed := true;     // Empêche une autre randomisation de commencer jusqu'à ce que celui-ci soit fini

  c64 := (c64 + a64) shr 1;
  c64 :=  ((c64 shl 8) or a64) xor GetTickCount64;

  b64 := c64;   // notre autre graine aléatoire basée sur l'allocation de la mémoire de thread en cours

  {fondamentalement, nous ne nous soucions pas si les threads passeront accidentellement
  'wait_for_seed' le verrouille.
   Grâce à b64 nous aurons des valeurs aléatoires différentes, mais ce n'est peut être pas optimal }

  if store_64bit_seed = 0 then
  begin //1ere randomization


   (* DecodeTime(Now,Hour,mins,Secs,msecs);
    Secs := Secs * 100;
    mins := mins * 60;
    Hour := Hour * 3600;
    a64:=Hour + Mins + Secs + msecs;
    c64 := (c64 + a64) shr 1; *)

    { Maintenant, nous devons nous assurer que l'ajout de 'a64' ne débordera pas
      Nous ajoutons quelques xorshift64 juste pour le plaisir au cas où}
    while (c64 > high(QWord)-date_order) do xorshift64;

    { Pour tuer la valeur discrette aléatoire introduit par gettickcount64 nous ajoutons 'Now'.
      'now' et 'gettickcount64' ne sont pas indépendants mais changent synchroniquement.
      Après plusieurs xorshift64, c64 n'a plus aucune informations laissé par gettickcount64 et
      nous introduisons un changement semi-indépendant dans la graine aléatoire}
    c64 := c64+ QWord(round(now*date_multiplier));

    { Un autre cycle xorshift de 64 bits pour tuer tout ce qui reste de 'Now' }
    xorshift64;
    { Maintenant nous sommes sûrs d'obtenir une graine aléatoire différente même
      dans le cas où nous lancons la procédure  exactement à la même milliseconde depuis
      le démarrage de l'OS.
      Une date et heure différentes donneront une autre graine aléatoire ...
      A moins de fixer délibérément la date et l'heure }


  end
  else
    c64 := store_64bit_seed; //On reprend juste la graine déja générer

  // c64 := c64 shr 1;  // note: nous jetons 1 bit de précision pour gagner de la vitesse
  { Maintenant, nous faisons juste un autre xorshift64, car nous avons une variable c64 aléatoire correcte }
  xorshift64;
  {On fusionne une autre variable aléatoire basée sur le thread en cours }
  c64 := c64 xor b64;

  {et pour finir, afin d"éviter d'avoir une graine à ZERO}
  repeat
    {Quelques xorshift64 de plus}
    xorshift64;
    {On garde les 32-bits haut de c64 pour avoir une veribale graine 64bits}
    FConstantSeed := longint(c64 shr 32);
  until FConstantSeed<>0;
  // FConstantSeed := FConstantSeed shr 1;
  FRandSeed := FConstantSeed;
  { On sauvegarde notre graine pour une réutilisation ultérieur au cas ou }
  store_64bit_seed := c64;
  {On passe la main au prochain thread}
  wait_for_seed := false;
end;

procedure  TBZRandomNumGenerator.XorShift;  inline;
begin
  // FRandSeed := FRandSeed shr 1;
  { Fonctionne un peu plus vite (+ 4%) en raison d'une meilleure optimisation
    par le compilateur (utilise des registres de CPU au lieu d'une variable) }
  FRandSeed := ((FRandSeed xor (FRandSeed shl 1)) xor ((FRandSeed xor (FRandSeed shl 1)) shr 15)) xor
         (((FRandSeed xor (FRandSeed shl 1)) xor ((FRandSeed xor (FRandSeed shl 1)) shr 15)) shl 4);

  (* FRandSeed:= FRandSeed xor (FRandSeed shl 1);
    FRandSeed:= FRandSeed xor (FRandSeed shr 15);
    FRandSeed :=FRandSeed xor (FRandSeed shl 4); *)

end;

function TBZRandomNumGenerator.Random: extended;  Inline;
//var tSeed : qword;
begin
  // MWC256 from Usenet posting by G. Marsaglia - Period 2^8222
 (* iSeed := (iSeed+1) AND $FF;
  tSeed := qword (809430660) * QSeed[iSeed] + cSeed;
  cSeed        :=  hi (tSeed);
  QSeed[iSeed] := lo (tSeed);
  result := CONVERT_TO_FLOAT*(QSeed[iSeed] shr 1); *)

  XorShift;
  result := CONVERT_TO_FLOAT*Longint(FRandSeed shr 1);  // note: nous jetons 1 bit de précision pour gagner de la vitesse
end;

function TBZRandomNumGenerator.Random(range: longint): longint;
begin
  XorShift;
  if range>1 then
    result := LongInt((int64(LongWord(FRandSeed))*range) shr 32) // Plus rapide que FRandSeed Mod Range
  else
    result := 0
end;

function TBZRandomNumGenerator.RandomInt: LongWord;
begin
  XorShift;
  result := LongWord(FRandSeed);
end;

function TBZRandomNumGenerator.Random(range: int64): int64;
var c64: QWord;
  procedure xorshift64; inline;
  begin
    c64:=c64 xor (c64 shl 12);
    c64:=c64 xor (c64 shr 25);
    c64:=c64 xor (c64 shl 27);
  end;
begin
  {Même si N = 0..1 pour faire un cycle de semences aléatoires de 32 bits nous devons le faire deux fois}
  c64 := qword(RandomInt) or (qword(RandomInt) shl 32);
  if range > 1 then
  begin
    {l'ajout d'un cycle xorshift64 nous garantit que c64 est vraiment aléatoire
     dans la plage 1..high (QWORD) mais ralentit l'exécution de ~ 10%}
    xorshift64;
    {Contrairement à SysUtils nous en faisons un vrai nombre aléatoire de 64-bit et non pas un faux de 63 bits :)
     Il ne peut pas y avoir de débordement ici, parce que N est int64 et il ne peut pas être
     plus grand que (Hi(QWORD) div 2)
     C'est-à-dire que nous ne pourrons jamais obtenir un résultat 'négatif' car le premier bit du résultat sera toujours zéro }
    result := int64(qword(c64) mod qword(Range))
  end
  else
    result := 0;
end;

//==============================================================================

initialization

  GlobalRandomGen:= TBZRandomNumGenerator.create;

finalization

  FreeAndNil(GlobalRandomGen);

//==============================================================================
End.

