(*
  @abstract(Prise en charge des Images au format X PixMap.)

  Spécifications : @br
  @unorderedList(
    @item(Méthode de compression    : Aucune)
    @item(Nombre de couleurs	      : Format RGB, 24 bits ou 32 bits RGB(A)) + Format HSL(A)
    @item(Supporte plusieurs images : Non, une seule image dans un même fichier)
    @item(Format des nombres	      : Texte)
    @item(Auteur                    : Colas Nahaboo et Daniel Dardailler.)
    @item(Extensions                : *.xpm ( *.xbm))
  )

  -------------------------------------------------------------------------------------------------------------

  @created(2017-05-11)
  @author(J.Delauney (BeanzMaster))
  Historique : @br
  @unorderedList(
    @item(11/05/2017 : Creation  )
  )

  -------------------------------------------------------------------------------------------------------------

  @bold(Notes) : @br
  Informations sur le format XPM :
  @unorderedList(
    @item(https://en.wikipedia.org/wiki/X_PixMap)
    @item(http://www.fileformat.info/format/xpm/egff.htm)
    @item(https://www.w3.org/People/danield/xpm_story.html)
    @item(http://www.linux-france.org/article/memo/node60.html)
    @item(https://www.x.org/docs/XPM/xpm.pdf)
  )

  Autres informations utiles : @br
  @unorderedList(
    @item(https://en.wikipedia.org/wiki/X_BitMap)
  )

  Fichiers test :@br
  @unorderedList(
    @item(http://www.fileformat.info/format/xpm/sample/index.htm)
    @item(Lazarus)
  )

  -------------------------------------------------------------------------------------------------------------

  @bold(Dépendances) : BZClasses, BZColors, BZGraphic,  BZBitmap, BZImageFileIO, BZImageStrConsts, BZUtils

  -------------------------------------------------------------------------------------------------------------

  @bold(Credits :)
   @unorderedList(
     @item(FPC/Lazarus)
   )

  -------------------------------------------------------------------------------------------------------------

  @bold(Licence) : MPL / GPL

  ------------------------------------------------------------------------------------------------------------- *)
unit BZImageFileXPM;

{
  TODO 0  -oBZBitmap -cSupport_Images_XPM : Ecriture Format XPM
  TODO 0  -oBZBitmap -cSupport_Images_XPM : Lecture des couleurs en HSV(A) (nb : la couleur est précédée de "%" au lieu "#" pour valeur RGB)
  TODO 0  -oBZBitmap -cSupport_Images_XPM : Lecture des extensions. Placées juste après les données de l'image
  TODO 0  -oBZBitmap -cSupport_Images_XPM : Gestion des différent mode de couleurs : "c", "m", "g", "g4" et "s"
}

//==============================================================================
{$mode objfpc}{$H+}
{$i ..\..\bzscene_options.inc}
//==============================================================================

interface

uses
  Classes, SysUtils,
  BZClasses, BZColors, BZGraphic,  BZBitmap, BZImageFileIO;

Const
  cXPM2_MagicID = '! XPM2';
  cXPM3_MagicID = '/* XPM */';
  // Caractères autorisés par defaut. Gimp ajoute d'autres comme "/"
  DefCharsCount = 78;
  DefPalChars = '.,-*abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789@#;:=+%$()[]';
  ExtPalCharsCount = 92;
  ExtPalChars = ' .XoO+@#$%&*=-;:>,<1234567890qwertyuipasdfghjklzxcvbnmMNBVCZASDFGHJKLPIUYTREWQ!~^/()_`''][{}|';


type
  TBZColorHolder = class
  public
    Color: TBZColor;
  end;

Type
  { TBZBitmapXPMImage : Classe de lecture et d"ecriture des images au format XPM }
  TBZBitmapXPMImage = class(TBZCustomImageFileIO)
  private
    procedure SkipComments;
  protected
   W,H,nbCols, cpp, xhot, yhot : integer;
   xpmext : boolean;
   PalLookUp : TBZStringList;

   procedure LoadFromMemory();override;
   function CheckFormat(): Boolean; override;
   function ReadImageProperties:Boolean;override;
  public

   Constructor Create(AOwner: TPersistent; AWidth, AHeight: integer);  override;
   Destructor Destroy;override;

   class function Capabilities: TBZDataFileCapabilities;override;

   property FullFileName;
  end;

implementation

// Uses BZStringUtils, BZLogger, Dialogs;
uses
  BZImageStrConsts, BZUtils;

const
  WhiteSpaces = [#8, #9, #13, #10, #32];

procedure SkipWhiteSpace(var Line: string);
begin
  while (Length(Line) > 0) and (Line[1] in WhiteSpaces) do
    Delete(Line, 1, 1);
end;

function ReadString(var Line: string): string;
begin
  Result := '';
  SkipWhiteSpace(Line);
  while (Length(Line) > 0) and not(Line[1] in WhiteSpaces) do
  begin
    SetLength(Result, Length(Result) + 1);
    Result[Length(Result)] := Line[1];
    Delete(Line, 1, 1);
  end;
end;

function ReadInt(var Line: string): Integer;
begin
  Result := StrToInt(ReadString(Line));
end;


{%region%=====[ TBZBitmapXPMImage ]===========================================}

Constructor TBZBitmapXPMImage.Create(AOwner: TPersistent; AWidth, AHeight: integer);
begin
  inherited create(AOwner,AWidth,AHeight);
  With DataFormatDesc do
  begin
    Name:='XPM';
    Desc:='X Window Pixmap';
    FileMask:='*.xpm';
  end;
 xpmext:=False;
  // Table de correspondance pour les couleur dans la palette (Basée sur le "Haschage"))
 PalLookup := TBZStringList.Create(nil);
 PalLookup.CaseSensitive := True;
end;

destructor TBZBitmapXPMImage.Destroy;
begin
  FreeAndNil(PalLookup);
  inherited destroy;
end;

class function TBZBitmapXPMImage.Capabilities: TBZDataFileCapabilities;
begin
  result:=[dfcRead]; //[dfcRead, dfcWrite]
end;

procedure TBZBitmapXPMImage.SkipComments;
Var NextC, C2, NextC2 : Char;
begin

  Memory.SkipNextByte;
  NextC:=Memory.ReadChar;
  if (Memory.Position<Memory.Size) and (NextC='*') then
  begin
   // C'est un tag ou commentaire
   // On cherche la fin
   while (Memory.Position<Memory.Size)  do
   begin
     C2:=Memory.ReadChar;
     NextC2:=Memory.ReadNextChar;
     if (C2='*') and (NextC2='/') then
     begin
       // Fin du tag ou commentaire
       Memory.GotoNextStringLine; // On passe à la ligne suivante
       break; // On sort de la boucle
     end;
   end;
  End;
End;

function TBZBitmapXPMImage.ReadImageProperties:Boolean;
Var
  xpmexts : String;
  C:Char;
  ok:Boolean;
begin
  Result:=false;
  C:=Memory.ReadNextChar; // On verifie si il y a commentaire
  if C='/' then SkipComments;

  Memory.SkipNextByte; // On saute le 1er '"'

  with Memory Do
  begin
    SkipChar; // On saute les caractères vide si ils existent (valeur par defaut  DefaultCharsDelims = #8#9#10#13#32 );        .
    W:=ReadStrIntToken; // On lit la largeur
    SkipChar;
    H:=ReadStrIntToken; // On Lit la hauteur
    SkipChar;
    nbCols:=ReadStrIntToken; // Le Nombre de couleur dans la palette
    SkipChar;
    cpp:=ReadStrIntToken; // Nombre de caractères décrivant 1 pixel
    SkipChar;
    C:=ReadNextChar;
    // Fin de la ligne atteinte
    if (C='"') or (C=',') then
    begin
      GotoNextStringLine;
      Ok:=True;
    end
    else // Il y a des informations pour les curseurs
    begin
      xHot:=ReadStrIntToken;   // Lecture du point chaud X
      yHot:=ReadStrIntToken;  // Lecture du point chaud Y
      xpmExts:=ReadStrToken;  // Lecture de l'extension

      // L'extension est elle valide ?
      // NB : Des extensions peuvent se trouver après les données de l'image
      Ok := (comparetext(xpmExts, 'XPMEXT') = 0);
      if (xpmExts <> '') and  xpmext and not(ok) then
      begin
        Ok:=false;
        RaiseInvalidImageFile(rsInvalidTagXPMExt);
      end;
    end;
  end;
  Result:=Ok;
  if Not(Ok) then RaiseInvalidImageHeader

end;

function TBZBitmapXPMImage.CheckFormat(): Boolean;
Var
  MagicID : String;
begin
  Result := False;
  DataFormatDesc.Version := '1.0';
  // On vérifie que l'on a bien à faire à un fichier XPM correct
  MagicID:=Memory.ReadLnString; // v2 ou v3 ?

  if (MagicID = cXPM3_MagicID) or (MagicID = cXPM2_MagicID) then
  begin
    //Version 3. On saute la ligne de description du nom de fichier (y'en n'a pas dans la 2)
    DataFormatDesc.Version := '2.0';
    if MagicID = cXPM3_MagicID  then
    begin
     Memory.ReadLnString;
     if (MagicID = cXPM3_MagicID) then DataFormatDesc.Version := '3.0';
    End;
    // On Lit les propriétes de l'image
    Result:= ReadImageProperties;
  end;
end;

procedure TBZBitmapXPMImage.LoadFromMemory();
Var
  SectionSize : Integer;

  function NameToColor(const ColStr: string): TBZColor;
  var
    S: string;
  begin

    S := LowerCase(ColStr);
    if (S = 'transparent') or (S = 'none') then
      Result := clrTransparent
    else if S = 'black' then
      Result := clrBlack
    else if S = 'blue' then
      Result := clrBlue
    else if S = 'green' then
      Result := clrGreen
    else if S = 'cyan' then
      Result := clrAqua
    else if S = 'red' then
      Result := clrRed
    else if S = 'magenta' then
      Result := clrFuchsia
    else if S = 'yellow' then
      Result := clrYellow
    else if S = 'white' then
      Result := clrWhite
    else if S = 'gray' then
      Result := clrLtGray
    else if S = 'dkblue' then
      Result := clrNavy
    else if S = 'dkgreen' then
      Result := clrGreen
    else if S = 'dkcyan' then
      Result := clrTeal
    else if S = 'dkred' then
      Result := clrMaroon
    else if S = 'dkmagenta' then
      Result := clrPurple
    else if S = 'dkyellow' then
      Result := clrOlive
    else if S = 'maroon' then
      Result := clrMaroon
    else if S = 'olive' then
      Result := clrOlive
    else if S = 'navy' then
      Result := clrNavy
    else if S = 'purple' then
      Result := clrPurple
    else if S = 'teal' then
      Result := clrTeal
    else if S = 'silver' then
      Result := clrSilver
    else if S = 'lime' then
      Result := clrLime
    else if S = 'fuchsia' then
      Result := clrFuchsia
    else if S = 'aqua' then
      Result := clrAqua
    else
      Result := clrTransparent;
  end;

  procedure ReadPalette;
  var
     I: Integer;
     S, ColType,ColStr, Code: string;
     Color: TBZColor;
     C, C3:Char;
     Delta : Single;
   begin
     Delta:= 100/nbCols;
     StartProgressSection((nbCols*100) / SectionSize ,'Chargement de la palette');
     C:=Memory.ReadNextChar; // On verifie si il y a commentaire
     if C='/' then SkipComments;

     // On Charge la palette
     S:='';
     I:=0;
     // On extrait ligne par ligne
     repeat
       C:=Memory.ReadChar;
       case C of
        '/': // Tags ou commentaires, on les ignores, on passe juste par dessus
         begin
             Memory.GotoPreviousByte;
             SkipComments;
           end;
         '"': // Lecture d'une ligne
         begin
           // Constante de type chaine de caractères
           C3:=Memory.ReadNextChar;
           while (Memory.Position<Memory.Size) and (C3<>'"') do
           begin
             C3:=Memory.ReadChar;
             if (C3='"')  then
             begin
               // Fin de la chaine de caractères
               break; // On stoppe la boucle
             end
             else S:=S+C3
           end;
         end;
         ',': // fin de la ligne (intervient normalement toujours apres le 2eme "
         begin
           { Evaluation de la ligne pour extraire les paramètres

             NOTE :
               On ne lit que le 1er paramètre. Mais il faudrait également traiter les autres
               si ils sont présent. Afin de gérér à 100% le format XPM.
           }
           Code := Copy(S, 1, cpp);
           Delete(S, 1, cpp);
           ColType := LowerCase(ReadString(S));
           ColStr := ReadString(S);

           // Conversion des couleurs par leur nom ou leur valeur hexa vers TBZColor
           Case Coltype of
             'c', 'm', 'g' :  // "C"ouleur,  "M"onochrome, "G"ris
             begin
               if ColStr[1] = '#' then    // Format RGB(A)
               begin
                 Delete(ColStr, 1, 1);
                 Color.Create(ColStr);
                 S:='';
                 inc(i);
               end
               else
               if ColStr[1]='%' then  // Format HSV(A) (aussi en Hexa)
               begin
                 { TODO 5 -oBZBitmap -cSupport_Images_XPM : Prise en charge format couleur HSV }
               end
               else
               begin
                 Color:=NameToColor(ColStr);
                 S:='';
                 Inc(I);
               end;
             end;
             's' :
             begin
               Color:=NameToColor(ColStr);
               S:='';
               Inc(I);
             end;
             else
               begin
                 RaiseInvalidImageFile('Fichier au format XPM invalide');
               end;
           end;
           // On ajoute la couleur à la palette
           PalLookup.AddInt(Code,Color.AsInteger);
           // On avance
           AdvanceProgress(Delta,0,0,False);
           // On sa place à la ligne suivante
           Memory.GotoNextStringLine;
         end;
       end;
     until (I=nbCols);
     FinishProgressSection(False);
   end;

  procedure ReadPixelData;
  var
    C, X, Y, Idx: Integer;
    Code: string;
    DstLine : PBZColor;
    AColor : TBZColor;
    Cr:Char;
    Delta : Single;
  begin
    // On Charge le bitmap
    idx:=0;
    X:=0;
    Y:=0;

    Delta := 100 / (Height);
    StartProgressSection((Height*100) / SectionSize ,'Chargement de l''image');

    Cr:=Memory.ReadNextChar; // On verifie si il y a commentaire sinon, On saute le 1er '"'
    if Cr='/' then
    begin
      SkipComments;
    End;
    // On se place sur la 1ere de notre bitmap
    DstLine:=GetScanLine(0);

    C:=0;
    Y:=0;
    repeat
      Memory.SkipNextByte;  // On saute le '"' de debut
      Code:='';
      Code:=Memory.ReadString(cpp);  // On lit le code
      idx:=PalLookUp.IndexOf(Code); // On va chercher couleur correspondante au code dans la palette
      AColor.AsInteger := PalLookUp.Items[Idx].Tag;
      DstLine^ := AColor; // On ecrit le pixel //DstLine^.AsInteger := PalLookUp.Items[Idx].Tag;
      Inc(X);
      if X>MaxWidth then
      begin
        X:=0;
        Inc(Y);
        AdvanceProgress(Delta,0,1,False);
        Memory.GotoNextStringLine; // On saute le '"' de fin , le ',' et les caractères de fin de ligne (#13#10)
      end;
      Inc(DstLine);
      Inc(C);
     until (C>MaxSize);
     FinishProgressSection(False);
  end;

begin
  // On à passé tous les tests de vérification, on peux lire les données
  // On Initialise le system de notification de la progression
  SectionSize := nbCols + H;
  InitProgress(W,H);
  StartProgressSection(0, ''); // On debute une nouvelle section globale
  SetSize(W,H); // On itialise notre bitmap aux bonnes dimensions
  // Lecture de la palette de couleurs
  ReadPalette;
  // Remplissage de notre bitmap avec les données
  ReadPixelData;
  FinishProgressSection(True);

  //ConvertRawDataToBitmapData;
end;

//--- Ecriture

(* procedure SaveToStream(aStream)
*)

{%endregion%}

initialization

  RegisterRasterFormat('xpm', 'X Window Pixmap', TBZBitmapXPMImage);
end.

