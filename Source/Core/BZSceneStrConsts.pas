(*
  @abstract(Contient les chaines de caractères (Resources String) pour l'affichage des messages.)

  Ces messages seront transformés par Lazarus/FPC en fichier PO qui pourront être
  édité pour traduire les messages dans différentes langues.

  -------------------------------------------------------------------------------------------------------------

  @created(24/02/2019)
  @author(J.Delauney (BeanzMaster))
  Historique : @br
  @unorderedList(
    @item(Creation : 24/02/2019)
  )

  -------------------------------------------------------------------------------------------------------------

  @bold(Notes) :

  -------------------------------------------------------------------------------------------------------------

  @bold(Dependances) : aucune

  -------------------------------------------------------------------------------------------------------------

   @bold(Credits :)@br
     @unorderedList(
       @item(J.Delauney (BeanzMaster))
     )

  -------------------------------------------------------------------------------------------------------------

  @bold(LICENCE) : MPL / GPL

  ------------------------------------------------------------------------------------------------------------- *)
unit BZSceneStrConsts;

{$mode objfpc}{$H+}

Interface

ResourceString
  rsIndexOutOfRange = 'Index hors limite';
  rsListOrArrayIsEmpty = 'La liste ou le tableau est vide';
  rsOutOfMemory ='Pas assez de mémoire disponible';
  rsTooLarge = 'Trop grand';

  rsAnimPowerInfosMsg = 'TBZAnimationController' + LineEnding +
                        'l''animation de type amPower à besoins d''paramètre supplémentaires dans la variables ExtraParams.' + LineEnding +
                        'Ce paramètre doit désigner la valeur de l''exposant. En son absence celui-ci est par défaut 2';

  cUnknownArchiveVersion = 'Version d''archive inconnue : ';

Implementation

End.

