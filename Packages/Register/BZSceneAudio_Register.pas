unit BZSceneAudio_Register;

{$i ..\..\Source\bzscene_options.inc}

interface

uses
  LResources,
  Resource,
  Classes,
  //GLObjectManager,
  ComponentEditors,
  PropEdits,
  LCLType,
  LazIDEIntf,
  ProjectIntf,
  ProjectResourcesIntf,
  Forms, Dialogs,
  Graphics,

  BZSound, BZOpenALManager;

procedure Register;

implementation

{ **-----------------------------------------------------------------------------
  * @Name        :  Register
  * @Description :  Enregistre nos composant dans l'IDE
  -----------------------------------------------------------------------------** }
procedure Register;
begin
   RegisterComponents('BZ-Audio', [TBZSoundLibrary, TBZSoundFXLibrary, TBZSoundOpenALManager]);
end;

initialization
  {$I ..\..\Resources\bzscene_audio.res}

end.

