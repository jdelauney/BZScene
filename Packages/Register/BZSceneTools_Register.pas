unit BZSceneTools_Register;

{$i ..\..\Source\bzscene_options.inc}

interface

uses
  LResources,
  Resource,
  Classes,
  ComponentEditors,
  PropEdits,
  LCLType,
  LazIDEIntf,
  ProjectIntf,
  ProjectResourcesIntf,
  MacroIntf,
  Forms, Dialogs,
  Graphics,

  {$IFDEF WINDOWS}
  BZThreadTimer,
  BZHotKeyManager,
  {$ENDIF}
  BZCadencer,
  BZScreenMode;

procedure Register;

implementation


{ **-----------------------------------------------------------------------------
  * @Name        :  Register
  * @Description :  Enregistre nos composants dans l'IDE
  -----------------------------------------------------------------------------** }
procedure Register;
begin

  RegisterComponents('BZ-Tools', [ TBZCadencer, TBZScreenMode]);
  {$IFDEF WINDOWS}
  RegisterComponents('BZ-Tools',[TBZThreadTimer, TBZHotKeyManager, TBZFormHotKeyManager, TBZAppHotKeyManager]);
  {$ENDIF}

end;

initialization

    {$I ../../Resources/bzscene_tools.res}



end.

