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
  Forms, Dialogs,
  Graphics,

  BZThreadTimer,
  BZCadencer,
  BZHotKeyManager,
  BZScreenMode;

procedure Register;

implementation


{ **-----------------------------------------------------------------------------
  * @Name        :  Register
  * @Description :  Enregistre nos composants dans l'IDE
  -----------------------------------------------------------------------------** }
procedure Register;
begin
  RegisterComponents('BZ-Tools', [TBZThreadTimer, TBZCadencer, TBZHotKeyManager, TBZFormHotKeyManager, TBZAppHotKeyManager, TBZScreenMode]);
end;

initialization
  {$i ..\..\Resources\bzscene_tools.lrs}

end.

