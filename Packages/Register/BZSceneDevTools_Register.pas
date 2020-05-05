unit BZSceneDevTools_Register;

{$i .\..\..\source\bzscene_options.inc}

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
  
  BZStopWatch,  
  BZProfiler,
  BZLogger;

procedure Register;

implementation




{ **-----------------------------------------------------------------------------
  * @Name        :  Register
  * @Description :  Enregistre nos composant dans l'IDE
  -----------------------------------------------------------------------------** }
procedure Register;
begin
  RegisterComponents('BZ-DevTools', [TBZStopWatch, TBZProfiler, TBZLogger]);
end;

initialization
  {$I ..\..\resources\bzscene_devtools.res}

end. 