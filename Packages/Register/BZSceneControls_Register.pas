unit BZSceneControls_Register;

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

  BZImageViewer,
  BZPictureDialogs;

procedure Register;

implementation

uses
  BZColors,
  BZGraphic,
  BZBitmap,
  uOpenPictureForm;

type
  TBZPictureProperty = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

function TBZPictureProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

// Edit;

procedure TBZPictureProperty.Edit;
VAr
  Pic : TBZPicture;
begin
  Pic := TBZPicture(GetObjectValue);
  if OpenPictureForm.Execute(Pic) then
  begin
    Modified;
  end;
end;

{ **-----------------------------------------------------------------------------
  * @Name        :  Register
  * @Description :  Enregistre nos composants et editeurs de propriétés dans l'IDE
  -----------------------------------------------------------------------------** }
procedure Register;
begin
  RegisterComponents('BZ-Controls', [TBZImageViewer, TBZOpenPictureDialog]);

  RegisterClasses([TBZPicture]);

  RegisterPropertyEditor(TypeInfo(TBZPicture), nil, '', TBZPictureProperty);
end;

initialization
  {$I ..\..\Resources\bzscene_controls.res}

end.

