{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit bzscene_controls_dt;

{$warn 5023 off : no warning about unused units}
interface

uses
  BZSceneControls_Register, uOpenPictureForm, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('BZSceneControls_Register', @BZSceneControls_Register.Register);
end;

initialization
  RegisterPackage('bzscene_controls_dt', @Register);
end.
