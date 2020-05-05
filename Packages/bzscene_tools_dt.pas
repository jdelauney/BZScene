{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit bzscene_tools_dt;

{$warn 5023 off : no warning about unused units}
interface

uses
  BZSceneTools_Register, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('BZSceneTools_Register', @BZSceneTools_Register.Register);
end;

initialization
  RegisterPackage('bzscene_tools_dt', @Register);
end.
