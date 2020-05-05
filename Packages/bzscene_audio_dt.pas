{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit bzscene_audio_dt;

{$warn 5023 off : no warning about unused units}
interface

uses
  BZSceneAudio_Register, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('BZSceneAudio_Register', @BZSceneAudio_Register.Register);
end;

initialization
  RegisterPackage('bzscene_audio_dt', @Register);
end.
