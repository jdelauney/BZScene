(*
@created(2019-12-27)
@author(J.Delauney (BeanzMaster))
Historique : @br
@unorderedList(
  @item(27/12/2019 : Creation  )
)
--------------------------------------------------------------------------------@br
  ------------------------------------------------------------------------------
  Description :


  ------------------------------------------------------------------------------
  @bold(Notes :)

  Quelques liens :
   @unorderedList(
       @item()

     )
  ------------------------------------------------------------------------------@br

  @bold(Credits :)
    @unorderedList(
      @item(FPC/Lazarus)
    )

  ------------------------------------------------------------------------------@br
  LICENCE : GPL/MPL
  @br
  ------------------------------------------------------------------------------
 *==============================================================================*)
unit BZSoftwareShader_Template;

//==============================================================================
{$mode objfpc}{$H+}
{$i ..\..\..\bzscene_options.inc}

//------------------------
{$ALIGN 16}
{$CODEALIGN CONSTMIN=16}
{$CODEALIGN LOCALMIN=16}
{$CODEALIGN VARMIN=16}
//------------------------
//==============================================================================
  

interface

uses
  Classes, SysUtils,
  BZClasses, BZMath, BZVectorMath, BZRayMarchMath,
  BZGraphic, BZBitmap,
  BZCadencer, BZCustomShader;

Type

  { TBZSoftShader_GroundAndDistortPhongSphere }

  TBZSoftShader_Template = Class(TBZCustomSoftwareShader)
  protected
  public
    Constructor Create; override;
    Destructor Destroy; override;

    function Clone : TBZCustomSoftwareShader; override;
    
    function ShadePixelFloat:TBZColorVector; override;
  end;

implementation

{ TBZSoftShader_GroundAndDistortPhongSphere }

Constructor TBZSoftShader_Template.Create;
begin
  inherited Create;
end;

Destructor TBZSoftShader_Template.Destroy;
begin
  inherited Destroy;
end;

function TBZSoftShader_Template.Clone : TBZCustomSoftwareShader;
begin
  Result := TBZSoftShader_Template.Create;
  Result.Assign(Self);
end; 

function TBZSoftShader_Template.ShadePixelFloat : TBZColorVector;
  function ComputePixel(Coord:TBZVector2f; aTime:Single) : TBZColorVector;
  var
    {$CODEALIGN VARMIN=16}
    finalColor : TBZColorVector;
    {$CODEALIGN VARMIN=4}   
  begin
    Result := FinaleColor;
  end;
begin
  Result := ComputePixel(FragCoords, iTime);
end;

end.

