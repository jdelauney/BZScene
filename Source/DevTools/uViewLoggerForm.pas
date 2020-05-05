unit uViewLoggerForm;

//==============================================================================
{$mode objfpc}{$H+}
{$i ..\bzscene_options.inc}
//==============================================================================

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TViewLoggerForm }

  TViewLoggerForm = class(TForm)
    MemoLog : TMemo;
    procedure FormShow(Sender : TObject);
  private

  public

  end;

//var
//  ViewLoggerForm : TViewLoggerForm;

implementation

{$R *.lfm}

{ TViewLoggerForm }

procedure TViewLoggerForm.FormShow(Sender : TObject);
begin
  // Hack pour se placer automatiquement à la dernière ligne d'un TMemo
  MemoLog.SelStart:=Length(MemoLog.lines.Text)-1;
  MemoLog.VertScrollBar.Position:=10000000;
end;

end.

