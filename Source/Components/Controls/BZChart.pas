(*====< BZChart.pas >==========================================================@br
  @created(2017-09-10)
  @author(J.Delauney (BeanzMaster))
  Historique : @br
  @unorderedList(
    @item(10/09/2017 : Creation  )
  )
--------------------------------------------------------------------------------@br

  @bold(Description :)@br
  Composant visuel servant à afficher un série de données sous forme de graphique :
  Points, lignes, barres Verticale, barres horizontale

  ------------------------------------------------------------------------------@br

  @bold(Note :)@br

  ------------------------------------------------------------------------------@br

  @bold(Credits :)
     @unorderedList(
       @item(FPC/Lazarus)
     )

  ------------------------------------------------------------------------------@br
  LICENCE : MPL / GPL @br
  @br
 *==============================================================================*)
unit BZChart;

{$mode objfpc}{$H+}
{$i ..\..\beanzlib_options.inc}

interface

uses
  Classes, SysUtils, Graphics, Dialogs,
  BZGraphic, BZBitmap, BZMath;


Type
  TBZChart = class;
  TBZChartSerieItem = class;
  TBZChartSerieType = (cstBars, cstPoints, cstLines);
  TBZChartGridType = (cgtNone, cgtVertical, cgtHorizontal, cgtBoth);
  TBZChartSerieSortType = (sstNone, sstByValue, sstByCaption);

  TBZChartValueItem = class
  private
    FSerie : TBZChartSerieItem;
    FValue : Double;
    FCaption : String;
    FColor : TBZColor;
  public
    constructor Create(AOwner:TBZChartSerieItem);

    property Value:Double read FValue write FValue;
    property Caption : String read FCaption Write FCaption;
    property Color : TBZColor read FColor write FColor;
  end;


  TBZChartSerieItem = Class
  private
    FCaption:String;
    FChartSerieType : TBZChartSerieType;
    FShowCaption : Boolean;
    FSortAsc : Boolean;
    FValueList : TList;
    FOwner : TBZChart;
  protected


    function GetItemsCount: integer;
    function GetValueItem(Const Index:Integer):TBZChartValueItem;
  public
    constructor Create(AOwner:TBZChart);
    destructor Destroy;override;

    function Add(Const ACaption:String; Const AValue:Double; Const AColor:TBZColor):Integer;
    procedure Clear;

    function getMax():double;
    function getMin():double;


    procedure Sort(Const ASortBy : TBZChartSerieSortType=sstByValue);
    property SortAsc : Boolean read FSortAsc write FSortAsc;

    property SerieType : TBZChartSerieType read FChartSerieType write FChartSerieType;
    property Caption : String read FCaption Write FCaption;
    property ShowCaption :Boolean read FShowCaption Write FShowCaption;
    property Items[I:Integer]:TBZChartValueItem read GetValueItem;
    property Count:Integer read GetItemsCount;

  end;

  TBZChart = class
  private
    FSerieList : TList;

    FBZBitmap : TBZBitmap;
    FAxisXLabel : String;
    FAxisYLabel : String;
    FTitle : String;
    FGridType : TBZChartGridType;
    FShowLegend : Boolean;
    FShowTitle : Boolean;
    FOuterMargin,
    FInnerMargin, FAxisFreq : Integer;

    procedure DrawBackground;
    //procedure DrawSeries;
  protected
    //procedure DrawBackground;
    //procedure DrawAxis;
    //procedure DrawSerie(Const Index:Integer);
    function GetSeriesCount: integer;
    function GetSerieItem(Const Index:Integer):TBZChartSerieItem;
  public

    constructor Create(Const ABitmap : TBZBitmap);
    destructor Destroy; override;

    function Add(Const ACaption:String;Const AType:TBZChartSerieType):Integer;
    procedure Clear;

    procedure Draw;

    property Series[I:Integer]:TBZChartSerieItem read GetSerieItem;
    property Count:Integer read GetSeriesCount;
    property OuterMargin : Integer read FOuterMargin write FOuterMargin;
    property InnerMargin : Integer read FInnerMargin write FInnerMargin;
    property AxisFrequency : Integer read FAxisFreq write FAxisFreq;
    property ShowTitle : Boolean read FShowTitle write FShowTitle;
    property Title : String read FTitle write FTitle;
    property ShowLegend : Boolean read FShowLegend write FShowLegend;
    property AxisXLabel : String read FAxisXLabel write FAxisXLabel;
    property AxisYLabel : String read FAxisXLabel write FAxisYLabel;

  end;

implementation

Uses BZUtils;


function muldiv(a,b,c:integer):integer;
begin
  result:=round((a*b)/c);
end;

function percent(Const Max:Integer;Const apercent:Integer):Integer;
Begin
  result := MulDiv(Max, apercent, 100)
End;

{%region%===[ TBZChartValueItem ]=============================================}

constructor TBZChartValueItem.Create(AOwner : TBZChartSerieItem);
Var c:integer;
begin
  Inherited Create;
  FSerie:=AOwner;
  FValue:=0;
  c:=FSerie.Count+1;
  FCaption:='Value #'+inttostr(c);
  FColor :=clrBlue;
end;

{%endregion%}

{%region%===[ TBZChartSerieItem ]=============================================}

//------------------------------------------------------------------------------
// Creation de TBZChartSerieItem
//------------------------------------------------------------------------------
Constructor TBZChartSerieItem.Create(AOwner:TBZChart);
begin
  Inherited Create;
  FOwner:=AOwner;
  FCaption:='';
  FChartSerieType := cstBars;
  FShowCaption :=true;
  FSortAsc:=false;
  FValueList:=TList.Create;
end;

destructor TBZChartSerieItem.Destroy;
begin
  FValueList.Free;
  FValueList:=nil;
  Inherited Destroy;
end;

function TBZChartSerieItem.GetItemsCount: integer;
begin
  result:= FValueList.Count;
end;

function TBZChartSerieItem.Add(Const ACaption:String; Const AValue:Double; Const AColor:TBZColor):Integer;
var
  AItem : TBZChartValueItem;
  C : Integer;
begin
   AItem:=TBZChartValueItem.Create(self);
   C := FValueList.Add(AItem);

   With  TBZChartValueItem(FValueList[C]) do
   begin
     Caption:=ACaption;
     Value:=AValue;
     Color:=AColor;
   end;

   result:=C;

end;

procedure TBZChartSerieItem.Clear;
begin
 FValueList.Clear;
end;

function TBZChartSerieItem.GetValueItem(Const Index:Integer):TBZChartValueItem;
begin
   result:=TBZChartValueItem(FValueList.Items[Index]);
end;

function TBZChartSerieItem.getMin():Double;
Var
  i:Integer;
  tmp,AValue:double;
begin
   result:=0;
   if FValueList.Count<1 then exit;
   tmp:=10000000000000;
   for i:=0 to FValueList.Count-1 do
   begin
     AValue:=TBZChartValueItem(FValueList.Items[I]).Value;
     if AValue<tmp then
       tmp:=AValue;
   end;
   result:=tmp;
end;

function TBZChartSerieItem.getMax():Double;
Var
  i:Integer;
  tmp,AValue:double;
begin
   result:=100;
   if FValueList.Count<1 then exit;
   tmp:=0;
   for i:=0 to FValueList.Count-1 do
   begin
     AValue:=TBZChartValueItem(FValueList.Items[I]).Value;
     if AValue>tmp then
       tmp:=AValue;
   end;
   result:=tmp;
end;

function CompareValueMax(Item1, Item2: Pointer): Integer;
begin
  result:= round(TBZChartValueItem(Item1).value - TBZChartValueItem(Item2).value);
end;
function CompareValueMin(Item1, Item2: Pointer): Integer;
begin
  result:=round(TBZChartValueItem(Item2).value-TBZChartValueItem(Item1).value);
end;

procedure TBZChartSerieItem.Sort(Const ASortBy : TBZChartSerieSortType=sstByValue);
begin
   if ASortBy = sstNone then exit;
   Case ASortBy of
     sstByValue :
     begin
       if FSortAsc then
         FValueList.Sort(@CompareValueMax)
       else
         FValueList.Sort(@CompareValueMin);
     end;
   end;
end;

{%endregion%}

{%region%===[ TBZChart ]======================================================}

//------------------------------------------------------------------------------
// Creation de TBZChart
//------------------------------------------------------------------------------
Constructor TBZChart.Create(Const ABitmap:TBZBitmap);
begin
  Inherited Create;
  FBZBitmap:=ABitmap;
  FTitle :='Beanz Chart';
  FGridType :=cgtBoth;;
  FShowLegend :=false;
  FShowTitle :=true;
  FSerieList:=TList.Create;
  FOuterMargin := 10;
  FInnerMargin:=5;
  FAxisFreq:=10;
end;

destructor TBZChart.Destroy;
begin
  FSerieList.Free;
  FSerieList:=nil;
  Inherited Destroy;
end;

function TBZChart.GetSeriesCount: integer;
begin
  result:= FSerieList.Count;
end;

function TBZChart.GetSerieItem(Const Index:Integer):TBZChartSerieItem;
begin
   result:=TBZChartSerieItem(FSerieList.Items[Index]);
end;

function TBZChart.Add(Const ACaption:String; Const AType:TBZChartSerieType):Integer;
var
  AValue : TBZChartSerieItem;
  C : Integer;
begin
   AValue:=TBZChartSerieItem.Create(self);
   C := FSerieList.Add(AValue);

   With  TBZChartSerieItem(FSerieList[C]) do
   begin
     Caption:=ACaption;
     SerieType:=AType;
   end;
   result:=C;
end;

procedure TBZChart.Clear;
Var
  I:Integer;
begin
 for i:=0 to FSerieList.Count-1 do
 begin
   TBZChartSerieItem(FSerieList.Items[I]).Clear;
 end;
 FSerieList.Clear;
end;

procedure TBZChart.DrawBackground;
var
  Width,Height,x2,y2 : Integer;
  OuterWidth, OuterHeight : Integer;
  InnerWidth, InnerHeight : Integer;
  LegendWidth, LegendHeight : Integer;
  AxisXRect, TitleRect,LegendRect, OuterRect, InnerRect, TmpRect : TRect;

  XStart, I,J,l,h, x,y, MaxSerie, DeltaX, DeltaY,LegendX, LegendY :Integer;
  Mini,Maxi:Double;
  AColor : TBZColor;

  function CalculPosValue(Const AValue:Double):Integer;
  begin
    result:=InnerWidth;
  end;

begin
  Width:=FBZBitmap.Width;
  Height:=FBZBitmap.Height;
  OuterRect.Left:=percent(Width,  FOuterMargin);
  OuterRect.Top:=percent(Height,  FOuterMargin);;
  OuterRect.Right:=Width-percent(Width,  FOuterMargin);
  OuterRect.Bottom:=Height-percent(Height,  FOuterMargin);
  OuterWidth:=OuterRect.Right-OuterRect.Left;
  OuterHeight:=OuterRect.Bottom-OuterRect.Top;

  InnerRect.Left:=OuterRect.Left+percent(OuterWidth,  FInnerMargin);
  InnerRect.Top:=OuterRect.Top+percent(OuterHeight,  FInnerMargin);//percent(Height,  FInnerMargin);
  InnerRect.Right:=OuterRect.Right-percent(OuterWidth,  FInnerMargin);;//percent(Width,  FInnerMargin);
  InnerRect.Bottom:=OuterRect.Bottom-percent(OuterHeight,  FInnerMargin);//percent(Height,  FInnerMargin);
  InnerWidth:=InnerRect.Right-InnerRect.Left;
  InnerHeight:=InnerRect.Bottom-InnerRect.Top;

  if FShowLegend  then
  begin
    InnerRect.Right:=InnerRect.Right-((InnerWidth div FSerieList.Count) div 2);
    InnerWidth:= InnerRect.Right-InnerRect.Left;
    LegendRect.Left:=InnerRect.Right+4;
    LegendRect.Top:= 4;//OuterRect.Top-4;
    LegendRect.Right:=Width-4;
    LegendRect.Bottom:=Height-4;//InnerRect.Bottom + 4;
    LegendWidth:=LegendRect.Right-LegendRect.Left;
    LegendHeight:=LegendRect.Bottom-LegendRect.Top;
  end;

  With FBZBitmap do
  begin
    Clear(clrWhite);
    // Axis X
    Canvas.Pen.Style := psSolid;
    Canvas.Pen.Color := clrBlack;
    Canvas.HLine(InnerRect.Left-4,InnerRect.Bottom,InnerWidth+4);

    // Axis Y
    Canvas.VLine(InnerRect.Left,InnerRect.Top,InnerHeight+4);
    Canvas.VLine(InnerRect.Right,InnerRect.Top,InnerHeight+4);
    if FShowTitle then
    begin
      TitleRect.Left:=InnerRect.Left;
      TitleRect.Top:=4;
      TitleRect.Right:=InnerRect.Right;
      TitleRect.Bottom:=InnerRect.Top-8;

      Canvas.Font.Color:=clrBlack;
      //Canvas.Font.Alignment:=ftaCenter;
      //Canvas.WriteText(FTitle, TitleRect);
      Canvas.TextOut(TitleRect.Left, TitleRect.Top, FTitle);
    end;

    if FShowLegend then
    begin
      AColor:=clrLtGray;
      AColor.Alpha:=128;
      Canvas.Pen.Color := clrGray;
      Canvas.Pen.Style := psSolid;
      Canvas.Brush.Style := bsSolid;
      Canvas.Brush.Color := AColor;
      Canvas.DrawMode.AlphaMode := amAlpha;
      LegendRect.Left:=InnerRect.Right+4;
      LegendRect.Top:= 4;//OuterRect.Top-4;
      LegendRect.Right:=Width-4;
      LegendRect.Bottom:=Height-4;//InnerRect.Bottom + 4;
      Canvas.Rectangle(LegendRect.Left, LegendRect.Top, LegendRect.Right, LegendRect.Bottom);
      Canvas.DrawMode.AlphaMode := amNone;
    end;

    MaxSerie:=0;
    Mini:=100000000000;
    Maxi:=0;
    for i:=0 to FSerieList.Count-1 do
    begin
     // Series[I].Sort(sstByValue);
      if (Series[I].SerieType = cstLines) or (Series[I].SerieType = cstPoints) then
      begin
        if Series[I].Count>MaxSerie then MaxSerie:=Series[I].Count;
        if (Series[I].getMin<Mini) then mini:=Series[I].getMin();
        if (Series[I].getMax>Maxi) then maxi:=Series[I].getMax();
      end
      else
      begin
        if (Series[I].getMin<Mini) then mini:=Series[I].getMin();
        if (Series[I].getMax>Maxi) then maxi:=Series[I].getMax();
        MaxSerie := MaxSerie + Series[I].Count;
      end;
    end;
    Maxi:=(Maxi*105)/100;
   //showmessage('deltaX : '+inttostr(MaxSerie));
   DeltaX:=(InnerWidth-4-(FSerieList.Count*4)) div MaxSerie;
   If DeltaX<3 then DeltaX:=3;
   //showmessage('deltaX : '+inttostr(DeltaX));
    DeltaY:=InnerHeight div FAxisFreq;

    AxisXRect.Left :=4;
    AxisXRect.Top:=OuterRect.Top-12;
    AxisXRect.Right:=InnerRect.Left-8;
    AxisXRect.Bottom:=AxisXRect.Top+18;
    Canvas.Font.Color:=clrBlack;
    //Canvas.Font.Alignment:=ftaRight;
    //WriteText(FAxisXLabel, AxisXRect);
    Canvas.TextOut(AxisXRect.Left,AxisXRect.Top,FAxisXLabel);

    for i:=1 to FAxisFreq do
    begin
      if (FGridType=cgtHorizontal) or (FGridType=cgtBoth) then l:=InnerWidth+4
      else l:=4;
      Canvas.Pen.Color := clrGray;
      Canvas.Pen.Style := psSolid;
      Canvas.HLine(InnerRect.Left-4, InnerRect.Bottom-(i*DeltaY),l);


      Canvas.Font.Color:=clrBlack;
      //Font.Alignment:=ftaRight;
      //WriteText(inttostr(round(i*(Maxi/FAxisFreq))),rect(4,InnerRect.Bottom-(i*DeltaY)-8,InnerRect.Left-8,InnerRect.Bottom-(i*DeltaY)+8));
       Canvas.TextOut(InnerRect.Left-8,InnerRect.Bottom-(i*DeltaY)+8,inttostr(round(i*(Maxi/FAxisFreq))));
    end;

    // Grid / TickMark

    if FSerieList.Count>0 then
    begin
      x:=InnerRect.Left+2;
      xStart:=x;
      LegendY:=LegendRect.Top+4;
      for i:=0 to FSerieList.Count-1 do
      begin
        LegendX:=LegendRect.Left+4;

        if (Series[I].SerieType = cstLines) or (Series[I].SerieType = cstPoints) then
        begin

        end
        else
        begin
          For J:=0 to Series[I].Count-1 do
          begin
             AColor:=Series[I].Items[J].Color;
            if FShowLegend and (I=0) then
            begin
              //AColor:=clrLtGray;

              Canvas.Pen.Color := AColor;
              Canvas.Pen.Style := psSolid;
              Canvas.Brush.Style := bsSolid;
              Canvas.Brush.Color := AColor;
              AColor.Alpha:=192;
              Canvas.DrawMode.AlphaMode := amAlpha;
              Canvas.Rectangle(LegendX, LegendY, LegendX+16, LegendY+12);
              Canvas.DrawMode.AlphaMode := amNone;

              Canvas.Font.Color:=clrBlack;
              //Canvas.Font.Alignment:=ftaLeft;
              TmpRect :=  rect(LegendX+18,LegendY+2,LegendRect.Right-4,LegendY+14);
              //WriteText(Series[I].Items[J].Caption, rect(LegendX+18,LegendY+2,LegendRect.Right-4,LegendY+14));
              Canvas.TextOut(tmpRect.left, tmpRect.Top,Series[I].Items[J].Caption);
              inc(LegendY,14);
            end;
            if (FGridType=cgtVertical) or (FGridType=cgtBoth) then
            begin
              if (J=0) then h:=InnerHeight+16 else h:=InnerHeight+8;
              Canvas.Pen.Color := clrGray;
              Canvas.Pen.Style := psSolid;
              Canvas.VLine(x-1,InnerRect.Top-4,h);
            end
            else
            begin
              h:=4;
              Canvas.Pen.Color := clrGray;
              Canvas.Pen.Style := psSolid;
              Canvas.VLine(x,InnerRect.Bottom,4);
            end;
            y:=round((100*Series[I].Items[J].Value)/(Maxi));
            //showmessage(Inttostr(Y));
            y:=round(y*InnerHeight/100);
            //showmessage(Inttostr(Y));
            AColor:=Series[I].Items[J].Color;
            AColor.alpha:=192;
            Canvas.Pen.Style := psClear;
            //Canvas.Pen.Color := AColor;
            Canvas.Brush.Style := bsSolid;
            Canvas.Brush.Color := AColor;
            Canvas.DrawMode.AlphaMode := amAlpha;
            Canvas.Rectangle(X,InnerRect.Bottom-y+1,X+DeltaX,InnerRect.Bottom-1);
            Canvas.DrawMode.AlphaMode := amNone;
  //          Box(x,InnerRect.Bottom-y,x+DeltaX,InnerRect.Bottom,Series[I].Items[J].Color);
            inc(x,DeltaX);
          end;
        end;

        if (FGridType=cgtVertical) or (FGridType=cgtBoth) then
        begin
          h:=InnerHeight+16;
          Canvas.Pen.Color := clrGray;
          Canvas.Pen.Style := psSolid;
          Canvas.VLine(x-1,InnerRect.Top-4,h);
        end
        else
        begin
          h:=4;
          Canvas.Pen.Color := clrGray;
          Canvas.Pen.Style := psSolid;
          Canvas.VLine(x,InnerRect.Bottom,4);
        end;
        Canvas.Font.Color:=clrBlack;
        //Canvas.Font.Alignment:=ftaCenter;
        //WriteText(Series[I].Caption, rect(XStart,InnerRect.Bottom+2,x,(InnerRect.Bottom+2)+(Height-InnerRect.Bottom-2)));
        tmpRect := rect(XStart,InnerRect.Bottom+2,x,(InnerRect.Bottom+2)+(Height-InnerRect.Bottom-2));
        Canvas.Textout(TmpRect.left, tmpRect.Top,Series[I].Caption);
        //Box(XStart,InnerRect.Bottom+2,x-XStart,Height-InnerRect.Bottom-2,clrBlack);
        inc(x,4);
        XStart:=x;
      end;
    end;
    //changed;
  end;
end;

procedure TBZChart.Draw;
begin
  DrawBackground();
end;

//==============================================================================

end.

