unit BZGizmoImage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Forms,
  BZClasses, BZArrayClasses, BZMath, BZVectorMath, BZColors, BZGraphic, BZBitmap, BZImageViewer;

Type
  TBZGizmoImageSelectionMode = (gdmNone, gdmPoint, gsmRectangular, gsmCircular, gsmFence, gsmLasso);
  TBZGizmoImageOperation = (goNone, goSelect, goMove, goRotate, goScale);
  TBZGizmoSelectionDragMode = (sdmNone, sdmInside, sdmTop, sdmBottom, sdmLeft, sdmRight,
                               sdmTopLeft, sdmTopRight, sdmBottomLeft, sdmBottomRight);
  { TBZGizmoImage }

  { TBZGizmoImageViewer }

  TBZGizmoImageViewer = Class(TBZUpdateAbleComponent)
  private
    FOperation : TBZGizmoImageOperation;
    FSelectionMode : TBZGizmoImageSelectionMode;

    FEnabled : Boolean;
    FImageViewer : TBZImageViewer;

    FControlPoint : TBZArrayOfPoints;

    FMoving: Boolean;
    FSelectionMouseActive : Boolean;
    FEmptySelection : Boolean;
    FRectSelectionDragMode : TBZGizmoSelectionDragMode;
    FPointRadiusSize : Byte;
    FStartPos, FEndPos : TBZPoint;
    FPreviousStartPos, FPreviousEndPos : TBZPoint;
    FCurrentMousePos, FLastMousePos : TBZPoint;
    FBoundTopLeftPoint,
    FBoundTopRightPoint,
    FBoundTopCenterPoint,
    FBoundBottomLeftPoint,
    FBoundBottomRightPoint,
    FBoundBottomCenterPoint,
    FBoundLeftCenterPoint,
    FBoundRightCenterPoint,
    FBoundCenterPoint,
    FSelectionRect : TBZRect;

    FPenPointColor   : TBZColor;
    FBrushPointColor : TBZColor;
    FPenShapeColor   : TBZColor;
    FBrushShapeColor : TBZColor;

    FOnUpdate: TNotifyEvent;
    //FOnSelect: TGLGizmoExAcceptEvent;
    FOnSelectionModeChange: TNotifyEvent;
    FOnOperationChange: TNotifyEvent;
    FOnSelectionLost: TNotifyEvent;
    procedure SetImageViewer(const AValue : TBZImageViewer);
    procedure SetSelectionMode(const AValue : TBZGizmoImageSelectionMode);
    procedure SetOperation(const AValue : TBZGizmoImageOperation);
  protected
    procedure DrawPoint(x,y : Integer; VirtualCanvas : TBZBitmapCanvas);
    procedure DrawShape(VirtualCanvas : TBZBitmapCanvas);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure ViewerMouseMove(Sender : TObject; Shift : TShiftState; X, Y : Integer);
    procedure ViewerMouseDown(Sender : TObject; Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
    procedure ViewerMouseUp(Sender : TObject; Button : TMouseButton; Shift : TShiftState; X, Y : Integer);

    procedure UpdateGizmo; overload;

    procedure LooseSelection; virtual;
    procedure LooseCursorSelection;

    function getControlPoints : TBZArrayOfPoints;

  published
    property ImageViewer: TBZImageViewer read FImageViewer write SetImageViewer;
    property SelectionMode: TBZGizmoImageSelectionMode read FSelectionMode write SetSelectionMode default gsmRectangular;
    property Operation : TBZGizmoImageOperation read FOperation write SetOperation;

    property Enabled: Boolean read FEnabled write FEnabled default True;
    //property Visible: Boolean read GetVisible write SetVisible;

    //property PenPointColor   : TBZColor read FPenPointColor write FPenPointColor;
    //property BrushPointColor : TBZColor read FBrushPointColor write FBrushPointColor;
    //property PenShapeColor   : TBZColor read FPenShapeColor write FPenShapeColor;
    //property BrushShapeColor : TBZColor read FBrushShapeColor write FBrushShapeColor;
    property PointRadiusSize       : Byte read FPointRadiusSize write FPointRadiusSize;

    property OnSelectionLost: TNotifyEvent read FOnSelectionLost write FOnSelectionLost;
    property OnSelectionModeChange: TNotifyEvent read FOnSelectionModeChange write FOnSelectionModeChange;
    property OnOperationChange: TNotifyEvent read FOnOperationChange write FOnOperationChange;
    //property OnSelect: TGLGizmoExAcceptEvent read FOnSelect write FOnSelect;
    property OnUpdate: TNotifyEvent read FOnUpdate write FOnUpdate;
  end;

implementation

{ TBZGizmoImage }

procedure TBZGizmoImageViewer.SetImageViewer(const AValue : TBZImageViewer);
begin
  if FImageViewer = AValue then Exit;
  FImageViewer := AValue;
  //FImageViewer.OnMouseDown := @ViewerMouseDown;
  //FImageViewer.OnMouseUp := @ViewerMouseUp;
  //FImageViewer.OnMouseMove := @ViewerMouseMove;
  FImageViewer.OnAfterPaint := @DrawShape;
end;

procedure TBZGizmoImageViewer.SetOperation(const AValue : TBZGizmoImageOperation);
begin
  if FOperation = AValue then Exit;
  FOperation := AValue;
end;

procedure TBZGizmoImageViewer.SetSelectionMode(const AValue : TBZGizmoImageSelectionMode);
begin
  if FSelectionMode = AValue then Exit;
  FSelectionMode := AValue;
end;

procedure TBZGizmoImageViewer.DrawPoint(x, y : Integer; VirtualCanvas : TBZBitmapCanvas);
Var
  OldPenStyle : TBZStrokeStyle;
  OldBrushStyle : TBZBrushStyle;
  OldCombineMode : TBZColorCombineMode;
  OldPixelMode : TBZBitmapDrawMode;
  OldAlphaMode : TBZBitmapAlphaMode;
  OldPenColor : TBZColor;
  OldBrushColor : TBZColor;
begin
  With VirtualCanvas do
  begin
    OldPenStyle := Pen.Style;
    OldPenColor := Pen.Color;
    OldBrushStyle := Brush.Style;
    OldBrushColor := Brush.Color;
    OldCombineMode := DrawMode.CombineMode;
    OldPixelMode := DrawMode.PixelMode;
    OldAlphaMode := DrawMode.AlphaMode;
    DrawMode.PixelMode := dmCombine;
    DrawMode.AlphaMode := amNone; //amAlphaBlendHQ;
    DrawMode.CombineMode := cmXOr;
    Pen.Style := ssSolid;
    Pen.Color := clrYellow;
    Brush.Style := bsSolid;
    Brush.Color := clrRed;
    //Rectangle(X - FPointRadiusSize, Y  - FPointRadiusSize, X + FPointRadiusSize, Y + FPointRadiusSize);
    Circle(X,Y, FPointRadiusSize);
    Pen.Style := OldPenStyle;
    Pen.Color := OldPenColor;
    Brush.Style := OldBrushStyle;
    Brush.Color := OldBrushColor;
    DrawMode.CombineMode := OldCombineMode;
    DrawMode.PixelMode := OldPixelMode;
    DrawMode.AlphaMode := OldAlphaMode;
  end;
end;

procedure TBZGizmoImageViewer.DrawShape(VirtualCanvas : TBZBitmapCanvas);
Var
  OldPenStyle : TBZStrokeStyle;
  OldBrushStyle : TBZBrushStyle;
  OldCombineMode : TBZColorCombineMode;
  OldAlphaMode : TBZBitmapAlphaMode;
  OldPixelMode : TBZBitmapDrawMode;
  OldPenColor : TBZColor;
  VertMiddle, HorizMiddle : Integer;
begin
  Case FSelectionMode of
    gdmNone: ;
    gdmPoint:
    begin
      ;
    end;
    gsmRectangular:
    begin
      With VirtualCanvas do
      begin
        OldCombineMode := DrawMode.CombineMode;
        OldPixelMode := DrawMode.PixelMode;
        OldAlphaMode := DrawMode.AlphaMode;
        OldPenStyle := Pen.Style;
        OldPenColor := Pen.Color;
        OldBrushStyle := Brush.Style;
        DrawMode.PixelMode := dmCombine;
        DrawMode.AlphaMode := amNone; //amAlphaBlendHQ;
        DrawMode.CombineMode := cmXor;
        Pen.Style := ssSolid;
        Pen.Color := clrYellow;
        Brush.Style := bsClear;
        Rectangle(FSelectionRect);// (FStartPos.X, FStartPos.Y, FEndPos.X, FEndPos.Y);
        Pen.Style := OldPenStyle;
        Pen.Color := OldPenColor;
        Brush.Style := OldBrushStyle;
        DrawMode.CombineMode := OldCombineMode;
        DrawMode.PixelMode := OldPixelMode;
        DrawMode.AlphaMode := OldAlphaMode;
      end;

      HorizMiddle :=   FSelectionRect.CenterPoint.X;
      VertMiddle := FSelectionRect.CenterPoint.Y;

      DrawPoint(FSelectionRect.Left, FSelectionRect.Top, VirtualCanvas);
      DrawPoint(FSelectionRect.Left, FSelectionRect.Bottom, VirtualCanvas);
      DrawPoint(FSelectionRect.Right, FSelectionRect.Top, VirtualCanvas);
      DrawPoint(FSelectionRect.Right, FSelectionRect.Bottom, VirtualCanvas);
      DrawPoint(HorizMiddle, FSelectionRect.Top, VirtualCanvas);
      DrawPoint(HorizMiddle, FSelectionRect.Bottom, VirtualCanvas);
      DrawPoint(FSelectionRect.Left, VertMiddle, VirtualCanvas);
      DrawPoint(FSelectionRect.Right, VertMiddle, VirtualCanvas);
      DrawPoint(HorizMiddle, VertMiddle, VirtualCanvas);
    end;
    gsmCircular: ;
    gsmFence: ;
    gsmLasso: ;
  end;
end;

constructor TBZGizmoImageViewer.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
end;

destructor TBZGizmoImageViewer.Destroy;
begin
  inherited Destroy;
end;

procedure TBZGizmoImageViewer.Loaded;
begin
  inherited Loaded;
end;

procedure TBZGizmoImageViewer.Notification(AComponent : TComponent; Operation : TOperation);
begin
  inherited Notification(AComponent, Operation);
end;

procedure TBZGizmoImageViewer.ViewerMouseMove(Sender : TObject; Shift : TShiftState; X, Y : Integer);
Var
  DeltaPos : TBZPoint;
begin
  FCurrentMousePos.Create(X, Y);
  FCurrentMousePos := FCurrentMousePos + FImageViewer.VirtualViewPort.TopLeft;


  if FSelectionMouseActive then
  begin
    if (ssLeft in Shift) then
    begin
      FPreviousStartPos := FStartPos;
      FPreviousEndPos := FEndPos;

      if FEmptySelection then
      begin
        FEndPos := FCurrentMousePos + FImageViewer.VirtualViewPort.TopLeft;
        UpdateGizmo;
        FImageViewer.Invalidate;
      end
      else
      begin
        Case FRectSelectionDragMode of
          sdmInside:
          begin
            DeltaPos := FCurrentMousePos - FLastMousePos;
            FStartPos := FStartPos + DeltaPos;
            FEndPos := FEndPos + DeltaPos;
          end;
          sdmTop:
          begin
            DeltaPos := FCurrentMousePos - FLastMousePos;
            FStartPos.Y := min((FStartPos.Y + DeltaPos.Y), FEndPos.Y);
          end;
          sdmBottom:
          begin
            DeltaPos := FCurrentMousePos - FLastMousePos;
            FEndPos.Y := max((FEndPos.Y + DeltaPos.Y), FStartPos.Y);
          end;
          sdmLeft:
          begin
            DeltaPos := FCurrentMousePos - FLastMousePos;
            FStartPos.X := min((FStartPos.X + DeltaPos.X), FEndPos.X);
          end;
          sdmRight:
          begin
            DeltaPos := FCurrentMousePos - FLastMousePos;
            FEndPos.X := max((FEndPos.X + DeltaPos.X), FStartPos.X);
          end;
          sdmTopLeft:
          begin
            DeltaPos := FCurrentMousePos - FLastMousePos;
            FStartPos := FStartPos + DeltaPos;
            FStartPos := FStartPos.Min(FEndPos);
          end;
          sdmTopRight:
          begin
            DeltaPos := FCurrentMousePos - FLastMousePos;
            FEndPos.X := max((FEndPos.X + DeltaPos.X), FStartPos.X);
            FStartPos.Y := min((FStartPos.Y + DeltaPos.Y), FEndPos.Y);
          end;
          sdmBottomLeft:
          begin
            DeltaPos := FCurrentMousePos - FLastMousePos;
            FEndPos.Y := max((FEndPos.Y + DeltaPos.Y), FStartPos.Y);
            FStartPos.X := min((FStartPos.X + DeltaPos.X), FEndPos.X);
          end;
          sdmBottomRight:
          begin
            DeltaPos := FCurrentMousePos - FLastMousePos;
            FEndPos := FEndPos + DeltaPos;
            FEndPos := FEndPos.Max(FStartPos);
          end;
        end;
        UpdateGizmo;
        FImageViewer.Invalidate;
      end;
      FLastMousePos := FCurrentMousePos;
    end
    else
    begin
      Screen.Cursor := crDefault;
    end;
  end
  else
  begin
    if Not(FEmptySelection) then
    begin
      if FBoundTopLeftPoint.PointInRect(FCurrentMousePos) then
      begin
        //FRectSelectionDragMode := sdmTopLeft;
        Screen.Cursor := crSizeNWSE;
      end
      else if FBoundTopRightPoint.PointInRect(FCurrentMousePos) then
      begin
        //FRectSelectionDragMode := sdmTopRight;
        Screen.Cursor := crSizeNESW;
      end
      else if FBoundTopCenterPoint.PointInRect(FCurrentMousePos) then
      begin
        //FRectSelectionDragMode := sdmTop;
        Screen.Cursor := crSizeNS;
      end
      else if FBoundBottomLeftPoint.PointInRect(FCurrentMousePos) then
      begin
        //FRectSelectionDragMode := sdmBottomLeft;
        Screen.Cursor := crSizeNESW;
      end
      else if FBoundBottomCenterPoint.PointInRect(FCurrentMousePos) then
      begin
        //FRectSelectionDragMode := sdmBottom;
        Screen.Cursor := crSizeNS;
      end
      else if FBoundBottomRightPoint.PointInRect(FCurrentMousePos) then
      begin
        //FRectSelectionDragMode := sdmBottomRight;
        Screen.Cursor := crSizeNWSE;
      end
      else if FBoundLeftCenterPoint.PointInRect(FCurrentMousePos) then
      begin
        //FRectSelectionDragMode := sdmLeft;
        Screen.Cursor := crSizeWE;
      end
      else if FBoundRightCenterPoint.PointInRect(FCurrentMousePos) then
      begin
        //FRectSelectionDragMode := sdmRight;
        Screen.Cursor := crSizeWE;
      end
      else if FSelectionRect.PointInRect(FCurrentMousePos) then
      begin
        //FRectSelectionDragMode := sdmInside;
        Screen.Cursor := crSizeAll;
      end
      else
      begin
        //FRectSelectionDragMode := sdmNone;
        Screen.Cursor := crDefault;
      end;
    end;
  end;
end;

procedure TBZGizmoImageViewer.ViewerMouseDown(Sender : TObject; Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
begin
  FSelectionMouseActive := True;
  FCurrentMousePos.Create(X, Y);
  if FEmptySelection then
  begin
    FStartPos := FCurrentMousePos + FImageViewer.VirtualViewPort.TopLeft;
    FRectSelectionDragMode := sdmBottomRight;
  end
  else
  begin
    if FBoundTopLeftPoint.PointInRect(FCurrentMousePos) then
    begin
      FRectSelectionDragMode := sdmTopLeft;
      Screen.Cursor := crSizeNWSE;
    end
    else if FBoundTopRightPoint.PointInRect(FCurrentMousePos) then
    begin
      FRectSelectionDragMode := sdmTopRight;
      Screen.Cursor := crSizeNESW;
    end
    else if FBoundTopCenterPoint.PointInRect(FCurrentMousePos) then
    begin
      FRectSelectionDragMode := sdmTop;
      Screen.Cursor := crSizeNS;
    end
    else if FBoundBottomLeftPoint.PointInRect(FCurrentMousePos) then
    begin
      FRectSelectionDragMode := sdmBottomLeft;
      Screen.Cursor := crSizeNESW;
    end
    else if FBoundBottomCenterPoint.PointInRect(FCurrentMousePos) then
    begin
      FRectSelectionDragMode := sdmBottom;
      Screen.Cursor := crSizeNS;
    end
    else if FBoundBottomRightPoint.PointInRect(FCurrentMousePos) then
    begin
      FRectSelectionDragMode := sdmBottomRight;
      Screen.Cursor := crSizeNWSE;
    end
    else if FBoundLeftCenterPoint.PointInRect(FCurrentMousePos) then
    begin
      FRectSelectionDragMode := sdmLeft;
      Screen.Cursor := crSizeWE;
    end
    else if FBoundRightCenterPoint.PointInRect(FCurrentMousePos) then
    begin
      FRectSelectionDragMode := sdmRight;
      Screen.Cursor := crSizeWE;
    end
    else if FSelectionRect.PointInRect(FCurrentMousePos) then
    begin
      FRectSelectionDragMode := sdmInside;
      Screen.Cursor := crSizeAll;
    end
    else
    begin
      FRectSelectionDragMode := sdmNone;
      Screen.Cursor := crDefault;
    end;
    FLastMousePos := FCurrentMousePos;
  end;

end;

procedure TBZGizmoImageViewer.ViewerMouseUp(Sender : TObject; Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
Var
  TempPos : TBZPoint;
begin
  FSelectionMouseActive := False;
  if FEmptySelection then FEmptySelection := False;
  FRectSelectionDragMode := sdmNone;

  if (FStartPos.X > FEndPos.X) or (FStartPos.Y > FEndPos.Y) then
  begin
    TempPos := FStartPos;
    FStartPos := FEndPos;
    FEndPos := TempPos;
    FImageViewer.Invalidate;
  end;
  Screen.Cursor := crDefault;
end;

procedure TBZGizmoImageViewer.UpdateGizmo;
begin
  FSelectionRect.Create(FStartPos.X + FPointRadiusSize, FStartPos.Y + FPointRadiusSize, FEndPos.X - FPointRadiusSize, FEndPos.Y - FPointRadiusSize, true);

  FBoundTopLeftPoint.Create((FSelectionRect.TopLeft - FPointRadiusSize), (FSelectionRect.TopLeft + FPointRadiusSize));

  FBoundTopRightPoint.Create((FSelectionRect.Right - FPointRadiusSize), (FSelectionRect.Top - FPointRadiusSize), (FSelectionRect.Right + FPointRadiusSize), (FSelectionRect.Top + FPointRadiusSize));
  //FBoundTopRightPoint.Create(FEndPos.X - FPointRadiusSize, FStartPos.Y  - FPointRadiusSize, FEndPos.X + FPointRadiusSize, FStartPos.Y + FPointRadiusSize);
  FBoundBottomLeftPoint.Create((FSelectionRect.Left - FPointRadiusSize), (FSelectionRect.Bottom - FPointRadiusSize), (FSelectionRect.Left + FPointRadiusSize), (FSelectionRect.Bottom + FPointRadiusSize));
  //FBoundBottomLeftPoint.Create(FStartPos.X - FPointRadiusSize, FEndPos.Y  - FPointRadiusSize, FStartPos.X + FPointRadiusSize, FEndPos.Y + FPointRadiusSize);
  FBoundBottomRightPoint.Create((FSelectionRect.BottomRight - FPointRadiusSize), (FSelectionRect.BottomRight + FPointRadiusSize));
  //FBoundBottomRightPoint.Create(FEndPos.X - FPointRadiusSize, FEndPos.Y  - FPointRadiusSize, FEndPos.X + FPointRadiusSize, FEndPos.Y + FPointRadiusSize);
  FBoundTopCenterPoint.Create((FSelectionRect.CenterPoint.X - FPointRadiusSize), (FSelectionRect.Top - FPointRadiusSize),(FSelectionRect.CenterPoint.X + FPointRadiusSize), (FSelectionRect.Top + FPointRadiusSize));
  //FBoundTopCenterPoint.Create(HorizMiddle - FPointRadiusSize, FStartPos.Y - FPointRadiusSize, HorizMiddle + FPointRadiusSize, FStartPos.Y + FPointRadiusSize);
  FBoundBottomCenterPoint.Create((FSelectionRect.CenterPoint.X - FPointRadiusSize), (FSelectionRect.Bottom - FPointRadiusSize),(FSelectionRect.CenterPoint.X + FPointRadiusSize), (FSelectionRect.Bottom + FPointRadiusSize));
  //FBoundBottomCenterPoint.Create(HorizMiddle - FPointRadiusSize, FEndPos.Y - FPointRadiusSize, HorizMiddle + FPointRadiusSize, FEndPos.Y + FPointRadiusSize);
  FBoundLeftCenterPoint.Create((FSelectionRect.Left - FPointRadiusSize), (FSelectionRect.CenterPoint.Y - FPointRadiusSize), (FSelectionRect.Left + FPointRadiusSize), (FSelectionRect.CenterPoint.Y + FPointRadiusSize));
  //FBoundLeftCenterPoint.Create(FStartPos.X - FPointRadiusSize, VertMiddle - FPointRadiusSize, FStartPos.X + FPointRadiusSize, VertMiddle + FPointRadiusSize);
  FBoundRightCenterPoint.Create((FSelectionRect.Right - FPointRadiusSize), (FSelectionRect.CenterPoint.Y - FPointRadiusSize), (FSelectionRect.Right + FPointRadiusSize), (FSelectionRect.CenterPoint.Y + FPointRadiusSize));
  //FBoundRightCenterPoint.Create(FEndPos.X - FPointRadiusSize, VertMiddle - FPointRadiusSize, FEndPos.X + FPointRadiusSize, VertMiddle + FPointRadiusSize);
  FBoundCenterPoint.Create((FSelectionRect.CenterPoint.X - FPointRadiusSize), (FSelectionRect.CenterPoint.Y - FPointRadiusSize),(FSelectionRect.CenterPoint.X + FPointRadiusSize), (FSelectionRect.CenterPoint.Y + FPointRadiusSize));
  //FBoundCenterPoint.Create(HorizMiddle - FPointRadiusSize, VertMiddle - FPointRadiusSize, HorizMiddle + FPointRadiusSize, VertMiddle + FPointRadiusSize);

end;

procedure TBZGizmoImageViewer.LooseSelection;
begin
  FStartPos.Create(0,0);
  FEndPos.Create(0,0);
  FPreviousStartPos.Create(0,0);
  FPreviousEndPos.Create(0,0);
  FEmptySelection := True;
  FRectSelectionDragMode := sdmNone;
  FSelectionMouseActive := False;
  FPointRadiusSize := 4;
end;

procedure TBZGizmoImageViewer.LooseCursorSelection;
begin

end;

function TBZGizmoImageViewer.getControlPoints : TBZArrayOfPoints;
begin

end;

end.

