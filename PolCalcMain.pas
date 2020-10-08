unit PolCalcMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Controls.Presentation, FMX.Ani, FMX.Edit, FMX.ListView.Types,
  FMX.ListView.Appearances, FMX.ListView.Adapters.Base, FMX.ListView,
  System.Sensors, System.Sensors.Components, FMX.DateTimeCtrls, System.DateUtils,
  System.Math, STAF_AstroUnit;

type

  T_Coord = record
    X, Y: double;
  end;

  TPoleForm = class(TForm)
    PaintBox1: TPaintBox;
    StyleBook1: TStyleBook;
    Label4: TLabel;
    LocationSensor1: TLocationSensor;
    Rectangle1: TRectangle;
    Label1: TLabel;
    lbLongitude: TLabel;
    lbLatitude: TLabel;
    Label2: TLabel;
    lbUT: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    lbJD: TLabel;
    lbLST: TLabel;
    lbDateTime: TLabel;
    Timer1: TTimer;
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    procedure PaintBox1Resize(Sender: TObject);
    procedure LocationSensor1LocationChanged(Sender: TObject;
      const [Ref] OldLocation, NewLocation: TLocationCoord2D);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Timer1Timer(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject; Canvas: TCanvas);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    Origo: TPointF;
    fokRadius: integer;
    FPicState: integer;  // 1 deg. radius on Image
    procedure doCalc(dat: TDateTime);
    procedure Init;
    procedure BaseDraw;
    procedure SetPicState(const Value: integer);
  public
    dat : TDateTime;
    UT  : TDateTime;
    JD  : double;
    LST : double;
    LON : extended; // = 20.2919304371;
    LAT : extended; // = 48.2167524154;
    SID : double;
    PolarisCoord : T_Coord;
    ProperMotion : T_Coord;
    procedure PoleDraw(LONGITUDE, LATITUDE: double);
    property PicState : integer read FPicState write SetPicState;
  end;

var
  PoleForm: TPoleForm;

// FMXUtils
   procedure SetStroke( ca : TCanvas;
                        stKind : TBrushKind;
                        stColor : TAlphaColor;
                        stThickness : single
                        );

   function DayAfter2000( ido :TDatetime ) : double;
   function YearAfter2000( ido :TDatetime ) : double;
   function GetUTCOffset(d : TDateTime): integer;


implementation

{$R *.fmx}
{$R *.NmXhdpiPh.fmx ANDROID}
{$R *.LgXhdpiPh.fmx ANDROID}

procedure SetStroke( ca : TCanvas;
                        stKind : TBrushKind;
                        stColor : TAlphaColor;
                        stThickness : single
                        );
   begin
     with ca.Stroke do begin
          Kind := stKind;
          Color := stColor;
          Thickness := stThickness;
     end;
   end;

// Eltelt napok száma JD 2000-01-01 12:00 óta
function DayAfter2000( ido :TDatetime ) : double;
    var year   : word;
    var month  : word;
    var day    : word;
    var hour   : word;
    var minute : word;
    var second,msec : word;
    var jd, jt, mst : double;
    var a, b, c, d : integer;
begin
    DecodeDate(ido,year,month,day);
    DecodeTime(ido,hour,minute,second,msec);

    if (month = 1) or (month = 2) then
    begin
        year  := year - 1;
        month := month + 12;
    end;

    a := floor( year/100 );
    b := 2 - a + floor( a/4 );
    c := floor( 365.25*year );
    d := floor( 30.6001*( month + 1 ) );

    // days since J2000.0
    jd := b + c + d - 730550.5 + day
        + (hour + minute/60.0 + second/3600.0)/24.0;
    Result := jd;
end;

// Eltelt évek száma JD 2000-01-01 12:00 óta
function YearAfter2000( ido :TDatetime ) : double;
    var JD1,JD2: double;
begin
    JD1 := DateTimeToJulianDate( EncodeDateTime(2000,1,1,12,0,0,0) );
    JD2 := DateTimeToJulianDate( ido );
    Result := jd2-jd1;
    Result := Result/365.25;
end;

// A 2000 epoch koordináták alapján T idõpontra kiszámítja a csillag koordinátáit
// a sajátmozgás (*_pm) ismeretében
// Ra2000,De2000 : fokokban;
// _pm           : ívmásodpercben
// T             : TDateTime (egész rész napok, tört rész a nap tört része
function StarProperMotion(Ra2000, De2000 : double; Ra_pm, De_pm : double; T: TDateTime): TEQKoord;
var years: double;
begin
  years := YearAfter2000( T );
  with Result do begin
       ra := Ra2000 + (years * Ra_pm * cos(de));
       de := De2000 + years * De_pm;
  end;
end;

function GetUTCOffset(d : TDateTime): integer;
var uts: string;
    ks : string;
    k,korr : integer;
begin
  Result := 0;
  uts:=DateToISO8601(d, false);
  k := AnsiPos('+',uts);
  ks := Copy(uts,Length(uts)-5,3);
  Result := strtoint(ks);
end;

// =============================================================================
procedure TPoleForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  LocationSensor1.Active := False;
end;

procedure TPoleForm.FormCreate(Sender: TObject);
begin
  FPicState := 0;
  LocationSensor1.Active := True;
end;

procedure TPoleForm.LocationSensor1LocationChanged(Sender: TObject;
  const [Ref] OldLocation, NewLocation: TLocationCoord2D);
const
  LGoogleMapsURL: String = 'https://maps.google.com/maps?q=%s,%s';
var
  ENUSLat, ENUSLong: String; // holders for URL strings
begin
  LON := NewLocation.Longitude;
  LAT := NewLocation.Latitude;
  ENUSLat := NewLocation.Latitude.ToString(ffGeneral, 6, 3, TFormatSettings.Create('en-US'));
  ENUSLong := NewLocation.Longitude.ToString(ffGeneral, 6, 3, TFormatSettings.Create('en-US'));
  { convert the location to latitude and longitude }
  lbLatitude.Text  := ENUSLat;
  lbLongitude.Text := ENUSLong;
  Init;
  Timer1.Enabled := True;
end;

procedure TPoleForm.PaintBox1Paint(Sender: TObject; Canvas: TCanvas);
begin
  if Timer1.Enabled then
     PoleDraw(LON,LAT)
  else
     BaseDraw;
end;

procedure TPoleForm.PaintBox1Resize(Sender: TObject);
begin
  PaintBox1.Repaint;
end;

procedure TPoleForm.SetPicState(const Value: integer);
begin
  FPicState := Value;
  PaintBox1.Repaint;
end;

procedure TPoleForm.Timer1Timer(Sender: TObject);
begin
  dat := now();
  doCalc(dat);
end;


procedure TPoleForm.Button1Click(Sender: TObject);
begin
  PicState := 0;
end;

procedure TPoleForm.Button2Click(Sender: TObject);
begin
  PicState := 1;
end;

procedure TPoleForm.Button3Click(Sender: TObject);
begin
  PicState := 2;
end;

procedure TPoleForm.doCalc(dat: TDateTime);
var Korr: integer;  // UT korrekció (+1, +2)
begin
  Korr := GetUTCOffset(dat);
  lbDateTime.Text := FormatDateTime('yyyy.mm.dd hh:nn:ss',dat);
  UT  := dat-(Korr/24);
  lbUT.Text  :=  FormatDateTime('hh:nn:ss',UT);
  JD := DateTimeToJulianDate( UT );
  lbJD.Text  := FormatFloat('#,##0.000000',JD);
//  GMSTLabel.Text := ARToStr_(mean_sidereal_time(utTime.DateTime,0));
  LST := mean_sidereal_time(dat,LON)-Korr;   // Ez a pontos !!!!
  if LST<0 then LST := LST+24.0;
  lbLST.Text := ARToStr_(LST);
end;

procedure TPoleForm.Init;
Var years: double;   // eltelt napok száma 2000-01-01 tõl
    ra,de: double;
    PMAlpha, PMDelta: double; // proper motion
    JT   : double;   // Julian date
    EQ   : TEQKoord;
begin
(* Polaris
J2000 RA:   2h31m48.70s   DE:+89°15'51.0"
*)
  ra := RaToReal(2,31,48.70);
  de := DeToReal(89,15,51.0);
  PMAlpha := 0.038;
  PMDelta := -0.015;
  JT := DateTimeToJulianDate( now );
  EQ := RealStarPosition( JT, ra, de, PMAlpha, PMDelta );
  PolarisCoord.X := EQ.RA;
  PolarisCoord.Y := EQ.DE;
  Origo := PointF(PaintBox1.Width / 2, PaintBox1.Height / 2);
  fokRadius := Round(PaintBox1.Width / 2 - 20);
end;

procedure TPoleForm.BaseDraw;
var
    R : TRectF;
    w,h: single;
    mRect : TRectF;
    i: integer;
    ro: integer;
    sx,sy : double;
begin
  ro := 20;
  w := PaintBox1.width;
  h := PaintBox1.height;
    if w<=h then
       R := RectF(ro,h/2-w/2+ro,w-ro,h/2+w/2-ro)
    else
       R := RectF(w/2-h/2,0,w/2+h/2,h);

  with PaintBox1.Canvas do
  begin
    BeginScene();
    PaintBox1.Canvas.Clear( TAlphaColors.Black );
    // Kereszt rajzolása
    SetStroke( PaintBox1.Canvas, TBrushKind.Solid, TAlphaColors.Gray, 2 );
    DrawLine(PointF(0, h/2), PointF(w, h/2), 1, Stroke);
    DrawLine(PointF(w/2, 0), PointF(w/2, PaintBox1.height), 1, Stroke);
    // Körök rajzolása
    SetStroke( PaintBox1.Canvas, TBrushKind.Solid, TAlphaColors.Red, 2 );
    DrawEllipse(R,1);
    InflateRect(R,-10,-10);
    DrawEllipse(R,1);

    // Óra beosztás  - Scale draw
    ro := 6;
    SetStroke( PaintBox1.Canvas, TBrushKind.Solid, TAlphaColors.Gray, 2 );
    for i := 0 to 72 do
    begin
      sy := sin(DegToRad(5)*i);
      sx := cos(DegToRad(5)*i);
      DrawLine(PointF(Origo.X+sx*(fokRadius+ro),Origo.y+sy*(fokRadius+ro)),
            PointF(Origo.X+sx*(fokRadius),Origo.y+sy*(fokRadius)), 1, Stroke);
    end;
    ro := 12;
    SetStroke( PaintBox1.Canvas, TBrushKind.Solid, TAlphaColors.Silver, 2 );
    for i := 0 to 12 do
    begin
      sy := sin(DegToRad(30)*i);
      sx := cos(DegToRad(30)*i);
      DrawLine(PointF(Origo.X+sx*(fokRadius+ro),Origo.y+sy*(fokRadius+ro)),
            PointF(Origo.X+sx*(fokRadius),Origo.y+sy*(fokRadius)), 1, Stroke);
    end;

     EndScene;
  end;
end;

procedure TPoleForm.PoleDraw(LONGITUDE, LATITUDE: double);
Var H,N,S,MS: word;
    RA,DE: double;
    HZ : THorzKoord;
    EQ : TEQKoord;
    SID : double;
    Pol : TPoint;
    r: integer;
    si,co: double;
    fx,fy: double;
    HA,HA1, HAHour  : double;
    HAHourStr   : string;
    c : string;
    GMT: TDateTime;
begin
  BaseDraw;
  // Show Times
  doCalc(now);
  SID := LST;
  RA  := PolarisCoord.X;
  DE  := PolarisCoord.Y;
  HZ := EqToHz( SID, LATITUDE, RA, DE );   // Polaris Horitontális koordinátái
//  lbRAPolaris.Caption := RaToStr(RealToRa(RA));
//  lbDEPolaris.Caption := DeToStr(RealToDe(DE));
//  lbAZPolaris.Caption := DEToStr_(HZ.AZ);
//  lbALTPolaris.Caption := DEToStr_(HZ.ALT);
  // Drawing Polaris
  HA  :=  15 * SID - Ra ;   // Óraszög fokokban
  if Ha<0 then HA := 360+HA;

  HA1 := DegToRad(HA)+pi/2;
  fx := cos(HA1);
  fy := sin(HA1);
  HA  :=  15 * SID - Ra ;   // Óraszög fokokban
  r :=  fokRadius-5;
  case FPicState of
  0:
  begin
  Pol.X := Round( Origo.X + r * fx );
  Pol.Y := Round( Origo.Y - r * fy );
  HAHour := 12 - HA/30;     // Óraszög óra számlapon
  c := 'NESW';
  end;
  1:
  begin
  Pol.X := Trunc( Origo.X - r * fx );
  Pol.Y := Trunc( Origo.Y + r * fy );
  HAHour := 18-HA/30;     // Óraszög óra számlapon
  c := 'SWNE';
  end;
  2:
  begin
  Pol.X := Trunc( Origo.X - r * fx );
  Pol.Y := Trunc( Origo.Y - r * fy );
  HAHour := HA/30;     // Óraszög óra számlapon
  c := 'NWSE';
  end;
  end;
  if HAHour>12.0 Then HAHour:=HAHour-12.0;
  if HAHour<0 then HAHour:=12+HAHour;

  HAHourStr := ARToStr_( HAHour );
  r := 5;
  With PaintBox1.Canvas do begin
    BeginScene();
       // Óraszög felirat
       Stroke.Thickness := 1.0;
       Fill.Color := TAlphaColors.Red;
       Font.Family:='Arial';
       Font.Style:=[TFontStyle.fsbold];
       Font.Size:=20;
       FillText(RectF(0,0,100,20), ' '+HAHourStr, false, 100, [], TTextAlign.taLeading);

       // Polaris vector and star draw
       SetStroke( PaintBox1.Canvas, TBrushKind.Solid, TAlphaColors.Yellow, 2 );
       DrawLine( PointF(Origo.X,Origo.Y), PointF(Pol.X,Pol.Y),1,Stroke);
       Fill.Color := TAlphaColors.White;
       FillEllipse(RectF(Pol.X-r,Pol.Y-r,Pol.X+r,Pol.Y+r),1);

   // Feliratok
    Stroke.Thickness := 1.0;
    Fill.Color := TAlphaColors.Red;
    Font.Family:='Arial';
    Font.Style:=[TFontStyle.fsbold];
    Font.Size:=20;
//    FillText(RectF(Origo.X+4,Origo.y-fokRadius-40,Origo.X+40,fokRadius), C[1], false, 100, [], TTextAlign.taLeading);
(*
       TextOut(Origo.X+4,4,C[3]);
       TextOut(Origo.X+4,PoleImage.Height-16,C[1]);
       TextOut(4,Origo.Y-16,C[4]);
       TextOut(PoleImage.Width-12,Origo.Y-16,C[2]);*)
    EndScene();
  end;
end;


end.
