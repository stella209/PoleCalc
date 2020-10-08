
{ Csillagászati segédfüggvények }
unit STAF_AstroUnit;

interface

Uses
  SysUtils, Classes,
  Szoveg, Szamok, Math, DateUtils, AASharpDelphi;

Type

  TRaRect = record
    h : integer;  {óra}
    m : integer;  {perc}
    s : real;     {ivmásodperc}
  end;

  TDeRect = record
    Pozitiv : boolean;
    f : integer;  {fok}
    m : integer;  {ívperc}
    s : real;     {ivmásodperc}
  end;

  TGeoKoord = record
    lambda: real;  {hosszúság}
    fi    : real;  {szélesség}
  end;

  THorzKoord = record
    ALT: real;  {hosszúság}
    AZ : real;  {szélesség}
  end;

  TEQKoord = record
    RA : double;  {hosszúság fok}
    DE : double;  {szélesség fok}
  end;


  // Base datas of CCD optical instrument for astrophoto
  TOpticRecord = packed record
    oOwner       : string;   // Name of Owner
    oInstrument  : string;   // Name of instrument
    oFocus       : integer;      // Optical focus of objectiv in mm
    oCCDx        : double;       // CCD x diameter in mm
    oCCDy        : double;       // CCD y diameter in mm
    oCCDPixelSize: double;       // CCD Pixels diameter in mm
    PixelPerSec  : double;       // 1 pixel = arcsec
    SecPerPixel  : double;       // 1" = pixel
  end;

var year,month,day : word;
    UTDiff : integer;     {eltérés a vilkágidőtől (BP-en = +1 óra)}

Const

   {1 Csillagnap = 23h 56m 4,09054s középidőben}
   csillagnapora  : real = 23+56/60+4.09054/3600;
   csillagnapperc : real = 23*60+56+4.09054/60;
   csillagnapmp   : real = 23*3600+56*60+4.09054;

   kozepnapora    : byte = 24;
   kozepnapperc   : real = 24*60;
   kozepnapmp     : real = 24*3600;


{Az h,m,s formátumú Ra értéket fokokká alakítja (1h=15fok)}
Function RaToReal(h,m:integer;s:real):real;
{A fokokban megadott Ra-t h,m,s formára alakítja}
Function RealToRa(ra:real):TRarect;
Function DeToReal(f,m:integer;s:real):real;
{A fokokban megadott De-t fok,m,s formára alakítja}
Function RealToDe(de:real):TDerect;
{Az fok,m,s formátumú De értéket fokokká alakítja}
Function Ratostr(ra:TRarect):string; overload;
Function Ratostr(ra:double):string; overload;
Function Detostr(de:TDerect):string; overload;
Function Detostr(de:double):string; overload;
function HoursToRadians( ra: double ): double;
function RadiansToHours( ra: double ): double;

Function sgn(x:Double):integer ;
Function ARToStr_(ar: Double) : string;
Function DEToStr_(de: Double) : string;
Function StrToAr_De(ar: string) : Double;
function CielCoordToCoord(CielCoord: string): string;

//function TimeStrToTime(tStr: string): TDateTime;

{Ra rekordból idő}
Function RatoDateTime(ra:TRarect):TDateTime;

function PixToMm(focus,d: double): double;
function PixToSec(focus,d: double): double;
function PixelPerAngle(focus,PixelSizeX: double): double;

procedure SaveOpticList(iniFile: String);
procedure LoadOpticList(iniFile: String);

{A megfigyelőhely csillagidejének meghatározása:
   In: kido = kozépidő [óra],
       fi   = földrajzi hosszúság}
Function KozepidoToCsillagido(kido:TDatetime;fi:real):TRarect;
function GetSiderealTime( lon : double ) : double; overload;
function GetSiderealTime( ido :TDatetime; lon : double ) : double; overload;
Function SideralTime(kido:TDatetime; UT_Diff: integer; fi:real):TRarect;
function mean_sidereal_time( ido :TDatetime; lon : double ) : double;
Function Szokoev(ev:integer):boolean;

Function UT: TDateTime;
function UTCTimeNow: TDateTime;
function CurrentLocalBias: TDateTime;

function AngularSeparation(ra1,dec1,ra2,dec2: double): double;

function GetLHA(lst, RA: real): real;
function EqToHz(LST, LAT, RA, DE : double): THorzKoord;
function HzToEq(LST, LAT, ALT, AZ : double): TEQKoord;

// New
function DMSToDegrees( Degrees, Minutes, Seconds : double; bPositive : boolean ): double;
function MapTo0To24Range( HourAngle : double ): double;

function Separation( Alpha1, Delta1, Alpha2, Delta2: double): double;
function PositionAngle( Alpha1, Delta1, Alpha2, Delta2: double): double;
FUNCTION Precession( Alpha, Delta, JD0, JD: double): TEQKoord;
function AdjustPositionUsingUniformProperMotion( t, Alpha, Delta: double; PMAlpha, PMDelta: double ): TEQKoord;
function RealStarPosition(  t, Alpha, Delta: double; PMAlpha, PMDelta: double ): TEQKoord;

implementation

Function sgn(x:Double):integer ;
begin
if x<0 then
   sgn:= -1
else
   sgn:=  1 ;
end ;


// Az óra formátumú Ra-ból -hh:mm:ss alakú stringet képez
// pl. Ra = 6.4 => 6:24:00
Function ARToStr_(ar: Double) : string;
var dd,min1,min,sec: Double;
begin
    dd  := Trunc(ar);
    min1:= Frac(ar)*60;
    if min1>=59.999 then begin
       dd:=dd+sgn(ar);
       min1:=0.0;
    end;
    min := Trunc(Abs(min1));
    sec:= Abs(min1-min)*60;
    if sec>=59.95 then begin
       min:=min+1;
       sec:=0.0;
    end;
    Result := Format('%.2d',[Trunc(dd)])+':'+Format('%.2d',[Trunc(min)])
           +':'+Format('%.2d',[Round(sec)]);
end;

// Az fok formátumú De-ból -ff:mm:ss alakú stringet képez
// pl. De = -6.4 => -6:24:00
Function DEToStr_(de: Double) : string;
var dd,min1,min,sec: Double;
begin
    dd  := Trunc(de);
    sec := Abs(Frac(de)*3600);
    min := Trunc(sec/60);
    sec := (sec - 60*min);
    Result := Format('%.2d',[Trunc(dd)])+':'+Format('%.2d',[Trunc(min)])
           +':'+Format('%.2d',[Trunc(sec)]);
end;

// A -hh:mm:ss alakú stringből real értéket számít
Function StrToAr_De(ar: string) : Double;
var h,m: integer;
    s: double;
    SgnValt : integer;     // Előjel jelzése
    arStr,e: string;
begin
  Result := 0;
  arStr  := Trim(ar);
  SgnValt:=1;
  If Pos('-',arStr)=1 then begin
     SgnValt:= -1;
     arStr  := Copy(arStr,2,1000);
  end;
  If Pos('+',arStr)=1 then begin
     SgnValt:= 1;
     arStr  := Copy(arStr,2,1000);
  end;
  e:=StrCountD(arStr,':',1);
  if e<>'' then h:=StrToInt(e);
  e:=StrCountD(arStr,':',2);
  if e<>'' then m:=StrToInt(e);
  e:=StrCountD(arStr,':',3);
  if e<>'' then s:=StrToFloat(e);
     Result := h+m/60+s/3600;
end;

// RA: 18h28m41.83s DEC:-13°28'21.8" FOV:+80°00'00" to 18:28:41.83 -13:28:21.8
function CielCoordToCoord(CielCoord: string): string;
var h,m,s1,f,p,s2    : string;
    x,mp       : integer;
begin
    FormatSettings.Decimalseparator := '.';
    h := Copy(CielCoord,5,2);
    m := Copy(CielCoord,8,2);
    s1:= Copy(CielCoord,11,5); s1[3]:='.';
    f := Copy(CielCoord,22,3);
    p := Copy(CielCoord,26,2);
    s2:= Copy(CielCoord,29,4); s2[3]:='.';
    x := Round(StrToFloat(s1));
    mp:= Round(StrToFloat(s2));
    s1:= Format('%.2d',[Trunc(x)]);
    s2:= Format('%.2d',[Trunc(mp)]);
    Result := h+':'+m+':'+s1+' '+f+':'+p+':'+s2;
end;


Function RaToReal(h,m:integer;s:real):real;
begin
  Result:=15*(h+m/60+s/3600);
end;

Function RealToRa(ra:real):TRarect;
begin
  With Result do begin
    h := Trunc(Ra/15);
    m := Abs(Trunc(60*(ra-15*h)/15));
    s := Abs(60*Frac(60*(ra-15*h)/15));
  end;
end;

Function DeToReal(f,m:integer;s:real):real;
begin
  If f>=0 then Result:=f+m/60+s/3600
  else Result:=f-m/60-s/3600;
end;

Function RealToDe(de:real):TDerect;
begin
  With Result do begin
    Pozitiv := de>0;
    f := Trunc(de);
    m := Abs(Trunc(60*Frac(de)));
    s := Abs(60*Frac(60*Frac(de)));
  end;
end;

Function Ratostr(ra:TRarect):string;
begin
  Result := Inttostr(ra.h)+'h '+Inttostr(ra.m)+'m '+Format('%4.2f',[ra.s])+'s';
end;

Function Ratostr(ra:double):string;
var ra_: TRarect;
begin
  ra_ := RealToRa(ra);
  Result := Inttostr(ra_.h)+'h '+Inttostr(ra_.m)+'m '+Format('%4.2f',[ra_.s])+'s';
end;

Function Detostr(de:TDerect):string;
begin
  Result := Inttostr(de.f)+'* '+Inttostr(de.m)+''' '+Format('%4.1f',[de.s])+'"';
end;

Function Detostr(de:double):string;
var de_: TDerect;
begin
  de_ := RealToDe(de);
  Result := Inttostr(de_.f)+'* '+Inttostr(de_.m)+''' '+Format('%4.1f',[de_.s])+'"';
end;

{Ra rekordból idő}
Function RatoDateTime(ra:TRarect):TDateTime;
begin
  Result := EncodeTime(ra.h,ra.m,Trunc(ra.s),0);
end;

// Órákban adott Ra konverzió radiánba
function HoursToRadians( ra: double ): double;
begin
  Result := DegToRad(15 * ra);
end;

// Órákban adott Ra konverzió radiánba
function RadiansToHours( ra: double ): double;
begin
  Result := RadToDeg(ra)/15;
end;

Function KozepidoToCsillagido(kido:TDatetime;fi:real):TRarect;
var dat1,dat2: TDateTime;
    elteltnap : real;
    alapido,maradek,d : real;
begin
  {Az adott középidőre meghatározzuk a Grewnichi helyi időt}
  {1986.09.22-én 0h világidőkor a csillagidő = 0h 02m 14.8s volt,
  1. meg kell határozni az azóta eltelt napok számát,
  2. az így kapott középnapok számát át kell számítani csillagnapba,
     a maradék rész adja a csillagidőt}
  UTDiff := Trunc(fi/15);
  dat1 := StrToDatetime('1986.09.22 00:00:00');
  dat2 := kido-UTDiff/24;
  elteltnap := dat2 - dat1;
  maradek := Frac(elteltnap*(24.00000/csillagnapora));//+UTDiff/24);
  alapido:=Ratoreal(0,2,14.8);
  d:=15*24*maradek;
  Result := RealToRa(alapido+d);
end;

Function SideralTime(kido:TDatetime; UT_Diff: integer; fi:real):TRarect;
var dat1,dat2: TDateTime;
    elteltnap : real;
    alapido,maradek,d : real;
begin
  {Az adott középidőre meghatározzuk a Grewnichi helyi időt}
  {1986.09.22-én 0h világidőkor a csillagidő = 0h 02m 14.8s volt,
  1. meg kell határozni az azóta eltelt napok számát,
  2. az így kapott középnapok számát át kell számítani csillagnapba,
     a maradék rész adja a csillagidőt}
  UTDiff := Trunc(fi/15);
  dat1 := StrToDatetime('1986.09.22 00:00:00');
  dat2 := kido-UT_Diff/24;
  elteltnap := dat2 - dat1;
  alapido   := Ratoreal(0,2,14.8);
  maradek   := Frac((csillagnapora/24.00000)*(elteltnap+UT_Diff/24));
  d := 15 * 24 * maradek + (fi-15*UT_Diff);
  Result := RealToRa(alapido+d);
end;

//
// display univeral time or mean sidereal time using system clock
// Use long := 0 to get the Greenwich MST.
// East longitudes are positive
// West longitudes are negative
// star = false for universal time, true for sidereal time
//
(*
procedure put_datetime( star : boolean, long : real );
var hour, minute, second : integer;
    mst, ras : double;
    yy, mm, dd, h, m, s : integer;
    tm : _SYSTEMTIME;
begin

    % get UTC from MS Windows operating system
    GetSystemTime(@tm);
    yy := dateyear( tm )
    mm := datemonth( tm )
    dd := dateday( tm )
    h  := timehour( tm )
    m  := timeminute( tm )
    s  := timesecond( tm )
    if not star then
        put d2(mm), '/', d2(dd), '/', yy, " "...
        put d2(h),  ':', d2(m),  ':', d2(s)...
        return
    end if

    % compute MST and RA
    mst := mean_sidereal_time( tm, long )           % in degrees
    ras := mst                                      % right ascension
    mst := mst / 15.0                               % time units

    % right ascension of vernal equinox
    var deg, min, sec : int
    deg := floor( ras )

    ras := ras - deg
    ras := ras * 60
    min := floor( ras )

    ras := ras - min
    ras := ras * 60
    sec := floor( ras )

    % sidereal time
    hour   := floor( mst )
        
    mst    := mst - hour
    mst    := mst * 60
    minute := floor( mst )

    mst    := mst - minute
    mst    := mst * 60
    second := floor( mst )

    % output right ascension and sidereal time
    put deg:3, '°', d2(min), '\'', d2(sec), "\" "...
    put d2(hour), 'h', d2(minute), 'm', d2(second), 's'...
end;
*)

(*
 "mean_sidereal_time" returns the Mean Sidereal Time in units of degrees.
 Use lon := 0 to get the Greenwich MST.
 East longitudes are positive
 West longitudes are negative

 returns: hour in double (pl. 6:30 = 6.5)
*)
function mean_sidereal_time( ido :TDatetime; lon : double ) : double;
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

    if (month = 1) or (month = 2) then begin
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

    // Julian centuries since J2000.0
    jt := jd/36525.0;

    // mean sidereal time
    mst := 280.46061837 + 360.98564736629*jd
         + 0.000387933*jt*jt - jt*jt*jt/38710000 + lon;

    if (mst > 0.0) then
        while (mst > 360.0) do
            mst := mst - 360.0
    else
        while (mst < 0.0) do
            mst := mst + 360.0;

    Result := mst/15;

end;

function CurrentLocalBias: TDateTime;
const
  MinsInDay = 1440;
var
  TZInfo: TTimeZoneInformation;
begin
  //Get the between UTC and time for this locale and convert
  //to TDateTime by dividing by MinsInDay
  //NB: If local time is ahead of UTC, bias is negative
  case GetTimeZoneInformation(TZInfo) of
    TIME_ZONE_ID_DAYLIGHT:
      Result := (TZInfo.Bias + TZInfo.DaylightBias) / MinsInDay;
    TIME_ZONE_ID_STANDARD:
      Result := (TZInfo.Bias + TZInfo.StandardBias) / MinsInDay;
    TIME_ZONE_ID_UNKNOWN:
      Result := TZInfo.Bias / MinsInDay;
  else
   Result := TZInfo.Bias / MinsInDay;
  end;
end;

(* Get Local Sideral Time at any Zone time in double *)
// nem komplett - fejleszteni!
function GetSiderealTime( ido :TDatetime; lon : double ) : double;
Var tmSys : SYSTEMTIME;
    UT: TDateTime;
    GMT: double;
    LST: double;
begin
    GetSystemTime(tmSys);

    UT := EncodeDateTime(tmSys.wYear,tmSys.wMonth,tmSys.wDay,
          tmSys.wHour,tmSys.wMinute,tmSys.wSecond,tmSys.wMilliseconds)
          + CurrentLocalBias;
    GMT := mean_sidereal_time(UT,0);
    LST := GMT+LON/15;
    if LST>=24 then LST := Frac(LST);
    Result := LST;
end;

(* Get Local Sideral Time from local/system time in double *)
function GetSiderealTime( lon : double ) : double;
Var tmSys : SYSTEMTIME;
    UT: TDateTime;
    GMT: double;
    LST: double;
begin
    // get UTC from MS Windows operating system
    GetSystemTime(tmSys);
    UT := EncodeDateTime(tmSys.wYear,tmSys.wMonth,tmSys.wDay,
          tmSys.wHour,tmSys.wMinute,tmSys.wSecond,tmSys.wMilliseconds);
    GMT := mean_sidereal_time(UT,0);
    LST := GMT+LON/15;
    if LST>=24 then LST := Frac(LST);
    Result := LST;
end;

function UTCTimeNow: TDateTime;
begin
  //UTC time = local time + Bias + Standard/Daylight bias
  Result := Now + CurrentLocalBias;
end;


(*
% synchronize to one second intervals
procedure synchronize

    var now : real
    repeat
        now := timemsecs( systemtime )
    until now < 50

end procedure

// format two digits with leading zero if needed
function d2( n : int ) : string;
begin
    assert (0 <= n) and (n < 100)
    if n < 10 then
        return "0" & intstr( n, 1 )
    else
        return intstr( n, 2 );
end;
*)

Function Szokoev(ev:integer):boolean;
begin
 Result := (ev mod 4)=0;
end;

// A sz.gép órája alapján számolja az UT-t
Function UT: TDateTime;
//Var tmSys : SYSTEMTIME;
begin
    Result := UTCTimeNow;
(*
    // get UTC from MS Windows operating system
    GetSystemTime(tmSys);
    Result := EncodeDateTime(tmSys.wYear,tmSys.wMonth,tmSys.wDay,
                   tmSys.wHour,tmSys.wMinute,tmSys.wSecond,tmSys.wMilliseconds);*)
end;

function GetPixelPerMM( CCDd: double; Focus: double ): double;
// A CCD chip méretei (CCDd mm-ben) alapján meghatározza egy
// pixel méretét mm-ben
begin
end;

function PixToMm(focus,d: double): double;
// Az optika fókusza(mm) alapján kiszámítja a távolságot mm-ben
// d=távolság a felvételenpixelben;
begin
end;

function PixToSec(focus,d: double): double;
// Az optika fókusza alapján kiszámítja a távolságot ''(ívmásodpercben)
// d=távolság a felvételen pixelben;
begin
  Result := 3600*RadToDeg(ArcTan2(d,focus));
end;

function PixelPerAngle(focus,PixelSizeX: double): double;
// A felvétel 1 pixelére leképzett égbolt részlet szögmérete ''-ben
begin
Try
  Result := 0;
  if PixelSizeX<>0 then
  Result := 3600*RadToDeg(ArcTan2(PixelSizeX,focus));
except
  Result := 0;
end;
end;

procedure SaveOpticList(iniFile: String);
//Save optic/CCD list to INI file
begin
end;

procedure LoadOpticList(iniFile: String);
//Load optic/CCD list from INI file
begin
end;

// Calculate difference between to EQ coordinates
// IN: coordinates in degrees
// OUT: difference in degrees
function AngularSeparation(ra1,dec1,ra2,dec2: double): double;
var a1,d1,a2,d2,x,y,z,d: double;
begin
	a1 := DegToRad(ra1);
	d1 := DegToRad(dec1);
	a2 := DegToRad(ra2);
	d2 := DegToRad(dec2);

	x := (cos(d1) * sin(d2)) - (sin(d1) * cos(d2) * cos(a2-a1));
	y := cos(d2) * sin(a2-a1);
	z := (sin (d1) * sin (d2)) + (cos(d1) * cos(d2) * cos(a2-a1));

	x := x * x;
	y := y * y;
	d := arctan2(sqrt(x+y),z);

	result := RadToDeg(d)
end;

// --------------------------------------------------------------------------------
// ÓRASZÖG KISZÁMÍTÁSA:
// --------------------------------------------------------------------------------
// lst: csillagidő fokokban; RA: csillag RA-ja fokokban
// Ha lst az 1. csillag, RA pedig a 2. csillag ra-ja fokokban, akkor a két csillag
//    ra külömbségét kapjuk NY-ra +, K-re - értékkel;
// lha: óraszög (fok). Ny-i irány pozítív.
// --------------------------------------------------------------------------------
function GetLHA(lst, RA: real): real;
Var lha, diff: real;
begin
  lha := lst - RA;
  diff := lha;
  if (abs(diff)>180) then
    if (diff<-180) then lha := 360 + diff
    else lha := 360 - diff;
  result := lha;
end;

(*
* HORIZONTÁLIS KOORDINÁTA - EKVATORIÁLISBÓL
* LST    : Hehyi csillagidő (órában pl. 6.5 = 6h30m)
* LAT    : Földrajzi szélesség (fok)
* RA,DE  : Ekvatorial koordináták (fok RA = 15*RAh)
* Result : THorzKoord(ALT,AZ) fokban (fok)
*)
function EqToHz(LST, LAT, RA, DE : double): THorzKoord;
var
    HA: double;       // Óraszög
    ALT: double;      // Horizont magasság
    sinALT: double;   // Horizont magasság szinusza
    cosAZ : double;   // Azimut coszinusza
begin
(*
  sin(ALT) = sin(DEC)·sin(LAT) + cos(DEC)·cos(LAT)·cos(HA)

             sin(DEC) - sin(ALT)·sin(LAT)
  cos(A)   = ----------------------------
             cos(ALT)·cos(LAT)
*)
  HA  := DegToRad( 15 * LST - Ra );
  LAT := DegToRad(LAT);
  DE  := DegToRad(DE);
  sinALT     := sin(DE)*sin(LAT) + cos(DE)*cos(LAT)*cos(HA);
  ALT        := arcsin(sinALT);
  cosAZ      := (sin(DE)-sinALT*sin(LAT)) / ( cos(LAT)*cos(ALT));
  Result.ALT := RadToDeg( ALT );
  Result.AZ  := RadToDeg( arccos(cosAZ) );
  if sin(HA)>0 then
     Result.AZ := 360 - Result.AZ;
end;

(*
* HORIZONTÁLIS KOORDINÁTA - EKVATORIÁLISBÓL
* LST    : Hehyi csillagidő (órában pl. 6.5 = 6h30m)
* LAT    : Földrajzi szélesség (fok)
* ALT    : Horizont magasság (fok)
* AZ     : Azimut (É-tól K-i irányban 0..360 fok
* Result : TEQKoord(RA,DE) (RA:fok; DE:fok)
*)
function HzToEq(LST, LAT, ALT, AZ : double): TEQKoord;
begin
  LAT := DegToRad(LAT);
  ALT := DegToRad(ALT);
  AZ  := DegToRad(AZ);
  Result.Ra := RadToDeg(ArcTan2(sin(AZ), -cos(AZ)*sin(LAT) + tan(ALT)*cos(LAT)));
  Result.De := RadToDeg(ArcSin(sin(LAT)*sin(ALT) + cos(LAT)*cos(ALT)*cos(AZ)));
  Result.Ra := Result.Ra+(15.0 * LST);
  if (Result.Ra > 360) then
    Result.Ra := Result.Ra - 360;
  if (Result.Ra < 0) then
    Result.Ra := Result.Ra + 360;
end;

// =============================================================================

function DMSToDegrees( Degrees, Minutes, Seconds : double; bPositive : boolean ): double;
begin
     if bPositive then
        result := Degrees + Minutes / 60 + Seconds / 3600
     else
        result := -Degrees - Minutes / 60 - Seconds / 3600;
end;

function MapTo0To24Range( HourAngle : double ): double;
begin
     Result := Frac(HourAngle/24)*24;
     if (Result < 0) then
        Result := Result+24
end;

// Két objektum szögtávolsága ( Angle difference between two objects )
function Separation( Alpha1, Delta1, Alpha2, Delta2: double): double;
var x,y,z : double;
    value : double;
begin
            Delta1 := DegToRad(Delta1);
            Delta2 := DegToRad(Delta2);

            Alpha1 := HoursToRadians(Alpha1);
            Alpha2 := HoursToRadians(Alpha2);

            x := Cos(Delta1) * Sin(Delta2) - Sin(Delta1) * Cos(Delta2) * Cos(Alpha2 - Alpha1);
            y := Cos(Delta2) * Sin(Alpha2 - Alpha1);
            z := Sin(Delta1) * Sin(Delta2) + Cos(Delta1) * Cos(Delta2) * Cos(Alpha2 - Alpha1);

            value := Math.Arctan2(Sqrt(x * x + y * y), z);
            value := RadToDeg(value);
            if (value < 0) then
                value := value + 180;

            result := value;
end;

// Két objektum közötti vektor dőlésszöge a De körökhöz
function PositionAngle( Alpha1, Delta1, Alpha2, Delta2: double): double;
var DeltaAlpha, value: double;
begin
            Delta1 := DegToRad(Delta1);
            Delta2 := DegToRad(Delta2);

            Alpha1 := HoursToRadians(Alpha1);
            Alpha2 := HoursToRadians(Alpha2);

            DeltaAlpha := Alpha1 - Alpha2;
            value := Math.Arctan2(Sin(DeltaAlpha), Cos(Delta2)
                     * Math.Tan(Delta1) - Sin(Delta2) * Cos(DeltaAlpha));
            value := RadToDeg(value);
            if (value < 0) then
                value := value + 180;

            result := value;
end;


FUNCTION Precession( Alpha, Delta, JD0, JD: double): TEQKoord;
var t,tsquared,tcubed  : double;
    sigma,zeta,phi,A,B,C : double;
    DeltaAlpha           : double;
begin
(*
            T := (JD0 - 2415020.3135) / 36524.2199;
            t := (JD - JD0) / 36524.2199;
            tsquared := t * t;
            tcubed := tsquared * t;

            //Now convert to radians
            Alpha := HoursToRadians(Alpha);
            Delta := DegToRad(Delta);

            sigma := DegToRad(DMSToDegrees(0, 0, (2304.250 + 1.396 * T) * t + 0.302 * tsquared + 0.018 * tcubed));
            zeta  := DegToRad(DMSToDegrees(0, 0, 0.791 * tsquared + 0.001 * tcubed)) + sigma;
            phi   := DegToRad(DMSToDegrees(0, 0, (2004.682 - 0.853 * T) * t - 0.426 * tsquared - 0.042 * tcubed));
            A := Cos(Delta) * Sin(Alpha + sigma);
            B := Cos(phi) * Cos(Delta) * Cos(Alpha + sigma) - Sin(phi) * Sin(Delta);
            C := Sin(phi) * Cos(Delta) * Cos(Alpha + sigma) + Cos(phi) * Sin(Delta);

            DeltaAlpha := DMSToDegrees(0, 0, 0.0775 + 0.0850 * T);
            Result.RA := MapTo0To24Range(RadiansToHours(Math.Arctan2(A, B) + zeta) + DeltaAlpha);
            Result.DE := RadToDeg(Math.Arcsin(C));
*)
end;


// Calculate the eq coord. with proper mottion
//   t: year; alpha,delta: degree; PM...: "
function AdjustPositionUsingUniformProperMotion( t, Alpha, Delta: double; PMAlpha, PMDelta: double ): TEQKoord;
begin
     Result.RA := Alpha + (PMAlpha * t / 3600);
     Result.DE := Delta + (PMDelta * t / 3600);
end;

(*  REAL STAR POSITION corrected by proper motion and precession (J2000 EPOCH)
         t                : JD Julian date
         Alpha, Delta     : Star position at J2000 epoch in degree
         PMAlpha, PMDelta : Proper motion / year in "
         Resul RA,DE      : New star position in degree (Ra in hour => Result.Ra/15)
*)
function RealStarPosition(  t, Alpha, Delta: double; PMAlpha, PMDelta: double ): TEQKoord;
var AC,AC1 : TAAS2DCoordinate;
    dt     : double;
begin
  dt := (t-2451545.0)/365.242189669781;  // year
  AC :=  AASPrecession.AdjustPositionUsingUniformProperMotion( dt, Alpha, Delta, PMAlpha, PMDelta);
  AC1 := AASPrecession.PrecessEquatorial(ac.x/15,ac.y,2451545.0,t);
  Result.RA := 15 * AC1.x;
  Result.DE := AC1.y;
end;

initialization
  FormatSettings.DecimalSeparator:='.';
  FormatSettings.ShortDateFormat:='yy.mm.dd';
  FormatSettings.LongDateFormat:='yyyy.mm.dd';
  FormatSettings.TimeSeparator:=':';
end.

