unit AASharpDelphi;

interface

Uses SysUtils, Math;

Type

    TAAS2DCoordinate = record
      X: double;
      Y: double;
    end;

    TAAS3DCoordinate = record
      X: double;
      Y: double;
      Z: double;
    end;

    TAASBinaryStarDetails = record
      r, Rho, Theta: double;
    end;

    TAASCoordinateTransformation = record
    public
        function RaStrToCoord( coordStr : string ): double;
        function RaCoordToStr( coord : double ): string;
        function DeStrToCoord( coordStr : string ): double;
        function DeCoordToStr( coord : double ): string;
        function DegreesToRadians( Degrees: double): double;
        function RadiansToDegrees( Radians: double): double;
        function RadiansToHours( Radians: double ): double;
        function HoursToRadians( Hours: double ): double;
        function HoursToDegrees( Hours: double ): double;
        function DegreesToHours( Degrees: double ): double;
        function MapTo0To360Range( Degrees: double ): double;
        function MapToMinus90To90Range( Degrees: double ): double;
        function MapTo0To24Range( HourAngle: double ): double;
        function MapTo0To2PIRange( Angle: double ): double;
        function DMSToDegrees( Degrees, Minutes, Seconds: double; bPositive: boolean = true): double;
        function Equatorial2Ecliptic( Alpha, Delta, Epsilon : double): TAAS2DCoordinate;
        function Ecliptic2Equatorial( Lambda, Beta, Epsilon : double): TAAS2DCoordinate;
        function Equatorial2Horizontal( LocalHourAngle, Delta, Latitude : double): TAAS2DCoordinate;
        function Horizontal2Equatorial( Azimuth, Altitude, Latitude: double ): TAAS2DCoordinate;
        function Equatorial2Galactic( Alpha, Delta: double): TAAS2DCoordinate;
        function Galactic2Equatorial( l, b: double): TAAS2DCoordinate;
    end;

(*
    AASDate = record
        Day,
        Month,
        Year,
        Hour,
        Minute  : integer;
        Second  : double;
        _mDblJulian; : double;
        _mBGregorianCalendar: bool;
        _mDblJulian : double;

        //public AASDate(long year, long month, double day, bool bGregorianCalendar)
        //public AASDate(long year, long month, double day, double hour, double minute, double second, bool isGregorianCalendar)
        //public AASDate(double JD, bool isGregorianCalendar)
        function DateToJD(long year, long month, double day, bool isGregorianCalendar): double;
        public static bool IsLeap(long year, bool isGregorianCalendar)
        public static void DayOfYearToDayAndMonth(long dayOfYear, bool isLeap, ref long dayOfMonth, ref long Month)
        public static AASCalendarDate JulianToGregorian(long year, long month, long day)
        public static AASCalendarDate GregorianToJulian(long year, long month, long day)
        public static long INT(double value)
        public static bool AfterPapalReform(long year, long month, double day)
        public static bool AfterPapalReform(double JD)
        public static double DayOfYear(double JD, long year, bool isGregorianCalendar)
        public static long DaysInMonth(long month, bool isLeap)
        public DAY_OF_WEEK DayOfWeek => (DAY_OF_WEEK)((long)(_mDblJulian + 1.5) % 7);

        public double DayOfYear()
        public long DaysInMonth()
        public long DaysInYear()
        public bool Leap => IsLeap(Year, _mBGregorianCalendar);

        public bool InGregorianCalendar => _mBGregorianCalendar;

        public double FractionalYear
        public void Set(long year, long month, double day, double hour, double minute, double second, bool isGregorianCalendar)
        public void Set(long year, long month, double day, double hour, double minute, double second, bool isGregorianCalendar)
        public void SetInGregorianCalendar(bool isGregorianCalendar)
    end;
*)
  TAASKepler = record
        function Calculate( M, e: double; nIterations: integer = 53): double;
  end;

  TAASAngularSeparation = record
        function Separation( Alpha1, Delta1, Alpha2, Delta2: double): double;
        function PositionAngle( Alpha1, Delta1, Alpha2, Delta2: double): double;
        function DistanceFromGreatArc( Alpha1, Delta1, Alpha2, Delta2, Alpha3, Delta3: double): double;
        function SmallestCircle( Alpha1, Delta1, Alpha2, Delta2, Alpha3, Delta3: double; bType1: boolean): double;
  end;

  TAASBinaryStar = record
        function Calculate( tt, P, T, e, a, i, omega, w: double): TAASBinaryStarDetails;
        function ApparentEccentricity( e, i, w: double): double;
  end;

  TAASPrecession = record
        function AdjustPositionUsingUniformProperMotion( t, Alpha, Delta, PMAlpha, PMDelta: double): TAAS2DCoordinate;
        function AdjustPositionUsingMotionInSpace( r, DeltaR, t, Alpha, Delta, PMAlpha, PMDelta:double): TAAS2DCoordinate;
        function PrecessEquatorial( Alpha, Delta, JD0, JD: double) : TAAS2DCoordinate;
        function PrecessEquatorialFK4( Alpha, Delta, JD0, JD: double): TAAS2DCoordinate;
        function PrecessEcliptic( Lambda, Beta, JD0, JD: double): TAAS2DCoordinate;
        function EquatorialPMToEcliptic( Alpha, Delta, Beta, PMAlpha, PMDelta, Epsilon: double): TAAS2DCoordinate;
  end;


Var
    AASCoordinateTransformation : TAASCoordinateTransformation;
    AASAngularSeparation        : TAASAngularSeparation;
    AASKepler                   : TAASKepler;
    AASPrecession               : TAASPrecession;

    function IEEERemainder( x, y: double ): double;

implementation
    // https://docs.microsoft.com/en-us/dotnet/api/system.ieeeremainder?redirectedfrom=MSDN&view=netframework-4.8#System_Math_IEEERemainder_System_Double_System_Double_
    // IEEERemainder = dividend - (divisor * Round(dividend / divisor))
    function IEEERemainder( x, y: double ): double;
    var regularMod, alternativeResult, divisionResult, roundedResult : double;
    begin
         Result := x - (y * Round(x / y));
    end;

{ AASCoordinateTransformation =================================================}

function TAASCoordinateTransformation.RaStrToCoord(coordStr: string): double;

begin
end;

function TAASCoordinateTransformation.RaCoordToStr(coord: double): string;
begin
//  Result :=
end;

function TAASCoordinateTransformation.DeStrToCoord(coordStr: string): double;

begin
end;

function TAASCoordinateTransformation.DeCoordToStr(coord: double): string;
begin
//  Result :=
end;

function TAASCoordinateTransformation.DegreesToHours(Degrees: double): double;
begin
            result := Degrees / 15;
end;

function TAASCoordinateTransformation.DegreesToRadians(Degrees: double): double;
begin
            result := Degrees * 0.017453292519943295769236907684886;
end;

function TAASCoordinateTransformation.DMSToDegrees(Degrees, Minutes,
  Seconds: double; bPositive: boolean): double;
  begin
  (*          //validate our parameters
            if (not bPositive) then
            {
                if (Degrees < 0)
                    throw new ArgumentOutOfRangeException("Degrees", "Degrees must be greater than 0");
                if (Minutes < 0)
                    throw new ArgumentOutOfRangeException("Minutes", "Minutes must be greater than 0");
                if (Seconds < 0)
                    throw new ArgumentOutOfRangeException("Seconds", "Seconds must be greater than 0");
            }
    *)
            if (bPositive) then
                result := Degrees + Minutes / 60 + Seconds / 3600
            else
                result := -Degrees - Minutes / 60 - Seconds / 3600;
  end;

function TAASCoordinateTransformation.Ecliptic2Equatorial(Lambda, Beta,
  Epsilon: double): TAAS2DCoordinate;
  begin
            Lambda  := DegreesToRadians(Lambda);
            Beta    := DegreesToRadians(Beta);
            Epsilon := DegreesToRadians(Epsilon);

            result.X := RadiansToHours(Arctan2(Sin(Lambda) * Cos(Epsilon) - Tan(Beta) * Sin(Epsilon), Cos(Lambda)));
            if (result.X < 0) then
                result.X := result.X + 24;
            result.Y := RadiansToDegrees(Arcsin(Sin(Beta) * Cos(Epsilon) + Cos(Beta) * Sin(Epsilon) * Sin(Lambda)));
  end;

function TAASCoordinateTransformation.Equatorial2Ecliptic( Alpha, Delta, Epsilon : double): TAAS2DCoordinate;
  begin
            Alpha   := HoursToRadians(Alpha);
            Delta   := DegreesToRadians(Delta);
            Epsilon := DegreesToRadians(Epsilon);

            result.X := RadiansToDegrees(Arctan2(Sin(Alpha) * Cos(Epsilon) + Tan(Delta) * Sin(Epsilon), Cos(Alpha)));
            if (result.X < 0) then
                result.X := result.X + 360;
            result.Y := RadiansToDegrees(Arcsin(Sin(Delta) * Cos(Epsilon) - Cos(Delta) * Sin(Epsilon) * Sin(Alpha)));
  end;

function TAASCoordinateTransformation.Equatorial2Galactic(Alpha,
  Delta: double): TAAS2DCoordinate;
begin
            Alpha := 192.25 - HoursToDegrees(Alpha);
            Alpha := DegreesToRadians(Alpha);
            Delta := DegreesToRadians(Delta);

            result.X := RadiansToDegrees(Arctan2(Sin(Alpha), Cos(Alpha) * Sin(DegreesToRadians(27.4)) - Tan(Delta) * Cos(DegreesToRadians(27.4)))) ;
            result.X := 303 - result.X;
            if (result.X >= 360) then
                result.X := result.X - 360;
            result.Y := RadiansToDegrees(Arcsin(Sin(Delta) * Sin(DegreesToRadians(27.4)) + Cos(Delta) * Cos(DegreesToRadians(27.4)) * Cos(Alpha)));
end;

function TAASCoordinateTransformation.Equatorial2Horizontal(LocalHourAngle,
         Delta, Latitude: double): TAAS2DCoordinate;
      begin
            LocalHourAngle := HoursToRadians(LocalHourAngle);
            Delta := DegreesToRadians(Delta);
            Latitude := DegreesToRadians(Latitude);

            result.X := RadiansToDegrees(Arctan2(Sin(LocalHourAngle), Cos(LocalHourAngle) * Sin(Latitude) - Tan(Delta) * Cos(Latitude)));
            if (result.X < 0) then
                result.X := result.X + 360;
            result.Y := RadiansToDegrees(Arcsin(Sin(Latitude) * Sin(Delta) + Cos(Latitude) * Cos(Delta) * Cos(LocalHourAngle)));
      end;

function TAASCoordinateTransformation.Galactic2Equatorial(l, b: double): TAAS2DCoordinate;
begin
            l := l - 123;
            l := DegreesToRadians(l);
            b := DegreesToRadians(b);

            result.X := RadiansToDegrees(Arctan2(Sin(l), Cos(l) * Sin(DegreesToRadians(27.4)) - Tan(b) * Cos(DegreesToRadians(27.4))));
            result.X := result.X + 12.25;
            if (result.X < 0) then
                result.X := result.X + 360;
            result.X := DegreesToHours(result.X);
            result.Y := RadiansToDegrees(Arcsin(Sin(b) * Sin(DegreesToRadians(27.4)) + Cos(b) * Cos(DegreesToRadians(27.4)) * Cos(l)));

end;

function TAASCoordinateTransformation.Horizontal2Equatorial(Azimuth, Altitude,
  Latitude: double): TAAS2DCoordinate;
begin
            //Convert from degress to radians
            Azimuth  := DegreesToRadians(Azimuth);
            Altitude := DegreesToRadians(Altitude);
            Latitude := DegreesToRadians(Latitude);

            result.X := RadiansToHours(Arctan2(Sin(Azimuth), Cos(Azimuth) * Sin(Latitude) + Tan(Altitude) * Cos(Latitude)));
            if (result.X < 0) then
                result.X := result.X + 24;
            result.Y := RadiansToDegrees(Arcsin(Sin(Latitude) * Sin(Altitude) - Cos(Latitude) * Cos(Altitude) * Cos(Azimuth)));
end;

function TAASCoordinateTransformation.HoursToDegrees(Hours: double): double;
begin
            result := Hours * 15;
end;

function TAASCoordinateTransformation.HoursToRadians(Hours: double): double;
begin
            result := Hours * 0.26179938779914943653855361527329;
end;

function TAASCoordinateTransformation.MapToMinus90To90Range(
  Degrees: double): double;
begin
            Result := MapTo0To360Range(Degrees);
            if (Result > 270) then
                Result := Result - 360
            else if (Result > 180) then
                Result := 180 - Result
            else if (Result > 90) then
                Result := 180 - Result;
end;

function TAASCoordinateTransformation.RadiansToDegrees(Radians: double): double;
begin
            result := Radians * 57.295779513082320876798154814105;
end;

function TAASCoordinateTransformation.RadiansToHours(Radians: double): double;
begin
            result := Radians * 3.8197186342054880584532103209403;
end;

function TAASCoordinateTransformation.MapTo0To24Range(HourAngle: double): double;
begin
            Result := IEEERemainder(HourAngle, 24);
            if (Result < 0) then
                Result := Result + 24;
end;

function TAASCoordinateTransformation.MapTo0To2PIRange(Angle: double): double;
begin
            result := IEEERemainder(Angle, 2 * PI());
            if (result < 0) then
                result := result + 2 * PI();
end;

function TAASCoordinateTransformation.MapTo0To360Range(Degrees: double): double;
begin
            result := IEEERemainder(Degrees, 360);
            if (result < 0) then
                result := result + 360;
end;

{ End of AASCoordinateTransformation =================================================}

{ AASAngularSeparation }

function TAASAngularSeparation.DistanceFromGreatArc(Alpha1, Delta1, Alpha2,
  Delta2, Alpha3, Delta3: double): double;
var x1,x2,y1,y2,z1,z2,A,B,C,m,n: double;
begin
            Delta1 := AASCoordinateTransformation.DegreesToRadians(Delta1);
            Delta2 := AASCoordinateTransformation.DegreesToRadians(Delta2);
            Delta3 := AASCoordinateTransformation.DegreesToRadians(Delta3);

            Alpha1 := AASCoordinateTransformation.HoursToRadians(Alpha1);
            Alpha2 := AASCoordinateTransformation.HoursToRadians(Alpha2);
            Alpha3 := AASCoordinateTransformation.HoursToRadians(Alpha3);

            X1 := Cos(Delta1) * Cos(Alpha1);
            X2 := Cos(Delta2) * Cos(Alpha2);

            Y1 := Cos(Delta1) * Sin(Alpha1);
            Y2 := Cos(Delta2) * Sin(Alpha2);

            Z1 := Sin(Delta1);
            Z2 := Sin(Delta2);

            A := Y1 * Z2 - Z1 * Y2;
            B := Z1 * X2 - X1 * Z2;
            C := X1 * Y2 - Y1 * X2;

            m := Tan(Alpha3);
            n := Tan(Delta3) / Cos(Alpha3);

            result := Arcsin((A + B * m + C * n) / (Sqrt(A * A + B * B + C * C) * Sqrt(1 + m * m + n * n)));
            result := AASCoordinateTransformation.RadiansToDegrees(result);
            if (result < 0) then
                result := Abs(result);

end;

function TAASAngularSeparation.PositionAngle(Alpha1, Delta1, Alpha2, Delta2: double): double;
var DeltaAlpha : double;
begin
            Delta1 := AASCoordinateTransformation.DegreesToRadians(Delta1);
            Delta2 := AASCoordinateTransformation.DegreesToRadians(Delta2);

            Alpha1 := AASCoordinateTransformation.HoursToRadians(Alpha1);
            Alpha2 := AASCoordinateTransformation.HoursToRadians(Alpha2);

            DeltaAlpha := Alpha1 - Alpha2;
            result := Arctan2(Sin(DeltaAlpha), Cos(Delta2) * Tan(Delta1) - Sin(Delta2) * Cos(DeltaAlpha));
            result := AASCoordinateTransformation.RadiansToDegrees(result);
            if (result < 0) then
                result := result + 180;
end;

function TAASAngularSeparation.Separation(Alpha1, Delta1, Alpha2, Delta2: double): double;
var x,y,z : double;
begin
            Delta1 := AASCoordinateTransformation.DegreesToRadians(Delta1);
            Delta2 := AASCoordinateTransformation.DegreesToRadians(Delta2);

            Alpha1 := AASCoordinateTransformation.HoursToRadians(Alpha1);
            Alpha2 := AASCoordinateTransformation.HoursToRadians(Alpha2);

            x := Cos(Delta1) * Sin(Delta2) - Sin(Delta1) * Cos(Delta2) * Cos(Alpha2 - Alpha1);
            y := Cos(Delta2) * Sin(Alpha2 - Alpha1);
            z := Sin(Delta1) * Sin(Delta2) + Cos(Delta1) * Cos(Delta2) * Cos(Alpha2 - Alpha1);

            result := Arctan2(Sqrt(x * x + y * y), z);
            result := AASCoordinateTransformation.RadiansToDegrees(result);
            if (result < 0) then
                result := result+180;

end;

function TAASAngularSeparation.SmallestCircle(Alpha1, Delta1, Alpha2, Delta2,
  Alpha3, Delta3: double; bType1: boolean): double;
var d1,d2,d3,a,b,c: double;
begin
            d1 := Separation(Alpha1, Delta1, Alpha2, Delta2);
            d2 := Separation(Alpha1, Delta1, Alpha3, Delta3);
            d3 := Separation(Alpha2, Delta2, Alpha3, Delta3);

            a := d1;
            b := d2;
            c := d3;
            if (b > a) then
            begin
                a := d2;
                b := d1;
                c := d3;
            end;
            if (c > a) then
            begin
                a := d3;
                b := d1;
                c := d2;
            end;

            if (a > Sqrt(b * b + c * c)) then
            begin
                bType1 := true;
                result := a;
            end
            else
            begin
                bType1 := false;
                result := 2 * a * b * c / (Sqrt((a + b + c) * (a + b - c) * (b + c - a) * (a + c - b)));
            end;
end;

{ End of AASAngularSeparation =================================================}

{ TAASBinaryStar }

function TAASBinaryStar.ApparentEccentricity(e, i, w: double): double;
Var cosi,cosw,sinw,esquared,A,B,C,D,sqrtD  : double;
begin
            i := AASCoordinateTransformation.DegreesToRadians(i);
            w := AASCoordinateTransformation.DegreesToRadians(w);

            cosi := Cos(i);
            cosw := Cos(w);
            sinw := Sin(w);
            esquared := e * e;
            A := (1 - esquared * cosw * cosw) * cosi * cosi;
            B := esquared * sinw * cosw * cosi;
            C := 1 - esquared * sinw * sinw;
            D := (A - C) * (A - C) + 4 * B * B;

            sqrtD := Sqrt(D);
            result := Sqrt(2 * sqrtD / (A + C + sqrtD));
end;

function TAASBinaryStar.Calculate(tt, P, T, e, a, i, omega, w: double): TAASBinaryStarDetails;
Var n,M,v,sinvw,cosvw,cosi : double;
begin
            n := 360 / P;
            M := AASCoordinateTransformation.MapTo0To360Range(n * (t - T));
            E := AASKepler.Calculate(M, e);
            E := AASCoordinateTransformation.DegreesToRadians(E);
            i := AASCoordinateTransformation.DegreesToRadians(i);
            w := AASCoordinateTransformation.DegreesToRadians(w);
            omega := AASCoordinateTransformation.DegreesToRadians(omega);

            result.r := a * (1 - e * Cos(E));

            v := Arctan(Sqrt((1 + e) / (1 - e)) * Tan(E / 2)) * 2;
            result.Theta := ARCtan2(Sin(v + w) * Cos(i), Cos(v + w)) + omega;
            result.Theta := AASCoordinateTransformation.MapTo0To360Range(AASCoordinateTransformation.RadiansToDegrees(result.Theta));

            sinvw := Sin(v + w);
            cosvw := Cos(v + w);
            cosi := Cos(i);
            result.Rho := result.r * Sqrt((sinvw * sinvw * cosi * cosi) + (cosvw * cosvw));

end;

{ TAASKepler }

function TAASKepler.Calculate(M, e: double; nIterations: integer): double;
var F,scale,R : double;
    i: integer;
begin
            //Convert from degrees to radians
            M := AASCoordinateTransformation.DegreesToRadians(M);

            F := 1;
            if (M < 0) then
                F := -1;
            M := Abs(M) / (2 * PI);
            M := (M - Trunc(M)) * 2 * PI * F;
            if (M < 0) then
                M := M + 2 * PI;
            F := 1;
            if (M > PI) then
                F := -1;
            if (M > PI) then
                M := 2 * PI - M;

            E := PI / 2;
            scale := PI / 4;
            for i:=0 to Pred(nIterations) do
            begin
                R := E - e * Sin(E);
                if (M > R) then
                    E := E + scale
                else
                    E := E - scale;
                scale := scale/2;
            end;

            //Convert the result back to degrees
            result := AASCoordinateTransformation.RadiansToDegrees(E) * F;
end;

{ TAASPrecession }

function TAASPrecession.AdjustPositionUsingMotionInSpace(r, DeltaR, t, Alpha,
  Delta, PMAlpha, PMDelta: double): TAAS2DCoordinate;
begin
(*
            //Convert DeltaR from km/s to Parsecs / Year
            DeltaR /= 977792;

            //Convert from seconds of time to Radians / Year
            PMAlpha /= 13751;

            //Convert from seconds of arc to Radians / Year
            PMDelta /= 206265;

            //Now convert to radians
            Alpha = AASCoordinateTransformation.HoursToRadians(Alpha);
            Delta = AASCoordinateTransformation.DegreesToRadians(Delta);

            double x = r * Cos(Delta) * Cos(Alpha);
            double y = r * Cos(Delta) * Sin(Alpha);
            double z = r * Sin(Delta);

            double DeltaX = x / r * DeltaR - z * PMDelta * Cos(Alpha) - y * PMAlpha;
            double DeltaY = y / r * DeltaR - z * PMDelta * Sin(Alpha) + x * PMAlpha;
            double DeltaZ = z / r * DeltaR + r * PMDelta * Cos(Delta);

            x += t * DeltaX;
            y += t * DeltaY;
            z += t * DeltaZ;

            TAAS2DCoordinate value = new TAAS2DCoordinate();
            value.X = AASCoordinateTransformation.MapTo0To24Range(AASCoordinateTransformation.RadiansToHours(Atan2(y, x)));
            value.Y = AASCoordinateTransformation.RadiansToDegrees(Atan2(z, Sqrt(x * x + y * y)));
*)
end;

function TAASPrecession.AdjustPositionUsingUniformProperMotion(t, Alpha, Delta,
  PMAlpha, PMDelta: double): TAAS2DCoordinate;
begin
  Result.X := Alpha + (PMAlpha * t / 3600);
  Result.Y := Delta + (PMDelta * t / 3600);
end;

function TAASPrecession.EquatorialPMToEcliptic(Alpha, Delta, Beta, PMAlpha,
  PMDelta, Epsilon: double): TAAS2DCoordinate;
begin

end;

function TAASPrecession.PrecessEcliptic(Lambda, Beta, JD0,
  JD: double): TAAS2DCoordinate;
begin

end;

function TAASPrecession.PrecessEquatorial(Alpha, Delta, JD0,
  JD: double): TAAS2DCoordinate;
Var TT,t,tsquared,tcubed,sigma,zeta,phi,A,B,C,DeltaAlpha : double;
begin

            TT:= (JD0 - 2451545.0) / 36525;
            Tsquared:= TT * TT;
            t:= (JD - JD0) / 36525;
            tsquared:= t * t;
            tcubed:= tsquared * t;

            //Now convert to radians
            Alpha:= AASCoordinateTransformation.HoursToRadians(Alpha);
            Delta:= AASCoordinateTransformation.DegreesToRadians(Delta);

            sigma:= AASCoordinateTransformation.DegreesToRadians(AASCoordinateTransformation.DMSToDegrees(0, 0, (2306.2181 + 1.39656 * TT - 0.000139 * Tsquared) * t + (0.30188 - 0.000344 * T) * tsquared + 0.017998 * tcubed));
            zeta:= AASCoordinateTransformation.DegreesToRadians(AASCoordinateTransformation.DMSToDegrees(0, 0, (2306.2181 + 1.39656 * TT - 0.000139 * Tsquared) * t + (1.09468 + 0.000066 * T) * tsquared + 0.018203 * tcubed));
            phi:= AASCoordinateTransformation.DegreesToRadians(AASCoordinateTransformation.DMSToDegrees(0, 0, (2004.3109 - 0.8533 * TT - 0.000217 * Tsquared) * t - (0.42665 + 0.000217 * T) * tsquared - 0.041833 * tcubed));
            A:= Cos(Delta) * Sin(Alpha + sigma);
            B:= Cos(phi) * Cos(Delta) * Cos(Alpha + sigma) - Sin(phi) * Sin(Delta);
            C:= Sin(phi) * Cos(Delta) * Cos(Alpha + sigma) + Cos(phi) * Sin(Delta);

            Result.X:= AASCoordinateTransformation.MapTo0To24Range(AASCoordinateTransformation.RadiansToHours(Arctan2(A, B) + zeta));
            Result.Y:= AASCoordinateTransformation.RadiansToDegrees(Arcsin(C));

 end;

function TAASPrecession.PrecessEquatorialFK4(Alpha, Delta, JD0, JD: double): TAAS2DCoordinate;
Var TT,t,tsquared,tcubed,sigma,zeta,phi,A,B,C,DeltaAlpha : double;
begin
            TT := (JD0 - 2415020.3135) / 36524.2199;
            t := (JD - JD0) / 36524.2199;
            tsquared := t * t;
            tcubed := tsquared * t;

            //Now convert to radians
            Alpha := AASCoordinateTransformation.HoursToRadians(Alpha);
            Delta := AASCoordinateTransformation.DegreesToRadians(Delta);

            sigma := AASCoordinateTransformation.DegreesToRadians(AASCoordinateTransformation.DMSToDegrees(0, 0, (2304.250 + 1.396 * TT) * t + 0.302 * tsquared + 0.018 * tcubed));
            zeta  := AASCoordinateTransformation.DegreesToRadians(AASCoordinateTransformation.DMSToDegrees(0, 0, 0.791 * tsquared + 0.001 * tcubed)) + sigma;
            phi   := AASCoordinateTransformation.DegreesToRadians(AASCoordinateTransformation.DMSToDegrees(0, 0, (2004.682 - 0.853 * TT) * t - 0.426 * tsquared - 0.042 * tcubed));
            A := Cos(Delta) * Sin(Alpha + sigma);
            B := Cos(phi) * Cos(Delta) * Cos(Alpha + sigma) - Sin(phi) * Sin(Delta);
            C := Sin(phi) * Cos(Delta) * Cos(Alpha + sigma) + Cos(phi) * Sin(Delta);

            DeltaAlpha := AASCoordinateTransformation.DMSToDegrees(0, 0, 0.0775 + 0.0850 * TT);
            Result.X := AASCoordinateTransformation.MapTo0To24Range(AASCoordinateTransformation.RadiansToHours(Arctan2(A, B) + zeta) + DeltaAlpha);
            Result.Y := AASCoordinateTransformation.RadiansToDegrees(Arcsin(C));
end;

end.


