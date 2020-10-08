program PoleCalc;

uses
  System.StartUpCopy,
  FMX.Forms,
  PolCalcMain in 'PolCalcMain.pas' {PoleForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TPoleForm, PoleForm);
  Application.Run;
end.
