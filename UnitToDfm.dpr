program UnitToDfm;

uses
  Vcl.Forms,
  Utd.Main in 'Utd.Main.pas' {FormUnitToDfm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormUnitToDfm, FormUnitToDfm);
  Application.Run;
end.

