program Project1;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  UInterpreteur in 'UInterpreteur.pas',
  UToken in 'UToken.pas',
  UParser in 'UParser.pas',
  UNodes in 'UNodes.pas',
  UValues in 'UValues.pas',
  UVariables in 'UVariables.pas',
  UEnvironnement in 'UEnvironnement.pas',
  UFunctions in 'UFunctions.pas',
  UStack in 'UStack.pas',
  UListing in 'UListing.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
