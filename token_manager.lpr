program token_manager;

{$mode objfpc}{$H+}
{$define debug}
uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp,
  clipbrd, { adds clipboard functionality }
  bc_strings; { provides getfieldtoken }

const
  DefaultFilename = '/home/bc/src/git_help/token.git';

type

  { TTokenManager }

  TTokenManager = class(TCustomApplication)
  protected
    fToken: string;
    fTokenString: string;
    fTokenFilename: string;
    fTokenFile: TStringList;
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
    function LoadTokenFile(aFilename: string): boolean;
    function ParseTokenString(const aTokenstring: string): boolean;
  end;

{ TTokenManager }

procedure TTokenManager.DoRun;
var
  ErrorMsg,lSwitch: String;
  Idx: integer;
begin
  // parse parameters
  {$ifdef debug}
  if ParamCount >= 2 then begin
    lSwitch:= Params[1];
    fTokenFilename:= Params[2];
  end;
  if ParamCount < 2 then begin
    lSwitch:= '-l'; { tricking it to believe it has parameter }
    fTokenFilename:= DefaultFilename;
  end;
  if lSwitch = '-h' then begin
    WriteHelp;
    Terminate;
    Exit;
  end;
  if lSwitch = '-l' then begin
    if LoadTokenFile(fTokenFilename) then begin
      if fTokenString <> '' then ParseTokenString(fTokenString);
    end;
  end;
  {$endif}
  // stop program loop
  Terminate;
end;

constructor TTokenManager.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;

end;

destructor TTokenManager.Destroy;
begin
  inherited Destroy;
end;

procedure TTokenManager.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ','./token_manager ','[-h] Writes this help');
  writeln('       ','./token_manager ','[-l] Path to location-filename');
end;

function TTokenManager.LoadTokenFile(aFilename: string): boolean;
var
  Idx: integer;
begin
  Result:= false;
  fTokenFile:= TStringList.Create;
  try
    if aFilename <> '' then begin
      fTokenFile.LoadFromFile(aFilename);
    end else begin
      fTokenFile.LoadFromFile(DefaultFilename);
    end;
    { linear search }
    for Idx:= 0 to fTokenFile.Count-1 do if pos('Token', fTokenFile[Idx]) > 0 then begin
      fTokenString:= fTokenFile[Idx];
      break;
    end;
    {$ifdef debug}
    writeln(aFilename);
    writeln('TokenString = ',fTokenString);
    {$endif}
  finally FreeAndNil(fTokenFile); end;
  Result:= true;
end;

function TTokenManager.ParseTokenString(const aTokenstring: string): boolean;
begin
  Result:= false;
  fToken:= bcGetFieldToken(2,aTokenstring,':');
  fToken:= trim(fToken);
  writeln('Token -> ',fToken);
  Result:= true;
end;

var
  Application: TTokenManager;
begin
  Application:=TTokenManager.Create(nil);
  Application.Title:='Token Manager';
  Application.Run;
  Application.Free;
end.

