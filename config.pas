unit config;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, rttiutils, eventlog, fpjson
  ;

type

  EWSWConfig = class(Exception);

  { TAdminConf }

  TAdminConf = class
  private
    FAdminAlias: String;
    FAdminHost: String;
    FAdminRemoteAddress: String;
    FAdminTemplate: String;
    FUsername: String;
    function GetAdminTemplate: String;
  published
    property UriAlias: String read FAdminAlias write FAdminAlias;
    property Host: String read FAdminHost write FAdminHost;
    property Template: String read GetAdminTemplate write FAdminTemplate;
    property RemoteAddress: String read FAdminRemoteAddress write FAdminRemoteAddress;
    property Username: String read FUsername write FUsername;
  end;

  { TTelegramConf }

  TTelegramConf = class
  private
    FToken: String;
  published
    property Token: String read FToken write FToken;
  end;

  { TSMTPConfig }

  TSMTPConfig = class
  private
    FHost: String;
    FPassword: String;
    FPort: String;
    FUserName: String;
  published
    property UserName: String read FUserName write FUserName;
    property Password: String read FPassword write FPassword;
    property Host: String read FHost write FHost;
    property Port: String read FPort write FPort;
  end;

  { TEmailConfig }

  TEmailConfig = class
  private
    FSender: String;
    Fsmtp: TSMTPConfig;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Sender: String read FSender write FSender;
    property smtp: TSMTPConfig read Fsmtp write Fsmtp;
  end;

  { TDebugInfo }

  TDebugInfo = class
  private
    FEnabled: Boolean;
    FResortStrings: Boolean;
    FSortedStrings: Boolean;
  public
    constructor Create;
  published
    property Enabled: Boolean read FEnabled write FEnabled;
    property SortedStrings: Boolean read FSortedStrings write FSortedStrings;
    property ResortStrings: Boolean read FResortStrings write FResortStrings;
  end;

  { TLoggerConfig }

  TLoggerConf = class
  private
    FActive: Boolean;
    FFilename: String;
  published
    property Active: Boolean read FActive write FActive;
    property FileName: String read FFilename write FFilename;
  end;

  { TDBConf }

  TDBConf = class
  private
    FDatabase: String;
    FDriver: String;
    FHost: String;
    FLoggerConf: TLoggerConf;
    FPassword: String;
    FUser: String;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Driver: String read FDriver write FDriver;
    property Database: String read FDatabase write FDatabase;
    property User: String read FUser write FUser;
    property Password: String read FPassword write FPassword;
    property Host: String read FHost write FHost;
    property Logger: TLoggerConf read FLoggerConf write FLoggerConf;
  end;

  { TAppConf }

  TAppConf = class
  private
    FTitle: String;
  published
    property Title: String read FTitle write FTitle;
  end;

  { TConf }

  TConf = class
  private
    FAdmin: TAdminConf;
    FApp: TAppConf;
    FDB: TDBConf;
    FDebug: TDebugInfo;
    Femail: TEmailConfig;
    FTelegram: TTelegramConf;
    FTemplatesDirectory: String;
    FUploadDirectory: String;
    function GetTemplatesDirectory: String;
  public
    constructor Create;
    destructor Destroy; override;
    function GetAdminTemplateDir: String;
  published
    property Admin: TAdminConf read FAdmin write FAdmin;
    property Debug: TDebugInfo read FDebug write FDebug;
    property DB: TDBConf read FDB write FDB;
    property Email: TEmailConfig read Femail write Femail;
    property Telegram: TTelegramConf read FTelegram write FTelegram;
    property App: TAppConf read FApp write FApp;
    property TemplatesDirectory: String read GetTemplatesDirectory write FTemplatesDirectory;
    property UploadDirectory: String read FUploadDirectory write FUploadDirectory;
  end;

var
  Conf: TConf;
  ConfDir, AppDir, TmltDir, AppName: String;
  AppLogger: TEventLog;

procedure LoadFromJSON(AObject: TObject; const AFileName: String);
procedure SaveToJSON(AObject: TObject; const AFileName: String);

implementation

uses
  typinfo, fpjsonrtti
  ;

var
  CnfgFileName: String;
  MyCriticalSection: TRTLCriticalSection;

const
  s_def='default';

procedure LoadFromJSON(AObject: TObject; const AFileName: String);
var
  ADeStreamer: TJSONDeStreamer;
  AJSON: TStringList;
begin
  if not FileExists(AFileName) then
    Exit;
  ADeStreamer:=TJSONDeStreamer.Create(nil);
  try
    AJSON:=TStringList.Create;
    try
      AJSON.LoadFromFile(AFileName);
      try
        ADeStreamer.JSONToObject(AJSON.Text, AObject);
      except
      end;
    finally
      AJSON.Free;
    end;
  finally
    ADeStreamer.Free;
  end;
end;

procedure SaveToJSON(AObject: TObject; const AFileName: String);
var
  AStreamer: TJSONStreamer;
  AJSON: TStringList;
  AJSONObject: TJSONObject;
begin
  AStreamer:=TJSONStreamer.Create(nil);
  try
    AJSON:=TStringList.Create;
    try
      try
        AJSONObject:=AStreamer.ObjectToJSON(AObject);
        try
          AJSON.Text:=AJSONObject.FormatJSON();
          AJSON.SaveToFile(AFileName);
        finally
          AJSONObject.Free;
        end;
      except
      end;
    finally
      AJSON.Free;
    end;
  finally
    AStreamer.Free;
  end;
end;

{ TDBConf }

constructor TDBConf.Create;
begin
  inherited;
  FLoggerConf:=TLoggerConf.Create;
end;

destructor TDBConf.Destroy;
begin
  FLoggerConf.Free;
  inherited Destroy;
end;

{ TDebugInfo }

constructor TDebugInfo.Create;
begin
  FSortedStrings:=False;
  FResortStrings:=False;
  FEnabled:=False;
end;

{ TEmailConfig }

constructor TEmailConfig.Create;
begin
  Fsmtp:=TSMTPConfig.Create;
end;

destructor TEmailConfig.Destroy;
begin
  Fsmtp.Free;
  inherited Destroy;
end;

{ TAdminConf }

function TAdminConf.GetAdminTemplate: String;
begin
  if FAdminTemplate<>EmptyStr then
    Result:=FAdminTemplate
  else
    Result:=s_def;
end;

{ TConf }

function TConf.GetTemplatesDirectory: String;
begin
  if FTemplatesDirectory<>EmptyStr then
    Result:=FTemplatesDirectory
  else
    Result:=TmltDir;
end;

constructor TConf.Create;
begin
  FAdmin:=TAdminConf.Create;
  FTelegram:=TTelegramConf.Create;
  FEmail:=TEmailConfig.Create;
  FDebug:=TDebugInfo.Create;
  FDB:=TDBConf.Create;
  FApp:=TAppConf.Create;
end;

destructor TConf.Destroy;
begin
  FreeAndNil(FApp);
  FreeAndNil(FDB);
  FreeAndNil(FDebug);
  FreeAndNil(FEmail);
  FreeAndNil(FTelegram);
  FreeAndNil(FAdmin);
  inherited Destroy;
end;

function TConf.GetAdminTemplateDir: String;
begin
  Result:=TemplatesDirectory+'admin'+PathDelim+FAdmin.Template+PathDelim;
end;

initialization
  AppLogger:=TEventLog.Create(nil);
  AppLogger.LogType:=ltFile;
  AppLogger.FileName:=ChangeFileExt(ParamStr(0), '.log');
  AppLogger.Paused:=False;
  InitCriticalSection(MyCriticalSection{%H-});
  try
    AppName:=ExtractFileName(ChangeFileExt(ParamStr(0), EmptyStr));
    AppDir:=IncludeTrailingPathDelimiter(ExtractFileDir(ParamStr(0)));
    ConfDir:=AppDir;
    TmltDir:=ConfDir+'templates'+DirectorySeparator;
    CnfgFileName:=ConfDir+AppName+'.ini';
    Conf:=TConf.Create;
    LoadFromJSON(Conf, ChangeFileExt(ParamStr(0), '.json'));
  //  SaveToJSON(Conf, ChangeFileExt(ParamStr(0), '.json'));
    {$IFDEF DEBUG}SaveToJSON(Conf, '~temp.json');{$ENDIF}
  except
    on E:Exception do
      AppLogger.Log(etError, E.ClassName+': '+E.Message)
  end;

finalization
  FreeAndNil(Conf);
  DoneCriticalsection(MyCriticalSection);
  AppLogger.Free;

end.

