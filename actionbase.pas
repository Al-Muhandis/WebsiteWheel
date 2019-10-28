unit actionbase;

{$mode objfpc}{$H+}

interface

uses
  BrookAction, JTemplate, QTemplate, config, BrookHttpDefs;

type

  { TBaseAction }

  TBaseAction = class(TBrookAction)
  private
    FBaseTemplateName: String;
    FDescription: String;
    FJTemplate: TJTemplate;
    FQTemplate: TQTemplate;
    FTemplateDirectory: String;
    FTitle: String;
    function GetTemplateFileName: String;
  protected
    procedure Parse; virtual;
    procedure ParseQTemplate; virtual; abstract;
    function RemoteAddress: String;
    property BaseTemplateName: String read FBaseTemplateName write FBaseTemplateName;
    property JTemplate: TJTemplate read FJTemplate;
    property QTemplate: TQTemplate read FQTemplate;
    property TemplateFileName: String read GetTemplateFileName;
    property TemplateDir: String read FTemplateDirectory write FTemplateDirectory;
    property Title: String read FTitle write FTitle;
    property Description: String read FDescription write FDescription;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    function IsMatchIP(const IPs: array of String): Boolean;
    procedure DoRequest(ARequest: TBrookRequest; AResponse: TBrookResponse); override;
    function SendMailNotify(const Subject, Msg: String; out ErrorStr: String;
      Receiver: String = ''): Boolean;
  end;

implementation

uses
  strutils, classes, sysutils, cmn_mail, tableentities
  ;

{ TBaseAction }

constructor TBaseAction.Create;
begin
  inherited Create;
  FQTemplate:=TQTemplate.Create;
  FQTemplate.StartDelimiter:=TgPfxQ;
  FQTemplate.EndDelimiter:=TgSfxQ;
  FJTemplate:=TJTemplate.Create(nil);
  FJTemplate.TagPrefix:=TgPfxJ;
  FJTemplate.TagSuffix:=TgSfxJ;
  FJTemplate.TagEscape:='¦';
  FBaseTemplateName:='index.html';
  FTemplateDirectory:=Conf.TemplatesDirectory+PathDelim+'default'+PathDelim;
end;

destructor TBaseAction.Destroy;
begin
  FQTemplate.Free;
  FJTemplate.Free;
  inherited Destroy;
end;

function TBaseAction.IsMatchIP(const IPs: array of String): Boolean;
var
  ARemoteAddress, IP: String;
begin
  ARemoteAddress:=RemoteAddress;
  Result:=False;
  for IP in IPs do
    if AnsiStartsStr(IP, ARemoteAddress) then
      Exit(True);
end;

function TBaseAction.SendMailNotify(const Subject, Msg: String; out
  ErrorStr: String; Receiver: String = ''): Boolean;
begin                               { TODO : Session user checking }
  if Receiver=EmptyStr then
    if Assigned(nil) then
      Receiver:='sample@sample.com'
    else begin
      AppLogger.Error('Failed to send email. UserConfig is nil');
      Exit(False);
    end;
  Result:=AppSendEMail(Subject, Msg, Receiver, ErrorStr);
  if not Result then
    AppLogger.Error(ErrorStr+'. Subject'+Subject);
end;

function TBaseAction.GetTemplateFileName: String;
begin
  Result:=FTemplateDirectory+FBaseTemplateName;
end;

procedure TBaseAction.Parse;
begin
  JTemplate.Fields.Add('title', Title);
  JTemplate.Fields.Add('description', Description);
  if FQTemplate.HasContent then
    JTemplate.Parser.Content:=QTemplate.GetContent
  else
    JTemplate.LoadFromFile(TemplateFileName);
  JTemplate.Parser.Replace(True);
  Write(JTemplate.Parser.Content);
end;

function TBaseAction.RemoteAddress: String;
var
  ARemoteAddress: String;
begin
  ARemoteAddress:=HttpRequest.CustomHeaders.Values['CF-Connecting-IP'];
  if ARemoteAddress<>EmptyStr then
    Exit(ARemoteAddress);
  Result:=HttpRequest.GetCustomHeader('X-Real-IP');
  if Result=EmptyStr then
    Result:=HttpRequest.RemoteAddress;
end;

procedure TBaseAction.DoRequest(ARequest: TBrookRequest;
  AResponse: TBrookResponse);
begin
  try
    inherited;
  except
    on E: Exception do
      AppLogger.Error('['+ClassName+'] '+E.ClassName+': '+E.Message);
  end;
end;

end.
