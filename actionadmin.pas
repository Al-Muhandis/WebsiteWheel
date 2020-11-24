unit actionadmin;

{$mode objfpc}{$H+}

interface

uses
  BrookAction, actionbase, config, Classes, BrookHttpDefs, BrookSession, configentities,
  tableentities
  ;

type

  { TAdminAction }

  TAdminAction = class(TBaseAction)
  private
    FAlias: String;
    FORM: TDataManagement;
    FSession: TBrookSession;
    FTplSuffix: String;
    FUser: TUserConf;
    function Authorize(const AUsername, APassword: String): Boolean;
    function GetItemContent(const Row, ItemName: String; AnItem: TConfItem): String;
    function QTemplateMod(const {%H-}ATag: String; {%H-}AParams: TStringList): String;
    function QTemplateList(const {%H-}ATag: String; AParams: TStringList): String;
  protected
    procedure AfterParse; virtual;
    function CreateORM: TDataManagement; virtual; abstract;
    function GetItemTemplate(const Row: String; AnItem: TDBEntity): String;
    function GetItemListContent(Header, Row, Footer: String; ACollection: TCollectionConf): String;
    function GetORM: TDataManagement; virtual;
    procedure Parse; override;
    property PathAlias: String read FAlias write FAlias;
    property TplSuffix: String read FTplSuffix write FTplSuffix;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetMainModFile: String; virtual;
    procedure Request(ARequest: TBrookRequest; AResponse: TBrookResponse); override;
    property ORM: TDataManagement read GetORM;
    property Session: TBrookSession read FSession;
    property User: TUserConf read FUser;
  end;

implementation

uses
  sysutils, JTemplate, BrookUtils, BrookException
  ;

const
  a_Login='login';

{ TAdminAction }

function TAdminAction.GetItemContent(const Row, ItemName: String;
  AnItem: TConfItem): String;
var
  AJTemplate: TJTemplate;
begin
  AJTemplate:=TJTemplate.Create(nil);
  try
    AJTemplate.TagPrefix:='${';
    AJTemplate.TagSuffix:='}';
    AJTemplate.TagEscape:='$';
    AJTemplate.Parser.Content:=Row;
    AJTemplate.Parser.Fields.Add('name', ItemName);
    AJTemplate.AddFields(AnItem);
    AJTemplate.Parser.Replace(True);
    Result:=AJTemplate.Parser.Content;
  finally
    AJTemplate.Free;
  end;
end;

function TAdminAction.Authorize(const AUsername, APassword: String): Boolean;
begin
  if not UserListConfs.Exists(AUsername) then
  begin
    HttpResponse.Code:=403;
    if Conf.Debug.Enabled then
      AppLogger.Debug('['+ClassName+'] Username "'+aUserName+'" not found');
    Exit(False);
  end;
  if not SameStr(APassword, TUserConf(UserListConfs.ItemConfs[AUsername]).Password) then
  begin
    HttpResponse.Code:=403;
    if Conf.Debug.Enabled then
      AppLogger.Debug('['+ClassName+'] Password of user "'+aUserName+'" not valid');
    Exit(False);
  end;
  Session.Field['name']:=AUsername;
  Session.Field['password']:=APassword;
  Session.CookiePath:='/'+Conf.Admin.UriAlias+'/';
  Session.CookieSecure:=False;
  {$IFDEF DEBUG}Session.CookieSecure:=False;
  Session.CookieDomain:=EmptyStr;{$ENDIF}
  Session.Finish(HttpResponse);
  Result:=True;
  Redirect('/'+Conf.Admin.UriAlias+'/', 302);
end;

function TAdminAction.GetItemTemplate(const Row: String; AnItem: TDBEntity): String;
var
  AJTemplate: TJTemplate;
begin
  AJTemplate:=TJTemplate.Create(nil);
  try
    AJTemplate.TagPrefix:='${';
    AJTemplate.TagSuffix:='}';
    AJTemplate.TagEscape:='$';
    AJTemplate.Parser.Content:=Row;
    AJTemplate.AddFields(AnItem);
    AJTemplate.Parser.Replace(True);
    Result:=AJTemplate.Parser.Content;
  finally
    AJTemplate.Free;
  end;
end;

function TAdminAction.GetItemListContent(Header, Row, Footer: String;
  ACollection: TCollectionConf): String;
var
  i: Integer;
begin
  Result:=Header;
  for i:=0 to ACollection.Count-1 do
    Result+=LineEnding+GetItemContent(Row, ACollection.ItemNames[i], ACollection.ItemByIndex[i]);
  Result+=LineEnding+Footer;
end;

function TAdminAction.GetORM: TDataManagement;
begin
  if not Assigned(FORM) then
    with Conf.DB do
      FORM:=CreateORM;
  Result:=FORM;
end;

procedure TAdminAction.Parse;
begin
  JTemplate.AddFields(FUser, 'user');
  JTemplate.Parser.Fields.Add('username', Session.Field['name']);
  JTemplate.Parser.Fields.Add('remoteaddress', RemoteAddress);

  JTemplate.Parser.Fields.Add('alias', FAlias);

  JTemplate.Fields.Add('template', Conf.Admin.Template);
  JTemplate.Fields.Add('adminalias', Conf.Admin.UriAlias);
  QTemplate['mod']:=@QTemplateMod;
  QTemplate.Template:=QTemplate.GetContent;
  QTemplate.FileName:=EmptyStr;
  inherited Parse;
end;

constructor TAdminAction.Create;
begin
  inherited Create;
  FSession:=TBrookSession.Create(nil);
  FSession.CookieSecure := True;
  FSession.TimeOut := 3*SecsPerDay;
  QTemplate['list']:=@QTemplateList;
  Title:=Conf.App.Title;
end;

function TAdminAction.QTemplateMod(const ATag: String; AParams: TStringList
  ): String;
var
  AStrings: TStringList;
begin
  AStrings:=TStringList.Create;
  try
    AStrings.LoadFromFile(TemplateDir+GetMainModFile);
    JTemplate.Parser.Content:=AStrings.Text;
    JTemplate.Parser.Replace(True);
    Result:=JTemplate.Parser.Content;
  finally
    AStrings.Free;
  end;
end;

function TAdminAction.QTemplateList(const ATag: String; AParams: TStringList): String;
var
  aOPF: IOPFInterface;
begin
  aOPF:=ORM.OPF[AParams.Values['entity']];
  if not Assigned(aOPF) then
    Exit('Entity '+AParams.Values['entity']+' not found!');
  aOPF.ListEntity(AParams.Values['filter']);
  Result:=ORM.GetTemplateListEntities(
    AParams.Values['header'], AParams.Values['row'], AParams.Values['footer'], aOPF.GetEntityList,
    StrToInt64Def(AParams.Values['sel_id'], -1), AParams.Values['sel_attr']);
end;

procedure TAdminAction.AfterParse;
begin

end;

destructor TAdminAction.Destroy;
begin
  FSession.Free;
  FORM.Free;
  inherited Destroy;
end;

function TAdminAction.GetMainModFile: String;

  function GetFile(const aAlias: String): String;
  begin
    if FTplSuffix=EmptyStr then
      Result:='mod_'+aAlias+'.html'
    else
      Result:='mod_'+aAlias+'_'+FTplSuffix+'.html';
  end;
begin
  Result:=GetFile(FAlias);
  if not FileExists(TemplateDir+Result) then
    Result:=GetFile('default');
end;

procedure TAdminAction.Request(ARequest: TBrookRequest;
  AResponse: TBrookResponse);
var
  LoginUrl, P, AUsername, APassword: String;
begin
  if (Conf.Admin.Host<>EmptyStr) and not AnsiSameStr(Conf.Admin.Host, HttpRequest.Host) then
  begin
    HttpResponse.Content:='Host not found!';
    HttpResponse.Code:=403;
    raise EBrookHttp404.Create(ARequest.PathInfo)
  end;
  if Conf.Admin.RemoteAddress<>EmptyStr then
    if not IsMatchIP(Conf.Admin.RemoteAddress.Split([' '])) then
    begin
      HttpResponse.Content:='Forbidden';
      HttpResponse.Code:=403;
      Exit;
    end;

  FAlias:=Variable['alias'];
  if FAlias=EmptyStr then
    FAlias:='index';
  TemplateDir:=Conf.GetAdminTemplateDir;
  if not FileExists(TemplateFileName) then
  begin
    HttpResponse.Code:=404;
    Exit;
  end;
  Session.Start(ARequest);
  P:=ARequest.PathInfo;
  LoginUrl:='/'+Conf.Admin.UriAlias+'/'+a_Login+'/';
  if Session.Exists('name') then
  begin
    if P<>LoginUrl then
    begin
      AUsername:=Session.Field['name'];
      FUser:=TUserConf(UserListConfs.ItemConfs[AUsername]);
      if Assigned(FUser) then
      begin
        QTemplate.FileName:=TemplateFileName;
        inherited;
      end
      else
        Exit; { TODO :  }
    end
    else
      Redirect('/'+Conf.Admin.UriAlias+'/', 302)
  end
  else begin
    if P = LoginUrl then
    begin
      AUsername := Field['name'];
      APassword := Field['password'];
      BaseTemplateName:='login'+'.html';
      if AUsername<>EmptyStr then
        Authorize(AUsername, APassword);
      QTemplate.FileName:=TemplateFileName;
      inherited
    end
    else
      Redirect(LoginUrl, 302);
  end;
  Parse;
end;

end.


