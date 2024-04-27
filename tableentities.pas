unit tableentities;

{$mode objfpc}{$H+}
{$interfaces corba}

interface

uses
  Classes, SysUtils, dSQLdbBroker, fgl, fpjson, dClasses, dSqlBuilder
  ;

type

  TDataManagement = class;

  { TDBEntity }

  TDBEntity = class(TPersistent)
  private
    FID: Int64;
    FORM: TDataManagement;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetCaption: String; virtual; abstract;
  public                
    property ORM: TDataManagement read FORM write FORM;
    procedure Clear; virtual; abstract;
    class function GetIgnoredFields: String; virtual;
    property Caption: String read GetCaption;
  published
    property id: Int64 read FID write FID;
  end;

  { TCommentedEntity }

  TCommentedEntity = class(TDBEntity)
  private
    FComment: String;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure Clear; override;
  published
    property Comment: String read FComment write FComment;
  end;

  { TNamedEntity }

  TNamedEntity = class(TCommentedEntity)
  private
    FName: String;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetCaption: String; override;
  public
    procedure Clear; override;
  published
    property Name: String read FName write FName;
  end;

  TItemTemplateFun = function (const Row: String; AnItem: TObject): String of object;

  TdComponentClass = class of TdComponent;

  { IOPFInterface }

  IOPFInterface = interface ['{DF60E350-C1D6-4831-8771-D06B02742FD0}']
    procedure AddEntity;
    procedure ApplyEntity;
    procedure RemoveEntity;
    procedure ListEntity(const aFilter: String = '');
    procedure ModifyEntity;
    function GetEntityList: TFPSList;
    function GetEntity: TDBEntity;
    function GetEntityByID(aID: Int64): TDBEntity; 
    function GetEntityByID(aID: Int64; out aFound: Boolean): TDBEntity;
    function GetEntityCaption: String;
    function GetEntitiesCaption: String;
  end;

  { TdGDBEntityOpf }

  generic TdGDBEntityOpf<T: TDBEntity> = class(specialize TdGSQLdbEntityOpf<T>, IOPFInterface)
  private
    FEntities: TEntities;
    FEntitiesCaption: String;
    FEntityCaption: String;
    FListFilter: String;
    function GetEntities: TEntities;
    function GetEntitiesCaption: String;
    function GetEntityCaption: String;
    class function QuoteName(const aNameValue: String; aQuote: AnsiChar): String;
  public
    procedure AddEntity;
    procedure ApplyEntity;
    constructor Create(AConnection: TdSQLdbConnector; const ATableName: string); reintroduce; override;
    constructor Create(AORM: TDataManagement; const ATableName: string; const aEntities, aEntity: String);
    procedure RemoveEntity;
    destructor Destroy; override;
    function GetEntity: TDBEntity;
    function GetEntityByID(aID: Int64; out aFound: Boolean): TDBEntity; virtual;
    function GetEntityByID(aID: Int64): TDBEntity;
    function GetEntityList: TFPSList;
    procedure ListEntity(const aFilter: String = '');
    procedure ModifyEntity;
    property Entities: TEntities read GetEntities;
    property EntitiesCaption: String read GetEntitiesCaption write FEntitiesCaption;
    property EntityCaption: String read GetEntityCaption write FEntityCaption;
  end;

  { TDataManagement }

  TDataManagement = class
  private
    Fcon: TdSQLdbConnector;
  protected
    function GetOPF(const aAlias: String): IOPFInterface; virtual; abstract;
  public
    constructor Create(const aDriver, aDBName: String; const aUser: String = '';
      const aPassword: String = ''; const aHost: String = ''; aLogFileName: String = '';
      aLogActive: Boolean = False); virtual;
    destructor Destroy; override;
    function GetEntityByID(const aAlias: String; aID: Int64): TDBEntity;
    function GetTemplateItem(const Row: String; AnItem: TDBEntity; aSelectedID: Int64 = -1;
      aSelectedAttr: String = ''): String;
    function GetTemplateListEntities(const Header, Row, Footer: String;
      aEntities: TFPSList; aSelectedID: Int64 = -1; aSelectedAttr: String = ''): String;
    function LastInsertID: Integer;
    property Con: TdSQLdbConnector read FCon;
    property OPF [aAlias: String]: IOPFInterface read GetOPF;
  end;

const
  TgPfxQ='${+';
  TgSfxQ='+}';
  TgPfxJ='${-';
  TgSfxJ='-}';

implementation

uses
  JTemplate, strutils
  ;

var
  FS: TFormatSettings;

function QuoteName(const aNameValue: String; aQuote: AnsiChar): String;
var
  i: Integer;
begin
  i:=Pos('=', aNameValue);
  if i>0 then
    Result:= aQuote+LeftStr(aNameValue, i-1)+aQuote+'='+RightStr(aNameValue, Length(aNameValue)-i)
  else
    Result:=aQuote+aNameValue+aQuote;
end;

function ExtractBetweenKeys(const ASource, Key1, Key2: String;
  var APos: Integer; out ADest: String): Boolean;
var
  AStart, AnEnd: Integer;
begin
  Result := False;
  AStart := PosEx(Key1, ASource, APos);
  if AStart <> 0 then
  begin
    Inc(AStart, Length(Key1));
    AnEnd := PosEx(Key2, ASource, AStart);
    if AnEnd <> 0 then
    begin
      ADest := copy(ASource, AStart, AnEnd - AStart);
      Result := True;
      APos := AnEnd + Length(Key2)
    end
  end
end;

{ TCommentedEntity }

procedure TCommentedEntity.AssignTo(Dest: TPersistent);
var
  ADest: TCommentedEntity;
begin
  if Dest is TCommentedEntity then
  begin
    ADest:=TCommentedEntity(Dest);
    ADest.FComment:=FComment;
  end;
  inherited AssignTo(Dest);
end;

procedure TCommentedEntity.Clear;
begin
  FComment:=EmptyStr;
end;

{ TNamedEntity }

procedure TNamedEntity.AssignTo(Dest: TPersistent);
var
  ADest: TNamedEntity;
begin
  if Dest is TNamedEntity then
  begin
    ADest:=TNamedEntity(Dest);
    ADest.FName:=FName;
  end;
  inherited AssignTo(Dest);
end;

function TNamedEntity.GetCaption: String;
begin
  Result:=FName;
end;

procedure TNamedEntity.Clear;
begin
  FName:=EmptyStr;
  inherited;
end;

{ TdGDBEntityOpf }

function TdGDBEntityOpf.GetEntities: TEntities;
begin
  if not Assigned(FEntities) then
    FEntities:=TEntities.Create;
  Result:=FEntities;
end;

function TdGDBEntityOpf.GetEntitiesCaption: String;
begin
  Result:=FEntitiesCaption;
end;

function TdGDBEntityOpf.GetEntityCaption: String;
begin
  Result:=FEntityCaption;
end;

class function TdGDBEntityOpf.QuoteName(const aNameValue: String; aQuote: AnsiChar): String;
var
  i: Integer;
begin
  i:=Pos('=', aNameValue);
  if i>0 then
    Result:= aQuote+Trim(LeftStr(aNameValue, i-1))+aQuote+'='+RightStr(aNameValue, Length(aNameValue)-i)
  else
    Result:=aQuote+aNameValue+aQuote;
end;

procedure TdGDBEntityOpf.AddEntity;
begin
  Add;
end;

procedure TdGDBEntityOpf.ApplyEntity;
begin
  Apply;
end;

constructor TdGDBEntityOpf.Create(AConnection: TdSQLdbConnector;
  const ATableName: string);
begin
  inherited Create(AConnection, ATableName);
  Table.IgnoredFields.CommaText:=T.GetIgnoredFields;
  FieldQuote:='`';
end;

constructor TdGDBEntityOpf.Create(AORM: TDataManagement; const ATableName: string; const aEntities,
  aEntity: String);
begin
  Create(AORM.Con, ATableName);
  Entity.ORM:=AORM;
  EntitiesCaption:=aEntities;
  EntityCaption:=aEntity;
  FieldQuote:='`';
end;

procedure TdGDBEntityOpf.RemoveEntity;
begin
  Remove;
end;

destructor TdGDBEntityOpf.Destroy;
begin
  FEntities.Free;
  inherited Destroy;
end;

function TdGDBEntityOpf.GetEntity: TDBEntity;
begin
  Result:=Entity;
end;

function TdGDBEntityOpf.GetEntityByID(aID: Int64; out aFound: Boolean): TDBEntity;
begin
  if Entity.id<>aID then
  begin
    Entity.id:=aID;
    aFound:=Get;
    if not aFound then
      Entity.Clear;
  end
  else
    aFound:=True;  // cached
  Result:=Entity;
end;

function TdGDBEntityOpf.GetEntityByID(aID: Int64): TDBEntity;
var
  aFound: Boolean;
begin
  Result:=GetEntityByID(aID, aFound);
end;

function TdGDBEntityOpf.GetEntityList: TFPSList;
begin
  Result:=Entities;
end;

procedure TdGDBEntityOpf.ListEntity(const aFilter: String);
var
  aSQL, aFS: string;
begin
  if Assigned(FEntities) then
    if SameStr(aFilter, FListFilter) then
      Exit;                   // No need sql to request if the list already exists
  Entities.Clear;
  FListFilter:=aFilter;
  if FListFilter=EmptyStr then
    List(Entities, nil, EmptyStr)
  else begin
    TSelectBuilder.MakeFields(Table, aFS, True, '`');
    aSQL:='select ' + aFS + ' from ' + Table.Name+' where '+QuoteName(FListFilter, '`');
    List(Entities, nil, aSQL);
  end;
end;

procedure TdGDBEntityOpf.ModifyEntity;
begin
  Modify;
end;

{ TDataManagement }

destructor TDataManagement.Destroy;
begin
  FCon.Free;
end;

function TDataManagement.GetEntityByID(const aAlias: String; aID: Int64
  ): TDBEntity;
var
  aOPF: IOPFInterface;
begin
  aOPF:=OPF[aAlias];
  if Assigned(aOPF) then
    Result:=aOPF.GetEntityByID(aID)
  else
    Result:=nil;
end;

function TDataManagement.GetTemplateItem(const Row: String; AnItem: TDBEntity; aSelectedID: Int64;
  aSelectedAttr: String): String;
var
  AJTemplate: TJTemplate;
begin
  AJTemplate:=TJTemplate.Create(nil);
  try
    AJTemplate.TagPrefix:=TgPfxJ;
    AJTemplate.TagSuffix:=TgSfxJ;
    AJTemplate.TagEscape:='$';
    AJTemplate.HtmlSupports:=False;
    AJTemplate.Parser.Content:=Row;
    AJTemplate.AddFields(AnItem);

    if aSelectedID>-1 then
      if aSelectedID=AnItem.id then
      begin
        AJTemplate.Fields.Add('%selected%', aSelectedAttr);
      end
      else
        AJTemplate.Fields.Add('%selected%', EmptyStr);
    AJTemplate.Parser.Replace(True);
    Result:=AJTemplate.Parser.Content;
  finally
    AJTemplate.Free;
  end;
end;

function TDataManagement.GetTemplateListEntities(const Header, Row, Footer: String;
  aEntities: TFPSList; aSelectedID: Int64; aSelectedAttr: String): String;
var
  i: Integer;
  aEntity: TDBEntity;
begin
  Result:=Header;
  for i:=0 to aEntities.Count-1 do
  begin
    aEntity:=TDBEntity(aEntities.Items[i]^);
    aEntity.ORM:=Self;
    Result+=LineEnding+GetTemplateItem(Row, aEntity, aSelectedID, aSelectedAttr);
  end;
  Result+=LineEnding+Footer;
end;

function TDataManagement.LastInsertID: Integer;
var
  aQuery: TdSQLdbQuery;
begin
  aQuery:=TdSQLdbQuery.Create(Con);
  try
    aQuery.SQL.Text:='SELECT LAST_INSERT_ID() AS ID';
    aQuery.Open;
    Result:=aQuery.Field('ID').AsInteger;
    aQuery.Close;
  finally
    aQuery.Free;
  end;
end;

constructor TDataManagement.Create(const aDriver, aDBName: String;
  const aUser: String; const aPassword: String; const aHost: String;
  aLogFileName: String; aLogActive: Boolean);
var
  aQuery: TdSQLdbQuery;
begin
  Fcon := TdSQLdbConnector.Create(nil);
  Fcon.Driver := aDriver;
  Fcon.Database := aDBName;
  Fcon.Logger.Active := aLogActive;
  if aLogFileName=EmptyStr then
    aLogFileName:=ClassName;
  Fcon.Logger.FileName := aLogFileName+'.log';
  Fcon.User:=aUser;
  Fcon.Host:=aHost;
  Fcon.Password:=aPassword;
  FCon.Connect;
  aQuery:=TdSQLdbQuery.Create(FCon);
  aQuery.SQL.Add('SET CHARACTER SET `utf8`');
  aQuery.Execute;
  aQuery.Free;
end;

{ TDBEntity }

procedure TDBEntity.AssignTo(Dest: TPersistent);
var
  ADest: TDBEntity;
begin
  if Dest is TDBEntity then
  begin
    ADest:=TDBEntity(Dest);
    ADest.FID:=FID;
  end
  else
    inherited AssignTo(Dest);
end;

class function TDBEntity.GetIgnoredFields: String;
begin
  Result:=EmptyStr;
end;

initialization
  FS:=DefaultFormatSettings;


end.

