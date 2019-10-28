unit configentities;

{$mode objfpc}{$H+}

interface

 uses
   Classes, SysUtils, rttiutils, IniFiles
   ;

 type


  { TConfItem }

  TConfItem = class(TCollectionItem)
  private
    FDefConfig: Boolean;
    FName: String;
    procedure SetDefConfig(AValue: Boolean);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure InitConfig; virtual;
    procedure SetName(AValue: String); virtual;
  public
    property Name: String read FName write SetName;
  published
    property DefConfig: Boolean read FDefConfig write SetDefConfig;
  end;

  TConfItemClass = class of TConfItem;

  TListCommand = (lcAdd, lcDelete, lcEdit);

  { TCollectionConf }

  TCollectionConf = class(TCollection)
  private
    FDefItem: TConfItem;
    FName: String;
    FItemStrings: TStrings;
    FPropList: TPropInfoList;
    FPropsStorage: TPropsStorage;
    FIni: TMemIniFile;
    function DebugErrorLocate: Boolean;
    function GetItemByIndex(Index: Integer): TConfItem;
    function GetItemConfs(ItemName: String): TConfItem;
    procedure PropsStorageEraseSection(const ASection: string);
    function PropsStorageReadString(const ASection, Item, Default: string
      ): string;
    procedure PropsStorageWriteString(const ASection, Item, Value: string);
    procedure SetItemByIndex(Index: Integer; AValue: TConfItem);
    procedure SetItemConfs(ItemName: String; AValue: TConfItem);
    procedure SetItemStrings(AValue: TStrings);
    procedure StoreItemConfig(const AnItemName: String; AnItem: TConfItem);
    procedure StoreItemConfig(const AnItemName: String; NameValuePairs: TStrings);
  protected
    procedure LoadConfig;
    procedure LoadItemConfig(const AnItemName: String; AnItem: TConfItem); virtual;
    procedure LoadList;
    procedure SaveList(const Prefix: String = '');
    property ItemStrings: TStrings read FItemStrings write SetItemStrings;
  public
    constructor Create(AItemClass: TCollectionItemClass); virtual;
    destructor Destroy; override;
    function Exists(const ItemName: String): Boolean;
    procedure ItemsAdd(AStrings: TStrings);
    procedure ItemsAdd(AStrings: TStrings; out Was, Became: Integer); overload;
    procedure ItemsDelete(AStrings: TStrings);
    procedure ItemsDelete(AStrings: TStrings; out Was, Became: Integer); overload;
    procedure ItemsEdit(ItemNames, NameValuePairs: TStrings);
    procedure Load; virtual;
    property ItemConfs[ItemName: String]: TConfItem read GetItemConfs write SetItemConfs;
    property ItemByIndex[Index: Integer]: TConfItem read GetItemByIndex write SetItemByIndex;
    property ItemNames: TStrings read FItemStrings;
    property Name: String read FName;
  end;

  { TUserConf }

  TUserConf = class(TConfItem)
  private
    FEmail: String;
    FID: Integer;
    FPassword: String;
    FTelegram: Int64;
  public
    procedure AssignTo(Dest: TPersistent); override;
  published
    property ID: Integer read FID write FID;
    property Email: String read FEmail write FEmail;
    property Telegram: Int64 read FTelegram write FTelegram;
    property Password: String read FPassword write FPassword;
  end;

  { TUserListConf }

  TUserListConf = class(TCollectionConf)
  private
    FAdminUser: TUserConf;
  protected
    procedure LoadItemConfig(const AnItemName: String; AnItem: TConfItem); override;
  public
    constructor Create; overload;
    function FindUserFromTelegram(UserID: Int64): TUserConf;
    property AdminUser: TUserConf read FAdminUser;
  end;

  { TTemplateConf }

  TTemplateConf = class(TConfItem)
  private
    FProperties: TStrings;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    property Properties: TStrings read FProperties write FProperties;
  end;

  { TTemplateList }

  TTemplateList = class(TCollectionConf)
  protected
    procedure LoadItemConfig(const AnItemName: String; AnItem: TConfItem); override;
  public
    constructor Create; overload;
  end;

var
  UserListConfs: TUserListConf;
  TemplateList: TTemplateList;
  ConfDir, AppDir, TmltDir, AppName: String;

implementation

uses
  typinfo, config
  ;

var
  MyCriticalSection: TRTLCriticalSection;

const
  PropKinds:TTypeKinds = tkProperties;
  s_def='default';

{ TTemplateList }

procedure TTemplateList.LoadItemConfig(const AnItemName: String;
  AnItem: TConfItem);
var
  TplDir, PropsFile: String;
begin
  inherited;
  TplDir:=Conf.TemplatesDirectory+AnItemName+DirectorySeparator;
  if DirectoryExists(TplDir) then
  begin
    PropsFile:=TplDir+'properties.conf';
    if FileExists(PropsFile) then
      TTemplateConf(AnItem).Properties.LoadFromFile(PropsFile);
  end
end;

constructor TTemplateList.Create;
begin
  FName:='templates';
  inherited Create(TTemplateConf);
end;

{ TTemplateConf }

constructor TTemplateConf.Create(ACollection: TCollection);
begin
  inherited;
  FProperties:=TStringList.Create;
end;

destructor TTemplateConf.Destroy;
begin
  FProperties.Free;
  inherited Destroy;
end;

{ TUserConf }

procedure TUserConf.AssignTo(Dest: TPersistent);
var
  ItemConf: TUserConf;
begin
  if not (Dest is TUserConf) then
  begin
    inherited AssignTo(Dest);
    Exit;
  end;
  inherited AssignTo(Dest);
  ItemConf:=TUserConf(Dest);
  ItemConf.FEmail:=FEmail;
  ItemConf.FTelegram:=FTelegram;
  ItemConf.FPassword:=FPassword;
  ItemConf.FID:=FID;
  //ItemConf.FDomainList:=FDomainList;
end;

{ TUserListConf }

procedure TUserListConf.LoadItemConfig(const AnItemName: String;
  AnItem: TConfItem);
begin
  if not Assigned(FAdminUser) and SameText(AnItemName, Conf.Admin.Username) then
    FAdminUser:=TUserConf(AnItem);
  inherited LoadItemConfig(AnItemName, AnItem);
end;

constructor TUserListConf.Create;
begin
  FName:='users';
  inherited Create(TUserConf);
end;

function TUserListConf.FindUserFromTelegram(UserID: Int64): TUserConf;
var
  u: TCollectionItem;
begin
  Result:=nil;
  for u in self do
    if TUserConf(u).Telegram=UserID then
      Exit(TUserConf(u));
end;

{ TCollectionConf }

function TCollectionConf.GetItemConfs(ItemName: String): TConfItem;
var
  i: Integer;
begin
  i:=FItemStrings.IndexOf(ItemName);
  if i=-1 then
    Exit(nil);
  Result:=TConfItem(FItemStrings.Objects[i]);
end;

function TCollectionConf.DebugErrorLocate: Boolean;
var
  i: Integer;
  s: String;
begin
  Result:=False;
  for s in FItemStrings do
  begin
    i:=FItemStrings.IndexOf(s);
    if i=-1 then Exit(True)
  end;
end;

function TCollectionConf.GetItemByIndex(Index: Integer): TConfItem;
begin
  Result:=TConfItem(FItemStrings.Objects[Index]);
end;

procedure TCollectionConf.LoadItemConfig(const AnItemName: String;
  AnItem: TConfItem);
var
  i: Integer;
begin {
  if not FIni.SectionExists(AnItemName) then
    Exit; }
  FPropsStorage.AObject:=AnItem;
  FPropsStorage.Section:=AnItemName;
  for i:=FPropList.Count-1 downto 0 do
    FPropsStorage.LoadAnyProperty(FPropList[i]);
end;

procedure TCollectionConf.PropsStorageEraseSection(const ASection: string);
begin
  FIni.EraseSection(ASection);
end;

function TCollectionConf.PropsStorageReadString(const ASection, Item,
  Default: string): string;
begin
  Result:=FIni.ReadString(ASection, Item, Default);
end;

procedure TCollectionConf.PropsStorageWriteString(const ASection, Item,
  Value: string);
begin
  FIni.WriteString(ASection, Item, Value);
end;

procedure TCollectionConf.SetItemByIndex(Index: Integer; AValue: TConfItem);
begin
  FItemStrings.Objects[Index]:=AValue;
end;

procedure TCollectionConf.SetItemConfs(ItemName: String; AValue: TConfItem);
var
  Buffer: TConfItem;
  i: Integer;
begin
  i:=FItemStrings.IndexOf(ItemName);
  if i=-1 then
  begin
    Buffer:=TConfItem(Add);
    FItemStrings.AddObject(ItemName, Buffer);
  end
  else begin
    Buffer:=TConfItem(FItemStrings.Objects[i]);
    if not Assigned(Buffer) then
      Buffer:=TConfItem(Add);
  end;
  Buffer.Assign(AValue);
end;

procedure TCollectionConf.SetItemStrings(AValue: TStrings);
var
  AnItemName: String;
  AnItem: TConfItem;
begin
  if FItemStrings=AValue then Exit;
  Clear;
  FItemStrings.Clear;
  for AnItemName in AValue do
  begin
    AnItem:=TConfItem(Add);
    FItemStrings.AddObject(AnItemName, AnItem);
    AnItem.Name:=AnItemName;
  end;
end;

procedure TCollectionConf.StoreItemConfig(const AnItemName: String;
  AnItem: TConfItem);
var
  i: Integer;
begin
  FPropsStorage.AObject:=AnItem;
  FPropsStorage.Section:=AnItemName;
  for i:=FPropList.Count-1 downto 0 do
    FPropsStorage.StoreAnyProperty(FPropList[i]);
end;

procedure TCollectionConf.StoreItemConfig(const AnItemName: String;
  NameValuePairs: TStrings);
var
  i: Integer;
begin
  for i:=NameValuePairs.Count-1 downto 0 do
    FIni.WriteString(AnItemName, NameValuePairs.Names[i], NameValuePairs.ValueFromIndex[i]);
end;

procedure TCollectionConf.ItemsAdd(AStrings: TStrings);
begin
  EnterCriticalsection(MyCriticalSection);
  try
    FItemStrings.AddStrings(AStrings);
    SaveList;
    Load;
  finally
    LeaveCriticalsection(MyCriticalSection);
  end;
end;

procedure TCollectionConf.ItemsAdd(AStrings: TStrings; out Was, Became: Integer
  );
begin
  Was:=FItemStrings.Count;
  ItemsAdd(AStrings);
  Became:=FItemStrings.Count;
end;

procedure TCollectionConf.ItemsDelete(AStrings: TStrings);
var
  s: String;
  i: Integer;
begin
  //PEncodeDomains(AStrings);
  EnterCriticalsection(MyCriticalSection);
  try
    for s in AStrings do
    begin
      i:=FItemStrings.IndexOf(s);
      if i>-1 then
        FItemStrings.Delete(i);
    end;
    SaveList;
    Load;
  finally
    LeaveCriticalsection(MyCriticalSection);
  end;
end;

procedure TCollectionConf.ItemsDelete(AStrings: TStrings; out Was,
  Became: Integer);
begin
  Was:=FItemStrings.Count;
  ItemsDelete(AStrings);
  Became:=FItemStrings.Count;
end;

procedure TCollectionConf.ItemsEdit(ItemNames, NameValuePairs: TStrings);
var
  s: String;
begin
  //PEncodeDomains(ItemNames);
  EnterCriticalsection(MyCriticalSection);
  try
    FIni:=TMemIniFile.Create(ConfDir+FName+'.ini');
    try
      for s in ItemNames do
        StoreItemConfig(s, NameValuePairs);
      FIni.UpdateFile;
    finally
      FreeAndNil(FIni);
    end;
    LoadConfig;
  finally
    LeaveCriticalsection(MyCriticalSection);
  end;
end;

constructor TCollectionConf.Create(AItemClass: TCollectionItemClass);
begin
  inherited Create(AItemClass);
  FItemStrings:=TStringList.Create;
  TStringList(FItemStrings).Sorted:=Conf.Debug.SortedStrings;
  TStringList(FItemStrings).Duplicates:=dupIgnore;
  FPropsStorage:=TPropsStorage.Create;
  FPropsStorage.OnReadString:=@PropsStorageReadString;
  FPropsStorage.OnWriteString:=@PropsStorageWriteString;
  FPropsStorage.OnEraseSection:=@PropsStorageEraseSection;
  FDefItem:=TConfItemClass(ItemClass).Create(nil);
  FDefItem.Name:=s_def;
  FPropList:=TPropInfoList.Create(FDefItem, PropKinds);
end;

destructor TCollectionConf.Destroy;
begin
  FreeAndNil(FPropList);
  FreeAndNil(FDefItem);
  FreeAndNil(FPropsStorage);
  FreeAndNil(FItemStrings);
  inherited Destroy;
end;

function TCollectionConf.Exists(const ItemName: String): Boolean;
begin
  Result:=FItemStrings.IndexOf(ItemName)<>-1;
end;

procedure TCollectionConf.LoadConfig;
var
  AnItemName: String;
  AItemConfig: TConfItem;
begin
  FIni:=TMemIniFile.Create(ConfDir+FName+'.ini');
  LoadItemConfig(s_def, FDefItem);
  for AnItemName in FItemStrings do
  begin
    AItemConfig:=ItemConfs[AnItemName];
    if Assigned(FDefItem) then
      AItemConfig.Assign(FDefItem);
    LoadItemConfig(AnItemName, AItemConfig);
  end;
  FreeAndNil(FIni);
end;

procedure TCollectionConf.LoadList;
var
  AFileName: String;
  AStrings: TStringList;
begin
  AFileName:=ConfDir+FName+'.lst';
  AStrings:=TStringList.Create;
  try
    if FileExists(AFileName) then
      AStrings.LoadFromFile(AFileName);
    ItemStrings:=AStrings;
  finally
    AStrings.Free;
  end;
end;

procedure TCollectionConf.SaveList(const Prefix: String);
begin
  FItemStrings.SaveToFile(ConfDir+Prefix+FName+'.lst');
end;

procedure TCollectionConf.Load;
begin
  LoadList;
  LoadConfig;
end;

{ TConfItem }

procedure TConfItem.SetDefConfig(AValue: Boolean);
begin
  if FDefConfig=AValue then Exit;
  FDefConfig:=AValue;
end;

procedure TConfItem.SetName(AValue: String);
begin
  if FName=AValue then Exit;
  FName:=AValue;
end;

procedure TConfItem.AssignTo(Dest: TPersistent);
begin
  if not (Dest is TConfItem) then
  begin
    inherited AssignTo(Dest);
    Exit;
  end;
  TConfItem(Dest).FDefConfig:=FDefConfig;
end;

procedure TConfItem.InitConfig;
begin
  FDefConfig:=True;
end;

initialization
  InitCriticalSection(MyCriticalSection{%H-});
  try
    TemplateList:=TTemplateList.Create;
    TemplateList.Load;
    UserListConfs:=TUserListConf.Create;
    UserListConfs.Load;
  except
    on E:Exception do
      AppLogger.Log(etError, E.ClassName+': '+E.Message)
  end;

finalization
  FreeAndNil(UserListConfs);
  FreeAndNil(TemplateList);
  DoneCriticalsection(MyCriticalSection);

end.

