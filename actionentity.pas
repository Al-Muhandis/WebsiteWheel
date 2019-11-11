unit actionentity;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, actionadmin, fpjson
  ;

type

  { TGetItemAction }

  TGetItemAction=class(TAdminAction)
  private
    FItemID: Int64;
  protected
    procedure Parse;override;
  public
    procedure DoAcceptFiles; virtual; abstract;
    property ItemID: Int64 read FItemID write FItemID;
    procedure Get; override;
  end;

  { TEditItemAction }

  TEditItemAction=class(TGetItemAction)
  public
    constructor Create; override;
    procedure Post; override;
  end;

  { TNewItemAction }

  TNewItemAction=class(TEditItemAction)
  public
    constructor Create; override;
  end;

implementation

uses
  BrookUtils, config, tableentities
  ;

{ TGetItemAction }

procedure TGetItemAction.Parse;
var
  AnItem: String;
  Item: TDBEntity;
  aAlert, aAlertType: String;
  aOPF: IOPFInterface;
  aFound: Boolean;
begin
  AnItem:=Variable['item'];
  Item:=nil;
  FItemID:=StrToInt64Def(AnItem, 0);
  aOPF:=ORM.OPF[PathAlias];
  Item:=aOPF.GetEntityByID(FItemID, aFound);
  if not aFound and not AnItem.IsEmpty then
  begin
    HttpResponse.Code:=404;
    Exit;
  end;
  if AnItem=EmptyStr then
  begin
    Title:='Creating a new entity. '+Conf.App.Title;
  end
  else begin
    Title:='Save the entity #'+FItemID.ToString+'. '+Conf.App.Title;
  end;
  if Fields.Count>0 then
  begin
    GetFields(Item);
    if Files.Count>0 then
      DoAcceptFiles;
    try
      if AnItem=EmptyStr then
      begin
        aOPF.AddEntity;                       // new entity
        Item.id:=ORM.LastInsertID;
        aAlert:='The entry was successfully added!';
        Redirect('/'+Conf.Admin.UriAlias+'/'+PathAlias+'/'+'edit'+'/'+Item.id.ToString, 302)
      end
      else begin
        aOPF.ModifyEntity;                    // modify entity
        aAlert:='The entry was successfully saved!';
      end;
      aOPF.ApplyEntity;
      aAlertType:='success';
    except
      on E: Exception do
      begin
        aAlert:='['+ClassName+'] '+E.ClassName+': '+E.Message;
        AppLogger.Error(aAlert);
        aAlertType:='danger';
      end;
    end;
  end
  else begin
    aAlertType:='light';
    aAlert:=EmptyStr;
  end;
  JTemplate.AddFields(Item, 'item');
  JTemplate.SetValues(Params, 'item');
  JTemplate.Parser.Fields.Add('alert', aAlert);
  JTemplate.Parser.Fields.Add('alerttype', aAlertType);
  JTemplate.Parser.Fields.Add('entity', aOPF.GetEntityCaption);
  JTemplate.Parser.Fields.Add('entities', aOPF.GetEntitiesCaption);
  inherited Parse;
end;

procedure TGetItemAction.Get;
begin

end;

{ TNewItemAction }

constructor TNewItemAction.Create;
begin
  inherited Create;
  TplSuffix:='new';
  Title:=Title+'. New';
end;

{ TEditItemAction }

constructor TEditItemAction.Create;
begin
  inherited Create;
  TplSuffix:='edit';
  Title:=Title+'. Edit';
end;

procedure TEditItemAction.Post;
begin

end;

end.

