unit actionentities;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, actionadmin, fpjson
  ;

type

  { TListItemAction }

  TListItemAction=class(TAdminAction)
  protected
    procedure Parse;override;
  public
    constructor Create; override;
    function GetMainModFile: String; override;
    procedure Get; override;
  end;

implementation

uses
  BrookUtils, config, tableentities
  ;

{ TListItemAction }

procedure TListItemAction.Parse;
var
  aOPF: IOPFInterface;
begin
  aOPF:=ORM.OPF[PathAlias];
  if not Assigned(aOPF) then
  begin
    HttpResponse.Code:=404;
    HttpResponse.CodeText:='Entity not found';
    Exit;
  end;
  JTemplate.Parser.Fields.Add('entity', aOPF.GetEntityCaption);
  JTemplate.Parser.Fields.Add('entities', aOPF.GetEntitiesCaption);
  inherited Parse;
end;

constructor TListItemAction.Create;
begin
  inherited Create;
  TplSuffix:='table';
end;

function TListItemAction.GetMainModFile: String;
begin
  Result:=inherited GetMainModFile;
end;

procedure TListItemAction.Get;
begin

end;

end.

