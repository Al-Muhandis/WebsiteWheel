unit actionauth;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, actionadmin
  ;

type

  { TLoginAction }

  TLoginAction=class(TAdminAction)
  private
  protected
  public
    procedure Get; override;
  end;

implementation

{ TLoginAction }

procedure TLoginAction.Get;
begin

end;

end.

