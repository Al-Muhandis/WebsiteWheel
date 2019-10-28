unit cmn_mail;

{$mode objfpc}{$H+}

interface

uses
  XMailer, Classes, SysUtils;

function AppSendEMail(const Subject, Msg, Receiver: String; out ErrorStr: String): Boolean;

implementation

uses
  config
  ;

function AppSendEMail(const Subject, Msg, Receiver: String; out ErrorStr: String): Boolean;
var
  AMail: TSendMail;
begin
  Result:=False;
  AMail := TSendMail.Create;
  try
    try
      AMail.Sender := Conf.EMail.Sender;
      AMail.Receivers.Add(Receiver);
      AMail.Subject := Subject;
      AMail.Message.Add(Msg);
      AMail.Smtp.UserName := Conf.EMail.smtp.UserName;
      AMail.Smtp.Password := Conf.EMail.smtp.Password;
      AMail.Smtp.Host := Conf.EMail.smtp.Host;
      AMail.Smtp.Port := Conf.EMail.smtp.Port;
      AMail.Smtp.SSL := True;
      AMail.Smtp.TLS := True;
      AMail.Send;
      ErrorStr:=EmptyStr;
      Result:=True;
    except
      on E: Exception do
        ErrorStr:=E.ClassName+': '+E.Message;
    end;
  finally
    AMail.Free;
  end;
end;

end.

