unit unit1;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  Buttons, ExtCtrls, mymetafile;

type

  TForm1 = class(TForm)
    Button1: TButton;
    Image1: TImage;
    procedure Button1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation




{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  MyMetafile: TMetafile;
  a: trect;
begin
     MyMetafile := TMetafile.Create;
     MyMetafile.Width := 100;
     MyMetafile.Height := 100;

     with TMetafileCanvas.CreateWithComment(MyMetafile, form1.canvas.handle,'Author', 'Made This') do
     try
        Brush.Color := clRed;
        Rectangle(0,0,100,100);
        TextOut(   50, 50, 'Welcome to Metafile');
        draw(0,0, image1.Picture.Bitmap);
     finally
       Free;
     end;
     Form1.Canvas.Draw(0,0,MyMetafile); //draw normal
     a.Left := 10;
     a.Top := 150;
     a.Bottom := 350;
     a.Right := 300;
     Form1.Canvas.StretchDraw(a,MyMetafile);  //stretch
end;


initialization
  {$I unit1.lrs}

end.

