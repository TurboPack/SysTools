<%@ Language=VBScript %>
<OBJECT RUNAT="SERVER" PROGID="systools.stmime" ID="StMime"></OBJECT>
<OBJECT RUNAT="SERVER" PROGID="systools.ststringlist" ID="StStringList"></OBJECT>
<%
  ShowConverted = False
  
  if UCase(Request("StMimeGen")) = UCase("Convert") then

    StToHTML.License("STD123456ABCDEF")
    StStringList.License("STD123456ABCDEF")

    Source = Request("Source")
    
    StStringList.Text = Source
    StMime.Encoding = Request("EncodeType")
    StMime.AddStreamAttachment StStringList.Stream, "StMimeExample.txt"
    ShowConverted = True
  end if
%>
<HTML>
<HEAD>
  <TITLE>SysTools Example: StMime</TITLE>
</HEAD>
<BODY>

<P>&nbsp;</P>


  <FORM METHOD="POST" ID="FORM1" NAME="FORM1">
    <TEXTAREA COLS="78" ROWS="15" NAME="Source" ID="Source"><%=Source%></TEXTAREA><BR>
    <BR><BR>
    Encoding method<BR>
    <INPUT TYPE="RADIO" NAME="EncodeType" ID="EncodeType" VALUE="base64" CHECKED>Base64
    <INPUT TYPE="RADIO" NAME="EncodeType" ID="EncodeType" VALUE="quoted-printable">Quoted-Printable
    <INPUT TYPE="RADIO" NAME="EncodeType" ID="EncodeType" VALUE="uuencoded">UUencoded
    <INPUT TYPE="RADIO" NAME="EncodeType" ID="EncodeType" VALUE="raw">Raw

    <BR><BR>
    <INPUT TYPE="SUBMIT" NAME="StMimeGen" ID="StMimeGen" VALUE="Convert">
    <INPUT TYPE="RESET" ID="RESET" NAME="RESET">
  </FORM>
  
  <% if ShowConverted then %>
  <HR>
  <PRE>
  <% Response.BinaryWrite StMime.Stream %>
  </PRE>
  <% end if %>
</BODY>
</HTML>

<% 
  On Error Resume Next
  Set StStringList = nothing 
  Set StMime = nothing 
%>
