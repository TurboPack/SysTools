<%@ Language=VBScript %>
<OBJECT RUNAT="SERVER" PROGID="systools.sttohtml" ID="StToHTML"></OBJECT>
<OBJECT RUNAT="SERVER" PROGID="systools.ststringlist" ID="StStringList"></OBJECT>
<%
  ShowConverted = False
  
  if UCase(Request("StToHTMLGen")) = UCase("Convert") then
     StToHTML.License("STD123456ABCDEF")
    StStringList.License("STD123456ABCDEF")

    Source = Request("Source")
    
    select case Request("ConvertFrom")
      case "PASCAL"
        StToHTML.CommentMarkers.Add("{=};<I>;</I>")
        StToHTML.CommentMarkers.Add("(*=*);<I>;</I>")
        StToHTML.CommentMarkers.Add("//=;<I>;</I>")
        StToHTML.StringMarkers.Add("'=';<FONT COLOR=""RED"">;</FONT>")
        
        StStringList.Text = Source
        StToHTML.Stream = StStringList.Stream 
        
        StToHTML.Keywords.LoadFromFile(Server.MapPath("kwpascal.txt"))
        StToHTML.GenerateHTML 
      
        ShowConverted = True
      case "C"
        StToHTML.CommentMarkers.Add("(*=*);<I>;</I>")
        StToHTML.CommentMarkers.Add("/*=*/;<I>;</I>")
        StToHTML.CommentMarkers.Add("//=;<I>;</I>")
        
        StStringList.Text = Source
        StToHTML.Stream = StStringList.Stream 

        StToHTML.Keywords.LoadFromFile(Server.MapPath("kwbuildr.txt"))
        StToHTML.GenerateHTML 
      
        ShowConverted = True
      case else
      
    end select
  end if
%>
<HTML>
<HEAD>
  <TITLE>SysTools Example: StToHTML</TITLE>
</HEAD>
<BODY>

<P>&nbsp;</P>


  <FORM METHOD="POST" ID="FORM1" NAME="FORM1">
    <TEXTAREA COLS="78" ROWS="15" NAME="Source" ID="Source"><%=Source%></TEXTAREA><BR>
    <INPUT TYPE="Radio" NAME="ConvertFrom" ID="ConvertFrom" VALUE="PASCAL" CHECKED>Convert from Pascal source
    <INPUT TYPE="Radio" NAME="ConvertFrom" ID="ConvertFrom" VALUE="C">Convert from C/C++ source
    <BR><BR>
    <INPUT TYPE="SUBMIT" NAME="StToHTMLGen" ID="StToHTMLGen" VALUE="Convert">
    <INPUT TYPE="RESET">
  </FORM>
  
  <% if ShowConverted then %>
  <HR>
  <% Response.BinaryWrite StToHTML.Stream %>    
  <% end if %>
</BODY>
</HTML>

<% 
  On Error Resume Next
  Set StStringList = nothing 
  Set StToHTML = nothing 
%>
