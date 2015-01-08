BASE_DIR = "D:\WSH"

Set ToHTML = CreateObject("SysTools.StToHTML")
Set S = CreateObject("SysTools.StString")

ToHTML.License("STD123456ABCDEF")
S.License("STD123456ABCDEF")

Set objArgs = WScript.Arguments

For I = 0 to objArgs.Count - 1
  ToHTML.LoadFromFile(objArgs(I))
  
  select case UCase(S.JustExtension(objArgs(I))) 
    case "C", "CPP", "H"
      ToHTML.CommentMarkers.Clear
      ToHTML.CommentMarkers.Add("/*=*/;<I>;</I>")
      ToHTML.CommentMarkers.Add("//=;<I>;</I>")
      ToHTML.Keywords.LoadFromFile(BASE_DIR & "\KWBUILDR.TXT")    
    
    case "PAS"
      ToHTML.CommentMarkers.Clear
      ToHTML.CommentMarkers.Add("{=};<I>;</I>")
      ToHTML.CommentMarkers.Add("(*=*);<I>;</I>")
      ToHTML.Keywords.LoadFromFile(BASE_DIR & "\KWPASCAL.TXT")
  end select
  
  ToHTML.GenerateHTML
  ToHTML.SaveToFile(objArgs(I) & ".htm")
  ToHTML.Clear
Next

Set objArgs = nothing
Set S = nothing
Set ToHTML = nothing




