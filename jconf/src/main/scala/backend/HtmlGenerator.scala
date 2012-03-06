package cap.jeeves.jconf.backend

object HtmlGenerator {
  def hyperLink(url: String, linkText: String): String = {
    "<a href=\"" + url + "\">" + linkText + "<\\a>"
  }
}
