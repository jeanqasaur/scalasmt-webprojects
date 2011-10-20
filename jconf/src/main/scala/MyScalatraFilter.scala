import cap.jeeves._
import cap.jeeves.jconf._
import JConfBackend._

import org.scalatra._
import java.net.URL
import scalate.ScalateSupport

class MyScalatraFilter extends ScalatraFilter with ScalateSupport with JeevesLib {
  val path = "/WEB-INF/views/"
  val title = "jeeves social net"
  val paperStage = Submission
  def emptyName = Title("")

  def mkUser( userName : String, name: String
            , pwd: String, userStatus : UserStatus)
    : ConfUser = {
    val u = new ConfUser(Username (userName), Name(name), pwd, userStatus);
    addUser(u);
    u
  }

  // Add some dummy users.
  val pcArmando =
    mkUser("armando", "Armando Solar-Lezama", "armando", PCStatus);
  val authorJean =
    mkUser("jeanyang", "Jean Yang", "jean", AuthorStatus);
  val reviewerKuat =
    mkUser("kuat", "Kuat Yessenov", "kuat", ReviewerStatus);

  // Add some dummy papers.
  val paper0Name = Title("A Language for Automatically Enforcing Privacy");
  val paper0 = addPaper(paper0Name, List(authorJean), Nil);
  assignReview(paper0, reviewerKuat);

  def checkLoggedIn() {
    session.get("user") match {
      case Some(user) => ()
      case None => redirect("login")
    }
  }

  def renderPage(page: String, args: Map[String, Any] = Map()) {
    contentType = "text/html"
    templateEngine.layout(path + page, args)
  }

  get("/") {
    session.get("user") match {
      case Some(user) =>
        val cur_user = user.asInstanceOf[ConfUser];
        contentType = "text/html";
        templateEngine.layout(path + "index.ssp"
          , Map("name" -> cur_user.name.name
                    , "papers" -> searchByAuthor(cur_user)))
      case None => redirect("login")
    }
  }

  post("/login_user") {
    println(params("username"));
    loginUser(params("username"), params("password")) match {
      case Some(user) =>
        session("user") = user
        renderPage("index.ssp"
          , Map( "name" -> user.name.name
          , "papers" -> searchByAuthor(user)))
      case None => redirect("login")
    }
  }

  post("/create_user") {
    /* If we are adding a new user. */
    val role: UserStatus =
      params("role") match {
        case "Author" => AuthorStatus
        case "Reviewer" => ReviewerStatus
        case "PC" => PCStatus
      }

    // Make a new user and add them to the database.
    val u =
      new ConfUser( Username(params("username")), Name(params("yourname"))
                  , params("password"), role );
    addUser(u);
    session("user") = u;
    val context = new ConfContext(u, paperStage);
    val pwd = u.getPassword(context);
    println(pwd);

    renderPage("index.ssp"
      , Map("user" -> u, "name" -> u.name.name, "role" -> params("role")
           , "papers" -> searchByAuthor(u) )
       )
  }

  get("/login") {
    session.get("user") match {
      case Some(user) => redirect("*")
      case None =>
        contentType = "text/html"
        templateEngine.layout(path + "login_screen.ssp")
    }
  }

  get("/signup") {
    contentType = "text/html"
    templateEngine.layout(path + "signup.ssp")
  }

  post("/signup_confirm") {
    contentType = "text/html"
    templateEngine.layout(path + "signup_confirm.ssp")
  }

  get("/profile") {
    session.get("user") match {
      case Some(user) =>
        contentType = "text/html"
        templateEngine.layout(path + "profile.ssp")
      case None => redirect("login")
    }
  }

  get("/logout") {
    // Unbind user.
    session.invalidate
    redirect("login")
  }

  notFound {
    // If no route matches, then try to render a Scaml template
    val templateBase = requestPath match {
      case s if s.endsWith("/") => s + "index"
      case s => s
    }
    val templatePath = "/WEB-INF/scalate/templates/" + templateBase + ".scaml"
    servletContext.getResource(templatePath) match {
      case url: URL => 
        contentType = "text/html"
        templateEngine.layout(templatePath)
      case _ => 
        filterChain.doFilter(request, response)
    } 
  }
}
