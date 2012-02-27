import cap.jeeves._
import cap.jeeves.jconf._
import JConfBackend._

import org.scalatra._
import java.net.URL
import scalate.ScalateSupport

class MyScalatraFilter extends ScalatraFilter with ScalateSupport with JeevesLib {
  System.setProperty("smt.home", "/opt/z3/bin/z3")

  val path = "/WEB-INF/views/"
  val title = "jeeves social net"
  val paperStage = Submission
  def emptyName = ""

  def mkUser( userName : String, name: String
            , pwd: String, userStatus : UserStatus): ConfUser = {
    addUser(userName, name, pwd, userStatus)
  }

  // Add some dummy users.
  val pcArmando =
    mkUser("armando", "Armando Solar-Lezama", "armando", PCStatus);
  val authorJean =
    mkUser("jeanyang", "Jean Yang", "jean", ReviewerStatus);
  val reviewerKuat =
    mkUser("kuat", "Kuat Yessenov", "kuat", ReviewerStatus);

  // Add some dummy papers.
  val paper0Name = "A Language for Automatically Enforcing Privacy";
  val paper0 = addPaper(paper0Name, List(authorJean), Nil);
  assignReview(paper0, reviewerKuat);

  val paper1Name = "Matchmaker";
  val paper1 = addPaper(paper1Name, List(reviewerKuat), Nil);
  assignReview(paper1, authorJean);

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
      case Some(u) =>
        val user = u.asInstanceOf[ConfUser];
        renderPage("index.ssp"
          , Map("name" -> user.showName(getContext(user))
               , "papers" -> searchByAuthor(user)))
      case None => redirect("login")
    }
  }

  post("/login_user") {
    println(params("username"));
    loginUser(params("username"), params("password")) match {
      case Some(user) =>
        session("user") = user
        renderPage("index.ssp"
          , Map( "name" -> user.showName(getContext(user))
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
      addUser(params("username"), params("yourname"), params("password"), role);
    session("user") = u;

    renderPage("index.ssp"
      , Map("user" -> u, "name" -> u.showName(getContext(u))
      , "role" -> params("role")
      , "papers" -> searchByAuthor(u) )
       )
  }

  get("/login") {
    session.get("user") match {
      case Some(user) => redirect("*")
      case None =>
        val tmp =
          Option(System.getProperty("smt.home")) match {
            case Some(path) => path
            case None => System.getProperty("user.home") + "/opt/z3/bin/z3"
          }
        renderPage("login_screen.ssp"
          , Map("x" -> tmp) )
    }
  }

  get("/signup") {
    contentType = "text/html"
    templateEngine.layout(path + "signup.ssp")
  }

  post("/signup_confirm") {
    renderPage("signup_confirm.ssp")
  }

  get("/papers") {
    session.get("user") match {
      case Some(u) =>
        val user = u.asInstanceOf[ConfUser];
        val ctxt = getContext(user);
        renderPage("papers.ssp"
          , Map(
            "user" -> user, "ctxt" -> ctxt
          , "submittedPapers" -> user.showSubmittedPapers(ctxt)
        ))
          // TODO: , "reviewPapers" -> user.showReviewPapers(ctxt)))
      case None => redirect("login")
    }
  }

  post("/profile") {
    session.get("user") match {
      case Some(u) =>
        val user = u.asInstanceOf[ConfUser];
        // TODO: Need to update other fields as well.
        user.setName(Name(params("name")));
        session("user") = user;
        renderPage("profile.ssp"
          , Map("user" -> user, "ctxt" -> getContext(user)))
      case None => redirect("login")
    }
  }
  get("/profile") {
    session.get("user") match {
      case Some(u) =>
        val curUser: ConfUser = u.asInstanceOf[ConfUser];
        var user: ConfUser = null;
        if (!(params.exists(_ == "id"))) {
          user = curUser;
        } else {
          getUserById(params("id").toInt) match {
            case Some(idUser) => user = idUser
            case None => ()
          }
        }
        renderPage("profile.ssp"
          , Map("user" -> user, "ctxt" -> getContext(curUser)))
      case None => redirect("login")
    }
  }

  // TODO: Edit review?
  get("/paper") {
    session.get("user") match {
      case Some(u) =>
        val user: ConfUser = u.asInstanceOf[ConfUser];
        var paper: PaperRecord = defaultPaper;
        getPaperById(multiParams("id").head.toInt) match {
          case Some(p) =>  paper = p
          case None =>
            println ("No paper found.");
            ()
        }
        renderPage("paper.ssp"
          , Map("paper" -> paper, "ctxt" -> getContext(user)))
      case None => redirect("login")
    }
  }

  get("/review") {
    session.get("user") match {
      case Some(u) =>
        val user: ConfUser = u.asInstanceOf[ConfUser];
        renderPage("review.ssp"
          , Map("ctxt" -> getContext(user)))
      case None => redirect("login")
    }
  }

  get("/edit_profile") {
    session.get("user") match {
      case Some(u) =>
        val user = u.asInstanceOf[ConfUser];
        renderPage("edit_profile.ssp"
          , Map("user" -> user, "ctxt" -> getContext(user)))
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
