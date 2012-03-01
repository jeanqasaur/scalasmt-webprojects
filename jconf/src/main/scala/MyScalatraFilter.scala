import cap.jeeves._
import cap.jeeves.jconf.backend._
import cap.jeeves.jconf.frontend._
import JConfBackend._

import org.scalatra._
import org.squeryl.adapters.MySQLAdapter
import org.squeryl.PrimitiveTypeMode._
import org.squeryl.Session
import org.squeryl.SessionFactory
import java.net.URL
import scalate.ScalateSupport

class MyScalatraFilter
extends ScalatraFilter with ScalateSupport with JeevesLib {
  class JConfInternalError extends Exception

  System.setProperty("smt.home", "/opt/z3/bin/z3")

  Init.initDB ()
  Init.initDummyUsers ()

  val path = "/WEB-INF/views/"
  val title = "jeeves social net"
  val paperStage = Submission
  def emptyName = ""

  def ifLoggedIn(displayPage: ConfUser => Any) {
    session.get("user") match {
      case Some(u) =>
        val user = u.asInstanceOf[ConfUser];
        displayPage(user)
      case None => redirect("login")
    }
  }

  def renderPage(page: String, args: Map[String, Any] = Map()): Unit = {
    contentType = "text/html"
    templateEngine.layout(path + page, args)
  }
  def renderPageWithUser(
    page: String, user: ConfUser, args: Map[String, Any] = Map()): Unit = {
    var newArgs = args;
    newArgs += ("user" -> user)
    newArgs += ("ctxt" -> getContext(user));
    renderPage(page, newArgs)
  }

  def loginRedirect(msg: String) = {
    renderPage("login_screen.ssp", Map("errorMsg" -> msg))
  }

  def renderRadioButtons(fieldName: String, fieldValue: Int): String = {
    println("Rendering radio buttons");
    val rendered: String =
      templateEngine.layout(path + "snippets/radio_button.ssp"
        , Map("fieldName" -> fieldName, "fieldValue" -> fieldValue))
    println(rendered);
    println("Done printing rendered");
    rendered
  }

  get("/") {
    ifLoggedIn{ (user: ConfUser) =>
      renderPage("index.ssp"
          , Map("name" -> user.showName(getContext(user))
               , "papers" -> searchByAuthor(user)))
    }
  }

  post("/login_user") {
    // If we are logging in...
    if (params.exists((param: (String, String)) => param._1 == "signup")) {
      JConfTables.getDBConfUserByEmail(params("username")) match {
        // User already exists in database.  Redirect to the login page.
        case Some(user) =>
          loginRedirect("Username already exists in database.")
        case None =>
          // Everyone is author status by default.
          println(params("username") + " does not already exist...");
          // TODO: Change this?
          val role: UserStatus = AuthorStatus

          // Make a new user and add them to the database.
          val u =
            addUser(params("username")
            , ""
            , JConfUtil.generatePassword()
            , role);
        session("user") = u;
        redirect("edit_profile")
      }
    } else {
      // Assuming otherwise we are logging in...
      loginUser(params("username"), params("password")) match {
        case Some(user) =>
          session("user") = user
          renderPage("index.ssp"
            , Map( "name" -> user.showName(getContext(user))
            , "papers" -> searchByAuthor(user)))
        case None =>
          loginRedirect("Incorrect username or password.")
     }
   }
  }

  get("/login") {
    session.get("user") match {
      case Some(user) => redirect("*")
      case None => renderPage("login_screen.ssp")
    }
  }

  get("/papers") {
    ifLoggedIn { (user: ConfUser) =>
      val ctxt = getContext(user);
      renderPage("papers.ssp"
          , Map(
            "user" -> user, "ctxt" -> ctxt
          , "submittedPapers" -> user.showSubmittedPapers(ctxt)
          , "reviewPapers" -> user.showReviewPapers(ctxt)
          , "reviews" -> user.showReviews(ctxt)))
    }
  }

  // Update and display profile.
  post("/profile") {
    ifLoggedIn { (user: ConfUser) =>
      // TODO: Need to update other fields as well.
      JConfUtil.updateUserProfile(user, params);
      session("user") = user;
      renderPageWithUser("profile.ssp", user)
    }
  }
  get("/profile") {
    ifLoggedIn { (user: ConfUser) =>
      var curUser: ConfUser = {
        if (!(params.exists(_ == "id"))) {
          user;
        } else {
          getUserById(params("id").toInt) match {
            case Some(idUser) => idUser
            case None => null
          }
        }
      }
      renderPage("profile.ssp"
          , Map("user" -> curUser, "ctxt" -> getContext(user)))
    }
  }

  // TODO: Edit review?
  get("/paper") {
    ifLoggedIn { (user: ConfUser) =>
      var paper: PaperRecord = defaultPaper;
      getPaperById(multiParams("id").head.toInt) match {
        case Some(p) =>  paper = p
        case None => ()
      }
      renderPageWithUser("paper.ssp", user, Map("paper" -> paper))
    }
  }

  def getReviewInfo(paperId: Int, reviewerId: Int)
    : (PaperRecord, ConfUser, PaperReview) = {
    val paper: PaperRecord =
      getPaperById(paperId) match {
        case Some(p) => p
        case None =>
          throw new JConfInternalError
      }

    val reviewer: ConfUser =
      getUserById(reviewerId) match {
        case Some(idUser) => idUser
        case None => throw new JConfInternalError
      }

    val review: PaperReview =
      JConfTables.getReviewByPaperReviewer(paperId, reviewerId) match {
        case Some(review) => review
        case None => throw new JConfInternalError
      }
    (paper, reviewer, review)
  }
  post("/review") {
    ifLoggedIn { (user: ConfUser) =>
      val paperId = params("paperId").toInt;
      val reviewerId = params("reviewerId").toInt;
      val (paper, reviewer, review) = getReviewInfo(paperId, reviewerId)
      JConfUtil.updatePaperReview(review, params)

      renderPageWithUser("review.ssp", user,
        Map("paper" -> paper, "reviewer" -> reviewer, "review" -> review))
    }
  }
  get("/review") {
    ifLoggedIn { (user: ConfUser) =>
      val paperId = params("paperId").toInt;
      val reviewerId = params("reviewerId").toInt;
      val (paper, reviewer, review) = getReviewInfo(paperId, reviewerId)
      renderPageWithUser("review.ssp", user,
        Map("paper" -> paper, "reviewer" -> reviewer, "review" -> review))
    }
  }

  get("/edit_profile") {
    ifLoggedIn { (user: ConfUser) =>
      renderPageWithUser("edit_profile.ssp", user)
    }
  }
  get("/edit_review") {
    ifLoggedIn { (user: ConfUser) =>
      val paperId = params("paperId").toInt;

      JConfTables.getDBPaperRecord(paperId) match {
        case Some(paper) =>
          val curReview = {
            JConfTables.getReviewByPaperReviewer(paperId, user.uid.toInt) match {
              case Some(review) => review
              case None => paper.addReview(user)
            }
          }
          val ctxt = getContext(user);
          renderPageWithUser("edit_review.ssp", user
            , Map("paper" -> paper
              , "review" -> curReview
              , "reviewer" -> user
                ))
        case None => redirect("/")
      }
    }
  }

  get("/logout") { session.invalidate; redirect("login") }

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
