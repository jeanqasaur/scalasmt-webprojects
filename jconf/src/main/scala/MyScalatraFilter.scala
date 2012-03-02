import cap.jeeves._
import cap.jeeves.jconf.backend._
import cap.jeeves.jconf.frontend._
import JConfBackend._

import org.scalatra._
import org.scalatra.fileupload.FileUploadSupport
import org.squeryl.adapters.MySQLAdapter
import org.squeryl.PrimitiveTypeMode._
import org.squeryl.Session
import org.squeryl.SessionFactory
import java.net.URL
import java.io.File
import scalate.ScalateSupport

class MyScalatraFilter
extends ScalatraFilter with ScalateSupport with FileUploadSupport
with JeevesLib {
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
        // TODO: Insert try/catch block here that redirects to index.
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

  get("/") { redirect("/index") }

  get("/index") {
    ifLoggedIn{ (user: ConfUser) =>
      val ctxt = getContext(user);

      renderPageWithUser("index.ssp", user
          , Map("name" -> user.showName(getContext(user))
               , "submittedPapers" -> user.showSubmittedPapers(ctxt)
               , "reviewPapers" -> user.showReviewPapers(ctxt)
               , "reviews" -> user.showReviews(ctxt)))
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
 
          // E-mail the user about the password.
          u.emailPassword();

          session("user") = u;
          redirect("edit_profile")
      }
    } else {
      // Assuming otherwise we are logging in...
      loginUser(params("username"), params("password")) match {
        case Some(user) =>
          session("user") = user
          redirect("/")
        case None =>
          loginRedirect("Incorrect username or password.")
     }
   }
  }

  get("/login") {
    session.get("user") match {
      case Some(user) => redirect("/")
      case None => renderPage("login_screen.ssp")
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

  /* Updating and displaying papers. */
  def getPaper(paperId: Int): PaperRecord = {
    getPaperById(paperId) match {
      case Some(p) =>  p
      case None => throw new JConfInternalError
    }
  }
  post("/paper") {
    ifLoggedIn { (user: ConfUser) =>
      val paper: PaperRecord = getPaper(params("id").toInt)

      // Write paper to place on disk.
      val uploadedFile = fileParams("thefile")
      val filename = {
        if (uploadedFile != null) {
          uploadedFile.getName()
        } else {
          ""
        }
      }
      // Write file to disk...
      if (uploadedFile != null) {
        uploadedFile.write(new File("papers/" + filename))
      }

      JConfUtil.updatePaperRecord(paper, filename, params)
      renderPageWithUser("paper.ssp", user, Map("paper" -> paper))
    }
  }
  get("/paper") {
    ifLoggedIn { (user: ConfUser) =>
      var paper: PaperRecord = getPaper(params("id").toInt)
      renderPageWithUser("paper.ssp", user, Map("paper" -> paper))
    }
  }

  /* Updating and displaying reviews. */
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

      val paper: PaperRecord = getPaper(paperId)
      val curReview = {
        JConfTables.getReviewByPaperReviewer(paperId, user.uid.toInt) match {
          case Some(review) => review
          case None => paper.addReview(user)
        } 
      }
      
      renderPageWithUser("edit_review.ssp", user
        , Map("paper" -> paper
          , "review" -> curReview
          , "reviewer" -> user
                ))
    }
  }

  get("/new_paper") {
    ifLoggedIn { (user: ConfUser) =>
      val paper: PaperRecord = JConfBackend.addPaper("Untitled", List(user))
      renderPageWithUser("edit_paper.ssp", user, Map("paper" -> paper))
    }
  }
  get("/edit_paper") {
    ifLoggedIn { (user: ConfUser) =>
      val paper: PaperRecord =
        JConfBackend.getPaperById(params("id").toInt) match {
          case Some(p) =>  p
          case None => throw new JConfInternalError
        }

      renderPageWithUser("edit_paper.ssp", user, Map("paper" -> paper))
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
