import cap.jeeves._
import cap.jeeves.jconf.backend._
import cap.jeeves.jconf.frontend._
import JConfBackend._

import org.apache.commons.io.FileUtils
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
  class IllegalAccessError extends Exception
  class JConfInternalError extends Exception

  val date = new java.util.Date()
  println("Initializing MyScalaFilter... " + date.getTime().toString())
  System.setProperty("smt.home", "/opt/z3/bin/z3")

  Init.initDB ()
  Init.initDirectory ()
  Init.initUsers ()

  val path = "/WEB-INF/views/"
  val title = "jeeves social net"
  val paperStage = Submission
  def emptyName = ""

  def ifLoggedIn(displayPage: ConfUser => Any) {
    session.get("user") match {
      case Some(u) =>
        // TODO: Insert try/catch block here that redirects to index.
        val user = u.asInstanceOf[ConfUser];
        try {
          displayPage(user)
        } catch {
          case ia: IllegalAccessError => redirect("index?msg=illegalAccess")
          case ie: JConfInternalError => redirect("index?msg=internalError")
        }
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

  def hasParam (params: Map[String, String], p: String) = {
    params.exists((param: (String, String)) => param._1 == p)
  }

  get("/") { redirect("index") }

  get("/index") {
    ifLoggedIn{ (user: ConfUser) =>
      val ctxt = getContext(user);

      val msg = {
        if (hasParam(params, "msg")) {
          params("msg") match {
            case "illegalAccess" =>
              "You do not have permission to access that page."
            case "internalError" => "An internal error has occured."
            case "password" =>
              "Your password has been sent to " + user.email + "."
          }
        } else { "" }
      }

      renderPageWithUser("index.ssp", user
        , Map( "msg" -> msg
               , "name" -> user.showName(getContext(user))
               , "submittedPapers" -> user.showSubmittedPapers(ctxt)
               , "reviewPapers" -> user.showReviewPapers(ctxt)
               , "reviews" -> user.showReviews(ctxt)))
    }
  }

  get("/send_password") {
    ifLoggedIn{ (user: ConfUser) =>
      user.emailPassword()
      redirect("index?msg=password")
    }
  }

  post("/login_user") {
    val username = JConfUtil.cleanUsername(params("username"))
    if (JConfUtil.isWellFormed(username)) {
      val isSigningUp: Boolean =
        params.exists((param: (String, String)) => param._1 == "signup")
      // If we are signing up in...
      if (isSigningUp) {
        JConfTables.getDBConfUserByEmail(username) match {
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
              , ""
              , RandomGenerator.generatePassword()
              , false
              , ""
              , role);

            // E-mail the user about the password.
            u.emailPassword();
            redirect("validate_profile?id="+ u.uid)
          }
        } else {
          // Assuming otherwise we are logging in...
          loginUser(params("username"), params("password")) match {
            case Some(user) =>
              session("user") = user
              val isValidating: Boolean =
                params.exists((param: (String, String)) =>
                  param._1 == "validate")
              if (isValidating) { redirect("edit_profile") }
              else { redirect("index") }
            case None =>
              loginRedirect("Incorrect username or password.")
          }
        }
      } else {
        loginRedirect("Incorrect/malformed username.")
      }
    }

  get("/login") {
    session.get("user") match {
      case Some(user) => redirect("index")
      case None => renderPage("login_screen.ssp")
    }
  }

  get("/validate_profile") {
    JConfBackend.getUserById(params("id").toInt) match {
      case Some(user) => renderPageWithUser("validate_profile.ssp", user)
      case None => throw new JConfInternalError
    }
  }

  // Update and display profile.
  post("/profile") {
    ifLoggedIn { (user: ConfUser) =>
      val conflicts: List[BigInt] = {
        try {
          multiParams("conflict").toList.map((v: String) => BigInt(v.toInt))
        } catch {
          case e: Exception => Nil 
        }
      }
      user.update(params, conflicts);
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
  def getPaper(paperId: Int, paperKey: String): PaperRecord = {
    getPaperById(paperId) match {
      case Some(p) =>
        if (paperKey == p.key) { p } else throw new IllegalAccessError
      case None => throw new JConfInternalError
    }
  }
  post("/paper") {
    ifLoggedIn { (user: ConfUser) =>
      val paperId: Int = params("id").toInt
      val paperKey: String = params("key")
      val paper: PaperRecord = getPaper(paperId, paperKey)

      // Write paper to place on disk.
      val uploadedFile = fileParams("thefile")
      val filename = {
        if (uploadedFile != null) {
          uploadedFile.getName()
        } else {
          ""
        }
      }

      // Update the paper record with the new information.
      JConfUtil.updatePaperRecord(paper, filename, params)

      // Write file to disk...
      if (uploadedFile.getSize > 0) {
        val (backupLoc, tomcatLoc) = paper.showFileLocations(getContext(user))
        if (!(backupLoc.isEmpty() || tomcatLoc.isEmpty())) {
          val backupFile = new File(backupLoc)
          val tomcatFile = new File(tomcatLoc)
          uploadedFile.write(backupFile)
         FileUtils.copyFile(backupFile, tomcatFile)
        }
      }

      renderPageWithUser("paper.ssp", user, Map("paper" -> paper))
    }
  }
  get("/paper") {
    ifLoggedIn { (user: ConfUser) =>
      var paper: PaperRecord = getPaper(params("id").toInt, params("key"))
      renderPageWithUser("paper.ssp", user, Map("paper" -> paper))
    }
  }

  get("/uploadedPaper") {
    ifLoggedIn { (user: ConfUser) =>
      val paperRecord: PaperRecord =
        getPaper(params("id").toInt, params("key"))
      redirect(paperRecord.showFileDisplayLocation(getContext(user)))
    }
  }

  /* Updating and displaying reviews. */
  def getReviewInfo(reviewId: Int, reviewKey: String, ctxt: ConfContext)
    : (PaperRecord, ConfUser, PaperReview) = {
    val review: PaperReview = {
      JConfTables.getDBPaperReview(reviewId) match {
        case Some(r) =>
          if (r.key == reviewKey) { r } else { throw new IllegalAccessError }
        case None => throw new JConfInternalError
      }
    }

    val paper: PaperRecord =
      getPaperById(review.paperId.toInt) match {
        case Some(p) => p
        case None =>
          throw new JConfInternalError
      }

    val reviewer: ConfUser = review.showReviewer(ctxt)
    
    (paper, reviewer, review)
  }
  post("/review") {
    ifLoggedIn { (user: ConfUser) =>
      val (paper, reviewer, review) =
        getReviewInfo(params("id").toInt, params("key"), getContext(user))
      JConfUtil.updatePaperReview(review, params)

      renderPageWithUser("review.ssp", user,
        Map("paper" -> paper, "reviewer" -> reviewer, "review" -> review))
    }
  }
  get("/review") {
    ifLoggedIn { (user: ConfUser) =>
      val (paper, reviewer, review) =
        getReviewInfo(params("id").toInt, params("key"), getContext(user))
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
      val (paper, reviewer, review) =
        getReviewInfo(params("id").toInt, params("key"), getContext(user))
      
      renderPageWithUser("edit_review.ssp", user
        , Map(
            "paper" -> paper
          , "review" -> review
          , "reviewer" -> reviewer ))
    }
  }

  get("/new_paper") {
    ifLoggedIn { (user: ConfUser) =>
      val paper: PaperRecord =
        JConfBackend.addPaper("Untitled", List(user))
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
