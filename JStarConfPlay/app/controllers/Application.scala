package controllers

import cap.jeeveslib.jeeves._
import cap.jeeves.jconf.backend._
import cap.jeeves.jconf.frontend._
import cap.jeeves.jconf.backend.JConfBackend._
import anorm._
import anorm.SqlParser._
import models.User
import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import play.api.db._
import play.mvc.Controller._
import java.util.Calendar
import java.util.GregorianCalendar
import java.util.Locale
import java.util.TimeZone
//import models.backend.ConfUser

object Application extends Controller {

  class IllegalAccessError extends Exception
  class JConfInternalError extends Exception
  
  def emptyName = ""

  def getCalendar (): Calendar =
    new GregorianCalendar(TimeZone.getTimeZone("EST"), Locale.US)
  val submissionDeadline: Calendar = {
    val c = getCalendar ()
    c.set(2012, Calendar.MARCH, 14, 22, 59, 59)
    c
  }
  val notificationDeadline: Calendar = {
    val c = getCalendar ()
    c.set(2012, Calendar.APRIL, 6, 11, 00, 00)
    c
  }
  val currentTime: Calendar = getCalendar ()

  def getConfStage () = Submission
  
  def getContext(request: RequestHeader, user: ConfUser): (Boolean,ConfContext) = {
    request.session.get("context") match {
      case Some(c) => {
        val ctxt = c.asInstanceOf[ConfContext]
        ctxt.stage = getConfStage()
        (false,ctxt)
      }
      case None => {
        val ctxt = new ConfContext(user, getConfStage())
        // setting a variable or field in the session
        // i need the action in order to do so
//        request.session("context") = ctxt
        (true, ctxt)
      }
    }
  }

  def ifLoggedIn(request: RequestHeader, displayPage: ConfUser => Result): Result = {
    request.session.get("user") match {
      case Some(u) =>
        // TODO: Insert try/catch block here that redirects to index.
        val user = u.asInstanceOf[ConfUser];
        try {
          displayPage(user)
        } catch {
          case ia: IllegalAccessError => Redirect(routes.Application.home("illegalAccess"));//redirect("index?msg=illegalAccess")
          case ie: JConfInternalError => Redirect(routes.Application.home("internalError"));//("index?msg=internalError")
        }
      case None => Redirect(routes.Application.login)//("login")
    }
  }

  def renderPage(tupCtxt: (Boolean,ConfContext) = null, page: String
    , args: Map[String, Any] = Map()): Result = {
    var newArgs = args;
    newArgs += ("title" -> "JConf")
//    contentType = "text/html"
//    templateEngine.layout(path + page, newArgs)
    if (tupCtxt != null && tupCtxt._1) {
    	Ok(Scalate(page).render('map -> newArgs)).withSession(
//    	    "context" -> ctxt.)
    	    )
    } 
    else {
    	Ok(Scalate(page).render('map -> newArgs))
    }
  }
  
  def renderPageWithUser(request: RequestHeader,
    page: String, user: ConfUser
    , args: Map[String, Any] = Map()): Result = {
    var newArgs = args;
    val tupCtxt = getContext(request,user);
    newArgs += ("user" -> user)
    newArgs += ("ctxt" -> tupCtxt._2);
    renderPage(tupCtxt, page, newArgs)
  }

  def loginRedirect(msg: String): Result = 
    renderPage(null,"login_screen.ssp", Map("errorMsg" -> msg));
  

  def hasParam (params: Map[String, String], p: String) = {
    params.exists((param: (String, String)) => param._1 == p)
  }

  def about = Action { implicit request =>
    session.get("user") match {
      case Some(u) =>
      val user = u.asInstanceOf[ConfUser];
      renderPageWithUser(request,"about.ssp", user)
      case None => renderPage(null, "about.ssp")
    }
  }
  
  def index = Action {
    Redirect(routes.Application.home(""));
  }

  def home (msg: String) = Action { request =>
    ifLoggedIn(request, {(user: ConfUser) =>
    val tupCtxt = getContext(request,user);
    val ctxt = tupCtxt._2;
    if (tupCtxt._1) {
      
    }
    
    val msg = {
      if (hasParam(request.session.data, "msg")) {
        request.session.data("msg") match {
          case "illegalAccess" =>
          "You do not have permission to access that page."
          case "internalError" => "An internal error has occured."
          case "assignments" =>
          "Your assignments have been made."
          case "password" =>
          "Your password has been sent to " + user.email + "."
          case "withdrawn" =>
          "Your paper has been withdrawn."
        }
      } else { "" }
    }

    renderPageWithUser(request, "index.ssp", user
      , Map( "msg" -> msg
        , "name" -> user.showName(getContext(request, user)._2)
        , "submissionDeadline" -> submissionDeadline.getTime().toString()
        , "notificationDeadline" -> notificationDeadline.getTime().toString()
                 , "submittedPapers" -> user.showSubmittedPapers(ctxt)
                 , "reviewPapers" -> user.showReviewPapers(ctxt)
                 , "reviews" -> user.showReviews(ctxt)))
      }
    );
  }

//  def sendPassword = Action { request =>
//    ifLoggedIn(request, {(user: ConfUser) =>
//      user.emailPassword()
//      Redirect(routes.Application.home("password"));//redirect("index?msg=password")
//    }
//    );
//  }
//
  def loginUser = Action { request =>
    val username = JConfUtil.cleanUsername(request.session.get("username").get)
    if (JConfUtil.isWellFormed(username)) {
      request.session.get("action").get match {
        case "Sign Up" =>
          // If we are signing up...
          getUserByEmail(username) match {
            // User already exists in database.  Redirect to the login page.
            case Some(user) =>
              loginRedirect("Username already exists in database.")
            case None =>
              // Everyone is author status by default.
              val u =
                addUser(request.session.get("username").get
                , "", "", RandomGenerator.generatePassword()
                , false, "", AuthorStatus);

              // E-mail the user about the password.
              u.emailPassword();
              Redirect(routes.Application.home("validate_profile?id="+ u.uid))
            }
//        case "Log In" =>
//          // Assuming otherwise we are logging in...
//          loginUser(params("username"), params("password")) match {
//            case Some(user) =>
//              session("user") = user
//              Redirect(routes.Application.index)
//            case None =>
//              loginRedirect("Incorrect username or password.")
//          }
//        case "Validate" =>
//          loginUser(params("username"), params("password")) match {
//            case Some(user) =>
//              session("user") = user
//              redirect("edit_profile")
//            case None =>
//              loginRedirect("Incorrect username or password.")
//          }
        case "Send Password" =>
          sendUserPassword(username);
          loginRedirect("Your password has been send to " + username + ".")
        case other =>
          loginRedirect("Incorrect/malformed username.")
      }
    }else {Redirect(routes.Application.home(""))}
  }

  def login = Action { request =>
      request.session.get("user") match {
        case Some(user) => Redirect(routes.Application.home(""))
        case None => renderPage(null,"login_screen.ssp")
      }
  }

//  def validateProfile = Action { request =>
//      getUserById(request.session.get("id").get.toInt) match {
//        case Some(user) =>
//          renderPageWithUser("validate_profile.ssp", user)
//        case None => throw new JConfInternalError
//      }
//  }
//
//  // Update and display profile.
//  def updateProfile = Action { request =>
//      ifLoggedIn (request, {(user: ConfUser) =>
//        val conflicts: List[BigInt] = {
//          try {
//            multiParams("conflict").toList.map((v: String) => BigInt(v.toInt))
//          } catch {
//            case e: Exception => Nil 
//          }
//        }
//        user.update(params, conflicts);
//        session("user") = user;
//        renderPageWithUser("profile.ssp", user)
//      })
//  }
//
//  def profile = Action { request =>
//      ifLoggedIn (request, {(user: ConfUser) =>
//        var curUser: ConfUser = {
//          if (!(params.exists(_ == "id"))) {
//            user;
//          } else {
//            getUserById(params("id").toInt) match {
//              case Some(idUser) => idUser
//              case None => null
//            }
//          }
//        }
//        renderPage("profile.ssp"
//            , Map("user" -> curUser, "ctxt" -> getContext(user)))
//      })
//  }
//
//  /* Updating and displaying papers. */
//  def getPaper(paperId: Int, paperKey: String)
//    : PaperRecord = {
//    getPaperById(paperId) match {
//      case Some(p) =>
//        if (paperKey == p.key) { p } else throw new IllegalAccessError
//      case None => throw new JConfInternalError
//    }
//  }
//  def postPaper = Action { request =>
//      ifLoggedIn (request, {(user: ConfUser) =>
//        val paperId: Int = request.session.get("id").get.toInt
//        val paperKey: String = request.session.get("key").get
//        val paper: PaperRecord = getPaper(paperId, paperKey)
//
//        // Write paper to place on disk.
//        val uploadedFile = fileParams("thefile")
//        val filename = {
//          if (uploadedFile != null) {
//            uploadedFile.getName()
//          } else { "" }
//        }
//
//        // Update the paper record with the new information.
//        JConfUtil.updatePaperRecord(paper, filename, params)
//
//        // Write file to disk...
//        if (uploadedFile.getSize > 0) {
//          val (backupLoc, tomcatLoc) =
//            paper.showFileLocations(getContext(user))
//          if (!(backupLoc.isEmpty() || tomcatLoc.isEmpty())) {
//            val backupFile = new File(backupLoc)
//            val tomcatFile = new File(tomcatLoc)
//            println("Writing file to " + backupLoc + "...")
//            uploadedFile.write(backupFile)
//            println("Copying file to " + tomcatFile + "...")
//           FileUtils.copyFile(backupFile, tomcatFile)
//          }
//        }
//
//        renderPageWithUser("paper.ssp", user, Map("paper" -> paper))
//      })
//  }
//  
//  def paper = Action { request =>
//    ifLoggedIn (request, { (user: ConfUser) =>
//    var paper: PaperRecord =
//      getPaper(request.session.get("id").get.toInt, request.session.get("key").get)
//    renderPageWithUser("paper.ssp", user, Map("paper" -> paper))
//    })
//  }
//
//  get("/uploadedPaper") {
//    ifLoggedIn { (user: ConfUser) =>
//    val paperRecord: PaperRecord =
//      getPaper(params("id").toInt, params("key"))
//    redirect(
//      paperRecord.showFileDisplayLocation(getContext(user)))
//    }
//  }
//
//  /* Updating and displaying reviews. */
//  def getReviewInfo( reviewId: Int, reviewKey: String, ctxt: ConfContext)
//    : (PaperRecord, ConfUser, PaperReview) = {
//    val review: PaperReview = {
//      getReviewById(reviewId) match {
//        case Some(r) =>
//          if (r.key == reviewKey) { r } else { throw new IllegalAccessError }
//        case None => throw new JConfInternalError
//      }
//    }
//
//    val paper: PaperRecord =
//      getPaperById(review.paperId.toInt) match {
//        case Some(p) => p
//        case None =>
//          throw new JConfInternalError
//      }
//
//    val reviewer: ConfUser = review.showReviewer(ctxt)
//    
//    (paper, reviewer, review)
//  }
//  post("/review") {
//      ifLoggedIn { (user: ConfUser) =>
//        val (paper, reviewer, review) =
//          getReviewInfo(params("id").toInt, params("key"), getContext(user))
//        JConfUtil.updatePaperReview(review, params)
//
//        renderPageWithUser("review.ssp", user,
//          Map("paper" -> paper, "reviewer" -> reviewer, "review" -> review))
//      }
//  }
//  get("/review") {
//      ifLoggedIn { (user: ConfUser) =>
//        val (paper, reviewer, review) =
//          getReviewInfo(params("id").toInt, params("key"), getContext(user))
//        renderPageWithUser("review.ssp", user,
//          Map("paper" -> paper, "reviewer" -> reviewer, "review" -> review))
//      }
//  }
//
//  get("/edit_profile") {
//    ifLoggedIn { (user: ConfUser) =>
//      renderPageWithUser("edit_profile.ssp", user)
//    }
//  }
//  
//  get("/edit_review") {
//      ifLoggedIn { (user: ConfUser) =>
//       val (paper, reviewer, review) =
//          getReviewInfo(params("id").toInt, params("key"), getContext(user))
//      
//        renderPageWithUser("edit_review.ssp", user
//          , Map(
//              "paper" -> paper
//            , "review" -> review
//            , "reviewer" -> reviewer ))
//      }
//  }
//
//  get("/new_paper") {
//      ifLoggedIn { (user: ConfUser) =>
//        val paper: PaperRecord =
//          addPaper("Untitled", List(user))
//        renderPageWithUser("edit_paper.ssp", user, Map("paper" -> paper))
//      }
//  }
//  get("/edit_paper") {
//    ifLoggedIn { (user: ConfUser) =>
//      val paper: PaperRecord =
//          getPaper(params("id").toInt, params("key"))
//        renderPageWithUser("edit_paper.ssp", user, Map("paper" -> paper))
//    }
//  }
//  get("/withdraw_paper") {
//    ifLoggedIn { (user: ConfUser) =>
//        // Get paper.
//        val paper: PaperRecord =
//          getPaper(params("id").toInt, params("key"))
//
//        // Remove paper for user.
//        user.withdrawSubmittedPaper(paper)
//
//        redirect("index?msg=withdrawn")
//    }
//  }
//
//  def assign_papers {
//      ifLoggedIn { (user: ConfUser) =>
//        if (user.role == PCStatus) {
//          val papers: List[PaperRecord] = getAllPapers()
//          val conflicts: List[ConfUser] = getPotentialConflicts()
//          renderPageWithUser("assign_papers.ssp", user
//            , Map("papers" -> papers, "conflicts" -> conflicts))
//        } else {
//          // note: the route index needs to add parameters for msg: String
//          Redirect(routes.Application.index("illegalAccess"))
//        }
//      }
//  }
//  def paper_assignments {
//      ifLoggedIn { (user: ConfUser) =>
//        if (user.role == PCStatus) {
//          val reviewers: List[ConfUser] = getPotentialConflicts()
//          val assignments: Seq[String] = multiParams("assignment")
//          assignments.foreach{ a =>
//            try {
//              val asst: Array[String] = a.split(":")
//              val paperId = asst.apply(0).toInt;
//              if (params.exists(_._1 == "changed"+paperId)) {
//              val (paper, reviewer) = {
//                val paperOpt = getPaperById(paperId);
//                val reviewOpt = reviewers.find(_.uid == asst.apply(1).toInt);
//                (paperOpt, reviewOpt) match {
//                    case (Some(p), Some(r)) => (p, r)
//                    case (_, _) => throw new JConfInternalError
//                  }
//                }
//                assignReview(paper, reviewer)
//              }
//            } catch {
//              case e: Exception => throw new JConfInternalError
//            }
//          }
//          redirect("index?msg=assignments")
//        } else {
//          redirect("index?msg=illegalAccess")
//        }
//      }
//  }
//
  def logout = Action { request =>
     Redirect(routes.Application.logout()).withNewSession 
  }
//
////  notFound {
////    // If no route matches, then try to render a Scaml template
////    val templateBase = requestPath match {
////      case s if s.endsWith("/") => s + "index"
////      case s => s
////    }
////    val templatePath = "/WEB-INF/scalate/templates/" + templateBase + ".scaml"
////    servletContext.getResource(templatePath) match {
////      case url: URL => 
////        contentType = "text/html"
////        templateEngine.layout(templatePath)
////      case _ => 
////        filterChain.doFilter(request, response)
////    } 
////  }

}