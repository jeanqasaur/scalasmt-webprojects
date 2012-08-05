package cap.jeeves.jconf.frontend

import cap.jeeves.jconf.backend._

object JConfUtil {
  def cleanUsername(username: String): String = {
    username.trim.toLowerCase
  }

  def isWellFormed(username: String): Boolean = {
    (username.length > 4) && (username.exists(_ == '@'))
  }

  def updatePaperReview(review: PaperReview, params: Map[String, String])
    : Unit = {
    val problemScore = params("problemScore").toInt
    val backgroundScore = params("backgroundScore").toInt
    val approachScore = params("approachScore").toInt
    val resultScore = params("resultScore").toInt

    review.setProblemScore(problemScore)
    review.setBackgroundScore(backgroundScore)
    review.setApproachScore(approachScore)
    review.setResultScore(resultScore)
    review.setBody(params("body"))

    JConfTables.updateDBReview(review,
      problemScore, backgroundScore, approachScore, resultScore, params("body"))
  }

  def updatePaperRecord(paper: PaperRecord, filename: String
    , params: Map[String, String]) {
    paper.setTitle(params("title"))
    paper.setFile(filename)

    JConfTables.updateDBPaper(paper, params("title"), filename)
  }
}
