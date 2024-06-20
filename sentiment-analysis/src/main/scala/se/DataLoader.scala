package sa

import scala.io.Source

object DataLoader {
  
  def loadAmazonReviews(filePath: String): Seq[Review] = {
    val bufferedSource = Source.fromFile(filePath)
    try {
      bufferedSource.getLines().drop(1).zipWithIndex.flatMap { case (line, index) =>
        val fields = line.split(",", -1)
        
        if (fields.length < 8) {
          None
        } else {
          Some(Review(
            fields(0), // reviewId
            fields(1), // userName
            fields(2), // content
            try { fields(3).toInt } catch { case _: NumberFormatException => 0 }, // score
            try { fields(4).toInt } catch { case _: NumberFormatException => 0 }, // thumbsUpCount
            fields(5), // reviewCreatedVersion
            fields(6), // at
            fields(7)  // appVersion
          ))
        }
      }.toSeq
    } finally {
      bufferedSource.close()
    }
  }
  
  case class Review(reviewId: String, userName: String, content: String, score: Int, thumbsUpCount: Int, reviewCreatedVersion: String, at: String, appVersion: String)
}
