package sa

import java.io._
import scala.collection.mutable

object Classifier {

  def trainAndSaveModel(reviews: Seq[DataLoader.Review]): NaiveBayesModel = {
    val model = train(reviews)
    saveModel(model)
    model
  }

  def train(reviews: Seq[DataLoader.Review]): NaiveBayesModel = {
    val model = new NaiveBayesModel()

    var numPositive = 0
    var numNegative = 0

    val wordCountPositive = mutable.Map.empty[String, Int].withDefaultValue(0)
    val wordCountNegative = mutable.Map.empty[String, Int].withDefaultValue(0)

    reviews.foreach { review =>
      val tokens = Preprocessing.processReview(review)
      val sentiment = review.score

      tokens.foreach { token =>
        if (sentiment >= 3) {
          wordCountPositive(token) += 1
          numPositive += 1
        } else {
          wordCountNegative(token) += 1
          numNegative += 1
        }
      }
    }

    val totalWords = wordCountPositive.size + wordCountNegative.size
    model.positiveProb = numPositive.toDouble / (numPositive + numNegative)
    model.negativeProb = numNegative.toDouble / (numPositive + numNegative)

    wordCountPositive.foreach { case (word, count) =>
      model.positiveWordProbs(word) = count.toDouble / numPositive
    }

    wordCountNegative.foreach { case (word, count) =>
      model.negativeWordProbs(word) = count.toDouble / numNegative
    }

    model
  }

  def saveModel(model: NaiveBayesModel): Unit = {
    val modelsDir = new File("./models")
    if (!modelsDir.exists()) {
      modelsDir.mkdirs()
    }

    val modelFile = new File(modelsDir, "naive_bayes_model")
    val oos = new ObjectOutputStream(new FileOutputStream(modelFile))
    oos.writeObject(model)
    oos.close()
    println(s"Model successfully saved to ${modelFile.getAbsolutePath}.")
  }

  class NaiveBayesModel extends Serializable {
    var positiveProb: Double = _
    var negativeProb: Double = _
    val positiveWordProbs: mutable.Map[String, Double] = mutable.Map.empty
    val negativeWordProbs: mutable.Map[String, Double] = mutable.Map.empty
  }

  def classifyReview(review: String, model: NaiveBayesModel): String = {
    val tokens = Preprocessing.tokenize(review)
    val processedTokens = Preprocessing.removeStopWords(tokens)
    val stemmedTokens = Preprocessing.stem(processedTokens)

    var positiveScore = math.log(model.positiveProb)
    var negativeScore = math.log(model.negativeProb)

    stemmedTokens.foreach { token =>
      if (model.positiveWordProbs.contains(token)) {
        positiveScore += math.log(model.positiveWordProbs(token))
      }
      if (model.negativeWordProbs.contains(token)) {
        negativeScore += math.log(model.negativeWordProbs(token))
      }
    }

    if (positiveScore > negativeScore) "positive"
    else "negative"
  }
}
