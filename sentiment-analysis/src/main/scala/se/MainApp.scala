package sa

object MainApp {

  def main(args: Array[String]): Unit = {
    val filePath = "./data/row/amazon_reviews.csv"

    // Učitavanje recenzija
    val reviews = DataLoader.loadAmazonReviews(filePath)

    val splitIndex = (reviews.size.toDouble * 0.8).toInt

    val (trainReviews, testReviews) = reviews.splitAt(splitIndex)

    // Treniranje i čuvanje modela
    val model = Classifier.trainAndSaveModel(trainReviews)

    // Klasifikacija recenzija na testnom setu i evaluacija modela
    val predictions = testReviews.map(review => Classifier.classifyReview(review.content, model))
    val labels = testReviews.map(review => if (review.score >= 3) "positive" else "negative")

    Evaluation.evaluate(predictions, labels)

    // Vizualizacija
    val positiveReviewsCount = labels.count(_ == "positive").toDouble
    val negativeReviewsCount = labels.count(_ == "negative").toDouble

    Visualization.plotBarChart(Seq("Positive", "Negative"), Seq(positiveReviewsCount, negativeReviewsCount), "Review Distribution", "Sentiment", "Number of Reviews")

    val confusionMatrix = Evaluation.calculateConfusionMatrix(predictions, labels)
    val classes = Seq("positive", "negative")
    Visualization.plotConfusionMatrixLegend(confusionMatrix, classes, "Confusion Matrix Legend")

    val accuracy = Evaluation.calculateAccuracy(confusionMatrix)
    val precision = Evaluation.calculatePrecision(confusionMatrix)
    val recall = Evaluation.calculateRecall(confusionMatrix)
    val f1Score = Evaluation.calculateF1Score(precision, recall)

    println(s"Accuracy: $accuracy")
    println(s"Precision: $precision")
    println(s"Recall: $recall")
    println(s"F1 Score: $f1Score")
  }
}
