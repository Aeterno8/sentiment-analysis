package sa

object Evaluation {

  def evaluate(predictions: Seq[String], labels: Seq[String]): Unit = {
    require(predictions.length == labels.length, "Number of predictions must match number of labels.")

    val confusionMatrix = calculateConfusionMatrix(predictions, labels)
    val accuracy = calculateAccuracy(confusionMatrix)
    val precision = calculatePrecision(confusionMatrix)
    val recall = calculateRecall(confusionMatrix)
    val f1Score = calculateF1Score(precision, recall)

    println(s"Confusion Matrix:\n$confusionMatrix")
    println(s"Accuracy: $accuracy")
    println(s"Precision: $precision")
    println(s"Recall: $recall")
    println(s"F1 Score: $f1Score")
  }

  def calculateConfusionMatrix(predictions: Seq[String], labels: Seq[String]): Map[(String, String), Int] = {
    val uniqueLabels = labels.distinct
    val matrix = uniqueLabels.flatMap(actual => uniqueLabels.map(predicted => ((actual, predicted), 0))).toMap

    predictions.zip(labels).foldLeft(matrix) { case (confMatrix, (predicted, actual)) =>
      confMatrix + ((actual, predicted) -> (confMatrix((actual, predicted)) + 1))
    }
  }

  def calculateAccuracy(confusionMatrix: Map[(String, String), Int]): Double = {
    val correct = confusionMatrix.collect { case ((label, pred), count) if label == pred => count }.sum.toDouble
    val total = confusionMatrix.values.sum.toDouble
    if (total > 0) correct / total else 0.0
  }

  def calculatePrecision(confusionMatrix: Map[(String, String), Int]): Map[String, Double] = {
    val labels = confusionMatrix.keys.map(_._1).toList.distinct
    labels.map { label =>
      val tp = confusionMatrix.getOrElse((label, label), 0)
      val relevant = confusionMatrix.filterKeys(_._2 == label).values.sum.toDouble
      if (relevant > 0) (label -> (tp / relevant)) else (label -> 0.0)
    }.toMap
  }

  def calculateRecall(confusionMatrix: Map[(String, String), Int]): Map[String, Double] = {
    val labels = confusionMatrix.keys.map(_._1).toList.distinct
    labels.map { label =>
      val tp = confusionMatrix.getOrElse((label, label), 0)
      val actual = confusionMatrix.filterKeys(_._1 == label).values.sum.toDouble
      if (actual > 0) (label -> (tp / actual)) else (label -> 0.0)
    }.toMap
  }

  def calculateF1Score(precision: Map[String, Double], recall: Map[String, Double]): Map[String, Double] = {
    precision.keys.map { label =>
      val prec = precision(label)
      val rec = recall(label)
      if (prec + rec > 0) (label -> (2 * prec * rec / (prec + rec))) else (label -> 0.0)
    }.toMap
  }
}
