package sa

object Preprocessing {

  def tokenize(text: String): Seq[String] = {
    text.toLowerCase.replaceAll("[^a-zA-Z0-9 ]", "").split("\\s+").filter(_.nonEmpty)
  }

  def removeStopWords(tokens: Seq[String]): Seq[String] = {
    val stopWords = Set(
      "i", "me", "my", "myself", "we", "our", "ours", "ourselves", "you", "your", "yours", "yourself", "yourselves",
      "he", "him", "his", "himself", "she", "her", "hers", "herself", "it", "its", "itself", "they", "them", "their",
      "theirs", "themselves", "what", "which", "who", "whom", "this", "that", "these", "those", "am", "is", "are", "was",
      "were", "be", "been", "being", "have", "has", "had", "having", "do", "does", "did", "doing", "a", "an", "the", "and",
      "but", "if", "or", "because", "as", "until", "while", "of", "at", "by", "for", "with", "about", "against", "between",
      "into", "through", "during", "before", "after", "above", "below", "to", "from", "up", "down", "in", "out", "on", "off",
      "over", "under", "again", "further", "then", "once", "here", "there", "when", "where", "why", "how", "all", "any",
      "both", "each", "few", "more", "most", "other", "some", "such", "no", "nor", "not", "only", "own", "same", "so",
      "than", "too", "very", "s", "t", "can", "will", "just", "don", "should", "now"
    )

    tokens.filterNot(stopWords.contains)
  }

  def stem(tokens: Seq[String]): Seq[String] = {
    tokens.map { token =>
      if (token.endsWith("ing")) {
        token.dropRight(3)
      } else if (token.endsWith("ly")) {
        token.dropRight(2)
      } else if (token.endsWith("ed")) {
        token.dropRight(2)
      } else {
        token
      }
    }
  }

  def processReview(review: DataLoader.Review): Seq[String] = {
    val text = review.content
    val tokens = tokenize(text)
    val cleanTokens = removeStopWords(tokens)
    val stemmedTokens = stem(cleanTokens)
    stemmedTokens
  }
}
