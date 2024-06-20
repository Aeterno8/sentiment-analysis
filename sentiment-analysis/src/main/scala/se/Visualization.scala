package sa

import java.awt.Color
import java.awt.Font
import java.awt.Graphics2D
import java.awt.image.BufferedImage
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import javax.imageio.ImageIO
import org.jfree.chart.ChartFactory
import org.jfree.chart.JFreeChart
import org.jfree.chart.plot.PlotOrientation
import org.jfree.data.category.DefaultCategoryDataset
import java.io.File

object Visualization {

  def plotBarChart(labels: Seq[String], values: Seq[Double], title: String, xLabel: String, yLabel: String): Unit = {
    val dataset = new DefaultCategoryDataset()
    labels.zip(values).foreach { case (label, value) =>
      dataset.addValue(value, "Category", label)
    }

    val chart: JFreeChart = ChartFactory.createBarChart(
      title,
      xLabel,
      yLabel,
      dataset,
      PlotOrientation.VERTICAL,
      true,
      true,
      false
    )

    saveChartAsImage(chart, title)
  }

  def plotConfusionMatrixLegend(confusionMatrix: Map[(String, String), Int], classes: Seq[String], title: String): Unit = {
    val width = 600
    val height = 400
    val padding = 50

    val image = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
    val g2d = image.createGraphics()

    g2d.setColor(Color.WHITE)
    g2d.fillRect(0, 0, width, height)

    g2d.setColor(Color.BLACK)
    g2d.setFont(new Font("Arial", Font.BOLD, 16))
    g2d.drawString("Confusion Matrix", padding, 30)

    val boxSize = 30
    val boxPadding = 10
    val startX = padding
    var currentY = 60

    classes.foreach { actual =>
      classes.foreach { predicted =>
        val count = confusionMatrix.getOrElse((actual, predicted), 0)

        val color = getColorForValue(count.toDouble / 100.0)
        g2d.setColor(color)
        g2d.fillRect(startX, currentY, boxSize, boxSize)

        g2d.setColor(Color.BLACK)
        g2d.drawRect(startX, currentY, boxSize, boxSize)
        g2d.drawString(s"$actual -> $predicted: $count", startX + boxSize + boxPadding, currentY + boxSize / 2 + 5)

        currentY += boxSize + boxPadding
      }
    }

    g2d.dispose()

    val folder = new File("visualisation")
    if (!folder.exists()) {
      folder.mkdir()
    }

    val timestamp = LocalDateTime.now.format(DateTimeFormatter.ofPattern("yyyyMMdd_HHmmss"))
    val fileName = s"visualisation/${title.replaceAll("\\s+", "_")}_legend_$timestamp.png"
    val file = new File(fileName)

    ImageIO.write(image, "png", file)

    println(s"Confusion matrix legend saved as $fileName")
  }

  private def getColorForValue(value: Double): Color = {
    val clampedValue = Math.max(0.0, Math.min(value, 1.0))
    val scaledValue = (clampedValue * 255.0).toInt
    new Color(scaledValue, 0, 0) 
  }

  private def saveChartAsImage(chart: JFreeChart, title: String): Unit = {
    val folder = new File("visualisation")
    if (!folder.exists()) {
      folder.mkdir()
    }

    val timestamp = LocalDateTime.now.format(DateTimeFormatter.ofPattern("yyyyMMdd_HHmmss"))
    val fileName = s"visualisation/${title.replaceAll("\\s+", "_")}_$timestamp.png"
    val file = new File(fileName)

    val width = 800
    val height = 600

    val bufferedImage = chart.createBufferedImage(width, height)
    ImageIO.write(bufferedImage, "png", file)

    println(s"Chart saved as $fileName")
  }
}
