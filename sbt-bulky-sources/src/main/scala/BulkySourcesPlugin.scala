import sbt.{ Def, IO, _ }
import Keys._

object BulkySourcesPlugin extends AutoPlugin {
  override def trigger = allRequirements

  object autoImport {
    lazy val bulkyThresholdInLines = settingKey[Int]("Bulky threshold setting")
    lazy val bulkySources          = taskKey[Seq[(Int, File)]]("Show bulky sources according to bulkyThresholdInLines setting")
  }

  import autoImport._

  lazy val baseBulkySources: Seq[Def.Setting[_]] = Seq(
    bulkySources := {
      sortedBulkySources(sources.value, bulkyThresholdInLines.value)
    }
  )

  override def projectSettings: Seq[Def.Setting[_]] =
    inConfig(Compile)(baseBulkySources) ++
        inConfig(Test)(baseBulkySources) ++
        Seq(bulkyThresholdInLines := 100)

  private def sortedBulkySources(sources: Seq[File], threshold: Int): Seq[(Int, File)] =
    sources
      .map(s => (IO.readLines(s).size, s))
      .filter { case (size, _) => size >= threshold }
      .sorted(implicitly[Ordering[(Int, File)]].reverse)
}
