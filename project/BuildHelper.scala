import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.core.ProblemFilters._
import com.typesafe.tools.mima.plugin.MimaKeys._
import sbt._
import sbt.Keys.{name, organization}
import org.snakeyaml.engine.v2.api.{Load, LoadSettings}

import java.util.{List => JList,Map => JMap}
import scala.jdk.CollectionConverters._

object BuildHelper {

  private val versions: Map[String, String] = {
    val doc = new Load(LoadSettings.builder().build())
      .loadFromReader(scala.io.Source.fromFile(".github/workflows/ci.yml").bufferedReader())
    val yaml = doc.asInstanceOf[JMap[String, JMap[String, JMap[String, JMap[String, JMap[String, JList[String]]]]]]]
    val list = yaml.get("jobs").get("build").get("strategy").get("matrix").get("scala").asScala
    list.map(v => (v.split('.').take(2).mkString("."), v)).toMap
  }

  val Scala213: String = versions("2.13")
  val Scala3: String   = versions("3.5")

  object Versions {

    val circe           = "0.14.10"
    val circeDerivation = "0.13.0-M5"
    val jsoniter        = "2.31.3"
    val scalaJavaTime   = "2.6.0"
    val zio             = "2.1.13"
    val zioSchema       = "1.5.0"
  }

  def mimaSettings(binCompatVersionToCompare: Option[String], failOnProblem: Boolean): Seq[Def.Setting[?]] =
    binCompatVersionToCompare match {
      case None                   => Seq(mimaPreviousArtifacts := Set.empty)
      case Some(binCompatVersion) =>
        Seq(
          mimaPreviousArtifacts := Set(organization.value %% name.value % binCompatVersion),
          mimaFailOnProblem     := failOnProblem,
        )
    }
}
