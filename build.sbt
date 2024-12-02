import sbt.Def
import BuildHelper._

import scala.scalanative.build.{GC, Mode}
import scala.scalanative.sbtplugin.ScalaNativePlugin.autoImport.nativeConfig

lazy val binCompatVersionToCompare = None

enablePlugins(ZioSbtEcosystemPlugin)

inThisBuild(
  List(
    name                     := "ZIO Schema Circe",
    zioVersion               := Versions.zio,
    scalaVersion             := Scala213,
    scala213                 := Scala213,
    scala3                   := Scala3,
    crossScalaVersions       := List(scala213.value, scala3.value),
    useCoursier              := false,
    Test / parallelExecution := false,
    Test / fork              := true,
    run / fork               := true,
    licenses                 := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
    scalafixDependencies ++= List(
      "com.github.vovapolu"                      %% "scaluzzi" % "0.1.23",
      "io.github.ghostbuster91.scalafix-unified" %% "unified"  % "0.0.9",
    ),
    developers               := List(
      Developer(
        "jirihausner",
        "Jiri Hausner",
        "jiri.hausner.j@gmail.com",
        url("https://github.com/jirihausner"),
      ),
    ),
  ),
)

addCommandAlias("testJVM", "zioSchemaCirceJVM/test; zioSchemaCirceJsoniterJVM/test")
addCommandAlias("testJS", "zioSchemaCirceJS/test; zioSchemaCirceJsoniterJS/test")
addCommandAlias("testNative", "zioSchemaCirceNative/test; zioSchemaCirceJsoniterNative/test")

addCommandAlias("mimaCheck", "+zioSchemaCirce/mimaReportBinaryIssues")

lazy val root = project
  .in(file("."))
  .settings(
    name           := "zio-schema-circe",
    publish / skip := true,
    crossScalaVersions := Nil, // https://www.scala-sbt.org/1.x/docs/Cross-Build.html#Cross+building+a+project+statefully,
  )
  .aggregate(
    zioSchemaCirce.jvm,
    zioSchemaCirce.js,
    zioSchemaCirce.native,
    zioSchemaCirceJsoniter.jvm,
    zioSchemaCirceJsoniter.js,
    zioSchemaCirceJsoniter.native,
  )

def compilerOptions(scalaVersion: String, optimize: Boolean) = {
  val stdOptions = Seq(
    "-deprecation",
    "-encoding",
    "UTF-8",
    "-feature",
    "-unchecked",
    "-language:existentials",
  ) ++ {
    if (sys.env.contains("CI")) {
      Seq("-Xfatal-warnings")
    } else {
      Seq()
    }
  }

  val std2xOptions = Seq(
    "-language:higherKinds",
    "-explaintypes",
    "-Yrangepos",
    "-Xlint:_,-missing-interpolator,-type-parameter-shadow,-infer-any",
    "-Ypatmat-exhaust-depth",
    "40",
    "-Ywarn-numeric-widen",
    "-Ywarn-value-discard",
    "-Wconf:msg=lambda-parens:s",
    "-Xsource:3-cross",
  )

  val optimizerOptions =
    if (optimize)
      Seq(
        "-opt:l:inline",
      )
    else Seq.empty

  val extraOptions = CrossVersion.partialVersion(scalaVersion) match {
    case Some((3, _))  =>
      Seq(
        "-language:implicitConversions",
        "-Xignore-scala2-macros",
        "-Xkind-projector",
        "-source:3.0-migration",
        "-rewrite",
      )
    case Some((2, 13)) =>
      Seq(
        "-opt-warnings",
        "-Ywarn-extra-implicit",
        "-Ywarn-unused",
        "-Ymacro-annotations",
        "-Ywarn-macros:after",
      ) ++ std2xOptions ++ optimizerOptions
      case Some((2, 12)) =>
        Seq(
          "-Ypartial-unification",
          "-opt-warnings",
          "-Ywarn-extra-implicit",
          "-Yno-adapted-args",
          "-Ywarn-inaccessible",
          "-Ywarn-nullary-override",
          "-Ywarn-nullary-unit",
          "-Wconf:cat=unused-nowarn:s"
        ) ++ std2xOptions ++ optimizerOptions
    case _             => Seq.empty
  }

  stdOptions ++ extraOptions
}

lazy val crossProjectSettings = Seq(
  Compile / unmanagedSourceDirectories ++= {
    crossPlatformSources(
      scalaVersion.value,
      crossProjectPlatform.value.identifier,
      "main",
      baseDirectory.value,
    )
  },
  Test / unmanagedSourceDirectories ++= {
    crossPlatformSources(
      scalaVersion.value,
      crossProjectPlatform.value.identifier,
      "test",
      baseDirectory.value,
    )
  },
  nativeConfig ~= { cfg =>
    val os = System.getProperty("os.name").toLowerCase
    // For some unknown reason, we can't run the test suites in debug mode on MacOS
    if (os.contains("mac")) cfg.withMode(Mode.releaseFast)
    else cfg.withGC(GC.boehm) // See https://github.com/scala-native/scala-native/issues/4032
  },
  scalacOptions += {
    if (crossProjectPlatform.value == NativePlatform)
      "-P:scalanative:genStaticForwardersForNonTopLevelObjects"
    else ""
  },
  Test / fork := crossProjectPlatform.value == JVMPlatform, // set fork to `true` on JVM to improve log readability, JS and Native need `false`
)

def stdSettings(projectName: String) = Seq(
  name                     := projectName,
  Compile / compile / scalacOptions ++=
    optionsOn("2.13")("-Wconf:cat=unused-nowarn:s").value,
  scalacOptions ++= compilerOptions(scalaVersion.value, optimize = !isSnapshot.value),
  libraryDependencies ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, x)) if x <= 12 =>
        Seq(
          compilerPlugin(("org.scalamacros" % "paradise" % "2.1.1").cross(CrossVersion.full))
        )
      case Some((2, _)) =>
        Seq(
          compilerPlugin(("org.typelevel" %% "kind-projector" % "0.13.3").cross(CrossVersion.full)),
          compilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1"),
        )
      case _            => List.empty
    }
  },
  Test / parallelExecution := !sys.env.contains("CI"),
  testFrameworks           := Seq(new TestFramework("zio.test.sbt.ZTestFramework")),
) ++ scalafixSettings

lazy val zioSchemaCirce =
  crossProject(JSPlatform, JVMPlatform, NativePlatform)
    .in(file("zio-schema-circe"))
    .enablePlugins(BuildInfoPlugin)
    .settings(stdSettings("zio-schema-circe"))
    .settings(buildInfoSettings("zio.schema.codec.circe"))
    .settings(mimaSettings(binCompatVersionToCompare, failOnProblem = true))
    .settings(enableZIO(enableStreaming = true))
    .settings(
      libraryDependencies ++= Seq(
        "io.circe" %%% "circe-core"            % Versions.circe,
        "io.circe" %%% "circe-generic"         % Versions.circe     % Test,
        "io.circe" %%% "circe-parser"          % Versions.circe,
        "dev.zio"  %%% "zio-schema"            % Versions.zioSchema,
        "dev.zio"  %%% "zio-schema-derivation" % Versions.zioSchema % Test,
        "dev.zio"  %%% "zio-schema-zio-test"   % Versions.zioSchema % Test,
      ),
    )
    .settings(macroDefinitionSettings)
    .settings(crossProjectSettings)
    .settings(Test / fork := crossProjectPlatform.value == JVMPlatform)
    .nativeSettings(Test / fork := false)
    .nativeSettings(
      libraryDependencies ++= Seq(
        "io.github.cquiroz" %%% "scala-java-time" % Versions.scalaJavaTime,
      ),
    )
    .jsSettings(
      libraryDependencies ++= Seq(
        "io.github.cquiroz" %%% "scala-java-time"      % Versions.scalaJavaTime,
        "io.github.cquiroz" %%% "scala-java-time-tzdb" % Versions.scalaJavaTime,
      ),
    )
    .jsSettings(
      scalaJSLinkerConfig ~= { _.withOptimizer(false) },
      scalaJSUseMainModuleInitializer := true,
    )

lazy val zioSchemaCirceJsoniter =
  crossProject(JSPlatform, JVMPlatform, NativePlatform)
    .in(file("zio-schema-circe-jsoniter"))
    .enablePlugins(BuildInfoPlugin)
    .settings(stdSettings("zio-schema-circe-jsoniter"))
    .settings(buildInfoSettings("zio.schema.codec.circe.jsoniter"))
    .settings(mimaSettings(binCompatVersionToCompare, failOnProblem = true))
    .settings(enableZIO(enableStreaming = true))
    .settings(
      libraryDependencies += "com.github.plokhotnyuk.jsoniter-scala" %%% "jsoniter-scala-circe" % Versions.jsoniter,
    )
    .settings(macroDefinitionSettings)
    .settings(crossProjectSettings)
    .settings(Test / fork := crossProjectPlatform.value == JVMPlatform)
    .nativeSettings(
      libraryDependencies ++= Seq(
        "io.github.cquiroz" %%% "scala-java-time" % Versions.scalaJavaTime,
      ),
    )
    .jsSettings(
      libraryDependencies ++= Seq(
        "io.github.cquiroz" %%% "scala-java-time"      % Versions.scalaJavaTime,
        "io.github.cquiroz" %%% "scala-java-time-tzdb" % Versions.scalaJavaTime,
      ),
    )
    .jsSettings(
      scalaJSLinkerConfig ~= { _.withOptimizer(false) },
      scalaJSUseMainModuleInitializer := true,
    )
    .dependsOn(zioSchemaCirce, zioSchemaCirce % "test->test")

lazy val lint = {
  val defaultLint = zio.sbt.Commands.ComposableCommand.lint
  defaultLint.copy(commandStrings = defaultLint.commandStrings :+ "mimaCheck").toCommand
}
