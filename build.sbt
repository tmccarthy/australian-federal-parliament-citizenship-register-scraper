val settingsHelper = ProjectSettingsHelper("au.id.tmm", "australian-federal-parliament-citizenship-register-scraper")(
  githubProjectName = "australian-federal-parliament-citizenship-register-scraper",
)

settingsHelper.settingsForBuild

lazy val root = project
  .in(file("."))
  .settings(settingsHelper.settingsForRootProject)
  .settings(console := (console in Compile in core).value)
  .aggregate(
    core,
  )

val tmmCollectionsVersion = "0.0.4"
val tmmUtilsVersion = "0.6.2"

lazy val core = project
  .in(file("core"))
  .settings(settingsHelper.settingsForSubprojectCalled("core"))
  .settings(
    libraryDependencies += "org.typelevel"                   %% "cats-effect"                    % "2.2.0",
    libraryDependencies += "au.id.tmm.ausgeo"                %% "ausgeo-core"                    % "0.2.4",
    libraryDependencies += "au.id.tmm.tmm-scala-collections" %% "tmm-scala-collections-core"     % tmmCollectionsVersion,
    libraryDependencies += "au.id.tmm.tmm-scala-collections" %% "tmm-scala-collections-cats"     % tmmCollectionsVersion,
    libraryDependencies += "au.id.tmm.tmm-utils"             %% "tmm-utils-syntax"               % tmmUtilsVersion,
    libraryDependencies += "au.id.tmm.tmm-utils"             %% "tmm-utils-errors"               % tmmUtilsVersion,
    libraryDependencies += "au.id.tmm.tmm-utils"             %% "tmm-utils-cats"                 % tmmUtilsVersion,
    libraryDependencies += "au.id.tmm.tmm-utils"             %% "tmm-utils-codec"                % tmmUtilsVersion,
    libraryDependencies += "com.softwaremill.sttp.client3"   %% "core"                           % "3.0.0-RC7",
    libraryDependencies += "com.softwaremill.sttp.client3"   %% "async-http-client-backend-cats" % "3.0.0-RC7",
    libraryDependencies += "org.jsoup"                        % "jsoup"                          % "1.13.1",
    libraryDependencies += "software.amazon.awssdk"           % "textract"                       % "2.15.33",
    libraryDependencies += "software.amazon.awssdk"           % "s3"                             % "2.15.33",
    libraryDependencies += "org.slf4j"                        % "slf4j-api"                      % "1.7.30",
    libraryDependencies += "org.slf4j"                        % "slf4j-simple"                   % "1.7.30" % Runtime,
  )

addCommandAlias("check", ";+test;scalafmtCheckAll")
