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
    libraryDependencies += "org.typelevel"                   %% "cats-effect"                % "2.2.0",
    libraryDependencies += "au.id.tmm.ausgeo"                %% "ausgeo-core"                % "0.2.4",
    libraryDependencies += "au.id.tmm.tmm-scala-collections" %% "tmm-scala-collections-core" % tmmCollectionsVersion,
    libraryDependencies += "au.id.tmm.tmm-scala-collections" %% "tmm-scala-collections-cats" % tmmCollectionsVersion,
    libraryDependencies += "au.id.tmm.tmm-utils"             %% "tmm-utils-syntax"           % tmmUtilsVersion,
    libraryDependencies += "au.id.tmm.tmm-utils"             %% "tmm-utils-errors"           % tmmUtilsVersion,
    libraryDependencies += "au.id.tmm.tmm-utils"             %% "tmm-utils-cats"             % tmmUtilsVersion,
    libraryDependencies += "org.jsoup"                        % "jsoup"                      % "1.13.1",
    libraryDependencies += "com.amazonaws"                    % "aws-java-sdk-textract"      % "1.11.890",
    libraryDependencies += "org.slf4j"                        % "slf4j-api"                  % "1.7.30",
    libraryDependencies += "org.slf4j"                        % "slf4j-simple"               % "1.7.30" % Runtime,
  )

addCommandAlias("check", ";+test;scalafmtCheckAll")
