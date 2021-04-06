::#! 2> /dev/null                                   #
@ 2>/dev/null # 2>nul & echo off & goto BOF         #
if [ -z ${SIREUM_HOME} ]; then                      #
  echo "Please set SIREUM_HOME env var"             #
  exit -1                                           #
fi                                                  #
exec ${SIREUM_HOME}/bin/sireum slang run "$0" "$@"  #
:BOF
setlocal
if not defined SIREUM_HOME (
  echo Please set SIREUM_HOME env var
  exit /B -1
)
%SIREUM_HOME%\bin\sireum.bat slang run -n "%0" %*
exit /B %errorlevel%
::!#
// #Sireum

import org.sireum._
import org.sireum.project.ProjectUtil._
import org.sireum.project.{JSON, Project}

val library = "library"

val proyek = "proyek"

val homeDir = Os.slashDir.up.canon

val proyekJvm = moduleJvmPub(
  id = proyek,
  baseDir = homeDir,
  jvmDeps = ISZ(library),
  jvmIvyDeps = ISZ("org.ow2.asm:asm:"),
  pubOpt = pub(
    desc = "Sireum Proyek",
    url = "github.com/sireum/proyek",
    licenses = org.sireum.project.ProjectUtil.bsd2,
    devs = ISZ(robby)
  )
)

val project = Project.empty + proyekJvm

projectCli(Os.cliArgs, project)