@echo off
setlocal ENABLEDELAYEDEXPANSION

cls
title %CD% - %0

rem assume mvn is in path
ECHO Calling maven to build CLASSPATH...
CALL mvn -q dependency:build-classpath -Dmdep.outputFile=target\classpath.txt || EXIT /B 1

FOR /F "delims=" %%p IN (target\classpath.txt) DO SET CLASSPATH=%%p

SET CLASSPATH=target\classes;%CLASSPATH%

SET BASEDIR=%~dp0

java org.psug.usi.Main
