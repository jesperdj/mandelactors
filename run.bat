@echo off
set JAVA_OPTS=-Xms256m -Xmx1024m
scala -cp target\classes org.jesperdj.mandelactors.Main
