@echo off
set JAVA_OPTS=-Xmx1024m
scala -cp target\classes org.jesperdj.mandelactors.Main
