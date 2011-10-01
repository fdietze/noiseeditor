Vorraussetzungen: JRE 1.6

zum Starten je nach Betriebssystem "run" bzw "run.bat" ausführen. Es wird automatisch kompiliert.
Das Skript "rerun" lauscht auf Dateiänderungen und kompiliert und startet das Programm bei jeder Änderung. Dies ist u.a. beim exportieren vom NoiseEditor in die GameEngine sinnvoll, da dann die Engine automatisch mit der neuen Welt gestartet wird. Wichtig hierbei ist, dass die Projektordner "noiseeditor" und "gameengine" im selben Verzeichnis liegen, da der Noiseeditor relativ auf die Engine zugreift und Dateien überschreibt.

Die Simple-Build-Tool-Konsole wird mit sbt gestartet. Befehle dafür: run, ~run, compile, clean. Weitere Informationen dazu: https://github.com/harrah/xsbt/wiki

Der Datei src/main/scala/Config.scala lassen sich die Tastaturbelegungen entnehmen und ändern. Zudem lassen sich hier weitere Änderungen vornehmen. Die Standardbelegungen sind:
