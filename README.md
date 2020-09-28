# fp_editor
 
Der Syntax-aufmerksame Editor fuer die funktionale Sprache aus Aufgabe 2 wurde in Haskell implementiert.

## how to compile

Um den Editor moeglichst simpel zu halten, laeuft dieser im Terminal. Um Trotzdem die UX zu verbessern, wird das Haskell-Paket terminal-size verwendet um die Groesse des Terminals zu erfassen. Dieses Paket kann mittels `cabal install --lib terminal-size` installiert werden.

Durch `ghc editor.hs` kann der Editor kompiliert werden.

Im Ordner `releases` liegt zusaetzlich eine kompilierte Version fuer Linux.

## how to run

```bash
./editor file
```

Dies oeffnet `file` falls es existiert, ansonsten wird ein Dokument mit ebendiesem Namen erstellt.

Der Editor wird folgendermassen bedient:
Mit den Pfeiltasten wird der Cursor bewegt.
Mit ESC wird das geoeffnete Dokument gespeichert.
Mittels Ctrl+C kann der Editor beendet werden.


## design decisions

Der Text wird durch 2 Listen p und q dargestellt, wobei p den Text vor dem Cursor und q den Text nach dem Cursor beschreibt. So entspricht das Einfuegen von einem Zeichen dem Anfuegen eines Zeichens an eine Liste.
Ausgehend von diesen Listen wird die Position des Cursors im Terminal bestimmt.

Aufgrund der Schwierigkeit auf den Raw Keyboard Input in Haskell zuzugreifen, aber Pfeiltasten und ESC mittels Escape-Sequenzen erkannt werden koennen, haben wir uns dafuer entschieden die ESC Taste zum Speichern des Dokuments zu verwenden.


## allocation of work

Herr Weissenfels hat sich vorallem mit dem Highlighting auseinandergesetzt, waehrend Herr Regen das Geruest des Editors gebaut hat. Tatsaechlich war der Entwicklungsprozess ineinander verflochten und eine genauere Aufteilung ist den Git-Logs zu entnehmen.

Hiermit bestaetigen wir, dieses Programm persoenlich entwickelt zu haben.
