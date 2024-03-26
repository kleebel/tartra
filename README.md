

# Forschungsprojekt: Tarifpolitik in der Transformation

Dieses Repository enthält die Daten, Skripte und Dokumentationen für unser Forschungsprojekt bzw. den automatisiert-quantitativen Teil. Momentan befinden wir uns noch in der Explorations- und Aufbereitungsphase. Die Daten stammen aus dem [Hamburger Tarifregister](https://www.hamburg.de/tarifvertraege/#) und wurden per OCR eingelesen.

### Übersicht

**1. data**<br>
"all_join.Rdata"
: unbereinigter OCR Datensatz <br>
"data_clean.rds"
: bereinigter OCR Datensatz <br>
**dfm** <br>
"dfm_preprocessed.rds"
: bereinigte Document-Feature-Matrix <br>
"dfm_matrix.csv"
: bereinigte dfm als lesbarer Datensatz <br>
**stopwords** <br>
"staedte_osm.txt"
: Liste im Textformat mit über 14.000 Städtenamen in Deutschland, Daten von [OpenStreetMap](http://www.openstreetmap.org/), extrahiert von [datenbörse.net](https://www.datenbörse.net/item/Liste_von_deutschen_Staedtenamen_.csv) <br>
"stopwords.csv"
: alle selbst zusammengestellten, [iterativ im Prozess gesammelten](https://pad.innocampus.tu-berlin.de/p/Stoppwörter_Tarifpolitik) Stoppwörter <br>

**2. scripts** <br>
"preprocessing.R"
: Aufbereitung des OCR-Datensatzes, Textbereinigung vor der Tokenisierung, Korpuserstellung & -aufbereitung, DFM erstellen & filtern & trimmen <br>
