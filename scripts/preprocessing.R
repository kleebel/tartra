#
# PREPROCESSING
#


#
# Packages ----
#

library(tidyverse)
library(cld2)
library(cld3)
library(quanteda)
library(tm)
library(SnowballC)
library(textTinyR)
library(textmineR)
library(textutils)
library(Matrix)
library(textreuse)


#
# Laden des Datensatzes & Textbereinigung vor der Tokenisierung ----
#

load("data/all_join.Rdata")
data <- data_join


#
# Aufbereitung des Datensatzes ----
#

# Auswahl von Kategorien von Tarifverträgen
data <- data %>% 
  filter(!kategorien %in% c("Entgelttarifvertrag, Vergütungstarifvertrag", 
                            "Gehaltstarifvertrag", 
                            "Lohntarifvertrag",
                            "Entgeltrahmentarifvertrag, Eingruppierungstarifvertrag"
  ))

# IDs zuweisen
# ID lang
data$realfilename <- paste(data$index, "_", data$filename, sep = "")
data$ID <- paste(data$realfilename, data$page, sep=" ")
data <- unique(data)

# ID kurz
data$ID_kurz <- NA
data$ID_kurz <- 1:length(data$ID)


# Textspalte kleinschreiben & überflüssige Leerzeichen entfernen
data$text <- data$rawtext %>%
  str_squish() %>% 
  tolower()
data <- data %>% 
  select(!rawtext)


# fremdsprachige Texte entfernen
data <- data %>% 
  mutate(cld2 = detect_language(text = text), 
         cld3 = detect_language(text = text))

data <- data %>% 
  filter(cld2 == "de" & cld3 == "de")
# von 77005 noch 72918 übrig


# Wörter mit Bindestrichen zusammenführen
word_as_one <- function(text) {
  # Ersetzt Muster wie "Wort- Wort" durch "WortWort"
  corrected_text <- str_replace_all(text, "\\b-\\s", "")
  return(corrected_text)
}
data$text <- word_as_one(data$text)


# Mehrwortausdrücke entfernen
stopp_mehr <- c("Industriegewerkschaft Metall", "IG Metall", 
                "Industriegewerkschaft Bergbau, Chemie, Energie", 
                "Gewerkschaft Erziehung und Wissenschaft", 
                "Vereinte Dienstleistungsgewerkschaft", 
                "Industriegewerkschaft Bauen, Agrar, Umwelt", 
                "IG BAU", "Gewerkschaft Nahrung, Genuss, Gaststätten", 
                "Eisenbahn- und Verkehrsgewerkschaft", 
                "Gewerkschaft der Polizei", "beamtenbund und tarifunion", 
                "Berufsverband Bayerischer Hygieneinspektoren", 
                "Deutsche Zoll- und Finanzgewerkschaft", 
                "Bund der Strafvollzugsbediensteten Deutschlands", 
                "Gewerkschaft Mess- und Eichwesen", 
                "Gewerkschaft Technik und Naturwissenschaft", 
                "Bund Deutscher Forstleute", "Bund Deutscher Rechtspfleger", 
                "Bundesverband der Lehrkräfte für Berufsbildung", 
                "Deutscher Amtsanwaltsverein", "Deutsche Justiz-Gewerkschaft", 
                "Deutscher Gerichtsvollzieherbund", "Deutscher Philologenverband", 
                "Deutsche Polizeigewerkschaft", "Deutsche Steuer-Gewerkschaft", 
                "Deutsche Verwaltungs-Gewerkschaft", "Deutscher Berufsverband für Soziale Arbeit",
                "Fachverband der Bediensteten der Landwirtschaftskammer NRW", 
                "Fachverband Gesundheitswesen Baden-Württemberg", 
                "Fachverband Wasserstraßen- und Schifffahrtsverwaltung", 
                "Gewerkschaft Deutscher Lokomotivführer", "Gewerkschaft für das Gesundheitswesen", 
                "Gewerkschaft der Sozialversicherung", "Gewerkschaft der Sozialverwaltung", 
                "Katholische Erziehergemeinschaft Deutschlands", "komba gewerkschaft", 
                "Kommunikationsgewerkschaft DPV", "Kommunikationsgewerkschaft", 
                "Gewerkschaft für das Gesundheitswesen in Bayern", "Gewerkschaft Arbeit und Soziales", 
                "Bundesbankgewerkschaft", "Berufsverband Agrar, Ernährung, Umwelt", 
                "Fachgewerkschaft der Straßen- und Verkehrsbeschäftigten", 
                "Verband Bildung und Erziehung", "Verband der Arbeitnehmer der Bundeswehr", 
                "Verband der Beamten und Beschäftigten der Bundeswehr", "Gewerkschaft Bundesbeschäftigte", 
                "Verband der Beschäftigten des Gewerblichen Rechtsschutzes", 
                "Verband Deutscher Realschullehrer", "Vereinigung Cockpit", 
                "Verband Hochschule und Wissenschaft", "Verein der Rechtspfleger im Bundesdienst", 
                "Die Mediengewerkschaft", "Christlicher Gewerkschaftsbund", 
                "Christliche Gewerkschaft Metall", "Gewerkschaft Öffentlicher Dienst und Dienstleistungen", 
                "Arbeitnehmerverband dt. Milchkontroll- u. Tierzuchtbediensteter", 
                "Christliche Gewerkschaft Postservice und Telekommunikation", 
                "Die Berufsgewerkschaft e. V. im CGB", "Die Berufsgewerkschaft", 
                "Beschäftigtenverband Industrie, Gewerbe, Dienstleistung", 
                "Bund der Hotel-, Restaurant- und Cafeangestellten", "Union Ganymed", 
                "Christliche Gewerkschaft Bergbau, Chemie, Energie", 
                "Christliche Gewerkschaft Deutscher Eisenbahner", 
                "Gewerkschaft für Kunststoffgewerbe und Holzverarbeitung", 
                "Gewerkschaft für Kunststoffgewerbe und Holzverarbeitung im CGB", 
                "Gewerkschaft Transport & Logistik", 
                "Fachgewerkschaft für Arbeitnehmer des Transport- und Logistikgewerbes", 
                "Marburger Bund", "Deutsche Angestelltengewerkschaft", "Deutsche Postgewerkschaft", 
                "IG Medien", "Gewerkschaft Öffentliche Dienste, Transport und Verkehr", 
                "Gewerkschaft Handel, Banken und Versicherungen", "Verkehrsgewerkschaft",
                "Arbeitgeber- und Wirtschaftsverbände Sachsen-Anhalt", 
                "Die Unternehmensverbände im Lande Bremen", "Die Arbeitgeber.", "unternehmer nrw", 
                "Landesvereinigung der Unternehmensverbände Nordrhein-Westfalen", 
                "Landesvereinigung Unternehmerverbände Rheinland-Pfalz", 
                "Die Unternehmer in Rheinlad-Pfalz", "Unternehmer Baden-Württemberg", 
                "Unternehmerverbände Niedersachsen", 
                "Vereinigung der Unternehmensverbände in Hamburg und Schleswig-Holstein", 
                "Vereinigung der Bayerischen Wirtschaft", "Verband der Wirtschaft Thüringens", 
                "Vereinigung der hessischen Unternehmerverbände", 
                "Vereinigung der Saarländischen Unternehmensverbände", "Die Arbeitgeber im Saarland.", 
                "Vereinigung der Sächsischen Wirtschaft", 
                "Vereinigung der Unternehmensverbände für Mecklenburg-Vorpommern", 
                "VU Die Arbeitgeber Mecklenburg-Vorpommern", 
                "Vereinigung der Unternehmensverbände in Berlin und Brandenburg", 
                "Unternehmensverbände Berlin-Brandenburg", 
                "Arbeitgeberverband für Telekommunikation und IT", "agv:community", "agv comunity", 
                "Arbeitgeberverband der Deutschen Immobilienwirtschaft", "Arbeitgeberverband Pflege", 
                "Arbeitgeberverband Postdienste", "bpa Arbeitgeberverband", 
                "Bundesarbeitgeberverband der Personaldienstleister", "Bundesverband Briefdienste", 
                "Bundesverband der Sicherheitswirtschaft", "Bundesverband der Systemgastronomie", 
                "Deutscher Bühnenverein", "Bundesverband der Theater und Orchester", 
                "Deutscher Hotel- und Gaststättenverband", "Arbeitgeberverband deutscher Fitness- und Gesundheits-Anlagen", 
                "Arbeitgeberverbände energie- und versorgungswirtschaftlicher Unternehmungen", 
                "Arbeitgeberverband der Versicherungsunternehmen in Deutschland", "Die Versicherer als Arbeitgeber", 
                "Arbeitgeberverband des privaten Bankgewerbes", "Bundesverband Großhandel, Außenhandel, Dienstleistungen", 
                "Handelsverband Deutschland", "Unternehmerverband Deutsches Handwerk", 
                "Zentralverband des Deutschen Baugewerbes", "Das deutsche Baugewerbe", 
                "Arbeitgeberverband der Cigarettenindustrie", "Arbeitgeberverband der Deutschen Kautschukindustrie", 
                "Arbeitgeberverband der Deutschen Lederindustrie", "Vereinigung Bergischer Unternehmerverbände", 
                "Arbeitgeberverband Stahl", "Arbeitgebervereinigung Nahrung und Genuss", 
                "Branchenverband Steinkohle und Nachbergbau", "Bundesarbeitgeberverband Chemie", 
                "Bundesarbeitgeberverband Glas und Solar", "Glas Solar", "Bundesverband Druck und Medien", 
                "Bundesverband Keramische Industrie", "Deutscher Braunkohlen-Industrie-Verein", 
                "Gesamtverband der deutschen Textil- und Modeindustrie", "Gesamtverband der metallindustriellen Arbeitgeberverbände", 
                "Hauptverband der Deutschen Bauindustrie", 
                "Hauptverband der Holzindustrie und Kunststoffe verarbeitenden Industrie und verwandter Industrie- und Wirtschaftszweige", 
                "Hauptverband der deutschen Holzindustrie", "Hauptverband Papier- und Kunststoffverarbeitung", 
                "Sozialpolitische Arbeitsgemeinschaft Steine und Erden", "Verein der Zuckerindustrie", "Die Papierindustrie", 
                "Verband der Kali- und Salzindustrie", "Bundesverband Garten-, Landschafts- und Sportplatzbau", 
                "Gesamtverband der Deutschen Land- und Forstwirtschaftlichen Arbeitgeberverbände", 
                "Arbeitgeber- und Wirtschaftsverband der Mobilitäts- und Verkehrsdienstleister", "Agv MoVe", 
                "Verband der Mobilitäts- und Verkerhsdienstleister", "Arbeitgeberverband Deutscher Eisenbahnen", 
                "Arbeitgeberverband Luftverkehr", "Sozialpolitische Arbeitsgemeinschaft Verkehr", "Verband Deutscher Reeder")
# Kleinschreiben
stopp_mehr <- stopp_mehr %>% 
  tolower()
# Entfernen
data$text <- Reduce(function(text, pattern) gsub(pattern, "", text), stopp_mehr, init = data$text)
# mehrwortige Stoppwörter abspeichern
# stopp_mehr <- as_tibble(stopp_mehr)
# write_csv2(stopp_mehr, "data/stopwords/stopp_mehr.csv")

# Firmennamen entfernen

# Firmennamen rausziehen
firmen <- unique(data$branchen_betriebe)
# Klammern und deren Inhalt entfernen, dann Einträge in Anführungszeichen setzen
firmen <- sapply(firmen, function(x) {
  x <- gsub("\\s*\\(.*?\\)", "", x)  # Entferne Text in Klammern und die Klammern selbst
  x <- gsub("\\s*\\(", "", x)  # Entferne auch öffnende Klammern, wenn keine schließende Klammer vorhanden ist
  x <- gsub("\\s*\\).*?", "", x) # <entferne auch schließende Klammern, wenn keine öffnende Klamme vorhanden ist
  paste0("'", x, "'")
}, USE.NAMES = FALSE)
# Firmennamen kleinschreiben & Leerzeichen entfernen
firmen <- firmen %>% 
  str_squish() %>% 
  tolower()
# Entfernen
data$text <- Reduce(function(text, pattern) gsub(pattern, "", text), firmen, init = data$text)
# Firmennamen abspeichern
# firmen <- as_tibble(firmen)
# write_csv2(firmen, "data/stopwords/firmen.csv")

# Adressen entfernen
data$text <- gsub("\\b[A-Za-zäöüß]+straße\\s\\d+", "", data$text)
data$text <- gsub("\\b[A-Za-zäöüß]+strasse\\s\\d+", "", data$text)
data$text <- gsub("\\b[A-Za-zäöüß]+str\\s\\d+", "", data$text)
data$text <- gsub("\\b[A-Za-zäöüß]+str.\\s\\d+", "", data$text)


# Wörter mit weniger als 3 Zeichen entfernen
data$text <- str_replace_all(data$text, "\\b\\w{1,2}\\b", "")


# Wörter, die Zahlen enthalten entfernen
data$text <- gsub("\\b\\S*\\d+\\S*\\b", "", data$text)


# bereinigten Datensatz zwischenspeichern
saveRDS(data, file = "data/data_clean.rds")


# bereinigte Daten lesen
data <- readRDS(file = "data/data_clean.rds")


# Texte für jeden Tarifvertrag zusammenführen
data_tv <- data %>%
  group_by(filename) %>%
  summarise_all(list(~first(.))) %>%
  mutate(text = paste(text, collapse = " ")) %>%
  select(-page) %>%
  ungroup()


#
# Sammlung Stoppwörter(1 Wort) ----
#

# Stoppwörter Rechtssprache 
stopp_jura <- read_csv("https://zenodo.org/record/3995594/files/SW-DE-RS_v1-0-0_Datensatz.csv?download=1")
stopp_jura <- stopp_jura %>% 
  unlist() %>% 
  unname() %>% 
  as.character() %>% 
  as_tibble() %>% 
  filter(value != "")
colnames(stopp_jura) <- "wort"


# Stoppwörter deutsch allgemein 
# Liste 1
stopp_allg_1 <- read_csv("https://raw.githubusercontent.com/solariz/german_stopwords/master/german_stopwords_full.txt", 
                         skip = 8)
colnames(stopp_allg_1) <- "wort"

# Liste 2
stopp_allg_2 <- read_csv("https://raw.githubusercontent.com/stopwords-iso/stopwords-de/master/stopwords-de.txt")
colnames(stopp_allg_2) <- "wort"


# Ort und Zeit 
stopp_ort <- c("Baden-Württemberg", "Bayern", "Berlin", 
               "Brandenburg", "Bremen", "Hamburg", "Hessen", 
               "Mecklenburg-Vorpommern", "Niedersachsen", 
               "Nordrhein-Westfalen", "Rheinland-Pfalz", 
               "Saarland", "Sachsen", "Sachsen-Anhalt", 
               "Schleswig-Holstein", "Thüringen", 
               "ort", "nord", "sued", "süd", "ost", 
               "west", "räumlich", "deutsch", 
               "Deutschland", "Bundesrepublik", 
               "land", "bezirk", "gebiet",
               "montag", "dienstag", "mittwoch", 
               "donnerstag", "freitag", "tag", "woche", 
               "monat", "jahr", "kalendertag", 
               "kalenderwoche", "kalendermonat", 
               "kalenderjahr", "täglich", "wöchentlich",
               "monatlich", "jährlich", "zeitraum", "datum", "laufzeit")
stopp_ort <- as_tibble(stopp_ort) 
colnames(stopp_ort) <- "wort"

# Städte 
stopp_staedte <- read_csv("data/stopwords/staedte_osm.txt", 
                        col_names = FALSE)
colnames(stopp_staedte) <- "wort"

# Gewerkschafts- und Tarifbezug ()
stopp_tarif <- c("IGBCE","GEW", "ver.di", "NGG", "EVG", "GdP", 
                 "dbb", "BBH", "BDZ", "BSBD", "BTE", "BTB", 
                 "BDF","BDR", "BvLB", "DAAV", "DJG", "DGVB", 
                 "DPhV", "DPolG", "DSTG", "DVG", "DBSH", 
                 "FVG", "FWSV", "GDL", "GeNi", "GdS", "GdV", 
                 "KEG", "DPV", "DPVKOM", "LBB", "vbba", "VdB", 
                 "VDL", "VDStra", "VBE", "VBB", "vbob", "VBGR", 
                 "VC", "vhw", "vrb", "vrff", "CGB", "CGM", "GÖD", 
                 "ADM", "CGPT", "DHV", "BigD", "CGBCE", "CGDE", 
                 "GKH", "GTL", "DAG", "DPG", "ÖTV", "HBV", "GDBA", 
                 "AWSA", "LVU", "UBW", "UVNord", "vbw", "vwr", 
                 "vhu", "vsu", "VSW", "VU", "UVB", "agv", "bpa", 
                 "BAP", "bbd", "BDSW", "BdS", "DEHOGA", "DSSV", 
                 "VAEU", "AGVBanken", "BGA","HDE","UDH", "AdC", 
                 "ADK", "VBU", "ANG", "bsn", "BAVC", "BAGV", 
                 "BVKI", "DEBRIV", "textil+mode", "Gesamtmetall", 
                 "Bauindustrie", "HDH", "hpv", "spo", "VKS", 
                 "GLFA", "AGVDE", "AGVL", "SAV", "tarifvertrag", 
                 "tarifvertragspartei", "Geschäftsführer", 
                 "Geschäftsführerin", "Geschäftsführung", 
                 "Geschäftsleitung", "Tarifgemeinschaft", 
                 "Innung", "Bund", "Bundesvorstand","Vorstand",  
                 "Geltungsbereich", "gültig", "Anhang", 
                 "Abschnitt", "protokollnotiz", "gewerkschaft", 
                 "dienstleistungsgewerkschaft", "arbeitgeberverband", 
                 "inhaltsverzeichnis", "unterschrift", "schriftlich", 
                 "landesbezirk", "landesbezirkleiter", "landesbezirkleiterin", 
                 "konzerntarifvertrag", "organisationstarifvertrag",
                 "telekom", "lufthansa")
stopp_tarif <- as_tibble(stopp_tarif) 
colnames(stopp_tarif) <- "wort"

# Reste
stopp_reste <- c("ohnehin", "zusätzlich", "zusätzliche", 
                 "11a", "--", "-einerseits-", "-andererseits-", 
                 "_", "__", "___", "____", "7a", "oo", "nn", 
                 "be","ss","dtag", "protokolinotiz", letters, LETTERS)
stopp_reste <- as_tibble(stopp_reste) 
colnames(stopp_reste) <- "wort"


# alle Stoppwörter Listen (1 Wort) zusammenfügen
stopwords <- full_join(stopp_allg_1, stopp_allg_2)
stopwords <- full_join(stopwords, stopp_ort)
stopwords <- full_join(stopwords, stopp_jura)
stopwords <- full_join(stopwords, stopp_reste)
stopwords <- full_join(stopwords, stopp_staedte)
stopwords <- full_join(stopwords, stopp_tarif)

# kleinschreiben
stopwords <- stopwords %>%
  mutate(wort = str_to_lower(wort, locale = "de"))
# abspeichern
# write_csv2(stopwords, "data/stopwords/stopwords.csv")
# lesen
stopwords <- read_csv2("data/stopwords/stopwords.csv")

# kleinschreiben
stopwords <- tolower(stopwords)


#
# gestemmte Stoppwörter (ACHTUNG: ERST NACH DEM STEMMING ENTFERNEN) ----
#

# gestemmt 
stopp_stem <- c("tarifvertrag", "arbeitnehm", "tarif", "beschaftigt", 
                "anlag", "betrieb", "gmbh", "arbeitgeb", "tv", "gesetz", 
                "gesetz", "kalenderjahr", "fassung", "tarifvertragspartei", 
                "kalendermonat", "protokollnotiz", "protokollerklar", 
                "zeitraum", "arbeit", "vereinbar", "ziff", "vereinbart", 
                "gelt", "erfolgt", "woch", "tarifvertragspartei", 
                "geltungsbereich", "tatig", "entsprech", "euro", 
                "bestimm", "gezahlt", "betragt", "erfor", "ver", "eur", 
                "abgeschloss", "erhalt", "fall", "besond", "betrag", 
                "beginn", "voraussetz", "ablauf", "geltend", "gewahrt", 
                "einschliess", "grund", "ander", "sonstig", "gem", 
                "inkrafttret", "berechn", "deutsch", "geleistet", 
                "spatest", "beendig", "endet", "zahl", "telekom", 
                "erhoht", "vertrag", "gewerkschaft", "wirkung", "pro", 
                "allgemein", "beschaft", "massgab", "massgeb", "schwierig", 
                "durchschnitt", "ersetzt", "ill", "findet", "abschluss", 
                "ausub", "gewahr", "geregelt", "vollendet", "absatz", 
                "unberuhrt", "nachvollg", "erstmal", "letzt", "laufzeit", 
                "zusatz", "eee", "erworb", "buchstab", "wort", "gultig", 
                "zustand", "erfullt", "notwend", "jahrlich", "tabell", 
                "leit", "verpflicht", "verlang", "verfug", "berucksichtigt", 
                "kost", "anerkannt", "ununterbroch", "main", "ausschliess", 
                "jahr", "kalendertag", "erganz", "nord", "zweck", "fertig", 
                "gebiet", "erreich", "beginnt", "dienstleistungsgewerkschaft", 
                "tarifpartei", "einzelvertrag", "nahrung-genuss-gaststatt", 
                "arbeitgeberverband", "landesbezirk", "protokoll", "-kraft-tret", 
                "lufthansa", "geandert", "gefasst", "gestrich", "eingefugt", 
                "angefugt", "satz", "punkt", "betreff", "inhaltsverzeichnis", 
                "hierbei", "sinngemäss", "land", "bauen-agrar-umwelt", 
                "energi", "unterzeichn", "bezirk", "kust", "kraft", "frist", 
                "gekundigt", "kundig", "erstmal", "fruhest", "vertrag", 
                "schlussbestimm", "nachwirk", "wirkung", "partei", 
                "verhandl", "verpflicht", "abweich", "monats", "tret")


#


#
# Seitenbasiert ----
#

#### Korpuserstellung & Tokenisierung

# Korpus erzeugen
data_corp <- corpus(data, docid_field = "ID_kurz", text_field = "text")

# Korpus aufbereiten & tokenisieren
data_tok <- data_corp %>% 
  tokens(remove_punct = TRUE,   # Punkte entfernen
         remove_numbers = TRUE, # Nummern entfernen
         remove_symbols = TRUE,  # Symbole entfernen
         remove_url = TRUE, # URL entfernen
         remove_separators = TRUE # Separatoren entfernen
         ) %>% 
  tokens_tolower() %>% 
  tokens_remove(stopwords("german")) %>%  # Stoppwörter aus Package entfernen
  tokens_remove(stopwords) %>%            # selbst erstellte Stoppwörter entfernen
  tokens_wordstem(language="german") %>%  # Auf Wortstamm beschränken
  tokens_remove(stopp_stem)               # gestemmte Stoppwörter entfernen

# gestemmte Stoppwörter abspeichern
# stopp_stem <- as_tibble(stopp_stem)
# write_csv2(stopp_stem, "data/stopwords/stopp_stem.csv")


#### DFM

# Matrix erstellen
dfm <- data_tok %>% 
  dfm() #%>% 
  # dfm_weight("boolean")

# Seiten & wörter zählen
dim(dfm)
# 72918 Seiten & 175814 Wörter

# zwischenspeichern
saveRDS(dfm, file = "data/dfm/dfm.rds")
# laden, damit nicht immer neu durchführen
dfm <- readRDS(file = "data/dfm/dfm.rds")



#
# Tarifvertragsbasiert ----
# 

#### Korpuserstellung & Tokenisierung 

# Korpus erzeugen
data_corp_tv <- corpus(data_tv, docid_field = "ID_kurz", text_field = "text")

# Korpus aufbereiten & tokenisieren
data_tok_tv <- data_corp_tv %>% 
  tokens(remove_punct = TRUE,   # Punkte entfernen
         remove_numbers = TRUE, # Nummern entfernen
         remove_symbols = TRUE,  # Symbole entfernen
         remove_url = TRUE, # URL entfernen
         remove_separators = TRUE # Separatoren entfernen
         ) %>% 
  tokens_tolower() %>% 
  tokens_remove(stopwords("german")) %>%  # Stoppwörter aus Package entfernen
  tokens_remove(stopwords) %>%            # selbst erstellte Stoppwörter entfernen
  tokens_wordstem(language="german") %>%  # Auf Wortstamm beschränken
  tokens_remove(stopp_stem) 


#### DFM

# Matrix erstellen
dfm_tv <- data_tok_tv %>% 
  dfm() #%>% 
# dfm_weight("boolean")

# Dokumente & wörter zählen
dim(dfm_tv)
# 72918 TVs & 175814 Wörter

# zwischenspeichern
saveRDS(dfm, file = "data/dfm/dfm_tv.rds")
# laden, damit nicht immer neu durchführen
dfm <- readRDS(file = "data/dfm/dfm_tv.rds")


#
# DFM filtern: inhaltsgleiche Seiten entfernen ----
#

#### Seiten-Ebene

# Schwellenwert von 80% Ähnlichkeit definieren
threshold <- 0.8

# Matrix umwandeln (0 Felder zu NULL Feldern machen)
sparse_dfm <- as(dfm, "sparseMatrix")

# BEI JJ & CHANTAL INS BUCH SCHAUEN --> ab S. 237

# Rechner zu schwach: Fehler: kann Vektor der Größe 39.6 GB nicht allozieren
# Berechnen der Kosinusähnlichkeiten zwischen den Dokumenten
similarity_matrix <- crossprod(sparse_dfm) / (sqrt(rowSums(sparse_dfm^2) %*% t(rowSums(sparse_dfm^2))))
# Setzen der Diagonalen auf 0 (da Dokumente nicht mit sich selbst verglichen werden sollen)
diag(similarity_matrix) <- 0
# Identifizieren der Dokumente, die entfernt werden sollen
docs_to_remove <- rownames(similarity_matrix)[rowSums(similarity_matrix > threshold) > 1]

# Entfernen der Dokumente aus der Dokument-Term-Matrix (dfm)
dfm_filtered <- dfm[!(rownames(dfm) %in% docs_to_remove), ]

# Entfernen der Dokumente aus der Liste der Dokumente
data <- data[!(rownames(data) %in% docs_to_remove)]

# Seiten & Wörter zählen 
dim(dfm_filtered)
# xx Seiten & xx Wörter

# über Package "TextReuse"
# https://docs.ropensci.org/textreuse/ 
# Korpus erstellen
corpus <- TextReuseCorpus(text = data$text, 
                          meta = list("docid" = "ID_kurz"), 
                          tokenizer = tokenize_words)
# Dokumente vergleichen
comparisons <- pairwise_compare(corpus, jaccard_similarity)
# trotzdem Fehlermeldeung: "kann Vektor der Größe 19.8 GB nicht allozieren"
pairwise_candidates(comparisons)



#### TV-Ebene

# Matrix umwandeln (0 Felder zu NULL Feldern machen)
sparse_dfm_tv <- as(dfm_tv, "sparseMatrix")

# Rechner zu schwach: Fehler: kann Vektor der Größe 39.6 GB nicht allozieren
# Berechnen der Kosinusähnlichkeiten zwischen den Dokumenten
similarity_matrix_tv <- crossprod(sparse_dfm_tv) / (sqrt(rowSums(sparse_dfm_tv^2) %*% t(rowSums(sparse_dfm_tv^2))))
# Setzen der Diagonalen auf 0 (da Dokumente nicht mit sich selbst verglichen werden sollen)
diag(similarity_matrix_tv) <- 0
# Identifizieren der Dokumente, die entfernt werden sollen
docs_to_remove_tv <- rownames(similarity_matrix_tv)[rowSums(similarity_matrix_tv > threshold) > 1]

# Entfernen der Dokumente aus der Dokument-Term-Matrix (dfm)
dfm_filtered_tv <- dfm_tv[!(rownames(dfm_tv) %in% docs_to_remove_tv), ]

# Entfernen der Dokumente aus der Liste der Dokumente
data_tv <- data_tv[!(rownames(data_tv) %in% docs_to_remove_tv)]

# Dokumente & Wörter zählen 
dim(dfm_filtered_tv)
# xx TVs & xx Wörter


#
# DFM trimmen ----
#

# trimmen
dfm_filtered_trimmed <- dfm_trim(dfm_filtered,
                         max_docfreq = 0.30, # Wörter, die in mehr als 30% der Dokumente vorkommen, werden entfernt
                         min_docfreq = 0.01, # ein Wort muss in mindestens 1% der Dokumente vorkommen, um behalten zu werden
                         docfreq_type = "prop") # min und max als Prozentsätze der Gesamtzahl interpretieren

# Dokumente & Wörter zählen 
dim(dfm_filtered_trimmed)
# xx Seiten & xx Wörter

# Wegen des Trimmens haben wir jetzt leere Zeilen in unserer dfm
# leere Zeilen entfernen
sel_idx <- slam::row_sums(dfm_filtered_trimmed) > 0 # Summe der Merkmale pro Dokument berechnen, muss größer als Null sein
dfm_filtered_trimmed <- dfm_filtered_trimmed[sel_idx, ] # nur die Dokumente mit min. 1 Merkmal behalten
data <- data[sel_idx, ] # nur die Metadaten behalten von den Dokumenten mit min. 1 Merkmal


# Matrix speichern
saveRDS(dfm_filtered_trimmed, file = "data/dfm_preprocessed.rds")


# in lesbaren Datensatz umwandeln
dfm_matrix <- convert(dfm_filtered_trimmed,to="data.frame")
write_csv2(dfm_matrix, "data/dfm/dfm_matrix.csv")


