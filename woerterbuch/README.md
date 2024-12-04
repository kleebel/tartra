
## woerterbuch Package

Dieses Package ermöglicht, große Textkorpora nach bestimmten Begriffen in UND/ODER Zusammenhängen effizient zu durchsuchen. Es nutzt parallele Verarbeitung, um die Leistung bei großen Datensätzen zu verbessern. Wir verwenden es, um Tarifvertragsdokumente nach Transformationsregelungen zu durchsuchen.


### Installation
Wie bei anderen Packages, musst du das R-Package zunächst installieren und dann laden.

```
library(devtools)
install_github("kleebel/tartra/woerterbuch", upgrade = "never")
library(woerterbuch)
```

### Arbeitsweise

1. Daten & Korpus laden: deine Daten müssen als Liste von Dokumenten vorliegen. Lade deinen bereits existierenden Korpus, erstelle ihn oder wandle Texte aus einem Dataframe um.
2. Bedingungen definieren: Definieren die Begriffe, nach denen gesucht werden soll, in einer Liste mit dem Schlüssel OR.
3. Suche ausführen: Verwende `oneor_apply_search` oder `moreor_apply_search`, um die Dokumente parallel zu durchsuchen.
4. Ergebnisse verarbeiten: Nutze `oneor_process_findings` oder `moreor_process_findings`, um die Ergebnisse in einem DataFrame für die Weiterverarbeitung oder Analyse zu formatieren.


### Beispielhafter Workflow

```
# weitere Packages laden
library(tidyverse)
library(quanteda)
library(rrapply)
library(foreach)
library(doParallel)

# Beispiel-Korpus erstellen
corpus <- list(
  doc1 = "Die Qualifizierung in der Digitalisierung ist wichtig.",
  doc2 = "Die Schulung zur Automatisierung ist der nächste Schritt.",
  doc3 = "Eine gute Weiterbildung und Fortbildung sind wichtig für die Karriere."
)

# Suchbedingungen definieren
more_conditions <- list(
  condition1 = list(
    AND = list(
      OR1 = c("qualifizierung",
              "schulung",
              "weiterbildung"),
      OR2 = c("digitalisierung",
              "automatisierung")
    )
  )
)
# Suchkonstrukt besteht aus mehreren Oder-Gruppen (hier Qualifizierung und Digitalisierung). Daher werden im folgenden die Funktionen mit dem Präfix "moreor_" verwendet

# Suche ausführen
found_documents_moreor <- moreor_apply_search(corpus, more_conditions)

# Ergebnisse transformieren
results_moreor <- moreor_process_findings(found_documents_moreor)

```

### Funktionen im Detail

#### `oneor_apply_search(corpus, conditions)`

Durchsucht einen Korpus basierend auf einer einzelnen OR-Bedingung.

**Parameter:**
- `corpus`: Liste von Zeichenketten, die die Dokumente repräsentieren.
- `conditions`: Eine Liste mit einer einzigen OR-Gruppe.

**Rückgabewert:**
- Liste der relevanten Dokumente und der gefundenen Begriffe.

---

#### `oneor_process_findings(relevant_documents)`

Verarbeitet die Ergebnisse einer OR-Suche und gibt diese als DataFrame aus.

**Parameter:**
- `relevant_documents`: Ausgabe von `oneor_apply_search`.

**Rückgabewert:**
- DataFrame mit Treffern und zugehörigen Dokumenten.

---

#### `moreor_apply_search(corpus, conditions)`

Durchsucht einen Korpus basierend auf mehreren AND-OR-Bedingungen.

**Parameter:**
- `corpus`: Liste von Zeichenketten, die die Dokumente repräsentieren.
- `conditions`: Liste von Bedingungen mit AND-OR-Kombinationen.

**Rückgabewert:**
- Liste der relevanten Dokumente und der gefundenen Begriffe.

---

#### `moreor_process_findings(relevant_documents)`

Verarbeitet die Ergebnisse einer AND-OR-Suche und gibt diese als DataFrame aus.

**Parameter:**
- `relevant_documents`: Ausgabe von `moreor_apply_search`.

**Rückgabewert:**
- DataFrame mit strukturierten Treffern und zugehörigen Dokumenten.
