
#' Process Findings and Transform into Dataset
#'
#' Diese Funktion verarbeitet die relevanten Dokumente und formatiert die Ergebnisse in einem
#' Datensatz, der mehrfach Treffer pro Dokument berücksichtigt und in ein breites Format bringt.
#'
#' @import dplyr
#' @import rrapply
#' @import tidyr
#' @param relevant_documents Eine Liste von relevanten Dokumenten (wie von apply_term_search zurückgegeben).
#' @return Ein DataFrame mit den formatierten Ergebnissen.
#' @export
process_findings <- function(relevant_documents) {

  # 1. Melt die relevanten Dokumente, um eine flache Struktur zu erzeugen
  findings <- rrapply(relevant_documents, how = "melt")

  # 2. Identifiziere Gruppen für mehrfach Treffer pro Dokument (z.B. mehrere relevante Begriffe pro Dokument)
  findings$group <- cumsum(findings$L1 == "doc_id")

  # 3. Auswahl der relevanten Spalten und Hinzufügen von Suffixen zur Unterscheidung der Suchgruppen
  df <- findings %>%
    select(L1, L2, value, group) %>%
    mutate(L1 = case_when(
      L2 == "OR1" ~ paste0(L1, "_OR1"),  # Qualifizierung-Begriffe
      L2 == "OR2" ~ paste0(L1, "_OR2"),   # Digitalisierung-Begriffe
      TRUE ~ L1
    )) %>%
    select(-L2)

  # 4. Umwandlung in breites Format und Entpacken von Listen
  result <- df %>%
    pivot_wider(names_from = "L1", values_from = "value") %>%
    unnest(cols = c(found_words_OR1, found_words_OR2, document, doc_id))

  # 5. Zusammenfassen der Wörter pro Seite
  result <- result %>%
    group_by(doc_id, document) %>%
    summarize(
      found_words_OR1 = paste0(found_words_OR1, collapse = ", "),
      found_words_OR2 = paste0(found_words_OR2, collapse = ", ")
    ) %>%
    ungroup()  # Entferne die Gruppierung

  # Rückgabe des formatierten DataFrames
  return(result)
}
