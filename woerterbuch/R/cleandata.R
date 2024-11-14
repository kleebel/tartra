
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
  # Ergebnisse in Datenrahmen-Format bringen und mögliche Duplikate behandeln
  findings_df <- rrapply(relevant_documents, how = "melt")
  findings_df$group <- cumsum(findings_df$L1 == "doc_id")

  # Dynamische Spaltenbenennung und Auswahl
  findings_df <- findings_df %>%
    select(L1, L2, value, group) %>%
    mutate(L1 = ifelse(L2 == "document", L1, paste0(L1, "_", L2))) %>%
    select(-L2)

  # Umwandlung in breites Format
  # Verwenden von values_fn, um Listenspalten zu vermeiden und Mehrfachwerte zu aggregieren
  results <- findings_df %>%
    pivot_wider(names_from = "L1", values_from = "value", values_fn = list(value = list)) %>%
    mutate(across(starts_with("found_words"), ~ sapply(.x, function(x) paste0(unlist(x), collapse = ", "))))

  # Dokument- und Wort-Spalten zusammenfassen
  if ("document" %in% names(results)) {
    results <- results %>%
      unnest(cols = c(document, doc_id)) %>%
      group_by(doc_id, document) %>%
      summarize(across(starts_with("found_words"), ~ paste0(.x, collapse = ", "))) %>%
      ungroup()
  } else {
    results <- results %>%
      unnest(cols = doc_id) %>%
      group_by(doc_id) %>%
      summarize(across(starts_with("found_words"), ~ paste0(.x, collapse = ", "))) %>%
      ungroup()
  }

  return(results)
}
