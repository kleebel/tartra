
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
  # Ergebnisse in Datenrahmen-Format bringen
  findings_df <- rrapply(relevant_documents, how = "melt")

  # Gruppen für mehrfach Treffer pro Dokument identifizieren
  findings_df$group <- cumsum(findings_df$L1 == "doc_id")

  # Dynamische Zuordnung der Spaltennamen
  findings_df <- findings_df %>%
    select(L1, L2, value, group) %>%
    mutate(L1 = ifelse(L2 == "document", L1, paste0(L1, "_", L2))) %>%
    select(-L2)

  # Umwandlung in breites Format und Entpacken von Listen
  results <- findings_df %>%
    pivot_wider(names_from = "L1", values_from = "value") %>%
    unnest(cols = c(any_of(names(findings_df)[grepl("^found_words_", names(findings_df))]), "document", "doc_id"))

  # Zusammenfassen der Wörter pro Dokument (dynamisch alle gefundenen Gruppen zusammenführen)
  results <- results %>%
    group_by(doc_id, document) %>%
    summarize(across(starts_with("found_words"), ~ paste0(.x, collapse = ", "))) %>%
    ungroup()

  return(results)
}
