
#' Process Findings and Transform into Dataset
#'
#' Diese Funktion verarbeitet die Suchergebnisse (relevant_documents) und formatiert sie in einen
#' Datensatz, der Mehrfach-Treffer pro Dokument berücksichtigt und in ein breites Format bringt.
#'
#' @import dplyr
#' @import rrapply
#' @import tidyr
#' @param relevant_documents Eine Liste von relevanten Dokumenten (wie von apply_term_search zurückgegeben).
#' @return Ein DataFrame mit den formatierten Ergebnissen.
#' @export
process_findings <- function(relevant_documents) {

  # Die Ergebnisse in ein "melted" Format bringen
  findings <- rrapply::rrapply(relevant_documents, how = "melt")

  # Gruppe für mehrfach Treffer pro Dokument identifizieren
  findings$group <- cumsum(findings$L1 == "doc_id")

  # Auswahl der relevanten Spalten und dynamische Benennung der OR-Gruppen
  findings <- findings %>%
    select(L1, L2, value, group) %>%
    mutate(L1 = case_when(
      str_detect(L2, "OR") ~ paste0(L1, "_", L2), # Dynamisches Suffix basierend auf OR-Gruppe
      TRUE ~ L1
    )) %>%
    select(-L2)

  # Umwandlung in breites Format
  result <- findings %>%
    pivot_wider(names_from = "L1", values_from = "value") %>%
    unnest(cols = where(is.list), names_repair = "unique") # Entpacke alle Listen

  # Zusammenfassen der Treffer pro Dokument und OR-Gruppe
  result <- result %>%
    group_by(doc_id, document) %>%
    summarize(across(starts_with("found_words_"), ~ paste(na.omit(.), collapse = ", ")), .groups = "drop") %>%
    ungroup()

  return(result)
}
