
#' Search multiple OR-Groups Across Multiple Documents (Parallel)
#'
#' Diese Funktion durchsucht mehrere Dokumente parallel nach Begriffen gemaess den Bedingungen.
#'
#' @import foreach
#' @import doParallel
#' @param corpus Ein Korpus von Dokumenten (Liste von Zeichenketten).
#' @param conditions Eine Liste von Bedingungen.
#' @return Eine Liste der relevanten Dokumente mit den gefundenen Begriffen.
#' @export
moreor_apply_search <- function(corpus) {
  # Erstelle einen Parallel-Cluster
  cl <- makeCluster(detectCores())
  registerDoParallel(cl)

  # Parallele Verarbeitung der Dokumente
  relevant_documents <- foreach(
    doc_id = names(corpus), .combine = "c", .packages = c("stringr", "foreach", "doParallel")
  ) %dopar% {
    doc <- corpus[[doc_id]]
    found_words <- find_terms(doc, oder_und_oder)

    # Wenn relevante Begriffe gefunden wurden, speichere sie
    if (length(found_words) > 0) {
      return(list(doc_id = doc_id, document = doc, found_words = found_words))
    } else {
      return(NULL)
    }
  }

  # Stoppe den Parallel-Cluster
  stopCluster(cl)

  # Entferne NULL-Werte (Dokumente ohne gefundene Begriffe)
  relevant_documents <- relevant_documents[!sapply(relevant_documents, is.null)]

  return(relevant_documents)
}


#' helper function for moreor_apply_search()
#' @rdname moreor_apply_search
#' @import stringr
#' @import dplyr
#' @param doc Ein Dokument (Zeichenkette), das durchsucht werden soll.
#' @param conditions Eine Liste von Bedingungen mit AND-OR-Kombinationen.
#' @return Eine Liste der gefundenen Woerter, die die Bedingungen erfuellen.
#' @export
moreor_term_search <- function(doc, conditions) {
  found_words <- list()

  # Iteriere durch alle Bedingungen
  for (condition in conditions) {
    and_match <- TRUE
    matched_words <- list()

    # Ueberpruefe jede OR-Gruppe innerhalb der AND-Bedingung
    for (or_group_name in names(condition$AND)) {
      or_terms <- condition$AND[[or_group_name]]
      or_matches <- sapply(or_terms, function(word) stringr::str_detect(doc, word))

      if (any(or_matches)) {
        matched_words[[or_group_name]] <- or_terms[or_matches]
      } else {
        and_match <- FALSE
        break
      }
    }

    # Wenn alle OR-Gruppen erfuellt sind, fuege die gefundenen Begriffe hinzu
    if (and_match) {
      found_words <- c(found_words, matched_words)
    }
  }

  return(found_words)
}


#' Process Findings of multiple OR-Groups
#'
#' Diese Funktion verarbeitet die Ergebnisse der Suche mit mehreren OR-Gruppen und formatiert sie in einen
#' Datensatz, der Mehrfach-Treffer pro Dokument beruecksichtigt und in ein breites Format bringt.
#'
#' @import dplyr
#' @import rrapply
#' @import tidyr
#' @param relevant_documents Eine Liste von relevanten Dokumenten (wie von apply_term_search zurueckgegeben).
#' @return Ein DataFrame mit den formatierten Ergebnissen.
#' @export
moreor_process_findings <- function(relevant_documents) {

  # 1. Melt die relevanten Dokumente, um eine flache Struktur zu erzeugen
  findings <- rrapply(relevant_documents, how = "melt")

  # 2. Identifiziere Gruppen für mehrfach Treffer pro Dokument (z.B. mehrere relevante Begriffe pro Dokument)
  findings$group <- cumsum(findings$L1 == "doc_id")

  # 3. Auswahl der relevanten Spalten und Hinzufügen von Suffixen zur Unterscheidung der Suchgruppen
  df <- findings %>%
    select(L1, L2, value, group) %>%
    mutate(L1 = paste0(L1, "_", L2)) %>%
    select(-L2)

  # 4. Umwandlung in breites Format und Entpacken von Listen
  result <- df %>%
    pivot_wider(names_from = "L1", values_from = "value") %>%
    unnest(cols = c(starts_with("found_words_"), document, doc_id))

  # 5. Zusammenfassen der Woerter pro Dokument
  result <- result %>%
    group_by(doc_id, document) %>%
    summarize(across(starts_with("found_words_"), ~ paste0(.x, collapse = ", "))) %>%
    ungroup()

  # Rueckgabe des formatierten DataFrames
  return(result)
}


