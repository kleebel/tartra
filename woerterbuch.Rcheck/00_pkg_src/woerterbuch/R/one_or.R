
#' Search one OR-Term Across Multiple Documents (Parallel)
#'
#' Diese Funktion durchsucht mehrere Dokumente parallel nach Begriffen gemaess einer einzelnen OR-Bedingung.
#'
#' @import foreach
#' @import doParallel
#' @param corpus Ein Korpus von Dokumenten (Liste von Zeichenketten).
#' @param condition Eine Bedingung (Liste) mit einer einzigen OR-Gruppe.
#' @return Eine Liste der relevanten Dokumente mit den gefundenen Begriffen.
#' @export
oneor_apply_search <- function(corpus, conditions) {
  # Erstelle einen Parallel-Cluster
  cl <- makeCluster(detectCores())
  registerDoParallel(cl)

  # Parallele Verarbeitung der Dokumente
  relevant_documents <- foreach(
    doc_id = names(corpus), .combine = "c", .packages = c("stringr", "foreach", "doParallel")
  ) %dopar% {
    doc <- corpus[[doc_id]]
    found_words <- oneor_term_search(doc, conditions)

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


#' helper function for oneor_apply_search()
#' @rdname oneor_apply_search
#' @import stringr
#' @param doc Ein Dokument (Zeichenkette), das durchsucht werden soll.
#' @param conditions Eine Bedingung (Liste) mit einer einzigen OR-Gruppe.
#' @return Eine Liste der gefundenen Woerter, die die Bedingung erfuellen.
#' @export
oneor_term_search <- function(doc, conditions) {
  if (!is.list(condition) || !"OR" %in% names(conditions)) {
    stop("Condition muss eine Liste mit einem OR-Schluessel sein.")
  }

  # Ueberpruefe die OR-Bedingung
  or_terms <- conditions$OR
  or_matches <- sapply(or_terms, function(word) stringr::str_detect(doc, word))

  # Sammle gefundene Begriffe
  if (any(or_matches)) {
    return(or_terms[or_matches])
  } else {
    return(list())
  }
}


#' Process Findings of a single OR-Group
#'
#' Diese Funktion verarbeitet die Ergebnisse der Suche mit einer OR-Gruppe und formatiert sie in einen
#' Datensatz, der Mehrfach-Treffer pro Dokument beruecksichtigt und in ein breites Format bringt.
#'
#' @import dplyr
#' @import tidyr
#' @param relevant_documents Eine Liste von relevanten Dokumenten (wie von oneor_apply_search ausgegeben).
#' @return Ein DataFrame mit den formatierten Ergebnissen.
#' @export
oneor_process_findings <- function(relevant_documents) {

  # Ueberpruefe, ob relevante Dokumente vorhanden sind
  if (length(relevant_documents) == 0) {
    return(data.frame(doc_id = character(0), document = character(0), found_words = character(0)))
  }

  # Extrahiere die Daten in eine flache Struktur
  results <- do.call(rbind, lapply(relevant_documents, function(doc) {
    data.frame(
      doc_id = doc$doc_id,
      document = doc$document,
      found_words = paste(doc$found_words, collapse = ", ")
    )
  }))

  # Rueckgabe als DataFrame
  return(results)
}
