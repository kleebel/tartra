#' Search one OR-Term with Optional Context Across Multiple Documents (Parallel)
#'
#' Diese Funktion durchsucht mehrere Dokumente parallel nach Begriffen gemaess einer einzelnen OR-Bedingung
#' und bezieht optional den Kontext (eine Seite davor und danach) ein, sofern vorhanden.
#'
#' @import foreach
#' @import doParallel
#' @param dataset Ein DataFrame mit den Seiteninformationen (inkl. text, page, realfilename).
#' @param conditions Eine Bedingung (Liste) mit einer einzigen OR-Gruppe.
#' @param with_context TRUE, wenn Kontextseiten (vorher/nachher) berücksichtigt werden sollen.
#' @return Ein DataFrame mit den relevanten Seiten, realfilename und einer Listenspalte gefundener Begriffe.
#' @export
oneor_apply_contextual_search <- function(dataset, conditions, with_context = TRUE) {
  cl <- makeCluster(detectCores())
  registerDoParallel(cl)

  relevant_pages <- foreach(
    page_id = seq_len(nrow(dataset)), .combine = "rbind", .packages = c("stringr", "dplyr")
  ) %dopar% {
    current_page <- dataset[page_id, ]
    current_filename <- current_page$realfilename

    # Wähle zu durchsuchenden Text abhängig vom Kontext-Parameter
    if (with_context) {
      prev_page <- if (page_id > 1 && dataset[page_id - 1, "realfilename"] == current_filename) dataset[page_id - 1, ] else NULL
      next_page <- if (page_id < nrow(dataset) && dataset[page_id + 1, "realfilename"] == current_filename) dataset[page_id + 1, ] else NULL

      search_text <- paste(
        if (!is.null(prev_page)) prev_page$text else "",
        current_page$text,
        if (!is.null(next_page)) next_page$text else "",
        sep = " "
      )
    } else {
      search_text <- current_page$text
    }

    found_words <- oneor_term_search(search_text, conditions)

    if (length(found_words) > 0) {
      return(data.frame(
        page = current_page$page,
        realfilename = current_filename,
        found_words = I(list(found_words))
      ))
    } else {
      return(NULL)
    }
  }

  stopCluster(cl)
  relevant_pages <- relevant_pages[!sapply(relevant_pages, is.null), ]

  return(relevant_pages)
}


#' Search one OR-Term Across Multiple Documents (Parallel)
#'
#' Diese Funktion durchsucht mehrere Dokumente parallel nach Begriffen gemaess einer einzelnen OR-Bedingung.
#'
#' @import foreach
#' @import doParallel
#' @param corpus Ein Korpus von Dokumenten (Liste von Zeichenketten).
#' @param conditions Eine Bedingung (Liste) mit einer einzigen OR-Gruppe.
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
  if (!is.list(conditions) || !"OR" %in% names(conditions)) {
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
  # 1. Extrahiere Daten und füge Gruppen-Identifikation hinzu
  findings <- rrapply(relevant_documents, how = "melt") %>%
    mutate(group = cumsum(L1 == "doc_id"))

  # 2. Umwandlung in breites Format und Entpacken von Listen
  result <- findings %>%
    pivot_wider(names_from = "L1", values_from = "value", values_fn = list) %>%
    unnest(cols = found_words) %>%
    unnest(cols = found_words)

  # 3. Zusammenfassen der Wörter pro Dokument und Entpacken von Listen
  result <- result %>%
    group_by(doc_id, document) %>%
    summarize(found_words = paste(found_words, collapse = ", "), .groups = "drop") %>%
    unnest(cols = c(doc_id, document)) %>%
    unnest(cols = c(doc_id, document))

  return(result)
}
