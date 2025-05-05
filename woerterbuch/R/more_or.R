
#' Search multiple OR-Groups Across Multiple Documents with Context (Parallel)
#'
#' Diese Funktion durchsucht mehrere Dokumente nach Begriffen aus mehreren OR-Gruppen und berücksichtigt dabei den Kontext (eine Seite danach),
#' sofern nur eine OR-Gruppe auf der aktuellen Seite zutrifft. Seiten müssen eine Spalte \code{page} und \code{realfilename} enthalten.
#'
#' @param dataset Ein DataFrame mit den Seiteninformationen. Muss mindestens \code{text}, \code{page} und \code{realfilename} enthalten.
#' @param conditions Eine Liste mit Suchbedingungen nach dem Muster \code{list(condition1 = list(AND = list(OR1 = ..., OR2 = ...)))}.
#'
#' @return Ein DataFrame mit Treffern, Seitenangaben und gefundenen Begriffen je OR-Gruppe.
#' @export
#'
#' @import dplyr
#' @import stringr
#' @importFrom tibble tibble
moreor_apply_contextual_search <- function(dataset, conditions) {
  # Hilfsfunktion zum Pattern-Match
  match_or_group <- function(patterns, text) {
    matches <- sapply(patterns, function(p) grepl(p, text, ignore.case = TRUE, perl = TRUE))
    names(matches) <- patterns
    return(matches)
  }

  or1_patterns <- conditions$condition1$AND$OR1
  or2_patterns <- conditions$condition1$AND$OR2

  results <- list()
  i <- 1

  while (i <= nrow(dataset)) {
    page1 <- dataset[i, ]
    text1 <- page1$text

    or1_match <- match_or_group(or1_patterns, text1)
    or2_match <- match_or_group(or2_patterns, text1)

    or1_hit <- any(or1_match)
    or2_hit <- any(or2_match)

    if (or1_hit && or2_hit) {
      results[[length(results) + 1]] <- tibble(
        realfilename = page1$realfilename,
        pages = as.character(page1$page),
        found_words_OR1 = paste(names(or1_match)[or1_match], collapse = ", "),
        found_words_OR2 = paste(names(or2_match)[or2_match], collapse = ", ")
      )
      i <- i + 1
      next
    }

    if ((or1_hit || or2_hit) && i < nrow(dataset)) {
      page2 <- dataset[i + 1, ]
      if (page1$realfilename == page2$realfilename) {
        combined_text <- paste(text1, page2$text, sep = " ")
        or1_combined <- match_or_group(or1_patterns, combined_text)
        or2_combined <- match_or_group(or2_patterns, combined_text)

        if (any(or1_combined) && any(or2_combined)) {
          results[[length(results) + 1]] <- tibble(
            realfilename = page1$realfilename,
            pages = paste(page1$page, page2$page, sep = ","),
            found_words_OR1 = paste(names(or1_combined)[or1_combined], collapse = ", "),
            found_words_OR2 = paste(names(or2_combined)[or2_combined], collapse = ", ")
          )
          i <- i + 2
          next
        }
      }
    }

    i <- i + 1
  }

  if (length(results) > 0) {
    return(bind_rows(results))
  } else {
    return(tibble(
      realfilename = character(),
      pages = character(),
      found_words_OR1 = character(),
      found_words_OR2 = character()
    ))
  }
}


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
moreor_apply_search <- function(corpus, conditions) {
  # Erstelle einen Parallel-Cluster
  cl <- makeCluster(detectCores())
  registerDoParallel(cl)

  # Parallele Verarbeitung der Dokumente
  relevant_documents <- foreach(
    doc_id = names(corpus), .combine = "c", .packages = c("stringr", "foreach", "doParallel")
  ) %dopar% {
    doc <- corpus[[doc_id]]
    found_words <- moreor_term_search(doc, conditions)

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
    mutate(L1 = ifelse(is.na(L2), L1, paste0(L1, "_", L2))) %>%
    select(-L2)

  # 4. Umwandlung in breites Format und Entpacken von Listen
  result <- df %>%
    pivot_wider(names_from = "L1", values_from = "value") %>%
    mutate(across(everything(), ~ if (is.list(.x)) { map(.x, ~ paste0(.x, collapse = ", ")) } else { .x })) %>%
    unnest(cols = c(starts_with("found_words_"), document, doc_id)) %>%
    select(-group)

  # Rueckgabe des formatierten DataFrames
  return(result)
}


