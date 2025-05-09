% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/more_or.R
\name{moreor_process_findings}
\alias{moreor_process_findings}
\title{Process Findings of multiple OR-Groups}
\usage{
moreor_process_findings(relevant_documents)
}
\arguments{
\item{relevant_documents}{Eine Liste von relevanten Dokumenten (wie von apply_term_search zurueckgegeben).}
}
\value{
Ein DataFrame mit den formatierten Ergebnissen.
}
\description{
Diese Funktion verarbeitet die Ergebnisse der Suche mit mehreren OR-Gruppen und formatiert sie in einen
Datensatz, der Mehrfach-Treffer pro Dokument beruecksichtigt und in ein breites Format bringt.
}
