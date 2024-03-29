% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_speech.R
\name{get_speech}
\alias{get_speech}
\title{Descarga discursos
(\emph{Download speeches})}
\usage{
get_speech(year = NULL, raw = FALSE)
}
\arguments{
\item{year}{integer con identificador de discurso que se quiere seleccionar. Se puede explorar un listado de discursos con \code{\link{show_available_speech}}
(\emph{integer id for a selected speech. Explore full list of speeches  with \code{\link{show_available_speech}}}).}

\item{raw}{boleano que permite descargar discurso en formato \emph{tidy} cuando \code{raw = FALSE}  o crudo caso contrario
(\emph{boolean that sets if you want to download raw or \emph{tidy} formated speech data}).}
}
\value{
Devuelve un tibble con clases \code{"spec_tbl_df" "tbl_df" "tbl" "data.frame"} con el contenido de un discurso presidencial en tres variables:
\code{discurso, presidente, year}.
(\emph{it retruns a tibble with three variables (speech - \code{discurso} - president -\code{presidente} and \code{year}).
The object is of \code{class "tbl_df","tbl","data.frame"}}).
}
\description{
Función que descarga los discursos presidenciales ante la Asamblea Legislativa desde 1854 hasta 2020
(\emph{Function that downloads presidential speeches to de National Legislative Assembly from 1854 to 2020.})
}
\examples{

  get_speech(year = 1949)


}
\seealso{
\code{\link{show_available_speech}}
}
