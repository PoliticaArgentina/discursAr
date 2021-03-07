#' \code{discursAr} package
#'
#' Caja de Herramientas para el procesamiento de discursos presidenciales de Argentina
#' See the README on
#' \href{https://github.com/electorArg/discrusAr/blob/master/README.md}{Github}
#'
#' @docType package
#' @name discurAr
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines

if(getRversion() >= "2.15.1")  utils::globalVariables(c("Presidente",
                                                        "V1",
                                                        "anio",
                                                        "n",
                                                        "name",
                                                        "president",
                                                        "word",
                                                        "year"))
