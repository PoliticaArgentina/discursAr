#' Descarga discursos
#'  (\emph{Download speeches})
#'
#' @description
#' Función que descarga los discursos presidenciales ante la Asamblea Legislativa desde 1854 hasta 2020
#'  (\emph{Function that downloads presidential speeches to de National Legislative Assembly from 1854 to 2020.})
#'
#' @param year integer con identificador de discurso que se quiere seleccionar. Se puede explorar un listado de discursos con \code{\link{show_available_speech}}
#' (\emph{integer id for a selected speech. Explore full list of speeches  with \code{\link{show_available_speech}}}).
#'
#' @param raw boleano que permite descargar discurso en formato \emph{tidy} cuando \code{raw = FALSE}  o crudo caso contrario
#' (\emph{boolean that sets if you want to download raw or \emph{tidy} formated speech data}).
#'
#'
#' @return Devuelve un tibble con clases \code{"spec_tbl_df" "tbl_df" "tbl" "data.frame"} con el contenido de un discurso presidencial en tres variables:
#' \code{discurso, presidente, year}.
#'  (\emph{it retruns a tibble with three variables (speech - \code{discurso} - president -\code{presidente} and \code{year}).
#' The object is of \code{class "tbl_df","tbl","data.frame"}}).
#'
#' @seealso  \code{\link{show_available_speech}}
#'
#' @examples
#'
#'   get_speech(year = 1949)
#'
#'
#' @export

  get_speech <- function(year = NULL,  raw = FALSE){



  ## Check for internet coection
  attempt::stop_if_not(.x = curl::has_internet(),
                       msg = "Internet access was not detected. Please check your connection //
No se detecto acceso a internet. Por favor chequear la conexion.")

  ## Check params
  # raw check


  assertthat::assert_that(is.logical(raw),
                          msg = "'raw' must be logical. Options = c(TRUE, FALSE) //
'raw' debe ser un boleano. Opciones = c(TRUE, FALSE)" )


  # Check available speeches


  check_speech <-   discursAr::show_available_speech()


  assertthat::assert_that(year %in% check_speech$year,
                          msg = "Please choose a valid speech year. Check them with 'show_available_speech()' //
Por favor seleccione una discurso valido. Consultelos con 'show_available_speech()'")

  # get RAW or Tidy (one token per row)


  if(raw == FALSE){ #Tidy version of speech

             speech_year <- year

             get_ids <- check_speech %>% # We build this because we need not only year but also speaker names
               dplyr::filter(year == speech_year)


             url_tidy <- glue::glue("https://raw.githubusercontent.com/PoliticaArgentina/data_warehouse/master/discursAr/{get_ids$year}-{get_ids$president}.csv?raw=true")

             ## FAIL SAFELEY


             # Set default value for try()

             default <- NULL

             df <- base::suppressWarnings(base::try(default <-  vroom::vroom(url_tidy,
                                                                             col_types = vroom::cols() ) %>%
                                                      janitor::clean_names(),
                                                    silent = TRUE))

               if(is.null(default)){

                 df <- base::message("Fail to download data. Source is not available // La fuente de datos no esta disponible")


                 } else{

                 return(df)

                   }

    } else {  # RAW version of speech

            url_raw <- "https://raw.githubusercontent.com/PoliticaArgentina/data_warehouse/master/discursAr/raw_opening_speeches.csv"



                    ## FAIL SAFELEY

                  # Set default value for try()

                  default <- NULL

                  df <- base::suppressWarnings(base::try(default <-  vroom::vroom(url_raw,
                                                                                  col_types = vroom::cols()) %>%
                                                           janitor::clean_names() %>%
                                                           dplyr::filter(year == anio),
                                                         silent = TRUE))

                  if(is.null(default)){

                    df <- base::message("Fail to download data. Source is not available // La fuente de datos no esta disponible")


                  } else{

                    return(df)

                  }




      } # Closes raw / tidy if-else


  } # Close function


