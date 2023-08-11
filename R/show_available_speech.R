#' Diccionario de discursos disponibles
#'  (\emph{Speeches collection})
#'
#' @description
#' Función que devuelve un listado de discursos de apertura de sesiones emitidos por los presidentes de Argentina ante la Asamblea Legislativa.
#'
#' @param viewer Por defecto es \code{FALSE}. Cuando \code{TRUE} devuelve una tabla en el \emph{Viewer} de \emph{RStudio}
#'  (\emph{The default is \code{FALSE}. When \code{TRUE} it returns a table in \emph{RStudio Viewer}}).
#'
#' @return El objeto de salida es un data set con los id dediscursos disponibles para usar como parámetro con
#'  con \code{\link{get_speech}}. Cuando el parámetro es \code{viewer = FALSE}, devuelve un tibble con \code{class "tbl_df","tbl","data.frame"}, y
#'  cuando es \code{viewer = TRUE} devuelve un objeto con \code{class "datatables","htmlwidget"}
#'  (\emph{The output is a data set with speeches id needed as parameters in \code{\link{get_speech}}.
#'  When parameter is set to \code{viewer = FALSE} it returns a tibble and when it is \code{viewer = TRUE} it returns an
#'  object of \code{class "datatables","htmlwidget"}}).
#'
#' @examples
#'
#'  show_available_speech()
#'
#'
#' @export


show_available_speech <- function(viewer = FALSE){


  # Check for internet coection
  attempt::stop_if_not(.x = curl::has_internet(),  # from eph package
                       msg = "No se detecto acceso a internet. Por favor checkea tu conexion.")



gh_url <- 'https://api.github.com/repos/PoliticaArgentina/data_warehouse/git/trees/master?recursive=1'

response <- httr::GET(url = gh_url)




check_status <- function(res){
  attempt::stop_if_not(.x = httr::status_code(res),
                       .p = ~ .x == 200,
                       msg = httr::message_for_status(res, "get data from  Github API"))
}





check_status(response)



# Get list of files from github data repo

# GET DATA

gh_content <- jsonlite::fromJSON(httr::content(response, 'text'))


# Wrangle data

filelist <- tibble::as_tibble(purrr::pluck(.x = gh_content, 'tree')) %>%
  dplyr::select(path)  %>%
  dplyr::filter(stringr::str_detect(path, "discursAr/"),
                stringr::str_detect(path, ".csv")) %>%
  dplyr::mutate(name = stringr::str_remove(path, pattern = "discursAr/"),
                year = stringr::str_sub(name, start = 1, end = 4),
                name = stringr::str_replace_all(string= name, pattern = "-", replacement = "")) %>%
  dplyr::mutate(name = stringr::str_remove_all(name, "\\d")) %>%
  dplyr::mutate(name = stringr::str_remove_all(name, ".csv")) %>%
  dplyr::rename(president = name) %>%
  dplyr::select(- path)





if(viewer == TRUE){

  x <-  filelist %>%
    dplyr::mutate(president = stringr::str_replace_all(string = president, pattern = "_", replacement = " ")) %>%
    dplyr::rename(id = year,
           Presidente = president) %>%
    dplyr::mutate(Presidente = stringr::str_to_title(Presidente)) %>%
    DT::datatable(options = list(
      language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')))


  print(x)


} else {


  return(filelist)

  }
}
