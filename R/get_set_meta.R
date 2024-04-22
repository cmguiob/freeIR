#' Get sepectral data from .sed file
#'
#' @param path The path where the .sed file is located
#'
#' @return A data frame with column names equal to wavenumbers and one row with reflectance values
#' @export
#'
#' @examples

get_sed_meta <- function(path){

  file <- readr::read_delim(file = "./data/230804_Blanco referencia.sed", delim = "\n") |>
    #renames first row as "info"
    dplyr::rename(info = 1) |>
    #creates new variable
    dplyr::mutate(
      #fills the variable conditionally
      tipo = dplyr::case_when(
        #when data in "info" is text then fill with "Metadato", otherwise with "Dato"
        stringr::str_detect(info, pattern = "^[A-Z]") ~ "Metadato", TRUE ~ "Dato")) |>
    #groups data frame by categories in "tipo"
    dplyr::group_by(tipo, .add = TRUE) |>
    #splits data frame into nested dataframes by group
    dplyr::group_split(tipo, .keep = FALSE)

  metadata <- file[[2]] |>
    dplyr::filter(dplyr::row_number() <= dplyr::n()-1) |>
    #splits strin by ":" into two columns
    tidyr::separate_wider_delim(cols = info,
                                delim = ": ",
                                names = c("Metadata", "Info"),
                                too_few = "align_start") |>
    #remove rows that have empty values in the Info column
    dplyr::filter(!is.na(Info))

  return(metadata)

}
