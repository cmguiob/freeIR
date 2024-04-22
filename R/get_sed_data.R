#' @title Get sepectral data from .sed file
#'
#' @param path The path where the .sed file is located
#'
#' @return A data frame with column names equal to wavenumbers and one row with reflectance values
#' @export
#'
#' @examples

get_sed_data <- function(path){

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

  data <- file[[1]] |>
          #split string by "\t" delimiter into two columns
          tidyr::separate_wider_delim(cols = info,
                                delim = "\t",
                                names = c("wvl", "reflect")) |>
          #transform to numeric
          dplyr::mutate_if(is.character, as.numeric) |>
          #turn to wide data frame
          tidyr::pivot_wider(names_from = wvl, values_from = reflect )

  return(data)

}
