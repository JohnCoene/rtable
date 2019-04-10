#' Convert
#'
#' Convert results to a `tibble`.
#'
#' @param records Records obtained from other functions of the package.
#' @param convert_date Set to `TRUE` to convert dates to `POSIX`.
#'
#' @return A `tibble` containing the fields of the table together with
#'  the `record_id` and `record_created_time` columns.
#' 
#' @export
records_to_tibble <- function(records, convert_date = TRUE){
  fields <- records %>% 
    purrr::map("fields") %>% 
    purrr::map_dfr(dplyr::as_tibble)

  record_id <- records %>% 
    purrr::map_chr("id")
  
  record_created_time <- records %>% 
    purrr::map_chr("createdTime") 

  if(convert_date)
    record_created_time <- as.POSIXct(record_created_time, "%Y-%d-%mT%H:%M:%OSZ", tz = Sys.timezone())

  dplyr::tibble(
    record_id = record_id,
    record_created_time = record_created_time
  ) %>% 
    dplyr::bind_cols(fields)
} 