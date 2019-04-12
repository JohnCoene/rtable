#' Convert
#'
#' Convert results to a \code{tibble}.
#'
#' @param records Records obtained from other functions of the package.
#' @param convert_date Set to \code{TRUE} to convert record creation time to \code{POSIXct}.
#' @param tz Timzone, used if \code{convert_date} is set to \code{TRUE}.
#'
#' @return A \code{tibble} containing the fields of the table together with
#'  the \code{record_id} and \code{record_created_time} columns.
#' 
#' @export
records_to_tibble <- function(records, convert_date = TRUE, tz = Sys.timezone()){
  fields <- records %>% 
    map("fields") %>% 
    map_dfr(function(x){
      lc <- map(x, function(y){
        if(length(y) > 1)
          y <- I(list(y))
        return(y)
      })
      as_tibble(lc)
    })

  record_id <- records %>% 
    map_chr("id")
  
  record_created_time <- records %>% 
    map_chr("createdTime") 

  if(convert_date)
    record_created_time <- as.POSIXct(record_created_time, "%Y-%d-%mT%H:%M:%OSZ", tz = tz)

  tibble(
    record_id = record_id,
    record_created_time = record_created_time
  ) %>% 
    bind_cols(fields)
}