#' Convert
#'
#' Convert results to a `tibble`.
#'
#' @param records Records obtained from other functions of the package.
#' @param convert_date Set to `TRUE` to convert record creation time to `POSIXct`.
#' @param tz Timzone, used if `convert_date` is set to `TRUE`.
#'
#' @return A `tibble` containing the fields of the table together with
#'  the `record_id` and `record_created_time` columns.
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