#' Retrive
#' 
#' Retrive a record
#' 
#' @inheritParams list-records
#' @param record A record to retrieve.
#' 
#' @export
retrieve_record <- function(record, base = NULL, table = NULL, 
  quiet = !interactive()) {

  if(missing(record))
    stop("Missing record", call. = FALSE)

  # Check if inputs present
  base <- .get_base(base)
  table <- .get_table(table)

  if(is.null(base) || is.null(table))
    stop("Missing base or view", call. = FALSE)

  # buil URL
  call <- .build_path(base, table, record) %>% 
    .build_url()

  # Call API
  token <- .get_bearer_token()
  response <- GET(call, add_headers(Authorization = token))
  stop_for_status(response)
  content <- content(response)

  if(!quiet){
    cat(
      crayon::green(cli::symbol$tick),
      "Record", record, "sucessfully retrieved\n"
    )
  }

  invisible(content)
}