#' Retrive
#' 
#' Retrive a record
#' 
#' @inheritParams list-records
#' @param record,records Record(s) to retrieve.
#' 
#' @name retrieve-records
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
  ok <- .check_response(response, record, quiet)
  if(ok)
    content <- content(response)
  else
    content <- list()

  if(!quiet && length(content) > 0){
    cat(
      crayon::green(cli::symbol$tick),
      "Record", record, "sucessfully retrieved\n"
    )
  }

  invisible(content)
}

#' @rdname retrieve-records
#' @export
retrieve_records <- function(records, base = NULL, table = NULL, 
  quiet = !interactive()){
  map(records, retrieve_record, base = base, table = table, quiet = quiet)
}