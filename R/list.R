#' List
#' 
#' List records in table.
#' 
#' @param base Base, whatever this is Airtable does not document it.
#' @param table Name of table.
#' @param max_records Maximum number of records to fetch.
#' @param page_size Size of pages, `100` max.
#' @param sort Sorting, `asc` or `desc`.
#' @param view View to fetch.
#' @param from_record Record wherefrom to start listing.
#' @param quiet Set to `TRUE` to print helpful messages.
#' 
#' @name list-records
#' @export
list_records <- function(base = NULL, table = NULL, view = NULL, 
  from_record = NULL, sort = NULL, page_size = 100, max_records = 1000, 
  quiet = !interactive()) {

  # Check if inputs present
  base <- .get_base(base)
  table <- .get_table(table)
  view <- .get_view(view)

  if(is.null(base) || is.null(table) || is.null(view))
    stop("Missing base, table, or view", call. = FALSE)

  # ensure page size is below 100
  if(page_size > 100){
    page_size <- 100
    if(!quiet)
      cat(
        crayon::yellow(cli::symbol$warning),
        "Setting page_size to 100\n"
      )
  }

  # initialise loop
  offset <- from_record
  if(is.null(from_record)) offset <- "initialise"
  records <- list()
  i <- 1

  # loop until no more offset
  while(!is.null(offset)){

    if(!quiet){
      cat(
        crayon::blue(cli::symbol$pointer),
        "Fetching page", i, "\n"
      )
    }

    # no offset for initial query
    if(i == 1 && is.null(from_record)) offset <- NULL

    # buil URL
    call <- .build_path(base, table) %>% 
      .build_query(
        maxRecords = max_records,
        pageSize = page_size,
        sort = sort,
        view = view,
        offset = offset
      ) %>% 
      .build_url()

    # Call API
    token <- .get_bearer_token()
    response <- GET(call, add_headers(Authorization = token))
    stop_for_status(response)
    content <- content(response)

    records <- append(records, content$records)

    offset <- content$offset # reassign offset
    i <- i + 1
  }

  if(!quiet){
    cat(
      crayon::green(cli::symbol$tick),
      length(records), "records downloaded\n"
    )
  }

  return(records)
  
}