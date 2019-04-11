#' List
#' 
#' List records in table.
#' 
#' @param base Base, whatever this is Airtable does not document it.
#' @param table Name of table.
#' @param max_records Maximum number of records to fetch.
#' @param page_size Size of pages, `100` max.
#' @param sort Sorting, `asc` or `desc`.
#' @param view The name or ID of a view. If set, only the records
#'  in that view will be returned. The records will be sorted according 
#'  to the order of the view.
#' @param from_record Record wherefrom to start listing.
#' @param quiet Set to `TRUE` to print helpful messages.
#' @param fields A vector of fields to retrieve.
#' @param filter A [formula](https://support.airtable.com/hc/en-us/articles/203255215-Formula-Field-Reference)
#'  used to filter records.
#' 
#' @name list-records
#' @export
list_records <- function(base = NULL, table = NULL, view = NULL, 
  from_record = NULL, sort = NULL, page_size = 100, max_records = 1000, 
  fields = NULL, filter = NULL, quiet = !interactive()) {

  # Check if inputs present
  base <- .get_base(base)
  table <- .get_table(table)

  if(is.null(base) || is.null(table))
    stop("Missing base, or table", call. = FALSE)

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
        offset = offset,
        fields = fields,
        filterByFormula = filter
      ) %>% 
      .build_url()

    # Call API
    token <- .get_bearer_token()
    response <- GET(call, add_headers(Authorization = token))
    ok <- .check_response(response, quiet = quiet)
    if(ok){
      content <- content(response)
      records <- append(records, content$records)
    } else {
      break
    }

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