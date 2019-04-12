#' Create
#' 
#' Create records
#' 
#' @inheritParams list-records
#' @param records A \code{data.frame} or \code{list} of records to create.
#' @param typecast The Airtable API will perform best-effort
#'  automatic data conversion from string values if this is set to \code{TRUE}.
#' 
#' @return A list of created records.
#' @name create-records
#' @export
create_records <- function(records, typecast = TRUE, base = NULL, 
  table = NULL, quiet = !interactive()) {

  # check inputs
  if(missing(records))
    stop("Missing records", call. = FALSE)

  # Check if inputs present
  base <- .get_base(base)
  table <- .get_table(table)

  if(is.null(base) || is.null(table))
    stop("Missing base or view", call. = FALSE)

  if(inherits(records, "data.frame"))
    records_list <- records %>% 
      apply(1, function(x, typecast){
        list(
          fields = as.list(x),
          typecast = typecast
        )
      }, typecast)
  else
    records_list <- records

  call <- .build_path(base, table) %>% 
    .build_url()
  token <- .get_bearer_token()

  created <- list()

  for(i in 1:length(records_list)){

    rec <- records_list[[i]]

    response <- POST(
      call,
      body = rec,
      add_headers(Authorization = token),
      encode = "json"
    )

    ok <- .check_response(response, rec, quiet)
    if(ok){
      content <- content(response)
      created <- append(created, list(content))
    }
    
  }

  if(!quiet){
    cat(
      crayon::green(cli::symbol$tick),
      "Sucessfully created", length(created), "records\n"
    )
  }

  invisible(created)
}
