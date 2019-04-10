#' Delete
#' 
#' Delete a record
#' 
#' @inheritParams list-records
#' @param records A `data.frame` of records to create.
#' @param id,ids Bare column name containing record ids to update.
#' 
#' @name delete-records
#' @export
delete_records <- function(records, ids, base = NULL, table = NULL, 
  quiet = !interactive()){
  
  # check basics present
  if(missing(records) || missing(ids))
    stop("Missing records, or ids", call. = FALSE)

  # Check if inputs present
  base <- .get_base(base)
  table <- .get_table(table)

  if(is.null(base) || is.null(table))
    stop("Missing base or view", call. = FALSE)

  # create record vector
  ids_enquo <- dplyr::enquo(ids)
  record_ids <- records %>% 
    dplyr::pull(!!ids_enquo)

  deleted <- list()

  for(i in 1:length(record_ids)){

    rec <- record_ids[[i]]
    del <- delete_record(rec, base, table, quiet)
    if(length(del) > 0)
      deleted <- append(deleted, list(del))
  }

  if(!quiet){
    cat(
      crayon::green(cli::symbol$tick),
      "Sucessfully deleted", length(deleted), "records\n"
    )
  }

  invisible(deleted)
}

#' @rdname delete-records
#' @export
delete_record <- function(id, base = NULL, table = NULL, 
  quiet = !interactive()){

  # check basics present
  if( missing(id))
    stop("Missing id", call. = FALSE)

  # Check if inputs present
  base <- .get_base(base)
  table <- .get_table(table)

  if(is.null(base) || is.null(table))
    stop("Missing base or view", call. = FALSE)

  # create call
  call <- .build_path(base, table, id) %>% 
    .build_url()
  
  token <- .get_bearer_token()

  response <- DELETE(
    call,
    add_headers(Authorization = token),
    encode = "json"
  )

  ok <- .check_response(response, id, quiet)
  if(ok)
    content <- content(response)
  else 
    content <- list()

  if(!quiet && length(content) > 0){
    cat(
      crayon::green(cli::symbol$tick),
      "Sucessfully deleted", id, "\n"
    )
  }

  invisible(content)
}