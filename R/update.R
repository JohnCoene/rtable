#' Create
#' 
#' Create records
#' 
#' @inheritParams list-records
#' @param records A \code{data.frame} of records to create.
#' @param ids Bare column name containing record ids to update.
#' 
#' @return A list of created records.
#' @export
update_records <- function(records, ids, base = NULL, table = NULL, 
  quiet = !interactive()) {

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
    dplyr::pull(!!ids_enquo) %>% 
    as.character()

  records_list <- records %>% 
    dplyr::select(-!!ids_enquo) %>% 
    apply(1, function(x){
      list(
        fields = as.list(x)
      )
    })
  
  token <- .get_bearer_token()

  updated <- list()

  for(i in 1:length(records_list)){

    rec <- record_ids[[i]]

    # create call
    call <- .build_path(base, table, rec) %>% 
      .build_url()
    
    response <- PATCH(
      call,
      body = records_list[[i]],
      add_headers(Authorization = token),
      encode = "json"
    )

    ok <- .check_response(response, rec)
    if(ok){
      content <- content(response)
      updated <- append(updated, list(content))
    }
  }

  if(!quiet){
    cat(
      crayon::green(cli::symbol$tick),
      "Sucessfully updated", length(updated), "records\n"
    )
  }

  invisible(updated)
}