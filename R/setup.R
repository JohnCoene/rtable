#' Configure
#' 
#' Specifies API KEY for session
#' 
#' @param api_key Your Airtable API key.
#' @param base An Airtable base name.
#' @param table An Airtable table name.
#' @param quiet Set to \code{TRUE} to print helpful messages.
#' 
#' @examples
#' setup("xXXXXxxxXXXxx")
#' get_setup()
#' 
#' @importFrom utils URLencode
#' @import httr
#' @import purrr
#' @import dplyr
#' @export
#' @name setup
setup <- function(base = NULL, table = NULL, api_key = NULL) {

  # setup
  if(!is.null(api_key))
    options("RTABLE_API_KEY" = api_key)
  if(!is.null(base))
    options("RTABLE_BASE" = base)
  if(!is.null(table))
    options("RTABLE_TABLE" = table)

  get_setup()
}

#' @rdname setup
#' @export
get_setup <- function(quiet = !interactive()){
  key <- getOption("RTABLE_API_KEY")
  base <- getOption("RTABLE_BASE")
  table <- getOption("RTABLE_TABLE")

  .prt <- function(val, what){
    if(!is.null(val))
      cat(
        crayon::green(cli::symbol$tick),
        sprintf("%s is set up", what),
        "\n"
      )
    else 
      cat(
        crayon::red(cli::symbol$cross),
        sprintf("%s is not set up", what),
        "\n"
      )
  }

  l <- list(
    api_key = key,
    base = base,
    table = table
  )
  names <- c("API KEY", "Base", "Table")

  if(!quiet)
    map2(l, names, .prt)

  invisible(l)
}

#' @rdname setup
#' @export
reset_setup <- function(base = TRUE, table = TRUE, api_key = FALSE){

  .prt <- function(what){
    cat(
      crayon::green(cli::symbol$tick),
      sprintf("%s sucessfully reset", what),
      "\n"
    )
  }

  if(isTRUE(api_key)){
    options("RTABLE_API_KEY" = NULL)
    .prt("API Key")
  }
  if(isTRUE(base)){
    options("RTABLE_BASE" = NULL)
    .prt("Base")
  }
  if(isTRUE(table)){
    options("RTABLE_TABLE" = NULL)
    .prt("Table")
  }

}