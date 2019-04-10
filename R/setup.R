#' Configure
#' 
#' Specifies API KEY for session
#' 
#' @param api_key Your Airtable API key.
#' @param base An Airtable base name.
#' @param table An Airtable table name.
#' @param view A view name.
#' @param quiet Set to \code{TRUE} to print helpful messages.
#' 
#' @examples
#' setup("xXXXXxxxXXXxx")
#' get_setup()
#' 
#' @importFrom utils URLencode
#' @import httr
#' @export
#' @name setup
setup <- function(api_key = NULL, base = NULL, table = NULL, view = NULL) {

  .prt <- function(what){
    cat(
      crayon::green(cli::symbol$tick),
      sprintf("%s sucessfully setup", what),
      "\n"
    )
  }

  # setup
  if(!is.null(api_key)){
    options("RTABLE_API_KEY" = api_key)
    .prt("API Key")
  }
  if(!is.null(base)){
    options("RTABLE_BASE" = base)
    .prt("Base")
  }
  if(!is.null(table)){
    options("RTABLE_TABLE" = table)
    .prt("Table")
  }
  if(!is.null(view)){
    options("RTABLE_VIEW" = view)
    .prt("View")
  }
}

#' @rdname setup
#' @export
get_setup <- function(quiet = !interactive()){
  key <- getOption("RTABLE_API_KEY")
  base <- getOption("RTABLE_BASE")
  table <- getOption("RTABLE_TABLE")
  view <- getOption("RTABLE_VIEW")

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
    table = table,
    view = view
  )
  names <- c("API KEY", "Base", "Table", "View")

  if(!quiet)
    purrr::map2(l, names, .prt)

  invisible(l)
}