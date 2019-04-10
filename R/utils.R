BASE_URL <- "https://api.airtable.com/"

.get_base_url <- function(){
  url <- httr::parse_url(BASE_URL)
  url$scheme <- "https"
  return(url)
}

.build_path <- function(...) {
  url <- .get_base_url()
  inputs <- c(...) %>% 
    purrr::map_chr(URLencode)
  path <- c("v0") %>% 
    append(inputs)
  url$path <- path
  return(url)
}

.build_query <- function(url, ...){
  url$query <- list(...)
  return(url)
}

.build_url <- function(url){
  build_url(url)
}

.get_base <- function(value){
  if(is.null(value))
    getOption("RTABLE_BASE")
  else
    value
}

.get_table <- function(value){
  if(is.null(value))
    getOption("RTABLE_TABLE")
  else
    value
}

.get_view <- function(value){
  if(is.null(value))
    getOption("RTABLE_VIEW")
  else
    value
}

.get_bearer_token <- function(){
  setup <- get_setup(TRUE)
  if(is.null(setup$api_key))
    stop("Missing API key", call. = FALSE)
  
  key <- getOption("RTABLE_API_KEY")

  paste("Bearer", key)
}

.status2bool <- function(status){
  v <- FALSE
  if(status == 200) v <- TRUE
  return(v)
}

.check_response <- function(response, record = NULL, quiet = FALSE){
  status <- status_code(response)

  if(status != 200 && !quiet){

    cnt <- content(response)

    err <- tryCatch(cnt$error$message, error = function(e) NULL)
    if(is.null(err))
      err <- cnt$error

    msg <- ""
    if(!is.null(record))
      msg <- paste("Error on", record, "- ")

    cat(
      crayon::red(cli::symbol$cross),
      msg,
      err,
      "\n"
    )
  }
  
  .status2bool(status)
}