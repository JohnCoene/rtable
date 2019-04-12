#' Airtable
#' 
#' Create an rtable object.
#' 
#' @section Methods:
#' Methods sarting in \code{get_} return \code{tibble} or \code{list} objects other methods return the 
#' class invisibly.
#' \itemize{
#'  \item{\code{new}: create an \code{rtable}.}
#'  \item{\code{silent}: make the object silent, equivalent to \code{quiet=FALSE} in other functions.}
#'  \item{\code{set_records}: add pre-existing records to the \code{rtable} from a \code{data.frame}.}
#'  \item{\code{list_records}: download records from Airtable, \emph{overrides} current records.}
#'  \item{\code{retrieve_record}: retrieve a specific record.}
#'  \item{\code{retrieve_records}: retrieve specific records}
#'  \item{\code{create_records}: create records.}
#'  \item{\code{update_records}: update records.}
#'  \item{\code{delete_records}: delete records.}
#'  \item{\code{delete_record}: delete a record.}
#'  \item{\code{get_created}: returns created records.}
#'  \item{\code{get_listed}: returns records downloaded.}
#'  \item{\code{get_updated}: returns updated records.}
#'  \item{\code{get_deleted}: returns deleted records.}
#'  \item{\code{get_retrieved}: returns retrieved records.}
#'  \item{\code{refresh}: refresh records uses arguments last used \code{list_records} or defaults if never used.}
#' }
#' 
#' @section Arguments:
#' \itemize{
#'  \item{\code{new}: see \code{\link{setup}}, also takes an additional \code{list_records} argument which defaults to \code{TRUE} and collects all records in table.}
#'  \item{\code{silent}: takes a boolean.}
#'  \item{\code{set_records}: a \code{data.frame} or \code{tibble}.}
#'  \item{\code{list_records}: see \code{\link{list_records}}.}
#'  \item{\code{retrieve_record}: see \code{\link{retrieve_record}}.}
#'  \item{\code{retrieve_records}: see \code{\link{retrieve_records}}.}
#'  \item{\code{create_records}: see \code{\link{create_records}}.}
#'  \item{\code{update_records}: see \code{\link{update_records}}.}
#'  \item{\code{delete_records}: see \code{\link{delete_records}}.}
#'  \item{\code{delete_record}: see \code{\link{delete_record}}.}
#'  \item{\code{get_created}: takes \code{to_tibble} to optionally convert to a \code{tibble}.}
#'  \item{\code{get_listed}: takes \code{to_tibble} to optionally convert to a \code{tibble}.}
#'  \item{\code{get_updated}: takes \code{to_tibble} to optionally convert to a \code{tibble}.}
#'  \item{\code{get_deleted}: takes \code{to_tibble} to optionally convert to a \code{tibble}.}
#'  \item{\code{get_retrieved}: takes \code{to_tibble} to optionally convert to a \code{tibble}.}
#'  \item{\code{refresh}: takes \code{to_tibble} to optionally convert to a \code{tibble}.}
#' }
#' 
#' @export
rtable <- R6::R6Class(
  "rtable",
  public = list(
    initialize = function(base, table, api_key = NULL, list_records = TRUE){

      if(missing(base) || missing(table))
        stop("Missing base, or table", call. = FALSE)
      
      if(is.null(api_key))
        api_key <- .get_api_key()

      if(is.null(api_key))
        stop("api_key was nor passed, nor setup", call. = FALSE)

      setup(api_key = api_key)

      private$.base <- base
      private$.table <- table

      if(list_records){
        private$.last_updated <- Sys.time()
        private$.records <- list_records(
          base = private$.base, 
          table = private$.table,
          quiet = private$.quiet
        )
      }

      invisible(self)
    },
    print = function(...){
      rec <- length(private$.records)
      upd <- private$.last_updated

      cat(
        crayon::blue(cli::symbol$pointer), rec, "records\n"
      )

      cat(
        crayon::yellow(cli::symbol$warning), "last updated at", 
        as.character(upd), "\n"
      )

    },
    set_records = function(records){
      if(inherits(records, "data.frame")){
        private$.records <- records
        private$.last_updated <- Sys.time()
      } else
        cat(
          crayon::red(cli::symbol$cross),
          "Records must be of class data.frame"
        )
    },
    silent = function(quiet = TRUE){
      private$.quiet <- quiet
    },
    refresh = function(){
      private$.records <- list_records(
        base = private$.base, table = private$.table, view = private$.view, 
        from_record = private$.from_record, sort = private$.sort, 
        page_size = private$.page_size, max_records = private$.max_records, 
        fields = private$.fields, filter = private$.filter, quiet = private$.quiet
      )
      private$.last_updated <- Sys.time()
      invisible(self)
    },
    list_records = function(view = NULL, from_record = NULL, sort = NULL, 
      page_size = 100, max_records = 10000, fields = NULL, filter = NULL){

      private$.records <- list_records(
        base = private$.base, table = private$.table, view = view, 
        from_record = from_record, sort = sort, page_size = page_size, 
        max_records = max_records, fields = fields, filter = filter, 
        quiet = private$.quiet
      )

      private$.last_updated <- Sys.time()
      private$.view <- view
      private$.from_record <- from_record
      private$.sort <- sort
      private$.page_size <- page_size
      private$.max_records <- max_records
      private$.fields <- fields
      private$.filter <- filter

      invisible(self)
    },
    create_records = function(records, typecast = TRUE){
      created <- create_records(records, typecast, base = private$.base, 
        table = private$.table, quiet = private$.quiet)
      private$.created <- append(private$.created, created)
      invisible(self)
    },
    retrieve_record = function(record){
      retrieved <- retrieve_record(record, base = private$.base, 
        table = private$.table, quiet = private$.quiet)
      private$.retrieved <- append(private$.retrieved, list(retrieved))
      invisible(self)
    },
    retrieve_records = function(records){
      retrieved <- retrieve_records(records, base = private$.base, 
        table = private$.table, quiet = private$.quiet)
      private$.retrieved <- append(private$.retrieved, retrieved)
      invisible(self)
    },
    update_records = function(records, ids){
      ids_enquo <- enquo(ids)
      updated <- update_records(records, !!ids_enquo, base = private$.base, 
        table = private$.table, quiet = private$.quiet)
      private$.updated <- append(private$.updated, updated)
      invisible(self)
    },
    delete_records = function(records, ids){
      ids_enquo <- enquo(ids)
      deleted <- delete_records(records, !!ids_enquo, base = private$.base, 
        table = private$.table, quiet = private$.quiet)
      private$.deleted <- append(private$.deleted, deleted)
      invisible(self)
    },
    delete_record = function(id){
      deleted <- delete_record(id, base = private$.base, 
        table = private$.table, quiet = private$.quiet)
      private$.deleted <- append(private$.deleted, deleted)
      invisible(self)
    },
    get_retrieved = function(to_tibble = TRUE){
      thing <- private$.retrieved
      if(to_tibble)
        thing <- records_to_tibble(thing)
      return(thing)
    },
    get_listed = function(to_tibble = TRUE){
      thing <- private$.records
      if(to_tibble)
        thing <- records_to_tibble(thing)
      return(thing)
    },
    get_updated = function(to_tibble = TRUE){
      thing <- private$.updated
      if(to_tibble)
        thing <- records_to_tibble(thing)
      return(thing)
    },
    get_created = function(to_tibble = TRUE){
      thing <- private$.created
      if(to_tibble)
        thing <- records_to_tibble(thing)
      return(thing)
    },
    get_deleted = function(to_tibble = TRUE){
      thing <- private$.deleted
      if(to_tibble)
        thing <- records_to_tibble(thing)
      return(thing)
    }
  ),
  active = list(
    records = function(records) {
      if(inherits(records, "data.frame"))
        private$.records <- records
      else
        cat(
          crayon::red(cli::symbol$cross),
          "Records must be of class data.frame"
        )
    }
  ),
  private = list(
    .quiet = FALSE,
    .records = list(),
    .base = NULL,
    .table = NULL,
    .created = list(),
    .retrieved = list(),
    .updated = list(),
    .deleted = list(),
    .view = NULL, 
    .from_record = NULL,
    .sort = NULL, 
    .page_size = 100,
    .max_records = 10000,
    .fields = NULL,
    .filter = NULL,
    .last_updated = Sys.time()
  )
)
#'  \item{\code{}}