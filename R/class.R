#' Airtable
#' 
#' Create an rtable object.
#' 
#' @section Methods:
#' Methods sarting in `get_` return `tibble` or `list` objects other methods return the 
#' class invisibly.
#' * `new` - Create an `rtable`.
#' * `silent` - Make the object silent, equivalent to `quiet=FALSE` in other functions.
#' * `set_records` - Add pre-existing records to the `rtable` from a `data.frame`.
#' * `list_records` - Download records from Airtable, _overrides_ current records.
#' * `retrieve_record` - Retrieve a specific record.
#' * `retrieve_records` - Retrieve specific records.
#' * `create_records` - Create records.
#' * `update_records` - Update records.
#' * `delete_records` - Delete records.
#' * `delete_record` - Delete a record.
#' * `get_created` - Returns created records.
#' * `get_listed` - Returns records downloaded.
#' * `get_updated` - Returns updated records.
#' * `get_deleted` - Returns deleted records.
#' * `get_retrieved` - Returns retrieved records.
#' * `refresh` - Refresh records uses arguments last used `list_records` or defaults if never used.
#' 
#' @section Arguments:
#' * `new` - see \code{\link{setup}}, also takes an additional `list_records` argument which defaults to `TRUE` and collects all records in table.
#' * `silent` - takes `TRUE` or `FALSE`.
#' * `set_records` - see \code{\link{setup}}.
#' * `list_records` - see \code{\link{list_records}}.
#' * `retrieve_record` - see \code{\link{retrieve_record}}.
#' * `retrieve_records` - see \code{\link{retrieve_records}}.
#' * `create_records` - see \code{\link{create_records}}.
#' * `update_records` - see \code{\link{update_records}}.
#' * `delete_records` - see \code{\link{delete_records}}.
#' * `delete_record` - see \code{\link{delete_record}}.
#' * `get_created` - Takes `to_tibble` to optionally convert to a `tibble`.
#' * `get_listed` - Takes `to_tibble` to optionally convert to a `tibble`.
#' * `get_updated` - Takes `to_tibble` to optionally convert to a `tibble`.
#' * `get_deleted` - Takes `to_tibble` to optionally convert to a `tibble`.
#' * `get_retrieved` - Takes `to_tibble` to optionally convert to a `tibble`.
#' * `refresh` - No arguments.
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
