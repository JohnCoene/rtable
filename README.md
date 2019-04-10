[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)

# rtable

`rtable` 

## Installation

You can install the development version from Github.

```r
# install.packages("remotes")
remotes::install_github("JohnCoene/rtable")
```

## How-to

First create an account if you do not have one already. Once logged in, go to your "Account" page and keep note of your API key as we will need it in the next section. Finally visit the [Airtable API documentation page](https://airtable.com/api) and select a base, the documentation will show you the `id` of the base you are viewing. Here we set up the session for the demo "Employee Onboarding" base and its "Onboarding Checklist" table. 

### Setup

Set up your session with the `setup` function. The function will let you pass your `api_key` (__required__), and optionally a `base`, a `table` and a `view`. Here we set up the session for the demo "Employee Onboarding" base, we'll also specify the "All Onboarding Tasks" `view`.


```r
library(rtable)
setup(api_key = "xxXXxxxXXx", base = "appfSQILnns4mrSUr", table = "Onboarding Checklist", view = "All Onboarding Tasks")
```


```
> ✔ Base sucessfully setup 
> ✔ Table sucessfully setup 
> ✔ View sucessfully setup
```

Note that you can set up your `api_key` as a global variable by adding the option below to your `.Renviron` or `.Rprofile`.

```r
options("RTABLE_API_KEY" = "xXxxXXXxx")
```

You can check what has been set up with.


```r
get_setup()
> ✔ API KEY is set up 
> ✔ Base is set up 
> ✔ Table is set up 
> ✔ View is set up
```

You can always reset the setup with `reset_setup`.

### GET

We can then list records with the `list_records` function. Since we specified a `base`, a `table` and a `view` in our `setup` function we do not need to do so here. If you had not setup the latter previously you can specify them in `list_records`.


```r
records <- list_records()
> ❯ Fetching page 1 
> ✔ 20 records downloaded
```

We can retrieve a single record with `retrieve_record`.


```r
record <- retrieve_record(records[[1]]$id)
> ✔ Record recQH369e9U4LcgzU sucessfully retrieved
```

## Create

We can create a new record with `create_records`


```r
df <- data.frame(Name = "It's me")
created <- create_records(df)
> ✔ Sucessfully created 1 records
```

To demonstrate that it worked we can retrieve it again with `retrieve_record`


```r
(rec <- created[[1]]$id) # we'll need it later
> [1] "recsT79bPrGt8d5J8"
retrieve_record(rec)
> ✔ Record recsT79bPrGt8d5J8 sucessfully retrieved
```

## Update

We can update records with `update_record`. Let's update the one we just created.


```r
df <- data.frame(Name = "It's me again!", record_id = rec)
updated <- update_records(df, record_id)
> ✔ Sucessfully updated 1 records
```

We'll retrieve the record we updated and see if it matches the one we updated.


```r
updated_record <- retrieve_record(rec)
> ✔ Record recsT79bPrGt8d5J8 sucessfully retrieved
identical(list(updated_record), updated)
> [1] TRUE
```

## Delete

Finally, we can delete the record we created and updaed.


```r
delete_record(rec)
```

Then retrieving the deleted record should error as the record we want to retrieve is inexistent.


```r
retrieve_record(rec)
> Error in retrieve_record(rec): Not Found (HTTP 404).
```
