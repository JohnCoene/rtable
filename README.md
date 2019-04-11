[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing) [![Travis build status](https://travis-ci.org/JohnCoene/rtable.svg?branch=master)](https://travis-ci.org/JohnCoene/rtable) [![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/JohnCoene/rtable?branch=master&svg=true)](https://ci.appveyor.com/project/JohnCoene/rtable)

# rtable

An R wrapper to the [Airtable API](https://airtable.com/api).

## Installation

You can install the development version from Github.

```r
# install.packages("remotes")
remotes::install_github("JohnCoene/rtable")
```

## How-to

First create an account if you do not have one already. Once logged in, go to your "Account" page and keep note of your API key as we will need it in the next section. Finally visit the [Airtable API documentation page](https://airtable.com/api) and select a base, the documentation will show you the `id` of the base. Here we set up the session for the demo "Employee Onboarding" base and its "Onboarding Checklist" table. 

The following snippets of code will fetch records, create, update and finally delete records to end with the original table unchanged.

### Setup

Set up your session with the `setup` function. The function will let you pass your `api_key` (__required__), and optionally a `base`, and a `table`. Here we set up the session for the demo "Employee Onboarding" base.


```r
library(rtable)
setup(api_key = "xxXXxxxXXx", base = "appfSQILnns4mrSUr", table = "Onboarding Checklist")
```


```
#> ✔ API KEY is set up 
#> ✔ Base is set up 
#> ✔ Table is set up
```

Note that you can set up your `api_key` as a global variable by adding the option below to your `.Renviron` or `.Rprofile`.

```r
options("RTABLE_API_KEY" = "xXxxXXXxx")
```

You can check what has been set up with.


```r
get_setup()
#> ✔ API KEY is set up 
#> ✔ Base is set up 
#> ✔ Table is set up
```

You can always reset the setup with `reset_setup`.

### GET

We can then list records with the `list_records` function. Since we specified a `base`, a `table` and a `view` in our `setup` function we do not need to do so here. If you had not setup the latter previously you can specify them in `list_records`.


```r
records <- list_records()
#> ❯ Fetching page 1 
#> ✔ 18 records downloaded
```

Note that all functions return `list`s, we can convert those to list columns with, `records_to_tibble`.


```r
df <- records_to_tibble(records)
dplyr::glimpse(df)
#> Observations: 18
#> Variables: 8
#> $ record_id            <chr> "rec0ews9ZIAHUI86P", "rec593llWKXu0FKpR", "…
#> $ record_created_time  <dttm> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ `When?`              <chr> "Before your first day", "First day", "Week…
#> $ Name                 <chr> "Complete your I-9 form and bring relevant …
#> $ `Complete?`          <lgl> TRUE, NA, NA, TRUE, NA, TRUE, TRUE, NA, NA,…
#> $ `Relevant Resources` <list> [NULL, "rec3z3adT99u9XQPm", "recY5ekTUA112…
#> $ Notes                <chr> NA, NA, "Our CEO, Jasmin, has a short orien…
#> $ Date                 <chr> NA, NA, "2019-04-17", NA, NA, NA, NA, NA, N…
```

We can retrieve a single record with `retrieve_record`.


```r
record <- retrieve_record(records[[1]]$id)
#> ✔ Record rec0ews9ZIAHUI86P sucessfully retrieved
```

## Create

We can create a new record with `create_records`


```r
df <- data.frame(Name = "It's me")
created <- create_records(df)
#> ✔ Sucessfully created 1 records
```

To demonstrate that it worked we can retrieve it again with `retrieve_record`


```r
(rec <- created[[1]]$id) # we'll need it later
#> [1] "recSGXj0xX8FPZmR1"
retrieve_record(rec)
#> ✔ Record recSGXj0xX8FPZmR1 sucessfully retrieved
```

## Update

We can update records with `update_record`. Let's update the one we just created.


```r
df <- data.frame(Name = "It's me again!", record_id = rec)
updated <- update_records(df, record_id)
#> ✔ Sucessfully updated 1 records
```

We'll retrieve the record we updated and see if it matches the one we updated.


```r
updated_record <- retrieve_record(rec)
#> ✔ Record recSGXj0xX8FPZmR1 sucessfully retrieved
identical(list(updated_record), updated)
#> [1] TRUE
```

## Delete

Finally, we can delete the record we created and updaed.


```r
delete_record(rec)
#> ✔ Sucessfully deleted recSGXj0xX8FPZmR1
```

Then retrieving the deleted record should error as the record we want to retrieve is inexistent.


```r
retrieve_record(rec)
#> ✖ Error on recSGXj0xX8FPZmR1 -  Record not found
```
