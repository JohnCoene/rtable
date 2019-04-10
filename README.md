# rtable

`rtable` 

## Installation

You can install the development version from Github.

```r
# install.packages("remotes")
remotes::install_github("JohnCoene/rtable")
```

## How-to

First create an account if you do not have one already. Once logged in, go to your "Account" page and keep note of your API key as we will need it in the next section. Finally visit the [Airtable API documentation page](https://airtable.com/api) and select a base. Here we set up the session for the demo "Employee Onboarding" base and its "Onboarding Checklist" table.

### Setup

Set up your session with the `setup` function. The function will let you pass your `api_key` (__required__), and optionally a `base`, a `table` and a `view`. Here we set up the session for the demo "Employee Onboarding" base, we'll also specify the "All Onboarding Tasks" `view`.


```r
library(rtable)
setup(api_key = "xxXXxxxXXx", base = "appfSQILnns4mrSUr", table = "Onboarding Checklist", view = "All Onboarding Tasks")
```


```
#> ✔ Base sucessfully setup 
#> ✔ Table sucessfully setup 
#> ✔ View sucessfully setup
```

Note that you can set up your `api_key` as a global variable with.

```r
options("RTABLE_API_KEY" = "xXxxXXXxx")
```

You can check what has been set up with.


```r
get_setup()
#> ✔ API KEY is set up 
#> ✔ Base is set up 
#> ✔ Table is set up 
#> ✔ View is set up
```

### GET

We can then list records with the `list_records` function. Since we specified a `base`, a `table` and a `view` in our `setup` function we do not need to do so here. If you had not setup the latter previously you can specify them in `list_records`.


```r
records <- list_records()
#> ❯ Fetching page 1 
#> ✔ 19 records downloaded
```

We can retrieve a single record with `retrieve_record`.


```r
record <- retrieve_record(records[[1]]$id)
#> ✔ Record recQH369e9U4LcgzU sucessfully retrieved
```
