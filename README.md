
# dbfaker

## Installation

1. Generate personal access token __PAT__ through your Github account (https://github.com/settings/tokens)
  * Give it `repo`, `read:repo_hook`, `read:org`, and `read:gpg_key` rights (if possible)
2. In RStudio, do:
```{r eval=FALSE}
install.packages('devtools')
devtools::install_github('newshipt/dbfaker', auth_token=<put your PAT here>)
```

## Usage
```{r eval=FALSE}

# parse cli arguments
library(optparse)
option_list <- list(
  make_option(c("-d", "--date"),
              type="character",
              default=as.character(as_date(with_tz(Sys.time(), "GMT"))),
              help="date of execution"),
  make_option(c("-t", "--test"),
              action="store_true",
              default=FALSE,
              help="execute tests and mask writes to databases")
)
opt <- parse_args(OptionParser(option_list=option_list))

# execute with parsed arguments
run(opt$date, opt$test)

# main function which encapsulates high level functions of your program
run <- function(date, test) {
  readData(...)
  transformData(...)
  trainModel(...)
  ifelse(test, dbfaker::verifyWrite(...), writeData(...))
}

# components of your program
readData <- function(...) {}
transformData <- function(...) {}
trainModel <- function(...) {}
writeData <- function(...) {
  dbWriteTable(...)
  dbSendQuery(...)
}
```
