library(rjson)

parse.count <- 0
fromJSON.with.logs <- function(data.line) {
    parse.count <<- parse.count + 1
    print(sprintf('Parsing %d line...', parse.count))
    fromJSON(data.line)
}

basic.parser <- function(data.lines, id.name) {
    parse.count <<- 0
    print(sprintf('Parsing for %s', id.name))
    data.list <- lapply(data.lines, fromJSON.with.logs)
    names(data.list) <- sapply(data.list, '[[', id.name)
    data.list
}
