#! /usr/bin/env Rscript

splitIntoElements <- function(line) unlist(strsplit(line, "\t"))

## **** could wo with a single readLines or in blocks
## For some general info, see this webpage
## https://datafireball.com/2013/10/10/putting-your-r-code-into-pipeline/
con <- file("stdin", open = "r")
while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
    vec <- splitIntoElements(line)
    new <- ifelse(as.numeric(vec[3]) > 40, 1, 0)
    cat(paste(paste(c(vec, new), collapse=" \t "), "\n", collapse=""))  
    
}
close(con)