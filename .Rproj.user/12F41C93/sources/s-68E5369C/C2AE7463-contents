library("bib2df")

columnsToKeep <- c(
  "AUTHOR",
  "JOURNAL",
  "MONTH",
  "PUBLISHER",
  "TITLE",
  "YEAR",
  "ABSTRACT",
  "LANGUAGE",
  "AFFILIATION",
  "DOI",
  "KEYWORDS.PLUS",
  "RESEARCH.AREAS",
  "NUMBER.OF.CITED.REFERENCES",
  "TIMES.CITED",
  "USAGE.COUNT.LAST.180.DAYS",
  "USAGE.COUNT.SINCE.2013"
)

# Only keeps the columns of interest form the data frame
filterColumns <- function(df) {
  return(df[columnsToKeep])
}

readAllRecs <- function() {
  # Start with the base one
  recs <- filterColumns(bib2df("savedrecs.bib"))
  
  # Read the next ones until there are no more left
  tryCatch(
    {
      for (index in 1:9999) {
        fileRecs <- bib2df(sprintf("savedrecs (%d).bib", index))
        recs <- rbind(recs, filterColumns(fileRecs))
      }
    },
    error=function(cond) {
      return(NA)
    },
    warning=function(cond) {
      return(NA)
    }
  )
  
  return(recs)
}