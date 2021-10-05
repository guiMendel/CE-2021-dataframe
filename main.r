library("bib2df")
library("dplyr")

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

# Returns dataframe with only the selected language
filterLanguage <- function(recs, language) {
  newDF <- filter(recs, LANGUAGE == language)
}

# Returns dataframe without the selected language
removeLanguage <- function(recs, language) {
  newDF <- filter(recs, LANGUAGE != language)
}

readAllRecs <- function() {
  # Start with the base one
  recs <- filterColumns(bib2df("savedrecs.bib"))
  
  # Read the next ones until there are no more left
  tryCatch(
    {
      for (index in 1:9999) {
        print(index)
        fileRecs <- bib2df(sprintf("savedrecs (%d).bib", index))

        # Make sure necessary columns are there
        for (column in columnsToKeep) {
          if(!column %in% colnames(fileRecs)) {
            fileRecs[, column] <- NA
          }
        }

        recs <- rbind(recs, filterColumns(fileRecs))
      }
    },
    error=function(cond) {
      print('Stopped. Message:')
      print(cond)
      return(NA)
    },
    warning=function(cond) {
      print('Stopped. Message:')
      print(cond)
      return(NA)
    }
  )
  
  # Ensure numeric columns
  recs <- transform(recs, YEAR = as.numeric(YEAR))
  recs <- transform(recs, NUMBER.OF.CITED.REFERENCES = as.numeric(NUMBER.OF.CITED.REFERENCES))
  recs <- transform(recs, TIMES.CITED = as.numeric(TIMES.CITED))
  recs <- transform(recs, USAGE.COUNT.LAST.180.DAYS = as.numeric(USAGE.COUNT.LAST.180.DAYS))
  recs <- transform(recs, USAGE.COUNT.SINCE.2013 = as.numeric(USAGE.COUNT.SINCE.2013))
  
  return(recs)
}