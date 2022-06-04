# An initial script to query the Dimensions API.
# June 2022
# Jeff Demaine
# Following the instructions at: https://github.com/massimoaria/dimensionsR
# And: https://cran.r-project.org/web/packages/RSQLite/vignettes/RSQLite.html

# If not already done, install RSQLite
# For example: C:\Program Files\R\R-4.1.3\library 
#install.packages("RSQLite")
install.packages("dimensionsR")
install.packages("bibliometrix")
install.packages("tidyverse")

library(DBI)
library(RSQLite)
library(tidyverse)
library(dimensionsR)
library(bibliometrix)

############## PART ONE ################

# [1] Load a file into a data frame.
# Do this first: "Session -> Set Working Directory -> To Source File Location"
my_data <- read.csv(file.choose()) # C:\abcd\

# [1.1] Check that the data looks like it should:
head(my_data, 6)

# [2] Make a local SQLite database and a connection object to it:
con <- dbConnect(RSQLite::SQLite(), "my_local_database.db")


############## PART TWO ################

# [3] Load my API authorization 
# token <- dsAuth(username = "", password = "")
token <- dsAuth(key = "")

# [4] Define query for Dimensions:
query.string <- "search publications 
  where (year in [2020]) and (type in [\"article\"]) and research_orgs.id = \"grid.25073.33\" 
  return publications[basics + extras + categories + concepts]"

# [5] Send the query string and the token to the Dimensions API. Return zero records:
res <- dsApiRequest(token = token, query = query.string, limit = 1, verbose = TRUE)

# [5.1] Just how many records am I going to download?
res$total_count



# [6] Send the query again and retrieve 250 records at a time:
Dimensions.results <- dsApiRequest(token = token, query = query.string, step = 250, limit = res$total_count)

# [7] Convert the Dimensions XML list into an R data frame.
# This function needs to be modified in order to return the sub-lists (OA, SDG):
Dimensions.results.DF <- dsApi2dfRev(P = Dimensions.results, format = "raw") # dsApi2df(Dimensions.results)

############## PART THREE ##############
# JOIN the two data frames using RSQLite:

# Create temporary database:
temp.db <- dbConnect(RSQLite::SQLite(), ":memory:")

# May need to delete old data:
#dbExecute(temp.db, 'DROP TABLE Dimensions_Records')
dbClearResult()

# Load dataframes into database tables:
dbWriteTable(temp.db, "Experts_data", my_data)
dbWriteTable(temp.db, "Dimensions_Records", Dimensions.results.DF)
dbListTables(temp.db)

# Run a query to JOIN tables where DOI matches:
J <- dbGetQuery(temp.db, 'SELECT * FROM Experts_data AS E INNER JOIN Dimensions_Records AS D ON E.DOI = D.DI')
View(J)


############## PART FOUR ##############
# Output the JOINed result to a database table:

# [3] Create a local table. The fields correspond to those in the data frame:
dbCreateTable(con, "Magic", J)

# [4] Push the data frame into the table:
dbAppendTable(con, "Magic", J)

# [6] Clean up the resultsconnection to avoid problems:
dbClearResult(res)

# [7] Close the database connection to avoid problems:
dbDisconnect(con)


############## PART FOUR - OPTIONS ##############
# Output the JOINed result to a csv file:
write.csv(J, file = "QueryResults_Dimensions.csv", row.names = FALSE) # quote = "TRUE", sep = ",", qmethod = "double")

# ALTERNATIVELY, analyze with bibliometrix package:
M <- convert2df(D, dbsource = "dimensions", format = "api")

# The Bibliometrix package has a standard analysis format: 
results <- biblioAnalysis(M)
summary(results)