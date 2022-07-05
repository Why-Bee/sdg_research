# An initial script to query the Dimensions API. Modified to tackle SDG queries.
# June 2022
# Jeff Demaine
# Modified by Yash Bhatia
# Following the instructions at: https://github.com/massimoaria/dimensionsR
# And: https://cran.r-project.org/web/packages/RSQLite/vignettes/RSQLite.html

# If not already done, install RSQLite
# For example: C:\Program Files\R\R-4.1.3\library 
install.packages("RSQLite")
install.packages("dimensionsR")
install.packages("bibliometrix")
install.packages("tidyverse")

library(DBI)
library(RSQLite)
library(tidyverse)
library(dimensionsR)
library(bibliometrix)

# IMPORTANT: Run dsApi2dfRev.R and make sure all functions in the file are declared in memory before running this code!

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
token <- dsAuth(key = "BC7F5884A2D445EAA189A8A2DBF3E1FB")

# [4] Define query for Dimensions:
query.string <- "search publications where (year in [ 2018 : 2022 ]) and research_orgs.id = \"grid.25073.33\" and (category_sdg.name = \"1 No Poverty\") return publications[basics + extras + categories + concepts]"

# [5] Send the query string and the token to the Dimensions API. Return zero records:
res <- dsApiRequest(token = token, query = query.string, limit = 1, verbose = TRUE)

# [5.1] Just how many records am I going to download?
res$total_count

# [5.2] Define a list to store all counts
countslist <- list(res$total_count)

# [6] Send the query again and retrieve 250 records at a time:
Dimensions.results <- dsApiRequest(token = token, query = query.string, step = 250, limit = res$total_count)

# [7] Convert the Dimensions XML list into an R data frame. This dataframe will function as the main data frame for all SDG outputs, appended consecutively.
Dimensions.results.DF <- dsApi2dfRev(P = Dimensions.results)
#dsApi2df(Dimensions.results)

# [8] Append this dataframe to the list of dataframes (will contain 16 data frames by the end)
frameslist = list(dsApi2dfRev(Dimensions.results))

# [9] Run a loop to go across all Sustainable Development Goals, from SDG 2 to SDG 16.

for (x in 2:16) 
{
  curr_sdg <- 40000 + x #Internally, the category ID for SDGs is 40000 plus the SDG number, going from 1 to 17.

  # [9.1] Assemble a query, this time with the SDG ID defined above
  query.string <- paste("search publications where (year in [2018:2022]) and (research_orgs.id = \"grid.25073.33\") and (category_sdg.id = ",toString(curr_sdg),") return publications[basics + extras + categories + concepts]", sep = '')

  # [9.2] Run a query returning 0 results to understand the expected limit of the actual query (how many results)
  res <- dsApiRequest(token = token, query = query.string, limit = 0, verbose = TRUE)

  # [9.3] Send the query again, retrieving 250 records at a time.
  Dimensions.results <- dsApiRequest(token = token, query = query.string, step = 250, limit = res$total_count)

  # [9.4] Store the current SDG Dimensions data in a temporary data frame
  M <- dsApi2dfRev(P = Dimensions.results) 

  # [9.5] Append this data to the list of dataframes, and the master data frame.
  Dimensions.results.DF <- rbind(Dimensions.results.DF, M)
  frameslist <- c(frameslist, M) 
  countslist <- append(counts_list, res$total_count)
 } #End of loop


############## PART THREE ##############
# [10] JOIN the two data frames using RSQLite:

# [10.1] Create temporary database:
temp.db <- dbConnect(RSQLite::SQLite(), ":memory:")

# May need to delete old data:
#dbExecute(temp.db, 'DROP TABLE Dimensions_Records')

# [10.2] Load dataframes into database tables:
dbWriteTable(temp.db, "Experts_data", my_data)
dbWriteTable(temp.db, "Dimensions_Records", Dimensions.results.DF)
dbListTables(temp.db)

# [10.3] Run a query to JOIN tables where DOI matches:
J <- dbGetQuery(temp.db, 'SELECT * FROM Experts_data AS E INNER JOIN Dimensions_Records AS D ON E.DOI = D.DI')
View(J)


############## PART FOUR ##############
# [11] Output the JOINed result to a database table:

# [11.1] Create a local table. The fields correspond to those in the data frame:
dbCreateTable(con, "Magic", J)

# [11.2] Push the data frame into the table:
dbAppendTable(con, "Magic", J)

# [11.3] Close the database connection to avoid problems:
dbDisconnect(con)


############## PART FOUR - OPTIONS ##############
# [12] Output the JOINed result to a csv file:
write.csv(J, file = "QueryResults_Dimensions_Merged.csv", row.names = FALSE) # quote = "TRUE", sep = ",", qmethod = "double")

# [13] ALTERNATIVELY, analyze with bibliometrix package:
M <- convert2df(D, dbsource = "dimensions", format = "api")

# [13.1] The Bibliometrix package has a standard analysis format: 
results <- biblioAnalysis(M)
summary(results)