### INTRO #######################################################
# dsApi2dfRev: A function to convert JSON outputs from the dimensions API into R data frames
# Written by: massimoaria (as part of the dimensionsR package at https://github.com/massimoaria/dimensionsR)
# Modified by: Yash Bhatia
# June 2022

### MAIN FUNCTION ###############################################

dsApi2dfRev <- function (P, format = "bibliometrix") 
{ #P is the JSON output used in the function.
  query <- P$query
  item <- P$item
  P <- P$data
  switch(item, publications = {
    df <- pub2dfRev(P, format)
  }, grants = {
    df <- grants2df(P)
  }, patents = {
    df <- patents2df(P)
  }, clinical_trials = {
    df <- clinicaltrials2df(P)
  }, policy_documents = {
    df <- policydocuments2df(P)
  })
  return(df)
}

### SUPPLEMENTARY FUNCTION ######################################

pub2dfRev <- function (P, format) 
{ 
  n <- length(P) #n = number of publications extracted
  df <- data.frame(AU = rep(NA, n), AF = "NA", TI = "NA", 
                   SO = "NA", SO_LIST = NA, LA = "English", DT = NA, IDE = NA, 
                   AB = "NA", C1 = NA, RP = NA, OI = NA, FU = NA, 
                   CR = NA, ALT = NA, TC = NA, TCR = NA, PU = NA, SN = NA, 
                   PY = NA, VL = NA, IS = NA, DI = NA, 
                   PG = NA, SC = NA, OA = NA, URL = NA, DB = "DIMENSIONS", 
                   AU_UN = NA, AU1_UN = NA, AU_CO = NA, AU1_CO = NA, SDG = NA, SDG_ID = NA, RCR = NA, 
                   stringsAsFactors = FALSE)
  
  # df is an empty data frame, with columns initialised. 
  # Glossary of all fields:
  # AU: Author's last name and first name inital. This is the standard naming scheme used by Bibliometrix and other research oriented packages
  # AF: Author full names
  # TI: Title
  # SO: Publication source
  # SO_LIST: Publication source, if found from a list of journals
  # LA: Language
  # DT: Data Type (article, chapter, preprint)
  # IDE: Ideas/Concepts of the paper
  # AB: Abstract
  # C1: Affiliation and country of all authors
  # RP: Affiliation of only the first corresponding author
  # OI: ORCID IDs of all authors
  # FU: Funders
  # CR: Internal Reference IDs
  # ALT: Altmetrics data
  # TC: Times cited
  # TCR: Recent Citations
  # PU: Publisher
  # SN: ISSN of publcation
  # PY: Publishing Year
  # VL: Volume
  # IS: Issue
  # DI: DOI link
  # PG: Pages
  # SC: Field of Research classification
  # OA: Open Access status
  # URL: Link
  # DB: Database
  # AU_UN: Affiliations of authors
  # AU1_UN: Affiliation of first corresponding author
  # AU_CO: Country of authors
  # AU1_CO: Country of first corresponding author
  # SDG: Sustainable Development Goal category
  # SDG_ID: ID of SDG category
  # RCR: Relative Citation Ratio
  
  # For more information on the above categories please visit the Dimensions API docs at https://docs.dimensions.ai/
  
  pb <- utils::txtProgressBar(min = 1, max = n, initial = 1, 
                              char = "=")
  
  for (i in 1:n) {
    utils::setTxtProgressBar(pb, i)
    if (P[[i]]$type %in% c("article", "chapter", "preprint")) {
      a <- list2char(P[[i]]) #P[i] is a single article, and all associated metadata. list2char converts this to a parseable format and stores it in 'a'
      items <- names(a) #This stores all the keys of the key-value pairs in the current publication's JSON data.
      #Matching some easily extracted tags:
      df$DT[i] <- a["type"]
      df$AB[i] <- a["abstract"]
      df$TI[i] <- a["title"]
      df$PY[i] <- a["year"]
      #Extracting all list indices of the authors' last and first names
      AU_last_ind <- which(items == "authors.last_name")
      AU_first_ind <- which(items == "authors.first_name")
      #Joining them to form the name array and pasting into the data frame
      name <- paste(a[AU_last_ind], a[AU_first_ind], sep = ", ")
      df$AF[i] <- paste(name, collapse = ";")
      #Extracting all countries and names of affiliated institutions of all authors 
      CO_ind <- which(items == "authors.affiliations.country")
      country <- a[CO_ind]
      Aff_name_ind <- which(items == "authors.affiliations.name")
      Affiliations <- a[Aff_name_ind]
      #Joining the above and pasting in C1, while also entering them individually in AU_CO and AU_UN
      df$C1[i] <- paste(Affiliations, country, sep = ", ", 
                        collapse = ";")
      df$AU_CO[i] <- paste(country, collapse = ";")
      df$AU_UN[i] <- paste(Affiliations, collapse = ";")
      #Acquiring indices of all names flagged as corresponding authors
      AU_corr <- which(items == "authors.corresponding")
      #Storing those authors where the author is corresponding in, and taking the first author as 'j'
      j <- which(a[AU_corr] == "TRUE")[1]
      if (is.na(j)) 
        j <- 1
      #Pasting affiliations and country of the first corresponding author in RP, and the individual data in AU1_CO and AU1_UN
      df$RP[i] <- paste(Affiliations[j], country[j], sep = ",", 
                        collapse = ";")
      df$AU1_UN[i] <- Affiliations[j]
      df$AU1_CO[i] <- country[j]
      #Acquiring all indices of Field of Research categories mentioned for the current publication
      SC_ind <- which(items == "category_for.name")
      #Removing all occurences of digits using a regex and gsub, and then trimming all leading/trailing whitespaces.Then, pasting the data into SC.
      df$SC[i] <- trimws(gsub("[[:digit:]]+", "", paste(a[SC_ind], 
                                                        collapse = ";")))
      #Extracting all indices of the term 'concepts' from the list of names, and checking which of those indices are greater than -1 (i.e. they exist).
      ID_ind <- which(regexpr("concepts", items) > -1)
      #Extracting the concepts using a[ID_ind] and pasting them in both DE and ID fields.
      df$IDE[i] <- paste(a[ID_ind], collapse = ";")
      #Acquiring sources, whether they be journals or books and pasting the items into SO field
      SO_ind <- which(items %in% c("journal.title", "book_title"))
      df$SO[i] <- a[SO_ind[1]]
      #Simple DOI query
      df$DI[i] <- a["doi"]
      #Matching indices where the regex 'journal_lists' is true, that is if the article has a journal_lists field
      SO_list_ind <- which(regexpr("journal_lists", items) > 
                             -1)
      #Pasting the above data into SO_LIst field
      df$SO_LIST[i] <- paste(a[SO_list_ind], collapse = ";")
      #Some more simple queries for easily accessed data
      df$URL[i] <- a["linkout"]
      df$TC[i] <- a["times_cited"]
      df$ALT[i] <- a["altmetric"]
      df$TCR[i] <- a["recent_citations"]
      df$SDG[i] <- a["category_sdg.name"]
      df$SDG_ID[i] <- a['category_sdg.id']
      df$RCR[i] <- a["relative_citation_ratio"]
      df$PU[i] <- a["publisher"]
      df$VL[i] <- a["volume"]
      df$IS[i] <- a["issue"]
      df$OA[i] <- a["open_access"]
      df$SN[i] <- a["issn"]
      df$PG[i] <- a["pages"]
      #Using a regex to find reference_ids if present in the data, then pasting the same into field CR
      CR_ind <- which(regexpr("reference_ids", items) > 
                        -1)
      df$CR[i] <- paste(a[CR_ind], collapse = ";")
      #Using Regex to find indices of funder names, acronyms, city names, and country names (if present)
      FU_name_ind <- which(regexpr("funders.name", items) > 
                             -1)
      FU_acronym_ind <- which(regexpr("funders.acronym", 
                                      items) > -1)
      FU_city <- which(regexpr("funders.city_name", items) > 
                         -1)
      FU_country <- which(regexpr("funders.country_name", 
                                  items) > -1)
      #Pasting all the funder data into field FU
      df$FU[i] <- paste(a[FU_name_ind], a[FU_acronym_ind], 
                        a[FU_city], a[FU_country], sep = ",", collapse = ";")
      #Finding indices of researchers with ORCID IDs, extracting them, and pasting them into the field OI
      OI_orcid_ind <- which(items == "researchers.orcid_id")
      df$OI[i] <- paste(a[OI_orcid_ind], collapse = ";")
    }
  }
  
  # If the supplied format to pub2dfRev is bibliometrix, the data must be revised to make sure the bibliometrix library can understand it.
  if (format == "bibliometrix") {
    DI <- df$DI
    URL <- df$URL
    df <- data.frame(lapply(df, toupper), stringsAsFactors = FALSE)
    df$DI <- DI
    df$URL <- URL
  }
  #Some datatype conversions to make sure the values can be analyzed and compared
  df$PY <- as.numeric(df$PY)
  df$TCR <- as.numeric(df$TCR)
  df$TCR[is.na(df$TCR)] <- 0
  df$TC <- as.numeric(df$TC)
  df$TC[is.na(df$TC)] <- 0
  df = df[!is.na(df$DT), ]
  
  #Author Sections: This code block takes the author field AU as the full name, then modifies it to produce initialisms. Eg: Doe, John becomes Doe, J
  
  df$AU <- df$AF
  df$AU <- gsub("\\s+", " ", df$AU)
  df$AU <- trimws(gsub("\\(|\\)", "", df$AU))
  listAU <- strsplit(df$AU, ";")
  AU <- lapply(listAU, function(l) {
    lastname <- trimws(gsub(",.*", "", l))
    firstname <- strsplit(trimws(gsub(".*,", "", l)), " ")
    i <- which(nchar(lastname) < 2)
    if (length(i) > 0) {
      lastname <- lastname[-i]
      firstname <- firstname[-i]
    }
    firstname <- lapply(firstname, function(x) {
      if (length(x) > 0) {
        x <- paste(substr(x, 1, 1), sep = "", collapse = "")
      }
      else {
        x = ""
      }
      return(x)
    })
    AU <- paste(lastname, unlist(firstname), sep = " ", 
                collapse = ";")
    return(AU)
  })
  #Cleaning up the AU field
  df$AU <- unlist(AU)
  df$AU[df$AU == "NA N"] <- NA
  #Progress bar closed and output data frame returned
  close(pb)
  return(df)
}

### LIST2CHAR #####################################################

# This function has been copied from the rbsb utilities package. It is used in pub2dfRev.
list2char <- function (x, use.names = TRUE, classes = "ANY") 
{
  lung <- sum(rapply(x, function(x) 1L, classes = classes))
  Ch <- vector("list", lung)
  i <- 0L
  items <- rapply(x, function(x) {
    i <<- i + 1L
    Ch[[i]] <<- x
    TRUE
  }, classes = classes)
  if (use.names && !is.null(nm <- names(items))) 
    names(Ch) <- nm
  Ch <- unlist(Ch)
  return(Ch)
}

