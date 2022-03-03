# Use the ProffdkR function group to gather data on Danish companies in various
# These are companies registered in Denmark != Danish Companies necessarily
# Industries related to Biotechnology and Life Science
setwd("C:/Users/dal/Biosyntia/Admin - David Lennox-Hvenekilde/Bioinformatics and data science/Courses/Data Science in R/Developing data products/Final Course Project - Shiny")
source("ProffdkR.R")
library(stringr)
# We will look at the pages for Biotech, Technical Consultants, Chemicals Industry and Med Tech.


# Biotechnology
BiotekPages <- Industry_overview_pages("https://www.proff.dk/s%C3%B8g-p%C3%A5-s%C3%B8geord/bioteknologi/I:2669/?q=Bioteknologi",Industry = "Bioteknologi")
# Technical Consultants - TAKES A LONG TIME TO RUN
# TCPages <- Industry_overview_pages("https://www.proff.dk/s%C3%B8g-p%C3%A5-s%C3%B8geord/tekniske-konsulenter/I:2190/?q=Tekniske%20konsulenter", Industry = "Tekniske konsulenter")
# Chemicals and Chemical products
ChemPages <- Industry_overview_pages("https://www.proff.dk/s%C3%B8g-p%C3%A5-s%C3%B8geord/kemikalier-og-kemiske-varer/I:926/?q=Kemikalier%20og%20kemiske%20varer", Industry = "Kemikalier og kemiske varer")
# Med Tech
MedTech <- Industry_overview_pages("https://www.proff.dk/s%C3%B8g-p%C3%A5-s%C3%B8geord/medicinsk-teknisk-udstyr/I:1350/?q=Medicinsk-teknisk%20udstyr", Industry = "Medicinsk-teknisk udstyr")

# Technical consultants pages
# use the Proff_company_URL() to get specific company webpages of the Industry search pages
TCcompanyPages <- c()
for (i in 1:length(TCPages)){
      TCcompanyPages <- c(TCcompanyPages, Proff_company_URL(TCPages[i]))
}
# Biotek pages
# use the Proff_company_URL() to get specific company webpages of the Industry search pages
BiotekCompanyPages <- c()
for (i in 1:length(BiotekPages)){
      BiotekCompanyPages <- c(BiotekCompanyPages, Proff_company_URL(BiotekPages[i]))
}
# Chemicals and Chemical products pages
# use the Proff_company_URL() to get specific company webpages of the Industry search pages
ChemCompanyPages <- c()
for (i in 1:length(ChemPages)){
      ChemCompanyPages <- c(ChemCompanyPages, Proff_company_URL(ChemPages[i]))
}
# Med Tech pages
# use the Proff_company_URL() to get specific company webpages of the Industry search pages
MedTechPages <- c()
for (i in 1:length(ChemPages)){
      MedTechPages <- c(MedTechPages, Proff_company_URL(MedTech[i]))
}

# There are a lot of Technical consultant companies that have nothing to do with the 
# Industries in which we are interested. We will only look at those from certain industries
TCdf <- data.frame(URL = TCcompanyPages, Industry = "Technical Consultants")
TCdfbio <- TCdf[str_detect(TCdf$URL, "bio"),]
TCdfpharma <- TCdf[str_detect(TCdf$URL, "pharma"),]
TCdfchem <- TCdf[str_detect(TCdf$URL, "chem"),]
TCdfmedic <- TCdf[str_detect(TCdf$URL, "medic"),]
TCdfNew <- rbind(TCdfbio,TCdfpharma,TCdfchem,TCdfmedic)

# Now lets make a dataframe for all the companies
BiotekDF <- data.frame(URL = BiotekCompanyPages, Industry = "Biotechnology")
ChemDF <- data.frame(URL = ChemCompanyPages, Industry = "Chemical Technologies")
MedTechDF <- data.frame(URL = MedTechPages, Industry = "Medical Technologies")

AllCompanyDF <- rbind(TCdfNew, BiotekDF, ChemDF, MedTechDF)
write.csv(AllCompanyDF, "CompanyURLandIndustries.csv")

# Now we have the URLs and Industries raw data. 
# We want to populate the DF with basic company metrics using the
# Proff_company_BasicMetrics() webscrape function

AllCompanyDF$Company <- NA
AllCompanyDF$YearEstablished <- NA
AllCompanyDF$Employees <- NA
AllCompanyDF$Revenue_DKK <- NA
AllCompanyDF$EBITA_DKK <- NA
AllCompanyDF$Address <- NA

# Using the aforementioned function we loop through each company website and
# populate the columns, "Company", "YearEstablished", "Employees", "Revenue_DKK"
# "EBITA_DKK"

for (i in 1:nrow(AllCompanyDF)){
      CompanyInfoTemporary <- Proff_company_BasicMetrics(AllCompanyDF$URL[i])
      AllCompanyDF$Company[i] <- CompanyInfoTemporary$Company
      AllCompanyDF$YearEstablished[i] <- CompanyInfoTemporary$YearEstablished
      AllCompanyDF$Employees[i] <- CompanyInfoTemporary$Employees
      AllCompanyDF$Revenue_DKK[i] <- CompanyInfoTemporary$Revenue_DKK
      AllCompanyDF$EBITA_DKK[i] <- CompanyInfoTemporary$EBITA_DKK
      AllCompanyDF$Address[i] <- CompanyInfoTemporary$Address
}
# Save the raw data file
write.csv(AllCompanyDF, "CompanyMetricsRaw.csv")
AllCompanyDF <- read.csv("CompanyMetricsRaw.csv")
# There seems to be some duplicate entries, which should be removed
length(AllCompanyDF$Company)
length(unique(AllCompanyDF$Company))
# Before we do further analysis, lets clean the data a bit. First lets remove
# companies with no registered employees. These are generally either holding
# companies, have no activities/closing down, or are single person companies
# many which are inactive.
AllCompanyDF <- subset(AllCompanyDF, AllCompanyDF$Employees > 0)
# We have now reduced the data from ~2500 companies to ~1000

# Now lets remove duplicate companies, ones registered multiple times but
# have the same revenue/ebita. These are likely companies with multiple locations
# First need to deal with special characters, didn't find a good way to do this
# automatically, so just brute forced it. Not very elegant, but whatever.
AllCompanyDF$Company <- gsub("&amp;", "&", AllCompanyDF$Company)
AllCompanyDF$CompanyNoSpecial <- gsub("ø", "oe", AllCompanyDF$Company)
AllCompanyDF$CompanyNoSpecial <- gsub("Ø", "oe", AllCompanyDF$CompanyNoSpecial)
AllCompanyDF$CompanyNoSpecial <- gsub("æ", "ae", AllCompanyDF$CompanyNoSpecial)
AllCompanyDF$CompanyNoSpecial <- gsub("Æ", "ae", AllCompanyDF$CompanyNoSpecial)
AllCompanyDF$CompanyNoSpecial <- gsub("å", "aa", AllCompanyDF$CompanyNoSpecial)
AllCompanyDF$CompanyNoSpecial <- gsub("Å", "aa", AllCompanyDF$CompanyNoSpecial)
AllCompanyDF$CompanyNoSpecial <- gsub("Å", "aa", AllCompanyDF$CompanyNoSpecial)
AllCompanyDF$CompanyNoSpecial <- gsub("ü", "u", AllCompanyDF$CompanyNoSpecial)
AllCompanyDF$CompanyNoSpecial <- gsub("Ü", "u", AllCompanyDF$CompanyNoSpecial)
AllCompanyDF$CompanyNoSpecial <- gsub("ó", "o", AllCompanyDF$CompanyNoSpecial)
AllCompanyDF$CompanyNoSpecial <- gsub("Ó", "o", AllCompanyDF$CompanyNoSpecial)
AllCompanyDF$CompanyNoSpecial <- gsub(".", "", AllCompanyDF$CompanyNoSpecial, fixed = TRUE)

# Make all lowercase for easy matching
AllCompanyDF$CompanyClean <- stri_trans_tolower(word(AllCompanyDF$CompanyNoSpecial, 1,2))
# For those with only one word in the company name
AllCompanyDF[is.na(AllCompanyDF$CompanyClean),]$CompanyClean <- 
      stri_trans_tolower(AllCompanyDF[is.na(AllCompanyDF$CompanyClean),]$CompanyNoSpecial, locale="en_CA")

# Check for duplicate cleaned company names
AllCompanyDF$CompanyDup <- duplicated(AllCompanyDF$CompanyClean)

# Made a new data frame with the only the companies that appear once
AllCompanyDFnoDUP <- AllCompanyDF[!(AllCompanyDF$CompanyClean %in% unique(subset(AllCompanyDF, AllCompanyDF$CompanyDup == TRUE)$CompanyClean)),]

# loop through the companies unique duplicated companies
for (company in unique(subset(AllCompanyDF, AllCompanyDF$CompanyDup == TRUE)$CompanyClean)){
      TempDupCompany <- AllCompanyDF[AllCompanyDF$CompanyClean == company,]
      # If there are multiple industries for the same company.
      DupIndustry <- ""
      for (industry in unique(TempDupCompany$Industry)){
            DupIndustry <- paste(DupIndustry, industry, sep=", ")
      }
      # Remove the first ", "
      DupIndustry <- substr(DupIndustry,3,nchar(DupIndustry))
      
      # Choose only the company duplicate with highest emplyee number
      TempDupCompany <- TempDupCompany[which.max(TempDupCompany$Employees),]
      # If there are several with the same highest employee number. Then choose by year established
      if (nrow(TempDupCompany)>1){
            TempDupCompany <- TempDupCompany[which.min(TempDupCompany$YearEstablished),]
            
            # If there are still more than one row in the DF then just choose one at random
            if (nrow(TempDupCompany)>1){
                  TempDupCompany <- TempDupCompany[1,]
            }
      }
      # Bind the result to the df with non-duplicated entries
      TempDupCompany$Industry <- DupIndustry
      AllCompanyDFnoDUP <- rbind(AllCompanyDFnoDUP, TempDupCompany)
}

#######################################################################################

# For global positions
###########################
# We loop though the adresses
library(tmaptools)
for (adr in 1:nrow(AllCompanyDFnoDUP)){
      # For ever row we use the address as input for the geocode_OSM() function
      # This search https://www.openstreetmap.org/
      LatLong <- geocode_OSM(AllCompanyDFnoDUP$Address[adr], keep.unfound = TRUE)$coords
      # Add the llongitude and latitude to the dataframe
      AllCompanyDFnoDUP$Lat[adr] <- LatLong[1]
      AllCompanyDFnoDUP$Long[adr] <- LatLong[2]
      
      # If the data is not found in the first call, we try again, without the
      # City and postnumber, as this seems to be the issue for the search
      if (is.na(AllCompanyDFnoDUP$Lat[adr])){
            # This removes everything after the first nume ber in the string
            # I.e."Ole MaalC8es Vej 3, 2200 KC8benhavn N" becomes "Ole MaalC8es Vej 3"
            testad <- AllCompanyDFnoDUP$Address[adr]
            testadnum <- as.character(parse_number(testad))
            temp_adr <- paste(sub(paste(" ", testadnum, ".*", sep=""), "", testad), testadnum)
            # Try to search with the reduced address
            LatLong <- geocode_OSM(temp_adr,keep.unfound = TRUE)$coords
            AllCompanyDFnoDUP$Lat[adr] <- LatLong[1]
            AllCompanyDFnoDUP$Long[adr] <- LatLong[2]
            
      }
}

# Lets save the final dataframe:
# First remove the temporary columns

row.names(AllCompanyDFnoDUP) <- NULL

write.csv(AllCompanyDFnoDUP[,-c(1:2)], "CompanyMetricsClean.csv")
