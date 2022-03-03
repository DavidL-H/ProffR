# Functions for gathering data from Proff.DK html
# Proff has an API, but cost money to use. Therefore these functions have been 
# Developed for the Coursera Course (Developing Data Products)
# I may turn this into a package if I feel the need later
# 220301 David Lennox-Hvenekilde

# Dependencies:
library(stringr)
library(readr)
library(strex)

### NOT EXPORTED FUNCTION, INTERNALLY USED ONLY ###
Next_Page_ProffDK <- function(Search_Page_link, Industry){
      # Take proff.dk industry search page url link as input
      # Need to specifiy the industry searched for
      # For example: 
      # PageLink = "https://www.proff.dk/s%C3%B8g-p%C3%A5-s%C3%B8geord/bioteknologi/I:2669/?q=Bioteknologi"
      # Industry = "Bioteknologi"
      # The industry searched is clear in the link
      
      url <- Search_Page_link
      # Read the HTML of page as a string
      html <- paste(readLines(url), collapse="\n")
      # Identify all links in the string and save in a dataframe
      AllLinks <- str_match_all(html, "<a href=\"(.*?)\"")
      AllLinksDF <- data.frame(AllLinks)$X2
      # Find the links that contain the Industry queried
      # This needs to be cleaned a bit as well
      NextPageLink <- AllLinksDF[str_detect(AllLinksDF, Industry)]
      NextPageLink <- NextPageLink[length(NextPageLink)]
      NextPageLink <- substring(NextPageLink, 2)
      ProffDK <- "https://www.proff.dk/"
      NextPageURL <- paste(ProffDK,NextPageLink, sep = "")
      # The function returns the URL pointing to the next page
      return(NextPageURL)
}

### EXPORTED, GIVE PROPER DOCUMENTATION ###
Industry_overview_pages <- function(Search_Page_link, Industry){
      url <- Search_Page_link
      # Read the HTML of page as a string
      html <- paste(readLines(url), collapse="\n") 
      
      # Find the number of companies in the search from the html
      No_companies <- str_match_all(gsub("[\r\n]", "", html), "Branche:</span>\\s*(.*?)\\s*resultater")
      No_companies <- No_companies[[1]][1]
      No_companies <- as.numeric(regmatches(No_companies, gregexpr("[[:digit:]]+", No_companies))[[1]][2])
      
      QueryPage1 <- Search_Page_link
      Pages <- Search_Page_link
      for (i in 1:(round(No_companies/25))){
            
            QueryPage1 <- Next_Page_ProffDK(
                  Next_Page_ProffDK(QueryPage1, Industry),Industry
            )
            #Store all the page URLs in one vector
            Pages <- c(Pages,QueryPage1)
      }
      
      # For some reason this only return every other page, we need to save this
      Pages1 <- unique(Pages)
      
      # Run it again with the second page in search manually entered:
      QueryPage2 <- Next_Page_ProffDK(Search_Page_link, Industry)
      Pages2 <- QueryPage2
      for (i in 1:(round(No_companies/25))){
            # Adding rest time
            # sleeptime(2)
            # Nested function to get the next page
            QueryPage2 <- Next_Page_ProffDK(
                  Next_Page_ProffDK(QueryPage2, Industry),Industry
            )
            # Store all the page URLs in one vector
            Pages2 <- c(Pages2,QueryPage2)
      }
      Pages2 <- unique(Pages2)
      
      # Combine the pages and write a file with them:
      PagesFinal <- c(Pages1, Pages2)
      return(PagesFinal)

}

# Example of Industry_overview_pages
#ProffDKBiotekHTML <- Industry_overview_pages("https://www.proff.dk/s%C3%B8g-p%C3%A5-s%C3%B8geord/bioteknologi/I:2669/?q=Bioteknologi", 
                                             # The industry is the same as the 'q= ....' in the URL above. Should add this to the function
                                       #Industry = "Bioteknologi")

### EXPORTED FUNCTION, GIVE PROPER DOCUMENTATION ###
# Grabs company names off a page in a search, given the URL
Proff_company_names <- function(search_page_url){
      # Read the html of the url
      html <- paste(readLines(search_page_url), collapse="\n")
      Company_names <- str_match_all(html, "data-name=\\s*(.*?)\\s*data-url=")
      # Convert from list to vector
      Company_namesDF <- data.frame(Company_names)$X2
      # Clean up the name
      return(str_sub(Company_namesDF, 2, -2))
}

# Example
#Proff_company_names("https://www.proff.dk/s%C3%B8g-p%C3%A5-s%C3%B8geord/YLoFmCo_zvNZxJID58xbvLFl_00WkiF1dDlpvs12g8Dq9ML7fkP1mBywa54Z7cJoB95okX8KgB9BXw6lfF3FGii-RGvAhN41dvxuC33fWrA/?q=Bioteknologi")

### EXPORTED FUNCTION, GIVE PROPER DOCUMENTATION ###
Proff_company_addresses <- function(Search_Page_link){
      # Read the html of the url
      html <- paste(readLines(Search_Page_link), collapse="\n")
      Company_Address <- str_match_all(
            # Need to remove line breaks in html string with gsub, or str_match() does work
            gsub("[\r\n]", "", html), 
            "address\\s*(.*?)\\s*</span>")
      
      Company_Address <- data.frame(Company_Address)$X2
      # Remove first entry
      Company_Address <- Company_Address[-1]
      # Get the part of string after "span>" and we have the clean names
      Company_Address <- sub(".*span>", "", Company_Address)
      return(Company_Address)
}
# Example
#Proff_company_addresses("https://www.proff.dk/s%C3%B8g-p%C3%A5-s%C3%B8geord/YLoFmCo_zvNZxJID58xbvLFl_00WkiF1dDlpvs12g8Dq9ML7fkP1mBywa54Z7cJoB95okX8KgB9BXw6lfF3FGii-RGvAhN41dvxuC33fWrA/?q=Bioteknologi")

### EXPORTED FUNCTION, GIVE PROPER DOCUMENTATION ###
# Grab company links off the search page
Proff_company_URL <- function(Search_Page_link, Full_page = TRUE){
      html<-paste(readLines(Search_Page_link), collapse="\n")
      Company_URL <- str_match_all(html, "firma\\s*(.*?)\\s*\" class=\"additional-link")
      Company_URL <- data.frame(Company_URL)$X2
      if (Full_page == TRUE){
            Company_URL <- paste("https://www.proff.dk/firma", Company_URL, sep = "")
      }else{
            Company_URL <- paste("https://www.proff.dk/regnskab", Company_URL, sep = "")
      }
      return(Company_URL)
}
# Example
#Proff_company_URL("https://www.proff.dk/s%C3%B8g-p%C3%A5-s%C3%B8geord/bioteknologi/I:2669/?q=Bioteknologi", Full_page = TRUE)



### TEST FUNCTION ###
# Grab the company employee numbers
Proff_company_employees <- function(Company_Page_link){
      html <- paste(readLines(Company_Page_link), collapse="\n")
      Company_employees <- str_match_all(gsub("[\r\n]", "", html), "employees-info\\s*(.*?)\\s*</em>")
      Company_employees <- as.numeric(gsub(".*?([0-9]+).*", "\\1", Company_employees))
      return(Company_employees)
}
# Example
test_company_page <- "https://www.proff.dk/firma/ferring-pharmaceuticals-as/kastrup/bioteknologi/09PNJ4I0225/"
Proff_company_employees(test_company_page)

### TEST FUNCTION ###
Proff_company_revenue <- function(Company_Page_link){
      html <- paste(readLines(Company_Page_link), collapse="\n")
      Company_revenue <- str_match_all(gsub("[\r\n]", "", html), "i tusindvis af kroner\\s*(.*?)\\s*DKK")
      Company_revenue <- gsub(".", "", Company_revenue, fixed = TRUE)
      Company_revenue <- as.numeric(gsub(".*?([0-9]+).*", "\\1", Company_revenue))
      return(Company_revenue)
      #  in 1000x DKK
}
Proff_company_revenue(test_company_page)

### TEST FUNCTION ###
Proff_company_established <- function(Company_Page_link){
      html <- paste(readLines(Company_Page_link), collapse="\n")
      company_established <- str_match_all(gsub("[\r\n]", "", html), "Etableret\\s*(.*?)\\s*</em>")
      company_established <- gsub(".", "", company_established, fixed = TRUE)
      company_established <- as.numeric(gsub(".*?([0-9]+).*", "\\1", company_established))
      return(company_established)
}
#Proff_company_established(test_company_page)


### EXPORTED FUNCTION, GIVE PROPER DOCUMENTATION ###
# Grab the basic metrics from the Proff company page
Proff_company_BasicMetrics <- function(Company_Page_Link){
      html <- paste(readLines(Company_Page_Link), collapse="\n")
      # Company Name
      Company_names <- str_match_all(html, "data-name=\\s*(.*?)\\s*data-url=")
      Company_names <- Company_names[[1]][2]
      Company_names <- str_sub(Company_names, 2, -2)
      # Revenue
      Company_revenue <- str_match_all(gsub("[\r\n]", "", html), "i tusindvis af kroner\\s*(.*?)\\s*DKK")
      Company_revenue <- gsub(".", "", Company_revenue, fixed = TRUE)
      Company_revenue <- str_extract_numbers(Company_revenue, negs = TRUE)[[1]][1]
      # EBITA (earnings before tax)
      Company_earnings <- str_match_all(gsub("[\r\n]", "", html), "Resultat før skat\\s*(.*?)\\s*DKK")
      Company_earnings <- gsub(".", "", Company_earnings, fixed = TRUE)
      Company_earnings <- str_extract_numbers(Company_earnings, negs = TRUE)[[1]][3]
      # Employees
      Company_employees <- str_match_all(gsub("[\r\n]", "", html), "employees-info\\s*(.*?)\\s*</em>")
      Company_employees <- as.numeric(gsub(".*?([0-9]+).*", "\\1", Company_employees))
      # Year Established
      Company_established <- str_match_all(gsub("[\r\n]", "", html), "Etableret\\s*(.*?)\\s*employees-info")
      Company_established <- gsub(".", "", Company_established, fixed = TRUE)
      Company_established <- str_extract_numbers(Company_established, negs = TRUE)[[1]][3]
      # Adress
      Company_address <- str_match_all(gsub("[\r\n]", "", html), "address-title\\s*(.*?)\\s*<span")
      Company_address <- Company_address[[1]][2]
      Company_address <- str_sub(Company_address, 19)
      # If there are two spaces in string, remove everthing including and after:
      Company_address <- gsub('  [A-z ]*', '' , Company_address)
      # Remove everything after "<"
      Company_address <- gsub("<.*","",Company_address)
      return(
            # We multiply financial numbers by 1000, to get the real values
            # All are in DKK (Danish Crowns)
            data.frame(Company = Company_names,
                       YearEstablished = Company_established, 
                       Employees = Company_employees,
                       Revenue_DKK = Company_revenue*1000,
                       EBITA_DKK = Company_earnings*1000,
                       Address = Company_address
            )
      ) 
}

Proff_company_BasicMetrics("https://www.proff.dk/firma/afyx-therapeutics-as/k%C3%B8benhavn-s/bioteknologi/0LC64BI0225/")



