library(rvest)
library(tidyverse)

sports = c("m-basebl",
           "w-softbl",
           "m-baskbl",
           "w-baskbl",
           "m-footbl",
           "m-golf",
           "w-golf",
           "w-swim",
           "w-gym",
           "w-soccer",
           "m-track",
           "w-track",
           "m-xc",
           "w-xc",
           "w-tennis",
           "m-tennis",
           "w-volley")


sports_df = data.frame(Sport = sports)

# Helper functions
hometown_split = function(L) {
  new_list = c()
  for (i in 1:length(L)) {

    if (is.na(L[i])) {
      new_list = c (new_list, NA)
      next
    }
    new_list = c(new_list, str_trim(str_split_1(L[i], "/")[1]))
  }
  return (new_list)
}

softball = function(R) {
  new_list = c()
  
  for (i in 1:nrow(R)) {
    
    if (is.na(R$Hometown[i]) || R$Hometown[i] == ""){
      new_list = c(new_list, str_trim(str_split_1(R$High.School.Previous.School[i], "/")[1]))
    } else {
      new_list = c(new_list, str_trim(str_split_1(R$Hometown[i], "/")[1]))
    }
  }
  
  return (new_list)
}

# Scraping
BASE_URL = "https://arkansasrazorbacks.com"
years = 2013:2023
college_df = data.frame()
for (i in 1:nrow(sports_df)) {
  sport = sports_df$Sport[i]
  for (y in 1:length(years)) {
    next_year = years[y] + 1
    query_str = paste(years[y], "-", substr(next_year, nchar(next_year) -1, nchar(next_year)), sep="")
    
    url = paste(BASE_URL, "/sport/", sport, "/roster/?season=", query_str, sep="")
    
    tables = read_html(url) %>% html_table()
    
    if (nrow(data.frame(tables[1])) == 0){
      next
    }
    
    roster = data.frame(tables[1])
    print(url)
    
    roster$Year = rep(years[y], nrow(roster))
    roster$Sport = rep(sport, nrow(roster))
    
    for (i in 1:length(colnames(roster))){
      if (length(grep("[hH]ometown", colnames(roster)[i])) > 0){
        if (years[y] %in% c(2014, 2015, 2016) && sport == "w-softbl") {
          roster$Hometown = softball(roster)
        }
        else{
          roster$Hometown = hometown_split(roster[[colnames(roster)[i]]])
        }
      }
      
      if (length(grep("[nN]ame", colnames(roster)[i])) > 0){
        roster$Name = roster[[colnames(roster)[i]]]
      }
    }
    
    if (!("Hometown" %in% colnames(roster))) {
      next
    }
    roster = roster %>% select(Name, Hometown, Year, Sport)
    college_df = rbind(college_df, roster)
  }
  print(paste("Finished with ", sport))
}

write.csv(college_df, "ARKANSAS.csv", row.names=F)