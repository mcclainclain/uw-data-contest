library(rvest)
library(tidyverse)

sports = c("baseball",
           "softball",
           "mens-basketball",
           "womens-basketball",
           "football",
           "mens-golf",
           "womens-golf",
           "swimming-and-diving",
           "womens-gymnastics",
           "womens-soccer",
           "xctrack",
           "womens-tennis",
           "mens-tennis",
           "womens-volleyball",
           "womens-rowing")

types = c(0,0,1,1,0,1,1,1,0,0,1,1,1,0,1)

sports_df = data.frame(Sport = sports, Type = types)

# Helper functions
hometown_split = function(L) {
  new_list = c()
  for (i in 1:length(L)) {
    new_list = c(new_list, str_trim(str_split_1(L[i], "/")[1]))
  }
  return (new_list)
}

# Scraping
BASE_URL = "https://rolltide.com"
years = 2013:2023
college_df = data.frame()
for (i in 1:nrow(sports_df)) {
  sport = sports_df$Sport[i]
  for (y in 1:length(years)) {
    year = years[y]
    if (sports_df$Type[i] == 1) {
      next_year = as.character(years[y] + 1)
      year_str = paste(years[y], "-", substr(next_year, nchar(next_year) -1, nchar(next_year)), sep="")
    }
    else {
      year_str = as.character(years[y])
    }
    url = paste(BASE_URL, "/sports/", sport, "/roster/", year_str, sep="")
    
    tables = read_html(url) %>% html_table()
    
    if (sport %in% c("swimming-and-diving", "xctrack")){
      roster = data.frame(tables[3])
      roster = rbind(roster, data.frame(tables[4]))
    }
    else{
      roster = data.frame(tables[3])
    }
    
    roster$Year = rep(year, nrow(roster))
    roster$Sport = rep(sport, nrow(roster))
    
    for (i in 1:length(colnames(roster))){
      if (length(grep("[hH]ometown", colnames(roster)[i])) > 0){
        roster$Hometown = hometown_split(roster[[colnames(roster)[i]]])
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

write.csv(college_df, "BAMA.csv", row.names=F)