library(rvest)
library(tidyverse)

sports = c("baseball"=0,
           "softball"=0,
           "mbball"=1,
           "wbball"=1,
           "wcross"=0,
           "football"=0,
           "mgolf"=1,
           "wgolf"=1,
           "swimming"=1,
           "msoc"=0,
           "wsoc"=0,
           "track"=1,
           "mten"=1,
           "wten"=1,
           "wvball"=0
)
types = c()
for (thing in sports) {
  types = c(types, thing)
}


sports_df = data.frame(Sport = names(sports), Type = types)

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

# Scraping
BASE_URL = "https://gamecocksonline.com"
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
    
    if (year > 2020 && sport == "track") {
      year_str = as.character(years[y])
    }
    
    url = paste(BASE_URL, "/sports/", sport, "/roster/season/", year_str, sep="")
    
  
    
    tables = read_html(url) %>% html_table()
    roster = data.frame(tables[1])
    
    if (sport %in% c("swimming", "track") && nrow(roster) > 0) {
      if (length(tables))
      roster = rbind(roster, data.frame(tables[2]))
    }
    roster$Year = rep(year, nrow(roster))
    roster$Sport = rep(sport, nrow(roster))
    
    
    for (j in 1:length(colnames(roster))){
      if (length(grep("[hH]ometown", colnames(roster)[j])) > 0){
        roster$Hometown = hometown_split(roster[[colnames(roster)[j]]])
      }
      
      if (length(grep("[nN]ame", colnames(roster)[j])) > 0){
        roster$Name = roster[[colnames(roster)[j]]]
      }
    }
    
    if (!("Hometown" %in% colnames(roster))) {
      print(url)
      next
    }
    roster = roster %>% select(Name, Hometown, Year, Sport)
    college_df = rbind(college_df, roster)
  }
  print(paste("Finished with ", sport))
}

write.csv(college_df, "USC.csv", row.names=F)