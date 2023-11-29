library(RSelenium)
library(tidyverse)

driver <- rsDriver(browser = "firefox",
                         chromever = "113.0.5672.63") # alt: chromever = NULL
remote_driver <- driver[["client"]]
remote_driver$open()

remote_driver$navigate("https://www.latlong.net/convert-address-to-lat-long.html")