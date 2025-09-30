## This code pulls data from the oracle database to produce the tables and 
## figures for the following reports:
## 1. NOAA PIFSC SAFE report Hawaii module report and presentation
## 2. WCPRMC Longline presentaiton to SSC and Council
## 3. ISC Annual report and presentation
## 4. WCPFC SC Annual Report and presenation(?)

## Usage: Source this code at the start of each quarto document,
## I recommend only pulling the data from the Oracle database once each year,
## then saving it to an .Rdata file (which should not be uploaded
## to the github repository) then loading the .RData file each
## time you want to render or preview the quarto document
## This prevents you from needing to be connected to the PIFSC
## network to make changes to the documents

## load your libraries (may need to first install)
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(ggmap)
# library(rnaturalearth)
# library(scales)
library(lubridate)
library(gridExtra) # For table inset
library(grid)
library(scales)
library(gt)
library(sf)
library(odbc)
library(DBI)
library(purrr)

# # Connect to Logbook Database
# 
# --- Connection Details ---
db_driver   <- "Oracle in instantclient_21_3"  # The exact name of your ODBC driver
db_host     <- "picdb.nmfs.local"      # Hostname or IP address
db_port     <- "1521"                        # Default Oracle port
db_service  <- "pic.pifscproddbsn.pifscprodvcn.oraclevcn.com"     # The database service name
db_user     <- "msculley"
db_pass     <- "XXXXX"

# --- Build the Connection String ---
# The .connection_string argument is where you provide all the details.
con_string <- paste0(
  "Driver={", db_driver, "};",
  "Dbq=", db_host, ":", db_port, "/", db_service, ";"
)

# --- Create the Connection ---
con <- dbConnect(
  odbc::odbc(),
  .connection_string = con_string,
  uid = db_user,
  pwd = db_pass
)

# # # # Once run code will see logbook data under connections tab.
# # # # Note you may need to periodically rerun this as you may 
# # get kicked out of database frequently.
# # # # Make sure you are connected to VPN.
# 
# # # # Pull summarized logbook data and clean-up
# # # #| echo: false
# 
# # # ## to avoid needing to be connected to the VPN/memory issues, I recommend pulling this once each year, saving it as an .Rdata file, and then loading it in future renderings
 HI_stats <- dbGetQuery(con, paste("SELECT * FROM LLDS.LLDS_REPORT_STATS_HC_RFMO", " WHERE  CONF !='0'", sep = ""))
 Log_effort <- dbGetQuery(con, paste("SELECT * FROM LLDS.LLDS_HDR_HAC", " WHERE FLEET = 'HI'AND CONF_5x5 != '0'", sep = ""))
# HI_MAP_DATA <- dbGetQuery(ora_con, paste("SELECT * FROM LLDS.LLDS_5x5xYRxSD_HC_NC", " WHERE  CONF !='0'", sep = ""))
# HI_all_gears <- dbGetQuery(ora_con, paste("SELECT * FROM LLDS.HM_HICA_SPECIES", sep = ""))
# HI_Small_boat_effort <- dbGetQuery(ora_con, paste("SELECT * FROM WP_HAWAII.H_NON_LL_EFFORT_V", sep = ""))
# HI_Small_boat_cpue_info <-dbGetQuery(ora_con, paste("SELECT * FROM WP_HAWAII.H_NON_LL_CATCH_DETAILED_V", sep = "")) # nolint # nolint
# AS_Log_effort <- dbGetQuery(ora_con, paste("SELECT * FROM LLDS.LLDS_HDR_HAC"," WHERE FLEET = 'AS'AND CONF_5x5 != '0'", sep = ""))
# AS_stats <- dbGetQuery(ora_con, paste("SELECT * FROM LLDS.LLDS_REPORT_STATS_AS_RFMO"," WHERE CONF !='0'", sep = ""))
# 
# ## Save the data to and .Rdata file so you can access it without connecting to the oracle database
# # Make sure to put the saved data file on the gitignore file so don't load any confidential data to github.
# save(HI_stats, Log_effort, HI_MAP_DATA, HI_all_gears, HI_Small_boat_effort, HI_Small_boat_cpue_info, AS_Log_effort, AS_stats, file="HI_SAFE_Data.Rdata")

## load the previously saved .Rdata file
 #load("HI_SAFE_Data.Rdata") #Don't load .Rdata on to github.
