

# Combining three datasets:
  # 1. "ED Status Jan 1 2020 - Dec 20 2021"
      # DOES NOT INCLUDE NCR Hospitals after Dec 15
  # 2. "NCR ED Status Dec 15 2021 Ongoing"
      # Includes ONLY NCR Hospitals
  # 3. "ED Status Dec 15 2021 Ongoing"
      # Includes ONLY NON-NCR Hospitals



# External pre-work steps (done externally):
  # 1. Download ED Status from EMResource
      # Report > Status Report > Status Detail
        # a. Dec 15 2021 through today
        # b .CSV file
        # c. All Statuses
        # d. Resource Group: "All * without FSED"
    # 1.1  Erase what was there before and paste results into "NON-NCR ED Status Dec 15 2021 Ongoing"; start copy/paste at Resource, etc. row, not Status and stop copying at the Summary information further down the spreadsheet
  # 2. Download NCR - ED Status from EMResource
      # Report > Status Report > Status Detail
        # a. Dec 15 2021 through today
        # b .CSV file
        # c. All Statuses
        # d. Resource Group: "All * without FSED"
    # 2.1 Erase what was there before and paste results into "NCR ED Status Dec 15 2021 Ongoing"; start copy/paste at Resource, etc. row, not Status and stop copying at the Summary information further down the spreadsheet


# Ongoing steps (done in R):
  # 1. Pull "EMResource Status Detail Report - All hospitals ED Status - Jan 1 2020 Ongoing"
  # 2. Add "NCR ED Status Dec 15 2021 Ongoing" to the bottom
   # 2.1 Assign License #s, referencing dbo.Facilities table
  # 3. Add "ED Status Dec 15 2021 Ongoing" to the bottom
   # 3.1 Assign License #s, referencing dbo.Facilities table
   # 3.2 Remove any duplicate start/end dates between Dec 15 and Dec 20 2021
  # 4. Check for and remove duplicate rows
  # 5. Save to "EMResource Status Detail Report - All hospitals ED Status - Jan 1 2020 Ongoing"







library(readxl)

# read in file for "EMResource Status Detail Report - All hospitals ED Status - Jan 1 2020 Ongoing"
EDStatus_Jan12020_Ongoing <- read.csv("H:/Documents/OEPR Data Requests/EMResource Status Detail Report - All hospitals ED Status - Jan 1 2020 Ongoing.csv")

# converting to datetime
  EDStatus_Jan12020_Ongoing$Start.Date <- parse_date_time(EDStatus_Jan12020_Ongoing$Start.Date, c("mdY!_HM", "Y!md_HMS", "dmY!_HM", "db!Y!_HM"), tz = "MST")

  EDStatus_Jan12020_Ongoing$End.Date <- parse_date_time(EDStatus_Jan12020_Ongoing$End.Date, c("mdY!_HM", "Y!md_HMS", "dmY!_HM", "db!Y!_HM"), tz = "MST")

  
# read in file for "NCR ED Status Dec 15 2021 Ongoing"
NCR_EDStatus_Dec152021Ongoing <- read_excel("H:/Documents/OEPR Data Requests/EMResource Status Detail Report - NCR ED Status - Dec 15 2021 Ongoing.xlsx")
  # match RegulatoryID
    NCR_EDStatus_Dec152021Ongoing$RegulatoryID <- EDStatus_Jan12020_Ongoing$RegulatoryID[match(NCR_EDStatus_Dec152021Ongoing$Resource, EDStatus_Jan12020_Ongoing$Resource)]
    # rename Start and End Dates
    NCR_EDStatus_Dec152021Ongoing <- rename(NCR_EDStatus_Dec152021Ongoing, Start.Date = "Start Date")
    NCR_EDStatus_Dec152021Ongoing <- rename(NCR_EDStatus_Dec152021Ongoing, End.Date = "End Date")
    
    glimpse(NCR_EDStatus_Dec152021Ongoing)
    
    # converting to datetime
    NCR_EDStatus_Dec152021Ongoing$Start.Date <- parse_date_time(NCR_EDStatus_Dec152021Ongoing$Start.Date, c("mdY!_HM", "Y!md_HMS", "dmY!_HM", "db!Y!_HM"), tz = "MST")
    
    NCR_EDStatus_Dec152021Ongoing$End.Date <- parse_date_time(NCR_EDStatus_Dec152021Ongoing$End.Date, c("mdY!_HM", "Y!md_HMS", "dmY!_HM", "db!Y!_HM"), tz = "MST")

    
# bind dfs together
All_EDStatus_Jan12020Ongoing <- bind_rows(EDStatus_Jan12020_Ongoing, NCR_EDStatus_Dec152021Ongoing)

# read in file for "ED Status Dec 15 2021 Ongoing"
NONNCR_EDStatus_Dec152021Ongoing <- read_excel("H:/Documents/OEPR Data Requests/EMResource Status Detail Report - NON-NCR ED Status - Dec 15 2021 Ongoing.xlsx")
  # match RegulatoryID
    NONNCR_EDStatus_Dec152021Ongoing$RegulatoryID <- EDStatus_Jan12020_Ongoing$RegulatoryID[match(NONNCR_EDStatus_Dec152021Ongoing$Resource, EDStatus_Jan12020_Ongoing$Resource)]
    # rename Start and End Dates
    NONNCR_EDStatus_Dec152021Ongoing <- rename(NONNCR_EDStatus_Dec152021Ongoing, Start.Date = "Start Date")
    NONNCR_EDStatus_Dec152021Ongoing <- rename(NONNCR_EDStatus_Dec152021Ongoing, End.Date = "End Date")
    # converting to datetime
    NONNCR_EDStatus_Dec152021Ongoing$Start.Date <- parse_date_time(NONNCR_EDStatus_Dec152021Ongoing$Start.Date, c("mdY!_HM", "Y!md_HMS", "dmY!_HM", "db!Y!_HM"), tz = "MST")
    
    NONNCR_EDStatus_Dec152021Ongoing$End.Date <- parse_date_time(NONNCR_EDStatus_Dec152021Ongoing$End.Date, c("mdY!_HM", "Y!md_HMS", "dmY!_HM", "db!Y!_HM"), tz = "MST")

# bind dfs together
All_EDStatus_Jan12020Ongoing <- bind_rows(All_EDStatus_Jan12020Ongoing, NONNCR_EDStatus_Dec152021Ongoing)

# fixing preceding zeros and converting to character
All_EDStatus_Jan12020Ongoing$RegulatoryID[All_EDStatus_Jan12020Ongoing$RegulatoryID == ""] <- NA

All_EDStatus_Jan12020Ongoing$RegulatoryID <- str_pad(All_EDStatus_Jan12020Ongoing$RegulatoryID, 6, pad = "0")

All_EDStatus_Jan12020Ongoing$RegulatoryID <- as.character(All_EDStatus_Jan12020Ongoing$RegulatoryID)

# change ED Divert to just Divert
All_EDStatus_Jan12020Ongoing$Status <- 
  case_when(All_EDStatus_Jan12020Ongoing$Status == "ED Divert" ~ "Divert", 
            TRUE ~ All_EDStatus_Jan12020Ongoing$Status)

# # remove any duplicate rows
# # QA: 102507 before, 91474 after
# All_EDStatus_Jan12020Ongoing <- distinct(All_EDStatus_Jan12020Ongoing, RegulatoryID, Resource, Start.Date, .keep_all = TRUE)

All_EDStatus_Jan12020Ongoing <- All_EDStatus_Jan12020Ongoing %>%
  group_by(RegulatoryID, Resource, Start.Date) %>%
  filter(row_number() == n())


# write csv
write.csv(All_EDStatus_Jan12020Ongoing, "H:/Documents/OEPR Data Requests/EMResource Status Detail Report - All hospitals ED Status - Jan 1 2020 Ongoing.csv", row.names = FALSE)



