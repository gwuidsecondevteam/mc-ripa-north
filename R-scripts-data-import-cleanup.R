#Clear memory
rm(list = ls())

#Set working directory
setwd("H:/My Drive/IDS Capstone - Private Sector Dev/Group/Last-Mile_Market Resilience")

#install packages
install.packages("reshape2")
install.packages("tidyverse")
install.packages("tibble")
install.packages("readxl")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggmap")
install.packages("leaflet")

#load libraries
library(reshape2) #load in dcast()
library(tidyverse)
library(tibble)
library(readxl) #read xlsx file
library(dplyr)
library(ggplot2)
library(ggmap)
library(leaflet)

###################################
### Import and Cleanup Datasets ###
###################################

### Market Data - Ethiopia Rural Market Monitor (ERMM)

  #Import file
  mkt_df <- read.csv("./raw data/forMC_20241212.csv", na.strings = "..")
  
  #Remove all entries for Sudan
  mkt_df <- subset(mkt_df, country != "Sudan")
  
  #Remove instrument column
  mkt_df <- subset(mkt_df, select = -instrument)
  
  #Remove all entries except for Afar, Oromia, and Somali
  mkt_df <- subset(mkt_df, admlvl1 %in% c("Afar", "Oromia", "Somali"))
  
  #Rename admlvl1 to "region" in accordance with SPEI Dataset
  names(mkt_df)[names(mkt_df) == "admlvl1"] <- "region"
  
  # Standardize dates in market data (format "17jul2017")
  mkt_df <- mkt_df %>%
    mutate(date = as.Date(tolower(date), format = "%d%b%Y"),
           date = format(date, "%Y/%m/%d"))
  
  #Check if it worked
  glimpse(mkt_df)
  
  # Export as CSV
  write.csv(mkt_df, "./clean data/mkt_data_clean.csv", row.names = FALSE)

### Conflict Data - Uppsala Conflict Data Program - UCDP
### UCDP Georeferenced Event Dataset (GED) Global version 24.1

  # Import file
  cflt_df <- read.csv("./raw data/GEDEvent_v24_1.csv", na.strings = "..")
  
  # Remove all entries for countries except for Ethiopia
  cflt_df <- subset(cflt_df, country == "Ethiopia")
  
  # Remove all entries except for Afar, Oromia, and Somali
  cflt_df <- subset(cflt_df, adm_1 %in% c("Afar state", "Oromiya state", "Somali state"))
  
  # Remove all entries from outside time frame of 2017-2024
  cflt_df <- subset(cflt_df, year %in% c("2017", "2018", "2019", "2020", 
                                         "2021", "2022", "2023", "2024"))
  
  # Rename columns for clarity
  names(cflt_df)[names(cflt_df) == "adm_1"] <- "region"
  names(cflt_df)[names(cflt_df) == "high"] <- "deaths_high_est"
  names(cflt_df)[names(cflt_df) == "best"] <- "deaths_best_est"
  names(cflt_df)[names(cflt_df) == "low"] <- "deaths_low_est"
  
  # Check for duplicate column names before proceeding
  if(any(duplicated(names(cflt_df)))) {
    cat("Warning: Duplicate column names detected in cflt_df:\n")
    print(names(cflt_df)[duplicated(names(cflt_df))])
    
    # Fix duplicate column names by adding suffix
    fix_names <- make.unique(names(cflt_df), sep = "_")
    names(cflt_df) <- fix_names
  }
  
  # Load dplyr for data manipulation
  library(dplyr)
  
  # Convert and standardize dates
  cflt_df_std <- cflt_df %>%
    mutate(
      # Convert to Date objects (strip time component)
      date_start_clean = as.Date(date_start),
      date_end_clean = as.Date(date_end),
      
      # Calculate middle date when dates differ
      date_middle = case_when(
        date_start_clean == date_end_clean ~ date_start_clean,
        TRUE ~ date_start_clean + floor((date_end_clean - date_start_clean) / 2)
      ),
      
      # Format as standard YYYY-MM-DD strings
      date_standard = format(date_middle, "%Y-%m-%d")
    )
  
  # Display sample of processed dates
  cat("\nProcessed dates:\n")
  print(head(cflt_df_std %>% 
               select(date_start, date_end, date_start_clean, date_end_clean, date_middle, date_standard)))
  
  # Count how many records had different start and end dates
  diff_dates_count <- sum(cflt_df_std$date_start_clean != cflt_df_std$date_end_clean, na.rm = TRUE)
  cat("\nNumber of records with different start and end dates:", diff_dates_count, 
      "(", round(diff_dates_count/nrow(cflt_df_std)*100, 1), "%)\n")
  
  # Save standardized conflict data
  # Keep original columns plus new date column
  cflt_df_std <- cflt_df_std %>%
    select(
      # Keep all original columns from cflt_df (not conflict_data)
      all_of(names(cflt_df)),
      # Add the new standardized date
      date_standard
    )
  
  # Remove columns that are not relevant
  # 
  list_to_remove = c("year", "conflict_name", 
                     "source_date", "source_article", "source_office",
                     "source_headline", "source_original", "number_of_sources",
                     "conflict_dset_id", "conflict_new_id", "geom_wkt", 
                     "dyad_name", "dyad_dset_id", "dyad_new_id", 
                     "side_a_dset_id", "side_a_new_id", "side_a",
                     "side_b_dset_id", "side_b_new_id", "side_b", 
                     "relid", "active_year", "active_year", 
                     "gwnoa", "gwnob","code_status",
                     "deaths_a", "deaths_b", "deaths_unknown",
                     "where_coordinates", "where_description", "country_id", "region_1",
                     "date_start", "date_end")
  cflt_df_std <- cflt_df_std %>% select(-one_of(list_to_remove))
  
  # Remove the word 'state' from region names (case insensitive)
  cflt_df_std$region <- gsub(" state", "", cflt_df_std$region, ignore.case = TRUE)
  
  # Standardize Oromiya to Oromia
  cflt_df_std$region <- gsub("Oromiya", "Oromia", cflt_df_std$region)
  
  # Check if it worked
  unique(cflt_df_std$region)
  
  # Write to CSV file in Clean Data Folder
  write.csv(cflt_df_std, "./clean data/cflt_data_clean.csv", row.names = FALSE)
  
### Standardised Precipitation Evapotranspiration Index (SPEI)
### Drought Data

  # Import file
  spei_df <- read.csv("./raw data/SPEI_Target_Areas.csv")
  
  # Standardize dates in SPEI data (format "1/1/2017")
  spei_df <- spei_df %>%
    mutate(date = as.Date(date, format = "%m/%d/%Y"),
           date = format(date, "%Y-%m-%d"))
  
  # Convert region names to proper case
  # Assuming your region column is named 'region' or similar
  spei_df$region <- tools::toTitleCase(tolower(spei_df$region))
  
  # View
  glimpse(spei_df)
  
  # Save standardized SPEI data
  write_csv(spei_df, "./clean data/spei_data_clean.csv")
  
### RIPA-North Recurrent Monitoring Survey - RMS
### Three different datasets with similar data that needs to be cleaned and compiled into one dataset
  
  # Install the haven package if you don't have it
  # install.packages("haven")
  
  library(haven) # For importing .dta format data
  library(dplyr)
  library(readr)
  library(tidyr)
  library(janitor) # For cleaning column names
  library(lubridate) # For easier manipulation of times and dates

  
### Rename Zones and Woredas to Match Spelling Convention ###
# Start with zones - fewer to deal with
    #Get Unique zone and woreda names
    
    unique(rms1$zone)  # Just numbers
    unique(rms2$zone)  # Numbers matched with labels - use this as a key
    unique(rms3$zone)  # just names
    
    # Create a mapping from numeric codes to text values for zones
    # Create a comprehensive mapping of all zone name variations to standard names
    zone_mapping <- tribble(
      ~original,          ~standard,
      "1",                "Awsi /Zone 1",           #RMS1
      "Awsi Resu",        "Awsi /Zone 1",           #RMS3
      "One/Awsi Resu",    "Awsi /Zone 1",       #RMS2
      
      "3",                "Gabi /Zone 3",
      "Gebi Resu Three",  "Gabi /Zone 3", 
      "Three/Gebi Resu",  "Gabi /Zone 3",
      
      "5",                "Hari /Zone 5",
      "Hari Resu Five",   "Hari /Zone ",
      "Five/Hari Resu",   "Hari /Zone 5",
      
      "11",               "West Hararge", 
      "West Hararghe",    "West Hararge",
      "West Hararge",     "West Hararge",
      
      "12",               "East Hararge",
      "East Hararge",     "East Hararge", 
      "East Harargeh",    "East Hararge",
      
      "21",               "Jarar",
      "Jerer",            "Jarar",   
      
      "22",               "Siti",
      "Sitti",            "Siti",
      
      "23",               "Fafan",
      "Fafen",            "Fafan",
      
      "24",               "Jarar"  #Jarar and Jerer are the same.
    )
    
    # Convert the zone columns from character (RMS1 was numeric)
    rms1$zone <- as.character(rms1$zone)
    rms2$zone <- as.character(rms2$zone)
    rms3$zone <- as.character(rms3$zone) 

    
    # Function to standardize zone names in any dataset
    standardize_zones <- function(df, column_name) {
      df <- df %>%
        mutate(!!column_name := case_when(
          !!sym(column_name) %in% zone_mapping$original ~ 
            zone_mapping$standard[match(!!sym(column_name), zone_mapping$original)],
          TRUE ~ !!sym(column_name)
        ))
      
      return(df)
    }
    
    # Apply to each dataset
    rms1 <- standardize_zones(rms1, "zone")
    rms2 <- standardize_zones(rms2, "zone")  
    rms3 <- standardize_zones(rms3, "zone")
    
    
    # Print sorted lists for visual comparison
    cat("Unique zones in rms1 (sorted):\n")
    print(unique(rms1$zone))
    
    cat("\nUnique zones in rms2 (sorted):\n")
    print(unique(rms2$zone))
    
    cat("\nUnique zones in rms3 (sorted):\n")
    print(unique(rms3$zone))
    
    
# Move on to woredas - There are a lot more of them than there are zones
    #Apply the mapping to rename zones
    #Get Unique zone and woreda names
    
    # Examine data  
    unique(rms1$woreda)  # Just numbers
    unique(rms2$woreda)  # Numbers matched with labels - use this as a key
    unique(rms3$woreda)  # just names
    
    # Woreda name standardization tribble
    # Using as found in shape file from HumData.org
    # https://data.humdata.org/dataset/cod-ab-eth
    
    woreda_mapping_numeric <- tribble(
      ~original,           ~standard,
      # RMS1 numeric codes
      "1",                 "Afambo",
      "2",                 "Asayita",
      "3",                 "Chifra",
      "4",                 "Dubti",
      "5",                 "Mile",
      "6",                 "Awash",
      "7",                 "Dulecha",
      "8",                 "Amibara",
      "9",                 "Gewane",
      "10",                "Dawe",
      "11",                "Dalefage",
      "12",                "Hadelela",
      "13",                "Samurobi",
      "14",                "Telalek",
      "15",                "Chiro Zuria",
      "16",                "Fedis",
      "17",                "Goro Gutu",
      "18",                "Goba Koricha",
      "19",                "Gumbi Bordede",
      "20",                "Mieso",
      "21",                "Babile (OR)",
      "22",                "Gursum (OM)",
      "23",                "Ararso",
      "24",                "Burqod",
      "25",                "Ayisha",
      "26",                "Gablalul",
      "27",                "Gota-Biki",
      "28",                "Hadhagala",
      "29",                "Afdem",
      "30",                "Dembel",
      "31",                "Erer (SM)",
      "32",                "Miesso",
      "33",                "Shinile",
      "34",                "Harawo",  #Derwenaji treated as woreda in RMS, but not HumData map
      "35",                "Goljano",
      "36",                "Haroreys",
      "37",                "Aw-Bare",
      "38",                "Babile (SM)",
      "39",                "Gursum (SM)",
      "40",                "Kebribeyah",
      "41",                "Shabeeley",
      "42",                "Tuliguled",
      "43",                "Degehabur"
    )
      
      # RMS3 text variations
      woreda_mapping_text <- tribble(
        ~original,           ~standard,
      "Aararaso",          "Ararso",
      "Afambo",            "Afambo",
      "Afdem",             "Afdem",
      "Amibara",           "Amibara",
      "Awebere",           "Aw-Bare",
      "Awash Fentaale",    "Awash",
      "Ayisha",            "Ayisha",
      "Aysaita",           "Asayita",
      "Babile",            "Babile (SM)",
      "Babile (Oromia)",   "Babile (OR)",
      "Birqod",            "Burqod",
      "Chifra",            "Chifra",
      "Chiro",             "Chiro Zuria",
      "Danbal",            "Dembel",
      "Derwenaji",         "Harawo",   #Derwenaji treated as woreda in RMS, but not HumData map
      "Dewe",              "Dawe",
      "Dhegahabour",       "Degehabur",
      "Dubti",             "Dubti",
      "Dulesa",            "Dulecha",
      "Erer",              "Erer (SM)",
      "Fedis",             "Fedis",
      "Gablalu",           "Gablalul",
      "Galifagi",          "Dalefage",
      "Gumbi Bordede",     "Gumbi Bordede",
      "Gewane",            "Gewane",
      "Goljano",           "Goljano",
      "Goro Gutu",         "Goro Gutu",
      "Bike",              "Gota-Biki",
      "Guba Koricha",      "Goba Koricha",
      "Gursum",            "Gursum (SM)",
      "Gursum (Oromia)",   "Gursum (OM)",
      "Hadeli Ela",        "Hadelela",
      "Hadhagale",         "Hadhagala",
      "Haroreysa",         "Haroreys",
      "Kebribeyah",        "Kebribeyah",
      "Meiso-Muli",        "Miesso",
      "Meisso",            "Mieso",
      "Mille",             "Mile",
      "Semurobi",          "Samurobi",
      "Shebelle",          "Shabeeley",
      "Shinlle",           "Shinile",
      "Telalak",           "Telalek",
      "Tuluguled",         "Tuliguled"
    )
    
      # Convert the zone columns from character (RMS1 was numeric)
      rms1$woreda <- as.character(rms1$woreda)
      rms2$woreda <- as.character(rms2$woreda)
      rms3$woreda <- as.character(rms3$woreda) 
      
      # Check for values in rms1 that don't have mappings
      missing_mappings_rms1 <- setdiff(rms1$woreda, woreda_mapping_numeric$original)
      print(missing_mappings_rms1)
      
      # Same for rms2 and rms3
      missing_mappings_rms2 <- setdiff(rms2$woreda, woreda_mapping_numeric$original)
      print(missing_mappings_rms2)
      
      missing_mappings_rms3 <- setdiff(rms3$woreda, woreda_mapping_text$original)
      print(missing_mappings_rms3)    
    
    # Function to standardize admin units to shapefile standards
    standardize_admin_units <- function(df, column_name, mapping_table) {
      df %>%
        mutate(!!column_name := case_when(
          !!sym(column_name) %in% mapping_table$original ~ 
            mapping_table$standard[match(!!sym(column_name), mapping_table$original)],
          TRUE ~ !!sym(column_name)
        ))
    }
    
    # Apply to your datasets
    rms1 <- standardize_admin_units(rms1, "woreda", woreda_mapping_numeric)
    rms2 <- standardize_admin_units(rms2, "woreda", woreda_mapping_numeric)
    rms3 <- standardize_admin_units(rms3, "woreda", woreda_mapping_text)
    
    # Get unique values from each dataset
    woredas_rms1 <- sort(unique(as.character(rms1$woreda)))
    woredas_rms2 <- sort(unique(as.character(rms2$woreda)))
    woredas_rms3 <- sort(unique(as.character(rms3$woreda)))
    
    # Print sorted lists for visual comparison
    cat("Unique woredas in rms1 (sorted):\n")
    print(woredas_rms1)
    
    cat("\nUnique woredas in rms2 (sorted):\n")
    print(woredas_rms2)
    
    cat("\nUnique woredas in rms3 (sorted):\n")
    print(woredas_rms3)
    
    all(setequal(woredas_rms1, woredas_rms2),setequal(woredas_rms3, woredas_rms2)) 
    
    # Slim down each dataset to just the parts we want to look at
    
    rms1_slim <- rms1 %>%
      select(region, zone, woreda, 
             hhs1, hhs1b, hhs2, hhs2b, hhs3, hhs3b, 
             hhs, shocks_primary_future, survey_date, round, treatment)
    rms2_slim <- rms2 %>%
      select(region, zone, woreda, 
             hhs1, hhs1b, hhs2, hhs2b, hhs3, hhs3b, 
             hhs, shocks_primary_future, survey_date, round, treatment)
    rms3_slim <- rms3 %>%
      select(region, zone, woreda, 
             hhs1, hhs1b, hhs2, hhs2b, hhs3, hhs3b, 
             hhs, shocks_primary_future, survey_date, round, treatment)
    
    # Make sure the Woreda column is character type in all datasets
    rms1_slim <- rms1_slim %>% mutate(Woreda = as.character(woreda))
    rms2_slim <- rms2_slim %>% mutate(Woreda = as.character(woreda))
    rms3_slim <- rms3_slim %>% mutate(Woreda = as.character(woreda))
    
    # Clean up whitespace and standardize case
    rms1_slim <- rms1_slim %>% mutate(Woreda = trimws(woreda))
    rms2_slim <- rms2_slim %>% mutate(Woreda = trimws(woreda))
    rms3_slim <- rms3_slim %>% mutate(Woreda = trimws(woreda))
    
### Label Woredas by Treatment Status in RMS1 and RMS2
### RMS3 already contains treatment status for each data point
### If treatment is consistent for each woreda, apply that information to RMS1 and RMS2
    
    # Step 1: Check if treatment assignment is consistent within each woreda in RMS3
    # They are consistent
    treatment_consistency_check <- rms3_slim %>%
      group_by(woreda) %>%
      summarize(
        unique_treatments = n_distinct(treatment),
        treatment_values = list(unique(treatment)),
        is_consistent = n_distinct(treatment) == 1
      )
    
    # Safety: Print woredas with inconsistent treatment assignments (if any)
    inconsistent_woredas <- treatment_consistency_check %>% 
      filter(!is_consistent)
    
    if(nrow(inconsistent_woredas) > 0) {
      print("Warning: The following woredas have inconsistent treatment assignments in RMS3:")
      print(inconsistent_woredas)
    } else {
      print("All woredas have consistent treatment assignments in RMS3.")
      
      # Step 2: Create a mapping from woreda to treatment
      woreda_treatment_map <- rms3_slim %>%
        group_by(woreda) %>%
        summarize(treatment = first(treatment)) %>%
        select(woreda, treatment)
      
      # Step 3: Apply treatment labels to RMS1 and RMS2
      rms1_slim <- rms1_slim %>%
        left_join(woreda_treatment_map, by = "woreda")
      
      rms2_slim <- rms2_slim %>%
        left_join(woreda_treatment_map, by = "woreda")
      
      # Check for any woredas in RMS1 or RMS2 that couldn't be matched
      unmatched_woredas_rms1 <- rms1 %>% 
        filter(is.na(treatment)) %>% 
        distinct(woreda)
      
      unmatched_woredas_rms2 <- rms2 %>% 
        filter(is.na(treatment)) %>% 
        distinct(woreda)
      
      if(nrow(unmatched_woredas_rms1) > 0) {
        print("Warning: The following woredas in RMS1 couldn't be matched:")
        print(unmatched_woredas_rms1)
      }
      
      if(nrow(unmatched_woredas_rms2) > 0) {
        print("Warning: The following woredas in RMS2 couldn't be matched:")
        print(unmatched_woredas_rms2)
      }
    }
    
    # Keep only the treatment.y column and rename it:
    rms1_slim <- rms1_slim %>%
      select(-treatment.x) %>% # Remove the original treatment column
      rename(treatment = treatment.y) # Rename the joined treatment column
    
    rms2_slim <- rms2_slim %>%
      select(-treatment.x) %>%
      rename(treatment = treatment.y)
    
### Clean up shocks_primary_future to be only numeric

# RMS1 - Create the clean dataset with standardized columns
rms1_slim <- rms1_slim %>%
  # Create binary treatment column and standardize shocks columns
  mutate(
    
    # First ensure shocks_primary_future is numeric if it isn't already
    shocks_primary_future_num = as.numeric(as.character(shocks_primary_future)),
    
    # Create text version based on the numeric values
    shocks_primary_future_text = case_when(
      shocks_primary_future_num == 1 ~ "Very Confident",
      shocks_primary_future_num == 2 ~ "Somewhat confident",
      shocks_primary_future_num == 3 ~ "Don't Know",
      shocks_primary_future_num == 4 ~ "Somewhat Unconfident",
      shocks_primary_future_num == 5 ~ "Very Unconfident",
      TRUE ~ NA_character_
    )
  )     

        
### Calculate HHS on scale of 0-6 for each RMS Round
    
    # Create new dataframes for safety
    # For RMS1 dataset
    rms1_hhs <- rms1_slim %>%
      mutate(
        # First question: No food in house
        hhs1_score = case_when(
          hhs1 == 1 & hhs1b %in% c(1, 2) ~ 1,  # Yes + Rarely/Sometimes
          hhs1 == 1 & hhs1b == 3 ~ 2,         # Yes + Often
          hhs1 == 0 ~ 0,                     # No
          TRUE ~ NA_real_                   # Handle missing values
        ),
        
        # Second question: Sleep hungry
        hhs2_score = case_when(
          hhs2 == 1 & hhs2b %in% c(1, 2) ~ 1,  # Yes + Rarely/Sometimes
          hhs2 == 1 & hhs2b == 3 ~ 2,         # Yes + Often
          hhs2 == 0 ~ 0,                     # No
          TRUE ~ NA_real_                   # Handle missing values
        ),
        
        # Third question: Whole day without eating
        hhs3_score = case_when(
          hhs3 == 1 & hhs3b %in% c(1, 2) ~ 1,  # Yes + Rarely/Sometimes
          hhs3 == 1 & hhs3b == 3 ~ 2,         # Yes + Often
          hhs3 == 0 ~ 0,                     # No
          TRUE ~ NA_real_                   # Handle missing values
        ),
        
        # Calculate new standardized total HHS score (0-6 scale)
        hhs_standardized = hhs1_score + hhs2_score + hhs3_score,
        
        # Categorize HHS score using scale found in RMS2
        hhs_category = case_when(
          hhs_standardized <= 1 ~ "Little to no hunger",
          hhs_standardized %in% c(2, 3) ~ "Moderate hunger",
          hhs_standardized >= 4 ~ "Severe hunger",
          TRUE ~ NA_character_
        )
      )
    
    # For RMS2 dataset - using only hhs#b columns on a 0-2 scale with match feature
    rms2_hhs <- rms2_slim %>%
      mutate(
        # Direct calculation from hhs#b columns (0-2 scale)
        # Assuming 0 = No hunger, 1 = Rarely/Sometimes, 2 = Often
        hhs1_score = case_when(
          hhs1b == 0 ~ 0,
          hhs1b == 1 ~ 1,
          hhs1b == 2 ~ 2,
          TRUE ~ NA_real_
        ),
        
        hhs2_score = case_when(
          hhs2b == 0 ~ 0,
          hhs2b == 1 ~ 1,
          hhs2b == 2 ~ 2,
          TRUE ~ NA_real_
        ),
        
        hhs3_score = case_when(
          hhs3b == 0 ~ 0,
          hhs3b == 1 ~ 1,
          hhs3b == 2 ~ 2,
          TRUE ~ NA_real_
        ),
        
        # Calculate standardized score
        hhs_standardized = hhs1_score + hhs2_score + hhs3_score,
        
        # Compare with existing hhs column if it exists
        hhs_match = ifelse(!is.na(hhs) & !is.na(hhs_standardized), 
                           ifelse(hhs == hhs_standardized, "Match", "Different"), 
                           NA),
        
        # Categorize HHS score
        hhs_category = case_when(
          hhs_standardized <= 1 ~ "Little to no hunger",
          hhs_standardized %in% c(2, 3) ~ "Moderate hunger",
          hhs_standardized >= 4 ~ "Severe hunger",
          TRUE ~ NA_character_
        )
      )
    
    # For RMS3 dataset with text values
    rms3_hhs <- rms3_slim %>%
      mutate(
        # First question: No food in house
        hhs1_score = case_when(
          hhs1 == "Yes" & hhs1b %in% c("Rarely (1-2 times)", "Sometimes (3-10 times)") ~ 1,  # Yes + Rarely/Sometimes
          hhs1 == "Yes" & hhs1b == "Often (more than 10 times)" ~ 2,                    # Yes + Often
          hhs1 == "No" ~ 0,                                        # No
          TRUE ~ NA_real_                                          # Handle missing values
        ),
        
        # Second question: Sleep hungry
        hhs2_score = case_when(
          hhs2 == "Yes" & hhs2b %in% c("Rarely (1-2 times)", "Sometimes (3-10 times)") ~ 1,  # Yes + Rarely/Sometimes
          hhs2 == "Yes" & hhs2b == "Often (more than 10 times)" ~ 2,                    # Yes + Often
          hhs2 == "No" ~ 0,                                        # No
          TRUE ~ NA_real_                                          # Handle missing values
        ),
        
        # Third question: Whole day without eating
        hhs3_score = case_when(
          hhs3 == "Yes" & hhs3b %in% c("Rarely (1-2 times)", "Sometimes (3-10 times)") ~ 1,  # Yes + Rarely/Sometimes
          hhs3 == "Yes" & hhs3b == "Often (more than 10 times)" ~ 2,                    # Yes + Often
          hhs3 == "No" ~ 0,                                        # No
          TRUE ~ NA_real_                                          # Handle missing values
        ),
        
        # Calculate new standardized total HHS score (0-6 scale)
        hhs_standardized = hhs1_score + hhs2_score + hhs3_score,
        
        # Categorize HHS score
        hhs_category = case_when(
          hhs_standardized <= 1 ~ "Little to no hunger",
          hhs_standardized %in% c(2, 3) ~ "Moderate hunger",
          hhs_standardized >= 4 ~ "Severe hunger",
          TRUE ~ NA_character_
        )
      )
    
### Merge and Export
    
    # Check names of columns
    names(rms1_hhs)
    names(rms2_hhs)
    names(rms3_hhs)
    
    # For RMS1: Create the clean dataset with all needed columns in one step
    rms1_clean <- rms1_hhs %>%
      # First create the binary treatment column
      mutate(
        treatment_binary = ifelse(treatment == "Treatment", 1, 0),
        # Convert shocks_primary_future to character
        shocks_primary_future = as.character(shocks_primary_future)
      ) %>%
      # Then select columns in the desired order
      select(
        
        region,
        zone,
        woreda,
        survey_date,
        treatment,
        treatment_binary,  # Positioned right after treatment
        shocks_primary_future,
        hhs_standardized,
        hhs_category,
        round
      )
    
    # For RMS2: Same approach
    rms2_clean <- rms2_hhs %>%
      mutate(
        treatment_binary = ifelse(treatment == "Treatment", 1, 0),
        shocks_primary_future = as.character(shocks_primary_future)
      ) %>%
      select(
        region,
        zone,
        woreda,
        survey_date,
        treatment,
        treatment_binary,
        shocks_primary_future,
        hhs_standardized,
        hhs_category,
        round
      )
    
    # For RMS3: Same approach
    rms3_clean <- rms3_hhs %>%
      mutate(
        treatment_binary = ifelse(treatment == "Treatment", 1, 0),
        shocks_primary_future = as.character(shocks_primary_future)
      ) %>%
      select(
        region,
        zone,
        woreda,
        survey_date,
        treatment,
        treatment_binary,
        shocks_primary_future,
        hhs_standardized,
        hhs_category,
        round
      )
    
    # Add source column
    rms1_clean <- rms1_clean %>% mutate(source = "RMS1")
    rms2_clean <- rms2_clean %>% mutate(source = "RMS2")
    rms3_clean <- rms3_clean %>% mutate(source = "RMS3")
    
    # Combine the datasets
    all_rms_combined <- bind_rows(
      rms1_clean,
      rms2_clean,
      rms3_clean
    )
    
    # Add Text version of shock confidence column
    #Rename Shock Confidence to reflect numeric contentx
    all_rms_combined <- rename(all_rms_combined, shocks_primary_future_num = shocks_primary_future)
    # Add text interpretation for shock values
    all_rms_combined <- all_rms_combined %>%
      mutate(
        # Create text version based on the numeric values
        # Lower numbers represent greater resilience
        # Follows the practice of Mercy Corps HHS - lower values are more food secure i.e. more resilient
        shocks_primary_future_text = case_when(
          shocks_primary_future_num == 1 ~ "Very Confident",
          shocks_primary_future_num == 2 ~ "Somewhat confident",
          shocks_primary_future_num == 3 ~ "Don't Know",
          shocks_primary_future_num == 4 ~ "Somewhat Unconfident",
          shocks_primary_future_num == 5 ~ "Very Unconfident",
          TRUE ~ NA_character_
        )
      ) 
    
    # Convert survey_date to proper date format and add year/month/quarter columns
    # Parse year and month directly from "YYYY-MM" format
    all_rms_combined <- all_rms_combined %>%
      mutate(
        # Extract year and month components
        survey_year = as.numeric(substr(survey_date, 1, 4)),
        survey_month = as.numeric(substr(survey_date, 6, 7)),
        
        # Calculate quarter
        survey_quarter = ceiling(survey_month / 3)
      )
    
    #Rename treatment column to reflect data type consistently
    all_rms_combined <- rename(all_rms_combined, 
                               treatment_cat = treatment,
                               treatment_bool = treatment_binary,
                               hhs_num = hhs_standardized,
                               hhs_cat = hhs_category,
                               shocks_primary_future_cat = shocks_primary_future_text
    )
    
    # Re-order columns
    # For RMS3: Same approach
    all_rms_combined <- all_rms_combined %>%
      select(
        region, zone, woreda,
        survey_date, survey_year, survey_month, survey_quarter,
        treatment_cat, treatment_bool, 
        shocks_primary_future_cat, shocks_primary_future_num,  
        hhs_cat, hhs_num,  
        source, round
      )
    
    View(all_rms_combined)
    colSums(is.na(all_rms_combined))
    
    # Number of NAs in shock confidence column matches before and after processing
    (sum(is.na(rms1_slim$shocks_primary_future)) + sum(is.na(rms2_slim$shocks_primary_future)) 
      + sum(is.na(rms3_slim$shocks_primary_future))) ==  sum(is.na(all_rms_combined$shocks_primary_future_num)) 

    # Save standardized RMS Data
    write_csv(all_rms_combined, "./clean data/rms_data_clean.csv")
