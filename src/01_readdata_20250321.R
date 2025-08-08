# !diagnostics off

rm(list=ls())
options(scipen=999)

# override table so that it always uses "NA"
table <- function(..., useNA = "always") {
  base::table(..., useNA = useNA)
}


options(scipen=999)
options(java.parameters = "-Xmx4g")

library(vdemdata)
library(democracyData)
library(rqog)
library(countrycode)
library(dplyr)
library(electoral)
library(tidyr)
library(wbstats)




# HISTORICAL COUNTRY NAME LOOKUP LIST -------------------------------------
# Define an expanded lookup table for historical country names
historical_iso3 <- c(
  # East Germany
  "East Germany" = "DDR",
  "Germany East" = "DDR",
  "German Democratic Republic" = "DDR",
  
  # West Germany
  "German Federal Republic" = "DEU",
  "Germany" = "DEU",
  
  # Czechoslovakia
  "Czechoslovakia" = "CSK",
  
  # Yugoslavia
  "Yugoslavia" = "YUG",
  "Yugoslavia, FPR" = "YUG",
  "Yugoslavia, FR (Serbia/Montenegro)" = "SCG",  # Serbia and Montenegro (1992-2006)
  
  # Serbia (post-Yugoslavia)
  "Serbia" = "SRB",
  
  # North Yemen
  "Yemen Arab Republic" = "YEM",
  "Yemen (Arab Republic of Yemen)" = "YEM",
  "North Yemen" = "YEM",
  
  # South Yemen
  "Yemen People's Republic" = "YMD",
  "Yemen, People's Republic of" = "YMD",
  "South Yemen" = "YMD",
  
  # South Vietnam
  "Republic of Vietnam" = "VNM",
  "Vietnam, Republic of" = "VNM",
  "South Vietnam" = "VNM"
)





# LOAD DATA ---------------------------------------------------------------
qog <- read_qog("standard", "time-series")
qog$ISO <- qog$ccodealp

vdem <- vdem
vdem$ISO <- vdem$country_text_id
vdem$country_text_id <- NULL
vdem$v2x_regime1 <- ifelse(vdem$v2x_regime==0, "Closed autocracy",
                           ifelse(vdem$v2x_regime==1, "Electoral autocracy",
                                  ifelse(vdem$v2x_regime==2, "Electoral democracy", 
                                         ifelse(vdem$v2x_regime==3, "Liberal democracy", NA))))
vdem$v2x_regime1 <- relevel(as.factor(vdem$v2x_regime1) , ref = "Liberal democracy")

vdem$v2x_regime2 <- ifelse(vdem$v2x_regime==0 | vdem$v2x_regime==1, "Autocracy",
                           ifelse(vdem$v2x_regime==2 | vdem$v2x_regime==3, "Democracy", NA))
vdem$v2x_regime2 <- relevel(as.factor(vdem$v2x_regime2) , ref = "Democracy")


eci <- read.csv("https://www.dropbox.com/s/rfde1u2qk2ceirq/Country%20Complexity%20Rankings%201995%20-%202018.csv?dl=1")
eci <- eci[,c(1,26:49)]
eci <- plyr::rename(eci, c(
              "ECI.2018" = "2018",
              "ECI.2017" = "2017",
              "ECI.2016" = "2016",
              "ECI.2015" = "2015",
              "ECI.2014" = "2014",
              "ECI.2013" = "2013",
              "ECI.2012" = "2012",
              "ECI.2011" = "2011",
              "ECI.2010" = "2010",  
              "ECI.2009" = "2009",
              "ECI.2008" = "2008",
              "ECI.2007" = "2007",
              "ECI.2006" = "2006",
              "ECI.2005" = "2005",
              "ECI.2004" = "2004",
              "ECI.2003" = "2003",
              "ECI.2002" = "2002",
              "ECI.2001" = "2001",
              "ECI.2000" = "2000", 
              "ECI.1999" = "1999",
              "ECI.1998" = "1998",
              "ECI.1997" = "1997",
              "ECI.1996" = "1996",
              "ECI.1995" = "1995"))


eci <- reshape2::melt(eci)
eci <- plyr::rename(eci, c("variable" = "year",
                           "value" = "eci",
                           "Country" = "country_name"))
eci$year <- as.numeric(as.character(eci$year))
eci$country_name[eci$country_name=="Myanmar"] <- "Burma/Myanmar"
eci$country_name[eci$country_name=="CÃ´te d'Ivoire"] <- "Ivory Coast"
eci$country_name[eci$country_name=="Czechia"] <- "Czech Republic"
eci$country_name[eci$country_name=="Eswatini"] <- "Swaziland"
eci$country_name[eci$country_name=="North Macedonia"] <- "Macedonia"
eci$ISO <- countrycode(eci$country_name , origin = 'country.name', destination = 'iso3c')
eci <- eci[c("ISO","year", "eci")]




# generalized index of economic complexity
genepy <- read.csv("https://www.dropbox.com/s/sluss7kba7ztp1c/genepy_index_1995_2017.csv?dl=1")

# Transform the data
genepy <- genepy %>%
  pivot_longer(cols = -ISO,  # All columns except ISO
               names_to = "year",  # New column for years
               values_to = "genepy") %>%  # New column for values
  mutate(year = as.integer(sub("X", "", year)))  # Remove 'X' prefix and convert to integer









bti <- bti
bti$bti_IGs <- bti$Q5.2_Interest_groups
bti$ISO <- countrycode(bti$cown , origin = 'cown', destination = 'iso3c')
bti$ISO[is.na(bti$ISO)] <- countrycode(bti$bti_country[is.na(bti$ISO)] , origin = 'country.name', destination = 'iso3c')
bti <- bti[c("ISO","year", "bti_IGs")]


# Geddes personalism index
personalism <- read.csv("https://www.dropbox.com/s/uk6gx11elg58suy/Geddes_personalism_index.csv?dl=1")
personalism$ISO <- countrycode(personalism$cowcode, origin = 'cown', destination = 'iso3c')
personalism <- personalism %>%      # Fill ISO for unmatched cases where country names match historical ones
  mutate(ISO = ifelse(is.na(ISO) & country %in% names(historical_iso3), 
                      historical_iso3[country], ISO))
personalism$ISO[is.na(personalism$ISO)] <- countrycode(personalism$country[is.na(personalism$ISO)] , origin = 'country.name', destination = 'iso3c')
personalism <- personalism[c("ISO","year", "xpers")]


kailitz <- kailitz[c("kailitz_country", "kailitz_kailitz_cown",  "year", "combined_regime",
                     "personal",
                     "communist",
                     "military",
                     "party",
                     "monarchy",
                     "electoral")]
kailitz <- kailitz %>%
  mutate(
    kailitz_regime = case_when(
      personal   ~ "Personal",
      communist  ~ "Communist",
      military   ~ "Military",
      party      ~ "Party",
      monarchy   ~ "Monarchy",
      electoral  ~ "Electoral",
      TRUE ~ NA_character_  # Keeps NA values if none are TRUE
    )
  )
kailitz$ISO <- countrycode(kailitz$kailitz_kailitz_cown, origin = 'cown', destination = 'iso3c')
kailitz <- kailitz %>%
  mutate(ISO = ifelse(is.na(ISO) & kailitz_country %in% names(historical_iso3), 
                      historical_iso3[kailitz_country], ISO))
kailitz <- kailitz[c("ISO", "year", "kailitz_regime")]



wth <- wahman_teorell_hadenius[c("wahman_teorell_hadenius_country",
                                 "wahman_teorell_hadenius_cowcode",
                                 "year",
                                 "regime1ny",
                                 "mon",
                                 "mil",
                                 "mul",
                                 "onep",
                                 "nop")]
# Recode wth$regime1ny with the specified labels
wth$regime1ny <- factor(wth$regime1ny, 
                        levels = c(1, 2, 3, 4, 5, 6, 7, 9, 10, 11, 12, 99, 100), 
                        labels = c("Monarchy", 
                                   "Military", 
                                   "One-party", 
                                   "Multi-party", 
                                   "Multi-monarchy", 
                                   "Military-multi", 
                                   "Military-one", 
                                   "No-party", 
                                   "No-party mon", 
                                   "Military noparty", 
                                   "Rebel regime", 
                                   "Other", 
                                   "Democracy"))
wth <- wth %>%
  mutate(
    wth_regime = case_when(
      mon == 1  ~ "Monarchy",
      mil == 1  ~ "Military",
      mul == 1  ~ "Multiparty",
      onep == 1 ~ "One-party",
      nop == 1  ~ "No-party",
      TRUE ~ NA_character_  # Keeps NA values if none of the conditions match
    )
  )
wth$ISO <- countrycode(wth$wahman_teorell_hadenius_cowcode, origin = 'cown', destination = 'iso3c')
wth <- wth %>%
  mutate(ISO = ifelse(is.na(ISO) & wahman_teorell_hadenius_country %in% names(historical_iso3), 
                      historical_iso3[wahman_teorell_hadenius_country], ISO))
wth <- wth[c("ISO", "year","wth_regime")]



# recode autocratic regime type
qog$gwf_regimetype <- factor(qog$gwf_regimetype, 
                               levels = 1:10, 
                               labels = c("Monarchy", 
                                          "Personal", 
                                          "Military", 
                                          "Party", 
                                          "Party-Personal", 
                                          "Party-Military", 
                                          "Military-Personal", 
                                          "Party-Personal-Military", 
                                          "Oligarchy", 
                                          "Indirect Military"))
qog <- qog %>%
  mutate(
    gwf_regimetype4 = case_when(
      gwf_regimetype %in% c("Party", "Party-Personal", "Party-Military", "Party-Personal-Military") ~ "Party",
      gwf_regimetype %in% c("Military", "Military-Personal", "Indirect Military") ~ "Military",
      gwf_regimetype == "Monarchy" ~ "Monarchy",
      gwf_regimetype == "Personal" ~ "Personal",
      TRUE ~ NA_character_  # Keep NA values as NA
    )
  )
gwf <- qog[c("ISO", "year", "gwf_regimetype4")]



# CLEA
# download https://electiondataarchive.org/wp-content/uploads/2024/04/clea_lc_20240419_r.zip
load("data/clea_lc_20240419.RData")
clea <- clea_lc_20240419[c("ctr_n", "yr", "cst", "pty", "pv1", "seat")]
clea$ISO <- countrycode(clea$ctr_n, origin = 'country.name', destination = 'iso3c')
clea$chamber <- "lower"

# Count unique constituencies (cst) per ISO and yr
cst_counts <- clea %>%
  group_by(ISO, yr) %>%
  summarise(unique_cst = n_distinct(cst), .groups = "drop")
cst_counts$year <- cst_counts$yr
cst_counts$districts <- cst_counts$unique_cst
cst_counts <- cst_counts[c("ISO", "year", "districts")]

# calculate effective number of parties
source('https://raw.github.com/santiago-alles/enp.script/master/enp.v1.5.r')
# Aggregate votes and seats at the ISO-yr-pty level
clea_aggregated <- clea %>%
  group_by(ISO, yr, pty) %>%
  summarise(
    total_seats = sum(seat, na.rm = TRUE),  # Sum votes for each party
    .groups = "drop"
  )
clea_aggregated$total_seats[clea_aggregated$total_seats<0] <- 0
clea_aggregated$chamber <- "lower"
clea_enp <- enp.FUN(clea_aggregated, seats = "total_seats", chamber = "chamber",
                    year = "yr", district = "ISO",
                    enp_s = T, save = F)
clea_enp$ISO <- clea_enp$district
clea_enp <- clea_enp[c("ISO", "year", "enp.seats")]

# forward fill ENP and electoral districts
full_years <- 1950:2022  # Define the full year range

cst_counts <- cst_counts %>%
  group_by(ISO) %>% 
  complete(year = full_years) %>%  # Expands data for all years per ISO
  arrange(ISO, year) %>%           # Ensure sorting
  fill(districts, .direction = "down") %>%  # Forward fill districts
  ungroup()

clea_enp <- clea_enp %>%
  group_by(ISO) %>% 
  complete(year = full_years) %>%  # Expands data for all years per ISO
  arrange(ISO, year) %>%           # Ensure sorting
  fill(enp.seats, .direction = "down") %>%  # Forward fill districts
  ungroup()

clea_enp <- clea_enp[clea_enp$year>=1950,]
print(clea_enp %>%
        arrange(desc(enp.seats)), n = 200)    #  outliers are elections where all candidates ran as independents (AFG 2008, PAK 1985)
clea_enp$enp.seats[clea_enp$ISO=="KWT" | clea_enp$ISO=="BLR" | clea_enp$ISO=="SWZ" | clea_enp$ISO=="BHR"] <- 0    # remove ENP for elections where all candidates run as independents
clea_enp$enp.seats[clea_enp$ISO=="AFG" & clea_enp$year<2018] <- 0    # remove ENP for elections where all candidates run as independents
clea_enp$enp.seats[clea_enp$ISO=="PAK" & clea_enp$year<1988 & clea_enp$year>1984] <- 0    # remove ENP for elections where all candidates run as independents






# CREATE VARIABLES --------------------------------------------------------

vdem$logpop <- log(vdem$e_wb_pop)
vdem$cso_repress <- 4-vdem$v2csreprss_osp
vdem$election <- ifelse(rowSums(!is.na(vdem[, paste0("v2eltype_", 0:7)])) > 0, 1, 0)  # designate election years

vdem <- vdem %>%
  mutate(e_regionpol_7C = case_when(
    e_regionpol_7C == 1 ~ "Eastern Europe",
    e_regionpol_7C == 2 ~ "Latin America and the Caribbean",
    e_regionpol_7C == 3 ~ "Middle East and North Africa",
    e_regionpol_7C == 4 ~ "Sub-Saharan Africa",
    e_regionpol_7C == 5 ~ "Western Europe and North America",
    e_regionpol_7C == 6 ~ "East Asia and the Pacific",
    e_regionpol_7C == 7 ~ "South and Central Asia",
    TRUE ~ NA_character_  # Preserve NA values
  ))



# CHECK -------------------------------------------------------------------

# List of data frame names
df_names <- c("vdem", "qog", "bti", "clea_enp", "cst_counts", "eci", 
              "genepy", "gwf", "kailitz", "personalism", "wth")

# Function to check for duplicates based on ISO and year
check_duplicates <- function(df_name) {
  df <- get(df_name)
  has_duplicates <- any(duplicated(df[c("ISO", "year")]))
  return(data.frame(dataframe = df_name, has_duplicates = has_duplicates))
}

results <- do.call(rbind, lapply(df_names, check_duplicates))  # Apply the function to each data frame
print(results)   # kailitz, personalism, and wth have duplicates

# view how many duplicates in each   
cat("kailitz duplicates:", nrow(kailitz_dups), "\n")   #42
cat("personalism duplicates:", nrow(personalism_dups), "\n")   #42
cat("wth duplicates:", nrow(wth_dups), "\n")    #128


# Function to extract duplicated rows
get_duplicates <- function(df_name) {
  df <- get(df_name)
  dup_rows <- df[duplicated(df[c("ISO", "year")]) | duplicated(df[c("ISO", "year")], fromLast = TRUE), ]
  return(dup_rows)
}

# Inspect duplicates for each problematic data frame
kailitz_dups <- get_duplicates("kailitz")
personalism_dups <- get_duplicates("personalism")
wth_dups <- get_duplicates("wth")

# Remove duplicates 
kailitz <- kailitz[!(duplicated(kailitz[c("ISO", "year")]) & is.na(kailitz$kailitz_regime)), ]
wth <- wth[!(duplicated(wth[c("ISO", "year")]) & is.na(wth$wth_regime)), ]
wth <- wth[!duplicated(wth[c("ISO", "year")]), ]
kailitz <- kailitz[!duplicated(kailitz[c("ISO", "year")]), ]

results <- do.call(rbind, lapply(df_names, check_duplicates))  # recheck


# Function to summarize year range and number of unique ISOs
summarize_df <- function(df_name) {
  df <- get(df_name)
  year_range <- range(df$year, na.rm = TRUE)
  n_iso <- length(unique(df$ISO))
  return(data.frame(
    dataframe = df_name,
    start_year = year_range[1],
    end_year = year_range[2],
    unique_ISOs = n_iso
  ))
}
summary_table <- do.call(rbind, lapply(df_names, summarize_df))  # Apply the function to each data frame
print(summary_table) # Display the result














# SUBSET ------------------------------------------------------------------

vdem <- vdem[c("ISO", "year",
               "logpop", "cso_repress", "election", "v2pscohesv_osp", "v2x_feduni", "v2x_regime2", "v2x_regime1", 
               "v2cscnsult_osp", "v2psparban_ord", "v2cseeorgs_osp", "v2csreprss_osp", "e_regionpol_7C")]
qog <- qog[c("ISO", "year", "undp_hdi", "br_pres")]

# Subset all data frames to year >= 1972
for (df_name in df_names) {
  df <- get(df_name)
  df <- df[df$year >= 1972, ]
  assign(df_name, df, envir = .GlobalEnv)
}

# Drop rows where ISO or year is NA
for (df_name in df_names) {
  df <- get(df_name)
  df <- df[!is.na(df$ISO) & !is.na(df$year), ]
  assign(df_name, df, envir = .GlobalEnv)
}



# MERGE -------------------------------------------------------------------

merge <- merge(vdem, qog, by = c("ISO", "year"),all.x = T, all.y = F)
merge <- merge(merge, eci, by = c("ISO", "year"),all.x = T, all.y = F)
merge <- merge(merge, genepy, by = c("ISO", "year"),all.x = T, all.y = F)
merge <- merge(merge, bti, by = c("ISO", "year"),all.x = T, all.y = T)
merge <- merge(merge, kailitz, by = c("ISO", "year"),all.x = T, all.y = F)
merge <- merge(merge, wth, by = c("ISO", "year"),all.x = T, all.y = F)
merge <- merge(merge, gwf, by = c("ISO", "year"),all.x = T, all.y = F)
merge <- merge(merge, personalism, by = c("ISO", "year"),all.x = T, all.y = F)
merge <- merge(merge, clea_enp, by = c("ISO", "year"),all.x = T, all.y = F)
merge <- merge(merge, cst_counts, by = c("ISO", "year"),all.x = T, all.y = F)


merge <- merge[!is.na(merge$e_regionpol_7C),]
merge$enp.seats[merge$v2psparban_ord==0 | merge$v2psparban_ord==1] <- 0

table(merge$v2x_regime1, merge$gwf_regimetype4)
table(merge$v2x_regime1, merge$kailitz_regime)
table(merge$v2x_regime1, merge$wth_regime)


# WRITE -------------------------------------------------------------------


substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
merge$yr <- paste("'",substrRight(as.character(merge$year),2), sep = "")
merge$ISOyr <- paste(merge$ISO, merge$yr, sep="")



save(merge, file = "data\\merge.RData")


