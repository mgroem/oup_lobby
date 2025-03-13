# !diagnostics off


rm(list=ls())

options(scipen=999)
options(java.parameters = "-Xmx4g")

library(vdemdata)
library(democracyData)
library(rqog)


# FUNCTIONS ---------------------------------------------------------------
source("src/00_functions.R")


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
#eci <- plyr::rename(eci, c("variable" = "year",
#                           "value" = "eci",
#                           "country" = "country_name"))
eci$year <- as.numeric(as.character(eci$year))
eci$country_name[eci$country_name=="Myanmar"] <- "Burma/Myanmar"
eci$country_name[eci$country_name=="CÃ´te d'Ivoire"] <- "Ivory Coast"
eci$country_name[eci$country_name=="Czechia"] <- "Czech Republic"
eci$country_name[eci$country_name=="Eswatini"] <- "Swaziland"
eci$country_name[eci$country_name=="North Macedonia"] <- "Macedonia"



# BTI 2018
tmp <- tempfile(fileext=".xlsx")
download.file("https://www.dropbox.com/s/jw16u6p2i7h9z1o/BTI%202006-2020%20Scores.xlsx?dl=1",
              destfile=tmp, mode="wb")
bti2020 <- as.data.frame(readxl::read_excel(tmp, sheet="BTI 2020", col_names = TRUE,col_types=NULL,na="",skip=0))
bti2018 <- as.data.frame(readxl::read_excel(tmp, sheet="BTI 2018", col_names = TRUE,col_types=NULL,na="",skip=0))
bti2016 <- as.data.frame(readxl::read_excel(tmp, sheet="BTI 2016", col_names = TRUE,col_types=NULL,na="",skip=0))
bti2014 <- as.data.frame(readxl::read_excel(tmp, sheet="BTI 2014", col_names = TRUE,col_types=NULL,na="",skip=0))
bti2012 <- as.data.frame(readxl::read_excel(tmp, sheet="BTI 2012", col_names = TRUE,col_types=NULL,na="",skip=0))
bti2010 <- as.data.frame(readxl::read_excel(tmp, sheet="BTI 2010", col_names = TRUE,col_types=NULL,na="",skip=0))
bti2008 <- as.data.frame(readxl::read_excel(tmp, sheet="BTI 2008", col_names = TRUE,col_types=NULL,na="",skip=0))
bti2006 <- as.data.frame(readxl::read_excel(tmp, sheet="BTI 2006", col_names = TRUE,col_types=NULL,na="",skip=0))

bti2020$year <- 2020
bti2018$year <- 2018
bti2016$year <- 2016
bti2014$year <- 2014
bti2012$year <- 2012
bti2010$year <- 2010
bti2008$year <- 2008
bti2006$year <- 2006

temp <- list(bti2006, bti2008, bti2010, bti2012, bti2014, bti2016, bti2018, bti2020)
temp <- lapply(temp, function(x) {colnames(x)[1] <- 'countryname'; x}) 
temp <- lapply(temp, function(x) {colnames(x)[grepl('Q14 | Steering Capability',colnames(x))] <- 'bti_steer'; x}) 
temp <- lapply(temp, function(x) {colnames(x)[grepl('GII | Governance Performance',colnames(x))] <- 'bti_govperf'; x}) 
temp <- lapply(temp, function(x) {colnames(x)[grepl('Q5.2 | Interest groups',colnames(x))] <- 'bti_IGs'; x}) 
temp <- lapply(temp, subset, select = c("year", "countryname", "bti_steer", "bti_govperf", "bti_IGs"))
temp <- lapply(temp, function(x) {colnames(x)[2] <- 'ISO'; x}) 



lookup <- list("Afghanistan" = "AFG",
               "Albania" = "ALB",
               "Algeria" = "DZA",
               "Angola" = "AGO",
               "Argentina" = "ARG",
               "Armenia" = "ARM",
               "Azerbaijan" = "AZE",
               "Bahrain" = "BHR",
               "Bangladesh" = "BGD",
               "Belarus" = "BLR",
               "Benin" = "BEN",
               "Bhutan" = "BTN",
               "Bolivia" = "BOL",
               "Bosnia and Herzegovina" = "BIH",
               "Botswana" = "BWA",
               "Brazil" = "BRA",
               "Bulgaria" = "BGR",
               "Burkina Faso" = "BFA",
               "Burundi" = "BDI",
               "Cambodia" = "KHM",
               "Cameroon" = "CMR",
               "Central African Republic" = "CAF",
               "Chad" = "TCD",
               "Chile" = "CHL",
               "China" = "CHN",
               "Colombia" = "COL",
               "Congo, DR" = "COD",
               "Congo, Rep." = "COG",
               "Costa Rica" = "CRI",
               "Côte d'Ivoire" = "CIV",
               "Croatia" = "HRV",
               "Cuba" = "CUB",
               "Czech Republic" = "CZE",
               "Djibouti" = "DJI",
               "Dominican Republic" = "DOM",
               "Ecuador" = "ECU",
               "Egypt" = "EGY",
               "El Salvador" = "SLV",
               "Equatorial Guinea" = "GNQ",
               "Eritrea" = "ERI",
               "Estonia" = "EST",
               "Eswatini" = "SWZ",
               "Ethiopia" = "ETH",
               "Gabon" = "GAB",
               "Gambia" = "GMB",
               "Georgia" = "GEO",
               "Ghana" = "GHA",
               "Guatemala" = "GTM",
               "Guinea" = "GIN",
               "Guinea-Bissau" = "GNB",
               "Haiti" = "HTI",
               "Honduras" = "HND",
               "Hungary" = "HUN",
               "India" = "IND",
               "Indonesia" = "IDN",
               "Iran" = "IRN",
               "Iraq" = "IRQ",
               "Jamaica" = "JAM",
               "Jordan" = "JOR",
               "Kazakhstan" = "KAZ",
               "Kenya" = "KEN",
               "Kosovo" = "XKX",
               "Kuwait" = "KWT",
               "Kyrgyzstan" = "KGZ",
               "Laos" = "LAO",
               "Latvia" = "LVA",
               "Lebanon" = "LBN",
               "Lesotho" = "LSO",
               "Liberia" = "LBR",
               "Libya" = "LBY",
               "Lithuania" = "LTU",
               "Madagascar" = "MDG",
               "Malawi" = "MWI",
               "Malaysia" = "MYS",
               "Mali" = "MLI",
               "Mauritania" = "MRT",
               "Mauritius" = "MUS",
               "Mexico" = "MEX",
               "Moldova" = "MDA",
               "Mongolia" = "MNG",
               "Montenegro" = "MNE",
               "Morocco" = "MAR",
               "Mozambique" = "MOZ",
               "Myanmar" = "MMR",
               "Namibia" = "NAM",
               "Nepal" = "NPL",
               "Nicaragua" = "NIC",
               "Niger" = "NER",
               "Nigeria" = "NGA",
               "North Korea" = "PRK",
               "North Macedonia" = "MKD",
               "Oman" = "OMN",
               "Pakistan" = "PAK",
               "Panama" = "PAN",
               "Papua New Guinea" = "PNG",
               "Paraguay" = "PRY",
               "Peru" = "PER",
               "Philippines" = "PHL",
               "Poland" = "POL",
               "Qatar" = "QAT",
               "Romania" = "ROU",
               "Russia" = "RUS",
               "Rwanda" = "RWA",
               "Saudi Arabia" = "SAU",
               "Senegal" = "SEN",
               "Serbia" = "SRB",
               "Sierra Leone" = "SLE",
               "Singapore" = "SGP",
               "Slovakia" = "SVK",
               "Slovenia" = "SVN",
               "Somalia" = "SOM",
               "South Africa" = "ZAF",
               "South Korea" = "KOR",
               "South Sudan" = "SSD",
               "Sri Lanka" = "LKA",
               "Sudan" = "SDN",
               "Syria" = "SYR",
               "Taiwan" = "TWN",
               "Tajikistan" = "TJK",
               "Tanzania" = "TZA",
               "Thailand" = "THA",
               "Timor-Leste" = "TLS",
               "Togo" = "TGO",
               "Trinidad and Tobago" = "TTO",
               "Tunisia" = "TUN",
               "Turkey" = "TUR",
               "Turkmenistan" = "TKM",
               "Uganda" = "UGA",
               "Ukraine" = "UKR",
               "United Arab Emirates" = "ARE",
               "Uruguay" = "URY",
               "Uzbekistan" = "UZB",
               "Venezuela" = "VEN",
               "Vietnam" = "VNM",
               "Yemen" = "YEM",
               "Zambia" = "ZMB",
               "Zimbabwe" = "ZWE")

#dplyr::recode(bti2006$countryname, !!!lookup)

for (i in 1:8){temp[[i]][[2]] <- dplyr::recode(temp[[i]][[2]], !!!lookup)}

bti <- rbind(as.data.frame(temp[[1]]),
              as.data.frame(temp[[2]]),
              as.data.frame(temp[[3]]),
              as.data.frame(temp[[4]]),
              as.data.frame(temp[[5]]),
              as.data.frame(temp[[6]]),
              as.data.frame(temp[[7]]),
              as.data.frame(temp[[8]]))




# democracy indices
democracy_data <- generate_democracy_scores_dataset(output_format = "wide",verbose = FALSE)
democracy_data <- as.data.frame((democracy_data))


# Geddes personalism index
personalism <- read.csv("https://www.dropbox.com/s/uk6gx11elg58suy/Geddes_personalism_index.csv?dl=1")




genepy <- read.csv("https://www.dropbox.com/s/sluss7kba7ztp1c/genepy_index_1995_2017.csv?dl=1")

  


# MERGE -------------------------------------------------------------------

merge <- merge(vdem, eci, by = c("country_name", "year"), all.x = T, all.y = F)
merge <- merge(merge, bti, by = c("ISO", "year"), all.x = T, all.y = F)
merge <- merge(merge, democracy_data, by.x = c("country_name", "year"), by.y = c("extended_country_name", "year"), all.x = T, all.y = F)
merge <- merge(merge, personalism, by.x=c("cown", "year"), by.y = c("cowcode", "year"), all.x = T, all.y = F)

merge <- merge(merge, qog, by = c("ISO","year"), all=T)

# WRITE -------------------------------------------------------------------


save(merge, file = "data\\merge.RData")


