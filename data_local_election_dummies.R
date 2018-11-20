## Local elections may produce winner-loser effects of there own
## To deal with this, I restrict the dataset to exclude respondents who
## lived in local authorities where local elections were being held
## To do this, I need dummies to indicate if

library(readxl)
library(zoo)
library(reshape)

# Create a function to extract just seat shares and just vote shares by local election
seat_share <- function(df) {
  names(df)[2:ncol(df)] <- c("district", "party", "votes", "vote_share", "wards_contested", 
                             "wards_share_contested", "wards_won", "wards_won_share", 
                             "seats_contested", "seats_contested_share", "seats_won", "seats_won_share")
  # Data is messy excel file, so start at row where local authorities begin (row 6)
  df <- df[6:nrow(df), 2:ncol(df)]
  df <- as.data.frame(df)
  # Repeat district name
  df$district <- zoo::na.locf(df$district)
  df <- df[,c(1:3, 11:12)]
  seatsshare <- df[, c(1, 2, 5)]
  seatsshare <- spread(seatsshare, party, seats_won_share)
  return(seatsshare)
}

# Apply the function for seat shares
# Because of idiosyncracies in the excel file, this cannot be automated more efficiently

# We will fill up this list with election dataframes
elections_list <- list()
seat_share_list <- list()

elections_list$local2014 <- read_xlsx("data/District level Authority Results.xlsx", sheet = "yr2014")
seat_share_list$seatshare2014 <- seat_share(elections_list$local2014)
seat_share_list$seatshare2014$year <- rep(2014, times = nrow(seat_share_list$seatshare2014))

elections_list$local2015 <- read_xlsx("data/District level Authority Results.xlsx", sheet = "yr2015")
seat_share_list$seatshare2015 <- seat_share(elections_list$local2015)
seat_share_list$seatshare2015$year <- rep(2015, times = nrow(seat_share_list$seatshare2015))

elections_list$local2016 <- read_xlsx("data/District level Authority Results.xlsx", sheet = "yr2016")
seat_share_list$seatshare2016 <- seat_share(elections_list$local2016)
seat_share_list$seatshare2016$year <- rep(2016, times = nrow(seat_share_list$seatshare2016))

elections_list$local2017 <- read_xlsx("data/District level Authority Results.xlsx", sheet = "yr2017")
elections_list$local2017 <- elections_list$local2017[-c(1637:1642),]
seat_share_list$seatshare2017 <- seat_share(elections_list$local2017)
seat_share_list$seatshare2017$year <- rep(2017, times = nrow(seat_share_list$seatshare2017))

# While the dataframes in the lists above have seat shares for each party, we do not need this info
# We will select just year, district, and total (total will be used to determine whether or not there was an election)
elections_list_clean <- lapply(seat_share_list, function(x) {
  x %>% dplyr::select(year, district, Total)
  }
  )

# Merge lists into a DF
merged_elections <- merge_all(elections_list_clean)
# Spread so that each year is its own variable
merged_elections <- merged_elections %>% spread(year, Total)

# Names start with numbers, not good practice
names(merged_elections) <- c("district", "local2014", "local2015", "local2016", "local2017")

# Get names for for loop
names_merged_elections <- c("local2014", "local2015", "local2016", "local2017")

# Loop through each column, if NA, no election
for(i in 1:length(names_merged_elections)) {
  election <- merged_elections[ , names_merged_elections[i]]
  merged_elections[ , names_merged_elections[i]] <- ifelse(is.na(election), 0, 1)
}

election_dummies <- merged_elections

## Clean up riding names
election_dummies$district <- gsub("&", "and", election_dummies$district)
election_dummies$district <- gsub("Upon", "upon", election_dummies$district)
election_dummies$district[election_dummies$district == "Herefordshire"] <- "Herefordshire, County of"
election_dummies$district[election_dummies$district == "Kingston upon Hull"] <- "Kingston upon Hull, City of"
election_dummies$district[election_dummies$district == "Bristol"] <- "Bristol, City of"
election_dummies$district[election_dummies$district == "Vale Of Glamorgan"] <- "The Vale of Glamorgan"
election_dummies$district[election_dummies$district == "Vale Of White Horse"] <- "Vale of White Horse"
election_dummies$district[election_dummies$district == "East Riding Of Yorkshire"] <- "East Riding of Yorkshire"
election_dummies$district[election_dummies$district == "Kings Lynn and West Norfolk" ] <- "King's Lynn and West Norfolk" 
election_dummies$district[election_dummies$district == "Neath and Port Talbot"] <- "Neath Port Talbot"
election_dummies$district[election_dummies$district == "Rhondda Cynon Taff"] <- "Rhondda, Cynon, Taff"
election_dummies$district[election_dummies$district == "Southend On Sea"] <- "Southend-on-Sea"
election_dummies$district[election_dummies$district == "St Helens"] <- "St. Helens"
election_dummies$district[election_dummies$district == "Stratford On Avon"] <- "Stratford-on-Avon"
election_dummies$district[election_dummies$district == "Southend On Sea"] <- "Southend-on-Sea"
election_dummies$district[election_dummies$district == "Stoke On Trent"] <- "Stoke-on-Trent"
election_dummies$district[election_dummies$district == "Barrow-in-Furness"] <- "Barrow In Furness"

# We no longer need these objects
rm(seat_share, elections_list, elections_list_clean, merged_elections, election, names_merged_elections, seat_share_list)

write.csv(election_dummies, file = "data/localelectiondummies.csv")
