## NOAA storm data analysis

## Packages
library(dplyr)

## Download data and prep for reading
if(!file.exists("./storm.csv.bz2")){
    file <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
    download.file(file, "./storm.csv.bz2", method = "curl")
    rm(file)
}
## Read data
storm <- tbl_df(read.csv("./storm.csv.bz2", stringsAsFactors = FALSE, 
                         na.strings = c(NA, "")
))

## Remove unnecessary variables
storm <- storm %>%
    select(STATE, EVTYPE, FATALITIES:CROPDMGEXP)

## Clean up variable names
names(storm) <- tolower(names(storm))

## Remove any unusable multipliers
storm <- mutate(storm, propdmgexp = tolower(propdmgexp),
                cropdmgexp = tolower(propdmgexp))
good <- c("k", "m", "b", NA) 
gProp <- storm$propdmgexp %in% good
gCrop <- storm$cropdmgexp %in% good
storm$propdmgexp[!gProp] <- NA
storm$cropdmgexp[!gCrop] <- NA
rm(good, gProp, gCrop)

## Convert multipliers, use multipliers on dmg, remove multipliers
storm <- storm %>%
    mutate(propdmgexp = gsub("k", 1000, propdmgexp),
           propdmgexp = gsub("m", 1000000, propdmgexp),
           propdmgexp = gsub("b", 1000000000, propdmgexp),
           cropdmgexp = gsub("k", 1000, cropdmgexp),
           cropdmgexp = gsub("m", 1000000, cropdmgexp),
           cropdmgexp = gsub("b", 1000000000, cropdmgexp),
           propdmgexp = as.numeric(propdmgexp),
           cropdmgexp = as.numeric(cropdmgexp),
           propdmg = propdmg * propdmgexp,
           cropdmg = cropdmg * cropdmgexp
    ) %>%
    select(-propdmgexp, -cropdmgexp)