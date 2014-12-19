## NOAA storm data analysis

## Packages
library(dplyr)

## Download data and prep for reading
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2",
              destfile = "./storm.csv.bz2", method = "curl")

## Read data
storm <- tbl_df(read.csv("./storm.csv.bz2", stringsAsFactors = FALSE, 
                          na.strings = c(NA, "")
                          )
                 )
## In case I mess up, use storm with backup
backup <- storm
glimpse(storm)

## Select only variables I'll need
storm <- storm %>%
    select(STATE, EVTYPE, FATALITIES:CROPDMGEXP)

## Clean up variable names
names(storm) <- tolower(names(storm))

## Failed attempt
with(storm, for(i in 1:nrow(storm)) {
    if(propdmgexp %in% c("K", "M", "B")){}
    else{propdmgexp <- NA}
})

## Remove any unusable multipliers
good <- c("K", "k", "M", "m", "B", "b", NA) 
gProp <- storm$propdmgexp %in% good
gCrop <- storm$cropdmgexp %in% good
storm$propdmgexp[!gProp] <- NA
storm$cropdmgexp[!gCrop] <- NA
storm <- mutate(storm, propdmgexp = tolower(propdmgexp),
        cropdmgexp = tolower(propdmgexp))

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
## Across the United States, which types of events (as indicated in the EVTYPE 
## variable) are most harmful with respect to population health?



## Across the United States, which types of events have the greatest economic 
## consequences?
