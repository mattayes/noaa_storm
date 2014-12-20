## NOAA storm data analysis

## Packages
library(tidyr)
library(dplyr)
library(ggplot2)
library(gridExtra)

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

## Gather fatalies and injuries into injType, propdgm and cropdmg into dmgType
storm <- storm %>%
    gather(dmgType, dmgCount, -(state:injuries)) %>%
    gather(injType, injCount, -(state:evtype), -(dmgType:dmgCount))

## Across the United States, which types of events (as indicated in the EVTYPE 
## variable) are most harmful with respect to population health?

## Subset injury data and summarize
health <- storm %>%
    group_by(evtype, injType) %>%
    summarize(total = sum(injCount, na.rm = TRUE),
              avg = mean(injCount, na.rm = TRUE)
    ) %>%
    ungroup()
health

## Top fatalities? Total vs. mean
fatTotal <- health %>%
    filter(injType == "fatalities") %>%
    arrange(desc(total)) %>%
    head(5)
fatTotal
fatAvg <- health %>%
    filter(injType == "fatalities") %>%
    arrange(desc(avg)) %>%
    head(5)
fatAvg

## Top injuries? Total vs. mean
injTotal<- health %>%
    filter(injType == "injuries") %>%
    arrange(desc(total))  %>%
    head(5)
injTotal
injAvg <- health %>%
    filter(injType == "injuries") %>%
    arrange(desc(avg)) %>%
    head(5)
injAvg

## Plot all four graphics together
h1 <- ggplot(fatTotal, aes(x = reorder(evtype, -total), y = total)) +
    geom_bar(stat = "identity") +
    labs(x = "Event Type", y = "Total Fatalities", title = "Highest Fatalities")
h2 <- ggplot(fatAvg, aes(x = reorder(evtype, -avg), y = avg)) +
    geom_bar(stat = "identity") +
    labs(x = "Event Type", y = "Mean Fatalities",
         title = "Average Fatalities per Event")
h3 <- ggplot(injAvg, aes(x = reorder(evtype, -total), y = total)) +
    geom_bar(stat = "identity") +
    labs(x = "Event Type", y = "Total Injuries", title = "Highest Injuries")
h4 <- ggplot(injAvg, aes(x = reorder(evtype, -avg), y = avg)) +
    geom_bar(stat = "identity") +
    labs(x = "Event Type", y = "Mean Injuries", 
         title = "Average Injuries per Event")
grid.arrange(arrangeGrob(h1, h2, h3, h4, main = "Effects on Population Health"))

## Across the United States, which types of events have the greatest economic 
## consequences?

econ <- storm %>%
    group_by(evtype, dmgType) %>%
    summarize(total = sum(dmgCount, na.rm = TRUE),
              avg = mean(dmgCount, na.rm = TRUE)
    ) %>%
    ungroup()
econ

## Top fatalities? Total vs. mean
propTotal <- econ %>%
    filter(dmgType == "propdmg") %>%
    arrange(desc(total)) %>%
    head(5)
propTotal
propAvg <- econ %>%
    filter(dmgType == "propdmg") %>%
    arrange(desc(avg)) %>%
    head(5)
propAvg

## Top injuries? Total vs. mean
cropTotal<- econ %>%
    filter(dmgType == "cropdmg") %>%
    arrange(desc(total))  %>%
    head(5)
cropTotal
cropAvg <- econ %>%
    filter(dmgType == "cropdmg") %>%
    arrange(desc(avg)) %>%
    head(5)
cropAvg

## Plot all four graphics together
e1 <- ggplot(propTotal, aes(x = reorder(evtype, -total), y = total)) +
    geom_bar(stat = "identity") +
    labs(x = "Event Type", y = "Total (dollars)", 
         title = "Highest Property Damage")
e2 <- ggplot(propAvg, aes(x = reorder(evtype, -avg), y = avg)) +
    geom_bar(stat = "identity") +
    labs(x = "Event Type", y = "Mean (dollars)",
         title = "Average Property Damage per Event")
e3 <- ggplot(cropTotal, aes(x = reorder(evtype, -total), y = total)) +
    geom_bar(stat = "identity") +
    labs(x = "Event Type", y = "Total (dollars)",
         title = "Highest Crop Damage")
e4 <- ggplot(cropAvg, aes(x = reorder(evtype, -avg), y = avg)) +
    geom_bar(stat = "identity") +
    labs(x = "Event Type", y = "Mean (dollars)",
         title = "Average Crop Damage per Event")
grid.arrange(arrangeGrob(e1, e2, e3, e4, main = "Economic Consequences"))
