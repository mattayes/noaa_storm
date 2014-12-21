## Across the United States, which types of events have the greatest economic 
## consequences?

## Preqrequisite: storm.R

## Packages
library(ggplot2)
library(gridExtra)

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