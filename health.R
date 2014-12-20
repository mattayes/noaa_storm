## Across the United States, which types of events (as indicated in the EVTYPE 
## variable) are most harmful with respect to population health?

## Subset injury data and summarize
harm <- storm %>%
    group_by(evtype, injType) %>%
    summarize(total = sum(injCount, na.rm = TRUE),
              avg = mean(injCount, na.rm = TRUE)
    ) %>%
    ungroup()
harm

## Top fatalities? Total vs. mean
fatTotal <- harm %>%
    filter(injType == "fatalities") %>%
    arrange(desc(total)) %>%
    head(5)
fatTotal
fatAvg <- harm %>%
    filter(injType == "fatalities") %>%
    arrange(desc(avg)) %>%
    head(5)
fatAvg

## Top injuries? Total vs. mean
injTotal<- harm %>%
    filter(injType == "injuries") %>%
    arrange(desc(total))  %>%
    head(5)
injTotal
injAvg <- harm %>%
    filter(injType == "injuries") %>%
    arrange(desc(avg)) %>%
    head(5)
injAvg

## Plot all four graphics together
a <- ggplot(fatTotal, aes(x = reorder(evtype, -total), y = total)) +
    geom_bar(stat = "identity") +
    labs(x = "Event Type", y = "Total Fatalities", title = "Highest Fatalities")
b <- ggplot(fatAvg, aes(x = reorder(evtype, -avg), y = avg)) +
    geom_bar(stat = "identity") +
    labs(x = "Event Type", y = "Mean Fatalities",
         title = "Average Fatalities per Event")
c <- ggplot(injAvg, aes(x = reorder(evtype, -total), y = total)) +
    geom_bar(stat = "identity") +
    labs(x = "Event Type", y = "Total Injuries", title = "Highest Injuries")
d <- ggplot(injAvg, aes(x = reorder(evtype, -avg), y = avg)) +
    geom_bar(stat = "identity") +
    labs(x = "Event Type", y = "Mean Injuries", 
         title = "Average Injuries per Event")
grid.arrange(arrangeGrob(a, b, c, d, main = "Effects on Population Health"))