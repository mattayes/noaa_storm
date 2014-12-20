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