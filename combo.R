## What events types are costliest to human health and economic well-being?

## Prerequisites: storm.R, health.R, econ.R

healthCombo <- health %>%
    group_by(evtype) %>%
    summarize(total = sum(total),
              avg = mean(avg)) %>%
    ungroup()
econCombo <- econ %>%
    group_by(evtype) %>%
    summarize(total = sum(total),
              avg = mean(avg)) %>%
    ungroup()
healthComboTotal <-healthCombo %>%
    arrange(desc(total)) %>%
    head(3)
healthComboTotal
healthComboAvg <-healthCombo %>%
    arrange(desc(avg)) %>%
    head(3)
healthComboAvg
econComboTotal <- econCombo %>%
    arrange(desc(total)) %>%
    head(3)
econComboTotal
econComboAvg <- econCombo %>%
    arrange(desc(avg)) %>%
    head(3)
econComboAvg

## Plot all the things

c1 <- ggplot(healthComboTotal, aes(x = reorder(evtype, -total), y = total)) +
    geom_bar(stat = "identity") +
    labs(x = "Event Type", y = "Total Fatalities/Injuries", 
         title = "Highest Human Health Consequences")
c2 <- ggplot(healthComboAvg, aes(x = reorder(evtype, -avg), y = avg)) +
    geom_bar(stat = "identity") +
    labs(x = "Event Type", y = "Mean Fatalities/Injuries",
         title = "Average Human Health Consequences per Event")
c3 <- ggplot(econComboTotal, aes(x = reorder(evtype, -total), y = total)) +
    geom_bar(stat = "identity") +
    labs(x = "Event Type", y = "Total Damage (dollars)",
         title = "Highest Economic Damage")
c4 <- ggplot(econComboAvg, aes(x = reorder(evtype, -avg), y = avg)) +
    geom_bar(stat = "identity") +
    labs(x = "Event Type", y = "Mean Damage (dollars)",
         title = "Average Economic Damage per Event")
grid.arrange(arrangeGrob(c1, c2, c3, c4, 
                         main = "Health and Economic Consequences"))