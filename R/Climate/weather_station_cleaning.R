precipitation2020 <- precipitation %>% 
  mutate_at(c("logger", "site"), factor) %>% 
  filter(
    # between(.$date,"2019-10-01 00:00:01", "2020-10-01 00:00:01")
    date >= "2019-10-01 00:00:00"
    & date < "2020-10-01 00:00:00"
    # & logger %in% c("counter", "counter1")
    & logger %in% c("rain", "nedbor")
    & site %in% c("lav", "ulv", "gud", "skj") 
    ) %>% 
  pivot_wider(names_from = logger, values_from = value)
  # rename(precipitation = value)
# summary(precipitation2020)
precipitation %>%
  filter(date < "2020-10-01 00:00:00") %>% 
  filter(logger == "rain") %>%
  ggplot(aes(x = date, y = value)) +
  geom_line() +
  facet_wrap(~ site)