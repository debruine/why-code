# create data to be plotted

dat <- readxl::read_xlsx("data/PopulationAgeSex-20220430074449.xlsx", sheet = "Data", skip = 1) %>%
  filter(Sex != "Both sexes combined",
         Location %in% c("More developed regions", "Less developed regions")) %>%
  mutate(age = factor(Age, unique(Age)), sex = Sex) %>%
  select(-1, -Note, -Age, -Sex) %>%
  pivot_longer(cols = `1950`:`2100`,
               names_to = "year",
               values_to = "pop",
               names_transform = list(year = as.integer)) %>%
  group_by(sex, year, age) %>%
  summarise(pop = sum(pop), .groups = "drop") %>%
  group_by(year) %>%
  mutate(pcnt = pop / sum(pop)) %>%
  ungroup()

write_csv(dat, "data/pop_data.csv")

dat2 <- readxl::read_xlsx("data/PopulationAgeSex-20220430074449.xlsx", sheet = "Data", skip = 1) %>%
  filter(Sex != "Both sexes combined",
         Location %in% c("More developed regions", "Less developed regions")) %>%
  mutate(Age = factor(Age, unique(Age))) %>%
  select(-1, -Note) %>%
  pivot_longer(cols = `1950`:`2100`,
               names_to = "year",
               values_to = "pop",
               names_transform = list(year = as.integer)) %>%
  group_by(year, Location) %>%
  mutate(pcnt = pop / sum(pop)) %>%
  ungroup() %>%
  select(-pop) %>%
  pivot_wider(names_from = Sex, 
              values_from = pcnt)

write_csv(dat2, "data/both_pop_data.csv")


anim2 <- ggplot(dat2, mapping = aes(x = Age)) +
  facet_wrap(~Location, nrow = 1) +
  geom_col(aes(y = Female), 
           fill = "hotpink3") +
  geom_col(aes(y = -Male), 
           fill = "dodgerblue3") +
  coord_flip(ylim = c(-.08, .08)) +
  labs(title = "Population by Age and Gender: {floor(frame_time/5)*5}",
       x = NULL, y = NULL) +
  scale_y_continuous(
    breaks = seq(-.08, .08, .02),
    labels = abs(seq(-8, 8, 2)) |> paste0("%")) +
  annotate("text", label = "Female", size = 8, 
           color = "hotpink3", x = 20, y = .05) +
  annotate("text", label = "Male", size = 8, 
           color = "dodgerblue3", x = 20, y = -.05) +
  theme(strip.text = element_text(size = 15)) +
  gganimate::transition_time(year)

frames <- unique(pop_data$year) %>% length()

gganimate::anim_save("images/anim2.gif", 
                     animation = anim2, 
                     nframes = frames*5, fps = 10, 
                     width = 12, height = 8, 
                     units = "in", res = 150)
