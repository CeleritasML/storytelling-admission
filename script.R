if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, here, styler,
  hrbrthemes, ggthemes, ggtext,
  glue, patchwork, skimr, janitor,
  waffle
)

gc()
rm(list = ls())

dat <- read_csv("SummerStudentAdmissions2.csv") |>
  janitor::clean_names()

glimpse(dat)
skim(dat)

# 8 x 88 not a large data set

sum(is.na(dat)) # 5 NAs, but multiple NAs appear in the same record


# decision (1 NA, 1 "Banana"?)
unique(dat$decision)
sum(is.na(dat$decision))

dat <- dat %>%
  filter(decision != "Banana") |>
  drop_na(decision)

# state (complete but need cleaning)
unique(dat$state)
dat <- dat %>%
  mutate(state = case_when(
    state == "mississippi" ~ "Mississippi",
    state == "virginia" ~ "Virginia",
    TRUE ~ state
  ))

# gpa (1 NA)
dat |>
  arrange(desc(gpa))

dat <- dat |>
  filter(gpa <= 4) |>
  drop_na(gpa)


# work_exp (complete)

dat |>
  arrange(desc(work_exp))

# remove outlier with 100 in work_exp
dat <- dat |>
  filter(work_exp != 100)

# test_score (1 NA)
# actually the NA has been removed from previous operations
# nothing to clean here?
summary(dat$test_score)

# writing_score (complete, but ...)
summary(dat$writing_score)
dat |>
  arrange(writing_score)
# it could be an outlier. Although it has the minimum in writing_score,
# it has an "admit" as the decision. Similarly for the record where
# writing_score==11
dat <- dat |>
  filter(!writing_score %in% c(1, 11))

# gender (2 NA)
# actually we need more information about the gender
# encoding. male = 1? female = 0?
unique(dat$gender)
# for now, we will keep -1 and NA as -1 as they are not disclosed
dat <- dat |>
  mutate(gender = if_else(!gender %in% c(0, 1), -1, gender))

# volunteer_level (complete)
summary(dat$volunteer_level)

glimpse(dat)

# > glimpse(dat)
# Rows: 81
# Columns: 8
# $ decision        <chr> "Admit", "Admit", "Admit", "Admit",…
# $ state           <chr> "California", "Florida", "Colorado"…
# $ gpa             <dbl> 3.90, 3.80, 3.60, 3.92, 3.88, 3.70,…
# $ work_exp        <dbl> 6.7, 1.4, 0.9, 1.2, 1.5, 1.2, 4.7, …
# $ test_score      <dbl> 962, 969, 969, 969, 967, 969, 961, …
# $ writing_score   <dbl> 100, 97, 97, 95, 95, 94, 93, 94, 91…
# $ gender          <dbl> 1, 1, 0, -1, 1, 1, 1, 1, -1, 1, 1, …
# $ volunteer_level <dbl> 0, 4, 2, 3, 5, 2, 1, 0, 3, 4, 2, 3,…

write_csv(dat, "ssa-cleaned.csv")

# ------------------------------------------------------------
# we need to decide what type of stories we want to share with
# the stokeholders (?) who we are presenting to...

# 0. overall decision

dat |>
  count(decision) -> dat_df

ggplot(dat_df, aes(fill = decision, values = n)) +
  geom_waffle(color = "white", size = 1.125, n_rows = 9, flip = TRUE) +
  scale_fill_manual(
    name = NULL,
    values = c("#a40000", "#c68958", "#ae6056"),
    labels = c("Admit", "Decline", "Waitlist")
  ) +
  coord_equal() +
  theme_fivethirtyeight() +
  # scale_y_continuous(
  #   limits = c(.4, .7),
  #   breaks = c(.5, .55, .6, .65, .7),
  #   # limits = c(.4, .9), # for 21 age groups
  #   # breaks = c(.5, .6, .7, .8), # for 21 age groups
  #   labels = scales::percent_format(accuracy = 1)
  # ) + # play around with the range of y-axis (or should I call it "x-axis"?)
  # scale_x_continuous(breaks = seq(0, 80, 10)) +
  theme(
    plot.background = element_rect(fill = "#F9EFE6", color = "#F9EFE6"),
    panel.background = element_rect(fill = "#F9EFE6", color = "#F9EFE6"),
    legend.position = "bottom",
    legend.background = element_rect(fill = "#F9EFE6", color = "#F9EFE6"),
    legend.key = element_rect(fill = "#F9EFE6", color = "#F9EFE6"),
    legend.title = element_text(size = 7, color = "#000000"),
    legend.text = element_text(size = 7, color = "#000000"),
    axis.title.y = element_text(
      vjust = 0.2, size = 14,
      family = "IBM Plex Sans", face = "bold"
    ), # repositioning
    axis.title.x = element_text(
      hjust = 0.5, size = 14,
      family = "IBM Plex Sans", face = "bold"
    ), # repositioning
    axis.text.x = element_text(size = 6),
    axis.text.y = element_text(size = 6, angle = 30),
    text = element_text(
      family = "IBM Plex Sans",
      color = "#3B372E"
    ),
    plot.title = element_text(
      family = "IBM Plex Sans", face = "bold", size = 20,
      hjust = 0.5
    ),
    plot.subtitle = element_markdown(
      family = "Roboto Slab", size = 12.4,
      hjust = 0.5
    ),
    plot.caption = element_markdown(size = 9, family = "Roboto Condensed"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(20, 20, 10, 10),
    strip.background = element_rect(fill = "#D8C8B3"),
    strip.text = element_text(
      family = "Roboto Slab", face = "bold", size = 13,
      color = "#654321"
    )
  ) +
  labs(
    title = "TITLE HERE",
    subtitle = "**SOMETHING HERE**<br><span style='color:#4682b4;'>**HERE**</span> vs <span style='color:#D51D38;'>**THERE**</span> AND THERE",
    caption = glue("AND <span style='color:#D8B500;'>**YELLOW**</span> SOMETHING.<br><br>
                   DATA FROM: **SummerStudentAdmission2.csv**"),
    x = "X (%)",
    y = "Y (%)"
  )

# 1. applicants by states




style_file("script.R")
