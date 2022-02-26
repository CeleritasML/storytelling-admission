if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, here, styler, skimr, janitor
)

gc()
rm(list = ls())

dat <- read_csv(here("data/SummerStudentAdmissions2.csv")) |>
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

write_csv(dat, here("data/data-cleaned.csv"))

style_file("cleaning-script.R")
