library(tidyverse)

set.seed(123)

bin_tb <- rbinom(n = 600, 
                 size = 1, 
                 prob = 0.65) %>% 
  matrix(nrow = 30)


bin_tb %>% 
  as_tibble %>% 
  set_names(paste0("Q",1:20)) -> q_dt


# Approach #1: using map()

q_dt %>% 
  map(~mean(.x)) %>% 
  as_tibble %>% 
  t %>% 
  as.data.frame %>% 
  rownames_to_column(var = "Items") %>%  
  rename(DI = V1)

# Approach #2: using map() alternative
q_dt %>% 
  map(~mean(.x) %>% as_tibble) %>% 
  bind_rows(.id = "Items") %>% 
  rename(DI = value)

# Approach #3: using pivot_longer() and reframe() alternative

q_dt %>% 
  pivot_longer(everything()) %>% 
  reframe(across(value, list(DI = mean), .names = "{.fn}"), .by = name) %>% 
  rename(Item = name)

# Labeling rows in terms of the value of a column (DI)

q_dt %>% 
  map(~mean(.x) %>% as_tibble) %>% 
  bind_rows(.id = "Items") %>% 
  rename(DI = value) %>% 
  mutate(Remark = ifelse(DI > 0.80, "Easy", 
                         ifelse(DI < 0.3, "Difficult", "Moderate"))) %>% 
  mutate(across(DI, ~sprintf("%.2f",.x)))

# just change the path to the data below (otherwise, there will an error)

readxl::read_xlsx("C:\\Users\\reycu\\Downloads\\pre-test.xlsx") %>% 
  # map(~janitor::tabyl(.x))
  select(!c(1,last_col())) %>% 
  # print(n = 100)
  slice(1:42) %>% 
  map(~mean(.x) %>% as_tibble) %>% 
  bind_rows(.id = "Items") %>% 
  rename(DI = value) %>% 
  mutate(Remark = ifelse(DI > 0.80, "Easy", 
                         ifelse(DI < 0.3, "Difficult", "Moderate"))) %>% 
  mutate(across(DI, ~sprintf("%.2f",.x))) -> actual_dt


actual_dt %>% 
  mutate(Remark = factor(Remark,
                         levels = c("Easy","Moderate","Difficult"))) %>% 
  count(Remark) %>% 
  mutate(Percentage  = 100*n/sum(n)) %>% 
  rename(Freq = n, "Percentage (%)" = 3)

actual_dt %>% 
  mutate(Remark = factor(Remark, levels = c("Easy","Moderate","Difficult"))) %>%
  janitor::tabyl(Remark) %>% 
  mutate(percent  = 100*percent) %>% 
  rename(Freq = n, "Percentage (%)" = 3)

actual_dt %>% 
  select(3) %>% 
  gtsummary::tbl_summary() %>% 
  gtsummary::bold_labels() %>% 
  gtsummary::italicize_levels() %>% 
  gtsummary::as_flex_table()
  


