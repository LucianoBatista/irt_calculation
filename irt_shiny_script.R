library(tidyverse)
library(randomNames)

# 95 items, 2460 respondents
items_full_tbl <- read_csv("lenguages_human_science_example.csv")

# only items
items_tbl <- items_full_tbl %>% 
  dplyr::select(starts_with("item"))

# only students
student_vect_ids <- items_full_tbl %>%
  pluck(2)

n_students <- length(student_vect)

student_vect <- randomNames(n_students, which.names = "first")

# activity code
activity_code <- items_full_tbl %>% 
  pluck(1)

# running 3PL model
threePL_model <- mirt(
  items_tbl, 
  1,          
  itemtype = '3PL')

# plots model specifics
# ICC for each item
plot(threePL_model, type = 'trace')
# Item information, separate plots
plot(threePL_model, type = 'infotrace')
# Item information, joint plot
plot(threePL_model, type = 'infotrace', facet_items = F)


# item fit: we can parametrize the index
# the user will choose each item to see
ItemPlot <- itemfit(
  mymodel, 
  group.bins=15,
  empirical.plot = 1,
  empirical.CI = .95,
  method = 'ML'
)
print(ItemPlot)


# latent scores
latent_answers <- as.vector(fscores(threePL_model))

# IRT coefficients
irt_coefs <- coef(mymodel, simplify = T, IRTpars =T)

# New Scores
new_score <- score.transform(
  latent_answers, mu.new = 500, sd.new = 100,
  normalize = FALSE
)

# data transformations
items_name <- names(items_tbl)

# irt coefs tibble
irt_coefs_tbl <- irt_coefs$items %>% 
  as_tibble()

# max len of rows
max_n <- dim(items_tbl[][1])[1]

# putting together some informations
items_tbl["student_code"] <- student_vect
irt_coefs_tbl["item"] <- items_name


items_longer_format <- items_tbl %>% 
  pivot_longer(cols = -student_code, names_to = "items", values_to = "count")

scores_all_students <- new_score[1]$new.scores[1:max_n]

scores_tbl <- enframe(scores_all_students) %>% 
  rename(student_code = name,
         score = value) %>% 
  mutate(student_code = as_factor(student_vect))

# scores_tbl to export
file_name  <- paste0(as.character(activity_code[1]), "_activiy.csv")
scores_tbl %>% 
  arrange(desc(score)) %>% 
  bind_cols("activity_code" = activity_code) %>% 
  write_csv(file_name)

# plotssss ----

# we go to parametrize this data
# user to choose the number of students
n <- 10
top_n <- scores_tbl %>% 
  mutate(student_code = student_code %>% as_factor()) %>% 
  arrange(desc(score)) %>% 
  top_n(n) %>% 
  pluck(1)

library(echarts4r)

# choosing the top n coef item
top_n_items_most_difficult <- irt_coefs_tbl %>% 
  select(b, item) %>% 
  arrange(desc(b)) %>% 
  slice_max(b, n = 10) %>% 
  pluck(2)

top_n_items_less_difficult <- irt_coefs_tbl %>% 
  select(b, item) %>% 
  arrange(desc(b)) %>% 
  slice_min(b, n = 6) %>% 
  pluck(2)

items_longer_experimental <- items_longer_format %>% 
  mutate(top_n_difficult = case_when(
    (items %in% top_n_items_most_difficult) & (count == 1) ~ 1,
    TRUE ~ 0
  )) %>% 
  mutate(top_n_easy = case_when(
    (items %in% top_n_items_less_difficult) & (count == 1) ~ "correct",
    (items %in% top_n_items_less_difficult) & (count == 0) ~ "wrong",
    TRUE ~ "not_important"
  ))


p1 <- items_longer_format %>% 
  filter(student_code %in% top_n) %>% 
  mutate(items = as_factor(items),
         student_code = as_factor(student_code)) %>% 
  ggplot(aes(x = items,
             y = student_code,
             fill = count == 1)) +
  geom_raster() +
  # scale_fill_manual(values = c("#e0e0e0", "orange")) +
  # scale_y_discrete(name="Students", limits=c(2000:2100)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle=45)
  )


p2 <- scores_tbl %>% 
  mutate(student_code = student_code %>% as_factor()) %>% 
  arrange(desc(score)) %>% 
  top_n(10) %>% 
  ggplot(aes(x = score,
             y = student_code %>% fct_reorder(score))) + 
  geom_bar(stat="identity", width=.7, fill="orange") +
  labs(
    title = "Classfication by score",
    y = "Students IDs",
    x = "Scores"
  ) +
  theme_minimal() 


p3 <- items_longer_format %>% 
  # filter(student_code %in% c(1:max_n)) %>% 
  group_by(student_code) %>% 
  summarise(total_corrects = sum(count)) %>% 
  ungroup() %>%
  arrange(desc(total_corrects)) %>% 
  top_n(10) %>% 
  mutate(student_code = as_factor(student_code)) %>% 
  ggplot(aes(x = total_corrects,
             y = student_code %>% fct_reorder(total_corrects))) +
  geom_bar(stat="identity", width = .7, fill="orange") +
  labs(
    x = "Quantity of Correct Answer",
    y = "",
    title = "Classification by correct answers"
  ) +
  theme_minimal()

p4 <- irt_coefs_tbl %>% 
  dplyr::select(-c(u, a, g)) %>%
  mutate(item = as_factor(item)) %>% 
  # filter(item != "item_3") %>% 
  ggplot(aes(x = b,
             y = item %>% fct_reorder(b))) +
  geom_bar(stat="identity", width = .7, fill = "orange") +
  theme_minimal() +
  labs(
    x = "b = Difficulty",
    y = "Items",
    title = "Items by order of difficulty"
  )

p2 | p3

library(echarts4r)

items_longer_format %>%
  filter(student_code %in% top_n) %>%
  e_charts(items) %>%
  e_heatmap(student_code, count, blurSize = 7, pointSize = 5) %>%
  e_visual_map(count, inRange = list(color = c("#F24C3D", "#4E8BBF"))) %>%
  e_title("Heatmap") %>%
  e_tooltip(trigger = "item",
            formatter = htmlwidgets::JS(
              "function(params){
                return('<strong> Student Name: </strong>' + params.value[1] + '<br /> <strong>Item: </strong>' + params.value[0])
              }"
            )) %>%
  e_legend(FALSE)


e_tooltip(formatter = htmlwidgets::JS("
      function(params){
        return('wt: ' + params.value[0] + '<br />mpg: ' + params.value[1])
      }
    ")
)

items_longer_experimental %>%
  filter(student_code %in% top_n) %>%
  e_charts(items) %>%
  e_heatmap(student_code, top_n_difficult, blurSize = 7, pointSize = 5) %>%
  e_visual_map(top_n_difficult, inRange = list(color = c("#081A40", "#F24C3D"))) %>%
  e_title("Heatmap") %>%
  e_tooltip(trigger = "item",
            formatter = htmlwidgets::JS(
              "function(params){
                return('<strong> Student Name: </strong>' + params.value[1] + '<br /> <strong>Item: </strong>' + params.value[0])
              }"
            )) %>%
  e_legend(FALSE)


e_tooltip(formatter = htmlwidgets::JS("
      function(params){
        return('wt: ' + params.value[0] + '<br />mpg: ' + params.value[1])
      }
    ")
)


items_longer_format %>% 
  # filter(student_code %in% c(1:max_n)) %>% 
  group_by(student_code) %>% 
  summarise(total_corrects = sum(count)) %>% 
  ungroup() %>%
  arrange(desc(total_corrects)) %>% 
  top_n(n) %>% 
  mutate(student_code = as_factor(student_code)) %>% 
  ggplot(aes(x = total_corrects,
             y = student_code %>% fct_reorder(total_corrects))) +
  geom_col() +
  geom_label(
    aes(label = total_corrects),
    hjust = 1,
    nudge_x = -1,
    size = 4,
    fontface = "bold",
    family = "Fira Sans",
    fill = "white",
    label.size = 0
  ) +
  labs(
    x = "Quantity of Correct Answer",
    y = "",
    title = "Classification by correct answers"
  ) +
  theme_minimal()


items_longer_format %>%
  filter(student_code %in% top_n) %>%
  mutate(items = items %>% parse_number() %>%  as_factor(),
         student_code = as_factor(student_code)) %>%
  ggplot(aes(x = items,
             y = student_code,
             fill = count == 1)) +
  geom_tile(color = "white") +
  scale_fill_manual(values = c("#D94E4E", "#4E8BBF")) +
  theme_minimal()