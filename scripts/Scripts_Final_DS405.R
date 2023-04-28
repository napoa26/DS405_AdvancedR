###################################################
##### ----- DS 405 FINAL PROJECT SCRIPT ----- #####
###################################################


##### Libraries #####

library(tidyverse)
library(ggplot2)
library(likert)
library(plotly)


##### Read-in Project Data #####

## Set the Working Directory ##

setwd("/Users/student/Desktop/Alii/DS405_AdvancedR/data/csv_data/")

## Read-in "demographic-survey" Data ##

Master <- read.csv("demographic-survey-5919021704675328.csv")
View(Master)
names(Master)


##### Manipulate Project Data #####

## Subset "Master" csv as "demo_raw" ##

demo_raw <- (Master %>% select(2, 12, 13, 14, 18, 20, 21, 22, 23, 24))

## Rename "demo_raw" Columns ##

demo_raw <- (demo_raw %>% rename("kids" = "do_you_have_any_children_living_at_home.",
                "eth_id" = "how_important_do_you_consider_your_ethnicity_as_a_part_of_your_self_image.",
                "rel_id" = "how_important_do_you_consider_your_religious_identity_as_a_part_of_your_self_image.",
                "ed_lev" = "what_is_the_highest_level_of_education_you_completed..",
                "dob" = "what_is_your_date_of_birth.",
                "eth" = "what_is_your_ethnicity.",
                "gen" = "what_is_your_gender.",
                "mar" = "what_is_your_marital_status.",
                "rel" = "what_is_your_religion"))
View(demo_raw)

## Remove duplicate rows from "demo_raw" as "demo", based upon "user_id" and "ed_lev" ##


demo <- (demo_raw %>% distinct(user_id, ed_lev, .keep_all = TRUE))


##### Plotting #####


plot <- ggplot(data = demo, aes(x = ed_lev, fill = kids)) +
  geom_bar(stat = "count", position = "dodge") +
  labs(title = "title", 
       x = "Level of Education",
       y = "Count") +
  theme_minimal() +
  facet_grid(~rel_id)+
  coord_flip()

ggplotly(plot)

ed_values <- c("post_graduate", "college_or_university",
               "secondary/high_school", "technical_school", 
               "primary", "none", "prefer_not_to_answer.")






set.seed(8675309)
df1 <- demo$kids(
  x=rep(ed_values, 2),
  type=rep(c("Yes", "No"), each = 7),
  y=sample(1:15, 12, replace=TRUE)
)
df$x <- factor(df$x, levels=x_values)

d_myline <- data.frame(
  x=x_values,
  rando=c(1,5,6,10,4,6)
)

p <- ggplot(df, aes(x,y)) + 
  geom_col(aes(fill=type), position="dodge", width=0.5)




##### Likert Analysis #####

## "eth_id" and "rel_id" only ##





likert_values <- likert(demo)
plot(likert_values)



likert_values <- likert(demo$eth_id[, "eth_id", drop = FALSE])
plot(likert_values)

plot(likert_responses,
     type = "heat",
     low.color = "white",
     high.color = "blue",
     text.color = "black",
     text.size = 4,
     wrap = 50)



food2 <- distinct(food2, user_id, .keep_all = TRUE) %>% 
  select(user_id, how_affordable_is_produce_in_your_neighborhood.) %>% 
  mutate(How_affordable_is_produce_in_your_neighborhood = 
           recode(how_affordable_is_produce_in_your_neighborhood.,
                  "very_unaffordable" = 1, "unaffordable" = 2, 
                  "affordable" = 3, "very_affordable" = 4)) %>% 
  select(-how_affordable_is_produce_in_your_neighborhood.)












