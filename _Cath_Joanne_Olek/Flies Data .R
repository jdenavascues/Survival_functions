# Creating the dataframe
data <- data.frame(
  GENOTYPE = c(rep("daxx[NP4778]", 24), rep("Dahomey", 24)),
  FOOD = c( rep( c(rep( "Normal", 12), rep("HCr 50mM", 12) ), 2) ),
  SEX = c(rep("M", 6), rep("F", 6)
          "F", "F", "M", "M", "M",
          "M", "M", "F", "F", "F",
          "F", "F", "M", "M", "M",
          "M", "M", "F", "F", "F",
          "F", "F", "M", "M", "M",
          "M", "M", "F", "F", "F",
          "F", "F", "M", "M", "M",
          "M", "M", "F", "F", "F",
          "F", "F", "M", "M", "M",
          "M", "M", "F", "F", "F",
          "F", "F"),
  INFECTION = c("vehicle", "vehicle", "vehicle", "P.ent.", "P.ent.",
                "P.ent.", "vehicle", "vehicle", "vehicle", "P.ent.",
                "P.ent.", "P.ent.", "vehicle", "vehicle", "vehicle",
                "P.ent.", "P.ent.", "vehicle", "vehicle", "vehicle",
                "P.ent.", "P.ent.", "vehicle", "vehicle", "vehicle",
                "P.ent.", "P.ent.", "vehicle", "vehicle", "vehicle",
                "P.ent.", "P.ent.", "vehicle", "vehicle", "vehicle",
                "P.ent.", "P.ent.", "vehicle", "vehicle", "vehicle",
                "P.ent.", "P.ent.", "vehicle", "vehicle", "vehicle"))

library(readxl)
survival_data_collection_template_ <- read_excel("~/flies/survival data collection template .xlsx")
View(survival_data_collection_template_)

t1 = read_excel("C:/Users/Laptop/Documents/flies/survival data collection template .xlsx")
colnames(t1) = c( colnames(t1)[1:5], 't1', 't2', 't3', 't4', 't5', 't6', 't7', 't8', 't9', 't10','t11', 't12', 't13', 't14', 't15', 't16')

library(tidyverse)
timpoints <- read_excel(
  "C:/Users/Laptop/Documents/flies/survival data collection template .xlsx",
  range='H3:W3',
  col_names = FALSE,
  col_types= 'date')
t1 = pivot_longer(t1,
                  cols=!c(1:5),
                  names_to = 'timepoint',
                  values_to='death_no')
t1 = mutate(t1, datetime = case_match(timepoint,
  't1' ~ timpoints[[1]],
  't2' ~ timpoints[[2]],
  't3' ~ timpoints[[3]],
  't4' ~ timpoints[[4]],
  't5' ~ timpoints[[5]],
  't6' ~ timpoints[[6]],
  't7' ~ timpoints[[7]],
  't8' ~ timpoints[[8]],
  't9' ~ timpoints[[9]],
  't10' ~ timpoints[[10]],
  't11' ~ timpoints[[11]],
  't12' ~ timpoints[[12]],
  't13' ~ timpoints[[13]],
  't14' ~ timpoints[[14]],
  't15' ~ timpoints[[15]],
  't16' ~ timpoints[[16]])
)
