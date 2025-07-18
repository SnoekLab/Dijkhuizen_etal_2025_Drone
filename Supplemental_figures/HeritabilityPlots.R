# This script creates the supplementary heritability plots

library(ggplot2)
library(readxl)
library(tidyr)
library(dplyr)
library(stringr)

#Get phenotypes for boxplot
heritabilities <- read_excel("D:/Drone-paper/Supplemental_data.xlsx", sheet = "Heritability")
heritabilities$phenotype <- sapply(strsplit(heritabilities$Pheno.names, "\\."), `[`, 1)
heritabilities <- heritabilities[heritabilities$phenotype != "ndvi2", ] #remove ndvi2 as it is redundant.

herit.long <- heritabilities %>%
  pivot_longer(cols = c(Day78, Day93, Ratio),
               names_to = "Measurement",
               values_to = "Value")



phenotypes <- ggplot(herit.long, aes(x = Measurement, y = Value)) +
  geom_boxplot(outlier.size = 0.5) +
  facet_wrap(~ phenotype, scales = "free_y") +
  ylim(0, 100) +
  theme_minimal() +
  ylab("Heritability") +
  xlab("") +
  theme_bw()
phenotypes

ggsave("Script_per_figure/Figures/PhenoHeritabilities.png", plot = phenotypes, width = 7.5, height = 5, dpi = 300)

# Now create per statistic.
heritabilities <- heritabilities %>%
  mutate(statistic = case_when(
    str_detect(Pheno.names, "mean$") ~ "mean",
    str_detect(Pheno.names, "trimmed") ~ "trimmed.mean",
    str_detect(Pheno.names, "Q") ~ "quantiles",
    str_detect(Pheno.names, "SD") ~ "SD",
    str_detect(Pheno.names, "median") ~ "median",
    str_detect(Pheno.names, "skewness") ~ "skewness",
    str_detect(Pheno.names, "minimum") ~ "minimum",
    str_detect(Pheno.names, "maximum") ~ "maximum",
    str_detect(Pheno.names, "kurtosis") ~ "kurtosis",
    TRUE ~ NA_character_  # fallback if none matched
  ))

herit.long <- left_join(herit.long, heritabilities %>% select(Pheno.names, statistic), by = "Pheno.names")

statistics <- ggplot(herit.long, aes(x = Measurement, y = Value)) +
  geom_boxplot(outlier.size = 0.5) +
  facet_wrap(~ statistic) +
  scale_y_continuous(limits = c(0, 100)) +
  theme_minimal() +
  ylab("Heritability") +
  xlab("") +
  theme_bw()
statistics

ggsave("Script_per_figure/Figures/StatisticsHeritabilities.png", plot = statistics, width = 7.5, height = 5, dpi = 300)
