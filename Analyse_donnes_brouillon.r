library("tidyverse")


########## Useful functions ##########
Count_NA_line <- function(df){
  df %>% 
    is.na %>% 
    rowSums()
}
########## Import phenotype data ##########
Offspring_phenotype <- read.table("C:/Users/Basile Pajot/Documents/Pro/Thèse/Projet/Manips/Manip_sexualselection1/Data/Offspring_phenotypes.tsv",
                                  sep = "\t", header = TRUE)

# Separate males and females
Offspring_males <- Offspring_phenotype %>% 
  filter(Sex == "M")



########## Look at missing data  ##########
nb_NA <- Offspring_males %>% 
  is.na() %>% 
  rowSums()

nb_col <- ncol(Offspring_males)

(Offspring_males %>% 
  select(starts_with(paste0("P", 1:7))) %>% 
  Count_NA_line() / nb_col) > 0.95

Offspring_males %>% 
  drop_na() %>% 
  nrow

# We decide to remove all lines with NA
Offspring_males_without_NA <- Offspring_males %>% 
  drop_na()

########## Look at the data  ##########

Offspring_males_without_NA %>% 
  summary()

column_names <- colnames(Offspring_males_without_NA)
position_of_superiors <- which(grepl(">", Offspring_males_without_NA))


Offspring_males_without_NA %>% 
  mutate(across(column_names[position_of_superiors], ~ str_remove_all(., ">")),
         across(column_names[position_of_superiors], ~ as.numeric(.))) %>% 
  summary()
