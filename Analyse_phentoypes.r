########## Set working directory ##########
setwd("C:/Users/Basile Pajot/Documents/Pro/Thèse/Projet/Manips/Manip_sexualselection1/Analyse/Jaera_phenotypes/")

########## Load libraries ###########
libraries <- c("tidyverse", "FactoMineR", "factoextra")
if (!require("pacman")) install.packages("pacman")
pacman::p_load(char = libraries, character.only = TRUE)
rm(libraries)

########## Useful variables ##########
my_theme <- theme_bw() +
  theme(text = element_text(size = 20))
########## Useful functions ##########
is_numeric_in_character <- function(x){
  tot_na <- x %>% 
    as.numeric %>% 
    is.na %>% sum %>% 
    suppressWarnings()
  return(tot_na != length(x))
}

Import_phenotypes <- function(path){
  read.table(path,
             header = TRUE, sep = "\t") %>% 
    # Remove the superior signs (">") from the table
    mutate(across(everything(), ~ str_remove_all(., ">")),
           # Convert the commas to points to be usable in R
           across(everything(), ~ str_replace_all(., ",", ".")),
           # Convert columns that are supposed to have numbers into numeric values
           across(where(is_numeric_in_character), ~ as.numeric(.))) %>% 
    return()
}
########## Import data ##########
phenotypes <- Import_phenotypes("../../Data/Offspring_phenotypes.tsv") %>% 
  mutate(Family_level = "Offpsring")

parent_phenotypes <- Import_phenotypes("../../Data/Father_phenotypes.tsv") %>% 
  mutate(Family_level = "Fathers")

# Separate males and female individuals
males <- phenotypes %>% 
  filter(Sex == "M")

females <- phenotypes %>% 
  filter(Sex == "F")

########## Males ##########
##### Analysis of missing data #####
# Test taking out individuals that have missing values
males_without_missing <- males %>% 
  select(-c(Note, Sequencing, Extracted, Library_prepared, contains("date"), Size, Photo_ID)) %>% 
  filter(!is.na(Phenotype)) %>% 
  drop_na() %>% 
  inner_join(males %>% 
               select(Alcool_ID, contains("date"), Size),
             by = "Alcool_ID")

pca <- FactoMineR::PCA(males_without_missing %>% 
                         select(-c(Label, Mother_ID, Sex, Phenotype, Alcool_ID, contains("date"), Size, Family_level)), graph = F)



pca$eig %>% 
  as_tibble() %>% 
  rownames_to_column("Dimensions") %>% 
  mutate(Dimensions = Dimensions %>% 
           factor(levels = c(1:nrow(pca$eig)) %>% as.character)) %>% 
  rename(Explained_var = "percentage of variance") %>% 
  ggplot() +
  geom_col(aes(x = Dimensions, y = Explained_var), fill = "dodgerblue") +
  my_theme +
  scale_x_discrete(breaks = factor(1:10), 
                   limits = c(1:10) %>% as.character) +
  geom_line(aes(x = Dimensions, y = Explained_var), stat = "identity", group = 1, lwd = 1.05) +
  geom_point(aes(x = Dimensions, y = Explained_var), stat = "identity", size = 1.5) +
  labs(x = "Percentage of explained variance")
  
## Or
fviz_eig(pca, addlabels = T)
  
pca1 <- pca$eig[1, 2] %>% round(digits = 2)
pca2 <- pca$eig[2, 2] %>% round(digits = 2)
pca3 <- pca$eig[3, 2] %>% round(digits = 2)

pca$ind$coord %>% 
  cbind(males_without_missing) %>% 
  mutate(Plate = str_split_fixed(Label, "-", 2)[, 1]) %>% 
  ggplot() +
  geom_point(aes(x = Dim.1, y = Dim.2, color = Size), size = 3, alpha = 0.7) +
  my_theme +
  labs(x = paste0("Axis 1 (", pca1, "%)"),
       y = paste0("Axis 2 (", pca2, "%)"))

########## Fathers ##########
pca_parents <- parent_phenotypes %>% 
  drop_na() %>% 
  column_to_rownames("Label") %>% 
  select(-c(alcool_ID, Phenotype, size, Family_level)) %>% 
  FactoMineR::PCA(graph = TRUE)

fviz_eig(pca_parents, addlabels = T)

pca_parents$ind$coord %>% 
  as.data.frame() %>% 
  rownames_to_column("Label") %>% 
  left_join(parent_phenotypes, by = "Label") %>% 
  ggplot() +
  geom_point(aes(x = Dim.1, y = Dim.3, color = Phenotype), size = 3, alpha = 0.7) +
  my_theme

# Compare parents with offspring
columns_in_common <- colnames(males_without_missing)[which(colnames(males_without_missing) %in% colnames(parent_phenotypes))]

pca <- FactoMineR::PCA(males_without_missing %>% 
                         select(all_of(columns_in_common)) %>% 
                         rbind(parent_phenotypes %>% 
                                 select(all_of(columns_in_common)) %>% 
                                 drop_na()) %>% 
                         select(-c(Family_level, Phenotype)) %>% 
                         column_to_rownames("Label"),
                       graph = F)



pca$eig %>% 
  as_tibble() %>% 
  rownames_to_column("Dimensions") %>% 
  mutate(Dimensions = Dimensions %>% 
           factor(levels = c(1:nrow(pca$eig)) %>% as.character)) %>% 
  rename(Explained_var = "percentage of variance") %>% 
  ggplot() +
  geom_col(aes(x = Dimensions, y = Explained_var), fill = "dodgerblue") +
  my_theme +
  scale_x_discrete(breaks = factor(1:10), 
                   limits = c(1:10) %>% as.character) +
  geom_line(aes(x = Dimensions, y = Explained_var), stat = "identity", group = 1, lwd = 1.05) +
  geom_point(aes(x = Dimensions, y = Explained_var), stat = "identity", size = 1.5) +
  labs(x = "Percentage of explained variance")

## Or
fviz_eig(pca, addlabels = T)

pca1 <- pca$eig[1, 2] %>% round(digits = 2)
pca2 <- pca$eig[2, 2] %>% round(digits = 2)
pca3 <- pca$eig[3, 2] %>% round(digits = 2)

pca$ind$coord %>% 
  as.data.frame() %>% 
  rownames_to_column("Label") %>% 
  left_join(males_without_missing %>% 
              select(all_of(columns_in_common)) %>% 
              rbind(parent_phenotypes %>% 
                      select(all_of(columns_in_common))),
            by = "Label") %>% 
  ggplot() +
  geom_point(aes(x = Dim.1, y = Dim.2, color = Phenotype), size = 3, alpha = 0.7) +
  my_theme +
  labs(x = paste0("Axis 1 (", pca1, "%)"),
       y = paste0("Axis 2 (", pca2, "%)")) +
  facet_wrap(vars(Family_level))


