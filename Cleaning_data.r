########## Import libraries ##########
library("tidyverse")

########## Useful variables ##########
my_theme <- theme_bw() +
  theme(text = element_text(size = 15))

########## Import data ##########
# Définition du chemin d'accès au jeu de données. Cette ligne sera sûrement à
# changer sur ta machine.
path_data <- "C:/Users/Basile Pajot/Documents/Pro/Thèse/Projet/Manips/Manip_sexualselection1/Data/Offspring_phenotypes.tsv"
# Importation jeu de données
Offspring_data <- read.table(file = path_data,             # Où est le jeu de données
                             sep = "\t",                   # Le séparateur utilisé. Pour un fichier csv, tu peux mettre ","
                             header = TRUE,                # Utile si la première ligne du jeu de données contient les noms de colonnes.
                             dec = ",")                    # Indique comment utiliser quel séparateur décimal utiliserS

# Il y a des ">" dans le jeu de données dont on aimerait se débarasser.
# Pour se faire, on repère les colonnes dans lesquelles on a des ">" avec:
columns_with_signs <- which(grepl(">", Offspring_data))
column_names <- colnames(Offspring_data)

# On modifie ces colonnes pour enlever les ">" et transformer le tout en valeurs numériques
Offspring_data <- Offspring_data %>% 
  rename(P4.isch.curv.setae = P4.isch.cirv.setae) %>% 
  mutate(across(column_names[columns_with_signs], ~ str_remove_all(., ">")),
         across(column_names[columns_with_signs], ~ as.numeric(.)),
         Isolation_date = as.Date(Isolation_date, "%d/%m/%Y"),
         Death_date = as.Date(Death_date, "%d/%m/%Y"))



Offspring_males <- Offspring_data %>% 
  # La condition ici est que la colonne "Sex" soit égale à la chaine de charactère "M"
  filter(Sex == "M")

########## Missing data ##########
# D'abord, faisons une fonction qui compte le nombre de données manquantes.
Count_NA <- function(df,                  # Cet argument est un data frame qui peut contenir des valeurs manquantes.
                     by_row = TRUE){      # Cet argument permet de choisir si on veut le nombre de données manquantes par ligne ou colonne. Par défaut, ce calcul se fait par ligne.
  # Ici, on choisit comment calculer le nombre de données manquantes (par lignes ou colonnes). On attribue une fonction à la variable temporaire ..function2use..
  ..function2use.. <- ifelse(by_row, rowSums, colSums)
  
  # Puis on calcule le nombre de données manquantes en fonction de ce qu'on a choisi.
  df %>% 
    is.na() %>%                     # La fonction is.na() retourne TRUE pour chaque NA rencontré dans le tableau d'entrée.
    ..function2use..() %>%          # Comme on a TRUE = 1 et FALSE = 0, on peut faire la somme de chaque TRUE pour connaître le nombre de données manquantes.
    return()                        # On utilise la fonction return en fin de fonction pour dire à notre fonction de retourner le calcul effectué.
}

# Puis la fonction qui détermine la proportion de données manquantes
Calculate_percentages_NA <- function(df, by_row = TRUE){         # Comme on va ré-utiliser la fonction qui compte les NA, on doit avoir au moins les mêmes arguments.
  # D'abord, il faut qu'on choisisse par quelle dimension on divise. Par exemple, si on veut la proportion de valeurs manquantes par ligne, il faut diviser le nombre de données manquantes par le nombre de colonnes du tableau. La variable ..dimension2use.. vaut donc 2 dans ce cas, car le nombre de colonnes est à la deuxième position quand on utilise la fonction dim(df).
  ..dimension2use.. <- ifelse(by_row, 2, 1)
  
  # Puis on calcule le nombre de valeurs manquantes en utilisant la fonction préparée au dessus
  ..nb_NA.. <- Count_NA(df = df, by_row = by_row)
  
  # Et enfin, on calcule la proportion de valeurs manquantes
  ..missing_proportion.. <- ..nb_NA.. / dim(df)[..dimension2use..]
  # Qu'on retourne ici
  return(..missing_proportion..)
}

# D'abord, faisons un vecteur des débuts de colonnes (de "P1" à "P7")
columns_to_keep <- paste0("P", 1:7)          # On utilise la fonction `paste` qui nous permet de coller du texte. Ici on colle la lettre "P" à chaque chiffre de la séquence 1:7.

# On sous-échantillonne le tableau pour ne garder que les colonnes qui nous intéressent
Phenotypes_males <- Offspring_males %>% 
  select(starts_with(columns_to_keep))

# On calcule le nombre de données manquantes
Individual_NA_ratio <- Calculate_percentages_NA(Phenotypes_males, by_row = TRUE) %>%
  round(digits = 2)


# D'abord, on veut voir si les individus on plus de 5% de données manquantes
Missing_threshold <- 0.05
Is_percentage_over_threshold <- Individual_NA_ratio > Missing_threshold
# Ceci nous donne un vecteur avec des TRUE et FALSE. On peut donc utiliser which pour trouver les indices des TRUE
Indices_over_threshold <- which(Is_percentage_over_threshold)

# D'abord, en utilisant les indices des individus dont on veut se débarasser:
## Les indices des individus qu'on a récupéré au-dessus correspondent au numéro de ligne sur lequel les individus sont dans le tableau de données. On peut donc enlever les individus en disant qu'on ne veut pas les lignes identifiées au-dessus.
Offspring_males_without_NA <- Offspring_males[-Indices_over_threshold, ]

# La deuxième méthode plus directe utilise la fonction filter
Offspring_males_without_NA <- Offspring_males %>% 
  filter(Calculate_percentages_NA(Phenotypes_males) < Missing_threshold)

# Et faisons de même pour les femelles.
Offspring_females_without_NA <- Offspring_data %>% 
  filter(Sex == "F") %>% 
  filter(Calculate_percentages_NA(Offspring_data %>% 
                                    filter(Sex == "F") %>% select(starts_with(columns_to_keep))) < Missing_threshold)

Offspring_data_without_NA <- Offspring_females_without_NA %>% 
  rbind(Offspring_males_without_NA)