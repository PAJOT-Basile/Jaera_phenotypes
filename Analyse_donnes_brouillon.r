# Import libraries
library("tidyverse")


########## Import data ##########
# Définition du chemin d'accès au jeu de données. Cette ligne sera sûrement à changer sur ta machine.
path_data <- "C:/Users/Basile Pajot/Documents/Pro/Thèse/Projet/Manips/Manip_sexualselection1/Data/Offspring_phenotypes.tsv"
# Importation jeu de données
Offspring_data <- read.table(file = path_data,        # Où est le jeu de données. 
                             sep = "\t",              # Le séparateur utilisé. Pour un fichier csv, tu peux mettre ","
                             header = TRUE) %>%       # Utile si la première ligne du jeu de données contient les noms de colonnes.
  rename(P4.isch.curv.setae = P4.isch.cirv.setae) %>% 
  mutate(Size = str_replace_all(Size, ",", ".") %>% as.numeric)


Offspring_males <- Offspring_data %>% 
  filter(Sex == "M")                                       # La condition ici est que la colonne "Sex" soit égale à la chaine de charactère "M"


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
  filter(Calculate_percentages_NA(Offspring_males) < Missing_threshold)



########## Graphic representation ##########
# Count number of each species
ggplot(Offspring_males_without_NA, aes(x = Phenotype, fill = Phenotype)) +
  geom_bar()
# On a des NA dans phenotype, mais on veut savoir combien
Offspring_males_without_NA %>% 
  Count_NA(by_row = FALSE)
## Reponse, on en a 6
# On veut maintenant savoir qui c'est
Offspring_males_without_NA %>% 
  filter(is.na(Phenotype))

# Regarder le nombre d'épines par pereiopode
Offspring_males_without_NA %>% 
  mutate(P3.ep = 0, P2.ep = 0, P1.ep = 0) %>% 
  rename(P4.ep = P4.small.ep) %>% 
  select(paste0("P", 1:7, ".ep"), Phenotype) %>% 
  pivot_longer(cols = -Phenotype,
               names_to = "Pereiopod",
               values_to = "Number_spines") %>% 
  ggplot(aes(x = Pereiopod, y = Number_spines, fill = Phenotype)) +
  geom_boxplot() +
  facet_wrap(vars(Phenotype))

# Regarder la distribution de soies par pereiopodes
Offspring_males_without_NA %>%
  rename(P6.curv.setae = P6.carp.curv.setae) %>% 
  mutate(P7.curv.setae = 0) %>% 
  select(paste0("P", 1:7, ".curv.setae"), Phenotype) %>%
  pivot_longer(cols = -Phenotype,
               names_to = "Pereiopod",
               values_to = "Number_setae") %>% 
  ggplot() +
  geom_boxplot(aes(x = Pereiopod, y = Number_setae, fill = Phenotype)) +
  facet_wrap(vars(Phenotype))

# Represent size distributions per species
Offspring_males_without_NA %>% 
  ggplot() +
  geom_density(aes(x = Size, fill = Phenotype), alpha = 0.5)


# Look at size distribution between sexes
Offspring_data %>% 
  ggplot() +
  geom_density(aes(x = Size, fill = Sex), alpha = 0.5) +
  facet_wrap(vars(Phenotype))




# Look at correlation between nb of spines in females and size
Offspring_females <- Offspring_data %>%
  filter(Sex == "F") %>%
  drop_na(starts_with(columns_to_keep))

## Plot the graphs
Offspring_females %>% 
  mutate(Tot_number_spines = P7.ep + P6.ep + P5.ep + P4.small.ep) %>% 
  ggplot() +
  geom_point(aes(x = Size, y = Tot_number_spines))

Offspring_with_spine_tot <- Offspring_females %>% 
  mutate(Tot_number_spines = P7.ep + P6.ep + P5.ep + P4.small.ep)

lm(Tot_number_spines ~ Size, data = Offspring_with_spine_tot) %>% 
  summary

Offspring_females %>% 
  mutate(Tot_number_spines = P7.ep + P6.ep + P5.ep + P4.small.ep) %>% 
  lm(data = ., Tot_number_spines ~ Size) %>% 
  summary

# Is there a shift in phenotype (shift in birth)
Offspring_males_without_NA %>% 
  mutate(Isolation_date = as.Date(Isolation_date, "%d/%m/%Y")) %>% 
  ggplot() +
  geom_bar(aes(x = Isolation_date, fill = Phenotype)) +
  facet_wrap(vars(Phenotype))
