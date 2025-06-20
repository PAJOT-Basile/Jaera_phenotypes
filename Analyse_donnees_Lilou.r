# Importer le(s) librarie(s)
library("tidyverse")



# Importer le jeu de données
# Définition du chemin d'accès au jeu de données. Cette ligne sera sûrement à changer sur ta machine.
path_data <- "C:/Users/Basile Pajot/Documents/Pro/Thèse/Projet/Manips/Manip_sexualselection1/Data/Offspring_phenotypes.tsv"
# Importation jeu de données
Offspring_data <- read.table(file = path_data,             # Où est le jeu de données
                             sep = "\t",                   # Le séparateur utilisé. Pour un fichier csv, tu peux mettre ","
                             header = TRUE,                # Utile si la première ligne du jeu de données contient les noms de colonnes.
                             dec = ",")                    # Indique comment utiliser quel séparateur décimal utiliserS


# Il y a des ">" dans le jeu de données dont on aimerait se débarasser. Pour se faire, on repère les colonnes dans lesquelles on a des ">" avec:
columns_with_signs <- which(grepl(">", Offspring_data))
column_names <- colnames(Offspring_data)

# On modifie ces colonnes pour enlever les ">" et transformer le tout en valeurs numériques
Offspring_data <- Offspring_data %>% 
  rename(P4.isch.curv.setae = P4.isch.cirv.setae) %>% 
  mutate(across(column_names[columns_with_signs], ~ str_remove_all(., ">")),
         across(column_names[columns_with_signs], ~ as.numeric(.)),
         Isolation_date = as.Date(Isolation_date, "%d/%m/%Y"))



Offspring_males <- Offspring_data %>% 
  filter(Sex == "M")                                       # La condition ici est que la colonne "Sex" soit égale à la chaine de charactère "M"


## Données manquantes

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



# Exploration du jeu de données


## Nombre de d'individus de chaque espèce
Offspring_males_without_NA %>% 
  ggplot() +
  geom_bar(aes(x = Phenotype, fill = Phenotype), colour = "black") +
  scale_fill_manual(values = c("forsmani" = "lightcoral", "hybrid" = "plum2", "praehirsuta" = "dodgerblue3", "NA" = "grey")) +
  labs(y = "Number of individuals") +
  my_theme

## Distribution du nombre d'épines par pereiopode
Offspring_males_without_NA %>% 
  rename(P4.ep = P4.small.ep) %>%                     # On renome une colonne pour qu'on puisse la sélectionner avec les autres
  select(paste0("P", 4:7, ".ep"), Phenotype) %>%      # On sélectionne les colonnes qu'on veut faire pivoter
  pivot_longer(cols = -Phenotype,                     # Quelles colonnes faire pivoter?
               names_to = "Pereiopod",                # Comment s'appelle la colonne dans laquelle les anciens noms de colonne se retrouvent?
               values_to = "Number_spines") %>%       # Comment s'appelle la colonne dans laquelle les anciennes valeurs se retrouvent?
  mutate(Pereiopod = Pereiopod %>% str_remove_all(., ".ep")) %>%     # On enlève la terminaison du nom de colonne
  ggplot(aes(x = Pereiopod, y = Number_spines, fill = Phenotype)) +  # On commence la représentation graphique
  geom_boxplot() +
  facet_wrap(vars(Phenotype)) +                       # Ceci permet de faire un graphique pour chaque phenotype
  scale_fill_manual(values = c("forsmani" = "lightcoral", "hybrid" = "plum2", "praehirsuta" = "dodgerblue3", "NA" = "grey")) +
  labs(y = "Number of spines per pereiopod") +
  my_theme

Offspring_males_without_NA %>% 
  rename(P4.ep = P4.small.ep) %>% 
  select(paste0("P", 4:7, ".ep"), Phenotype) %>% 
  pivot_longer(cols = -Phenotype,
               names_to = "Pereiopod",
               values_to = "Number_spines") %>%
  mutate(Pereiopod = Pereiopod %>% str_remove_all(., ".ep")) %>% 
  filter(Phenotype %in% c("forsmani", "praehirsuta")) %>%
  ggplot(aes(x = Pereiopod, y = Number_spines, fill = Phenotype)) +
  geom_boxplot() +
  scale_fill_manual(values = c("forsmani" = "lightcoral", "hybrid" = "plum2", "praehirsuta" = "dodgerblue3", "NA" = "grey")) +
  labs(y = "Number of spines per pereiopod") +
  my_theme


## Distribution du nombre de soies par pereiopode
Offspring_males_without_NA %>%
  rename(P6.curv.setae = P6.carp.curv.setae) %>% 
  select(paste0("P", 1:6, ".curv.setae"), Phenotype) %>%
  pivot_longer(cols = -Phenotype,
               names_to = "Pereiopod",
               values_to = "Number_setae") %>% 
  mutate(Pereiopod = Pereiopod %>% str_remove_all(., ".curv.setae")) %>% 
  ggplot() +
  geom_boxplot(aes(x = Pereiopod, y = Number_setae, fill = Phenotype)) +
  facet_wrap(vars(Phenotype)) +
  scale_fill_manual(values = c("forsmani" = "lightcoral", "hybrid" = "plum2", "praehirsuta" = "dodgerblue3", "NA" = "grey")) +
  labs(y = "Number of curved setae per pereiopod") +
  my_theme

## Distribution de taille
### Entre sexes
Offspring_data_without_NA %>% 
  ggplot() +
  geom_density(aes(x = Size, fill = Sex), alpha = 0.5)

### Entre espèces
Offspring_data_without_NA %>% 
  filter(Sex == "M") %>% 
  ggplot() +
  geom_density(aes(x = Size, fill = Phenotype), alpha = 0.5)+
  scale_fill_manual(values = c("forsmani" = "lightcoral", "hybrid" = "plum2", "praehirsuta" = "dodgerblue3", "NA" = "grey")) +
  my_theme


### Corrélation taille nombre d'épines
#### Males
Offspring_males_without_NA %>% 
  rename(P4.ep = P4.small.ep) %>% 
  select(paste0("P", 4:7, ".ep"), Phenotype, Size) %>% 
  pivot_longer(cols = -c(Phenotype, Size),
               names_to = "Pereiopod",
               values_to = "Number_spines") %>%
  mutate(Pereiopod = Pereiopod %>% str_remove_all(., ".ep")) %>%
  ggplot() +
  geom_point(aes(x = Size, y = Number_spines)) +
  facet_wrap(vars(Phenotype)) +
  my_theme


#### Femelles
Offspring_females_without_NA %>% 
  rename(P4.ep = P4.small.ep) %>% 
  select(paste0("P", 4:7, ".ep"), Size) %>% 
  pivot_longer(cols = -Size,
               names_to = "Pereiopod",
               values_to = "Number_spines") %>% 
  ggplot() +
  geom_point(aes(x = Size, y = Number_spines)) +
  my_theme


### Corrélation taille nombre de soies
Offspring_males_without_NA %>% 
  rename(P6.curv.setae = P6.carp.curv.setae) %>% 
  select(paste0("P", 1:6, ".curv.setae"), Phenotype, Size) %>%
  pivot_longer(cols = -c(Phenotype, Size),
               names_to = "Pereiopod",
               values_to = "Number_setae") %>% 
  mutate(Pereiopod = Pereiopod %>% str_remove_all(., ".curv.setae")) %>%
  ggplot() +
  geom_point(aes(x = Size, y = Number_setae)) +
  facet_wrap(vars(Phenotype)) +
  my_theme


## Différence de phénologie entre espèces
Offspring_males_without_NA %>% 
  ggplot() +
  geom_histogram(aes(x = Isolation_date, fill = Phenotype), bins = 30) +
  facet_wrap(vars(Phenotype), ncol = 1) +
  my_theme +
  labs(x = "Date d'isolation")


