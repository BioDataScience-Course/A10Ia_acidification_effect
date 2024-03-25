# Importation des données et remaniement
# (Ce script a déjà été exécuté pour vous, vous ne devez pas le relancer)

# Configuration de l'environnement
SciViews::R(lang = "fr")



# Importation des données -------------------------------------------------

# Note: les données originales sont récupérées depuis le site Dryad, dézippées
# et placées dans le dossier data/raw
# Nous récupérons les données de la feuille "Growth Wet Weight" du tableau Excel
growth <- read("data/raw/Doo_Eco-Evol_Data_Final.xlsx",
  sheet = "Growth Wet Weight")


# Remaniement des données -------------------------------------------------

# Note: on utilise sub() pour éliminer "pH" ou "temp" des noms de niveaux
growth %>.%
  smutate(.,
    pH          = as.factor(sub("pH", "", pH)),
    Temp        = as.factor(sub("temp", "", Temp)),
    Association = as.factor(Association)) %>.%
  smutate(.,
    Condition = factor(paste0(Temp, pH))) ->
  growth
# Changement des niveaux de 'Condition' pour des codes plus courts
# 'n' for naturel, 't' for temp. élevée, 'p' pour pH bas et 'tp' pour les deux
levels(growth$Condition) <- c("n", "p", "t", "tp")

# Ajout des labels --------------------------------------------------------

growth <- labelise(growth,
  label = list(
    Name        = "Individu",
    pH          = "pH",
    Temp        = "Température",
    Association = "Association",
    Wet_Weight  = "Croissance en masse/jour",
    Condition     = "Condition"),
  units = list(
    Temp = "°C",
    Wet_Weight = "%")
)

# Sauvagarde des données retravaillées ------------------------------------

write$rds(growth, "data/growth.rds")

# Nettoyage de l'environnement
rm(growth)
