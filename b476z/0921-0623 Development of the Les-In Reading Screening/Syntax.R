# 1. Pakete installieren und Working Directory setzen ####
library(tidyverse) # Data Wrangling
library(pairwise) # Rasch Analysen
library(TAM) # IRT Analysen
library(WrightMap) # Wright Map
library(RColorBrewer) # Color Scale
library(sfsmisc) # Statistik für TAM

Data <- read.csv2("Daten.csv") # Einlesen Datensatz

#_______________________________________________________________________________
# 1. Stichprobe auswerten

table(Data$Gender) # Geschlecht
mean(Data$Alter) # Alter
sd(Data$Alter) # Alter SD
table(Data$Schulart) # Schulart
table(filter(Data, Schulart == 0)$SUB) # FB an inkl. Grundschulen
table(filter(Data, Schulart == 2)$SUB) # FB an Förderschulen

#_______________________________________________________________________________
# 2. Antworten korrigieren ####

Data <- Data %>% relocate(1:7, .after = last_col()) # Schülerinformationen ans Ende des DF schieben
Data <- Data %>%
  mutate(
    X1_1A = if_else(X1_1A == 1, 1, 0), # Aufgabe 1
    X1_1B = if_else(X1_1B == 1, 1, 0),
    X1_1D = if_else(X1_1D == 1, 1, 0),
    X1_1E = if_else(X1_1E == 1, 1, 0),
    X1_1F = if_else(X1_1F == 1, 1, 0),
    X1_1G = if_else(X1_1G == 1, 1, 0),
    X1_1H = if_else(X1_1H == 1, 1, 0),
    X1_1I = if_else(X1_1I == 1, 1, 0),
    X1_1J = if_else(X1_1J == 1, 1, 0),
    X1_1K = if_else(X1_1K == 1, 1, 0),
    X1_2A = if_else(X1_2A == 3, 1, 0),
    X1_2B = if_else(X1_2B == 3, 1, 0),
    X1_2C = if_else(X1_2C == 3, 1, 0),
    X1_2D = if_else(X1_2D == 3, 1, 0),
    X1_2E = if_else(X1_2E == 3, 1, 0),
    X1_2F = if_else(X1_2F == 3, 1, 0),
    X1_2G = if_else(X1_2G == 3, 1, 0),
    X1_2H = if_else(X1_2H == 3, 1, 0),
    X1_2I = if_else(X1_2I == 3, 1, 0),
    X1_2J = if_else(X1_2J == 3, 1, 0),
    X1_3A = if_else(X1_3A == 2, 1, 0),
    X1_3B = if_else(X1_3B == 2, 1, 0),
    X1_3C = if_else(X1_3C == 2, 1, 0),
    X1_3D = if_else(X1_3D == 4, 1, 0),
    X1_3E = if_else(X1_3E == 4, 1, 0),
    X1_3F = if_else(X1_3F == 4, 1, 0),
    X1_3G = if_else(X1_3G == 4, 1, 0),
    X1_3H = if_else(X1_3H == 2, 1, 0),
    X1_3I = if_else(X1_3I == 2, 1, 0),
    X1_3J = if_else(X1_3J == 2, 1, 0),
    X1_4A = if_else(X1_4A == 1, 1, 0),
    X1_4B = if_else(X1_4B == 1, 1, 0),
    X1_4C = if_else(X1_4C == 1, 1, 0),
    X1_4D = if_else(X1_4D == 1, 1, 0),
    X1_4E = if_else(X1_4E == 1, 1, 0),
    X1_4F = if_else(X1_4F == 1, 1, 0),
    X1_4G = if_else(X1_4G == 1, 1, 0),
    X1_4H = if_else(X1_4H == 1, 1, 0),
    X1_4I = if_else(X1_4I == 1, 1, 0),
    X1_4J = if_else(X1_4J == 1, 1, 0),
    X1_5A = if_else(X1_5A == 3, 1, 0),
    X1_5B = if_else(X1_5B == 3, 1, 0),
    X1_5C = if_else(X1_5C == 3, 1, 0),
    X1_5D = if_else(X1_5D == 3, 1, 0),
    X1_5E = if_else(X1_5E == 3, 1, 0),
    X1_5G = if_else(X1_5G == 3, 1, 0),
    X1_5H = if_else(X1_5H == 3, 1, 0),
    X1_5I = if_else(X1_5I == 3, 1, 0),
    X1_5J = if_else(X1_5J == 3, 1, 0),
    X1_6A = if_else(X1_6A == 2, 1, 0),
    X1_6B = if_else(X1_6B == 2, 1, 0),
    X1_6C = if_else(X1_6C == 2, 1, 0),
    X1_6D = if_else(X1_6D == 4, 1, 0),
    X1_6E = if_else(X1_6E == 2, 1, 0),
    X1_6G = if_else(X1_6G == 4, 1, 0),
    X1_6H = if_else(X1_6H == 2, 1, 0),
    X1_6I = if_else(X1_6I == 2, 1, 0),
    X1_6J = if_else(X1_6J == 2, 1, 0),
    X1_6K = if_else(X1_6K == 2, 1, 0)
  ) %>%
  mutate(across(X2_1A:X2_4F, ~ na_if(., 0))) # Aufgabe 2
Data[, 60:101][Data[, 60:101] == 2] <- 0
Data[, 60:101][Data[, 60:101] > 2] <- NA
Data <- Data %>%
  mutate(
    X2_4A = if_else(X2_4A == 0, 1, 0),
    X2_4B = if_else(X2_4B == 0, 1, 0),
    X2_4C = if_else(X2_4C == 0, 1, 0),
    X2_4D = if_else(X2_4A == 0, 1, 0),
    X2_4E = if_else(X2_4C == 0, 1, 0),
    X2_4F = if_else(X2_4C == 0, 1, 0)
  ) %>%
  mutate(
    X3_1A = if_else(X3_1A == 1, 1, 0), # Aufgabe 3
    X3_3B = if_else(X3_3B == 1, 1, 0),
    X3_1C = if_else(X3_1C == 1, 1, 0),
    X3_1D = if_else(X3_1D == 1, 1, 0),
    X3_1E = if_else(X3_1E == 1, 1, 0),
    X3_2F = if_else(X3_2F == 1, 1, 0),
    X3_2G = if_else(X3_2G == 1, 1, 0),
    X3_2H = if_else(X3_2H == 1, 1, 0),
    X3_1J = if_else(X3_1J == 1, 1, 0),
    X3_1B = if_else(X3_1B == 2, 1, 0),
    X3_2B = if_else(X3_2B == 2, 1, 0),
    X3_2D = if_else(X3_2D == 2, 1, 0),
    X3_2E = if_else(X3_2E == 2, 1, 0),
    X3_1G = if_else(X3_1G == 2, 1, 0),
    X3_1H = if_else(X3_1H == 2, 1, 0),
    X3_2I = if_else(X3_2I == 2, 1, 0),
    X3_2J = if_else(X3_2J == 2, 1, 0),
    X3_2A = if_else(X3_2A == 3, 1, 0),
    X3_3C = if_else(X3_3C == 3, 1, 0),
    X3_3E = if_else(X3_3E == 3, 1, 0),
    X3_1F = if_else(X3_1F == 3, 1, 0),
    X3_3G = if_else(X3_3G == 3, 1, 0),
    X3_3I = if_else(X3_3I == 3, 1, 0),
    X3_3J = if_else(X3_3J == 3, 1, 0),
    X3_3A = if_else(X3_3A == 4, 1, 0),
    X3_2C = if_else(X3_2C == 4, 1, 0),
    X3_3D = if_else(X3_3D == 4, 1, 0),
    X3_3F = if_else(X3_3F == 4, 1, 0),
    X3_3H = if_else(X3_3H == 4, 1, 0),
    X3_1I = if_else(X3_1I == 4, 1, 0)
  ) %>%
  mutate(
    X4_1A = if_else(X4_1A == 1, 1, 0), # Aufgabe 4
    X4_4A = if_else(X4_4A == 1, 1, 0),
    X4_3B = if_else(X4_3B == 1, 1, 0),
    X4_4C = if_else(X4_4C == 1, 1, 0),
    X4_3E = if_else(X4_3E == 1, 1, 0),
    X4_5E = if_else(X4_5E == 1, 1, 0),
    X4_5F = if_else(X4_5F == 1, 1, 0),
    X4_2G = if_else(X4_2G == 1, 1, 0),
    X4_1I = if_else(X4_1I == 1, 1, 0),
    X4_4I = if_else(X4_4I == 1, 1, 0),
    X4_3J = if_else(X4_3J == 1, 1, 0),
    X4_3K = if_else(X4_3K == 1, 1, 0),
    X4_5K = if_else(X4_5K == 1, 1, 0),
    X4_2L = if_else(X4_2L == 1, 1, 0),
    X4_5L = if_else(X4_5L == 1, 1, 0),
    X4_3M = if_else(X4_3M == 1, 1, 0),
    X4_3N = if_else(X4_3N == 1, 1, 0),
    X4_5N = if_else(X4_5N == 1, 1, 0),
    X4_5O = if_else(X4_5O == 1, 1, 0),
    X4_3A = if_else(X4_3A == 2, 1, 0),
    X4_1B = if_else(X4_1B == 2, 1, 0),
    X4_5B = if_else(X4_5B == 2, 1, 0),
    X4_5C = if_else(X4_5C == 2, 1, 0),
    X4_1D = if_else(X4_1D == 2, 1, 0),
    X4_3D = if_else(X4_3D == 2, 1, 0),
    X4_2E = if_else(X4_2E == 2, 1, 0),
    X4_1F = if_else(X4_1F == 2, 1, 0),
    X4_3F = if_else(X4_3F == 2, 1, 0),
    X4_4F = if_else(X4_4F == 2, 1, 0),
    X4_5G = if_else(X4_5G == 2, 1, 0),
    X4_2H = if_else(X4_2H == 2, 1, 0),
    X4_3H = if_else(X4_3H == 2, 1, 0),
    X4_5H = if_else(X4_5H == 2, 1, 0),
    X4_5J = if_else(X4_5J == 2, 1, 0),
    X4_2M = if_else(X4_2M == 2, 1, 0),
    X4_4M = if_else(X4_4M == 2, 1, 0),
    X4_4N = if_else(X4_4N == 2, 1, 0),
    X4_3O = if_else(X4_3O == 2, 1, 0),
    X4_2A = if_else(X4_2A == 3, 1, 0),
    X4_2B = if_else(X4_2B == 3, 1, 0),
    X4_1C = if_else(X4_1C == 3, 1, 0),
    X4_2C = if_else(X4_2C == 3, 1, 0),
    X4_2D = if_else(X4_2D == 3, 1, 0),
    X4_5D = if_else(X4_5D == 3, 1, 0),
    X4_4E = if_else(X4_4E == 3, 1, 0),
    X4_1G = if_else(X4_1G == 3, 1, 0),
    X4_4G = if_else(X4_4G == 3, 1, 0),
    X4_1H = if_else(X4_1H == 3, 1, 0),
    X4_3I = if_else(X4_3I == 3, 1, 0),
    X4_5I = if_else(X4_5I == 3, 1, 0),
    X4_2J = if_else(X4_2J == 3, 1, 0),
    X4_4J = if_else(X4_4J == 3, 1, 0),
    X4_1K = if_else(X4_1K == 3, 1, 0),
    X4_1L = if_else(X4_1L == 3, 1, 0),
    X4_1N = if_else(X4_1N == 3, 1, 0),
    X4_2O = if_else(X4_2O == 3, 1, 0),
    X4_5A = if_else(X4_5A == 4, 1, 0),
    X4_4B = if_else(X4_4B == 4, 1, 0),
    X4_3C = if_else(X4_3C == 4, 1, 0),
    X4_4D = if_else(X4_4D == 4, 1, 0),
    X4_1E = if_else(X4_1E == 4, 1, 0),
    X4_2F = if_else(X4_2F == 4, 1, 0),
    X4_3G = if_else(X4_3G == 4, 1, 0),
    X4_4H = if_else(X4_4H == 4, 1, 0),
    X4_2I = if_else(X4_2I == 4, 1, 0),
    X4_1J = if_else(X4_1J == 4, 1, 0),
    X4_2K = if_else(X4_2K == 4, 1, 0),
    X4_4K = if_else(X4_4K == 4, 1, 0),
    X4_3L = if_else(X4_3L == 4, 1, 0),
    X4_4L = if_else(X4_4L == 4, 1, 0),
    X4_1M = if_else(X4_1M == 4, 1, 0),
    X4_5M = if_else(X4_5M == 4, 1, 0),
    X4_2N = if_else(X4_2N == 4, 1, 0),
    X4_1O = if_else(X4_1O == 4, 1, 0),
    X4_4O = if_else(X4_4O == 4, 1, 0)
  )


dir.create("Zwischenergebnisse", showWarnings = FALSE, recursive = TRUE)
write.csv2(Data, "Zwischenergebnisse/Schülerantworten.csv", row.names = F) # Zwischenspeichern Datensatz
#_______________________________________________________________________________
# 4. Graphischer Modelltest ####
Data <- read.csv2("Zwischenergebnisse/Schülerantworten.csv") # Inputdaten Alle Klassen

Split <- select(Data, c("SUB", "Gender", "Schulart", "KS")) # Splitliste bauen
Split["SUB"][Split["SUB"] != 0] <- 1 # Split recodieren
Split["SUB"][Split["SUB"] == 0] <- 2 # Split recodieren
Split$SUB <- as.factor(Split$SUB) # Split zu Faktor

GRM <- Data %>%
  select(
    -c("X1_1K", "X1_5D", "X1_6I", "X1_3I", "X1_2H", "X1_5H", "X1_5G", "X1_5E")
  ) %>% # Items entfernen, die unpraktikabel waren
  select(starts_with("X")) # Nur Items auswählen

set.seed(1) # Seed setzen

GRM_1 <- grm(daten = select(GRM, starts_with("X1")), m = 2, split = Split$SUB) # GRM Aufgabe 1
plot(
  GRM_1,
  main = "Phonologische Bewusstheit",
  itemNames = F, # GRM Abbildung Aufgabe 1
  xlab = "Förderbedarf",
  ylab = "Kein Förderbedarf",
  xymin = -13,
  xymax = 13
)

GRM_2 <- grm(daten = select(GRM, starts_with("X2")), m = 2, split = Split$SUB) # GRM Aufgabe 2
plot(
  GRM_2,
  main = "Wortschatz",
  itemNames = F, # GRM Abbildung Aufgabe 2
  xlab = "Förderbedarf",
  ylab = "Kein Förderbedarf",
  xymin = -13,
  xymax = 13
)

GRM_3 <- grm(daten = select(GRM, starts_with("X3")), m = 2, split = Split$SUB) # GRM Aufgabe 3
plot(
  GRM_3,
  main = "Blitzlesen",
  itemNames = F, # GRM Abbildung Aufgabe 3
  xlab = "Förderbedarf",
  ylab = "Kein Förderbedarf",
  xymin = -13,
  xymax = 13
)

GRM_4 <- grm(daten = select(GRM, starts_with("X4")), m = 2, split = Split$SUB) # GRM Aufgabe 4
plot(
  GRM_4,
  main = "Satzlesen",
  itemNames = F, # GRM Abbildung Aufgabe 4
  xlab = "Förderbedarf",
  ylab = "Kein Förderbedarf",
  xymin = -13,
  xymax = 13
)

Data_Fair <- GRM %>% # Items entfernen, die nicht fair testen
  select(
    -c(
      "X1_6K",
      "X1_3E",
      "X1_1G",
      "X1_2A",
      "X1_6J",
      "X1_2E",
      "X1_5C",
      "X1_3H", # Aufgabe 1
      "X1_6D",
      "X1_6E",
      "X1_6H",
      "X1_1J",
      "X1_4F",
      "X1_3J",
      "X1_6G", # Aufgabe 1
      "X2_2B",
      "X2_1F",
      "X2_3K",
      "X2_2L",
      "X2_2J",
      "X2_2D",
      "X2_2E",
      "X2_1J", # Aufgabe 2
      "X3_2C",
      "X3_1A",
      "X3_1J",
      "X3_1F",
      "X3_3A", # Aufgabe 3
      "X4_1M",
      "X4_3L",
      "X4_5O",
      "X4_3N",
      "X4_2N",
      "X4_4L"
    )
  ) # Aufgabe 4

GRM_1 <- grm(
  daten = select(Data_Fair, starts_with("X1")),
  m = 2,
  split = Split$SUB
) # GRM Aufgabe 1 Überprüfung
plot(GRM_1, itemNames = T)
GRM_2 <- grm(
  daten = select(Data_Fair, starts_with("X2")),
  m = 2,
  split = Split$SUB
) # GRM Aufgabe 2 Überprüfung
plot(GRM_2, itemNames = F)
GRM_3 <- grm(
  daten = select(Data_Fair, starts_with("X3")),
  m = 2,
  split = Split$SUB
) # GRM Aufgabe 3 Überprüfung
plot(GRM_3, itemNames = T)
GRM_4 <- grm(
  daten = select(Data_Fair, starts_with("X4")),
  m = 2,
  split = Split$SUB
) # GRM Aufgabe 4 Überprüfung
plot(GRM_4, itemNames = T)

#_______________________________________________________________________________
# 5. Rasch Analyse ####
Data <- Data_Fair

# 5.1 Phonologische Bewusstheit
Rasch_1 <- pair(select(Data, starts_with("X1")))
Sigma_1 <- as.data.frame(Rasch_1$sigma) # Itemschwierigkeit
Pers_1 <- pers(Rasch_1) # Personenparameter
Itemfit_1 <- pairwise.item.fit(Pers_1) # Itemfit
plot(Pers_1, itemNames = F, main = "Phonologische Bewusstheit", ra = 6) # Person-Item-Map

# 5.2 Wortschatz
Rasch_2 <- pair(select(Data, starts_with("X2"))) # Rasch Modell
Sigma_2 <- as.data.frame(Rasch_2$sigma) # Itemschwierigkeit
Pers_2 <- pers(Rasch_2) # Personenparameter Aufgabe 2
Itemfit_2 <- pairwise.item.fit(Pers_2) # Itemfit Aufgabe 2
plot(Pers_2, itemNames = F, main = "Wortschatz", ra = 6, sortdif = F) # Person-Item-Map Aufgabe 2

# 5.3 Blitzlesen
Rasch_3 <- pair(select(Data, starts_with("X3"))) # Rasch Modell
Sigma_3 <- as.data.frame(Rasch_3$sigma) # Itemschwierigkeit
Pers_3 <- pers(Rasch_3) # Personenparameter Aufgabe 3
Itemfit_3 <- pairwise.item.fit(Pers_3) # Itemfit Aufgabe 3
plot(Pers_3, itemNames = F, main = "Blitzlesen", ra = 6) # Person-Item-Map Aufgabe 3

# 5.4 Sinnentnehmendes Satzlesen
Rasch_4 <- pair(select(Data, starts_with("X4"))) # Rasch-Modell
Sigma_4 <- as.data.frame(Rasch_4$sigma) # Itemschwierigkeit
Pers_4 <- pers(Rasch_4) # Personenparameter Aufgabe 4
Itemfit_4 <- pairwise.item.fit(Pers_4) # Itemfit Aufgabe 4
plot(Pers_4, itemNames = F, main = "Satzverstehen", ra = 6) # Person-Item-Map Aufgabe 4
#_______________________________________________________________________________
# 7. Modellanalyse Screening ####

Data <- Data %>% # Items entfernen, die nicht fair testen
  select(
    -c(
      "X1_6K",
      "X1_3E",
      "X1_1G",
      "X1_2A",
      "X1_6J",
      "X1_2E",
      "X1_5C",
      "X1_3H", # Aufgabe 1
      "X1_6D",
      "X1_6E",
      "X1_6H",
      "X1_1J",
      "X1_4F",
      "X1_3J",
      "X1_6G", # Aufgabe 1
      "X2_2B",
      "X2_1F",
      "X2_3K",
      "X2_2L",
      "X2_2J",
      "X2_2D",
      "X2_2E",
      "X2_1J", # Aufgabe 2
      "X3_2C",
      "X3_1A",
      "X3_1J",
      "X3_1F",
      "X3_3A", # Aufgabe 3
      "X4_1M",
      "X4_3L",
      "X4_5O",
      "X4_3N",
      "X4_2N",
      "X4_4L"
    )
  )
Data <- select(Data, 1:172)

Q <- data.frame("Item" = colnames(Data)) # Grundgerüst für Q Matrix

# 7.1 Eindimensionales Rasch-Modell
Rasch_1D <- tam(Data) # Eindimensionales Rasch-Modell berechnen

# 7.2 Zweidimensionales Rasch-Modell
Q2 <- Q %>%
  mutate(
    "D1" = case_when(
      startsWith(Item, "X1") ~ 1, # Q-Matrix für 2D Modell erstellen
      startsWith(Item, "X2") ~ 0,
      startsWith(Item, "X3") ~ 1,
      startsWith(Item, "X4") ~ 1
    ),
    "D2" = case_when(
      startsWith(Item, "X1") ~ 0,
      startsWith(Item, "X2") ~ 1,
      startsWith(Item, "X3") ~ 0,
      startsWith(Item, "X4") ~ 0
    )
  )
Rasch_2D <- tam(
  Data,
  Q = select(Q2, -c("Item")),
  control = list(snodes = 1500, QMC = T)
) # Zweidimensionales Rasch-Modell berechnen

# 7.3 Dreidimensionales Rasch-Modell
Q3 <- Q %>%
  mutate(
    "D1" = case_when(
      startsWith(Item, "X1") ~ 1, # Q-Matrix für 2D Modell erstellen
      startsWith(Item, "X2") ~ 0,
      startsWith(Item, "X3") ~ 0,
      startsWith(Item, "X4") ~ 0
    ),
    "D2" = case_when(
      startsWith(Item, "X1") ~ 0,
      startsWith(Item, "X2") ~ 1,
      startsWith(Item, "X3") ~ 1,
      startsWith(Item, "X4") ~ 0
    ),
    "D3" = case_when(
      startsWith(Item, "X1") ~ 0,
      startsWith(Item, "X2") ~ 0,
      startsWith(Item, "X3") ~ 0,
      startsWith(Item, "X4") ~ 1
    )
  )
Rasch_3D <- tam(
  Data,
  Q = select(Q3, -c("Item")),
  control = list(snodes = 1500, QMC = T)
) # Zweidimensionales Rasch-Modell berechnen

# 7.3 Vierdimensionales Rasch-Modell
Q4 <- Q %>%
  mutate(
    "D1" = case_when(
      startsWith(Item, "X1") ~ 1, # Q-Matrix für 4D Modell erstellen
      startsWith(Item, "X2") ~ 0,
      startsWith(Item, "X3") ~ 0,
      startsWith(Item, "X4") ~ 0
    ),
    "D2" = case_when(
      startsWith(Item, "X1") ~ 0,
      startsWith(Item, "X2") ~ 1,
      startsWith(Item, "X3") ~ 0,
      startsWith(Item, "X4") ~ 0
    ),
    "D3" = case_when(
      startsWith(Item, "X1") ~ 0,
      startsWith(Item, "X2") ~ 0,
      startsWith(Item, "X3") ~ 1,
      startsWith(Item, "X4") ~ 0
    ),
    "D4" = case_when(
      startsWith(Item, "X1") ~ 0,
      startsWith(Item, "X2") ~ 0,
      startsWith(Item, "X3") ~ 0,
      startsWith(Item, "X4") ~ 1
    )
  )
Rasch_4D <- tam(
  Data,
  Q = select(Q4, -c("Item")),
  control = list(snodes = 1500, QMC = T)
) # Vierdimensionales Rasch-Modell berechnen
Pers_4D <- tam.wle(Rasch_4D) # Personenfähigkeit berechnen
Item_4D <- tam.fit(Rasch_4D) # Itemfit berechnen
Itemfit_4D <- Item_4D$itemfit

wrightMap(
  thetas = select(Pers_4D, starts_with("theta")), # Wright Map
  thresholds = tam.threshold(Rasch_4D),
  show.thr.sym = T,
  show.thr.lab = F,
  item.prop = 0.5,
  thr.sym.pch = 1,
  thr.lab.cex = 0.5,
  label.items = "",
  thr.lab.text = paste("I", 1:128, sep = ""),
  label.items.ticks = F
)

# 7.4 Sechsdimensionales Rasch-Modell
Q6 <- Q %>%
  mutate(
    "D1" = case_when(
      startsWith(Item, "X1") ~ 1,
      startsWith(Item, "X2") ~ 0, # Q-Matrix für 6D Modell erstellen
      startsWith(Item, "X3") ~ 0,
      startsWith(Item, "X4") ~ 0
    ),
    "D2" = case_when(
      startsWith(Item, "X1") ~ 0,
      startsWith(Item, "X2") ~ 1,
      startsWith(Item, "X3") ~ 0,
      startsWith(Item, "X4") ~ 0
    ),
    "D3" = case_when(
      startsWith(Item, "X1") ~ 0,
      startsWith(Item, "X2") ~ 0,
      startsWith(Item, "X3") ~ 1,
      startsWith(Item, "X4") ~ 0
    ),
    "D4" = case_when(
      startsWith(Item, "X1") ~ 0,
      startsWith(Item, "X2") ~ 0,
      startsWith(Item, "X3") ~ 0,
      startsWith(Item, "X4_1") ~ 1,
      startsWith(Item, "X4_2") ~ 0,
      startsWith(Item, "X4_3") ~ 0,
      startsWith(Item, "X4_4") ~ 0,
      startsWith(Item, "X4_5") ~ 0
    ),
    "D5" = case_when(
      startsWith(Item, "X1") ~ 0,
      startsWith(Item, "X2") ~ 0,
      startsWith(Item, "X3") ~ 0,
      startsWith(Item, "X4_1") ~ 0,
      startsWith(Item, "X4_2") ~ 1,
      startsWith(Item, "X4_3") ~ 1,
      startsWith(Item, "X4_4") ~ 0,
      startsWith(Item, "X4_5") ~ 0
    ),
    "D6" = case_when(
      startsWith(Item, "X1") ~ 0,
      startsWith(Item, "X2") ~ 0,
      startsWith(Item, "X3") ~ 0,
      startsWith(Item, "X4_1") ~ 0,
      startsWith(Item, "X4_2") ~ 0,
      startsWith(Item, "X4_3") ~ 0,
      startsWith(Item, "X4_4") ~ 1,
      startsWith(Item, "X4_5") ~ 1
    )
  )

Rasch_6D <- tam(
  Data,
  Q = select(Q6, -c("Item")),
  control = list(snodes = 1500, QMC = T)
) # Sechsdimensionales Rasch-Modell berechnen
Pers_6D <- tam.wle(Rasch_6D) # Personenfähigkeit berechnen

# 7.5 Modellvergleich
IRT.compareModels(Rasch_1D, Rasch_2D, Rasch_4D, Rasch_6D)
IRT.compareModels(Rasch_1D, Rasch_2D, Rasch_3D, Rasch_4D)
#_______________________________________________________________________________
# 8. Richtwerte ####
Data <- Data %>%
  select(
    -c(
      "X1_1K",
      "X1_5D",
      "X1_6I",
      "X1_3I",
      "X1_2H",
      "X1_5H",
      "X1_5G",
      "X1_5E", # Items, die vorher aussortier wurden
      "X1_6K",
      "X1_3E",
      "X1_1G",
      "X1_2A",
      "X1_6J",
      "X1_2E",
      "X1_5C",
      "X1_3H",
      "X1_6D",
      "X1_6E",
      "X1_6H",
      "X1_1J",
      "X1_4F",
      "X1_3J",
      "X1_6G",
      "X2_2B",
      "X2_1F",
      "X2_3K",
      "X2_2L",
      "X2_2J",
      "X2_2D",
      "X2_2E",
      "X2_1J",
      "X3_2C",
      "X3_1A",
      "X3_1J",
      "X3_1F",
      "X3_3A",
      "X4_1M",
      "X4_3L",
      "X4_5O",
      "X4_3N",
      "X4_2N",
      "X4_4L"
    )
  )
Data_GS <- Data %>%
  filter(Schulart == 0) %>%
  select(starts_with("X")) %>% # Daten nur Förderbedarf
  mutate(
    Sum_X1 = rowSums(.[1:36], na.rm = T), # Summenscore Aufgabe 1
    Sum_X2 = rowSums(.[37:70], na.rm = T), # Summenscore Aufgabe 2
    Sum_X3 = rowSums(.[71:95], na.rm = T), # Summenscore Aufgabe 3
    Sum_X4 = rowSums(.[96:164], na.rm = T)
  ) %>% # Summenscore Aufgabe 4
  select(165:168) %>%
  mutate(Schulart = 0) # Tabelle umbauen

Data_FS <- Data %>%
  filter(Schulart == 2) %>%
  select(starts_with("X")) %>% # Daten nur F?rderbedarf
  mutate(
    Sum_X1 = rowSums(.[1:36], na.rm = T), # Summenscore Aufgabe 1
    Sum_X2 = rowSums(.[37:70], na.rm = T), # Summenscore Aufgabe 2
    Sum_X3 = rowSums(.[71:95], na.rm = T), # Summenscore Aufgabe 3
    Sum_X4 = rowSums(.[96:164], na.rm = T)
  ) %>% # Summenscore Aufgabe 4
  select(165:168) %>%
  mutate(Schulart = 1) # Tabelle umbauen

perc_10_X1 <- quantile(Data_GS$Sum_X1, .10)
perc_10_X2 <- quantile(Data_GS$Sum_X2, .10)
perc_10_X3 <- quantile(Data_GS$Sum_X3, .10)
perc_10_X4 <- quantile(Data_GS$Sum_X4, .10)
perc_20_X1 <- quantile(Data_GS$Sum_X1, .25)
perc_20_X2 <- quantile(Data_GS$Sum_X2, .25)
perc_20_X3 <- quantile(Data_GS$Sum_X3, .25)
perc_20_X4 <- quantile(Data_GS$Sum_X4, .25)

Summenscores <- rbind(Data_FS, Data_GS) # Summenscores für alle Aufgaben und Sch?ler
Summenscores[, 5][Summenscores[, 5] == 1] <- "Förderschulen"
Summenscores[, 5][Summenscores[, 5] == 0] <- "Grundschulen"
Summenscores$Schulart <- factor(
  Summenscores$Schulart,
  levels = c("Grundschulen", "Förderschulen")
)

# MANOVA
manova <- manova(
  cbind(
    Summenscores$Sum_X1,
    Summenscores$Sum_X2,
    Summenscores$Sum_X3,
    Summenscores$Sum_X4
  ) ~ Summenscores$Schulart
)
summary(manova) # over all groups
summary.aov(manova) # results per group


TukeyHSD(aov(Summenscores$Sum_X1 ~ Summenscores$Schulart))
TukeyHSD(aov(Summenscores$Sum_X2 ~ Summenscores$Schulart))
TukeyHSD(aov(Summenscores$Sum_X3 ~ Summenscores$Schulart))
TukeyHSD(aov(Summenscores$Sum_X4 ~ Summenscores$Schulart))

# Abbildungen
(sum(Data_FS$Sum_X1 <= 16) / 145) * 100
((length(Data_FS["Sum_X1"][Data_FS["Sum_X1"] > 16 & Data_FS["Sum_X1"] <= 23])) /
  145) *
  100
(sum(Data_FS$Sum_X2 <= 26) / 145) * 100
((length(Data_FS["Sum_X2"][Data_FS["Sum_X2"] > 26 & Data_FS["Sum_X2"] <= 29])) /
  145) *
  100
(sum(Data_FS$Sum_X3 <= 22) / 145) * 100
((length(Data_FS["Sum_X3"][Data_FS["Sum_X3"] > 22 & Data_FS["Sum_X3"] <= 24])) /
  145) *
  100
(sum(Data_FS$Sum_X4 <= 10) / 145) * 100
((length(Data_FS["Sum_X4"][Data_FS["Sum_X4"] > 10 & Data_FS["Sum_X4"] <= 16])) /
  145) *
  100

Box_1 <- ggplot(Summenscores, aes(x = Schulart, y = Sum_X1)) +
  geom_boxplot() +
  theme_bw() +
  xlab("") +
  ylab("Summenscore") +
  geom_hline(yintercept = 16, color = "black") +
  geom_hline(yintercept = 23, color = "black", linetype = "dashed")
Box_1

Box_2 <- ggplot(Summenscores, aes(x = Schulart, y = Sum_X2)) +
  geom_boxplot() +
  theme_bw() +
  xlab("") +
  ylab("Summenscore") +
  geom_hline(yintercept = 26, color = "black") +
  geom_hline(yintercept = 29, color = "black", linetype = "dashed")
Box_2

Box_3 <- ggplot(Summenscores, aes(x = Schulart, y = Sum_X3)) +
  geom_boxplot() +
  theme_bw() +
  xlab("") +
  ylab("Summenscore") +
  geom_hline(yintercept = 22, color = "black") +
  geom_hline(yintercept = 24, color = "black", linetype = "dashed")
Box_3

Box_4 <- ggplot(Summenscores, aes(x = Schulart, y = Sum_X4)) +
  geom_boxplot() +
  theme_bw() +
  xlab("") +
  ylab("Summenscore") +
  geom_hline(yintercept = 10, color = "black") +
  geom_hline(yintercept = 16, color = "black", linetype = "dashed")
Box_4
#...............................................................................
Box_1 <- ggplot(Summenscores, aes(x = Schulart, y = Sum_X1)) +
  geom_boxplot() +
  theme_bw() +
  xlab("") +
  ylab("Summenscore") +
  ylim(0, 66) +
  geom_hline(yintercept = 16, color = "black") +
  geom_hline(yintercept = 23, color = "black", linetype = "dashed")
Box_1

Box_2 <- ggplot(Summenscores, aes(x = Schulart, y = Sum_X2)) +
  geom_boxplot() +
  theme_bw() +
  xlab("") +
  ylab("Summenscore") +
  ylim(0, 66) +
  geom_hline(yintercept = 26, color = "black") +
  geom_hline(yintercept = 29, color = "black", linetype = "dashed")
Box_2

Box_3 <- ggplot(Summenscores, aes(x = Schulart, y = Sum_X3)) +
  geom_boxplot() +
  theme_bw() +
  xlab("") +
  ylab("Summenscore") +
  ylim(0, 66) +
  geom_hline(yintercept = 22, color = "black") +
  geom_hline(yintercept = 24, color = "black", linetype = "dashed")
Box_3

Box_4 <- ggplot(Summenscores, aes(x = Schulart, y = Sum_X4)) +
  geom_boxplot() +
  theme_bw() +
  xlab("") +
  ylab("Summenscore") +
  ylim(0, 66) +
  geom_hline(yintercept = 10, color = "black") +
  geom_hline(yintercept = 16, color = "black", linetype = "dashed")
Box_4
