load("Mehrthemenbefragung_Rohdaten.RData")
library(dplyr) #für "select" -> Variablen Postion verändern
library(tidyr) #für "gather" -> Variablen zusammenführen 


#Variablen Namen geben 
names(dataset) <- c("Sportl.aktiv", "Verletzt", "Ballett","Basketball", "Dienssport","Eishockey","Fitness-/Kraftsport","Fußball","Handball","Inliner/Longboard","Kampfsport","Kletter","Laufen/Walking/Wandern","Parkour","Radfahren","Reitsport","Rudern","Schneesport", "Schulsport", "Tennis","Turnen/Gymnastik","Volleyball","Yoga","habe mich nicht verletzt","Sonstiges","Weiß nicht","K.A","Region","Geschlecht","Alter", "Schulabschluss", "Einkommen", "Ortsgröße","Gewichtungsfaktor","Alter(nummerisch)","Anzahl_Verletzungen")

#Variable Sportart erstellen + Reihenfolge ändern
dataset$Sportart_1 <- NA
dataset$Sportart_2 <- NA
dataset$Sportart_3 <- NA
dataset <- dataset %>% 
  select("Sportl.aktiv", "Verletzt", "Sportart_1","Sportart_2", "Sportart_3", "Ballett","Basketball", "Dienssport","Eishockey","Fitness-/Kraftsport","Fußball","Handball","Inliner/Longboard","Kampfsport","Kletter","Laufen/Walking/Wandern","Parkour","Radfahren","Reitsport","Rudern","Schneesport", "Schulsport", "Tennis","Turnen/Gymnastik","Volleyball","Yoga","habe mich nicht verletzt","Sonstiges","Weiß nicht","K.A","Region","Geschlecht","Alter", "Schulabschluss", "Einkommen", "Ortsgröße","Gewichtungsfaktor","Alter(nummerisch)","Anzahl_Verletzungen")

#Variablen zusammenführen (vom  "Wide-Format" ins "long-Format") 
dataset_long <- dataset
dataset_long <- gather(dataset_long, key = "Sportart", value = "Sportart_Aktive", "Ballett","Basketball", "Dienssport","Eishockey","Fitness-/Kraftsport","Fußball","Handball","Inliner/Longboard","Kampfsport","Kletter","Laufen/Walking/Wandern","Parkour","Radfahren","Reitsport","Rudern","Schneesport", "Schulsport", "Tennis","Turnen/Gymnastik","Volleyball","Yoga","habe mich nicht verletzt","Sonstiges","Weiß nicht","K.A", factor_key = TRUE)
