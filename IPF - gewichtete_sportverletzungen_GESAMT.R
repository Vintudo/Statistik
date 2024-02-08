##### einlesen und namen aendern:
load("Mehrthemenbefragung_Rohdaten.RData")
var_namen <- c("Sportausuebung","Verletzungshaeufigkeit", "Ballett", "Basketball", 
               "Dienstsport", "Eishockey", "Fitness-/ Kraftsport", "Fußball", 
               "Handball", "Inliner/ Longboard", "Kampfsport", "Klettern", 
               "Laufen/ Walking/Nordic-Walking/ Wandern", "Parkour", "Radfahren", 
               "Reitsport", "Rudern", "Schneesport", "Schulsport", "Tennis", 
               "Turnen/ Gymnastik", "Volleyball", "Yoga", "habe mich nicht verletzt", 
               "sonstiges", "weiss nicht", "k.A.", "verletzt")
names(dataset)[c(1:27, 36) ] <- var_namen

# Quatsch entfernen:
dat <- subset(dataset, verletzt != 99)
dat <- subset(dat, `habe mich nicht verletzt` != "habe mich nicht verletzt")
dat <- dat[, -24]

###############Verteilung der Sportarten unter allen Verletzten###############

###### Gewichtete Anteile

### umcodierung
recodeIt <- function(sport){
  dat[, sport] <<- (dat[, sport] == sport) # 1 = sport ausgeuebt, 0 = rest
}

# nicht-Sportarten entfernen und umcodierung ausführen:
sportnamen <- var_namen[-c(1, 2, 24, 28)]
sapply(sportnamen, recodeIt)

### gewichteter Anteil der Sportarten an Verletzt Ja/Nein
dat_gewichtet <- dat[, 3:26] * dat$weight
gew_gesamt <- sum(dat_gewichtet)

gewRate <- function(sport){
  sum(dat_gewichtet[, sport]) / gew_gesamt
}

sort(sapply(sportnamen, gewRate), decreasing = TRUE)
sort(round(sapply(sportnamen, gewRate), digits = 2), decreasing = TRUE)

###################Wie viele (in%) m/w verletzten sich in Sportart X################### 
### für jede einzelne sportarten

# erstmal alter und geschlecht dranhängen:
dat_gewichtet_ga <- cbind(geschlecht=dat$ges, alter=dat$alter, dat_gewichtet)

## Sportart: Wandern:
# datensatz aufs wandern reduzieren
dat_gewichtet_wandern <- dat_gewichtet_ga[dat_gewichtet_ga$`Laufen/ Walking/Nordic-Walking/ Wandern` != 0,]
gew_gesamt_wandern <- sum(dat_gewichtet_wandern$`Laufen/ Walking/Nordic-Walking/ Wandern`)

## jetzt für einzelne gruppen
# zur vereinfachung der schreibarbeit: vw -> VerletzungWandern
vw <- dat_gewichtet_ga$`Laufen/ Walking/Nordic-Walking/ Wandern`

# Frauen und männer:
sum(vw[dat_gewichtet_ga$geschlecht == "weiblich"]) / gew_gesamt_wandern  
sum(vw[dat_gewichtet_ga$geschlecht == "männlich"]) / gew_gesamt_wandern 

# altersgruppen:
sum(vw[dat_gewichtet_ga$alter == "14-29 Jahre"]) / gew_gesamt_wandern 
sum(vw[dat_gewichtet_ga$alter == "30-44 Jahre"]) / gew_gesamt_wandern 
sum(vw[dat_gewichtet_ga$alter == "45-59 Jahre"]) / gew_gesamt_wandern 
sum(vw[dat_gewichtet_ga$alter == "60 Jahre und älter"]) / gew_gesamt_wandern 
sum(vw[dat_gewichtet_ga$alter == "K.A."]) / gew_gesamt_wandern 

##Sportart: Radfahren 
# datensatz aufs Radfahren reduzieren
dat_gewichtet_radfahren <- dat_gewichtet_ga[dat_gewichtet_ga$`Radfahren` != 0,]
gew_gesamt_radfahren <- sum(dat_gewichtet_radfahren$`Radfahren`)

## jetzt für einzelne gruppen
# zur vereinfachung der schreibarbeit: vw -> VerletzungRadfahren
vr<- dat_gewichtet_ga$`Radfahren`

# Frauen und männer:
sum(vr[dat_gewichtet_ga$geschlecht == "weiblich"]) / gew_gesamt_radfahren 
sum(vr[dat_gewichtet_ga$geschlecht == "männlich"]) / gew_gesamt_radfahren 

# altersgruppen:
sum(vr[dat_gewichtet_ga$alter == "14-29 Jahre"]) / gew_gesamt_radfahren 
sum(vr[dat_gewichtet_ga$alter == "30-44 Jahre"]) / gew_gesamt_radfahren 
sum(vr[dat_gewichtet_ga$alter == "45-59 Jahre"]) / gew_gesamt_radfahren
sum(vr[dat_gewichtet_ga$alter == "60 Jahre und älter"]) / gew_gesamt_radfahren
sum(vr[dat_gewichtet_ga$alter == "K.A."]) / gew_gesamt_radfahren

##Sportart: Fitness-/kraftsport 
# datensatz auf Fitness-/kraftsport  reduzieren
dat_gewichtet_Fitness <- dat_gewichtet_ga[dat_gewichtet_ga$`Fitness-/ Kraftsport` != 0,]
gew_gesamt_Fitness <- sum(dat_gewichtet_Fitness$`Fitness-/ Kraftsport`)

## jetzt für einzelne gruppen
# zur vereinfachung der schreibarbeit: vfk -> VerletzungFitness-/Kraftsport
vfk<- dat_gewichtet_ga$`Fitness-/ Kraftsport`

# Frauen und männer:
sum(vfk[dat_gewichtet_ga$geschlecht == "weiblich"]) / gew_gesamt_Fitness 
sum(vfk[dat_gewichtet_ga$geschlecht == "männlich"]) / gew_gesamt_Fitness 

# altersgruppen:
sum(vfk[dat_gewichtet_ga$alter == "14-29 Jahre"]) / gew_gesamt_Fitness
sum(vfk[dat_gewichtet_ga$alter == "30-44 Jahre"]) / gew_gesamt_Fitness 
sum(vfk[dat_gewichtet_ga$alter == "45-59 Jahre"]) / gew_gesamt_Fitness
sum(vfk[dat_gewichtet_ga$alter == "60 Jahre und älter"]) / gew_gesamt_Fitness
sum(vfk[dat_gewichtet_ga$alter == "K.A."]) / gew_gesamt_Fitness

###############TOP-3 Verletzungs-Sportarten unter allen Männer/Frauen/..Alter###############

# nur gewichte frauen (datensatz)
dat_gewichtet_frauen <- dat_gewichtet_ga[dat_gewichtet_ga$geschlecht == "weiblich", ]
# summe aller gew. verletzungen der frauen (spalte 1 und 2 löschen, weil keine sportart!): 
gew_gesamt_frauen <- sum(dat_gewichtet_frauen[, -c(1,2)])

# nur gewichte männer (datensatz)
dat_gewichtet_m <- dat_gewichtet_ga[dat_gewichtet_ga$geschlecht == "männlich", ]
# summe aller gew. verletzungen der männer (spalte 1 und 2 löschen, weil keine sportart!): 
gew_gesamt_m <- sum(dat_gewichtet_m[, -c(1,2)])


### berechnung top 3 verletzungssportart der frauen
sum(vw[dat_gewichtet_ga$geschlecht == "weiblich"]) / gew_gesamt_frauen

# Funktion zur berechnung Anteil der verletzungen pro Sportart nach geschlecht
gewRateGeschlecht <- function(sport, geschlecht){
  if(geschlecht == "weiblich") return(sum(dat_gewichtet_frauen[, sport]) / gew_gesamt_frauen)
  if(geschlecht == "männlich") return(sum(dat_gewichtet_m [, sport]) / gew_gesamt_m)
}

# z.B.: wandern und frauen:
gewRateGeschlecht("Laufen/ Walking/Nordic-Walking/ Wandern", "weiblich")
# und alles:
sort(sapply(sportnamen, gewRateGeschlecht, geschlecht = "weiblich"), decreasing = TRUE)
sort(sapply(sportnamen, gewRateGeschlecht, geschlecht = "männlich"), decreasing = TRUE)

#Gerundet 
sort(round(sapply(sportnamen, gewRateGeschlecht, geschlecht = "weiblich"), digits = 2), decreasing = TRUE)
sort(round(sapply(sportnamen, gewRateGeschlecht, geschlecht = "männlich"), digits = 2), decreasing = TRUE)

# beispiel alter:
dat_gewichtet_14 <- dat_gewichtet_ga[dat_gewichtet_ga$alter == "14-29 Jahre", ]
# summe aller gew. verletzungen der 14-29j (spalte 1 und 2 löschen, weil keine sportart!): 
gew_gesamt_14 <- sum(dat_gewichtet_14[, -c(1,2)])

dat_gewichtet_30 <- dat_gewichtet_ga[dat_gewichtet_ga$alter == "30-44 Jahre", ]
gew_gesamt_30 <- sum(dat_gewichtet_30[, -c(1,2)])

dat_gewichtet_45 <- dat_gewichtet_ga[dat_gewichtet_ga$alter == "45-59 Jahre", ]
gew_gesamt_45 <- sum(dat_gewichtet_45[, -c(1,2)])

dat_gewichtet_60 <- dat_gewichtet_ga[dat_gewichtet_ga$alter == "60 Jahre und älter", ]
gew_gesamt_60 <- sum(dat_gewichtet_60[, -c(1,2)])

gewRateAlter <- function(sport, alter){
  if(alter == "14-29 Jahre")        return(sum(dat_gewichtet_14[, sport]) / gew_gesamt_14)
  if(alter == "30-44 Jahre")        return(sum(dat_gewichtet_30[, sport]) / gew_gesamt_30)
  if(alter == "45-59 Jahre")        return(sum(dat_gewichtet_45[, sport]) / gew_gesamt_45)
  if(alter == "60 Jahre und älter") return(sum(dat_gewichtet_60[, sport]) / gew_gesamt_60)
}
sort(sapply(sportnamen, gewRateAlter, alter = "14-29 Jahre"), decreasing = TRUE)
sort(sapply(sportnamen, gewRateAlter, alter = "30-44 Jahre"), decreasing = TRUE)
sort(sapply(sportnamen, gewRateAlter, alter = "45-59 Jahre"), decreasing = TRUE)
sort(sapply(sportnamen, gewRateAlter, alter = "60 Jahre und älter"), decreasing = TRUE)

#Gerundet
sort(round(sapply(sportnamen, gewRateAlter, alter = "14-29 Jahre"), digits = 2), decreasing = TRUE)
sort(round(sapply(sportnamen, gewRateAlter, alter = "30-44 Jahre"), digits = 2), decreasing = TRUE)
sort(round(sapply(sportnamen, gewRateAlter, alter = "45-59 Jahre"), digits = 2), decreasing = TRUE)
sort(round(sapply(sportnamen, gewRateAlter, alter = "60 Jahre und älter"), digits = 2), decreasing = TRUE)


############%-Anteil an Verletzungen (Verletzunsgrate) unten allen Männer/Frauen..Alter############

dat_aktiv <- subset(dat, Sportausuebung == "ja, habe in den letzten 12 Monaten eine oder mehrere Sportarten zumindest hin und wieder ausgeübt")

# alle nicht-verletzten:
index_gesamt <- which(dat_aktiv[, "verletzt"] == 0)
nicht_verletzt_gesamt <- sum(dat_aktiv[index_gesamt, ]$weight)

# Berechnung der Verletzungsrate - gesamt
gew_gesamt_aktive <- sum(dat_aktiv$weight)
index_gesamt_Verletzte <- which(dat_aktiv[, "verletzt"] == 0)
gew_gesamt_nicht_Verletzte <- sum(dat_aktiv[index_gesamt_Verletzte, ]$weight)

(gew_gesamt_aktive - gew_gesamt_nicht_Verletzte) / gew_gesamt_aktive


# Berechnung der Verletzungsrate - geschlecht 
#Weiblich 
index_weiblich <- which(dat_aktiv[, "verletzt"] == 0 & dat_aktiv[, "ges"] == "weiblich")
nicht_verletzt_weiblich <- sum(dat_aktiv[index_weiblich, ]$weight)
gew_gesamt_frauen / (gew_gesamt_frauen + nicht_verletzt_weiblich) 

#Männlich 
index_m <- which(dat_aktiv[, "verletzt"] == 0 & dat_aktiv[, "ges"] == "männlich")
nicht_verletzt_m <- sum(dat_aktiv[index_m, ]$weight)
gew_gesamt_m / (gew_gesamt_m + nicht_verletzt_m)

# Berechnung der Verletzungsrate - alter 
#14-29 Jahre
index_14 <- which(dat_aktiv[, "verletzt"] == 0 & dat_aktiv[, "alter"] == "14-29 Jahre")
nicht_verletzt_14 <- sum(dat_aktiv[index_14, ]$weight)
gew_gesamt_14 / (gew_gesamt_14 + nicht_verletzt_14)

#30-44 Jahre 
index_30 <- which(dat_aktiv[, "verletzt"] == 0 & dat_aktiv[, "alter"] == "30-44 Jahre")
nicht_verletzt_30 <- sum(dat_aktiv[index_30, ]$weight)
gew_gesamt_30 / (gew_gesamt_30 + nicht_verletzt_30)

#45-59 Jahre 
index_45 <- which(dat_aktiv[, "verletzt"] == 0 & dat_aktiv[, "alter"] == "45-59 Jahre")
nicht_verletzt_45 <- sum(dat_aktiv[index_45, ]$weight)
gew_gesamt_45 / (gew_gesamt_45 + nicht_verletzt_45)

#60+ Jahre 
index_60 <- which(dat_aktiv[, "verletzt"] == 0 & dat_aktiv[, "alter"] == "60 Jahre und älter")
nicht_verletzt_60 <- sum(dat_aktiv[index_60, ]$weight)
gew_gesamt_60 / (gew_gesamt_60 + nicht_verletzt_60)