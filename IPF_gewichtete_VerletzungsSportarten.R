load("Mehrthemenbefragung_Rohdaten.RData")
var_namen <- c("Sportausuebung","Verletzungshaeufigkeit", "Ballett", "Basketball", 
               "Dienstsport", "Eishockey", "Fitness-/ Kraftsport", "Fußball", 
               "Handball", "Inliner/ Longboard", "Kampfsport", "Klettern", 
               "Laufen/ Walking/Nordic-Walking/ Wandern", "Parkour", "Radfahren", 
               "Reitsport", "Rudern", "Schneesport", "Schulsport", "Tennis", 
               "Turnen/ Gymnastik", "Volleyball", "Yoga", "habe mich nicht verletzt", 
               "sonstiges", "weiss nicht", "k.A.", "verletzt")
names(dataset)[c(1:27, 36) ] <- var_namen

#Datenbereinigung:
dat <- subset(dataset, verletzt != 99)
dat <- subset(dat, `habe mich nicht verletzt` != "habe mich nicht verletzt")
dat <- dat[, -24]



###### Gewichtete Anteile

### umcodierung
recodeIt <- function(sport){
  dat[, sport] <<- (dat[, sport] == sport) # 1 = sport ausgeuebt, 0 = rest

}

# nicht-Sportarten entfernen:
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










