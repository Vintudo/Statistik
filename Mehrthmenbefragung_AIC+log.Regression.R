library(foreign)
library(MASS)

load("Mehrthemenbefragung_Rohdaten.RData")

subset(dataset,nspo2 != 99) -> NEU

which(NEU$spo3_22 == "habe mich nicht verletzt")

NEU[-(c(80,269,443,589)),] -> NEU1


NEU1[,-(c(3:28,34))] -> NEU2
subset(NEU2,spo1 != "k.A." & spo2 != -1 & alter != "K.A." & demo9n != "K.A." & demo12 != "K.A." & altq != 0 & nspo2 != -1) -> Fertig


Fertig$nspo2 -> Fertig$nspo3
Fertig$nspo3[Fertig$nspo3 >= 1] <- 1

glm(data = Fertig, nspo3 ~ ges + demo9n + demo12 + einw + altq + ges:altq,family = "binomial") -> START_MODEL
step(START_MODEL)

subset(NEU1, spo1 != "k.A." & altq != 0 & nspo2 != -1 & nspo2 != 99) -> FINAL_FINAL
FINAL_FINAL$nspo2 -> FINAL_FINAL$nspo3
FINAL_FINAL$nspo3[FINAL_FINAL$nspo3 >= 1] <- 1

glm(data = FINAL_FINAL, nspo3 ~ ges + altq,family = "binomial") -> FINAL_MODEL
summary(FINAL_MODEL)
