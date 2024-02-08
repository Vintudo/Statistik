## AIC + log. Regression + OD

load("Mehrthemenbefragung_Rohdaten.RData")

#Datenbereinigung
subset(dataset,nspo2 != 99) -> dataset1
which(dataset1$spo3_22 == "habe mich nicht verletzt")
dataset1[-(c(80,269,443,589)),] -> dataset2
dataset2[,-(c(3:28,34))] -> dataset3
subset(dataset3,spo1 != "k.A." & alter != "K.A." & demo9n != "K.A." & demo12 != "K.A." & altq != 0) -> Fertig_Dataset
Fertig_Dataset$nspo2 -> Fertig_Dataset$nspo3
Fertig_Dataset$nspo3[Fertig_Dataset$nspo3 >= 1] <- 1
Fertig_Dataset$nspo3[Fertig_Dataset$nspo3 == -1] <- 0

#Step-AIC
glm(data = Fertig_Dataset, nspo3 ~ ges + demo9n + demo12 + einw + altq + ges:altq,family = "binomial") -> START_MODEL_AIC
step(START_MODEL_AIC)

#Vorbereitung log. Regression
subset(dataset3, spo1 != "k.A." & altq != 0 & nspo2 != -1 & nspo2 != 99) -> Model_log.Regression
Model_log.Regression$nspo2 -> Model_log.Regression$nspo3
Model_log.Regression$nspo3[Model_log.Regression$nspo3 >= 1] <- 1

#Berechnung log. Regression
Model_log.Regression$ges <- relevel(Model_log.Regression$ges, ref = "weiblich")
glm(data = Model_log.Regression, nspo3 ~ ges + altq,family = "binomial") -> FINAL_MODEL_log.Regression
summary(FINAL_MODEL_log.Regression)

#Berechnung der Oddr Ratio
coef <- coef(FINAL_MODEL_log.Regression)
se <- summary(FINAL_MODEL_log.Regression)$coefficients[, 2]
OR <- exp(coef)
lower <- exp(coef - 1.96 * se)
upper <- exp(coef + 1.96 * se)
CI <- paste0("[", round(lower, 4), ", ", round(upper, 4), "]")
results <- data.frame(OR, CI)
rownames(results) <- names(coef)
results

#Berechnung der Oddr Ratio + Bonf.-Korrektur (bei 4 KI & 1 Tests (p-Wert))
coef <- coef(FINAL_MODEL_log.Regression)
se <- summary(FINAL_MODEL_log.Regression)$coefficients[, 2]
OR <- exp(coef)
lower <- exp(coef - 2.77 * se)
upper <- exp(coef + 2.77 * se)
CI <- paste0("[", round(lower, 4), ", ", round(upper, 4), "]")
results <- data.frame(OR, CI)
rownames(results) <- names(coef)
results

#Multikollinearität -> zur Überprüfung, dass die abhängigen Variablen nicht zu sehr korrelieren (nicht >5)
install.packages("car")
library("car")
vif(FINAL_MODEL_log.Regression)

plot(FINAL_MODEL_log.Regression)

