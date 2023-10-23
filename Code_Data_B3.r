##Phase_1: Analyse statistique et graphique d'une série temporelle:
##Objectif a et b:
#implrtation de DataFrame
Data_B3= read.table(file = file.choose(),header = F)
attach(Data_B3)
Data_B3
#Type de data
class(Data_B3)
#Convertir DataFrame en Serie temporelle
Data_B3.ts=ts(data = Data_B3, start = c(2002,1),end = c(2021,12) ,frequency =12 )
Data_B3.ts
#Effectuer un analyse a long terme
#Tandance , saisonalite , aleatoire
class(Data_B3.ts)
print(Data_B3.ts)
plot(Data_B3.ts,xlab="Date")
#interpretation_1:
#La série d'origine figure dans le graphique du Data_B3.ts il semble qu’il reste une tendance légèrement croissante.
tsp(Data_B3.ts)
plot(Data_B3.ts,plot.type = "multiple",lty = 50:300)
cycle(Data_B3.ts)
layout(1:1)
plot(aggregate(Data_B3.ts))
boxplot(Data_B3.ts~cycle(Data_B3.ts))
#Visualisation de tendance et de saisonnalité.
#interpretation_2:
#on a d'apres la graphique de la Date en fonction de V2 il ya un changement répété donc c'est une saisonnalité ,et cette saisonnalité est de période basse (Janvier-Décembre).
#Auto-corrélation
lag.plot(Data_B3.ts,lags=12)
acf(Data_B3.ts, lag.max = 12, type = c("correlation"), plot = FALSE)
#D'apres ACF on remarque que la serie data_B3.ts admet une tandance decoroissante.
#effectuer une analyse a court terrme:
modeleAditiveData_B3 = decompose(Data_B3.ts , "additive")
modeleMultiData_B3 = decompose(Data_B3.ts , "multiplicative")
plot(modeleAditiveData_B3)
plot(modeleMultiData_B3)
#on remarque ilya une difference au niveau du random donc on peut pas choisir la decomposition.
#Donc il faut calculer l'auto correlation.
#Le corrélogramme
acf(modeleAditiveData_B3$random , na.action = na.pass)
acf(modeleMultiData_B3$random , na.action = na.pass)
#la premiére modele est (admet set batons qui depassent l'intervalle ) on choisie modele additive
#tous les informations ont ete bien exploite
#danc on peut passé au ajustement
#interpretation: le coefficient pour le décalage 0.0833( càd 1/12) vaut 0.973.
#Phase_2: Ajustement de la série temporelle:
##Objectif 1 et 2:
####### Modélisation de la tendance :  𝑡=𝑚(𝑡)+𝜀𝑡, 𝑡∈ℕ #########
#AJUSTEMENT PAR LA METHODE DES MOYENNES MOBILES
#pour k = 4
moyenneMobileB4 = filter(Data_B3.ts,rep(1/4,4),method = "convolution")
plot(Data_B3.ts)
lines(moyenneMobileB4 , col="red")
#pour k = 5
moyenneMobileB5 = filter(Data_B3.ts,rep(1/5,5),method = "convolution")
plot(Data_B3.ts)
lines(moyenneMobileB5 , col="red")
#pour bien ajuster la tandance l'ordre a retenir est 4 car il va vous permettez 
#de eliminer la composante saisonniare et aleger la composante aleatoire
#AJUSTEMENT PAR LA MODELE LINEAIRES
t<- 1:length(Data_B3.ts)
#identification de coefficients des modele par la methode de moindre carrée
#liniaire
modelB1 = lm(Data_B3.ts~t)
#Quadratique
modelB2 = lm(Data_B3.ts~t+I(t^2))
AIC(modelB1) #1135.283
AIC(modelB2) #1108.278
#donc on choisie le modele 2 qui a le plus petit AIC
tempStandard <-  t - mean(t)/sd(t)
modelB3 <- lm(Data_B3.ts~tempStandard+I(tempStandard^2))
AIC(modelB3)
#la standardisation de temp t n a rien changer de parter deterministe
#representer graphiquement la fonction d autocorrelation de partie reseduelle
### AMELIORE LE MODELE ######
# il y a perte information cyclique
#extraction de la fonction cyclique  condition : frequence paire ( trimestre.. )
tps <-1 : length(Data_B3.ts)
MC<-matrix(0, length(Data_B3.ts), 6)
MS<-matrix(0, length(Data_B3.ts), 6)
for (i in 1:6) MC[ ,i]<-cos(2 * pi * tps * i/12)
for (i in 1:6) MS[ ,i]<-sin(2 * pi * tps* i/12)
modB3<-lm(Data_B3.ts~tps + MC + MS)
summary(modB3)
modB4<- lm(Data_B3.ts~tps + I(t^2) + I(t^3) + I(t^4) + I(t^5) + MC + MS)
summary(modB4)
AIC(modB4,modB3,modelB2,modelB1)
#on choisie le modele 4 qui a le plus petit AIC
##Phase_3: Analyse et modélisation des résidus issus de l’ajustement:
##Objectif a et b:
### Stationarite
acf(modB4$residuals)
#perte information cyclique
Box.test(modB4$residuals)
# notre p_value de test est trop inférieur à 0.05 on conclue qu'on
#peut modéliser la composante aléatoire a travers un modéle ARMA ou ARIMA
library(tseries)
adf.test(modB4$residuals)
#notre p_value = 0.06222 > 5% donc notre processus est stationnaire et par 
##suite on utilise ARIMA
acf(modB4$residuals)
#elle est stationnaire et contien une perte en passe au ARMA
#modalisation Sarima car on admet une differenciation au niveau du notre modele 4
# de plus notre modele est un modele ARMA saisoniere donc on va utiliser le modele
#SARIMA
install.packages("forecast")
library(forecast)
sarima<-auto.arima(modB4$residuals, d=1)
summary(sarima)
checkresiduals(sarima)
Box.test(sarima$residuals) #p-value=0.6768 > 5%
##le test de Box Pierce donne un p_value pour notre nouveau modele sarima 
##qui est superieur a 5% donc on constate que le modele4 est un bon modele
#*********Application du modele ARMA(forecasting/provision)*************
ar = auto.arima(Data_B3.ts)
ar
#notre modele est de saisonnalite de 12 et notre parametre sont (2,0,1) et 
#(0,1,1) qu'on va utiliser dans la provision
mod=arima(Data_B3.ts,order = c(2,0,1),seasonal= list(order=c(0,1,1),period=12))
plot(forecast(mod, 36))
