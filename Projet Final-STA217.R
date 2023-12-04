

################################################################
################ IMPORTER LA BASE DE DONNEES####################
################################################################
###### methode 1: en utilisant la fonction read.csv()
data_BTC=read.csv("C:/Users/LF ELEC/Desktop/CNAM/STA- 217/Project/BTC-USD.csv")
data_ETH=read.csv("C:/Users/LF ELEC/Desktop/CNAM/STA- 217/Project/ETH-USD.csv")

##### methode 2: en utilisant a fonction getsymbols()
library(tidyverse)
library(quantmod)
# --> Pour le Bitcoin: BTC
getSymbols(Symbols = "BTC-USD", src = "yahoo", 
           from = as.Date("2018-08-14"), to = as.Date("2023-08-14"))
# Utilisation de la fonction get() pour accéder aux données
data_BTC_yahoo<- get("BTC-USD")
class(data_BTC_yahoo)
head(data_BTC_yahoo)
dim(data_BTC_yahoo)

#--> Pour l'Ethereum: ETH
getSymbols(Symbols = "ETH-USD", src = "yahoo", 
           from = as.Date("2018-08-14"), to = as.Date("2023-08-14"))
# Utilisation de la fonction get() pour accéder aux données
data_ETH_yahoo<- get("ETH-USD")
class(data_ETH_yahoo)
head(data_ETH_yahoo)
dim(data_ETH_yahoo)


##########################################################################
#################### EXPLICATION SUR LA BASE DE DONNEES ###################
##########################################################################
library("Hmisc") 

class(data_BTC)
head(data_BTC)
tail(data_BTC)

summary(data_BTC)
summary_stats_BTC <- summary(data_BTC$Adj.Close); summary_stats_BTC
describe(data_BTC$Adj.Close)

##########

class(data_ETH)
head(data_ETH)
tail(data_ETH)

summary(data_ETH)
summary_stats_ETH <- summary(data_ETH$Adj.Close) ; summary_stats_ETH
describe(data_ETH$Adj.Close)



#############################################################################
###################### VISUALISER LA BASE DE DONNEES ########################
#############################################################################

###### Representation Graphique de la serie temporelle financier du BTC 
par(mfrow=c(2,2))
# Créez un histogramme
hist(data_BTC$Adj.Close, main="Histogramme du prix de BTC",
     col = "red",freq = TRUE, xlab = "Adjusted close value")
# Créez un boxplot
boxplot(data_BTC$Adj.Close, main="Boxplot du prix de BTC", 
        xlab = "Adjusted close value", col="blue" )

###### Representation Graphique de la serie temporelle financier d' ETH 
# Créez un histogramme
hist(data_ETH$Adj.Close, main="Histogramme du prix d' ETH",
     col = "red",freq = TRUE, xlab = "Adjusted close value")
# Créez un boxplot
boxplot(data_ETH$Adj.Close, main="Boxplot du prix d' ETH", 
        xlab = "Adjusted close value", col="blue" )



#############################################################################
########## #PRORIETES D'UNE SERIE TEMPORELLES FINANCIERES ###################
#############################################################################

##### en utilisant a fonction getsymbols() pour tracer les graphes
library(tidyverse)
library(quantmod)
# --> Pour le Bitcoin: BTC
getSymbols(Symbols = "BTC-USD", src = "yahoo", 
           from = as.Date("2018-08-14"), to = as.Date("2023-08-14"))
# Utilisation de la fonction get() pour accéder aux données
data_BTC_yahoo<- get("BTC-USD")
#--> Pour l'Ethereum: ETH
getSymbols(Symbols = "ETH-USD", src = "yahoo", 
           from = as.Date("2018-08-14"), to = as.Date("2023-08-14"))
# Utilisation de la fonction get() pour accéder aux données
data_ETH_yahoo<- get("ETH-USD")



#______________________________________________________________#
#______________________________________________________________#
###################################################################
###### propriete 1: stationnaire ou non-stationnaire ##############

####### --> Pour le BTC: on a 3 graphes
par(mfrow=c(1,2))
##### GRAPHE 1: Overture et Clôture
#graphique de l'ouverture
options(repr.plot.res = 300, repr.plot.height = 3)
plot(data_BTC_yahoo[, "BTC-USD.Open"], main = "Prix d'ouverture du BTC", col = "lightblue")
#graphique de la clôture
options(repr.plot.res = 300, repr.plot.height = 3)
plot(data_BTC_yahoo[, "BTC-USD.Close"], main = "Prix de la clôture du  BTC", col = "blue")

##### GRAPHE 2: Volatilite 
par(mfrow=c(1,1))
#graphique high et low
options(repr.plot.res = 300, repr.plot.height = 4.4)
plot.xts(data_BTC_yahoo[,1:2],legend.loc = "left", main = "Prix plus haut et plus bas", col = rainbow(n= 2))

##### GRAPHE 3: de comparaison 
#graphique d'action
options(repr.plot.res = 300, repr.plot.height = 4.4)
plot.xts(data_BTC_yahoo[,1:4],legend.loc = "left", main = "Prix plus haut et plus bas", col = rainbow(n= 4))


####### --> Pour l' ETH: on a 3 graphes
par(mfrow=c(1,2))
##### GRAPHE 1: Overture et Clôturehttp://127.0.0.1:32163/graphics/311cc117-b11e-4506-bd7c-4626feca18f2.png
#graphique de l'ouverture
options(repr.plot.res = 300, repr.plot.height = 3)
plot(data_ETH_yahoo[, "ETH-USD.Open"], main = "Prix d'ouverture du ETH", col = "lightblue")
#graphique de la clôture
options(repr.plot.res = 300, repr.plot.height = 3)
plot(data_ETH_yahoo[, "ETH-USD.Close"], main = "Prix de la clôture du  ETH", col = "blue")

##### GRAPHE 2: Volatilite 
par(mfrow=c(1,1))
#graphique high et low
options(repr.plot.res = 300, repr.plot.height = 4.4)
plot.xts(data_ETH_yahoo[,1:2],legend.loc = "left", main = "Prix plus haut et plus bas", col = rainbow(n= 2))

##### GRAPHE 3: de comparaison 
#graphique d'action
options(repr.plot.res = 300, repr.plot.height = 4.4)
plot.xts(data_ETH_yahoo[,1:4],legend.loc = "left", main = "Prix plus haut et plus bas", col = rainbow(n= 4))


#______________________________________________________________#
#______________________________________________________________#
#########################################################################
####### propriete 2: autocorrelation de carres des variations de prix ####

#Propriete 2: etape 1--> calculer le Rendement Rt
#calculer le rendement du BTC
Rendement_data_BTC=dailyReturn(data_BTC_yahoo)
head(Rendement_data_BTC)
#calculer le rendement d' ETH 
Rendement_data_ETH=dailyReturn(data_ETH_yahoo)
head(Rendement_data_ETH)


#Propriete 2: etape 2--> Representation Graphique du Rendement Rt & Rt^2
#graphique du rendement Rt(BTC) 
options(repr.plot.res = 300,rer.plot.height=3)
par(mfrow=c(2,1))
plot(Rendement_data_BTC,main="Rendement journalier du BTC")
#graphique de rendement au carre Rt^2(BTC)
Rendement_carre_data_BTC=Rendement_data_BTC^2
head(Rendement_carre_data_BTC)
plot(Rendement_carre_data_BTC, main="Carre du rendement journalier du BTC")

#graphique du rendement Rt(ETH) 
options(repr.plot.res = 300,rer.plot.height=3)
par(mfrow=c(2,1))
plot(Rendement_data_ETH,main="Rendement journalier d' ETH")
#graphique de rendement au carre Rt^2(BTC)
Rendement_carre_data_ETH=Rendement_data_ETH^2
head(Rendement_carre_data_ETH)
plot(Rendement_carre_data_ETH, main="Carre du rendement journalier d'ETH")



#Propriete 2: etape 3--> Etudier l'autocorrelation (existe une relation ou non)
#test de correlation de rendement

# --> Pour le BTC
#h0:abscence de correlation --> on rejette h0 et accepte h1 
#(car p-value = 0.03324  < 0.05 seuil de signification)
#il y a une corrélation significative dans les donnees
Box.test(Rendement_data_BTC,type="Box-Pierce")
#test de correlation du carre de rendement
#h0:abscence de correlation --> on rejette h0 et accepte h1
#(car p-value = 0.0002099 < 0.05 seuil de signification)
#il y a une corrélation significative dans les donnees
Box.test(Rendement_carre_data_BTC,type="Box-Pierce")

# --> Pour l' ETH
#h0:abscence de correlation --> on rejette h0
Box.test(Rendement_data_ETH,type="Box-Pierce")
#(car p-value = 0.006339 < 0.05 seuil de signification)
#il y a une corrélation significative dans les donnees
#test de correlation du carre de rendement
#h0:abscence de correlation --> on rejette h0
Box.test(Rendement_carre_data_ETH,type="Box-Pierce")
#(car p-value = 1.03e-07 < 0.05 seuil de signification)
#il y a une corrélation significative dans les donnees





#______________________________________________________________#
#______________________________________________________________#
#########################################################################
####### propriete 3: Queue de distribution epaisse ####

### propriete 3: etape 1 --> supposons que la dist est normale
library(tseries)
#h0:suit une loi normal --> on rejette h0
jarque.bera.test(Rendement_data_BTC)
jarque.bera.test(Rendement_data_ETH)

### propriete 3: etape 2 --> Etudier la normalite (Kurtosis, Skewness, p-vlue)
#h0:# D'Agostino Normality Test
library(fBasics)
dagoTest(Rendement_data_BTC)
dagoTest(Rendement_data_ETH)

### propriete 3: etape 3 --> Representation Graphique de la dist du queue
#graphique de queue pour comparer la dist du que et la dist normal
##--> en bleue c'est la dist des Rt reels
##--> en rouge c'est la dist normal aved sdt=Rt et moyenne=0
library(gridExtra)

#--> Pour le BTC
distribution_queue_BTC_1 = qplot(Rendement_data_BTC , geom = 'density') + geom_density(fill = 'blue' , alpha = 0.4) + 
  geom_density(aes(rnorm(200000 , 0 , sd(Rendement_data_BTC))) , fill = 'red' , alpha = 0.25) + 
  labs(x = '')

distribution_queue_BTC_2 = qplot(Rendement_data_BTC , geom = 'density') + geom_density(fill = 'blue' , alpha = 0.4) + 
  geom_density(aes(rnorm(200000 , 0 , sd(Rendement_data_BTC))) , fill = 'red' , alpha = 0.25) + 
  coord_cartesian(xlim = c(-0.07 , -0.02) , ylim = c(0 , 10)) + 
  geom_vline(xintercept = c(qnorm(p = c(0.01 , 0.05) , mean = mean(Rendement_data_BTC) , sd = sd(Rendement_data_BTC))) , 
             color = c('darkgreen' , 'green') , size = 1) + labs(x = 'Daily Returns')

grid.arrange(distribution_queue_BTC_1 , distribution_queue_BTC_2 , ncol = 1)

## ANALYSE : Si les rendements réels sont très éloignés de ces seuils (2 lignes vertes), 
# cela peut suggérer des comportements inhabituels ou une distribution non normale des rendements.
#N.B: La ligne verte verticale est une "ligne de référence" ou une "ligne de seuil".



#--> Pour l'ETH
distribution_queue_ETH_1 <- qplot(Rendement_data_ETH, geom = 'density') +
  geom_density(fill = 'blue', alpha = 0.4) +
  geom_density(aes(rnorm(200000, 0, sd(Rendement_data_ETH))), fill = 'red', alpha = 0.25) +
  labs(x = '')

distribution_queue_ETH_2 <- qplot(Rendement_data_ETH, geom = 'density') +
  geom_density(fill = 'blue', alpha = 0.4) +
  geom_density(aes(rnorm(200000, 0, sd(Rendement_data_ETH))), fill = 'red', alpha = 0.25) +
  coord_cartesian(xlim = c(-0.07, -0.02), ylim = c(0, 10)) +
  geom_vline(xintercept = c(qnorm(p = c(0.01, 0.05), mean = mean(Rendement_data_ETH), sd = sd(Rendement_data_ETH))),
             color = c('darkgreen', 'green'), size = 1) +
  labs(x = 'Daily Returns')

grid.arrange(distribution_queue_ETH_1, distribution_queue_ETH_2, ncol = 1)

## ANALYSE : Si les rendements réels sont très éloignés de ces seuils (2 lignes vertes), 
# cela peut suggérer des comportements inhabituels ou une distribution non normale des rendements.
#N.B: La ligne verte verticale est une "ligne de référence" ou une "ligne de seuil".



#______________________________________________________________#
#______________________________________________________________#
################################################################################
################# propriete 4: stationnarite du rendement #################

#############################################################################
## propriete 4: etape 1: Graphique représentant les variations entre les rendements
par(mfrow=c(2,1))
##--> Pour le BTC
d_Rendement_data_BTC<- diff(Rendement_data_BTC)
ts.plot(d_Rendement_data_BTC, type="l", col="blue")
##--> Pour l'ETH
d_Rendement_data_ETH<- diff(Rendement_data_ETH)
ts.plot(d_Rendement_data_ETH, type="l", col="blue")


#######################################################################
## propriete 4: etape 2: test de stationnarite :
##--> Pour le BTC
#h0: non-stationnaire --> on rejette h0 (car p-value = 0.01 < 0.05)
library(tseries)
d_Rendement_data_BTC=na.omit(d_Rendement_data_BTC)
adf.test(d_Rendement_data_BTC)
## ANALYSE : les données des différences de rendements du Bitcoin sont stationnaires, 
# ce qui suggère que les variations dans les rendements ont une structure constante 
#et ne présentent pas de tendances ou de schémas importants.


##--> Pour l' ETH
#test de stationnarite --> on rejette h0 (car p-value = 0.01 < 0.05)
library(tseries)
d_Rendement_data_ETH=na.omit(d_Rendement_data_ETH)
adf.test(d_Rendement_data_ETH)
# ANALYSE : les données des différences de rendements de l'Ethereum sont 
# également stationnaireS ce qui suggère que les variations dans les rendements 
# ont une structure constante et ne présentent pas de tendances ou de schémas importants.


#### CONCLUSION : En résumé, les tests de Dickey-Fuller augmenté indiquent que 
# les séries des différences de rendements pour le Bitcoin et l'Ethereum sont 
# stationnaires, ce qui est important lors de l'analyse de séries temporelles 
# car cela permet de travailler avec des données dont les propriétés statistiques 
#ne changent pas de manière significative dans le temps.



#______________________________________________________________#
#______________________________________________________________#
################################################################################
################# propriete 5: Queue epaisse conditionnelle #################

### POUR BTC & ETH:
#Propriete 5:etape 1: TESTER L'EXISTENCE DES EFFETS DE SAISON
#Puisqu'il existe des effets de saison --> ARIMA(p,d,q)
########________________________________________________####################

### POUR BTC:
#Propriete 5:etape 2: TESTER SI LA DISTRIBUTION DES ERREURS EST HETEROSCEDASTIQUE
# Puisqu'il existe d'heterosedasite --> ARHCH(p) et GARCH(p,q) et HARCH(p)
### Etape 2: 2-1 --> 2-3 :Etudier l'hétéroscédasticité des résidus sur differente 
#                        ordres p,d,q et travailler par "BEST MODEL" 
### Etape 2: 2-4 --> 2-6 :Etudier l'hétéroscédasticité des résidus sur differente 
#                        ordres p,d,q et travailler par "RANDOM MODEL" 
########________________________________________________####################
### POUR ETH:
#Propriete 5:etape 2: TESTER SI LA DISTRIBUTION DES ERREURS EST HETEROSCEDASTIQUE
# Puisqu'il existe d'heterosedasite --> ARHCH(p) et GARCH(p,q) et HARCH(p)
### Etape 2: 2-1 --> 2-3 :Etudier l'hétéroscédasticité des résidus sur differente 
#                        ordres p,d,q et travailler par "BEST MODEL" en utilisant 
#                     Methode 1: "ML" et Methode 2: "STEPWISE"
### Etape 2: 2-4 --> 2-6 :Etudier l'hétéroscédasticité des résidus sur differente 
#                        ordres p,d,q et travailler par "RANDOM MODEL" 

############### REMARQUE: 
# DANS BTC : le modèle est en mesure de capturer toute la variabilité des données
# DANS L'ETH :le modèle n'est pas en mesure de capturer toute la variabilité des données
########________________________________________________####################


### POUR BTC & ETH:
#Propriete 5:etape 3: TESTER SI LA DISTRIBUTION DES ERREURS EST NORMALE +
#                     REPRESENTATION GRAPHIQUE DES ERREURS
########________________________________________________####################



###########--> Pour le BTC
#######################################################################
## propriete 5: etape 1:Estimer l'ordre p et q :
#cette une serie ARIMA alors cherchons les ordres p et q

#choix de p et q
library(forecast)
#Calcul de l'autocorrélation et de l'autocorrélation partielle des données de rendement 
acf(d_Rendement_data_BTC)
ACF_d_Rendement_data_BTC=pacf(d_Rendement_data_BTC) 
#Utilisation de la fonction auto.arima pour trouver automatiquement les ordres
#p, d et q optimaux pour le modèle ARIMA :
model_BTC=auto.arima(d_Rendement_data_BTC,max.p=3,max.q=3,d=1,trace=TRUE,test="adf",stationary=F)
Best_model_BTC=arima(d_Rendement_data_BTC,order=c(1,1,2),method='ML')
Random_model_BTC=arima(d_Rendement_data_BTC,order=c(1,2,3),method='ML')


#######################################################################
## propriete 5: etape 2:Tester la dépendance temporelle des résidus (hétéroscédasticité) :
#effet heterocidacite des residus (bruit blanc depend du temps)
#variance varie au cours du temps il ya un heterocidacite

#propriete 5: etape 2-1): Calcul des résidus du modèle ARIMA
res_BTC=residuals(Best_model_BTC) 
#propriete 5: etape 2-2): Test de Box-Pierce pour l'hétéroscédasticité des résidus 
# h0:indépendance des résidus
# --> (p-value = 0.3281 n'est pas <0.05 alors on ne rejette pas h0 )
Box.test(res_BTC, lag = 10, type = "Box-Pierce")
#propriete 5: etape 2-3): Test de Ljung-Box pour l'hétéroscédasticité des résidus
# h0:indépendance des résidus
# --> (p-value = 0.3244 n'est pas <0.05 alors on ne rejette pas h0 )
Box.test(res_BTC,type="Ljung-Box",lag=10)

# ANALYSE : Ce qui indique que les ordre choisi p=1 , d=1 , q=2 ont changer 
# cette serie et maitenant les residus ne depend pas du temps, alors c'est pour 
# cela je veux prendre des valeurs d'ordre differents pour travailler par 
# ARCH(p), HARCH(p) et GARCH(p,q)
Random_model_BTC=arima(d_Rendement_data_BTC,order=c(1,2,3),method='ML')
#propriete 5: etape 2-4): Calcul des résidus du modèle ARIMA (Random model)
res_Random_model_BTC=residuals(Random_model_BTC) 
#propriete 5: etape 2-5): Test de Box-Pierce pour l'hétéroscédasticité des résidus 
# h0:indépendance des résidus
# --> (p-value < 2.2e-16  <0.05 alors rejette h0 )
Box.test(res_Random_model_BTC, lag = 10, type = "Box-Pierce")
#propriete 5: etape 2-6): Test de Ljung-Box pour l'hétéroscédasticité des résidus
# h0:indépendance des résidus
# --> (p-value < 2.2e-16  <0.05 alors on rejette h0 )
Box.test(res_Random_model_BTC,type="Ljung-Box",lag=10)


#######################################################################
## Propriete 5: etape 3: Tester la normalite des erreurs
# Pour savoir si la distribution des erreurs suivent une loi normale ou no

#propriete 5: etape 3-1) QQ-plot test (normality test)
#Test de normalite en utilisant QQ-plot -->pas de mormalite on rejette h0
qqnorm(res_BTC, col="green")
qqline(res_BTC, col = 2)  # Adding a reference line for normality

#propriete 5: etape 3-2) Jarque-Bera test (normality test)
# H0:les données suivent une distribution normale 
# H1:les données ne suivent pas une distribution normale.
# p-value < 2.2e-16 < 0.05 --> H0 is rejecter --> distribution not normal
jarque.bera.test(res_BTC)

#propriete 5: etape 3-2) Representation Graphique des residus
#plot des residus
tsdiag(Best_model_BTC, gof.lag=30)

## ANALYSE : les graphiques de diagnostic des séries temporelles

######################################################################################
############### GRAPHE 1: Graphique des résidus en fonction du temps#################
#Ce sous-graphique représente les résidus en fonction du temps.
#--> Cherchez des motifs ou des tendances dans les résidus. Des schémas périodiques ou d'autres structures indiqueraient que le modèle n'a pas capturé toutes les tendances.

#EXPLICATION SUR GRAPHE Standardized Residuals:
# --> CAS 1: Absence de Modèle :
# Si les résidus semblent être distribués de manière aléatoire autour de zéro (sans motif particulier) et ne montrent pas de tendance croissante ou décroissante, 
# cela suggère que le modèle a capturé correctement les tendances et les structures de la série temporelle.

# --> CAS 2: Tendance ou Motifs :
# Si vous observez une tendance croissante ou décroissante dans les résidus, cela peut indiquer que le modèle n'a pas réussi à capturer une tendance sous-jacente dans les données. 
# Cela peut signifier que le modèle est sous-spécifié ou que des facteurs importants n'ont pas été pris en compte.

# --> CAS 3: Modèle Saisonnier Inadéquat :
# Si vous remarquez des schémas périodiques ou des motifs récurrents dans les résidus, cela peut indiquer que le modèle n'a pas correctement capturé les variations saisonnières. 
# Cela peut nécessiter l'ajout de composantes saisonnières au modèle.

# --> CAS 4: Hétéroscédasticité :
# Si la dispersion des résidus semble augmenter ou diminuer avec le temps, cela peut indiquer l'hétéroscédasticité (variance non constante) des résidus. 
#Cela pourrait nécessiter des transformations ou des ajustements pour stabiliser la variance.

# --> CAS 5: Points Aberrants ou Anormaux :
# Si vous identifiez des valeurs aberrantes (outliers) ou des points inhabituels dans les résidus, cela peut indiquer des observations exceptionnelles non capturées par le modèle. 
# Cela peut nécessiter une enquête plus approfondie pour comprendre ces points atypiques.

#### ICI C'EST LE CAS 4= PRESENCE D'Hétéroscédasticité


######################################################################################
################### GRAPHE 2: Graphique ACF et PACF des résidus ###################
#Ces sous-graphiques montrent les fonctions d'autocorrélation (ACF) et d'autocorrélation partielle (PACF) des résidus.
#--> Recherchez des autocorrélations significatives en dehors des intervalles de confiance pour évaluer si les résidus sont autocorrélés.

#EXPLICATION SUR GRAPHE ACF:
#Graphique ACF (Fonction d'Autocorrélation) :
#L'ACF mesure la corrélation linéaire entre une observation et ses observations passées à différents retards. Voici ce que vous pourriez observer dans le graphique ACF :
#-->CAS 1: Si les autocorrélations diminuent rapidement et deviennent non significatives après quelques retards, cela suggère que les résidus ne sont pas significativement autocorrélés.
#-->CAS 2: Si des autocorrélations significatives persistent même après plusieurs retards, cela indique que les résidus peuvent être autocorrélés.
#-->CAS 3: Si l'ACF présente une oscillation sinusoidale (alternance de valeurs positives et négatives) au-dessus et en dessous des intervalles de confiance, cela peut indiquer une autocorrélation saisonnière.

#### ICI C'EST LE CAS 3= AUTOCORELLATION SAISONNERE


#EXPLICATION SUR GRAPHE PACF:
#Graphique PACF (Fonction d'Autocorrélation Partielle) :
#La PACF mesure la corrélation linéaire entre une observation et ses observations passées, en éliminant l'influence des retards intermédiaires. 
#-->CAS 1: Si les valeurs de la PACF s'atténuent rapidement et deviennent non significatives après quelques retards, cela suggère que les résidus ne sont pas significativement autocorrélés.
#-->CAS 2: Si des valeurs de la PACF significatives persistent après plusieurs retards, cela peut indiquer une autocorrélation dans les résidus.

#### ICI LE GRAPHE N'EXISTE PAS


######################################################################################
################### GRAPHE 3: Test de Ljung-Box des résidus  ###################
# Ce sous-graphique montre le test de Ljung-Box pour l'autocorrélation des résidus.
#--> Si les p-values pour les retards sont inférieures à un certain seuil (par exemple, 0.05), cela pourrait indiquer une autocorrélation non nulle.

# EXPLICATION SUE GRAPHE P-VALUE FOR Ljung-Box statisic
#1)Hypothèses du Test :
#L'hypothèse nulle (H0) : Les résidus sont indépendants (absence d'autocorrélation).
#L'hypothèse alternative (H1) : Les résidus sont autocorrélés jusqu'à un certain retard.

#2)Choix de la Lag :
#Vous devez spécifier le nombre de retards à considérer dans le test. Cela peut être basé sur des connaissances antérieures du domaine ou sur des essais et des erreurs. 
#Un choix courant est de choisir le retard maximal où les autocorrélations peuvent potentiellement être significatives.

#3)Calcul du Test :
# Le test de Ljung-Box est basé sur les autocorrélations partielles des résidus jusqu'au retard spécifié.
# Le test calcule une statistique de test (par exemple, le test de Ljung-Box Q-statistic) en utilisant les autocorrélations partielles et le nombre d'observations.

#4)Interprétation du Résultat :
#Le résultat du test est une valeur statistique (par exemple, le Q-statistic) et une p-value associée.
#-->CAS 1: Si la p-value est inférieure à un seuil prédéfini (par exemple, 0.05), vous pouvez rejeter l'hypothèse nulle et conclure qu'il y a une autocorrélation significative dans les résidus jusqu'au retard spécifié.
#-->CAS 2: Si la p-value est supérieure au seuil, vous ne pouvez pas rejeter l'hypothèse nulle et pouvez considérer que les résidus sont indépendants.

#5)Analyse des Résultats :
#-->CAS 1: Si la p-value est faible et que vous rejetez l'hypothèse nulle, cela suggère que le modèle ne capture pas complètement les structures de dépendance résiduelles.
#-->CAS 2: Si la p-value est supérieure au seuil de signification les résidus sont significativement autocorrélés. Cela peut suggérer que le modèle a capturé les dépendances temporelles dans les données.

#### ICI C'EST LE CAS 2= AUTOCORELLATION DEPENDANTE 
# (Pour BTC les p-values <0.05 tout le temps)
# (Pour ETH ls p-values >0.05 tout le temps)
# ce modele ARIMA(1,2,3) ne capture  pas complètement les structures de dépendance temporelle présentes dans les données de Bitcoin.




###########--> Pour l' ETH
#######################################################################
## propriete 5: etape 1:Estimer l'ordre p et q :
#cette une serie ARIMA alors cherchons les ordres p et q

#choix de p et q
library(forecast)
#Calcul de l'autocorrélation et de l'autocorrélation partielle des données de rendement 
acf(d_Rendement_data_ETH)
ACF_d_Rendement_data_ETH=pacf(d_Rendement_data_ETH) 
#Utilisation de la fonction auto.arima pour trouver automatiquement les ordres
#p, d et q optimaux pour le modèle ARIMA :
model_ETH=auto.arima(d_Rendement_data_ETH,max.p=3,max.q=3,d=1,trace=TRUE,test="adf",stationary=F)
Best_model_ETH_method1=arima(d_Rendement_data_ETH,order=c(1,1,0),method='ML')#--> methode 1: 
Best_model_ETH_method2=auto.arima(d_Rendement_data_ETH, stepwise = TRUE)# --> methode 2: stepewise
####### Alors on doit tester les 2 methodes pour choisir la meilleure:
##--> ARIMA(1,1,0) : methode 1: MAximum Likelihood --> se concentre sur l'estimation précise des paramètres
##--> ARIMA(1,0,0) : methode 2: Strepwise Methode --> explore les différentes combinaisons d'ordres en ajoutant ou en retirant 

###########Explication sur MAXIMUM LIKELIHOOD & STEPWISE :
# 1)La méthode "ML" est utilisée pour estimer les paramètres du modèle ARIMA (ordres p, d, q) en maximisant la vraisemblance des données observées sous le modèle.
# Elle vise à trouver les valeurs des paramètres qui rendent les données observées les plus probables compte tenu de la structure du modèle.
# L'estimation des paramètres se fait en trouvant les valeurs qui maximisent la fonction de vraisemblance, souvent en utilisant des algorithmes d'optimisation numérique.
# Cette méthode est basée sur une approche statistique et estime les paramètres de manière à ajuster au mieux les données en termes de distribution.

# 2)L'approche "stepwise" explore les différentes combinaisons d'ordres en ajoutant ou en retirant progressivement des termes autorégressifs (AR) ou moyennants (MA) 
#pour identifier le modèle qui minimise le critère d'information.
# Cette méthode vise à automatiser le processus de sélection des ordres du modèle en évaluant systématiquement différentes combinaisons.

Random_model_ETH=arima(d_Rendement_data_ETH,order=c(1,2,1),method='ML')
Best_model_ETH=arima(d_Rendement_data_ETH,order=c(1,1,0),method='ML')#--> methode 1: 

#######################################################################
## propriete 5: etape 2:Tester la dépendance temporelle des résidus (hétéroscédasticité) :
#effet heterocidacite des residus (bruit blanc depend du temps)
#variance varie au cours du temps il ya un heterocidacite

#propriete 5: etape 2-1): Calcul des résidus du modèle ARIMA
res_ETH_method1=residuals(Best_model_ETH_method1) 
res_ETH=residuals(Best_model_ETH_method1) 
res_ETH_method2=residuals(Best_model_ETH_method2) 
#propriete 5: etape 2-2): Test de Box-Pierce pour l'hétéroscédasticité des résidus 
# h0:indépendance des résidus
# --> (p-value < 2.2e-16 <0.05 alors on rejette  h0 ) --> existe des effets d'hétéroscédasticité
Box.test(res_ETH_method1, lag = 10, type = "Box-Pierce")
Box.test(res_ETH_method2, lag = 10, type = "Box-Pierce")
#propriete 5: etape 2-3): Test de Ljung-Box pour l'hétéroscédasticité des résidus
# h0:indépendance des résidus
# --> (p-value < 2.2e-16 <0.05 alors on rejette h0 ) --> existe des effets d'hétéroscédasticité
Box.test(res_ETH_method1,type="Ljung-Box",lag=10)
Box.test(res_ETH_method2,type="Ljung-Box",lag=10)

# ANALYSE : Ce qui indique que les ordre choisi p=1 , d=1 , q=2 ont changer 
# cette serie et maitenant les residus ne depend pas du temps, alors c'est pour 
# cela je veux prendre des valeurs d'ordre differents pour travailler par 
# ARCH(p), HARCH(p) et GARCH(p,q)
Random_model_ETH=arima(d_Rendement_data_ETH,order=c(1,2,1),method='ML')
#propriete 5: etape 2-4): Calcul des résidus du modèle ARIMA (Random model)
res_Random_model_ETH=residuals(Random_model_ETH) 
#propriete 5: etape 2-5): Test de Box-Pierce pour l'hétéroscédasticité des résidus 
# h0:indépendance des résidus
# --> (p-value < 2.2e-16  <0.05 alors rejette h0 )
Box.test(res_Random_model_ETH, lag = 10, type = "Box-Pierce")
#propriete 5: etape 2-6): Test de Ljung-Box pour l'hétéroscédasticité des résidus
# h0:indépendance des résidus
# --> (p-value < 2.2e-16  <0.05 alors on rejette h0 )
Box.test(res_Random_model_ETH,type="Ljung-Box",lag=10)


#######################################################################
## Propriete 5: etape 3: Tester la normalite des erreurs
# Pour savoir si la distribution des erreurs suivent une loi normale ou no

#propriete 5: etape 3-1) QQ-plot test (normality test)
#Test de normalite en utilisant QQ-plot -->pas de mormalite on rejette h0
par(mfrow=c(1,2))
#--> methode 1: ML
qqnorm(res_ETH_method1, col="green")
qqline(res_ETH_method1, col = 2)  # Adding a reference line for normality
#--> methode 2: Stepwise
qqnorm(res_ETH_method2, col="green")
qqline(res_ETH_method2, col = 2)  # Adding a reference line for normality

#propriete 5: etape 3-2) Jarque-Bera test (normality test)
# H0:les données suivent une distribution normale 
# H1:les données ne suivent pas une distribution normale.
# p-value < 2.2e-16 < 0.05 --> H0 is rejecter --> distribution not normal
jarque.bera.test(res_ETH_method1)
jarque.bera.test(res_ETH_method2)


#propriete 5: etape 3-2) Representation Graphique des residus
#plot des residus
plot1=tsdiag(Best_model_ETH_method1, gof.lag=30)
plot2=tsdiag(Best_model_ETH_method2, gof.lag=30)



## ANALYSE : les graphiques de diagnostic des séries temporelles

######################################################################################
############### GRAPHE 1: Graphique des résidus en fonction du temps#################
#Ce sous-graphique représente les résidus en fonction du temps.
#--> Cherchez des motifs ou des tendances dans les résidus. Des schémas périodiques ou d'autres structures indiqueraient que le modèle n'a pas capturé toutes les tendances.

#EXPLICATION SUR GRAPHE Standardized Residuals:
# --> CAS 1: Absence de Modèle :
# Si les résidus semblent être distribués de manière aléatoire autour de zéro (sans motif particulier) et ne montrent pas de tendance croissante ou décroissante, 
# cela suggère que le modèle a capturé correctement les tendances et les structures de la série temporelle.

# --> CAS 2: Tendance ou Motifs :
# Si vous observez une tendance croissante ou décroissante dans les résidus, cela peut indiquer que le modèle n'a pas réussi à capturer une tendance sous-jacente dans les données. 
# Cela peut signifier que le modèle est sous-spécifié ou que des facteurs importants n'ont pas été pris en compte.

# --> CAS 3: Modèle Saisonnier Inadéquat :
# Si vous remarquez des schémas périodiques ou des motifs récurrents dans les résidus, cela peut indiquer que le modèle n'a pas correctement capturé les variations saisonnières. 
# Cela peut nécessiter l'ajout de composantes saisonnières au modèle.

# --> CAS 4: Hétéroscédasticité :
# Si la dispersion des résidus semble augmenter ou diminuer avec le temps, cela peut indiquer l'hétéroscédasticité (variance non constante) des résidus. 
#Cela pourrait nécessiter des transformations ou des ajustements pour stabiliser la variance.

# --> CAS 5: Points Aberrants ou Anormaux :
# Si vous identifiez des valeurs aberrantes (outliers) ou des points inhabituels dans les résidus, cela peut indiquer des observations exceptionnelles non capturées par le modèle. 
# Cela peut nécessiter une enquête plus approfondie pour comprendre ces points atypiques.

#### ICI C'EST LE CAS 4= PRESENCE D'Hétéroscédasticité


######################################################################################
################### GRAPHE 2: Graphique ACF et PACF des résidus ###################
#Ces sous-graphiques montrent les fonctions d'autocorrélation (ACF) et d'autocorrélation partielle (PACF) des résidus.
#--> Recherchez des autocorrélations significatives en dehors des intervalles de confiance pour évaluer si les résidus sont autocorrélés.

#EXPLICATION SUR GRAPHE ACF:
#Graphique ACF (Fonction d'Autocorrélation) :
#L'ACF mesure la corrélation linéaire entre une observation et ses observations passées à différents retards. Voici ce que vous pourriez observer dans le graphique ACF :
#-->CAS 1: Si les autocorrélations diminuent rapidement et deviennent non significatives après quelques retards, cela suggère que les résidus ne sont pas significativement autocorrélés.
#-->CAS 2: Si des autocorrélations significatives persistent même après plusieurs retards, cela indique que les résidus peuvent être autocorrélés.
#-->CAS 3: Si l'ACF présente une oscillation sinusoidale (alternance de valeurs positives et négatives) au-dessus et en dessous des intervalles de confiance, cela peut indiquer une autocorrélation saisonnière.

#### ICI C'EST LE CAS 3= AUTOCORELLATION SAISONNERE


#EXPLICATION SUR GRAPHE PACF:
#Graphique PACF (Fonction d'Autocorrélation Partielle) :
#La PACF mesure la corrélation linéaire entre une observation et ses observations passées, en éliminant l'influence des retards intermédiaires. 
#-->CAS 1: Si les valeurs de la PACF s'atténuent rapidement et deviennent non significatives après quelques retards, cela suggère que les résidus ne sont pas significativement autocorrélés.
#-->CAS 2: Si des valeurs de la PACF significatives persistent après plusieurs retards, cela peut indiquer une autocorrélation dans les résidus.

#### ICI LE GRAPHE N'EXISTE PAS


######################################################################################
################### GRAPHE 3: Test de Ljung-Box des résidus  ###################
# Ce sous-graphique montre le test de Ljung-Box pour l'autocorrélation des résidus.
#--> Si les p-values pour les retards sont inférieures à un certain seuil (par exemple, 0.05), cela pourrait indiquer une autocorrélation non nulle.

#### ICI L'AUTOCORRELATION EST NON NULL CAR TOUTES LES P-VALUE A PEU PRES =0 (CTE)
# (Pour BTC les p-values <0.05 tout le temps)
# (Pour ETH ls p-values >0.05 tout le temps)
# ce modèle ARIMA (1,2,1) capture efficacement les dépendances temporelles présentes dans les données d'Ethereum.








##############################################################################
##################### APPLICATION SUR ARCH ET GARCH ##########################

#Etape 1 Tester s'il exist des effets ARCH --> ARCH test
#test h0:pas d'effet arch vs h1:exist d'effet ARCH

#Etape 2: Creer un modele pour travailler
#modele ARCH(p) et GARCH(p,q)

#Etape 3: Ajusté le modèle GARCH spécifié aux données de résidus du Bitcoin 
# en utilisant la fonction ugarchfit()

#Etape 4: TESTER LES variances conditionneles (GARCH)

#Etape 5: TESTER DE NOUVEAU LES RESIDUS APRES TRANSFORMATION (GARCH)

#ETAPE 6:Representation grapgique d'un histogramme des rendements

########################################################################
########################### --> Pour le BTC :

#Etape 1 Tester s'il exist des effets ARCH --> ARCH test
#test h0:pas d'effet arch vs h1:exist d'effet ARCH
# p-value = 0.003074 < 0.05 -->on rejette h0--> on a un effet d'heterocadicite
library(FinTS)
ArchTest(res_BTC,lag=1)

#Etape 2: Creer un modele pour travailler
#modele ARCH(p) et GARCH(p,q)
#ugarchspec(). Cela crée un objet spécification de modèle GARCH qui définit les caractéristiques et les paramètres du modèle à estimer.
library(rugarch)
library(rmgarch)
univ_garch = ugarchspec()
univ_garch

#Etape 3: Ajusté le modèle GARCH spécifié aux données de résidus du Bitcoin en utilisant la fonction ugarchfit()
#Cette fonction prend en entrée la spécification du modèle et les données et renvoie un objet de modèle ajusté contenant les résultats de l'estimation.
#prendre les parametres a estimer pour creer le model
model_estime_BTC <- ugarchfit(spec = univ_garch, data = res_BTC)
model_estime_BTC
#les parametres estimes du modele GARCH
model_estime_BTC@fit$matcoef
###### ANALYSE :
# Partie 1: Conditional Variance Dynamics
#le modele obtenu apres l'application du modele GARCH--> sGARCH(1,1)
#--> ce qui signifie qu'il a un ordre GARCH de 1 et un ordre ARCH de 1.

# Partie 2: Optimal Parameters
#Cette section affiche les estimations des paramètres du modèle GARCH. Chaque paramètre est accompagné de son estimation, de son écart type, de la valeur de t-test et de la p-value correspondante.
#--> GARCH (omega, alpha1, beta1).

# Partie 3: Robust Standard Errors :
#Cette section affiche les estimations des paramètres du modèle GARCH, mais cette fois avec des erreurs standard robustes.

# Partie 4: LogLikelihood :
#Le LogLikelihood est une mesure de l'ajustement du modèle aux données. Un LogLikelihood plus élevé indique un meilleur ajustement.

# Partie 5: Information Criteria :
#Ces critères d'information (Akaike, Bayes, Shibata, Hannan-Quinn) sont utilisés pour évaluer la qualité de l'ajustement du modèle.

# Partie 6: Weighted Ljung-Box Test on Standardized Residuals :
#Ce test évalue l'autocorrélation dans les résidus standardisés du modèle. Il teste l'hypothèse nulle d'absence de corrélation jusqu'au retard spécifié. Les statistiques de test et les p-values sont fournies.

# Partie 7: Weighted Ljung-Box Test on Standardized Squared Residuals :
#Ce test évalue l'autocorrélation dans les carrés des résidus standardisés du modèle.

# Partie 8: Weighted ARCH LM Tests :
#Ces tests évaluent l'autocorrélation conditionnelle hétéroscédastique des résidus. Ils testent si les résidus présentent une hétéroscédasticité conditionnelle significative.

# Partie 9: Nyblom stability test :
#Ce test évalue la stabilité des paramètres du modèle.

# Partie 10: Sign Bias Test :
#Ce test évalue la présence de biais de signe dans les résidus.

# Partie 11: Adjusted Pearson Goodness-of-Fit Test :
#Ces tests évaluent l'ajustement global du modèle aux données, en comparant les statistiques de test avec des valeurs critiques.

### CONCLUSION : 
#les résultats suggèrent que le modèle GARCH ajusté aux données de résidus du Bitcoin semble bien capturer les caractéristiques temporelles et de volatilité des données. 
#Les tests de diagnostic indiquent une absence significative d'autocorrélation conditionnelle hétéroscédastique dans les résidus (car on a prit des le debut des ordres optimaux)
#######--> Vous pouvez conclure que ce modèle sGARCH-ARMA semble bien ajusté aux données de résidus du Bitcoin en fonction des tests de diagnostic effectués et des mesures d'ajustement obtenues. 
#######-> Cela permet de capturer la volatilité et la dynamique de la moyenne de la série temporelle.




#Etape 4: TESTER LES variances conditionneles (GARCH)
#etape 4-1: variances conditionnelles estimées
variance_BTC <- ts(model_estime_BTC@fit$var)
names(variance_BTC) <- "variance"
head(variance_BTC)
par(mfrow=c(1,3))
#etape 4-2: génère un graphique qui montre comment les variances conditionnelles évoluent dans le temps.
plot(variance_BTC, main = "variance conditionnelle estimee", col = "lightblue")
#etape 4-3: Calcul et tracé des résidus :
resg_BTC=residuals(model_estime_BTC)
options(repr.plot.res = 300, repr.plot.height = 3) 
#etape 4-4: trace la série temporelle des résidus du modèle GARCH. 
plot.ts(resg_BTC, main = "Residus du modele", col = "lightblue")




#Etape 5: TESTER DE NOUVEAU LES RESIDUS APRES TRANSFORMATION (GARCH)
#etape 5-1: calculer le residus au caree et representer graphiquement
resid2_BTC <- ts(model_estime_BTC@fit$residuals^2)
plot.ts(resid2_BTC, main = "Carre des residus du modele", col = "lightblue")
#etape 5-2: Test la normalite des erreurs du modele GARCH (jarque.bera.test)
#test de normalite des residus de model garch (#pas necessaire de la normalite)
# (p-value < 2.2e-16 < 0.05 --> les erreurs suivent une distribution normale)
jarque.bera.test(resg_BTC)
#etape 5-3: tester l'existence des effets d'hétéroscédasticité
#Box-Ljung test
# h0:indépendance des résidus
# --> (p-value < p-value = 0.3373 n'est pas <0.05 alors on ne rejette pas h0 ) --> n'existe pas des effets d'hétéroscédasticité
Box.test(resg_BTC,type="Ljung-Box",lag=10)

##REMARQUE: APRES TRANSFORMATION DE CETTE SERIE TEMPORELLE EN UTILISANT GARCH ALORS ON A CHOISIT 
#           P=1 ET Q=1 ON A OBTENU QUE LES RESIDUS:
#         1) NE DEPEND PAS DU TEMPS
#         2) N'EXISTE PAS D'EFFETS D'HETEROSCEDASITE
#--> donc on accepte l'hypothese d'independance les residus de modele GRACH(1,1)



#ETAPE 6:Representation grapgique d'un histogramme des rendements
#Representation grapgique d'un  histogramme des rendements du Bitcoin 
#en ajoutant une distribution rouge pour les rendements inférieurs au quantile à 5%.
#variance ne depand pas de temps
#VAR PAR GARCH
library(tidyverse)
library(ggthemes)
library(forecast)
library(gridExtra)
library(rugarch)

# Calculer les quantiles :
#--> Cela vous donnera la valeur en dessous de laquelle se situent 5% des rendements.
quantile(Rendement_data_BTC , 0.05)

# Tracé d'un histogramme des rendements : ( in light blue)
# Tracé d'un histogramme des rendements inférieurs au quantile à 5%  ( in red)
qplot(Rendement_data_BTC  , geom = 'histogram') + geom_histogram(fill = 'lightblue' , bins = 30) +
  geom_histogram(aes(Rendement_data_BTC [Rendement_data_BTC  < quantile(d_Rendement_data_BTC , 0.05)]) , fill = 'red' , bins = 30) +
  labs(x = 'Daily Returns')

### CONCLUSION:Voici quelques raisons pour lesquelles vous pourriez souhaiter visualiser ces rendements inférieurs :
# 1)Détection d'événements extrêmes : Les rendements qui sont significativement en dessous de la moyenne peuvent indiquer des événements exceptionnels ou des conditions inhabituelles sur le marché.
# 2)Identification des crises financières :  Les périodes de crises financières ou d'instabilité économique peuvent entraîner des rendements anormalement bas
# 3)Évaluation de la volatilité : Les rendements inférieurs à un seuil peuvent être associés à une volatilité accrue, car les variations sont plus prononcées dans ces périodes.
# 4)Gestion des risques : Les rendements inférieurs à un certain seuil peuvent être pertinents pour la gestion des risques.
# 5)Confirmation de modèles financiers : La visualisation des rendements inférieurs peut aider à confirmer si le modèle capture correctement la volatilité pendant ces périodes.
######## En somme, visualiser les rendements inférieurs à un certain seuil permet d'obtenir des informations précieuses sur 
#-->la dynamique du marché, 
#-->la volatilité, 
#-->les risques et les événements exceptionnels. 
#Cela peut jouer un rôle essentiel dans l'analyse financière et la prise de décision.





#######################################################################################
########################### --> Pour l' ETH :
#Etape 1 Tester s'il exist des effets ARCH --> ARCH test
#test h0:pas d'effet arch vs h1:exist d'effet ARCH
# p-value < 2.2e-16 < 0.05 -->on rejette h0--> on a un effet d'heterocadicite
library(FinTS)
ArchTest(res_ETH,lag=1)

##################################################################################
################################ MODELE GARCH(1,1)
################################ MODELE GARCH(2,2)

#Etape 2: Creer un modele pour travailler
#modele ARCH(p) et GARCH(p,q)
#ugarchspec(). Cela crée un objet spécification de modèle GARCH qui définit les caractéristiques et les paramètres du modèle à estimer.
library(rugarch)
library(rmgarch)
univ_garch = ugarchspec()
univ_garch

#Etape 3: Ajusté le modèle GARCH spécifié aux données de résidus du Bitcoin en utilisant la fonction ugarchfit()
#Cette fonction prend en entrée la spécification du modèle et les données et renvoie un objet de modèle ajusté contenant les résultats de l'estimation.
#prendre les parametres a estimer pour creer le model
model_estime_ETH<- ugarchfit(spec = univ_garch, data = res_ETH)
model_estime_ETH
#les parametres estimes du modele GARCH
model_estime_ETH@fit$matcoef
###### ANALYSE :
# Partie 1: Conditional Variance Dynamics
#le modele obtenu apres l'application du modele GARCH--> sGARCH(1,1)
#--> ce qui signifie qu'il a un ordre GARCH de 1 et un ordre ARCH de 1.

# Partie 2: Optimal Parameters
#Cette section affiche les estimations des paramètres du modèle GARCH. Chaque paramètre est accompagné de son estimation, de son écart type, de la valeur de t-test et de la p-value correspondante.
#--> GARCH (omega, alpha1, beta1).

# Partie 3: Robust Standard Errors :
#Cette section affiche les estimations des paramètres du modèle GARCH, mais cette fois avec des erreurs standard robustes.

# Partie 4: LogLikelihood :
#Le LogLikelihood est une mesure de l'ajustement du modèle aux données. Un LogLikelihood plus élevé indique un meilleur ajustement.

# Partie 5: Information Criteria :
#Ces critères d'information (Akaike, Bayes, Shibata, Hannan-Quinn) sont utilisés pour évaluer la qualité de l'ajustement du modèle.

# Partie 6: Weighted Ljung-Box Test on Standardized Residuals :
#Ce test évalue l'autocorrélation dans les résidus standardisés du modèle. Il teste l'hypothèse nulle d'absence de corrélation jusqu'au retard spécifié. Les statistiques de test et les p-values sont fournies.

# Partie 7: Weighted Ljung-Box Test on Standardized Squared Residuals :
#Ce test évalue l'autocorrélation dans les carrés des résidus standardisés du modèle.

# Partie 8: Weighted ARCH LM Tests :
#Ces tests évaluent l'autocorrélation conditionnelle hétéroscédastique des résidus. Ils testent si les résidus présentent une hétéroscédasticité conditionnelle significative.

# Partie 9: Nyblom stability test :
#Ce test évalue la stabilité des paramètres du modèle.

# Partie 10: Sign Bias Test :
#Ce test évalue la présence de biais de signe dans les résidus.

# Partie 11: Adjusted Pearson Goodness-of-Fit Test :
#Ces tests évaluent l'ajustement global du modèle aux données, en comparant les statistiques de test avec des valeurs critiques.

### CONCLUSION : 
#les résultats suggèrent que le modèle GARCH ajusté aux données de résidus d'ethereum NE semble PAS bien capturer les caractéristiques temporelles et de volatilité des données. 
#Les tests de diagnostic indiquent une PRESENCE significative d'autocorrélation conditionnelle hétéroscédastique dans les résidus (car on a prit des le debut des ordres optimaux)
#######--> Vous pouvez conclure que ce modèle sGARCH-ARMA semble bien ajusté aux données de résidus d'Ethereum en fonction des tests de diagnostic effectués et des mesures d'ajustement obtenues. 
#######-> Cela permet de capturer la volatilité et la dynamique de la moyenne de la série temporelle.


#################################################################################
##################### MODELE GARCH(1,1)

#Etape 4: TESTER LES variances conditionneles (GARCH)
#etape 4-1: variances conditionnelles estimées
variance_ETH <- ts(model_estime_ETH@fit$var)
names(variance_ETH) <- "variance"
head(variance_ETH)
par(mfrow=c(1,3))
#etape 4-2: génère un graphique qui montre comment les variances conditionnelles évoluent dans le temps.
plot(variance_ETH, main = "variance conditionnelle estimee", col = "lightblue")
#etape 4-3: Calcul et tracé des résidus :
resg_ETH=residuals(model_estime_ETH)
options(repr.plot.res = 300, repr.plot.height = 3) 
#etape 4-4: trace la série temporelle des résidus du modèle GARCH. 
plot.ts(resg_ETH, main = "Residus du modele", col = "lightblue")



#Etape 5: TESTER DE NOUVEAU LES RESIDUS APRES TRANSFORMATION (GARCH)
#etape 5-1: calculer le residus au caree et representer graphiquement
resid2_ETH <- ts(model_estime_ETH@fit$residuals^2)
plot.ts(resid2_ETH, main = "Carre des residus du modele", col = "lightblue")
#etape 5-2: Test la normalite des erreurs du modele GARCH (jarque.bera.test)
#test de normalite des residus de model garch (#pas necessaire de la normalite)
# (p-value < 2.2e-16 < 0.05 --> les erreurs suivent une distribution normale)
jarque.bera.test(resg_ETH)
#etape 5-3: tester l'existence des effets d'hétéroscédasticité
#Box-Ljung test
# h0:indépendance des résidus
# --> (p-value < p-value = 0.3373 n'est pas <0.05 alors on ne rejette pas h0 ) --> n'existe pas des effets d'hétéroscédasticité
Box.test(resg_ETH,type="Ljung-Box",lag=10)

##REMARQUE: APRES TRANSFORMATION DE CETTE SERIE TEMPORELLE EN UTILISANT GARCH ALORS ON A CHOISIT 
#           P=1 ou 2 ET Q=1 ou 2 ON A OBTENU QUE LES RESIDUS:
#         1) NE DEPEND PAS DU TEMPS
#         2) EXISTE D'EFFETS D'HETEROSCEDASITE
#--> donc on n'accepte pas l'hypothese d'independance les residus de modele GRACH(1,1)


### Etape 4 et Etape 5 --> MODELE GARCH(1,1)
################# APPLICATION SUR ARCH ET GARCH  #################################
#################################################################################
########################### MODELE GARCH(1,1)
library(rugarch)
library(FinTS)

# Spécification du modèle GARCH(1,1)
garch_order <- c(1, 1)  # Ordre GARCH(1,1)
spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = garch_order),
                   mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
                   distribution.model = "std")
# Ajustement du modèle GARCH(1,1) aux données
model <- ugarchfit(spec = spec, data = res_ETH)
# Affichage des résultats du modèle GARCH(1,1)
print(model)
# Tester la normalité des résidus
jarque.bera.test(residuals(model))
# Tester l'autocorrélation des résidus
Box.test(residuals(model), type = "Ljung-Box", lag = 10)



### Etape 4 et Etape 5 --> MODELE GARCH(2,2)
################# APPLICATION SUR ARCH ET GARCH  #################################
#################################################################################
########################### MODELE GARCH(2,2)
library(rugarch)
library(FinTS)

# Spécification du modèle GARCH(2,2)
garch_order <- c(2, 2)  # Ordre GARCH(2,2)
spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = garch_order),
                   mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
                   distribution.model = "std")
# Ajustement du modèle GARCH(2,2) aux données
model <- ugarchfit(spec = spec, data = res_ETH)
# Affichage des résultats du modèle GARCH(2,2)
print(model)
# Tester la normalité des résidus
jarque.bera.test(residuals(model))
# Tester l'autocorrélation des résidus
Box.test(residuals(model), type = "Ljung-Box", lag = 10)


### Etape 4 et Etape 5 --> MODELE eGARCH(1,1)
################# APPLICATION SUR ARCH ET GARCH  #################################
##########################################################################
####################### MODELE eGARCH(1,1)

# Charger les bibliothèques nécessaires
library(rugarch)
library(FinTS)

# Spécifier le modèle EGARCH(1,1)
egarch_spec <- ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(2, 2)))
# Ajuster le modèle EGARCH aux données des résidus
egarch_model <- ugarchfit(spec = egarch_spec, data = res_ETH)
# Afficher les résultats du modèle EGARCH
print(egarch_model)
# Tester la normalité des résidus
jarque.bera.test(residuals(egarch_model))
# Tester l'autocorrélation des résidus
Box.test(residuals(egarch_model), type = "Ljung-Box", lag = 10)


### Etape 4 et Etape 5 --> MODELE eGARCH(2,2)
################# APPLICATION SUR ARCH ET GARCH  #################################
##########################################################################
####################### MODELE eGARCH(2,2)

# Charger les bibliothèques nécessaires
library(rugarch)
library(FinTS)

# Spécifier le modèle eGARCH(2,2)
egarch_order <- c(2, 2)  # Ordre GARCH(2,2)
egarch_spec <- ugarchspec(
  variance.model = list(model = "eGARCH", garchOrder = egarch_order),
  mean.model = list(armaOrder = c(1, 1))
)
# Ajuster le modèle eGARCH aux données des résidus
egarch_model <- ugarchfit(spec = egarch_spec, data = res_ETH)
# Afficher les résultats du modèle eGARCH
print(egarch_model)
# Tester la normalité des résidus
jarque.bera.test(residuals(egarch_model))
# Tester l'autocorrélation des résidus
Box.test(residuals(egarch_model), type = "Ljung-Box", lag = 10)


### Etape 4 et Etape 5 --> MODELE GJR-GARCH(1,1,1)
################# APPLICATION SUR ARCH ET GARCH  #################################
##############################################################################
####################### MODELE GJR-GARCH(1,1,1)

# Charger les bibliothèques nécessaires
library(rugarch)
library(FinTS)

# Spécifier le modèle GJR-GARCH(1,1,1)
gjr_garch_spec <- ugarchspec(
  variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(1, 1))
)
# Ajuster le modèle GJR-GARCH aux données des résidus
gjr_garch_model <- ugarchfit(spec = gjr_garch_spec, data = res_ETH)
# Afficher les résultats du modèle GJR-GARCH
print(gjr_garch_model)
# Tester la normalité des résidus
jarque.bera.test(residuals(gjr_garch_model))
# Tester l'autocorrélation des résidus
Box.test(residuals(gjr_garch_model), type = "Ljung-Box", lag = 10)



### Etape 4 et Etape 5 --> MODELE GJR-GARCH(2,2,2)
################# APPLICATION SUR ARCH ET GARCH  #################################
##############################################################################
####################### MODELE GJR-GARCH(2,2,2)

# Charger les bibliothèques nécessaires
library(rugarch)
library(FinTS)

# Spécifier le modèle GJR-GARCH(2,2,2)
gjr_garch_order <- c(2, 2)  # Ordre GARCH(2,2)
gjr_garch_spec <- ugarchspec(
  variance.model = list(model = "gjrGARCH", garchOrder = gjr_garch_order),
  mean.model = list(armaOrder = c(2, 2))
)
# Ajuster le modèle GJR-GARCH aux données des résidus
gjr_garch_model <- ugarchfit(spec = gjr_garch_spec, data = res_ETH)
# Afficher les résultats du modèle GJR-GARCH
print(gjr_garch_model)
# Tester la normalité des résidus
jarque.bera.test(residuals(gjr_garch_model))
# Tester l'autocorrélation des résidus
Box.test(residuals(gjr_garch_model), type = "Ljung-Box", lag = 10)


 
########## ETAPE 6: COMPARER LE MODELE GARCH ET RESEAU DE NEURONE ####
###############################################################################
##############################################################################
####################### MODELE Reseau de Neurone:
library(rugarch)
library(neuralnet)

# Charger vos données Ethereum
ethereum_data <- read.csv("C:/Users/ebouserhal/Desktop/cv/STA217/ETH-USD.csv")

# Séparer les données en ensembles d'apprentissage et de test
train_size <- 0.8
train_index <- 1:round(train_size * nrow(ethereum_data))
train_data <- ethereum_data[train_index, ]
test_data <- ethereum_data[-train_index, ]

# Modèles GARCH
garch_order <- c(1, 1)
garch_model <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = garch_order))
garch_fit <- ugarchfit(spec = garch_model, data = as.numeric(train_data$Close)) # Convertir au type numérique

# Prédiction des modèles sur les données de test
garch_forecast <- ugarchforecast(garch_fit, n.ahead = nrow(test_data))
garch_predictions <- sigma(garch_forecast)

# Modèle de Réseau de Neurones
nn_formula <- as.formula("Close ~ lag1 + lag2 + lag3")
nn_data <- cbind(train_data, lag1 = lag(train_data$Close, 1), lag2 = lag(train_data$Close, 2), lag3 = lag(train_data$Close, 3))

# Supprimer les lignes avec des valeurs manquantes
nn_data <- nn_data[complete.cases(nn_data), ]

nn_model <- neuralnet(nn_formula, data = nn_data, hidden = c(5, 3), linear.output = TRUE)

# Prédiction des modèles de Réseau de Neurones sur les données de test
nn_data_test <- cbind(test_data, lag1 = lag(test_data$Close, 1), lag2 = lag(test_data$Close, 2), lag3 = lag(test_data$Close, 3))
nn_predictions <- predict(nn_model, nn_data_test)[, 1]

# Calculer les erreurs RMSE pour chaque modèle
garch_rmse <- sqrt(mean((test_data$Close - garch_predictions)^2, na.rm = TRUE))
nn_rmse <- sqrt(mean((test_data$Close - nn_predictions)^2, na.rm = TRUE))

# Afficher les résultats
cat("RMSE pour le modèle GARCH:", garch_rmse, "\n")
cat("RMSE pour le modèle de Réseau de Neurones:", nn_rmse, "\n")




############################################################################
########################################################################## 
################################################################################
################################################################################
##### MEILLEUR MODELE d'ETH SELON RMSE :--> eGARCH(1,1)

library(rugarch)
library(FinTS)

# Diviser les données en ensemble d'apprentissage et de test (par exemple, 80% / 20%)
train_size <- floor(0.8 * length(res_ETH))
train_data <- res_ETH[1:train_size]
test_data <- res_ETH[(train_size + 1):length(res_ETH)]

# Liste de modèles candidats
models <- list(
  garch11 = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1))),
  garch22 = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2, 2))),
  garch33 = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3, 3))),
  gjr_garch111 = ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1, 1))),
  gjr_garch222 = ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(2, 2, 2))),
  gjr_garch333 = ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(3, 3, 3))),
  egarch11 = ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1, 1))),
  egarch11 = ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(2, 2))),
  egarch22 = ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(3, 3)))
)

# Ajustement et évaluation des modèles
results <- list()
for (model_name in names(models)) {
  model <- ugarchfit(spec = models[[model_name]], data = train_data)
  results[[model_name]] <- model
  
  # Évaluation sur l'ensemble de test
  forecast <- ugarchforecast(model, n.ahead = length(test_data))
  rmse <- sqrt(mean((test_data - forecast@forecast$seriesFor[1:length(test_data)])^2))
  cat(paste(model_name, "RMSE:", rmse, "\n"))
}

# Sélection du modèle avec le RMSE le plus bas
min_rmse <- Inf
best_model <- NULL
for (model_name in names(results)) {
  model <- results[[model_name]]
  forecast <- ugarchforecast(model, n.ahead = length(test_data))
  rmse <- sqrt(mean((test_data - forecast@forecast$seriesFor[1:length(test_data)])^2))
  if (rmse < min_rmse) {
    min_rmse <- rmse
    best_model <- model_name
  }
}

cat("Le meilleur modèle est:", best_model, "avec un RMSE de", min_rmse, "\n")


##### conclusion : en se basant sur RMSE
#RMSE signifie "Root Mean Square Error" (erreur quadratique moyenne). 
#C'est une mesure couramment utilisée pour évaluer la précision d'un modèle de prévision ou de régression.
#Le RMSE mesure l'écart entre les valeurs prédites par le modèle et les valeurs réelles (observées) en termes d'erreur quadratique.
#En d'autres termes, le RMSE calcule la racine carrée de la moyenne des carrés des différences entre les valeurs réelles et les valeurs prédites.
#Cela mesure la dispersion moyenne des erreurs entre les prédictions du modèle et les données réelles. Un RMSE plus faible indique une meilleure adéquation du modèle aux données observées.


#### conclusion: les valeurs RMSE semblent être assez proches les unes des autres pour les modèles :
#      GARCH(1,1), GARCH(2,2), eGARCH(1,1) et eGARCH(2,2). Cela pourrait signifier que ces modèles
# n'apportent pas d'améliorations significatives en termes de prévision de la volatilité par rapport à vos données.


### RMSE :
#garch11 RMSE: 0.0619572241980702 
#garch22 RMSE: 0.0619593469461596 
#garch33 RMSE: 0.0619583534994281 
#gjr_garch111 RMSE: 0.0619516653289017 
#gjr_garch222 RMSE: 0.0619529097448991 
#gjr_garch333 RMSE: 0.0619527736049707 
#egarch11 RMSE: 0.061950422296986 
#egarch11 RMSE: 0.061950422296986 
#egarch22 RMSE: 0.0619519065681546

## RMSE pour le modèle GARCH: 1566.055
## RMSE pour le modèle de Réseau de Neurones: 482.1824








#ETAPE 6:Representation grapgique d'un histogramme des rendements
#Representation grapgique d'un  histogramme des rendements du Bitcoin 
#en ajoutant une distribution rouge pour les rendements inférieurs au quantile à 5%.
#variance ne depand pas de temps
#VAR PAR GARCH
library(tidyverse)
library(ggthemes)
library(forecast)
library(gridExtra)
library(rugarch)

# Calculer les quantiles :
#--> Cela vous donnera la valeur en dessous de laquelle se situent 5% des rendements.
quantile(Rendement_data_ETH , 0.05)

# Tracé d'un histogramme des rendements : ( in light blue)
# Tracé d'un histogramme des rendements inférieurs au quantile à 5%  ( in red)
qplot(Rendement_data_ETH  , geom = 'histogram') + geom_histogram(fill = 'lightblue' , bins = 30) +
  geom_histogram(aes(Rendement_data_ETH  [Rendement_data_ETH  < quantile(d_Rendement_data_ETH  , 0.05)]) , fill = 'red' , bins = 30) +
  labs(x = 'Daily Returns')

### CONCLUSION:Voici quelques raisons pour lesquelles vous pourriez souhaiter visualiser ces rendements inférieurs :
# 1)Détection d'événements extrêmes : Les rendements qui sont significativement en dessous de la moyenne peuvent indiquer des événements exceptionnels ou des conditions inhabituelles sur le marché.
# 2)Identification des crises financières :  Les périodes de crises financières ou d'instabilité économique peuvent entraîner des rendements anormalement bas
# 3)Évaluation de la volatilité : Les rendements inférieurs à un seuil peuvent être associés à une volatilité accrue, car les variations sont plus prononcées dans ces périodes.
# 4)Gestion des risques : Les rendements inférieurs à un certain seuil peuvent être pertinents pour la gestion des risques.
# 5)Confirmation de modèles financiers : La visualisation des rendements inférieurs peut aider à confirmer si le modèle capture correctement la volatilité pendant ces périodes.
######## En somme, visualiser les rendements inférieurs à un certain seuil permet d'obtenir des informations précieuses sur 
#-->la dynamique du marché, 
#-->la volatilité, 
#-->les risques et les événements exceptionnels. 
#Cela peut jouer un rôle essentiel dans l'analyse financière et la prise de décision.




######################################################################################
########################### COMPARAISON ENTRE BTC & ETH ########################



##########################################################################################
############################ CONCLUSION ##########################################
########## POUR AIDER L'INVESTISSEUR A PREDNRE LA DECISION : 
# En fonction des résultats que vous avez obtenus pour les modèles GARCH ajustés aux données de résidus du Bitcoin et d'Ethereum, voici ce que vous pourriez conclure pour aider l'investisseur à choisir une monnaie virtuelle pour son portefeuille :
#  Similitudes dans la Volatilité : Les modèles GARCH ajustés aux résidus du Bitcoin et d'Ethereum montrent des similitudes dans les caractéristiques de volatilité. Cela indique que les deux cryptomonnaies présentent des variations de volatilité qui sont capturées efficacement par le modèle.
#  Comportement Temporel : Les modèles indiquent que les résidus du Bitcoin et d'Ethereum ont des structures de volatilité similaires au fil du temps. Cela suggère que les deux cryptomonnaies peuvent partager des motifs de variations de volatilité.
#  Modèles GARCH-ARFIMA : Les modèles sGARCH(1,1)-ARFIMA(1,0,1) semblent être appropriés pour modéliser les résidus des deux cryptomonnaies. Cela signifie que ces modèles capturent bien la volatilité conditionnelle et les caractéristiques de tendance dans les séries temporelles.Investissement Éclairé : Pour l'investisseur, ces résultats suggèrent que tant le Bitcoin que l'Ethereum ont des modèles de volatilité relativement bien compris. Cependant, le choix entre les deux dépendrait d'autres facteurs tels que les objectifs d'investissement, la tolérance au risque et les analyses fondamentales.
#  Diversification : Puisque les deux cryptomonnaies montrent des comportements similaires en termes de volatilité et de tendance, il pourrait être judicieux d'envisager la diversification du portefeuille en incluant les deux. Cela pourrait potentiellement atténuer les risques spécifiques liés à chaque cryptomonnaie.
#  Surveillance Continue : Les marchés des cryptomonnaies sont extrêmement volatils et peuvent être influencés par des facteurs imprévisibles. Il est important de rappeler à l'investisseur que même si les modèles actuels semblent appropriés, il est essentiel de surveiller en permanence les conditions du marché et de revoir les stratégies d'investissement en conséquence.



############################################## LE BUT :
# Comprendre et modéliser la volatilité des actifs financiers est un défi complexe, et les performances des modèles peuvent varier en fonction des données et des conditions du marché. Dans votre cas, il semble que la série temporelle des prix d'Ethereum présente des caractéristiques de volatilité qui ne sont pas bien capturées par les modèles GARCH standard que vous avez testés, tels que GARCH(1,1), GARCH(2,2), eGARCH(1,1), etc.
# Il est important de noter que la volatilité des actifs financiers peut être influencée par de nombreux facteurs, tels que les nouvelles économiques, les événements mondiaux, les tendances du marché, etc. Par conséquent, il est difficile de prédire avec certitude quel modèle sera le plus approprié pour toutes les situations.
# Dans un tel scénario, il peut être utile d'adopter une approche plus holistique pour prendre une décision d'investissement entre Bitcoin et Ethereum. Vous pourriez envisager d'examiner d'autres indicateurs et facteurs, tels que la capitalisation boursière, l'adoption par les entreprises et les investisseurs, les développements technologiques, les cas d'utilisation réels, etc.
# De plus, diversifier un portefeuille d'investissement en incluant plusieurs actifs plutôt que de se concentrer sur un seul pourrait également aider à réduire les risques liés à la volatilité d'un actif spécifique.
# En fin de compte, il est recommandé de consulter des experts financiers et de réaliser des analyses approfondies avant de prendre une décision d'investissement, surtout dans le cas des actifs volatils comme les cryptomonnaies.
