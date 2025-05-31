# ETAPE1 : préparation des données

## 1.1 Importation du tableau
don <- read.table(file = "data/africa_GDL/data/don_reg_2012.csv",
                  header= TRUE,
                  dec=",",
                  sep =";")

## 1.2 Sélection du pays
sel <- don[don$iso_code=="CIV",]

## 1.3 Sélection des variables
sel <-sel[, c("country","region","pop","pib_hab","esp_vie")]

## 1.4 Affichage du tableau
sel

## ETAPE 2 : Poids du pyas

sum(sel$pop)*1000

## ETAPE 3 : Paramètres principaux
summary(sel)


## ETAPE 4 :Histogramme et boxplot de X

X<- sel$pib_hab
hist(X, 
     main="Histogramme",
     breaks = quantile(X,probs = c(0,0.25,0.5,0.75,1)),
     ylab= "Densité",
     xlab = "X = PIB par habitant",
     probability = T,
     col= "lightyellow",
)
rug(X, 
    col="blue",
    lwd=3)
lines(density(X, bw=sd(X)/2),
      col="red",
      lwd=2,
)

## ETAPE 5 : Histogramme de Y

Y <- sel$esp_vie
hist(Y, 
     main="Histogramme",
     breaks = quantile(Y,probs = c(0,0.25,0.5,0.75,1)),
     ylab= "Densité",
     xlab = "Y = Espérance de vie",
     probability = T,
     col= "lightyellow",
)

rug(Y, 
    col="blue",
    lwd=3)

lines(density(Y, bw=sd(Y)/2),
      col="red",
      lwd=2,
)

## ETAPE 6 : Relation entre X et Y 

plot(sel$pib_hab,sel$esp_vie,
     xlab = "X : PIB par habitant",
     ylab = "Y : Espérance de vie",
     main = "Relation entre X et Y",
     pch=20,
     col="red")

text(sel$pib_hab,sel$esp_vie,sel$region,
     cex=0.4,
     col="blue",
     pos=1)

grid()


## ETAPE 7 : Test de la relation entre X et Y

## 7.1 Corrélation
cor(sel$pib_hab,sel$esp_vie)

## 7.2 Test de significativité
cor.test(sel$pib_hab,sel$esp_vie)

## 7.3 Régression
modreg<-lm(esp_vie~pib_hab,data=sel)
summary(modreg)

## 7.4 Graphique + droite de régression


plot(sel$pib_hab,sel$esp_vie,
     xlab = "X : PIB par habitant",
     ylab = "Y : Espérance de vie",
     main = "Relation entre X et Y",
     pch=20,
     col="red")

text(sel$pib_hab,sel$esp_vie,sel$region,
     cex=0.4,
     col="blue",
     pos=1)

grid()

abline(modreg, col="black",lwd=2)




