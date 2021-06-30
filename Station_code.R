#----------------------------------------
# Chargement du package PCAmixdata
#----------------------------------------
require(PCAmixdata) # permet de charger le package "PCAmixdata"
# afin de pouvoir l'utiliser par la suite
help(PCAmix) # permet de disposer de l'aide sur la fonction PCAmix

#----------------------------------------
#Ouverture du jeu de données station (import dataset)
#----------------------------------------

#(import data set)

#----------------------------------------
# Analyse des indicateurs
#----------------------------------------

boxplot(station$ventes,main = "Boxplot des ventes par station",ylab = "ventes en miliers de litres")
boxplot(station$nbpompes,main = "Boxplot du nombre de pompes par station",ylab = "nombre de pompes")
boxplot(station$nbconc,main = "Boxplot du nombre de concurrents par station",ylab = "nombre de concurrents")
boxplot(station$trafic,main = "Boxplot du trafic quotidien par station",ylab = "trafic quotidien en miliers de voitures")

#----------------------------------------
# Analyse corrélations entre les variables
#----------------------------------------
plot(station$nbpompes, station$ventes,xlab = "nb de pompes par stations",ylab = "nb de vente par station")
title("corrélation entre le nombre de vente par station et le nombre de pompe par station")
cor(station$nbpompes,station$ventes)

plot(station$nbconc,station$ventes,xlab = "nb de concurrents par station" ,ylab ="nb de vente par station" )
title("corrélation entre le nombre de vente par station et le nombre de concurrents par station")
cor(station$nbconc,station$ventes)

plot(station$trafic,station$ventes,xlab = "traffic en millier de voitures par stations" ,ylab ="nb de vente par station")
title("corrélation entre le nombre de vente par station et le traffic par station")
cor(station$trafic,station$ventes)

# --------------------------------------
# ACP
# --------------------------------------

res_ACP<-PCAmix(station, graph=FALSE)
summary(res_ACP, ncp=2)

round(res_ACP$eig,digit=3)
plot(res_ACP,axes=c(1,2),choice="cor")

#----------------------------------------------
#Etude des régressions linéaires simples
#----------------------------------------------

# -----------
# Ventes et nbpomes
# -----------
# Régression linaire
res12=lm(station$ventes~station$nbpompes)
summary(res12)
plot(res12$fitted, station$ventes)
abline(0,1, col=2)

# Etude des résidus
plot(res12$fitted, res12$residuals)
abline(h=0, col=2)

# Test de normalité des résidus
shapiro.test(res12$residuals)
residus.stud1<-rstudent(res12)
plot(residus.stud, ylab="res.student")
abline(h=c(-2,0,2), lty=c(2,1,2))

# -----------
# ventes et nbconc
# -----------
res13=lm(station$ventes~station$nbconc)
summary(res13)
plot(res13$fitted, station$ventes)
abline(0,1, col=2)

# Etude des résidus
plot(res13$fitted, res13$residuals)
abline(h=0, col=2)

# Test de normalité des résidus
shapiro.test(res13$residuals)
residus.stud2<-rstudent(res13)
plot(residus.stud2, ylab="res.student")
abline(h=c(-2,0,2), lty=c(2,1,2))


# -----------
# ventes et trafic
# -----------
res14=lm(station$ventes~station$trafic)
summary(res14)
plot(res14$fitted, station$ventes)
abline(0,1, col=2)

# Etude des résidus
plot(res14$fitted, res14$residuals)
abline(h=0, col=2)

# Test de normalité des résidus
shapiro.test(res14$residuals)
residus.stud3<-rstudent(res13)
plot(residus.stud3, ylab="res.student")
abline(h=c(-2,0,2), lty=c(2,1,2))



#----------------------------------------------
#Modele regression linéraire à trois variables
#----------------------------------------------

# creation d'un "tableau de donnees"
matYX <- data.frame(station$ventes,station$nbpompes,station$nbconc,station$trafic) 
plot(matYX)

#Modele de régression linéaire à trois variables
res <- lm(station$ventes~station$trafic+station$nbconc+station$nbpompes, data=matYX)
summary(res)

#Etude des residus
plot(res$residuals)
abline(h=0,col=2)

plot(res$fitted,res$residuals)
abline(h=0,col=2)

plot(res$fitted,station$ventes)
abline(0,1,col=2)

shapiro.test(res$residuals)
residus.stud<-rstudent(res)
plot(residus.stud,ylab="res. student.",ylim=c(-3.5,3.5))
abline(h=c(-2,0,2),lty=c(2,1,2))

#--------------
#La station n°1 est anormale. On l'enlève et on fait les mêmes analyses.
#--------------

# creation d'un "tableau de donnees"
matYXModif <- data.frame(stationModif$ventes,stationModif$nbpompes,stationModif$nbconc,stationModif$trafic) 
plot(matYXModif)

#Modele de régression linéaire à trois variables
resModif <- lm(stationModif$ventes~stationModif$trafic+stationModif$nbconc+stationModif$nbpompes, data=matYXModif)
summary(resModif)

#Etude des residus
plot(resModif$residuals)
abline(h=0,col=2)

plot(resModif$fitted,resModif$residuals)
abline(h=0,col=2)

plot(resModif$fitted,stationModif$ventes)
abline(0,1,col=2)

shapiro.test(resModif$residuals)
residus.studModif<-rstudent(resModif)
plot(residus.studModif,ylab="res. student.",ylim=c(-3.5,3.5))
abline(h=c(-2,0,2),lty=c(2,1,2))

#----------------------------------------------
#Modele à deux variables explicatives retenues
#----------------------------------------------

attach(stationModif)
res2 <- lm(ventes~trafic+nbpompes, data=matYXModif)
summary(res2)

#------------------------------------------------------------------------------------------
#A partir de ce modèle, prédire les ventes en fonction d'un trafic et d'un nombre de pompes
#------------------------------------------------------------------------------------------

c=data.frame(trafic=20, nbpompes=15);
predict(res2, newdata=c, se.fit=TRUE, interval = "prediction", level = 0.99)


#Grondin Lea et Pelletreau-Duris Tom pour Projet R ENSC 2020.
