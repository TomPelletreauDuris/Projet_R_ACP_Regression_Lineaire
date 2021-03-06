data<- read.table("C:/Users/mlucas004/Documents/mlucas_pret/2A/Maths/Projet_R_CC/ozone.txt", header=TRUE)
attach(data)
#=============================================================================
#----------------------ACP-------------------------------

#Test de normalit� avec Shapiro
#---------------------------------------------------------

#On supprime les colonnes qui ne conviennent pas � une ACP
dataACP <- data[,!colnames(data)%in%c("vent","pluie")]

shapiro.test(data[,1])
shapiro.test(data[,2])
shapiro.test(data[,3])
shapiro.test(data[,4])
shapiro.test(data[,5])
shapiro.test(data[,6])
shapiro.test(data[,7])
shapiro.test(data[,8])
shapiro.test(data[,9])
shapiro.test(data[,10])
shapiro.test(data[,11])

#avec les test de shapiro les valeurs obtenues pour les colonnes 8,9 et 10
#ne sont pas satisfaisantes
#----------------------------
#Test de Wilcoxon

wilcox.test(data[,8])
wilcox.test(data[,9])
wilcox.test(data[,10])

#Les nouvelles valeurs obtenues pour la p-value sont satisfaisantes

#Toutes les colonnes sont normalement distribu�es

#Creation ACP globale avec les variables de temp�rature, de n�bulosit�, de vent
#et du max de la concentration en ozone de la veille

#----------------------------------------
# Chargement du package PCAmixdata
#----------------------------------------
install.packages("PCAmixdata")
require(PCAmixdata) # permet de charger le package "PCAmixdata"
# afin de pouvoir l'utiliser par la suite

res<-PCAmix(dataACP)

#---------------------------------------------
#Choix de l'axe � retenir
#---------------------------------------------
#on affiche les valeurs propres des axes
round(res$eig,digit=2)
barplot(res$eig[,1],main="Eigenvalues",names.arg=1:nrow(res$eig)) #graphique de l'�bouli des valeurs propres
abline(h=1,col=2,lwd=2)


#--------------------------------------------------
#Cercle des corr�lations des dimensions 1-2 et 1-3 pour choisir les dimensions � retenir
#----------------------------------------------------------

plot(res,axes=c(1,2),choice="cor")#, main="Cercle de corr�lation sur les dimensions 1-2")
plot(res,axes=c(1,3),choice="cor")#, main="Cercle de corr�lation sur les dimensions 1-3")


#----------------------------------------------------
#Sorties num�riques pour les individus
#--------------------------------------------------
res$ind #permet d'afficher les sorties num�riques associ�es aux individus
round(res$ind$cos2,digits=3)

#====================================
#Sorties num�riques pour les variables quantitatives
#=====================================
res$quanti #sorties num�riques associ�es aux variables quantitatives
round(res$quanti$cos2,digits=3)



#=============================
#--------------2----------------
#==============================

#on commence par �tudier l'impact de la variable "pluie" sur max03.
#on construit une nouvelle dataset avec uniquement les 2 colonnes qui nous int�ressent
dataPluiemax03<- data[c(1,13)]
#la variable "pluie" est qualitative: pour comprendre l'influence qu'elle a sur max03, nous allons comparer les boxplots des 2 modalit�s de cette variable.

#on s�pare pluie de sec
dataSec=subset(dataPluiemax03,pluie=="Sec")
dataPluie=subset(dataPluiemax03,pluie=="Pluie")
#on construit les boxplots  correspondants
#c'est le plus pertinent pour comparer les influences
#des variables
par(mfrow=c(1,2))
boxplot(dataPluie, main="Influence de la pluie sur max03",ylab="max03", ylim=c(0,166))
boxplot(dataSec, main="Influence du sec sur max03", ylab="max03", ylim=c(0,166))

#on passe au vent
#le vent a 4 modalit�s: nord, sud, est, ouest
#on cr�e une dataset avec les colonnes vent et max03
dataVent<-data[c(1,12)]
dataEst=subset(dataVent,vent=="Est")
dataSud=subset(dataVent,vent=="Sud")
dataNord=subset(dataVent,vent=="Nord")
dataOuest=subset(dataVent,vent=="Ouest")
#166 est le max, donc on met le maximum des boxplots (en ordonn�e) sur 166
#pour pouvoir plus facilement les comparer
par(mfrow=c(1,4))
boxplot(dataEst,main="Influence d'un vent Est sur max03",ylab="max03",ylim=c(0,166))
boxplot(dataOuest,main="Influence d'un vent Ouest sur max03",ylab="max03",ylim=c(0,166))
boxplot(dataSud,main="Influence d'un vent Sud sur max03",ylab="max03",ylim=c(0,166))
boxplot(dataNord,main="Influence d'un vent Nord sur max03",ylab="max03",ylim=c(0,166))

#=============================
#--------------3----------------
#==============================


#stockage des donn�es quantitatives et des donn�es qualitatives dans des jeux de donn�es diff�rents
donneesQuanti <- dataACP
donneesQuali <- data[,!colnames(data)%in%c("T9","T12","T15","Ne9","Ne12","Ne15","Vx9","Vx12","Vx15","maxO3v","maxO3")]

res2<-PCAmix(X.quanti=donneesQuanti,X.quali = donneesQuali)
res2

#Choix du nombre d'axes � retenir
#------------------------------------------------
round(res2$eig,digits=2) #valeur propre

barplot(res2$eig[,1],main="Eigenvalues",names.arg=1:nrow(res2$eig)) #graphique de l'�bouli des valeurs propres
abline(h=1,col=2,lwd=2)
#------------------------------------------------
#Graphique des squared loadings sur les dimensions 1-2, 1-3 et 1-4

plot(res2,axes=c(1,2), choice="sqload", main="Graphique des squared loadings sur les dimesions 1-2") #graphique des "square loadings" pour toutes les variables
plot(res2,axes=c(1,3), choice="sqload", main="Graphique des squared loadings sur les dimesions 1-3")
plot(res2,axes=c(1,4), choice="sqload", main="Graphique des squared loadings sur les dimesions 1-4")


#On choisit les dimensions 1-2

#-----------------------------------------------------------
#Sortie num�rique pour les individus

res2$ind #afficher les sorties num�riques associ�es aux indiv
round(res2$ind$cos2,digits=3) #pour les cosinus carr�s

#-----------------------------------------------------------
#Sortie num�rique pour les variables quantitatives

res2$quanti #sorties num�riques associ�es aux variables quantitatives
round(res2$quanti$cos2,digits=3) #pour les cosinus carr�s

#------------------------------------------------------------
# Sorties num�riques pour les variables qualitatives

res2$levels #ensemble des sorties num�riques associ�es aux modalit�s des variables quali
round(res2$levelscos2,digits=3) #cosinus carr�s

res2$quali #ensemble des sorties num�riques associ�es aux variables quali
round(res2$quali$contrib.pct,digits=3) #contribution relative en pourcentage


plot(res2,axes=c(1,2),choice="levels") #permet de voir les corr�lations entre les variables qualitatives

#=============================
#--------------4----------------
#==============================

dataRes<-data[,!colnames(data)%in%c("vent","pluie")]
res<-lm(maxO3~T9+T12+T15+Vx9+Vx12+Vx15+Ne9+Ne12+Ne15+maxO3v, data=dataRes)

#on v�rifie la normalit� de l'�chantillon
residus.stud<-rstudent(res)
plot(residus.stud,ylab="res.student",ylim=c(-3.5,3.5))
abline(h=c(-2,0,2),lty=c(2,1,2))
#3 observations sortent du cadre [-2.2 2.2]
#on cherche � savoir ce qu'elles sont
rownames(data)[which(rstudent(res)>2.2)]

#les valeurs concern�es sont "20010707" "20010725" "20010824" qui correspondent � des journ�es de d�parts en vacances
#on peut l�gitimement enlever ces valeurs puisque ce sont des cas exceptionnels

data<-data[-80,]#suppression du 25 ao�t
data<-data[-52,]#suppression du 25 juillet
data<-data[-34,]#suppression du 7 juillet

#on peut maintenant proc�der � nouveau � la r�gression

#maintenant on fait la r�gression
dataRes<-data[,!colnames(data)%in%c("vent","pluie")]
res<-lm(maxO3~T9+T12+T15+Vx9+Vx12+Vx15+Ne9+Ne12+Ne15+maxO3v, data=dataRes)
#on a une R-squared � 78%
#on enl�ve pas-�-pas les variables du mod�le dont la p-value st sup�rieure � 5%
#donc les variables qui ne sont pas n�cessaires au mod�le
#nous avons choisi d'enlever ces variables une par une afin de v�rifier que la valeur
# de R-Squared ne baissait pas trop, et afin de garder un bon contr�le sur nos actions
#toutefois il aurait �t� possible de le faire autrement avec le code:
#resStep<-lm(maxO3~T9+T12+T15+Vx9+Vx12+Vx15+Ne9+Ne12+Ne15+maxO3v,
#step(resStep)

#on enl�ve donc les variables une par une:

#on a une R-squared � 78% environ
#on enl�ve la variable Vx12, le R-squared n'est pas modifi�
#on enl�ve N15, le R-squared passe � 77,97%
res<-lm(maxO3~T9+T12+T15+Vx9+Vx12+Vx15+Ne9+Ne12+maxO3v, data=dataRes)
# 77,92 on enl�ve T15, le R-squared passe � 77,92%
res<-lm(maxO3~T9+T12+Vx9+Vx12+Vx15+Ne9+Ne12+maxO3v, data=dataRes)
# 77,90 on enl�ve T9, le R-squared passe � 77,90%
res<-lm(maxO3~T12+Vx9+Vx12+Vx15+Ne9+Ne12+maxO3v, data=dataRes)
# 0,777 on enl�ve N12, le R-squared passe � 77,77%
res<-lm(maxO3~T12+Vx9+Vx12+Vx15+Ne9+maxO3v, data=dataRes)
# 0,7765 on enl�ve Vx15, le R-squared passe � 77,65%
res<-lm(maxO3~T12+Vx9+Vx12+Ne9+maxO3v, data=dataRes)
# on enl�ve Vx12, le R-squared reste � 77,65%
res<-lm(maxO3~T12+Vx9+Ne9+maxO3v, data=dataRes)
#� partir de l� les p-value sont syst�matiquement inf�rieures � 5%: on rejette donc l'hypoth�se selon laquelle elles sont inutiles au mod�le
summary(res)
#on a ainsi le mod�le final

