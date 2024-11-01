getwd("/Users/albamileci")
setwd("/Users/albamileci/Desktop/UNINE/Statistique:Info/Statistique descriptive/projet")

#Langues des cantons:
  
install.packages("readxl")
library("readxl")
Votation= read_excel("votations_4.xlsx")

#R1
n=dim(Votation)
ncol(Votation)
nrow(Votation)
colnames(Votation)

# min et max pop
min(Votation$pop_inscrite)
max(Votation$pop_inscrite)

which.max(table(Votation$pop_inscrite))
which.min(table(Votation$pop_inscrite))

#min et max moyenne
min(Votation$moyenne)
max(Votation$moyenne)

which.max(table(Votation$moyenne))
which.min(table(Votation$moyenne))


#graphique géneral
x1=Votation$pop_inscrite
y1=Votation$moyenne 
plot(x1, y1, col = "red", pch = 16, main = "Registered population vs average canton participation", xlab = "Registered population", ylab = "Average participation")
#Aggiungere etichette per i cantoni
text(Votation$pop_inscrite, Votation$moyenne, labels = Votation$canton, pos = 1, col = "black")


#Graphique

#1)Variable x2 = suisse allamnique
x2= Votation$pop_inscrite[1:20]
y2= Votation$moyenne[1:20]

#2)Variable x3 = suisse française
x3=Votation$pop_inscrite[21:25]
y3=Votation$moyenne[21:25]

#3)Variable x4 = suisse italiene
x4=Votation$pop_inscrite[26:26]
y4=Votation$moyenne[26:26]


#4)Donné noms canton 
canton_Allemand_x2 = Votation$canton[1:20]
canton_Romand_x3 = Votation$canton[21:25]
canton_Italien_x4 = Votation$canton[26:26]

#5)plot
plot(x2, y2, col = "blue", pch = 16, xlim = c(min(c(x2, x3, x4)), max(c(x2, x3, x4))), ylim = c(min(c(y2, y3, y4)), max(c(y2, y3, y4))), xlab = " Registered population", ylab = "Average participation %", main = "Plot Cantons: German, French and Italian")
points(x3, y3, col = "red", pch = 16)
points(x4, y4, col = "green", pch = 16)
abline(v= mean(x1), col= "pink")
abline (h=mean(y1), col="pink")


#6)Canton
text(x = x2, y = y2, labels = canton_Allemand_x2, pos = 1, col = "blue")
text(x = x3, y = y3, labels = canton_Romand_x3, pos = 3, col = "red")
text(x = x4, y = y4, labels = canton_Italien_x4, pos = 4, col = "green")

#7)Legend
legend("topright", legend = c("German", "French", "Italian"), col = c("blue", "red", "green"), pch = 16)

#graphique séparé
par(mfrow=c(3,1))
plot(x2, y2, col = "blue", pch = 16, xlim = c(min(c(x2, x3, x4)), max(c(x2, x3, x4))), ylim = c(min(c(y2, y3, y4)), max(c(y2, y3, y4))), xlab = "Registered population ", ylab = "Average participation %", main = "German Switzerland")
plot(x3, y3, col = "red", pch = 16, xlim = c(min(c(x2, x3, x4)), max(c(x2, x3, x4))), ylim = c(min(c(y2, y3, y4)), max(c(y2, y3, y4))), xlab = "Registered population ", ylab = "Average participation %", main = "French Switzerland")
plot(x4, y4, col = "green", pch = 16, xlim = c(min(c(x2, x3, x4)), max(c(x2, x3, x4))), ylim = c(min(c(y2, y3, y4)), max(c(y2, y3, y4))), xlab = "Registered population ", ylab = "Average participation %", main = "Italian Switzerland")


#Histogram
#histogram pop_inscrite + canton
barplot(height = Votation$pop_inscrite, names.arg = Votation$canton, col = "lightgreen", main = "Population registered by canton", xlab = "Canton", ylab = "Registered population")
#histogram moyenne +canton
barplot(height = Votation$moyenne, names.arg = Votation$canton, col = "lightblue", main = "Average participation by canton", xlab = "Canton", ylab = "Average participation %")

#1) histogrem moyenne + canton
barplot(height = Votation$moyenne, names.arg = Votation$canton, col = "lightblue", main = "Average participation by canton", xlab = "Canton", ylab = "Average %")
# 2) deuxième partie avec 'pop_inscrite'
par(new = TRUE)
barplot(height = Votation$pop_inscrite, col = "lightgreen", axes = FALSE)
# 3) axe pour deuxième partie
axis(side = 4)
mtext("Pop_Inscrite", side = 4, line = 3)


#Tableau de contengence

#1)caractére pop_inscrite
contingency_pop_inscrite=table(Votation$pop_inscrite)
print(contingency_pop_inscrite)

#2) caractére moyenne
contingency_moyenne = table(Votation$moyenne)
print(contingency_moyenne)

#3) densité pop_enscrit
densité_pop_inscrite = density(Votation$pop_inscrite)

#4) densité moyenne
densité_moyenne = density(Votation$moyenne)

# Frequenze cumulate
freq_cumulative_pop = cumsum(contingency_pop_inscrite)
freq_cumulative_moyenne=cumsum(contingency_moyenne)


# Percentuali
pourcentage = prop.table(contingency_pop_inscrite) * 100


tableau_complet = cbind(contingency_pop_inscrite, pourcentage)
colnames(tableau_complet) = c("Contingency_pop_enscrit", "Pourcentage")

tableau_complet2= cbind(contingency_moyenne, pourcentage)
colnames(tableau_complet) = c("Contingency_moyenne", "Pourcentage")

# tableau complet
print(tableau_complet)
prop.table(tableau_complet)

print(tableau_complet2)
prop.table(tableau_complet2)


#R3

#1)Variable moyenne
F_moyenne= ecdf(Votation$moyenne)
par (mfrow= c(1,1))
plot(F_moyenne, xlab="Votation$moyenne", ylab="Votation$moyenne", main="Cdf")

f_moyenne = prop.table(table(Votation$moyenne))
par(mfrow = c(2, 1))
barplot(f_moyenne, xlab = "Moyenne_group", ylab = "Probabilities", main = "Density")
plot(F_moyenne, xlab = "Moyenne_group", ylab = "Probabilities", main = "Cdf")
quantile(F_moyenne, prop = 0.5)
abline(h = 0.5, lty = 2, lwd = 2, col = "red")
abline(v = c(50.00, 57.00, 58.00, 60.75,72.00), lty = 2, lwd = 2, col = "violet")

#hist
par(mfrow = c(1, 2))
hist(Votation$moyenne)
barplot(f_moyenne, xlab = "Moyenne", ylab = "Probabilities", main = "Density")

par(mfrow = c(1, 2))
hist(Votation$pop_inscrite)
barplot(f_pop, xlab = "Population", ylab = "Probabilities", main = "Density")

#densité jointe 
#22) Construire la densité jointe pour les variables X et Y.
X1=x1
Y1=y1
Joint_density=table(X1,Y1)
print(Joint_density)

TP=Votation$pop_inscrite
TM=Votation$moyenne
Cant=Votation$canton


aa=table(Votation$pop_inscrite,Votation$moyenne)

TM_med=median(TM)
TM_med

tm=TM
tm[TM<TM_med]=1
tm[TM>=TM_med]=2

TP_med=median(TP)
TP_med

tp=TP
tp[TP<TP_med]=1
tp[TP>=TP_med]=2

A=data.frame(cbind(Cant,tp,tm))
Contengency=table(A$tp,A$tm)
Contengency
Density=Contengency/nrow(Votation)
Density

#Faire des densités conditionelles
tm_1=prop.table(table(tp[tm==1]))#montre la densité conditionelle du caractère "populaation inscrite" quand "Moyenne de participation par contans" est = 1
tm_1
rownames(tm_1)=c ("tp=1","tp=2")
barplot(tm_1, ylim=c(0,0.8), main ="Conditioned to tm = 1", ylab="Density") 


tm_2=prop.table(table(tp[tm==2]))#montre la densité conditionelle du caractère "population inscrite" quand "Moyenne de participation par contans" est = 1
tm_2
rownames(tm_2)=c ("tp=1","tp=2")
barplot(tm_2, ylim=c(0,0.8), main ="Conditioned to tm = 2", ylab="Density") 

par(mfrow= c(1,2))
barplot(tm_1, ylim=c(0,0.8), main ="Conditioned to tm = 1", ylab="Density")
barplot(tm_2, ylim=c(0,0.8), main ="Conditioned to tm = 2", ylab="Density") 

#Sièges au Parlement des cantons: 

#1) Installer le tableau excel
install.packages("readxl")
library(readxl)
Base=read_excel("Sièges_cantons.xlsx")

#2) Définir nos variables
Cantons=Base$Cantons
Cantons
Average_voting=Base$`Moyennes participation votes`
Average_voting
Voting_figures=Base$Chiffres
Voting_figures
Total_seats=Base$`Total sièges`
Total_seats


#3) Extraire les noms de notre tableau
colnames(Base)


#4) Extraire le nombre le plus petit des sièges détenus par les cantons
min(Total_seats)


#5)Analyser les 10 premières et dernières observations du tableau.
n=dim(Base)[1]
n
Base[1:10,]
Base[(n - 9):n, ]


#6) Voir la variabilité du caractère "Total_sièges_canton" avec un tableau de contingence.
contingency_total_seats = table(Total_seats) 
contingency_total_seats


#7)Faire un tableau de fréquence pour cette même variable.
Frequency_total_seats=contingency_total_seats/n
Frequency_total_seats


#8)Visualiser les tableaux de contingence et de fréquence en même temps.
par(mfrow = c(1, 2))
barplot(contingency_total_seats, ylab = "number of cantons by number of seats", xlab = "number of seats",
        main = "Contingency")
barplot(Frequency_total_seats, ylab = "density", xlab = "number of seats", main = "Frequency") 

#Nous voyons que le nombre de sièges que ont le plus de cantons est 2 sièges.


#9)Extraire le canton qui a le plus de sièges ainsi que le nombre de sièges possédé.
Cantons[which.max(Total_seats)]
max(Total_seats)

#Zurich est le canton avec le plus de sièges.


#10)La proportion de ce maximum de sièges parmis les sièges totaux.
max(table(Cantons))/nrow(Base)


#11) Extraire le minimum de sièges ainsi que les cantons ayant ce nombre de sièges.
Min_number_seats=min(Total_seats)
Cantons[Total_seats==Min_number_seats]


#12) La proportion de ce minimum de sièges parmis les sièges totaux.
Min_number_seats/nrow(Base)


#13)Fonction de densité pour la variable "sièges".
Density_function_seats=ecdf(Total_seats)
par(mfrow = c(1, 1))
plot(Density_function_seats, xlab = "Number of seats", ylab = "Probabilities", main = "Cumulative distribution function")

#Visualiser la médiane du graphique.
quantile(Density_function_seats, prob=0.5)
abline(h = 0.5, lty = 2, lwd = 2, col = "red")
abline(v=7.5, lty = 2, lwd = 2, col = "purple")


#17)Histogramme pour la variable "Nombre de sièges" comparé à la densité (METTRE DANS LE RAPPORT)
par(mfrow = c(1, 2))
hist(Total_seats,xlab="Number of seats", main="Histogram")
barplot(Density_function_seats, xlab = "Number of seats", ylab = "Probabilities",main = "Density")

#Nous voyons ici d'une manière plus globale, le nombre de fois que l'on retrouve un même nombre de sièges parmis les cantons.
#Cela nous permet de voir que la plupart des cantons disposent d'un nombre de sièges faible, et la plupart du temps, inférieur à 10 sièges.



#18) Probabilité de tomber sur un canton avec au moins 4 sièges.
Density_function_seats(4)

#19) Probabilité qu'en choisissant 1 canton, on tombe sur un canton avec un nombre de sièges entre 2 et 10.
Density_function_seats(10)-Density_function_seats(2)

#20)Combien de sièges au moins ont le 30% des cantons avec le plus de sièges?
quantile(Density_function_seats, prob=0.7)

#21) Création d'un scatterplot réunissant deux variables dans un graphique et montrer les moyennes des variables.(A METTRE DANS RAPPORT)

#On extrait les variables "Total sièges" et "Moyennes particpation votes" du tableau.
#On choisit de prendre les moyennes des votations pour voir l'effectif réel de la particpation de la population
# aux votations et faire en sorte que la taille de la population de chaque canton n'influence pas sa participation aux votations.

X = Total_seats
X
Y = Average_voting
Y
plot(X,Y, main="Scatterplot X&Y",xlab ="X=number of seats", ylab= "Y=voting participation")
text(X, Y, labels = Cantons, pos = 1, cex = 0.6, col = "blue")
abline(v = mean(X), col = "red") 
abline(h = mean(Y), col = "red")

#Plus les points sont situés à droite et plus les cantons possèdent de sièges. 
#Plus on va vers le haut dans le graphique, et plus la participation aux votes est élevée. 
#Nous observons qu'en général, plus le nombre de sièges pour un canton est petit, plus celui-ci participera aux votations.
#Les cantons avec un taux de participation élevé donc supérieur à la médiane, tel que Schaffhouse, Obwald, etc. n'ont pas beaucoup de sièges au parlement (nombre inférieur à 10 sièges).
#Inversement, les cantons avec beaucoup de sièges tel que Zürich, qui possède plus de 35 sièges au parlement,
#ont une participation aux votations assez basse comparais aux cantons avec moins de sièges.
#La médiane -> montre que la plupart des cantons ont une particpation modérée, donc située entre un taux de participation de 50
#et de moins de 60%. Cette majorité de cantons disposent d'un nombre de sièges au parlement, 
#assez bas, donc inférieur à 10 sièges.




#22) Construire la densité jointe pour les variables X et Y.
TS=Base$`Total sièges`
TP=Base$`Moyennes participation votes`
Cant=Base$Cantons


aa=table(TS,TP)

TP_med=median(TP)#On fait la médiane pour savoir ce qui va être considéré comme une participation modérée (< médiane)et une participation élevée (> médiane).
TP_med

tp=TP
tp[TP<TP_med]=1 #On définit ce qui est considéré comme participation modérée =1
tp[TP>=TP_med]=2 #On définit ce qui est considéré comme participation élevée =2

TS_med=median(TS)#On fait la médiane pour savoir ce qui est considéré comme peu de sièges (=1) et beaucoup de sièges (=2).
TS_med

ts=TS
ts[TS<TS_med]=1 #peu de sièges -> 1
ts[TS>=TS_med]=2 #beaucoup de sièges -> 2

A=data.frame(cbind(Cant,ts,tp)) #Tableau de contingence joint
Contingency=table(A$ts,A$tp)
Contingency
Density= Contingency/nrow(Base) #Tableau de densité jointe
Density

#23) Faire des densités conditionnelles: (METTRE DANS LE RAPPORT)

tss_1=prop.table(table(tp[ts==1])) 
#Montre la densité conditionnelle du caractère "Moyenne participation votes" quand la variable "Total sièges" est =1 (peu de sièges).
tss_1
rownames(tss_1) = c("tp=1", "tp=2")
barplot(tss_1, ylim = c(0, 0.8), main = "Conditioned to ts=1", ylab = "density") 
#On fait un graphique qui représente cette densité conditionnelle.

tss_2=prop.table(table(tp[ts==2]))
#Montre la densité conditionnelle du caractère "Moyenne participation votes" quand la variable "Total sièges" est =2 (beaucoup de sièges).
tss_2
rownames(tss_2) = c("tp=1", "tp=2")
barplot(tss_2, ylim = c(0, 0.8), main = "Conditioned to ts=2", ylab = "density")
#On fait un graphique qui représente cette densité conditionnelle.

par(mfrow = c(1, 2))
barplot(tss_1, ylim = c(0, 0.8), main = "Conditioned to ts=1", ylab = "density")
barplot(tss_2, ylim = c(0, 0.8), main = "Conditioned to ts=2", ylab = "density")



#PIB cantons:

install.packages("readxl")
library(readxl)

PIB_Votes = read_excel("PIB_Votations.xls")


## Creation des variables##

Cantons = PIB_Votes$Cantons
Cantons

PIB = PIB_Votes$`PIB(millions de francs)`
PIB

Votes = PIB_Votes$Votations
Votes


## Analyse de la variable PIB ##

Mediane_PIB = quantile(PIB,0.5)
Mediane_PIB 
# donc 50% des cantons ont un PIB <= 19 768  million et 50% >= 19 768  million 

Moyenne_PIB = mean(PIB)
Moyenne_PIB # =~ 28 589.62 millions

Total_PIB = sum(PIB)
Total_PIB # = 743 330 millions

Max_PIB = max(PIB)
Max_PIB # = 152 547 millions

Min_PIB = min(PIB)
Min_PIB # = 1 155 millions


## Analyse de la variable Votation ##

Mediane_Votation = quantile(Votes,0.5)
Mediane_Votation 
# 50% des cantons ont un % de vote <= 57.3% et 50% ont >= 57.3%

Moyenne_Votation = mean(Votes)
Moyenne_Votation # =~ 57.9 %

Max_Votation = max(Votes)
Max_Votation # =~ 72 %

Min_Votation = min(Votes)
Min_Votation # =~ 50.3 %


## Graphs ##

# graph 1 #

plot(PIB,Votes, xlab = "GDB (million)", ylab = "Votes (%)")
abline(v = Moyenne_PIB, col = "red")
abline(h = Moyenne_Votation, col = "red")
text(PIB,Votes,label=Cantons, pos = 2, cex=0.5) # pos == position

# graph 2 #

par(mfrow=c(1,1))

# graph 2.1 #
barplot(Votes, xlab = "Cantons", ylab = "Average % of votes", ylim = c(0,70), names.arg = Cantons)

# graph 2.2 #

barplot(PIB, ylim = c(0,200000), names.arg = Cantons, xlab = "Cantons", ylab = "GDB (in million)")
# GDB (Gross Domestic Product) == PIB en anglais

# graph 3 #

hist(Votes, main = "Votes", ylim = c(0,15)) 


# graph 4 #

Flog=ecdf(log10(PIB)) # utiliser ecdf() pour construire les fonctions de repartitions
Flog

par(mfrow=c(1,1))

plot(Flog)
abline(h = 0.5, col = "red")
abline(v = 4.0, col = "red")

hist(Flog, ylim = c(0,15),main="GDB's log10", xlab = "log10(GDB)")



