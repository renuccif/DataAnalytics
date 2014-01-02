###########################################
# TP 1 - Jérôme CRÉTIEN - Florent RENUCCI #
# 25 septembre - pour le 15 octobre		  #
###########################################


###########################
# exemples d'instructions #
###########################
# iris
# ncol(iris)
# dim(iris)
# names(iris)
# plot(iris)
# plot(iris,col=c("blue","red","green")[iris$Species])
# mean(iris)
# sd(iris)
# iris[1,]
# iris[,5]
# iris$Species
# tapply(iris$Sepal.Width,iris$Species,mean)
# tapply(iris$Species,iris$Species,length)


#############################################
# Analyse discriminante linéaire prédictive #
#############################################
rm(list=ls())

#-----------
# Question 1
#-----------
library(MASS)
#-----------
# Question 2
#-----------
help(lda)
model = lda(Species~.,data=iris)
#-----------
# Question 3
#-----------
model
# Prior : il s'agit des probabilités a priori d'appartenance à chaque classe (dans une approche bayésienne). Ici, on n'a pas renseigné d'a priori. La fonciton considère donc que l'appartenance à chaque classe est a priori équiprobable (3 classes => p=1/3).
# Group means : renvoie la moyenne de chaque variable, par classe
# Coefficients of linear discriminants : renvoie les coefficients des vecteurs réalisant l'analyse discriminante linéaire
# Proportion of trace : renvoie la proportion de la trace (variabilité) captée par chaque direction
# Le critère de décision retenu dans le cadre de l'analyse discriminante linéaire est la maximisation de la variance inter, en ajustant des hyperplans pour classifier (séparer) les observations.

#-----------
# Question 4
#-----------
plot(model)
# plot(model) permet d'observer la répartition des observations dans le plan de projection (LD1,LD2) qui maximise la variabilité. On constate que les trois classes sont assez bien réparties. En particulier, les setosa sont bien distinctes des deux autres groupes.
# Ceci confirme les valeurs de "proportion of trace" : le 1er vecteur, même seul, permettrait quand même de bien séparer les groupes.

#-----------
# Question 5
#-----------
learning_indices = sample(nrow(iris),0.80*nrow(iris))
# on choisit 80% des lignes, au hasard, qui formeront le learning set

learning_set = iris[learning_indices,]
test_set = iris[-learning_indices,]
# le reste des lignes forment la base de test


#-----------
# Question 6
#-----------
lda_model = lda(Species~.,data=learning_set)
prediction_learning = predict(lda_model,learning_set)
#-----------
# Question 7
#-----------
ConfusionMatrix_learning = table(prediction_learning$class,learning_set$Species)
# Matrice de confusion sur la base d'apprentissage :

# /!\ On ne sait pas a priori, sans regarder l'aide, si on a la confusion matrix ou sa transposée
# ICI : on a la prédiction en ligne et la cible en colonne
# Matrice de confusion sur la base d'apprentissage :
ConfusionMatrix_learning
# Erreur de prédiction sur la base d'apprentissage :
erreur= 1-sum(diag(ConfusionMatrix_learning))/sum(ConfusionMatrix_learning)

# C'est proche de 0 : c'est une condition nécessaire non suffisante de fiabilité du modèle : la base d'apprentissage permet "de se prédire elle-même", ce qui ne signifie pas forcément qu'elle peut prédire les classes d'individus extérieurs. 

#-----------
# Question 8
#-----------
prediction_test = predict(lda_model,test_set)
ConfusionMatrix_test = table(prediction_test$class,test_set$Species)
# Matrice de confusion sur la base de test :
ConfusionMatrix_test
# Erreur de prédiction sur la base de test :
1-sum(diag(ConfusionMatrix_test))/sum(ConfusionMatrix_test)
# REMARQUES :
# L'erreur de prédiction (en proportion) est comparable pour la base de test et la base d'apprentissage.
# On remarque même que l'erreur de prédiction est souvent plus faible sur la base de test que sur la base d'apprentissage.
# Ceci n'est pas surprenant : faire des erreurs sur la base d'apprentissage est une bonne chose pour éviter le sur-apprentissage (overfitting).
#-----------
# Question 9
#-----------
# REMARQUES :
# On remarque qu'il ne suffit que de peu d'individus dans la base d'apprentissage pour avoir une erreur de prédiction très faible : même 30% ou 20% des observations dans la base d'apprentissage permet d'identifier de manière fiable les classes des individus de la base de test. Par ailleurs, il y a rarement des erreurs.
# Après plusieurs tests, on constate que le classiffieur ne se trompe que très rarement (entre 0 et 2 fois selon la base de test)
# En particulier, il ne se trompe jamais pour les setosas, qui sont (comme on l'a vu plus haut) bien séparés (géométriquement) des deux autres groupes.
# En somme, les données utilisées ici sont particulièrement propres et adaptées à la méthode utilisée.

################################################
# Analyse discriminante quadratique prédictive #
################################################

#-----------
# Question 1
#-----------
help(qda)
model_qda = qda(Species~.,data=iris)
prediction_qda = predict(model_qda,iris)
ConfusionMatrix_qda = table(prediction_qda$class,iris$Species)
# Matrice de confusion QDA :
ConfusionMatrix_qda
# Erreur de prédiction QDA :
1-sum(diag(ConfusionMatrix_qda))/sum(ConfusionMatrix_qda)

#-----------
# Question 2
#-----------
# RETOUR AU LDA (pour comparaison) :
#-----------------------------------
# prediction LDA :
prediction = predict(model,iris)
ConfusionMatrix_lda  = table(prediction$class,iris$Species)
# Matrice de confusion LDA :
ConfusionMatrix_lda
# Erreur de prédiction LDA :
1-sum(diag(ConfusionMatrix_lda))/sum(ConfusionMatrix_lda)
# REMARQUES :
# Si on effectue une LDA ou une QDA sur l'ensemble des données, on obtient la même matrice de confusion.
# Pour comparer les deux méthodes, nous allons donc tester leurs résultats plusieurs fois sur des jeux d'apprentissages / test choisis aléatoirement.
# On effectue 1000 tests et on note à chaque fois le nombre d'erreurs de classification pour chaque méthode.
# (ceci est fait par la portion de code ci-dessous :)
#-------------------------------------------------------
# COMPARAISON LDA/QDA sur des bases apprentissage/test :
#-------------------------------------------------------
N = 1000
lda_errors = rep(0,0.20*nrow(iris))
qda_errors = rep(0,0.20*nrow(iris))
for (k in 1:N) {
	# sets :
	learning_indices = sample(nrow(iris),0.80*nrow(iris))
	learning_set = iris[learning_indices,]
	test_set = iris[-learning_indices,]
	# LDA model :
	lda_model = lda(Species~.,data=learning_set)
	lda_prediction = predict(lda_model,test_set)
	lda_ConfusionMatrix = table(lda_prediction$class,test_set$Species)
	lda_errors[sum(lda_ConfusionMatrix)-sum(diag(lda_ConfusionMatrix))] = lda_errors[sum(lda_ConfusionMatrix)-sum(diag(lda_ConfusionMatrix))] + 1
	# QDA model :
	qda_model = qda(Species~.,data=learning_set)
	qda_prediction = predict(qda_model,test_set)
	qda_ConfusionMatrix = table(qda_prediction$class,test_set$Species)
	qda_errors[sum(qda_ConfusionMatrix)-sum(diag(qda_ConfusionMatrix))] = qda_errors[sum(qda_ConfusionMatrix)-sum(diag(qda_ConfusionMatrix))] + 1
}
plot(lda_errors,type="l",col="red")
lines(qda_errors,col="blue")
title(main="LDA (rouge) / QDA (bleu) : nombre d'erreurs")
legend(c("LDA","QDA"),col=c("red","blue"));
# REMARQUES :
# Dans l'ensemble (sur les 1000 tests réalisés), la LDA donne de meilleurs résultats que la QDA : pour un nombre d'erreurs donné, il y a eu plus de réalisations pour la QDA que pour la LDA.
# Ainsi, il semblerait que la géométrie des données soit plutôt adaptée à un modèle linéaire que quadratique.
# La comparaison des deux méthodes se limite bien sûr aux résultats obtenus sur les données étudiées.

#########################
# Régression logistique #
#########################
rm(list=ls())

#-----------
# Question 1
#-----------

file = "D:/frenucci/Dropbox/Donn�es/Scolaire/cours/3A/Statistiques et Data-Mining/TP/TP1/SAHeart.txt"

data = read.table(file,sep=",",head=T,row.names=1)
data = data[,-6]
data

# A retrospective sample of males in a heart-disease high-risk region of the Western Cape, South Africa. There are roughly two controls per case of CHD. Many of the CHD positive men have undergone blood pressure reduction treatment and other programs to reduce their risk factors after their CHD event. In some cases the measurements were made after these treatments. These data are taken from a larger dataset, described in  Rousseauw et al, 1983, South African Medical Journal.

# Variables explicatives :
# sbp  	    systolic blood pressure
# tobacco   cumulative tobacco (kg)
# ldl		low densiity lipoprotein cholesterol
# adiposity 
# famhist	family history of heart disease (Present, Absent)
# typea		type-A behavior
# obesity	
# alcohol	current alcohol consumption
# age		age at onset
# chd		response, coronary heart disease

#-----------
# Question 2
#-----------
pairs(data,pch=22,bg=c("red","blue")[unclass(factor(data[,"chd"]))])

# la dernière ligne de graphiques nous permet de savoir à quoi correspondent les couleurs : les rouges sont les malades.
# On constate que sur tous les graphiques, tous les points sont assez éloignés pour qu’il n’y ait pas un unique facteur explicatif suffisant pour expliquer la maladie.
# Causes visibles à l'oeil nu : grands fumeurs, et gens à forts taux de ldl.

#-----------
# Question 3
#-----------
help(glm)
fit = glm(data$chd~.,data=data,family=binomial)
summary(fit)
#-----------
# Question 4
#-----------
classification = predict.glm(fit,data,type="response")
# classification = predict(fit,data,type="response")
classification = 1*(classification>0.5)
# pour avoir des 0 et 1 à la place de TRUE et FALSE
CM = table(classification,data$chd)
CM
# on peut aussi calculer l'erreur sur la base de test, on trouve 26%
erreur=1-sum(diag(CM))/sum(CM)
# les faux positifs sont au nombre de 77 (élément (1,2) de la matrice de confusion), les faux négatifs sont 46 (élément (2,1) de la matrice de confusion)
fauxneg=CM[1,2]/sum(CM)
fauxneg
# 17% de faux négatifs
fauxpos=CM[2,1]/sum(CM)
fauxpos
# 10% de faux positifs
# Dans l'ensemble, ces résultats ne sont pas très satisfaisants.

#-----------
# Question 5
#-----------

learning_indices = sample(nrow(data),0.75*nrow(data))
# on choisit 75% des lignes, au hasard, qui formeront le learning set
learning_set = data[learning_indices,]
testing_set = data[-learning_indices,]

lda_model = lda(chd~.,data=learning_set)
prediction_learning = predict(lda_model,learning_set)
CM_learning = table(prediction_learning$class,learning_set$chd)

erreur_learning=1-sum(diag(CM_learning))/sum(CM_learning)
fauxneg_learning=CM_learning[1,2]/sum(CM_learning)
fauxpos_learning=CM_learning[2,1]/sum(CM_learning)

# beaucoup d'erreurs : autour de 25% au total, en répétant le test plusieurs fois et en changeant les bases d'apprentissage et de test.

prediction_test = predict(lda_model,testing_set)
CM_test = table(prediction_test$class,testing_set$chd)

erreur_test=1-sum(diag(CM_test))/sum(CM_test)
fauxneg_test=CM_test[1,2]/sum(CM_test)
fauxpos_test=CM_test[2,1]/sum(CM_test)

# On pouvait s'attendre à de tels résultats : autour de 35% d'erreur pour la base de test. C'est normal : la prédiction à partir de la base d'apprentissage, pour les individus de la base d'apprentissage, n'était déjà pas excellente (25% d'erreur).
# Une approche par validation croisée permet de tester la robustesse de notre régression par rapport à l'ajout de données.

#-----------
# Question 6
#-----------

result = step(fit)
result$coefficients
summary(result)

# On retient les variables ldl, tobacco, famhist et age (modèle d'AIC le plus faible).
# Les coefficients les plus significatifs (i.e. au seuil 0.001) sont l'age et la présence d'antécédents familiaux. Ce sont les plus déterminants.
# Les autres sont tout de même significatifs au seuil 0.01.