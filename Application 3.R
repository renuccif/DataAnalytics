###########################################
# TP 3 - Jérôme CRÉTIEN - Florent RENUCCI #
#    20 Novembre - pour le 11 Décembre	  #
###########################################

rm(list=ls())

####################################
##### I - Analyse préliminaire #####
####################################

# mettre les fichiers spam.txt et indtrain.txt à la racine de R. 
tab = read.table("spam.txt",sep=";",header=TRUE)
ind = read.table("indtrain.txt",sep=";",header=TRUE)[,1]
#tab = read.table("/Users/renucciflorent/Dropbox/Projets/Etudes/ECP/Cours ECP/E2 - Méthodes Numériques pour la Finance/SDMA/TP3/spam.txt",sep=";",header=TRUE)
#ind = read.table("/Users/renucciflorent/Dropbox/Projets/Etudes/ECP/Cours ECP/E2 - Méthodes Numériques pour la Finance/SDMA/TP3/indtrain.txt",sep=";",header=TRUE)


# nombre d'obervations :
nrow(tab)
# il y a donc 4601 observations

# nombre de variables :
ncol(tab)
# il y a donc 58 variables

# On retrouve le résultat avec :
dim(tab)
# qui donne le nombre de lignes x le nombre de colonnes

# les noms indiqués dans le dataframe ne sont pas très explicites :
names(tab)

# d'après spambase.names : 
# - il y a 48 variables réelles A.i pour i de 1 à 48, représentant le pourcentage du mot i dans le mail. Ces variables vivent dans [0;100], les mots sont caractéristiques des mails spams ("money", "business" etc...)
# - il y a 6 variables A.i, pour i de 49 à 54, correspondant au pourcentage du caractère i dans le mail. Ces variables vivent dans [0;100]. Les caractères sont typiques des spams : ;([!$#
# - A.55 : nombre de lettres majuscules successives moyen.
# - A.56 : nombre maximum de lettres majuscules successives
# - A.57 : nombre total de majuscules
# - A.58 : résultat du test : spam ("spam") ou pas ("email")

#########################################
##### II - Arbres de classification #####
#########################################

library(rpart)
learn=tab[ind,];
test=tab[-ind,];
model = rpart("spam ~.", learn)
plot(model)
text(model)

# 5 variables interviennent, A.53, A.7, A.52, A.57 et A.25, et la plus influente est la A.53 (première coupure) : c'est le pourcentage du symbole "$" dans le mail (ce qui n'est pas surprenant!)


## erreurs de classification

# sur la base d'apprentissage :
 
prediction_learn = predict(model,learn,type="class")
CM_learn = table(prediction_learn,learn$spam)
erreur_learn= 1-sum(diag(CM_learn))/sum(CM_learn)

# on trouve la matrice de confusion (en ligne : le vrai statut du message; en colonne: le résultat de la classification):
CM_learn

# et l'erreur de prédiction :
erreur_learn 
# l'erreur est de 9.9%, ce qui n'est pas très bon

# donc 3108 (90.1%) messages sont bien classés, et 342 (9.9%) non. Parmi eux, 98 email normaux sont considérés comme spam (c'est grave), et 244 spams sont considérés comme mails (c'est moins grave).
# il existe en effet un asymétrie entre faux positifs (fausse alerte) et faux négatifs (non détection) : il est nettement plus grave de filtrer un email considéré à tord comme spam que de laisser passer un spam dans la boîte de réception.
# taux de fausse alerte : 2.8%
# taux de non détection : 7.1%


# sur la base de test : 

prediction_test=predict(model,test,type="class")
CM_test = table(prediction_test,test$spam)
erreur_test= 1-sum(diag(CM_test))/sum(CM_test)

# on trouve la matrice de confusion (en ligne: le vrai statut du message; en colonne : le résultat de la classification):
CM_test

#et l'erreur de prédiction :
erreur_test
# l'erreur est de  11%, ce qui n'est pas très bon (mais cohérent avec le résultat précédent : erreur_test>erreur_learn)

# donc 1024 (89%) messages sont bien classés, et 127 (11%) non. Parmi eux, 30 email sont considérés comme spam (c'est grave), et 97 spams sont considérés comme mails (c'est moins grave).
# taux de fausse alerte : 2.6%
# taux de non détection : 8.4%

# Dans l'ensemble, les résultats de la méthode par arbre de classification ne sont pas très bons. Le taux de fausse alerte demeure nettement trop élevé : dans les deux cas, plus de 4% des (vrais) emails sont filtrés comme spam. 1 (vrai) email sur 25 n'arrive pas à destination... C'est trop.

#########################
##### III - Bagging #####
#########################

library(adabag)
bagging=bagging(spam~.,learn)
# Par défaut, 100 arbres sont générés. Nous n'avons pas changé cette valeur (le temps de calcul est donc important)
prediction_learn_bagging=predict(bagging,learn,type="class")
prediction_test_bagging=predict(bagging,test,type="class")

# base d'apprentissage :

prediction_learn_bagging$error
# valeur : 8.3% sur la base d'apprentissage

prediction_learn_bagging$confusion

#              Observed Class
# Predicted Class email spam
#           email  1998  187
#           spam    101 1164

# On a un taux de fausse alerte de 2.9% (identique au cas précédent), mais on tombe à 5.4% de non-détection (les résultats sont susceptibles de varier, puisque le bootstrap est par nature aléatoire)

# base de test :

prediction_test_bagging$error
# valeur : 9.6% sur la base de test (les résultats sont susceptibles de varier, puisque le bootstrap est par nature aléatoire)

prediction_test_bagging$confusion
#              Observed Class
# Predicted Class email spam
#           email   659   81
#           spam     30  381

# On a un taux de fausse alerte de 2.6% (identique au cas précédent), mais on tombe à 7% de non-détection (les résultats sont susceptibles de varier, puisque le bootstrap est par nature aléatoire)

# Conclusion : comme on pouvait s'y attendre, le modèle Bagging est plus performant que celui de l'arbre simple (on gagne pour chacun des sets environ 1.5% en non-détection)
# On pourrait peut-être améliorer les résultats en augmentant le nombre de tirages bootstrap (bien que le temps de calcul soit déjà important).

##############################
##### IV - Random Forest #####
##############################

library(randomForest)
randomForest_learn=randomForest(spam~.,learn)
randomForest_test = predict(randomForest_learn,test,type="class");

# base d'apprentissage :

randomForest_learn$confusion

# On obtient : 
#       email spam
# email  2035   64 
# spam    109 1242 

# Soit une erreur de 5%  avec 3.1% de fausses alertes et 1.9% de non détection.
# Cette méthode montre de très bonnes performances.

# base de test :

CM_test = table(randomForest_test,test$spam)
CM_test

# On obtient : 
#       email spam
# email   676   41  
# spam     13  421  

# Soit une erreur de 6.5% avec 4.3% de fausses alertes et 2.1% de non détection sur la base de test, ce qui est toujours plus élevé que ce qu'on trouve sur la base d'entrainement (attendu). Mais les performances de l'algorithme random forest sont nettement meilleures que celles des deux premières méthodes, y-compris sur la base de test.

######################################
##### V - Support Vector Machine #####
######################################

library(kernlab)
ksvm_learn_lin=ksvm(spam~.,learn,kernel="vanilladot")
ksvm_learn_gauss=ksvm(spam~.,learn)


prediction_learn_ksvm_lin=predict(ksvm_learn_lin,learn)
prediction_learn_ksvm_gauss=predict(ksvm_learn_gauss,learn)

prediction_test_ksvm_lin=predict(ksvm_learn_lin,test)
prediction_test_ksvm_gauss=predict(ksvm_learn_gauss,test)


CM_learn_lin = table(prediction_learn_ksvm_lin,learn$spam)
erreur_learn_ksvm_lin = 1-sum(diag(CM_learn_lin))/sum(CM_learn_lin)

CM_learn_gauss = table(prediction_learn_ksvm_gauss,learn$spam)
erreur_learn_ksvm_gauss = 1-sum(diag(CM_learn_gauss))/sum(CM_learn_gauss)


# base d'apprentissage :

# On trouve : 
CM_learn_lin
CM_learn_gauss

erreur_learn_ksvm_lin
erreur_learn_ksvm_gauss

# Pour le noyau linéaire : une erreur de 6.7%, pour un taux de fausse alerte de 2.8% et un taux de non détection de 3.9%.
# Ce modèle obtient des résultats moins bons que les Random Forest, mais meilleurs que les arbres de classification et que le bagging

# Pour le noyau gaussien : une erreur de 4.5%, pour un taux de fausse alerte de 1.5% et un taux de non détection de 3%.
# Ce modèle obtient des résultats significativement supérieurs aux autres.

# base de test :

CM_test_lin = table(prediction_test_ksvm_lin,test$spam)
erreur_test_ksvm_lin = 1-sum(diag(CM_test_lin))/sum(CM_test_lin)

CM_test_gauss = table(prediction_test_ksvm_gauss,test$spam)
erreur_test_ksvm_gauss = 1-sum(diag(CM_test_gauss))/sum(CM_test_gauss)

# On trouve :
CM_test_lin
erreur_test_ksvm_lin

CM_test_gauss
erreur_test_ksvm_gauss

# Pour le noyau linéaire : une erreur de 7%, pour un taux de fausse alerte de 2.7% et un taux de non détection de 4.3%.
# Ce modèle obtient des résultats moins bons que les Random Forest, mais meilleurs que les arbres de classification et que le bagging.

# Pour le noyau gaussien : une erreur de 5.9% , pour un taux de fausse alerte de 1.9% et un taux de non détection de 4%.
# Ce modèle obtient des résultats significativement supérieurs aux autres.


##### Choix du paramètre C #####
# Pour C donné, R utilise une valeur optimale de sigma
# Ainsi, si on choisit une valeur de C à l'extrémité du domaine d'intérêt, on peut ensuite la faire diminuer tant que l'erreur diminue (on s'arrête donc dès que l'erreur augmente).
# initialisation
new_error = 1;
old_error = 2;
C_opt = 100000;

while (TRUE) {
old_error = new_error
ksvm_learn = ksvm(spam~.,data=learn,C=C_opt)
prediction_test = predict(ksvm_learn,test)
CM_test = table(prediction_test,test$spam)
new_error = 1-sum(diag(CM_test))/sum(CM_test)
	if(new_error>old_error){
		print("On obtient C_opt :")
		print(C_opt*10)
		break
	}
	else{
		C_opt = C_opt/10
	}
}
# Pour la base d'apprentissage choisie, on a C_opt de l'ordre de 10.
# Pour s'affranchir de la dépendance en la base d'apprentissage choisie, il faudrait réitérer l'opération à partir de plusieurs bases.

########################################################################
##### VI - Scoring, régression logistique et analyse discriminante #####
########################################################################

### régression logistique

log = glm(spam~.,learn,family=binomial)

summary(log)

# Les 11 variables significativement plus importantes (sur 14 indiquées ***) sont les suivantes : A.5, A.6, A.7, A.16, A.23, A.25, A.27, A.45, A.46, A.52, A.53.

learn_log = predict.glm(log,learn,type="response")<0.5
test_log = predict.glm(log,test,type="response")<0.5

traitement = function(data,data_predict) {
for (i in 1:nrow(data)){
		if (data_predict[i]) {
			data_predict[i]="email";
		}
		else data_predict[i]="spam";
	}
	data_predict;
}

learn_log = traitement(learn, learn_log)
test_log = traitement(test, test_log)

# on transforme des probabilités en valeurs TRUE et FALSE, puis en valeurs "spam" et "email"

# base d'apprentissage :

# on trouve : 
CM_learn_log = table(learn_log,learn$spam)
CM_learn_log

# learn_log email spam
#     email  2000  138
#     spam     99 1213

erreur_log_learn = 1-sum(diag(CM_learn_log))/sum(CM_learn_log)
erreur_log_learn
# erreur = 6.9%, pour 2.9% de fausse détection et 4% de non détection. Performances plutôt bonnes (similaires au SVM noyau linéaire).

# base de test :

# on trouve : 
CM_test_log = table(test_log,test$spam)
CM_test_log

# test_log email spam
#    email   658   49
#    spam     31  413

erreur_log_test = 1-sum(diag(CM_test_log))/sum(CM_test_log)
erreur_log_test
# erreur = 6.9%, pour 2.7% de fausse détection et 4.2% de non détection. Performances plutôt bonnes  (similaires au SVM noyau linéaire).

### analyse discriminante

library(MASS)
ad = lda(spam~.,data=learn)
ad_learn = predict(ad,learn)$class
ad_test = predict(ad,test)$class

# base d'apprentissage :

# on trouve :
CM_learn_ad = table(ad_learn,learn$spam)
erreur_ad_learn = 1-sum(diag(CM_learn_ad))/sum(CM_learn_ad)

CM_learn_ad

# ad_learn email spam
#    email  2002  298
#    spam     97 1053

erreur_ad_learn
# Soit une erreur de 11.4%, 2.8% de fausses détections, 8.6% de non détection. Ce qui est très mauvais.

# base de test :

# on trouve : 
CM_test_ad = table(ad_test,test$spam)
erreur_ad_test = 1-sum(diag(CM_test_ad))/sum(CM_test_ad)

CM_test_ad

# ad_test email spam
#   email   665   97
#   spam     24  365

erreur_ad_test
# Soit une erreur de 10.5%, 2.1% de fausses détections, 8.4% de non détection. Ce qui est aussi très mauvais.

##### Conclusion #####
# Avec la base d'apprentissage et la base de test choisies initialement, les différentes méthodes se classent ainsi (de la plus efficace à la moins efficace) :
# SVM noyau gaussien > Random Forest > SVM noyau linéaire >~ Régression Logistique > Bagging > Arbres de Classification > Analyse Discriminante
# Dans la suite, on va chercher à s'affranchir de la dépendance vis-à-vis du choix des bases d'apprentissage et de test pour comparer les méthodes.

###########################################################
##### VII - Comparaison des modèles de classification #####
###########################################################

K = 10

ETrain = matrix(0,K,7)
ETest = matrix(0,K,7)

for (k in 1:K) {
	indices = sample(nrow(tab),0.75*nrow(tab))
	learn = tab[indices,]
	test = tab[-indices,]
	
	##########
	# Arbres #
	##########
	arbre = rpart("spam ~.", learn)
	prediction_arbre_learn = predict(arbre,learn,type="class")
	CM_arbre_learn = table(prediction_arbre_learn,learn$spam)
	ETrain[k,1] = 1-sum(diag(CM_arbre_learn))/nrow(learn)
	
	prediction_arbre_test = predict(arbre,test,type="class")
	CM_arbre_test = table(prediction_arbre_test,test$spam)
	ETest[k,1] = 1-sum(diag(CM_arbre_test))/nrow(test)
	
	###########
	# Bagging #
	###########
	bagging=bagging(spam~.,learn,mfinal = 5)
	prediction_bagging_learn=predict(bagging,learn,type="class")
	CM_bagging_learn = prediction_bagging_learn$confusion
	ETrain[k,2] = 1-sum(diag(CM_bagging_learn))/nrow(learn)
	
	prediction_bagging_test=predict(bagging,test,type="class")
	CM_bagging_test = prediction_bagging_test$confusion
	ETest[k,2] = 1-sum(diag(CM_bagging_test))/nrow(test)
	
	#################
	# Random forest #
	#################
	randomForest_learn=randomForest(spam~.,learn)
	CM_randomForest_learn = randomForest_learn$confusion[,-3]
	ETrain[k,3] = 1-sum(diag(CM_randomForest_learn))/nrow(learn)
	
	randomForest_test = predict(randomForest_learn,test,type="class");
	CM_randomForest_test = table(randomForest_test,test$spam)[,-3]
	ETest[k,3] = 1-sum(diag(CM_randomForest_test))/nrow(test)
	
	################
	# SVM linéaire #
	################
	
	svm_lin_learn = ksvm(spam~.,learn,kernel="vanilladot")
	prediction_svm_lin_learn = predict(svm_lin_learn,learn)
	CM_svm_lin_learn = table(prediction_svm_lin_learn,learn$spam)
	ETrain[k,4] = 1-sum(diag(CM_svm_lin_learn))/nrow(learn)
	
	prediction_svm_lin_test = predict(svm_lin_learn,test)
	CM_svm_lin_test = table(prediction_svm_lin_test,test$spam)
	ETest[k,4] = 1-sum(diag(CM_svm_lin_test))/nrow(test)

	################
	# SVM gaussien #
	################
	
	svm_gauss_learn = ksvm(spam~.,learn)
	prediction_svm_gauss_learn = predict(svm_gauss_learn,learn)
	CM_svm_gauss_learn = table(prediction_svm_gauss_learn,learn$spam)
	ETrain[k,5] = 1-sum(diag(CM_svm_gauss_learn))/nrow(learn)
	
	prediction_svm_gauss_test = predict(svm_gauss_learn,test)
	CM_svm_gauss_test = table(prediction_svm_gauss_test,test$spam)
	ETest[k,5] = 1-sum(diag(CM_svm_gauss_test))/nrow(test)
	
	#########################
	# Régression logistique #
	#########################
	log = glm(spam~.,learn,family=binomial)
	log_learn = predict.glm(log,learn,type="response")<0.5
	log_learn = traitement(learn,log_learn)
	CM_log_learn = table(log_learn,learn$spam)
	erreur_log_learn = 1-sum(diag(CM_log_learn))/sum(CM_log_learn)
	ETrain[k,6] = 1-sum(diag(CM_log_learn))/nrow(learn)
	
	
	log_test = predict.glm(log,test,type="response")<0.5
	log_test = traitement(test,log_test)
	CM_log_test = table(log_test,test$spam)
	ETest[k,6] = 1-sum(diag(CM_log_test))/nrow(test)
	
	#########################
	# Analyse discriminante #
	#########################
	ad = lda(spam~.,data=learn)
	ad_learn = predict(ad,learn)$class
	CM_ad_learn = table(ad_learn,learn$spam)
	ETrain[k,7] = 1-sum(diag(CM_ad_learn))/nrow(learn)
	
	ad_test = predict(ad,test)$class
	CM_ad_test = table(ad_test,test$spam)
	ETest[k,7] = 1-sum(diag(CM_ad_test))/nrow(test)
}

######################################
##### Exploitation des résultats #####
######################################

methods = c('Arbres','Bagging','RandForest','SVM lin','SVM gauss','Rég log','AD')
mean_error = rep(0,7)
sd_error = rep(0,7)
mean_error = as.data.frame(mean_error,methods)
sd_error = as.data.frame(sd_error,methods)
error_summary_learn = cbind(mean_error,sd_error)
error_summary_test = cbind(mean_error,sd_error)

for(i in 1:7){
	error_summary_learn[i,1] = mean(ETrain[,i])
	error_summary_learn[i,2] = sd(ETrain[,i])
	error_summary_test[i,1] = mean(ETest[,i])
	error_summary_test[i,2] = sd(ETest[,i])
}

# erreur moyenne et écart-type pour chaque méthode sur la base d'apprentissage (en %) :
error_summary_learn*100
# erreur moyenne et écart-type pour chaque méthode sur la base de test (en %) :
error_summary_test*100

# On synthétise les résultats graphiquemetn grâce à des boîtes à moustache :
boxplot(ETrain, names=methods,main = "Erreurs de classification de la base d'apprentissage")
boxplot(ETest, names=methods,main = "Erreurs de classification de la base de test")

# Ainsi, sur la base d'apprentissage, on retrouve le même ordre d'efficacité des méthodes.
# Par contre, sur la base de test, la Random Forest a de meilleurs résultats que le SVM (envirion 2% de mieux en moyenne).
# Évidemment, la volatilité observée du taux d'erreur est nettement plus faible sur la base d'apprentissage que sur la base de test.

##### Conclusion #####
# Les méthodes de type arbres de classification, bagging et analyse discriminante sont clairement à éliminer pour le problème posé, avec plus de 10% d'erreurs (ou presque pour le bagging) en moyenne.
# La régression logistique et la SVM linéaire sont comparables et arrivent à un niveau intermédiaire, sans être aussi performante que la SVM ou la Random Forest.
# La Random Forest semble plus robuste que la SVM, puisqu'elle est donne des résultat nettement meilleurs sur la base de test (2% de mieux).
# Ainsi, la méthode à privilégier est la Random Forest.

# Enfin, on remarque que toutes les méthodes donnent des taux de fausses alarmes plus faibles que le taux de non détection, ce qui est de bon goût étant donnée l'asymétrie évoquée plus haut (plus grave de déclarer un email spam que l'inverse).
######################