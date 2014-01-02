###########################################
# TP 2 - Jérôme CRÉTIEN - Florent RENUCCI #
# 16 octobre - pour le 20 novembre		  #
###########################################


#################
# Application 1 #
#################
rm(list=ls())

file = '/Users/JEROME/Documents/My-Docs/Etudes/ECP/3A/OMA/OMA - 2012-2013/Statistiques - Data_Mining - Apprentissage/TP/TP-2/UsCrime.txt'
file = "D:/frenucci/Dropbox/Donn�es/Scolaire/cours/3A/Statistiques et Data-Mining/TP/TP2/UsCrime.txt"
tab = read.table(file,sep='\t',header=TRUE)

# Nombre d'observations disponibles :
nrow(tab)
# On a 47 observations

pairs(tab,pch=22,bg=c('red','blue')[unclass(factor(tab[,'R']))])
# On remarqe à l'oeil nu que certaines variables sont fortement corrélées. 
# Par exemple U1 et U2, qui représentent le taux de chômage pour les 14-24 ans et les 35-39 ans respectivement. Il est évident que ces 2 variables sont corrélées (sans pour autant être liées par une relation de causalité directe) : si le taux de chômage de l'Etat augmente, il augmente pour les 2 tranches d'âge.
# Autre exemple, W et X sont aussi corrélées (négativement cette fois) : apparemment plus la médiane de la richesse d'une famille est grande (donc plus les familles de l'Etat sont riches), moins nombreuses seront les familles dont la richesse se situe sous la moitié de la médiane (donc moins il y aura de "très pauvres"). 
# Autre exemple, Ex0 et Ex1 sont aussi corrélées positivement : plus les dépenses pour la police sont importantes en 1959, plus elles le seront en 1960, pour un état donné.

cov(tab)	# matrice de covariance
cor(tab)	# matrice de corrélation

# Comme intuité précédemment, Ex0 et Ex1 sont fortement corrélés (0.99), U1 et U2 aussi (0.74), et W et X sont corrélés négativement (-0.88).
# D'une manière générale, un résultat proche de -1 explique une forte corrélation négative (l'augmentation d'une variable entraine la diminution de l'autre, presque linéairement), un résultat proche de 1 explique à l'inverse une forte corrélation positive. Un résultat proche de 0 dénote d'une certaine indépendance.

#################################
# Modèle de régression linéaire #
#################################

# le modèle est de type Y = XB + epsilon. Y la variable expliquée, X les variables explicatives (matrice n*K, n observations, K variables), B un vecteur de taille K, epsilon une perturbation (vecteur de taille n). On cherche les coefficients de B pour ensuite pouvoir prédire Y.

help(lm)
res = lm('R~.',data=tab)


# Question 1
#-----------
print(res)
summary(res)
attributes(res)

# plus la t-value (statistique de Student) est faible en valeur absolue (ce qui correspond à une forte p-value), moins la variable est significative. A un risque fixé, elle permet donc d'accepter ou de rejeter H0. Avec ici H0 = "coefficient de corrélation = 0" (ie "le coefficient n'est pas significatif").
# Ed, X, Age et U2 sont significatifs ici. L'intercept l'est également : Y n'est donc pas centrée.
# les coefficients estimés se lisent dans la première colonne (Age = 1.040, S = -8.308 etc...).

# Question 2
#-----------

# R^2 = SCE/SCT avec SCE = variance estimée et SCT = variance réelle.

# On lit dans summary(res) :
# R^2 = 0.7692
# C'est aussi : R^2 :=
var(res$fit)/var(tab$R)
# R^2 relativement proche de 1, donc modèle globalement significatif
# La significativité globale du modèle est en fait donnée par la statistique de Fisher => summary(fit) donne sa valeur et sa p-value : 3.686e-07 qui est très faible. Il y a donc au moins un coefficient non nul (le modèle est globalement significatif).

# Question 3
#-----------
# summary(res) donne toutes les informations nécessaires à juger la significativité de chacun des coefficients.
# L'hypothèse nulle testée est la non significativité de chaque coefficient (H0 = "coefficient de corrélation = 0"). Plus la p-value est faible, plus H0 doit être rejetée.
# summary(res) retranscrit cette information par les codes ., *, **, *** qui expriment le niveau de confiance (ou de risque) attaché à la significativité de chaque coefficient, selon la légende suivante : 0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# par exemple, un coefficient *** est significatif, au risque 1 pour 1000 (ou moins).
# Comme dit plus haut : Ed, X, Age et U2 sont significatifs ici. L'intercept l'est également : Y n'est donc pas centrée.


# Intervalles de confiance :
#---------------------------
confint(res,level=0.95)	# intervalle de confiance au risque 5%
confint(res,level=0.99)	# intervalle de confiance au risque 1%

# On remarque que les intervalles de confiance augmentent quand le niveau de risque diminue (ce qui est attendu) et qu'ils sont centrés sur la valeur estimée (également attendu).
# Pour décider de la significativité d'un coefficient, on peut vérifier si la valeur 0 appartient ou non à l'intervalle de confiance. Si c'est le cas, on consière que le coefficient n'est pas significatif (qu'il est nul).
# On retrouve bien que Ed, X et l'intercept sont significatifs au risque 1% et que Age et U2 le sont au risque 5%.


# Question 4
#-----------
# REM : res$fit = predict(res)
plot(tab$R,res$fit)
# On est autour de la 1ère bissectrice, ce qui tend à montrer que la régression permet d'expliquer la variable étudiée.

help(predict.lm)
predict(res,tab,interval='confidence',level=0.95)
predict(res,tab,interval='prediction',level=0.95)
print(cbind(predict(res,tab,interval='confidence',level=0.95),tab$R))
# les observtions 2 et 3 ne sont pas dans l'intervalle de confiance...
print(cbind(predict(res,tab,interval='prediction',level=0.95),tab$R))
# l'observtion 11 n'est pas dans l'intervalle de prediction. Pourtant, comme cet intervalle prend en compte la perturbation, il est plus grand que l'intervalle de confiance.
# Ceci montre les limites prédictives de notre modèle (tout en nous rassurant quant à un problème de surapprentissage).

# Question 5
#-----------
# Erreur quadratique des résidus : 
sum(res$residuals^2)
# un résidu sur une observation est (mesure - fit) ^ 2. S'ils sont très faibles, le risque est d'overfitter (trop "coller" aux données, et trouver un modèle très complexe, en oubliant donc la réalité de la question), s'ils sont très forts, le risque est d'oversmoother (trop "survoler" les données et donc ce qu'elles représentent, pour favoriser un modèle plus simple).

# Variance résiduelle :
var(res$residuals)
# On a variance totale = variance résiduelle + variance expliquée. On veut donc que variance résiduelle/variance totale soit proche de 0.
var(res$residuals)/(var(res$fit)+var(res$residuals))
# on trouve 23%, donc on explique 77% de la variance grâce au fit, ce qui est satisfaisant.

plot(tab$R,res$residuals)
# On cherche ici à évaluer l'homo/hétéro-scédasticité des résidus. Sur le graphe, on n'observe pas de variabilité particulière des résidus. On peut donc raisonablement penser qu'ils sont bien homo-scésastique, ce qui valide (sous réserve de gaussianité) la démarche du modèle linéaire.

# On trace le qqplot des résidus :
qqnorm(res$residuals)
# Il nous paraît plus lisible de renormaliser :
qqnorm(res$residuals/sqrt(var(res$residuals)))
# Les points sont assez bien alignés sur la 1ère bissectrice :
# masse centrale des résidus bien alignés sur les quantiles de la loi normale
# qq outliers pour les valeurs extrêmes
# On peut ainsi conclure graphiquement à la gaussianité des résidus.

shapiro.test(res$residuals)
# Grande p-value devant 0.05 => pas de raison de rejeter l'hypothèse gaussienne.
# Cela confirme le résultat précédent et valide donc la démarche du modèle linéaire : résidus gaussiens et homoscédastiques.


# Question 6
#-----------
indTest = seq(1,nrow(tab),3)
tabTest = tab[indTest,]
tabTrain = tab[-indTest,]
resTrain = lm('R~.',data=tabTrain)
predTest = predict.lm(resTrain,tabTest)
resTest = predTest - tabTest$R
# L'erreur quadratique moyenne n'est pas très satisfaisante :
sqrt(var(resTest))

# On peut également calculer la part de variance résiduelle :
var(resTest)/(var(predTest)+var(resTest))
# on trouve 51%, donc on explique 49% de la variance sur la base de test grâce au modèle. C'est inférieur au résultat précédent, certainement parce que la base d'apprentissage est moins grande (2/3 de la précédente).

# Question 7
#-----------
x11();par(mfrow=c(2,2));plot(res);
# Les 2 graphes du haut ont déjà été tracés à la question 5 :
# Le 1er permet de visualiser graphiquement la variance (homo/hétéro-scédastique) - ici, on conclut à l'homoscédasticité
# Le 2ème permet de voir si les données sont gaussiennes ou non : QQ-plot.
# Les 2 graphes du bas permettent de tester l'homoscédasticité de manière plus précise ('effet levier'... pour trouver des outliers...)

########################
# Sélection de modèles #
########################

# 1 - Régression Backward
#------------------------
regbackward = step(res,direction='backward')
summary(regbackward)
# On enlève la variable qui a le plus grand RSS (critère AIC => mais ça revient à considérer le RSS ie les résidus)
# A chaque étape, on calcule les RSS associés à chaque variable, on éliminele plus faible (à la 1ère itération par exemple, c'est la variable NW), et on recommence. L'AIC calculé à chaque étape représente la perte en informations lorsqu'on néglige une variable. On cherche à le minimiser, donc lorsque la suppression d'une variable n'entraîne plus une baisse de l'AIC, on arrête l'algorithme. 
# Finalement, il ne reste que les variables Age, Ed, Ex0, U2, W, X, avec une p-value de 1.441 10^-10. Dans la 1ère partie on avait une p-value de 3.7 10^-7. 
# Par ailleurs on avait R^2=0.68, on a maintenant 0.71.
# Le modèle est plus parcimonieux mais a une plus grande valeur explicative. Les variables éliminées n'apportaient donc pas d'information pertinente.

# 2 - Régression Forward
#-----------------------
regforward = step(lm(R~1,data=tab),list(upper=res),direction='forward')
summary(regforward)
# On cherche à rajouter la variable qui donne le modèle avec les plus petits résidus.
# On s'arrête quand la p-value du coeff qu'on veut ajouter est inférieure à un seuil (quand le coeff n'est pas significatif).
# On obtient le même modèle parcimonieux avec les 2 méthodes, ce qui n'avait rien d'évident a priori, puisqu'il s'agit d'optimisation partielle.

# 3 - Régression Stepwise
#------------------------
regboth = step(res,direction='both')
summary(regboth)
# Ici, on part du modèle nul, qu'on augmente, comme en régression forward. A chaque étape, on ajoute la variable qui augmente le plus le RSS, tout en rejetant les variables jugées non pertinentes (non significatives pour le modèle courant). On s'arrête lorsque toutes les variables ont été sélectionnées ou rejetées.
# Cette approche revient à combiner les deux précédentes, en regardant les effets de l'ajout ou de la suppression d'une variable à chaque étape.
# Ici encore, on obtient le même modèle parcimonieux qu'avec les 2 méthodes précédentes (ce qui n'était toujours pas attendu a priori).

# 4 -
#----
# En entrant formula(regboth), on obtient les variables conservées par la régression stepwise :
formula(regboth)
reg0 = lm(formula(regboth),data=tab)

# On retrouve le modèle parcimonieux :
summary(reg0)
# On retrouve le modèle correspondant au résultat de l'algorithme stepwise, donc avec seulement 6 variables. R^2 et la p-value sont satisfaisants.


####################
# Régression LASSO #
####################

# Question 1
#-----------
library(lars)
help(lars)
# la régression Lasso minimise le RSS avec la contrainte : somme des valeurs absolues des coeff < constante (paramètre à fixer par le statisticien).
# elle peut également se formaliser comme la minimisation du RSS pénalisé de lambda * la somme des valeurs absolues des coeff (norme 1 de B). Ici, c'est lambda le paramètre.

# Question 2
#-----------
X = data.matrix(tab[-c(1)])
Y = tab$R
reslasso = lars(X,Y,type='lasso')
plot(reslasso)
# On représente les coefficients (standardisés) en fonction de lambda : c'est le "chemin de régularisation".
# On voit bien ici que le Lasso favorise la parcimonie et la sélection de variables : leur nombre diminue quand lambda -> 0.
plot(reslasso$lambda)
# On représente les lambdas selon la norme 1 de B divisée par la norme 1 max obtenue.

# Question 3
#-----------
coef_lasso_l0 = predict.lars(reslasso,X,type='coefficients',mode='lambda',s=0)
# On trace les coefficients :
x11();barplot(coef_lasso_l0$coefficients)
# On peut comparer les résultat du Lasso avec lambda = 0 :
coef_lasso_l0$coef
# Avec ceux du modèle linéaire trouvés plus tôt :
res$coef
# On obtient bien la même chose (on fait la même chose, puisque lambda = 0 : on ne pénalise pas).
# Seule différence : l'intercept n'apparaît pas dans les coefs du Lasso.

# Question 4
#-----------
coef_lasso_l1 = predict.lars(reslasso,X,type='coefficients',mode='lambda',s=1)
coef_lasso_l4 = predict.lars(reslasso,X,type='coefficients',mode='lambda',s=4)
coef_lasso_l20 = predict.lars(reslasso,X,type='coefficients',mode='lambda',s=20)
# Coefficients LASSO pour lambda = 1 :
coef_lasso_l1$coef
# On a tué 3 coefficients
# Coefficients LASSO pour lambda = 4 :
coef_lasso_l4$coef
# On a tué 1 coefficient de plus (depuis lambda = 1) [en tout 4]
# Coefficients LASSO pour lambda = 20 :
coef_lasso_l20$coef
# On a tué 3 coefficients de plus (depuis lambda = 4) [en tout 7]

# Lorsque lambda augmente, on risque moins d'overfitter, parce que la contrainte de minimisation de B est prépondèrante.
# Et ce jusqu'à ce que B arrive à la norme 0.
# Il faut donc trouver un équilibre entre la régression linéaire (lambda = 0) et une annulation de B.
# Ceci revient à la problématique d'optimisation biais/variance.

# La capacité de sélection de variable du Lasso apparaît encore ici.


# Question 5
#-----------
pY = predict.lars(reslasso,X,type='fit',model='lambda',s=2)
sqrt(var(Y - pY$fit))
# l'erreur quadratique moyenne est plus élevée que dans le modèle linéaire.

# On peut également considérer :
var(Y-pY$fit)/var(Y)
# Var totale = var expliquée + var résiduelle
# ici on explique donc 44% de la variance. Le modèle linéaire donnait de meilleurs résultats.
# Ainsi, la sélection de variables, qui rend le modèle plus parcimonieux et plus robuste, lui a hôté de son caractère prédictif.


####################
# Régression RIDGE #
####################

# Question 1
#-----------
# la régression Ridge minimise le RSS avec la contrainte : norme 2 du vecteur coeff < constante (paramètre à fixer par le statisticien).
# elle peut également se formaliser comme la minimisation du RSS pénalisé de lambda * la norme 2 de B. Ici, c'est lambda le paramètre.
# enfin, l'estimateur Ridge s'exprime explicitement en fonction de lambda, X et Y.
library(MASS)
help(lm.ridge)

# Question 2
#-----------
resridge_l0 = lm.ridge('R~.',data=tab,lambda=0)
resridge_l100 = lm.ridge('R~.',data=tab,lambda=100)

resridge_l0$coef	# ne retourne pas l'intercep
coef(resridge_l0)	# retourne tous les coefs, yc l'intercept
resridge_l0			# idem coef(resridge_l0)

resridge_l0
# On obtient les mêmes résultats qu'avec le modèle linéaire (puisqu'on ne le pénalise pas ici).
resridge_l100
# Ici, la pénalité est importante. Beaucoup de coefficients (les moins significatifs) voient leur valeur absolue diminuer de beaucoup. Neanmoins, contrairement au Lasso, Ridge ne sélectionne pas les variables (du moins pas explicitement : il faudrait fixer un seuil de significativité...).	

# Question 3
#-----------
lambda = seq(0,100,0.01)
resridge = lm.ridge('R~.',data=tab,lambda=lambda)
plot(lambda,resridge$GCV,type='l')
plot(resridge)	# chemin de régularisation
# Certains coeffs changent de signe (ce qui peut surprendre a prrmière vue...)
# Tous les coefficients tendent vers 0.
# Encore une fois, la sélection de variables est moins efficace qu'avec le Lasso.

# On choisit le modèle à retenir en minimisant GCV. On obtient lambda = 2 environ, par lecture sur plot(lambda,resridge$GCV,type='l').
# Une approche plus précise donne lambda optimal = 2,19, atteint en 220 :
which.min(resridge$GCV)
# Ainsi, les coefs du modèle sont en coef(resridge)[220,] :
Coefridge = coef(resridge)[220,]
Coefridge

# Question 4
#-----------
Xridge=cbind(rep(1,47),tab[,2:14])
Yridge=as.matrix(Xridge)%*%as.vector(Coefridge)
# Ici Yridge = X.B. On trouve Yridge environ égal à Y.

# L'erreur quadratique moyenne est :
sqrt(var(Y-Yridge))
# C'est la plus faible qu'on ait obtenue !

# On calcule la part de variance résiduelle :
var(Y-Yridge)/var(Y)
# Ici, on explique 75% de la variance. C'est le meilleur résultat.

# Il semble donc que la régression Ridge ait le meilleur pouvoir prédictif pour le jeu de données considéré.
# Notons malgré tout qu'il s'agit de la solution optimale en lambda (ce qui n'était pas le cas a priori pour le Lasso).

##########################################
# Comparaison des méthodes de régression #
##########################################

# Question 1
#-----------
learning_indices = sample(nrow(tab),0.70*nrow(tab))
# on choisit 70% des lignes, au hasard, qui formeront le learning set
learning_set = tab[learning_indices,]
test_set = tab[-learning_indices,]
# le reste des lignes forment la base de test

# Question 2
#-----------
#Modèle linéaire
res_learning_lm = lm('R~.',data=learning_set)
prediction_test_lm = predict.lm(res_learning_lm,test_set)

sqrt(var(res_learning_lm$res))
sqrt(var(prediction_test_lm-test_set[,1]))

#Backward
regbackward = step(res,direction='backward')
prediction_learning_backward = predict(regbackward,learning_set)
prediction_test_backward = predict(regbackward,test_set)

sqrt(var(prediction_learning_backward-learning_set[,1]))
sqrt(var(prediction_test_backward-test_set[,1]))

#Forward
#-------
regforward = step(res,direction='forward')
prediction_learning_forward = predict(regforward,learning_set)
prediction_test_forward = predict(regforward,test_set)

sqrt(var(prediction_learning_forward-learning_set[,1]))
sqrt(var(prediction_test_forward-test_set[,1]))

#Stepwise
#--------
regstepwise = step(res,direction='both')
prediction_learning_stepwise = predict(regstepwise,learning_set)
prediction_test_stepwise = predict(regstepwise,test_set)

sqrt(var(prediction_learning_stepwise-learning_set[,1]))
sqrt(var(prediction_test_stepwise-test_set[,1]))

#Lasso
#-----
X_learning_set = data.matrix(learning_set[-c(1)])
X_test_set = data.matrix(test_set[-c(1)])

Y_learning_set = learning_set$R
Y_test_set= test_set$R

reglasso = lars(X_learning_set,Y_learning_set,type="lasso")

prediction_learning_lasso = predict.lars(reglasso,X_learning_set,type='fit',mode='lambda',s=4)
prediction_test_lasso = predict.lars(reglasso,X_test_set,type='fit',mode='lambda',s=4)

sqrt(var(prediction_learning_lasso$fit-learning_set[,1]))
sqrt(var(prediction_test_lasso$fit-test_set[,1]))

#Ridge
#-----
X_learning_set_ridge = cbind(rep(1,nrow(learning_set)),learning_set[,2:14])
X_test_set_ridge = cbind(rep(1,nrow(test_set)),test_set[,2:14])

resridge = lm.ridge('R~.',data=learning_set,lambda=lambda)
which.min(resridge$GCV)
# Ainsi, les coefs du modèle sont en coef(resridge)[9,] :
Coefridge = coef(resridge)[arrayInd(which.min(resridge$GCV),1000)[1],]


Y_learning_ridge = as.matrix(X_learning_set_ridge)%*%as.vector(Coefridge)
Y_test_ridge = as.matrix(X_test_set_ridge)%*%as.vector(Coefridge)

sqrt(var(Y_learning_ridge-learning_set[,1]))
sqrt(var(Y_test_ridge-test_set[,1]))


# Question 3
#-----------
# après plusieurs simulations, on remarque que : 
# - le modèle linéaire fait clairement du surapprentissage, avec une erreur quadratique moyenne faible sur la base d'apprentissage et très élevée sur la base de test
# - dans notre cas, backward = stepwise (on le savait déjà) < forward (ces méthodes simples semblent performantes ici)
# - le lasso et ridge sont mieux que toutes les autres approches sur la base d'apprentissage, et moins bien sur la base de test (seul le modèle linéaire fait pire)
#     => peut-être qu'il existe une utilisation de ces méthodes dans un certain cadre pour construire un modèle parcimonieux sans chercher à l'utiliser ? 
#     => malgré tout, ce constat est surprenant car il devrait être révélateur d'un surapprentissage, alors que ridge et lasso sont des méthodes parcimonieuses (surtout le lasso) qui visent justement à construire des modèles plus robustes. La selection de données induite par ces méthodes peut-elle cependant conduire à un surapprentissage ?