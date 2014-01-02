############################################
# TP 3 - Jérôme CRÉTIEN - Florent RENUCCI  #
#   11 Décembre - pour le 11 Janvier 2013  #
############################################


######################
##### Exercice 1 #####
######################

##### Partie A #####
rm(list=ls())
epsilon = 10^-15
n=50
p=5
X=matrix(rnorm(p*n),n);
S=cov(X);

# S est symétrique (c'est une matrice de covariance) définie positive (car les variables sont iid).

U = svd(S)$u 
V = svd(S)$v
sigma = diag(svd(S)$d)

# U contient les vecteurs d'analyse, de R^5
# V contient les vecteurs de sortie, de R^5
# sigma contient les valeurs singulières de S sur sa diagonale
# il s'agit d'une diagonalisation "généralisée", c'est à dire applicable aussi à des matrices non carrées (en l'occurence, la matrice S est carrée)

# sigma contient les valeurs singulières (en l'occurence les valeurs propres, puisque S est carrée) de S

U-V<epsilon
# U et V sont égales à 10^-15 près. C'est normal : S est symétrique définie positive, donc orthodiagonalisable, de matrice de passage U, donc t(V) = U^-1 = t(U) ie U=V
norm(U-V)<epsilon
norm(U-V,"f")<epsilon

sum(diag(S))-sum(diag(sigma))<epsilon
# la trace est conservée par changement de base, donc trace(S)=trace(sigma)

# on vérifie que :
U%*%sigma%*%t(V)-S<epsilon
# donc U sigma t(V) = S


##### Partie B #####
rm(list=ls())
epsilon = 10^-15
p=5

n=10
X10=matrix(rnorm(p*n),n);
S10=cov(X10);
x11();image(1:p,1:p,S10,col=gray(0:10/10),xlab='',ylab='')
title(sprintf('Covariance matrix n %d, p%d',n,p))
U10 = svd(S10)$u 
V10 = svd(S10)$v
sigma10 = diag(svd(S10)$d)

n=100
X100=matrix(rnorm(p*n),n);
S100=cov(X100);
x11();image(1:p,1:p,S100,col=gray(0:10/10),xlab='',ylab='')
title(sprintf('Covariance matrix n %d, p%d',n,p))
U100 = svd(S100)$u 
V100 = svd(S100)$v
sigma100 = diag(svd(S100)$d)

n=1000
X1000=matrix(rnorm(p*n),n);
S1000=cov(X1000);
x11();image(1:p,1:p,S1000,col=gray(0:10/10),xlab='',ylab='')
title(sprintf('Covariance matrix n %d, p%d',n,p))
U1000 = svd(S1000)$u 
V1000 = svd(S1000)$v
sigma1000 = diag(svd(S1000)$d)

# Lorsque n-> infini, la matrice de covariance tend vers l'identité. En effet, l'estimateur de la matrice de covariance (S) tend vers la vraie matrice de covariance (par la Loi des Grands Nombres). Comme les variables aléatoires sont indépendantes, la vraie matrice est l'identité. 

barplot(sigma10)
barplot(sigma100)
barplot(sigma1000)

# les matrices sigma convergent vers l'identité. En effet, si S converge vers l'identité, ses valeurs propres (ou ses valeurs singulières, dans un cas plus général) convergent aussi vers l'identité (on peut s'en convaincre grâce à la norme spectrale).
# La convergence n'est pas très franche pour les valeurs propres. On pourrait essayer des valeurs de n plus grandes (10000 par exemple).


### loi uniforme ###
p=5
n=1000
X=matrix(runif(n*p,0,12),n)

# variance de la loi uniforme = (b-a)^2/12. Ici on a a=0, b=12, donc var = 12. On s'attend à trouver environ S = 12 Id.

S=cov(X)
# et c'est bien le cas. L'écart à 12 Id est dû au fait que S est un estimateur de la matrice de covariance, et non la vraie matrice.
U = svd(S)$u 
V = svd(S)$v
sigma = diag(svd(S)$d)


##### Partie C #####
rm(list=ls())
epsilon = 10^-15
n=50
p=5
X=matrix(rnorm(p*n),n);
S=cov(X);

Z = cbind(X,X)

S = cov(Z)
# S est toujours symétrique
# Par contre, les lignes 5+i et i sont identiques (pour i = 1..5)
# Et les colonnes 5+i et i sont identiques (pour i = 1..5)
# C'était attendu, puisque Z = [X,X] => donc S est de rang 5.

U = svd(S)$u
V = svd(S)$v
sigma = diag(svd(S)$d)

barplot(sigma)
#barplot(svd(S)$d)	les deux donnent le même affichage : R comprend qu'il doit afficher la diagonale de la matrice passée en argument à barplot.

# On a 5 valeurs propres non nulles et 5 valeurs propres nulles, ce qu'on peut vérifier ainsi :
sigma < epsilon
# Ceci était attendu, puisque la matrice X est de rang plein (=5 ici). Ainsi, la matrice Z = [X,X] est également de rang 5, et sa matrice de covariance S également.


##### Partie D #####
rm(list=ls())
epsilon = 10^-15
n=20
p=20
X=matrix(rnorm(p*n),n);
S=cov(X);

U = svd(S)$u 
V = svd(S)$v
sigma = diag(svd(S)$d)

barplot(sigma)

# On n'observe que 9 valeurs singulières non nulles, ce qu'on peut vérifier ainsi :
sigma < epsilon
# A priori, on aurait penser en observer 10, la matrice X étant de rang 10 (du moins théoriquement).
# En pratique, le nombre d'observation n'est pas suffisant (n<p) pour que l'indépendance des colonnes de X apparaisse clairement. En d'autres termes, on le faible nombre d'observation fait que l'on peut observer des relations linéaires entre les colonnes qui n'existent pas théoriquement.


######################
##### Exercice 2 #####
######################

##### Analyse préliminaire #####


X = read.table("~/Desktop/cardata.txt",sep=";",header=TRUE,row.names=1)
# il y a des headers qui représentent les noms des colonnes, et toutes les informations sont séparées par des ";".

plot(X)
# Chaque point représente un modèle de voiture, et les axes représentent les propriétés indiquées. Cela permet de vérifier les dépendances entre les variables.

cor(X)
# On remarque que : 
# - la cylindrée est fortement corrélée à la puissance, le poids et la longueur
# - la puissance est fortement corrélée à la cylindrée et la vitesse
# - la vitesse est fortement corrélée à la puissance
# - le poids est fortement corrélé à la cylindrée et la longueur
# - la longueur est fortement corrélée à la cylindrée, au poids, et à la largeur
# - la largeur est corrélée à la longueur. 

# Il est intéressant de constater qu'une variable peut être fortement corrélée à 2 autres, et les 2 autres faiblement corrélées entre elles (mais toujours de même signe). La vision matricielle est donc indispensable pour capturer toutes les dépendances. 

#### ACP ####

for (k in 1:6) {X[,k]=(X[,k]-mean(X[,k]))/sqrt(var(X[,k]));}

# X est maintenant centrée réduite (on a retranché la moyenne de chaque ligne et divisé chaque ligne par son écart-type), et on l'a appelé res.

res=prcomp(X, center=TRUE,scale.=TRUE)

# center = TRUE signifie que les données ont déjà été centrées, scale.= TRUE signifie qu'elles ont déjà été réduites.

res$sdev
# renvoie les valeurs propres de res, par ordre décroissant

res$rotation
# renvoie la matrice des vecteurs propres (matrice de passage). Les axes sont les PC_i. Les variances associés à chaque axe est lamba_i^2, et l'information capturée est la somme des lambda_i^2.
# renvoie les coordonnées des vecteurs propres dans l'ancienne base. En particulier, PC_1 est un vecteur plus ou moins proportionnel de (1 ... 1), il sépare les variables selon le fait qu'elles ont des grandes coordonnées ou non : on appelle cela l'effet taille. 

res$center
# renvoie le centre de l'espace sur lequel on projette. Ici c'est 0 parce qu'on a centré les données. 

res$scale
# donne la variance des variables (elles ont été réduites, donc c'est 1 ici)

res$x 
# donne les coordonnées des objets de res dans le nouvel espace formé par vect(PC_i).

valpr=res$sdev

### étude des valeurs propres ###

barplot(valpr)

# la variance expliquée par chaque axe principal est : 
valpr^2/6

(valpr[1]^2+valpr[2]^2)/sum(valpr^2)
# par exemple l'information capturée sur le plan factoriel PC1 PC2 représente 92.9% des informations de départ. 

(valpr[1]^2+valpr[2]^2+valpr[3]^2)/sum(valpr^2)
# on explique 96.8% de la variance, donc il faut 3 axes pour capturer 95% de la variance

barplot(cumsum(valpr^2/6))
# ça se voit bien sur ce graphique

(valpr[1]^2+valpr[2]^2+valpr[3]^2+valpr[4]^2)/sum(valpr^2)
# on explique 98.6% de la variance


### étude des vecteurs propres ###

res$rotation
# - comme expliqué auparavant, PC1 capture l'effet taille
# - PC2 discrimine les voitures rapides et puissantes des autres (effet sportif)
# - PC3 discrimine les voitures larges, mais peu cylindrées et donc peu lourdes des autres
# - PC4 discrimine les voitures puissantes et petites des autres

### Analyse des individus projetés et cercle des corrélations ###

biplot(res,choices=c(1,2))
biplot(res,choices=c(2,3))

# on observe bien une maximisation de la variance, avec un bon "éclatement" dans les 2 plans.

library(ade4)
# D=dudi.pca(X)
s.corcircle(res$rotation[,1:2]);
s.corcircle(res$rotation[,3:4]);

# Classification non-supervisée (K-means)
kmeans = kmeans(X,1)
kmeans = kmeans(X,2)
kmeans = kmeans(X,3)
kmeans = kmeans(X,4)
# centers donne les centres des clusters
# cluster donne les étiquettes des données (pour savoir à quel cluster appartient chaque observation)
# withinss donne la variance intra, de chaque cluster
# betweenss donne la variance inter, entre clusters

plot((kmeans$centers %*% res$rotation)[,1:2])
# les centres sont assez éloignés, dans le 1er plan factoriel. S'ils étaient trop proches, cela signifierait que l'on a choisi trop de clusters. En pratique, le choix du "k" de l'algorithme k-means est difficile, on peut par exemple utiliser le critère ICL, qui pénalise le nombre de clusters (parce que sinon dans l'absolu, rien ne m'empêche d'avoir autant de clusters que d'observations, et dans ce cas je surapprends).





######################
##### Exercice 3 #####
######################
rm(list=ls())

##### Partie A #####

digits=load("digits3-8.rdata")

d3 = data.frame(d3)
d8 = data.frame(d8)

mImage <- function(vImage){
	tableau = t(matrix(as.matrix(vImage),16,16))
	image(tableau,axes=F,col=gray(0:255/255))
	# après quelques essais, il semblerait qu'il faille transposer les matrices pour obtenir des chiffres "à l'endroit".
}

mImage(d3[1,])
mImage(d3[2,])
mImage(d3[3,])
mImage(d3[4,])
mImage(d8[1,])
mImage(d8[2,])
mImage(d8[3,])
mImage(d8[4,])

##### Partie B #####

d3index = sample(nrow(d3),dim(d3)[1]-100)
d8index = sample(nrow(d8),dim(d3)[1]-100)

d3train = data.frame(d3[d3index,])
d3test = data.frame(d3[-d3index,])
d8train = data.frame(d8[d8index,])
d8test = data.frame(d8[-d8index,])

mean_3 = colMeans(d3train)
mean_8 = colMeans(d8train)
# on calcule le 3 et le 8 moyens

mImage(mean_3)
mImage(mean_8)
# on les affiche

d3train_sd = (d3train-colMeans(d3train))/sapply(d3train,sd)
d8train_sd = (d8train-colMeans(d8train))/sapply(d8train,sd)
# les données sont centrées réduites

cov3 = cov(d3train_sd)
cov8 = cov(d8train_sd)
# on calcule les matrices de covariance

vp3=eigen(cov3)
vp8=eigen(cov8)

# Si X est la matrice de design centrée réduite, cov(X)=tX.X
# d'où : valeurs propres de cov(X) = valeurs singulières de X, associées aux mêmes vecteurs


modes3=vp3$vectors;
modes8=vp8$vectors;
# sont les modes propres pour 3 et 8

mImage(modes3[1,])
mImage(modes3[2,])
mImage(modes8[1,])
mImage(modes8[123,])

# on constate que plus l'ordre augmente, moins l'image obtenue ressemble à l'image moyenne, car on capture moins d'information utile au n+1 ième vecteur propre qu'au n ième.

space3 = modes3[1:5,];
proj3 = t(space3)%*%space3;

space8 = modes8[1:5,];
proj8 = t(space8)%*%space8;

# Ce sont bien des projecteurs (tels que p^2=p) :

norm(proj8%*%proj8-proj8)
norm(proj3%*%proj3-proj3)

# ces normes sont bien proches de 0

# pour compresser une image, il suffit de la projeter sur son espace engendré par les i (disons i=5) composantes principales. On peut augmenter i,  on gardera plus d'information, mais la compression prendra évidemment plus de place.
# avec i=5 par exemple, on passe d'une représentation sur 256 variables à une représentation à 5 variables


# pour trouver cet espace, il faut savoir quel est ce nombre. Supposons que l'on dispose de la même base de données, pour tous les chiffres. Par un algorithme de k-means (où k=10), on connait les endroits de l'espace des features où l'on a plus de chances de trouver un 1 qu'un 2 etc... Donc en fonction de l'endroit où tombe le nouveau point (ie l'image que l'on souhaite étiqueter), on peut prédire son label n (par vote majoritaire par exemple).

# une fois que l'on connait n, on zippe l'image (si par exemple n=3) : 

zip = function(im){
	image = ((im-mean_3)/norm(as.matrix(im)))
	projection = space3%*%image
	return(projection)
}

# pour la dézipper c'est exactement pareil, on considère l'image compressée, on observe le point qui la représente, on prédit son label, et on la dézippe (si par exemple n=3 ou 8) : 

unzip =function(imzip,n){
	
	if (n==3) {
		mean=mean_3
		space=space3
	}
	else if (n==8) {
		mean=mean_8
		space=space8
	}
	
	# etc pour les autres chiffres...
	
	dim=dim(as.matrix(mean))
	d=dim[1]
	
	for(i in 1:5){
		im = imzip[i]*space3[i,]
	}
	return(d*im+mean)
}

# on remarque que :
test = d3[1,]
mImage(test)

# et
mImage(unzip(zip(t(test)),3))

# se ressemblent. Donc l'image avant et après compression se ressemblent (comme on n'a conservé que 5 composantes pendant la compression, l'image décompressée est quand même très proche de l'image moyenne)


# on bruite une image au hasard, en lui retirant des pixels au hasard

test=d3[sample(1100,1),]
index = sample(ncol(as.matrix(test)),30)
bruitee=test


k=1
while (k<=nrow(as.matrix(test))) {
	bruitee[index[k]]=0
	k=k+1
}

mImage(bruitee)

mImage(unzip(zip(t(bruitee)),3))

# on récupère bien un 3.


# On applique l'algorithme expliqué précédemment, avec seulement 3 et 8 : k-means (ici k=2), on sait donc suivant où tombe le nouveau point (l'image) s'il est plus probable que ce soit un 3 ou un 8. C'est plus efficace qu'un simple calcul de distance aux centroïdes, car la dispersion des points peut être "écrasée", de la force d'une ellipse non circulaire (dans le cas dimension 2). Si c'est le cas, en calculant seulement la distance euclidienne, on pénaliserait à tort une dimension par rapport à l'autre. Ce qui ne se produit pas si l'on utilise l'algorithme k-means. 