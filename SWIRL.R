install.packages("swirl")
packageVersion("swirl")
library(swirl)
install_course("The R Programming Environment")
swirl() # Your name and select course
Yosri
1
1
9
getwd()
ls()
x <- 9
ls()
dir()
?list.files
args(list.files)
old.dir<-getwd()
dir.create("testdir")
setwd("testdir")
file.create("mytest.R")
dir("mytes.R")
list.files()
dir()
file.exists("mytest.R")
file.info("mytest.R")
file.rename("mytest.R","mytest2.R")
filw.remove("mytest.R")
file.copy("mytest2.R","mytest3.R")
file.path("mytest3.R")
file.path("folder1","folder2")
?dir.create
dir.create(file.path("testdir2","testdir3"),recursive=TRUE)
setwd(old.dir)
3
10
1
library(readr)
print(datapath)
datafile <- file.path(datapath, "urban.csv.gz")
urban <- read_csv(datafile)# on l a mis dans une nouvelle trame de données
head(urban)# il nous montre
urban <- read_csv(datafile, col_types = "cccdc")#on a utilisé l argument pour rendre une colonne en caractére
urban <- read_csv(datafile, col_types = "cccd-")# on a utilisé "-" pour sauter la colonne 
head(urban)
urban <- read_csv(datafile, col_types = "cccd-", n_max = 100)#on a mis un maximum avec nmax
11#quand on travail avec une nouvelle base de donnée il faut voir le format les dimensions les noms des variables comment ils sont stocker ou si il ya des données manquantes
ls()#pour voir les variables dans´l'espace de travail
class(plants)#pour connaitre la classe de la variable( si elle est stocké dans une data frame donc elle a deux dimensions colonne et lignes )
dim(plants)#pour voir nombre de lignes et colonnes
nrow(plants)#pour voir le Nombres de lignes
ncol(plants)#pour voir le nombre de colonne
object.size(plants)#pour voir l'occupation en mémoires
names(plants)#nous donne un vecteur "caractére" des noms des colonnes 
head(plants)#nous donne un apercu sur la base de donnée(par defaut 6 lignes)
head(plants,10)#on a ajouté le nombre de lignes souhaité qui est 10 dans ce cas
tail(plants)#pour voir les  derniéres lignes 
tail(plants,15)#pour voir les 15 derniéres lignes
summary(plants)# pour connaitre les variabes,leurs distribution et voir meilleur la base de donnée (si il y a des donnés NA )
table(plants$Active_Growth_Period)#pour voir combien de fois chaque valeur se produit
str(plants)# nous combine et nous donne les donnés sur les variables de la BD
12
1
