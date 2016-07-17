#' Une fonction permettant de produire des graphiques pour le menu PDD.
#'
#' Les analyses dépendent du fait qu'il y ait une ou deux variables à analyser ainsi que du type (général) de chacune des variables à analyser.
#' @param data une data frame.
#'
#' - S'il s'agit d'une \bold{analyse monovariée} et que le type général est \bold{Qualitatif}, \bold{Echelle sémantique} ou \bold{Echelle sémantique inversée} : le nombre de lignes de \code{data} est égal au nombre de modalités de la variable à analyser et le nombre de colonne de \code{data} est égale au nombre de variables à représenter. (NB : dans ce cas \code{rownames(data)} doit contenir les noms de modalités, ces modalités étant les mêmes ou non pour toutes les variables).
#'
#' - S'il s'agit d'une \bold{analyse monovariée} et que le type général est \bold{Quantitatif}.  : le nombre de lignes de \code{data} est égal au nombre d'observations de la population à analyser et le nombre de colonne \code{data} vaut 1.
#'
#' - S'il s'agit d'une \bold{analyse bivariée} et que les deux types généraux sont \bold{Qualitatif}, \bold{Echelle sémantique} ou \bold{Echelle sémantique inversée} : le nombre de lignes de \code{data} est égal au nombre de modalités de la première variable à analyser et le nombre de colonne de \code{data} au nombre de modalités de la seconde variable à analyser. (NB : dans ce cas \code{rownames(data)} doit contenir les noms des modalités de la première variable et \code{colnames(data)} ceux de la seconde variable).
#'
#' - S'il s'agit d'une \bold{analyse bivariée} et que les deux types généraux sont \bold{Quantitatif} : le nombre de lignes de \code{data} est égal au nombre d'observation de la population à analyser et le nombre de colonnes \code{data} vaut 2.
#'
#' - S'il s'agit d'une \bold{analyse bivariée} et le premier type général est \bold{Quantitatif} et le second \bold{Qualitatif}, \bold{Echelle sémantique} ou \bold{Echelle sémantique inversée} : le nombre de lignes de \code{data} est égal au nombre d'observations de la population à analyser et le nombre de colonne \code{data} vaut 2 ; la première colonne correspondant aux données quantitatives, et la seconde aux données qualitatives (ou sémantiques).
#'
#' - S'il s'agit d'une \bold{analyse bivariée} et le premier type général est \bold{Qualitatif}, \bold{Echelle sémantique} ou \bold{Echelle sémantique inversée} et le second \bold{Quantitatif} : le nombre de lignes de \code{data} est égal au nombre d'observations de la population à analyser et le nombre de colonne \code{data} vaut 2 ; la première colonne correspondant aux données qualitatives (ou sémantiques) et la seconde aux données quantitatives.


#' @param type_general un vecteur de une ou deux chaines de caractères parmis \code{c("Quantitatif","Qualitatif","Echelle sémantique","Echelle sémantique inversée")}.
#' @param is_mono est un booléen indiquant s'il s'agit d'une (ou plusieurs) analyse(s) monovariées (\code{TRUE}) ou d'une analyse bivariée (\code{FALSE}).
#' @param weight \code{NULL} ou un vecteur de poids de longueur égale au nombre d'observation totale (utile que si au moins une des variables à analyser est \bold{Quantitatif} et que l'analyse est pondérée).
#' @param is_heatmap est un booléen indiquant, pour une annlyse bivariée \bold{Qualitatif} X \bold{Qualitatif} (ou avec \bold{Echelle sémantique} ou \bold{Echelle sémantique inversée}), si le graphique à produire doit être un heatmap (par défaut \code{is_heatmap = FALSE}). Si un heatmap est demandé, le heatmap sera calculé sur les indices en base 100 (ces indices seront calculés automatiquement).
#' @param is_indice est un booléen indiquant, si pour une analyse en heatmap les données sont déjà en indices en base 100 (par défaut non, c'est à dire que les données sont des comptages par défaut et non des indices).
#' @param title une chaine de caractère représentant le titre de l'analyse.
#' @param transpose un boléan pour transposer les analyses de variables qualitatives monovariées de deux varibales ou plus, ou bivariées.

#' @export graphpdd
#' @examples
#' ######################################
#' # Exemple 1 : mono qualitatif (une seule variable)
#' ######################################
#' data<-data.frame(c(10000,2000,888))
#' rownames(data)<-c("Un premier nom de modalité","Un deuxième nom de modalité","Un troisième nom de modalité")
#' p <- graphpdd(data = data
#'               ,lib_var = "Un libellé de variable"
#'               ,type_general = "Qualitatif"
#'               ,is_mono = TRUE
#'               ,title = "Titre du graphique")
#' print(p)
#'
#' ######################################
#' # Exemple 2 : mono qualitatif (3 variables)
#' ######################################
#' data<-data.frame(Var1=c(1000,2000,888,0),Var2=c(0,0,500,1000),Var3=c(250,350,100,0))
#' rownames(data)<-c("Un premier nom de modalité","Un deuxième nom de modalité","Un troisième nom de modalité","Un quatrième nom de modalité")
#' p <- graphpdd(data = data
#'               ,lib_var = c("Libellé de variable 1","Libellé de la variable 2","Libellé très très très très très long de la variable 3")
#'               ,type_general = c("Qualitatif","Qualitatif","Qualitatif")
#'               ,is_mono = TRUE
#'               ,title = "Titre du graphique")
#' print(p)
#'
#' ######################################
#' # Exemple 3 : mono quantitatif
#' ######################################
#' set.seed(123)
#' x<-runif(1000)
#' data<-data.frame(x=rnorm(n=1000,mean = 0,sd=1)*(x>0.5)+(x<=0.5)*rnorm(n=1000,mean=5,sd=2))
#' p <- graphpdd(data = data
#'               ,lib_var = "Un libellé de variable"
#'               ,type_general = "Quantitatif"
#'               ,is_mono=TRUE
#'               ,title = "Titre du graphique")
#' print(p)
#'
#' ######################################
#' # Exemple 4 : mono quantitatif  (3 variables)
#' ######################################
#' set.seed(123)
#' x<-runif(1000)
#' data<-data.frame(x=rnorm(n=1000,mean = 0,sd=1)*(x>0.5)+(x<=0.5)*rnorm(n=1000,mean=5,sd=2))
#' data$y <- rnorm(n=1000,mean = 100,sd=10)*(x>0.5)+(x<=0.5)*rnorm(n=1000,mean=5,sd=2)
#' data$z<-200*data$x+1000
#' p <- graphpdd(data = data
#'               ,lib_var = paste0("Un libellé de variable ",seq(3))
#'               ,type_general = rep("Quantitatif",3)
#'               ,is_mono=TRUE
#'               ,title = "Titre du graphique")
#' print(p)
#'
#' ######################################
#' # Exemple 5 : mono quantitatif avec un poids
#' ######################################
#' set.seed(123)
#' x<-runif(1000)
#' data<-data.frame(x=rnorm(n=1000,mean = 0,sd=1)*(x>0.5)+(x<=0.5)*rnorm(n=1000,mean=5,sd=2))
#' w<-runif(nrow(data))
#' p <- graphpdd(data = data
#'               ,lib_var = "Un libellé de variable"
#'               ,type_general = "Quantitatif"
#'               ,weight = w
#'               ,is_mono=TRUE
#'               ,title = "Titre du graphique PONDERE")
#' print(p)
#'
#' ######################################
#' # Exemple 6 : bivarié quali x quali (barres normalisées sur une des deux variables)
#' ######################################
#' set.seed(123)
#' a<-paste0("Le libellé de la modalité (var1) numéro ",seq(7))
#' b<-paste0("Le libellé de la modalité (var2) numéro ",seq(9))
#' data<-reshape2::dcast(data.frame(x=sample(a,1000,replace=TRUE),y=sample(b,1000,replace=TRUE)),x~y)
#' rownames(data)<-data[,1]
#' data<-data[,-1]
#' lib_var<-
#'   type_general<-c("Qualitatif","Qualitatif")
#' title="Un titre"
#'
#' p <- graphpdd(data = data
#'               ,lib_var = c("Une première variable avec un libellé super long","Une seconde variable")
#'               ,type_general = c("Qualitatif","Qualitatif")
#'               ,is_mono = FALSE
#'               ,title = "Titre du graphique")
#' print(p)
#'
#' ######################################
#' # Exemple 7 : bivarié quali x sémantiques (barres normalisées sur une des deux variables)
#' ######################################
#' set.seed(123)
#' a<-paste0("Le libellé de la modalité (var1) numéro ",seq(7))
#' b<-paste0("Le libellé de la modalité (var2) numéro ",seq(9))
#' data<-reshape2::dcast(data.frame(x=sample(a,1000,replace=TRUE),y=sample(b,1000,replace=TRUE)),x~y)
#' rownames(data)<-data[,1]
#' data<-data[,-1]
#' lib_var<-
#'   type_general<-c("Qualitatif","Qualitatif")
#' title="Un titre"
#'
#' p <- graphpdd(data = data
#'               ,lib_var = c("Une première variable avec un libellé super long","Une seconde variable")
#'               ,type_general = c("Qualitatif","Echelle sémantique")
#'               ,is_mono = FALSE
#'               ,title = "Titre du graphique")
#' print(p)
#'
#' ######################################
#' # Exemple 8 : bivarié quali x quali (heatmap)
#' ######################################
#' #' set.seed(123)
#' a<-paste0("Le libellé de la modalité (var1) numéro ",seq(7))
#' b<-paste0("Le libellé de la modalité (var2) numéro ",seq(9))
#' data<-reshape2::dcast(data.frame(x=sample(a,1000,replace=TRUE),y=sample(b,1000,replace=TRUE)),x~y)
#' rownames(data)<-data[,1]
#' data<-data[,-1]
#' lib_var<-
#'   type_general<-c("Qualitatif","Qualitatif")
#' title="Un titre"
#' p <- graphpdd(data = data
#'               ,lib_var = c("Une première variable avec un libellé super long","Une seconde variable")
#'               ,type_general = c("Qualitatif","Qualitatif")
#'               ,is_mono = FALSE
#'               ,is_heatmap = TRUE
#'               ,title = "Titre du graphique")
#' print(p)
#'
#' ######################################
#' # Exemple 9 : bivarié quanti x quanti
#' ######################################
#' set.seed(123)
#' x<-runif(10000)
#' x<-rnorm(n=10000,mean = 0,sd=1)*(x>0.5)+(x<=0.5)*rnorm(n=10000,mean=5,sd=2)
#' y<-x+0.25*x^2+2+rnorm(n=10000,mean=0,sd=3)
#' data<-data.frame(x=x,y=y)
#' p <- graphpdd(data = data
#'               ,lib_var = c("Une première variable avec un libellé super long","Une seconde variable")
#'               ,type_general = c("Quantitatif","Quantitatif")
#'               ,is_mono = FALSE
#'               ,title = "Titre du graphique")
#' print(p)
#'
#' ######################################
#' # Exemple 10 : bivarié quanti x quanti avec un poids
#' ######################################
#' set.seed(123)
#' x<-runif(10000)
#' x<-rnorm(n=10000,mean = 0,sd=1)*(x>0.5)+(x<=0.5)*rnorm(n=10000,mean=5,sd=2)
#' y<-x+0.25*x^2+2+rnorm(n=10000,mean=0,sd=3)
#' data<-data.frame(x=x,y=y)
#' w<-ifelse(data[,1]<=1,0.1,10)
#' p <- graphpdd(data = data
#'               ,lib_var = c("Une première variable avec un libellé super long","Une seconde variable")
#'               ,type_general = c("Quantitatif","Quantitatif")
#'               ,is_mono = FALSE
#'               , weight = w
#'               ,title = "Titre du graphique (pondéré)")
#' print(p)
#'
#' ######################################
#' # Exemple 11 : bivarié quanti x quali
#' ######################################
#' set.seed(123)
#' x<-runif(10000)
#' x<-rnorm(n=10000,mean = 0,sd=1)*(x>0.5)+(x<=0.5)*rnorm(n=10000,mean=5,sd=2)
#' b<-paste0("Le libellé de la modalité (var2) numéro ",seq(6))
#' y<-sample(b,10000,replace=TRUE)
#' data<-data.frame(x=x,y=y)
#'
#' p <- graphpdd(data = data
#'               ,lib_var = c("Une première variable avec un libellé super long","Une seconde variable")
#'               ,type_general = c("Quantitatif","Qualitatif")
#'               ,is_mono = FALSE
#'               ,title = "Titre du graphique")
#' print(p)
#'
#' ######################################
#' # Exemple 11 : bivarié quanti x quali
#' ######################################
#' set.seed(123)
#' x<-runif(10000)
#' x<-rnorm(n=10000,mean = 0,sd=1)*(x>0.5)+(x<=0.5)*rnorm(n=10000,mean=5,sd=2)
#' b<-paste0("Le libellé de la modalité (var2) numéro ",seq(6))
#' y<-sample(b,10000,replace=TRUE)
#' data<-data.frame(x=x,y=y)
#' set.seed(123)
#' w<-runif(nrow(data))
#' p <- graphpdd(data = data
#'               ,lib_var = c("Une première variable avec un libellé super long","Une seconde variable")
#'               ,type_general = c("Quantitatif","Qualitatif")
#'               ,is_mono = FALSE
#'               ,weight = w
#'               ,title = "Titre du graphique")
#' print(p)
#' # On peut inverser les données (quanti <-> quali), cela ne change rien
#' p <- graphpdd(data = data[,2:1]
#'               ,lib_var = c("Une première variable avec un libellé super long","Une seconde variable")
#'               ,type_general = c("Qualitatif","Quantitatif")
#'               ,is_mono = FALSE
#'               ,title = "Titre du graphique (2)")
#' print(p)



graphpdd <- function(data, type_general, is_mono = TRUE,lib_var, weight = NULL, is_heatmap = FALSE, is_indice = FALSE, title = "", transpose = FALSE, nr1 = NULL, angle = NULL) {
  out <- NULL

  if(is_mono){
    if(all(type_general %in% c("Qualitatif","Echelle sémantique","Echelle sémantique inversée"))){
      out <- graphpdd_mono_quali(data=data, lib_var=lib_var, title = title, transpose = transpose, nr1 = nr1, angle = angle)
    } else if(all(type_general=="Quantitatif")) {
      out <- graphpdd_mono_quanti(data=data, lib_var=lib_var,weight = weight, title = title)
    }

  } else if(!is_mono & length(type_general)==2){
    if(all(type_general %in%  c("Qualitatif","Echelle sémantique","Echelle sémantique inversée"))){
      out <- graphpdd_bi_quali_quali(data=data, lib_var=lib_var, type_general=type_general, is_heatmap = is_heatmap, is_indice = is_indice, title = title, transpose = transpose, nr1 = nr1, angle = angle)
    } else if(all(type_general %in%  c("Quantitatif"))){
      out <- graphpdd_bi_quanti(data = data, lib_var = lib_var, title = title, w = weight)
    } else if(!all(type_general %in%  c("Quantitatif")) & "Quantitatif" %in% type_general){
      out <- graphpdd_bi_quanti_quali(data=data, lib_var=lib_var, type_general=type_general, title = title, weight = weight)
    }

  }
  return(out)

}

graphpdd_mono_quali <- function(data, lib_var, title = "", transpose = FALSE, nr1 = NULL, angle = NULL, ...){
  library(ggplot2)
  library(ggthemes)
  library(scales)
  library(stringr)
  library(reshape2)
  for(k in seq(ncol(data))){
    data[,k] <- as.numeric(as.character(data[,k]))
    data[,k]<-ifelse(is.na(data[,k]),0,data[,k])
  }

  if(transpose){
    data <- t(data)
    rownames(data) <- lib_var
    lib_var <- colnames(data)

  }
  lib_var <- str_wrap(lib_var,20)
  colnames(data)<-lib_var
  title <- str_wrap(title,20)
  if(nrow(data)<=5){
    if(is.null(nr1))nr1<-15
    if(is.null(angle))angle<-0
  } else if(nrow(data)>=25){
    if(is.null(nr1))nr1<-35
      if(is.null(angle))angle<-50
  } else {
    if(is.null(nr1)) nr1 <- round(15+1*(nrow(data)-5))
    if(is.null(angle))angle <- round(0+2.5*(nrow(data)-5))

    }

  rownames(data) <- str_wrap(rownames(data) ,nr1)
  x <- data.frame(x=rownames(data),data,check.names = FALSE,stringsAsFactors = FALSE)
  x <- melt(x,id.vars="x",variable.name ="Variable")
  x$x<-factor(x$x,levels=rownames(data))
  x$Variable<-factor(x$Variable,levels=lib_var)

  if(length(lib_var)==1){
    .e <- environment()
    p<-ggplot(data=x,aes(x=x,y=value)) +
      geom_bar(stat = "identity",fill=gdocs_pal()(1),alpha=0.85) +
      scale_y_continuous(labels = function(x,...)format(x, ..., big.mark = " ", scientific = FALSE, trim = TRUE))+
      xlab(lib_var) +ggtitle(title)
    p<- p+theme_pander()

  } else {

    colourCount = length(lib_var)
    g0<-gdocs_pal()(min(20,colourCount))
    getPalette = colorRampPalette(g0)
    pal<- scale_fill_manual(name= if(transpose) "Modalités" else "Variable",values = getPalette(colourCount),guide=guide_legend(reverse=FALSE))

    .e <- environment()
    p<-ggplot(data=x,aes(x=x,y=value,fill=Variable)) +
      geom_bar(stat = "identity",alpha=0.85, position=position_dodge()) +
      scale_y_continuous(labels = function(x,...)format(x, ..., big.mark = " ", scientific = FALSE, trim = TRUE))+
      xlab( if(transpose) "Variable" else "Modalités") +ggtitle(title)
    p<- p+theme_pander()+pal+ theme(legend.position="bottom")
  }
p<-p+ylab("Fréquence")
p<-p+theme(axis.text.x = element_text(angle = angle, hjust = 1))
  p$plot_env<-.e
  return(p)
}

graphpdd_bi_quali_quali <- function(data, lib_var, type_general, title = "", is_heatmap = FALSE, is_indice = FALSE, transpose = FALSE, nr1 = NULL, angle = NULL,...){
  # save(file="dom",list=ls())
  # load("dom")
  # print("++")
  library(ggplot2)
  library(ggthemes)
  library(scales)
  library(stringr)
  library(reshape2)
  library(dplyr)
  library(RColorBrewer)
  for(k in seq(ncol(data))){
    data[,k] <- as.numeric(as.character(data[,k]))
    data[,k]<-ifelse(is.na(data[,k]),0,data[,k])
  }

  if(transpose){
    data <- t(data)
    lib_var<-rev(lib_var)


  }

  # lib_var <- str_wrap(lib_var,20)
  # colnames(data)<-lib_var
  # title <- str_wrap(title,20)

  if(ncol(data)<=5){
    if(is.null(nr1))nr1<-15
    if(is.null(angle))angle<-0
  } else if(ncol(data)>=25){
    if(is.null(nr1))nr1<-35
    if(is.null(angle))angle<-50
  } else {
    if(is.null(nr1))nr1 <- round(15+1*(ncol(data)-5))
    if(is.null(angle))angle <- round(0+2.5*(ncol(data)-5))

  }


  lib_var[1] <- str_wrap(lib_var[1],40)
  lib_var[2] <- str_wrap(lib_var[2],15)
  title <- str_wrap(title,40)
  rownames(data) <- str_wrap(rownames(data) ,20)
  colnames(data) <- str_wrap(colnames(data) ,nr1)

  if(!is_heatmap){
    x <- data.frame(xxxr_var1=rownames(data),data,check.names = FALSE,stringsAsFactors = FALSE,row.names = NULL)
    x<-reshape2::melt(x,id.vars=c("xxxr_var1"),variable.name="xxxr_var2")
    x$xxxr_var1<-factor(x$xxxr_var1,levels=rownames(data))
    x$xxxr_var2<-factor(x$xxxr_var2,levels=colnames(data))
    y<-x%>%group_by(xxxr_var1)%>%summarise(s=sum(value))%>%as.data.frame
    x<-left_join(x,y,by="xxxr_var1")%>%mutate(value=value/s)%>%as.data.frame%>%select(-s)

# print(head(x))
    colourCount = length(unique(x[,2]))
    # getPalette = colorRampPalette(brewer.pal(9, "Set1"))
    g0<-gdocs_pal()(min(20,colourCount))
    if(type_general[2]=="Echelle sémantique"){
      g0<-(tableau_div_gradient_pal(palette = "Red-Green", space = "Lab")(seq(0,1,length.out=colourCount)))
    } else if(type_general[2]=="Echelle sémantique inversée"){
      g0<-(tableau_div_gradient_pal(palette = "Red-Green", space = "Lab")(seq(1,0,length.out=colourCount)))
    }
    getPalette = colorRampPalette(g0)
    pal<- scale_fill_manual(name=lib_var[2],values = getPalette(colourCount),guide=guide_legend(reverse=TRUE))
    #
    .e <- environment()
    p<-ggplot(data=x,aes(x=xxxr_var1 ,y=value,fill=xxxr_var2)) +
      geom_bar(stat="identity",alpha=0.75) +
      scale_y_continuous(labels = percent)+
      xlab(lib_var[1])+ylab("Répartition")+
      pal+
      ggtitle(title)
    p<- p+theme_pander()+theme(axis.text.x = element_text(angle = angle, hjust = 1))
    p$plot_env<-.e
  } else {
    x <- data.frame(xxxr_var1=rownames(data),data,check.names = FALSE,stringsAsFactors = FALSE,row.names = NULL)
    x<-reshape2::melt(x,id.vars=c("xxxr_var1"),variable.name="xxxr_var2")
    head(x)
    x$xxxr_var1<-factor(x$xxxr_var1,levels=rownames(data))
    x$xxxr_var2<-factor(x$xxxr_var2,levels=colnames(data))


    if(!is_indice){
      y1<-x%>%group_by(xxxr_var1)%>%summarise(s1=mean(value))%>%as.data.frame
      y2<-x%>%group_by(xxxr_var2)%>%summarise(s2=mean(value))%>%as.data.frame
      s<-mean(x$value)
    x<-x%>%left_join(y1,by="xxxr_var1")%>%
      left_join(y2,by="xxxr_var2")%>%
      mutate(`Indice\n(base 100)`=100*value/(s1*s2)*s)%>%as.data.frame%>%select(-s1,-s2)
    } else {
      x<-x%>%mutate(`Indice\n(base 100)`=value)
    }
    c<-tableau_div_gradient_pal(palette = "Red-Green", space = "Lab")(seq(0,1,length.out=21))
    c<-c(c[1],"white",c[21])
    .e <- environment()
    p<-ggplot(x, aes(xxxr_var1, xxxr_var2)) +
      geom_raster(aes(fill=`Indice\n(base 100)`,color="white"), interpolate = FALSE,alpha=0.85) +
      scale_fill_gradientn(colours=c
                           ,values=rescale(c(min(x$`Indice\n(base 100)`),100,max(x$`Indice\n(base 100)`)))
      )+   xlab(lib_var[1])+ylab(lib_var[2])+
      ggtitle(title)
    p<- p+theme_pander()+theme(axis.text.x = element_text(angle = angle, hjust = 1))
    p$plot_env<-.e


  }
  return(p)
}

graphpdd_mono_quanti <- function(data, lib_var, weight = NULL, title = "", ...){

  library(ggplot2)
  library(ggthemes)
  library(scales)
  library(stringr)
  library(reshape2)
  library(dplyr)
  if(nrow(na.omit(data))==0)return(NULL)
  lib_var <- str_wrap(lib_var,40)
  colnames(data)<-lib_var
  title <- str_wrap(title,40)
  if(is.null(weight))weight<-rep(1,nrow(data))
  data$weight_variable_<-weight
  x <- melt(data,id.vars="weight_variable_",variable.name = "Variable")
  x$Variable<-factor(x$Variable,levels=lib_var)

  if(length(lib_var)==1){
    e<-hist(x$value,plot=FALSE)
    .e <- environment()
    p<-ggplot(data=x,aes(x=value)) +
      geom_histogram(aes(weight=weight_variable_), bins =  length(e$breaks)-1 ,center=mean(e$breaks[seq(min(2,length(e$breaks)))])
                     ,fill=gdocs_pal()(1),alpha=0.85,color="white",size=0.8)+
      scale_y_continuous(labels = function(x,...)format(x, ..., big.mark = " ", scientific = FALSE, trim = TRUE))+
      scale_x_continuous(labels = function(x,...)format(x, ..., big.mark = " ", scientific = FALSE, trim = TRUE))+
      xlab(lib_var)+ylab("Fréquence")+
      ggtitle(title)
    p<- p+theme_pander()
    p$plot_env<-.e
  } else {
    x <- x %>%
      group_by(Variable) %>%
      mutate(
        bins =length(hist(value,plot=FALSE)$breaks)-1
        , center = mean(hist(value,plot=FALSE)$breaks[seq(min(2,length(hist(value,plot=FALSE)$breaks)))])
        ,width = diff(range(value))/bins
      )
    colourCount = length(lib_var)
    g0<-gdocs_pal()(min(20,colourCount))
    getPalette = colorRampPalette(g0)
    pal<- scale_fill_manual(name="Variables",values = getPalette(colourCount),guide=guide_legend(reverse=FALSE))


    p<-ggplot(data=x,aes(x=value,fill=Variable))
    for( t in sort(unique(x$Variable))){
      xx<-subset(x,Variable==t)
      a=xx$bins[1]
      b=xx$center[1]
      p<-p+geom_histogram(data=xx,aes(weight=weight_variable_),bins=a,center=b
                          ,alpha=0.85,color="white",size=0.8)
    }
    p<-p+scale_y_continuous(labels = function(x,...)format(x, ..., big.mark = " ", scientific = FALSE, trim = TRUE))+
      scale_x_continuous(labels = function(x,...)format(x, ..., big.mark = " ", scientific = FALSE, trim = TRUE))+
      xlab("Valeurs")+ylab("Fréquence")+
      ggtitle(title)
    p<- p+theme_pander()+  facet_grid(Variable~., scales = "free")
    p <- p+ pal+ facet_wrap(~Variable, scales = "free",ncol=1)+guides(fill=FALSE)
    .e <- environment()
     p$plot_env<-.e
  }


  p
  return(p)
}




graphpdd_bi_quanti <- function(data, lib_var, title = "", w = NULL, ...){
  library(ggplot2)
  library(ggthemes)
  library(scales)
  library(stringr)
  if(nrow(na.omit(data))==0)return(NULL)
  lib_var <- str_wrap(lib_var,40)
  title <- str_wrap(title,40)

  x <- data.frame(x=data[,1],y=data[,2],check.names = FALSE,stringsAsFactors = FALSE)

  n<-function(x,a=0,b=1){
    if(sd(x)==0)return(rep((a+b)/2,length(x)))
    x<-x-min(x)
    a+(b-a)*x/max(x)
  }
  if(is.null(w))w<-rep(1,nrow(data))
  .e <- environment()
  p<-ggplot(data=x,aes(x=x,y=y)) +
    geom_point(color=gdocs_pal()(1),alpha=n(w,0.05,0.85),size=1.5)+
    scale_y_continuous(labels = function(x,...)format(x, ..., big.mark = " ", scientific = FALSE, trim = TRUE))+
    scale_x_continuous(labels = function(x,...)format(x, ..., big.mark = " ", scientific = FALSE, trim = TRUE))+
    xlab(lib_var[1])+ylab(lib_var[2])+
    ggtitle(title)
  p<- p+theme_pander()
  p<-p+ theme(panel.border = element_blank(),
              axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
              axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))
  p$plot_env<-.e
  return(p)
}




graphpdd_bi_quanti_quali <- function(data, lib_var, type_general, title = "", weight = NULL, ...){
  if(type_general[2]=="Quantitatif"){
    return(graphpdd_bi_quanti_quali(data[,2:1], lib_var[2:1], type_general[2:1], title = title, weight=weight))
  } else {
    library(ggplot2)
    library(ggthemes)
    library(scales)
    library(stringr)
    lib_var <- str_wrap(lib_var,40)
    title <- str_wrap(title,40)
    n<-function(x,a=0,b=1){
      if(sd(x)==0)return(rep((a+b)/2,length(x)))
      x<-x-min(x)
      a+(b-a)*x/max(x)
    }
    if(is.null(weight))weight<-rep(1,nrow(data))

    x <- data.frame(x=data[,1],y=data[,2],weight=weight,check.names = FALSE,stringsAsFactors = FALSE)
    x[,2]<-factor(x[,2])
    levels(x[,2]) <- str_wrap(levels(x[,2]) ,10)
    e<-hist(x[,1],plot=FALSE)


    colourCount = length(unique(x$y))
    g0<-gdocs_pal()(min(20,colourCount))
    getPalette = colorRampPalette(g0)
    pal<- scale_fill_manual(name="Variables",values = getPalette(colourCount),guide=guide_legend(reverse=FALSE))

    .e <- environment()

    p<-ggplot(data=x,aes(x=x,fill=y)) +
      geom_histogram(aes(weight = weight), bins =  length(e$breaks)-1 ,center=mean(e$breaks[seq(min(2,length(e$breaks)))])
                     ,alpha=0.85,color="white",size=0.8)+
      scale_y_continuous(labels = function(x,...)format(x, ..., big.mark = " ", scientific = FALSE, trim = TRUE))+
      scale_x_continuous(labels = function(x,...)format(x, ..., big.mark = " ", scientific = FALSE, trim = TRUE))+
      xlab(lib_var)+ylab("Fréquence")+
      ggtitle(title)
    p<- p+theme_pander()+pal+guides(fill=FALSE)
    p<-p+facet_grid(y~.)
    p$plot_env<-.e
    return(p)
  }
}
