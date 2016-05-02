#' Une fonction permettant de produire des graphiques pour le menu PDD.
#'
#' Les analyses dépendent du fait qu'il y ait une ou deux variables à analyser ainsi que du type (général) de chacune des variables à analyser.
#' @param data une data frame.
#'
#' - S'il s'agit d'une \bold{analyse monovariée} et que le type général est \bold{Qualitatif}, \bold{Echelle sémantique} ou \bold{Echelle sémantique inversée} : le nombre de lignes de \code{data} est égal au nombre de modalités de la variable à analyser et le nombre de colonne de \code{data} vaut 1. (NB : dans ce cas \code{rownames(data)} doit contenir les noms de modalités).
#'
#' - S'il s'agit d'une \bold{analyse monovariée} et que le type général est \bold{Quantitatif}.  : le nombre de lignes de \code{data} est égal au nombre d'observations de la population à analyser et le nombre de colonne \code{data} vaut 1.
#'
#' - S'il s'agit d'une \bold{analyse bivariée} et que les deux types généraux sont \bold{Qualitatif}, \bold{Echelle sémantique} ou \bold{Echelle sémantique inversée} : le nombre de lignes de \code{data} est égal au nombre de modalités de la première variable à analyser et le nombre de colonne \code{data} au nombre de modalités de la seconde variable à analyser. (NB : dans ce cas \code{rownames(data)} doit contenir les noms des modalités de la première variable et \code{colnames(data)} ceux de la seconde variable).
#'
#' - S'il s'agit d'une \bold{analyse bivariée} et que les deux types généraux sont \bold{Quantitatif} : le nombre de lignes de \code{data} est égal au nombre d'observation de la population à analyser et le nombre de colonne \code{data} vaut 2.
#'
#' - S'il s'agit d'une \bold{analyse bivariée} et le premier type général est \bold{Quantitatif} et le second \bold{Qualitatif}, \bold{Echelle sémantique} ou \bold{Echelle sémantique inversée} : le nombre de lignes de \code{data} est égal au nombre d'observations de la population à analyser et le nombre de colonne \code{data} vaut 2 ; la première colonne correspondant aux données quantitatives, et la seconde aux données qualitatives (ou sémantiques).
#'
#' - S'il s'agit d'une \bold{analyse bivariée} et le premier type général est \bold{Qualitatif}, \bold{Echelle sémantique} ou \bold{Echelle sémantique inversée} et le second \bold{Quantitatif} : le nombre de lignes de \code{data} est égal au nombre d'observations de la population à analyser et le nombre de colonne \code{data} vaut 2 ; la première colonne correspondant aux données qualitatives (ou sémantiques) et la seconde aux données quantitatives.


#' @param type_general un vecteur de une ou deux chaines de caractères parmis \code{c("Quantitatif","Qualitatif","Echelle sémantique","Echelle sémantique inversée")}.
#' @param lib_var un vecteur de une ou deux chaines de caractères indiquant le ou les libellés de la (des) variable(s) à représenter.
#' @param weight \code{NULL} ou un vecteur de poids de longueur égale au nombre de modalités (utile que si au moins une des variables à analyser est \bold{Quantitatif} et que l'analyse est pondérée).
#' @param title une chaine de caractère représentant le titre de l'analyse.

#' @export graphpdd
#' @examples
#' ######################################
#' # Exemple mono qualitatif
#' ######################################
#' data<-data.frame(c(10000,2000,888))
#' rownames(data)<-c("Un premier nom de modalité","Un deuxième nom de modalité","Un troisième nom de modalité")
#' p <- graphpdd(data = data
#'               ,lib_var = "Un libellé de variable"
#'               ,type_general = "Qualitatif"
#'               ,title = "Titre du graphique")
#' print(p)
#' ######################################
#' # Exemple mono quantitatif
#' ######################################
#' set.seed(123)
#' x<-runif(1000)
#' data<-data.frame(x=rnorm(n=1000,mean = 0,sd=1)*(x>0.5)+(x<=0.5)*rnorm(n=1000,mean=5,sd=2))
#' p <- graphpdd(data = data
#'               ,lib_var = "Un libellé de variable"
#'               ,type_general = "Quantitatif"
#'               ,title = "Titre du graphique")
#' print(p)
#'
#' ######################################
#' # Exemple bivarié quali x quali
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
#'               ,title = "Titre du graphique")
#' print(p)
#'
#' p <- graphpdd(data = data
#'               ,lib_var = c("Une première variable avec un libellé super long","Une seconde variable")
#'               ,type_general = c("Qualitatif","Echelle sémantique")
#'               ,title = "Titre du graphique")
#' print(p)
#'
#'
#' ######################################
#' # Exemple  bivarié quanti x quanti
#' ######################################
#' set.seed(123)
#' x<-runif(10000)
#' x<-rnorm(n=10000,mean = 0,sd=1)*(x>0.5)+(x<=0.5)*rnorm(n=10000,mean=5,sd=2)
#' y<-x+0.25*x^2+2+rnorm(n=10000,mean=0,sd=3)
#' data<-data.frame(x=x,y=y)
#' p <- graphpdd(data = data
#'               ,lib_var = c("Une première variable avec un libellé super long","Une seconde variable")
#'               ,type_general = c("Quantitatif","Quantitatif")
#'               ,title = "Titre du graphique")
#' print(p)
#' # avec un poids
#' w<-log(1+abs(data[,1]))*(1+runif(10000)/5)
#' p <- graphpdd(data = data
#'               ,lib_var = c("Une première variable avec un libellé super long","Une seconde variable")
#'               ,type_general = c("Quantitatif","Quantitatif")
#'               , weight = w
#'               ,title = "Titre du graphique (pondéré)")
#' print(p)
#'
#' ######################################
#' # Exemple  bivarié quanti x quali
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
#'               ,title = "Titre du graphique")
#' print(p)
#' # On peut inverser les données (quanti <-> quali), cela ne change rien
#' p <- graphpdd(data = data[,2:1]
#'               ,lib_var = c("Une première variable avec un libellé super long","Une seconde variable")
#'               ,type_general = c("Qualitatif","Quantitatif")
#'               ,title = "Titre du graphique (2)")
#' print(p)
#'
#'


graphpdd <- function(data, type_general, lib_var, weight = NULL, title = "") {
  out <- NULL
  if(length(type_general)==1){
    if(type_general %in% c("Qualitatif","Echelle sémantique","Echelle sémantique inversée")){
      out <- graphpdd_mono_quali(data=data, lib_var=lib_var, title = title)
    } else if(type_general=="Quantitatif") {
      out <- graphpdd_mono_quanti(data=data, lib_var=lib_var, title = title)
    }

  } else if(length(type_general)==2){
    if(all(type_general %in%  c("Qualitatif","Echelle sémantique","Echelle sémantique inversée"))){
      out <- graphpdd_bi_quali_quali(data=data, lib_var=lib_var, type_general=type_general, title = title)
    } else if(all(type_general %in%  c("Quantitatif"))){
      out <- graphpdd_bi_quanti(data = data, lib_var = lib_var, title = title, w = weight)
    } else if(!all(type_general %in%  c("Quantitatif")) & "Quantitatif" %in% type_general){
      out <- graphpdd_bi_quanti_quali(data=data, lib_var=lib_var, type_general=type_general, title = title, w= weight)
    }

  }
  return(out)

}

graphpdd_mono_quali <- function(data, lib_var, title = "", ...){
  library(ggplot2)
  library(ggthemes)
  library(scales)
  library(stringr)
  lib_var <- str_wrap(lib_var,40)
  title <- str_wrap(title,40)
  rownames(data) <- str_wrap(rownames(data) ,20)
  x <- data.frame(x=rownames(data),`Fréquence`=data[,1],check.names = FALSE,stringsAsFactors = FALSE)
  .e <- environment()
  p<-ggplot(data=x,aes(x=x,y=`Fréquence`)) +
    geom_bar(stat = "identity",fill=excel_pal('new')(1),alpha=0.85) +
    scale_y_continuous(labels = function(x,...)format(x, ..., big.mark = " ", scientific = FALSE, trim = TRUE))+
    xlab(lib_var)+
    ggtitle(title)
  p<- p+theme_pander()
  p$plot_env<-.e
  return(p)
}


graphpdd_bi_quali_quali <- function(data, lib_var, type_general, title = "", ...){
  library(ggplot2)
  library(ggthemes)
  library(scales)
  library(stringr)
  library(reshape2)
  library(dplyr)
  library(RColorBrewer)
  lib_var[1] <- str_wrap(lib_var[1],40)
  lib_var[2] <- str_wrap(lib_var[2],15)
  title <- str_wrap(title,40)
  rownames(data) <- str_wrap(rownames(data) ,20)
  colnames(data) <- str_wrap(colnames(data) ,15)

  x <- data.frame(xxxr_var1=rownames(data),data,check.names = FALSE,stringsAsFactors = FALSE,row.names = NULL)
  x<-reshape2::melt(x,id.vars=c("xxxr_var1"),variable.name="xxxr_var2")
  x$xxxr_var1<-factor(x$xxxr_var1,levels=rownames(data))
  x$xxxr_var2<-factor(x$xxxr_var2,levels=colnames(data))
  y<-x%>%group_by(xxxr_var1)%>%summarise(s=sum(value))%>%as.data.frame
  x<-left_join(x,y,by="xxxr_var1")%>%mutate(value=value/s)%>%as.data.frame%>%select(-s)


  colourCount = length(unique(x[,2]))
  # getPalette = colorRampPalette(brewer.pal(9, "Set1"))
  g0<-gdocs_pal()(min(20,colourCount))
  if(type_general[2]=="Echelle sémantique"){
    g0<-(tableau_div_gradient_pal(palette = "Red-Blue", space = "Lab")(seq(0,1,length.out=colourCount)))
  } else if(type_general[2]=="Echelle sémantique inversée"){
    g0<-(tableau_div_gradient_pal(palette = "Red-Blue", space = "Lab")(seq(1,0,length.out=colourCount)))
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
  p<- p+theme_pander()
  p$plot_env<-.e
  return(p)
}


graphpdd_mono_quanti <- function(data, lib_var, title = "", ...){
  library(ggplot2)
  library(ggthemes)
  library(scales)
  library(stringr)
  lib_var <- str_wrap(lib_var,40)
  title <- str_wrap(title,40)

  x <- data.frame(x=data[,1],check.names = FALSE,stringsAsFactors = FALSE)

  e<-hist(x[,1],plot=FALSE)
  .e <- environment()
  p<-ggplot(data=x,aes(x=x)) +
    geom_histogram(bins =  length(e$breaks)-1 ,center=mean(e$breaks[seq(min(2,length(e$breaks)))])
                   ,fill=excel_pal('new')(1),alpha=0.85,color="white",size=0.8)+
    scale_y_continuous(labels = function(x,...)format(x, ..., big.mark = " ", scientific = FALSE, trim = TRUE))+
    scale_x_continuous(labels = function(x,...)format(x, ..., big.mark = " ", scientific = FALSE, trim = TRUE))+
    xlab(lib_var)+ylab("Fréquence")+
    ggtitle(title)
  p<- p+theme_pander()
  p$plot_env<-.e
  return(p)
}




graphpdd_bi_quanti <- function(data, lib_var, title = "", w = NULL, ...){
  library(ggplot2)
  library(ggthemes)
  library(scales)
  library(stringr)
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
    geom_point(color=excel_pal('new')(1),alpha=n(w,0.05,0.85),size=1.5)+
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




graphpdd_bi_quanti_quali <- function(data, lib_var, type_general, title = "", w= NULL, ...){
  if(type_general[2]=="Quantitatif"){
    return(graphpdd_bi_quanti_quali(data[,2:1], lib_var[2:1], type_general[2:1], title = title, w=w))
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
    if(is.null(w))w<-rep(1,nrow(data))

    x <- data.frame(x=data[,1],y=data[,2],w=w,check.names = FALSE,stringsAsFactors = FALSE)
    x[,2]<-factor(x[,2])
    levels(x[,2]) <- str_wrap(levels(x[,2]) ,10)
    e<-hist(x[,1],plot=FALSE)
    .e <- environment()

    p<-ggplot(data=x,aes(x=x)) +
      geom_histogram(bins =  length(e$breaks)-1 ,center=mean(e$breaks[seq(min(2,length(e$breaks)))])
                     ,fill=excel_pal('new')(1),alpha=0.85,color="white",size=0.8)+
      scale_y_continuous(labels = function(x,...)format(x, ..., big.mark = " ", scientific = FALSE, trim = TRUE))+
      scale_x_continuous(labels = function(x,...)format(x, ..., big.mark = " ", scientific = FALSE, trim = TRUE))+
      xlab(lib_var)+ylab("Fréquence")+
      ggtitle(title)
    p<- p+theme_pander()
    p<-p+facet_grid(y~.)
    p$plot_env<-.e
    return(p)
  }
}
