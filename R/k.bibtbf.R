#' K.bibtbf
#'
#' @return
#' @export
#' @details position the limite between earlywood, transition-wood and latewood. Create a new object adding a "tlim" at the beginning of the original name.
#' @param tt the pattern you want to recognize in the name of the files you want to work on.
#' @examples
k.bibtbf<-function(tt = "dat")
  #source("k.bibf.R");k.bibf.R()
{
  par(mfrow=c(1,1))
  liste.profils<-objects( pattern = tt ,pos=1)

  for (i in 1:length(liste.profils))
  {
    xx<-get(liste.profils[i],pos=1)

    profil<-liste.profils[i]
    cat(profil,"\n")

    x<-xx$profil
    x<-x[!is.na(x)]
    lim.cernes<-xx$limites
    lim.cernes<-lim.cernes[!is.na(lim.cernes)]
    lim.cernes.x<-lim.cernes
    nbr.cernes<-length(lim.cernes)
    lim.cernes.x<-c(1,lim.cernes)
    lim.cernes<-c(1,lim.cernes)
    nbr.limites<-nbr.cernes+1

    #                           PROFIL DE DEPART:

    #                      LISSAGE, LIMITES "CLASSIQUES" :
    #                      ******************************
    z<-x
    profil.lisse<-NULL
    lim.bibf.Nancy<-NULL
    # On travaille cerne par cerne :
    for (k in 1:(nbr.cernes))
    {
      cat(k,"\n")
      #decoupage du profil en cernes :
      #*******************************
      profil.cerne <- x[(lim.cernes.x[k]):(lim.cernes.x[k+1])]
      l <- length(profil.cerne)
      NA.debut <- length(profil.cerne[1:(l/2)][is.na(profil.cerne[1:(l/2)])]) #nbre de NA en debut de profil
      NA.fin <- length(profil.cerne[(l/2):l][is.na(profil.cerne[(l/2):l])])  #nbre de NA en fin de profil
      profil.cerne.0.NA<-profil.cerne[!is.na(profil.cerne)]

      #limites Nancy :
      #****************
      # ttt<-0.01
      aaa<-1
      # while (ttt<0.1)
      # {
      y <- profil.cerne.0.NA
      y <- y[aaa:(length(y))]
      # aaa<-aaa+1
      tt <- seq(along=y)
      lll <- length(y)
      amax <- tt[y==max(y)] #abcisse du max
      ttt <- amax/lll
      if (length(amax)>1) {amax<-amax[1]}
      y <- y[1:(amax)]
      tt <- seq(along=y)
      amin <- tt[y==min(y)] #abcisse du min
      if (length(amin)>1) {amin<-amin[length(amin)]}
      y <- y[amin:(length(y))]
      tt <- seq(along=y)
      alN <- tt[(y-(max(y)+min(y))/2)^2==min((y-(max(y)+min(y))/2)^2)] # on cherche la valeur la plus proche de la moyenne des extremes (max(y)+min(y))/2
      if  (length(alN>1)) {alN<-alN[1]}
      alN<-amin+alN


      ## Limite inférieur de BT
      #### Attention pbm dans les amin et amax entre le 1er
      y <- profil.cerne.0.NA
      y <- y[amin:alN]
      # aaa<-aaa+1
      tt <- seq(along=y)
      lll <- length(y)
      amaxt <- tt[y==max(y)] #abcisse du max
      ttt <- amaxt/lll
      if (length(amaxt)>1) {amaxt<-amaxt[1]}
      y <- y[1:(amaxt)]
      tt <- seq(along=y)
      amint <- tt[y==min(y)] #abcisse du min
      if (length(amint)>1) {amint<-amint[length(amint)]}
      y <- y[amint:(length(y))]
      tt <- seq(along=y)
      albti <- tt[(y-(max(y)+min(y))/2)^2==min((y-(max(y)+min(y))/2)^2)]
      if (length(alN>1)) {albti<-albti[1]}
      albti <- lim.cernes[k]+amin+albti

      ## Limite superieur de BT
      y <- profil.cerne.0.NA
      y <- y[(amin + alN):(length(y))]
      # aaa <- aaa+1
      tt <- seq(along=y)
      lll <- length(y)
      amaxt <- tt[y==max(y)] #abcisse du max
      ttt <- amaxt/lll
      if (length(amaxt)>1) {amaxt<-amaxt[1]}
      y <- y[1:(amaxt)]
      tt <- seq(along=y)
      amint <- tt[y==min(y)] #abcisse du min
      if (length(amint)>1) {amint<-amint[length(amint)]}
      y <- y[amint:(length(y))]
      tt <- seq(along=y)
      albts <- tt[(y-(max(y)+min(y))/2)^2==min((y-(max(y)+min(y))/2)^2)]
      if (length(alN>1)) {albts<-albts[1]}
      albts <- lim.cernes[k]+alN+albts

      # }
      lim.bibf.Nancy <- c(lim.bibf.Nancy,albti,albts)
    }

    #           Affichage du profil complet avec l'emplacement des limites ponctuelles
    #           **********************************************************************


    plot(x,type="n",xlab="Longueur (X24 microns)",ylab="densite (g/dm3)", main = profil)
    lines(x)
    mtext("Definition des limites bois initial-bois final",3,outer=T,cex=1.4)
    points(lim.bibf.Nancy,x[lim.bibf.Nancy],type="p",pch=0,cex=1)
    abline(v=lim.bibf.Nancy,lty=2)
    abline(v=lim.cernes,lty=1)
    xxx <- length(x)/50
    yyy <- min(x,na.rm=T)+0.1*min(x,na.rm=T)

    #readline()
    #                     SORTIE: profil avec limites
    #                     ***************************


    #controle
    if (length(lim.bibf.Nancy)!= 2 * nbr.cernes)
    {
      cat("Warning!",liste.profils[[i]])
      readline()
    }
    #Profil complet:
    cat(paste(length(lim.cernes),"// "))
    sortie <- list(length=4) #au lieu de 5 : pas de profil
    sortie[[1]] <- xx$profil #lim de cernes
    sortie[[2]] <- xx$limites[!is.na(xx$limites)]
    sortie[[3]] <- sort(lim.bibf.Nancy) #lim bibf type Nancy (moy min max) ATTENTION en 5 depuis les epiceas Geniality
    sortie[[4]] <- xx$millesimes[!is.na(xx$millesimes)]
    names(sortie) <- c("profil","lim.cernes","lim.bibtbf","millesimes")#
    cat("Suivant?")
    readline()
    assign(paste("tlim",profil,sep=""),sortie,pos=1,immediate=T) #changer

    cat("\nFIN:\n")

    #cat("\nTaper <Entree> pour continuer, x pour arreter\n")
    #zzz<-readline()
    zzz<-"bouclage"
    if (zzz=="x") {stop()}

  } #fin de boucle "automatisation de la lecture des profils d'un .Data"

} #fin de fonction

