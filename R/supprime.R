#' Supprime
#'
#' @return
#' @export
#' @param oo the pattern you want to recognize in the name of the files you want to work on.
#' @details this function is made to delete ring limite by clicking on it.
#' @examples
supprime<-function(oo = "dat")
{
  liste<-objects(pattern= oo,pos=1)
  n<-50
  par(mfrow=c(1,1),pty="m")
  for (i in 1:length(liste)) #sequence #:length(liste))
  {
    nouvelim<-NULL #vecteur pour les nlles limites
    nom<-liste[i]
    print(i);print(nom)
    donnees<-get(nom,pos=1)
    profil<-donnees$profil
    x<-seq(along=profil)
    limites<-c(1,donnees$limites)
    mil<-donnees$millesimes
    l<-length(mil)
    p<-ceiling(l/n) #nb of windows
    print(paste("nb of windows :",p))
    pt<-NULL
    for (j in 1:p)
    {
      m1<-(j-1)*n+1
      m2<-j*n
      miln<-mil[m1:m2]
      miln<-miln[!is.na(miln)]
      limn<-limites[m1:(m2+1)]
      limn<-limn[!is.na(limn)]
      nc<-length(miln) #nb ring
      p1<-min(limn)
      p2<-max(limn)
      pp<-profil[p1:p2]
      pt<-c(pt,pp)
      lp<-length(pp)
      px<-x[p1:p2]
      plot(px,pp,type="l")
      abline(v=limn)
      points(limn,pt[limn],col="green",pch=3,cex=2)
      title(main=paste(nom,"windows",j,"",p))
      pos<-diff(limn)
      text(limn[-length(limn)]+pos/2,0.9*max(pp),as.character(miln),srt=90,col="blue")
      print("Entrer le nombre de limites a supprimer")
      nls<-readline() #nombre limites a supprimer
      if (nls!="0")
      { pid<-identify(limn,pt[limn],n=nls) #designation d'une limite a supprimer
        lpid<-length(pid) #nb de limites supprimmees
        limn2<-limn[-pid]
        }
      else
      { print("And he signs from the tip of his sword a zero...")
        limn2<-limn}
      plot(px,pp,type="l")
      title(main=paste(nom))
      abline(v = limn2,col=2,lwd=2)     #abline(0,1)
      points(limn2, pp[limn2], pch = 3, cex = 2,col=3,lwd=2)
      pos<-diff(limn2)/2
      #text(limn2[-length(limn2)]+pos,0.9*max(pp),as.character(miln[-length(miln)]),srt=90,col="red",cex=1)
      nouvelim<-unique(c(nouvelim, limn2))
      # readline()
    }
    plot(profil, type="l")
    abline(v=nouvelim)
    mila<-max(na.omit(mil))
    nouveaumil<-sort(seq(from=mila,to=mila-length(nouvelim)+2))
    print(nouveaumil)
    pos<-diff(nouvelim)
    text(nouvelim[-length(nouvelim)]+pos/2,0.95*max(pp),as.character(nouveaumil),cex=0.7,srt=90,col="red")
    # readline()
    sortie <- list(length = 3)
    sortie[[1]] <- profil
    sortie[[2]] <- nouvelim[-1]
    sortie[[3]] <- nouveaumil
    names(sortie) <- c("profil","limites","millesimes")
    assign(paste(nom,sep="p"), sortie, pos = 1, immediate = T)

  }
}
