#' Ajout_branches
#'
#' @return
#' @export
#' @param pp the pattern you want to recognize in the name of the files you want to work on.
#' @examples
ajout_branch<-function(pp = "dat", milmax )
{
  #source("ajout.R");ajout.R()
  liste<-objects(pattern= pp ,pos=1)
  #ajouter des limites
  n<-50
  par(mfrow=c(1,1),pty="m")
  for (i in 1:length(liste)) #sequence) #:length(liste))
  {
    nouvelim<-NULL #vecteur destin? ? contenir les nlles limites
    nom<-liste[i]
    print(i);print(nom)
    donnees<-get(nom,pos=1)
    profil<-donnees$profil
    x<-seq(along=profil)
    limites<-c(1,donnees$limites)
    mil<-donnees$millesimes
    l<-length(mil)
    p<-ceiling(l/n) #nb de fen?tres
    print(paste("nb of windows :",p))
    pt<-NULL
    for (j in 1:p)
    {
      m1<-(j-1)*n+1
      m2<-j*n
      miln<-mil[m1:m2]
      print(miln)
      miln<-miln[!is.na(miln)]
      limn<-limites[m1:(m2+1)]
      limn<-limn[!is.na(limn)]
      print(limn)
      nc<-length(miln) #nb cernes
      p1<-min(limn)
      p2<-max(limn)
      pp<-profil[p1:p2]
      pt<-c(pt,pp)
      lp<-length(pp)
      px<-x[p1:p2]
      plot(px,pp)
      abline(v=limn)
      #points(limn,pt[limn],col="green",pch=3,cex=2)
      title(main=paste(nom,"windows",j,"",p))
      pos<-diff(limn)
      text(limn[-length(limn)]+pos/2,0.9*max(pp),as.character(miln),srt=90,col="red")
      print("Entrer le nombre de limites a rajouter")
      nlr <- readline() #nombre limites ? rajouter = nombre total de limites
      pid <- identify(px,pp,n=nlr) #d?signation d'une limite ? ajouter
      lpid <- length(pid) #nb de limites ? ajouter

      if(lpid>0) #si on a d?sign? une (ou plusieurs ?) limite :
      {
        print(pid)
        pidv<-px[pid]
        limn2<-sort(pidv)
        nc2<-length(limn2)-1 #nouveau nb de cernes
        miln2<-miln #min(miln)-1+c(1:nc2)
        title(main=paste(nom,"windows",j,"",p))
        pos1<-c(limn2,NA)-c(NA,limn2)
        pos2<-pos1[!is.na(pos1)]
        nouvelim<-sort(c(nouvelim,limn2,limn))
      }
      else
      {
        nouvelim<-c(nouvelim,limn)
      }
      #readline()
    }
    nouvelim<-unique(nouvelim)
    plot(profil,type="l")
    abline(v=nouvelim)
    points(nouvelim,profil[nouvelim],col=2)
    # mila<-max(na.omit(mil))
    nouveaumil<-sort(seq(from=milmax,to=milmax-length(nouvelim)+2)) #A VOIR ?
    pos<-diff(nouvelim)
    text(nouvelim[-length(nouvelim)]+pos/2,0.95*max(pp),as.character(nouveaumil),cex=0.7,srt=90,col="blue")
    readline()

    sortie <- list(length = 3)
    sortie[[1]] <- profil
    sortie[[2]] <- nouvelim[-1]
    sortie[[3]] <- nouveaumil
    names(sortie) <- c("profil", "limites","millesimes")
    assign(paste(nom,sep=""), sortie, pos = 1, immediate = T)

  }
}


