#' Cut_portion
#'
#' @param ii pattern in files you want to cut
#' @details You have to click two times on the profil to select the area to cut
#' @return a profil without the cut part
#' @export
#'
#' @examples
cut_portion<-function(ii = "dat")
{
  par(mfrow=c(1,1),pty="m")
  liste<-objects(pattern= ii ,pos=1)
  #
  vl<-NULL
  for (i in 1:length(liste))
  {
    k<-NULL
    print(i)
    nom<-liste[i]
    data<-get(nom,pos=1)
    profil<-data$profil
    l=length(profil)
    plot(profil,col="dark grey")
    title(main=paste("arbre",nom))
    lines(profil,col="grey")
    limites<-data$limites
    abline(v=limites)
    points(limites,profil[limites],pc=1,col="green",cex=1.2,lwd=3)
    year<-data$millesimes
    pos<-c(limites[1]/2,diff(limites)/2)
    text(limites-pos,0.95*max(profil),as.character(year),cex=0.7,srt=90,col="red")
    k<-identify(seq(along=profil),profil,n=2)
    if (length(k)>=1)
    {
      print(k)
      procut<-profil[c(1:k[1],k[2]:l)]
      limites[limites>k[1]]=limites[limites>k[1]]-(k[2]-k[1])
      plot(procut,col="dark grey")
      title(main=paste("CORRECTED",nom))
      lines(procut,col="grey")
      abline(v=limites)
      points(limites,procut[limites],pc=1,col="green",cex=1.2,lwd=3)
      pos<-c(limites[1]/2,diff(limites)/2)
      text(limites-pos,0.95*max(procut),as.character(year),cex=0.7,srt=90,col="red")
      readline()
      #sauvegarde
      sortie <- list(length = 3)
      sortie[[1]] <- procut
      sortie[[2]] <- limites
      sortie[[3]] <- year
      names(sortie) <- c("profil", "limites","millesimes")
      assign(paste(nom,sep=""), sortie, pos = 1, immediate = T)
    }
  }
}
