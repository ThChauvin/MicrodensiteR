#' cut_reverse_CW
#'
#' @param ii pattern in files you want to cut
#' @details You have to click where you want to slip the profil
#' @return two separate profil
#' @export
#'
#' @examples
cut_reverse_CW<-function(ii = "dat")
{
  par(mfrow=c(1,1),pty="m")
  liste<-objects(pattern= ii ,pos=1)

  for (i in 1:length(liste))
  {
    k<-NULL
    print(i)
    nom<-liste[i]
    data<-get(nom,pos=1)
    profil<-data$profil
    l <- length(profil)
    plot(profil,col="dark grey")
    title(main=paste("arbre",nom))
    lines(profil,col="grey")
    limites<-data$limites
    abline(v=limites)
    points(limites,profil[limites],pc=1,col="green",cex=1.2,lwd=3)
    year<-data$millesimes
    pos<-c(limites[1]/2,diff(limites)/2)
    text(limites-pos,0.95*max(profil),as.character(year),cex=0.7,srt=90,col="red")
    k<-identify(seq(along=profil),profil,n=1)

    print(k)
    limP1 <- limites[limites>k]
    limP2 <- limites[limites<k]
    P1 <- profil[k:max(limP1)]
    P2 <- profil[min(limP2):k]
    P2 <- rev(P2)

    plot(P1,col="dark grey")
    title(main=paste("CORRECTED",nom,"P1"))
    lines(P1,col="grey")
    abline(v=limP1-k)
    limiP1 <- limP1-k
    points(limiP1,P1[limiP1],pc=1,col="green",cex=1.2,lwd=3)
    points(0,P1[1],,pc=1,col="green",cex=1.2,lwd=3)
    pos<-c(limiP1[1]/2,diff(limiP1)/2)
    yearP1 <- year[c(length(limP1)+1:length(year))]
    text(limiP1-pos,0.95*max(P1),as.character(yearP1),cex=0.7,srt=90,col="red")

    plot(P2,col="dark grey")
    title(main=paste("CORRECTED",nom,"P2"))
    lines(P2,col="grey")
    abline(v=limP2)
    limiP2 <- limP2
    points(limiP2,P2[limiP2],pc=1,col="green",cex=1.2,lwd=3)
    points(0,P2[1],pc=1,col="green",cex=1.2,lwd=3)
    pos<-c(limiP2[1]/2,diff(limiP2)/2)
    yearP2 <- year[c(length(limP2)+1:length(year))]
    text(limiP2-pos,0.95*max(P2),as.character(yearP2),cex=0.7,srt=90,col="red")

    readline()
    #sauvegarde
    sortie1 <- list(length = 4)
    sortie1[[1]] <- P1
    sortie1[[2]] <- limiP1
    sortie1[[3]] <- yearP1
    sortie1[[4]] <- "1"
    names(sortie1) <- c("profil", "limites","millesimes","sample_number")
    assign(paste(nom,"1",sep="_"), sortie1, pos = 1, immediate = T)

    sortie2 <- list(length = 4)
    sortie2[[1]] <- P2
    sortie2[[2]] <- limiP2
    sortie2[[3]] <- yearP2
    sortie2[[4]] <- "2"
    names(sortie2) <- c("profil", "limites","millesimes","sample_number")
    assign(paste(nom,"2",sep="_"), sortie2, pos = 1, immediate = T)

  }

}
