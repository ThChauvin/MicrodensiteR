#' cut_reverse_OW
#'
#' @param ii pattern in files you want to cut
#' @details You have to click where you want to slip the profil
#' @return two separate profil
#' @export
#'
#' @examples
cut_reverse_OW<-function(ii = "dat")
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
    limP3 <- limites[limites>k]
    limP4 <- limites[limites<k]
    P3 <- profil[k:max(limP3)]
    P4 <- profil[min(limP4):k]
    P4 <- rev(P4)

    plot(P3,col="dark grey")
    title(main=paste("CORRECTED",nom,"P3"))
    lines(P3,col="grey")
    abline(v=limP3-k)
    limiP3 <- limP3-k
    points(limiP3,P3[limiP3],pc=1,col="green",cex=1.2,lwd=3)
    points(0,P3[1],pc=1,col="green",cex=1.2,lwd=3)
    pos<-c(limiP3[1]/2,diff(limiP3)/2)
    yearP3 <- year[c(length(limP3)+1:length(year))]
    text(limiP3-pos,0.95*max(P3),as.character(yearP3),cex=0.7,srt=90,col="red")

    plot(P4,col="dark grey")
    title(main=paste("CORRECTED",nom,"P4"))
    lines(P4,col="grey")
    abline(v=limP4)
    limiP4 <- limP4
    points(limiP4,P4[limiP4],pc=1,col="green",cex=1.2,lwd=3)
    points(0,P4[1],pc=1,col="green",cex=1.2,lwd=3)
    pos<-c(limiP4[1]/2,diff(limiP4)/2)
    yearP4 <- year[c(length(limP4)+1:length(year))]
    text(limiP4-pos,0.95*max(P4),as.character(yearP4),cex=0.7,srt=90,col="red")

    readline()
    #sauvegarde
    sortie1 <- list(length = 4)
    sortie1[[1]] <- P3
    sortie1[[2]] <- limiP3
    sortie1[[3]] <- yearP3
    sortie1[[4]] <- "3"
    names(sortie1) <- c("profil", "limites","millesimes","sample_number")
    assign(paste(nom,"3",sep="_"), sortie1, pos = 1, immediate = T)

    sortie2 <- list(length = 4)
    sortie2[[1]] <- P4
    sortie2[[2]] <- limiP4
    sortie2[[3]] <- yearP4
    sortie2[[4]] <- "4"
    names(sortie2) <- c("profil", "limites","millesimes","sample_number")
    assign(paste(nom,"4",sep="_"), sortie2, pos = 1, immediate = T)

  }

}
