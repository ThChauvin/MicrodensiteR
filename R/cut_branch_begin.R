#' Cut_branche_begin
#'
#' @return
#' @export
#'
#' @examples
cut_branche_begin <- function(gg = "dat")
{

  liste<-objects(pattern= gg,pos=1)
  noms <- substring(liste, 0)

  for(i in 1:length(liste))
  {
    par(mfrow=c(2,1))
    nom<-liste[i]
    print(nom)
    objet<-get(nom,pos=1)
    profil<-objet$profil
    limites<-objet$limites
    millesimes<-objet$millesimes
    plot(profil)
    lines(profil,col="grey")
    abline(v=limites,col="green")
    points(limites,profil[limites],cex=1,col="red",lwd=2)
    text(limites-(limites-c(1,limites[-length(limites)]))/2,0.9*max(profil),as.character(millesimes),srt=90,cex=0.8,col="purple")
    title(main=paste("sample",substring(liste[i],2,7)))
    x=seq(along=profil)
    p1=identify(x,profil,n=1) #valeur de la limite p1
    p2=identify(x[limites],profil[limites],n=1) #position de la limite p2 dans limites
    p2x=limites[p2] #valeur de la limite p2 (gauche de la moelle)
    profilc=profil[p1:length(profil)] #profil sans le morceau à gauche du segment de cerne 2016
    xc=x[p1:length(x)] #abcisse de profilc
    profilg=profil[p1:p2x]
    p3=limites[p2+1] #valeur de la limite p3 (droite de la moelle)
    moelle=profil[p2x:p3]
    profild=profil[p3:limites[length(limites)]]
    plot(profilc,col="grey")
    #lines(profilc,col="grey")
    limites2=limites-p1+1
    abline(v=limites2,col="grey")
    points(limites2,profilc[limites2],cex=1,col="grey",lwd=2)
    text(limites2-(limites2-c(1,limites2[-length(limites2)]))/2,0.9*max(profilc),as.character(millesimes),srt=90,cex=0.8,col="grey")

    #graphique de controle
    p1c=1
    p2c=p2x-p1+1
    p3c=p3-p1+1
    xg=p1c:p2c
    xm=p2c:p3c
    xd=p3c:length(profilc)

    #print(paste(p1,p2x,p3))
    #print(paste(p1c,p2c,p3c))
    #print(paste(length(profilc),length(profilg),length(moelle),length(profild)))
    #print(paste(length(xc),length(xg),length(xm),length(xd)))

    lines(xg,profilg,col="red")
    lines(xm,moelle,col="green")
    lines(xd,profild,col="blue")

    readline()

    #calcul et controle des limites et des millesimes des 2 segments gauche et droite
    #le profilg doit être retourné pour avoir le coeur à gauche
    pgr=rev(profilg)
    #calcul des nouvelles limites du profil gauche retourné
    lg=sort(length(pgr)-(limites[1:p2]-p1))
    mg=sort(millesimes[1:p2]) #millesimes
    #print(sort(lg))
    #affichage pour contrôle
    par(mfrow=c(1,2))
    plot(pgr,col="grey")
    lines(pgr,col="red")
    abline(v=lg)
    milieu=diff(c(lg,length(pgr)))/2 #position des millesimes sur le graphique
    #print(c(lg,length(pgr)))
    #print(milieu)
    text(lg+milieu,0.9*max(pgr),as.character(mg),srt=90,cex=0.8,col="red")
    title(main="Rayon gauche retourné")
    #idem pour profil droit
    plot(profild,col="grey")
    lines(profild,col="blue")
    #print(limites[(p2+1):length(limites)])
    #print(limites[(p2+1):length(limites)]-(p1+length(pgr)+length(moelle)))
    ld=limites[(p2+1):length(limites)]-p1-length(pgr)-length(moelle)+3
    ld=ld[-length(ld)] #position des limites droites
    print(ld)
    abline(v=ld)
    md=seq(from=min(mg),to=min(mg)+length(ld)-1,by=1)
    print(md)
    milieu=diff(c(ld,length(profild)))/2 #position des millesimes sur le graphique
    print(ld+milieu)
    text(ld+milieu,0.9*max(profild),as.character(md),srt=90,cex=0.8,col="red")
    title(main="Rayon droit")

    readline()
    #sauvegarde
    sortie <- list(length = 7)
    sortie[[1]] <- profilg
    sortie[[2]] <- profild
    sortie[[3]] <- moelle
    sortie[[4]] <- lg
    sortie[[5]] <- ld
    sortie[[6]] <- mg
    sortie[[7]] <- md
    names(sortie) <- c("profilg","profild","moelle","limg","limd","milg","mild")
    assign(paste("x",nom,sep=""), sortie, pos = 1, immediate = T) #nouveau nom : avec x devant
  }
}
