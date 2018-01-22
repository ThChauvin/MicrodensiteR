#' Affiche
#'
#' @return
#' @export
#' @param yy the pattern you want to recognize in the name of the profile you want to see.
#' @examples
affiche<-function(yy = "dat")
{
par(mfrow=c(1,1))
#source("affiche.R");affiche.R()
liste<-objects(pattern=yy,pos=1)     #liste<-c("fichier1",...
print(liste)
#avec fonction de correction de la fin du profil
n<-30 #longueur d'une fen?tre en nb de cernes
for (i in 1:length(liste)) #sequence) #:length(liste))
	{
	nom<-liste[i]
	ylr<-NA
	print(i);print(nom)
	donnees<-get(nom,pos=1)
	profil<-donnees$profil
	limites<-c(1,donnees$limites)
	mil<-donnees$millesimes
	x<-seq(along=profil)
	plot(x,profil,type="n")
	lines(x,profil,col=1,lwd=1)
	title(main=paste(nom," (profil complet)"))
        abline(v = limites,col=2,lwd=2)     #abline(0,1)
        points(limites, profil[limites], pch = 3, cex = 2,col=3,lwd=2)
	position<-(c(limites,NA)-c(0,limites))/2
	position<-position[!is.na(position)]
	position<-position[-1]
	text(limites[-length(limites)]+position,0.9*max(profil),as.character(mil),srt=90,col=3,cex=0.8)
	l<-length(mil) #nb total de cernes (mill?simes)
	p<-ceiling(l/n) #nb de fen?tres tombant juste (en terme de cernes)
	print(paste("nb total de fen?tres :",p))
	pt<-NULL
print(paste("nb de cernes",l,", 1er cerne",min(mil),", dernier cerne",max(mil)))
readline()
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
		plot(px,pp,type="l")
		abline(v=limn)
		points(limn,pp[limn],col="green",pch=3,cex=2)
		title(main=paste(nom,"fen?tre",j,"sur",p))
		pos<-diff(limn)
		text(limn[-length(limn)]+pos/2,0.9*max(pp),as.character(miln),srt=90,col="red")
readline()
		}
	}
}


