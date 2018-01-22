#' Ajustepositionlimit
#'
#' @return
#' @export
#' @param uu the pattern you want to recognize in the name of the files you want to work on.
#' @param intliss1 Intensity of the first smoothing function.
#' @param intliss1 Intensity of the seconde smoothing function.
#' @examples
ajustepositionlimite<-function(uu = "dat",intliss1,intliss2)
{
#source("ajustepositionlimite.R");ajustepositionlimite.R()
par(mfrow=c(1,1),pty="m")
liste<-objects(pattern= uu ,pos=1)
for (i in 1:length(liste))
	{
	nom<-liste[i]
	print(nom)
	data<-get(liste[i],pos=1)
	profil<-data$profil
	profilm<-supsmu(seq(along=profil),profil,span=intliss1)[[2]] #lissage léger
	plot(profilm)
	profils<-supsmu(seq(along=profil),profil,span=intliss2)[[2]] #lissage un peu plus fort
	lines(profils,col=2)
	l<-length(profil)*0.0254
	limites<-data$limites[-length(data$limites)]
	vnlim<-NULL
	year<-data$millesimes
  #readline()
	for(j in limites)
		{
		lim<-c(j-8,j-7,j-6,j-5,j-4,j-3,j-2,j-1,j,j+1,j+2,j+3,j+4,j+5,j+6,j+7,j+8)
		lim<-c(j-6,j-5,j-4,j-3,j-2,j-1,j,j+1,j+2,j+3,j+4,j+5,j+6)
		dop<-profilm[lim]
		dsp<-profils[lim]
		prof<-(dop-dsp)^2
		pos<-seq(along=prof)[prof==min(prof)][1] # position des limite
		nlim<-lim[pos] # nouvelle limite
		vnlim<-c(vnlim,nlim) # on garde les valeurs des nouvelles limites
		}
	vnlim<-c(vnlim,length(profil))
	position<-c(nlim[1]/2,nlim[-length(nlim)]+diff(nlim)/2)
	text(position,max(profil),as.character(year),cex=0.8,col=2)
	plot(profil,pch=".",cex=2)
	lines(profilm,col="grey")
	lines(profils,col="light green")
	title(main=paste("New limits, profile",nom,"length",round(l,1),"mm"))
	#abline(v=vnlim)
	points(vnlim,profil[vnlim],pc=1,col=2,cex=2)
	points(limites,profil[limites],pc=1,col="pink",cex=1.5)
  #readline()
	sortie <- list(length = 3)
        sortie[[1]] <- profil # profil originel
        sortie[[2]] <- unique(vnlim) # nouvelles limites
	sortie[[3]] <- year # millésimes originaux
        names(sortie) <- c("profil","limites","millesimes")
        assign(paste(nom,sep=""), sortie, pos = 1, immediate = T)

	}
}
