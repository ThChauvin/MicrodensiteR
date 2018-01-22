#' cut_one_side
#'
#' @param ii pattern in files you want to cut
#' @param dir side where you want to delete part of the profile : "l" left , r "right"
#' @details You have to click close to the limit you want to be the end of the profile
#' @return a profil without the cut part
#' @export
#'
#' @examples
cut_one_side<-function(ii = "dat",gg ="dir")
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
	if (gg == "l")
		{
		print(k)
	  diffmill <- limites[limites<k]
	  limites <- limites[limites>k]
	  procut<-profil[min(limites):max(limites)] ## A finir, pbm sur les limites....
		limites <- limites-min(limites)
		plot(procut,col="dark grey")
		title(main=paste("CORRECTED",nom))
		lines(procut,col="grey")
		abline(v=limites)
		limites <- limites[c(2:length(limites))]
		points(limites,procut[limites],pc=1,col="green",cex=1.2,lwd=3)
		points(0,procut[1],pc=1,col="green",cex=1.2,lwd=3)
		pos<-c(limites[1]/2,diff(limites)/2)
		year <- year[c(length(diffmill)+2:length(year))]
		text(limites-pos,0.95*max(procut),as.character(year),cex=0.7,srt=90,col="red")
		readline()
		#sauvegarde
	        sortie <- list(length = 3)
	        sortie[[1]] <- procut
	        sortie[[2]] <- limites
		sortie[[3]] <- year
	        names(sortie) <- c("profil", "limites","millesimes")
	        assign(paste(nom,sep=""), sortie, pos = 1, immediate = T)
		} else 	if (gg == "r")
		{
		  print(k)
		  diffmill <- limites[limites>k]
		  limites <- limites[limites<k]
		  procut<-profil[c(1:max(limites))] ## A finir, pbm sur les limites....
		  plot(procut,col="dark grey")
		  title(main=paste("CORRECTED",nom))
		  lines(procut,col="grey")
		  abline(v=limites)
		  # limites <- limites[c(2:length(limites))]
		  points(limites,procut[limites],pc=1,col="green",cex=1.2,lwd=3)
		  pos<-c(limites[1]/2,diff(limites)/2)
		  year <- year[c(length(diffmill)+1:length(year))]
		  text(limites-pos,0.95*max(procut),as.character(year),cex=0.7,srt=90,col="red")
		  readline()
		  #sauvegarde
		  sortie <- list(length = 3)
		  sortie[[1]] <- procut
		  sortie[[2]] <- limites
		  sortie[[3]] <- year
		  names(sortie) <- c("profil", "limites","millesimes")
		  assign(paste(nom,sep=""), sortie, pos = 1, immediate = T)
		} else {
		  print("You only have two choice...How can you fail...")
		}
	}
}

