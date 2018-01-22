#' Import
#'
#' @return
#' @export
#' @param zz the pattern you want to recognize in the name of the files you want to import.
#' @param pathway path to the files
#' @details to import profiles in R; one by one pressing enter at each new profile.
#' @examples Import("pxb"), will import all the profile with pxb in their name.
Import<-function(zz="dat",pathway=".")
{
#source("Import.R");Import.R()
par(mfrow = c(1, 1), pty = "m")
liste <- list.files(path=pathway,pattern=zz)     #fichiers a traiter
noms <- substring(liste, 0)     #
reste <- liste
    for(i in 1:length(reste))
		{
    profil.sauve <- NULL
    x.sauve <- NULL
    limil.sauve <- NULL
    fichier <- reste[i]
    nom <- fichier
    print(nom)
    #attention a ce niveau. Voir le commentaire suivant
		assign(nom, matrix(as.numeric(scan(paste0(pathway,fichier), skip = 9, multi.line = T)), byrow = T, ncol = 2), pos = 1, immediate = T)
print(i)
    #recuperation de donnees Windendro.
    donnees <- get(nom, pos = 1)
    profil <- donnees[, 1]
		profil<-profil[profil<2]
		profil<-profil[profil>0]
    limil <- donnees[, 2]
    limites <- seq(along = limil)[limil > 1]
		profil<-profil[1:max(limites)] #eliminer ce qui est au dela de la derniere limite
    lp <- length(profil)    #longueur du profil
		x.profil<-seq(along=profil)
    plot(x.profil,profil,cex=.4)
		lines(x.profil,profil,col=1)
		title(main=nom)
    abline(v = limites,col=2,lwd=2)     #abline(0,1)
    points(limites, profil[limites], pch = 3, cex = 2,col=3,lwd=2)
    position<-(c(0,limites)-c(limites,NA))/2
    position<-position[-length(position)]
    text(limites+position,max(profil),as.character(limil[limil>1]))
#sauvegarde
    sortie <- list(length = 3)
    sortie[[1]] <- profil[!is.na(profil)]
    sortie[[2]] <- seq(along = limil)[limil > 1]
    sortie[[2]] <- sortie[[2]][!is.na(sortie[[2]])]
    sortie[[3]] <- limil[limil>1]
    names(sortie) <- c("profil", "limites","millesimes")
    readline()
    assign(paste(nom, sep = ""), sortie, pos = 1, immediate = T)
    }
}
