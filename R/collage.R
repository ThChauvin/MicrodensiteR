#' Collage
#'
#' @return
#' @export
#' @param rr the pattern you want to recognize in the name of the files you want to work on.
#' @examples
collage<-function( rr = "dat" )
{
#source("collage.R");collage.R()
arbres= rr
#rm("pn104a coeur.pxb","pn104a ?corce.pxb","pn159a coeur.pxb","pn159a ?corce.pxb","pn167b coeur.pxb","pn167b ?corce.pxb","pn172a coeur.pxb")
for (i in arbres)
	{
	objet=get(i,pos=1)
	assign(paste("z",i,sep=""),objet,pos=1,immediate=T)
	}
readline()

arbres=c("pn104a","pn159a","pn167b","pn172a")
for (i in arbres)
	{
	liste=objects(pattern=i,pos=1)
	print(liste)
	newpro=NULL
	nl=0
	nlim=NULL
	nmil=NULL
	newlim=NULL
	newmil=NULL
	p1=get(liste[1],pos=1)
	p2=get(liste[2],pos=1)
	pro1=p1$profil #segment de profil c?t? coeur
	pro2=p2$profil #c?t? ?corce
	lim1=p1$limites
	lim2=p2$limites+length(pro1) #rajouter la longueur du segment coeur
	mil1=p1$millesimes
	mil2=p2$millesimes
	newpro=c(pro1,pro2)
	newlim=c(lim1,lim2)
	newmil=c(mil1,mil2)
	plot(seq(along=newpro),newpro,type="n")
	abline(v=length(pro1),col="pink",lwd=10)
	lines(seq(along=newpro),newpro,col="blue")
	abline(v=newlim,col="light blue",lwd=2)
	points(newlim,newpro[newlim],cex=1,col="red",lwd=2)
	text(newlim-(newlim-c(1,newlim[-length(newlim)]))/2,0.9*max(newpro),as.character(newmil),srt=90,cex=0.8,col="red")
	title(main=paste("arbre",i))
	#readline()
#sauvegarde
	sortie <- list(length = 3)
	newmil=sort(max(newmil)):(max(newmil)-length(newmil)+1)
        sortie[[1]] <- newpro
        sortie[[2]] <- newlim
	sortie[[3]] <- newmil
        names(sortie) <- c("profil", "limites","millesimes")
        assign(paste(i,".pxb",sep=""), sortie, pos = 1, immediate = T)
	}
}
