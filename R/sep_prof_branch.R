#' sep_prof_branch
#'
#' @param mama
#'
#' @return
#' @export
#'
#' @examples
sep_prof_branch <- function(mama="dat")
{
  liste<-objects(pattern= mama ,pos=1)
  for (i in 1: length(liste))
  {
    nom <- liste[i]
    print(nom)
    donnees<-get(nom,pos=1)
    pgauche <- donnees[c(1,4,6)]
    pdroit <- donnees[c(2,5,7)]

    profilg <- pgauche$profilg
    nouvelimg <- c(pgauche$limg,length(profilg))
    nouveaumilg <- na.omit(pgauche$milg)

    profild <- pdroit$profild
    nouvelimd <- c(pdroit$limd,length(profild))
    nouveaumild <- na.omit(pdroit$mild)

    sortieg <- list(length = 4)
    sortieg[[1]] <- profilg
    sortieg[[2]] <- nouvelimg[-1]
    sortieg[[3]] <- nouveaumilg
    sortieg[[4]] <- "g"
        names(sortieg) <- c("profil", "limites","millesimes","rayon")
    assign(paste("rg",nom,sep=""), sortieg, pos = 1, immediate = T)

    sortied <- list(length = 4)
    sortied[[1]] <- profild
    sortied[[2]] <- nouvelimd[-1]
    sortied[[3]] <- nouveaumild
    sortied[[4]] <- "d"
    names(sortied) <- c("profil", "limites","millesimes","rayon")
    assign(paste("rd",nom,sep=""), sortied, pos = 1, immediate = T)

  }
}
