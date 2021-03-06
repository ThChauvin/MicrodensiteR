#' Calcul.mat_branche
#'
#' @return
#' @export
#'
#' @examples
calcul.mat_branche<-function(ww = "dat", papa)
{
  par(mfrow=c(1,1))
  liste.profils <- objects(pattern = ww ,pos=1)
  matrix.resultats <- NULL        #matrice des resultats
  vec <- NULL     #vecteur forme a chaque boucle : donnees par cerne
  pas <- papa
  for (i in 1:length(liste.profils))
  {
    #DEPART!
    print(i)
    nom<-liste.profils[i]
    print(nom)
    x <- get(nom, pos = 1) #profil
    rayon <- x$rayon
    millesime <- max(as.numeric(x$millesimes))    #millesime VERIFIER
    lim.cernes <- x$lim.cernes    #limites de cerne
    lim.bibf1 <- x$lim.bibf     #decoupe selon la limite BI-BF Nancy
    plot(x$profil,pch=".")
    abline(v=lim.cernes)
    abline(v=lim.bibf1)
    text(lim.cernes-(c(lim.cernes,NA)-c(0,lim.cernes))/2,max(x$profil),as.character(x$millesimes),cex=0.7)
    nn <- length(lim.cernes)
    num.cernes<-1:nn
    text(lim.cernes-(c(lim.cernes,NA)-c(0,lim.cernes))/2,0.9*max(x$profil),as.character(num.cernes),cex=0.8)
    #readline()
    mm <- length(lim.bibf1)
    profil <- x$profil
    #boucle "cerne par cerne dans l'arbre"
    toutes.lim.cernes<-c(1,lim.cernes)
    for (j in num.cernes)
    {
      lim1 <- toutes.lim.cernes[j]
      lim2 <- toutes.lim.cernes[j + 1]
      l.bibf1 <- lim.bibf1[j]
      Lo <- round((length(profil[lim1:lim2])) * pas, 4) #largeur de cerne en mm
      Li <- round((length(profil[lim1:l.bibf1])) * pas, 4) #largeur du bi en mm type Nancy
      Lf <- round(Lo - Li, 4) #largeur du bf en mm
      Do <- round(mean((profil[lim1:lim2]), na.rm = T), 3) #densite moyenne de cerne
      Di <- round(mean((profil[lim1:l.bibf1]), na.rm = T), 3) #densite moyenne bi type Nancy
      Df <- round(mean((profil[l.bibf1:lim2]), na.rm = T), 3) #densite moyenne bf
      Mi <- round(min((profil[lim1:lim2]), na.rm = T), 3) #densite min cerne
      Ma <- round(max((profil[lim1:lim2]), na.rm = T), 3) #densite max cerne
      profil.sans.NA <- profil[lim1:lim2][!is.na(profil[lim1:lim2])]
      Eo <- round(sqrt(var(profil.sans.NA)), 3) #?cart type de densit? intra-cerne
      Ei <- round(sqrt(var(profil[lim1:l.bibf1])), 3) #?cart type de densit? intra-bi
      Ef <- round(sqrt(var(profil[l.bibf1:lim2])), 3) #?cart type de densit? intra-bf
      Co <- Ma - Mi # contraste de densit? intra-cerne
      annee <- x$millesimes[j] #annee du cerne
      #prov<-pac[i,"prov"]
      vec <- c( substring(liste.profils[i],6,11 ), rayon , annee, j, Lo, Li, Lf, Do, Di, Df, Mi, Ma, Eo, Ei, Ef, Co) #cas particulier ?
      #ADAPTER le substring !
      matrix.resultats <- rbind(matrix.resultats, vec)
      dimnames(matrix.resultats) <- list(NULL, c("code","rayon", "ye","ce", "Lo", "Li", "Lf", "Do", "Di", "Df", "Mi", "Ma", "Eo", "Ei", "Ef", "Co"))
      #sauvegarde
      assign("matmdm_B", matrix.resultats, pos = 1, immediate = T)
    }
  }
}

