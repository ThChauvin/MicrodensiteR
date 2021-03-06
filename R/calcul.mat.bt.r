#' Calcul.mat.bt
#'
#' @return
#' @export
#' @param ww the pattern you want to recognize in the name of the files you want to used to calculate variables.
#' @param papa indicate the value of the interval of resolution in Windendro.
#' @param subst1 where to start in the substring of the file name to determined the identification code
#' @param subst2 where to stop in the substring  of the file name to determined the identification code
#' @examples
calcul.mat.bt<-function(ww = "dat", papa , subst1 , subst2)
{
  par(mfrow=c(1,1))
  liste.profils <- objects(pattern = ww ,pos=1)
  matrix.resultats <- NULL        #matrice des resultats
  vec <- NULL     #vecteur forme a chaque boucle : donnees par cerne
  pas <- papa #0.006350 #distance entre deux points de mesure successifs windendro
  #attention, ne pas oublier de l'adapter en fonction de la fenetre du mdm ou de la definition de Windendro
  #microdensitometre optique 0.024, mdm Windendro 0.025..., voire encore moins si scan haute r?solution
  #boucle "arbre par arbre" (a adapter pour 2 profils par arbre...)

  #pac<-read.table(file="prov_SALA.txt",header=TRUE)

  for (i in 1:length(liste.profils))
  {
    #DEPART!
    print(i)
    nom<-liste.profils[i]
    print(nom)
    x <- get(nom, pos = 1) #profil
    millesime <- max(as.numeric(x$millesimes))    #millesime VERIFIER
    lim.cernes <- x$lim.cernes    #limites de cerne
    lim.bibtbf <- x$lim.bibtbf     #decoupe selon la limite BI-BT-BF
    plot(x$profil,pch=".")
    abline(v=lim.cernes,col = "Red")
    abline(v=lim.bibtbf)
    text(lim.cernes-(c(lim.cernes,NA)-c(0,lim.cernes))/2,max(x$profil),as.character(x$millesimes),cex=0.7)
    nn <- length(lim.cernes)
    num.cernes<-1:nn
    text(lim.cernes-(c(lim.cernes,NA)-c(0,lim.cernes))/2,0.9*max(x$profil),as.character(num.cernes),cex=0.8)
    #readline()
    mm <- length(lim.bibtbf)
    profil <- x$profil
    #boucle "cerne par cerne dans l'arbre"
    toutes.lim.cernes<-c(1,lim.cernes)
    lim.bibtbf1 <- lim.bibtbf[1:length(lim.bibtbf) %% 2 == 1] # on sélectionne les limite de BT inférieurs
    lim.bibtbf2 <- lim.bibtbf[1:length(lim.bibtbf) %% 2 == 0] # on sélectionne les limite de BT supérieurs

    for (j in num.cernes)
    {
      lim1 <- toutes.lim.cernes[j]
      lim2 <- toutes.lim.cernes[j + 1]
      l.bibtbf1 <- lim.bibtbf1[j]
      l.bibtbf2 <- lim.bibtbf2[j]
      Lo <- round((length(profil[lim1:lim2])) * pas, 4) #largeur de cerne en mm
      Li <- round((length(profil[lim1:l.bibtbf1])) * pas, 4) #largeur du bi en mm type Nancy
      Lt <- round((length(profil[l.bibtbf1:l.bibtbf2])) * pas, 4) #largeur du bt en mm type Nancy
      Lf <- round(Lo - (Li+Lt), 4) #largeur du bf en mm
      Do <- round(mean((profil[lim1:lim2]), na.rm = T), 3) #densite moyenne de cerne
      Di <- round(mean((profil[lim1:l.bibtbf1]), na.rm = T), 3) #densite moyenne bi type Nancy
      Dt <- round(mean((profil[l.bibtbf1:l.bibtbf2]), na.rm = T), 3) #densite moyenne bt
      Df <- round(mean((profil[l.bibtbf2:lim2]), na.rm = T), 3) #densite moyenne bf
      Mi <- round(min((profil[lim1:lim2]), na.rm = T), 3) #densite min cerne
      Ma <- round(max((profil[lim1:lim2]), na.rm = T), 3) #densite max cerne
      profil.sans.NA <- profil[lim1:lim2][!is.na(profil[lim1:lim2])]
      Eo <- round(sqrt(var(profil.sans.NA)), 3) #?cart type de densit? intra-cerne
      Et <- round(sqrt(var(profil[l.bibtbf1:l.bibtbf2])), 3) #?cart type de densit? intra-bt
      Ei <- round(sqrt(var(profil[lim1:l.bibtbf1])), 3) #?cart type de densit? intra-bi
      Ef <- round(sqrt(var(profil[l.bibtbf2:lim2])), 3) #?cart type de densit? intra-bf
      Co <- Ma - Mi # contraste de densit? intra-cerne
      annee <- x$millesimes[j] #annee du cerne
      #prov<-pac[i,"prov"]
      vec <- data.frame( substring(liste.profils[i] , subst1 , subst2 ), annee, j, Lo, Li, Lt, Lf, Do, Di, Dt, Df, Mi, Ma, Eo, Ei, Et, Ef, Co) #cas particulier ?
      #ADAPTER le substring !
      colnames(vec) <- c("code", "ye","ce", "Lo", "Li", "Lt", "Lf", "Do", "Di", "Dt", "Df", "Mi", "Ma", "Eo", "Ei", "Et", "Ef", "Co")
      matrix.resultats <- rbind(matrix.resultats, vec)
      #sauvegarde
      assign("matmdm_BT", matrix.resultats, pos = 1, immediate = T)
    }
  }
}

