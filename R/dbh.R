#' @title Diameter at breast height prediction using height and crown diameter
#' @author Michele Dalponte
#' @description Prediction of diameter at breast height (DBH) using height and crown diameter and the equations of Jucker et al. (2017).
#' @param H Tree height in meters.
#' @param CA Crown diameter in meters.
#' @param biome Integer number indicating the type of biome:
#'
#' 0 = 'Global'
#'
#' 1 = 'Afrotropic-Tropical forests-Angiosperm'
#'
#' 2 = 'Afrotropic-Woodlands and savannas-Angiosperm'
#'
#' 3 = 'Australasia-Temperate mixed forests-Angiosperm'
#'
#' 4 = 'Australasia-Temperate mixed forests-Gymnosperm'
#'
#' 5 = 'Australasia-Woodlands and savannas-Angiosperm'
#'
#' 6 = 'Indo-Malaya-Tropical forests-Angiosperm'
#'
#' 7 = 'Nearctic-Boreal forests-Angiosperm'
#'
#' 8 = 'Nearctic-Boreal forests-Gymnosperm'
#'
#' 9 = 'Nearctic-Temperate coniferous forests-Angiosperm'
#'
#' 10 = 'Nearctic-Temperate coniferous forests-Gymnosperm'
#'
#' 11 = 'Nearctic-Temperate mixed forests-Angiosperm'
#'
#' 12 = 'Nearctic-Temperate mixed forests-Gymnosperm'
#'
#' 13 = 'Nearctic-Woodlands and savannas-Angiosperm'
#'
#' 14 = 'Nearctic-Woodlands and savannas-Gymnosperm'
#'
#' 15 = 'Neotropic-Tropical forests-Angiosperm'
#'
#' 16 = 'Palearctic-Boreal forests-Angiosperm'
#'
#' 17 = 'Palearctic-Boreal forests-Gymnosperm'
#'
#' 18 = 'Palearctic-Temperate coniferous forests-Angiosperm'
#'
#' 19 = 'Palearctic-Temperate coniferous forests-Gymnosperm'
#'
#' 20 = 'Palearctic-Temperate mixed forests-Angiosperm'
#'
#' 21 = 'Palearctic-Temperate mixed forests-Gymnosperm'
#'
#' 22 = 'Palearctic-Tropical forests-Angiosperm'
#'
#' 23 = 'Palearctic-Woodlands and savannas-Angiosperm'
#'
#' 24 = 'Palearctic-Woodlands and savannas-Gymnosperm'
#' @return The DBH value in centimeters.
#' @import methods
#' @import grDevices
#' @export dbh
#' @references T. Jucker, J. Caspersen, J. Chave, C. Antin, N. Barbier, F. Bongers, M. Dalponte, K. Y. van Ewijk, D. I. Forrester, M. Haeni, S. I. Higgins, R. J. Holdaway, Y. Iida, C. Lorimer, P. L. Marshall, S. Momo, G. R. Moncrieff, P. Ploton, L. Poorter, K. A. Rahman, M. Schlund, B. Sonke, F. J. Sterck, A. T. Trugman, V. A. Usoltsev, M. C. Vanderwel, P. Waldner, B. M. M. Wedeux, C. Wirth, H. Woell, M. Woods, W. Xiang, N. E. Zimmermann, and D. A. Coomes, "Allometric equations for integrating remote sensing imagery into forest monitoring programs," Global Change Biology, 23 (1), pp. 177-190, January 2017.
#' @examples
#' \dontrun{
#' data(lasData)
#'
#' ## function takes a while to run
#'
#' #Extraction of the ITCs
#' se<-itcLiDAR(lasData$X,lasData$Y,lasData$Z,epsg=32632)
#'
#' summary(se)
#'
#' #Computation of the crown diameter from the crown area
#' se$CD_m<-2*sqrt(se$CA_m2/pi)
#'
#' #DBH prediction
#' se$dbh<-NA
#' se$dbh<-dbh(se$Height_m,se$CD_m,biome=0)
#'
#' summary(se)
#'
#' }
#'
dbh<-function(H=NULL,CA=NULL,biome=0){

  pa<-c(0,0.557,0.809,0.056,1,0.428,0.821,0,
  2,1.268,0.75,0,
  3,0.769,0.811,0,
  4,0.603,0.891,0,
  5,0.519,0.89,0,
  6,0.486,0.802,0,
  7,0.286,0.865,0,
  8,0.815,0.771,0,
  9,0.343,0.844,0,
  10,0.589,0.817,0,
  11,0.367,0.887,0,
  12,0.861,0.755,0,
  13,0.617,0.79,0,
  14,1.133,0.745,0,
  15,0.591,0.781,0,
  16,0.467,0.839,0,
  17,1.43,0.649,0,
  18,0.273,0.927,0,
  19,0.974,0.748,0,
  20,0.694,0.73,0,
  21,1.004,0.73,0,
  22,0.54,0.791,0,
  23,0.91,0.738,0,
  24,1.373,0.717,0)

  par=matrix(25,4,data=pa,byrow=T)
  par<-data.frame(par)
  names(par)<-c("bio","a","b","g")

  dd=NULL

  if (biome %in% seq(0,24,1)){

    dd<-(par$a[par$bio==biome]*(H*CA)^par$b[par$bio==biome])*exp((par$g[par$bio==biome]^2)/2)
    return<-dd

  }else{

    stop("ERROR: wrong value of biome")

  }

}
