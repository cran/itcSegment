#' @title Aboveground biomass prediction using height and crown diameter
#' @author Michele Dalponte
#' @description Prediction of aboveground biomass (AGB) using height and crown diameter and the equations of Jucker et al. (2017).
#' @param H Tree height in meters.
#' @param CA Crown diameter in meters.
#' @param species Integer number indicating the species group:
#'
#' 1 = gymnosperm
#'
#' 2 = angiosperm
#'
#' @return The AGB value in kilograms.
#' @import methods
#' @import grDevices
#' @export agb
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
#' #AGB prediction
#' se$agb<-NA
#' se$agb<-agb(se$Height_m,se$CD_m,species=1)
#'
#' summary(se)
#'
#' }
#'
agb<-function(H=NULL,CA=NULL,species=1){

  d=NULL

  par=matrix(2,3,data=c(1,0.093,-0.223,2,0,0),byrow=T)
  par<-data.frame(par)
  names(par)<-c("sp","a","b")

  if (species %in% c(1,2)){

    dd<-((0.016+par$a[par$sp==species])*(H*CA)^(2.013+par$b[par$sp==species]))*exp((0.204^2)/2)
    return<-dd

  }else{

    stop("ERROR: wrong value of biome")

  }

}
