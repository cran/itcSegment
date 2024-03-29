#' @title Individual Tree Crowns segmentation with LiDAR data
#' @author Michele Dalponte
#' @description The ITC delineation approach finds local maxima within a rasterized canopy height model (CHM), designates these as tree tops, then uses a decision tree method to grow individual crowns around the local maxima. The approach goes through the following steps: (1) a low-pass filter is applied to the rasterized CHM to smooth the surface and reduce the number of local maxima; (2) local maxima are located using a moving window with size that adapts inside a user defined range (minimum and maximum size) according the pixel height; a pixel of the CHM is labelled as local maxima if its z value is greater than all other z values in the window, and with z greater than some minimum height above-ground; (3) each local maximum is labelled as an 'initial region' around which a tree crown can grow; the heights of the four neighboring pixels are extracted from the CHM and these pixels are added to the region if their vertical distance from the local maximum is less than some user-defined percentage of the local-maximum height, and less than some user-defined maximum difference; this procedure is repeated for all the neighbors of cells now included in the region, and so on iteratively until no further pixels are added to the region; (4) from each region that had been identified the first-return ALS points are extracted (having first removed low elevation points), (5) a 2D convex hull is applied to these points, and the resulting polygons becomes the final ITCs.
#' @param X A column vector of x coordinates.
#' @param Y A column vector of y coordinates (it must have the same length as X).
#' @param Z A column vector of z coordinates (it must have the same length as X). Z must be normalized respect to the ground.
#' @param epsg The EPSG code of the reference system of the X,Y coordinates.
#' @param resolution The resolution of the raster on which the first segmentation is carried out.
#' @param MinSearchFilSize Minimum size (in pixels) of the moving window used to the detect the local maxima. It should be an odd number larger than 3.
#' @param MaxSearchFilSize Maximum size (in pixels) of the moving window used to the detect the local maxima. It should be bigger or equal to MinSearchFilSize, and it should be an odd number larger than 3.
#' @param TRESHSeed Growing threshold 1. It should be between 0 and 1.
#' @param TRESHCrown Growing threshold 2. It should be between 0 and 1.
#' @param minDIST Minimum value of the crown diameter of a detected tree (in meters).
#' @param maxDIST Maximum value of the crown diameter of a detected tree (in meters). It should be bigger or equal to minDIST.
#' @param HeightThreshold Minimum height of the trees.
#' @param cw Weighting exponent used to increase the contrast in the CHM used to detect the local maxima (default cw=1).
#' @return An object of the class SpatVector containing the delineated ITCs. The informaion for each ITC contained in the data frame are the X and Y coordinates position of the tree, the tree height in meters (Height_m) and its crown area in square meters (CA_m2).
#' @import terra
#' @export itcLiDAR
#' @references M. Dalponte, and D. A. Coomes, "Tree-centric mapping of forest carbon density from airborne laser scanning and hyperspectral data," Methods in Ecology and Evolution, Vol. 7, No. 10, pp. 1236-1245, 2016.
#' @examples
#' \dontrun{
#' data(lasData)
#'
#' ## function takes a while to run
#' se<-itcLiDAR(lasData$X,lasData$Y,lasData$Z,epsg=32632)
#' summary(se)
#' plot(se,axes=T)
#'
#' ## If we want to seperate the height of the trees by grayscales:
#'
#' plot(se,col=gray((max(se$Height_m)-se$Height_m)/(max(se$Height_m)-min(se$Height_m))),axes=T)
#'
#' ## to save the data use rgdal function called writeOGR. For more help see rgdal package.
#'
#' }

itcLiDAR<-function(X=NULL,Y=NULL,Z=NULL, epsg=NULL , resolution=0.5, MinSearchFilSize=3, MaxSearchFilSize=7,TRESHSeed=0.55,TRESHCrown=0.6,minDIST=5,maxDIST=40,HeightThreshold=2,cw=1){


  filtro<-function(x){
    if (is.na(x[5])){
      if (length(which(!is.na(x)))>6){
        MOD<<-1
      }
      return<-mean(x,na.rm=T)
    }else{
      return<-x[5]
    }
  }

  otsu <- function(y){

    m=1

    yvals <- sort(unique(y))
    L <- length(yvals)
    per <- as.vector(table(y)) / length(y)

    P <- matrix(0, nrow=L, ncol=L)
    S <- matrix(0, nrow=L, ncol=L)
    H <- matrix(0, nrow=L, ncol=L)

    P[1,] <- cumsum(per)
    S[1,] <- cumsum(per * yvals[1:L])
    for(u in 2:L)
      for(v in u:L){
        P[u,v] <- P[1,v] - P[1,u-1]
        S[u,v] <- S[1,v] - S[1,u-1]
      }
    H <- S^2 / P


    x <- seq(L)
    n <- length(x)
    if (n -1 < m)
      stop("The number of thresholds is larger than the unique values minus 1.")
    e <- 0
    h <- m
    a <- 1:m
    rule <- c(0, a, L)
    sigma2 <- sum(sapply(1:(m+1), function(i) H[rule[i]+1, rule[i+1]]))

    thresh <- yvals[a]
    nmmp1 <- n - m + 1
    mp1 <- m + 1
    while (a[1] != nmmp1) {
      if (e < n - h) {
        h <- 1
        e <- a[m]
        j <- 1
      }
      else {
        h <- h + 1
        e <- a[mp1 - h]
        j <- 1:h
      }
      a[m - h + j] <- e + j
      if(a[m] != L){
        rule <- c(0, a, L)
        new <- sum(sapply(1:(m+1), function(i) H[rule[i]+1, rule[i+1]]))
        if(new > sigma2){
          sigma2 <- new
          thresh <- yvals[a]
        }
      }
    }
    thresh
  }

  if (MinSearchFilSize>=3 & MaxSearchFilSize>=3){

    if (MinSearchFilSize %% 2 != 0){

      if(MaxSearchFilSize %% 2 != 0){

        if (minDIST<=maxDIST){

          if (MinSearchFilSize<=MaxSearchFilSize){

            if (length(X)==length(Y) & length(X)==length(Z)){

              LAS1<-cbind(X,Y,Z)
              LAS1<-data.frame(LAS1)
              names(LAS1)<-c("X","Y","Z")

              H99<-max(Z)

              stepsSearchFilSize<-seq(MinSearchFilSize,MaxSearchFilSize,2)
              thSearchFilSize<-seq(from = HeightThreshold, to = H99, length.out=length(stepsSearchFilSize))

              stepsDIST<-exp(seq(log(minDIST),log(maxDIST),length.out=maxDIST-minDIST))
              stepsHeightDIST<-seq(from = HeightThreshold, to = H99, length.out=length(stepsDIST))

              HH<-terra::rast(resolution=resolution,xmin=min(X),xmax=max(X),ymin=min(Y),ymax=max(Y))
              terra::crs(HH)<-paste("epsg:",epsg,sep="")

              LAS1_vect<-terra::vect(as.matrix(LAS1[,c(1,2)]))
              terra::crs(LAS1_vect)<-paste("epsg:",epsg,sep="")
              LAS1_vect$Z<-LAS1$Z

              H<-terra::rasterize(x=LAS1_vect,y=HH,field="Z",fun=function(x,...){max(x,na.rm=T)})

              MOD<-1

              while(MOD==1){
                MOD<-0
                H <- terra::focal(H, w=matrix(1,3,3), fun=filtro)
              }

              H <- terra::focal(H, w=matrix(1,3,3), fun=function(x){mean(x^cw,na.rm=T)})

              rm(MOD)
              gc()

              if (ncell(H)!=length(which(values(is.na(H))))){

                Max<-matrix(dim(H)[2],dim(H)[1],data=H[,],byrow=FALSE)

                Max<-Max[1:dim(H)[2],dim(H)[1]:1]

                Gnew<-Max
                Max[,]<-0
                Index<-Max
                Index[,]<-0

                Gnew[is.na(Gnew)]<-0
                Max[is.na(Max)]<-0
                Index[is.na(Index)]<-0

                #Put to 0 heights below HeightThreshold
                Gnew[Gnew<HeightThreshold]<-0

                #--------Find Tree tops--------------------------------------------------------------------------------------------------

                index=1

                II<-which(Gnew!=0,arr.ind=T)
                if (length(II)>3){
                  II<-II[which(II[,1]>=ceiling(MinSearchFilSize/2)),]
                  if (length(II)>3){
                    II<-II[which(II[,1]<=dim(Gnew)[1]-ceiling(MinSearchFilSize/2)),]
                    if (length(II)>3){
                      II<-II[which(II[,2]>=ceiling(MinSearchFilSize/2)),]
                      if (length(II)>3){
                        II<-II[which(II[,2]<=dim(Gnew)[2]-ceiling(MinSearchFilSize/2)),]
                      }
                    }
                  }
                }

                if (dim(II)[1]>3){

                  for (indexII in 1:dim(II)[1]){

                    r=as.numeric(II[indexII,1])
                    k=as.numeric(II[indexII,2])

                    pos<-findInterval(Gnew[r,k],thSearchFilSize)
                    if (pos==0){pos=1}

                    SearchFilSize<-stepsSearchFilSize[pos]

                    FIL<-matrix(SearchFilSize,SearchFilSize,data=NA)
                    minR<-(r-floor(SearchFilSize/2))
                    if (minR<1){
                      minR=1
                    }
                    minC<-(k-floor(SearchFilSize/2))
                    if (minC<1){
                      minC=1
                    }
                    maxR<-(r+floor(SearchFilSize/2))
                    if (maxR>dim(Gnew)[1]){
                      maxR=dim(Gnew)[1]
                    }
                    maxC<-(k+floor(SearchFilSize/2))
                    if (maxC>dim(Gnew)[2]){
                      maxC=dim(Gnew)[2]
                    }

                    FIL<-Gnew[minR:maxR,minC:maxC]

                    if (Gnew[r,k]==max(FIL,na.rm=T) & Gnew[r,k]!=0){
                      Max[r,k]<-1
                      Index[r,k]<-index
                      index<-index+1
                    }

                  }

                  Ntrees<-max(Index)


                  if (Ntrees>0){

                    Cb<-H
                    Mb<-H

                    Cb[]<-as.numeric(Gnew[1:dim(Gnew)[1],dim(Gnew)[2]:1],byrow=TRUE)
                    Mb[]<-as.numeric(Max[1:dim(Max)[1],dim(Max)[2]:1],byrow=TRUE)

                    #------------------------------------------------------------------------------------------------------------------------------------------

                    Crowns<-Index

                    OldCrowns<-Crowns

                    Check<-OldCrowns
                    Check[,]<-0

                    filsize<-3

                    Niter<-100

                    it=1

                    while (it==1){

                      it=0

                      II<-which(Crowns!=0 & Check==0,arr.ind=T)

                      if (length(II)>3){

                        for (indexII in 1:dim(II)[1]){

                          r=as.numeric(II[indexII,1])
                          k=as.numeric(II[indexII,2])

                          if (r!=1 & r!=dim(Gnew)[1] & k!=1 & k!=dim(Gnew)[2]){

                            ind<-Crowns[r,k]

                            coordSeed<-which(Index==ind,arr.ind=TRUE)
                            coordCrown<-which(Crowns==ind,arr.ind=TRUE)

                            rvSeed<-Gnew[coordSeed]
                            rvCrown<-mean(Gnew[coordCrown])

                            posD<-findInterval(rvSeed,stepsHeightDIST)
                            DIST<-stepsDIST[posD]

                            filData<-matrix(4,3,data=0)
                            filData[1,1]<-r-1
                            filData[1,2]<-k
                            filData[1,3]<-Gnew[r-1,k]
                            filData[2,1]<-r
                            filData[2,2]<-k-1
                            filData[2,3]<-Gnew[r,k-1]
                            filData[3,1]<-r
                            filData[3,2]<-k+1
                            filData[3,3]<-Gnew[r,k+1]
                            filData[4,1]<-r+1
                            filData[4,2]<-k
                            filData[4,3]<-Gnew[r+1,k]

                            GFIL<-((filData[,3] < (Gnew[r,k]+Gnew[r,k]*0.005)) & (filData[,3]>(rvSeed*TRESHSeed)) & (filData[,3]>(rvCrown*TRESHCrown)) & (filData[,3]<=(rvSeed+(rvSeed*0.05))) & (sqrt((coordSeed[1]-filData[,1])^2 + (coordSeed[2]-filData[,2])^2)<DIST))

                            filData<-filData[GFIL,]

                            if (length(filData)>3){

                              for (pp in 1:dim(filData)[1]){

                                rr<-filData[pp,1]
                                kk<-filData[pp,2]

                                if(Crowns[rr,kk]==0 & Gnew[rr,kk]!=0 ){

                                  Crowns[rr,kk]<-Crowns[r,k]
                                  it<-1

                                }
                              }
                            }
                          }

                        }
                      }

                      Check<-OldCrowns
                      OldCrowns<-Crowns

                    }

                    Cb<-H
                    Mb<-H

                    Cb[]<-as.numeric(Crowns[1:dim(Crowns)[1],dim(Crowns)[2]:1],byrow=TRUE)
                    Mb[]<-as.numeric(Max[1:dim(Max)[1],dim(Max)[2]:1],byrow=TRUE)


                    #----------------Write Shapefile----------------------------------------------------------------------------------

                    # Convert to polygons
                    m3.shp <- terra::as.polygons(Cb)
                    names(m3.shp)<-"ID"

                    ITCoriginal<-terra::subset(m3.shp,m3.shp$ID!=0)

                    #------------------------------------------------

                    ITC<-ITCoriginal

                    LASsp<-terra::vect(as.matrix(LAS1[,c(1,2)]))
                    terra::crs(LASsp)<-paste("epsg:",epsg,sep="")

                    initializator<-1
                    uid<-1

                    for (indexITC in 1: dim(ITCoriginal)[1]){

                      o <- relate(ITCoriginal[indexITC,], LASsp, "intersects")[1,]

                      LF<-LAS1[o==T & LAS1$Z>HeightThreshold,]

                      if(dim(LF)[1]>2){

                        if (length(unique(LF$Z))>1){

                          MaxPoints<-300
                          he<-0
                          if (length(LF$Z)>MaxPoints){
                            he<-sort(LF$Z)[seq(1,length(LF$Z),ceiling(length(LF$Z)/MaxPoints))]
                          }else{
                            he<-LF$Z
                          }

                          if (length(unique(he))>3){
                            OT<-otsu(he)
                          }else{
                            OT<-0
                          }

                          if (length(he[he>OT])<10){
                            OT<-0
                          }

                          LF2<-0
                          LF2<-LF[LF$Z>=OT,]

                          sp2<-0
                          sp2<-terra::convHull(vect(as.matrix(LF2[,c(1,2)])))
                          terra::crs(sp2)<-paste("epsg:",epsg,sep="")

                          sp2$ID<-ITCoriginal$ID[indexITC]
                          sp2$X<-LF2$X[which.max(LF2$Z)]
                          sp2$Y<-LF2$Y[which.max(LF2$Z)]
                          sp2$Height_m<-max(LF2$Z)
                          sp2$CA_m2<-round(terra::expanse(sp2, unit="m"),2)

                          if (initializator==1){
                            poly.data<-sp2
                            initializator<-0
                          }else{
                            poly.data <- rbind(poly.data,sp2)
                          }
                        }
                      }
                    }

                    if (exists("poly.data")){

                      return<-poly.data

                    }

                  }
                }

              }
            }
            else{

              stop("ERROR: X Y Z lengths differs")

            }

          }
          else{

            stop("ERROR: MinSearchFilSize bigger than MaxSearchFilSize")

          }
        }
        else{

          stop("ERROR: minDIST bigger than maxDIST")

        }
      }
      else{

        stop("ERROR: MaxSearchFilSize is not an odd number")

      }
    }
    else{

      stop("ERROR: MaxSearchFilSize is not an odd number")

    }
  }
  else{

    stop("ERROR: MaxSearchFilSize or MisSerchFilSize smaller than 3")

  }

}




