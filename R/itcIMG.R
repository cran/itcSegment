#' @title Individual Tree Crowns segmentation with imagery data
#' @author Michele Dalponte
#' @description The ITC delineation approach finds local maxima within an imagery, designates these as tree tops, then uses a decision tree method to grow individual crowns around the local maxima.
#' @param imagery An object of class raster on which to perform the segmentation. The image should be projected.
#' @param epsg The EPSG code of the reference system of the image.
#' @param searchWinSize Size (in pixels) of the moving window used to the detect the local maxima. It should be an odd number larger than 3.
#' @param TRESHSeed Growing threshold 1. It should be between 0 and 1.
#' @param TRESHCrown Growing threshold 2. It should be between 0 and 1.
#' @param DIST Maximum value of the crown diameter of a detected tree (in meters).
#' @param th Digital number value below which a pixel cannot be a local maxima.
#' @param ischm TRUE if the imagery is a Canopy Height Model (CHM). Default: FALSE.
#' @return An object of the class SpatVector containing the delineated ITCs. The informaion for each ITC contained in the data frame are the X and Y coordinates position of the tree, the tree height in meters (Height_m;  only if ischm=TRUE) and its crown area in square meters (CA_m2).
#' @import terra
#' @export itcIMG
#' @references M. Dalponte, F. Reyes, K. Kandare, and D. Gianelle, "Delineation of Individual Tree Crowns from ALS and Hyperspectral data: a comparison among four methods," European Journal of Remote Sensing, Vol. 48, pp. 365-382, 2015.
#' @examples
#' \dontrun{
#' library(terra)
#' library(itcSegment)
#'
#' imgData<-rast("./inst/extdata/imgData.tif")
#'
#' se<-itcIMG(imgData,epsg=32632)
#' summary(se)
#' plot(se,axes=T)
#'
#' ## to save the data use rgdal function called writeOGR. For more help see rgdal package.
#'
#' }

itcIMG<-function(imagery=NULL,epsg=NULL,searchWinSize=3,TRESHSeed=0.45, TRESHCrown=0.55, DIST=10, th=0, ischm=FALSE){

  if (searchWinSize>=3 & searchWinSize %% 2 !=0){

    imagery <- terra::focal(imagery, w=matrix(1,3,3), fun=function(x){mean(x,na.rm=T)})

    Max<-matrix(dim(imagery)[2],dim(imagery)[1],data=imagery[,],byrow=FALSE)

    Max<-Max[1:dim(imagery)[2],dim(imagery)[1]:1]

    Gnew<-Max
    Max[,]<-0
    Index<-Max
    Index[,]<-0

    Gnew[is.na(Gnew)]<-0
    Gnew[Gnew<th]<-0

    #--------Find Tree tops--------------------------------------------------------------------------------------------------

    index=1

    II<-which(Gnew!=0,arr.ind=T)
    dim(II)
    II<-II[which(II[,1]>=ceiling(searchWinSize/2)),]
    dim(II)
    II<-II[which(II[,1]<=dim(Gnew)[1]-ceiling(searchWinSize/2)),]
    dim(II)
    II<-II[which(II[,2]>=ceiling(searchWinSize/2)),]
    dim(II)
    II<-II[which(II[,2]<=dim(Gnew)[2]-ceiling(searchWinSize/2)),]
    dim(II)

    for (indexII in 1:dim(II)[1]){

      r=as.numeric(II[indexII,1])
      k=as.numeric(II[indexII,2])
      FIL<-matrix(searchWinSize,searchWinSize,data=NA)
      FIL<-Gnew[(r-floor(searchWinSize/2)):(r+floor(searchWinSize/2)),(k-floor(searchWinSize/2)):(k+floor(searchWinSize/2))]
      if (FIL[ceiling(searchWinSize/2),ceiling(searchWinSize/2)]==max(FIL,na.rm=T) & max(Max[(r-floor(searchWinSize/2)):(r+floor(searchWinSize/2)),(k-floor(searchWinSize/2)):(k+floor(searchWinSize/2))],na.rm=T)==0 & max(FIL,na.rm=T)!=0){
        Max[r,k]<-1
        Index[r,k]<-index
        index<-index+1
      }
    }

    Ntrees<-max(Index,na.rm=T)

    if (Ntrees>0){

      Cb<-imagery
      Mb<-imagery

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

        if (length(II)>0){

          for (indexII in 1:dim(II)[1]){

            r=as.numeric(II[indexII,1])
            k=as.numeric(II[indexII,2])

            if (r!=1 & r!=dim(Gnew)[1] & k!=1 & k!=dim(Gnew)[2]){

              ind<-Crowns[r,k]

              coordSeed<-which(Index==ind,arr.ind=TRUE)
              coordCrown<-which(Crowns==ind,arr.ind=TRUE)

              rvSeed<-Gnew[coordSeed]
              rvCrown<-mean(Gnew[coordCrown],na.rm=T)

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

              GFIL<-(filData[,3]!=0 & filData[,3]>(rvSeed*TRESHSeed) & (filData[,3]>(rvCrown*TRESHCrown)) & (filData[,3]<=(rvSeed+(rvSeed*0.05))) & (sqrt((coordSeed[1]-filData[,1])^2 + (coordSeed[2]-filData[,2])^2)<DIST))

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

      Cb<-imagery
      Mb<-imagery

      Cb[]<-as.numeric(Crowns[1:dim(Crowns)[1],dim(Crowns)[2]:1],byrow=TRUE)
      Mb[]<-as.numeric(Max[1:dim(Max)[1],dim(Max)[2]:1],byrow=TRUE)


      #----------------Write Shapefile----------------------------------------------------------------------------------

      # Convert to polygons
      m3.shp <- terra::as.polygons(Cb)
      names(m3.shp)<-"ID"

      HyperCrowns<-terra::subset(m3.shp,m3.shp$ID!=0)

      HyperCrowns$X<-round(terra::geom(HyperCrowns)[,3],2)
      HyperCrowns$Y<-round(terra::geom(HyperCrowns)[,4],2)

      if (ischm==T){HyperCrowns$Height_m<-round(terra::extract(imagery,HyperCrowns,fun=max)[,1],2)}

      HCbuf<-terra::buffer(HyperCrowns,width=-res(imagery)[1]/2)

      ITCcv<-terra::convHull(HCbuf,by="ID")

      ITCcv$X<-HCbuf$X
      ITCcv$Y<-HCbuf$Y
      if (ischm==T){ITCcv$Height_m<-HCbuf$Height_m}

      ITCcv$CA_m2<-round(terra::expanse(ITCcv, unit="m"),2)

      ITCcv<-terra::subset(ITCcv,ITCcv$CA_m2>1)
      crs(ITCcv)  <- paste("epsg:",epsg,sep="")

      if (exists("ITCcv")){

        return<-ITCcv

      }

    }
  }
  else{

    stop("ERROR: searchWinSize not correct")

  }

}



