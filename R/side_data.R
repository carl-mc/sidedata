#' @title SIDE Data Package
#' @description Facilitates access to the SIDE data.
#' 
#' @seealso \url{url-to-paper}; \url{https://github.com/carl-mc/side_data}, \url{icr.ethz.ch/side}
#'
#' @author Carl Müller-Crepon and Philipp Hunziker
#' @name side_data
#' @docType package
#' @import
#' raster
#' utils




####################################
# FUNCTIONS 
####################################


#' @title Load SIDE metadata
#' @description Function to load meta data of the SIDE data set
#' @param path Specify path. Default refers to csv file located in package directory.
side_metadata <- function(path = "package/extdata/side_metadata.csv"){
  path <- ifelse(!is.null(path),path,system.file("extdata", "side_metadata.csv", package = "sidedata"))
  return(read.csv(file = path, header = T, stringsAsFactors = F))
}

#' @title Load codebook for SIDE meta data
#' @description Function to open the codebook for the metadata
side_metadata_codebook <- function(){
  
}


#' @title Download a set of SIDE maps
#'
#' @description Facilitates bulk-download of SIDE data from \url{icr.ethz.ch/side}.
#' 
#' @param sideid Vector of sideid as listed in `side_metadata()`. 
#' @param mapid Vector of mapid as listed in `side_metadata()`. 
#' @param country Vector of country names as specified in `side_metadata()`
#' @param year Vector of country codes as specified in `side_metadata()`
#' @param dhs.round Vector of DHS rounds as specified in `side_metadata()`
#' @param dhs.subround Vector of DHS subrounds as specified in `side_metadata()`
#' @param groupname Vector of group names as specified in `side_metadata()`
#' @param marker Type of group; one of `c("ethnic","religion","ethnic.religion")`
#' @param dest.dir Destination directory. If `conv.hull = TRUE`, a subdirectory `dest.dir/conv_hull/` will be created in which raster files of the convex hulls are saved. 
#' @param overwrite Overwrite existing SIDE Data
#' @param conv.hull Download convex hull of DHS points used for interpolation?
#' @param side.metadata.df Meta data set of SIDE, results from `side_metadata()`. You can speed up things a little bit, if you load that only once.
#' 
#' @details Specify the subset of SIDE maps you want to download using the above parameters. 
#' The inner join of all parameters determines which maps are downloaded.
#' 
side_download <- function(sideid = c(), mapid = c(), country = c(), year = c(), 
                          dhs.round = c(), dhs.subround = c(), groupname = c(), marker = c(),
                          dest.dir = getwd(), overwrite = FALSE, conv.hull = FALSE, side.metadata.df = side_metadata()){
  # Server url
  server.url <- "icr.ethz.ch:/side/side_v1/"
  
  # Destination directory
  dest.dir <- ifelse(substr(dest.dir, nchar(dest.dir),nchar(dest.dir)) == "/",
                     substr(dest.dir, 1,nchar(dest.dir)-1), dest.dir)
  
  # Metadata
  side.metadata.df <- side_metadata()
  
  # Subset
  subset.ls <- list(sideid = sideid,  mapid = mapid,
                    country = country, year = year, 
                 dhs.round = dhs.round, dhs.subround = dhs.subround,
                 groupname = groupname, marker = marker)
  for(i in c(1:length(subset.ls))){
    if(length(subset.ls[[i]]) > 0){
      side.metadata.df <- side.metadata.df[side.metadata.df[,names(subset.ls)[i]] %in% subset.ls[[i]],]
    }
  }
  
  # Make path
  side.metadata.df$path <- paste0(side.metadata.df$sideid, ".asc")
  
  # Check whether already in dest.folder
  exist <- list.files(dest.dir)
  if(!overwrite){
    exist <- exist[exist %in% side.metadata.df$path]
    if(length(exist) > 0){
      print(paste0(length(exist)," files already exist..."))
      side.metadata.df <- side.metadata.df[!side.metadata.df$path %in% exist,]
    }
  } else {
    exist <- exist[exist %in% side.metadata.df$path]
    print("Overwriting ",paste0(length(exist)," files..."))
  }
  
  # Print number of downloads:
  print(paste0("Downloading ", nrow(side.metadata.df), " SIDE maps"))
  
  # Download
  lapply(side.metadata.df$path, function(f){
    download.file(url = paste0(server.url, f),
                  destfile = paste0(dest.dir,"/", f))
  })
  
  # Download convex hull
  if(conv.hull){
    print(paste0("Downloading ",length(unique(side.metadata.df$mapid))," convex hull(s)"))
    dir.create(paste0(dest.dir,"/conv_hull"))
    conv.hull.stub <- "conv_hull/side_v1_convhull_"
    lapply(unique(side.metadata.df$mapid), function(f){
      download.file(url = paste0(server.url,conv.hull.stub, f, ".asc"),
                    destfile = paste0(dest.dir,conv.hull.stub,"/", f, ".asc"))
    })
  }
  
  # Return
  return(TRUE)
}
  

#' @title Open a set of SIDE maps
#'
#' @description Facilitates the loading of SIDE data.
#' 
#' @param sideid Vector of sideid.
#' @param mapid Single mapid as listed in `side_metadata()`. 
#' @param country Single country
#' @param year Single year
#' @param dhs.round Single DHS round
#' @param dhs.subround Single DHS subroud; needed if multiple DHS survey in the same country and survey wave
#' @param groupname Name of group
#' @param marker Type of group; one of `c("ethnic","religion","ethnic.religion")`
#' @param source.dir Directory in which SIDE data is stored
#' @param conv.hull Download convex hull of DHS points used for interpolation?
#' @param side.metadata.df Meta data set of SIDE, results from `side_metadata()`. You can speed up things a little bit, if you load that only once.
#' 
#' @details Specify the subset of SIDE maps you want to load using the above parameters. 
#' To guarantee that maps form a raster-stack, parameters 
#' must specify a subset of maps that is based on a single DHS round from one country and year.
#' The inner join of all parameters determines which maps are loaded. 
#' 
side_load <- function(sideid = c(), mapid = NULL, country = NULL, 
                      year = NULL, dhs.round = NULL, dhs.subround = NULL, groupname = c(), marker = c(),
                      source.dir = getwd(), conv.hull = FALSE, side.metadata.df = side_metadata()){
  # Need the raster package here..
  require(raster)
  
  # Source directory
  source.dir <- ifelse(substr(source.dir, nchar(source.dir),nchar(source.dir)) == "/",
                     substr(source.dir, 1,nchar(source.dir)-1), source.dir)
  
  # Subset
  subset.ls <- list(sideid = sideid, mapid = mapid,
                    country = country, year = year, 
                    dhs.round = dhs.round, dhs.subround = dhs.subround,
                    groupname = groupname, marker = marker)
  for(i in c(1:length(subset.ls))){
    if(length(subset.ls[[i]]) > 0){
      side.metadata.df <- side.metadata.df[side.metadata.df[,names(subset.ls)[i]] %in% subset.ls[[i]],]
    }
  }
  
  # Check whether this is one mapid only
  if(length(unique(side.metadata.df$mapid)) > 1){
    stop("Maps from more than one country, year. or DHS round selected")
  } else if(nrow(side.metadata.df) == 0){
    warning("No map selected, returning NULL")
    return(NULL)
  }
  
  # Make path
  side.metadata.df$path <- paste0(side.metadata.df$sideid, ".asc")
  
  if(!conv.hull){
    # Load ethnic side maps
    
    # Check whether all maps in source folder
    not.exist <- side.metadata.df$path[!side.metadata.df$path %in% list.files(source.dir)]
    if(length(not.exist) > 0){
      warning("Some files are missing: ")
      warning(paste(not.exist, collapse = ", "))
      side.metadata.df <- side.metadata.df[!side.metadata.df$path %in% not.exist,]
    }
    
    # Load
    print(paste0(source.dir,"/",side.metadata.df$path))
    stack <- stack(paste0(source.dir,"/",side.metadata.df$path))
    
    # Name stack
    names(stack) <- side.metadata.df$sideid
    
    # Return
    return(stack)
  } else { 
    # Load convex Hull
    path <- paste0(source.dir,"/conv_hull/side_v1_convhull_",unique(side.metadata.df$mapid),".asc")
    raster <- try(raster(path))
    if(class(raster) == "try-error"){
      stop(paste0("Convex hull raster not found at: ",path) )
    } else {
      # Return
      return(raster)
    }
  }
}

#' @title Query SIDE meta-data matching a set of SIDE maps
#'
#' @description Function to query SIDE metadata that matches a named SIDE raster
#'
#' @param side.raster any `raster*` object named with existing sideid(s) 
#' @param side.metadata.df Meta data set of SIDE, results from `side_metadata()`. 
#' You can speed up things a little bit, if you load that only once.
sidemap2data <- function(side.raster, side.metadata.df = side_metadata()){
  # Need the raster package here..
  require(raster)
  
  # Names of side raster stack
  query.names <- names(side.raster)
  
  # Query data
  meta.df <- do.call(rbind, lapply(query.names, 
                                   function(n){side.metadata.df[side.metadata.df$sideid == n,]}))
  
  # Return
  return(meta.df)
}
