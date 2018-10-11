#' @title SIDE Data Package
#' @description Facilitates access to the SIDE data.
#' 
#' @seealso \url{https://side.ethz.ch}; \url{link-to-paper}; \url{https://github.com/carl-mc/sidedata}
#'
#' @author Carl Müller-Crepon and Philipp Hunziker
#' @name sidedata
#' @docType package
#' @import
#' raster
#' utils




####################################
# FUNCTIONS 
####################################


#' @title Load SIDE metadata
#' @description Function to load meta data of the SIDE data set
#' @export
side_metadata <- function(){
  return(sidedata::side.metadata.df)
}

#' @title Load codebook for SIDE meta data
#' @description Function to open the codebook for the metadata
side_metadata_codebook <- function(){
  
}


#' @title Download a set of SIDE maps
#'
#' @description Facilitates bulk-download of SIDE data from \url{icr.ethz.ch/data/side}.
#' 
#' @param sideid Vector of sideid as listed in \code{side_metadata()}. 
#' @param mapid Vector of mapid as listed in \code{side_metadata()}. 
#' @param country Vector of country names as specified in \code{side_metadata()}. 
#' @param year Vector of country codes as specified in \code{side_metadata()}. 
#' @param dhs.round Vector of DHS rounds as specified in \code{side_metadata()}. 
#' @param dhs.subround Vector of DHS subrounds as specified in \code{side_metadata()}. 
#' @param groupname Vector of group names as specified in \code{side_metadata()}. 
#' @param marker Type of group; one of \code{c("ethnic","religion","ethnic.religion")}
#' @param dest.dir Destination directory. If \code{conv.hull = TRUE}, a subdirectory \code{"dest.dir/conv_hull/"} will be created in which raster files of the convex hulls are saved. 
#' @param overwrite Overwrite existing SIDE Data
#' @param conv.hull Download convex hull of DHS points used for interpolation?
#' 
#' @details Specify the subset of SIDE maps you want to download using the above parameters. 
#' The inner join of all parameters determines which maps are downloaded.
#' 
#' @export
side_download <- function(sideid = c(), mapid = c(), country = c(), year = c(), 
                          dhs.round = c(), dhs.subround = c(), groupname = c(), marker = c(),
                          dest.dir = getwd(), overwrite = FALSE, conv.hull = FALSE){
  # Server url
  server.url <- "https://side.ethz.ch/raw/v1/"
  
  # Destination directory
  dest.dir <- ifelse(substr(dest.dir, nchar(dest.dir),nchar(dest.dir)) == "/",
                     substr(dest.dir, 1,nchar(dest.dir)-1), dest.dir)
  
  # Metadata
  side.metadata.df <- sidedata::side.metadata.df
  
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
    }
    main.metadata.df <- side.metadata.df[!side.metadata.df$path %in% exist,]
  } else {
    exist <- exist[exist %in% side.metadata.df$path]
    main.metadata.df <- side.metadata.df
    print(paste0("Overwriting ",length(exist)," files..."))
  }
  
  # Download
  if(nrow(main.metadata.df) == 0){
    print("No files to download.")
  } else {
    print(paste0("Downloading ", nrow(main.metadata.df), " SIDE maps from side.ethz.ch/raw/v1/"))
    
    # Make Progress Bar
    pb <- txtProgressBar(min = 0, max = length(main.metadata.df$path), style = 3)
    
    # Download
    res <- lapply(1:length(main.metadata.df$path), function(f){
      r <- download.file(url = paste0(server.url, main.metadata.df$path[f]),
                         destfile = paste0(dest.dir,"/", main.metadata.df$path[f]),
                         quiet = T)
      setTxtProgressBar(pb, f)
      return(r)
    })
    
    # Close progress bar
    close(pb)
  }
  
  
  # Download convex hull
  if(conv.hull){
    
    # Make Convex Hull directory
    conv.dir <- paste0(dest.dir, "/conv_hull/")
    if(!dir.exists(conv.dir)){
      dir.create(conv.dir)
    }
    
    # File prefix
    con.prefix <- "side_v1_convhull_"
    
    # Unique mapids 
    uni.map.ids <- unique(side.metadata.df$mapid)
    
    # Files
    conv.files <- paste0(con.prefix, uni.map.ids, ".asc")
    
    # Check whether already in dest.folder
    exist <- list.files(conv.dir)
    if(!overwrite){
      exist <- exist[exist %in% conv.files]
      if(length(exist) > 0){
        print(paste0(length(exist)," convex hull(s) already exist..."))
        conv.files <- conv.files[!conv.files%in% exist]
      }
    } else {
      exist <- exist[exist %in% conv.files]
      print(paste0("Overwriting ",length(exist)," convex hull(s)..."))
    }
    
    # Downlaod
    if(length(conv.files) > 0){
      
      # Message
      print(paste0("Downloading ",length(conv.files)," convex hull(s) from https://side.ethz.ch/raw/v1/conv_hull/"))
      
      # Make Progress Bar
      pb <- txtProgressBar(min = 0, max = length(conv.files), style = 3)
      
      # Download
      conv.hull.stub <- "conv_hull/side_v1_convhull_"
      res <- lapply(1:length(conv.files), function(f){
        r <- download.file(url = paste0(server.url,"conv_hull/", conv.files[f]),
                           destfile = paste0(conv.dir, conv.files[f]),
                           quiet = T)
        setTxtProgressBar(pb, f)
        return(r)
      })
      
      # Close progress bar
      close(pb)
    } else {
      print("No convex hulls to download.")
    }
  }
    
  
  # Return
  return(TRUE)
}
  

#' @title Load a set of SIDE maps into environment
#'
#' @description Facilitates the loading of SIDE data.
#' 
#' @param sideid Vector of sideid.
#' @param mapid Single mapid as listed in \code{side_metadata()}. 
#' @param country Single country as listed in \code{side_metadata()}. 
#' @param year Single year as listed in \code{side_metadata()}. 
#' @param dhs.round Single DHS round as listed in \code{side_metadata()}. 
#' @param dhs.subround Single DHS subroud as listed in \code{side_metadata()}. ; needed if multiple DHS survey in the same country and survey wave
#' @param groupname Name of group as listed in \code{side_metadata()}. 
#' @param marker Type of group; one of \code{c("ethnic","religion","ethnic.religion")}
#' @param source.dir Directory in which SIDE data is stored
#' @param conv.hull Load convex hull of DHS points used for interpolation?
#' 
#' @details Specify the subset of SIDE maps you want to load using the above parameters. 
#' To guarantee that maps form a raster-stack, parameters 
#' must specify a subset of maps that is based on a single DHS round from one country and year.
#' The inner join of all parameters determines which maps are loaded. 
#' 
#' @export
side_load <- function(sideid = c(), mapid = NULL, country = NULL, 
                      year = NULL, dhs.round = NULL, dhs.subround = NULL, groupname = c(), marker = c(),
                      source.dir = getwd(), conv.hull = FALSE){
  
  # Load Metadata
  side.metadata.df <- sidedata::side.metadata.df
  
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
    stack <- raster::stack(paste0(source.dir,"/",side.metadata.df$path))
    
    # Name stack
    names(stack) <- side.metadata.df$sideid
    
    # Return
    return(stack)
  } else { 
    # Load convex Hull
    path <- paste0(source.dir,"/conv_hull/side_v1_convhull_",unique(side.metadata.df$mapid),".asc")
    raster <- try(raster::raster(path))
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
#' @param side.raster any Raster* object named with existing sideid(s) 
#' 
#' @return data.frame with the metadata for the bands of \code{side.raster}.
#' 
#' @export
sidemap2data <- function(side.raster){
  
  # Load Metadata
  side.metadata.df <- sidedata::side.metadata.df
  
  # Names of side raster stack
  query.names <- names(side.raster)
  
  # Query data
  if(all(grepl("convhull",query.names))){
    query.vars <- c("mapid", "cowcode", "year", "dhs.round", "dhs.subround", "sample.size", "fit.comb", "tps.par.knn", "tps.par.sphkm",
                    "tps.par.knncut", "tps.par.unsurv", "tps.fit", "knn.par.knn", "knn.par.sphkm", "knn.par.weights", "knn.par.unsurv", 
                    "knn.par.knncut", "knn.fit")
    meta.df <- do.call(rbind, lapply(query.names, 
                                     function(n){
                                       unique(side.metadata.df[side.metadata.df$mapid == as.numeric(strsplit(n, "_", fixed = T)[[1]][4]),query.vars])
                                     }))
    
  } else if(all(!grepl("convhull",query.names))){
    meta.df <- do.call(rbind, lapply(query.names, 
                                     function(n){side.metadata.df[side.metadata.df$sideid == n,]}))
    
  } else {
    stop("Please check proper naming of raster bands. Also, do not mix group rasters and convex hulls for this query.")
  }

  # Return
  return(meta.df)
}
