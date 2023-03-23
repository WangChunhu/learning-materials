read_apsim_met  <-  function(file, src.dir = ".", verbose = TRUE){
  #' @param file apsim met file 
  
  if(!grepl("[Mm][Ee][Tt]$",file)) stop("file should have a .met extension")
  
  file.path  <-  file.path(src.dir,file)
  
  ## Read the header
  header  <-  scan(file = file.path, 
                   what = "character", 
                   sep = "\n",
                   blank.lines.skip = FALSE,
                   nlines = 100, 
                   quiet = TRUE)
  ## hdrl is for keeping track of header lines
  hdrl  <-  0; skip.lines  <-  0 
  ## attrs  <-  c("name","site","latitude","longitude","tav","amp","clnms","clunits")
  name  <-  NULL; site  <-  NULL; latitude  <-  NULL; longitude  <-  NULL; 
  tav  <-  NULL; amp  <-  NULL; clnms  <-  NULL; clunits  <-  NULL; comments  <-  NULL
  constants  <-  vector(mode = "list",30); constant.count  <-  0; fnd  <-  FALSE
  comment.lines  <-  0
  ## This is as ugly as it gets but so are met files
  for(i in 1:100){
    if(grepl("^!",header[i])){comment.lines  <-  comment.lines + 1; next}
    if(grepl("[weather.met.weather]",header[i],fixed=TRUE)){name  <-  header[i];hdrl  <-  hdrl + 1; fnd  <-  TRUE}
    if(grepl("^site",header[i],ignore.case=TRUE)){site  <-  header[i];hdrl  <-  hdrl + 1; fnd  <-  TRUE}
    if(grepl("^latitude",header[i],ignore.case=TRUE)){latitude  <-  header[i];hdrl  <-  hdrl + 1; fnd  <-  TRUE} 
    if(grepl("^longitude",header[i],ignore.case=TRUE)){longitude  <-  header[i];hdrl  <-  hdrl + 1; fnd  <-  TRUE} 
    if(grepl("^tav",header[i])){tav  <-  header[i];hdrl  <-  hdrl + 1; fnd  <-  TRUE}
    if(grepl("^amp",header[i])){amp  <-  header[i];hdrl  <-  hdrl + 1; fnd  <-  TRUE}
    if(grepl("year",header[i]) && grepl("radn",header[i])){clnms  <-  header[i];hdrl  <-  hdrl + 1; fnd  <-  TRUE}
    if(grepl("()",header[i],fixed=TRUE)){clunits  <-  header[i];skip.lines  <-  i;hdrl  <-  hdrl + 1; fnd  <-  TRUE}
    if(grepl("=",header[i],fixed=TRUE) && fnd == FALSE){
      constant.count  <-  constant.count + 1
      constants[constant.count]  <-  header[i]
      hdrl  <-  hdrl + 1
    } 
    fnd  <-  FALSE
  }
  
  constants  <-  unlist(constants[1:constant.count])
  
  if(constant.count == 0){
    constants  <-  NA
  }
  
  if(verbose){
    cat("Found ",hdrl," header lines \n")
    cat("Found ",comment.lines," comment lines \n")
    cat("Found ",skip.lines," skip lines \n")
    cat("Found ",constant.count,"constants \n")
  }
  
  ## I only check the first 6 column names but there might be more
  clnms  <-  sub("^\\s+","",clnms)
  clnms.s  <-  strsplit(clnms,"\\s+")[[1]]
  if(sum(clnms.s %in% c("year","day","radn","maxt","mint","rain")) < 6){
    cat("All column names:",clnms,"\n") 
    warning("column names might be wrong")
  }
  
  clunits  <-  sub("^\\s+","",clunits)
  clunits.s  <-  strsplit(clunits,"\\s+")[[1]]
  ## Sounds like there is no point in checking units
  ## As they are a complete mess
  
  met  <-  utils::read.table(file = file.path, 
                             header = FALSE, 
                             as.is = TRUE,
                             na.strings = c(NA,-99),
                             comment.char = "!", 
                             col.names = clnms.s,
                             skip = skip.lines)
  
  attr(met, "filename")  <-  file
  attr(met, "site")  <-  ifelse(is.null(site),NA,site)
  attr(met, "latitude")  <-  latitude
  attr(met, "longitude")  <-  ifelse(is.null(longitude),NA,longitude)
  attr(met, "tav")  <-  tav
  attr(met, "amp")  <-  amp
  attr(met, "colnames")  <-  clnms.s
  attr(met, "units")  <-  clunits.s
  attr(met, "constants")  <-  constants
  attr(met, "comments")  <-  ifelse(is.null(comments),NA,comments)
  class(met)  <-  c("met","data.frame")
  return(met)
}