#'Discussion with Nicolas & Camille to select the right variables, take as much as possible
#'Important info
#'CITES status
#'IUCN status
#'Preferred temperature
#'Niveau trophique
#'Generation time
#'Résilience
#'Catégorie de prix

#### Function for: Fishbase.org data extraction

get_fishbase_data <- function(x="Regalecus-glesne"){
  
  #url2 <- paste("http://www.fishbase.de/summary/",x,".html",sep="")
  url2 <- paste("http://www.fishbase.se/summary/",x,".html",sep="")
  c <- htmlParse(getURLContent(url2, followlocation=TRUE))
  link_list <- getHTMLLinks(c, externalOnly = TRUE, xpQuery = "//a/@href", baseURL = docName(c))
  
  if(length(link_list) == 0){
    stop(paste(x, " is not an accepted name in fishbase, check for spelling mistakes and/or synonyms", sep = ""))
  }
  
  
  a1 <-getNodeSet(c, "//div ")
  a <- getNodeSet(c, "//span ")
  rm(c)
  
  if (length(a)!=0){
    d <- xmlValue(a[[which.max(sapply(lapply(a,xmlValue), function(x){regexec(pattern="Ecology", x)[[1]][1]}))+2]])
    m <- regmatches(d,gregexpr(pattern= "[-[:alpha:]]+;", d))
    m1 <- regmatches(d,gregexpr(pattern= "[[:alpha:]]+", d))[[1]]
    m <- gsub(";","",unlist(m))
    
    List_env1 <- c("Marine","Freshwater","brackish")
    List_env2 <- c("bathydemersal", "bathypelagic", "benthopelagic","benthopelagic.","demersal","demersal.",
                   "pelagic", "pelagic-neritic", "pelagic-oceanic", "reef-associated")
    clim <- c("Tropical","Temperate","Boreal","Subtropical","Deep-water") 
    
    env1 <- paste(m[which(is.element(m,List_env1)==T)],collapse="_")
    
    env2 <- m1[which(is.element(m1,List_env2)==T)]  
    env2_1 <-  m[which(is.element(m,List_env2)==T)]  
    
    
    
    climate <- m1[which(is.element(m1,clim)==T)]  
    
    if(length(env2)==0) {
      env2 <- env2_1}
    if(length(env2)==0) {env2 <- NA}
    
    if(length(env1)==0) {env1 = NA}
    if(length(climate)==0) {climate = NA}
    
    temp <- regmatches(d,gregexpr(pattern= "[0-9]+°C", d))
    if(length(temp[[1]])==0){
      temp_max <- temp_min <-NA
    } else{
      temp_min <-as.numeric(sub(pattern="°C",replacement="",temp[[1]][1]))
      temp_max <-as.numeric(sub(pattern="°C",replacement="",temp[[1]][2]))
    } # end of else
    
    depth_max <- as.numeric(gsub( pattern="[[:space:]]m",replacement="",unlist(regmatches(d,gregexpr(pattern= "[0-9]+ m", d)))[1]))  	
    depth_min <- as.numeric(gsub(pattern="[[:space:]]-",replacement="",unlist(regmatches(d,gregexpr(pattern= "[0-9]+ -", d)))[1]))
    
    depth_max_us <- as.numeric(gsub( pattern="[[:space:]]m",replacement="",unlist(regmatches(d,gregexpr(pattern= "[0-9]+ m", d)))[2]))  	
    depth_min_us <-as.numeric(gsub(pattern="[[:space:]]-",replacement="",unlist(regmatches(d,gregexpr(pattern= "[0-9]+ -", d)))[2]))
    
    
    b <- xmlValue(a[[which.max(sapply(lapply(a,xmlValue), function(x){regexec(pattern="Max length", x)[[1]][1]}))]])
    rm(a)
    
    Max_length <- substr(b, regexec(pattern="Max length", b)[[1]][1]+13, regexec(pattern="Max length", b)[[1]][1]+22)
    Max_length <-  strsplit(Max_length,split=" ")[[1]][1]
    
    if(sum(grep(pattern=",",Max_length))==1){Max_length = sub (pattern = ",",replacement = "",Max_length) }
    
    Common_length = substr(b, regexec(pattern="common length", b)[[1]][1]+16, regexec(pattern="common length", b)[[1]][1]+23)
    #Common_length =strsplit( Common_length,split=" ")[[1]][1]
    
    Common_length = gsub("[[:alpha:]]", "", gsub("[[:space:]]*", "", Common_length, perl=TRUE), perl=TRUE)
    
    if(sum(grep(pattern=",",Common_length))==1){Common_length = sub(pattern = ",",replacement = "",Common_length) }
    if(Common_length=="y:" | Common_length==":&") {Common_length <- NA}
    
    ##### Resilience 
    w <- which(sapply(lapply(a1,xmlValue), function(x){regexec(pattern="Resilience", x)[[1]][1]})>0)
    if(length(w)==0){
      Resilience <- NA
    } else {
      d1       <- xmlValue(a1[[w[length(w)]]])
      Res <- regmatches(d1,gregexpr(pattern= "[[:alpha:]]+,", d1))
      Resilience <- sub(pattern=",",replacement="",Res[[1]][1])
    } # end of ifesle
    
    # Vulnérabilité
    w_Vul <- which(sapply(lapply(a1,xmlValue), function(x){regexec(pattern="Vulnerability", x)[[1]][1]})>0)
    if(length(w_Vul)==0){
      Vul <- NA
    } else {
      d1_Vul <- xmlValue(a1[[w_Vul[length(w_Vul)]]])
      Vul <- strsplit(d1_Vul,split=":" )[[1]][2]
      Vul <- regmatches(Vul, gregexpr(pattern= "[[:digit:]]",Vul ))
      if(length(Vul)>5){
        Vul=100
      } else{
        Vul=as.numeric(paste(Vul[[1]][1],Vul[[1]][2],sep=""))
      } # end of ifesle
    } # end of ifesle
    
    ##### Price category
    w_Price <-which(sapply(lapply(a1,xmlValue), function(x){regexec(pattern="Price category", x)[[1]][1]})>0)
    if(length(w_Price)==0){
      Price=NA
    } else {
      d1_Price <- xmlValue(a1[[w_Price[length(w_Price)]]])
      int <- strsplit(d1_Price,":")[[1]][2]
      Price <- unlist(regmatches(int, gregexpr(pattern= "[[:alpha:]]+.",int )))
      if(length(Price)>1){
        Price[1] <- sub(" ","",Price[1])
        Price[2] <- sub(pattern="[[:punct:]]",replacement="",Price[2])
        Price <- paste(Price[1],Price[2],sep="_")
      } else {
        Price <- sub(" ","",Price)
        Price <- sub(pattern="[[:punct:]]",replacement="",Price)
      } # end of ifelse	
    } # end of ifelse
    
    #### Repro
    Repro_link <- link_list[grep("FishReproSummary",link_list)]
    Repro_link <-Repro_link[which.max(nchar(unique(Repro_link)))]
    Repro_link <- gsub("..","",Repro_link,fixed=T)
    
    url_end <- paste ("http://www.fishbase.se/",Repro_link,sep="")
    tt <- getURLContent(url_end, followlocation=TRUE, .encoding="CE_UTF8")
    res.t <- readHTMLTable(tt,header=TRUE,colClasses=NULL,skip.rows=integer(),
                           stringsAsFactors=FALSE,trim=TRUE,elFun=xmlValue,
                           as.data.frame=TRUE,which=integer())$dataTable
    
    if(is.null(res.t)){
      Repro.Mode = NA
      Repro.Fertil = NA
      Repro.MatingT = NA
      Repro.SpawnFreq = NA
      Repro.ParentCare = NA
    } else {
      Repro.Mode = res.t[which(res.t[,1] == "Mode"),2] # Mode
      Repro.Fertil = res.t[which(res.t[,1] == "Fertilization"),2] # Fertilization
      Repro.MatingT = res.t[which(res.t[,1] == "Mating type"),2] # Mating type
      Repro.SpawnFreq = res.t[which(res.t[,1] == "Spawning frequency"),2] # Spawning frequency
      Repro.ParentCare = res.t[which(res.t[,1] == "Parental Care"),2] # Parental Care
      
      if(Repro.Mode == "") {Repro.Mode = NA}
      if(Repro.Fertil == "") {Repro.Fertil = NA}
      if(Repro.MatingT == "") {Repro.MatingT = NA}
      if(Repro.SpawnFreq == "") {Repro.SpawnFreq = NA}
      if(Repro.ParentCare == "") {Repro.ParentCare = NA}
    }
    
    #### Eggs
    
    Eggs_link <- link_list[grep("FishEggInfoSummary",link_list)]
    Eggs_link <-Eggs_link[which.max(nchar(unique(Eggs_link)))]
    Eggs_link <- gsub("..","",Eggs_link,fixed=T)
    
    url_egg <- paste ("http://www.fishbase.se/",Eggs_link,sep="")
    tt <- getURLContent(url_egg, followlocation=TRUE, .encoding="CE_UTF8")
    res.t <- tryCatch(readHTMLTable(tt,header=TRUE,colClasses=NULL,skip.rows=integer(),
                                    stringsAsFactors=FALSE,trim=TRUE,elFun=xmlValue,
                                    as.data.frame=TRUE,which=integer())[[1]], error=function(e) paste("Error: Eggs is absent for ", x, sep=""))
    
    if(is(res.t, "character")){
      Egg.Shape = NA
      Egg.Attrib = NA
      Egg.Color = NA
    } else {
      Egg.Shape = res.t[which(res.t[,1] == "Shape of Egg"),2] # Shape of Egg
      Egg.Attrib = res.t[which(res.t[,1] == "Attributes"),2] # Attributes
      Egg.Color = res.t[which(res.t[,1] == "Color of Eggs"),2] # Color of Eggs
      if(Egg.Shape == "") {Egg.Shape = NA}
      if(Egg.Attrib == "") {Egg.Attrib = NA}
      if(Egg.Color == "") {Egg.Color = NA}
    }
    
    
    #### Larvae
    Larvae_link <- link_list[grep("LarvaeInfoList",link_list)]
    Larvae_link <-Larvae_link[which.max(nchar(unique(Larvae_link)))]
    Larvae_link <- gsub("..","",Larvae_link,fixed=T)
    
    # url_Larvae <- paste ("http://www.fishbase.org/",Larvae_link,sep="")
    url_Larvae <- paste ("http://www.fishbase.se/",Larvae_link,sep="")
    tt <- getURLContent(url_Larvae, followlocation=TRUE, .encoding="CE_UTF8")
    res.t <- readHTMLTable(tt,header=TRUE,colClasses=NULL,skip.rows=integer(),
                           stringsAsFactors=FALSE,trim=TRUE,elFun=xmlValue,
                           as.data.frame=TRUE,which=integer())
    
    if(length(res.t) == 0){
      Larvae.LenghtAtBirth = rep(NA, 3)
      names(Larvae.LenghtAtBirth) = c("Larvae.LengthAtBirth.mm.Max", "Larvae.LengthAtBirth.mm.Min", 		"Larvae.LengthAtBirth.mm.Mode")
      Larvae.PlaceDevelop = NA
      names(Larvae.PlaceDevelop) = c("Larvae.PlaceOfDevelopment")
      Larvae.YolkSac = NA
      names(Larvae.YolkSac) = c("Larvae.YolkSac")
    } else {
      
      # Larvae.LenghtAtBirth
      Larvae.LenghtAtBirth = unlist(lapply(seq(1, length(res.t)), function(x){
        if(length(grep("Length at birth", res.t[[x]])) > 0){
          res.t[[x]][grep("Length at birth", res.t[[x]][,1]),]
        }
      }))[2:4]
      if(is.null(Larvae.LenghtAtBirth)){
        Larvae.LenghtAtBirth = rep(NA, 3)
        names(Larvae.LenghtAtBirth) = c("Larvae.LengthAtBirth.mm.Max", "Larvae.LengthAtBirth.mm.Min", 		"Larvae.LengthAtBirth.mm.Mode")
      } else {
        options(warn=-1)
        Larvae.LenghtAtBirth = as.numeric(as.character(Larvae.LenghtAtBirth))
        options(warn=0)
        names(Larvae.LenghtAtBirth) = c("Larvae.LengthAtBirth.mm.Max", "Larvae.LengthAtBirth.mm.Min", 		"Larvae.LengthAtBirth.mm.Mode")
      }
      
      # Place of development
      Larvae.PlaceDevelop =  unlist(lapply(seq(1, length(res.t)), function(x){
        if(length(grep("Place of development", res.t[[x]])) > 0){
          res.t[[x]][grep("Place of development", res.t[[x]][,1]),]
        }
      }))
      if(is.null(Larvae.PlaceDevelop)){
        Larvae.PlaceDevelop = NA
        names(Larvae.PlaceDevelop) = c("Larvae.PlaceOfDevelopment")
      } else {
        names(Larvae.PlaceDevelop) = c("Larvae.PlaceOfDevelopment")
      }
      
      # Yolk-sac
      Larvae.YolkSac =  unlist(lapply(seq(1, length(res.t)), function(x){
        if(length(grep("Yolk-sac", res.t[[x]])) > 0){
          res.t[[x]][grep("Yolk-sac", res.t[[x]][,1]),2]
        }
      }))[2]
      if(is.null(Larvae.YolkSac)){
        Larvae.YolkSac = NA
        names(Larvae.YolkSac) = c("Larvae.YolkSac")
      } else {
        names(Larvae.YolkSac) = c("Larvae.YolkSac")
      }
    } # End if(length(res.t) == 0){
    
    
    ##### IUCN Status
    w_IUCN  <-which(sapply(lapply(a1,xmlValue), function(x){regexec(pattern="IUCN", x)[[1]][1]})>0)
    if(length(w_IUCN)==0){ 
      IUCN_status=NA
    } else {
      d1_IUCN  <- xmlValue(a1[[w_IUCN[length(w_IUCN)]]])
      IUCN <- unlist(regmatches(d1_IUCN,gregexpr(pattern= "[[:alpha:]]+)", d1_IUCN)))
      IUCN_status <- sub(pattern="[[:punct:]]",replacement="",IUCN[1] ) 
    } # end of ifelse 
    
    ##### Trophic level
    w_TL <- which(sapply(lapply(a1,xmlValue), function(x){regexec(pattern="Troph", x)[[1]][1]})>0)
    if(length(w_TL)==0){
      Trophic_Level=NA; TL_var=NA
    } else {
      d1_TL <- xmlValue(a1[[w_TL[length(w_TL)]]])
      TL <- unlist(regmatches(d1_TL,gregexpr(pattern= "[0-9].[0-9]", d1_TL)))
      TL_var <- TL[3] 	
      Trophic_Level <- TL[2]	
    }# end of ifelse
    
    test = rfishbase::ecology(version="19.04")
    
    rm(a1) 
    
    ##### Length-weight   
    if(length(grep (pattern = "LW",link_list))==0){
      
      Length_weight = data.frame(a=NA,b=NA,Length_type=NA,n=NA)
      
    } else {
      link_url <-  substr(link_list [[grep (pattern = "LW",link_list)]],2,nchar(link_list [[grep (pattern = "LW",link_list)]]))  
      url_end <- paste ("http://www.fishbase.org/", link_url,sep="")
      
      tt <- getURLContent(url_end, followlocation=TRUE, .encoding="CE_UTF8")
      res <- readHTMLTable(tt,header=TRUE,colClasses=NULL,skip.rows=integer(),
                           stringsAsFactors=FALSE,trim=TRUE,elFun=xmlValue,
                           as.data.frame=TRUE,which=integer())[[3]]
      
      selected_length <- names(which.max(table(res[,"Lengthtype"])))
      
      res <- res[which(res$Lengthtype==selected_length),]
      res$n <- as.character(gsub("\u00A0","",res$n))
      
      if(sum(res$n=="")>=1) {res <- res[-which(res$n==""),]}
      
      n <- sum(as.numeric(as.character(res$n)))
      a <- mean(as.numeric(as.character(res[,"a"])),na.rm=TRUE)
      b <- mean(as.numeric(as.character(res[,"b"])),na.rm=TRUE)
      
      Length_weight <- data.frame(a=a,b=b,Length_type=selected_length,n=n)
    }  	
    
    res <- data.frame(c(Max_length= as.numeric(Max_length),Common_length=Common_length,
                        Depth_min=depth_min,Depth_max=depth_max,Depth_min_us=depth_min_us,
                        Depth_max_us=depth_max_us,Env_1=env1,Env_2=env2,Climate=climate,
                        Price=Price,Length_weight,Resilience=Resilience,Vul=Vul,
                        Trophic_Level=Trophic_Level,IUCN_status=IUCN_status,
                        Temp_min=temp_min,Temp_max=temp_max, Repro.Mode=Repro.Mode, Repro.Fertil=Repro.Fertil,
                        Repro.MatingT=Repro.MatingT, Repro.SpawnFreq=Repro.SpawnFreq, 
                        Repro.ParentCare=Repro.ParentCare, Egg.Shape=Egg.Shape, Egg.Attrib=Egg.Attrib,
                        Egg.Color=Egg.Color, Larvae.LenghtAtBirth, Larvae.PlaceDevelop, Larvae.YolkSac))
    
    rownames(res)=x
    
  } else {
    
    Length_weight <- data.frame(a="A_verifier",b="A_verifier",Length_type="A_verifier",n="A_verifier")
    Max_length=Common_length=Resilience=depth_max=depth_min=env_1=env_2=depth_min_us=Climate=climate=depth_max_us=Price=Trophic_Level=IUCN_status=IUCN_status=temp_min=temp_max=Vul= "A_verifier"
    
    res <- data.frame(c(Max_length= Max_length,Common_length=Common_length,Depth_min=depth_min,Depth_max=depth_max,Depth_min_us=depth_min_us,Depth_max_us=depth_max_us,Env_1=env1,Env_2=env2,Climate=climate,Price=Price,Length_weight,
                        Resilience=Resilience,Vul=Vul,Trophic_Level=Trophic_Level,IUCN_status=IUCN_status,Temp_min=temp_min,Temp_max=temp_max, Repro.Mode=Repro.Mode, Repro.Fertil=Repro.Fertil, Repro.MatingT=Repro.MatingT, Repro.SpawnFreq=Repro.SpawnFreq, Repro.ParentCare=Repro.ParentCare, Egg.Shape=Egg.Shape, Egg.Attrib=Egg.Attrib, Egg.Color=Egg.Color, Larvae.LenghtAtBirth, Larvae.PlaceDevelop, Larvae.YolkSac))
    rownames(res)=x  
  }
  
  return(res)
  
}