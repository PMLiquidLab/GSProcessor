#' The Master Node
#' @param parameters.list a list containing the parameters. The possible ones are: 'considerAutoLoop' and 'threshold'. 'considerAutoLoop' is a boolean which indicates if the autoloops have to be admitted, while 'threshold' is the minimum value that a probability should have to do not be set to zero, in the transition matrix.
#' @import stringr XML zip jsonlite
#' @export
#'
masterNode <- function(lst.running.nodes , operative_folder.dir, masterDescriptionSkel, output_folder, 
                       masterScript = c(), nodeScript=c() ) {
  global.lst.running.nodes <- c()
  global.operative_folder.dir <- c()
  global.variables <- list()
  global.mtr.nodes <- c()
  global.masterScript <- c()
  global.nodeScript <- c()
  global.output_folder <- c()

  # --------------------------------------------------
  # Verifica le n cartelle di ingresso
  # Quando TUTTE sono piene, passa alla fase successiva
  # @toDo : 
  #   2 steps polling :  per evitare di pescare qualcosa che non e' ancora stabile
  # --------------------------------------------------
  checkInputFolder <- function( timeForSleepint = 2 ) {
    
    # Prendi l'array contenente tutte le input Folders che sono state passate 
    arr.input.folders <- unlist(lapply( names(global.lst.running.nodes), function(nodo) {    global.lst.running.nodes[[nodo]]$input.folder  } ))
    
    # cicla finche' non c'e' un token in TUTTE
    lst.ff <- list()
    while( length(unlist(lst.ff)) < length( global.lst.running.nodes )  ) {
      global.mtr.nodes <<-c()
      for( inputFolder in arr.input.folders ) {
        lst.ff[[ inputFolder ]] <- list.files( inputFolder )  
        node <- names(global.lst.running.nodes)[which(arr.input.folders == inputFolder)]
        if( length(lst.ff[[ inputFolder ]])  == 1) {
          global.mtr.nodes <<- rbind(global.mtr.nodes , c( "node" =   node, "path" = inputFolder,"file" = lst.ff[[ inputFolder ]]  ))  
        }
        if( length(lst.ff[[ inputFolder ]] ) > 1 ) {
          stop("ERROR: something went wrong... too many tokens in a folder")
        }
      }
      Sys.sleep(timeForSleepint)
    }
    # Esci per passare alla fase successiva. in 'global.mtr.nodes' c'e' una tabella che riassume cosa ha trovato in quale cartella, 
    # relativo a quale nodo
    return( lst.ff  )
  } 
  # --------------------------------------------------
  # Lancia il Processore del MASTER, colleziona i risulati, li impacchetta e li manda ai nodi
  # @toDo Spezzettare 
  #     la funzione in piu' sottofunzioni: questa fa troppe cose
  # --------------------------------------------------  
  doSomething <- function(  lst.ff ) {
    global.variables$iteration <<- global.variables$iteration + 1
    
    # copia i files in una cartella interna, rimuovendoli dalle input_folder
    destinationFolder <- paste(c(global.operative_folder.dir,"/log.received.input/",as.character(global.variables$iteration) ),collapse = '') 
    masterNode.outputPreparation <- paste(c(destinationFolder,"/master"),collapse = '') 
    dir.create(  destinationFolder )
    dir.create(  masterNode.outputPreparation  )
    for( i in 1:nrow(global.mtr.nodes)) {
      dir.create(  paste(c(destinationFolder,"/",global.mtr.nodes[i,"node"]),collapse = '') )
    }
    
    arr.folder.from.copy <- unlist(lapply(  names(lst.ff), function(nome){   paste(c( nome,"/",lst.ff[[nome]]),collapse = '')  } ))
    arr.folder.to.copy <- unlist(lapply(  names(lst.ff), function(nome){   
      nodo <- global.mtr.nodes[which(global.mtr.nodes[,"file"] == lst.ff[[nome]]), "node" ]
      paste(c( destinationFolder,"/",nodo,"/",lst.ff[[nome]]),collapse = '') 
    } ))
    arr.dest.extract.folder<- unlist(lapply(  names(lst.ff), function(nome){   
      nodo <- global.mtr.nodes[which(global.mtr.nodes[,"file"] == lst.ff[[nome]]), "node" ]
      paste(c( destinationFolder,"/",nodo,"/"),collapse = '') 
    } ))
    arr.nodi <- unlist(lapply(  names(lst.ff), function(nome){   
      global.mtr.nodes[which(global.mtr.nodes[,"file"] == lst.ff[[nome]]), "node" ]
    } ))
    
    # Ora scompattali
    for(i in 1:length(arr.folder.from.copy)) {
      file.copy(from = arr.folder.from.copy[i],to = arr.folder.to.copy[i])
      file.remove(arr.folder.from.copy[i])
      unzip(zipfile = arr.folder.to.copy[i], exdir =  arr.dest.extract.folder[i])
      file.remove(arr.folder.to.copy[i])
    }
    
    # Carica l'XML del MasterNode ed inizia a rimpiazzare i placeholder
    # Questo XML verra' poi passato al processore perche' svolga i calcoli di quanto in ingresso dai nodi
    tokenInstanceUID <- paste(c(format(Sys.time(), format = "%Y%m%d%I%M%S"),"_",runif(n = 1,0,100000)),collapse = '')
    creationDateTime <- format(Sys.time(), format = "%Y-%m-%d %I:%M:%S")
    inputFolderFromNodes <- paste(arr.dest.extract.folder,collapse = ",")
    
    fileConn <- file(gobal.masterDescriptionSkel)
    lst.lines <- readLines(fileConn)
    close(fileConn)
    XMLScript <- paste(lst.lines,collapse = "\n")
    
    XMLScript = str_replace(XMLScript, "##tokenInstanceUID##", tokenInstanceUID)
    XMLScript = str_replace(XMLScript, "##creationDateTime##", creationDateTime)
    XMLScript = str_replace(XMLScript, "##inputFolderFromNodes##", inputFolderFromNodes)
    XMLScript = str_replace(XMLScript, "##mainOutputFolder##", global.output_folder)
    XMLScript = str_replace(XMLScript, "##nodeNames##", paste(names(lst.running.nodes),collapse = ',') )   
    
    names(lst.running.nodes)
    # browser()
    # Crea la cartella dove il master potra' evventualmente mettere dei file aggiuntivi
    # arr.nodeInputAdditionalFile <- c()
    for( nodo in names(lst.running.nodes)) {
      dir.nodo <- paste(c(operative_folder.dir,"/workingDir/",nodo),collapse = '')
      dir.create(path =  dir.nodo )
      if( dir.exists( dir.nodo ) == FALSE) { stop("ERROR: workingDir/nodename not created") }
      # arr.nodeInputAdditionalFile <- c( arr.nodeInputAdditionalFile , lst.running.nodes[[nodo]]$nodeInputAdditionalFile)
    }
    # XMLScript = str_replace(XMLScript, "##nodeInputAdditionalFile##", paste(arr.nodeInputAdditionalFile,collapse = ','))   
    
    newDescriptionFileName <- paste(c( masterNode.outputPreparation,"/description.xml" ),collapse = '')
    fileConn <- file(newDescriptionFileName)
    lst.lines <- writeLines(XMLScript,fileConn)
    close(fileConn)
    
    # Ora devi preparare uno ZIP perche' il processore prende in ingresso un TOKEN zippato...
    arr.file.da.zippare <- c( paste(c(masterNode.outputPreparation,"/description.xml"),collapse = '') )
    # prendi il Masterscript che e' stato passato
    if( length(global.masterScript) > 0 ) {
      scriptMasterFileName <- substr(global.masterScript,(max(unlist(str_locate_all(  global.masterScript,"/")))+1),str_length(global.masterScript))  
      destinazione <- paste( c( masterNode.outputPreparation , "/", scriptMasterFileName),collapse = '')
      file.copy(from = global.masterScript , to = destinazione )
      arr.file.da.zippare <- c(arr.file.da.zippare , destinazione)
    }
    # zippa il contenuto della cartella di preparazione
    zipFile <- paste( c( operative_folder.dir,"input/token.",global.variables$iteration,".zip"), collapse = '')
    zipr(zipfile = zipFile, files = arr.file.da.zippare, recurse = TRUE, compression_level = 9)
    
    # prepariamoci a lanciare un processore
    objS <- bckGndProcessor( input_folder.dir = paste(c(operative_folder.dir,"/input/"),collapse = ''),
                             output_folder.dir = paste(c(operative_folder.dir,"/output/"),collapse = ''),
                             tmp_folder.dir = paste(c(operative_folder.dir,"/temp/"),collapse = ''),
                             sync_folder.dir = paste(c(operative_folder.dir,"/sync/"),collapse = ''),
                             override.repeated.tUID = TRUE, stopAfterExecution = 1
    )
    
    #browser()
    
    objS$start()
    #browser()
    # Se c'e' qualcosa nella output folder, significa che la computazione e' finita: ritorna'
    lst.ff <- check.output_folder()
    if( length(lst.ff) > 0 ) return()
    # browser()
    # Ora prendi il file che e' stato generato e fanne delle copie, contestuali ai diversi nodi
    file.in.uscita <- list.files(paste(c(operative_folder.dir,"/output/"),collapse = ''))
    file.in.uscitaFullName.from <- paste(c(operative_folder.dir,"/output/",file.in.uscita),collapse = '')
    file.in.uscitaFullName.to <- paste(c(operative_folder.dir,"/workingDir/",file.in.uscita),collapse = '')
    # spostalo nella workingDir
    file.copy(from = file.in.uscitaFullName.from,to = file.in.uscitaFullName.to)
    unlink(file.in.uscitaFullName.from)
    # unzippa
    unzip(zipfile = file.in.uscitaFullName.to,exdir = paste(c(operative_folder.dir,"/workingDir/"),collapse = ''))
    file.remove(file.in.uscitaFullName.to)
    
    # Siccome il description che e' arrivato contiene dei placeholder che dovranno essere valorizzati in maniera diversa in funzione dei nodi
    # di destinazione, caricalo
    fileConn<-file( paste(c(operative_folder.dir,"/workingDir/description.xml"),collapse = '') )
    arr.testone <- readLines(fileConn)
    close(fileConn)
    # browser()
    # E fai il REPLACE dei placeholders
    lst.testone <- list()
    arr.folder.to.zip <- c()
    arr.file.to.zip <- list()
    for( nodo in names(lst.running.nodes)) { 
      str.testone <- paste( arr.testone, collapse = "\n")
      tokenInstanceUID <- paste(c(format(Sys.time(), format = "%Y%m%d%I%M%S"),"_",runif(n = 1,0,100000)),collapse = '')
      creationDateTime <- format(Sys.time(), format = "%Y-%m-%d %I:%M:%S")
      nodeNameDataSource <- lst.running.nodes[[ nodo ]]$localDB
      nodeInputAdditionalFile <- lst.running.nodes[[ nodo ]]$nodeInputAdditionalFile
      str.testone <- str_replace_all(string =  str.testone,pattern = "##nodeName##",nodo)
      str.testone <- str_replace_all(string =  str.testone,pattern = "##tokenInstanceUID##",tokenInstanceUID)
      str.testone <- str_replace_all(string =  str.testone,pattern = "##creationDateTime##",creationDateTime)
      str.testone <- str_replace_all(string =  str.testone,pattern = "##nodeNameDataSource##",nodeNameDataSource)
      #str.testone <- str_replace_all(string =  str.testone,pattern = "##nodeName##",nodo)
      lst.testone[[nodo]] <- str.testone
      dir.nodo <- paste(c(operative_folder.dir,"/workingDir/",nodo),collapse = '')
      # dir.create(path =  dir.nodo )
      if( dir.exists( dir.nodo ) == FALSE) { stop("ERROR: workingDir/nodename not created") }
      description.xml.fileName <- paste(c(dir.nodo,"/description.xml"),collapse = '')
      fileConn<-file( description.xml.fileName )
      writeLines(str.testone , fileConn)
      close(fileConn)
      
      # copia altri file ancillari presenti
      tmpArrfile <- list.files(path =  paste(c(operative_folder.dir,"/workingDir/"),collapse = ''), pattern = paste(c(nodo,".*"),collapse = '') )
      tmpArrfile <- tmpArrfile[which(!tmpArrfile %in% c(nodo))]
      arr.ancillaryFile.to.zip <- c()
      for(fileToCopy in tmpArrfile ) { 
        file.copy(from = paste( c(operative_folder.dir,"/workingDir/",fileToCopy)  ,collapse = '' ),
                  to =   paste( c(operative_folder.dir,"/workingDir/",nodo),collapse = ''), )    
        # browser()
        arr.ancillaryFile.to.zip <- c( arr.ancillaryFile.to.zip , paste( c(operative_folder.dir,"/workingDir/",nodo,"/",fileToCopy),collapse = '')) 
      }
      
      indice <- paste(c(operative_folder.dir,"/workingDir/",nodo,".zip"),collapse = '')
      arr.file.to.zip[[ indice  ]] <- description.xml.fileName
      arr.file.to.zip[[ indice  ]]  <- c( arr.file.to.zip[[ indice  ]]  , arr.ancillaryFile.to.zip )
      
      if( length(global.nodeScript) > 0) {
        
        file.copy(from = global.nodeScript,to = dir.nodo)
        scriptNoderFileName <- substr(global.nodeScript,(max(unlist(str_locate_all(  global.nodeScript,"/")))+1),str_length(global.nodeScript))  
        arr.file.to.zip[[ indice  ]] <- c( arr.file.to.zip[[ indice  ]] , paste( c( dir.nodo,"/",scriptNoderFileName),collapse = ''))
      }
    }
    # browser()
    # ora leggi il contenuto delle cartelle e vedi di creare gli zip per i singoli nodi
    for( nomeFileZip in names(arr.file.to.zip)) {
      zipr(zipfile = nomeFileZip ,files = as.character(arr.file.to.zip[[nomeFileZip]]),  recurse = TRUE, compression_level = 9)
    }
    
    # copia i files nelle INPUT_FOLDER dei relativi nodi
    for( i in 1:length(lst.running.nodes)) {
      nodo <- names(lst.running.nodes)[i]
      file.copy(from = names(arr.file.to.zip)[i],to = lst.running.nodes[[nodo]]$output.folder)
      file.remove(names(arr.file.to.zip)[i])
    }
    
    #browser()
    
  }
  # --------------------------------------------------------------------------------------------------------------------------------
  # --------------------------------------------------------------------------------------------------------------------------------
  # --------------------------------------------------------------------------------------------------------------------------------
  old.doSomething <- function(  lst.ff ) {
    global.variables$iteration <<- global.variables$iteration + 1
    
    # copia i files in una cartella interna, rimuovendoli dalle input_folder
    destinationFolder <- paste(c(global.operative_folder.dir,"/log.received.input/",as.character(global.variables$iteration) ),collapse = '') 
    masterNode.outputPreparation <- paste(c(destinationFolder,"/master"),collapse = '') 
    dir.create(  destinationFolder )
    dir.create(  masterNode.outputPreparation  )
    for( i in 1:nrow(global.mtr.nodes)) {
      dir.create(  paste(c(destinationFolder,"/",global.mtr.nodes[i,"node"]),collapse = '') )
    }
    
    arr.folder.from.copy <- unlist(lapply(  names(lst.ff), function(nome){   paste(c( nome,"/",lst.ff[[nome]]),collapse = '')  } ))
    arr.folder.to.copy <- unlist(lapply(  names(lst.ff), function(nome){   
      nodo <- global.mtr.nodes[which(global.mtr.nodes[,"file"] == lst.ff[[nome]]), "node" ]
      paste(c( destinationFolder,"/",nodo,"/",lst.ff[[nome]]),collapse = '') 
    } ))
    arr.dest.extract.folder<- unlist(lapply(  names(lst.ff), function(nome){   
      nodo <- global.mtr.nodes[which(global.mtr.nodes[,"file"] == lst.ff[[nome]]), "node" ]
      paste(c( destinationFolder,"/",nodo,"/"),collapse = '') 
    } ))
    arr.nodi <- unlist(lapply(  names(lst.ff), function(nome){   
      global.mtr.nodes[which(global.mtr.nodes[,"file"] == lst.ff[[nome]]), "node" ]
    } ))
    
    # Ora scompattali
    for(i in 1:length(arr.folder.from.copy)) {
      file.copy(from = arr.folder.from.copy[i],to = arr.folder.to.copy[i])
      file.remove(arr.folder.from.copy[i])
      unzip(zipfile = arr.folder.to.copy[i], exdir =  arr.dest.extract.folder[i])
      file.remove(arr.folder.to.copy[i])
    }
    
    # Carica l'XML del MasterNode ed inizia a rimpiazzare i placeholder
    # Questo XML verra' poi passato al processore perche' svolga i calcoli di quanto in ingresso dai nodi
    tokenInstanceUID <- paste(c(format(Sys.time(), format = "%Y%m%d%I%M%S"),"_",runif(n = 1,0,100000)),collapse = '')
    creationDateTime <- format(Sys.time(), format = "%Y-%m-%d %I:%M:%S")
    inputFolderFromNodes <- paste(arr.dest.extract.folder,collapse = ",")
    
    fileConn <- file(gobal.masterDescriptionSkel)
    lst.lines <- readLines(fileConn)
    close(fileConn)
    XMLScript <- paste(lst.lines,collapse = "\n")
    
    XMLScript = str_replace(XMLScript, "##tokenInstanceUID##", tokenInstanceUID)
    XMLScript = str_replace(XMLScript, "##creationDateTime##", creationDateTime)
    XMLScript = str_replace(XMLScript, "##inputFolderFromNodes##", inputFolderFromNodes)
    XMLScript = str_replace(XMLScript, "##mainOutputFolder##", global.output_folder)   
    
    newDescriptionFileName <- paste(c( masterNode.outputPreparation,"/description.xml" ),collapse = '')
    fileConn <- file(newDescriptionFileName)
    lst.lines <- writeLines(XMLScript,fileConn)
    close(fileConn)
    
    # Ora devi preparare uno ZIP perche' il processore prende in ingresso un TOKEN zippato...
    arr.file.da.zippare <- c( paste(c(masterNode.outputPreparation,"/description.xml"),collapse = '') )
    # prendi il Masterscript che e' stato passato
    if( length(global.masterScript) > 0 ) {
      scriptMasterFileName <- substr(global.masterScript,(max(unlist(str_locate_all(  global.masterScript,"/")))+1),str_length(global.masterScript))  
      destinazione <- paste( c( masterNode.outputPreparation , "/", scriptMasterFileName),collapse = '')
      file.copy(from = global.masterScript , to = destinazione )
      arr.file.da.zippare <- c(arr.file.da.zippare , destinazione)
    }
    # zippa il contenuto della cartella di preparazione
    zipFile <- paste( c( operative_folder.dir,"input/token.",global.variables$iteration,".zip"), collapse = '')
    zipr(zipfile = zipFile, files = arr.file.da.zippare, recurse = TRUE, compression_level = 9)
    
    # prepariamoci a lanciare un processore
    objS <- bckGndProcessor( input_folder.dir = paste(c(operative_folder.dir,"/input/"),collapse = ''),
                             output_folder.dir = paste(c(operative_folder.dir,"/output/"),collapse = ''),
                             tmp_folder.dir = paste(c(operative_folder.dir,"/temp/"),collapse = ''),
                             sync_folder.dir = paste(c(operative_folder.dir,"/sync/"),collapse = ''),
                             override.repeated.tUID = TRUE, stopAfterExecution = 1
    )
    
    objS$start()
    
    # Se c'e' qualcosa nella output folder, significa che la computazione e' finita: ritorna'
    lst.ff <- check.output_folder()
    if( length(lst.ff) > 0 ) return()
    # browser()
    # Ora prendi il file che e' stato generato e fanne delle copie, contestuali ai diversi nodi
    file.in.uscita <- list.files(paste(c(operative_folder.dir,"/output/"),collapse = ''))
    file.in.uscitaFullName.from <- paste(c(operative_folder.dir,"/output/",file.in.uscita),collapse = '')
    file.in.uscitaFullName.to <- paste(c(operative_folder.dir,"/workingDir/",file.in.uscita),collapse = '')
    # spostalo nella workingDir
    file.copy(from = file.in.uscitaFullName.from,to = file.in.uscitaFullName.to)
    unlink(file.in.uscitaFullName.from)
    # unzippa
    unzip(zipfile = file.in.uscitaFullName.to,exdir = paste(c(operative_folder.dir,"/workingDir/"),collapse = ''))
    file.remove(file.in.uscitaFullName.to)
    
    # Siccome il description che e' arrivato contiene dei placeholder che dovranno essere valorizzati in maniera diversa in funzione dei nodi
    # di destinazione, caricalo
    fileConn<-file( paste(c(operative_folder.dir,"/workingDir/description.xml"),collapse = '') )
    arr.testone <- readLines(fileConn)
    close(fileConn)
    
    # E fai il REPLACE dei placeholders
    lst.testone <- list()
    arr.folder.to.zip <- c()
    arr.file.to.zip <- list()
    for( nodo in names(lst.running.nodes)) { 
      str.testone <- paste( arr.testone, collapse = "\n")
      tokenInstanceUID <- paste(c(format(Sys.time(), format = "%Y%m%d%I%M%S"),"_",runif(n = 1,0,100000)),collapse = '')
      creationDateTime <- format(Sys.time(), format = "%Y-%m-%d %I:%M:%S")
      nodeNameDataSource <- lst.running.nodes[[ nodo ]]$localDB
      str.testone <- str_replace_all(string =  str.testone,pattern = "##nodeName##",nodo)
      str.testone <- str_replace_all(string =  str.testone,pattern = "##tokenInstanceUID##",tokenInstanceUID)
      str.testone <- str_replace_all(string =  str.testone,pattern = "##creationDateTime##",creationDateTime)
      str.testone <- str_replace_all(string =  str.testone,pattern = "##nodeNameDataSource##",nodeNameDataSource)
      lst.testone[[nodo]] <- str.testone
      dir.nodo <- paste(c(operative_folder.dir,"/workingDir/",nodo),collapse = '')
      dir.create(path =  dir.nodo )
      if( dir.exists( dir.nodo ) == FALSE) { stop("ERROR: workingDir/nodename not created") }
      description.xml.fileName <- paste(c(dir.nodo,"/description.xml"),collapse = '')
      fileConn<-file( description.xml.fileName )
      writeLines(str.testone , fileConn)
      close(fileConn)
      
      indice <- paste(c(operative_folder.dir,"/workingDir/",nodo,".zip"),collapse = '')
      arr.file.to.zip[[ indice  ]] <- description.xml.fileName
      
      if( length(global.nodeScript) > 0) {
        
        file.copy(from = global.nodeScript,to = dir.nodo)
        scriptNoderFileName <- substr(global.nodeScript,(max(unlist(str_locate_all(  global.nodeScript,"/")))+1),str_length(global.nodeScript))  
        arr.file.to.zip[[ indice  ]] <- c( arr.file.to.zip[[ indice  ]] , paste( c( dir.nodo,"/",scriptNoderFileName),collapse = ''))
      }
    }
    
    # ora leggi il contenuto delle cartelle e vedi di creare gli zip per i singoli nodi
    for( nomeFileZip in names(arr.file.to.zip)) {
      zipr(zipfile = nomeFileZip ,files = as.character(arr.file.to.zip[[nomeFileZip]]),  recurse = TRUE, compression_level = 9)
    }
    
    # copia i files nelle INPUT_FOLDER dei relativi nodi
    for( i in 1:length(lst.running.nodes)) {
      nodo <- names(lst.running.nodes)[i]
      file.copy(from = names(arr.file.to.zip)[i],to = lst.running.nodes[[nodo]]$output.folder)
      file.remove(names(arr.file.to.zip)[i])
    }
    
  }
  # --------------------------------------------------------------------------------------------------------------------------------
  # --------------------------------------------------------------------------------------------------------------------------------
  # --------------------------------------------------------------------------------------------------------------------------------  
  check.output_folder <- function( ) {
    a <- list.files(global.output_folder)
    return(a)
  }
  start <- function() {
    lst.ff <- check.output_folder()
    if( length(lst.ff) > 0 ) return()
    
    for( i in 1:10 ) {
      lst.ff <- checkInputFolder()
      doSomething( lst.ff )
      
      # verifica se e' stato messo qualcosa nella global.output_folder, se si, ferma
      lst.ff <- check.output_folder()
      if( length(lst.ff) > 0 )break
    }
  }
  cleanFolders <- function( tmpFolder = TRUE ) {
    if( tmpFolder == TRUE ) {
      folder <- paste( global.operative_folder.dir$path )
      unlink(folder,recursive = TRUE)
      dir.create( folder ) 
      dir.create( paste(c(folder,"/log.received.input"),collapse = '') )
      dir.create( paste(c(folder,"/input"),collapse = '') )
      dir.create( paste(c(folder,"/output"),collapse = '') )
      dir.create( paste(c(folder,"/temp"),collapse = '') )
      dir.create( paste(c(folder,"/sync"),collapse = '') )
      dir.create( paste(c(folder,"/workingDir"),collapse = '') )
      dir.create( paste(c(folder,"/outGoingDir"),collapse = '') )
    }
  }
  # -----------------------------------------------------------
  # Costruttore
  # -----------------------------------------------------------
  constructor <- function( lst.running.nodes, operative_folder.dir,  masterDescriptionSkel, masterScript,  nodeScript, output_folder) {
    
    global.lst.running.nodes <<- lst.running.nodes
    global.operative_folder.dir <<- list("path" = operative_folder.dir)
    global.variables$iteration <<- 0
    gobal.masterDescriptionSkel <<- masterDescriptionSkel
    global.masterScript <<- masterScript
    global.nodeScript <<- nodeScript
    global.output_folder <<- output_folder
    cleanFolders( tmpFolder = TRUE )
  }
  # ===========================================================
  constructor( lst.running.nodes = lst.running.nodes ,
              operative_folder.dir = operative_folder.dir, masterDescriptionSkel = masterDescriptionSkel,
              masterScript = masterScript, nodeScript = nodeScript , output_folder = output_folder)
  # ===========================================================
  
  return( list(
    "start"=start
  ))
  
}