
#' Processor class
#'
#' @description class for instantiating a processor running in background
#' @param input_folder.dir Processor's input folder. This folder is constantly monitored and whenever a token is found processing is started.
#' @param output_folder.dir Processor's output folder. Processor saves in the output_folder.dir the output token containing the processing results.
#' @param tmp_folder.dir Processor's temporary folder. tmp_folder.dir is a staging folder where input token are unzipped and processing output are created.
#' @param sync_folder.dir Processor's synchronization folder. Processor uses sync_folder.dir to log the start and the end of all the processing that handles.
#' @param log_folder.dir Processor's log folder. Processor saves in the log_folder.dir all the execution logs associated with the processes that it is handling.
#' @param cache_folder.dir Processor's log folder. Processor saves in the cache_folder.dir info about the current run.
#' @param override.repeated.tUID If TRUE if two processes have the same unique ID, temporary files and logs are overwritten. If FALSE ???. By default override.repeated.tUID = FALSE.
#' @param stopAfterExecution Number of process handled after which the Processor stops. By default it is set to Inf.
#' @import stringr XML zip jsonlite
#' @examples
#' objS <- bckGndProcessor(input_folder.dir = "./inputFolder/",
#'                         output_folder.dir = "./outputFolder/",
#'                         tmp_folder.dir = "./tmpFolder/",
#'                         sync_folder.dir = "./syncFolder/",
#' )
#' objS$start()

#' @export
#'

library(zip)
library(zipR)
library(stringr)

bckGndProcessor <- function( input_folder.dir , output_folder.dir ,  tmp_folder.dir , sync_folder.dir ,
                             log_folder.dir=NA, cache_folder.dir=NA, override.repeated.tUID = FALSE,
                             stopAfterExecution = Inf) {

  global.lst.log_folder <- ""
  global.lst.tmp_folder <- ""
  global.lst.cache_folder <- ""
  global.sync.tmp_folder <- ""
  global.lst.input_folder <- ""
  global.lst.output_folder <- ""
  global.processes.master.table <- list()
  global.other.param <- list()
  objLogHandler <- LogHandler()

  global.current.state <- ""

  start <- function() {

    set.current.state("RUNNING")

    while( get.current.state() =="RUNNING" ) {

      # -----------------------------------------------------------------------
      # POOLING from input_folder
      # -----------------------------------------------------------------------
      lst.res <- checkInputFolder( global.lst.input_folder$path , timeForSleepint = 2)

      if( get.current.state() != "RUNNING" ) break;

      # -----------------------------------------------------------------------
      # TOKEN PARSING
      # -----------------------------------------------------------------------
      fileName <- lst.res$fileName
      fullFileName <- lst.res$fullFileName
      tmpZipExtractionFolder <- paste(c("zip.",format(Sys.time(), format = "%Y%m%d%I%M%S"),"_",runif(n = 1,0,100000)),collapse = '')
      zipTmpFolder <- paste(c(global.lst.tmp_folder$path,"/",tmpZipExtractionFolder),collapse = '')
      res <- openZippedPackage( fileName , fullFileName , zipTmpFolder)
      current.tokenInstanceUID <- res$lst.header$tokenInstanceUID

      # -----------------------------------------------------------------------
      # RUN PROCESSES
      # -----------------------------------------------------------------------
      cleanUp.tmp.folders(temp = FALSE, run = TRUE, current.tokenInstanceUID = current.tokenInstanceUID)

      runFolder <- paste(c(global.lst.tmp_folder$path,"/",paste(c("tUID_",current.tokenInstanceUID),collapse = '')),collapse = '')
      runFolderShort <- paste(c("tUID_",current.tokenInstanceUID),collapse = '')
      persistentEnvFolder <- paste(c(global.lst.tmp_folder$pathEnv,"/",paste(c("tUID_",current.tokenInstanceUID),collapse = '')),collapse = '')
      persistentEnvFolderShort <- paste(c(paste(c("tUID_",current.tokenInstanceUID),collapse = '')),collapse = '')

      dir.create(runFolder)
      if(!dir.exists(runFolder)) { stop("ERORR: token run folder not created")}
      if( !dir.exists(persistentEnvFolder) ) {
        dir.create(persistentEnvFolder)
        if(!dir.exists(persistentEnvFolder)) { stop("ERORR: token persistentEnvFolder folder not created")}
      }

      # -----------------------------------------------------------------------
      # RUN PROCESSES - unzip into tmp_folder and create run_folder
      # -----------------------------------------------------------------------

      file.copy(list.files(zipTmpFolder, full.names = TRUE), to = runFolder, recursive = TRUE)
      unlink(zipTmpFolder, recursive = TRUE)

      write.Log( msg =  c("Process started - ",current.tokenInstanceUID) )

      signal.token.folder.in <- paste(c(global.sync.tmp_folder$path,"/processes/signal.in/",current.tokenInstanceUID ),collapse = '')
      if(!dir.exists(signal.token.folder.in)) {  dir.create(signal.token.folder.in) }
      if( !dir.exists(signal.token.folder.in) ) { stop("ERROR: subProcess (in) Folder not crated successfully")}

      for( run.id in names(res$lst.run)) {
        tokenInstanceUID <- paste(c(format(Sys.time(), format = "%Y%m%d%I%M%S"),"_",runif(n = 1,0,100000)),collapse = '')

                scriptAlias <- res$lst.run[[run.id]]$scriptAlias
        scriptType <- res$lst.script[[ scriptAlias ]]$scriptType

        persistentEnvSubFolder <- paste(c(persistentEnvFolder,"/",run.id),collapse = '')
        persistentEnvSubFolderShort <- run.id
        if( !dir.exists(persistentEnvSubFolder) ) {
          dir.create(persistentEnvSubFolder)
          if(!dir.exists(persistentEnvSubFolder)) { stop("ERORR: token.run.id persistentEnvSubFolder folder not created")}
        }

        if (scriptType == 'script'){
          scriptFileName <- res$lst.script[[ scriptAlias ]]$scriptFileName
          scriptFullFileName <- paste(c(runFolder,"/",scriptFileName),collapse = '')
          scriptProgrammingLanguage <- res$lst.script[[ scriptAlias ]]$scriptProgrammingLanguage

        } else if (scriptType == 'docker'){
          dockerImageName <- res$lst.script[[ scriptAlias ]]$dockerImageName



        }

        executionFolderName <- paste(c("run.id_",run.id),collapse = '')
        executionFolderFullName <- paste(c(runFolder,"/",executionFolderName),collapse = '')

        # ok, prepara l'env per eseguire lo script
        dir.create(executionFolderFullName)
        if( !dir.exists(executionFolderFullName) ) stop("ERROR : not able to build the executionFolder.. ")

        # componi il file di passaggio parameetri: metti tutte le tag del RUN
        cosa <- res$lst.header
        arr.stringa.0 <- unlist(lapply(1:length(cosa),function(i) {  paste(c(names(cosa)[i]," = ",cosa[[i]]),collapse='') }))
        cosa <- res$lst.run[[run.id]]
        arr.stringa.1 <- unlist(lapply(1:length(cosa),function(i) {  paste(c(names(cosa)[i]," = ",cosa[[i]]),collapse='') }))
        cosa <- res$lst.script[[ res$lst.run[[run.id]]$scriptAlias ]]
        arr.stringa.2 <- unlist(lapply(1:length(cosa),function(i) {  paste(c(names(cosa)[i]," = ",cosa[[i]]),collapse='') }))
        arr.stringa.tot <- c( arr.stringa.0, arr.stringa.1 , arr.stringa.2)

        token.waiting.filename <- paste(c(signal.token.folder.in,"/",tokenInstanceUID,".txt"),collapse = '')
        parameterFileName <- paste(c(executionFolderFullName,".txt"),collapse = '')
        fileConn <- file(parameterFileName)

        if (scriptType == 'docker'){
          lst.parameter <- list("header"=list("run.id"=run.id,"currentDir"=runFolder,"executionFolderFullName"=executionFolderName,
                                              "persistentEnvSubFolder"=persistentEnvSubFolder,
                                              "tokenInstanceUID"=tokenInstanceUID,"token.waiting.filename"= paste(tokenInstanceUID,".txt", sep="")),"payload"=res)

        } else{
          lst.parameter <- list("header"=list("run.id"=run.id,"currentDir"=runFolder,"executionFolderFullName"=executionFolderFullName,
                                              "persistentEnvSubFolder"=persistentEnvSubFolder,
                                              "tokenInstanceUID"=tokenInstanceUID,"token.waiting.filename"=token.waiting.filename),"payload"=res)
        }

        if (scriptType == 'docker') {
        if (length(lst.parameter$payload$lst.dataSource) != 0){
          dataSourceAlias = names(lst.parameter$payload$lst.dataSource)
          dataSourceFileName = lst.parameter[["payload"]][["lst.dataSource"]][[dataSourceAlias]][["dataSourceFileName"]] #lst.parameter$payload$lst.dataSource
          dataSourcePath <- runFolder

        } else {
          dataSourceFileName = lst.parameter$payload$lst.run[[run.id]]$paramSlot02
          split <- str_split(dataSourceFileName, "/")[[1]]
          dataSourcePath <- paste(split[c(1:length(split)-1)], sep = "/", collapse = "/")
          dataSourceFileName <- split[length(split)]
        } }

        writeLines(prettify(toJSON(lst.parameter)),fileConn)
        close(fileConn)


        # -----------------------------------------------------------------------
        # RUN PROCESSES - launch the execution in background on the host machine
        # -----------------------------------------------------------------------

        if( "scriptFileName" %in% names(res$lst.script[[ scriptAlias ]] ) ) {
          fullScriptFileName <- paste(c(runFolder,"/",scriptFileName),collapse = '')
        }

        logFileName <- paste(c(global.sync.tmp_folder$path,"processes/log/",current.tokenInstanceUID,".",tokenInstanceUID,".bck"),collapse = '')
        if (scriptType =='script'){
          scriptSystemCall(scriptProgrammingLanguage, parameterFileName, fullScriptFileName, executionFolderFullName, logFileName, lst.parameter)

        } else if (scriptType == 'docker'){
          dockerSystemCall(dockerImageName, runFolder, runFolderShort, signal.token.folder.in, dataSourcePath, dataSourceFileName, executionFolderName, tokenInstanceUID, logFileName)
        }

        add.process.to.master.table( current.tokenInstanceUID = current.tokenInstanceUID, tokenInstanceUID = tokenInstanceUID,
                                     run.id = run.id, executionFolderFullName = executionFolderFullName, expected.brothers = names(res$lst.run))

        write.Log( msg =  c("SubProcess started - ",current.tokenInstanceUID,"::",tokenInstanceUID) )
      }
    }
  }

  # --------------------------------------------------
  # MASTER TABLE handlign methods
  # --------------------------------------------------

  # Add record
  add.process.to.master.table <- function( current.tokenInstanceUID , tokenInstanceUID , run.id = run.id,
                                           executionFolderFullName = executionFolderFullName, expected.brothers = expected.brothers) {

    current.dateTime <- format(Sys.time(), format = "%Y-%m-%d %I:%M:%S")
    nuova.linea <- c( "process.tUID" = current.tokenInstanceUID, "subprocess.UID" = tokenInstanceUID, "run.id" = run.id,
                      "executionFolderFullName" = executionFolderFullName, "n.expected.brothers" = length(expected.brothers),
                      "status" = "submitted" , "DT.create" = current.dateTime, "DT.finish" = ""  )

    if( length(global.processes.master.table) > 0) {
      quali.gia.presenti <- which(nuova.linea["process.tUID"] %in% global.processes.master.table[,"process.tUID"] & nuova.linea["run.id"] %in% global.processes.master.table[,"run.id"])
      if( length(quali.gia.presenti ) > 0 & global.other.param[["override.repeated.tUID"]] == TRUE ) {
        global.processes.master.table <<- global.processes.master.table[-quali.gia.presenti, ]
      }
    }

    global.processes.master.table <<- rbind( global.processes.master.table , nuova.linea)
    rownames(global.processes.master.table) <- c()

    # now place in the sync token folder
    # ----------------------------------------
    # if it is not there, create the main Process folder
    signal.token.folder.out <- paste(c(global.sync.tmp_folder$path,"/processes/signal.out/",current.tokenInstanceUID ),collapse = '')
    if(!dir.exists(signal.token.folder.out)) {  dir.create(signal.token.folder.out) }
    if( !dir.exists(signal.token.folder.out) ) { stop("ERROR: subProcess (out) Folder not crated successfully")}

    # Now enter the token for the launched subProcess
    path.out.processes  <- paste(c(signal.token.folder.out,"/",tokenInstanceUID,".txt" ),collapse = '')
    writeLines(c("Submitted"), path.out.processes )
    if( !file.exists(path.out.processes) ) { stop("ERROR: subProcess Folder not crated successfully")}

    write.processor.state.log()
  }
  # Edit an existing record
  chg.process.master.table.status <- function( whatToDo, arr.subprocess.UID = c() , process.tUID = NA, arr.new.values = c() ) {
    if( !(whatToDo %in% c("change","remove"))) stop("ERROR: command not found in changing the process.master.table")
    if( !is.na(process.tUID) ) { arr.subprocess.UID <- global.processes.master.table[which(global.processes.master.table[,"process.tUID"]==process.tUID),"subprocess.UID"] }
    if( whatToDo == "change" ) {
      for( subprocess.UID in arr.subprocess.UID) {
        for( colonna in names(arr.new.values)) {
          global.processes.master.table[ which(global.processes.master.table[,"subprocess.UID"] == subprocess.UID), colonna] <<- arr.new.values[colonna]
        }
      }
    }
    write.processor.state.log()
  }

  # --------------------------------------------------
  # Check whether Subprocesses have finished
  # --------------------------------------------------
  check.in.subProcess.folder <- function( ) {
    signal.token.folder <- paste(c(global.sync.tmp_folder$path,"/processes/signal.in" ),collapse = '')
    arr.dirs <- list.dirs(signal.token.folder,full.names = FALSE)
    arr.dirs <- arr.dirs[which(!(arr.dirs %in% c("")))]

    # Cycle through all folders in FS
    for(cartella in arr.dirs) {
      arr.files.in <- list.files(paste(c(signal.token.folder,"/",cartella),collapse = ''))
      arr.files.out <- list.files(paste(c(global.sync.tmp_folder$path,"/processes/signal.out/",cartella ),collapse = ''))

      # It implements the simplest control of all: if the number of tokens is equal
      if(length(arr.files.in) == length(arr.files.out)) {
        write.Log( msg =  c("Process succesfully finished - ",cartella ))

        righe <- which(global.processes.master.table[,"process.tUID"] == cartella)
        global.processes.master.table[righe,"status"] <- "finished"

        # -----------------------------------------------------------------------
        # Construction of subprocess zips
        # -----------------------------------------------------------------------
        lst.zipped.files.to.move <- c()
        for( riga in righe  ) {
          runFolder <- global.lst.tmp_folder$path
          executionFolderFullName <- as.character(global.processes.master.table[riga,"executionFolderFullName"])
          subprocess.UID <- as.character(global.processes.master.table[riga,"subprocess.UID"])
          process.tUID <- as.character(global.processes.master.table[riga,"process.tUID"])
          zipFile <- paste( c( runFolder,"/tUID_",process.tUID,".",subprocess.UID,".zip"), collapse = '')
          listaFilesDaZippare <- list.files(executionFolderFullName)

          # Zips the files of each subProcess separately
          for( i in 1:length(listaFilesDaZippare)) {
            nomeF <- listaFilesDaZippare[i]
            if(i==1) {
              zipr(zipfile = zipFile, files = paste(c(executionFolderFullName,"/",nomeF),collapse=''), recurse = TRUE, compression_level = 9)
            } else {
              zipr_append(zipfile = zipFile, files = paste(c(executionFolderFullName,"/",nomeF),collapse=''), recurse = TRUE, compression_level = 9)
            }
          }
          current.dateTime <- format(Sys.time(), format = "%Y-%m-%d %I:%M:%S")
          chg.process.master.table.status(subprocess.UID , whatToDo ="change" , arr.new.values = c("status"="finished","DT.finish"=current.dateTime) )
          write.Log( msg =  c("Output zipped for Process::subProcess - ",cartella,"::",subprocess.UID ))
          lst.zipped.files.to.move <- c( lst.zipped.files.to.move , zipFile )
        }

        # -----------------------------------------------------------------------
        # PACKAGING of results
        # -----------------------------------------------------------------------
        lst.zipped.files.to.move <-  unique(lst.zipped.files.to.move)
        for(file2Move in lst.zipped.files.to.move) {
          nomeSemplice <- substr( file2Move , max(unlist(str_locate_all(file2Move,"/")))+1, str_length(file2Move) )
          destinazione <- paste(c(global.lst.output_folder$path,"/",nomeSemplice),collapse = '')
          file.copy(from = file2Move,to = destinazione)
          file.remove(file2Move)
          write.Log( msg =  c("Zip deliveded to outcome folder - ",destinazione ))
          chg.process.master.table.status( process.tUID = process.tUID , whatToDo ="change" , arr.new.values = c("status"="delivered") )
        }

        global.processes.master.table[righe,"status"] <- "delivered"

        # CLEANING the temporary and signalling folders
        clean.process.signal.in.out( cartella )
        clean.process.tmpFolder( cartella )

        global.other.param[["executedProcesses"]] <- global.other.param[["executedProcesses"]] + 1

        if(global.other.param[["stopAfterExecution"]] <= global.other.param[["executedProcesses"]]  ) {
          set.current.state("STOP")
        }
      }
    }
  }
  clean.process.signal.in.out <- function( tUID ) {
    folder <- paste(c(global.sync.tmp_folder$path,"processes/signal.in/",tUID),collapse = '')
    unlink(folder,recursive = TRUE)
    folder <- paste(c(global.sync.tmp_folder$path,"processes/signal.out/",tUID),collapse = '')
    unlink(folder,recursive = TRUE)
  }
  clean.process.tmpFolder <- function( tUID ) {
    folder <- paste(c(global.lst.tmp_folder$path,"/tUID_",tUID),collapse = '')
    unlink(folder,recursive = TRUE)
  }


  # --------------------------------------------------
  # Methods for running scripts
  # --------------------------------------------------

  # Launches R script
  RscriptSystemCall <- function(parameterFileName, scriptFileName,logFileName){
    RScript_path = system("which Rscript", inter = TRUE)
    parameterFileName_str = paste("'", parameterFileName, "'", sep = '')
    system(paste(RScript_path, scriptFileName, parameterFileName_str," > ",logFileName," 2>&1 & "), intern = T)
  }

  # Launches Python script
  PYscriptSystemCall <- function(parameterFileName, scriptFileName, executionFolderFullName, logFileName,lst.parameter){

    if (length(lst.parameter[["payload"]][["lst.other"]][["requirements"]]["alias"])){
      command1 = paste("python3 -m venv",executionFolderFullName)
      command2 = paste(". ",executionFolderFullName,"/bin/activate; pip install --upgrade pip", sep ="")
      command3 = paste("pip install -r ", lst.parameter$header$currentDir,"/requirements.txt",sep = "")
      command4 = "echo Virtual environment is ready!"
    }else{
      command1 = "echo ..."
      command2 = "echo ..."
      command3 = "echo No requirement file found!"
      command4 = "echo No virtual environment is created!"
    }
    fileConn<-file("script_launch.sh")
    command5 = paste("python3",scriptFileName, parameterFileName, sep = ' ')
    writeLines(c(command1,command2,command3,command4,command5), fileConn)
    close(fileConn)
    system(paste("sh script_launch.sh"," > ",logFileName," 2>&1 & "), intern = T)
    system("rm script_launch.sh", intern = T)  }


  # Run Docker images
  dockerSystemCall <- function(dockerImageName, runFolder, runFolderShort, syncFolder, dataSourcePath, dataSourceFileName, executionFolderName, tokenInstanceUID, logFileName){
    library(stringr)
    command = 'docker run -v runFolder:/runFolderShort -v syncFolder:/syncOut -v dataFolder:/data dockerImageName /runFolderShort executionFolderName.txt tokenInstanceUID.txt dataSourceFileName runFolderShort'
    command = str_replace(command, "dockerImageName", dockerImageName)
    command = str_replace(command, "runFolder", runFolder)
    command = str_replace_all(command, "runFolderShort", runFolderShort)
    command = str_replace(command, "syncFolder", syncFolder)
    command = str_replace(command, "dataFolder", dataSourcePath)
    command = str_replace(command, "dockerImageName", dockerImageName)
    command = str_replace(command, "executionFolderName", executionFolderName)
    command = str_replace(command, "tokenInstanceUID", tokenInstanceUID)
    command = str_replace(command, "dataSourceFileName", dataSourceFileName)
    command = str_replace(command, "//", "/")
    command = paste(command," > ",logFileName," 2>&1 & ")
    system(command, intern = T)
  }
  scriptSystemCall <- function(programming_language, parameterFileName, scriptFileName, executionFolderFullName, logFileName, lst.parameter){
    if (programming_language == 'R.general' | programming_language == 'R'){
      RscriptSystemCall(parameterFileName, scriptFileName,logFileName)
    } else if (programming_language == 'Python'){
      PYscriptSystemCall(parameterFileName, scriptFileName, executionFolderFullName, logFileName,lst.parameter)
    }}


  # --------------------------------------------------
  # LOG management methods
  # --------------------------------------------------
  write.Log<-function( msg ) {
    ProcessFileLog <- paste(c(global.sync.tmp_folder$path,"/processor/log/Process.logfile.txt"),collapse = '')
    current.dateTime <- format(Sys.time(), format = "%Y-%m-%d %I:%M:%S")
    message <- paste( c(current.dateTime," : ",msg), collapse = '')
    write( message, ProcessFileLog , append=TRUE)
  }

  # --------------------------------------------------
  # Methods for managing the MASTER TABLE
  # --------------------------------------------------

  checkInputFolder <- function( inputFolder , timeForSleepint = 2 ) {

    fileFound <- FALSE; whichFileName <- ""
    list.previous.files <- list()

    oldFileList <- c()
    while( fileFound == FALSE & get.current.state() =="RUNNING"  ) {
      arr.ff <- list.files( inputFolder )
      for(fileName in arr.ff) {
        fullFileName <- paste( c(inputFolder,"/",fileName), collapse = '')
        if( fileName %in% names(list.previous.files) ) {
          list.previous.files[[fileName]]$size == file.size( fullFileName )
          fileFound <- TRUE;  whichFileName <- fullFileName
        } else {
          #  timeStamp' is not used at the moment: in the future it may be used to avoid starvation
          #  of submitted jobs (older jobs will be favoured).
          list.previous.files[[fileName]] <- list( "size"=file.size( paste( fullFileName ) ),
                                                   "timeStamp" = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
        }
      }
      Sys.sleep(timeForSleepint)

      # Check that subprocessors are not finished and must be zipped
      check.in.subProcess.folder()
    }
    return( list( "fileName" = fileName,
                  "fullFileName" = whichFileName))
  }

  write.processor.state.log <- function() {
    write.csv(global.processes.master.table,file = paste(c(global.sync.tmp_folder$path,"processor/log/processMasterTable.csv"),collapse=''))
  }
  loadParamfileName <- function( parameterFileName ) {

    fileConn <- file(parameterFileName)
    lst.lines <- readLines(fileConn)
    close(fileConn)

    lst.par <- list()
    for(ct.riga in 1:length(lst.lines)) {
      riga <- str_trim(string = lst.lines[ct.riga])
      arr.obj <- str_split(string = riga,pattern = "=")[[1]]
      lst.par[[ str_trim(arr.obj[1]) ]] <- str_trim(arr.obj[2])
    }

    return(lst.par)
  }
  openZippedPackage <- function( fileName , fullFileName , zipTmpFolder) {

    # Create the tmp folder where the zip is to be opened
    dir.create(zipTmpFolder)
    if( !dir.exists(zipTmpFolder) ) {
      stop("ERORR:tmpZipExtractionFolder not created.")
    }
    # move the file and remove it from the inputFolder
    file.copy(from = fullFileName , to = zipTmpFolder)
    file.remove(fullFileName)
    # unzip the file
    unzip(zipfile = paste( c(zipTmpFolder,"/",fileName), collapse = ''), exdir =  zipTmpFolder)
    file.remove(paste( c(zipTmpFolder,"/",fileName), collapse = ''))

    # load the objXML and build a structure with the required services (lst.service) after reading the XML

    objXML <- xmlInternalTreeParse(file = paste(c(zipTmpFolder,"/","description.xml"),collapse = ''))

    tokenInstanceUID <- unlist(xpathApply(objXML,path="/xml/XMLheader/tokenInstanceUID",xmlValue))
    runUID <- unlist(xpathApply(objXML,path="/xml/XMLheader/runUID",xmlValue))
    creationDateTime <- unlist(xpathApply(objXML,path="/xml/XMLheader/creationDateTime",xmlValue))

    lst.header <- list()
    lst.header$tokenInstanceUID <- tokenInstanceUID
    lst.header$runUID <- runUID
    lst.header$creationDateTime <- creationDateTime

    # --------------------------------------------------
    # Scroll through the dataSources
    # --------------------------------------------------
    subXML <- xpathApply(objXML,path="/xml/XMLobj[@objType='dataSource']")
    lst.dataSource <- list()
    if( length(subXML) > 0 ) {
      for( ct in 1:length(subXML)) {

        subXMLDoc <- xmlDoc(subXML[[ct]])
        if( length(unlist(xpathApply(subXMLDoc,path="/XMLobj/dataSourceType",xmlValue)))!=1 ) stop("ERROR : missing a 'dataSourceType' withing an XMLobj tag")
        if( length(unlist(xpathApply(subXMLDoc,path="/XMLobj/dataSourceFileName",xmlValue)))!=1 ) stop("ERROR : missing a 'dataSourceFileName' withing an XMLobj tag")
        if( unlist(xpathApply(subXMLDoc,path="/XMLobj/dataSourceType",xmlValue))!="csv" ) stop("ERROR : only 'csv' is supported for the 'dataSourceType' tag, in the current version")

        dataSourceFileName <- unlist(xpathApply(subXMLDoc,path="/XMLobj/dataSourceFileName",xmlValue))
        dataSourceAlias <- unlist(xpathApply(subXMLDoc,path="/XMLobj/alias",xmlValue))
        dataSourceType <- unlist(xpathApply(subXMLDoc,path="/XMLobj/dataSourceType",xmlValue))

        lst.dataSource[[dataSourceAlias]] <- list()
        lst.dataSource[[dataSourceAlias]]$dataSourceFileName <- dataSourceFileName
        lst.dataSource[[dataSourceAlias]]$dataSourceType <- dataSourceType

        free(subXMLDoc)
      }
    }
    # --------------------------------------------------
    # Scroll through the others
    # --------------------------------------------------
    subXML <- xpathApply(objXML,path="/xml/XMLobj[@objType='other']")
    lst.other <- list()
    if( length(subXML) > 0 ) {
      for( ct in 1:length(subXML)) {
        lst.tmp <- xmlToList(subXML[[ct]])
        nomiCampi <- names(lst.tmp)[!(names(lst.tmp) %in% ".attrs")]
        subXMLDoc <- xmlDoc(subXML[[ct]])
        lst.lista.pre <- list()
        scriptAlias <- unlist(xpathApply(subXMLDoc,path="/XMLobj/alias",xmlValue))
        for(i in 1:length(nomiCampi)) {
          lst.lista.pre[[nomiCampi[i]]] <- unlist(xpathApply(subXMLDoc,path=paste(c("/XMLobj/",nomiCampi[i]),collapse = ''),xmlValue))
        }
        lst.other[[scriptAlias]] <- lst.lista.pre
      }
    }
    # --------------------------------------------------
    # Scroll through the Scripts
    # --------------------------------------------------
    subXML <- xpathApply(objXML,path="/xml/XMLobj[@objType='script']")
    lst.script <- list()
    if( length(subXML) > 0 ) {
      for( ct in 1:length(subXML)) {
        subXMLDoc <- xmlDoc(subXML[[ct]])
        scriptType <- unlist(xpathApply(subXMLDoc,path="/XMLobj/scriptType",xmlValue))


        if (scriptType =="script"){
          scriptSource <- unlist(xpathApply(subXMLDoc,path="/XMLobj/scriptSource",xmlValue))
          scriptProgrammingLanguage <- unlist(xpathApply(subXMLDoc,path="/XMLobj/scriptProgrammingLanguage",xmlValue))
          scriptFileName <- unlist(xpathApply(subXMLDoc,path="/XMLobj/scriptFileName",xmlValue))
          scriptAlias <- unlist(xpathApply(subXMLDoc,path="/XMLobj/alias",xmlValue))
          processorConformanceClass <- unlist(xpathApply(subXMLDoc,path="/XMLobj/processorConformanceClass",xmlValue))
        } else if(scriptType == "docker" ){

          scriptSource <- unlist(xpathApply(subXMLDoc,path="/XMLobj/scriptSource",xmlValue))
          dockerImageName <- unlist(xpathApply(subXMLDoc,path="/XMLobj/dockerImageName",xmlValue))
          scriptAlias <- unlist(xpathApply(subXMLDoc,path="/XMLobj/alias",xmlValue))
        }

        if( length(scriptAlias) != 1 ) stop("ERROR : 'scriptAlias' is a mandatory tag")

        if (scriptType =="script"){
          lst.script[[scriptAlias]] <- list()
          lst.script[[scriptAlias]]$scriptType <- scriptType
          lst.script[[scriptAlias]]$scriptFileName <- scriptFileName
          lst.script[[scriptAlias]]$processorConformanceClass <- processorConformanceClass
          lst.script[[scriptAlias]]$scriptSource <- scriptSource
          lst.script[[scriptAlias]]$scriptProgrammingLanguage <- scriptProgrammingLanguage
        } else if (scriptType=="docker"){
          lst.script[[scriptAlias]] <- list()
          lst.script[[scriptAlias]]$scriptType <- scriptType
          lst.script[[scriptAlias]]$dockerImageName <- dockerImageName
          lst.script[[scriptAlias]]$scriptSource <- scriptSource
        }

        free(subXMLDoc)
      }
    }

    # --------------------------------------------------
    # Scroll through the runs
    # --------------------------------------------------
    subXML <- xpathApply(objXML,path="/xml/XMLobj[@objType='run']")
    lst.run <- list()
    if( length(subXML) > 0 ) {
      for( ct in 1:length(subXML)) {
        subXMLDoc <- xmlDoc(subXML[[ct]])

        runAlias <- unlist(xpathApply(subXMLDoc,path="/XMLobj/alias",xmlValue))
        scriptAlias <- unlist(xpathApply(subXMLDoc,path="/XMLobj/scriptAlias",xmlValue))
        dataSourceAlias <- unlist(xpathApply(subXMLDoc,path="/XMLobj/dataSourceAlias",xmlValue))
        otherAlias <- unlist(xpathApply(subXMLDoc,path="/XMLobj/otherAlias",xmlValue))
        outXMLDescriptorType <- unlist(xpathApply(subXMLDoc,path="/XMLobj/outXMLDescriptorType",xmlValue))
        outXMLDescriptorFileName <- unlist(xpathApply(subXMLDoc,path="/XMLobj/outXMLDescriptorFileName",xmlValue))
        paramSlot01 <- unlist(xpathApply(subXMLDoc,path="/XMLobj/paramSlot01",xmlValue))
        paramSlot02 <- unlist(xpathApply(subXMLDoc,path="/XMLobj/paramSlot02",xmlValue))
        paramSlot03 <- unlist(xpathApply(subXMLDoc,path="/XMLobj/paramSlot03",xmlValue))
        paramSlot04 <- unlist(xpathApply(subXMLDoc,path="/XMLobj/paramSlot04",xmlValue))
        paramSlot05 <- unlist(xpathApply(subXMLDoc,path="/XMLobj/paramSlot05",xmlValue))
        paramSlot06 <- unlist(xpathApply(subXMLDoc,path="/XMLobj/paramSlot06",xmlValue))
        paramSlot07 <- unlist(xpathApply(subXMLDoc,path="/XMLobj/paramSlot07",xmlValue))

        if( length(scriptAlias) != 1 ) stop("ERROR : 'scriptAlias' is a mandatory tag")

        lst.run[[runAlias]] <- list()
        lst.run[[runAlias]]$scriptAlias <- scriptAlias
        lst.run[[runAlias]]$dataSourceAlias <- dataSourceAlias
        lst.run[[runAlias]]$otherAlias <- otherAlias
        lst.run[[runAlias]]$outXMLDescriptorType <- outXMLDescriptorType
        lst.run[[runAlias]]$outXMLDescriptorFileName <- outXMLDescriptorFileName
        lst.run[[runAlias]]$paramSlot01 <- paramSlot01
        lst.run[[runAlias]]$paramSlot02 <- paramSlot02
        lst.run[[runAlias]]$paramSlot03 <- paramSlot03
        lst.run[[runAlias]]$paramSlot04 <- paramSlot04
        lst.run[[runAlias]]$paramSlot05 <- paramSlot05
        lst.run[[runAlias]]$paramSlot06 <- paramSlot06
        lst.run[[runAlias]]$paramSlot07 <- paramSlot07

        free(subXMLDoc)
      }
    }
    if(length(lst.script) != 1 ) stop("ERROR : in the current version exactly one objType is needed")

    return(
      list( "lst.dataSource" = lst.dataSource,
            "lst.other" = lst.other,
            "lst.script" = lst.script,
            "lst.run" = lst.run,
            "lst.header" = lst.header
      )
    )
  }

  # -----------------------------------------------------------------------------------------------------------
  # GET and SET of processor and process status
  # -----------------------------------------------------------------------------------------------------------
  set.current.state <- function( toState ) {
    global.current.state <<- toState
  }
  get.current.state <- function( toState ) {
    return( global.current.state )
  }
  # -----------------------------------------------------------------------------------------------------------
  # Prepares folders for process and processor sync
  # -----------------------------------------------------------------------------------------------------------
  build.sync.folder <- function( ) {
    if( !dir.exists( global.sync.tmp_folder$path) ) { stop("ERROR: global.sync.tmp_folder does not exist")   }

    arr.folder.tree <- list( "/processor", "/processor/log","/processor/signal.in","/processor/signal.out",
                             "/processes","/processes/log","/processes/signal.in","/processes/signal.out")

    # Distruggi e ricrea le cartelle ad ogni lancio di processore
    for( addendum in arr.folder.tree) {
      path.2c.process  <- paste(c(global.sync.tmp_folder$path,addendum ),collapse = '')
      unlink(path.2c.process, recursive = TRUE)
      dir.create( path.2c.process )
      if( !dir.exists( path.2c.process ) ) { stop("ERROR: build.sync.folder subtree creation fails")   }
    }
  }
  # -----------------------------------------------------------------------------------------------------------
  # Cleaning Run and Env Folders
  # -----------------------------------------------------------------------------------------------------------
  cleanUp.tmp.folders <- function(run = TRUE, temp = TRUE , current.tokenInstanceUID = NA, cleanUpPersistentFolder = FALSE) {
    if( cleanUpPersistentFolder == TRUE) {
    # if(run==TRUE ) {
      if(is.na(current.tokenInstanceUID)) {
        if(dir.exists(global.lst.tmp_folder$pathEnv)) unlink( global.lst.tmp_folder$pathEnv,recursive = T)
        if(dir.exists(global.lst.tmp_folder$pathEnv)== FALSE) { dir.create(global.lst.tmp_folder$pathEnv) }
        if(dir.exists(global.lst.tmp_folder$pathEnv)== FALSE) stop("ERROR: global.lst.tmp_folder$pathEnv not created")
      } else {
        folderToCheck <- paste(c(global.lst.tmp_folder$pathEnv,"/",current.tokenInstanceUID),collapse = '')
        if(dir.exists(folderToCheck)) unlink( folderToCheck,recursive = T)
        if(dir.exists(folderToCheck)== FALSE) { dir.create(folderToCheck) }
        if(dir.exists(folderToCheck)== FALSE) stop("ERROR: global.lst.tmp_folder$pathEnv/current.tokenInstanceUID not created")
      }
    }
    if(temp==TRUE) {
      if(dir.exists(global.lst.tmp_folder$path)) unlink( global.lst.tmp_folder$path,recursive = T)
      if(dir.exists(global.lst.tmp_folder$path)== FALSE) { dir.create(global.lst.tmp_folder$path) }
      if(dir.exists(global.lst.tmp_folder$path)== FALSE) stop("ERROR: global.lst.tmp_folder$path not created")
    }
  }
  # -----------------------------------------------------------
  # Constructor
  # -----------------------------------------------------------
  constructor <- function( input_folder.dir , output_folder.dir , tmp_folder.dir,
                           log_folder.dir, cache_folder, sync_folder.dir , override.repeated.tUID,
                           stopAfterExecution ) {

    if( is.na(tmp_folder.dir) ) stop("Error: tmp_folder.dir must be specifified ")
    if( is.na(input_folder.dir) ) stop("Error: input_folder.dir must be specifified ")
    if( is.na(output_folder.dir) ) stop("Error: output_folder.dir must be specifified ")

    global.lst.input_folder <<- list("path" = input_folder.dir)
    global.lst.output_folder <<- list("path" = output_folder.dir)
    global.lst.log_folder <<- list( "path" = log_folder.dir )
    global.lst.tmp_folder <<- list( "path" = paste(c(tmp_folder.dir,"/run"),collapse = ''),
                                    "pathEnv" = paste(c(tmp_folder.dir,"/envs"),collapse = '')
    )

    global.sync.tmp_folder <<- list("path" = sync_folder.dir)
    global.lst.cache_folder <<- list( "path" = cache_folder )
    global.other.param <<- list()
    global.other.param[["override.repeated.tUID"]] <<- override.repeated.tUID
    global.other.param[["stopAfterExecution"]] <<- stopAfterExecution
    global.other.param[["executedProcesses"]] <<- 0

    # Pulisci le cartelle temporanee
    cleanUp.tmp.folders(cleanUpPersistentFolder = TRUE)
    build.sync.folder()

    # set the machine to state READY
    set.current.state("READY")

    # begin to write the proccessor log
    write.processor.state.log()
  }
  # ===========================================================
  constructor(input_folder.dir = input_folder.dir, output_folder.dir = output_folder.dir,
              cache_folder = cache_folder.dir,
              log_folder.dir=log_folder.dir, tmp_folder.dir=tmp_folder.dir, sync_folder.dir=sync_folder.dir,
              override.repeated.tUID = override.repeated.tUID , stopAfterExecution = stopAfterExecution)
  # ===========================================================

  return( list(
    "processor"= processor,
    "set.current.state"= set.current.state,
    "get.current.state"= get.current.state,
    "start"= start
  ))

}
