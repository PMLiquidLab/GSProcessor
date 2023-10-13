library(GSProcessor)
library(jsonlite)
library(XML)
library(stringr)
library(zip)


objS <- bckGndProcessor(input_folder.dir = "./inputFolder/",
                        output_folder.dir = "./outputFolder/",
                        tmp_folder.dir = "./tmpFolder/",
                        sync_folder.dir = "./syncFolder/",
                        override.repeated.tUID = TRUE
)

objS$start()
