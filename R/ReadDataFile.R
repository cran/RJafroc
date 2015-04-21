#' Reads the data file and creates a dataset object
#' 
#' Reads the dataset file to be analyzed and creates a dataset object for subsequent analysis.
#' 
#' @param fileName A string specifying the name of the file that contains the dataset. 
#' The extension of the file must match the corresponding format specified below.
#' @param format A string specifying the format of the data file. It can be \code{JAFROC} (the default), \code{MRMC} or \code{iMRMC}. 
#' For \code{MRMC} the format is determined by the extension of the data file as specified in \url{http://perception.radiology.uiowa.edu/}: 
#' the file extension can be .csv or .txt or .lrc. For file extension .imrmc the format is described in \url{https://code.google.com/p/imrmc/}.
#' @param delimiter The string delimiter to be used for the MRMC format ("," is the default), see \url{http://perception.radiology.uiowa.edu/}.
#' This parameter is not used when reading \code{JAFROC} or \code{iMRMC} data files.
#' 
#' @return A dataset with the specified structure, see \link{RJafroc-package}.
#' 
#' @examples
#' 
#' fullName <- "http://www.devchakraborty.com/RocData/rocData.xlsx"
#' download.file(url = fullName, basename(fullName), mode = "wb")
#' RocDataXlsx<- ReadDataFile(basename(fullName))
#' 
#' \dontrun{
#' fullName <- "http://www.devchakraborty.com/RocData/rocData.csv"
#' download.file(url = fullName, basename(fullName))
#' RocDataCsv<- ReadDataFile(basename(fullName), format = "MRMC")
#' 
#' fullName <- "http://www.devchakraborty.com/RocData/rocData.imrmc"
#' download.file(url = fullName, basename(fullName))
#' RocDataImrmc<- ReadDataFile(basename(fullName), format = "iMRMC")
#' 
#' fullName <- "http://www.devchakraborty.com/FrocData/frocData.xlsx"
#' download.file(url = fullName, basename(fullName), mode = "wb")
#' FrocDataXlsx <- ReadDataFile(basename(fullName))
#' 
#' fullName <- "http://www.devchakraborty.com/RoiData/roiData.xlsx"
#' download.file(url = fullName, basename(fullName), mode = "wb")
#' RoiDataXlsx <- ReadDataFile(basename(fullName))
#' }
#' 
#' @importFrom tools file_ext
#' 
#' @export
#' 
ReadDataFile <- function(fileName, format = "JAFROC", delimiter = ",") {
  if (format == "JAFROC") {
    if (!(file_ext(fileName) %in% c("xls", "xlsx"))) 
      stop("The extension of JAFROC data file must be \"*.xls\" or \"*.xlsx\" ")
    return(ReadJAFROC(fileName))
  } else if (format == "iMRMC") {
    return(ReadImrmc(fileName))
  } else if (format == "MRMC") {
    if (file_ext(fileName) == "lrc") {
      return(ReadLrc(fileName))
    } else {
      return(ReadOrDbmMrmc(fileName, delimiter))
    }
  } else {
    errMsg <- sprintf("%s is not an available file format.", format)
    stop(errMsg)
  }
} 
