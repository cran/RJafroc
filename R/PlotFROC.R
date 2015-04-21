#' @import ggplot2
PlotFROC <- function(dataset, plottingModalities, plottingReaders, legendPosition = "bottom") {
  NL <- dataset$NL
  LL <- dataset$LL
  lesionNum <- dataset$lesionNum
  maxNL <- dim(NL)[4]
  dataType <- dataset$dataType
  modalityID <- dataset$modalityID
  readerID <- dataset$readerID
  
  if (length(dim(NL)) != 4 || length(dim(LL)) != 4) 
    stop("The dimension of NL or LL is not corretc. ")
  
  if (!is.list(plottingModalities) && !is.list(plottingReaders)) {
    FROCPoints <- CalculateFROCPoints(NL, LL, modalityID, readerID, maxNL, lesionNum, plottingModalities, plottingReaders)
  } else {
    if (is.list(plottingModalities) && is.list(plottingReaders) && length(plottingModalities) == length(plottingReaders)) {
      FROCPoints <- data.frame(NLF = NULL, LLF = NULL)
      for (i in 1:length(plottingReaders)) {
        if (length(plottingModalities[[i]]) == 1 && (length(plottingReaders[[i]]) == 1)) {
          tempFROCPoints <- CalculateFROCPoints(NL, LL, modalityID, readerID, maxNL, lesionNum, plottingModalities[[i]], plottingReaders[[i]])
          FROCPoints <- rbind(FROCPoints, tempFROCPoints)
        } else {
          tempFROCPoints <- CalculateAvgFROCPoints(NL, LL, modalityID, readerID, maxNL, lesionNum, plottingModalities[[i]], plottingReaders[[i]])
          FROCPoints <- rbind(FROCPoints, tempFROCPoints)
        }
      }
    } else if (is.list(plottingModalities) && length(plottingReaders) == 1) {
      plottingReaders <- plottingReaders[[1]]
      FROCPoints <- data.frame(NLF = NULL, LLF = NULL)
      for (i in 1:length(plottingModalities)) {
        if (length(plottingModalities[[i]]) == 1) {
          tempFROCPoints <- CalculateFROCPoints(NL, LL, modalityID, readerID, maxNL, lesionNum, plottingModalities[[i]], plottingReaders)
          FROCPoints <- rbind(FROCPoints, tempFROCPoints)
        } else {
          tempFROCPoints <- CalculateAvgFROCPoints(NL, LL, modalityID, readerID, maxNL, lesionNum, plottingModalities[[i]], plottingReaders)
          FROCPoints <- rbind(FROCPoints, tempFROCPoints)
        }
      }
    } else if (is.list(plottingReaders) && length(plottingModalities) == 1) {
      if (is.list(plottingModalities)) 
        plottingModalities <- plottingModalities[[1]]
      FROCPoints <- data.frame(NLF = NULL, LLF = NULL)
      for (i in 1:length(plottingReaders)) {
        if (length(plottingReaders[[i]]) == 1) {
          tempFROCPoints <- CalculateFROCPoints(NL, LL, modalityID, readerID, maxNL, lesionNum, plottingModalities, plottingReaders[[i]])
          FROCPoints <- rbind(FROCPoints, tempFROCPoints)
        } else {
          tempFROCPoints <- CalculateAvgFROCPoints(NL, LL, modalityID, readerID, maxNL, lesionNum, plottingModalities, plottingReaders[[i]])
          FROCPoints <- rbind(FROCPoints, tempFROCPoints)
        }
      }
    } else {
      stop("Lengths of plottingModalities and plottingReaders do not match.")
    }
  }
  
  xLim <- ceiling(max(FROCPoints$NLF))
  yLim <- ceiling(max(FROCPoints$LLF))
  if (!is.list(plottingModalities) && !is.list(plottingReaders)) {
    mr <- unlist(strsplit(as.character(FROCPoints$class), split = "\n"))
    dim(mr) <- c(2, length(mr)/2)
    FROCPoints <- cbind(FROCPoints, data.frame(Modality = mr[1, ], Reader = mr[2, ]))
    opratingPoints <- FROCPoints[FROCPoints$type == "individual" & !((FROCPoints$NLF == 0 & FROCPoints$LLF == 0) | (FROCPoints$NLF == 1 & FROCPoints$LLF == 1)), ]
    
    FROCPlot <- with(FROCPoints, {
      FROCPlotTemp <- ggplot()
      mStrings <- unique(as.character(FROCPoints$Modality))
      for (i in 1:length(plottingModalities)) {
        FROCPlotTemp <- FROCPlotTemp + geom_line(data = FROCPoints[FROCPoints$Modality == mStrings[i], ], aes(x = NLF, y = LLF, color = Reader, linetype = Modality), size = 1)
      }
      FROCPlotTemp <- FROCPlotTemp + geom_point(data = opratingPoints, size = 4, aes(x = NLF, y = LLF, color = Reader)) + theme(legend.title = element_blank(), legend.position = legendPosition) + scale_x_continuous(expand = c(0, 
                                                                                                                                                                                                                                  0), limits = c(0, xLim)) + scale_y_continuous(expand = c(0, 0), limits = c(0, yLim))
    })
    FROCPoints <- data.frame(NLF = FROCPoints$NLF, LLF = FROCPoints$LLF, class = FROCPoints$class, type = FROCPoints$type)
  } else {
    opratingPoints <- FROCPoints[FROCPoints$type == "individual" & !((FROCPoints$NLF == 0 & FROCPoints$LLF == 0) | (FROCPoints$NLF == 1 & FROCPoints$LLF == 1)), ]
    
    legendLength <- length(levels(FROCPoints$class))
    shapeVector <- rep(NA, length(levels(FROCPoints$class)))
    for (n in 1:legendLength) {
      index <- which(FROCPoints$class == levels(FROCPoints$class)[n])[1]
      if (FROCPoints$type[index] == "individual") 
        shapeVector[n] <- 16
    }
    FROCPlot <- with(FROCPoints, {
      ggplot(data = FROCPoints, aes(x = NLF, y = LLF, color = class)) + geom_line(size = 1) + geom_point(data = opratingPoints, size = 4) + theme(legend.title = element_blank(), legend.position = legendPosition) + 
        guides(color = guide_legend(override.aes = list(shape = shapeVector))) + scale_x_continuous(expand = c(0, 0), limits = c(0, xLim)) + scale_y_continuous(expand = c(0, 0), limits = c(0, yLim))
    })
  }
  
  # print(FROCPlot)
  return(list(FROCPlot = FROCPlot, FROCPoints = FROCPoints))
} 
