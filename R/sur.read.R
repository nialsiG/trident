# sur.read----
#' @title sur.read
#' @description Description
#' @param sur surface file
#' @param type character vector indicating the desired output format - either a matrix of heights, or a data.frame with x, y and z as columns
#' @return the result of fun
#' @examples
#' #to do
#' @export
sur.read <- function(sur, type = "xyz") {
  fileID <- file(sur, 'rb')
  res.signature <- readChar(fileID, n = 12);
  res.format <- readBin(fileID, n = 1, what = 'int', size = 2);
  res.objNum <- readBin(fileID, n = 1,  what = 'int', size = 2);
  res.version <- readBin(fileID, n = 1,  what = 'int', size = 2);
  res.objType <- readBin(fileID, n = 1,  what = 'int', size = 2);
  res.objName <- readChar(fileID, n = 30);
  res.operatorName <- readChar(fileID, n = 30);
  res.materialCode <- readBin(fileID, n = 1,  what = 'int', size = 2);
  res.acquisitionType <- readBin(fileID, n = 1,  what = 'int', size = 2);
  res.rangeType <- readBin(fileID, n = 1,  what = 'int', size = 2);
  res.specialPoints <- readBin(fileID, n = 1,  what = 'int', size = 2);
  res.absoluteHeights <- readBin(fileID, n = 1,  what = 'int', size = 2);
  res.gaugeResulution <- readBin(fileID, n = 1,  what = 'numeric', size = 4);
  readChar(fileID, n = 4);
  #data format of points
  res.sizeOfPoints <- readBin(fileID, n = 1,  what = 'int', size = 2);
  res.zMin <- readBin(fileID, n = 1,  what = 'int', size = 4);
  res.zMax  <- readBin(fileID, n = 1,  what = 'int', size = 4);
  #Number of points per axis
  res.xPoints <- readBin(fileID, n = 1,  what = 'int', size = 4);
  res.yPoints <- readBin(fileID, n = 1,  what = 'int', size = 4);
  res.totalNumberOfPoints <- readBin(fileID, n = 1,  what = 'int', size = 4);
  #Distance between points
  res.xSpacing <- readBin(fileID, n = 1,  what = 'numeric', size = 4);
  res.ySpacing <- readBin(fileID, n = 1,  what = 'numeric', size = 4);
  res.zSpacing <- readBin(fileID, n = 1,  what = 'numeric', size = 4);
  #Name of axis
  res.xName <- readChar(fileID, n = 16);
  res.yName <- readChar(fileID, n = 16);
  res.zName <- readChar(fileID, n = 16);
  #Unit of distance between points
  res.xStepUnit <- readChar(fileID, n = 16);
  res.yStepUnit <- readChar(fileID, n = 16);
  res.zStepUnit <- readChar(fileID, n = 16);
  #Unit of axis
  res.xLengthUnit <- readChar(fileID, n = 16);
  res.yLengthUnit <- readChar(fileID, n = 16);
  res.zLengthUnit <- readChar(fileID, n = 16);
  #Skaling of distance between points
  res.xUnitRatio <- readBin(fileID, n = 1,  what = 'numeric', size = 4);
  res.yUnitRatio <- readBin(fileID, n = 1,  what = 'numeric', size = 4);
  res.zUnitRatio <- readBin(fileID, n = 1,  what = 'numeric', size = 4);
  res.imprint <- readBin(fileID, n = 1,  what = 'int', size = 2);
  res.inverted <- readBin(fileID, n = 1,  what = 'int', size = 2);
  res.levelled <- readBin(fileID, n = 1,  what = 'int', size = 2);
  readChar(fileID, n = 12);
  #timestamp
  res.startSeconds <- readBin(fileID, n = 1,  what = 'int', size = 2);
  res.startMinutes <- readBin(fileID, n = 1,  what = 'int', size = 2);
  res.startHours <- readBin(fileID, n = 1,  what = 'int', size = 2);
  res.startDays <- readBin(fileID, n = 1,  what = 'int', size = 2);
  res.startMonths <- readBin(fileID, n = 1,  what = 'int', size = 2);
  res.startYears <- readBin(fileID, n = 1,  what = 'int', size = 2);
  res.startWeekDay <- readBin(fileID, n = 1,  what = 'int', size = 2);
  res.measurementDuration <- readBin(fileID, n = 1,  what = 'numeric', size = 4);
  readChar(fileID, n = 10);
  #Size of comment field
  res.commentSize <- readBin(fileID, n = 1,  what = 'int', size = 2);
  res.privateSize <- readBin(fileID, n = 1,  what = 'int', size = 2);
  res.clientZone <- readChar(fileID, n = 128);
  #Axis offset
  res.xOffset <- readBin(fileID, n = 1,  what = 'numeric', size = 4);
  res.yOffset <- readBin(fileID, n = 1,  what = 'numeric', size = 4);
  res.zOffset <- readBin(fileID, n = 1,  what = 'numeric', size = 4);
  #temperature skale
  res.tSpacing <- readBin(fileID, n = 1,  what = 'numeric', size = 4);
  res.tOffset <- readBin(fileID, n = 1,  what = 'numeric', size = 4);
  res.tStepUnit <- readChar(fileID, n = 13);
  res.tAxisName <- readChar(fileID, n = 13);
  res.comment <- readChar(fileID, n = res.commentSize);
  res.private <- readChar(fileID, n = res.privateSize);
  #read datapoints
  if (res.sizeOfPoints == 16) res.points <- readBin(fileID,  n = res.totalNumberOfPoints, what = 'int', size = 2);
  if (res.sizeOfPoints == 32) res.points <- readBin(fileID,  n = res.totalNumberOfPoints, what = 'int', size = 4)

  #Generate Axis without Offset;
  res.xAxis <- seq(from = 0, to = res.xSpacing * res.xPoints / res.xUnitRatio, length.out = res.xPoints)
  res.yAxis <- seq(from = 0, to = res.ySpacing * res.yPoints / res.yUnitRatio, length.out = res.yPoints)

  if (type == "matrix") {
    #reshape datapoints into 2D-Matrix
    res.pointsAligned <- matrix(data = res.points, nrow = res.xPoints, ncol = res.yPoints)
    #Skale datapoints without Offset
    res.pointsAligned <- (res.pointsAligned - res.zMin) * res.zSpacing / res.zUnitRatio
    #results
    return(res.pointsAligned)
  }

  if (type == "xyz") return(data.frame(x = rep(res.xAxis, each = res.xPoints),
                                       y = rep(res.yAxis, res.yPoints),
                                       z = (res.points - res.zMin) * res.zSpacing / res.zUnitRatio))
}
