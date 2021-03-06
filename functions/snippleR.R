## these are functions that I sometimes use in my code
## they are intended to be able to be run on any computer
## with R installed.

 
#################################
###
### Settings
###
#################################
options(stringsAsFactors=FALSE)
 
 
#################################
###
### Aliases for frequently used functions
###
#################################
s <- base::summary;
h <- utils::head;
n <- base::names;
as.dataframe<-base::data.frame;
 
#################################
###
### FUNCTION: write.txt
###
#################################
# write.table with my most frequently used settings
#
# Args: 
# ... (vector of items to paste)
#
# Returns:
# 
write.txt<-function (x, file="", quote=FALSE, sep="\t", row.names=FALSE, ...){
 
	write.table(x, file, quote=quote, sep=sep, row.names=row.names, ...)
 
	}
 
 
#################################
###
### FUNCTION: read.txt
###
#################################
# read.table with my most frequently used settings
#
# Args: 
# ... (vector of items to paste)
#
# Returns:
# data frame
# 
read.txt<-function (file="", sep="\t", header=TRUE,row.names=NULL, ...){
 
	read.table(file, sep= sep, header= header, row.names= row.names, ...)
 
	}
 
 
#################################
###
### FUNCTION: pasteNS
###
#################################
# Paste arguments with no spaces between them
#
# Args: 
# ... (vector of items to paste)
#
# Returns:
# character vector result of paste
# 
pasteNS<-function (...){
 
	paste(..., sep="")
 
	}
   
#################################
###
### FUNCTION: convertColsToNumeric
###
#################################
#   Converts specified columns to numeric
#
# Args: 
# data frame, column indices
#
# Returns:
# original DF with specified columns changed
# 
convertColsToNumeric<-function(dfConv,colIndices){
	for (i in colIndices){
		dfConv[,i]=as.numeric(dfConv[,i])
		}
		return(dfConv)
		}
 
#################################
###
### FUNCTION: printNumberedVector
###
#################################
# 
# prints a vector to screen, one item per line with each line numbered
#
# Input: a vector
#
# Returns:
# nothing
# 
##
printNumberedVector<-function(someVector){
cat(paste(1:length(someVector ), someVector,"\n"))
}
 
  
#################################
###
### FUNCTION: fixRowNamesColumn
###
#################################
# 
# moves data in "Row.names" column to rownames
#
# Args: df
#
# Returns:
# dataframe
# 
##
 
fixRowNamesColumn<-function(mergeResultsDF){
 
	#mergeResultsDF= popByAgeWide
	rownames(mergeResultsDF)= mergeResultsDF$Row.names
	mergeResultsDF= mergeResultsDF[,2:ncol(mergeResultsDF)]
	}
 
 
#################################
###
### FUNCTION: getLabelsAndMidPointsOfGroups
###
#################################
# 
# useful for labeling axes and creating 
#
# Args: df, valname, byname, passFUN, fnDesc
# Example Vals
	# df= creatDF
	# valname="creat"
	# byname="breed"
	# passFUN=length
	# fnDesc="count"
	# fReplaceValName=FALSE  ## the default is to append
#
# Returns:
# dataframe
# 
##
 
 
getLabelsAndMidPointsOfGroups<- function(charVec,alternatingColors=c("grey", "black")){
 
plotAnnotation<-data.frame(values= charVec)
previousVal=	 c("", charVec[1:(length(charVec )-1)])
switchPoint= charVec != previousVal
groupBreaks=data.frame(start=which(switchPoint))
groupBreaks$length= c(groupBreaks $start[2:nrow(groupBreaks)],length(charVec)+1)-groupBreaks$start
groupBreaks$end= groupBreaks$start+   (groupBreaks$length-1)
groupBreaks$midpoint=round(rowMeans(groupBreaks[,c("start","end")]),0)
groupBreaks$label= plotAnnotation$values[groupBreaks$midpoint]
 
 
groupBreaks$color= alternatingColors[1]
groupBreaks$color[ c(T,F) ]=alternatingColors[2]
 
 
plotAnnotation$color=as.character(unlist(apply(groupBreaks,1,function(x){
	    rep(x["color"],x["length"])
	    })))
 
plotAnnotation$labelnames= ""
 
plotAnnotation$labelnames[groupBreaks$midpoint]=plotAnnotation$value[groupBreaks$midpoint]
list(labelsDF=plotAnnotation, breaksDF=groupBreaks)
}
 
 
 
#################################
###
### FUNCTION: prettyAggregate
###
#################################
# wrapper for aggregate
# renames columns based on input
#
# Args: df, valname, byname, passFUN, fnDesc
# Example Vals
	# df= creatDF
	# valname="creat"
	# byname="breed"
	# passFUN=length
	# fnDesc="count"
	# fReplaceValName=FALSE  ## the default is to append
#
# Returns:
# dataframe
# 
##
 
prettyAggregate<-function(df, valname, byname, passFUN, fnDesc, fReplaceValName=FALSE){
	if (fReplaceValName) {
		agColName=fnDesc
	} else {
		agColName=pasteNS(valname,"_", fnDesc)
	}
	result=aggregate(df[, valname],by=list(df[, byname]), FUN= passFUN)
	colnames(result)=c(byname, agColName)
	return(result)
	}
 
 
 
#################################
###
### FUNCTION: cbindList/rbindList
###
#################################
# tries to return a dataframe from a list of bind-able vectors or dataframes
# based on first example in Reduce help
#
# Args: 
# List
#
# Returns:
# dataframe
# 
##
 
cbindList <- function(x) Reduce("cbind", x)
rbindList <-function(x) Reduce("rbind",x)
 
 
#################################
###
### FUNCTION: bindRepeat
###
#################################
# returns a dataframe composed of n 
# copies of the vector
 
# Args: 
# vector2Rep - vector to be repeated
# n - times to repeat
## abandoned:
# fBindAsColumns - optional, boolean
#
# Returns:
# dataframe
# 
##
bindRepeat <- function(vector2Rep,n){
	a <- list()
	 for(i in 1:n) a[[i]] <- vector2Rep
 	rbindList(a)
}
 
  
 
#################################
###
### FUNCTION: textListToVec
###
#################################
# Convert a chunk of text (one item per line) to a vector
# Just type textListToVec(" and then paste in the lines and type ")
#
# Args: 
# newline-separated list
#
# Returns:
# vector
# 
##
 
 
textListToVec <-function(a) strsplit(a,"\n")[[1]]
 
 
 
#################################
###
### FUNCTION: tableDF
###
#################################
# performs the table function
# but returns a well-formatted data.frame
#
# Args: 
# valVector
#
# Returns:
# dataframe
# 
##
 
tableDF<-function(valVector=c("a", "a", "b")){
	t=data.frame(as.matrix(table(valVector)))
	t2=data.frame(value=row.names(t), frequency=t[,1])
	row.names(t2)=row.names(t)#	colnames(t)=c("frequency")
	return(t2)
	}
 
 
 
 
#################################
###
### FUNCTION: interleave
###
#################################
# Interleave two vectorsAdd timestamp and data to plot
#
# Args: 
# v1
# v2
#
# Returns:
# vector
# 
##
 
interleave <- function(v1,v2)
{
ord1 <- 2*(1:length(v1))-1
ord2 <- 2*(1:length(v2))
c(v1,v2)[order(c(ord1,ord2))]
} 
 
 
 
#################################
###
### FUNCTION: stampPlot
###
#################################
# Add timestamp and data to plot
#
# Args: 
# desc.txt=""
# fOuter=FALSE
#
# Returns:
# nothinbg
# 
##
 
stampPlot=function(desc.txt="",fOuter=FALSE	){
	if(fOuter) {
		pAdj=-2
		} else {
		pAdj=-0.6
		}
mtext(paste(desc.txt, hbTimeStamp()),side=4,outer= fOuter,padj= pAdj,cex=.8)
}
 
 
 
#################################
###
### FUNCTION: snap
###
#################################
# like head, but also limits the number of columns. default is 6 columns 6 rows.
 
 snap <-function (df,rowLim=6,colLim=6) {
	# troubleshooting
		#age=18:29
		#height=runif(12,62,74)
		#df =data.frame(age=age,height=height)
 
 
	if (is(df)[1]=="data.frame" | is(df)[1]=="matrix"){
		nrowDF =nrow(df)
		ncolDF =ncol(df)
		if (rowLim<nrowDF) nrowDF=rowLim
		if (colLim<ncolDF) ncolDF=colLim
	print(df[1:nrowDF,1:ncolDF])	
		} else {
		print("cannot parse input")
			}
	}
 
 
#################################
###
### FUNCTION: getAllListSubItemsByIndex
###
#################################
# get the nth sub item from each item
#
# Args: 
# theList, list to pull from
# theIndex, the index of desired items
#
# Returns:
# vector or results
# 
getAllListSubItemsByIndex <-function (theList, theIndex){
 
paste(lapply(theList,function(x) x=x[[theIndex]]))
 
 
}
 
 
 
#################################
###
### FUNCTION: greaterOf
###
#################################
# get the greater of two items
#
# Args: 
# values to compare
#
# Returns:
# greater value
# 
greaterOf <- function (x,y) {
 
	if (x>y | y==x){
		x
	} else {
		if (y>x) {
			y
		} else {
			NA
		}
	}
 
}
 
 
#################################
###
### FUNCTION: lesserOf
###
#################################
# get the lesser of two items
#
# Args: 
# values to compare
#
# Returns:
# lesser value
# 
lesserOf <-  function (x,y) {
 
	if (x<y | y==x ){
		x
	} else {
		if (y<x) {
			y
		} else {
				NA
		}
	}
 
}
 
 
 
### FUNCTION: isBetween
###
#################################
# is one value between two others
#
# Args: 
# 3 values
#
# Returns:
# TRUE or FALSE
#
isBetween <- function (x,y,z) {
 
	x>y & x<z	
 
}
 
 
#################################
###
### FUNCTION: initialCap
###
#################################
# Converts each string in a vector to initial upper case
#
# Args: 
# wordsToConvert: vector of genes symbols
#
# Returns:
# vector with changed words
# 
initialCap <- function(wordsToConvert) {
 
	return_list<-NULL
	for (i in 1:length(wordsToConvert)){
		r<-tolower(wordsToConvert[i])
	    s <- strsplit(r, " ")[[1]]
    	return_list[i]=paste(toupper(substring(s, 1,1)), substring(s, 2), sep="", collapse=" ")
       }
       return(return_list)
}
 
#################################
###
### FUNCTION: allIdentical
###
#################################
# tests whether each item in a vector is identical
#
# Args: 
# vectorToTest: vector 
#
# Returns:
# TRUE or FALSE
# 
 
 
allIdentical <- function(vectorToTest){
	sum(! vectorToTest %in% vectorToTest[1])}
 
 
#################################
###
### FUNCTION: set_up_plot
###
#################################
# Determines axis  values from data
# adds a time stamp
#
# Args: 
# x & y data
# fPlotFromZero
# fPlotEvenAxes
 
# Returns:
# plot
# 
 
 
set_up_plot <-function(x,y,fPlotFromZero=TRUE, fPlotEvenAxes=FALSE,fDateStamp=TRUE,stampText="",fOuter=FALSE,xAxLabel=colnames(x), yAxLabel=colnames(y), ...){
#print(names(x))
#	if 
# xAxName=colnames(x)
# yAxLabel=colnames(y)
if(fPlotEvenAxes){
	x=c(min(c(x,y)),max(c(x,y)))
	y=x
	}
if (fPlotFromZero) {
	plot_y_min=0
	plot_x_min=0
} else {
	plot_y_min=min(y)
	plot_x_min=min(x)
} 
 
	plot_y_max=max(y)
	plot_x_max=max(x)
 
	plot(c(plot_x_min, plot_x_max),c(plot_y_min, plot_y_max), xlab= xAxLabel , ylab= yAxLabel , type="n", ... )
 
	if (fDateStamp) stampPlot(stampText,fOuter= fOuter)
 
	 }
 
 
 
 
 
#################################
###
### FUNCTION: hbTimeStamp
###
#################################
# Get formatted time and date
#
# Args (all optional): 
# sepYMD 
# sepHMS
# sepDateTime
 
# Returns:
# character vector
# 
 
 
hbTimeStamp<-function(sepYMD="-", sepHMS=".", sepDateTime="_", dateVars=c("Y","m","d"), timeVars=c("I", "M", "S")){
	dateSpec=paste(paste("%", dateVars, sep=""), collapse= sepYMD)
	timeSpec=paste(paste("%", timeVars, sep=""), collapse= sepHMS)
 
	paste(format(Sys.time(), dateSpec),paste(format(Sys.time(), timeSpec),substring(format(Sys.time(), "%r"),10,11),sep=""),sep= sepDateTime)
	}
 
 
#################################
###
### FUNCTION: breakPlotIntoPages
###
#################################
# Breaks data into page-size chunks and calls a plotting function for each chunk
#
# Args: 
# dataframe with plot data
# optional: desired rows per page
 
# Returns:
# multiple plots
# 
 
breakPlotIntoPages<-function(multipagePlotData,rowsPerPage=40,varList){
 
#if (! "myPlot" %in% varList) {
#	print("the 'myPlot' function must exist")
#	return()
#	}
 
plotRows=nrow(multipagePlotData)
dataBreaks=unique(c(seq(from=1,to=plotRows,by=rowsPerPage),plotRows))
breakCount=length(dataBreaks)
penultimateBreakCount= breakCount-1
lastPageRowCount= dataBreaks[breakCount ]-dataBreaks[penultimateBreakCount]
 
 
for (i in 1:(length(dataBreaks)-1)){
	brokenData=multipagePlotData[dataBreaks[i]:dataBreaks[i+1],]
	xLimVals=c(0,rowsPerPage*1.6)
	barPos=myPlot(brokenData,xLimVals,1)
	}
 
}
 
 
 
## a useful function: rev() for strings
strReverse <- function(x)
        sapply(lapply(strsplit(x, NULL), rev), paste, collapse="")
##strReverse(c("abc", "Statistics"))
 
 
 
#make transparenbt
 
 
 
 
################################
##
## FUNCTION: makeTransparent
 
##
################################
#Add transparency to colors
 
# Args: 
# vector of colors in name or hex format, e.g. grey or #FFCDFF
# desiredPctTranparent
 
# Returns:
# same colors with transparency added
 
#
 
makeTransparent<-function(colorVector=c("#FFCDFF", "#C0108C", "#CB7600"), desiredPctTranparent=50){
 
#colorVector=c("grey", "blue")
if (substr(colorVector[1],0,1)!="#") {
	colorVector =rgb(t(col2rgb(colorVector)/255))
	} 
s=seq(0,255,length.out=21)
transparencyCodes=data.frame(pctTransparent=100*(1-(s/255)))
t2=unlist(lapply(s,function(x) rgb(0,100,0,x,maxColorValue=255)))
transparencyCodes$hexSuffix= substring(t2,8,9)
paste(rgb(t(col2rgb(colorVector)),maxColorValue=255), transparencyCodes[transparencyCodes$pctTransparent== as.character(desiredPctTranparent),2],sep="")
 
}
 
 
 
 
#################################
###
### FUNCTION: plotMultipleCors
###
#################################
# Plots multiple vectors against 
# a single vector and returns 
# correlations
 
#
# Args: 
# plotData 
## plot data should have x values in first col, and any number of cols after that can be Y
## column names determine legend text
# vColors -- a vector of colors the length of each Y col in plotData
# ylabText -- text for Y label
# legendPos -- legend position, like "topleft"
# fNormalizeY -- normalize each Y value so the max val is 100 
 
# Returns:
# plot
# 
 
plotMultipleCors <-function(plotData, vColors="",ylabText="", xlabText ="Breed average creatinine level",legendPos="topleft",fNormalizeY=TRUE,...){
## plot data should have x values in first col, and any number of cols after that can be Y
#test plotData= gAcdDesc[,c("mean","se","sd","Height","Weight")]
 library(plotrix)
 x= plotData[,1]
 if (fNormalizeY) {
		plotData[,2:ncol(plotData)]=apply(plotData[,2:ncol(plotData)],2,function(y){
# 			100*y/max(y)
 			rescale(y,c(0,100))
 			})
 		}
 
 maxY=max(plotData[,2:ncol(plotData)],na.rm=TRUE)
 set_up_plot( x,0: maxY,xAxLabel= xlabText, yAxLabel=ylabText ,...)
 
 
# if (vColors =""){
 	nColorsNeeded= ncol(plotData)-1
 	pal1=brewer.pal(8,"Set1")
	vColors=pal1[rep(1:length(pal1), nColorsNeeded)[1: nColorsNeeded]]
#	}
 
	plotSettings=data.frame(dataName =colnames(plotData[,2:ncol(plotData)]),color= vColors)
	plotSettings$notNA=apply(plotData[,2:ncol(plotData)],2,function(x){
		sum(!is.na(x))
		})
 	plotSettings$legendText= plotSettings$dataName
	plotSettings$pch=0:(nrow(plotSettings)-1)
 for (i in 1:nrow(plotSettings)){
 	y=plotData[,(i+1)]
	points(x,y,col= plotSettings$color[i],pch=plotSettings$pch[i])
 abline(lm(y ~ x),col= plotSettings$color[i])
 rsq <-cor(x,y, use="complete.obs")
  plotSettings$corr_value[i]  <- format(c(rsq, 0.123456789), digits=2)[1]
 ##text(mean(x),mean(y),substitute(paste("R"^{2}, " = "*x),list(x= corr_value) ),cex=1,col= vColors[i-1])
 ##legend(210, 110, bquote(r^2 ==.(format(summary(regression)$adj.r.squared,digits=3)))) 
	print(plotSettings$color[i])
  	 } #end for loop
  legend(legendPos,	legend=pasteNS(plotSettings$legendText,", Rsq=",plotSettings$corr_value, ", n=",plotSettings$notNA), pch= plotSettings$pch, col= plotSettings$color)
 
 # colnames(plotData[,2:ncol(plotData)]), lty=1, col=vColors)
 
  	 } ## end function
 
#################################
###
### FUNCTION: plotCors
###
#################################
# Plots multiple vectors against 
# a single vector and returns 
# correlations
 
#
# Args: 
# plotData 
## plot data should have x values in first col, and any number of cols after that can be Y
## column names determine legend text
# vColors -- a vector of colors the length of each Y col in plotData
# ylabText -- text for Y label
# legendPos -- legend position, like "topleft"
# fNormalizeY -- normalize each Y value so the max val is 100 
 
# Returns:
# plot
# 
 
plotCors<-function(plotData, vColors="",ylabText=colnames(plotData)[2], xlabText =colnames(plotData)[1],legendPos="topleft",fNormalizeY=TRUE,...){
## plot data should have x values in first col, and any number of cols after that can be Y
#test plotData= gAcdDesc[,c("mean","se","sd","Height","Weight")]
#plotData=na.omit(plotData)
 x= plotData[,1]
 	if (fNormalizeY) {
 		plotData[,2]=100*plotData[,2]/max(plotData[,2])
 		}
y=plotData[,2]
 
 set_up_plot( x,y,xAxLabel= xlabText, yAxLabel=ylabText ,...)
 
	points(x,y)#,col= plotSettings$color[i])
 abline(lm(y ~ x)) #,col= plotSettings$color[i])
 rsq <-cor(x,y, use="complete.obs")
  rsqTxt  <- format(c(rsq, 0.123456789), digits=2)[1]
 ##text(mean(x),mean(y),substitute(paste("R"^{2}, " = "*x),list(x= corr_value) ),cex=1,col= vColors[i-1])
 ##legend(210, 110, bquote(r^2 ==.(format(summary(regression)$adj.r.squared,digits=3)))) 
	#print(plotSettings$color[i])
 title(sub=pasteNS("Rsq=", rsqTxt ))
 
 # colnames(plotData[,colSpec]), lty=1, col=vColors)
 
  	 } ## end function
 
