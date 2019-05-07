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
### FUNCTION: sourceDir
###
#################################
# sources any *.R files in a given directory
#
# Args: path 
# trace is a boolean to print file names as they are sourced
#
# Returns: nothing
# 
# this function is copied from the "source" help text
# 
sourceDir <- function(path, trace = TRUE, ...) {
    for (nm in list.files(path, pattern = "\\.[Rr]$")) {
        if(trace) cat(nm,":")           
        source(file.path(path, nm), ...)
        if(trace) cat("\n")
    }
}

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
write.txt<-function (x, file="", quote=FALSE, sep="\t", 
                     row.names=FALSE, ...){
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
read.txt<-function (file="", sep="\t", header=TRUE,
                    row.names=NULL, dec=".", na.strings="NA", ...){
    read.table(file, sep= sep, header= header, row.names= row.names, ...)
}

