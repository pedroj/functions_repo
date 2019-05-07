##########################################################################
# BETA_TF: function to compute taxonomic and functional beta-diversity and their turnover and nestedness-resultant components for a pair of communities        #
#         author: Sébastien Villéger, sebastien.villeger@univ-montp2.fr   
#
#                                                                         
#
#   -> taxonomic beta-diversity and its turnover and nestedness-resultant components are computed according to the decompostion proposed by Baselga (2012)     #
#   -> functional beta-diversity and its turnover and nestedness-resultant components are computed based on convex hulls intersection in a multidimensional functional space #
#
#  functional beta-diversity computation requires libraries "geometry" and "rcdd"
#
# INPUTS:
#
# - "comm": matrix(2*S) with species presence/absence in the two communites of interest coded as 0/1 (NA are considered as absence)
#
# - "traits": matrix(S*D) with traits values of the S species present in the two communities (NA are not allowed)
#
# ->  D must be stricly greater than 1 and number of species in each of the two communities must be strictly higher than D              
#
#                                                                        
# OUTPUT:                                                                
#         a list with:                                                  
#
# - "beta": a matrix (3*2) with values of taxonomic and functional (columns) beta-diversity, turnover and nestedness-resultant components (rows)      
#
# - "richness":: a matrix (5*2) with taxonomic and functional richness of the two communities ("comm1" and "comm2"), as well as richness shared by the two communities ("a") or unique to community 1 ("b") or to community 2 ("c")                       #
# NB: all indices values are rounded to 6 decimals                        
#
##########################################################################

beta_TF<-function(comm,traits) {

################################
# loading required libraries   #
library(geometry)              #
library(rcdd)                  #
################################

###########################
#     checking inputs     #
###########################
# traits
if (ncol(traits)<2)  stop(paste("error : species msut be described by at least 2 traits",sep=""))
if (length(which(is.na(traits)!=0)))  stop(paste("error : NA are not allowed in 'traits'",sep=""))

# number of species
if(ncol(comm)!=nrow(traits)) stop("error : different number of species in 'comm' and 'traits'")
if (sum(comm[1,])<=ncol(traits))  stop(paste("error : 'community 1' must contain at least ",ncol(set1)+1, " species",sep=""))
if (sum(comm[2,])<=ncol(traits))  stop(paste("error : 'community 2' must contain at least ",ncol(set2)+1, " species",sep=""))


# coordinates of species in the multidimensional functional space for each community
set1<-traits[which(comm[1,]==1),]
set2<-traits[which(comm[2,]==1),]

##########################################################################

# computation of convex hull for each of the two communities and for their intersection using functions from 'rcdd' library    

# tranforming species coordinates in the multidimensional space in true rational number written as character strings
set1rep <- d2q(cbind(0, cbind(1, set1)))
set2rep <- d2q(cbind(0, cbind(1, set2)))

# reduce set of points to keep only vertices
polytope1 <- redundant(set1rep, representation = "V")$output
polytope2 <- redundant(set2rep, representation = "V")$output

# changing polytope representation: vertices coordinates -> inequality constraints
H_chset1 <- scdd(polytope1, representation = "V")$output
H_chset2 <- scdd(polytope2, representation = "V")$output

# intersection between the two polytopes
H_inter <- rbind(H_chset1, H_chset2)
V_inter <- scdd(H_inter, representation = "H")$output

# extracting coordinates of vertices
vert_1 <- q2d(polytope1[ , - c(1, 2)])
vert_2 <- q2d(polytope2[ , - c(1, 2)])
vert_1n2 <- q2d(V_inter[ , - c(1, 2)])


# computing convex hull volume of the two polytopes and of their intersection if it exists 
# NB: no intersection if only one vertex in common
vol<-rep(0,3) ; names(vol)<-c("vol_1","vol_2","vol_1n2")
vol["vol_1"]<-convhulln(vert_1,"FA")$vol
vol["vol_2"]<-convhulln(vert_2,"FA")$vol
if (is.matrix(vert_1n2)==T) # vector if one vertex in common
      if( nrow(vert_1n2)>ncol(vert_1n2) ) vol["vol_1n2"]<-convhulln(vert_1n2,"FA")$vol

##########################################################################

# matrices to store results
beta<-matrix(0,3,2,dimnames=list(c("beta","turnover","nestedness"),c("taxonomic","functional")) )
richness<-matrix(0,5,2,dimnames=list(c("comm_1","comm_2","a","b","c"),c("taxonomic","functional")) )

# taxonomic richness
richness[c("comm_1","comm_2"),"taxonomic"]<-apply(comm,1,sum,na.rm=T)
richness["a","taxonomic"]<-sum(comm[1,]*comm[2,],na.rm=T)
richness["b","taxonomic"]<-richness["comm_1","taxonomic"]-richness["a","taxonomic"]
richness["c","taxonomic"]<-richness["comm_2","taxonomic"]-richness["a","taxonomic"]

# functional richness
richness[c("comm_1","comm_2"),"functional"]<-vol[c("vol_1","vol_2")]
richness["a","functional"]<-vol["vol_1n2"]
richness["b","functional"]<-richness["comm_1","functional"]-vol["vol_1n2"]
richness["c","functional"]<-richness["comm_2","functional"]-vol["vol_1n2"]


# taxonomic and functional beta diversity and their turnover and nestedness-resultant components
dis<-function(a,b,c) { (b+c) / (a+b+c) }
turn<-function(a,b,c) { 2*min(b,c) / ( a + 2*min(b,c) )  }
nest<-function(a,b,c) { abs(b-c) / (a+b+c) * a / ( a + 2*min(b,c) )  }

beta["beta","taxonomic"]<-dis( richness["a","taxonomic"], richness["b","taxonomic"], richness["c","taxonomic"]  )
beta["turnover","taxonomic"]<-turn( richness["a","taxonomic"], richness["b","taxonomic"], richness["c","taxonomic"]  )
beta["nestedness","taxonomic"]<- nest( richness["a","taxonomic"], richness["b","taxonomic"], richness["c","taxonomic"]  )

beta["beta","functional"]<-dis( richness["a","functional"], richness["b","functional"], richness["c","functional"]  )
beta["turnover","functional"]<-turn( richness["a","functional"], richness["b","functional"], richness["c","functional"]  )
beta["nestedness","functional"]<- nest( richness["a","functional"], richness["b","functional"], richness["c","functional"]  )

##########################################################################
# results

res<-list( beta=round(beta,6) , richness=round(richness,6) )

return(res)

} # end of function beta_TF


##########################################################################
