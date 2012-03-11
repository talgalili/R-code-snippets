siegel.tukey <-function(x,y,id.col=TRUE,adjust.median=F,rnd=-1,alternative="two.sided",mu=0,paired=FALSE,exact=FALSE,correct=TRUE,conf.int=FALSE,conf.level=0.95)
{
###### published on: http://www.r-statistics.com/2010/02/siegel-tukey-a-non-parametric-test-for-equality-in-variability-r-code/
## Main author of the function:  Daniel Malter

# x: a vector of data

# y: Group indicator (if id.col=TRUE); data of the second group (if
# id.col=FALSE). If y is the group indicator it MUST take 0 or 1 to indicate
# the groups, and x must contain the data for both groups.

# id.col: If TRUE (default), then x is the data column and y is the ID column,
# indicating the groups. If FALSE, x and y are both data columns. id.col must
# be FALSE only if both data columns are of the same length.

# adjust.median: Should between-group differences in medians be leveled before
# performing the test? In certain cases, the Siegel-Tukey test is susceptible
# to median differences and may indicate significant differences in
# variability that, in reality, stem from differences in medians.

# rnd: Should the data be rounded and, if so, to which decimal? The default
# (-1) uses the data as is. Otherwise, rnd must be a non-negative integer.
# Typically, this option is not needed. However, occasionally, differences in
# the precision with which certain functions return values cause the merging
# of two data frames to fail within the siegel.tukey function. Only then
# rounding is necessary. This operation should not be performed if it affects
# the ranks of observations.

# … arguments passed on to the Wilcoxon test. See ?wilcox.test

# Value: Among other output, the function returns the data, the Siegel-Tukey
# ranks, the associated Wilcoxon’s W and the p-value for a Wilcoxon test on
# tie-adjusted Siegel-Tukey ranks (i.e., it performs and returns a
# Siegel-Tukey test). If significant, the group with the smaller rank sum has
# greater variability.

# References: Sidney Siegel and John Wilder Tukey (1960) “A nonparametric sum
# of ranks procedure for relative spread in unpaired samples.” Journal of the
# American Statistical Association. See also, David J. Sheskin (2004)
# ”Handbook of parametric and nonparametric statistical procedures.” 3rd
# edition. Chapman and Hall/CRC. Boca Raton, FL.

# Notes: The Siegel-Tukey test has relatively low power and may, under certain
# conditions, indicate significance due to differences in medians rather than
# differences in variabilities (consider using the argument adjust.median).

# Output (in this order)

# 1. Group medians (after median adjustment if specified)
# 2. Wilcoxon-test for between-group differences in medians (after the median
# adjustment if specified)
# 3. Data, group membership, and the Siegel-Tukey ranks
# 4. Mean Siegel-Tukey rank by group (smaller values indicate greater
# variability)
# 5. Siegel-Tukey test (Wilcoxon test on tie-adjusted Siegel-Tukey ranks)


if(id.col==FALSE){
  data=data.frame(c(x,y),rep(c(0,1),c(length(x),length(y))))
  } else {
       data=data.frame(x,y)
  }
names(data)=c("x","y")
data=data[order(data$x),]
if(rnd>-1){data$x=round(data$x,rnd)}

if(adjust.median==T){
   cat("\n","Adjusting medians...","\n",sep="")
   data$x[data$y==0]=data$x[data$y==0]-(median(data$x[data$y==0]))
   data$x[data$y==1]=data$x[data$y==1]-(median(data$x[data$y==1]))
}
cat("\n","Median of group 1 = ",median(data$x[data$y==0]),"\n",sep="")
cat("Median of group 2 = ",median(data$x[data$y==1]),"\n","\n",sep="")
cat("Testing median differences...","\n")
print(wilcox.test(data$x[data$y==0],data$x[data$y==1]))

# The following must be done for the case when id.col==F
x <- data$x
y <- data$y

cat("Performing Siegel-Tukey rank transformation...","\n","\n")

 
 
sort.x<-sort(data$x)
sort.id<-data$y[order(data$x)]

data.matrix<-data.frame(sort.x,sort.id)

base1<-c(1,4)
iterator1<-matrix(seq(from=1,to=length(x),by=4))-1
rank1<-apply(iterator1,1,function(x) x+base1)

iterator2<-matrix(seq(from=2,to=length(x),by=4))
base2<-c(0,1)
rank2<-apply(iterator2,1,function(x) x+base2)

#print(rank1)
#print(rank2)

if(length(rank1)==length(rank2)){
       rank<-c(rank1[1:floor(length(x)/2)],rev(rank2[1:ceiling(length(x)/2)]))
       } else{
       rank<-c(rank1[1:ceiling(length(x)/2)],rev(rank2[1:floor(length(x)/2)]))
}


unique.ranks<-tapply(rank,sort.x,mean)
unique.x<-as.numeric(as.character(names(unique.ranks)))

rank.matrix<-data.frame(unique.x,unique.ranks)

ST.matrix<-merge(data.matrix,rank.matrix,by.x="sort.x",by.y="unique.x")

print(ST.matrix)

cat("\n","Performing Siegel-Tukey test...","\n",sep="")

ranks0<-ST.matrix$unique.ranks[ST.matrix$sort.id==0]
ranks1<-ST.matrix$unique.ranks[ST.matrix$sort.id==1]

cat("\n","Mean rank of group 0: ",mean(ranks0),"\n",sep="")
cat("Mean rank of group 1: ",mean(ranks1),"\n",sep="")

print(wilcox.test(ranks0,ranks1,alternative=alternative,mu=mu,paired=paired,exact=exact,correct=correct,conf.int=conf.int,conf.level=conf.level))
}



if(F) {

#Example:
 
x=c(4,4,5,5,6,6)
y=c(0,0,1,9,10,10)
siegel.tukey(x,y, F)

# example for a non equal number of cases:
x=c(4,4,5,5,6,6)
y=c(0,0,1,9,10)
siegel.tukey(x,y,F)


x <- c(33, 62, 84, 85, 88, 93, 97, 4, 16, 48, 51, 66, 98)
id <- c(0,0,0,0,0,0,0,1,1,1,1,1,1)

siegel.tukey(x,id,adjust.median=F,exact=T)

x<-c(0,0,1,4,4,5,5,6,6,9,10,10)
id<-c(0,0,0,1,1,1,1,1,1,0,0,0)

siegel.tukey(x,id)

x <- c(85,106,96, 105, 104, 108, 86)
id<-c(0,0,1,1,1,1,1)

siegel.tukey(x,id)

x<-c(177,200,227,230,232,268,272,297,47,105,126,142,158,172,197,220,225,230,262,270)
id<-c(rep(0,8),rep(1,12))

siegel.tukey(x,id,adjust.median=T)




x=c(33,62,84,85,88,93,97)
y=c(4,16,48,51,66,98)
 
siegel.tukey(x,y)


}

