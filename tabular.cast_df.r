tabular.cast_df <- function(xx,...)
{
	# a bunch of assumptions that must be met for this function to work:
	if(!require(reshape)) stop("The {reshape} package must be installed for this function to work")
	if(!require(tables)) stop("The {tables} package must be installed for this function to work")
	if(! any(class(xx) == "cast_df")) stop("This function only works for cast_df objects")
	# xx is a casted object

	m_xx <- melt(xx)
	rdimnames_xx <- attr(xx, "rdimnames")
	if(length(rdimnames_xx)>2) stop("This function only works for 2D tables")

	ROWS <- colnames(rdimnames_xx[[1]])
	COLUMNS <- colnames(rdimnames_xx[[2]])
	colnames_m_xx <- colnames(m_xx)

	# This is for cases when one of the equations has "(all)" in them due to something like cast(DATA, x ~.)
	if(all(ROWS == "value")) ROWS <- 1
	if(all(COLUMNS == "value")) COLUMNS <- 1

	if(any(colnames_m_xx == "value.1")) {	# then we are supposed to have a "(all)" case (e.g: cast(DATA, .~x)  ) 
		# m_xx <- m_xx[, -c(which(colnames_m_xx == "value")[-1])] # then remove the column with no value but "(all)"	# This would only work for cast(DATA, x~.) and not for cast(DATA, .~x)
		m_xx[,"value"]   <- m_xx[,"value.1"]
		column_where_all_is <- which(colnames_m_xx  == "value.1")
		m_xx <- m_xx[, -column_where_all_is] # then remove the column with no value but "(all)"
		colnames_m_xx <- colnames(m_xx)
		}
	if(sum(colnames_m_xx == "value") > 1 ) {	# then we are supposed to have a "(all)" case (e.g: cast(DATA, x~.)  ) 
		# m_xx <- m_xx[, -c(which(colnames_m_xx == "value")[-1])] # then remove the column with no value but "(all)"	# This would only work for cast(DATA, x~.) and not for cast(DATA, .~x)
		column_where_all_is <- which(m_xx[1,] == "(all)")
		m_xx <- m_xx[, -column_where_all_is] # then remove the column with no value but "(all)"
		colnames_m_xx <- colnames(m_xx)
		}

	LEFT <- paste(ROWS , collapse="*")
	RIGHT <- paste(COLUMNS , collapse="*")

	# turn all ROWS/COLUMNS variables into factors - so to make sure that the tabular will work on them as we expect
	column_to_turn_into_factor <- intersect(c(ROWS, COLUMNS), colnames_m_xx)	# this removes the "1"s in case of cast(DATA, x~.) 
	for(i in column_to_turn_into_factor) m_xx[,i] <- factor(m_xx[,i])

	# Further motivation for the above two lines have been given by Duncan (on 11.12.11): 
		# The problem here is that tabular() needs to figure out what you want to do with each variable.  value and month are both numeric, so it can't tell which one you want as an analysis variable.  temp2 is a character variable; those are also treated as possible analysis variables, but perhaps they should be treated like factors instead.  (But then there would need to be syntax to say "don't treat this character as a factor".)
		# So another way to get what you want would be to change the table spec to
		# tabular(value*v*factor(month)*factor(temp2) ~ variable2*result_variable, data = m_xx)
		# but this changes the headings too; so maybe I should have a function Factor that does what factor() does without changing the heading. Here's a quick definition:
		# Factor <- function( x ) substitute(Heading(xname)*x, list(xname = as.name(substitute(x)), x = factor(x)))
		# tabular(value*v*Factor(month)*Factor(temp2)~variable2*result_variable, data = melt(xx), suppress=2)
	
	v <- function(x) x[1L]
	txt <- paste("tabular(value*v*", LEFT , "~" ,RIGHT ,", data = m_xx, suppressLabels  = 2,...)", sep = "")
	# suppressLabels is in order to remove the value and the v labels (which are added so to make sure the information inside the table is presented)	
	eval(parse(text = txt ))
}



if(F) {

###########################################
###### Examples
###########################################

###### loading libraries and data
###########################################

library(tables)
library(reshape)

# getting our data ready
names(airquality) <- tolower(names(airquality))
airquality2 <- airquality
airquality2$temp2 <- ifelse(airquality2$temp > median(airquality2$temp), "hot", "cold")
aqm <- melt(airquality2, id=c("month", "day","temp2"), na.rm=TRUE)
colnames(aqm)[4] <- "variable2"	# because otherwise the function is having problem when relying on the melt function of the cast object
head(aqm,4)
  # month day temp2 variable2 value
# 1     5   1  cold     ozone    41
# 2     5   2  cold     ozone    36
# 3     5   3  cold     ozone    12


###### Running examples:
###########################################

# Examples that work:

# Trivial, but works:
tabular.cast_df(cast(aqm, month ~ ., mean))
# month All  
 # 5     68.71
 # 6     87.38
 # 7     93.50
 # 8     79.71
 # 9     71.83

tabular.cast_df(cast(aqm, . ~ temp2, mean))
    # cold  hot  
 # All 69.88 91.06

tabular.cast_df(cast(aqm, . ~ ., mean))
     # All  
 # All 80.06

tabular.cast_df(cast(aqm, month ~ temp2, mean))
 # month cold  hot  
 # 5     67.64 98.22
 # 6     79.63 98.83
 # 7     58.15 96.05
 # 8     69.21 84.83
 # 9     67.25 80.86

 # Here starts the cool examples:
tabular.cast_df(cast(aqm, month ~ temp2, c(mean,sd)))
      # cold        hot         
 # month mean  sd    mean  sd    
 # 5     67.64 86.21 98.22 106.02
 # 6     79.63 84.49 98.83  95.99
 # 7     58.15 87.92 96.05  89.44
 # 8     69.21 71.18 84.83  73.06
 # 9     67.25 75.87 80.86  68.43

tabular.cast_df(cast(aqm, month ~ variable2, c(mean,sd)))
       # ozone       solar.r        wind         temp       
 # month mean  sd    mean    sd     mean   sd    mean  sd   
 # 5     23.62 22.22 181.3   115.08 11.623 3.531 65.55 6.855
 # 6     29.44 18.21 190.2    92.88 10.267 3.769 79.10 6.599
 # 7     59.12 31.64 216.5    80.57  8.942 3.036 83.90 4.316
 # 8     59.96 39.68 171.9    76.83  8.794 3.226 83.97 6.585
 # 9     31.45 24.14 167.4    79.12 10.180 3.461 76.90 8.356

tabular.cast_df(cast(aqm, month ~ variable2*temp2, c(mean,sd)))
       # ozone                    solar.r                    wind                     temp                    
       # cold         hot         cold           hot         cold        hot          cold         hot        
 # month mean  sd     mean  sd    mean    sd     mean  sd    mean  sd    mean   sd    mean  sd     mean  sd   
 # 5     22.76 22.242 45.00   NaN 178.6   116.47 252.0    NA 11.51 3.538 14.900   NaN 65.03 6.3326 81.00    NA
 # 6     20.60 10.015 40.50 21.38 170.0    97.58 220.4 79.78 10.38 4.578 10.092 2.237 74.89 3.8177 85.42 4.441
 # 7     13.00  4.243 62.96 29.78 135.5   181.73 222.1 72.63 10.60 5.233  8.828 2.948 73.50 0.7071 84.62 3.416
 # 8     31.75 16.360 72.50 40.80 149.7    91.19 184.2 67.26 10.88 2.819  7.800 2.970 77.00 2.2111 87.29 5.198
 # 9     19.95  8.847 53.30 29.10 163.6    90.30 175.1 53.46 10.95 2.808  8.640 4.243 72.15 4.6257 86.40 5.420

 
# This one doesn't work - nor should it work:
tabular.cast_df(cast(aqm, month ~ variable|temp2, fun.aggregate = mean))	# stops the function, since it doesn't work for 3D objects...

####################
# Bug that was fixed in tables version 0.5.20:
# "BUG" this one gets the "temp2" header in all of the first 2 columns 
tabular.cast_df(cast(aqm, month*temp2 ~ variable2, c(mean,sd))) # same problem (but this is a problem with tables not in reshape)
tabular.cast_df(cast(aqm, month*temp2*variable2 ~ ., c(mean,sd))) # same issue
 # variable2 variable2 variable2 mean    sd      
 # 5         cold      ozone      22.760  22.2416
                     # solar.r   178.577 116.4664
                     # wind       11.513   3.5381
                     # temp       65.033   6.3326
           # hot       ozone      45.000      NaN
                     # solar.r   252.000       NA
                     # wind       14.900      NaN
                     # temp       81.000       NA
	# ......


}  # end of if(F)