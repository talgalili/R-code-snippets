tabular.cast_df <- function(xx,...)
{
	# a bunch of assumptions that must be met for this function to work:
	if(!require(reshape)) stop("The {reshape} package must be installed for this function to work")
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

	v <- function(x) x[1L]
	txt <- paste("tabular(value*v*", LEFT , "~" ,RIGHT ,", data = m_xx, suppressLabels  = 2,...)", sep = "")
	# suppressLabels is in order to remove the value and the v labels (which are added so to make sure the information inside the table is presented)	
	eval(parse(text = txt ))
}


# loading libraries
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

# Examples that work:
tabular.cast_df(cast(aqm, month ~ ., mean))
tabular.cast_df(cast(aqm, . ~ temp2, mean))
tabular.cast_df(cast(aqm, . ~ ., mean))
tabular.cast_df(cast(aqm, month ~ temp2, mean))
tabular.cast_df(cast(aqm, month ~ temp2, c(mean,sd)))
tabular.cast_df(cast(aqm, month ~ variable2, c(mean,sd)))
tabular.cast_df(cast(aqm, month ~ variable2*temp2, c(mean,sd)))

# This one doesn't work - nor should it work:
tabular.cast_df(cast(aqm, month ~ variable|temp2, fun.aggregate = mean))	# stops the function, since it doesn't work for 3D objects...


# "BUG"?! this one gets the "temp2" header in all of the first 4 columns.
tabular(month*temp2~variable2*result_variable, data = m_xx)
tabular.cast_df(cast(aqm, month*temp2 ~ variable2, c(mean,sd))) # same problem (but this is a problem with tables not in reshape)
tabular.cast_df(cast(aqm, month*temp2*variable2 ~ ., c(mean,sd))) # same issue



########################
######## bug (?!) report
########################

# loading libraries
library(tables)
library(reshape)

# getting our data ready
names(airquality) <- tolower(names(airquality))
airquality2 <- airquality
airquality2$temp2 <- ifelse(airquality2$temp > median(airquality2$temp), "hot", "cold")
aqm <- melt(airquality2, id=c("month", "day","temp2"), na.rm=TRUE)
colnames(aqm)[4] <- "variable2"	# because otherwise the function is having problem when relying on the melt function of the cast object
head(aqm,4)

xx <- cast(aqm, month*temp2 ~ variable2, c(mean,sd))
m_xx <- melt(xx)
for(i in c(1:5)[-3]) m_xx[,i] <- factor(m_xx[,i])	# without this line I get the following error:
			# Error in term2table(rows[[i]], cols[[j]], data, n) : 
			  # Duplicate values: value and month
v <- function(x) x[1L]
tabular(value*v*month*temp2~variable2*result_variable, data = m_xx)


