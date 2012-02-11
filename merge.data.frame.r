#############
# Explenation for the new keep_order parameter:
# keep_order can accept the numbers 1 or 2, in which case it will make sure the resulting merged data.frame will be ordered according to the original order of rows of the data.frame entered to x (if keep_order=1) or to y (if keep_order=2).  If keep_order is missing, merge will continue working as usual.  If keep_order gets some input other then 1 or 2, it will issue a warning that it doesn't accept these values, but will continue working as merge normally would.  Notice that the parameter "sort" is practically overridden when using keep_order (with the value 1 or 2).

# an example is offered at the end of this code chunk

merge.data.frame <- function (x, y, by = intersect(names(x), names(y)), by.x = by, 
    by.y = by, all = FALSE, all.x = all, all.y = all, sort = TRUE, 
    suffixes = c(".x", ".y"), incomparables = NULL, keep_order, ...) 
{
	# if we use the "keep_order" parameter, we might need to modify either the x or y data.frame objects: (either by placing 1 or "x", or by placing 2 or "y")
	if(!missing(keep_order))
	{
	
		# some functions we will use soon:
		add.id.column.to.data <- function(DATA)
		{
			data.frame(DATA, id... = seq_len(nrow(DATA)))
		}
		# example:
		# add.id.column.to.data(data.frame(x = rnorm(5), x2 = rnorm(5)))
		order.by.id...and.remove.it <- function(DATA)
		{
			# gets in a data.frame with the "id..." column.  Orders by it and returns it
			if(!any(colnames(DATA)=="id...")) 
				{
					warning("The function order.by.id...and.remove.it is useful only for data.frame objects that includes the 'id...' order column")
					return(DATA)
				}
			
			
			ss_r <- order(DATA$id...)
			ss_c <- colnames(DATA) != "id..."
			DATA[ss_r, ss_c]		
		}
		# example: 
			# set.seed(3424)
			# x  <- data.frame(x = rnorm(5), x2 = rnorm(5))
			# x2 <- add.id.column.to.data(x)[c(1,4,2,5,3),]
			# x2
			# order.by.id...and.remove.it(x2)
		if(keep_order == "x") keep_order <- 1
		if(keep_order == "y") keep_order <- 2

		if(keep_order == 1) x<-add.id.column.to.data(x)
		if(keep_order == 2) y<-add.id.column.to.data(y)
		# if you didn't get 1 or 2 - issue a warning:
		if(!(any(keep_order == c(1,2)) ))  warning("The parameter 'keep_order' in the function merge.data.frame only accepts the values 1 (for the x data.frame) or 2 (for the y data.frame)")

		# sort <- FALSE
		# notice that if sort was TRUE, using the keep_order parameter will eventually override it...
	}

    fix.by <- function(by, df) {
        if (is.null(by)) 
            by <- numeric()
        by <- as.vector(by)
        nc <- ncol(df)
        if (is.character(by)) 
            by <- match(by, c("row.names", names(df))) - 1L
        else if (is.numeric(by)) {
            if (any(by < 0L) || any(by > nc)) 
                stop("'by' must match numbers of columns")
        }
        else if (is.logical(by)) {
            if (length(by) != nc) 
                stop("'by' must match number of columns")
            by <- seq_along(by)[by]
        }
        else stop("'by' must specify column(s) as numbers, names or logical")
        if (any(is.na(by))) 
            stop("'by' must specify valid column(s)")
        unique(by)
    }
    nx <- nrow(x <- as.data.frame(x))
    ny <- nrow(y <- as.data.frame(y))
    by.x <- fix.by(by.x, x)
    by.y <- fix.by(by.y, y)
    if ((l.b <- length(by.x)) != length(by.y)) 
        stop("'by.x' and 'by.y' specify different numbers of columns")
    if (l.b == 0L) {
        nm <- nm.x <- names(x)
        nm.y <- names(y)
        has.common.nms <- any(cnm <- nm.x %in% nm.y)
        if (has.common.nms) {
            names(x)[cnm] <- paste(nm.x[cnm], suffixes[1L], sep = "")
            cnm <- nm.y %in% nm
            names(y)[cnm] <- paste(nm.y[cnm], suffixes[2L], sep = "")
        }
        if (nx == 0L || ny == 0L) {
            res <- cbind(x[FALSE, ], y[FALSE, ])
        }
        else {
            ij <- expand.grid(seq_len(nx), seq_len(ny))
            res <- cbind(x[ij[, 1L], , drop = FALSE], y[ij[, 
                2L], , drop = FALSE])
        }
    }
    else {
        if (any(by.x == 0L)) {
            x <- cbind(Row.names = I(row.names(x)), x)
            by.x <- by.x + 1L
        }
        if (any(by.y == 0L)) {
            y <- cbind(Row.names = I(row.names(y)), y)
            by.y <- by.y + 1L
        }
        row.names(x) <- NULL
        row.names(y) <- NULL
        if (l.b == 1L) {
            bx <- x[, by.x]
            if (is.factor(bx)) 
                bx <- as.character(bx)
            by <- y[, by.y]
            if (is.factor(by)) 
                by <- as.character(by)
        }
        else {
            bx <- x[, by.x, drop = FALSE]
            by <- y[, by.y, drop = FALSE]
            names(bx) <- names(by) <- paste("V", seq_len(ncol(bx)), 
                sep = "")
            bz <- do.call("paste", c(rbind(bx, by), sep = "\r"))
            bx <- bz[seq_len(nx)]
            by <- bz[nx + seq_len(ny)]
        }
        comm <- match(bx, by, 0L)
        bxy <- bx[comm > 0L]
        xinds <- match(bx, bxy, 0L, incomparables)
        yinds <- match(by, bxy, 0L, incomparables)
        if (nx > 0L && ny > 0L) 
            m <- .Internal(merge(xinds, yinds, all.x, all.y))
        else m <- list(xi = integer(), yi = integer(), x.alone = seq_len(nx), 
            y.alone = seq_len(ny))
        nm <- nm.x <- names(x)[-by.x]
        nm.by <- names(x)[by.x]
        nm.y <- names(y)[-by.y]
        ncx <- ncol(x)
        if (all.x) 
            all.x <- (nxx <- length(m$x.alone)) > 0L
        if (all.y) 
            all.y <- (nyy <- length(m$y.alone)) > 0L
        lxy <- length(m$xi)
        has.common.nms <- any(cnm <- nm.x %in% nm.y)
        if (has.common.nms) 
            nm.x[cnm] <- paste(nm.x[cnm], suffixes[1L], sep = "")
        x <- x[c(m$xi, if (all.x) m$x.alone), c(by.x, seq_len(ncx)[-by.x]), 
            drop = FALSE]
        names(x) <- c(nm.by, nm.x)
        if (all.y) {
            ya <- y[m$y.alone, by.y, drop = FALSE]
            names(ya) <- nm.by
            ya <- cbind(ya, x[rep.int(NA_integer_, nyy), nm.x, 
                drop = FALSE])
            x <- rbind(x, ya)
        }
        if (has.common.nms) {
            cnm <- nm.y %in% nm
            nm.y[cnm] <- paste(nm.y[cnm], suffixes[2L], sep = "")
        }
        y <- y[c(m$yi, if (all.x) rep.int(1L, nxx), if (all.y) m$y.alone), 
            -by.y, drop = FALSE]
        if (all.x) 
            for (i in seq_along(y)) is.na(y[[i]]) <- (lxy + 1L):(lxy + 
                nxx)
        if (has.common.nms) 
            names(y) <- nm.y
        res <- cbind(x, y)
        if (sort) 
            res <- res[if (all.x || all.y) 
                do.call("order", x[, seq_len(l.b), drop = FALSE])
            else sort.list(bx[m$xi]), , drop = FALSE]
    }
    attr(res, "row.names") <- .set_row_names(nrow(res))

	if(!missing(keep_order) && any(keep_order == c(1,2))) return(order.by.id...and.remove.it(res))	
	# notice how it is essential to use && here, since if the first argument is false, it will not be possible to evaluate the second argument
	
    res
}




if(F) # example
{

	
    merge( x.labels, x.vals, by='ref', all.y = T, sort=F )
	merge( x.labels, x.vals, by='ref', all.y = T, sort=F ,keep_order = 2) # yay - works as we wanted it to...

}
