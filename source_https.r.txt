# Filename: source_https.R
# Purpose: function to source raw code from github project
# Author: Tony Bryal
# Date: 2011-12-10
# http://tonybreyal.wordpress.com/2011/11/24/source_https-sourcing-an-r-script-from-github/
	# First found on:
	# http://www.r-bloggers.com/source_https-sourcing-an-r-script-from-github-over-https/

source_https <- function(u, unlink.tmp.certs = FALSE) {
  # load package
  require(RCurl)

  # read script lines from website using a security certificate
  if(!file.exists("cacert.pem")) download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile = "cacert.pem")
  script <- getURL(u, followlocation = TRUE, cainfo = "cacert.pem")
  if(unlink.tmp.certs) unlink("cacert.pem")

  # parase lines and evealuate in the global environement
  eval(parse(text = script), envir= .GlobalEnv)
}

# example:
# source_https("https://raw.github.com/talgalili/R-code-snippets/master/tabular.cast_df.r")
