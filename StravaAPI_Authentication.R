


#########################################################
# Function StravaSync
# Glenn Newnham
# 11.06.2022
#
# For generating a strava authentication signature
# based on the folliwng web site:
# https://rviews.rstudio.com/2021/11/22/strava-data/
#
# My app details here:
# https://www.strava.com/settings/api
#
#########################################################

# Prerequisites: Rtools external installation, Create an OAuth Strava app, install the following packages:
# install.packages("usethis")

library(usethis)
library(tarchetypes)
library(conflicted)
library(tidyverse)
library(lubridate)
library(jsonlite)
library(targets)
library(httpuv)
library(httr)
library(pins)
library(httr)
library(fs)
library(readr)

conflict_prefer("filter", "dplyr")

# ### type "usethis::edit_r_environ()" and add thee following to 'C:/Users/new298/Documents/.Renviron'
# STRAVA_KEY = 87643
# STRAVA_SECRET = "fbf47f3fbb53b43af16e9c3abfe1d64f3519d691"

App_Name = "RSync"

define_strava_app <- function() {
	oauth_app(
		appname = App_Name,
		key = Sys.getenv("STRAVA_KEY"),
		secret = Sys.getenv("STRAVA_SECRET")
	)
}

my_app = define_strava_app()
my_app


define_strava_endpoint <- function() {
	oauth_endpoint(request = NULL,
				   authorize = "https://www.strava.com/oauth/authorize",
				   access = "https://www.strava.com/oauth/token")
}

my_endpoint = define_strava_endpoint()
my_endpoint


define_strava_sig <- function(endpoint, app) {
	oauth2.0_token(
		endpoint,
		app,
		scope = "activity:read_all,activity:read,profile:read_all",
		type = NULL,
		use_oob = FALSE,
		as_header = FALSE,
		use_basic_auth = FALSE,
		cache = FALSE
	)
}

my_sig = define_strava_sig(my_endpoint, my_app)
my_sig$credentials

