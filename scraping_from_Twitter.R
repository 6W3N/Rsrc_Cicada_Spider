## scraping_from_Google.R to download tweets with images

## Refs
### https://cran.r-project.org/web/packages/rtweet/vignettes/auth.html
### https://www.earthdatascience.org/courses/earth-analytics/get-data-using-apis/use-twitter-api-r/
### https://rud.is/books/21-recipes/

## Install library
pacman::p_load(tidyverse, rtweet, foreach)

## Twitter API keys and tokens setting 
### Note: The following settings will differ for each individual
appname <- "hogehoge" #The appname is named by you on https://developer.twitter.com/.
key <- "*************************"  #Each asterisk represents an alphanumeric character. CAUTION: the length is variable.
secret <- "**************************************************"
broken <- "******************************************************************************************************************"
access_token <- "**************************************************"
access_secret <- "*********************************************"

## Create token named "twitter_token"
twitter_token <- create_token(
	app = appname,
	consumer_key = key,
	consumer_secret = secret,
	access_token = access_token,
	access_secret = access_secret
)

## Function
twimg_search_func <- function(search_text_semi, search_text_kumo, fromD="200603210000", toD="201312312359"){
	## Creating word combinations passed to the function
	search_text_semi_kumo <- expand.grid(semi=search_text_semi, kumo=search_text_kumo) %>% 
		mutate(text_tmp = str_c(semi, kumo, sep = " ")) %>% 
		mutate(text = str_c("(", text_tmp, ")")) %>% 
		select(text)
	## Run a search for a tweet
	search_results <- foreach(i=1:nrow(search_text_semi_kumo), .combine=rbind) %do% {
		search_item <- search_text_semi_kumo %>% slice(i) %>% pull(text)
		tmp <- search_fullarchive(q=str_c(search_item, "has:images",  sep=" "), 	# has:images means that it select tweet with images
			env_name="env_name",							# Note: env_name is different for each user
			fromDate=fromD,
			toDate=toD) %>%
			mutate(search_text=search_item)
	}
return(search_results)
}

formatting_func <- function(filename){
	dat <- readRDS(str_c("dats/", filename)) %>% filter(is_retweet=="FALSE") %>% 
		select(media_url, search_text, created_at, text) 
	unlist_dat <- foreach(i=1:nrow(dat), .combine=rbind) %do% {
		tmp <- dat %>% select(media_url) %>%
			slice(i) %>% unlist() %>% as_tibble() %>%
			mutate(search_text= dat %>% slice(i) %>% pull(search_text), 
			created_at= dat %>% slice(i) %>% pull(created_at))
	}
	return(unlist_dat)
}

scrape_twitter_func <- function(dat){
	res <- foreach(i=1:nrow(dat)) %do% {
		urls <- dat %>% slice(i) %>% pull(value)
		op_name <- dat %>% 
			group_by(search_text) %>% 
			mutate(group_id=cur_group_id()) %>%	#To check the number of usable images per search_text, you need to check against the group_id later
			ungroup() %>% 
			mutate(serialnum=1:nrow(.)) %>%
			mutate(comb=str_c(group_id, "_", serialnum, ".jpg")) %>% 
			slice(i) %>% 
			pull(comb)
		system(str_c("wget ", urls, " -O ", op_name))
	}
	return(res)
}

reftable_func <- function(dat) {
	refs <- dat %>% 
		group_by(search_text) %>% 
		mutate(group_id=cur_group_id()) %>%	#To check the number of usable images per search_text, you need to check against the group_id later
		summarize(num=n(), group_id=mean(group_id)) %>% 
		select(search_text, group_id, num) %>%
		ungroup()
	return(refs)
}

## Setting up a search term 
search_text_semi <- c("セミ", "蝉", "ゼミ", "ヒグラシ", "ツクツク", "ニイニイ") #Japanese words representing for "cicada".
search_text_kumo <- c("クモ", "蜘蛛", "グモ") #Japanese words representing for "spider".

## Setting the search period
### If you want to search for a period other than the default, change following values
fromD_ip <- "200603210000"
toD_ip <- "201312312359"

## Run "twimg_search_func"
res <- twimg_search_func(search_text_semi=search_text_semi, search_text_kumo=search_text_kumo,
	fromD=fromD_ip, toD=toD_ip)	# If you want to search for a period other than the default, change this line

## Save the result
res %>% saveRDS(file=str_c("dats/", "semi_kumo_img_", fromD_ip, "_", toD_ip, ".rds"))	# PATH (first argument) should be changed on an individual basis


lists <- list.files("dats")

## Scraping using RDS
foreach(i=1:length(lists)) %do% {
	setwd("./") 
	tmp_filename <- lists[i]
	unlist_dat <- formatting_func(filename=tmp_filename)
	dirname <- tmp_filename %>% str_remove(".rds")
	dir.create(str_c("imgs_twitter/", dirname))
	setwd(str_c("imgs_twitter/", dirname))
	scrape_twitter_func(dat=unlist_dat)
	reftable_func(unlist_dat) %>% write.csv(str_c(dirname, ".csv"))
}

