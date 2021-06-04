## scraping_from_Google.R to download url of images using Google image search and Selenium
## Refs
### https://rpubs.com/johndharrison/RSelenium-Basics

## Launch docker
system("docker run -d -p 4445:4444 selenium/standalone-firefox:2.53.1")

## Install library
pacman::p_load(tidyverse, RSelenium, httr, foreach)

## Functions
google_search_path <- function(str) {
	url <- list(
		hostname = "google.com",
		scheme = "https",
		path = "search",
		query = list(
			q = str,
			tbm = "isch"
		)
	)
	class(url) <- "url"
	httr::build_url(url)
}

urlScraping <- function(str){
	search_url <- google_search_path(str)
	remDr$navigate(search_url)
	Sys.sleep(3)
	goEnd <- remDr$findElement("css", "body")
	goEnd$sendKeysToElement(list(key = "end"))
	Sys.sleep(3)
	webElem <- remDr$findElements(using = 
			'xpath', "//a[contains(@class, 'wXeWr')]")
		
	urls <- foreach(i=1:length(webElem), .combine=rbind) %do% {	#Google shows 100 photos (or length(webElem))
		remDr$mouseMoveToLocation(webElement = webElem[[i]]) 	#move location
		remDr$click(1)						#single click 	
		tmp_webElem <- remDr$findElements(using = 		
			'xpath', "//a[contains(@class, 'wXeWr')]")	#get web elements after click
		tmp <- tmp_webElem[[i]]$getElementAttribute("href")	#get tmp url 
		tmp_url <- httr::parse_url(tmp[[1]])$query$imgurl	#parse, decode, and get url
		tibble(id=i, url=tmp_url)
	}
	return(urls)
}

## Start remote driver
remDr <- remoteDriver(
	remoteServerAddr = "localhost",
	port = 4445L,
	browserName = "firefox"
)
remDr$open()

## Setting up a search term 
search_text_semi <- c("セミ", "蝉", "ゼミ", "ヒグラシ", "ツクツク", "ニイニイ") #Japanese words representing for "cicada".
search_text_kumo <- c("クモ", "蜘蛛", "グモ") #Japanese words representing for "spider".
search_text_semi_kumo <- expand.grid(semi=search_text_semi, kumo=search_text_kumo) %>% 
	mutate(text = str_c(semi, kumo, sep = " ")) %>% 
	select(text)


foreach(i=1:nrow(search_text_semi_kumo)) %do% {
	str <- search_text_semi_kumo %>% pull(text) %>% .[i]
	urls <- urlScraping(str)
	urls %>% select(url) %>% 
		write.table(str_c("search_urls/urls_google", i, ".txt"), 
			col.names = FALSE, quote=FALSE, row.names=FALSE)
}

setwd("figs_scraping/")
system("wget -i ../urls.txt")

