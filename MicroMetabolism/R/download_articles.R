#' Get Url Names For Articles
#'
#' This function gets url names for articles for articles from Bergey's Manual table of contents
#'
#' @return A character vector of url names for articles
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_text html_attr
#' @importFrom svMisc progress
#' @importFrom stringr word
#' @export

get_url_names = function(){

	url_list=vector()

	for(i in 1:length(letters))
		{
			#Get number of pages in table of contents
			url_name = paste("https://onlinelibrary.wiley.com/browse/book/10.1002/9781118960608/title?pageSize=20&startPage=&alphabetRange=", letters[i],sep="")
			n_article_page = 20 #Each page has 20 articles (results) per page
			html_list = read_html(url_name)
			count_xml = html_nodes(html_list, ".result__count")
			count_text = html_text(count_xml)
			count_text = word(count_text)
			count_text = gsub("[^0-9]", "", count_text) 
			count_num=as.numeric(count_text)
			n_pages=ceiling(count_num/n_article_page)

			#Extract out url names for articles for each article
			for(j in 1:n_pages)
			{
				url_name = paste("https://onlinelibrary.wiley.com/browse/book/10.1002/9781118960608/title?startPage=", j-1,"&alphabetRange=", letters[i], "&pageSize=20", sep="")
				html_list = read_html(url_name)
				url_xml=html_nodes(html_list, ".rlist--inline.separator li:nth-child(2) a")
				url_text=html_attr(url_xml, "href")
				url_text=paste("https://onlinelibrary.wiley.com", url_text, sep="")	
				url_list=append(url_list, url_text)	
			}
			#Show progress of loop
			progress(value=i, max.value=length(letters))
		}
		
	return(url_list)
}

#' Filter Url Names For Articles
#'
#' This function keeps url name only if article is for a genus (not phylum, class, order, or family)
#'
#' @param url_list A character vector of url names for articles
#' @return A character vector of url names for articles
#' @export
filter_url_names = function(url_list){
	#url names for articles to keep contain "gbm"
	match_vec=which(grepl("gbm",url_list))
	url_list=url_list[match_vec]

	return(url_list)
}

#' Set HTML Names
#'
#' This function sets file names for articles when downloaded
#'
#' @param url_list A character vector of url names for articles
#' @return A list of file names for articles
#' @export
set_html_names = function(url_list){
	destfile_names_Bergey = rep(list(list()), length(url_list))
	for(i in 1:length(url_list))
	{
		matches_url = gregexpr("gbm.*", url_list[i])
		destfile_names_Bergey[[i]] = unlist(regmatches(url_list[i], matches_url))
		destfile_names_Bergey[[i]] = paste(destfile_names_Bergey[[i]], ".html", sep="")
	}

		return(destfile_names_Bergey)
}

#' Download HTML Pages
#'
#' This function downloads articles from Bergey's Manual
#'
#' @param directory_fp A character string giving the file path for the download directory
#' @param url_list A character vector of url names for articles
#' @param destfile_names_Bergey A list of file names for articles
#' @importFrom svMisc progress
#' @export
download_html_pages = function(directory_fp, url_list, destfile_names_Bergey){
setwd(directory_fp)

for(i in 1:length(url_list))
		{
			download.file(url = url_list[i], destfile = destfile_names_Bergey[[i]], quiet = TRUE)

			#Show progress of loop
			progress(value=i, max.value=length(url_list))
		}
}
