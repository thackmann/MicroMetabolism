#' Read HTML pages
#'
#' This function retrieves downloaded articles and reads them into R
#'
#' @param directory_fp A character string giving the file path for the download directory
#' @param url_list A character vector of url names for articles
#' @param destfile_names_Bergey A list of file names for articles (one element per page)
#' @return A xml_document and xml_node object for the articles
#' @importFrom xml2 read_html
#' @importFrom svMisc progress
#' @export
read_html_pages = function(directory_fp, url_list, destfile_names_Bergey){
	setwd(directory_fp)

	html_list=vector(mode = "list", length = length(url_list))

	for(i in 1:length(url_list))
	{
		html_filepath=file.path=(destfile_names_Bergey[[i]])
		html_list[[i]] = read_html(html_filepath)

		#Show progress of loop
			progress(value=i, max.value=length(url_list))
	}

  return(html_list)
}

#' Get Full Text
#'
#' This function gets the full text from the downloaded articles
#'
#' @param directory_fp A character string giving the file path for the download directory
#' @param url_list A character vector of url names for articles
#' @param destfile_names_Bergey A list of file names for articles
#' @param html_list A xml_document and xml_node object for the articles
#' @return A list containing the full text (one element per article)
#' @importFrom rvest html_nodes html_text
#' @importFrom svMisc progress
#' @export
get_full_text = function(directory_fp, url_list, destfile_names_Bergey, html_list=html_list){
  setwd(directory_fp)

  text_full = rep(list(list()), length(url_list))

  for(i in 1:length(url_list))
  {
    html_filepath = file.path=(destfile_names_Bergey[[i]])

    text_full_xml = html_nodes(html_list[[i]], ".article-row-left")
    text_full[[i]] = html_text(text_full_xml)
    if(identical(text_full[[i]], character(0))){text_full[[i]]=""}

    #Show progress of loop
    progress(value=i, max.value=length(url_list))
  }

  return(text_full)
}


#' Get Tables
#'
#' This function gets text of tables from the downloaded articles
#'
#' @param directory_fp A character string giving the file path for the download directory
#' @param url_list A character vector of url names for articles
#' @param destfile_names_Bergey A list of file names for articles
#' @param html_list A xml_document and xml_node object for the articles
#' @return A list containing the text of tables (one element per article)
#' @importFrom rvest html_nodes html_text
#' @importFrom svMisc progress
#' @export
get_tables = function(directory_fp, url_list, destfile_names_Bergey, html_list){
	setwd(directory_fp)

	text_tables = rep(list(list()), length(url_list))

	for(i in 1:length(url_list))
	{
		html_filepath = file.path=(destfile_names_Bergey[[i]])

		text_tables_xml = html_nodes(html_list[[i]], ".article-table-content")
		text_tables[[i]] = html_text(text_tables_xml)

		#Show progress of loop
			progress(value=i, max.value=length(url_list))
	}

  return(text_tables)
}

#' Get Abstracts
#'
#' This function gets text of abstracts from the downloaded articles
#'
#' @param directory_fp A character string giving the file path for the download directory
#' @param url_list A character vector of url names for articles
#' @param destfile_names_Bergey A list of file names for articles
#' @param html_list A xml_document and xml_node object for the articles
#' @return  A list containing the text of abstracts (one element per article)
#' @importFrom rvest html_nodes html_text
#' @importFrom svMisc progress
#' @export
get_abstract = function(directory_fp, url_list, destfile_names_Bergey, html_list){
	setwd(directory_fp)

	text_abstract = rep(list(list()), length(url_list))

	for(i in 1:length(url_list))
	{
		html_filepath = file.path=(destfile_names_Bergey[[i]])

		text_abstract_xml = html_nodes(html_list[[i]], xpath='//*[@id="section-1-en"]')

		text_abstract[[i]] = html_text(text_abstract_xml)

		#Show progress of loop
			progress(value=i, max.value=length(url_list))
	}

  return(text_abstract)
}

#' Get Taxonomy
#'
#' This function gets taxonomic ranks (phylum to genus) from the downloaded articles
#'
#' @param directory_fp A character string giving the file path for the download directory
#' @param url_list A character vector of url names for articles
#' @param destfile_names_Bergey A list of file names for articles
#' @param html_list A xml_document and xml_node object for the articles
#' @return A dataframe with columns for taxonomic ranks and rows for articles
#' @importFrom rvest html_nodes html_text
#' @importFrom svMisc progress
#' @export
get_taxonomy = function(directory_fp, url_list, destfile_names_Bergey, html_list){
  setwd(directory_fp)

  header_taxonomy = c("Phylum", "Class", "Order", "Family", "Genus")
  selector_taxonomy = c(".partTile", ".supPartTile", ".subSubpartTile", ".subSubSubpartTile", ".citation__title")
  taxonomy = data.frame(array(0,dim=c(length(url_list),length(header_taxonomy))))
  colnames(taxonomy) = c(header_taxonomy)

  for(i in 1:length(url_list))
  {
    html_filepath = file.path=(destfile_names_Bergey[[i]])
    for(j in 1:length(header_taxonomy))
    {
      name_xml = html_nodes(html_list[[i]], selector_taxonomy[j])
      name_text = html_text(name_xml)
      if(identical(name_text, character(0))){name_text="NA"}
      name_text = trimws(name_text)
      taxonomy[i,j] = name_text
    }

    #Convert to ASCII formatting (removes non-ASCII characters that interfere in downstream extraction of organism names)
    #Converting from UTF-8 to latin1 to ASCII (in that order) results in fewest characters being removed
    taxonomy[i,j] = iconv(taxonomy[i,j], from="UTF-8", to="latin1", "")
    taxonomy[i,j] = iconv(taxonomy[i,j], from="latin1", to="ASCII", "")

    #Show progress of loop
    progress(value=i, max.value=length(url_list))
  }

  return(taxonomy)
}


#' Get Main Text
#'
#' This function gets all text between abstract and references (or acknowledgements, end notes, further reading)
#'
#' @param url_list A character vector of url names for articles
#' @param text_full A list the full text (one element per article)
#' @return A list containing the main text (one element per article)
#' @importFrom svMisc progress
#' @export
get_main_text = function(url_list, text_full){
	text_main = rep(list(list()), length(url_list))
	for(i in 1:length(url_list))
	{
		text_main[[i]] = text_full[[i]]

		#Remove text before abstract
		text_main[[i]] = sub(".*\r\n[ ]*Abstract\r\n[ ]*","Abstract\r\n[ ].*", text_main[[i]])

		#Remove text after references, acknowledgements, end notes, and further reading
		text_main[[i]] = sub("\r\n[ ]+End [nN]ote[s]*\r\n.*","", text_main[[i]])
		text_main[[i]] = sub("\r\n[ ]+Acknowledgment[s]*\r\n.*","", text_main[[i]])
		text_main[[i]] = sub("\r\n[ ]+Reference[s]*\r\n.*","", text_main[[i]])
		text_main[[i]] = sub("\r\n[ ]+Further [rR]eading\r\n.*","", text_main[[i]])

		#Get text for genus Bacillus
		#Format of article different from others
		#See https://onlinelibrary.wiley.com/doi/full/10.1002/9781118960608.gbm00530
		if(url_list[[i]] == "https://onlinelibrary.wiley.com/doi/full/10.1002/9781118960608.gbm00530")
		{
			text_main[[i]] = text_full[[i]]
			text_main[[i]] = sub(".*\r\n[ ]*Abstract\r\n[ ]*","Abstract\r\n[ ].*", text_main[[i]])
			text_main[[i]] = sub("\r\n[ ]+Reference[s]*\r\n.*","", text_main[[i]])
		}

		#Show progress of loop
			progress(value=i, max.value=length(url_list))
	}

  return(text_main)
}

#' Get Genus Text
#'
#' This function gets text pertaining to the genus (all text between abstract and before species descriptions)
#'
#' @param url_list A character vector of url names for articles
#' @param text_full A list the full text (one element per article)
#' @param text_tables A list containing the text of tables (one element per article)
#' @return A list containing the genus text (one element per article)
#' @importFrom svMisc progress
#' @export
get_genus_text = function(url_list, text_full, text_tables){
	text_genus = rep(list(list()), length(url_list))

	for(i in 1:length(url_list))
	{
		text_genus[[i]] = text_full[[i]]



		#Remove text before abstract
			text_genus[[i]] = sub(".*\r\n[ ]*Abstract\r\n[ ]*","Abstract\r\n[ ].*", text_genus[[i]])

		#Remove tables
			#Run loop only if there are tables
			if(length(text_tables[[i]])>0)
			{
				for(k in 1:length(text_tables[[i]]))
				{
					#Replace text from tables with nothing
					text_genus[[i]] = gsub(text_tables[[i]][k], "", text_genus[[i]], fixed = TRUE)
				}
			}

		#Remove text after "List of [the][tenative] species of the genus . . . "
			text_genus[[i]] = sub("List of (the )*(tentative )*species of the genus[ ]*.*", "", text_genus[[i]])

			#Some articles are worded differently, and following applies instead
				text_genus[[i]] = sub("List of (the )*(tentative )*species in the genus[ ]*.*", "", text_genus[[i]])
				text_genus[[i]] = sub("Characteristics of the species of the genus[ ]*.*", "", text_genus[[i]])
				text_genus[[i]] = gsub("Differentiation of the species of the genus[ ]*.*", "", text_genus[[i]])

		#Remove text after references, acknowledgements, end notes, and further reading
			text_genus[[i]] = sub("\r\n[ ]+End [nN]ote[s]*\r\n.*","", text_genus[[i]])
			text_genus[[i]] = sub("\r\n[ ]+Acknowledgment[s]*\r\n.*","", text_genus[[i]])
			text_genus[[i]] = sub("\r\n[ ]+Reference[s]*\r\n.*","", text_genus[[i]])
			text_genus[[i]] = sub("\r\n[ ]+Further [rR]eading\r\n.*","", text_genus[[i]])

		#Show progress of loop
			progress(value=i, max.value=length(url_list))

	}

  return(text_genus)
}

#' Get Species Text
#'
#' This function gets text under "List of the species of the genus" or similar section
#'
#' @param url_list A character vector of url names for articles
#' @param text_full A list the full text (one element per article)
#' @param text_tables A list containing the text of tables (one element per article)
#' @return A list containing the species text (one element per article)
#' @importFrom svMisc progress
#' @export
get_species_text = function(url_list, text_full, text_tables){
	text_species = rep(list(list()), length(url_list))
	for(i in 1:length(url_list))
	{
		text_species[[i]] = text_full[[i]]

		#Remove text before abstract
			text_species[[i]] = sub(".*\r\n[ ]*Abstract\r\n[ ]*","Abstract\r\n[ ].*", text_species[[i]])

		#Remove tables
			#Tables include species names, which makes it hard to extract the names from the full text
			#Run loop only if there are tables
			if(length(text_tables[[i]])>0)
			{
				for(k in 1:length(text_tables[[i]]))
				{
					#Replace text from tables with nothing
					text_species[[i]] = gsub(text_tables[[i]][k], "", text_species[[i]], fixed = TRUE)
				}
			}


		#Remove text before "List of [the][tenative] species of the genus . . . "
			text_species[[i]] = sub(".*List of (the )*(tentative )*species of the genus[ ]*", "List of species of the genus ", text_species[[i]])

			#Some articles are worded differently, and following applies instead
				text_species[[i]] = sub(".*List of species in the genus[ ]*", "List of species of the genus ", text_species[[i]])
				text_species[[i]] = sub(".*Characteristics of the species of the genus[ ]*", "List of species of the genus ", text_species[[i]])
				if(url_list[[i]] != "https://onlinelibrary.wiley.com/doi/full/10.1002/9781118960608.gbm00567") #Prevents erroneous removal of text from article for Macrococcus
				{
					text_species[[i]] = sub(".*Differentiation of the species of the genus[ ]*", "List of species of the genus ", text_species[[i]])
				}

		#Remove text after species incertae sedis
			text_species[[i]] = sub("\r\n[ ]+Species incertae sedis.*","", text_species[[i]])

		#Remove text after references, acknowledgements, end notes, and further reading
			text_species[[i]] = sub("\r\n[ ]+End [nN]ote[s]*\r\n.*","", text_species[[i]])
			text_species[[i]] = sub("\r\n[ ]+Acknowledgment[s]*\r\n.*","", text_species[[i]])
			text_species[[i]] = sub("\r\n[ ]+Reference[s]*\r\n.*","", text_species[[i]])
			text_species[[i]] = sub("\r\n[ ]+Further [rR]eading\r\n.*","", text_species[[i]])

		#Convert to ASCII formatting (removes non-ASCII characters that interfere in downstream extraction of organism names)
		#Converting from UTF-8 to latin1 to ASCII (in that order) results in fewest characters being removed
			text_species[[i]]=iconv(text_species[[i]], from="UTF-8", to="latin1", "")
			text_species[[i]] = iconv(text_species[[i]], from="latin1", to="ASCII", "")

		#Show progress of loop
			progress(value=i, max.value=length(url_list))
	}
  return(text_species)
}
