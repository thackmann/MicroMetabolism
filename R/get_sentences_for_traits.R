#' Get Sentences Related to Fermentation
#'
#' This function finds sentences in the organism description including keywords related to fermentation
#'
#' @param url_list A character vector of url names for articles
#' @param names A list of organism names (one element per organism)
#' @param text_genus A list containing the genus text (one element per organism)
#' @param species_description A list containing the species descriptions (one element per organism)
#' @return A list of sentences related to fermentation (one element per organism)
#' @importFrom svMisc progress
#' @export
get_sentences_fermentation  = function(url_list, names, text_genus, species_description){
	sentences_fermentation = rep(list(list()), length(url_list))
		for(i in 1:length(url_list))
		{
			#Sentences that include keywords related to fermentation
				for(j in 1:length(names[[i]]))
				{	
				#Keywords
					keywords = paste("[fF]erment", sep="|")
				
				#Anti-keywords (sentences are removed if they match these)
					#anti_keywords = paste("[nN]on[-]*ferment", "[nN]o[t]*[ ]ferment", "[nN]ever[ ]ferment", collapse=",", sep="|")

				#Sentences are from genus description
					sentences = unlist(text_genus[[i]])
				
				#Sentences are also from species description
					sentences = c(sentences, unlist(species_description[[i]][j]))
				
				#Split text into sentences (by periods)
					sentences = unlist(strsplit(sentences, "\\."))

				#Split text further by line breaks (\r\n)
					sentences = unlist(strsplit(sentences, "\r\n"))
				
				#Extract out sentences (or lines) that have keywords
					sentences = sentences[grep(pattern = keywords, x=sentences)]

				#Remove sentences that match anti-keywords
					#sentences = sentences[!grepl(pattern = anti_keywords, x=sentences)]
			
				#Remove any duplicate sentences
					sentences = unique(sentences)
			
				#Paste sentences back together (by periods)
					sentences = paste(sentences, collapse=".") 		

					sentences_fermentation[[i]][j] = sentences
				}
			
			#Show progress of loop
				progress(value=i, max.value=length(url_list))
		}
		
  return(sentences_fermentation)
}

#' Get Sentences Related to Acetate Production
#'
#' This function finds sentences in the organism description including keywords related to acetate production during fermentation
#'
#' @param url_list A character vector of url names for articles
#' @param names A list of organism names (one element per organism)
#' @param text_genus A list containing the genus text (one element per organism)
#' @param species_description A list containing the species descriptions (one element per organism)
#' @return A list of sentences related to acetate production during fermentation (one element per organism)
#' @importFrom svMisc progress
#' @export
get_sentences_acetate = function(url_list, names, text_genus, species_description){
sentences_fermentation_to_acetate = rep(list(list()), length(url_list))
	for(i in 1:length(url_list))
	{
		#Sentences that include keywords related to fermentation and acetate
			for(j in 1:length(names[[i]]))
			{	
			#Keywords
				keywords_1 = paste("[fF]erment", sep="|")
				keywords_2 = paste("[aA]cetate", "[aA]cetic", sep="|")
				keywords = paste(keywords_1 ,keywords_2, sep="|")
			
			#Anti-keywords (sentences are removed if they match these)
				#anti_keywords_1 = paste("[nN]on[-]*ferment", "[nN]o[t]*[ ]ferment", "[nN]ever[ ]ferment", collapse=",", sep="|")
				#anti_keywords_2 = paste("[A]{1}acetate", "[A]{1}acetic", collapse=",", sep="|")
				#anti_keywords = paste(anti_keywords_1, anti_keywords_2, sep="|")

			#Sentences are from genus description
				sentences = unlist(text_genus[[i]])
			
			#Sentences are also from species description
				sentences = c(sentences, unlist(species_description[[i]][j]))
			
			#Split text into sentences (by periods)
				sentences = unlist(strsplit(sentences, "\\."))

			#Split text further by line breaks (\r\n)
				sentences = unlist(strsplit(sentences, "\r\n"))
			
			#Extract out sentences (or lines) that have keywords
				sentences = sentences[grep(pattern = keywords, x=sentences)]

			#Remove sentences that match anti-keywords
				#sentences = sentences[!grepl(pattern = anti_keywords, x=sentences)]
				
			#Remove any duplicate sentences
				sentences = unique(sentences)
		
			#Paste sentences back together (by periods)
				sentences = paste(sentences, collapse=".") 		

			#Remove sentences (replace with "") if keywords_1 and keywords_2 are not present
				if(!grepl(pattern = keywords_1, x=sentences)|!grepl(pattern = keywords_2, x=sentences))
				{
					sentences = ""
				}

				sentences_fermentation_to_acetate[[i]][j] = sentences
			}
		
		#Show progress of loop
			progress(value=i, max.value=length(url_list))
	}

  return(sentences_fermentation_to_acetate)
}