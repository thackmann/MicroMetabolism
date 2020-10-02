#' Combine Data Into One Object
#'
#' This function combines information about each organism into one dataframe
#'
#' @param taxonomy A dataframe with columns for taxonomic ranks and rows for articles
#' @param url_list A character vector of url names for articles
#' @param names A list of organism names (one element per organism)
#' @param species_names A list of species names (one element per organism)
#' @param subspecies_names A list of subspecies names (one element per organism)
#' @param strain_ID A list of type strain IDs (one element per organism)
#' @param sentences_fermentation A list of sentences related to fermentation (one element per organism)
#' @param sentences_fermentation_to_acetate A list of sentences related to acetate production during fermentation (one element per organism)
#' @return A dataframe with columns for taxonomy and other information and rows for each organism
#' @importFrom dplyr "%>%" group_by group_size n_groups anti_join
#' @importFrom plyr rbind.fill
#' @importFrom svMisc progress
#' @export
combine_data = function(taxonomy, url_list, names, species_names, subspecies_names, strain_ID, sentences_fermentation, sentences_fermentation_to_acetate){

	combined = cbind(taxonomy, url_list) 
	combined = data.frame(lapply(combined, as.character), stringsAsFactors=FALSE) #Remove factors, which interfere with rbind.full command below

	#Add species_names to dataframe (one row per species)	
	header_combined_data = c(colnames(combined, do.NULL = FALSE), "species", "subspecies", "strain_ID", "sentences_fermentation", "sentences_fermentation_to_acetate")
	combined_data = data.frame(matrix(NA, nrow = 0, ncol = length(header_combined_data)))
	colnames(combined_data) = header_combined_data

	for(i in 1:length(names))
	{
		for(j in 1:length(names[[i]]))
		{
			#Add row
			combined_data = rbind.fill(combined_data,combined[i,])

			#Add species name
			combined_data[nrow(combined_data),"species"] = species_names[[i]][j]

			#Add subspecies name
			combined_data[nrow(combined_data),"subspecies"] = subspecies_names[[i]][j]

			#Add strain ID
			combined_data[nrow(combined_data), "strain_ID"] =  paste(unlist(strain_ID[[i]][j]), collapse=", ")

			#Add sentences_fermentation
			combined_data[nrow(combined_data), "sentences_fermentation"] =  paste(unlist(sentences_fermentation[[i]][j]), collapse=", ")

			#Add sentences_fermentation
			combined_data[nrow(combined_data), "sentences_fermentation_to_acetate"] =  paste(unlist(sentences_fermentation_to_acetate[[i]][j]), collapse=", ")
		}
	}

	#Clean up formatting
		#Remove rows where species name is "NA"
			#If Bergey's Manual does not report a species name for an organism, any other information it reports has limited use 
			combined_data=combined_data[!is.na(combined_data$species),]

		#Remove rows where subspecies name is "NA" (when it should not be)
			#When Bergey's Manual reports all the subspecies of a parent species, it can report the parent species as seperate (duplicate) entry
			#These duplicate entries are manifested as "NA" as the subspecies names

			#Number of subspecies per species
			grp_size = combined_data %>% group_by(Genus, species) %>% group_size()

			#Names of groups (genus and species), in same order as grp_size
			grp_names = combined_data %>% group_by(Genus, species) %>% dplyr::summarize()

			#Number of species
			n_grp = combined_data %>% group_by(Genus, species) %>% n_groups()

			#Initialize values
			exclude = data.frame(matrix(NA, nrow = 0, ncol = ncol(combined_data)))

			#Find all rows that have multiple subspecies per species, and remove those subspecies that are "NA"
			for(i in 1:n_grp)
			{
				if(grp_size[i]>1) 
				{	
					row_NA = combined_data %>% dplyr::filter(Genus==grp_names$Genus[i]) %>% dplyr::filter(species==grp_names$species[i]) %>% dplyr::filter(is.na(subspecies)) #Find rows with "NA"
					exclude = rbind(exclude, row_NA)
				}
			}
				combined_data = anti_join(combined_data, exclude) #Remove rows

		#Clean up genus names
			for(i in 1:nrow(combined_data))
			{
				combined_data$Genus[i] = gsub("\u0080\u009c", "", combined_data$Genus[i], fixed=TRUE) #Remove "\u0080\u009c"
				combined_data$Genus[i] = gsub("\u0080\u009d\u0080 ", "", combined_data$Genus[i], fixed=TRUE) #Remove "\u0080\u009d\u0080 "
			}

	return(combined_data)
}
