#' Get Organism Names
#'
#' This function gets the genus, species, and subspecies names of each organism
#'
#' @param url_list A character vector of url names for articles
#' @param taxonomy A dataframe with columns for taxonomic ranks and rows for articles
#' @param text_species A list containing the species text (one element per article)
#' @return A list of organism names (one element per organism)
#' @importFrom svMisc progress
#' @export
get_organism_names = function(url_list, taxonomy, text_species){
	names = rep(list(list()), length(url_list))
	for(i in 1:length(url_list))
	{
		#Extract lines containing binomial name (include genus, species, and subspecies)
			#Set pattern for finding names
				#Species names
					#The pattern is 1) "\r\n" with one or more spaces, 2) the genus name, 3) species name in one or more lowercase letters, and 4) "\r\n"
					#Certain other characters (".*) are allowed in certain positions
						#Set genus name
						genus_name = taxonomy[i,5]
						genus_name = paste("(Candidatus )*", genus_name, sep="") #Give option for "Candidatus " to appear in genus name
						genus_name = sub(" gen. nov[.]*", "", genus_name) #Remove " gen. nov[.]"
						genus_name = sub("[Ii]ncertae [sS]edis[ IVX.]* ", "", genus_name) #Remove "Incertae Sedis [Roman numeral] "
						genus_name = sub(" corrig", "", genus_name) #Remove " corrig"
						genus_name = sub("\u0080\u009c", "", genus_name, fixed = TRUE) #Remove left quotation mark
						genus_name = sub("\u0080\u009d\u0080", "", genus_name, fixed = TRUE) #Remove right quotation mark
						genus_name = sub("Candidatus", "Ca[.]*(ndidatus)*[ ]*", genus_name) #Give option to abbreviate "Candidatus" as "Ca." and for a space to be present between Candidatus and genus name
			
					pattern_species_names = paste("\r\n[ ]+[\"]*", genus_name, " [abcdefghijklmnopqrstuvwxyz.\"*]+\r\n", sep="")

				#Subspecies names v. 1
					#The pattern is 1) "\r\n" with one or more spaces, 2) the genus name, 3) species name in one or more lowercase letters, 4) "\r\n            \r\n            ", 5) "subsp[.] ", and 6) subspecies name in one or more lowercase letters
					#Certain other characters (".*) are allowed in certain positions
					pattern_subspecies_names_v1 = paste("\r\n[ ]+[\"]*", genus_name, " [abcdefghijklmnopqrstuvwxyz.\"*]+\r\n[ ]+\r\n[ ]+subsp. [abcdefghijklmnopqrstuvwxyz.\"*]*", sep="")

				#Subspecies names v. 2
					#The pattern is 1) "\r\n" with one or more spaces, 2) the genus name, 3) species name in one or more lowercase letters, 4) "\r\n            \r\n            ", 5) "subsp ", and 6) subspecies name in one or more lowercase letters
					#Certain other characters (".*) are allowed in certain positions
					pattern_subspecies_names_v2 = paste("\r\n[ ]+[\"]*", genus_name, " [abcdefghijklmnopqrstuvwxyz.\"*]+ subsp\r\n[ ]+\r\n[ ]+[abcdefghijklmnopqrstuvwxyz.\"*]*", sep="")
				
				#Subspecies names v. 3
					#The pattern is 1) "\r\n" with one or more spaces, 2) the genus name, 3) species name in one or more lowercase letters, 4) "\r\n            \r\n            ", 5) "subsp[.] ", and 6) subspecies name in one or more lowercase letters
					#Certain other characters (".*) are allowed in certain positions
					pattern_subspecies_names_v3 = paste("\r\n[ ]+[\"]*", genus_name, " [abcdefghijklmnopqrstuvwxyz.\"*]+ subsp. [abcdefghijklmnopqrstuvwxyz.\"*]+\r\n[ ]+\r\n[ ]+", sep="")

				#Subspecies names v. 4
					#The pattern is 1) "\r\n" with one or more spaces, 2) the genus name, 3) species name in one or more lowercase letters, 4) "\r\n            \r\n            ", 5) "subsp[.] ", and 6) subspecies name in one or more lowercase letters
					#Certain other characters (".*) are allowed in certain positions
					pattern_subspecies_names_v4 = paste("\r\n[ ]+[\"]*", genus_name, " [abcdefghijklmnopqrstuvwxyz.\"*]+ subsp. [abcdefghijklmnopqrstuvwxyz.\"*]+ subsp. nov.[ ]*[abcdefghijklmnopqrstuvwxyz.*]*\r\n[ ]+\r\n[ ]+", sep="")

				#Pathovar names
					#The pattern is 1) "\r\n" with one or more spaces, 2) the genus name, 3) species name in one or more lowercase letters, 4) "\r\n            \r\n            ", 5) "pathovar ", and 6) subspecies name in one or more lowercase letters
					#Certain other characters (".*) are allowed in certain positions
					pattern_pathovar_names = paste("\r\n[ ]+[\"]*", genus_name, " [abcdefghijklmnopqrstuvwxyz.\"*]+\r\n[ ]+\r\n[ ]+pathovar [abcdefghijklmnopqrstuvwxyz.\"*]*", sep="")

				#Biovar names
					#The pattern is 1) "\r\n" with one or more spaces, 2) the genus name, 3) species name in one or more lowercase letters, 4) "\r\n            \r\n            ", 5) "biovar ", and 6) subspecies name in one or more lowercase letters
					#Certain other characters (".*) are allowed in certain positions
					pattern_biovar_names = paste("\r\n[ ]+[\"]*", genus_name, " [abcdefghijklmnopqrstuvwxyz.\"*]+\r\n[ ]+\r\n[ ]+biovar [abcdefghijklmnopqrstuvwxyz.\"*]*", sep="")

				#Genomospecies number
					#The pattern is 1) "\r\n" with one or more spaces, 2) the genus name, 3) "genomospecies", 4) a number
					#Certain other characters (".*) are allowed in certain positions
					pattern_genomospecies_names = paste("\r\n[ ]+[\"]*", genus_name, " genomospecies ", "[1234567890]*", sep="")

				#Pattern for names
					#Combines pattern for subspecies and species
					pattern_names =  paste("(",pattern_subspecies_names_v1,")|(",pattern_subspecies_names_v2,")|(",pattern_subspecies_names_v3,")|(",pattern_subspecies_names_v4,")|(", pattern_pathovar_names, ")|(", pattern_biovar_names, ")|(", pattern_species_names, ")|(", pattern_genomospecies_names, ")", sep="")

			#Find matches and extract names
				matches_names = gregexpr(pattern_names, text_species[[i]])
				names[[i]] = unlist(regmatches(text_species[[i]], matches_names))

				#If there are no matches, search for names using other patterns
					if(matches_names[[1]][1]==-1)
					{
						#Pattern is species name flanked by "\u0080\u009c" and "\u0080\u009d"
							#https://onlinelibrary.wiley.com/doi/full/10.1002/9781118960608.gbm00370
							#Patterns
							pattern_species_names = paste("\r\n[ ]+(\u0080\u009c)*", genus_name, " [abcdefghijklmnopqrstuvwxyz.*]+(\u0080\u009d)*\r\n", sep="")
							pattern_subspecies_names = paste("\r\n[ ]+(\u0080\u009c)*", genus_name, " [abcdefghijklmnopqrstuvwxyz.*]+(\u0080\u009d)*\r\n[ ]+\r\n[ ]+subsp. [abcdefghijklmnopqrstuvwxyz.*]*", sep="")
							pattern_pathovar_names = paste("\r\n[ ]+(\u0080\u009c)*", genus_name, " [abcdefghijklmnopqrstuvwxyz.*]+(\u0080\u009d)*\r\n[ ]+\r\n[ ]+pathovar [abcdefghijklmnopqrstuvwxyz.*]*", sep="")
							pattern_names =  paste("(",pattern_subspecies_names,")|(", pattern_pathovar_names, ")|(", pattern_species_names, ")", sep="")

							#Try matching again
							matches_names = gregexpr(pattern_names, text_species[[i]])
							names[[i]] = unlist(regmatches(text_species[[i]], matches_names))
						
						#Continue to search if no matches found
						if(matches_names[[1]][1]==-1)
						{
						#A line begins with genus name and preceded by a line that ends in "Candidatus"
							#https://onlinelibrary.wiley.com/doi/full/10.1002/9781118960608.gbm01141
							#Set genus name
							genus_name = taxonomy[i,5]
							genus_name = sub("Candidatus ", "", genus_name) #Remove "Candidatus"
							
							#Patterns
							pattern_species_names = paste("Candidatus\r\n[ ]+\r\n[ ]+", genus_name, " [abcdefghijklmnopqrstuvwxyz.*]+", sep="")
							pattern_subspecies_names = paste("Candidatus\r\n[ ]+ \r\n[ ]+", genus_name, " [abcdefghijklmnopqrstuvwxyz.*]+\r\n[ ]+\r\n[ ]+subsp. [abcdefghijklmnopqrstuvwxyz.*]*", sep="")
							pattern_pathovar_names = paste("Candidatus\r\n[ ]+ \r\n[ ]+", genus_name, " [abcdefghijklmnopqrstuvwxyz.*]+\r\n[ ]+\r\n[ ]+pathovar [abcdefghijklmnopqrstuvwxyz.*]*", sep="")
							pattern_names =  paste("(",pattern_subspecies_names,")|(", pattern_pathovar_names, ")|(", pattern_species_names, ")", sep="")

							#Try matching again
							matches_names = gregexpr(pattern_names, text_species[[i]])
							names[[i]] = unlist(regmatches(text_species[[i]], matches_names))
							
						#Continue to search if no matches found
						if(matches_names[[1]][1]==-1)
						{
						#Any line that begins with genus name
							#https://onlinelibrary.wiley.com/doi/full/10.1002/9781118960608.gbm00042
							#https://onlinelibrary.wiley.com/doi/full/10.1002/9781118960608.gbm00646
							#https://onlinelibrary.wiley.com/doi/full/10.1002/9781118960608.gbm00001

							#Patterns
							pattern_species_names = paste("\r\n[ ]+", genus_name, " [abcdefghijklmnopqrstuvwxyz.*]+", sep="")
							pattern_subspecies_names = paste("\r\n[ ]+", genus_name, " [abcdefghijklmnopqrstuvwxyz.*]+\r\n[ ]+\r\n[ ]+subsp. [abcdefghijklmnopqrstuvwxyz.*]*", sep="")
							pattern_pathovar_names = paste("\r\n[ ]+", genus_name, " [abcdefghijklmnopqrstuvwxyz.*]+\r\n[ ]+\r\n[ ]+pathovar [abcdefghijklmnopqrstuvwxyz.*]*", sep="")
							pattern_names =  paste("(",pattern_subspecies_names,")|(", pattern_pathovar_names, ")|(", pattern_species_names, ")", sep="")

							#Try matching again
							matches_names = gregexpr(pattern_names, text_species[[i]])
							names[[i]] = unlist(regmatches(text_species[[i]], matches_names))
						
						#Continue to search if no matches found
						if(matches_names[[1]][1]==-1)
						{
						#Any genus name and word (species name) that follows "Type species: "
						#Search full text
							#https://onlinelibrary.wiley.com/doi/full/10.1002/9781118960608.gbm00623
							
							#Patterns
							pattern_species_names = paste("\r\n[ ]+Type species: ", genus_name, " [abcdefghijklmnopqrstuvwxyz.*]+", sep="")
							pattern_subspecies_names = paste("\r\n[ ]+Type species: ", genus_name, " [abcdefghijklmnopqrstuvwxyz.*]+\r\n[ ]+\r\n[ ]+subsp. [abcdefghijklmnopqrstuvwxyz.*]*", sep="")
							pattern_pathovar_names = paste("\r\n[ ]+Type species: ", genus_name, " [abcdefghijklmnopqrstuvwxyz.*]+\r\n[ ]+\r\n[ ]+pathovar [abcdefghijklmnopqrstuvwxyz.*]*", sep="")
							pattern_names =  paste("(",pattern_subspecies_names,")|(", pattern_pathovar_names, ")|(", pattern_species_names, ")", sep="")

							#Try matching again
							matches_names = gregexpr(pattern_names, text_main[[i]])
							names[[i]] = unlist(regmatches(text_main[[i]], matches_names))
							names[[i]] = sub("Type species: ", "", names[[i]]) #Remove "Type species: "

						#Species is two words before "consortium "
							#https://onlinelibrary.wiley.com/doi/full/10.1002/9781118960608.gbm00379

						#Species description is after "Further Reading"
							#https://onlinelibrary.wiley.com/doi/full/10.1002/9781118960608.gbm00940

						#List for groups following "Form- . . . "
						}
						}
						}	
					}
				
				#Perform matching for genus Bacillus
				#Format of article different from others
				#https://onlinelibrary.wiley.com/doi/full/10.1002/9781118960608.gbm00530
				if(genus_name == "Bacillus")
				{
					#Match any line that begins with genus name
					#Patterns
					pattern_species_names = paste("\r\n[ ]+", genus_name, " [abcdefghijklmnopqrstuvwxyz.*]+", sep="")
					pattern_subspecies_names = paste("\r\n[ ]+", genus_name, " [abcdefghijklmnopqrstuvwxyz.*]+[ ]+subsp. [abcdefghijklmnopqrstuvwxyz.*]*", sep="")
					pattern_pathovar_names = paste("\r\n[ ]+", genus_name, " [abcdefghijklmnopqrstuvwxyz.*]+[ ]+pathovar [abcdefghijklmnopqrstuvwxyz.*]*", sep="")
					pattern_names =  paste("(",pattern_subspecies_names,")|(", pattern_pathovar_names, ")|(", pattern_species_names, ")", sep="")

					matches_names = gregexpr(pattern_names, text_species[[i]])
					names[[i]] = unlist(regmatches(text_species[[i]], matches_names))
				}

				#If there are still no matches, assign "NA" to name
					if(matches_names[[1]][1]==-1)
					{
						names[[i]]=NA
					}

		#Show progress of loop
			progress(value=i, max.value=length(url_list))
	}

  return(names)
}
	
#' Get Species Names
#'
#' This function gets species names for each organism
#'
#' @param url_list A character vector of url names for articles
#' @param names A list of organism names (one element per organism)
#' @param taxonomy A dataframe with columns for taxonomic ranks and rows for articles
#' @return A list of species names (one element per organism)
#' @importFrom svMisc progress
#' @export
get_species_names = function(url_list, taxonomy, names){
	species_names = rep(list(list()), length(url_list))
	
	for(i in 1:length(url_list))
	{	
		for(j in 1:length(names[[i]]))
		{	
			#Species name is from organism name
			#Remove genus name
			pattern = paste(".*", taxonomy[i,5], " ", sep="")
			species_names[[i]][j] = sub(pattern, "", names[[i]][j])

			#Clean up formatting of species name
			#Remove "\r\n" and spaces
			species_names[[i]][j] = gsub("\r\n[ ]*", "", species_names[[i]][j])

			#Remove "\"
			species_names[[i]][j] = gsub("\"", "", species_names[[i]][j])

			#Remove "*"
			species_names[[i]][j] = gsub("\\*", "", species_names[[i]][j])

			#Add back a space between species name and subspecies
			species_names[[i]][j] = gsub("subsp.", " subsp.", species_names[[i]][j])

			#Add back a space between species name and pathovar
			species_names[[i]][j] = gsub("pathovar", " pathovar", species_names[[i]][j])

			#Add back a space between species name and biovar
			species_names[[i]][j] = gsub("biovar", " biovar", species_names[[i]][j])
						
			#Remove "\u0080\u009c"
			species_names[[i]][j] = gsub("\u0080\u009c", "", species_names[[i]][j])

			#Remove "\u0080\u009d"
			species_names[[i]][j] = gsub("\u0080\u009d", "", species_names[[i]][j])

			#Remove "<U+0080>"
			species_names[[i]][j] = gsub("<U+0080>", "", species_names[[i]][j])

			#Remove "<U+009C>"
			species_names[[i]][j] = gsub("<U+009C>", "", species_names[[i]][j])

			#Remove all text after species name
			species_names[[i]][j] = strsplit(unlist(species_names[[i]][j]), " ")[[1]][1]
		}
		
		#Show progress of loop
			progress(value=i, max.value=length(url_list))
	}
	
  return(species_names)
}

#' Get Subspecies Names
#'
#' This function gets subspecies names for each organism
#'
#' @param url_list A character vector of url names for articles
#' @param names A list of organism names (one element per organism)
#' @return A list of subspecies names (one element per organism)
#' @importFrom svMisc progress
#' @export
get_subspecies_names  = function(url_list, names){
subspecies_names = rep(list(list()), length(url_list))
	for(i in 1:length(url_list))
	{	
			for(j in 1:length(names[[i]]))
			{		
				#Subspecies name is from organism name
				#Also includes biovar and pathovar
				subspecies_names[[i]][j] = names[[i]][j]
			
				#Match subspecies names
					#Set pattern
					#Subspecies names v. 1
						#The pattern is 1) "subsp[.] ", and 2) subspecies name in one or more lowercase letters
						#Certain other characters (.-*) are allowed in certain positions
						pattern_subspecies_names_v1 = paste("subsp. [abcdefghijklmnopqrstuvwxyz.*]*", sep="")

					#Subspecies names v. 2
						#The pattern is 1) "subsp ", and 2) subspecies name in one or more lowercase letters
						#Certain other characters (.-*) are allowed in certain positions
						pattern_subspecies_names_v2 = paste("subsp\r\n[ ]+\r\n[ ]+[abcdefghijklmnopqrstuvwxyz.*]*", sep="")
					
					#Subspecies names v. 3
						#The pattern is 1) "subsp[.] ", and 2) subspecies name in one or more lowercase letters
						#Certain other characters (.-*) are allowed in certain positions
						pattern_subspecies_names_v3 = paste("subsp. [abcdefghijklmnopqrstuvwxyz.*]*", sep="")

					#Subspecies names v. 4
						#The pattern is 1) "subsp[.] nov.[ ]", and 2) subspecies name in one or more lowercase letters
						#Certain other characters (.-*) are allowed in certain positions
						pattern_subspecies_names_v4 = paste("subsp. nov.[ ]*[abcdefghijklmnopqrstuvwxyz.*]*", sep="")

					#Pathovar names
						#The pattern is 1) "pathovar ", and 2) subspecies name in one or more lowercase letters
						#Certain other characters (.-*) are allowed in certain positions
						pattern_pathovar_names = paste("pathovar [abcdefghijklmnopqrstuvwxyz.*]*", sep="")

					#Biovar names
						#The pattern is 1) "biovar ", and 2) subspecies name in one or more lowercase letters
						#Certain other characters (.-*) are allowed in certain positions
						pattern_biovar_names = paste("biovar [abcdefghijklmnopqrstuvwxyz.*]*", sep="")

					#Genomospecies number
						#The pattern is 1) "genomospecies", 2) a number
						#Certain other characters (.-*) are allowed in certain positions
						pattern_genomospecies_names = paste("genomospecies [1234567890]*", sep="")

						#Pattern for names
						pattern_names =  paste("(",pattern_subspecies_names_v1,")|(",pattern_subspecies_names_v2,")|(",pattern_subspecies_names_v3,")|(",pattern_subspecies_names_v4,")|(", pattern_pathovar_names, ")|(", pattern_biovar_names, ")|(", pattern_genomospecies_names, ")", sep="")

					#Find matches and extract names
						matches_names = gregexpr(pattern_names, subspecies_names[[i]][j])
						
					#If there are no matches, assign "NA" to name
						if(matches_names[[1]][1]==-1)
						{
							subspecies_names[[i]][j]=NA
						} else
						{
							subspecies_names[[i]][j] = unlist(regmatches(subspecies_names[[i]][j], matches_names))
						
							#Remove "subsp.", "biovar", and "pathovar"
								subspecies_names[[i]][j]= gsub("subsp[.]*[ ]*[n]*[o]*[v]*[.]*[ ]+|pathovar[ ]+|biovar[ ]+", "", subspecies_names[[i]][j])
				
							#Clean up formatting of subspecies name
								#Remove "\r\n" and spaces
								subspecies_names[[i]][j] = gsub("\r\n[ ]*", "", subspecies_names[[i]][j])

								#Remove "\"
								subspecies_names[[i]][j] = gsub("\"", "", subspecies_names[[i]][j])

								#Remove "*"
								subspecies_names[[i]][j] = gsub("\\*", "", subspecies_names[[i]][j])

								#Remove "\u0080\u009c"
								subspecies_names[[i]][j] = gsub("\u0080\u009c", "", subspecies_names[[i]][j])

								#Remove "\u0080\u009d"
								subspecies_names[[i]][j] = gsub("\u0080\u009d", "", subspecies_names[[i]][j])

								#Remove "<U+0080>"
								subspecies_names[[i]][j] = gsub("<U+0080>", "", subspecies_names[[i]][j])

								#Remove "<U+009C>"
								subspecies_names[[i]][j] = gsub("<U+009C>", "", subspecies_names[[i]][j])
						}
			}
			
		#Show progress of loop
			progress(value=i, max.value=length(url_list))		
	}

  return(subspecies_names)
}

#' Get Species Description
#'
#' This function gets a description of each organism
#' It finds the part of the species text that is relevant to each organism 
#'
#' @param url_list A character vector of url names for articles
#' @param names A list of organism names (one element per organism)
#' @param text_species A list containing the species text (one element per article)
#' @return A list containing the species descriptions (one element per organism)
#' @importFrom svMisc progress
#' @export
get_species_description  = function(url_list, names, text_species){

species_description = rep(list(list()), length(url_list))
	for(i in 1:length(url_list))
	{	
		for(j in 1:length(names[[i]]))
		{
			#Remove text before description of species j
			#Description starts with species name
			pattern_name = gsub("*", "[**]", names[[i]][j], fixed=TRUE)
			pattern_name = paste(pattern_name, ".*", sep="")
			matches_name = regexpr(pattern_name, text_species[[i]])

			#Run loop only if there are matches
			if(matches_name[[1]][1]!=-1)
			{
				species_description[[i]][j] = regmatches(text_species[[i]], matches_name)
			}

			#Remove text after description of species j
			#Description ends with next species name
				#If it is the last species, no text is parsed
				if(j==length(names[[i]]))
				{
					species_description[[i]][j] = species_description[[i]][j]
				} else
				{		
					pattern_next_name = paste(".*\r\n[ ]+", names[[i]][j+1], sep="")
					matches_next_name = regexpr(pattern_next_name, species_description[[i]][j])	
					#Run loop only if there are matches
					if(matches_next_name[[1]][1]!=-1)
					{
						species_description[[i]][j] = regmatches(species_description[[i]][j], matches_next_name)	
						species_description[[i]][j] = gsub(names[[i]][j+1], "", species_description[[i]][j])
					}
				}
				
			#If there are no matches, set species description to "NA"
			if(matches_name[[1]][1]==-1)
			{
				species_description[[i]][j] = NA
			}
		}
		
		#Show progress of loop
			progress(value=i, max.value=length(url_list))
	}
	
  return(species_description)
}

#' Get Strain ID
#'
#' This function gets type the strain ID for each organism
#'
#' @param url_list A character vector of url names for articles
#' @param names A list of organism names (one element per organism)
#' @param species_description A list containing the species descriptions (one element per organism)
#' @return A list of type strain IDs (one element per organism)
#' @importFrom svMisc progress
#' @export
get_strain_ID = function(url_list, names, species_description){
strain_ID = rep(list(list()), length(url_list))
	for(i in 1:length(url_list))
	{	
		#Extract IDs
		for(j in 1:length(names[[i]]))
		{

			#Set pattern for finding type strain ID
				#The pattern is 1) "1) Type strain: ", 2) any number of characters [the strain ID(s)], 3) ".\r\n"	
				#Certain characters (e.g., "\r\n") are also allowed in pattern
				#The text "(?<!(Source[ ][(]))" prevents a match if "Type strain" is preceded by "Source (" (see https://onlinelibrary.wiley.com/doi/full/10.1002/9781118960608.gbm00032)
				pattern_ID = paste("(?<!(Source[ ][(]))Type strain[ ]*(\r\n)?[ ]*:[ ]*", "(.*?)", "[.]\r\n", sep="")

			#Find matches and extract IDs
				matches_ID = gregexpr(pattern_ID, species_description[[i]][j], perl = TRUE)
				strain_ID[[i]][j] = regmatches(species_description[[i]][j], matches_ID)

			#If there are no matches, search for IDs using other patterns
					if(matches_ID[[1]][1]==-1)
					{
						##The pattern is 1) "Type strain: ", 2) any number of characters [the strain ID(s)], 3) "\r\n"	
						pattern_ID = paste("Type strain[ ]*(\r\n)?[ ]*:[ ]*", "(.*?)", "\r\n", sep="")
						matches_ID = gregexpr(pattern_ID, species_description[[i]][j], perl = TRUE)
						strain_ID[[i]][j] = regmatches(species_description[[i]][j], matches_ID)
						
					#Continue to search if no matches found
					if(matches_ID[[1]][1]==-1)
					{
						#The pattern is 1) "Deposited strain: ", 2) any number of characters [the strain ID(s)], 3) ".\r\n"	
						pattern_ID = paste("Deposited strain[ ]*(\r\n)?[ ]*:[ ]*", "(.*?)", "[.]\r\n", sep="")
						matches_ID = gregexpr(pattern_ID, species_description[[i]][j], perl = TRUE)
						strain_ID[[i]][j] = regmatches(species_description[[i]][j], matches_ID)

					#Continue to search if no matches found
					if(matches_ID[[1]][1]==-1)
					{
						#The pattern is 1) "Reference strain: ", 2) any number of characters [the strain ID(s)], 3) ".\r\n"	
						pattern_ID = paste("Reference strain[ ]*(\r\n)?[ ]*:[ ]*", "(.*?)", "[.]\r\n", sep="")
						matches_ID = gregexpr(pattern_ID, species_description[[i]][j], perl = TRUE)
						strain_ID[[i]][j] = regmatches(species_description[[i]][j], matches_ID)
						
					#Continue to search if no matches found
					if(matches_ID[[1]][1]==-1)
					{
						#The pattern is 1) "Type specimen: ", 2) any number of characters [the strain ID(s)], 3) ".\r\n"	
						pattern_ID = paste("Type specimen[ ]*(\r\n)?[ ]*:[ ]*", "(.*?)", "[.]\r\n", sep="")
						matches_ID = gregexpr(pattern_ID, species_description[[i]][j], perl = TRUE)
						strain_ID[[i]][j] = regmatches(species_description[[i]][j], matches_ID)
							
					#Continue to search if no matches found
					if(matches_ID[[1]][1]==-1)
					{
						#The pattern is 1) "Type material: ", 2) any number of characters [the strain ID(s)], 3) ".\r\n"	
						pattern_ID = paste("Type material[ ]*(\r\n)?[ ]*:[ ]*", "(.*?)", "[.]\r\n", sep="")
						matches_ID = gregexpr(pattern_ID, species_description[[i]][j], perl = TRUE)
						strain_ID[[i]][j] = regmatches(species_description[[i]][j], matches_ID)
						
					#Continue to search if no matches found
					if(matches_ID[[1]][1]==-1)
					{
						#The pattern is 1) "Type species: ", 2) any number of characters [the strain ID(s)], 3) ".\r\n"	
						pattern_ID = paste("Type species[ ]*(\r\n)?[ ]*:[ ]*", "(.*?)", "[.]\r\n", sep="")
						matches_ID = gregexpr(pattern_ID, species_description[[i]][j], perl = TRUE)
						strain_ID[[i]][j] = regmatches(species_description[[i]][j], matches_ID)

					#Continue to search if no matches found
					if(matches_ID[[1]][1]==-1)
					{
						#The pattern is 1) "Proposed type strain: ", 2) any number of characters [the strain ID(s)], 3) ".\r\n"	
						pattern_ID = paste("Proposed type strain[ ]*(\r\n)?[ ]*:[ ]*", "(.*?)", "[.]\r\n", sep="")
						matches_ID = gregexpr(pattern_ID, species_description[[i]][j], perl = TRUE)
						strain_ID[[i]][j] = regmatches(species_description[[i]][j], matches_ID)

					#Continue to search if no matches found
					if(matches_ID[[1]][1]==-1)
					{
						#The pattern is 1) "The proposed type strain: ", 2) any number of characters [the strain ID(s)], 3) ".\r\n"	
						pattern_ID = paste("The proposed type strain[ ]*(\r\n)?[ ]*:[ ]*", "(.*?)", "[.]\r\n", sep="")
						matches_ID = gregexpr(pattern_ID, species_description[[i]][j], perl = TRUE)
						strain_ID[[i]][j] = regmatches(species_description[[i]][j], matches_ID)

					#Continue to search if no matches found
					if(matches_ID[[1]][1]==-1)
					{
						#The pattern is 1) "Strain of effective publication: ", 2) any number of characters [the strain ID(s)], 3) ".\r\n"	
						pattern_ID = paste("Strain of [eE]ffective publication[ ]*(\r\n)?[ ]*:[ ]*", "(.*?)", "[.]\r\n", sep="")
						matches_ID = gregexpr(pattern_ID, species_description[[i]][j], perl = TRUE)
						strain_ID[[i]][j] = regmatches(species_description[[i]][j], matches_ID)
					
					#Continue to search if no matches found
					if(matches_ID[[1]][1]==-1)
					{
						#The pattern is 1) "Strain of effective publication: ", 2) any number of characters [the strain ID(s)], 3) "\r\n"	
						pattern_ID = paste("Strain of [eE]ffective publication[ ]*(\r\n)?[ ]*:[ ]*", "(.*?)", "\r\n", sep="")
						matches_ID = gregexpr(pattern_ID, species_description[[i]][j], perl = TRUE)
						strain_ID[[i]][j] = regmatches(species_description[[i]][j], matches_ID)

					#Continue to search if no matches found
					if(matches_ID[[1]][1]==-1)
					{
						#The pattern is 1) "Type strain is ", 2) any number of characters [the strain ID(s)], 3) ".\r\n"	
						pattern_ID = paste("Type strain is ", "(.*?)", "[.]\r\n", sep="")
						matches_ID = gregexpr(pattern_ID, species_description[[i]][j], perl = TRUE)
						strain_ID[[i]][j] = regmatches(species_description[[i]][j], matches_ID)
						
					#Continue to search if no matches found
					if(matches_ID[[1]][1]==-1)
					{
						#The pattern is 1) "The type strain is ", 2) any number of characters [the strain ID(s)], 3) ".\r\n"	
						pattern_ID = paste("The type strain is ", "(.*?)", "[.]\r\n", sep="")
						matches_ID = gregexpr(pattern_ID, species_description[[i]][j], perl = TRUE)
						strain_ID[[i]][j] = regmatches(species_description[[i]][j], matches_ID)
						
					#Continue to search if no matches found
					if(matches_ID[[1]][1]==-1)
					{
						#The pattern is 1) "Type strain ", 2) any number of characters [the strain ID(s)], 3) ".\r\n"	
						pattern_ID = paste("Type strain ", "(.*?)", "[.]\r\n", sep="")
						matches_ID = gregexpr(pattern_ID, species_description[[i]][j], perl = TRUE)
						strain_ID[[i]][j] = regmatches(species_description[[i]][j], matches_ID)

					#Continue to search if no matches found
					if(matches_ID[[1]][1]==-1)
					{
						#The pattern is 1) "Type strain", 2) "\r\n", 3) any number of characters [the strain ID(s)], 3) ".\r\n"	
						pattern_ID = paste("Type strain", "(\r\n)?", "(.*?)", "[.]\r\n", sep="")
						matches_ID = gregexpr(pattern_ID, species_description[[i]][j], perl = TRUE)
						strain_ID[[i]][j] = regmatches(species_description[[i]][j], matches_ID)
					
					#Continue to search if no matches found
					if(matches_ID[[1]][1]==-1)
					{

						#The pattern is 1) "Reference strain\r\n            \r\n            ", 2) "PCC ", and 3) any number	
						pattern_ID = paste("Reference strain[s]\r\n[ ]*+\r\n[ ]*", "PCC ", "[1234567890]*", sep="")
						matches_ID = gregexpr(pattern_ID, species_description[[i]][j], perl = TRUE)
						strain_ID[[i]][j] = regmatches(species_description[[i]][j], matches_ID)
						
					#If there are still no matches, assign "NA" to ID
					if(matches_ID[[1]][1]==-1)
					{
						strain_ID[[i]][j]=NA
					}}}}}}}}}}}}}}}}
			}		

			#Clean up formatting
			for(j in 1:length(names[[i]]))
			{
				strain_ID[[i]][j] = list(gsub("character...", NA, unlist(strain_ID[[i]][j]))) #Replace case of no match ["character" or "character(0)"] with "NA"
				strain_ID[[i]][j] = list(gsub("Type strain[ ]*(\r\n)?[ ]*:[ ]*", "", unlist(strain_ID[[i]][j]), perl = TRUE)) #Remove "Type strain: "
				strain_ID[[i]][j] = list(gsub("Deposited strain[ ]*(\r\n)?[ ]*:[ ]*", "", unlist(strain_ID[[i]][j]), perl = TRUE)) #Remove "Deposited strain: "
				strain_ID[[i]][j] = list(gsub("Reference strain[ ]*(\r\n)?[ ]*:[ ]*", "", unlist(strain_ID[[i]][j]), perl = TRUE)) #Remove "Reference strain: "
				strain_ID[[i]][j] = list(gsub("Reference strain[s]", "", unlist(strain_ID[[i]][j]), perl = TRUE)) #Remove "Reference strain"
				strain_ID[[i]][j] = list(gsub("Strain of [Ee]ffective publication[ ]*(\r\n)?[ ]*:[ ]*", "", unlist(strain_ID[[i]][j]), perl = TRUE)) #Remove "Strain of effective publication: "
				strain_ID[[i]][j] = list(gsub("The type strain is[ ]*(\r\n)?[ ]*", "", unlist(strain_ID[[i]][j]), perl = TRUE)) #Remove "The type strain is "
				strain_ID[[i]][j] = list(gsub("[.]\r\n.*", "", unlist(strain_ID[[i]][j]))) #Remove text after ".\r\n"
				strain_ID[[i]][j] = list(gsub("[.]\r\n", "", unlist(strain_ID[[i]][j]))) #Remove ".\r\n"
				strain_ID[[i]][j] = list(gsub('c[(]["]', "", unlist(strain_ID[[i]][j]))) #Remove 'c("'
				strain_ID[[i]][j] = list(gsub('")', "", unlist(strain_ID[[i]][j]))) #Remove '")'
				strain_ID[[i]][j] = list(gsub('\"', "", unlist(strain_ID[[i]][j]))) #Remove '\"'
				
				#Replace "no culture isolated" or similar langauge with "NA"
					strain_ID[[i]][j] = list(gsub("[Dd]escription, not isolated in axenic culture", "None", unlist(strain_ID[[i]][j])))
					strain_ID[[i]][j] = list(gsub("[Dd]escription, not isolated in axenic culture", "None", unlist(strain_ID[[i]][j]))) 
					strain_ID[[i]][j] = list(gsub("[Dd]escriptions and illustrations serving as type", "None", unlist(strain_ID[[i]][j])))
					strain_ID[[i]][j] = list(gsub("[Dd]escriptions and illustrations serving as type", "None", unlist(strain_ID[[i]][j]))) 
					strain_ID[[i]][j] = list(gsub("[Nn]o culture", "None", unlist(strain_ID[[i]][j]))) 
					strain_ID[[i]][j] = list(gsub("[Nn]o culture available", "None", unlist(strain_ID[[i]][j]))) 
					strain_ID[[i]][j] = list(gsub("[Nn]o culture has been isolated", "None", unlist(strain_ID[[i]][j])))
					strain_ID[[i]][j] = list(gsub("[Nn]o culture has been isolated", "None", unlist(strain_ID[[i]][j]))) 
					strain_ID[[i]][j] = list(gsub("[Nn]o culture isolated", "None", unlist(strain_ID[[i]][j])))
					strain_ID[[i]][j] = list(gsub("[Nn]o longer in culture", "None", unlist(strain_ID[[i]][j])))
					strain_ID[[i]][j] = list(gsub("[Nn]o pure culture has been isolated", "None", unlist(strain_ID[[i]][j])))
					strain_ID[[i]][j] = list(gsub("[Nn]o pure culture", "None", unlist(strain_ID[[i]][j])))
					strain_ID[[i]][j] = list(gsub("[Nn]o strain extant", "None", unlist(strain_ID[[i]][j])))
					strain_ID[[i]][j] = list(gsub("[Nn]o strain isolated", "None", unlist(strain_ID[[i]][j])))
					strain_ID[[i]][j] = list(gsub("[Nn]o type culture currently available", "None", unlist(strain_ID[[i]][j])))
					strain_ID[[i]][j] = list(gsub("[Nn]o type culture is currently available", "None", unlist(strain_ID[[i]][j]))) 
					strain_ID[[i]][j] = list(gsub("[Nn]o type material is available", "None", unlist(strain_ID[[i]][j])))
					strain_ID[[i]][j] = list(gsub("[Nn]o type material available", "None", unlist(strain_ID[[i]][j]))) 
					strain_ID[[i]][j] = list(gsub("[Nn]o type strain available", "None", unlist(strain_ID[[i]][j])))
					strain_ID[[i]][j] = list(gsub("[Nn]o type strain is available", "None", unlist(strain_ID[[i]][j])))
					strain_ID[[i]][j] = list(gsub("[Nn]o type", "None", unlist(strain_ID[[i]][j])))
					strain_ID[[i]][j] = list(gsub("[Nn]o type", "None", unlist(strain_ID[[i]][j]))) 
					strain_ID[[i]][j] = list(gsub("[Nn]one available", "None", unlist(strain_ID[[i]][j])))
					strain_ID[[i]][j] = list(gsub("[Nn]one cultivated", "None", unlist(strain_ID[[i]][j])))
					strain_ID[[i]][j] = list(gsub("[Nn]one designated", "None", unlist(strain_ID[[i]][j])))
					strain_ID[[i]][j] = list(gsub("[Nn]one has been designated", "None", unlist(strain_ID[[i]][j])))
					strain_ID[[i]][j] = list(gsub("[Nn]one isolated", "None", unlist(strain_ID[[i]][j])))
					strain_ID[[i]][j] = list(gsub("[Nn]one", "None", unlist(strain_ID[[i]][j])))
					strain_ID[[i]][j] = list(gsub("[Nn]on-specified due to difficulties in cultivation", "None", unlist(strain_ID[[i]][j])))
					strain_ID[[i]][j] = list(gsub("[Nn]ot avialable", "None", unlist(strain_ID[[i]][j])))
					strain_ID[[i]][j] = list(gsub("[Nn]ot cultivated; none designated", "None", unlist(strain_ID[[i]][j])))
					strain_ID[[i]][j] = list(gsub("[Nn]ot cultivated", "None", unlist(strain_ID[[i]][j])))
					strain_ID[[i]][j] = list(gsub("[Nn]ot yet grown in pure culture", "None", unlist(strain_ID[[i]][j])))
					strain_ID[[i]][j] = list(gsub("[Nn]ot designated", "None", unlist(strain_ID[[i]][j])))
					strain_ID[[i]][j] = list(gsub("[Nn]ot established", "None", unlist(strain_ID[[i]][j])))
					strain_ID[[i]][j] = list(gsub("[Nn]ot cultivated", "None", unlist(strain_ID[[i]][j])))
					strain_ID[[i]][j] = list(gsub("(?<!\\S)-(?!\\S)", "None", unlist(strain_ID[[i]][j]), perl=TRUE)) #"-" if not preceded or followed by any character (except space)

				strain_ID[[i]][j] = list(gsub("[Ss]train", "", unlist(strain_ID[[i]][j]))) #Remove "Strain"
				strain_ID[[i]][j] = list(gsub("[(]neopathotype strain[)]", "", unlist(strain_ID[[i]][j]))) #Remove "(neopathotype strain)"
				strain_ID[[i]][j] = list(gsub("[Hh]olotype", "", unlist(strain_ID[[i]][j]))) #Remove "holotype"
				strain_ID[[i]][j] = list(gsub("[Nn]eotype", "", unlist(strain_ID[[i]][j]))) #Remove "neotype"
				strain_ID[[i]][j] = list(gsub("[Tt]ype", "", unlist(strain_ID[[i]][j]))) #Remove "type"
				strain_ID[[i]][j] = list(gsub("[Ff]ormerly", "", unlist(strain_ID[[i]][j]))) #Remove "formely"
				strain_ID[[i]][j] = list(gsub("[Nn]ow", "", unlist(strain_ID[[i]][j]))) #Remove "now"
				strain_ID[[i]][j] = list(gsub("<U+0394>", "", unlist(strain_ID[[i]][j]))) #Remove "<U+0394>"
				
				#Delineate strains with commas
				#Strains are already delineated with commas in most articles, but not all
				#Replace any characters that can delineate strains with commas
				strain_ID[[i]][j] = list(gsub(', "', ", ", unlist(strain_ID[[i]][j]))) #Replace ', "' with ', '
				strain_ID[[i]][j] = list(gsub("[(]", ",", unlist(strain_ID[[i]][j]))) #Replace parentheses with ","
				strain_ID[[i]][j] = list(gsub("\\[", ",", unlist(strain_ID[[i]][j]))) #Replace parentheses with ","
				strain_ID[[i]][j] = list(gsub("]", ",", unlist(strain_ID[[i]][j]))) #Replace bracket with ","
				strain_ID[[i]][j] = list(gsub("[)]", ",", unlist(strain_ID[[i]][j]))) #Replace bracket with ","
				strain_ID[[i]][j] = list(gsub("=", ",", unlist(strain_ID[[i]][j]))) #Replace "=" with ","
				strain_ID[[i]][j] = list(gsub(";", ",", unlist(strain_ID[[i]][j]))) #Replace ";" with ","
				strain_ID[[i]][j] = strain_ID[[i]][[j]][1] #Choose only first element of ID (in case ID was split)

				#Split into list at commas
					strain_ID[[i]][j] = strsplit(unlist(strain_ID[[i]][j]), ",") 

				#Clean up formatting again
					strain_ID[[i]][j] = list(gsub("[(].*[)][ ]*", "", unlist(strain_ID[[i]][j]))) #Remove any paranthetical statements not removed above
					strain_ID[[i]][j] = list(trimws(unlist(strain_ID[[i]][j]))) #Remove leading and trailing spaces
					strain_ID[[i]][j]=lapply(strain_ID[[i]][j], function(x) x[!x %in% ""]) #Remove any empty elements

				#Remove elements that have 5 more spaces (likely to be full sentences)
					for(k in 1:length(strain_ID[[i]][[j]]))
					{
						strain_ID[[i]][[j]][k]=gsub("^([^ ]*[ ]){5,}[^ ]*$", "", strain_ID[[i]][[j]][k]) #Make element empty if 5 or more spaces
					}
					strain_ID[[i]][j]=lapply(strain_ID[[i]][j], function(x) x[!x %in% ""]) #Remove empty elements

				#Replace strain_ID with no elements ["character(0)"] with "NA"
					if(identical(unlist(strain_ID[[i]][j]), character(0)))
					{
						strain_ID[[i]][j]=NA
					}	
		
			}
	
	#Show progress of loop
		progress(value=i, max.value=length(url_list))
	}
	
  return(strain_ID)
}
