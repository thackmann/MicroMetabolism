##Install and load packages
	install.packages("devtools")
	devtools::install_github(repo="thackmann/MicroMetabolism", subdir="MicroMetabolism")
	library(MicroMetabolism)

##Download articles from Bergey's Manual
	#Get url names for articles
	url_list=get_url_names()

	#Filter url names for articles
	url_list=filter_url_names(url_list=url_list)

	#Set HTML Names
	destfile_names_Bergey=set_html_names(url_list=url_list)

	#Download HTML Pages
	directory_fp="G:/My Drive/UF desktop BU/L drive BU/other/Bergey"
	download_html_pages(directory_fp=directory_fp, url_list=url_list, destfile_names_Bergey=destfile_names_Bergey)

##Extract text from Bergey's Manual
	#Read HTML pages
	html_list = read_html_pages(directory_fp=directory_fp, url_list=url_list, destfile_names_Bergey=destfile_names_Bergey)

	#Get Full Text
	text_full = get_full_text(directory_fp=directory_fp, url_list=url_list, destfile_names_Bergey=destfile_names_Bergey, html_list=html_list)

	#Get Tables
	text_tables = get_tables(directory_fp=directory_fp, url_list=url_list, destfile_names_Bergey=destfile_names_Bergey, html_list=html_list)

	#Get Abstracts
	text_abstract = get_abstract(directory_fp=directory_fp, url_list=url_list, destfile_names_Bergey=destfile_names_Bergey, html_list=html_list)

	#Get Taxonomy
	taxonomy = get_taxonomy(directory_fp=directory_fp, url_list=url_list, destfile_names_Bergey=destfile_names_Bergey, html_list=html_list)

	#Get Main Text
	text_main = get_main_text(url_list=url_list, text_full=text_full)

	#Get Genus Text
	text_genus = get_genus_text(url_list=url_list, text_full=text_full, text_tables=text_tables)

	#Get Species Text
	text_species = get_species_text(url_list=url_list, text_full=text_full, text_tables=text_tables)

##Extract information on organisms from Bergey's Manual
	#Get Organism Names
	names = get_organism_names(url_list=url_list, taxonomy=taxonomy, text_species=text_species)

	#Get Species Names
	species_names = get_species_names(url_list=url_list, taxonomy=taxonomy, names=names)

	#Get Subspecies Names
	subspecies_names = get_subspecies_names(url_list=url_list, names=names)

	#Get Species Description
	species_description = get_species_description(url_list=url_list, names=names, text_species=text_species)

	#Get Strain ID
	strain_ID = get_strain_ID(url_list=url_list, names=names, species_description=species_description)

##Extract sentences related to metabolic traits
	#Get Sentences Related to Fermentation
	sentences_fermentation = get_sentences_fermentation(url_list=url_list, names=names, text_genus=text_genus, species_description=species_description)

	#Get Sentences Related to Acetate Production
	sentences_fermentation_to_acetate = get_sentences_acetate(url_list=url_list, names=names, text_genus=text_genus, species_description=species_description)

##Export data as csv
	#Combine Data Into One Object
	combined_data = combine_data(taxonomy = taxonomy, url_list=url_list, names=names, species_names=species_names, subspecies_names=subspecies_names, strain_ID=strain_ID, sentences_fermentation=sentences_fermentation, sentences_fermentation_to_acetate=sentences_fermentation_to_acetate)
	write.csv(x = combined_data, file = "combined_data.csv")
