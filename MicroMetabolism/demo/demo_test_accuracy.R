##Install TensorFlow
	install.packages("tensorflow")
	install.packages("remotes")
	remotes::install_github("rstudio/reticulate")
	remotes::install_github("rstudio/tensorflow")
	remotes::install_github("rstudio/keras")
	library(tensorflow)
	install_tensorflow()

##Install and load MicroMetabolism
	install.packages("devtools")
	devtools::install_github(repo="thackmann/MicroMetabolism", subdir="MicroMetabolism")
	library(MicroMetabolism)

##Predict metabolic traits with neural networks
	#Load Data
	data_fp = system.file("extdata", "test_accuracy.csv", package="MicroMetabolism")
	raw_data = read.csv(data_fp, stringsAsFactors=FALSE)

	#Get IDs
	ID_colname = "ID"
	ID=get_ID(raw_data, ID_colname)
  
	#Get Labels
	labels_colname = "Label_trait"
	label_positive = "Y"
	label_negative = "N"
	label_none = ""
	labels = get_labels(raw_data, labels_colname, label_positive, label_negative, label_none)

	#Get Text
	text_colname = "Sentences_trait"
	text = get_text(raw_data, text_colname)

	#Tokenize Text
	nchar_max = 20000
	max_vocab = 3000
	text_seqs = tokenize_text(text, nchar_max, max_vocab)

	#Pad Text
	maxlen = 8
	text_seqs_padded = pad_text(text_seqs, maxlen)

	#Set Model Architecture
	embedding_dim = 50
	filters = 64
	kernel_size = 3
	hidden_dim = 50
	model = set_model(maxlen, max_vocab, embedding_dim, filters, kernel_size, hidden_dim)

	#Set Data
	train_fraction = 0.8
	data = set_data(ID, text_seqs_padded, labels, train_fraction)

	#Train Model
	x_train = data$x_train
	y_train = data$y_train
	batch_size = 32
	epochs = 50
	model_fp = "model_keras.hdf5"
	train_model(model, x_train, y_train, batch_size, epochs, model_fp)

	#Make Predictions
	x_eval = data$x_eval
	y_pred = make_predictions(model_fp, x_eval)
	
	#Evaluate Predictions
	y_eval = data$y_eval
	metrics = evaluate_predictions(y_pred, y_eval)
	print(metrics)
