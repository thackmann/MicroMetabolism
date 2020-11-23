##Install and load packages
	install.packages("devtools")
	devtools::install_github(repo="thackmann/MicroMetabolism", subdir="MicroMetabolism")
	library(MicroMetabolism)

##Predict metabolic traits with neural networks
	#Load Data
	data_fp = system.file("extdata", "train_eval_data.csv", package="MicroMetabolism")
	raw_data = load_data(data_fp)

	#Get Labels
	labels_colname = "Label_trait"
	labels = get_labels(raw_data, labels_colname)

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
	embedding_dims = 50
	filters = 64
	kernel_size = 3
	hidden_dims = 50
	model = set_model(maxlen, max_vocab, embedding_dim, filters, kernel_size, hidden_dim)

	#Set Data
	train_fraction = 0.5
	data = set_data(text_seqs_padded, labels, train_fraction)

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
