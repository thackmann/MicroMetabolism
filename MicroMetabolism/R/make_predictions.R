#' Get IDs
#'
#' This function gets species IDs from the raw data
#'
#' @param raw_data A dataframe containing the raw data (columns for ID, genus, species, text, and labels)
#' @param ID_colname  A character string giving the column name for IDs in the raw data
#' @return A numeric vector IDs with one element per species
#' @importFrom dplyr "%>%"
#' @export
get_ID = function(raw_data, ID_colname){
  ID = (raw_data %>% select(all_of(ID_colname)))
  ID = ID[[1]]
  return(ID)
}

#' Get Labels
#'
#' This function gets data labels from the raw data and formats them for input into the neural network
#'
#' @param raw_data A dataframe containing the raw data (columns for ID, genus, species, text, and labels)
#' @param labels_colname  A character string giving the column name for labels in the raw data
#' @param label_positive  A character string for when trait is labeled as positive
#' @param label_negative  A character string for when trait is labeled as negative
#' @param label_none  A character string for when trait is not labeled
#' @return A numeric vector of data labels with one element per species (1=trait positive, 0=trait negative, NA=trait not labeled)
#' @importFrom dplyr select
#' @export
#'
get_labels = function(raw_data, labels_colname, label_positive, label_negative, label_none){
  labels = (raw_data %>% select(all_of(labels_colname)))
  labels = labels[[1]]

  labels[which(labels==label_positive)]=1
  labels[which(labels==label_negative)]=0
  labels[which(labels==label_none)]=""

  labels=(as.numeric(labels))

  return(labels)
}


#' Get Text
#'
#' This function gets text from the raw data and formats it for tokenization
#'
#' @param raw_data A dataframe containing the raw data (columns for ID, genus, species, text, and labels)
#' @param text_colname  A character string giving the column name for text in the raw data
#' @return A character vector of text with one element per species
#' @importFrom dplyr "%>%"
#' @export
get_text = function(raw_data, text_colname){
  text = (raw_data %>% select(all_of(text_colname)))
  text = text[[1]]
  return(text)
}

#' Tokenize Text
#'
#' This function converts text (written species descriptions) into machine-readable tokens
#'
#' @param text A character vector of text with one element per species
#' @param nchar_max A numeric scalar giving the maximum length of the text (in characters); additional characters are truncated
#' @param nchar_max A numeric scalar giving the number of unique words to keep in the text; additional words are removed
#' @return A list of tokenized text with one element per species
#' @importFrom dplyr "%>%"
#' @importFrom keras text_tokenizer fit_text_tokenizer texts_to_sequences
#' @export
tokenize_text = function(text, nchar_max=25000, max_vocab=3000){
  text = ifelse(text == "", yes = "NA", no = text)
  text[which(nchar(text) > nchar_max)] = substr(text[which(nchar(text) > nchar_max)], start = 0, stop = nchar_max)
  tokenizer = text_tokenizer(num_words = max_vocab)
  tokenizer %>% fit_text_tokenizer(text)
  text_seqs = texts_to_sequences(tokenizer, text)

  return(text_seqs)
}

#' Pad Text
#'
#' This function pads the tokenized text for input into the network
#'
#' @param text_seqs A list of tokenized text with one element per species
#' @param maxlen A numeric scalar giving the length of text (in words); additional words are truncated
#' @return A matrix of tokenized and padded text
#' @importFrom dplyr "%>%"
#' @importFrom keras pad_sequences
#' @export
pad_text = function(text_seqs, maxlen=3000){
  text_seqs_padded = text_seqs %>% pad_sequences(maxlen = maxlen)

  return(text_seqs_padded)
}

#' Set Model Architecture
#'
#' This function defines the architecture of the neural network model in Keras
#'
#' @param maxlen A numeric scalar giving the length of text (in words); additional words are truncated
#' @param max_vocab A numeric scalar giving the number of unique words to keep in the text; additional words are removed
#' @param embedding_dim A numeric scalar of the output dimensions for the embedding layer
#' @param filters A numeric scalar of the number of fulters for convolution
#' @param kernel_size A numeric scalar of the kernel size for convolution
#' @param hidden_dim A numeric scalar of the output dimensions for the hidden dense layer
#' @return A Keras neural network model
#' @importFrom dplyr "%>%"
#' @importFrom keras keras_model_sequential layer_embedding layer_dropout layer_conv_1d layer_global_max_pooling_1d layer_dense layer_dropout layer_activation compile
#' @export
set_model = function(maxlen=3000, max_vocab=3000, embedding_dim=50, filters=64, kernel_size=3, hidden_dim=50){
  model = keras_model_sequential() %>%
    layer_embedding(input_dim=max_vocab, output_dim = embedding_dim, input_length = maxlen) %>%
    layer_dropout(0.2) %>%
    layer_conv_1d(filters=filters, kernel_size=kernel_size, padding = "valid", activation = "relu", strides = 1) %>%
    layer_global_max_pooling_1d() %>%
    layer_dense(units=hidden_dim) %>%
    layer_dropout(0.2) %>%
    layer_activation("relu") %>%
    layer_dense(1) %>%
    layer_activation("sigmoid") %>% compile(
      loss = "binary_crossentropy",
      optimizer = "adam",
      metrics = "accuracy"
    )

  return(model)
}


#' Set Model Architecture for RNN
#'
#' This function defines the architecture of the neural network model in Keras
#'
#' @param maxlen A numeric scalar giving the length of text (in words); additional words are truncated
#' @param max_vocab A numeric scalar giving the number of unique words to keep in the text; additional words are removed
#' @param embedding_dim A numeric scalar of the output dimensions for the embedding layer
#' @param lstm_units A numeric scalar of the number LSTM units
#' @return A Keras neural network model
#' @importFrom dplyr "%>%"
#' @importFrom keras keras_model_sequential layer_embedding layer_lstm layer_dense compile
#' @export
set_model_RNN = function(maxlen=20000, max_vocab=3000, embedding_dim=128, lstm_units=64){
  model <- keras_model_sequential()
  model %>%
    layer_embedding(input_dim = max_vocab, output_dim = embedding_dim) %>%
    layer_lstm(units = lstm_units, dropout = 0.2, recurrent_dropout = 0.2) %>%
    layer_dense(units = 1, activation = 'sigmoid')
  model %>% compile(
    loss = 'binary_crossentropy',
    optimizer = 'adam',
    metrics = c('accuracy')
    )

  return(model)
}

#' Set Data
#'
#' This function splits labels and text into training and evaluation data for input into the network.
#'
#' @param ID A numeric vector IDs with one element per species
#' @param text_seqs_padded A matrix of tokenized and padded text
#' @param labels A numeric vector of data labels with one element per species (1=trait positive, 0=trait negative, NA=trait not labeled)
#' @param train_fraction A numeric scalar giving the fraction of data to be split off for training; if set to 1, all data is used for training except those with no labels
#' @return A list of training and evaluation data and their species IDs
#' @export
set_data = function (ID, text_seqs_padded, labels, train_fraction=1)
{
  x_data = text_seqs_padded
  y_data = labels

  if(train_fraction<1)
  {
    ind = sample(c(TRUE, FALSE), length(text_seqs), replace = TRUE,
                 prob = c(train_fraction, 1 - train_fraction))

    train_ID = ID[ind]
    eval_ID = ID[!ind]
    x_train = x_data[ind, ]
    x_eval = x_data[!ind, ]
    y_train = y_data[ind]
    y_eval = y_data[!ind]
  }

  if(train_fraction==1)
  {
    train_ID = ID[!is.na(labels)]
    eval_ID = ID[is.na(labels)]
    x_train = x_data[!is.na(labels),]
    x_eval = x_data[is.na(labels),]
    y_train = y_data[!is.na(labels)]
    y_eval = y_data[is.na(labels)]
  }

  data=list(train_ID, x_train, y_train, eval_ID, x_eval, y_eval)
  names(data) = c("train_ID", "x_train", "y_train", "eval_ID", "x_eval", "y_eval")

  return(data)
}

#' Train Model
#'
#' This function trains the neural network model and saves it in HDF5 format
#'
#' @param model A Keras neural network model
#' @param x_train A matrix of of tokenized and paddded text used as training data
#' @param y_train A numeric vector of labels used as training data
#' @param batch_size A numeric scalar of the batch size used in training
#' @param epochs A numeric scalar of the number of epochs used for training
#' @param model_fp The file path to save the model in HDF5 format
#' @return A list of tokenized text with one element per species
#' @importFrom dplyr "%>%"
#' @importFrom keras fit save_model_hdf5
#' @export
train_model = function(model, x_train, y_train, batch_size=32, epochs=10, model_fp){
  hist = model %>%
    fit(
      x = x_train,
      y = y_train,
      batch_size = batch_size,
      epochs = epochs,
      validation_split = 0
    )

  save_model_hdf5(model, model_fp)
}

#' Make Predictions
#'
#' This function loads a trained neural network model (HDF5 format) then makes predictions
#'
#' @param model_fp The file path where the model is saved in HDF5 format
#' @param x_eval A matrix of tokenized and paddded text used as evaluation data
#' @return A numeric vector of predictions (1=trait positive, 0=trait negative)
#' @importFrom dplyr "%>%"
#' @importFrom keras load_model_hdf5
#' @export
  make_predictions = function(model_fp, x_eval){
   model = load_model_hdf5(model_fp)
   y_pred = model %>% predict(x_eval[1:nrow(x_eval),])
   y_pred = round(y_pred)

   return(y_pred)
  }


#' Evaluate Predictions
#'
#' This function calculates accuracy, precision, and sensivity of predictions
#'
#' @param y_eval A numeric vector of labels used as evaluation data
#' @param y_pred A numeric vector of predictions (1=trait positive, 0=trait negative)
#' @return A list containing numeric scalars for accuracy, precision, and sensitivity
#' @importFrom dplyr "%>%"
#' @export

evaluate_predictions = function(y_pred, y_eval){

  results=data.frame(y_pred,y_eval)

  true_positive = nrow(results %>% dplyr::filter(y_eval==1) %>% dplyr::filter(y_pred==1))
  true_negative = nrow(results %>% dplyr::filter(y_eval==0) %>% dplyr::filter(y_pred==0))
  false_positive = nrow(results %>% dplyr::filter(y_eval==0) %>% dplyr::filter(y_pred==1))
  false_negative = nrow(results %>% dplyr::filter(y_eval==1) %>% dplyr::filter(y_pred==0))

  accuracy = (true_positive+true_negative)/(true_positive + true_negative + false_positive + false_negative)
  precision = (true_positive)/(true_positive + false_positive)
  sensitivity = (true_positive)/(true_positive + false_negative)

  metrics = list(accuracy, precision, sensitivity)
  names(metrics) = c("accuracy", "precision", "sensitivity")

  return(metrics)
}

#' Save Evaluation Metrics
#'
#' This function writes evaluation metrics to a csv file
#'
#' @param accuracy A numeric scalar of the accuracy of predictions
#' @param precision A numeric scalar of the precision of predictions
#' @param sensitivity A numeric scalar of the sensitivity of predictions
#' @param evaluation_fp  A character string giving the file path of the csv file
#' @param evaluation_number A numeric scalar for
#' #' @export

save_evaluation = function(accuracy, precision, sensitivity, evaluation_number, evaluation_fp){
  evaluation_summary = read.csv(evaluation_fp)

  evaluation_summary$accuracy[evaluation_number]=accuracy
  evaluation_summary$precision[evaluation_number]=precision
  evaluation_summary$sensitivity[evaluation_number]=sensitivity

  write.csv(evaluation_summary, evaluation_fp, row.names=FALSE)
}

#' Load Data
#'
#' This function loads a csv file containing the raw data for input into the neural network
#'
#' @param data_fp A character string giving the file path of the csv file
#' @return A dataframe containing the raw data (columns for ID, genus, species, text, and labels)
#' @export
load_data = function(data_fp){
  raw_data = read.csv(data_fp, stringsAsFactors=FALSE)
  raw_data$ID <- seq.int(nrow(raw_data))

  return(raw_data)
}

#' Load Parameters
#'
#' This function loads a csv file containing parameters to run the neural network
#'
#' @param parameters_fp A character string giving the file path of the csv file
#' @return A dataframe containing the parameters
#' @export
load_parameters = function(parameters_fp){
  parameters = read.csv(parameters_fp, stringsAsFactors=FALSE)

  return(parameters)
}

#' Create CSV for Evaluation Metrics
#'
#' This function creates a csv file where evaluation metrics can be stored
#'
#' @param parameters A dataframe containing the parameters
#' @return A dataframe with columns for evalaution metrics
#' @export

create_evaluation_file = function(parameters, evaluation_fp){
  evaluation_summary = parameters

  evaluation_summary$accuracy=NA
  evaluation_summary$precision=NA
  evaluation_summary$sensitivity=NA

  write.csv(evaluation_summary, evaluation_fp)
}
