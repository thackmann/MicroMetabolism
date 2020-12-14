# MicroMetabolism
Tools for predicting metabolic traits of microbes from written text.

## Overview
MicroMetabolism predicts metabolic traits of microbes from written descriptions. With it, users can download articles from Bergey's Manual, extract out names of microbial species, and retrieve descriptions for those species. Users can then predict metabolic traits from the descriptions using a neural network model. 

Before predicting traits, users must provide data to train the neural network model.  The training data should be descriptions the user has manually labelled as positive or negative for traits of interest.   

Users can verify installation with test data.  Test data provided are descriptions labelled as positive or negative for one trait for 100 hypothetical species.  

Bergey's Manual is accessible only with a license. Users should follow all applicable copyright laws.

## Installation 
### Experienced R users
In R, run 
<font face="Courier New">devtools::install_github(repo="thackmann/MicroMetabolism", subdir="MicroMetabolism")</font> 

### New R users
1)  Download and install R (https://cran.r-project.org/mirrors.html).  Install latest version (or any version later than v. 3.6.0).
2)  In the R menu, click “File -> New script”.  Paste the <a href="https://github.com/thackmann/MicroMetabolism/blob/master/MicroMetabolism/demo/demo_make_predictions.R">demo script</a> for making predictions into the new window.
3)  In R menu, click “Edit -> Run all”.  

### Making predictions		
#### With test data
1)  In R, open the <a href="https://github.com/thackmann/MicroMetabolism/blob/master/MicroMetabolism/demo/demo_make_predictions.R">demo script</a> for making predictions.
2)  In R menu, click “Edit -> Run all”.  Accept all prompts to update R packages and install Miniconda.  

#### With user data
1)  In R, open the <a href="https://github.com/thackmann/MicroMetabolism/blob/master/MicroMetabolism/demo/demo_download_and_extract.R">demo script</a> for downloading articles and extracting descriptions.
2)  In R menu, click “Edit -> Run all”.
3)  Use the data extracted to prepare training data.  Specifically, read written descriptions and label some species as positive or negative for traits. Species not labeled will have their traits predicted.  See <a href="https://github.com/thackmann/MicroMetabolism/blob/master/MicroMetabolism/inst/extdata/make_predictions.csv">test data</a> for an example.
  
### Testing accuracy
#### With test data
1)  In R, open the <a href="https://github.com/thackmann/MicroMetabolism/blob/master/MicroMetabolism/demo/demo_test_accuracy.R">demo script</a> for testing accuracy.
2)  In R menu, click “Edit -> Run all”.  Accept all prompts to update R packages and install Miniconda.  

#### With user data
1)  In R, open the <a href="https://github.com/thackmann/MicroMetabolism/blob/master/MicroMetabolism/demo/demo_download_and_extract.R">demo script</a> for downloading articles and extracting descriptions.
2)  In R menu, click “Edit -> Run all”.
3)  Use the data extracted to prepare training data.  Specifically, read written descriptions and label species as positive or negative for traits. All species must be labeled, or accuracy cannot be evalauted.  See <a href="https://github.com/thackmann/MicroMetabolism/blob/master/MicroMetabolism/inst/extdata/test_accuracy.csv">test data</a> for an example.

## Troubleshooting
* <a href= "https://github.com/thackmann/MicroMetabolism/blob/main/troubleshoot/cannot-run-TensorFlow.md">When running R, TensorFlow delivers message "Error: Installation of TensorFlow not found."</a>
* <a href= "https://github.com/thackmann/MicroMetabolism/blob/main/troubleshoot/cannot-download-Bergey.md">Articles fail to download from Bergey's Manual, or extracted information looks incomplete</a>
 
## License
Copyright 2020 Timothy J. Hackmann

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
