# SemanticNetworkVizR
codes to perform semantic network analysis on multiple concepts (defined as multiple words-set, i.e. dictionaries) across multiple texts with R



The repository consists in a .Rmd file, that allows to run single chunks of code in R (similar to a notebook), a folder ('SemanticApp'), which can be downloaded to run the relative shiny web application on locale, and a app.R file (plus a www folder containing the images of the app) that is the same file contained in the SemanticApp folder but allows to run the app directly from GitHub.

1) To run the .Rmd file: download the file and open it on your computer with R or RStudio (suggested). The R markdown script has the advantage of allowing multiple texts analysis at the same time. The web app, for the moment, allows the analysis of just one text at time. 

2) To run the web app on your computer: download the SemanticApp folder and then use the following codes -->                             From R/RStudio: shiny::runApp('<the SemanticApp location on your computer>/SemanticApp/app.R')
From Windows Command Prompt: "<the location of R on your computer>\R.exe" -e "shiny::runApp('<the SemanticApp location on your computer>/SemanticApp/app.R')"    
*NB: the Command Prompt will give a message saying "Listening on:" followed by an https address, copy the https address on your browser to open the web app.
  
3) To run the web app directly from GitHub use the following codes-->
From R/Rstudio: shiny::runGitHub('Ighina/SemanticNetworkVizR','Ighina')
From Windows Command Prompt: "<the location of R on your computer>\R.exe" -e "shiny::runGitHub('Ighina/SemanticNetworkVizR','Ighina')
*NB: the Command Prompt will give a message saying "Listening on:" followed by an https address, copy the https address on your browser to open the web app.

The repository includes also a "Text&dictionary example" folder containing three erotic books (in .txt format) across different countries and times on which to perform the codes. The "dictionaries" file contains an example of possible dictionaries to be used as the concepts to be investigated in the books.

*NB at the moment, for a better performance, is preferable to include every word in the dictionary twice, one with first letter capitalised and one without.
