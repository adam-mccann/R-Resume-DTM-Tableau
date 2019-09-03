# create a function with your steps to run
nlp_function <- function(TableauDF)
  {
    # set local working directory where resume files are saved
    setwd("/Users/adam.mccann/Documents/Confidential/Resume NLP/Junior BI Analyst")
    # list files
    files <- list.files(pattern = "pdf$")
    # pull out texti
    text <- lapply(files, pdf_text)
    # set the number of files a term must appear in to appear in the tdm to 10% of files
    columns <-(nrow(data.frame(files))*.10)
    # create corpus of files
    corp <- Corpus(URISource(files),
               readerControl = list(reader = readPDF))
    # clean up corpurs
    corp <- tm_map(corp, removePunctuation, ucp = TRUE)
    # create term document matrix removing punctuation, stop words, numbers and convert to lower
    resume.tdm <- TermDocumentMatrix(corp, 
                                   control = 
                                     list(removePunctuation = TRUE,
                                          stopwords = TRUE,
                                          tolower = TRUE,
                                          stemming = TRUE,
                                          removeNumbers = TRUE,
                                          bounds = list(global = c(columns, Inf)))) 
    # create a data frame from tdm
    df <- data.frame(as.matrix(resume.tdm))
    setDT(df, keep.rownames = TRUE)
    colnames(df)[colnames(df)=="rn"] <- "Term"
    
    # transpose data frame
    return(data.frame(gather(df, "Resume", "Freq", -Term, na.rm=FALSE, convert=FALSE)))
      };

getOutputSchema <- nlp_function(){
    return(data.frame(df));}
