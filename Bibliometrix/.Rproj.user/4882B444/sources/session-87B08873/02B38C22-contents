install.packages('bibliometrix')   
install.packages('xlsx')  
install.packages("reshape")
install.packages("ggplot2")
install.packages('dplyr')
biblioshiny() 
library(reshape)
library(bibliometrix) 
library(readxl)  
library(xlsx)  
library(tidytext)
library(forcats)
library(ggplot2)
library(dplyr)

# 1 - Annual Scientific Production
# 2 - Most Global Cited Documents
# 3 - Trending topics 
# 4 - Words' Frequency over Time



################################################################################
#                       ===>>> DATA LOADING AND CONVERTING 
################################################################################


# Saving papers from Lens in a variable
Lens_Bibliometrix_Export_File_2022_11_07 <- read_excel("Lens_Bibliometrix-Export-File-2022-11-07.xlsx")
#View(Lens_Bibliometrix_Export_File_2022_11_07) # to call a spreadsheet-style data viewer

# Saving papers from Dimensions in a variable
Dimensions_Bibliometrix_Export_File_2022_11_07 <- read_excel("Dimensions_Bibliometrix-Export-File-2022-11-07.xlsx")
#View(Dimensions_Bibliometrix_Export_File_2022_11_07) 

# Saving papers from Scopus in a variable
Scopus_Bibliometrix_Export_File_2022_11_07 <- read_excel("Scopus_Bibliometrix-Export-File-2022-11-07.xlsx")
#View(Scopus_Bibliometrix_Export_File_2022_11_07)

# Merging files 
Merged <- mergeDbSources(Dimensions_Bibliometrix_Export_File_2022_11_07, 
                         Scopus_Bibliometrix_Export_File_2022_11_07, 
                         Lens_Bibliometrix_Export_File_2022_11_07, 
                         remove.duplicated=TRUE)

# Debug only 
#write.xlsx(Merged,file="new.file_with_3_databases.xlsx") # Writing a data frame to Excel


################################################################################
#                       ===>>> PLOT 1: Annual Scientific Production
################################################################################

# Creating another variable to preserve the original variable "Merged" 
x <- Merged
# Creating a copy of the column PY using variable "Years", which is the variable used in the original code 
x$Years <- x$PY

# Placing the histogram in a variable 
Tab=table(x$Years)

## checking if there are missing years. The result was empty. There are no missing years.
YY=setdiff(seq(min(x$Years, na.rm=TRUE),max(x$Years, na.rm=TRUE)),names(Tab))
# Building data frame with corresponding years and values
Y=data.frame(Year=as.numeric(c(names(Tab),YY)),
             Freq=c(as.numeric(Tab),rep(0,length(YY))))
# Sorting  
Y=Y[order(Y$Year),]

# Removing papers from 2023 (3 papers) 
# New data frame Y without 2023:
Y <- Y[Y$Year < 2023,] 

# Creating subtitles 
names(Y)=c("Year","Frequency")

# Plotting
g=ggplot(Y, aes(x = .data$Year, y = .data$Frequency)) +
  geom_line() +
  geom_area(fill = 'grey90', alpha = .5) +
  labs(x = 'Year'
       , y = 'Articles'
       ) +
  scale_x_continuous(breaks= (Y$Year[seq(1,length(Y$Year),by=2)])) +
  theme(text = element_text(color = "#444444")
        ,panel.background = element_rect(fill = '#FFFFFF')
        ,panel.grid.minor = element_line(color = '#EFEFEF')
        ,panel.grid.major = element_line(color = '#EFEFEF')
        ,plot.title = element_text(size = 18)  
        ,axis.title = element_text(size = 8, color = '#555555') #articles,year titles 
        ,axis.title.y = element_text(vjust = 1, angle = 90)
        ,axis.title.x = element_text(hjust = 0.5)
        ,axis.text.x = element_text(size=8, angle = 90) # size of x dates 
        ,axis.text.y = element_text(size=8, angle = 90) # size of y dates 
        ,axis.line.x = element_line(color="black",size=0.5)
        ,axis.line.y = element_line(color="black",size=0.5)
  ) 

# Plotting the graph 
plot(g)

# Choosing a file name
filename = paste("AnnualScientificProduction", Sys.Date(), ".png", sep="")

# Setting specifications and saving the file 
ggsave(filename = filename, plot = g, dpi = 300, height = 8, width = 16, units="cm", bg="white")

################################################################################
#                 ===>>> PLOT 2: Most Global Cited Documents
################################################################################

# Creating another variable to preserve the original variable "Merged" 
values <- Merged

# Current Year - 2022 
current_year <- 2022

# Processing data (generates a table containing all the infomation about each paper and calculates citations)
TAB <- values %>% 
  mutate(TCperYear = .data$TC/(current_year+1-.data$PY)) %>% # creates the field for total citations per Year
  select(.data$SR,.data$DI, .data$TC, .data$TCperYear, .data$PY) %>%  # organizes the data set in five fields
  group_by(.data$PY) %>% # grouping by publication year 
  mutate(NTC = .data$TC/mean(.data$TC)) %>% # normalizing total citations
  ungroup() %>% # ungrouping 
  select(-.data$PY) %>% # remove column PY 
  arrange(desc(.data$TC)) %>% # arranging in descending order 
  as.data.frame() # creates a data frame 

# Adding names to fields:
names(TAB)=c("Paper", "DOI","Total Citations","TC per Year","Normalized TC") # Columns generated

# Measuring - Total citation
xx <- TAB %>% select(1,3)
lab="Global Citations"   

# To catch the first 41 documents
if (41>dim(xx)[1]){
  k=dim(xx)[1]
} else {k=41}
xx=xx[1:k,]  

# Preparing the Graph 
x=2 # corresponds to the second column (total citations)
y=1 # corresponds to the first column (document title)
textLaby = "Documents"
textLabx = lab
 
# Setting specifications to generate the graph 
g <- ggplot(xx, aes(x =xx[,x], y = xx[,y], label = xx[,x])) +
  geom_segment(aes(x = 0, y = xx[,y], xend = xx[,x], yend = xx[,y]), color = "grey50") +
  geom_point(aes(color=-xx[,x], size=xx[,x]), show.legend = FALSE) +
  scale_radius(range=c(8, 17))+
  geom_text(color = "white", size = 3.5) +
  scale_y_discrete(limits = rev(xx[,y])) +
  scale_fill_continuous(type = "gradient")+
  labs(y = textLaby)+
  labs(x = textLabx)+
  expand_limits(y= c(1, length(xx[,y]) + 2),x=c(1,300) )+
  theme_minimal()+
  theme(axis.title = element_text(size = 8, color = '#555555')  #articles,year titles 
  ,axis.text.x = element_text(size=8, angle = 90)  # size of x dates 
  ,axis.text.y = element_text(size=8, angle = 0)  # size of y dates 
        ) 
# Plotting the graph 
plot(g)

# Choosing a file name
filename = paste("MostGlobalCitedDocuments", Sys.Date(), ".png", sep="")

# Setting specifications and saving the file 
ggsave(filename = filename, plot = g, dpi = 300, height = 20, width = 16, units="cm", bg="white")


 
################################################################################
#                        ===>>> PLOT 3 - Trending topics 
################################################################################

# Creating another variable to preserve the original variable "Merged" 
values <- Merged

# load file with terms to remove
remove.terms <- trimws(unlist(strsplit(readr::read_lines("bigrama-filter-words.txt"), ",")))

# load file with synonyms to remove (synonyms file) 
syn.terms <- readr::read_lines("word-dynamics-synonyms.txt")
syn.terms <- gsub(",",";",syn.terms) #using function gsub because it needs to translate it to a specific pattern

### load file with synonyms
synonyms <- trimws(syn.terms)

# Preparing data for graph
 
# Defining variables - choosing options and preparing data for graph 
M=values 
Field = "AB" #AB means "abstract" 
stemming = FALSE
verbose = FALSE
ngrams=2

# Load stop words (for stop word lists in R)
data("stopwords",envir=environment()) 
data("stop_words", envir=environment(), package = "tidytext")  
stop_words <- stop_words %>% as.data.frame() # creating a data frame for all stop words

# Creating the list of the terms to be removed 
remove.terms <- c(remove.terms,stopwords$bigrams)

# Getting stop words in English 
stopwords=(stop_words$word)

# Set lower case 
stopwords <- tolower(stopwords)

# remove all special characters (except "-")
TERMS <- M %>% 
  select(.data$SR,!!Field)

names(TERMS) <- c("SR","text") # creating a title for the two columns (source / text - abstract)

TERMS$text <- gsub(" - "," ",TERMS$text) # Removing "-"

# Save original multi-words keywords
TERMS <- TERMS %>%
  mutate(text = tolower(gsub("[^[:alnum:][:blank:]\\-]", "", .data$text)), # getting rid of unimportant characters
         text = gsub("-", "__",.data$text)) 
 
# remove numbers - replace every digit with "" (empty)  
TERMS <- TERMS %>% mutate(text = gsub("[[:digit:]]","",.data$text))

# avoid run time errors (when the value is null it becomes "")
if (is.null(remove.terms)) remove.terms <- ""

# Text is data frame containing the corpus data text 
text=TERMS 

# Var is a string indicating the column name
Var="text"  

# nword is a integer vector indicating the ngrams to extract.  
nword=ngrams  
custom_stopwords=tolower(remove.terms)

# Typical stop words from scientific papers
stopwords <- c(stopwords,"elsevier", "springer", "wiley", "mdpi", "emerald", "originalityvalue", "designmethodologyapproach", 
                 "-", " -", "-present", "-based", "-literature", "-matter")
custom_stopngrams <- c(custom_stopwords,"rights reserved", "john wiley", "john wiley sons", "science bv", "mdpi basel", 
                         "mdpi licensee", "emerald publishing", "taylor francis", "paper proposes", 
                         "we proposes", "paper aims", "articles published", "study aims", "research limitationsimplications")
# processing phase
ngram <- NULL  

# variable ngrams is receiving the processed results
ngrams <- text %>%  
    drop_na(any_of(Var)) %>% # removing any empty values (example: a paper with no abstract)
    unnest_tokens(ngram, !!Var, token = "ngrams", n = nword) # Split a column into tokens, flattening the table into one-token-per-row. Creating bigrams 
  
# Finding ngrams starting with __ and processing
ind <- which(substr(ngrams$ngram,1,2) %in% "__")
ngrams$ngram[ind] <- trimws(substr(ngrams$ngram[ind],3,nchar(ngrams$ngram[ind]))) 

# Separating bigrams in two columns 
ngrams <- ngrams %>%  
    separate(.data$ngram, paste("word",1:nword,sep=""), sep = " ")
  
# come back to the original multi-word format
ngrams <- ngrams %>%
    mutate_at(paste("word",seq(1,nword),sep=""), ~gsub("__", "-",.)) %>% # replacing "__" with "-"
    mutate_at(paste("word",seq(1,nword),sep=""), ~gsub("_", " ",.))
   
# removing stop words (generic) 
ngrams <- ngrams %>% dplyr::filter(if_all(starts_with("word"), ~ !.x %in% stopwords)) 
  
# removing customized stop words  
ngrams <- ngrams %>% 
    unite(ngram, paste("word",1:nword,sep=""), sep = " ") %>%
    dplyr::filter(!.data$ngram %in% custom_stopngrams) %>%
    mutate(ngram = toupper(.data$ngram))
  
# Merged synonyms in the vector synonyms
if (length(synonyms)>0 & is.character(synonyms)){
  s <- strsplit(toupper(synonyms),";")
  snew <- trimws(unlist(lapply(s,function(l) l[1])))
  sold <- (lapply(s,function(l){
    l <- trimws(l[-1])
  }))
  for (i in 1:length(s)){
    ngrams$ngram[ngrams$ngram %in% unlist(sold[[i]])] <- snew[i]
  }
}

# Now the variable TERMS contains all bigrams
TERMS<-ngrams

# Bigrams
ngrams=2

# Grouping bigrams per document and separating with ";" 
TERMS <- TERMS %>%
  dplyr::filter(!(.data$ngram %in% paste(rep("NA",ngrams),sep="",collapse=" "))) %>% 
  group_by(.data$SR) %>%
  summarize(text = paste(.data$ngram, collapse=";"))

# Removing column AB_TM in case it exists
col_name <- paste(Field,"_TM",sep="") 
M <- M[!names(M) %in% col_name] 

# Joining bigrams with documents in the database
M <- TERMS %>% 
  right_join(M, by = "SR") 

# Column containing bigrams was called "text". Now it changes the column name to "AB_TM" 
names(M)[which(names(M) %in% "text")] <- col_name

# "M" Class is being redefined. It's turned into "data.frame" 
class(M) <- c("bibliometrixDB", "data.frame")
# The rows now receive the article titles 
row.names(M) <- M$SR
# The results are now in the variable values$M
values$M=M

# Choosing specifications to build the graph:
min.freq = 20 
n.items = 15

# Defining the time span (2020 - 2022)
timespan = c(2020,2022)

# cocMatrix (Co-occurrence matrix) computes co-occurences between elements of a Tag Field (AB_TM) from a bibliographic data frame. 
A <- cocMatrix(M, Field = "AB_TM", binary = FALSE, remove.terms = remove.terms, synonyms = synonyms)
n <- colSums(as.array(A)) #"n" contains each bigram with the corresponding number of ocurrence 

# organizing citation percentage in each quartile. Trend topics are organized in columns 
trend_med <- apply(A, 2, function(x) {   
  round(quantile(rep(M$PY, x), c(0.25,0.50,0.75), na.rm=TRUE))
})

# Trend topics are organized in rows and frequency is included (with number of occurrences of each term)
trend_med <- as_tibble(t(trend_med)) %>% 
  rename("year_q1"='25%', "year_med"='50%', "year_q3"='75%') %>%  
  mutate(item=rownames(t(trend_med)), freq=n) %>% 
  relocate(c(.data$item,.data$freq), .data$year_q1)

# Now it will lowercase and build a table with the parameters: n.items = 15, min.freq = 20 
df <- trend_med %>%
  mutate(item = tolower(.data$item)) %>%
  group_by(.data$year_med) %>%
  arrange(desc(.data$freq), .data$item) %>%
  arrange(desc(.data$year_med)) %>%   
  dplyr::slice_head(n=n.items) %>% 
  dplyr::filter(.data$freq >= min.freq) %>%
  dplyr::filter(between(.data$year_med, timespan[1],timespan[2])) %>%
  mutate(item = fct_reorder(.data$item, .data$freq))

# the axis with year range
yrange <- range(unlist(df[,which(regexpr("year",names(df))>-1)]))

# setting axis margins 
x <- c(0+0.5,0.05+length(levels(df$item))*0.125)+1
y <- c(yrange[2]-0.02-diff(yrange)*0.125,yrange[2]-0.02)

# Setting specifications to generate the graph 
g <- ggplot(df, aes(x=.data$item, y=.data$year_med, 
                    text = paste("Term: ", .data$item,"\nYear: ",
                                 .data$year_med ,"\nTerm frequency: ",.data$freq )))+
  geom_point(aes(size = .data$freq), alpha=0.6, color="dodgerblue4")+ 
  scale_size(range=c(2,6))+
  #scale_alpha(range=c(0.3,1))+
  scale_y_continuous(breaks = seq(timespan[1],timespan[2]))+
  guides(size = guide_legend(order = 1, "Term frequency"), alpha = guide_legend(order = 2, "Term frequency"))+
  theme(legend.position = 'right'
        #,aspect.ratio = 1
        ,text = element_text(color = "#444444")
        ,panel.background = element_rect(fill = '#FFFFFF')
        ,panel.grid.major.x = element_blank()
        ,panel.grid.major.y = element_line(color = 'grey95')
        ,plot.title = element_text(size = 24)
        ,axis.title = element_text(size = 14, color = '#555555')
        ,axis.title.y = element_text(vjust = 1, angle = 90, face="bold")
        ,axis.title.x = element_text(hjust = .95)
        ,axis.text.x = element_text(face="bold", angle = 90)#, size=labelsize)
        ,axis.text.y = element_text(face="bold",)
        ,axis.line.x = element_line(color="black", size=0.5)) 

# The small balls (shape, size, color)
g <- g+geom_vline(xintercept=nrow(df)-(which(c(diff(df$year_med))==-1)-0.5), color="grey70",alpha=0.6, linetype=6)+
      geom_point(aes(y=.data$year_q1), alpha=0.6, size = 3, color="royalblue4", shape="|")+
      geom_point(aes(y=.data$year_q3), alpha=0.6, size = 3, color="royalblue4", shape="|")

# Choosing titles (empty)
g <- g+
    labs(title="", 
         x="",
         y="")+
    geom_segment(data=df, aes(x = .data$item, y = .data$year_q1, xend = .data$item, yend = .data$year_q3), size=1.0, color="royalblue4", alpha=0.3) +
    coord_flip() 

# Plotting the graph 
plot(g)
  
# Choosing a file name
filename = paste("TrendingTopics", Sys.Date(), ".png", sep="")

# Setting specifications and saving the file 
ggsave(filename = filename, plot = g, dpi = 300, height = 20, width = 16, units="cm", bg="white")
  

 

################################################################################
#                        ===>>> PLOT 4 - Words' Frequency over Time
################################################################################

############
# Functions
############

KeywordGrowth <- function(M, Tag = "ID", sep = ";", top=10, cdf=TRUE, remove.terms=NULL, synonyms=NULL){
  i<-which(names(M)==Tag)
  PY=as.numeric(M$PY)
  Tab<-(strsplit(as.character(M[,i]),sep))
  Y=rep(PY,lengths(Tab))
  A=data.frame(Tab=unlist(Tab),Y=Y)
  A$Tab=trim.leading(A$Tab)
  A=A[A$Tab!="",]
  A=A[!is.na(A$Y),]
  
  ### remove terms
  terms <- data.frame(Tab=toupper(remove.terms))
  A <- anti_join(A,terms)
  # end of block
  
  ### Merge synonyms in the vector synonyms
  if (length(synonyms)>0 & is.character(synonyms)){
    s <- strsplit(toupper(synonyms),";")
    snew <- trimws(unlist(lapply(s,function(l) l[1])))
    sold <- (lapply(s,function(l) trimws(l[-1])))
    for (i in 1:length(s)){
      A <- A %>% 
        mutate(
          # Tab = str_replace_all(Tab, paste(sold[[i]], collapse="|",sep=""),snew[i])
          Tab= str_replace_all(Tab, str_replace_all(str_replace_all(paste(sold[[i]], collapse="|",sep=""),"\\(","\\\\("),"\\)","\\\\)"),snew[i])
          
        )
    }
  }
  # end of block
  
  Ymin=min(A$Y)
  Ymax=max(A$Y)
  Year=Ymin:Ymax
  Tab<-names(sort(table(A$Tab),decreasing=TRUE))[1:top]
  
  words=matrix(0,length(Year),top+1)
  words=data.frame(words)
  names(words)=c("Year",Tab)
  words[,1]=Year
  for (j in 1:length(Tab)){
    word=(table(A[A$Tab %in% Tab[j],2]))
    words[,j+1]=trim.years(word,Year,cdf)
    
  }
  return(words)
}

trim.years<-function(w,Year,cdf){
  
  Y=as.numeric(names(w))
  W=matrix(0,length(Year),1)
  
  for (i in 1:length(Year)){
    if (Y[1]==Year[i] & length(Y)>0){W[i,1]=w[1]
    Y=Y[-1]
    w=w[-1]}
  }
  if (isTRUE(cdf)) W=cumsum(W)
  names(W)=Year
  W=data.frame(W)
  return(W)}

####################

# Creating another variable to preserve the original variable "Merged" 
values <- Merged

### load file with terms to remove (stop words file)

remove.terms <- trimws(unlist(strsplit(readr::read_lines("bigrama-filter-words.txt"), ",")))

### load file with synonyms to remove (synonyms file) 

syn.terms <- readr::read_lines("word-dynamics-synonyms.txt")
syn.terms <- gsub(",",";",syn.terms) #using function gsub because it needs to translate it to a specific pattern

### load file with synonyms
synonyms <- trimws(syn.terms)


# Counting words  

cdf=TRUE # if input cumulative terms
laby="Cumulate occurrences"

# Defining variables - choosing options and preparing graph 
M=values 
Field = "AB"
stemming = FALSE
verbose = FALSE
ngrams=2

# load stop words

data("stopwords",envir=environment())
data("stop_words", envir=environment(), package = "tidytext")
stop_words <- stop_words %>% as.data.frame() # dataframe for all stop words

if (ngrams == 2){remove.terms <- c(remove.terms,stopwords$bigrams)}
language = "english"
switch(language,
       english={stopwords=(stop_words$word)},
       italian={stopwords=stopwords$it},
       german={stopwords=stopwords$de},
       french={stopwords=stopwords$fr},
       spanish={stopwords=stopwords$es}
)
stopwords <- tolower(stopwords)

# remove all special characters (except "-" becoming "_")
TERMS <- M %>% 
  select(.data$SR,!!Field)

names(TERMS) <- c("SR","text") #SR - source / text - abstract

TERMS$text <- gsub(" - "," ",TERMS$text)

# save original multi-words keywords

TERMS <- TERMS %>%
  mutate(text = tolower(gsub("[^[:alnum:][:blank:]\\-]", "", .data$text)),
         text = gsub("-", "__",.data$text))


# remove numbers
# replace every digit with "" (empty) - cleaning numbers
TERMS <- TERMS %>% mutate(text = gsub("[[:digit:]]","",.data$text))

keep.terms=NULL
# keep terms in the vector keep.terms
if (length(keep.terms)>0 & is.character(keep.terms)){
  keep.terms <- tolower(keep.terms)
  if (Field %in% c("DE","ID")){
    kt <- gsub(" ","_",keep.terms)
    kt <- gsub("-","__",keep.terms)
  } else {
    kt <- gsub("-","__",keep.terms)
  }
  for (i in 1:length(keep.terms)){
    TERMS <- TERMS %>%
      mutate(text = gsub(keep.terms[i],kt[i],.data$text))
  }
}

if (is.null(remove.terms)) remove.terms <- ""


text=TERMS # text is data frame containing the corpus data text 
Var="text" # Var is a string indicating the column name
nword=ngrams # nword is a integer vector indicating the ngrams to extract. I.e. nword = c(2,3)
custom_stopwords=tolower(remove.terms)

stopwords <- c(stopwords,"elsevier", "springer", "wiley", "mdpi", "emerald", "originalityvalue", "designmethodologyapproach", 
               "-", " -", "-present", "-based", "-literature", "-matter")
custom_stopngrams <- c(custom_stopwords,"rights reserved", "john wiley", "john wiley sons", "science bv", "mdpi basel", 
                       "mdpi licensee", "emerald publishing", "taylor francis", "paper proposes", 
                       "we proposes", "paper aims", "articles published", "study aims", "research limitationsimplications")
ngram <- NULL # processing phase


ngrams <- text %>% # variable ngrams is receiving the processed results
  drop_na(any_of(Var)) %>% # removing empty values 
  unnest_tokens(ngram, !!Var, token = "ngrams", n = nword) # Split a column into tokens, flattening the table into one-token-per-row. 

ind <- which(substr(ngrams$ngram,1,2) %in% "__")

ngrams$ngram[ind] <- trimws(substr(ngrams$ngram[ind],3,nchar(ngrams$ngram[ind])))

ngrams <- ngrams %>%  
  separate(.data$ngram, paste("word",1:nword,sep=""), sep = " ")

## come back to the original multi-word format

ngrams <- ngrams %>%
  mutate_at(paste("word",seq(1,nword),sep=""), ~gsub("__", "-",.)) %>% 
  mutate_at(paste("word",seq(1,nword),sep=""), ~gsub("_", " ",.))


ngrams <- ngrams %>% dplyr::filter(if_all(starts_with("word"), ~ !.x %in% stopwords)) ## removing stop words (generic)

# removing customized stop words  
ngrams <- ngrams %>% 
  unite(ngram, paste("word",1:nword,sep=""), sep = " ") %>%
  dplyr::filter(!.data$ngram %in% custom_stopngrams) %>%
  mutate(ngram = toupper(.data$ngram))

# Merged synonyms in the vector synonyms
if (length(synonyms)>0 & is.character(synonyms)){
  s <- strsplit(toupper(synonyms),";")
  snew <- trimws(unlist(lapply(s,function(l) l[1])))
  sold <- (lapply(s,function(l){
    l <- trimws(l[-1])
  }))
  for (i in 1:length(s)){
    ngrams$ngram[ngrams$ngram %in% unlist(sold[[i]])] <- snew[i]
  }
}

TERMS<-ngrams

#### 

ngrams=2
TERMS <- TERMS %>%
  dplyr::filter(!(.data$ngram %in% paste(rep("NA",ngrams),sep="",collapse=" "))) %>% 
  group_by(.data$SR) %>%
  summarize(text = paste(.data$ngram, collapse=";"))

# assign the vector to the bibliographic data frame
col_name <- paste(Field,"_TM",sep="")
M <- M[!names(M) %in% col_name]

M <- TERMS %>% 
  right_join(M, by = "SR") 
names(M)[which(names(M) %in% "text")] <- col_name


# display results 
if (verbose==TRUE){ 
  s <- tableTag(M,col_name)
  
  if (length(s>25)){print(s[1:25])}else{print(s)}
}

class(M) <- c("bibliometrixDB", "data.frame")
row.names(M) <- M$SR

#agregando uma coluna m

values$M=M

KW=KeywordGrowth(values$M, Tag = "AB_TM", sep = ";", top = 5, cdf = cdf)

topKW=KW
DF=melt(topKW, id='Year')

# Building graph

values=KW 

term=names(values)[-1]
term=rep(term,each=dim(values)[1])
n=dim(values)[1]*(dim(values)[2]-1)
freq=matrix(as.matrix(values[,-1]),n,1)

results_to_graph_from_DF=data.frame(Year=rep(KW$Year,(dim(KW)[2]-1)),Term=term, Freq=freq, stringsAsFactors = TRUE)
width_scale <- 2.5 * 26 / length(unique(results_to_graph_from_DF$Term))

Text <- paste(DF$Term," (",results_to_graph_from_DF$Year,") ",results_to_graph_from_DF$Freq, sep="")

x <- c(max(results_to_graph_from_DF$Year)-0.02-diff(range(2016:2022))*0.20, max(results_to_graph_from_DF$Year)-0.02)-1
y <- c(min(results_to_graph_from_DF$Freq),min(results_to_graph_from_DF$Freq)+diff(range(results_to_graph_from_DF$Freq))*0.20)

# Setting specifications to generate the graph 
g <- ggplot(results_to_graph_from_DF, aes(x=.data$Year,y=.data$Freq, group=.data$Term, color=.data$Term, text = Text))+
  geom_line()+
  labs(x = 'Year'
       , y = laby
       , title = "") +
  scale_x_continuous(breaks= (KW$Year[seq(1,length(KW$Year),by=ceiling(length(KW$Year)/20))])) +
  geom_hline(aes(yintercept=0), alpha=0.1)+
  labs(color = "")+
  theme(text = element_text(color = "#444444"),
        legend.text=ggplot2::element_text(size=8),
        legend.box.margin = margin(6, 6, 6, 6),
        legend.title=ggplot2::element_text(size=1.5*width_scale,face="bold"),
        legend.position=c(0.2,0.85),,
        legend.direction = "vertical",
        legend.key.size = grid::unit(width_scale/50, "inch"),
        legend.key.width = grid::unit(width_scale/50, "inch")
        ,plot.caption = element_text(size = 8, hjust = 0.5, color = "black", face = "bold")
        ,panel.background = element_rect(fill = '#FFFFFF')
        ,panel.grid.minor = element_line(color = '#EFEFEF')
        ,panel.grid.major = element_line(color = '#EFEFEF')
        ,plot.title = element_text(size = 8)
        ,axis.title = element_text(size = 8, color = '#555555')
        ,axis.title.y = element_text(vjust = 1, angle = 90)
        ,axis.title.x = element_text(hjust = 0.5, angle = 0)
        ,axis.text.x = element_text(size=8, angle = 90)
        ,axis.line.x = element_line(color="black",size=0.5)
        ,axis.line.y = element_line(color="black",size=0.5)
  ) 

# Plotting the graph 
plot(g)

# Choosing a file name
filename = paste("WordDynamics", Sys.Date(), ".png", sep="")

# Setting specifications and saving the file
ggsave(filename = filename, plot = g, dpi = 300, height = 20, width = 16, units="cm", bg="white")

 
 
 
 

