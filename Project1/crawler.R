#Required Libraries
# install.packages('bitops')
# install.packages('RCurl')
# install.packages('XML')
# install.packages('stringr')
# install.packages('httr')


library(bitops)
library(RCurl)
library(XML)
library(stringr)
library(httr)

# Url of the main page
site = "https://bmcmedgenet.biomedcentral.com/articles"
pagination_link = "searchType=journalSearch&sort=PubDate&page="

# Few regex patterns needed
link_pattern = "<a itemprop=\"url\" data-test=\"title-link\" href=\"/articles/(.*?)\">"
pub_date_pattern = "itemprop=\"datePublished\">(.*?)<"

# Function to convert string to date
to_date <- function(str){
    return(as.Date(str_replace_all(str,'\\s','-'),format="%d-%b-%Y"))
}

# Function that extracts links and sets up the link to navigate to every article based on the year and main articl page provided
get_links <- function(link, year){
    pageLinks = c()
    articlePaths = c()
    date = to_date(paste('01 January ',year, sep=""))

    page = readLines(link)
    articlePaths = str_match(page, link_pattern)
    articlePaths <- articlePaths[,2][ !is.na( articlePaths[,2] ) ]
    
    article_pub_dates = str_match(page, pub_date_pattern)
    article_pub_dates <- article_pub_dates[,2][ !is.na( article_pub_dates[,2] ) ]

    for (path in 1:length(articlePaths)){
        article_date = to_date(article_pub_dates[path])
        
        if( article_date >= date) {
            pageLinks[path] = paste(site,'/',articlePaths[path],sep="")
        }
        
    }
    return(pageLinks)
   
}

print('Process starting...')

options(warn=-1)

# Number of total pages in https://bmcmedgenet.biomedcentral.com/articles
lastPage = 43

# function to take input from user
readinteger <- function()
{ 
    n <- readline(prompt="Enter an integer: ")
    if (is.na(n)){
        n <- readinteger()
    }
    return(n)
}

min_year = readinteger()

# Navigates through all pages of the main list of articles and creates a list of all artcile links
allLinks = list()
print('Getting all article links...')
for(i in 1:lastPage){
    sitePage = paste(site,'?',pagination_link,i, sep="")
    allLinks[i] = list(get_links(sitePage, min_year))
}
allLinks = unlist(allLinks)
link_count = length(allLinks)
print(paste('Total of',link_count,'articles reteived ...'))


# Setting up the dataframe for outputting the data of each article 
colnames = c('title', 'authors', 'affiliations', 'corr_author', 'corr_author_link', 
                  'pub_date', 'pub_abstract', 'key_words', 'full_paper')
out = data.frame(matrix(ncol=length(colnames),nrow=0, dimnames=list(NULL, colnames)))


for(link in 1:link_count){
    pageData = readLines(allLinks[link])

    pattern_title = "dc.title\" content=\"(.*?)\""
    temp_title = str_match(pageData, pattern_title)
    title = temp_title[,2][ !is.na( temp_title[,2] ) ]
#     title

    pattern_author = "citation_author\" content=\"(.*?)\""
    temp_author = str_match(pageData, pattern_author)
    authors = temp_author[,2][ !is.na( temp_author[,2] ) ]
#     authors

    pattern_affiliations = "c-article-author-affiliation__address u-h3\">(.*?)</span>"
    temp_affiliations = grep(pattern_affiliations, pageData)
    affiliations = str_match_all(pageData[temp_affiliations] , pattern_affiliations)[[1]][,2]
#     affiliations

    pattern_corr_author = "email/correspondent/c1/new\">(.*?)<"
    temp_corr_author = str_match(pageData, pattern_corr_author)
    corr_author = temp_corr_author[,2][ !is.na( temp_corr_author[,2] ) ]
#     corr_author

    pattern_corr_author_link = "<a id=\"corresp-c1\" rel=\"nofollow\" href=\"/articles/(.*?)/email"
    temp_corr_author_link = str_match(pageData, pattern_corr_author_link)
    corr_author_link = temp_corr_author_link[,2][ !is.na( temp_corr_author_link[,2] ) ]
    corr_author_link = paste(site,"/",corr_author_link,"/email/correspondent/c1/new", sep="")
#     corr_author_link

    pattern_pub_date = "dc.date\" content=\"(.*?)\""
    temp_pub_date = str_match(pageData, pattern_pub_date)
    pub_date = temp_pub_date[,2][ !is.na( temp_pub_date[,2] ) ]
#     pub_date

    pattern_abstract = "citation_abstract\" content=\"(.*?)\""
    temp_abstract = str_match(pageData, pattern_abstract)
    pub_abstract = temp_abstract[,2][ !is.na( temp_abstract[,2] ) ]
#     pub_abstract

    pattern_key_words = "<span itemprop=\"about\">(.*?)<"
    temp_key_words = grep(pattern_key_words, pageData)
    if(length(temp_key_words) != 0){
        key_words = str_match_all(pageData[temp_key_words] , pattern_key_words)[[1]][,2]
    } else {
        key_words = 'None'
    }
#     key_words

    pattern_full_paper = "<p>(.*)<"
    temp_full_paper = grep(pattern_full_paper, pageData)
    full_paper = c()
    count = 1
    for (line in temp_full_paper){
        full_paper[count] = str_match_all(pageData[line] , pattern_full_paper)[[1]][,2]
        count = count + 1
    }
#     full_paper
    print(paste('Data Extracted and stored for article ',link,' out of ',link_count,sep=""))
	# Adding each article's extracted data to the dataframe
    out[link,]$title = title
    out[link,]$authors = paste(authors,collapse=", ")
    out[link,]$affiliations = paste(affiliations,collapse=", ")
    out[link,]$corr_author = paste(corr_author,collapse=",")
    out[link,]$corr_author_link = paste(corr_author_link,collapse=", ")
    out[link,]$pub_date = paste(pub_date,collapse=" ")
    out[link,]$pub_abstract = paste(pub_abstract,collapse=" ")
    out[link,]$key_words = paste(key_words,collapse=", ")
    out[link,]$full_paper = paste(full_paper,collapse=" ")


}

print('Outputting to csv...')
# Outputting dataframe to csv

print('Process Completed.')
write.csv(out,"C:/Users/Narasimha/repos/cs636-1/output.csv", row.names = FALSE)