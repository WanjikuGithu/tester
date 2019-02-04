if(!require(RSelenium))
{
  install.packages("RSelenium")
}

if(!require(readr))
{
  install.packages(readr)
}


if(!require(dplyr))
{
  install.packages("dplyr")
}

message("Launching browser")

rD <- rsDriver(port=4445L, check=FALSE)
remDr <- rD$client
remDr$setImplicitWaitTimeout(milliseconds = 20000)

# go to the webpage
message("Opening main page")
remDr$navigate("http://www.vagas.com.br/")

users <- list(list(UserName="Josinaldo Borges Leal", 
                   searchTxts = list("Gerente de Infraestrutura de TI e/ou Coordena??o",
                                     "Gerente de Tecnologia da Informa??o e/ou Coordena??o",
                                     "Gerente de Seguran?a da Informa??o e/ou Coordena??o"
                   )
),
list(UserName="Silvio Cesar de Campos", 
     searchTxts = list("Gerente de Projetos",
                       "Gerente de Produtos",
                       "Gerente de Portf?lios"
     )
)
)

for(userIdx in 1:length(users))
{
  userName <- users[[userIdx]]$UserName
  
  message("Processing user ", userIdx, "[", userName, "]")
  
  searchTxts <- users[[userIdx]]$searchTxts
  
  for(searchTxt in searchTxts)
  {
    message("Search criteria: ", searchTxt)
    
    #only use search bar first time
    if(userIdx == 1 && searchTxt == searchTxts[1])
    {
      message("Getting search bar element")
      #enter interest as search criteria
      repeat
      {
        searchBar <- suppressMessages(try(remDr$findElement('id', "q"), TRUE))
        
        if(class(searchBar) == "try-error")
        {
          message("Not yet. Sleeping 1")
          Sys.sleep(1)
          next
        }
        break
      }
      
      message("Clicking on search bar")
      #click on the search bar
      searchBar$clickElement()
      
      message("Clearing search bar")
      #clear the search bar
      searchBar$clearElement()
      
      message("Entering search criteria '", searchTxt, "'")
      remDr$sendKeysToActiveElement(list(searchTxt))
      
      #submit the search
      message("Locating search button")
      
      repeat
      {
        searchButton <- suppressMessages(try(remDr$findElement('id', "s"),TRUE))
        
        if(class(searchButton) == "try-error")
        {
          message("Not yet. Sleeping 1")
          Sys.sleep(1)
          next
        }
        break
      }
      
      message("Clicking search button")
      searchButton$clickElement()
      
      message("Sleeping 3")
      Sys.sleep(3)
      
      message("Setting cidade filter criteria")
      
      #Setting the cidade filter to Sao Paulo
      
      repeat
      {
        cidadeInput <- suppressMessages(try(remDr$findElement('class', "js-facet-ul"),TRUE))
        
        if(class(cidadeInput) == "try-error")
        {
          message("Not yet. Sleeping 1")
          Sys.sleep(1)
          next
        }
        break
      }
      
      cidadeElements <- cidadeInput$findChildElements("xpath", "li")
      
      message("clicking cidade elements")
      cidadeElements[[1]]$clickElement()
      
    }
  }
}

#getting all job titles and companies on the page

  message(Sys.time(), ": Getting job nodes")
  jobNodes <- suppressMessages(try(remDr$findElements('class',"informacoes-header"),TRUE))
  
  if(class(jobNodes) == "try-error")
  {
    message(Sys.time(), ": Not yet. Sleeping 1")
    Sys.sleep(1)
    next
  }
  break

a <- remDr$findElement("class", "cargo")
if(length(jobNodes) > 0)
 {
  message(Sys.time(), ": Getting job titles")
  jobTitles <- unlist(sapply(jobNodes, 
                             function(x)
                             {
                               a <- x$findElement("class", "cargo")
                               
                               b <- a$findChildElement("class", "link-detalhes-vaga")
                               
                               b$getElementAttribute("title")
                             }
  
  )
  )
}  
  message(Sys.time(), ": Getting job urls")
  jobUrls <- unlist(sapply(jobNodes, 
                           function(x)
                           {
                             a <- x$findElement("class", "cargo")
                             
                             b <- a$findChildElement("class", "link-detalhes-vaga")
                             
                             b$getElementAttribute("href")
                           }
  )
  )
  
  remDr$client$setImplicitWaitTimeout(1000)
  #is repeat required
  message(Sys.time(), ": Getting company names")
  companyNameNodes <- suppressMessages(try(remDr$findElements('class',"emprVaga"),TRUE))
  
  if(class(companyNameNodes) == "try-error")
  {
    message(Sys.time(), ": Not yet. Sleeping 1")
    Sys.sleep(1)
    next
  }
  break


companyNames <- unlist(sapply(companyNameNodes, function(x) x$getElementText()))

  remDr$setImplicitWaitTimeout(1000)                            
 
#job summaries
  remDr$setImplicitWaitTimeout(1000)
  
  message(Sys.time(), ": Getting job summaries")
  summaryNodes <- suppressMessages(try(remDr$findElements('class',"detalhes"),TRUE))
  
  if(class(summaryNodes) == "try-error")
  {
    message(Sys.time(), ": Not yet. Sleeping 1")
    Sys.sleep(1)
    next
  }
  
  jobSummaries <- unlist(sapply(summaryNodes, function(x) x$getElementText()))
  
  remDr$setImplicitWaitTimeout(20000)
  
  message(Sys.time(), ": Combining company details")
  #cbind jobnames with details
  jobAds <- dplyr::bind_cols("JobTitle"=jobTitles, "JobUrl"=jobUrls, "CompanyName"=companyNames, "ShortDescription"=jobSummaries)
  
  #Excluding confidencial ads

  notConfidencial <- grep("CONFIDENCIAL", jobAds$CompanyName, invert = T, ignore.case = T)

  jobAds <- jobAds[notConfidencial,]

  #add timestamp and searchtext
  jobAds <- dplyr::bind_cols("SearchTime"=rep(Sys.time(), nrow(jobAds)),
                           "SearchTxt"=rep(searchTxt, nrow(jobAds)),
                           jobAds)
  
  #next page
  message("Locating next page link")
  nextPageLink <- suppressMessages(try(remDr$findElement("class", "btMaisVagas"),TRUE))

  remDr$setImplicitWaitTimeout(20000)

  message("Sleeping 1")
  Sys.sleep(1)

  if(class(nextPageLink) == 'try-error')
{
  message("Last page detected. Sleeping 1")
  Sys.sleep(1)
  break
}

  message("Sleeping 3")
  Sys.sleep(3)

  message("Clicking next page")
  nextPageLink$clickElement()
  
  message("Sleeping 5")
  Sys.sleep(5)
#repeat until there's no next page link

#nextPageLink to have all the fields

#finished search for one criterion

#write to search results csv
message("Writing results to file")
#readr::write_csv(searchResults, jobSearchResults.csv)




