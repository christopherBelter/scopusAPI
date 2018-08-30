## version 0.4
searchByString <- function(string, content = "complete", myStart = 0, retCount = 25, retMax = Inf, mySort = "-coverDate", cursor = "*", outfile) {
	if (!content %in% c("complete", "standard")) {
		stop("Invalid content value. Valid content values are 'complete', and 'standard'")
	}
	else {
		##library(httr)
		##library(XML)
		key <- "yourAPIKey"
		print("Retrieving records.")
		theURL <- httr::GET("https://api.elsevier.com/content/search/scopus", query = list(apiKey = key, query = string, sort = mySort, httpAccept = "application/xml", view = content, count = retCount, start = myStart, cursor = cursor)) ## format the URL to be sent to the API
		httr::stop_for_status(theURL) ## pass any HTTP errors to the R console
		theData <- httr::content(theURL, as = "text") ## extract the content of the response
		newData <- XML::xmlParse(theURL) ## parse the data to extract values
		resultCount <- as.numeric(XML::xpathSApply(newData,"//opensearch:totalResults", XML::xmlValue)) ## get the total number of search results for the string
		cursor <- XML::xpathSApply(newData, "//cto:cursor", XML::xmlGetAttr, name = "next", namespaces = "cto")
		print(paste("Found", resultCount, "records."))
		retrievedCount <- retCount + myStart ## set the current number of results retrieved for the designated start and count parameters
		while (resultCount > retrievedCount && retrievedCount < retMax) { ## check if it's necessary to perform multiple requests to retrieve all of the results; if so, create a loop to retrieve additional pages of results
			myStart <- myStart + retCount ## add the number of records already returned to the start number
			print(paste("Retrieved", retrievedCount, "of", resultCount, "records. Getting more."))
			theURL <- httr::GET("https://api.elsevier.com/content/search/scopus", query = list(apiKey = key, query = string,  sort = mySort, httpAccept = "application/xml", view = content, count = retCount, cursor = cursor)) ## get the next page of results
			theData <- paste(theData, httr::content(theURL, as = "text")) ## paste new theURL content to theData; if there's an HTTP error, the XML of the error will be pasted to the end of theData
			newData <- httr::content(theURL, as = "text")
			newData <- XML::xmlParse(theURL)
			cursor <- XML::xpathSApply(newData, "//cto:cursor", XML::xmlGetAttr, name = "next", namespaces = "cto")
			if (httr::http_error(theURL) == TRUE) { ## check if there's an HTTP error
				print("Encountered an HTTP error. Details follow.") ## alert the user to the error
				print(httr::http_status(theURL)) ## print out the error category, reason, and message
				break ## if there's an HTTP error, break out of the loop and return the data that has been retrieved
				}
			retrievedCount <- retrievedCount + retCount ## add the number of results retrieved in this iteration to the total number of results retrieved
			Sys.sleep(1)
		} ## repeat until retrievedCount >= resultCount
		print(paste("Retrieved", retrievedCount, "records. Formatting and saving results."))
		writeLines(theData, outfile, useBytes = TRUE) ## if there were multiple pages of results, they come back as separate XML files pasted into the single outfile; the theData XML object can't be coerced into a string to do find/replace operations, so I think it must be written to a file and then reloaded; useBytes = TRUE keeps the UTF-8 encoding of special characters like the copyright symbol so they won't throw an error later
		theData <- readChar(outfile, file.info(outfile)$size) ## convert the XML results to a character vector of length 1 that can be manipulated
		theData <- gsub("<?xml version=\"1.0\" encoding=\"UTF-8\"?>", "", theData, fixed = TRUE, useBytes = TRUE)
		theData <- gsub("<search-results.+?>", "", theData, useBytes = TRUE)
		theData <- gsub("</search-results>", "", theData, fixed = TRUE) ## remove all headers and footers of the separate XML files
		theData <- paste("<?xml version=\"1.0\" encoding=\"UTF-8\"?>", "<search-results xmlns=\"http://www.w3.org/2005/Atom\" xmlns:cto=\"http://www.elsevier.com/xml/cto/dtd\" xmlns:atom=\"http://www.w3.org/2005/Atom\" xmlns:prism=\"http://prismstandard.org/namespaces/basic/2.0/\" xmlns:opensearch=\"http://a9.com/-/spec/opensearch/1.1/\" xmlns:dc=\"http://purl.org/dc/elements/1.1/\">", theData, "</search-results>", sep = "\n")
		#theData <- paste(theData, "</search-results>") ## add the correct header to the beginning of the file and the correct footer to the end of the file
		writeLines(theData, outfile, useBytes = TRUE) ## save the correctly formatted XML file
		print("Done")
		return(theData) ## return the final, correctly formatted XML file
	}
}

searchByID <- function(theIDs, idtype, datatype = "application/xml", content = "complete", myStart = 0, retCount = 25, outfile) {
	##library(httr)
	##library(XML)
	key <- "yourAPIKey"
	if (length(theIDs) == 1) {
		theIDs <- unique(scan(theIDs, what = "varchar")) ## load the list of IDs into a character vector
	}
	else {
		theIDs <- unique(as.character(theIDs))
	}
	resultCount <- as.numeric(length(theIDs)) ## get the total number of IDs
	idList <- split(theIDs, ceiling(seq_along(theIDs)/25)) ## split the IDs into batches of 25
	theData <- " " ## create an empty character holder for the XML
	retrievedCount <- 0 ## set the current number of records retrieved to zero
	if (idtype == "pmid") {
		idList <- lapply(mapply(paste, "PMID(", idList, collapse = ") OR "), paste, ")") ## append the correct scopus search syntax around each number in each batch of IDs
	}
	else if (idtype == "doi") {
		idList <- lapply(mapply(paste, "DOI(", idList, collapse = ") OR "), paste, ")")
	}
	else if (idtype == "eid") {
	idList <- lapply(mapply(paste, "EID(", idList, collapse = ") OR "), paste, ")") ## append the correct scopus search syntax around each number
	}
	else {
	stop("Invalid idtype. Valid idtypes are 'pmid', 'doi', or 'eid'")
	}
	print(paste("Retrieving", resultCount, "records."))
	for (i in 1:length(idList)) { ## loop through the list of search strings and return data for each one
		string <- idList[i]
		theURL <- httr::GET("https://api.elsevier.com/content/search/scopus", query = list(apiKey = key, query = string, httpAccept = "application/xml", view = content, count = retCount, start = myStart))
		theData <- paste(theData, httr::content(theURL, as = "text")) ## paste new theURL content to theData
		if (httr::http_error(theURL) == TRUE) { ## check if there's an HTTP error
			print("Encountered an HTTP error. Details follow.") ## alert the user to the error
			print(httr::http_status(theURL)) ## print out the error category, reason, and message
			break ## if there's an HTTP error, break out of the loop and return the data that has been retrieved
			}
		Sys.sleep(1)
		retrievedCount <- retrievedCount + retCount
		print(paste("Retrieved", retrievedCount, "of", resultCount, "records. Getting more."))
	}
	print(paste("Retrieved", retrievedCount, "records. Formatting and saving results."))
	writeLines(theData, outfile, useBytes = TRUE)
	theData <- readChar(outfile, file.info(outfile)$size)
	theData <- gsub("<?xml version=\"1.0\" encoding=\"UTF-8\"?>", "", theData, fixed = TRUE, useBytes = TRUE)
	theData <- gsub("<search-results.+?>", "", theData, useBytes = TRUE)
	theData <- gsub("</search-results>", "", theData, fixed = TRUE) ## remove all headers and footers of the separate XML files
	theData <- paste("<?xml version=\"1.0\" encoding=\"UTF-8\"?>", "<search-results xmlns=\"http://www.w3.org/2005/Atom\" xmlns:cto=\"http://www.elsevier.com/xml/cto/dtd\" xmlns:atom=\"http://www.w3.org/2005/Atom\" xmlns:prism=\"http://prismstandard.org/namespaces/basic/2.0/\" xmlns:opensearch=\"http://a9.com/-/spec/opensearch/1.1/\" xmlns:dc=\"http://purl.org/dc/elements/1.1/\">", theData, "</search-results>", sep = "\n")
	#theData <- paste(theData, "</search-results>") ## add the correct header to the beginning of the file and the correct footer to the end of the file
	writeLines(theData, outfile, useBytes = TRUE)
	print("Done")
	return(theData)
}

extractXML <- function(theFile) {
	##library(XML)
	newData <- XML::xmlParse(theFile) ## parse the XML
	records <- XML::getNodeSet(newData, "//cto:entry", namespaces = "cto") ## create a list of records for missing or duplicate node handling
	scopusID <- lapply(records, XML::xpathSApply, "./cto:eid", XML::xmlValue, namespaces = "cto") ## handle potentially missing eid nodes
	scopusID[sapply(scopusID, is.list)] <- NA
	scopusID <- unlist(scopusID)
	doi <- lapply(records, XML::xpathSApply, "./prism:doi", XML::xmlValue, namespaces = c(prism = "http://prismstandard.org/namespaces/basic/2.0/")) ## handle potentially missing doi nodes
	doi[sapply(doi, is.list)] <- NA
	doi <- unlist(doi)
	pmid <- lapply(records, XML::xpathSApply, "./cto:pubmed-id", XML::xmlValue, namespaces = "cto") ## handle potentially missing pmid nodes: returns a list with the node value if the node is present and an empty list if the node is missing
	pmid[sapply(pmid, is.list)] <- NA ## find the empty lists in pmid and set them to NA
	pmid <- unlist(pmid) ## turn the pmid list into a vector
	authLast <- lapply(records, XML::xpathSApply, ".//cto:surname", XML::xmlValue, namespaces = "cto") ## grab the surname and initials for each author in each record, then paste them together 
	authLast[sapply(authLast, is.list)] <- NA
	authInit <- lapply(records, XML::xpathSApply, ".//cto:initials", XML::xmlValue, namespaces = "cto")
	authInit[sapply(authInit, is.list)] <- NA
	authors <- mapply(paste, authLast, authInit, collapse = "|")
	authors <- sapply(strsplit(authors, "|", fixed = TRUE), unique) ## remove the duplicate author listings
	authors <- sapply(authors, paste, collapse = "|")
	affiliations <- lapply(records, XML::xpathSApply, ".//cto:affilname", XML::xmlValue, namespaces = "cto") ## handle multiple affiliation names
	affiliations[sapply(affiliations, is.list)] <- NA
	affiliations <- sapply(affiliations, paste, collapse = "|")
	affiliations <- sapply(strsplit(affiliations, "|", fixed = TRUE), unique) ## remove the duplicate affiliation listings
	affiliations <- sapply(affiliations, paste, collapse = "|")
	countries <- lapply(records, XML::xpathSApply, ".//cto:affiliation-country", XML::xmlValue, namespaces = "cto")
	countries[sapply(countries, is.list)] <- NA
	countries <- sapply(countries, paste, collapse = "|")
	countries <- sapply(strsplit(countries, "|", fixed = TRUE), unique) ## remove the duplicate country listings
	countries <- sapply(countries, paste, collapse = "|") 
	year <- lapply(records, XML::xpathSApply, "./prism:coverDate", XML::xmlValue, namespaces = c(prism = "http://prismstandard.org/namespaces/basic/2.0/"))
	year[sapply(year, is.list)] <- NA
	year <- unlist(year)
	year <- gsub("\\-..", "", year) ## extract only year from coverDate string (e.g. extract "2015" from "2015-01-01")
	articletitle <- lapply(records, XML::xpathSApply, "./dc:title", XML::xmlValue, namespaces = c(dc = "http://purl.org/dc/elements/1.1/"))
	articletitle[sapply(articletitle, is.list)] <- NA
	articletitle <- unlist(articletitle)
	journal <- lapply(records, XML::xpathSApply, "./prism:publicationName", XML::xmlValue, namespaces = c(prism = "http://prismstandard.org/namespaces/basic/2.0/")) ## handle potentially missing issue nodes
	journal[sapply(journal, is.list)] <- NA
	journal <- unlist(journal)
	volume <- lapply(records, XML::xpathSApply, "./prism:volume", XML::xmlValue, namespaces = c(prism = "http://prismstandard.org/namespaces/basic/2.0/")) ## handle potentially missing issue nodes
	volume[sapply(volume, is.list)] <- NA
	volume <- unlist(volume)
	issue <- lapply(records, XML::xpathSApply, "./prism:issueIdentifier", XML::xmlValue, namespaces = c(prism = "http://prismstandard.org/namespaces/basic/2.0/")) ## handle potentially missing issue nodes
	issue[sapply(issue, is.list)] <- NA
	issue <- unlist(issue)
	pages <- lapply(records, XML::xpathSApply, "./prism:pageRange", XML::xmlValue, namespaces = c(prism = "http://prismstandard.org/namespaces/basic/2.0/")) ## handle potentially missing issue nodes
	pages[sapply(pages, is.list)] <- NA
	pages <- unlist(pages)
	abstract <- lapply(records, XML::xpathSApply, "./dc:description", XML::xmlValue, namespaces = c(dc = "http://purl.org/dc/elements/1.1/")) ## handle potentially missing abstract nodes
	abstract[sapply(abstract, is.list)] <- NA
	abstract <- unlist(abstract)
	keywords <- lapply(records, XML::xpathSApply, "./cto:authkeywords", XML::xmlValue, namespaces = "cto")
	keywords[sapply(keywords, is.list)] <- NA
	keywords <- unlist(keywords)
	keywords <- gsub(" | ", "|", keywords, fixed = TRUE)
	ptype <- lapply(records, XML::xpathSApply, "./cto:subtypeDescription", XML::xmlValue, namespaces = "cto")
	ptype[sapply(ptype, is.list)] <- NA
	ptype <- unlist(ptype)
	timescited <- lapply(records, XML::xpathSApply, "./cto:citedby-count", XML::xmlValue, namespaces = "cto")
	timescited[sapply(timescited, is.list)] <- NA
	timescited <- unlist(timescited)
	theDF <- data.frame(scopusID, doi, pmid, authors, affiliations, countries, year, articletitle, journal, volume, issue, pages, keywords, abstract, ptype, timescited, stringsAsFactors = FALSE)
	return(theDF)
}