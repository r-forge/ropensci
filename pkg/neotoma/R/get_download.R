
#' Function to return full dataset records.
#'
#' Using the dataset ID, return all records associated with the data.  At present,
#'    only returns the dataset in an unparsed format, not as a data table.   This function will only download one dataset at a time.
#'
#' @import RJSONIO RCurl
#' @param datasetid A single numeric dataset ID or a vector of numeric dataset IDs as returned by \code{get_datasets}.
#' @param verbose logical; should messages on API call be printed?
#' @author Simon J. Goring \email{simon.j.goring@@gmail.com}
#' @return This command returns either a 'try-error' definined by the error returned
#'    from the Neotoma API call, or a list comprising the following items:
#'
#' \itemize{
#'  \item{metadata}{A table describing the collection, including dataset information, PI data compatable with \code{get_contacts} and site data compatable with \code{get_sites}.}
#'  \item{sample.meta}{Dataset information for the core, primarily the age-depth model and chronology.}
#'  \item{taxon.list}{The list of taxa contained within the dataset, unordered, including information that can be used in \code{get_taxa}}
#'  \item{counts}{The assemblage data for the dataset, arranged with each successive depth in rows and the taxa as columns.  All taxa are described in \code{taxon.list}, the chronology is in \code{sample.data}}
#'  \item{lab.data}{A data frame of laboratory data, such as exotic pollen
#'  spike, amount of sample counted, etc.}
#' }
#'
#'    A full data object containing all the relevant assemblage information and metadata neccessary to understand a site.
#'    The data object is a list of lists and data.frames that describe an assemblage, the constituent taxa, the chronology, site and PIs who contributed the data.
#' @examples \dontrun{
#' #  Search for sites with "Thuja" pollen that are older than 8kyr BP and
#' #  that are on the west coast of North America:
#' t8kyr.datasets <- get_datasets(taxonname='Thuja*', loc=c(-150, 20, -100, 60), ageyoung = 8000)
#'
#' #  Returns 3 records (as of 04/04/2013), get dataset for the first record, Gold Lake Bog.
#' GOLDKBG <- get_download(t8kyr.datasets[[1]]$DatasetID)
#'
#' taxa.no <- nrow(GOLDKBG$taxon.list)
#'
#' #  Of the 60 taxa in the record, plot the pollen curve for Abies over time:
#'
#' ##pollen <- GOLDKBG$taxon.list$VariableElement == 'pollen'
#' pol.curve <- data.frame(age = GOLDKBG$sample.meta$Age,
#'                         Abies = GOLDKBG$counts[,'Abies'] / rowSums(GOLDKBG$counts, na.rm = TRUE))
#' plot(Abies * 100 ~ age, data = pol.curve, type='b',
#'      ylab = '% Abies', xlab='Calibrated Years BP', pch=19)
#'
#' }
#' @references
#' Neotoma Project Website: http://www.neotomadb.org
#' API Reference:  http://api.neotomadb.org/doc/resources/contacts
#' @keywords Neotoma Palaeoecology API
#' @export
get_download <- function(datasetid, verbose = TRUE){

    ## Updated the processing here. There is no need to be fiddling with
    ## call. Use missing() to check for presence of argument
    ## and then process as per usual
    base.uri <- 'http://api.neotomadb.org/v1/data/downloads'

    if(missing(datasetid)) {
        stop(paste(sQuote("datasetid"), "must be provided."))
    } else {
        if(!is.numeric(datasetid))
            stop('datasetid must be numeric.')
    }

    get.sample <- function(x){
      ## query Neotoma for data set
      aa <- try(fromJSON(paste0(base.uri, '/', x), nullValue = NA))
  
      ## Might as well check here for error and bail
      if(inherits(aa, "try-error"))
          return(aa)
  
      ## if no error continue processing
      if (isTRUE(all.equal(aa[[1]], 0))) {
          stop(paste('Server returned an error message:\n', aa[[2]]),
               call.=FALSE)
      }
  
      if (isTRUE(all.equal(aa[[1]], 1))) {
          aa <- aa[[2]]
        
          if(verbose) {
              writeLines(strwrap(paste("API call was successful. Returned record for ",
                                       aa[[1]]$Site$SiteName)))
          }
  
          ##  Here the goal is to reduce this list of lists to as
          ##  simple a set of matrices as possible.
          nams <- names(aa[[1]])
          aa1 <- aa[[1]]
          if ('Samples' %in% nams) {
              ## data set meta data
              meta.data <- list(
                  dataset = data.frame(dataset.id = aa1$DatasetID,
                                       dataset.name = aa1$DatasetName,
                                       collection.type = aa1$CollUnitType,
                                       collection.handle = aa1$CollUnitHandle,
                                       dataset.type =  aa1$DatasetType, stringsAsFactors = FALSE),
                  site.data = as.data.frame(aa1$Site[c('SiteID', 'SiteName',
                                           'Altitude','LatitudeNorth',
                                           'LongitudeWest','LatitudeSouth',
                                           'LongitudeEast','SiteDescription',
                                           'SiteNotes')], stringsAsFactors = FALSE),
                  pi.data = aa1$DatasetPIs)
  
              ## copy to make indexing below easier?
              samples <- aa1$Samples
  
              ## sample meta data - no with() in functions
              sample.meta <- do.call(rbind.data.frame,
                                     lapply(samples, `[`,
                                            c("AnalysisUnitDepth",
                                              "AnalysisUnitThickness",
                                              "SampleID", "AnalysisUnitName"
                                              )))
  
              ## sample age data
              ages <- do.call(rbind.data.frame,
                              lapply(lapply(samples, `[[`, "SampleAges"),
                                     `[[`, 1))
  
              ## sample names - can be NULL hence replace with NA if so
              tmp <- sapply(sample.names <-
                            lapply(samples, `[[`, "SampleUnitName"), is.null)
              sample.names[tmp] <- NA
  
              ## stick all that together, setting names, & reordering cols
              sample.meta <- cbind.data.frame(sample.meta, ages,
                                              unlist(sample.names))
              names(sample.meta) <- c("depths", "thickness", "IDs", "unit.name",
                                      names(ages), "sample.name")
              sample.meta <- sample.meta[, c(1:2, 5:10, 3, 11, 4)]
  
              ## sample data/counts
              ##  1) extract each SampleData component & then rbind. Gives a
              ##     list of data frames
              sample.data <- lapply(lapply(samples, `[[`, "SampleData"),
                                    function(x) do.call(rbind.data.frame, x))
              ##  2) How many counts/species in each data frame?
              nsamp <- sapply(sample.data, nrow)
              ##  3) bind each data frame - result is a data frame in long format
              sample.data <- do.call(rbind, sample.data)
              ##  4) add a Sample column that is the ID from smaple.meta
              sample.data$Sample <- rep(sample.meta$IDs, times = nsamp)
  
              #  We're going to isolate the count data and clean it up:
              take <- !(sample.data$TaxaGroup == "Laboratory analyses" | sample.data$TaxaGroup == "Charcoal")
              
              count.data <- sample.data[take, ]
              
              ## Ensure duplicate taxa are renamed (if variable context is different)
              count.data$TaxonName <- as.character(count.data$TaxonName)
              var.context <- !is.na(count.data$VariableContext)
              count.data$TaxonName[var.context] <- paste(count.data$TaxonName, count.data$VariableContext, sep='.')[var.context]
              
              ## data frame of unique taxon info
              taxon.list <- sample.data[!duplicated(sample.data$TaxonName), 1:5]
  
              mod.dups <- duplicated(count.data[,c(1,7)])
              
              if(sum(mod.dups) > 0){
                tax.dups <- unique(count.data$TaxonName[duplicated(count.data[,c(1,7)])])
                if(length(tax.dups) == 1){
                  message <- paste('\nModifiers seem absent from the taxon ', tax.dups, '. \nget_download will sum at depths with multiple entries to resolve the problem.', sep = '')
                }
                if(length(tax.dups) > 1){
                  tax.dups.col <- paste(tax.dups, collapse = ', ')
                  message <- paste('\nModifiers seem absent from the taxons ', tax.dups.col, '. \nget_download will sum at depths with multiple entries to resolve the problem.', sep = '')
                }
                warning(immediate. = TRUE, message, call. = FALSE)
              }
              
              ## reshape long sample.data into a sample by taxon data frame
              ## take here *only* counts - but needs work FIXME
              
              counts <- dcast(count.data,
                              formula = Sample ~ TaxonName, value.var = "Value", fun.aggregate = sum, na.rm=TRUE)
              ## add Sample col as the rownames
              rownames(counts) <- counts$Sample
              ## remove the Sample col, but robustly
              counts <- counts[, -which(names(counts) == "Sample")]
  
              ## Pull out the lab data
              
              take <- sample.data$TaxaGroup == "Laboratory analyses" | sample.data$TaxaGroup == "Charcoal"
              lab.data <- sample.data[take, ]
              
              if(nrow(lab.data) > 0) {
                  lab.data$LabNameUnits <- paste0(lab.data$TaxonName, " (",
                                                  lab.data$VariableElement, ": ",
                                                  lab.data$VariableUnits, ")")
                  
                  
                  lab.data <- dcast(lab.data, formula = Sample ~ LabNameUnits,
                                    value.var = "Value")
              } else {
                  lab.data <- NA
              }
  
              ## stick all this together
              aa <- list(metadata = meta.data,
                         sample.meta = sample.meta,
                         taxon.list = taxon.list,
                         counts = counts,
                         lab.data = lab.data)
          }
      }
      aa
    }
    

    if(length(datasetid) == 1) aa <- get.sample(datasetid)
    else                       aa <- lapply(datasetid, get.sample)
    
    aa
    
}
