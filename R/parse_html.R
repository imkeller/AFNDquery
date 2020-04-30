# read html file from the web
# special function that will be formated according to BC recommendations
# to handle web specific issues like reachability, response time...

#' getURL
#'
#' @param URL   url that will be read
#' @param N.TRIES   number, how often should the function try to read the html
#'
#' @return returns output of read_html(), a html character string.
#' @import rvest

getURL <- function(URL, N.TRIES=1L) {
    N.TRIES <- as.integer(N.TRIES)
    stopifnot(length(N.TRIES) == 1L, !is.na(N.TRIES))
    while (N.TRIES > 0L) {
        result <- tryCatch(xml2::read_html(URL), error=identity)
        if (!inherits(result, "error"))
            break
        N.TRIES <- N.TRIES - 1L
    }
    if (N.TRIES == 0L) {
        stop("'getURL()' failed:",
             "\n  URL: ", URL,
             "\n  error: ", conditionMessage(result))
    }
    result
}

read_url <- function(url) {
    getURL(url)
}

# FUNCTIONS TO READ ALLELE FREQUENCY DATA

get_nb_pages <- function(page_tbl) {
    str_extract(page_tbl$X6, "\\d*$")
}

parse_allele_freq_html <- function(allele_freq_table) {
    colnames(allele_freq_table) <- c("line", "allele", "", "population", "perc_individuals_with_allele",
                                     "allele_frequency", "", "sample_size")
    allele_freq_table[c("allele","population","allele_frequency","sample_size")]
}

#' extract_population_id
#'
#' @param data html character string to parse population ids from
#'
#' @return list of population ids present in data
#' @import stringr
extract_population_id <- function(data) {
    pop_hrefs <- html_attr(html_nodes(html_nodes(data, "td:nth-child(4)"), "a"), "href")
    # on page > 1 there is another href on top and below
    filtered_pop_hrefs <- pop_hrefs[grep("pop6001", pop_hrefs)]
    str_extract(filtered_pop_hrefs , "\\d\\d\\d\\d$")
}

read_complete_allele_freq_table <- function(url) {
    html_input <- read_url(url)

    rvest_tables <- html_table(html_input, fill = TRUE)

    # get the number of pages that need to be read
    page_nb <- get_nb_pages(rvest_tables[[4]])

    output_table <- data.frame()
    # for 1-n pages query the allele frequencies
    for (page_id in 1:page_nb) {
        url_tmp <- paste0(url, "&page=", as.character(page_id), collapse = "")
        html_input_tmp <- read_url(url_tmp)
        rvest_tables_tmp <- html_table(html_input_tmp, fill = TRUE)
        # add the population ID
        pop_ids <- extract_population_id(html_input_tmp)
        freq_table <- parse_allele_freq_html(rvest_tables_tmp[[5]])
        freq_table$population_id <- pop_ids

        output_table <- rbind(output_table, freq_table)
    }
    # return the final output table
    output_table
}

# FUNCTIONS TO READ POPULATION DETAILS

extract_population_name <- function(data) {
    html_text(
        html_nodes(data, xpath = "//td//h1"),
        trim = TRUE)
}

extract_population_info <- function(data) {
    raw_pop_data <- html_table(
        html_nodes(data, ".table04:nth-child(4)"),
        fill = TRUE)[[1]]
    # reformat in a key value manner
    key_names <- str_replace(
        raw_pop_data$X2[3:nrow(raw_pop_data)], ":",
        "")
    values <- raw_pop_data$X3[3:nrow(raw_pop_data)]

    names(values) <- key_names
    values
}

extract_sample_info <- function(data) {
    raw_sample_data <- html_table(
        html_nodes(data, ".table04:nth-child(6)"),
        fill = TRUE)[[1]][1:8,]
    # reformat in a key value manner
    key_names <- str_replace(
        raw_sample_data$X2[3:nrow(raw_sample_data)], ":",
        "")
    values <- raw_sample_data$X3[3:nrow(raw_sample_data)]

    names(values) <- key_names
    values
}

read_population_detail <- function(url, population_id) {
    data <- read_url(url)

    # Population
    population_name <- extract_population_name(data)

    # Population data
    pop_data <- extract_population_info(data)

    # sample data
    sample_data <- extract_sample_info(data)

    # Assemble into a data frame
    # one row with all info in columns
    data.frame(population_id = population_id,
               t(pop_data),
               t(sample_data), stringsAsFactors = FALSE)
}
