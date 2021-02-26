check_hla_locus <- function(hla_locus) {
    if (is.na(hla_locus)) {TRUE} else {
    # classical HLA loci
    valid_hla_loci <- c("A","B","C",
                        "DPA1", "DPB1", "DQA1", "DQB1", "DRB1")
    if (hla_locus %in% valid_hla_loci) {
        TRUE
    } else {
        stop("The hla_locus ", hla_locus, " does not belong to the list of valid HLA loci: ",
                 paste(valid_hla_loci, collapse=",")) 
    }}
}

check_hla_selection <- function(hla_selection) {
    if (is.na(hla_selection)) {TRUE} else {
    # we do not check whether the indicated allele is in the database
    # we just check general formatting
    # hla_selection can be a list of alleles
    pattern_match <- grepl("^(\\w*|\\w*\\d)\\*\\d\\d", hla_selection)
    if (all(pattern_match)) {
        TRUE
    } else {
        wrong_allele <- hla_selection[!pattern_match]
        stop("The following alleles do not seem to be formated correctly (expected format e.g. A*01:01): ",
             paste(wrong_allele, collapse=","))
    }}
}

check_population <- function(hla_population) {
    if (is.na(hla_population)) {TRUE} else {
    # We just check formatting
    # we do not check whether this population actually exists
    # format seems to be 4 numbers, but it has to be formatted as numeric
    pattern_match <- sapply(hla_population, is.numeric)
    if (all(pattern_match)) {
        TRUE
    } else {
        wrong_pop <- hla_population[!pattern_match]
        stop("The following population IDs do not seem to be formated correctly (expected format e.g. 1920): ",
             paste(as.character(wrong_pop), collapse=","))
    }}
}

check_sample_size <- function(hla_sample_size_pattern, hla_sample_size) {
    if (is.na(hla_sample_size_pattern) & is.na(hla_sample_size)) {TRUE} else {
    valid_pattern <- c("bigger_than",
                        "equal", "less_than",
                       "less_equal_than", "bigger_equal_than", "different")
    
    if (hla_sample_size_pattern %in% valid_pattern) {
        if (is.numeric(hla_sample_size)) {TRUE}
        else { stop("hla sample size must be numeric") }
    } else {
        stop("The hla_sample_size_pattern must be one of these options: ",
             paste(valid_pattern, collapse=","))
    }}
}

check_standard <- function(standard) {
    # all, silver or gold standard of population data quality
    valid_standard <- c("a","s","g")
    if(standard %in% valid_standard) {
        TRUE
    } else { stop("standard must be one of the following: ",
                   paste(valid_standard, collapse=",") )}
}


verify_parameters <- function(hla_locus,
                              # for now we don't check the 
                              hla_selection,
                              hla_population,
                              hla_sample_size_pattern,
                              hla_sample_size,
                              standard) {
    
    # do the checks only if variables are not na
    if (all(is.na(c(hla_locus, hla_selection, hla_population,
          hla_sample_size_pattern,
          hla_sample_size)))) {
        stop("You need to at least specify one of the paramters hla_locus, hla_selection, hla_population, hla_sample_size_pattern, hla_sample_size.")
    } else {
        # do the checks only if variables are not na
        all(check_standard(standard),
            check_sample_size(hla_sample_size_pattern, hla_sample_size),
            check_population(hla_population),
            check_hla_selection(hla_selection),
            check_hla_locus(hla_locus))
    }
}