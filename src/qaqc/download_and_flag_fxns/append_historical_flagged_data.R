# function to append the historical flagged data to the incoming data
append_historical_flagged_data <- function(site_arg, parameter_arg, incoming_data, flagged_data) {
    # generate the name of the index
    index_name <- paste0(site_arg, "-", parameter_arg)
    
    # pull in the result for all data summary list for a specific site and parameter
    incoming_data_chunk <- incoming_data[[index_name]]

    # pull in the correspoding subset of the historical flagged data
    flagged_data_chunk <- flagged_data[[index_name]] %>%
        # filter for the last 6 hours of data
        filter(DT_round > (min(incoming_data_chunk$DT_round) - hours(6)))

    # add a replace column to the flagged data


    # append them
    appended_data <- bind_rows(flagged_data_chunk, incoming_data_chunk)

    return(appended_data)
}
