#' @title Generate site metaparameter data from the HydroVu API.
#'
#' @description
#' A function that generates a site metaparameter dataframe from the HydroVu API.
#' A metaparameter is a parameter that is used to generate flags for parameters
#' other than itself.
#'
#' @param api_data A dataframe with the munged API data.
#' 
#' @param require An upstream dependency (i.e., alternative to `{necessary object}`).
#'
#' @return A dataframe with site metaparameter data that will be joined to the other
#' site-parameter data.
#'
#' @examples
#' generate_site_metaparam(site_arg = "archery", metaparameter_arg = "Battery Level", api_data = incoming_data_collated_csvs)
#' 
#' @seealso [summarize_site_param()]

generate_site_metaparam <- function(api_data, require = NULL) {

    sites <- unique(api_data$site)
    metaparameters <- c("Temperature", "Battery Level", "Baro", "External Voltage")

    df_list <- list()
    for (i in sites) {
        metaparameter_data <- api_data %>%
            data.table::data.table() %>%
            dplyr::select(DT_join, site, parameter, value) %>%
            dplyr::filter(site == i & (parameter %in% metaparameters)) %>%
            dplyr::select(-site) %>%
            tidyr::pivot_wider(names_from = parameter, values_from = value)
        df_list[[i]] <- metaparameter_data
    }
    return(df_list)
}
