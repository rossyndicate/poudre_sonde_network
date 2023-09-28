# This can be used to develop site specific flags for specific parameters
# (later)

# Use grep to find elements with "archery" in their title
archery_objects <- all_data_summary_list[grep("archery", names(all_data_summary_list), ignore.case = TRUE)]

# Resulting list of objects with "archery" in the title
archery_objects_df <- bind_rows(archery_objects)
