# global obs
site = tar_read(site_names)[1]
start_dt = tar_read(start_dt)[1]
end_dt = current_time
api_token = tar_read(hv_token)
dump_dir = "scratch/scratch_data/"

current_time = Sys.time()

test <- list(
        site = tar_read(site_names),
        start_time = tar_read(start_dt),
        end_time = list(current_time),
        token = list(tar_read(hv_token)),
        dump_dir = list("scratch/scratch_data/")
        )

test[[3]][[1]]
