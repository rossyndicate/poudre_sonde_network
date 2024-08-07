relevant_sonde_selector <- function(site_arg) {
  if (site_arg == "joei") {
    plot_filter <- c("cbri")
  }
  if (site_arg == "cbri") {
    plot_filter <- c("joei", "chd")
  }
  if (site_arg == "chd") {
    plot_filter <- c("cbri", "pfal")
  }
  if (site_arg == "pfal") {
    plot_filter <- c("chd", "sfm")
  }
  if (site_arg == "sfm") {
    plot_filter <- c("pfal", "pbd")
  }
  if (site_arg == "penn") {
    plot_filter <- c("sfm")
  }
  if (site_arg == "lbea") {
    plot_filter <- c("sfm")
  }
  if (site_arg == "pbd") {
    plot_filter <- c("sfm", "tamasag")
  }
  if (site_arg == "tamasag") {
    plot_filter <- c("pbd", "legacy")
  }
  if (site_arg == "legacy") {
    plot_filter <- c("tamasag", "lincoln")
  }
  if (site_arg == "lincoln") {
    plot_filter <- c("legacy", "timberline", "timberline virridy")
  }
  if (site_arg == "timberline") {
    plot_filter <- c("lincoln", "timberline virridy", "prospect")
  }
  if (site_arg == "timberline virridy") {
    plot_filter <- c("lincoln", "timberline", "prospect", "prospect virridy")
  }
  if (site_arg == "springcreek") {
    plot_filter <- c("prospect virridy", "prospect")
  }
  if (site_arg == "prospect") {
    plot_filter <- c("timberline", "prospect virridy", "boxelder")
  }
  if (site_arg == "prospect virridy") {
    plot_filter <- c("timberline virridy", "prospect", "boxelder")
  }
  if (site_arg == "boxelder") {
    plot_filter <- c("prospect",
                     "prospect virridy",
                     "archery",
                     "archery virridy")
  }
  if (site_arg == "boxcreek") {
    plot_filter <- c("archery", "archery virridy")
  }
  if (site_arg == "archery") {
    plot_filter <- c("boxelder", "archery virridy", "river bluffs")
  }
  if (site_arg == "archery virridy") {
    plot_filter <- c("boxelder", "archery", "river bluffs")
  }
  if (site_arg == "river bluffs") {
    plot_filter <- c("archery", "archery virridy")
  }
  return(plot_filter)
}
