
photo_plotter <- function(index = 200, output_folder){

  param_1_label <- paste0(parameters[1], " (", param_unit[1],")")
  param_2_label <- paste0(parameters[2], " (", param_unit[2],")")

# get the adjustment, breaks etc so axes look nice
  param_1_max <- max(wq_tl[[parameters[1]]], na.rm = T)
  param_1_max_int <- as.integer(max(wq_tl[[parameters[1]]], na.rm = T))+1

  adjustment <- max(wq_tl[[parameters[2]]], na.rm = T) / param_1_max

  brk <- ifelse(param_1_max_int < 6, 1,2)

# Bounds for the y-axis

  lower_bound <- 0
  upper_bound <-  max(wq_tl[[parameters[2]]], na.rm = T)


  #This is the index of the image for the background for the individual photo
  simul = wq_tl[index,]
  # this is all the data before the image (ie previous photos from the timelapse)
  upto = wq_tl[1:index,]
  #read the image for the background
  photo_bg <- readJPEG(simul$filename)
  #create an individual image
  back <- ggplot() +
    #plot the photo
    annotation_custom(rasterGrob(photo_bg,
                                 width = unit(1,"npc"),
                                 height = unit(1,"npc")),
                      -Inf, Inf, -Inf, Inf)

  #plot the data for this image (includes all the preivous data)
  inset <- ggplot() +
    geom_ribbon(data = upto, aes(x = DT_round,
#to do: need to figure out adjustment factors for each parameter to plot correctly
                                 y = .data[[parameters[1]]] *adjustment,
                                 ymin = 0,
                                 ymax = .data[[parameters[1]]] * adjustment),
                color = "white",
                fill = "white",
                #linetype = "dash",
                alpha = 0.75)+
    geom_path(data = upto, aes(x = DT_round, y = .data[[parameters[2]]]),
              color = "#F34646", size=2) +
    geom_point(data = simul, aes(x = DT_round, y = .data[[parameters[2]]]),
               color = "#F34646")+
    # ylim(min(wq_tl[[parameters[2]]], na.rm = T),
    #      max(wq_tl[[parameters[2]]], na.rm = T))+
    xlim(min(wq_tl$DT_round, na.rm = T),
         max(wq_tl$DT_round, na.rm = T))+
    scale_y_continuous(name = param_2_label, limits = c(0, upper_bound),
                       sec.axis = sec_axis(trans = ~./adjustment ,
                                           name = param_1_label,
                                           breaks = seq(0,param_1_max_int,brk)))+
    dark_theme_light(base_size = 10) +
    theme(axis.title.y.right = element_text(color="white"),axis.text.y.right = element_text(color = "white"), axis.title.y.left = element_text(color="#F34646"), axis.text.y.left = element_text(color = "#F34646")) +
    theme(
      panel.background = element_rect(fill = "transparent"),
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.background = element_rect(fill = "transparent"),
      legend.box.background = element_rect(fill = "transparent"),
      panel.border = element_blank(),
      axis.line = element_line(color = 'gray80'),
    ) +
    ylab(param_2_label) +
    xlab('')

  gp1 <- back +
    inset_element(inset,
                  left = 0.16,
                  bottom = 0.15,
                  right = 0.9,
                  top = 0.6)
  #print(gp1)
  ggsave(paste0(folder_path, index, ".png"),
         width = 1920,
         height = 1080,
         units = "px")
  #dev.copy(gp1,paste0('data/timelapse_photos/vid_image/', max(upto$rowid), ".png"))
  #dev.off()
}

#test functions
#photo_plotter(2, "data/timelapse_photos/sjs_test/")
#map(1:nrow(wq_tl), ~photo_plotter(.x, "data/timelapse_photos/sjs_test/"))
