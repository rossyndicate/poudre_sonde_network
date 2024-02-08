

basic_plot <- function(){
  max_1 <- max(wq_tl[[parameters[1]]], na.rm = T)

 param_1 <- ggplot(wq_tl, aes(x = DT_round, y = .data[[parameters[1]]], ymin = 0, ymax = .data[[parameters[1]]]))+
   geom_ribbon(color = "blue", fill = "blue" )+
   theme_bw()+
   labs(x = "Date", y = parameters[1])

 param_2 <- ggplot(wq_tl, aes(x = DT_round, y = .data[[parameters[2]]]))+
   geom_path(color = "red")+
   theme_bw()+
   labs(x = "Date", y = parameters[2])

 ggarrange(param_1, param_2,ncol = 2, nrow = 1)

}
