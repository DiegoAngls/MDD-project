# Network analysis  -------------------------------------------------------
# Diego Angeles-Valdez

setwd("~/Documents/Projects/Depression/Depression")

# Packages
pacman::p_load(tidyverse, qgraph, bootnet, mgm,memisc,foreign,summarytools)

datasetF1 <- data.frame(as.data.set(spss.system.file("filesForDiego/Baseline_questionnaires_AM.sav"))) %>% 
  dplyr::select(NPno,Group,  contains("bIDS"), -c("bIDS11A","bIDS11B","bIDS12A","bIDS12B", "bIDSTotal", "bIDS9B","bIDS9C")) 

datasetF2 <- data.frame(as.data.set(spss.system.file("filesForDiego/1_FollowUp_questionnaires_AM.sav"))) %>% 
  dplyr::select(Group,Condition,  contains("f1IDS"),-c("f1IDS11A","f1IDS11B","f1IDS12A","f1IDS12B", "f1IDSTotal", "f1IDS9B","f1IDS9C") ) %>% 
  filter(Group =="Remitted patient group")


dat_mat <- datasetF2 %>%
  dplyr::select(-c( "Group")) %>% 
  mutate( Condition = case_when(Condition == "Control" ~ 0,
                                Condition == "Treatment" ~ 1)) %>% 
  mutate(
    across(
      .cols = everything(), 
      .fns = function(col) {
        ifelse(is.na(col), median(col, na.rm = T), col)
      }
    )
  ) %>% 
  as.matrix()

p <- ncol(dat_mat)
set.seed(911)



fit_mgm <- estimateNetwork(
  data =  dat_mat, 
  default = "mgm", 
  type = c("c" , rep("g", p - 1)),
  level = c(2, rep(1, p - 1)),
  criterion = "EBIC"
)


plot(fit_mgm)

predictability <- predict(fit_mgm$results, data = dat_mat)
errors <- predictability$errors$R2
errors[1] <- predictability$errors$CC[1]


# Figure 6 in paper 
plot(fit_mgm, 
     layout = "spring",
     pie = errors, 
     label.font = 2,
     pieColor = c("#CBD5E8", rep("#7570B3", 28)),
     shape = c("square", rep("circle", 28)),
     color = c("#7570B3", rep("#CBD5E8", 28)),
     label.color = c("white", rep("black", 28)),
     legend.cex = 0.5,
     nodeNames = c("PCT", 
                   "Item 1", 
                   "Item 2", 
                   "Item 3", 
                   "Item 4", 
                   "Item 5", 
                   "Item 6", 
                   "Item 7", 
                   "Item 8", 
                   "Item 9", 
                   "Item 10", 
                   "Item 11", 
                   "Item 12", 
                   "Item 13", 
                   "Item 14", 
                   "Item 15", 
                   "Item 16", 
                   "Item 17", 
                   "Item 18", 
                   "Item 19", 
                   "Item 20", 
                   "Item 21", 
                   "Item 22", 
                   "Item 23", 
                   "Item 24", 
                   "Item 25", 
                   "Item 26", 
                   "Item 27", 
                   "Item 28"),
     filetype = "png", 
     filename = "mgm_net" )


# Metrics and network stability

# Centrality 
measures <- c("Strength", "Closeness", "Betweenness")
centralityPlot(fit_mgm, 
               include = measures, 
               labels = colnames(dat_mat))


color <- RColorBrewer::brewer.pal(3, name = "Set1")[2]
cent_plot <- centralityPlot(fit_mgm,  
                            include = measures, 
                            labels = colnames(dat_mat), scale = "z-scores")

cent_plot$layers[[1]] <- geom_path(col = color)
cent_plot$layers[[2]] <- geom_point(col = color)

fig_cent <- 
  cent_plot +
  theme_bw() +
  theme(strip.text = element_text(face = "bold"), 
        panel.grid.major = element_blank())

fig_cent

#ggsave(fig_cent, filename = "centrality_fig.png", device = "png", dpi = 400, 
#       units = "cm", height = 10, width = 15)


# Bootstrapping
boot_mgm <- bootnet(fit_mgm, nBoots = 2500, nCores = 4)
plot(boot_mgm, labels = T, order = "sample")

summary(boot_mgm) %>% 
  filter(node1 == "Treat", CIupper < 0)

summary(boot_mgm) %>% 
  #filter(node1 == "Treat") %>% 
  select(type, node1, node2, mean, sd, CIlower, CIupper) %>% 
  mutate(across(.cols = where(is.numeric), .fns = round, 2)) %>% 
  filter(CIlower > 0) %>% View()

summary(boot_mgm) %>% 
  #filter(node1 == "Treat") %>% 
  select(type, node1, node2, mean, sd, CIlower, CIupper) %>% 
  mutate(across(.cols = where(is.numeric), .fns = round, 2)) %>% 
  writexl::write_xlsx("table_edge_boot.xlsx")

boot_mgm_case <- bootnet(fit_mgm, 
                         nBoots = 2500,
                         nCores = 4, 
                         type = "case",
                         statistics = c("edge", "strength", 
                                        "closeness", "betweenness"))

plot(boot_mgm_case, statistics = c("strength", 
                                   "closeness", "betweenness"))
corStability(boot_mgm_case)


differenceTest(boot_mgm, 8, 4, "strength")
differenceTest(boot_mgm, 5, 4, "strength")

differenceTest(boot_mgm, 5, 7, "strength")
differenceTest(boot_mgm, 5, 13, "strength")
differenceTest(boot_mgm, 4, 13, "strength")

plot(boot_mgm, "edge", plot = "difference", 
     onlyNonZero = TRUE, order = "sample")

