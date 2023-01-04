theme_opts <- list(theme(axis.ticks.length =unit(-0.1, "cm"),  
                         axis.text.x = element_text(margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm")), 
                         axis.text.y = element_text(margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"))))

main_cols <-  c("#97B8C2", "#BF9A77", "#D35C37") 

colset_mid <- c( "#4D648D", "#337BAE", "#97B8C2",  "#739F3D", "#ACBD78",  
                 "#F4CC70", "#EBB582",  "#BF9A77",
                 "#E38B75", "#CE5A57",  "#D24136", "#785A46" )
colset_mid_qual <- colset_mid[c(11, 2, 4, 6,  1, 8, 10, 5, 7, 3, 9, 12)]

palette_mid <- colorRampPalette(colset_mid)
palette_mid_qual <- colorRampPalette(colset_mid_qual)
