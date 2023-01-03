theme_opts <- list(theme(axis.ticks.length =unit(-0.1, "cm"),  
                         axis.text.x = element_text(margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm")), 
                         axis.text.y = element_text(margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"))))

period_cols <-  c("#97B8C2", "#BF9A77", "#D35C37")  
var_cols <- c("#336B87", "#90AFC5", "#BF9A77", "#D13525", "#F2C057")
alpha_cols <- melt(data.table(col2rgb(var_cols[1:3])))$value
col1 <- rgb(alpha_cols[1], alpha_cols[2], alpha_cols[3], 210, maxColorValue = 255)
col2 <- rgb(alpha_cols[4], alpha_cols[5], alpha_cols[6], 150, maxColorValue = 255)
col3 <- rgb(alpha_cols[7], alpha_cols[8], alpha_cols[9], 150, maxColorValue = 255)
var_cols_alpha <- c(col1, col2, col3)

colset_mid <- c( "#4D648D", "#337BAE", "#97B8C2",  "#739F3D", "#ACBD78",  
                 "#F4CC70", "#EBB582",  "#BF9A77",
                 "#E38B75", "#CE5A57",  "#D24136", "#785A46" )

colset_mid_qual <- colset_mid[c(11, 2, 4, 6,  1, 8, 10, 5, 7, 3, 9, 12)]
palette_mid <- colorRampPalette(colset_mid)
palette_mid_qual <- colorRampPalette(colset_mid_qual)
