promoter_ergastf1dj = function(g) {grid.arrange(g, nrow=1,
                                    #top="My title",
                                    bottom = textGrob("Includes data from ergast.com. To learn how to create this chart: f1datajunkie.com",
                                                      gp = gpar(fontface=3, fontsize=9),
                                                      hjust=1, x=1))}

promoterf1dj= function(g) {grid.arrange(g, nrow=1,
                                    #top="My title",
                                    bottom = textGrob("To learn how to create this chart: f1datajunkie.com",
                                                      gp = gpar(fontface=3, fontsize=9),
                                                      hjust=1, x=1))}
promoter = function(g) {grid.arrange(g, nrow=1,
                                                #top="My title",
                                                bottom = textGrob("To learn how to create this chart: f1datajunkie.com",
                                                                  gp = gpar(fontface=3, fontsize=9),
                                                                  hjust=1, x=1))}
