# rmaMVModel <- function(yi,vi,measure,d,pred1=NULL,pred2=NULL) {
#
#   library(metafor)
#   library(psych)
#   library(jsonlite)
#   library(labelVector)
#
#   dat<-get(d)
#
#   pred1<-unlist(pred1)
#   pred2<-unlist(pred2)
#
#
#   mod_data <- reactive({
#     dots <- if (is.null(input$moderators)) {
#       NULL
#     } else {
#       sprintf("!is.na(%s)", input$moderators)
#     }
#     data() %>%
#       filter_(.dots = dots) %>%
#       combine_mods(categorical_mods())
#   })
#
#
#       mods <- paste("s_meanage", collapse = "+")
#       rma_formula <- as.formula(sprintf("%s ~ %s", "o_g_calc",mods()))
#
#         metafor::rma.mv(rma_formula, V =dat[,vi],
#                         random = ~ 1 | short_cite / same_infant_calc / unique_row,
#                         #Cluster by paper, then participant group, then add random effect for each effect size
#                         slab = make.unique(short_cite), data = mod_data())
#
#         metafor::rma(rma_formula, vi = mod_data()[[es_var()]],
#                      slab = make.unique(short_cite), data = mod_data(),
#                      method = ma_method)
#       }
#     }
#   })
#
#   no_mod_model <- reactive({
#     if (ma_method == "REML_mv") {
#       metafor::rma.mv(yi = data()[[es()]], V = data()[[es_var()]],
#                       #random = ~ 1 | data()[["short_cite"]],
#                       random = ~ 1 | data()[["short_cite"]] / data()[["same_infant_calc"]] / data()[["unique_row"]],
#                       slab = make.unique(data()[["short_cite"]]),
#                       method = "REML")
#     } else {
#       metafor::rma(yi = data()[[es()]], vi = data()[[es_var()]],
#                    slab = make.unique(data()[["short_cite"]]),
#                    method = ma_method)
#
#     }
#   })
#
# }
