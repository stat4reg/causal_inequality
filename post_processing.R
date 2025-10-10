
n <- 3000
Path <- paste('res/',n,'NewEQflexNM/', sep = '')

paste(Path, 'result00111.csv', sep = '')

result111= read.csv(paste(Path, 'result11100.csv', sep = ''))[1:1000,]
result101= read.csv(paste(Path, 'result10100.csv', sep = ''))[1:1000,]
result011= read.csv(paste(Path, 'result01100.csv', sep = ''))[1:1000,]
result000f= read.csv(paste(Path, 'result00011.csv', sep = ''))[1:1000,]
result001= read.csv(paste(Path, 'result00111.csv', sep = ''))[1:1000,]


#Contrast

pl_y_c_EF = data.frame(name='plugh-in' ,bias_E01 = (mean(result001$cov_E0)-mean(result001$cov_E1)+mean(result001$real_E1) - mean(result001$real_E0)), MC.sd_E01 = sd(result001$cov_E0 -result001$cov_E1), est.sd_E01 = mean(sqrt(result001$var_01)), cove_E01= mean(result001$coverage_COn_E10),
                       bias_E02 = (mean(result001$cov_E0)-mean(result001$cov_E2) + mean(result001$real_E2) - mean(result001$real_E0)), MC.sd_E02 = sd(result001$cov_E0 -result001$cov_E2), est.sd_E02 = mean(sqrt(result001$var_02)), cove_E02= mean(result001$coverage_COn_E20))
EIF_yT_EF_v1 = data.frame(name='EIF_EQ' ,bias_E01 = (mean(result001$est_EQ_E0)-mean(result001$est_EQ_E1) + mean(result001$real_E1) - mean(result001$real_E0)), MC.sd_E01 = sd(result001$est_EQ_E0 -result001$est_EQ_E1), est.sd_E01 = mean(sqrt(result001$var_01)), cove_E01= mean(result001$coverage_CO_EQ_E10),
                          bias_E02 = (mean(result001$est_EQ_E0)-mean(result001$est_EQ_E2) + mean(result001$real_E2) - mean(result001$real_E0)), MC.sd_E02 = sd(result001$est_EQ_E0 -result001$est_EQ_E2), est.sd_E02 = mean(sqrt(result001$var_02)), cove_E02= mean(result001$coverage_CO_EQ_E20))
EIF_yT_EF_v2 = data.frame(name='EIF_1S' ,bias_E01 = (mean(result001$est_1S_E0)-mean(result001$est_1S_E1) + mean(result001$real_E1) - mean(result001$real_E0)), MC.sd_E01 = sd(result001$est_1S_E0 -result001$est_1S_E1), est.sd_E01 = mean(sqrt(result001$var_01)), cove_E01= mean(result001$coverage_CO_1S_E10),
                          bias_E02 = (mean(result001$est_1S_E0)-mean(result001$est_1S_E2) + mean(result001$real_E2) - mean(result001$real_E0)), MC.sd_E02 = sd(result001$est_1S_E0 -result001$est_1S_E2), est.sd_E02 = mean(sqrt(result001$var_02)), cove_E02= mean(result001$coverage_CO_1S_E20))
table_EF = rbind(pl_y_c_EF,EIF_yT_EF_v1,EIF_yT_EF_v2 )


pl_y_c_yF2 = data.frame(name='plugh-in' ,bias_E01 = (mean(result000f$cov_E0)-mean(result000f$cov_E1)+mean(result000f$real_E1) - mean(result000f$real_E0)), MC.sd_E01 = sd(result000f$cov_E0 -result000f$cov_E1), est.sd_E01 = mean(sqrt(result000f$var_01)), cove_E01= mean(result000f$coverage_COn_E10),
                        bias_E02 = (mean(result000f$cov_E0)-mean(result000f$cov_E2) + mean(result000f$real_E2) - mean(result000f$real_E0)), MC.sd_E02 = sd(result000f$cov_E0 -result000f$cov_E2), est.sd_E02 = mean(sqrt(result000f$var_02)), cove_E02= mean(result000f$coverage_COn_E20))
EIF_yF_EF_v1 = data.frame(name='EIF_EQ' ,bias_E01 = (mean(result000f$est_EQ_E0)-mean(result000f$est_EQ_E1) + mean(result000f$real_E1) - mean(result000f$real_E0)), MC.sd_E01 = sd(result000f$est_EQ_E0 -result000f$est_EQ_E1), est.sd_E01 = mean(sqrt(result000f$var_01)), cove_E01= mean(result000f$coverage_CO_EQ_E10),
                          bias_E02 = (mean(result000f$est_EQ_E0)-mean(result000f$est_EQ_E2) + mean(result000f$real_E2) - mean(result000f$real_E0)), MC.sd_E02 = sd(result000f$est_EQ_E0 -result000f$est_EQ_E2), est.sd_E02 = mean(sqrt(result000f$var_02)), cove_E02= mean(result000f$coverage_CO_EQ_E20))
EIF_yF_EF_v2 = data.frame(name='EIF_1S' ,bias_E01 = (mean(result000f$est_1S_E0)-mean(result000f$est_1S_E1) + mean(result000f$real_E1) - mean(result000f$real_E0)), MC.sd_E01 = sd(result000f$est_1S_E0 -result000f$est_1S_E1), est.sd_E01 = mean(sqrt(result000f$var_01)), cove_E01= mean(result000f$coverage_CO_1S_E10),
                          bias_E02 = (mean(result000f$est_1S_E0)-mean(result000f$est_1S_E2) + mean(result000f$real_E2) - mean(result000f$real_E0)), MC.sd_E02 = sd(result000f$est_1S_E0 -result000f$est_1S_E2), est.sd_E02 = mean(sqrt(result000f$var_02)), cove_E02= mean(result000f$coverage_CO_1S_E20))
table_yF_EF = rbind(pl_y_c_yF2,EIF_yF_EF_v1,EIF_yF_EF_v2 )
tabler100F = rbind(table_EF,table_yF_EF)



pl_y_c = data.frame(name='plugh-in' ,bias_E01 = (mean(result111$cov_E0)-mean(result111$cov_E1)+mean(result111$real_E1) - mean(result111$real_E0)), MC.sd_E01 = sd(result111$cov_E0 -result111$cov_E1), est.sd_E01 = mean(sqrt(result111$var_01)), cove_E01= mean(result111$coverage_COn_E10),
                    bias_E02 = (mean(result111$cov_E0)-mean(result111$cov_E2) + mean(result111$real_E2) - mean(result111$real_E0)), MC.sd_E02 = sd(result111$cov_E0 -result111$cov_E2), est.sd_E02 = mean(sqrt(result111$var_02)), cove_E02= mean(result111$coverage_COn_E20))
EIF_yT_ET_v1 = data.frame(name='EIF_EQ' ,bias_E01 = (mean(result111$est_EQ_E0)-mean(result111$est_EQ_E1) + mean(result111$real_E1) - mean(result111$real_E0)), MC.sd_E01 = sd(result111$est_EQ_E0 -result111$est_EQ_E1), est.sd_E01 = mean(sqrt(result111$var_01)), cove_E01= mean(result111$coverage_CO_EQ_E10),
                          bias_E02 = (mean(result111$est_EQ_E0)-mean(result111$est_EQ_E2) + mean(result111$real_E2) - mean(result111$real_E0)), MC.sd_E02 = sd(result111$est_EQ_E0 -result111$est_EQ_E2), est.sd_E02 = mean(sqrt(result111$var_02)), cove_E02= mean(result111$coverage_CO_EQ_E20))
EIF_yT_ET_v2 = data.frame(name='EIF_1S' ,bias_E01 = (mean(result111$est_1S_E0)-mean(result111$est_1S_E1) + mean(result111$real_E1) - mean(result111$real_E0)), MC.sd_E01 = sd(result111$est_1S_E0 -result111$est_1S_E1), est.sd_E01 = mean(sqrt(result111$var_01)), cove_E01= mean(result111$coverage_CO_1S_E10),
                          bias_E02 = (mean(result111$est_1S_E0)-mean(result111$est_1S_E2) + mean(result111$real_E2) - mean(result111$real_E0)), MC.sd_E02 = sd(result111$est_1S_E0 -result111$est_1S_E2), est.sd_E02 = mean(sqrt(result111$var_02)), cove_E02= mean(result111$coverage_CO_1S_E20))
table = rbind(pl_y_c,EIF_yT_ET_v1,EIF_yT_ET_v2 )


pl_y_c_EF = data.frame(name='plugh-in' ,bias_E01 = (mean(result101$cov_E0)-mean(result101$cov_E1)+mean(result101$real_E1) - mean(result101$real_E0)), MC.sd_E01 = sd(result101$cov_E0 -result101$cov_E1), est.sd_E01 = mean(sqrt(result101$var_01)), cove_E01= mean(result101$coverage_COn_E10),
                       bias_E02 = (mean(result101$cov_E0)-mean(result101$cov_E2) + mean(result101$real_E2) - mean(result101$real_E0)), MC.sd_E02 = sd(result101$cov_E0 -result101$cov_E2), est.sd_E02 = mean(sqrt(result101$var_02)), cove_E02= mean(result101$coverage_COn_E20))
EIF_yT_EF_v1 = data.frame(name='EIF_EQ' ,bias_E01 = (mean(result101$est_EQ_E0)-mean(result101$est_EQ_E1) + mean(result101$real_E1) - mean(result101$real_E0)), MC.sd_E01 = sd(result101$est_EQ_E0 -result101$est_EQ_E1), est.sd_E01 = mean(sqrt(result101$var_01)), cove_E01= mean(result101$coverage_CO_EQ_E10),
                          bias_E02 = (mean(result101$est_EQ_E0)-mean(result101$est_EQ_E2) + mean(result101$real_E2) - mean(result101$real_E0)), MC.sd_E02 = sd(result101$est_EQ_E0 -result101$est_EQ_E2), est.sd_E02 = mean(sqrt(result101$var_02)), cove_E02= mean(result101$coverage_CO_EQ_E20))
EIF_yT_EF_v2 = data.frame(name='EIF_1S' ,bias_E01 = (mean(result101$est_1S_E0)-mean(result101$est_1S_E1) + mean(result101$real_E1) - mean(result101$real_E0)), MC.sd_E01 = sd(result101$est_1S_E0 -result101$est_1S_E1), est.sd_E01 = mean(sqrt(result101$var_01)), cove_E01= mean(result101$coverage_CO_1S_E10),
                          bias_E02 = (mean(result101$est_1S_E0)-mean(result101$est_1S_E2) + mean(result101$real_E2) - mean(result101$real_E0)), MC.sd_E02 = sd(result101$est_1S_E0 -result101$est_1S_E2), est.sd_E02 = mean(sqrt(result101$var_02)), cove_E02= mean(result101$coverage_CO_1S_E20))
table_EF = rbind(pl_y_c_EF,EIF_yT_EF_v1,EIF_yT_EF_v2 )


pl_y_c_yF = data.frame(name='plugh-in' ,bias_E01 = (mean(result011$cov_E0)-mean(result011$cov_E1)+mean(result011$real_E1) - mean(result011$real_E0)), MC.sd_E01 = sd(result011$cov_E0 -result011$cov_E1), est.sd_E01 = mean(sqrt(result011$var_01)), cove_E01= mean(result011$coverage_COn_E10),
                       bias_E02 = (mean(result011$cov_E0)-mean(result011$cov_E2) + mean(result011$real_E2) - mean(result011$real_E0)), MC.sd_E02 = sd(result011$cov_E0 -result011$cov_E2), est.sd_E02 = mean(sqrt(result011$var_02)), cove_E02= mean(result011$coverage_COn_E20))
EIF_yF_ET_v1 = data.frame(name='EIF_EQ' ,bias_E01 = (mean(result011$est_EQ_E0)-mean(result011$est_EQ_E1) + mean(result011$real_E1) - mean(result011$real_E0)), MC.sd_E01 = sd(result011$est_EQ_E0 -result011$est_EQ_E1), est.sd_E01 = mean(sqrt(result011$var_01)), cove_E01= mean(result011$coverage_CO_EQ_E10),
                          bias_E02 = (mean(result011$est_EQ_E0)-mean(result011$est_EQ_E2) + mean(result011$real_E2) - mean(result011$real_E0)), MC.sd_E02 = sd(result011$est_EQ_E0 -result011$est_EQ_E2), est.sd_E02 = mean(sqrt(result011$var_02)), cove_E02= mean(result011$coverage_CO_EQ_E20))
EIF_yF_ET_v2 = data.frame(name='EIF_1S' ,bias_E01 = (mean(result011$est_1S_E0)-mean(result011$est_1S_E1) + mean(result011$real_E1) - mean(result011$real_E0)), MC.sd_E01 = sd(result011$est_1S_E0 -result011$est_1S_E1), est.sd_E01 = mean(sqrt(result011$var_01)), cove_E01= mean(result011$coverage_CO_1S_E10),
                          bias_E02 = (mean(result011$est_1S_E0)-mean(result011$est_1S_E2) + mean(result011$real_E2) - mean(result011$real_E0)), MC.sd_E02 = sd(result011$est_1S_E0 -result011$est_1S_E2), est.sd_E02 = mean(sqrt(result011$var_02)), cove_E02= mean(result011$coverage_CO_1S_E20))
table_yF = rbind(pl_y_c_yF,EIF_yF_ET_v1,EIF_yF_ET_v2 )


tabler100 = rbind(table,table_EF,table_yF,tabler100F)
write.csv(tabler100, 'resultNEWn3000r1000final.csv')



#G0

pl_y_c_EF = data.frame(name='plugh-in' ,bias_E01 = (mean(result001$cov_E0) - mean(result001$real_E0)), MC.sd_E01 = sd(result001$cov_E0 ), est.sd_E01 = mean(sqrt(result001$var1_E0)), cove_E01= sum(result001$coveragen_E0))
EIF_yT_EF_v1 = data.frame(name='EIF_EQ' ,bias_E01 = (mean(result001$est_EQ_E0) - mean(result001$real_E0)), MC.sd_E01 = sd(result001$est_EQ_E0 ), est.sd_E01 = mean(sqrt(result001$var1_E0)), cove_E01= sum(result001$coverage_EQ_E0))
EIF_yT_EF_v2 = data.frame(name='EIF_1S' ,bias_E01 = (mean(result001$est_1S_E0) - mean(result001$real_E0)), MC.sd_E01 = sd(result001$est_1S_E0 ), est.sd_E01 = mean(sqrt(result001$var1_E0)), cove_E01= sum(result001$coverage_1S_E0))
table_EF = rbind(pl_y_c_EF,EIF_yT_EF_v1,EIF_yT_EF_v2 )

pl_y_c_yF2 = data.frame(name='plugh-in' ,bias_E01 = (mean(result000f$cov_E0) - mean(result000f$real_E0)), MC.sd_E01 = sd(result000f$cov_E0 ), est.sd_E01 = mean(sqrt(result000f$var1_E0)), cove_E01= sum(result000f$coveragen_E0))
EIF_yF_EF_v1 = data.frame(name='EIF_EQ' ,bias_E01 = (mean(result000f$est_EQ_E0) - mean(result000f$real_E0)), MC.sd_E01 = sd(result000f$est_EQ_E0 ), est.sd_E01 = mean(sqrt(result000f$var1_E0)), cove_E01= sum(result000f$coverage_EQ_E0))
EIF_yF_EF_v2 = data.frame(name='EIF_1S' ,bias_E01 = (mean(result000f$est_1S_E0) - mean(result000f$real_E0)), MC.sd_E01 = sd(result000f$est_1S_E0 ), est.sd_E01 = mean(sqrt(result000f$var1_E0)), cove_E01= sum(result000f$coverage_1S_E0))
table_yF_EF = rbind(pl_y_c_yF2,EIF_yF_EF_v1,EIF_yF_EF_v2 )

tabler100F = rbind(table_EF,table_yF_EF)


pl_y_c = data.frame(name='plugh-in' ,bias_E01 = (mean(result111$cov_E0) - mean(result111$real_E0)), MC.sd_E01 = sd(result111$cov_E0 ), est.sd_E01 = mean(sqrt(result111$var1_E0)), cove_E01= sum(result111$coveragen_E0))
EIF_yT_ET_v1 = data.frame(name='EIF_EQ' ,bias_E01 = (mean(result111$est_EQ_E0) - mean(result111$real_E0)), MC.sd_E01 = sd(result111$est_EQ_E0 ), est.sd_E01 = mean(sqrt(result111$var1_E0)), cove_E01= sum(result111$coverage_EQ_E0))
EIF_yT_ET_v2 = data.frame(name='EIF_1S' ,bias_E01 = (mean(result111$est_1S_E0) - mean(result111$real_E0)), MC.sd_E01 = sd(result111$est_1S_E0 ), est.sd_E01 = mean(sqrt(result111$var1_E0)), cove_E01= sum(result111$coverage_1S_E0))

table = rbind(pl_y_c,EIF_yT_ET_v1,EIF_yT_ET_v2 )


pl_y_c_EF = data.frame(name='plugh-in' ,bias_E01 = (mean(result101$cov_E0) - mean(result101$real_E0)), MC.sd_E01 = sd(result101$cov_E0 ), est.sd_E01 = mean(sqrt(result101$var1_E0)), cove_E01= sum(result101$coveragen_E0))
EIF_yT_EF_v1 = data.frame(name='EIF_EQ' ,bias_E01 = (mean(result101$est_EQ_E0) - mean(result101$real_E0)), MC.sd_E01 = sd(result101$est_EQ_E0 ), est.sd_E01 = mean(sqrt(result101$var1_E0)), cove_E01= sum(result101$coverage_EQ_E0))
EIF_yT_EF_v2 = data.frame(name='EIF_1S' ,bias_E01 = (mean(result101$est_1S_E0) - mean(result101$real_E0)), MC.sd_E01 = sd(result101$est_1S_E0 ), est.sd_E01 = mean(sqrt(result101$var1_E0)), cove_E01= sum(result101$coverage_1S_E0))

table_EF = rbind(pl_y_c_EF,EIF_yT_EF_v1,EIF_yT_EF_v2 )


pl_y_c_yF = data.frame(name='plugh-in' ,bias_E01 = (mean(result011$cov_E0) - mean(result011$real_E0)), MC.sd_E01 = sd(result011$cov_E0 ), est.sd_E01 = mean(sqrt(result011$var1_E0)), cove_E01= sum(result011$coveragen_E0))
EIF_yF_ET_v1 = data.frame(name='EIF_EQ' ,bias_E01 = (mean(result011$est_EQ_E0) - mean(result011$real_E0)), MC.sd_E01 = sd(result011$est_EQ_E0 ), est.sd_E01 = mean(sqrt(result011$var1_E0)), cove_E01= sum(result011$coverage_EQ_E0))
EIF_yF_ET_v2 = data.frame(name='EIF_1S' ,bias_E01 = (mean(result011$est_1S_E0) - mean(result011$real_E0)), MC.sd_E01 = sd(result011$est_1S_E0 ), est.sd_E01 = mean(sqrt(result011$var1_E0)), cove_E01= sum(result011$coverage_1S_E0))

table_yF = rbind(pl_y_c_yF,EIF_yF_ET_v1,EIF_yF_ET_v2 )


tabler100 = rbind(table,table_EF,table_yF,tabler100F)
write.csv(tabler100, 'resultG0n3000r1000final.csv')



#G1

pl_y_c_EF = data.frame(name='plugh-in' ,bias_E01 = abs(mean(result001$cov_E1) - mean(result001$real_E1)), MC.sd_E01 = sd(result001$cov_E1 ), est.sd_E01 = mean(sqrt(result001$var1_E1)), cove_E01= sum(result001$coveragen_E1))
EIF_yT_EF_v1 = data.frame(name='EIF_EQ' ,bias_E01 = abs(mean(result001$est_EQ_E1) - mean(result001$real_E1)), MC.sd_E01 = sd(result001$est_EQ_E1 ), est.sd_E01 = mean(sqrt(result001$var1_E1)), cove_E01= sum(result001$coverage_EQ_E1))
EIF_yT_EF_v2 = data.frame(name='EIF_1S' ,bias_E01 = abs(mean(result001$est_1S_E1) - mean(result001$real_E1)), MC.sd_E01 = sd(result001$est_1S_E1 ), est.sd_E01 = mean(sqrt(result001$var1_E1)), cove_E01= sum(result001$coverage_1S_E1))

table_EF = rbind(pl_y_c_EF,EIF_yT_EF_v1,EIF_yT_EF_v2 )


pl_y_c_yF2 = data.frame(name='plugh-in' ,bias_E01 = abs(mean(result000f$cov_E1) - mean(result000f$real_E1)), MC.sd_E01 = sd(result000f$cov_E1 ), est.sd_E01 = mean(sqrt(result000f$var1_E1)), cove_E01= sum(result000f$coveragen_E1))
EIF_yF_EF_v1 = data.frame(name='EIF_EQ' ,bias_E01 = abs(mean(result000f$est_EQ_E1) - mean(result000f$real_E1)), MC.sd_E01 = sd(result000f$est_EQ_E1 ), est.sd_E01 = mean(sqrt(result000f$var1_E1)), cove_E01= sum(result000f$coverage_EQ_E1))
EIF_yF_EF_v2 = data.frame(name='EIF_1S' ,bias_E01 = abs(mean(result000f$est_1S_E1) - mean(result000f$real_E1)), MC.sd_E01 = sd(result000f$est_1S_E1 ), est.sd_E01 = mean(sqrt(result000f$var1_E1)), cove_E01= sum(result000f$coverage_1S_E1))

table_yF_EF = rbind(pl_y_c_yF2,EIF_yF_EF_v1,EIF_yF_EF_v2 )
tabler100F = rbind(table_EF,table_yF_EF)


pl_y_c = data.frame(name='plugh-in' ,bias_E01 = abs(mean(result111$cov_E1) - mean(result111$real_E1)), MC.sd_E01 = sd(result111$cov_E1 ), est.sd_E01 = mean(sqrt(result111$var1_E1)), cove_E01= sum(result111$coveragen_E1))
EIF_yT_ET_v1 = data.frame(name='EIF_EQ' ,bias_E01 = abs(mean(result111$est_EQ_E1) - mean(result111$real_E1)), MC.sd_E01 = sd(result111$est_EQ_E1 ), est.sd_E01 = mean(sqrt(result111$var1_E1)), cove_E01= sum(result111$coverage_EQ_E1))
EIF_yT_ET_v2 = data.frame(name='EIF_1S' ,bias_E01 = abs(mean(result111$est_1S_E1) - mean(result111$real_E1)), MC.sd_E01 = sd(result111$est_1S_E1 ), est.sd_E01 = mean(sqrt(result111$var1_E1)), cove_E01= sum(result111$coverage_1S_E1))
table = rbind(pl_y_c,EIF_yT_ET_v1,EIF_yT_ET_v2 )


pl_y_c_EF = data.frame(name='plugh-in' ,bias_E01 = abs(mean(result101$cov_E1) - mean(result101$real_E1)), MC.sd_E01 = sd(result101$cov_E1 ), est.sd_E01 = mean(sqrt(result101$var1_E1)), cove_E01= sum(result101$coveragen_E1))
EIF_yT_EF_v1 = data.frame(name='EIF_EQ' ,bias_E01 = abs(mean(result101$est_EQ_E1) - mean(result101$real_E1)), MC.sd_E01 = sd(result101$est_EQ_E1 ), est.sd_E01 = mean(sqrt(result101$var1_E1)), cove_E01= sum(result101$coverage_EQ_E1))
EIF_yT_EF_v2 = data.frame(name='EIF_1S' ,bias_E01 = abs(mean(result101$est_1S_E1) - mean(result101$real_E1)), MC.sd_E01 = sd(result101$est_1S_E1 ), est.sd_E01 = mean(sqrt(result101$var1_E1)), cove_E01= sum(result101$coverage_1S_E1))
table_EF = rbind(pl_y_c_EF,EIF_yT_EF_v1,EIF_yT_EF_v2 )


pl_y_c_yF = data.frame(name='plugh-in' ,bias_E01 = abs(mean(result011$cov_E1) - mean(result011$real_E1)), MC.sd_E01 = sd(result011$cov_E1 ), est.sd_E01 = mean(sqrt(result011$var1_E1)), cove_E01= sum(result011$coveragen_E1))
EIF_yF_ET_v1 = data.frame(name='EIF_EQ' ,bias_E01 = abs(mean(result011$est_EQ_E1) - mean(result011$real_E1)), MC.sd_E01 = sd(result011$est_EQ_E1 ), est.sd_E01 = mean(sqrt(result011$var1_E1)), cove_E01= sum(result011$coverage_EQ_E1))
EIF_yF_ET_v2 = data.frame(name='EIF_1S' ,bias_E01 = abs(mean(result011$est_1S_E1) - mean(result011$real_E1)), MC.sd_E01 = sd(result011$est_1S_E1 ), est.sd_E01 = mean(sqrt(result011$var1_E1)), cove_E01= sum(result011$coverage_1S_E1))
table_yF = rbind(pl_y_c_yF,EIF_yF_ET_v1,EIF_yF_ET_v2 )


tabler100 = rbind(table,table_EF,table_yF,tabler100F)
write.csv(tabler100, 'resultG1n3000r1000final.csv')


#G2

pl_y_c_EF = data.frame(name='plugh-in' ,bias_E21 = (mean(result001$cov_E2) - mean(result001$real_E2)), MC.sd_E21 = sd(result001$cov_E2 ), est.sd_E21 = mean(sqrt(result001$var1_E2)), cove_E21= sum(result001$coveragen_E2))
EIF_yT_EF_v1 = data.frame(name='EIF_EQ' ,bias_E21 = (mean(result001$est_EQ_E2) - mean(result001$real_E2)), MC.sd_E21 = sd(result001$est_EQ_E2 ), est.sd_E21 = mean(sqrt(result001$var1_E2)), cove_E21= sum(result001$coverage_EQ_E2))
EIF_yT_EF_v2 = data.frame(name='EIF_1S' ,bias_E21 = (mean(result001$est_1S_E2) - mean(result001$real_E2)), MC.sd_E21 = sd(result001$est_1S_E2 ), est.sd_E21 = mean(sqrt(result001$var1_E2)), cove_E21= sum(result001$coverage_1S_E2))

table_EF = rbind(pl_y_c_EF,EIF_yT_EF_v1,EIF_yT_EF_v2 )


pl_y_c_yF2 = data.frame(name='plugh-in' ,bias_E21 = (mean(result000f$cov_E2) - mean(result000f$real_E2)), MC.sd_E21 = sd(result000f$cov_E2 ), est.sd_E21 = mean(sqrt(result000f$var1_E2)), cove_E21= sum(result000f$coveragen_E2))
EIF_yF_EF_v1 = data.frame(name='EIF_EQ' ,bias_E21 = (mean(result000f$est_EQ_E2) - mean(result000f$real_E2)), MC.sd_E21 = sd(result000f$est_EQ_E2 ), est.sd_E21 = mean(sqrt(result000f$var1_E2)), cove_E21= sum(result000f$coverage_EQ_E2))
EIF_yF_EF_v2 = data.frame(name='EIF_1S' ,bias_E21 = (mean(result000f$est_1S_E2) - mean(result000f$real_E2)), MC.sd_E21 = sd(result000f$est_1S_E2 ), est.sd_E21 = mean(sqrt(result000f$var1_E2)), cove_E21= sum(result000f$coverage_1S_E2))


table_yF_EF = rbind(pl_y_c_yF2,EIF_yF_EF_v1,EIF_yF_EF_v2 )
tabler100F = rbind(table_EF,table_yF_EF)



pl_y_c = data.frame(name='plugh-in' ,bias_E21 = (mean(result111$cov_E2) - mean(result111$real_E2)), MC.sd_E21 = sd(result111$cov_E2 ), est.sd_E21 = mean(sqrt(result111$var1_E2)), cove_E21= sum(result111$coveragen_E2))
EIF_yT_ET_v1 = data.frame(name='EIF_EQ' ,bias_E21 = (mean(result111$est_EQ_E2) - mean(result111$real_E2)), MC.sd_E21 = sd(result111$est_EQ_E2 ), est.sd_E21 = mean(sqrt(result111$var1_E2)), cove_E21= sum(result111$coverage_EQ_E2))
EIF_yT_ET_v2 = data.frame(name='EIF_1S' ,bias_E21 = (mean(result111$est_1S_E2) - mean(result111$real_E2)), MC.sd_E21 = sd(result111$est_1S_E2 ), est.sd_E21 = mean(sqrt(result111$var1_E2)), cove_E21= sum(result111$coverage_1S_E2))

table = rbind(pl_y_c,EIF_yT_ET_v1,EIF_yT_ET_v2 )

pl_y_c_EF = data.frame(name='plugh-in' ,bias_E21 = (mean(result101$cov_E2) - mean(result101$real_E2)), MC.sd_E21 = sd(result101$cov_E2 ), est.sd_E21 = mean(sqrt(result101$var1_E2)), cove_E21= sum(result101$coveragen_E2))
EIF_yT_EF_v1 = data.frame(name='EIF_EQ' ,bias_E21 = (mean(result101$est_EQ_E2) - mean(result101$real_E2)), MC.sd_E21 = sd(result101$est_EQ_E2 ), est.sd_E21 = mean(sqrt(result101$var1_E2)), cove_E21= sum(result101$coverage_EQ_E2))
EIF_yT_EF_v2 = data.frame(name='EIF_1S' ,bias_E21 = (mean(result101$est_1S_E2) - mean(result101$real_E2)), MC.sd_E21 = sd(result101$est_1S_E2 ), est.sd_E21 = mean(sqrt(result101$var1_E2)), cove_E21= sum(result101$coverage_1S_E2))

table_EF = rbind(pl_y_c_EF,EIF_yT_EF_v1,EIF_yT_EF_v2 )


pl_y_c_yF = data.frame(name='plugh-in' ,bias_E21 = (mean(result011$cov_E2) - mean(result011$real_E2)), MC.sd_E21 = sd(result011$cov_E2 ), est.sd_E21 = mean(sqrt(result011$var1_E2)), cove_E21= sum(result011$coveragen_E2))
EIF_yF_ET_v1 = data.frame(name='EIF_EQ' ,bias_E21 = (mean(result011$est_EQ_E2) - mean(result011$real_E2)), MC.sd_E21 = sd(result011$est_EQ_E2 ), est.sd_E21 = mean(sqrt(result011$var1_E2)), cove_E21= sum(result011$coverage_EQ_E2))
EIF_yF_ET_v2 = data.frame(name='EIF_1S' ,bias_E21 = (mean(result011$est_1S_E2) - mean(result011$real_E2)), MC.sd_E21 = sd(result011$est_1S_E2 ), est.sd_E21 = mean(sqrt(result011$var1_E2)), cove_E21= sum(result011$coverage_1S_E2))

table_yF = rbind(pl_y_c_yF,EIF_yF_ET_v1,EIF_yF_ET_v2 )


tabler100 = rbind(table,table_EF,table_yF,tabler100F)
write.csv(tabler100, 'resultG2n3000r1000final.csv')


