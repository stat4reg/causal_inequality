
n <- 2000

result111= read.csv(paste('./res/',n,'NewEQflexNM/result111.csv',sep = ''))
result101= read.csv(paste('./res/',n,'NewEQflexNM/result101.csv',sep = ''))
result011= read.csv(paste('./res/',n,'NewEQflexNM/result011.csv',sep = ''))
result000= read.csv(paste('./res/',n,'NewEQflexNM/result000.csv',sep = ''))
result110= read.csv(paste('./res/',n,'NewEQflexNM/result110.csv',sep = ''))

result000f= read.csv(paste('./res/',n,'NewEQflexNM/result000f2.csv',sep = ''))
result001= read.csv(paste('./res/',n,'NewEQflexNM/result001f2.csv',sep = ''))
# pl_y_c = data.frame(name='plugh-in, correct Y' ,bias_E0 = abs(mean(result111$cov_E0) - mean(result111$real_E0)), MC.sd_E0 = sd(result111$cov_E0), est.sd_E0 = mean(sqrt(result111$varn_E0)), cove_E0= sum(result111$coveragen_E0),bias_E1 = abs(mean(result111$cov_E1) - mean(result111$real_E1)), MC.sd_E1 = sd(result111$cov_E1), est.sd_E1 = mean(sqrt(result111$varn_E1)), cove_E1= sum(result111$coveragen_E1),bias_E2 = abs(mean(result111$cov_E2) - mean(result111$real_E2)), MC.sd_E2 = sd(result111$cov_E2), est.sd_E2 = mean(sqrt(result111$varn_E2)), cove_E2= sum(result111$coveragen_E2))
# EIF_yT_ET_v1 = data.frame(name='EIF_EQ, correct Y,E' ,bias_E0 = abs(mean(result111$est_EQ_E0) - mean(result111$real_E0)), MC.sd_E0 = sd(result111$est_EQ_E0), est.sd_E0 = mean(sqrt(result111$var1_E0)), cove_E0= sum(result111$coverage_EQ_E0),bias_E1 = abs(mean(result111$est_EQ_E1) - mean(result111$real_E1)), MC.sd_E1 = sd(result111$est_EQ_E1), est.sd_E1 = mean(sqrt(result111$var1_E1)), cove_E1= sum(result111$coverage_EQ_E1),bias_E2 = abs(mean(result111$est_EQ_E2) - mean(result111$real_E2)), MC.sd_E2 = sd(result111$est_EQ_E2), est.sd_E2 = mean(sqrt(result111$var1_E2)), cove_E2= sum(result111$coverage_EQ_E2))
# EIF_yT_ET_v2 = data.frame(name='EIF_1S, correct Y,E' ,bias_E0 = abs(mean(result111$est_1S_E0) - mean(result111$real_E0)), MC.sd_E0 = sd(result111$est_1S_E0), est.sd_E0 = mean(sqrt(result111$var2_E0)), cove_E0= sum(result111$coverage_1S_E0),bias_E1 = abs(mean(result111$est_1S_E1) - mean(result111$real_E1)), MC.sd_E1 = sd(result111$est_1S_E1), est.sd_E1 = mean(sqrt(result111$var2_E1)), cove_E1= sum(result111$coverage_1S_E1),bias_E2 = abs(mean(result111$est_1S_E2) - mean(result111$real_E2)), MC.sd_E2 = sd(result111$est_1S_E2), est.sd_E2 = mean(sqrt(result111$var2_E2)), cove_E2= sum(result111$coverage_1S_E2))
# table = rbind(pl_y_c,EIF_yT_ET_v1,EIF_yT_ET_v2 )


#result000= read.csv('./res/1000NewEQflex/result000f4nt.csv')
#result001= read.csv('./res/1000NewEQflex/result001f4nt.csv')
#result111= read.csv('./res/1000NewEQflex/result111.csv')


pl_y_c_EF = data.frame(name='plugh-in, correct Y' ,bias_E01 = abs(mean(result001$cov_E0)-mean(result001$cov_E1)+mean(result001$real_E1) - mean(result001$real_E0)), MC.sd_E01 = sd(result001$cov_E0 -result001$cov_E1), est.sd_E01 = mean(sqrt(result001$var_01)), cove_E01= sum(result001$coverage_COn_E10),
                       bias_E02 = abs(mean(result001$cov_E0)-mean(result001$cov_E2) + mean(result001$real_E2) - mean(result001$real_E0)), MC.sd_E02 = sd(result001$cov_E0 -result001$cov_E2), est.sd_E02 = mean(sqrt(result001$var_02)), cove_E02= sum(result001$coverage_COn_E20))
EIF_yT_EF_v1 = data.frame(name='EIF_EQ, correct Y,inc E' ,bias_E01 = abs(mean(result001$est_EQ_E0)-mean(result001$est_EQ_E1) + mean(result001$real_E1) - mean(result001$real_E0)), MC.sd_E01 = sd(result001$est_EQ_E0 -result001$est_EQ_E1), est.sd_E01 = mean(sqrt(result001$var_01)), cove_E01= sum(result001$coverage_CO_EQ_E10),
                          bias_E02 = abs(mean(result001$est_EQ_E0)-mean(result001$est_EQ_E2) + mean(result001$real_E2) - mean(result001$real_E0)), MC.sd_E02 = sd(result001$est_EQ_E0 -result001$est_EQ_E2), est.sd_E02 = mean(sqrt(result001$var_02)), cove_E02= sum(result001$coverage_CO_EQ_E20))
EIF_yT_EF_v2 = data.frame(name='EIF_1S, correct Y,inc E' ,bias_E01 = abs(mean(result001$est_1S_E0)-mean(result001$est_1S_E1) + mean(result001$real_E1) - mean(result001$real_E0)), MC.sd_E01 = sd(result001$est_1S_E0 -result001$est_1S_E1), est.sd_E01 = mean(sqrt(result001$var_01)), cove_E01= sum(result001$coverage_CO_1S_E10),
                          bias_E02 = abs(mean(result001$est_1S_E0)-mean(result001$est_1S_E2) + mean(result001$real_E2) - mean(result001$real_E0)), MC.sd_E02 = sd(result001$est_1S_E0 -result001$est_1S_E2), est.sd_E02 = mean(sqrt(result001$var_02)), cove_E02= sum(result001$coverage_CO_1S_E20))

table_EF = rbind(pl_y_c_EF,EIF_yT_EF_v1,EIF_yT_EF_v2 )



pl_y_c_yF2 = data.frame(name='plugh-in, incorrect Y' ,bias_E01 = abs(mean(result000f$cov_E0)-mean(result000f$cov_E1)+mean(result000f$real_E1) - mean(result000f$real_E0)), MC.sd_E01 = sd(result000f$cov_E0 -result000f$cov_E1), est.sd_E01 = mean(sqrt(result000f$var_01)), cove_E01= sum(result000f$coverage_COn_E10),
                        bias_E02 = abs(mean(result000f$cov_E0)-mean(result000f$cov_E2) + mean(result000f$real_E2) - mean(result000f$real_E0)), MC.sd_E02 = sd(result000f$cov_E0 -result000f$cov_E2), est.sd_E02 = mean(sqrt(result000f$var_02)), cove_E02= sum(result000f$coverage_COn_E20))
EIF_yF_EF_v1 = data.frame(name='EIF_EQ, incorrect Y,E' ,bias_E01 = abs(mean(result000f$est_EQ_E0)-mean(result000f$est_EQ_E1) + mean(result000f$real_E1) - mean(result000f$real_E0)), MC.sd_E01 = sd(result000f$est_EQ_E0 -result000f$est_EQ_E1), est.sd_E01 = mean(sqrt(result000f$var_01)), cove_E01= sum(result000f$coverage_CO_EQ_E10),
                          bias_E02 = abs(mean(result000f$est_EQ_E0)-mean(result000f$est_EQ_E2) + mean(result000f$real_E2) - mean(result000f$real_E0)), MC.sd_E02 = sd(result000f$est_EQ_E0 -result000f$est_EQ_E2), est.sd_E02 = mean(sqrt(result000f$var_02)), cove_E02= sum(result000f$coverage_CO_EQ_E20))
EIF_yF_EF_v2 = data.frame(name='EIF_1S, incorrect Y,E' ,bias_E01 = abs(mean(result000f$est_1S_E0)-mean(result000f$est_1S_E1) + mean(result000f$real_E1) - mean(result000f$real_E0)), MC.sd_E01 = sd(result000f$est_1S_E0 -result000f$est_1S_E1), est.sd_E01 = mean(sqrt(result000f$var_01)), cove_E01= sum(result000f$coverage_CO_1S_E10),
                          bias_E02 = abs(mean(result000f$est_1S_E0)-mean(result000f$est_1S_E2) + mean(result000f$real_E2) - mean(result000f$real_E0)), MC.sd_E02 = sd(result000f$est_1S_E0 -result000f$est_1S_E2), est.sd_E02 = mean(sqrt(result000f$var_02)), cove_E02= sum(result000f$coverage_CO_1S_E20))

table_yF_EF = rbind(pl_y_c_yF2,EIF_yF_EF_v1,EIF_yF_EF_v2 )

tabler100F = rbind(table_EF,table_yF_EF)
#write.csv(tabler100F, 'resultn1000r1000FLEXLAst.csv')







pl_y_c = data.frame(name='plugh-in, correct Y' ,bias_E01 = abs(mean(result111$cov_E0)-mean(result111$cov_E1)+mean(result111$real_E1) - mean(result111$real_E0)), MC.sd_E01 = sd(result111$cov_E0 -result111$cov_E1), est.sd_E01 = mean(sqrt(result111$var_01)), cove_E01= sum(result111$coverage_COn_E10),
                    bias_E02 = abs(mean(result111$cov_E0)-mean(result111$cov_E2) + mean(result111$real_E2) - mean(result111$real_E0)), MC.sd_E02 = sd(result111$cov_E0 -result111$cov_E2), est.sd_E02 = mean(sqrt(result111$var_02)), cove_E02= sum(result111$coverage_COn_E20))
EIF_yT_ET_v1 = data.frame(name='EIF_EQ, correct Y,E' ,bias_E01 = abs(mean(result111$est_EQ_E0)-mean(result111$est_EQ_E1) + mean(result111$real_E1) - mean(result111$real_E0)), MC.sd_E01 = sd(result111$est_EQ_E0 -result111$est_EQ_E1), est.sd_E01 = mean(sqrt(result111$var_01)), cove_E01= sum(result111$coverage_CO_EQ_E10),
                          bias_E02 = abs(mean(result111$est_EQ_E0)-mean(result111$est_EQ_E2) + mean(result111$real_E2) - mean(result111$real_E0)), MC.sd_E02 = sd(result111$est_EQ_E0 -result111$est_EQ_E2), est.sd_E02 = mean(sqrt(result111$var_02)), cove_E02= sum(result111$coverage_CO_EQ_E20))
EIF_yT_ET_v2 = data.frame(name='EIF_1S, correct Y,E' ,bias_E01 = abs(mean(result111$est_1S_E0)-mean(result111$est_1S_E1) + mean(result111$real_E1) - mean(result111$real_E0)), MC.sd_E01 = sd(result111$est_1S_E0 -result111$est_1S_E1), est.sd_E01 = mean(sqrt(result111$var_01)), cove_E01= sum(result111$coverage_CO_1S_E10),
                          bias_E02 = abs(mean(result111$est_1S_E0)-mean(result111$est_1S_E2) + mean(result111$real_E2) - mean(result111$real_E0)), MC.sd_E02 = sd(result111$est_1S_E0 -result111$est_1S_E2), est.sd_E02 = mean(sqrt(result111$var_02)), cove_E02= sum(result111$coverage_CO_1S_E20))
EIF_yT_ET_v12 = data.frame(name='EIF_EQv2, correct Y,E' ,bias_E01 = abs(mean(result111$est12_E0)-mean(result111$est12_E1) + mean(result111$real_E1) - mean(result111$real_E0)), MC.sd_E01 = sd(result111$est12_E0 -result111$est12_E1), est.sd_E01 = mean(sqrt(result111$var_01)), cove_E01= sum(result111$coverage_CO12_E10),
                           bias_E02 = abs(mean(result111$est12_E0)-mean(result111$est12_E2) + mean(result111$real_E2) - mean(result111$real_E0)), MC.sd_E02 = sd(result111$est12_E0 -result111$est12_E2), est.sd_E02 = mean(sqrt(result111$var_02)), cove_E02= sum(result111$coverage_CO12_E20))


table = rbind(pl_y_c,EIF_yT_ET_v1,EIF_yT_ET_v2 )






pl_y_c_EF = data.frame(name='plugh-in, correct Y' ,bias_E01 = abs(mean(result101$cov_E0)-mean(result101$cov_E1)+mean(result101$real_E1) - mean(result101$real_E0)), MC.sd_E01 = sd(result101$cov_E0 -result101$cov_E1), est.sd_E01 = mean(sqrt(result101$var_01)), cove_E01= sum(result101$coverage_COn_E10),
                       bias_E02 = abs(mean(result101$cov_E0)-mean(result101$cov_E2) + mean(result101$real_E2) - mean(result101$real_E0)), MC.sd_E02 = sd(result101$cov_E0 -result101$cov_E2), est.sd_E02 = mean(sqrt(result101$var_02)), cove_E02= sum(result101$coverage_COn_E20))
EIF_yT_EF_v1 = data.frame(name='EIF_EQ, correct Y,inc E' ,bias_E01 = abs(mean(result101$est_EQ_E0)-mean(result101$est_EQ_E1) + mean(result101$real_E1) - mean(result101$real_E0)), MC.sd_E01 = sd(result101$est_EQ_E0 -result101$est_EQ_E1), est.sd_E01 = mean(sqrt(result101$var_01)), cove_E01= sum(result101$coverage_CO_EQ_E10),
                          bias_E02 = abs(mean(result101$est_EQ_E0)-mean(result101$est_EQ_E2) + mean(result101$real_E2) - mean(result101$real_E0)), MC.sd_E02 = sd(result101$est_EQ_E0 -result101$est_EQ_E2), est.sd_E02 = mean(sqrt(result101$var_02)), cove_E02= sum(result101$coverage_CO_EQ_E20))
EIF_yT_EF_v2 = data.frame(name='EIF_1S, correct Y,inc E' ,bias_E01 = abs(mean(result101$est_1S_E0)-mean(result101$est_1S_E1) + mean(result101$real_E1) - mean(result101$real_E0)), MC.sd_E01 = sd(result101$est_1S_E0 -result101$est_1S_E1), est.sd_E01 = mean(sqrt(result101$var_01)), cove_E01= sum(result101$coverage_CO_1S_E10),
                          bias_E02 = abs(mean(result101$est_1S_E0)-mean(result101$est_1S_E2) + mean(result101$real_E2) - mean(result101$real_E0)), MC.sd_E02 = sd(result101$est_1S_E0 -result101$est_1S_E2), est.sd_E02 = mean(sqrt(result101$var_02)), cove_E02= sum(result101$coverage_CO_1S_E20))
table_EF = rbind(pl_y_c_EF,EIF_yT_EF_v1,EIF_yT_EF_v2 )
EIF_yT_EF_v12 = data.frame(name='EIF_EQv2, correct Y,inc E' ,bias_E01 = abs(mean(result101$est12_E0)-mean(result101$est12_E1) + mean(result101$real_E1) - mean(result101$real_E0)), MC.sd_E01 = sd(result101$est12_E0 -result101$est12_E1), est.sd_E01 = mean(sqrt(result101$var_01)), cove_E01= sum(result101$coverage_CO12_E10),
                           bias_E02 = abs(mean(result101$est12_E0)-mean(result101$est12_E2) + mean(result101$real_E2) - mean(result101$real_E0)), MC.sd_E02 = sd(result101$est12_E0 -result101$est12_E2), est.sd_E02 = mean(sqrt(result101$var_02)), cove_E02= sum(result101$coverage_CO12_E20))






pl_y_c_yF = data.frame(name='plugh-in, incorrect Y' ,bias_E01 = abs(mean(result011$cov_E0)-mean(result011$cov_E1)+mean(result011$real_E1) - mean(result011$real_E0)), MC.sd_E01 = sd(result011$cov_E0 -result011$cov_E1), est.sd_E01 = mean(sqrt(result011$var_01)), cove_E01= sum(result011$coverage_COn_E10),
                       bias_E02 = abs(mean(result011$cov_E0)-mean(result011$cov_E2) + mean(result011$real_E2) - mean(result011$real_E0)), MC.sd_E02 = sd(result011$cov_E0 -result011$cov_E2), est.sd_E02 = mean(sqrt(result011$var_02)), cove_E02= sum(result011$coverage_COn_E20))
EIF_yF_ET_v1 = data.frame(name='EIF_EQ, incorrect Y,c E' ,bias_E01 = abs(mean(result011$est_EQ_E0)-mean(result011$est_EQ_E1) + mean(result011$real_E1) - mean(result011$real_E0)), MC.sd_E01 = sd(result011$est_EQ_E0 -result011$est_EQ_E1), est.sd_E01 = mean(sqrt(result011$var_01)), cove_E01= sum(result011$coverage_CO_EQ_E10),
                          bias_E02 = abs(mean(result011$est_EQ_E0)-mean(result011$est_EQ_E2) + mean(result011$real_E2) - mean(result011$real_E0)), MC.sd_E02 = sd(result011$est_EQ_E0 -result011$est_EQ_E2), est.sd_E02 = mean(sqrt(result011$var_02)), cove_E02= sum(result011$coverage_CO_EQ_E20))
EIF_yF_ET_v2 = data.frame(name='EIF_1S, incorrect Y,c E' ,bias_E01 = abs(mean(result011$est_1S_E0)-mean(result011$est_1S_E1) + mean(result011$real_E1) - mean(result011$real_E0)), MC.sd_E01 = sd(result011$est_1S_E0 -result011$est_1S_E1), est.sd_E01 = mean(sqrt(result011$var_01)), cove_E01= sum(result011$coverage_CO_1S_E10),
                          bias_E02 = abs(mean(result011$est_1S_E0)-mean(result011$est_1S_E2) + mean(result011$real_E2) - mean(result011$real_E0)), MC.sd_E02 = sd(result011$est_1S_E0 -result011$est_1S_E2), est.sd_E02 = mean(sqrt(result011$var_02)), cove_E02= sum(result011$coverage_CO_1S_E20))
table_yF = rbind(pl_y_c_yF,EIF_yF_ET_v1,EIF_yF_ET_v2 )
EIF_yF_ET_v12 = data.frame(name='EIF_EQv2, incorrect Y,c E' ,bias_E01 = abs(mean(result011$est12_E0)-mean(result011$est12_E1) + mean(result011$real_E1) - mean(result011$real_E0)), MC.sd_E01 = sd(result011$est12_E0 -result011$est12_E1), est.sd_E01 = mean(sqrt(result011$var_01)), cove_E01= sum(result011$coverage_CO12_E10),
                           bias_E02 = abs(mean(result011$est12_E0)-mean(result011$est12_E2) + mean(result011$real_E2) - mean(result011$real_E0)), MC.sd_E02 = sd(result011$est12_E0 -result011$est12_E2), est.sd_E02 = mean(sqrt(result011$var_02)), cove_E02= sum(result011$coverage_CO12_E20))






pl_y_c_yF2 = data.frame(name='plugh-in, incorrect Y' ,bias_E01 = abs(mean(result000$cov_E0)-mean(result000$cov_E1)+mean(result000$real_E1) - mean(result000$real_E0)), MC.sd_E01 = sd(result000$cov_E0 -result000$cov_E1), est.sd_E01 = mean(sqrt(result000$var_01)), cove_E01= sum(result000$coverage_COn_E10),
                        bias_E02 = abs(mean(result000$cov_E0)-mean(result000$cov_E2) + mean(result000$real_E2) - mean(result000$real_E0)), MC.sd_E02 = sd(result000$cov_E0 -result000$cov_E2), est.sd_E02 = mean(sqrt(result000$var_02)), cove_E02= sum(result000$coverage_COn_E20))
EIF_yF_EF_v1 = data.frame(name='EIF_EQ, incorrect Y,E' ,bias_E01 = abs(mean(result000$est_EQ_E0)-mean(result000$est_EQ_E1) + mean(result000$real_E1) - mean(result000$real_E0)), MC.sd_E01 = sd(result000$est_EQ_E0 -result000$est_EQ_E1), est.sd_E01 = mean(sqrt(result000$var_01)), cove_E01= sum(result000$coverage_CO_EQ_E10),
                          bias_E02 = abs(mean(result000$est_EQ_E0)-mean(result000$est_EQ_E2) + mean(result000$real_E2) - mean(result000$real_E0)), MC.sd_E02 = sd(result000$est_EQ_E0 -result000$est_EQ_E2), est.sd_E02 = mean(sqrt(result000$var_02)), cove_E02= sum(result000$coverage_CO_EQ_E20))
EIF_yF_EF_v2 = data.frame(name='EIF_1S, incorrect Y,E' ,bias_E01 = abs(mean(result000$est_1S_E0)-mean(result000$est_1S_E1) + mean(result000$real_E1) - mean(result000$real_E0)), MC.sd_E01 = sd(result000$est_1S_E0 -result000$est_1S_E1), est.sd_E01 = mean(sqrt(result000$var_01)), cove_E01= sum(result000$coverage_CO_1S_E10),
                          bias_E02 = abs(mean(result000$est_1S_E0)-mean(result000$est_1S_E2) + mean(result000$real_E2) - mean(result000$real_E0)), MC.sd_E02 = sd(result000$est_1S_E0 -result000$est_1S_E2), est.sd_E02 = mean(sqrt(result000$var_02)), cove_E02= sum(result000$coverage_CO_1S_E20))
table_yF_EF = rbind(pl_y_c_yF2,EIF_yF_EF_v1,EIF_yF_EF_v2 )
EIF_yF_EF_v12 = data.frame(name='EIF_EQv2,incorrect Y,E' ,bias_E01 = abs(mean(result000$est12_E0)-mean(result000$est12_E1) + mean(result000$real_E1) - mean(result000$real_E0)), MC.sd_E01 = sd(result000$est12_E0 -result000$est12_E1), est.sd_E01 = mean(sqrt(result000$var_01)), cove_E01= sum(result000$coverage_CO12_E10),
                           bias_E02 = abs(mean(result000$est12_E0)-mean(result000$est12_E2) + mean(result000$real_E2) - mean(result000$real_E0)), MC.sd_E02 = sd(result000$est12_E0 -result000$est12_E2), est.sd_E02 = mean(sqrt(result000$var_02)), cove_E02= sum(result000$coverage_CO12_E20))


tabler100 = rbind(table,table_EF,table_yF,table_yF_EF)


tabler100 = rbind(table,table_EF,table_yF,table_yF_EF,tabler100F)
write.csv(tabler100, 'resultNMn1000r1000FLEX.csv')


# 
# pl_y_c_IF = data.frame(name='plugh-in, incorrect I' ,bias_E01 = abs(mean(result110$cov_E0)-mean(result110$cov_E1)+mean(result110$real_E1) - mean(result110$real_E0)), MC.sd_E01 = sd(result110$cov_E0 -result110$cov_E1), est.sd_E01 = mean(sqrt(result110$var_01)), cove_E01= sum(result110$coverage_COn_E10),
#                        bias_E02 = abs(mean(result110$cov_E0)-mean(result110$cov_E2) + mean(result110$real_E2) - mean(result110$real_E0)), MC.sd_E02 = sd(result110$cov_E0 -result110$cov_E2), est.sd_E02 = mean(sqrt(result110$var_02)), cove_E02= sum(result110$coverage_COn_E20))
# EIF_IF_ET_v1 = data.frame(name='EIF_EQ, incorrect I' ,bias_E01 = abs(mean(result110$est_EQ_E0)-mean(result110$est_EQ_E1) + mean(result110$real_E1) - mean(result110$real_E0)), MC.sd_E01 = sd(result110$est_EQ_E0 -result110$est_EQ_E1), est.sd_E01 = mean(sqrt(result110$var_01)), cove_E01= sum(result110$coverage_CO_EQ_E10),
#                           bias_E02 = abs(mean(result110$est_EQ_E0)-mean(result110$est_EQ_E2) + mean(result110$real_E2) - mean(result110$real_E0)), MC.sd_E02 = sd(result110$est_EQ_E0 -result110$est_EQ_E2), est.sd_E02 = mean(sqrt(result110$var_02)), cove_E02= sum(result110$coverage_CO_EQ_E20))
# EIF_IF_ET_v2 = data.frame(name='EIF_1S, incorrect I' ,bias_E01 = abs(mean(result110$est_1S_E0)-mean(result110$est_1S_E1) + mean(result110$real_E1) - mean(result110$real_E0)), MC.sd_E01 = sd(result110$est_1S_E0 -result110$est_1S_E1), est.sd_E01 = mean(sqrt(result110$var_01)), cove_E01= sum(result110$coverage_CO_1S_E10),
#                           bias_E02 = abs(mean(result110$est_1S_E0)-mean(result110$est_1S_E2) + mean(result110$real_E2) - mean(result110$real_E0)), MC.sd_E02 = sd(result110$est_1S_E0 -result110$est_1S_E2), est.sd_E02 = mean(sqrt(result110$var_02)), cove_E02= sum(result110$coverage_CO_1S_E20))
# EIF_IF_ET_v12 = data.frame(name='EIF_EQv2, incorrect I' ,bias_E01 = abs(mean(result110$est12_E0)-mean(result110$est12_E1) + mean(result110$real_E1) - mean(result110$real_E0)), MC.sd_E01 = sd(result110$est12_E0 -result110$est12_E1), est.sd_E01 = mean(sqrt(result110$var_01)), cove_E01= sum(result110$coverage_CO12_E10),
#                            bias_E02 = abs(mean(result110$est12_E0)-mean(result110$est12_E2) + mean(result110$real_E2) - mean(result110$real_E0)), MC.sd_E02 = sd(result110$est12_E0 -result110$est12_E2), est.sd_E02 = mean(sqrt(result110$var_02)), cove_E02= sum(result110$coverage_CO12_E20))
# table_IF = rbind(pl_y_c_IF,EIF_IF_ET_v1,EIF_IF_ET_v2,EIF_IF_ET_v12 )


