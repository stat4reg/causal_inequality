
library(nnet)
library(RcppNumerical)




DGP10 = function(n= 1000 , verbose= TRUE, sample = TRUE){
  X1 = rnorm(n, 1,1)
  X2 = rnorm(n, 10,1)
  
  l1 =  (5*(X1 + X2) - 55)/ (10*sqrt(2))
  l2 = (3*sqrt(2)*X1 -43*sqrt(2) + 4*sqrt(2)*X2)/ (10*sqrt(2))
  
  p1 = exp(0)/(exp(l1)+exp(0)+exp(l2))
  p2 = exp(l1)/(exp(l1)+exp(0)+exp(l2))
  p3 = exp(l2)/(exp(l1)+exp(0)+exp(l2))  #E = sample(c(0,1,2),n, replace = T,prob =c(p1,p2,p3) )
  #P = array(c(p1,p2,p3) , dim = c(3,n))
  P = rbind(p1,p2,p3)
  E = apply(P, MARGIN = 2 , function(x){sample(c(0,1,2),1, replace = T,prob =x )})
  
  m0 = 10 * X1 +X2 
  n0 = X1 - 0.1 * X2  
  Y0 = m0 + rnorm(n, 0, 2)
  I0 = n0 + rnorm(n, 0, 2) 
  m1 = 10 * X1 +X2 + 8
  n1 = -X1 + 1.5 * X2 
  Y1 = m1 + rnorm(n, 0, 2)
  I1 = n1 + rnorm(n, 0, 2)
  m2 = 10 * X1 +X2 + 18
  n2 = 20 * X1 - X2 + 10
  Y2 = m2 + rnorm(n, 0, 2)
  I2 = n2 + rnorm(n, 0, 2) 
  
  ecdfI0 = ecdf(I0)
  ecdfI1 = ecdf(I1)
  ecdfI2 = ecdf(I2)
  
  rankI0 = ecdfI0(I0)
  rankI1 = ecdfI1(I1)
  rankI2 = ecdfI2(I2)
  
  realG0 = 2*cov(Y0, rankI0)/ mean(Y0)
  realG1 = 2*cov(Y1, rankI1)/ mean(Y1)
  realG2 = 2*cov(Y2, rankI2)/ mean(Y2)
  
  realA1 = mean(Y1 * rankI1)
  realA0 = mean(Y0 * rankI0)
  realA2 = mean(Y2 * rankI2)
  realD1 = mean(I1 * rankI1)
  realB1 = mean(Y1)
  realB0 = mean(Y0)
  realB2 = mean(Y2)
  realC1 = mean(rankI1)
  realrank1 = rankI1
  realGini1 = 2 *cov(I1 , rankI1)/ mean(I1)
  #realG1 = 2*cov(Y1[E==0], I1[E==0])/ mean(Y1[E==0])
  
  Y = Y0 *(E==0) +Y1*(E==1) +Y2 *(E==2) 
  I = I0 *(E==0) +I1*(E==1) +I2 *(E==2) 
  
  
  ploteach = function( pp, ee){
    
    h0 = hist(pp[ee==0],breaks = 100, plot = FALSE)
    h1 = hist(pp[ee==1],breaks = 100, plot = FALSE)
    h2 = hist(pp[ee==2],breaks = 100, plot = FALSE)
    
    plot(h0, col = "#FF000050", xlim = range(0:1), main="Histogram of Probabilities", xlab="P")
    legend("topright",c("E=0", "E=1", "E=2"),col=c("#FF000050", "#00FF0050", "#0000FF50"), lwd=4)
    
    plot(h1, col = "#00FF0050", add = TRUE)
    plot(h2, col = "#0000FF50", add = TRUE)
  }
  
  
  
  
  
  
  if(verbose & sample){
    par(mfrow=c(3,1))
    ploteach(p1,E)
    ploteach(p2,E)
    ploteach(p3,E)
  }
  if (sample){
    return(list(X1=X1,X2=X2,Y=Y,I=I,E=E,realG1 = realG1,realA1 = realA1, realB1=realB1 ,realC1 = realC1,realD1 =realD1, n=n, realrank1=realrank1, realGini1=realGini1))
  }
  return(list(realG1 = realG1,realA1 = realA1, realB1=realB1 ,realG0 = realG0,realA0 = realA0, realB0=realB0, realG2 = realG2,realA2 = realA2, realB2=realB2 ,realC1 = realC1,realD1 =realD1, n=n, realGini1=realGini1))
}



sim_one_treatment =function(data,j, realG1, realA1, realB1 ,Right_Y_model = TRUE, Right_E_model = TRUE, Right_I_model = TRUE )  {
  
  X1 = data$X1
  X2 = data$X2
  Y = data$Y
  I = data$I
  E = data$E
  
  n = data$n
  n2 = sum(data$E==2)
  n1 = sum(data$E==1)
  n0 = sum(data$E==0)
  ### estimation
  
  ##Gj
  # TODO works for any number of covariates
  if(!Right_Y_model){
    m1y = lm('Y~X1+X2', list('Y'=Y[E==j], 'X1'=log(X1[E==j]^2),'X2'=log(X2[E==j]^2) ))
    m1ya = predict.lm(m1y ,  list( 'X1'=log(X1^2),'X2'=log(X2^2)))
    m1y1 = predict.lm(m1y ,  list( 'X1'=log(X1[E==j]^2),'X2'=log(X2[E==j]^2)))
  }else{
    m1y = lm('Y~X1+X2', list('Y'= Y[E==j], 'X1'=X1[E==j],'X2'=X2[E==j]))
    m1ya = predict.lm(m1y ,  list( 'X1'=X1,'X2'=X2))
    m1y1 = predict.lm(m1y ,  list( 'X1'=X1[E==j],'X2'=X2[E==j]))
  }
  
  if(Right_I_model){
    nd1I = lm('Y~X1+X2', list('Y'=I[E==j], 'X1'=X1[E==j],'X2'=X2[E==j] ))
    I2 = c(outer(I[E==j],I[E==j],'-')) 
    I2c = I2<=0 
    XX1 = c(outer(X1[E==j],X1[E==j],'-')) 
    XX2 = c(outer(X2[E==j],X2[E==j],'-')) 
    n11 = glm('I~X1+X2',list('I'=I2c,'X1'=XX1 , 'X2'=XX2 ),family = binomial(link = "probit"))
    
    XX1f = c(outer(X1,X1,'-')) 
    XX2f = c(outer(X2,X2,'-')) 
    n1p = predict.glm(n11, list('X1'=XX1f , 'X2'=XX2f ),type = "response")
    n1pall = matrix(n1p,n,n)
    n1I1 = predict.lm(nd1I ,  list( 'X1'=X1[E==j],'X2'=X2[E==j]))
    n1I = predict.lm(nd1I ,  list( 'X1'=X1,'X2'=X2))
  }else {
    nd1I = lm('Y~X1+X2', list('Y'=I[E==j], 'X1'=log(X1[E==j]^2),'X2'=log(X2[E==j]^2) ))
    I2 = c(outer(I[E==j],I[E==j],'-')) 
    I2c = I2<=0 
    XX1 = c(outer(log(X1[E==j]^2),log(X1[E==j]^2),'-')) 
    XX2 = c(outer(log(X2[E==j]^2),log(X2[E==j]^2),'-')) 
    n11 = glm('I~X1+X2',list('I'=I2c,'X1'=XX1 , 'X2'=XX2 ),family = binomial(link = "probit"))
    
    XX1f = c(outer(log(X1^2),log(X1^2),'-')) 
    XX2f = c(outer(log(X2^2),log(X2^2),'-')) 
    n1p = predict.glm(n11, list('X1'=XX1f , 'X2'=XX2f ),type = "response")
    n1pall = matrix(n1p,n,n)
    n1I1 = predict.lm(nd1I ,  list( 'X1'=log(X1[E==j]^2),'X2'=log(X2[E==j]^2)))
    n1I = predict.lm(nd1I ,  list( 'X1'=log(X1^2),'X2'=log(X2^2)))
  }
  
  rankr1y1 = colMeans(n1pall)[E==j]
  rankr1ya = colMeans(n1pall)
  
  
  product_model = sweep(n1pall, MARGIN=2, n1I, `*`)
  
  E1 <- relevel(as.factor(E), ref = '0')
  if(!Right_E_model){
    invisible(capture.output(pro <- multinom('T~X1+X2', list('T'= E1, 'X1'=log(X1^2),'X2'=log(X2^2)))))
  }else{
    invisible(capture.output(pro <- multinom('T~X1+X2', list('T'= E1, 'X1'=X1,'X2'=X2))))
    
  }
  prs1 = pro$fitted.values[,(j+1)]
  
  ################# F estimator
  
  
  rI1 = numeric(0)
  rI0 = numeric(0)
  arI0 = numeric(0)
  
  if(Right_I_model){
    for (i1 in I[E==j]){
      Ind = I[E==j] <= i1
      Ind2 = i1  <= I[E==j]
      Fhat1 <- fastLR(x = cbind(1,X1[E==j],X2[E==j]), y= Ind)
      coeff = Fhat1$coefficients
      Fhatall = 1/ (1 + exp(-coeff %*% rbind(1,X1,X2))[1,])
      
      rI1 = c(rI1, mean(Fhatall) + mean((Ind-Fhatall[E==j])/prs1[E==j])*n1/n)
      Fhat2 <- fastLR(x = cbind(1,X1[E==j],X2[E==j]), y= Ind2)
      coeff2 = Fhat2$coefficients
      Fhatall2 = 1/ (1 + exp(-coeff2 %*% rbind(1,X1,X2))[1,])
      
      rI0 = c(rI0, mean(Fhatall2) )
      arI0 = c(arI0, mean(Fhatall2* m1ya) )
    }
  }else{
    for (i1 in I[E==j]){
      Ind = I[E==j] <= i1
      Ind2 = i1  <= I[E==j]
      Fhat1 <- fastLR(x = cbind(1,log(X1[E==j]^2),log(X2[E==j]^2)), y= Ind)
      coeff = Fhat1$coefficients
      Fhatall = 1/ (1 + exp(-coeff %*% rbind(1,log(X1^2),log(X2^2)))[1,])
      rI1 = c(rI1, mean(Fhatall) + mean((Ind-Fhatall[E==j])/prs1[E==j])*n1/n)
      
      Fhat2 <- fastLR(x = cbind(1,log(X1[E==j]^2),log(X2[E==j]^2)), y= Ind2)
      coeff2 = Fhat2$coefficients
      Fhatall2 = 1/ (1 + exp(-coeff2 %*% rbind(1,log(X1^2),log(X2^2)))[1,])
      rI0 = c(rI0, mean(Fhatall2) )
      arI0 = c(arI0, mean(Fhatall2* m1ya) )
    }
  }
  
  
  very_naive = 2*sum(Y[E==j]*rankr1y1) / sum(Y[E==j]) - 1
  
  
  ###############################################################################
  phi_B =  (1/prs1[E==j])* (Y[E==j] -  m1y1 )  
  phi_B2 = m1ya - mean(m1ya)
  
  ####################
  ##### A and B
  IF_B = numeric(n)
  IF_B[E==j] <- ((phi_B))
  IF_B = IF_B + phi_B2
  
  estB = mean(m1ya) + mean(IF_B)
  varB = var(IF_B)
  ####################
  ####################
  #####################################################################################
  
  A_naive = mean(m1ya * rankr1ya)
  phi_a11 = arI0  #unlist(lapply(I[E==1], function(x){mean(m1y0*(as.numeric(x < n1y0)))}))
  phi_a12 = rowMeans(sweep(n1pall, MARGIN=2, m1ya, `*`))[E==j] #colMeans(n1pall[E==0,]*m1y0)[E==1]
  phi_a3 = rowMeans(sweep(n1pall, MARGIN=2, m1ya, `*`))#rowMeans(n1pall[,E==0]*m1y0)[E==0]#colMeans(n1pall[E==0,]*m1y0)
  Phi_A1 = (1/prs1[E==j]) * (Y[E==j] *rI1 - m1y1*rankr1y1  +    phi_a11 - phi_a12 )
  #Phi_A2 = (rankr1y0*m1y0 - A_naive   +   phi_a3 - A_naive   )* (n/ n0)
  IF_A = numeric(n)
  IF_A[E==j] <- (Phi_A1)
  #IF_A[E==0] <- (Phi_A2)
  #IF_A = IF_A + phi_a3 - A_naive
  IF_A = IF_A + rankr1ya*m1ya - A_naive   +   phi_a3 - A_naive 
  estA2 = A_naive + mean(IF_A)/2 
  estA1 = A_naive + mean(IF_A)
  varA = var(IF_A)
  
  ####################################
  ########################################
  
  C_naive = mean(rankr1ya)#0.5 #* (n +1)/n mean(rankr1ya)#
  estC2 =0.5#* (n +1)/n
  estC1 =0.5#* (n +1)/n
  varC =0
  IF_C = numeric(n)
  IF_C2 = numeric(n)
  
  ####################################3
  
  #####################
  # IF A/B - c
  # Ap/B - BpA/BB - cp
  
  IF = IF_A / mean(m1ya) - (IF_B * mean(m1ya* rankr1ya)) / mean(m1ya)^2 
  ### one step
  #est1 = 2*( mean(IF - IF_C- IF_C2) + mean(m1ya* rankr1ya)/ mean(m1ya) -C_naive) 
  
  ### solve A and B sep
  
  est_EQ = 2*( (estA2/ estB) -C_naive)
  est12 = 2*( (2*estA2/ (estB + mean(m1ya))) -C_naive)
  
  Var1 = var(2 *(IF - IF_C- IF_C2))    
  Varn = var(2 *(IF - IF_C- IF_C2))
  
  ##################### additional terms because of rank
  ########### are added to A and C
  #IF_D = (phi_D - mean(m1y0* rankn1ya(n1y0))) / mean(m1y0) - phi_DI + mean(rankn1ya(n1y0))
  
  ####################### final
  IF2 = IF_A / mean(m1ya) - (IF_B * mean(m1ya* rankr1ya)) / mean(m1ya)^2  #- IF_C - IF_C2 #+ IF_D
  #est2 = 2*( mean((IF_A +2*A_naive)/mean(m1ya)) - mean(IF_B + mean(m1ya)) *mean(m1ya* rankr1ya)/ mean(m1ya)^2 - mean(IF_C+ IF_C2) -C_naive  )
  #est2 = 2*(    (  mean((IF_A +2*A_naive)) / ( mean(2*IF_B + 2*mean(m1ya)) )  )  -C_naive)  
  est_1S = 2*(  mean(IF2) +  (A_naive/ mean(m1ya))  -C_naive ) 
  #IF2[E==1] <- IF2[E==1] + (((phi_A1- phi_A1_alt))* (n/ n0))/mean(m1y0)
  Var2 = var(2 *IF2)
  
  #print(list(estA1= estA1,estA2= estA2, varA = varA/n, realA1 = realA1 ,estB= estB, varB = varB/n, realB1 = realB1, est_EQ = est_EQ,est12 = est12,est_1S = est_1S, real =realG1, cov = 2*cov(m1ya, rankr1ya)/ mean(m1ya), var1 = Var1/(n), var2 = Var2/(n), varn = Varn/(n), very_naive=very_naive))  # coverage = c, coverage2 = c2
  
  dat <- list(IF= IF, df = data.frame(estA1= estA1,estA2= estA2, varA = varA/n, realA1 = realA1 ,estB= estB, varB = varB/n, realB1 = realB1, est_EQ = est_EQ,est12 = est12,est_1S = est_1S, real =realG1, cov = 2*cov(m1ya, rankr1ya)/ mean(m1ya), var1 = Var1/(n), var2 = Var2/(n), varn = Varn/(n), very_naive=very_naive))  # coverage = c, coverage2 = c2
  #dat$i <- i  # maybe you want to keep track of which iteration produced it?
  #datalist[[i]] <- 
  dat # add it to your list
}



simulation_v12 = function(dgp, r = 1000, n = 10000, verbose = FALSE , seed = 13824, Right_Y_model = TRUE, Right_E_model = TRUE, Right_I_model = TRUE, n_param = 1000000){
  set.seed(seed)
  #datalist = list()
  
  parameters = dgp(n_param,verbose=verbose, sample= FALSE)
  realG1 = parameters$realG1
  realA1 = parameters$realA1
  realB1 = parameters$realB1
  #realC1 = parameters$realC1
  realG0 = parameters$realG0
  realA0 = parameters$realA0
  realB0 = parameters$realB0
  
  realG2 = parameters$realG2
  realA2 = parameters$realA2
  realB2 = parameters$realB2
  
  fin_data =foreach(i=1:r, .packages=c('nnet','RcppNumerical'), .combine=rbind) %dopar% {
    source('./contrast_sim_utility.R', local = TRUE)
    if((i %% 100) == 0 )print(i)
    
    data = dgp(n,verbose=verbose)
    
    dat0 = sim_one_treatment(data,0 ,realG0, realA0, realB0, Right_Y_model = Right_Y_model, Right_E_model = Right_E_model, Right_I_model = Right_I_model)
    dat00 = dat0$df
    colnames(dat00) = paste(colnames(dat00), 'E0', sep = '_')
    dat1 = sim_one_treatment(data,1 ,realG1, realA1, realB1, Right_Y_model = Right_Y_model, Right_E_model = Right_E_model, Right_I_model = Right_I_model)
    dat11 = dat1$df
    colnames(dat11) = paste(colnames(dat11), 'E1', sep = '_')
    dat2 = sim_one_treatment(data,2 ,realG2, realA2, realB2, Right_Y_model = Right_Y_model, Right_E_model = Right_E_model, Right_I_model = Right_I_model)
    dat22 = dat2$df
    colnames(dat22) = paste(colnames(dat22), 'E2', sep = '_')
    VAR = data.frame('var_01' = var(2*(dat1$IF -  dat0$IF))/n , 'var_02' = var(2*(dat2$IF -  dat0$IF))/n )
    dat = cbind(dat00,dat11,dat22,VAR)
    dat$i <- i
    print(dat00)
    print(VAR)
    dat
  }
  
  
  #fin_data = do.call(rbind, datalist)
  #attr(fin_data, 'class') <- 'simulation'
  
  fin_data$coverage_EQ_E0 = (mean(fin_data$real_E0) >= fin_data$est_EQ_E0+qnorm(0.025) * sqrt(fin_data$var1_E0 )) & (mean(fin_data$real_E0) <= fin_data$est_EQ_E0+qnorm(0.975) * sqrt(fin_data$var1_E0 ) )
  fin_data$coverage12_E0 = (mean(fin_data$real_E0) >= fin_data$est12_E0+qnorm(0.025) * sqrt(fin_data$var1_E0 )) & (mean(fin_data$real_E0) <= fin_data$est12_E0+qnorm(0.975) * sqrt(fin_data$var1_E0 ) )
  fin_data$coverage_1S_E0 = (mean(fin_data$real_E0) >= fin_data$est_1S_E0+qnorm(0.025) * sqrt(fin_data$var2_E0 )) & (mean(fin_data$real_E0) <= fin_data$est_1S_E0+qnorm(0.975) * sqrt(fin_data$var2_E0) )
  fin_data$coveragen_E0 = (mean(fin_data$real_E0) >= fin_data$cov_E0+qnorm(0.025) * sqrt(fin_data$varn_E0 )) & (mean(fin_data$real_E0) <= fin_data$cov_E0+qnorm(0.975) * sqrt(fin_data$varn_E0) )
  fin_data$coverageB_E0 = (mean(fin_data$realB1_E0) >= fin_data$estB_E0+qnorm(0.025) * sqrt(fin_data$varB_E0 )) & (mean(fin_data$realB1_E0) <= fin_data$estB_E0+qnorm(0.975) * sqrt(fin_data$varB_E0 ) )
  fin_data$coverageA1_E0 = (mean(fin_data$realA1_E0) >= fin_data$estA1_E0+qnorm(0.025) * sqrt(fin_data$varA_E0 )) & (mean(fin_data$realA1_E0) <= fin_data$estA1_E0+qnorm(0.975) * sqrt(fin_data$varA_E0 ) )
  fin_data$coverageA2_E0 = (mean(fin_data$realA1_E0) >= fin_data$estA2_E0+qnorm(0.025) * sqrt(fin_data$varA_E0 )) & (mean(fin_data$realA1_E0) <= fin_data$estA2_E0+qnorm(0.975) * sqrt(fin_data$varA_E0 ) )
  
  fin_data$coverage_EQ_E1 = (mean(fin_data$real_E1) >= fin_data$est_EQ_E1+qnorm(0.025) * sqrt(fin_data$var1_E1 )) & (mean(fin_data$real_E1) <= fin_data$est_EQ_E1+qnorm(0.975) * sqrt(fin_data$var1_E1 ) )
  fin_data$coverage12_E1 = (mean(fin_data$real_E1) >= fin_data$est12_E1+qnorm(0.025) * sqrt(fin_data$var1_E1 )) & (mean(fin_data$real_E1) <= fin_data$est12_E1+qnorm(0.975) * sqrt(fin_data$var1_E1 ) )
  fin_data$coverage_1S_E1 = (mean(fin_data$real_E1) >= fin_data$est_1S_E1+qnorm(0.025) * sqrt(fin_data$var2_E1 )) & (mean(fin_data$real_E1) <= fin_data$est_1S_E1+qnorm(0.975) * sqrt(fin_data$var2_E1) )
  fin_data$coveragen_E1 = (mean(fin_data$real_E1) >= fin_data$cov_E1+qnorm(0.025) * sqrt(fin_data$varn_E1 )) & (mean(fin_data$real_E1) <= fin_data$cov_E1+qnorm(0.975) * sqrt(fin_data$varn_E1) )
  fin_data$coverageB_E1 = (mean(fin_data$realB1_E1) >= fin_data$estB_E1+qnorm(0.025) * sqrt(fin_data$varB_E1 )) & (mean(fin_data$realB1_E1) <= fin_data$estB_E1+qnorm(0.975) * sqrt(fin_data$varB_E1 ) )
  fin_data$coverageA1_E1 = (mean(fin_data$realA1_E1) >= fin_data$estA1_E1+qnorm(0.025) * sqrt(fin_data$varA_E1 )) & (mean(fin_data$realA1_E1) <= fin_data$estA1_E1+qnorm(0.975) * sqrt(fin_data$varA_E1 ) )
  fin_data$coverageA2_E1 = (mean(fin_data$realA1_E1) >= fin_data$estA2_E1+qnorm(0.025) * sqrt(fin_data$varA_E1 )) & (mean(fin_data$realA1_E1) <= fin_data$estA2_E1+qnorm(0.975) * sqrt(fin_data$varA_E1 ) )
  
  fin_data$coverage_EQ_E2 = (mean(fin_data$real_E2) >= fin_data$est_EQ_E2+qnorm(0.025) * sqrt(fin_data$var1_E2 )) & (mean(fin_data$real_E2) <= fin_data$est_EQ_E2+qnorm(0.975) * sqrt(fin_data$var1_E2 ) )
  fin_data$coveragE22_E2 = (mean(fin_data$real_E2) >= fin_data$est12_E2+qnorm(0.025) * sqrt(fin_data$var1_E2 )) & (mean(fin_data$real_E2) <= fin_data$est12_E2+qnorm(0.975) * sqrt(fin_data$var1_E2 ) )
  fin_data$coverage_1S_E2 = (mean(fin_data$real_E2) >= fin_data$est_1S_E2+qnorm(0.025) * sqrt(fin_data$var2_E2 )) & (mean(fin_data$real_E2) <= fin_data$est_1S_E2+qnorm(0.975) * sqrt(fin_data$var2_E2) )
  fin_data$coveragen_E2 = (mean(fin_data$real_E2) >= fin_data$cov_E2+qnorm(0.025) * sqrt(fin_data$varn_E2 )) & (mean(fin_data$real_E2) <= fin_data$cov_E2+qnorm(0.975) * sqrt(fin_data$varn_E2) )
  fin_data$coverageB_E2 = (mean(fin_data$realB1_E2) >= fin_data$estB_E2+qnorm(0.025) * sqrt(fin_data$varB_E2 )) & (mean(fin_data$realB1_E2) <= fin_data$estB_E2+qnorm(0.975) * sqrt(fin_data$varB_E2 ) )
  fin_data$coverageA1_E2 = (mean(fin_data$realA1_E2) >= fin_data$estA1_E2+qnorm(0.025) * sqrt(fin_data$varA_E2 )) & (mean(fin_data$realA1_E2) <= fin_data$estA1_E2+qnorm(0.975) * sqrt(fin_data$varA_E2 ) )
  fin_data$coverageA2_E2 = (mean(fin_data$realA1_E2) >= fin_data$estA2_E2+qnorm(0.025) * sqrt(fin_data$varA_E2 )) & (mean(fin_data$realA1_E2) <= fin_data$estA2_E2+qnorm(0.975) * sqrt(fin_data$varA_E2 ) )
  
  
  
  fin_data$coverage_CO_EQ_E10 = (mean(fin_data$real_E1 - fin_data$real_E0) >= fin_data$est_EQ_E1 - fin_data$est_EQ_E0+qnorm(0.025) * sqrt(fin_data$var_01 )) & (mean(fin_data$real_E1 - fin_data$real_E0) <= fin_data$est_EQ_E1 - fin_data$est_EQ_E0+qnorm(0.975) * sqrt(fin_data$var_01 ) )
  fin_data$coverage_CO12_E10 = (mean(fin_data$real_E1 - fin_data$real_E0) >= fin_data$est12_E1- fin_data$est12_E0+qnorm(0.025) * sqrt(fin_data$var_01 )) & (mean(fin_data$real_E1 - fin_data$real_E0) <= fin_data$est12_E1 - fin_data$est12_E0+qnorm(0.975) * sqrt(fin_data$var_01 ) )
  fin_data$coverage_CO_1S_E10 = (mean(fin_data$real_E1 - fin_data$real_E0) >= fin_data$est_1S_E1 - fin_data$est_1S_E0+qnorm(0.025) * sqrt(fin_data$var_01 )) & (mean(fin_data$real_E1 - fin_data$real_E0) <= fin_data$est_1S_E1 - fin_data$est_1S_E0+qnorm(0.975) * sqrt(fin_data$var_01) )
  fin_data$coverage_COn_E10 = (mean(fin_data$real_E1 - fin_data$real_E0) >= fin_data$cov_E1 - fin_data$cov_E0+qnorm(0.025) * sqrt(fin_data$var_01 )) & (mean(fin_data$real_E1 - fin_data$real_E0) <= fin_data$cov_E1 - fin_data$cov_E0+qnorm(0.975) * sqrt(fin_data$var_01) )
  
  fin_data$coverage_CO_EQ_E20 = (mean(fin_data$real_E2 - fin_data$real_E0) >= fin_data$est_EQ_E2 - fin_data$est_EQ_E0+qnorm(0.025) * sqrt(fin_data$var_02 )) & (mean(fin_data$real_E2 - fin_data$real_E0) <= fin_data$est_EQ_E2 - fin_data$est_EQ_E0+qnorm(0.975) * sqrt(fin_data$var_02 ) )
  fin_data$coverage_CO12_E20 = (mean(fin_data$real_E2 - fin_data$real_E0) >= fin_data$est12_E2- fin_data$est12_E0+qnorm(0.025) * sqrt(fin_data$var_02 )) & (mean(fin_data$real_E2 - fin_data$real_E0) <= fin_data$est12_E2 - fin_data$est12_E0+qnorm(0.975) * sqrt(fin_data$var_02 ) )
  fin_data$coverage_CO_1S_E20 = (mean(fin_data$real_E2 - fin_data$real_E0) >= fin_data$est_1S_E2 - fin_data$est_1S_E0+qnorm(0.025) * sqrt(fin_data$var_02 )) & (mean(fin_data$real_E2 - fin_data$real_E0) <= fin_data$est_1S_E2 - fin_data$est_1S_E0+qnorm(0.975) * sqrt(fin_data$var_02) )
  fin_data$coverage_COn_E20 = (mean(fin_data$real_E2 - fin_data$real_E0) >= fin_data$cov_E2 - fin_data$cov_E0+qnorm(0.025) * sqrt(fin_data$var_02 )) & (mean(fin_data$real_E2 - fin_data$real_E0) <= fin_data$cov_E2 - fin_data$cov_E0+qnorm(0.975) * sqrt(fin_data$var_02) )
  
  
  return(fin_data)
}


print.simulation = function(x){
  print(paste('real poi:',mean(x$real_E1)))
  print('#####  est_EQ #####')
  print(paste('est_EQ:', mean(x$est_EQ_E1)))
  print(paste('var est_EQ:',mean(x$var1_E1)))
  print(paste('mc var est_EQ:', var(x$est_EQ_E1)))
  print(paste('coverage_EQ:',sum(x$coverage_EQ_E1)))

  print('#####  est12 #####')
  print(paste('est12:', mean(x$est12_E1)))
  print(paste('var est12:',mean(x$var1_E1)))
  print(paste('mc var est12:', var(x$est12_E1)))
  print(paste('coverage12:',sum(x$coverage12_E1)))

  print('#####  est_1S #####')
  print(paste('est_1S:', mean(x$est_1S_E1)))
  print(paste('var est_1S:',mean(x$var2_E1)))
  print(paste('mc var est_1S:', var(x$est_1S_E1)))
  print(paste('coverage_1S:',sum(x$coverage_1S_E1)))

  print('#####  estA1 #####')
  print(paste('real poi A:',mean(x$realA1_E1)))
  print(paste('est1:', mean(x$estA1_E1)))
  print(paste('var estA:',mean(x$varA_E1)))
  print(paste('mc var estA1:', var(x$estA1_E1)))
  print(paste('coverageA1:',sum(x$coverageA1_E1)))

  print('#####  estA2 #####')
  print(paste('real poi A:',mean(x$realA1_E1)))
  print(paste('est2:', mean(x$estA2_E1)))
  print(paste('var estA:',mean(x$varA_E1)))
  print(paste('mc var estA2:', var(x$estA2_E1)))
  print(paste('coverageA2:',sum(x$coverageA2_E1)))

  print('#####  estB #####')
  print(paste('real poi B:',mean(x$realB1_E1)))
  print(paste('estB:', mean(x$estB_E1)))
  print(paste('var estB:',mean(x$varB_E1)))
  print(paste('mc var estB:', var(x$estB_E1)))
  print(paste('coverageB:',sum(x$coverageB_E1)))

 

}
