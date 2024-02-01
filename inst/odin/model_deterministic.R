
interpolation_time[] <- user()
dim(interpolation_time) <- N_steps
#dt <- user(1)
#initial(time) <- 1s
#deriv(time) <- (step + 1) * dt
N_steps <- user()
#steps_per_day <- 1/dt

# Vaccination
vax_time_step[,] <- interpolate(interpolation_time, vaccinations, "linear") 
dim(vax_time_step) <- c(n, n_vac)
dim(vaccinations) <- c(N_steps, n, n_vac)
vaccinations[,,] <- user()


N_imp[,,] <- interpolate(interpolation_time, import_vec, "linear")
dim(import_vec) <- c(N_steps, n, n_vac, n_strain)
dim(N_imp) <- c( n, n_vac, n_strain)
import_vec[,,,] <- user()
                                        # import

## Different beta modes:
## 1: input beta_day
## 2: Keep hospitalisation under/around threshold
## 3: Random walk on log_beta scale
## 4: Spontaneous behviour change
## 5: Cut peak


dim(beta) <- c(n, n_vac)
beta_mode <- user()

## Daily varying beta
dim(beta_day) <- c(N_steps,n)
beta_day[,] <- user()


# ##threshold based beta
# current_tot_in_hosp <- sum(H[,,]) + sum(ICU_H[,,]) + sum(ICU_R[,,]) + sum(ICU_P[,,])
# new_tot_in_hosp <- sum(H[,,]) + sum(ICU_H[,,]) + sum(ICU_R[,,]) + sum(ICU_P[,,]) + sum(n_IH[,,])  - sum(n_H[,,])  +sum(n_IICU[,,]) -sum(n_ICU_P[,,])
# current_infected <- sum(I[,,])
# new_infected <- sum(I[,,]) + sum(n_PI[,,]) - sum(n_I[,,])
# r <-  threshold[1]- current_tot_in_hosp
# dr_dt <- (current_tot_in_hosp - new_tot_in_hosp)/dt
# deriv(trigger) <- if(current_tot_in_hosp >threshold[2]) 1 else trigger
# new_beta_thresh <- if(trigger == 1) change_factor[1] + 1/threshold[1]*(change_factor[2]*r + change_factor[3]*e_int +  change_factor[4]*(dr_dt)) else beta_thresh
# deriv(beta_thresh) <- if(new_beta_thresh > threshold_max) threshold_max else ( if(new_beta_thresh < threshold_min) threshold_min else new_beta_thresh)
# dim(threshold) <- 2
# threshold[] <- user(100)
# change_factor[] <- user(1)
# dim(change_factor) <- 4

# ## Random walk
# dim(rand_beta_factors) <- n
# rand_beta_factors[] <- user()
# rand_beta_sd <- user(0.1)
# deriv(log_beta) <- if(step %% (rand_beta_days*steps_per_day)==0) log_beta + rnorm(0, rand_beta_sd) else (log_beta)

## Spontaneous behaviour change
## Here beta_day is treated as the "government policy" and the total effect is a combination 

dim(spont_behav_change_params) <- 6 # beta_0 beta without any interventions or regulation, T - time horizon, v - cost of isolation, a the speed of transition in logit function, parameter 1 for combing self reg and gov, param 2 for combining self reg and gov
spont_behav_change_params[] <- user(0)
dim(expected_health_loss) <- c(n, n_vac)
expected_health_loss[,] <- user(0)

# [0,1]
dim(contact_change) <- c(n, n_vac)


p_SE[,,] <- if(S[i,j] > 0 ) (n_EaA[i,j,k] + n_EsI[i,j,k])/S[i,j] else 0 #1 - exp(-sum(lambda_ij[i,j,,,])*symp_asymp_effect[i,j,k])
dim(p_SE) <- c(n, n_vac, n_strain)

kernel[,] <- spont_behav_change_params[4]*( (1-(1- sum(p_SE[i,j,]))^spont_behav_change_params[2]) * expected_health_loss[i,j] - spont_behav_change_params[2] *spont_behav_change_params[3])
dim(kernel) <- c(n, n_vac)

contact_change[,] <- if(kernel[i,j] > 15) 1 else (exp(kernel[i,j])/(1 + exp(kernel[i,j])))
output(contact_change) <- TRUE
#deriv(contact_change[,]) <- logit(spont_behav_change_params[4]*( (1-(1- sum(p_SE[i,j,])^spont_behav_change_params[2])) * expected_health_loss[i,j] - spont_behav_change_params[2] *spont_behav_change_params[3]))
dim(beta_reduction) <- c(n, n_vac)
beta_int[] <- interpolate(interpolation_time, beta_day, "linear")
dim(beta_int) <- n
beta_reduction[,] <- 1 - beta_int[i] /spont_behav_change_params[1] 


dim(beta_spont_behaviour) <- c(n,n_vac)

beta_spont_behaviour[,] <- spont_behav_change_params[1]*(1 - spont_behav_change_params[5]*(beta_reduction[i,j] + contact_change[i,j]) + spont_behav_change_params[6]*(beta_reduction[i,j]*contact_change[i,j]))
output(beta_spont_behaviour) <- TRUE

# ## Cut peak

# initial(beta_cut_peak) <-  beta_cut_peak_param[1] 
# deriv(beta_cut_peak) <- if(peak_trigger==1 && peak_timer < beta_cut_peak_param[4]) beta_cut_peak_param[2] else beta_cut_peak_param[1] 
# dim(beta_cut_peak_param) <- 4
# beta_cut_peak_param[] <- user(0)
# initial(trigger) <- 0
# initial(incidence_int) <- 0
# deriv(incidence_int) <- sum(n_SE[,,])
# initial(peak_trigger) <- 0
# initial(e_int) <- 0
# deriv(e_int) <- e_int + r*dt

# deriv(peak_trigger) <- if(sum(S[,])/sum(beta_norm[])< beta_cut_peak_param[3]) 1 else 0#if(time>20 && current_infected > beta_cut_peak_param[3] && sum(n_SE[,,]) < incidence_int) 1 else peak_trigger
# initial(peak_timer) <- 0
# deriv(peak_timer) <- if(peak_trigger==0) 0 else peak_timer + dt





beta[,] <- if(beta_mode==1) beta_int[i] else
          #   (if(beta_mode == 2) beta_thresh*rand_beta_factors[i] else
           #     (if(beta_mode == 3) exp(log_beta)*rand_beta_factors[i] else
                  ( if(beta_mode==4)  beta_spont_behaviour[i,j] else 0)#
                      #( if(beta_mode==5) beta_cut_peak*rand_beta_factors[i] 
                    #  else 0)#)#))
output(beta) <- TRUE
## Core equations for transitions between compartments:
dim(n_vac_help) <- c(n, n_vac)
n_vac_help[,1] <- 0#if(vax_type ==1) 0 else (rbinom(vax_time_step[i,1], S[i,1]/sum(S[i,])))
vax_type <- user(1)
n_vac_help[,2:n_vac] <- 0#if(vax_type==1) 0 else (rbinom(vax_time_step[i,1] - sum(n_vac_help[i, 1:(j - 1)]), S[i,j]/sum(S[i,j:n_vac])))

n_vac_now[,] <- if(vax_type==1) if (-vax_time_step[i,j] + sum(n_SE[i,j,])  > S[i,j]) - S[i,j] +  sum(n_SE[i,j,])  else round(vax_time_step[i,j]*S[i,1]/N[i,1]) else(
  if(j!=n_vac) if(n_vac_help[i,j] > S[i,j]) -S[i,j] else (-n_vac_help[i,j]) else (vax_time_step[i,1]-n_vac_help[i,j]))
#initial(sum_vac[]) <-0
#dim(sum_vac) <- n_vac
#deriv(sum_vac[]) <- sum(n_vac_now[,i])
#initial(sum_help_vac) <-0
#deriv(sum_help_vac) <- sum(n_vac_help[,])

N_imp_non_zero[,,] <- if(S[i,j] - sum(n_SE[i,j,]) +  n_vac_now[i,j]  -sum(N_imp[i,j,])<0) 0 else N_imp[i,j,k]
#N_imp_non_zero[,,] <- if(S[i,j] - sum(n_SE[i,j,]) +  n_vac_now[i,j] - n_waning[i,j] + waning_tmp[i,j] -sum(N_imp[i,j,])<0) 0 else N_imp[i,j,k]
 
dim(n_vac_now) <- c(n, n_vac)
dim(N_imp_non_zero) <- c(n, n_vac, n_strain)

vac_struct_length <- user(0)


index[] <- as.integer((i - 1)/vac_struct_length) + 1
dim(index) <- n_vac
waning_tmp[,] <- if(vac_struct_length == 0) sum(n_RS[i,j,]) else (
                     if(j %% vac_struct_length == 0) sum(n_RS[i,,index[j]]) else 0)

dim(waning_tmp) <- c(n, n_vac)

#deriv(S[,]) <-  S[i,j] - sum(n_SE[i,j,]) +  n_vac_now[i,j] - n_waning[i,j] + waning_tmp[i,j] -sum(N_imp_non_zero[i,j,])  #+ (sum(mig_S[i,])- S[i]/reg_pop_long[i] * sum(migration_matrix[1:n,i]))*dt + dt*S_waning[i]/waning_immunity_vax[i]*0   - n_imp[i] 
deriv(S[,]) <-  - sum(n_SE[i,j,]) +  n_vac_now[i,j] - -sum(N_imp_non_zero[i,j,]) + sum(n_RS[i,j,])  #+ (sum(mig_S[i,])- S[i]/reg_pop_long[i] * sum(migration_matrix[1:n,i]))*dt + dt*S_waning[i]/waning_immunity_vax[i]*0   - n_imp[i] 
#S[] <- if(S[i] <0 ) 0 S[i]

deriv(Ea[,,]) <-  n_SEa[i,j,k] - n_EaA[i,j,k] #+ n_RIA[i,j,k]# +  dt*(sum(mig_Ea[i,])- Ea[i]/reg_pop_long[i] * sum(migration_matrix[1:n,i]))

deriv(Es[,,]) <- n_SEi[i,j,k] - n_EsI[i,j,k] #+ n_RIS[i,j,k]# +   dt*( sum(mig_Es[i,])- Es[i]/reg_pop_long[i] * sum(migration_matrix[1:n,i]))

deriv(P[,,]) <-  n_EsI[i,j,k] - n_PI[i,j,k]# + dt*(sum(mig_P[i,])- P[i]/reg_pop_long[i] * sum(migration_matrix[1:n,i]))
deriv(I[,,]) <-   n_PI[i,j,k] - n_I[i,j,k] + N_imp_non_zero[i,j,k]# + dt*(sum(mig_I[i,])- I[i]/reg_pop_long[i] * sum(migration_matrix[1:n,i]))

#deriv(I_imp[]) <-  I_imp[i] -n_I_imp[i] +  n_imp[i]

deriv(A[,,]) <-  n_EaA[i,j,k] - n_AR[i,j,k]# + dt*(sum(mig_A[i,])- A[i]/reg_pop_long[i] * sum(migration_matrix[1:n,i]))
deriv(H[,,]) <-   n_IH[i,j,k]  - n_H[i,j,k]
deriv(ICU_H[,,]) <-  n_IICU[i,j,k] - n_ICU_HR[i,j,k]
deriv(ICU_R[,,]) <-  n_ICU_HR[i,j,k] - n_ICU_RP[i,j,k]

deriv(ICU_P[,,]) <- -n_ICU_P[i,j,k] +n_ICU_RP[i,j,k]

deriv(B_D[,,]) <-   n_ID[i,j,k] - n_B_D_D[i,j,k] 

deriv(B_D_H[,,]) <-  n_HD[i,j,k] - n_B_H_D[i,j,k]

deriv(B_D_ICU[,,]) <- n_ICU_D[i,j,k] - n_B_ICU_D[i,j,k]


deriv(R[,,]) <-  n_AR[i,j,k] + n_IR[i,j,k]  + n_ICU_R[i,j,k] + n_HR[i,j,k] - n_RS[i,j,k] #- n_RI[i,j,k]#+ dt*(sum(mig_R[i,])- R[i]/reg_pop_long[i] * sum(migration_matrix[1:n,i])) + n_MISCR[i]- n_I_PRE_MISC[i] + n_I_imp[i]

deriv(D[,,]) <-  n_B_D_D[i,j,k] + n_B_H_D[i,j,k] + n_B_ICU_D[i,j,k]

#fraction_of_vaccines_given_to_S[1:ng] <- S[i] /(S[i] + Ea[i]+ Es[i] + P[i] + I[i] + A[i] + H[i] + ICU_H[i] + ICU_R[i]+ICU_P[i]+ R[i] + B_D[i] + B_D_H[i] + B_D_ICU[i] + 1e-10)
#fraction_of_vaccines_given_to_S[(ng+1):(ng2)] <- fraction_of_vaccines_given_to_S[i - ng]
#fraction_of_vaccines_given_to_S[(ng2+1):(n)] <- fraction_of_vaccines_given_to_S[i - ng2]
#derivd(W[,]) <- W[i,j] + n_waning[i,j] - sum(n_WE[i,j,])


deriv(tot_infected[,,]) <-  n_SE[i,j,k]
  
#deriv(hosp_inc[]) <- if(step %% (incidence_steps_measurement*steps_per_day)==0) sum(n_IH[i,,]) + sum(n_IICU[i,,]) else hosp_inc[i]+sum(n_IH[i,,]) + sum(n_IICU[i,,])

#deriv(incidence[]) <-  if(step %% (incidence_steps_measurement*steps_per_day)==0) sum(n_SE[,,i]) else incidence[i]+sum(n_SE[,,i])
deriv(tot_hosp_inc) <- sum(n_IH[,,]) + sum(n_IICU[,,])
deriv(tot_hosp[,,]) <-  n_IH[i,j,k] + n_IICU[i,j,k]

deriv(tot_resp[,,]) <- n_ICU_HR[i,j,k]
deriv(tot_vac[,]) <- n_vac_now[i,j]
deriv(tot_vac_adm[,]) <- vax_time_step[i,j]
#deriv(tot_misc[]) <- tot_misc[i] + n_PRE_MISC[i]
#deriv(tot_imp[]) <- n_imp[i]




                                        #Transition probs:
                                        #S - > Ea , Ei
symp_asymp_effect[,,] <- (asympt_frac[i,j,k]*susceptibility_asymp[i,j,k] + sympt_frac[i,j,k]*susceptibility_symp[i,j,k])
dim(symp_asymp_effect) <- c(n, n_vac, n_strain)
#p_full_effect[] <- 1 - exp(-dt/vac_time_full_effect[i,j,k])
#p_waning <- 1 - exp(-dt/waning_immunity)

#dim(p_full_effect) <- c(n,n_vac,n_strain)
#dim(p_MISCR) <- (n,n_vac,n_strain)

## HARD CODED 2 strains
n_SE_tot[,] <- S[i,j]*sum(lambda_ij[i,j,,,])*sum(symp_asymp_effect[i,j,])
#rel_strain[,,] <- if(n_strain==1) 1 else (p_SE[i,j,k]/sum(p_SE[i,j,]))
n_SE[,,] <- n_SE_tot[i,j]#if(k==1 || n_strain==1) n_SE_tot[i,j] else
          # (if (n_strain==2) n_SE_tot[i,j] - n_SE[i,j,1] else (
           #     if(k==2) rbinom(n_SE_tot[i,j] - n_SE[i,j,1], rel_strain[i,j,2]/(rel_strain[i,j,2] + rel_strain[i,j,3])) else(
            #                                                                    if (k==3) n_SE_tot[i,j] - n_SE[i,j,2] - n_SE[i,j,1]
             #                                                                                                       else 0)))

#n_RI[,,] <- 0#if( n_strain==1) 0 else
             #(if (k==1) rbinom(R[i,j,1], 1 - exp(-sum(lambda_ij[i,j,,,1])*cross_protection[1,2]*symp_asymp_effect[i,j,k]* dt)) else (if (k==2)  rbinom(R[i,j,2], 1 - exp(-sum(lambda_ij[i,j,,,1])*cross_protection[2,1]*symp_asymp_effect[i,j,k] * dt)) else 0))
#n_RI_op[,,] <- 0 #if(n_strain==1) 0 else
                 #                  ( if(k==1) n_RI[i,j,2] else n_RI[i,j,1])


#n_RIA[,,] <- 0 #rbinom(n_RI_op[i,j,k], pa[i,j,k])
#n_RIS[,,] <- 0 #n_RI_op[i,j,k] - n_RIA[i,j,k]



n_IR[,,] <- I[i,j,k]/infectious_period


n_SEa[,,] <-n_SE[i,j,k]*pa[i,j,k]



dim(n_SE_tot) <- c(n,n_vac)
#dim(rel_strain) <- c(n,n_vac,n_strain)

# For waning_type=2
#n_waning_tmp[,] <- rbinom(S[i,j] - sum(n_SE[i,j,  ]) , p_waning[i,j]) 
##dim(n_waning_tmp) <- c(n,n_vac)
#dim(n_waning) <- c(n,n_vac)

#n_waning[,] <- if(include_waning==1) if(j != n_vac) rbinom(S[i,j], p_waning[i,j]) else(-sum(n_waning[i, 1:(j-1)])) else ( 
#   if(include_waning==2) if(j==1) - n_waning_tmp[i, j+1] else( if(j != n_vac) n_waning_tmp[i,j] - n_waning_tmp[i, j+1] else (n_waning_tmp[i,j]))
#  else 0)
 
pa[,,] <- asympt_frac[i,j,k]*susceptibility_asymp[i,j,k]/(asympt_frac[i,j,k]*susceptibility_asymp[i,j,k] + sympt_frac[i,j,k]*susceptibility_symp[i,j,k])

dim(pa) <- c(n, n_vac, n_strain)
n_SEi[,,] <- n_SE[i,j,k] -n_SEa[i,j,k]

n_EaA[,,] <- Ea[i,j,k]/latent_period

n_EsI[,,] <- Es[i,j,k]/latent_period
n_PI[,,] <- P[i,j,k]/pre_sympt_period

n_AR[,,] <- A[i,j,k]/infectious_period
n_I[,,] <- I[i,j,k]/infectious_period
#n_I_imp[] <- rbinom(I_imp[i], p_IR)


# nested binomial
n_IR[,,] <- n_I[i,j,k]*(1-hosp_prob[i,j,k])*(1-prob_death_non_hosp[i,j,k])

n_IH[,,] <- n_I[i,j,k]* hosp_prob[i,j,k]*(1-icu_prob[i,j,k])

n_IICU[,,] <- n_I[i,j,k]*hosp_prob[i,j,k]*icu_prob[i,j,k]

n_ID[,,] <- n_I[i,j,k]*(1-hosp_prob[i,j,k])*prob_death_non_hosp[i,j,k]


## n_I_PRE_MISC[] <- rbinom(n_IR[i,j,k], prob_misc[i,j,k])
## n_PRE_MISC[] <- rbinom(PRE_MISC[i,j,k], 1 - exp(-dt/time_to_misc))
## n_PRE_MISC_H[] <- rbinom(n_PRE_MISC[i,j,k], 1 - misc_icu[i,j,k])
## n_PRE_MISC_ICU_H[] <- n_PRE_MISC[i] - n_PRE_MISC_H[i]
## n_MISC_ICU[] <- rbinom(MISC_ICU_H[i], 1 - exp(-dt/misc_icu_pre))
## n_MISC_ICU_R[] <- rbinom(MISC_ICU[i], 1 - exp(-dt/misc_icu_length))
## n_MISCR[] <- rbinom(MISC[i], p_MISCR[i])


n_H[,,] <- H[i,j,k]/length_hosp[i,j,k]

n_HD[,,] <- n_H[i,j,k]*prob_death_hosp[i,j,k]
n_HR[,,] <- n_H[i,j,k] - n_HD[i,j,k]

n_ICU_HR[,,] <- ICU_H[i,j,k]/pre_icu[i,j,k]
n_ICU_RP[,,] <- ICU_R[i,j,k]/length_icu[i,j,k]
n_ICU_P[,,] <-  ICU_P[i,j,k]/post_icu[i,j,k]

n_ICU_D[,,] <- n_ICU_P[i,j,k]*prob_death_icu[i,j,k]
n_ICU_R[,,] <- n_ICU_P[i,j,k] - n_ICU_D[i,j,k]

n_B_D_D[,,] <- B_D[i,j,k]/time_before_death
n_B_H_D[,,] <- B_D_H[i,j,k]/time_before_death_hosp
n_B_ICU_D[,,] <- B_D_ICU[i,j,k]/time_before_death_icu

n_RS[,,] <- R[i,j,k]/waning_inf


dim(n_SE)<- c(n, n_vac, n_strain)
dim(n_SEi)<- c(n, n_vac, n_strain)
dim(n_SEa)<- c(n, n_vac, n_strain)
dim(n_EsI)<- c(n, n_vac, n_strain)
dim(n_EaA)<- c(n, n_vac, n_strain)
dim(n_PI)<- c(n, n_vac, n_strain)
dim(n_AR)<- c(n, n_vac, n_strain)
dim(n_I)<- c(n, n_vac, n_strain)
#dim(n_I_imp)<- c(n, n_vac, n_strain)
dim(n_IH)<- c(n, n_vac, n_strain)
dim(n_IICU)<- c(n, n_vac, n_strain)
dim(n_IR)<- c(n, n_vac, n_strain)
dim(n_ID)<- c(n, n_vac, n_strain)
dim(n_H)<- c(n, n_vac, n_strain)
dim(n_HD)<- c(n, n_vac, n_strain)
dim(n_HR)<- c(n, n_vac, n_strain)
dim(n_ICU_RP)<- c(n, n_vac, n_strain)
dim(n_ICU_P)<- c(n, n_vac, n_strain)
dim(n_ICU_HR)<- c(n, n_vac, n_strain)
dim(n_ICU_D)<- c(n, n_vac, n_strain)
dim(n_ICU_R)<- c(n, n_vac, n_strain)
dim(n_B_D_D)<- c(n, n_vac, n_strain)
dim(n_B_H_D)<- c(n, n_vac, n_strain)
dim(n_B_ICU_D)<- c(n, n_vac, n_strain)
dim(n_RS)<- c(n, n_vac, n_strain)
#dim(n_I_PRE_MISC)<- c(n, n_vac, n_strain)
#dim(n_PRE_MISC)<- c(n, n_vac, n_strain)
#dim(n_MISCR)<- c(n, n_vac, n_strain)
#dim(n_PRE_MISC_H)<- c(n, n_vac, n_strain)
#dim(n_PRE_MISC_ICU_H)<- c(n, n_vac, n_strain)
#dim(n_MISC_ICU)<- c(n, n_vac, n_strain)
#dim(n_MISC_ICU_R)<- c(n, n_vac, n_strain)



## Total population size (odin will recompute this at each timestep:
## automatically)
#initial(N[,]) <- S_ini[i,j]  + sum(Ea_ini[i,j,])+ sum(Es_ini[i,j,]) + sum(P_ini[i,j,]) +  sum(I_ini[i,j,]) + sum(A_ini[i,j,]) + sum(H_ini[i,j,]) + sum(ICU_H_ini[i,j,]) + sum(ICU_R_ini[i,j,]) + sum(ICU_P_ini[i,j,]) + sum(B_D_ini[i,j,]) + sum(B_D_ICU_ini[i,j,]) + sum(B_D_H_ini[i,j,]) + sum(R_ini[i,j,]) + sum(D_ini[i,j,])

N[,] <- S[i,j]  + sum(Ea[i,j,])+ sum(Es[i,j,]) + sum(P[i,j,]) +  sum(I[i,j,]) + sum(A[i,j,]) + sum(H[i,j,]) + sum(ICU_H[i,j,]) + sum(ICU_R[i,j,]) + sum(ICU_P[i,j,]) + sum(B_D[i,j,]) + sum(B_D_ICU[i,j,]) + sum(B_D_H[i,j,]) + sum(R[i,j,]) + sum(D[i,j,])


tot_N <- sum(N[,])

output(N) <- TRUE
output(tot_N) <- TRUE

                                        # Transitions

lambda_ij[,,,,] <- beta[i,j]*beta_strain[i5] * mixing_matrix[i,k]/beta_norm[i]*transmisibility[k,l,i5]*(pre_sympt_infect*P[k,l,i5] + symp_trans[k,l,i5]*(I[k,l,i5] ) + asympt_infect*A[k,l,i5]) #I_imp[j]*susceptibility[i,j,i5]

#initial(lambda[,,,,]) <- 0
#deriv(lambda[,,,,]) <- lambda_ij[i,j,k,l,i5]
#dim(lambda) <-  c(n, n_vac, n, n_vac, n_strain)
#output(lambda_ij) <- TRUE

## # Migrations
## mig_S[,] <- migration_matrix[i,j]*S[j]/reg_pop_long[j]
## mig_Ea[,] <- migration_matrix[i,j]*Ea[j]/reg_pop_long[j]
## mig_Es[,] <- migration_matrix[i,j]*Es[j]/reg_pop_long[j]
## mig_P[,] <- migration_matrix[i,j]*P[j]/reg_pop_long[j]
## mig_I[,] <- migration_matrix[i,j]*I[j]/reg_pop_long[j]
## mig_A[,] <- migration_matrix[i,j]*A[j]/reg_pop_long[j]
## mig_R[,] <- migration_matrix[i,j]*R[j]/reg_pop_long[j]

## Initial states:
#initial(log_beta) <- log_beta_ini
#initial(beta_thresh) <- threshold_ini
initial(S[,]) <- S_ini[i,j] # will be user-defined
initial(Ea[,,]) <-  Ea_ini[i,j,k]
initial(Es[,,]) <-  Es_ini[i,j,k]
initial(P[,,]) <-  P_ini[i,j,k]
initial(I[,,]) <-  I_ini[i,j,k]
#initial(I_imp[,,]) <-  I_imp_ini[i,j,k]
initial(A[,,]) <-  A_ini[i,j,k]
initial(H[,,]) <-  H_ini[i,j,k]
initial(ICU_H[,,]) <-  ICU_H_ini[i,j,k]
initial(ICU_R[,,]) <- ICU_R_ini[i,j,k]
initial(ICU_P[,,]) <- ICU_P_ini[i,j,k]
initial(B_D[,,]) <- B_D_ini[i,j,k]
initial(B_D_H[,,]) <- B_D_H_ini[i,j,k]
initial(B_D_ICU[,,]) <- B_D_ICU_ini[i,j,k]
#initial(PRE_MISC[,,]) <- PRE_MISC_ini[i,j,k]
#initial(MISC[,,]) <- MISC_ini[i,j,k]
#initial(MISC_ICU_H[,,]) <- MISC_ICU_H_ini[i,j,k]
#initial(MISC_ICU[,,]) <- MISC_ICU_ini[i,j,k]
initial(R[,,]) <-  R_ini[i,j,k]
initial(D[,,]) <-  D_ini[i,j,k]
#initial(Ni[,,]) <-N[i,j,k] 
#initial(tot_N) <- 0
#initial(hosp_inc[]) <- 0
#initial(incidence[]) <- 0
initial(tot_hosp_inc) <- 0
initial(tot_infected[,,]) <- tot_infected_ini[i,j,k]
initial(tot_hosp[,,]) <- tot_hosp_ini[i,j,k]
initial(tot_resp[,,]) <- tot_resp_ini[i,j,k]
initial(tot_vac[,]) <- tot_vac_ini[i,j]
initial(tot_vac_adm[,]) <- tot_vac_adm_ini[i,j]
#initial(tot_misc[,,]) <- tot_misc_ini[i,j,k]
#initial(tot_imp[,,]) <- 0
#initial(log_beta) <- 0

## User defined parameters - default in parentheses:


dim(S)<- c(n, n_vac)
dim(Ea)<- c(n, n_vac, n_strain)
dim(Es)<- c(n, n_vac, n_strain)
dim(P)<- c(n, n_vac, n_strain)
dim(I)<- c(n, n_vac, n_strain)
#dim(I_imp)<- c(n, n_vac, n_strain)
dim(A)<- c(n, n_vac, n_strain)
dim(R)<- c(n, n_vac, n_strain)
dim(H)<- c(n, n_vac, n_strain)
dim(ICU_H)<- c(n, n_vac, n_strain)
dim(ICU_R)<- c(n, n_vac, n_strain)
dim(ICU_P)<- c(n, n_vac, n_strain)
dim(B_D)<- c(n, n_vac, n_strain)
dim(B_D_H)<- c(n, n_vac, n_strain)
dim(B_D_ICU)<- c(n, n_vac, n_strain)
#dim(MISC)<- c(n, n_vac, n_strain)
#dim(MISC_ICU)<- c(n, n_vac, n_strain)
#dim(MISC_ICU_H)<- c(n, n_vac, n_strain)
#dim(PRE_MISC)<- c(n, n_vac, n_strain)
dim(D)<- c(n, n_vac, n_strain)
#dim(hosp_inc) <- n
#dim(incidence) <- n
dim(N)<- c(n, n_vac)
dim(tot_infected)<- c(n, n_vac, n_strain)
dim(tot_hosp)<- c(n, n_vac, n_strain)
dim(tot_resp)<- c(n, n_vac, n_strain)
dim(tot_vac)<- c(n, n_vac)
dim(tot_vac_adm)<- c(n, n_vac)
#dim(tot_misc)<- c(n, n_vac, n_strain)
#dim(tot_imp)<- c(n, n_vac, n_strain)
#dim(fraction_of_vaccines_given_to_S)<- c(n, n_vac, n_strain)


## dim(mig_S) <- c(n,n)
## dim(mig_Ea) <- c(n,n)
## dim(mig_Es) <- c(n,n)
## dim(mig_P) <- c(n,n)
## dim(mig_I) <- c(n,n)
## dim(mig_A) <- c(n,n)
## dim(mig_R) <- c(n,n)




#dim(N)<- c(n, n_vac, n_strain)
dim(S_ini)<- c(n, n_vac)
dim(Ea_ini)<- c(n, n_vac, n_strain)
dim(Es_ini)<- c(n, n_vac, n_strain)
dim(P_ini)<- c(n, n_vac, n_strain)
dim(I_ini)<- c(n, n_vac, n_strain)
dim(I_imp_ini)<- c(n, n_vac, n_strain)
dim(A_ini)<- c(n, n_vac, n_strain)
dim(R_ini)<- c(n, n_vac, n_strain)
dim(H_ini)<- c(n, n_vac, n_strain)
dim(ICU_H_ini)<- c(n, n_vac, n_strain)
dim(ICU_R_ini)<- c(n, n_vac, n_strain)
dim(ICU_P_ini)<- c(n, n_vac, n_strain)
dim(B_D_ini)<- c(n, n_vac, n_strain)
dim(B_D_H_ini)<- c(n, n_vac, n_strain)
dim(B_D_ICU_ini)<- c(n, n_vac, n_strain)
#dim(MISC_ini)<- c(n, n_vac, n_strain)
#dim(MISC_ICU_H_ini)<- c(n, n_vac, n_strain)
#dim(MISC_ICU_ini)<- c(n, n_vac, n_strain)
#dim(PRE_MISC_ini)<- c(n, n_vac, n_strain)
dim(D_ini)<- c(n, n_vac, n_strain)

dim(tot_infected_ini)<- c(n, n_vac, n_strain)
dim(tot_hosp_ini)<- c(n, n_vac, n_strain)
dim(tot_resp_ini)<- c(n, n_vac, n_strain)
dim(tot_vac_ini)<- c(n, n_vac)
dim(tot_vac_adm_ini)<- c(n, n_vac)
#dim(tot_misc_ini)<- c(n, n_vac, n_strain)

#dim(reg_pop_long)<- c(n, n_vac, n_strain)
dim(beta_norm)<- n
dim(mixing_matrix) <- c(n, n)
dim(lambda_ij) <- c(n, n_vac, n, n_vac, n_strain)
#dim(migration_matrix) <- c(n,n)
dim(length_hosp)<- c(n, n_vac, n_strain)
dim(hosp_prob)<- c(n, n_vac, n_strain)
dim(length_icu)<- c(n, n_vac, n_strain)
dim(pre_icu)<- c(n, n_vac, n_strain)
dim(post_icu)<- c(n, n_vac, n_strain)
dim(icu_prob)<- c(n, n_vac, n_strain)
dim(prob_death_non_hosp)<- c(n, n_vac, n_strain)
dim(prob_death_hosp)<- c(n, n_vac, n_strain)
dim(prob_death_icu)<- c(n, n_vac, n_strain)
#dim(susceptibility)<- c(n, n_vac, n_strain)
dim(transmisibility)<- c(n, n_vac, n_strain)
dim(waning_immunity_vax)<- c(n, n_vac, n_strain)
#dim(prob_misc)<- c(n, n_vac, n_strain)
#dim(misc_length)<- c(n, n_vac, n_strain)
dim(susceptibility_asymp)<- c(n, n_vac, n_strain)
dim(susceptibility_symp)<- c(n, n_vac, n_strain)
#dim(misc_icu)<- c(n, n_vac, n_strain)
dim(asympt_frac)<- c(n, n_vac, n_strain)
dim(sympt_frac)<- c(n, n_vac, n_strain)
dim(symp_trans)<- c(n, n_vac, n_strain)
dim(T_waning)<- c(n, n_vac)
dim(beta_strain) <- n_strain
include_waning <- user(0)
mixing_matrix[,] <- user()
#migration_matrix[,] <- user()
latent_period <- user()
infectious_period <- user()
length_hosp[,,] <- user()
length_icu[,,] <- user()
hosp_prob[,,] <- user()
icu_prob[,,] <- user()
pre_icu[,,] <- user()
post_icu[,,] <- user()
#time_to_misc <- user()
#prob_misc[,,] <- user()


time_before_death <- user()
time_before_death_hosp <- user()
time_before_death_icu <- user()



#susceptibility[,,] <- user()
transmisibility[,,] <- user()
susceptibility_asymp[,,] <- user()
susceptibility_symp[,,] <- user()
symp_trans[,,] <- user()
threshold_ini <- user(0.1)
threshold_max <- user(0.2)
threshold_min <- user(0.05)


n_vac <- user()
n_strain <- user()
T_waning[,] <- user()
waning_inf <- user()
prob_death_non_hosp[,,] <- user()
prob_death_hosp[,,] <- user()
prob_death_icu[,,] <- user()
waning_immunity_vax[,,] <- user()
waning_immunity <- user(10000)

sympt_frac[,,] <- user()
asympt_frac[,,] <- user()
pre_sympt_infect <- user()
asympt_infect <- user()

#vac_time <- user()
#vac_time_full_effect_1 <- user()
rand_beta_days <- user(1)
incidence_steps_measurement <- user(1)

pre_sympt_period <- user()


dim(cross_protection) <- c(n_strain, n_strain)
cross_protection[,] <- user()
n <- user(198)
S_ini[,] <- user()
I_ini[,,] <- user()
I_imp_ini[,,] <- user(0)
Ea_ini[,,] <- user(0)
Es_ini[,,] <- user(0)
A_ini[,,] <- user(0)
P_ini[,,] <- user(0)
beta_strain[] <- user()
H_ini[,,] <- user(0)
ICU_H_ini[,,] <- user(0)
ICU_R_ini[,,] <- user(0)
ICU_P_ini[,,] <- user(0)
B_D_ini[,,] <- user(0)
B_D_H_ini[,,] <- user(0)
B_D_ICU_ini[,,] <- user(0)
R_ini[,,] <- user(0)
D_ini[,,] <- user(0)
tot_infected_ini[,,] <- user(0)
tot_hosp_ini[,,] <- user(0)
tot_resp_ini[,,] <- user(0)
tot_vac_ini[,] <- user(0)
tot_vac_adm_ini[,] <- user(0)
#reg_pop_long[,,] <- user()
beta_norm[] <- user(0)
log_beta_ini <- user()
#MISC_ini[,,] <- user(0)
#MISC_ICU_ini[,,] <- user(0)
#MISC_ICU_H_ini[,,] <- user(0)
#PRE_MISC_ini[,,] <- user(0)
#tot_misc_ini[,,] <- user(0)
