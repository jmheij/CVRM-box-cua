
#### 1. General ####

#Notes: 
# A. This script contains all the necessary custom functions used to run the model. Fuctions 5-8 are to run the subgroup-specific models. Also note that the wrapper functions (15 & 16) are necessary for running functions 5-14 (see app.R script). 
# B. In this R script, care as usual is abbreviated as "SOC" or "SoC" or "CaU". While the intervention is abbreviated with "CVRM-Box" or "Strategy A".


#### 2. SCORE2 model predictions  ####

# Function to calculate SCORE2 and SCORE2-OP probs 
gen_score_tprobs <- function(intervention, subgroup) {
  
  #define necessary predictor values depending on the subgroup
  if(intervention == "SOC" & subgroup == "controlled_nohis") {
    score_smoke_m = 0.1
    score_sbp_m = 126
    score_tchol_m = 4.9
    score_hdl_m = 1.2
    score_dm_m = 0.23
    score_smoke_f = 0.08
    score_sbp_f = 124.2
    score_tchol_f = 5.3
    score_hdl_f = 1.5
    score_dm_f = 0.2
    sbp_diff_tmp = 0
    stop_smoke_tmp = 0
    
    score_op_smoke_m = 0.06
    score_op_sbp_m = 127.1
    score_op_tchol_m = 4.45
    score_op_hdl_m = 1.29
    score_op_dm_m = 0.27
    score_op_smoke_f = 0.05
    score_op_sbp_f = 127.3
    score_op_tchol_f = 5.1
    score_op_hdl_f = 1.55
    score_op_dm_f = 0.21
    sbp_diff_tmp = 0
    stop_smoke_tmp = 0
    
  }else{
    if(intervention == "SOC" & subgroup == "uncontrolled_nohis"){
      score_smoke_m = 0.1
      score_sbp_m = 152.1
      score_tchol_m = 5.1
      score_hdl_m = 1.21
      score_dm_m = 0.19
      score_smoke_f = 0.08
      score_sbp_f = 151.5
      score_tchol_f = 5.4
      score_hdl_f = 1.5
      score_dm_f = 0.14
      sbp_diff_tmp = 0
      stop_smoke_tmp = 0
      
      score_op_smoke_m = 0.05
      score_op_sbp_m = 153.4
      score_op_tchol_m = 4.66
      score_op_hdl_m = 1.31
      score_op_dm_m = 0.25
      score_op_smoke_f = 0.04
      score_op_sbp_f = 154.8
      score_op_tchol_f = 5.28
      score_op_hdl_f = 1.57
      score_op_dm_f = 0.2
      sbp_diff_tmp = 0
      stop_smoke_tmp = 0
    }else{
      if(intervention == "Box" & subgroup == "controlled_nohis"){
        score_smoke_m = 0.1
        score_sbp_m = 126
        score_tchol_m = 4.9
        score_hdl_m = 1.2
        score_dm_m = 0.23
        score_smoke_f = 0.08
        score_sbp_f = 124.2
        score_tchol_f = 5.3
        score_hdl_f = 1.5
        score_dm_f = 0.2
        sbp_diff_tmp = sbp_diff_controlled
        stop_smoke_tmp = stop_smoke
        
        score_op_smoke_m = 0.06
        score_op_sbp_m = 127.1
        score_op_tchol_m = 4.45
        score_op_hdl_m = 1.29
        score_op_dm_m = 0.27
        score_op_smoke_f = 0.05
        score_op_sbp_f = 127.3
        score_op_tchol_f = 5.1
        score_op_hdl_f = 1.55
        score_op_dm_f = 0.21
        sbp_diff_tmp = sbp_diff_controlled
        stop_smoke_tmp = stop_smoke
      }else{
        if(intervention == "Box" & subgroup == "uncontrolled_nohis"){
          score_smoke_m = 0.1
          score_sbp_m = 152.1
          score_tchol_m = 5.1
          score_hdl_m = 1.21
          score_dm_m = 0.19
          score_smoke_f = 0.08
          score_sbp_f = 151.5
          score_tchol_f = 5.4
          score_hdl_f = 1.5
          score_dm_f = 0.14
          sbp_diff_tmp = sbp_diff_uncontrolled
          stop_smoke_tmp = stop_smoke
          
          score_op_smoke_m = 0.05
          score_op_sbp_m = 153.4
          score_op_tchol_m = 4.66
          score_op_hdl_m = 1.31
          score_op_dm_m = 0.25
          score_op_smoke_f = 0.04
          score_op_sbp_f = 154.8
          score_op_tchol_f = 5.28
          score_op_hdl_f = 1.57
          score_op_dm_f = 0.2
          sbp_diff_tmp = sbp_diff_uncontrolled
          stop_smoke_tmp = stop_smoke
        }
      }
    }
  }
  sbp_diff_tmp0 <- sbp_diff_tmp
  stop_smoke_tmp0 <- stop_smoke_tmp
  
  m_tmp <- m_tmp_MI1 <- m_tmp_S1 <- m_tmp_CVD <- data.frame(matrix(NA, nrow = n_sim, ncol = n_cycles))
  colnames(m_tmp) <- colnames(m_tmp_MI1) <- colnames(m_tmp_S1) <- colnames(m_tmp_CVD) <- 1:n_cycles
  
  age_tmp <- n_age_init
  for (cycle in 1:n_cycles) {
    if(cycle >= year_reduction_effect){
      sbp_diff_tmp <- sbp_diff_tmp0*(1-percentage_reduction_effect)
      stop_smoke_tmp <- stop_smoke_tmp0*(1-percentage_reduction_effect)
    }
    age_tmp1 <- n_age_init + cycle #for updating of proportions (event type)
    if(cycle %% 10 == 0){
      age_tmp <- n_age_init + cycle #for ensuring 10y risk remains constant for 10yrs
    }
    
    if(age_tmp1 >= 20 & age_tmp1 < 45) {
      score_prop_MI1 <- prob_nonfatal_MI_men_20
      score_prop_S1 <- prob_nonfatal_stroke_men_20
      score_prop_CVD <- prob_fatal_event_men_20
    }
    if(age_tmp1 >= 45 & age_tmp1 < 65) {
      score_prop_MI1 <- prob_nonfatal_MI_men_45
      score_prop_S1 <- prob_nonfatal_stroke_men_45
      score_prop_CVD <- prob_fatal_event_men_45
    }
    if(age_tmp1 >= 65 & age_tmp1 < 80) {
      score_prop_MI1 <- prob_nonfatal_MI_men_65
      score_prop_S1 <- prob_nonfatal_stroke_men_65
      score_prop_CVD <- prob_fatal_event_men_65
    }
    if(age_tmp1 >= 80) {
      score_prop_MI1 <- prob_nonfatal_MI_men_80
      score_prop_S1 <- prob_nonfatal_stroke_men_80
      score_prop_CVD <- prob_fatal_event_men_80
    }
    if(age_tmp < 70){
      scale1_m <- -0.5699 
      scale2_m <- 0.7476
      m_tmp[,cycle] <- (1 - 0.9605^exp(
        0.3742 * ((age_tmp - 60) / 5) +
          0.6012 * (score_smoke_m-(score_smoke_m*stop_smoke_tmp)) +
          0.2777 * (((score_sbp_m-sbp_diff_tmp) - 120) / 20) +
          0.1458 * ((score_tchol_m - 6) / 1) +
          -0.2698 * ((score_hdl_m - 1.3) / 0.5) +
          -0.0755 * (((age_tmp - 60) / 5) * (score_smoke_m-(score_smoke_m*stop_smoke_tmp))) +
          -0.0255 * (((age_tmp - 60) / 5) * (((score_sbp_m-sbp_diff_tmp) - 120) / 20)) +
          -0.0281 * (((age_tmp - 60) / 5) * ((score_tchol_m - 6) / 1)) +
          0.0426 * (((age_tmp - 60) / 5) * ((score_hdl_m - 1.3) / 0.5))
      ))
      m_tmp[,cycle] <- 1 - exp(-exp(scale1_m + scale2_m * log(-log(1 - m_tmp[,cycle]))))
    }else{
      scale1_m <- -0.61
      scale2_m <- 0.89
      m_tmp[,cycle] <- (1 - 0.7576^exp((
        0.0634 * (age_tmp - 73) +
          0.4245 * score_op_dm_m +
          0.3524 * (score_op_smoke_m-(score_op_smoke_m*stop_smoke_tmp)) +
          0.0094 * ((score_op_sbp_m-sbp_diff_tmp) - 150) +
          0.0850 * (score_op_tchol_m - 6) +
          -0.3564 * (score_op_hdl_m - 1.4) +
          -0.0174 * ((age_tmp - 73) * score_op_dm_m) +
          -0.0247 * ((age_tmp - 73) * (score_op_smoke_m-(score_op_smoke_m*stop_smoke_tmp))) +
          -0.0005 * ((age_tmp - 73) *  ((score_op_sbp_m-sbp_diff_tmp) - 150)) +
          0.0073 * ((age_tmp - 73) *  (score_op_tchol_m - 6)) +
          0.0091 * ((age_tmp - 73) *  (score_op_hdl_m - 1.4))
      )-0.0929))
      m_tmp[,cycle] <- 1 - exp(-exp(scale1_m + scale2_m * log(-log(1 - m_tmp[,cycle]))))
    }
    m_tmp[,cycle] <- -(1/10)*log(1-m_tmp[,cycle]) 
    m_tmp[,cycle] <- rate_to_prob(r = m_tmp[,cycle], t=cycle_length) 
    m_tmp_MI1[,cycle] <- m_tmp[,cycle]*score_prop_MI1
    m_tmp_S1[,cycle] <- m_tmp[,cycle]*score_prop_S1
    m_tmp_CVD[,cycle] <- m_tmp[,cycle]*score_prop_CVD
  }
  
  f_tmp <- f_tmp_MI1 <- f_tmp_S1 <- f_tmp_CVD <- data.frame(matrix(NA, nrow = n_sim, ncol = n_cycles))
  colnames(f_tmp) <- colnames(f_tmp_MI1) <- colnames(f_tmp_S1) <- colnames(f_tmp_CVD) <- 1:n_cycles
  
  age_tmp <- n_age_init
  for (cycle in 1:n_cycles) {
    age_tmp1 <- n_age_init + cycle #for updating of proportions (event type)
    if(cycle %% 10 == 0){
      age_tmp <- n_age_init + cycle #for updating which score model to use
    }
    if(age_tmp1 >= 20 & age_tmp1 < 45) {
      score_prop_MI1 <- prob_nonfatal_MI_women_20
      score_prop_S1 <- prob_nonfatal_stroke_women_20
      score_prop_CVD <- prob_fatal_event_women_20
    }
    if(age_tmp1 >= 45 & age_tmp1 < 65) {
      score_prop_MI1 <- prob_nonfatal_MI_women_45
      score_prop_S1 <- prob_nonfatal_stroke_women_45
      score_prop_CVD <- prob_fatal_event_women_45
    }
    if(age_tmp1 >= 65 & age_tmp1 < 80) {
      score_prop_MI1 <- prob_nonfatal_MI_women_65
      score_prop_S1 <- prob_nonfatal_stroke_women_65
      score_prop_CVD <- prob_fatal_event_women_65
    }
    if(age_tmp1 >= 80) {
      score_prop_MI1 <- prob_nonfatal_MI_women_80
      score_prop_S1 <- prob_nonfatal_stroke_women_80
      score_prop_CVD <- prob_fatal_event_women_80
    }
    if(age_tmp < 70){  
      scale1_f <- -0.7380
      scale2_f <- 0.7019
      f_tmp[,cycle] <- (1 - 0.9776^exp(
        0.4648 * ((age_tmp - 60) / 5) +
          0.7744 * (score_smoke_f-(score_smoke_f*stop_smoke_tmp)) +
          0.3131 * (((score_sbp_f-sbp_diff_tmp) - 120) / 20) +
          0.1002 * ((score_tchol_f - 6) / 1) +
          -0.2606 * ((score_hdl_f - 1.3) / 0.5) +
          -0.1088 * (((age_tmp - 60) / 5) * (score_smoke_f-(score_smoke_f*stop_smoke_tmp))) +
          -0.0277 * (((age_tmp - 60) / 5) * (((score_sbp_f-sbp_diff_tmp) - 120) / 20)) +
          -0.0226 * (((age_tmp - 60) / 5) * ((score_tchol_f - 6) / 1)) +
          0.0613 * (((age_tmp - 60) / 5) * ((score_hdl_f - 1.3) / 0.5))
      ))
      f_tmp[,cycle] <- 1 - exp(-exp(scale1_f + scale2_f * log(-log(1 - f_tmp[,cycle]))))
    }else{
      scale1_f <- -0.85
      scale2_f <- 0.82
      f_tmp[,cycle] <- (1 - 0.8082^exp((
        0.0789 * (age_tmp - 73) +
          0.6010 * score_op_dm_f +
          0.4921 * (score_op_smoke_f-(score_op_smoke_f*stop_smoke_tmp)) +
          0.0102 * ((score_op_sbp_f-sbp_diff_tmp) - 150) +
          0.0605 * (score_op_tchol_f - 6) +
          -0.3040 * (score_op_hdl_f - 1.4) +
          -0.0107 * ((age_tmp - 73) * score_op_dm_f) +
          -0.0255 * ((age_tmp - 73) * (score_op_smoke_f-(score_op_smoke_f*stop_smoke_tmp))) +
          -0.0004 * ((age_tmp - 73) *  ((score_op_sbp_f-sbp_diff_tmp) - 150)) +
          -0.0009 * ((age_tmp - 73) *  (score_op_tchol_f - 6)) +
          0.0154 * ((age_tmp - 73) *  (score_op_hdl_f - 1.4))
      )-0.229))
      f_tmp[,cycle] <- 1 - exp(-exp(scale1_f + scale2_f * log(-log(1 - f_tmp[,cycle]))))
    }
    f_tmp[,cycle] <- -(1/10)*log(1-f_tmp[,cycle]) #annual rate
    f_tmp[,cycle] <- rate_to_prob(r = f_tmp[,cycle], t=cycle_length)
    f_tmp_MI1[,cycle] <- f_tmp[,cycle]*score_prop_MI1
    f_tmp_S1[,cycle] <- f_tmp[,cycle]*score_prop_S1
    f_tmp_CVD[,cycle] <- f_tmp[,cycle]*score_prop_CVD
  }
  
  score_composite <- score_MI1 <- score_S1 <- score_CVD <- data.frame(matrix(NA, nrow = n_sim, ncol = n_cycles))
  colnames(score_composite) <- colnames(score_MI1) <- colnames(score_S1) <- colnames(score_CVD) <- 1:n_cycles
  
  for (cycle in 1:n_cycles) {
    score_composite[,cycle] <- m_tmp[,cycle]*prop_male + f_tmp[,cycle]*(1-prop_male)
    score_MI1[,cycle] <- m_tmp_MI1[,cycle]*prop_male + f_tmp_MI1[,cycle]*(1-prop_male)
    score_S1[,cycle] <- m_tmp_S1[,cycle]*prop_male + f_tmp_S1[,cycle]*(1-prop_male)
    score_CVD[,cycle] <- m_tmp_CVD[,cycle]*prop_male + f_tmp_CVD[,cycle]*(1-prop_male)
    
  }
  
  return(list(aggregate = t(score_composite), score_MI1 = t(score_MI1), score_S1 = t(score_S1), score_CVD = t(score_CVD)))
}


#### 3. SMART model predictions ####

# Function to calculate smart2 probs from MI or stroke states
gen_smart_tprobs <- function(from_state, intervention, subgroup) {
  
  if(intervention == "SOC"){
    stop_smoke_tmp = 0
    sbp_diff_tmp = 0
  }
  if(intervention == "Box" & (subgroup == "controlled_nohis" | subgroup == "controlled_yeshis")){
    stop_smoke_tmp = stop_smoke
    sbp_diff_tmp = sbp_diff_controlled
  } 
  if(intervention == "Box" & (subgroup == "uncontrolled_nohis" | subgroup == "uncontrolled_yeshis")){
    stop_smoke_tmp = stop_smoke
    sbp_diff_tmp = sbp_diff_uncontrolled
  } 
  
  
  if(from_state == "MI" & (subgroup == "controlled_nohis" | subgroup == "controlled_yeshis") ){
    u70_smart_age = n_age_init 
    u70_smart_smoke = (0.15*prop_male)+(0.12*(1-prop_male)) 
    u70_smart_sbp = (125.4*prop_male)+(124.6*(1-prop_male))
    u70_smart_nonhdl = (1.15*prop_male)+(1.44*(1-prop_male))
    u70_smart_male = prop_male 
    u70_smart_dm = (0.25*prop_male)+(0.21*(1-prop_male))
    u70_smart_egfr = (80.2*prop_male)+(79.6*(1-prop_male)) 
    u70_smart_aspirin = (0.86*prop_male)+(0.8*(1-prop_male)) 
    u70_smart_hscrp = 2
    u70_smart_tdiag = (9*prop_male)+(7.7*(1-prop_male)) 
    u70_smart_CAD = 1 
    u70_smart_PAD = (0.03*prop_male)+(0.03*(1-prop_male)) 
    u70_smart_CeVD = 0 
    u70_smart_aaa = 0
    
    o70_smart_age = n_age_init 
    o70_smart_smoke = (0.07*prop_male)+(0.06*(1-prop_male)) 
    o70_smart_sbp = (125.9*prop_male)+(126.4*(1-prop_male))
    o70_smart_nonhdl = (1.21*prop_male)+(1.5*(1-prop_male))
    o70_smart_male = prop_male 
    o70_smart_dm = (0.28*prop_male)+(0.26*(1-prop_male))
    o70_smart_egfr = (65*prop_male)+(63.1*(1-prop_male)) 
    o70_smart_aspirin = (0.86*prop_male)+(0.82*(1-prop_male)) 
    o70_smart_hscrp = 2
    o70_smart_tdiag = (12.8*prop_male)+(10.2*(1-prop_male))
    o70_smart_CAD = 1 
    o70_smart_PAD = (0.06*prop_male)+(0.05*(1-prop_male)) 
    o70_smart_CeVD = 0 
    o70_smart_aaa = 0
  }else{
    if(from_state == "MI" & (subgroup == "uncontrolled_nohis" | subgroup == "uncontrolled_yeshis")){
      u70_smart_age = n_age_init 
      u70_smart_smoke = (0.14*prop_male)+(0.14*(1-prop_male)) 
      u70_smart_sbp = (147.7*prop_male)+(148.0*(1-prop_male))
      u70_smart_nonhdl = (1.15*prop_male)+(1.42*(1-prop_male))
      u70_smart_male = prop_male 
      u70_smart_dm = (0.27*prop_male)+(0.23*(1-prop_male))
      u70_smart_egfr = (80.7*prop_male)+(78.24*(1-prop_male)) 
      u70_smart_aspirin = (0.84*prop_male)+(0.75*(1-prop_male)) 
      u70_smart_hscrp = 2 
      u70_smart_tdiag = (8.4*prop_male)+(7.1*(1-prop_male)) 
      u70_smart_CAD = 1 
      u70_smart_PAD = (0.05*prop_male)+(0.03*(1-prop_male)) 
      u70_smart_CeVD = 0 
      u70_smart_aaa = 0
      
      o70_smart_age = n_age_init 
      o70_smart_smoke = (0.06*prop_male)+(0.04*(1-prop_male)) 
      o70_smart_sbp = (150.3*prop_male)+(151.9*(1-prop_male))
      o70_smart_nonhdl = (1.23*prop_male)+(1.47*(1-prop_male))
      o70_smart_male = prop_male 
      o70_smart_dm = (0.3*prop_male)+(0.27*(1-prop_male))
      o70_smart_egfr = (64.3*prop_male)+(62.9*(1-prop_male)) 
      o70_smart_aspirin = (0.85*prop_male)+(0.8*(1-prop_male)) 
      o70_smart_hscrp = 2 
      o70_smart_tdiag = (12.6*prop_male)+(10.1*(1-prop_male)) 
      o70_smart_CAD = 1 
      o70_smart_PAD = (0.06*prop_male)+(0.06*(1-prop_male)) 
      o70_smart_CeVD = 0 
      o70_smart_aaa = 0
    }else{
      if(from_state == "S" & (subgroup == "controlled_nohis" | subgroup == "controlled_yeshis")){
        u70_smart_age = n_age_init 
        u70_smart_smoke = (0.14*prop_male)+(0.17*(1-prop_male)) 
        u70_smart_sbp = (125.94*prop_male)+(124.1*(1-prop_male))
        u70_smart_nonhdl = (1.22*prop_male)+(1.5*(1-prop_male))
        u70_smart_male = prop_male 
        u70_smart_dm = (0.16*prop_male)+(0.14*(1-prop_male))
        u70_smart_egfr = (79.6*prop_male)+(78.8*(1-prop_male)) 
        u70_smart_aspirin = (0.85*prop_male)+(0.83*(1-prop_male)) 
        u70_smart_hscrp = 2 
        u70_smart_tdiag = (7.1*prop_male)+(8*(1-prop_male)) 
        u70_smart_CAD = (0.1*prop_male)+(0.05*(1-prop_male))
        u70_smart_PAD = (0.02*prop_male)+(0.02*(1-prop_male)) 
        u70_smart_CeVD = 1 
        u70_smart_aaa = 0
        
        o70_smart_age = n_age_init 
        o70_smart_smoke = (0.06*prop_male)+(0.06*(1-prop_male)) 
        o70_smart_sbp = (126.4*prop_male)+(126.3*(1-prop_male))
        o70_smart_nonhdl = (1.25*prop_male)+(1.55*(1-prop_male))
        o70_smart_male = prop_male 
        o70_smart_dm = (0.24*prop_male)+(0.20*(1-prop_male))
        o70_smart_egfr = (64.7*prop_male)+(64.8*(1-prop_male)) 
        o70_smart_aspirin = (0.88*prop_male)+(0.85*(1-prop_male)) 
        o70_smart_hscrp = 2 
        o70_smart_tdiag = (9.7*prop_male)+(8.7*(1-prop_male)) 
        o70_smart_CAD = (0.18*prop_male)+(0.12*(1-prop_male))
        o70_smart_PAD = (0.06*prop_male)+(0.03*(1-prop_male)) 
        o70_smart_CeVD = 1 
        o70_smart_aaa = 0
      }else{
        if(from_state == "S" & (subgroup == "uncontrolled_nohis" | subgroup == "uncontrolled_yeshis")){
          u70_smart_age = n_age_init 
          u70_smart_smoke = (0.16*prop_male)+(0.14*(1-prop_male)) 
          u70_smart_sbp = (146.6*prop_male)+(147.5*(1-prop_male))
          u70_smart_nonhdl = (1.2*prop_male)+(1.44*(1-prop_male))
          u70_smart_male = prop_male 
          u70_smart_dm = (0.19*prop_male)+(0.2*(1-prop_male))
          u70_smart_egfr = (79.3*prop_male)+(78.5*(1-prop_male)) 
          u70_smart_aspirin = (0.83*prop_male)+(0.80*(1-prop_male)) 
          u70_smart_hscrp = 2 
          u70_smart_tdiag = (7.2*prop_male)+(8.5*(1-prop_male)) 
          u70_smart_CAD = (0.11*prop_male)+(0.08*(1-prop_male))
          u70_smart_PAD = (0.04*prop_male)+(0.02*(1-prop_male)) 
          u70_smart_CeVD = 1
          u70_smart_aaa = 0
          
          o70_smart_age = n_age_init 
          o70_smart_smoke = (0.06*prop_male)+(0.05*(1-prop_male)) 
          o70_smart_sbp = (149.9*prop_male)+(152.2*(1-prop_male))
          o70_smart_nonhdl = (1.27*prop_male)+(1.53*(1-prop_male))
          o70_smart_male = prop_male 
          o70_smart_dm = (0.24*prop_male)+(0.20*(1-prop_male))
          o70_smart_egfr = (63.9*prop_male)+(64.5*(1-prop_male)) 
          o70_smart_aspirin = (0.86*prop_male)+(0.84*(1-prop_male)) 
          o70_smart_hscrp = 2 
          o70_smart_tdiag = (10*prop_male)+(8.8*(1-prop_male)) 
          o70_smart_CAD = (0.19*prop_male)+(0.11*(1-prop_male))
          o70_smart_PAD = (0.06*prop_male)+(0.05*(1-prop_male)) 
          o70_smart_CeVD = 1 
          o70_smart_aaa = 0}
      }
    }
  }
  
  
  sbp_diff_tmp0 <- sbp_diff_tmp
  stop_smoke_tmp0 <- stop_smoke_tmp
  
  tmp <- tmp_MI <- tmp_S <- tmp_CVD <- data.frame(matrix(NA, nrow = n_sim, ncol = n_cycles))
  colnames(tmp) <- colnames(tmp_MI) <- colnames(tmp_S) <- colnames(tmp_CVD) <- 1:n_cycles
  
  for (cycle in 1:n_cycles) {
    if(cycle >= year_reduction_effect){
      sbp_diff_tmp <- sbp_diff_tmp0*(1-percentage_reduction_effect)
      stop_smoke_tmp <- stop_smoke_tmp0*(1-percentage_reduction_effect)
    }
    age_tmp <- n_age_init + cycle 
    if(age_tmp>=20 & age_tmp<45){
      smart_prop_MI <- prop_male*prob_nonfatal_MI_men_20+(1-prop_male)*prob_nonfatal_MI_women_20
      smart_prop_S <- prop_male*prob_nonfatal_stroke_men_20+(1-prop_male)*prob_nonfatal_stroke_women_20
      smart_prop_CVD <- prop_male*prob_fatal_event_men_20+(1-prop_male)*prob_fatal_event_women_20
    }
    if(age_tmp>=40 & age_tmp<65){
      smart_prop_MI <- prop_male*prob_nonfatal_MI_men_45+(1-prop_male)*prob_nonfatal_MI_women_45
      smart_prop_S <- prop_male*prob_nonfatal_stroke_men_45+(1-prop_male)*prob_nonfatal_stroke_women_45
      smart_prop_CVD <- prop_male*prob_fatal_event_men_45+(1-prop_male)*prob_fatal_event_women_45
    }
    if(age_tmp>=65 & age_tmp<80){
      smart_prop_MI <- prop_male*prob_nonfatal_MI_men_65+(1-prop_male)*prob_nonfatal_MI_women_65
      smart_prop_S <- prop_male*prob_nonfatal_stroke_men_65+(1-prop_male)*prob_nonfatal_stroke_women_65
      smart_prop_CVD <- prop_male*prob_fatal_event_men_65+(1-prop_male)*prob_fatal_event_women_65
    }
    if(age_tmp>=80){
      smart_prop_MI <- prop_male*prob_nonfatal_MI_men_80+(1-prop_male)*prob_nonfatal_MI_women_80
      smart_prop_S <- prop_male*prob_nonfatal_stroke_men_80+(1-prop_male)*prob_nonfatal_stroke_women_80
      smart_prop_CVD <- prop_male*prob_fatal_event_men_80+(1-prop_male)*prob_fatal_event_women_80
    }
    if(age_tmp<70){
      meanlin <- -0.0463729
      tmp[,cycle] <- 1-(1 - 0.020867965)^exp(
        (0.330356631 * u70_smart_aaa +
           -0.03496022 * age_tmp +
           0.000551072 * age_tmp^2 +
           -0.21072103 * u70_smart_aspirin +
           0.294701954 * u70_smart_CAD +
           -0.03967521 * u70_smart_egfr +
           0.000218613 * u70_smart_egfr^2 +
           0.151760173 * u70_smart_hscrp +
           0.345583271 * (u70_smart_smoke-(u70_smart_smoke*stop_smoke_tmp)) +
           0.34831786 * u70_smart_CeVD +
           0.318170659 * u70_smart_dm +
           0.540364249 * log(u70_smart_nonhdl) +
           0.22446658 * u70_smart_PAD +
           0.018913154 * (u70_smart_sbp-sbp_diff_tmp) +
           0.287658743 * u70_smart_male + 
           0.047699585 * u70_smart_tdiag +
           -0.00164973 * u70_smart_tdiag^2) - meanlin - log(0.81590))
    }else{
      meanlin <- -0.0463729
      tmp[,cycle] <- 1-(1 - 0.020867965)^exp(
        (0.330356631 * o70_smart_aaa +
           -0.03496022 * age_tmp +
           0.000551072 * age_tmp^2 +
           -0.21072103 * o70_smart_aspirin +
           0.294701954 * o70_smart_CAD +
           -0.03967521 * o70_smart_egfr +
           0.000218613 * o70_smart_egfr^2 +
           0.151760173 * o70_smart_hscrp +
           0.345583271 * (o70_smart_smoke-(o70_smart_smoke*stop_smoke_tmp)) +
           0.34831786 * o70_smart_CeVD +
           0.318170659 * o70_smart_dm +
           0.540364249 * log(o70_smart_nonhdl) +
           0.22446658 * o70_smart_PAD +
           0.018913154 * (o70_smart_sbp-sbp_diff_tmp) +
           0.287658743 * o70_smart_male + 
           0.047699585 * o70_smart_tdiag +
           -0.00164973 * o70_smart_tdiag^2) - meanlin - log(0.81590))
    }
    tmp_MI[,cycle] <- tmp[,cycle]*smart_prop_MI
    tmp_S[,cycle] <- tmp[,cycle]*smart_prop_S
    tmp_CVD[,cycle] <- tmp[,cycle]*smart_prop_CVD
  }
  
  
  return(list(aggregate = t(tmp), smart_MI = t(tmp_MI), smart_S = t(tmp_S), smart_CVD = t(tmp_CVD)))
}


#### 4. Genpop mortality ####
#source: https://mortality.org/File/GetDocument/hmd.v6/NLD/STATS/Mx_1x1.txt
nl_mort <- function(){
  # Create the data
  mortall <- data.frame(
    Year = rep(2021, times = 111),
    Age = 0:110,
    Female = c(0.003123,	0.000193,	0.000132,	0.000036,	0.000059,	0.000093,	0.000046,	0.00008,	0.00008,	0.000089,	0.000043,	0.000054,	0.000064,	0.000108,	0.000129,	0.000085,	0.000104,	0.000131,	0.000086,	0.000147,
               0.000206,	0.000188,	0.000181,	0.000267,	0.000147,	0.000215,	0.000191,	0.000267,	0.00024,	0.000323,	0.000286,	0.000334,	0.000415,	0.000462,	0.0004,	0.000402,	0.000508,	0.000458,	0.000709,	0.000459,	
               0.000795,	0.000747,	0.000844,	0.000902,	0.000951,	0.00127,	0.001202,	0.001168,	0.001466,	0.001565,	0.002109,	0.002006,	0.002181,	0.002373,	0.002755,	0.003038,	0.003708,	0.003726,	0.003942,	0.00481,
               0.005359,	0.005871,	0.006286,	0.006708,	0.007903,	0.00881,	0.009081,	0.010228,	0.011093,	0.012196,	0.013278,	0.014013,	0.015324,	0.017143,	0.020122,	0.021925,	0.025115,	0.027937,	0.03036,	0.035406,	
               0.040171,	0.045555,	0.050783,	0.05567,	0.068935,	0.080729,	0.088679,	0.104888,	0.121139,	0.143997,	0.162124,	0.1859,	0.209357,	0.239038,	0.264938,	0.296095,	0.33399,	0.361186,	0.389904,	0.431014,	
               0.470846,	0.498464,	0.60289,	0.637048,	0.686134,	0.73506,	0.785165,	0.867199,	1.007796,	1.455823,	3.050647),
    Male = c(0.003751,	0.000184,	0.000137,	0.000034,	0.000112,	0.000066,	0.000055,	0.000033,	0.000033,	0.000074,	0.000082,	0.000041,	0.000132,	0.000113,	0.000185,	0.000152,	0.000208,	0.000163,	0.000265,	0.000365,	
             0.000417,	0.00038,	0.000419,	0.000419,	0.000339,	0.000478,	0.000431,	0.000499,	0.00043,	0.000541,	0.000511,	0.000597,	0.000493,	0.000484,	0.000684,	0.000665,	0.00071,	0.000751,	0.000903,	0.000733,	
             0.000887,	0.001118,	0.001101,	0.001231,	0.00133,	0.001593,	0.001656,	0.001969,	0.001863,	0.002447,	0.002721,	0.002811,	0.003006,	0.0036,	0.003774,	0.004174,	0.004906,	0.005135,	0.005977,	0.006331,	
             0.006898,	0.007193,	0.008903,	0.009419,	0.010469,	0.012146,	0.012598,	0.014441,	0.015566,	0.017971,	0.020561,	0.022913,	0.023318,	0.026483,	0.027956,	0.03295,	0.038316,	0.042029,	0.047357,	0.053462,	
             0.059792,	0.067926,	0.077339,	0.085566,	0.098416,	0.111859,	0.131896,	0.151222,	0.167707,	0.187108,	0.220314,	0.241765,	0.263273,	0.297369,	0.333886,	0.372267,	0.383881,	0.422095,	0.484561,	0.458822,	
             0.515469,	0.534534,	0.630011,	0.674162,	0.740533,	0.81879,	1.006749,	1.587088,	4.278574,	4.278574,	4.278574),
    Total = c(0.003445,	0.000188,	0.000135,	0.000035,	0.000086,	0.000079,	0.000051,	0.000056,	0.000056,	0.000081,	0.000063,	0.000047,	0.000099,	0.00011,	0.000157,	0.000119,	0.000157,	0.000148,	0.000177,	0.000258,	
              0.000313,	0.000286,	0.000302,	0.000344,	0.000244,	0.000348,	0.000313,	0.000385,	0.000336,	0.000434,	0.0004,	0.000468,	0.000454,	0.000473,	0.000543,	0.000535,	0.00061,	0.000606,	0.000807,	0.000596,	
              0.000841,	0.000932,	0.000972,	0.001066,	0.00114,	0.001431,	0.001427,	0.001565,	0.001663,	0.002004,	0.002415,	0.002409,	0.002595,	0.002988,	0.003267,	0.003609,	0.004311,	0.004434,	0.004962,	0.00557,	
              0.006127,	0.00653,	0.00759,	0.008056,	0.009178,	0.010462,	0.010819,	0.012311,	0.0133,	0.015044,	0.016861,	0.018372,	0.019234,	0.021687,	0.023912,	0.027226,	0.031388,	0.034591,	0.038314,	0.043746,	
              0.049104,	0.055519,	0.062397,	0.068483,	0.08122,	0.093355,	0.105623,	0.122323,	0.137919,	0.158922,	0.181266,	0.203291,	0.225379,	0.255633,	0.283509,	0.31511,	0.345506,	0.374619,	0.409738,	0.436416,	
              0.479208,	0.505015,	0.607505,	0.642946,	0.694283,	0.746574,	0.811283,	0.927528,	1.127509,	1.455823,	3.050647)
  )
  return(mortall)
}

mort_p_scaling <- function(){
  df_mort_p_scaling <- data.frame(
    Year = rep(2021, times = 111),
  Age = 0:110,
  Female = c(
    0.003787879,
    0, 	0, 	0, 	0, 	0, 
    0, 	0, 	0, 	0, 	0, 
    0, 	0, 	0, 	0, 	0, 
    0.0178571428571429, 	0.0178571428571429, 	0.0178571428571429, 	0.0178571428571429, 	0.0178571428571429, 
    0.0275229357798165, 	0.0275229357798165, 	0.0275229357798165, 	0.0275229357798165, 	0.0275229357798165, 
    0.0144927536231884, 	0.0144927536231884, 	0.0144927536231884, 	0.0144927536231884, 	0.0144927536231884, 
    0.0613207547169811, 	0.0613207547169811, 	0.0613207547169811, 	0.0613207547169811, 	0.0613207547169811, 
    0.0374531835205993, 	0.0374531835205993, 	0.0374531835205993, 	0.0374531835205993, 	0.0374531835205993, 
    0.0795454545454545, 	0.0795454545454545, 	0.0795454545454545, 	0.0795454545454545, 	0.0795454545454545, 
    0.100133511348465, 	0.100133511348465, 	0.100133511348465, 	0.100133511348465, 	0.100133511348465, 
    0.0758714969241285, 	0.0758714969241285, 	0.0758714969241285, 	0.0758714969241285, 	0.0758714969241285, 
    0.0855371900826446, 	0.0855371900826446, 	0.0855371900826446, 	0.0855371900826446, 	0.0855371900826446, 
    0.104149715215622, 	0.104149715215622, 	0.104149715215622, 	0.104149715215622, 	0.104149715215622, 
    0.110347460862925, 	0.110347460862925, 	0.110347460862925, 	0.110347460862925, 	0.110347460862925, 
    0.142729932578552, 	0.142729932578552, 	0.142729932578552, 	0.142729932578552, 	0.142729932578552, 
    0.175690719562797, 	0.175690719562797, 	0.175690719562797, 	0.175690719562797, 	0.175690719562797, 
    0.201546825943794, 	0.201546825943794, 	0.201546825943794, 	0.201546825943794, 	0.201546825943794, 
    0.239333531686998, 	0.239333531686998, 	0.239333531686998, 	0.239333531686998, 	0.239333531686998, 
    0.252224919093851, 	0.252224919093851, 	0.252224919093851, 	0.252224919093851, 	0.252224919093851, 
    0.251050420168067, 	0.251050420168067, 	0.251050420168067, 	0.251050420168067, 	0.251050420168067, 
    0.251050420168067, 	0.251050420168067, 	0.251050420168067, 	0.251050420168067, 	0.251050420168067, 
    0.251050420168067, 	0.251050420168067, 	0.251050420168067, 	0.251050420168067, 	0.251050420168067
    ),
  Male = c(
    0,
    0, 	0, 	0, 	0, 	0, 
    0, 	0, 	0, 	0, 	0, 
    0, 	0, 	0, 	0, 	0, 
    0, 	0, 	0, 	0, 	0, 
    0.0222222222222222, 	0.0222222222222222, 	0.0222222222222222, 	0.0222222222222222, 	0.0222222222222222, 
    0.0437956204379562, 	0.0437956204379562, 	0.0437956204379562, 	0.0437956204379562, 	0.0437956204379562, 
    0.0407523510971787, 	0.0407523510971787, 	0.0407523510971787, 	0.0407523510971787, 	0.0407523510971787, 
    0.101990049751244, 	0.101990049751244, 	0.101990049751244, 	0.101990049751244, 	0.101990049751244, 
    0.172354948805461, 	0.172354948805461, 	0.172354948805461, 	0.172354948805461, 	0.172354948805461, 
    0.156603773584906, 	0.156603773584906, 	0.156603773584906, 	0.156603773584906, 	0.156603773584906, 
    0.171554252199413, 	0.171554252199413, 	0.171554252199413, 	0.171554252199413, 	0.171554252199413, 
    0.159773876822374, 	0.159773876822374, 	0.159773876822374, 	0.159773876822374, 	0.159773876822374, 
    0.170186590116875, 	0.170186590116875, 	0.170186590116875, 	0.170186590116875, 	0.170186590116875, 
    0.169479800774765, 	0.169479800774765, 	0.169479800774765, 	0.169479800774765, 	0.169479800774765, 
    0.177211843496651, 	0.177211843496651, 	0.177211843496651, 	0.177211843496651, 	0.177211843496651, 
    0.186379928315412, 	0.186379928315412, 	0.186379928315412, 	0.186379928315412, 	0.186379928315412, 
    0.201353413388178, 	0.201353413388178, 	0.201353413388178, 	0.201353413388178, 	0.201353413388178, 
    0.222622071690658, 	0.222622071690658, 	0.222622071690658, 	0.222622071690658, 	0.222622071690658, 
    0.248913568324481, 	0.248913568324481, 	0.248913568324481, 	0.248913568324481, 	0.248913568324481, 
    0.260306242638398, 	0.260306242638398, 	0.260306242638398, 	0.260306242638398, 	0.260306242638398, 
    0.260306242638398, 	0.260306242638398, 	0.260306242638398, 	0.260306242638398, 	0.260306242638398, 
    0.260306242638398, 	0.260306242638398, 	0.260306242638398, 	0.260306242638398, 	0.260306242638398
    )
  )
  return(df_mort_p_scaling)
}

#### 5. Model for subgroup CN ####

#For subgroup controlled BP, no CVD history

run_model_controlled_nohis <- function() {
  
  
  ### Risk prediction model inputs (SOC) ----
  
  #The effect of the intervention on the transitions is due to the difference on systolic blood pressure 'sbp_diff'
  
  
  ### Transition rates (annual) ----
  #SCORE2 and SCORE2-OP
  score_tprobs <- gen_score_tprobs(intervention = "SOC",
                                   subgroup = "controlled_nohis")
  
  score_p_MI1 <- as.matrix(score_tprobs$score_MI1)
  score_p_S1 <- as.matrix(score_tprobs$score_S1)
  score_p_CVD <- as.matrix(score_tprobs$score_CVD)
  
  
  box_score_tprobs <- gen_score_tprobs(intervention = "Box",
                                       subgroup = "controlled_nohis")
  
  box_score_p_MI1 <- as.matrix(box_score_tprobs$score_MI1)
  box_score_p_S1 <- as.matrix(box_score_tprobs$score_S1)
  box_score_p_CVD <- as.matrix(box_score_tprobs$score_CVD)
  
  
  #SMART2
  smart_tprobs_fromMI <- gen_smart_tprobs(from_state = "MI",
                                          intervention = "SOC",
                                          subgroup = "controlled_nohis")
  
  smart_p_MI1MI2 <- as.matrix(smart_tprobs_fromMI$smart_MI)
  smart_p_MI1S1 <- as.matrix(smart_tprobs_fromMI$smart_S)
  smart_p_MI2S1 <- as.matrix(smart_tprobs_fromMI$smart_S) # 
  smart_p_MI1CVD <- as.matrix(smart_tprobs_fromMI$smart_CVD)
  smart_p_MI2CVD <- as.matrix(smart_tprobs_fromMI$smart_CVD) # 
  
  
  smart_tprobs_fromS <- gen_smart_tprobs(from_state = "S",
                                         intervention = "SOC",
                                         subgroup = "controlled_nohis")
  
  smart_p_S1S2 <- as.matrix(smart_tprobs_fromS$smart_S)
  smart_p_S1CVD <- as.matrix(smart_tprobs_fromS$smart_CVD)
  smart_p_S2CVD <- as.matrix(smart_tprobs_fromS$smart_CVD) #
  
  
  
  box_smart_tprobs_fromMI <- gen_smart_tprobs(from_state = "MI",
                                              intervention = "Box",
                                              subgroup = "controlled_nohis")
  
  box_smart_p_MI1MI2 <- as.matrix(box_smart_tprobs_fromMI$smart_MI)
  box_smart_p_MI1S1 <- as.matrix(box_smart_tprobs_fromMI$smart_S)
  box_smart_p_MI2S1 <- as.matrix(box_smart_tprobs_fromMI$smart_S) # 
  box_smart_p_MI1CVD <- as.matrix(box_smart_tprobs_fromMI$smart_CVD)
  box_smart_p_MI2CVD <- as.matrix(box_smart_tprobs_fromMI$smart_CVD) # 
  
  
  box_smart_tprobs_fromS <- gen_smart_tprobs(from_state = "S",
                                             intervention = "Box",
                                             subgroup = "controlled_nohis")
  
  box_smart_p_S1S2 <- as.matrix(box_smart_tprobs_fromS$smart_S)
  box_smart_p_S1CVD <- as.matrix(box_smart_tprobs_fromS$smart_CVD)
  box_smart_p_S2CVD <- as.matrix(box_smart_tprobs_fromS$smart_CVD) #
  
  ## Age-dependent mortality rates ----
  lt_NL_2021 <- nl_mort()   
  #* Extract age-specific all-cause mortality for ages in model time horizon
  v_r_mort_by_age <- lt_NL_2021 %>% 
    dplyr::filter(Age >= n_age_init & Age < n_age_max) %>%
    dplyr::select(Female, Male) 
  v_r_mort_by_age$Total <- v_r_mort_by_age$Female*(1-prop_male)+v_r_mort_by_age$Male*prop_male
  v_r_mort_by_age <- as.matrix(v_r_mort_by_age$Total)
  
  #Also extract age-specific scaling values for all-cause mortality to account for CVD deaths
  df_mort_p_scaling <- mort_p_scaling()
  v_p_mort_scale <- df_mort_p_scaling %>% 
    dplyr::filter(Age >= n_age_init & Age < n_age_max) %>%
    dplyr::select(Female, Male) 
  v_p_mort_scale$Total <- v_p_mort_scale$Female*(1-prop_male)+v_p_mort_scale$Male*prop_male
  v_p_mort_scale <- as.numeric(v_p_mort_scale$Total)
  
  ### Discount weight for costs and effects ----
  v_dwc  <- 1 / ((1 + (d_c * cycle_length)) ^ (0:n_cycles))
  v_dwe  <- 1 / ((1 + (d_e * cycle_length)) ^ (0:n_cycles))
  
  # Process model inputs ----
  ## Age-specific transition rates to the Dead state for all cycles ----
  v_r_HDage  <- rep(v_r_mort_by_age, each = 1/cycle_length)
  #* Name age-specific mortality vector 
  names(v_r_HDage) <- v_age_names
  
  #* Function included in "R/Functions.R". The latest version can be found in `darthtools` package
  v_p_HDage  <- rate_to_prob(v_r_HDage, t = cycle_length)  # Age-specific mortality risk in the Healthy state 
  v_p_HDage <- v_p_HDage*(1-v_p_mort_scale) 
  # Construct state-transition models ----
  ## Initial state vector ----
  #* All starting healthy
  v_m_init <- c(R = 1, MI1=0, MI2=0, S1 = 0, S2 = 0, CVD=0 , D = 0) 
  ## Initialize cohort traces ----
  ### Initialize cohort trace under SoC ----
  m_M_SoC <- array(NA, 
                   dim = c((n_cycles + 1), n_states, n_sim), 
                   dimnames = list(0:n_cycles, v_names_states, 1:n_sim))
  #* Store the initial state vector in the first row of the cohort trace
  m_M_SoC[1, , ] <- v_m_init
  ### Initialize cohort trace for competing strategies ----
  #* Structure and initial states are the same as for SoC
  m_M_strA  <- m_M_SoC # Strategy A
  
  
  
  ## Create transition probability arrays for strategy SoC ----
  ### Initialize transition probability array for strategy SoC ----
  #* All transitions to a non-death state are assumed to be conditional on survival
  a_P_SoC <- array(0,
                   dim  = c(n_states, n_states, n_cycles, n_sim),
                   dimnames = list(v_names_states, 
                                   v_names_states, 
                                   0:(n_cycles - 1),
                                   1:n_sim))
  
  # ### Fill in array
  ## From R
  a_P_SoC["R", "R", ,]   <- (1 - (v_p_HDage + score_p_CVD[,])) * (1 - (score_p_MI1[,] + score_p_S1[,]))
  a_P_SoC["R", "MI1", , ]  <- (1 - (v_p_HDage + score_p_CVD[,])) * score_p_MI1[,] 
  a_P_SoC["R", "S1", ,]  <- (1 - (v_p_HDage + score_p_CVD[,])) * score_p_S1[,] 
  a_P_SoC["R", "CVD", ,] <- score_p_CVD[,] 
  a_P_SoC["R", "D", ,]   <- v_p_HDage 
  ## From MI1
  a_P_SoC["MI1", "MI1", ,]  <- (1 - (smart_p_MI1CVD[,] + v_p_HDage)) * (1- (smart_p_MI1S1[,] + smart_p_MI1MI2[,]))
  a_P_SoC["MI1", "MI2", ,]  <- (1 - (smart_p_MI1CVD[,] + v_p_HDage)) * smart_p_MI1MI2[,]
  a_P_SoC["MI1", "S1", ,]  <-  (1 - (smart_p_MI1CVD[,] + v_p_HDage)) * smart_p_MI1S1[,] 
  a_P_SoC["MI1", "CVD", ,]  <- smart_p_MI1CVD[,] 
  a_P_SoC["MI1", "D", ,]  <-  v_p_HDage 
  ## From MI2
  a_P_SoC["MI2", "MI2", ,]  <- (1 - (smart_p_MI2CVD[,] + v_p_HDage)) * (1-smart_p_MI2S1[,]) 
  a_P_SoC["MI2", "S1", ,]  <- (1 - (smart_p_MI2CVD[,] + v_p_HDage)) * smart_p_MI2S1[,] 
  a_P_SoC["MI2", "CVD", ,]  <- smart_p_MI2CVD[,] 
  a_P_SoC["MI2", "D", ,]  <-  v_p_HDage 
  
  ## From S1
  a_P_SoC["S1", "S1", ,] <- (1 - (v_p_HDage + smart_p_S1CVD[,])) * (1-smart_p_S1S2[,]) 
  a_P_SoC["S1", "S2", ,] <- (1 - (v_p_HDage + smart_p_S1CVD[,])) * smart_p_S1S2[,]
  a_P_SoC["S1", "CVD", ,] <- smart_p_S1CVD[,]
  a_P_SoC["S1", "D", ,] <- v_p_HDage 
  
  ## From S2
  a_P_SoC["S2", "S2", ,] <- (1 - (v_p_HDage + smart_p_S2CVD[,])) 
  a_P_SoC["S2", "CVD", ,] <- smart_p_S2CVD[,]
  a_P_SoC["S2", "D", ,] <- v_p_HDage 
  
  ## From CVD
  a_P_SoC["CVD", "CVD", ,]   <- 1
  
  ## From D
  a_P_SoC["D", "D", ,]   <- 1
  
  ### Transition probability array for strategy: CVRM-Box ----
  a_P_strA <- array(0,
                    dim  = c(n_states, n_states, n_cycles, n_sim),
                    dimnames = list(v_names_states, 
                                    v_names_states, 
                                    0:(n_cycles - 1),
                                    1:n_sim))
  ## From R
  a_P_strA["R", "R", ,]   <- (1 - (v_p_HDage + box_score_p_CVD[,])) * (1 - (box_score_p_MI1[,] + box_score_p_S1[,]))
  a_P_strA["R", "MI1", ,]  <- (1 - (v_p_HDage + box_score_p_CVD[,])) * box_score_p_MI1[,] 
  a_P_strA["R", "S1", ,]  <- (1 - (v_p_HDage + box_score_p_CVD[,])) * box_score_p_S1[,] 
  a_P_strA["R", "CVD", ,] <- box_score_p_CVD[,] 
  a_P_strA["R", "D", ,]   <- v_p_HDage 
  ## From MI1
  a_P_strA["MI1", "MI1", ,]  <- (1 - (box_smart_p_MI1CVD[,] + v_p_HDage)) * (1- (box_smart_p_MI1S1[,] + box_smart_p_MI1MI2[,]))
  a_P_strA["MI1", "MI2", ,]  <- (1 - (box_smart_p_MI1CVD[,] + v_p_HDage)) * box_smart_p_MI1MI2[,]
  a_P_strA["MI1", "S1", ,]  <-  (1 - (box_smart_p_MI1CVD[,] + v_p_HDage)) * box_smart_p_MI1S1[,] 
  a_P_strA["MI1", "CVD", ,]  <- box_smart_p_MI1CVD[,] 
  a_P_strA["MI1", "D", ,]  <-  v_p_HDage 
  ## From MI2
  a_P_strA["MI2", "MI2", ,]  <- (1 - (box_smart_p_MI2CVD[,] + v_p_HDage)) * (1-box_smart_p_MI2S1[,]) 
  a_P_strA["MI2", "S1", ,]  <- (1 - (box_smart_p_MI2CVD[,] + v_p_HDage)) * box_smart_p_MI2S1[,] 
  a_P_strA["MI2", "CVD", ,]  <- box_smart_p_MI2CVD[,] 
  a_P_strA["MI2", "D", ,]  <-  v_p_HDage 
  
  ## From S1
  a_P_strA["S1", "S1", ,] <- (1 - (v_p_HDage + box_smart_p_S1CVD[,])) * (1-box_smart_p_S1S2[,]) 
  a_P_strA["S1", "S2", ,] <- (1 - (v_p_HDage + box_smart_p_S1CVD[,])) * box_smart_p_S1S2[,]
  a_P_strA["S1", "CVD", ,] <- box_smart_p_S1CVD[,]
  a_P_strA["S1", "D", ,] <- v_p_HDage 
  
  ## From S2
  a_P_strA["S2", "S2", ,] <- (1 - (v_p_HDage + box_smart_p_S2CVD[,])) 
  a_P_strA["S2", "CVD", ,] <- box_smart_p_S2CVD[,]
  a_P_strA["S2", "D", ,] <- v_p_HDage 
  
  ## From CVD
  a_P_strA["CVD", "CVD", ,]   <- 1
  
  ## From D
  a_P_strA["D", "D", ,]   <- 1
  
  
  ## Check if transition probability arrays are valid ----
  #* Functions included in "R/Functions.R". The latest version can be found in `darthtools` package
  ### Check that transition probabilities are [0, 1] ----
  for(sim in 1:n_sim) {
    #print(sim)
    check_transition_probability(a_P_SoC[,,,sim], verbose = TRUE)
    check_transition_probability(a_P_strA[,,,sim], verbose = TRUE)
    #Check that all rows for each slice of the array sum to 1 
    sum_soc_tmp <- sum(rowSums(a_P_SoC[,,,sim]))
    if (sum_soc_tmp != n_states*n_cycles) {print("Invalid")}
    sum_a_tmp <- sum(rowSums(a_P_strA[,,,sim]))
    if (sum_a_tmp != n_states*n_cycles) {print("Invalid")}
  }
  
  ## Create transition dynamics arrays ----
  
  # These arrays will capture transitions from each state to another over time 
  ## Initialize transition dynamics array for strategy SoC ----
  a_A_SoC <- array(0,
                   dim      = c(n_states, n_states, n_cycles + 1, n_sim),
                   dimnames = list(v_names_states, v_names_states, 0:n_cycles, 1:n_sim))
  a_A_strA  <- a_A_SoC #Structure and initial states are the same as for SoC
  
  
  #  Run Markov model ----
  #* Iterative solution of age-dependent cSTM
  for(i in 1:n_sim) {
    #* Set first slice of a_A_SoC and a_A_strA with the initial state vector in its diagonal
    diag(a_A_SoC[, , 1,i]) <- v_m_init
    diag(a_A_strA[, , 1,i]) <- v_m_init
    for(t in 1:n_cycles){
      ## Fill in cohort trace
      # For SoC
      m_M_SoC[t + 1, ,i]  <- m_M_SoC[t, ,i]  %*% a_P_SoC[, , t,i]
      # for strategy: CVRM-Box
      m_M_strA[t + 1, ,i] <- m_M_strA[t,,i ] %*% a_P_strA[, , t,i]
      
      ## Fill in transition-dynamics array
      # For SoC
      a_A_SoC[, , t + 1,i]  <- diag(m_M_SoC[t, ,i]) %*% a_P_SoC[, , t,i]
      # for strategy: CVRM-Box
      a_A_strA[, , t + 1,i] <- diag(m_M_strA[t, ,i]) %*% a_P_strA[, , t,i]
    }
  }
  ## Store the cohort traces in a list ----
  l_m_M <- list(SoC =  m_M_SoC,
                A   =  m_M_strA) 
  
  names(l_m_M) <- v_names_str
  
  ## Store the transition dynamics array for each strategy in a list ----
  l_a_A <- list(SoC =  a_A_SoC,
                A   =  a_A_strA) 
  names(l_a_A) <- v_names_str
  
  # Plot Outputs ----
  #* (Functions included in "R/Functions.R"; depends on the `ggplot2` package)
  
  # Plot the cohort trace for strategy SoC ----
  mean_trace_SoC <- apply(m_M_SoC, c(1:2), mean)
  trace_soc <- plot_trace(mean_trace_SoC)
  
  mean_trace_strA <- apply(m_M_strA, c(1:2), mean)
  trace_box <- plot_trace(mean_trace_strA)
  
  ## Plot the cohort trace for all strategies ----
  trace_all <- plot_trace_strategy(l_m_M)
  
  #time spent in states
  time_in_states_tmp <- array(NA, dim=c(n_str, n_states, n_sim), dimnames = list(v_names_str, v_names_states, 1:n_sim))
  time_in_states_tmp["Standard of care",,] <- colMeans(apply(m_M_SoC,c(1,2), mean))*100
  time_in_states_tmp["Strategy A",,] <- colMeans(m_M_strA[,,])*100
  
  time_in_states <- matrix(nrow = length(v_names_str), ncol = n_states+1)
  rownames(time_in_states) <- v_names_str
  colnames(time_in_states) <- c("Option", "At risk", "Post-MI", "Post-recurrent MI", "Post-Stroke", "Post-recurrent Stroke", "CVD Death", "All-cause Death")

  time_in_states["Standard of care",] <- c("No treatment", 
                                           round(mean(time_in_states_tmp["Standard of care","R",]),1),
                                           round(mean(time_in_states_tmp["Standard of care","MI1",]),1),
                                           round(mean(time_in_states_tmp["Standard of care","MI2",]),1),
                                           round(mean(time_in_states_tmp["Standard of care","S1",]),1),
                                           round(mean(time_in_states_tmp["Standard of care","S2",]),1),
                                           round(mean(time_in_states_tmp["Standard of care","CVD",]),1),
                                           round(mean(time_in_states_tmp["Standard of care","D",]),1))
  
  time_in_states["Strategy A",] <- c("Treatment",
                                     round(mean(time_in_states_tmp["Strategy A","R",]),1),
                                     round(mean(time_in_states_tmp["Strategy A","MI1",]),1),
                                     round(mean(time_in_states_tmp["Strategy A","MI2",]),1),
                                     round(mean(time_in_states_tmp["Strategy A","S1",]),1),
                                     round(mean(time_in_states_tmp["Strategy A","S2",]),1),
                                     round(mean(time_in_states_tmp["Strategy A","CVD",]),1),
                                     round(mean(time_in_states_tmp["Strategy A","D",]),1))

  #Life expectancy output and costs of added life years
  LEs_full <- matrix(NA,nrow=n_sim,ncol=n_str)
  PAID_full_SOC <- matrix(NA,nrow=n_sim,ncol=n_cycles+1)
  PAID_full_strA <- matrix(NA,nrow=n_sim,ncol=n_cycles+1)

  colnames(LEs_full) <- v_names_str
  colnames(PAID_full_SOC) <- c(0:n_cycles)
  colnames(PAID_full_strA) <- c(0:n_cycles)

  for(i in 1:n_sim){
    LEs_full[i,"Standard of care"] <- sum(rowSums(m_M_SoC[, -which(v_names_states == "D" | v_names_states == "CVD"),i])*v_wcc)
    LEs_full[i,"Strategy A"] <- sum(rowSums(m_M_strA[, -which(v_names_states == "D" | v_names_states == "CVD"),i])*v_wcc)
    PAID_full_SOC[i,] <- as.numeric(rowSums(m_M_SoC[, -which(v_names_states == "D" | v_names_states == "CVD"),i]))
    PAID_full_strA[i,] <- as.numeric(rowSums(m_M_strA[, -which(v_names_states == "D" | v_names_states == "CVD"),i]))
  }
  PAID_final <- get_paid(intervention = PAID_full_strA, control = PAID_full_SOC) 
  
  LEs <- data.frame(Option = c("No treatment", "Treatment"),
                    LE = c(round(mean(LEs_full[,"Standard of care"]),2),
                           paste0(
                             round(mean(LEs_full[,"Strategy A"]),2),
                             " (",
                             round(quantile(LEs_full[,"Strategy A"], probs = 0.025),2),
                             ", ",
                             round(quantile(LEs_full[,"Strategy A"], probs = 0.975),2),
                             ")"
                           )))
  
  df_surv <- calc_surv(l_m_M, v_names_death_states= c("CVD", "D"))
  survival_plot <- ggplot(df_surv,
                          aes(x = Cycle, y = Survival, group = as.factor(Strategy))) +
    geom_line(aes(linetype = Strategy), size = 0.8) +
    xlab("Cycle") +
    ylab("Proportion") +
    ylim(0,1) +
    ggtitle("Survival probabilities") +
    theme_bw(base_size = 14) +
    theme()

  
  # State Rewards ----
  v_u_SoC    <- cbind(u_R_sgCN,  
                      u_R_sgCN - u_MI1,  
                      u_R_sgCN - u_S1, 
                      u_R_sgCN - u_MI1 - u_MI2, 
                      u_R_sgCN - u_S1 - u_S2, 
                      u_D,
                      u_CVD)
  colnames(v_u_SoC) <- v_names_states 
  
  #* Vector of state costs under strategy SoC
  v_c_SoC    <- cbind(c_SOC_controlled,  
                      c_SOC_controlled + c_MI1, 
                      c_SOC_controlled + c_S1,  
                      c_SOC_controlled + c_MI1 + c_MI2,  
                      c_SOC_controlled + c_S1 + c_S2,  
                      c_D,
                      c_CVD)
  colnames(v_c_SoC) <- v_names_states 
  #* Vector of state utilities under strategy A
  v_u_strA   <- v_u_SoC
  
  #* Vector of state costs under strategy A
  v_c_strA   <- cbind(c_strA_controlled, 
                      c_MI1+ c_strA_controlled,
                      c_S1+ c_strA_controlled, 
                      c_MI2+ c_MI1+ c_strA_controlled, 
                      c_S2+ c_S1 + c_strA_controlled, 
                      c_D,
                      c_CVD) 
  colnames(v_c_strA) <- v_names_states 
  
  
  ## Store state rewards ----
  #* Store the vectors of state utilities for each strategy in a list 
  l_u <- list(SoC = v_u_SoC,
              A   = v_u_strA)
  #* Store the vectors of state cost for each strategy in a list 
  l_c <- list(SoC =  v_c_SoC,
              A   =  v_c_strA)
  
  #* assign strategy names to matching items in the lists
  names(l_u) <- names(l_c) <- v_names_str
  
  # Compute expected outcomes ----
  #* Create empty vectors to store total utilities and costs 
  v_tot_qaly <- v_tot_cost <- vector(mode = "numeric", length = n_str)
  names(v_tot_qaly) <- names(v_tot_cost) <- v_names_str
  
  
  
  total_costs_sims <- as.data.frame(matrix(NA, nrow=n_sim, ncol=n_str))
  colnames(total_costs_sims) <- v_names_str
  total_QALY_sims <- as.data.frame(matrix(NA, nrow=n_sim, ncol=n_str))
  colnames(total_costs_sims) <- v_names_str
  
  cycle_costs_strA <- as.data.frame(matrix(data=NA, nrow = n_sim, ncol=n_cycles+1))
  cycle_costs_SOC<- as.data.frame(matrix(data=NA, nrow = n_sim, ncol=n_cycles+1))
  
  ## Loop through each strategy and calculate total utilities and costs ----
  for (sim in 1:n_sim){
    for (i in 1:n_str) { # i <- 1
      v_u_str <- l_u[[i]][sim,]   # select the vector of state utilities for the i-th strategy
      v_c_str <- l_c[[i]][sim,]   # select the vector of state costs for the i-th strategy
      a_A_str <- l_a_A[[i]][,,,sim] # select the transition array for the i-th strategy, simulation
      ##* Array of state rewards 
      #* Create transition matrices of state utilities and state costs for the i-th strategy 
      m_u_str   <- matrix(v_u_str, nrow = n_states, ncol = n_states, byrow = T)
      m_c_str   <- matrix(v_c_str, nrow = n_states, ncol = n_states, byrow = T)
      #* Expand the transition matrix of state utilities across cycles to form a transition array of state utilities
      a_R_u_str <- array(m_u_str, 
                         dim      = c(n_states, n_states, n_cycles + 1),
                         dimnames = list(v_names_states, v_names_states, 0:n_cycles))
      # Expand the transition matrix of state costs across cycles to form a transition array of state costs
      a_R_c_str <- array(m_c_str, 
                         dim      = c(n_states, n_states, n_cycles + 1),
                         dimnames = list(v_names_states, v_names_states, 0:n_cycles))

      ##* Apply transition rewards
      #* Apply disutility due to transition from H to S1
      a_R_u_str["R", "MI1", ]  <- a_R_u_str["R", "MI1", ]      - du_MI1[sim] #
      a_R_u_str["MI1", "MI2", ] <-  a_R_u_str["MI1", "MI2", ]  - du_MI2[sim] #
      a_R_u_str["R", "S1", ]  <- a_R_u_str["R", "S1", ]      - du_S1[sim] #
      a_R_u_str["MI1", "S1", ]  <- a_R_u_str["MI1", "S1", ]  - du_MItoS[sim] #
      a_R_u_str["MI2", "S1", ]  <- a_R_u_str["MI2", "S1", ]  - du_MItoS[sim] #
      a_R_u_str["S1", "S2", ] <-  a_R_u_str["S1", "S2", ]  - du_S2[sim] #
      
      
      #* Add transition cost per cycle due to transition from H to S1
      a_R_c_str["R", "MI1",]      <- a_R_c_str["R", "MI1",] + ic_MI1[sim] # below we apply productivity lossess
      if(n_age_init<=66){a_R_c_str["R", "MI1", 1:(67-(n_age_init+1))]      <- a_R_c_str["R", "MI1", 1:(67-(n_age_init+1))]  + ic_fric[sim]} #
      a_R_c_str["MI1", "MI2", ]      <- a_R_c_str["MI1", "MI2", ]       + ic_MI2[sim] #
      a_R_c_str["MI1", "S1", ]      <- a_R_c_str["MI1", "S1", ]       + ic_S1[sim] #
      a_R_c_str["R", "S1", ]      <- a_R_c_str["R", "S1", ]       + ic_S1[sim]  #
      if(n_age_init<=66){a_R_c_str["R", "S1", 1:(67-(n_age_init+1))]      <- a_R_c_str["R", "S1", 1:(67-(n_age_init+1))]       + ic_fric[sim]} #
      a_R_c_str["S1", "S2", ]      <- a_R_c_str["S1", "S2", ]       + ic_S2[sim] #
      a_R_c_str["MI2", "CVD", ]      <- a_R_c_str["MI2", "CVD", ]       + ic_CVD[sim]
      a_R_c_str["MI2", "S1", ]      <- a_R_c_str["MI2", "S1", ]       + ic_S2[sim]
      a_R_c_str["S2", "CVD", ]      <- a_R_c_str["S2", "CVD", ]       + ic_CVD[sim]
      a_R_c_str["R", "CVD", ]      <- a_R_c_str["R", "CVD", ]       + ic_CVD[sim]
      
      
      
      
      ###* Expected QALYs and costs for all transitions per cycle
      #* QALYs = life years x QoL
      
      a_Y_c_str <- a_A_str * a_R_c_str
      a_Y_u_str <- a_A_str * a_R_u_str 
      
      ###* Expected QALYs and costs per cycle
      ##* Vector of QALYs and costs
      v_qaly_str <- apply(a_Y_u_str, 3, sum) # sum the proportion of the cohort across transitions 
      v_cost_str <- apply(a_Y_c_str, 3, sum) # sum the proportion of the cohort across transitions
      
      #* QALYs
      v_tot_qaly[i] <- t(v_qaly_str) %*% (v_dwe * v_wcc)
      #* Costs
      v_tot_cost[i] <- t(v_cost_str) %*% (v_dwc * v_wcc)
      
      ## Vector with discounted values
      if(i==1){
        total_QALY_sims[sim,i] <- v_tot_qaly[i]
        total_costs_sims[sim,i] <- v_tot_cost[i]
        cycle_costs_SOC[sim,] <- v_cost_str 
      }
      if(i==2){
        total_QALY_sims[sim,i] <- v_tot_qaly[i]
        total_costs_sims[sim,i] <- v_tot_cost[i] + c_init_strA + (PAID_final$intervention[sim] - PAID_final$control[sim]) # note here we add the initial intervention costs and costs of added life years
        cycle_costs_strA[sim,] <- v_cost_str
      }
    }
  }
  
  
  
  ## Visualize PSA results for CEA ----
  ### Create PSA object ----
  #* Function included in "R/Functions.R" The latest version can be found in `dampack` package
  l_psa <- make_psa_obj(cost          = total_costs_sims, 
                        effectiveness = total_QALY_sims, 
                        parameters    = df_psa_input, 
                        strategies    = v_names_str)
  l_psa$strategies <- v_names_str
  colnames(l_psa$effectiveness) <- v_names_str
  colnames(l_psa$cost) <- v_names_str
  
  #* Vector with willingness-to-pay (WTP) thresholds.
  v_wtp <- seq(0, 80000, by = 1000)
  
  ### Cost-Effectiveness Scatter plot ----
  txtsize <- 13
  
  gg_scatter <- inc_scatter(l_psa, wtp_line = 20000, txtsize=txtsize)+
    theme(legend.position = c(0.8, 0.48),
          axis.title.x = element_text(size=14),
          axis.title.y = element_text(size=14),
          axis.text.x = element_text(size=14),
          axis.text.y = element_text(size=14)
    )
  
  
  ### Incremental cost-effectiveness ratios (ICERs) with probabilistic output ----
  #* Compute expected costs and effects for each strategy from the PSA
  #* Function included in "R/Functions.R". The latest version can be found in `dampack` package
  df_out_ce_psa <- summary(l_psa)
  
  #* Function included in "R/Functions.R"; depends on the `dplyr` package
  #* The latest version can be found in `dampack` package
  df_cea_psa <- calculate_icers(cost       = df_out_ce_psa$meanCost, 
                                effect     = df_out_ce_psa$meanEffect,
                                strategies = df_out_ce_psa$Strategy)
  df_cea_psa
  
  
  ### Cost-effectiveness acceptability curves (CEACs) and frontier (CEAF) ---
  #* Functions included in "R/Functions.R". The latest versions can be found in `dampack` package
  ceac_obj <- ceac(wtp = v_wtp, psa = l_psa)

  #* Regions of highest probability of cost-effectiveness for each strategy
  summary(ceac_obj)
  #* CEAC & CEAF plot
  gg_ceac <- plot.ceac(ceac_obj, frontier = F, points = F, ylim = c(0,1), txtsize = txtsize, xlim = c(0, NA), n_x_ticks = 14) +
    ggthemes::scale_color_colorblind() +
    ggthemes::scale_fill_colorblind() +
    theme(legend.position = c(0.8, 0.48),
      axis.title.x = element_text(size=14),
          axis.title.y = element_text(size=14),
          axis.text.x = element_text(size=14),
          axis.text.y = element_text(size=14)
    )
  gg_ceac
  
  
  
  return(list(l_psa = l_psa, 
              df_out_ce_psa = df_out_ce_psa, 
              df_cea_psa = df_cea_psa,  
              ceac_obj = ceac_obj,
              gg_ceac = gg_ceac, 
              gg_scatter = gg_scatter,  
              mean_trace_SoC = mean_trace_SoC,
              mean_trace_strA = mean_trace_strA,
              trace_all = trace_all,
              time_in_states = time_in_states, 
              LEs = LEs,
              LEs_full = LEs_full,
              PAID_final = PAID_final
  ))
}


#### 6. Model for subgroup UN ####

#For subgroup uncontrolled BP, no CVD history

run_model_uncontrolled_nohis <- function() {
  
  
  ### Risk prediction model inputs (SOC) ----
  
  #The effect of the intervention on the transitions is due to the difference on systolic blood pressure 'sbp_diff'
  
  
  ### Transition rates (annual) ----
  #SCORE2 and SCORE2-OP
  score_tprobs <- gen_score_tprobs(intervention = "SOC",
                                   subgroup = "uncontrolled_nohis")
  
  score_p_MI1 <- as.matrix(score_tprobs$score_MI1)
  score_p_S1 <- as.matrix(score_tprobs$score_S1)
  score_p_CVD <- as.matrix(score_tprobs$score_CVD)
  
  
  box_score_tprobs <- gen_score_tprobs(intervention = "Box",
                                       subgroup = "uncontrolled_nohis")
  
  box_score_p_MI1 <- as.matrix(box_score_tprobs$score_MI1)
  box_score_p_S1 <- as.matrix(box_score_tprobs$score_S1)
  box_score_p_CVD <- as.matrix(box_score_tprobs$score_CVD)
  
  
  #SMART2
  smart_tprobs_fromMI <- gen_smart_tprobs(from_state = "MI",
                                          intervention = "SOC",
                                          subgroup = "uncontrolled_nohis")
  
  smart_p_MI1MI2 <- as.matrix(smart_tprobs_fromMI$smart_MI)
  smart_p_MI1S1 <- as.matrix(smart_tprobs_fromMI$smart_S)
  smart_p_MI2S1 <- as.matrix(smart_tprobs_fromMI$smart_S) # 
  smart_p_MI1CVD <- as.matrix(smart_tprobs_fromMI$smart_CVD)
  smart_p_MI2CVD <- as.matrix(smart_tprobs_fromMI$smart_CVD) # 
  
  
  smart_tprobs_fromS <- gen_smart_tprobs(from_state = "S",
                                         intervention = "SOC",
                                         subgroup = "uncontrolled_nohis")
  
  smart_p_S1S2 <- as.matrix(smart_tprobs_fromS$smart_S)
  smart_p_S1CVD <- as.matrix(smart_tprobs_fromS$smart_CVD)
  smart_p_S2CVD <- as.matrix(smart_tprobs_fromS$smart_CVD) #
  
  
  
  box_smart_tprobs_fromMI <- gen_smart_tprobs(from_state = "MI",
                                              intervention = "Box",
                                              subgroup = "uncontrolled_nohis")
  
  box_smart_p_MI1MI2 <- as.matrix(box_smart_tprobs_fromMI$smart_MI)
  box_smart_p_MI1S1 <- as.matrix(box_smart_tprobs_fromMI$smart_S)
  box_smart_p_MI2S1 <- as.matrix(box_smart_tprobs_fromMI$smart_S) # 
  box_smart_p_MI1CVD <- as.matrix(box_smart_tprobs_fromMI$smart_CVD)
  box_smart_p_MI2CVD <- as.matrix(box_smart_tprobs_fromMI$smart_CVD) # 
  
  
  box_smart_tprobs_fromS <- gen_smart_tprobs(from_state = "S",
                                             intervention = "Box",
                                             subgroup = "uncontrolled_nohis")
  
  box_smart_p_S1S2 <- as.matrix(box_smart_tprobs_fromS$smart_S)
  box_smart_p_S1CVD <- as.matrix(box_smart_tprobs_fromS$smart_CVD)
  box_smart_p_S2CVD <- as.matrix(box_smart_tprobs_fromS$smart_CVD) #
  
  ## Age-dependent mortality rates ----
  lt_NL_2021 <- nl_mort()   
  #* Extract age-specific all-cause mortality for ages in model time horizon
  v_r_mort_by_age <- lt_NL_2021 %>% 
    dplyr::filter(Age >= n_age_init & Age < n_age_max) %>%
    dplyr::select(Female, Male) 
  v_r_mort_by_age$Total <- v_r_mort_by_age$Female*(1-prop_male)+v_r_mort_by_age$Male*prop_male
  v_r_mort_by_age <- as.matrix(v_r_mort_by_age$Total)
  
  #Also extract age-specific scaling values for all-cause mortality to account for CVD deaths
  df_mort_p_scaling <- mort_p_scaling()
  v_p_mort_scale <- df_mort_p_scaling %>% 
    dplyr::filter(Age >= n_age_init & Age < n_age_max) %>%
    dplyr::select(Female, Male) 
  v_p_mort_scale$Total <- v_p_mort_scale$Female*(1-prop_male)+v_p_mort_scale$Male*prop_male
  v_p_mort_scale <- as.numeric(v_p_mort_scale$Total)
  
  ### Discount weight for costs and effects ----
  v_dwc  <- 1 / ((1 + (d_c * cycle_length)) ^ (0:n_cycles))
  v_dwe  <- 1 / ((1 + (d_e * cycle_length)) ^ (0:n_cycles))
  
  # Process model inputs ----
  ## Age-specific transition rates to the Dead state for all cycles ----
  v_r_HDage  <- rep(v_r_mort_by_age, each = 1/cycle_length)
  #* Name age-specific mortality vector 
  names(v_r_HDage) <- v_age_names
  
  #* Function included in "R/Functions.R". The latest version can be found in `darthtools` package
  v_p_HDage  <- rate_to_prob(v_r_HDage, t = cycle_length)  # Age-specific mortality risk in the Healthy state 
  v_p_HDage <- v_p_HDage*(1-v_p_mort_scale) 
  
  # Construct state-transition models ----
  ## Initial state vector ----
  #* All starting healthy
  v_m_init <- c(R = 1, MI1=0, MI2=0, S1 = 0, S2 = 0, CVD=0 , D = 0) 
  ## Initialize cohort traces ----
  ### Initialize cohort trace under SoC ----
  m_M_SoC <- array(NA, 
                   dim = c((n_cycles + 1), n_states, n_sim), 
                   dimnames = list(0:n_cycles, v_names_states, 1:n_sim))
  #* Store the initial state vector in the first row of the cohort trace
  m_M_SoC[1, , ] <- v_m_init
  
  ### Initialize cohort trace for competing strategies ----
  #* Structure and initial states are the same as for SoC
  m_M_strA  <- m_M_SoC # Strategy A
  
  
  
  ## Create transition probability arrays for strategy SoC ----
  ### Initialize transition probability array for strategy SoC ----
  #* All transitions to a non-death state are assumed to be conditional on survival
  a_P_SoC <- array(0,
                   dim  = c(n_states, n_states, n_cycles, n_sim),
                   dimnames = list(v_names_states, 
                                   v_names_states, 
                                   0:(n_cycles - 1),
                                   1:n_sim))
  
  # ### Fill in array
  ## From R
  a_P_SoC["R", "R", ,]   <- (1 - (v_p_HDage + score_p_CVD[,])) * (1 - (score_p_MI1[,] + score_p_S1[,]))
  a_P_SoC["R", "MI1", , ]  <- (1 - (v_p_HDage + score_p_CVD[,])) * score_p_MI1[,] 
  a_P_SoC["R", "S1", ,]  <- (1 - (v_p_HDage + score_p_CVD[,])) * score_p_S1[,] 
  a_P_SoC["R", "CVD", ,] <- score_p_CVD[,] 
  a_P_SoC["R", "D", ,]   <- v_p_HDage 
  ## From MI1
  a_P_SoC["MI1", "MI1", ,]  <- (1 - (smart_p_MI1CVD[,] + v_p_HDage)) * (1- (smart_p_MI1S1[,] + smart_p_MI1MI2[,]))
  a_P_SoC["MI1", "MI2", ,]  <- (1 - (smart_p_MI1CVD[,] + v_p_HDage)) * smart_p_MI1MI2[,]
  a_P_SoC["MI1", "S1", ,]  <-  (1 - (smart_p_MI1CVD[,] + v_p_HDage)) * smart_p_MI1S1[,] 
  a_P_SoC["MI1", "CVD", ,]  <- smart_p_MI1CVD[,] 
  a_P_SoC["MI1", "D", ,]  <-  v_p_HDage 
  ## From MI2
  a_P_SoC["MI2", "MI2", ,]  <- (1 - (smart_p_MI2CVD[,] + v_p_HDage)) * (1-smart_p_MI2S1[,]) 
  a_P_SoC["MI2", "S1", ,]  <- (1 - (smart_p_MI2CVD[,] + v_p_HDage)) * smart_p_MI2S1[,] 
  a_P_SoC["MI2", "CVD", ,]  <- smart_p_MI2CVD[,] 
  a_P_SoC["MI2", "D", ,]  <-  v_p_HDage 
  
  ## From S1
  a_P_SoC["S1", "S1", ,] <- (1 - (v_p_HDage + smart_p_S1CVD[,])) * (1-smart_p_S1S2[,]) 
  a_P_SoC["S1", "S2", ,] <- (1 - (v_p_HDage + smart_p_S1CVD[,])) * smart_p_S1S2[,]
  a_P_SoC["S1", "CVD", ,] <- smart_p_S1CVD[,]
  a_P_SoC["S1", "D", ,] <- v_p_HDage 
  
  ## From S2
  a_P_SoC["S2", "S2", ,] <- (1 - (v_p_HDage + smart_p_S2CVD[,])) 
  a_P_SoC["S2", "CVD", ,] <- smart_p_S2CVD[,]
  a_P_SoC["S2", "D", ,] <- v_p_HDage 
  
  ## From CVD
  a_P_SoC["CVD", "CVD", ,]   <- 1
  
  ## From D
  a_P_SoC["D", "D", ,]   <- 1
  
  ### Transition probability array for strategy: CVRM-Box ----
  #a_P_strA <- a_P_SoC
  a_P_strA <- array(0,
                    dim  = c(n_states, n_states, n_cycles, n_sim),
                    dimnames = list(v_names_states, 
                                    v_names_states, 
                                    0:(n_cycles - 1),
                                    1:n_sim))
  ## From R
  a_P_strA["R", "R", ,]   <- (1 - (v_p_HDage + box_score_p_CVD[,])) * (1 - (box_score_p_MI1[,] + box_score_p_S1[,]))
  a_P_strA["R", "MI1", ,]  <- (1 - (v_p_HDage + box_score_p_CVD[,])) * box_score_p_MI1[,] 
  a_P_strA["R", "S1", ,]  <- (1 - (v_p_HDage + box_score_p_CVD[,])) * box_score_p_S1[,] 
  a_P_strA["R", "CVD", ,] <- box_score_p_CVD[,] 
  a_P_strA["R", "D", ,]   <- v_p_HDage 
  ## From MI1
  a_P_strA["MI1", "MI1", ,]  <- (1 - (box_smart_p_MI1CVD[,] + v_p_HDage)) * (1- (box_smart_p_MI1S1[,] + box_smart_p_MI1MI2[,]))
  a_P_strA["MI1", "MI2", ,]  <- (1 - (box_smart_p_MI1CVD[,] + v_p_HDage)) * box_smart_p_MI1MI2[,]
  a_P_strA["MI1", "S1", ,]  <-  (1 - (box_smart_p_MI1CVD[,] + v_p_HDage)) * box_smart_p_MI1S1[,] 
  a_P_strA["MI1", "CVD", ,]  <- box_smart_p_MI1CVD[,] 
  a_P_strA["MI1", "D", ,]  <-  v_p_HDage 
  ## From MI2
  a_P_strA["MI2", "MI2", ,]  <- (1 - (box_smart_p_MI2CVD[,] + v_p_HDage)) * (1-box_smart_p_MI2S1[,]) 
  a_P_strA["MI2", "S1", ,]  <- (1 - (box_smart_p_MI2CVD[,] + v_p_HDage)) * box_smart_p_MI2S1[,] 
  a_P_strA["MI2", "CVD", ,]  <- box_smart_p_MI2CVD[,] 
  a_P_strA["MI2", "D", ,]  <-  v_p_HDage 
  
  ## From S1
  a_P_strA["S1", "S1", ,] <- (1 - (v_p_HDage + box_smart_p_S1CVD[,])) * (1-box_smart_p_S1S2[,]) 
  a_P_strA["S1", "S2", ,] <- (1 - (v_p_HDage + box_smart_p_S1CVD[,])) * box_smart_p_S1S2[,]
  a_P_strA["S1", "CVD", ,] <- box_smart_p_S1CVD[,]
  a_P_strA["S1", "D", ,] <- v_p_HDage 
  
  ## From S2
  a_P_strA["S2", "S2", ,] <- (1 - (v_p_HDage + box_smart_p_S2CVD[,])) 
  a_P_strA["S2", "CVD", ,] <- box_smart_p_S2CVD[,]
  a_P_strA["S2", "D", ,] <- v_p_HDage 
  
  ## From CVD
  a_P_strA["CVD", "CVD", ,]   <- 1
  
  ## From D
  a_P_strA["D", "D", ,]   <- 1
  
  ## Check if transition probability arrays are valid ----
  #* Functions included in "R/Functions.R". The latest version can be found in `darthtools` package
  ### Check that transition probabilities are [0, 1] ----
  for(sim in 1:n_sim) {
    #print(sim)
    check_transition_probability(a_P_SoC[,,,sim], verbose = TRUE)
    check_transition_probability(a_P_strA[,,,sim], verbose = TRUE)
    # Check that all rows for each slice of the array sum to 1
    sum_soc_tmp <- sum(rowSums(a_P_SoC[,,,sim]))
    if (sum_soc_tmp != n_states*n_cycles) {print("Invalid")}
    sum_a_tmp <- sum(rowSums(a_P_strA[,,,sim]))
    if (sum_a_tmp != n_states*n_cycles) {print("Invalid")}
  }
  
  ## Create transition dynamics arrays ----
  #* These arrays will capture transitions from each state to another over time 
  ### Initialize transition dynamics array for strategy SoC ----
  a_A_SoC <- array(0,
                   dim      = c(n_states, n_states, n_cycles + 1, n_sim),
                   dimnames = list(v_names_states, v_names_states, 0:n_cycles, 1:n_sim))
  a_A_strA  <- a_A_SoC #Structure and initial states are the same as for SoC
  
  
  #  Run Markov model ----
  #* Iterative solution of age-dependent cSTM
  for(i in 1:n_sim) {
    #* Set first slice of a_A_SoC and a_A_strA with the initial state vector in its diagonal
    diag(a_A_SoC[, , 1,i]) <- v_m_init
    diag(a_A_strA[, , 1,i]) <- v_m_init
    for(t in 1:n_cycles){
      ## Fill in cohort trace
      # For SoC
      m_M_SoC[t + 1, ,i]  <- m_M_SoC[t, ,i]  %*% a_P_SoC[, , t,i]
      # for strategy: CVRM-Box
      m_M_strA[t + 1, ,i] <- m_M_strA[t,,i ] %*% a_P_strA[, , t,i]
      
      ## Fill in transition-dynamics array
      # For SoC
      a_A_SoC[, , t + 1,i]  <- diag(m_M_SoC[t, ,i]) %*% a_P_SoC[, , t,i]
      # for strategy: CVRM-Box
      a_A_strA[, , t + 1,i] <- diag(m_M_strA[t, ,i]) %*% a_P_strA[, , t,i]
    }
  }
  ## Store the cohort traces in a list ----
  l_m_M <- list(SoC =  m_M_SoC,
                A   =  m_M_strA) 
  
  names(l_m_M) <- v_names_str
  
  ## Store the transition dynamics array for each strategy in a list ----
  l_a_A <- list(SoC =  a_A_SoC,
                A   =  a_A_strA) 
  names(l_a_A) <- v_names_str
  
  # Plot Outputs ----
  #* (Functions included in "R/Functions.R"; depends on the `ggplot2` package)
  
  # Plot the cohort trace for strategy SoC ----
  mean_trace_SoC <- apply(m_M_SoC, c(1:2), mean)
  trace_soc <- plot_trace(mean_trace_SoC)

  mean_trace_strA <- apply(m_M_strA, c(1:2), mean)
  trace_box <- plot_trace(mean_trace_strA)
  ## Plot the cohort trace for all strategies ----
  
  trace_all <- trace_all <- plot_trace_strategy(l_m_M)
  
  #time spent in states
  time_in_states_tmp <- array(NA, dim=c(n_str, n_states, n_sim), dimnames = list(v_names_str, v_names_states, 1:n_sim))
  #for SOC, there is no variation in the risks
  time_in_states_tmp["Standard of care",,] <- colMeans(apply(m_M_SoC,c(1,2), mean))*100
  time_in_states_tmp["Strategy A",,] <- colMeans(m_M_strA[,,])*100
  
  time_in_states <- matrix(nrow = length(v_names_str), ncol = n_states+1)
  rownames(time_in_states) <- v_names_str
  colnames(time_in_states) <- c("Option", "At risk", "Post-MI", "Post-recurrent MI", "Post-Stroke", "Post-recurrent Stroke", "CVD Death", "All-cause Death")
  time_in_states["Standard of care",] <- c("No treatment", 
                                           round(mean(time_in_states_tmp["Standard of care","R",]),1),
                                           round(mean(time_in_states_tmp["Standard of care","MI1",]),1),
                                           round(mean(time_in_states_tmp["Standard of care","MI2",]),1),
                                           round(mean(time_in_states_tmp["Standard of care","S1",]),1),
                                           round(mean(time_in_states_tmp["Standard of care","S2",]),1),
                                           round(mean(time_in_states_tmp["Standard of care","CVD",]),1),
                                           round(mean(time_in_states_tmp["Standard of care","D",]),1))
  
  time_in_states["Strategy A",] <- c("Treatment",
                                     round(mean(time_in_states_tmp["Strategy A","R",]),1),
                                     round(mean(time_in_states_tmp["Strategy A","MI1",]),1),
                                     round(mean(time_in_states_tmp["Strategy A","MI2",]),1),
                                     round(mean(time_in_states_tmp["Strategy A","S1",]),1),
                                     round(mean(time_in_states_tmp["Strategy A","S2",]),1),
                                     round(mean(time_in_states_tmp["Strategy A","CVD",]),1),
                                     round(mean(time_in_states_tmp["Strategy A","D",]),1))
  
  #Life expectancy output and costs of added life years
  LEs_full <- matrix(NA,nrow=n_sim,ncol=n_str)
  PAID_full_SOC <- matrix(NA,nrow=n_sim,ncol=n_cycles+1)
  PAID_full_strA <- matrix(NA,nrow=n_sim,ncol=n_cycles+1)
  
  
  colnames(LEs_full) <- v_names_str
  colnames(PAID_full_SOC) <- c(0:n_cycles)
  colnames(PAID_full_strA) <- c(0:n_cycles)
  
  
  for(i in 1:n_sim){
    LEs_full[i,"Standard of care"] <- sum(rowSums(m_M_SoC[, -which(v_names_states == "D" | v_names_states == "CVD"),i])*v_wcc)
    LEs_full[i,"Strategy A"] <- sum(rowSums(m_M_strA[, -which(v_names_states == "D" | v_names_states == "CVD"),i])*v_wcc)
    PAID_full_SOC[i,] <- as.numeric(rowSums(m_M_SoC[, -which(v_names_states == "D" | v_names_states == "CVD"),i]))
    PAID_full_strA[i,] <- as.numeric(rowSums(m_M_strA[, -which(v_names_states == "D" | v_names_states == "CVD"),i]))
    
  }
  PAID_final <- get_paid(intervention = PAID_full_strA, control = PAID_full_SOC)  
  
  LEs <- data.frame(Option = c("No treatment", "Treatment"),
                    LE = c(round(mean(LEs_full[,"Standard of care"]),2),
                           paste0(
                             round(mean(LEs_full[,"Strategy A"]),2),
                             " (",
                             round(quantile(LEs_full[,"Strategy A"], probs = 0.025),2),
                             ", ",
                             round(quantile(LEs_full[,"Strategy A"], probs = 0.975),2),
                             ")"
                           )))
  
  ## Plot the epidemiology outcomes ----
  ### Survival ----
  
  df_surv <- calc_surv(l_m_M, v_names_death_states= c("CVD", "D"))
  #print(df_surv)
  survival_plot <- ggplot(df_surv,
                          aes(x = Cycle, y = Survival, group = as.factor(Strategy))) +
    geom_line(aes(linetype = Strategy), size = 0.8) +
    
    xlab("Cycle") +
    ylab("Proportion") +
    ylim(0,1) +
    ggtitle("Survival probabilities") +
    theme_bw(base_size = 14) +
    theme()

  # State Rewards ----
  v_u_SoC    <- cbind(u_R_sgCN - u_hypertension,  
                      u_R_sgCN - u_hypertension - u_MI1,  
                      u_R_sgCN - u_hypertension - u_S1, 
                      u_R_sgCN - u_hypertension - u_MI1 - u_MI2, 
                      u_R_sgCN - u_hypertension - u_S1 - u_S2, 
                      u_D,
                      u_CVD)
  colnames(v_u_SoC) <- v_names_states 
  
  #* Vector of state costs under strategy SoC
  v_c_SoC    <- cbind(c_SOC_uncontrolled ,  
                      c_SOC_uncontrolled  + c_MI1, 
                      c_SOC_uncontrolled  + c_S1,  
                      c_SOC_uncontrolled  + c_MI1 + c_MI2,  
                      c_SOC_uncontrolled  + c_S1 + c_S2,  
                      c_D,
                      c_CVD)
  colnames(v_c_SoC) <- v_names_states 
  #* Vector of state utilities under strategy A
  v_u_strA   <- v_u_SoC
  
  #* Vector of state costs under strategy A
  v_c_strA   <- cbind(c_strA_uncontrolled , 
                      c_MI1  + c_strA_uncontrolled,
                      c_S1  + c_strA_uncontrolled, 
                      c_MI2 + c_MI1+ c_strA_uncontrolled, 
                      c_S2 + c_S1 + c_strA_uncontrolled, 
                      c_D,
                      c_CVD) 
  colnames(v_c_strA) <- v_names_states 
  
  
  ## Store state rewards ----
  #* Store the vectors of state utilities for each strategy in a list 
  l_u <- list(SoC = v_u_SoC,
              A   = v_u_strA)
  #* Store the vectors of state cost for each strategy in a list 
  l_c <- list(SoC =  v_c_SoC,
              A   =  v_c_strA)
  
  #* assign strategy names to matching items in the lists
  names(l_u) <- names(l_c) <- v_names_str
  
  # Compute expected outcomes ----
  #* Create empty vectors to store total utilities and costs 
  v_tot_qaly <- v_tot_cost <- vector(mode = "numeric", length = n_str)
  names(v_tot_qaly) <- names(v_tot_cost) <- v_names_str
  
  
  
  total_costs_sims <- as.data.frame(matrix(NA, nrow=n_sim, ncol=n_str))
  colnames(total_costs_sims) <- v_names_str
  total_QALY_sims <- as.data.frame(matrix(NA, nrow=n_sim, ncol=n_str))
  colnames(total_costs_sims) <- v_names_str
  
  cycle_costs_strA <- as.data.frame(matrix(data=NA, nrow = n_sim, ncol=n_cycles+1))
  cycle_costs_SOC<- as.data.frame(matrix(data=NA, nrow = n_sim, ncol=n_cycles+1))
  
  ## Loop through each strategy and calculate total utilities and costs ----
  for (sim in 1:n_sim){
    for (i in 1:n_str) { # i <- 1
      v_u_str <- l_u[[i]][sim,]   # select the vector of state utilities for the i-th strategy
      v_c_str <- l_c[[i]][sim,]   # select the vector of state costs for the i-th strategy
      a_A_str <- l_a_A[[i]][,,,sim] # select the transition array for the i-th strategy, simulation
      ##* Array of state rewards 
      #* Create transition matrices of state utilities and state costs for the i-th strategy 
      m_u_str   <- matrix(v_u_str, nrow = n_states, ncol = n_states, byrow = T)
      m_c_str   <- matrix(v_c_str, nrow = n_states, ncol = n_states, byrow = T)
      #* Expand the transition matrix of state utilities across cycles to form a transition array of state utilities
      a_R_u_str <- array(m_u_str, 
                         dim      = c(n_states, n_states, n_cycles + 1),
                         dimnames = list(v_names_states, v_names_states, 0:n_cycles))
      # Expand the transition matrix of state costs across cycles to form a transition array of state costs
      a_R_c_str <- array(m_c_str, 
                         dim      = c(n_states, n_states, n_cycles + 1),
                         dimnames = list(v_names_states, v_names_states, 0:n_cycles))

      ##* Apply transition rewards
      #* Apply disutility due to transition from H to S1
      a_R_u_str["R", "MI1", ]  <- a_R_u_str["R", "MI1", ]      - du_MI1[sim] #
      a_R_u_str["MI1", "MI2", ] <-  a_R_u_str["MI1", "MI2", ]  - du_MI2[sim] #
      a_R_u_str["MI2", "S1", ]  <- a_R_u_str["MI2", "S1", ]  - du_MItoS[sim] #
      a_R_u_str["R", "S1", ]  <- a_R_u_str["R", "S1", ]      - du_S1[sim] #
      a_R_u_str["MI1", "S1", ]  <- a_R_u_str["MI1", "S1", ]  - du_MItoS[sim] #
      a_R_u_str["S1", "S2", ] <-  a_R_u_str["S1", "S2", ]  - du_S2[sim] #
      
      
      #* Add transition cost per cycle due to transition from H to S1
      a_R_c_str["R", "MI1",]      <- a_R_c_str["R", "MI1",] + ic_MI1[sim] # below we apply productivity lossess
      if(n_age_init<=66){a_R_c_str["R", "MI1", 1:(67-(n_age_init+1))]      <- a_R_c_str["R", "MI1", 1:(67-(n_age_init+1))]  + ic_fric[sim]} #
      a_R_c_str["MI1", "MI2", ]      <- a_R_c_str["MI1", "MI2", ]       + ic_MI2[sim] #
      a_R_c_str["MI1", "S1", ]      <- a_R_c_str["MI1", "S1", ]       + ic_S1[sim] #
      a_R_c_str["R", "S1", ]      <- a_R_c_str["R", "S1", ]       + ic_S1[sim]  #
      if(n_age_init<=66){a_R_c_str["R", "S1", 1:(67-(n_age_init+1))]      <- a_R_c_str["R", "S1", 1:(67-(n_age_init+1))]       + ic_fric[sim]} #
      a_R_c_str["S1", "S2", ]      <- a_R_c_str["S1", "S2", ]       + ic_S2[sim] #
      a_R_c_str["MI2", "CVD", ]      <- a_R_c_str["MI2", "CVD", ]       + ic_CVD[sim]
      a_R_c_str["MI2", "S1", ]      <- a_R_c_str["MI2", "S1", ]       + ic_S2[sim]
      a_R_c_str["S2", "CVD", ]      <- a_R_c_str["S2", "CVD", ]       + ic_CVD[sim]
      a_R_c_str["R", "CVD", ]      <- a_R_c_str["R", "CVD", ]       + ic_CVD[sim]
      
      
      
      
      ###* Expected QALYs and costs for all transitions per cycle
      #* QALYs = life years x QoL
      
      a_Y_c_str <- a_A_str * a_R_c_str
      a_Y_u_str <- a_A_str * a_R_u_str 
      
      ###* Expected QALYs and costs per cycle
      ##* Vector of QALYs and costs
      v_qaly_str <- apply(a_Y_u_str, 3, sum) # sum the proportion of the cohort across transitions 
      v_cost_str <- apply(a_Y_c_str, 3, sum) # sum the proportion of the cohort across transitions
      
      
      #* QALYs
      v_tot_qaly[i] <- t(v_qaly_str) %*% (v_dwe * v_wcc)
      #* Costs
      v_tot_cost[i] <- t(v_cost_str) %*% (v_dwc * v_wcc)
      
      ## Vector with discounted values
      if(i==1){
        total_QALY_sims[sim,i] <- v_tot_qaly[i]
        total_costs_sims[sim,i] <- v_tot_cost[i]
        cycle_costs_SOC[sim,] <- v_cost_str
      }
      if(i==2){
        total_QALY_sims[sim,i] <- v_tot_qaly[i]
        total_costs_sims[sim,i] <- v_tot_cost[i] + c_init_strA + (PAID_final$intervention[sim] - PAID_final$control[sim]) # note here we add the initial intervention costs and costs of added life years
        cycle_costs_strA[sim,] <- v_cost_str
      }
    }
  }
  
  
  ## Visualize PSA results for CEA ----
  ### Create PSA object ----
  #* Function included in "R/Functions.R" The latest version can be found in `dampack` package
  l_psa <- make_psa_obj(cost          = total_costs_sims, 
                        effectiveness = total_QALY_sims, 
                        parameters    = df_psa_input, 
                        strategies    = v_names_str)
  l_psa$strategies <- v_names_str
  colnames(l_psa$effectiveness) <- v_names_str
  colnames(l_psa$cost) <- v_names_str
  
  #* Vector with willingness-to-pay (WTP) thresholds.
  v_wtp <- seq(0, 80000, by = 1000)
  
  ### Cost-Effectiveness Scatter plot ----
  txtsize <- 13
  
  gg_scatter <- inc_scatter(l_psa, wtp_line = 20000, txtsize=txtsize)+
    theme(legend.position = c(0.8, 0.48),
          axis.title.x = element_text(size=14),
          axis.title.y = element_text(size=14),
          axis.text.x = element_text(size=14),
          axis.text.y = element_text(size=14)
    )
  
  ### Incremental cost-effectiveness ratios (ICERs) with probabilistic output ----
  #* Compute expected costs and effects for each strategy from the PSA
  #* Function included in "R/Functions.R". The latest version can be found in `dampack` package
  df_out_ce_psa <- summary(l_psa)
  
  #* Function included in "R/Functions.R"; depends on the `dplyr` package
  #* The latest version can be found in `dampack` package
  df_cea_psa <- calculate_icers(cost       = df_out_ce_psa$meanCost, 
                                effect     = df_out_ce_psa$meanEffect,
                                strategies = df_out_ce_psa$Strategy)
  df_cea_psa
  
  
  ### Cost-effectiveness acceptability curves (CEACs) and frontier (CEAF) ---
  #* Functions included in "R/Functions.R". The latest versions can be found in `dampack` package
  ceac_obj <- ceac(wtp = v_wtp, psa = l_psa)
  
  #* Regions of highest probability of cost-effectiveness for each strategy
  summary(ceac_obj)
  #* CEAC & CEAF plot
  gg_ceac <- plot.ceac(ceac_obj, frontier = F, points = F, ylim = c(0,1), txtsize = txtsize, xlim = c(0, NA), n_x_ticks = 14) +
    ggthemes::scale_color_colorblind() +
    ggthemes::scale_fill_colorblind() +
    theme(legend.position = c(0.8, 0.48),
          axis.title.x = element_text(size=14),
          axis.title.y = element_text(size=14),
          axis.text.x = element_text(size=14),
          axis.text.y = element_text(size=14)
    )
  gg_ceac

  
  return(list(l_psa = l_psa, 
              df_out_ce_psa = df_out_ce_psa, 
              df_cea_psa = df_cea_psa,  
              ceac_obj = ceac_obj,
              gg_ceac = gg_ceac, 
              gg_scatter = gg_scatter,  
              mean_trace_SoC = mean_trace_SoC,
              mean_trace_strA = mean_trace_strA,
              trace_all = trace_all,
              time_in_states = time_in_states, 
              LEs = LEs,
              LEs_full = LEs_full,
              PAID_final = PAID_final
  ))
  
}


#### 7. Model for subgroup CY ####

#For subgroup controlled BP, with a CVD history

run_model_controlled_yeshis <- function() {
  
  
  ### Risk prediction model inputs (SOC) ----
  
  #The effect of the intervention on the transitions is due to the difference on systolic blood pressure 'sbp_diff'
  
  
  ### Transition rates (annual) ----
  #SCORE2 and SCORE2-OP
  score_tprobs <- gen_score_tprobs(intervention = "SOC",
                                   subgroup = "controlled_nohis") #this will not be used in this run because people start in MI or Stroke
  
  score_p_MI1 <- as.matrix(score_tprobs$score_MI1)
  score_p_S1 <- as.matrix(score_tprobs$score_S1)
  score_p_CVD <- as.matrix(score_tprobs$score_CVD)
  
  
  box_score_tprobs <- gen_score_tprobs(intervention = "Box",
                                       subgroup = "controlled_nohis") #this will not be used in this run because people start in MI or Stroke
  
  box_score_p_MI1 <- as.matrix(box_score_tprobs$score_MI1)
  box_score_p_S1 <- as.matrix(box_score_tprobs$score_S1)
  box_score_p_CVD <- as.matrix(box_score_tprobs$score_CVD)
  
  
  #SMART2
  smart_tprobs_fromMI <- gen_smart_tprobs(from_state = "MI",
                                          intervention = "SOC",
                                          subgroup = "controlled_yeshis")
  
  smart_p_MI1MI2 <- as.matrix(smart_tprobs_fromMI$smart_MI)
  smart_p_MI1S1 <- as.matrix(smart_tprobs_fromMI$smart_S)
  smart_p_MI2S1 <- as.matrix(smart_tprobs_fromMI$smart_S) # 
  smart_p_MI1CVD <- as.matrix(smart_tprobs_fromMI$smart_CVD)
  smart_p_MI2CVD <- as.matrix(smart_tprobs_fromMI$smart_CVD) # 
  
  
  smart_tprobs_fromS <- gen_smart_tprobs(from_state = "S",
                                         intervention = "SOC",
                                         subgroup = "controlled_yeshis")
  
  smart_p_S1S2 <- as.matrix(smart_tprobs_fromS$smart_S)
  smart_p_S1CVD <- as.matrix(smart_tprobs_fromS$smart_CVD)
  smart_p_S2CVD <- as.matrix(smart_tprobs_fromS$smart_CVD) #
  
  
  
  box_smart_tprobs_fromMI <- gen_smart_tprobs(from_state = "MI",
                                              intervention = "Box",
                                              subgroup = "controlled_yeshis")
  
  box_smart_p_MI1MI2 <- as.matrix(box_smart_tprobs_fromMI$smart_MI)
  box_smart_p_MI1S1 <- as.matrix(box_smart_tprobs_fromMI$smart_S)
  box_smart_p_MI2S1 <- as.matrix(box_smart_tprobs_fromMI$smart_S) # 
  box_smart_p_MI1CVD <- as.matrix(box_smart_tprobs_fromMI$smart_CVD)
  box_smart_p_MI2CVD <- as.matrix(box_smart_tprobs_fromMI$smart_CVD) # 
  
  
  box_smart_tprobs_fromS <- gen_smart_tprobs(from_state = "S",
                                             intervention = "Box",
                                             subgroup = "controlled_yeshis")
  
  box_smart_p_S1S2 <- as.matrix(box_smart_tprobs_fromS$smart_S)
  box_smart_p_S1CVD <- as.matrix(box_smart_tprobs_fromS$smart_CVD)
  box_smart_p_S2CVD <- as.matrix(box_smart_tprobs_fromS$smart_CVD) #
  
  ## Age-dependent mortality rates ----
  lt_NL_2021 <- nl_mort()   
  #* Extract age-specific all-cause mortality for ages in model time horizon
  v_r_mort_by_age <- lt_NL_2021 %>% 
    dplyr::filter(Age >= n_age_init & Age < n_age_max) %>%
    dplyr::select(Female, Male) 
  v_r_mort_by_age$Total <- v_r_mort_by_age$Female*(1-prop_male)+v_r_mort_by_age$Male*prop_male
  v_r_mort_by_age <- as.matrix(v_r_mort_by_age$Total)
  
  #Also extract age-specific scaling values for all-cause mortality to account for CVD deaths
  df_mort_p_scaling <- mort_p_scaling()
  v_p_mort_scale <- df_mort_p_scaling %>% 
    dplyr::filter(Age >= n_age_init & Age < n_age_max) %>%
    dplyr::select(Female, Male) 
  v_p_mort_scale$Total <- v_p_mort_scale$Female*(1-prop_male)+v_p_mort_scale$Male*prop_male
  v_p_mort_scale <- as.numeric(v_p_mort_scale$Total)
  
  ### Discount weight for costs and effects ----
  v_dwc  <- 1 / ((1 + (d_c * cycle_length)) ^ (0:n_cycles))
  v_dwe  <- 1 / ((1 + (d_e * cycle_length)) ^ (0:n_cycles))
  
  # Process model inputs ----
  ## Age-specific transition rates to the Dead state for all cycles ----
  v_r_HDage  <- rep(v_r_mort_by_age, each = 1/cycle_length)
  #* Name age-specific mortality vector 
  names(v_r_HDage) <- v_age_names
  
  #* Function included in "R/Functions.R". The latest version can be found in `darthtools` package
  v_p_HDage  <- rate_to_prob(v_r_HDage, t = cycle_length)  # Age-specific mortality risk in the Healthy state 
  v_p_HDage <- v_p_HDage*(1-v_p_mort_scale) 
  
  # Construct state-transition models ----
  ## Initial state vector ----
  #* All starting healthy
  v_m_init <- c(R = 0, MI1=0.64, MI2=0, S1 = 0.36, S2 = 0, CVD=0 , D = 0) 
  ## Initialize cohort traces ----
  ### Initialize cohort trace under SoC ----
  m_M_SoC <- array(NA, 
                   dim = c((n_cycles + 1), n_states, n_sim), 
                   dimnames = list(0:n_cycles, v_names_states, 1:n_sim))
  #* Store the initial state vector in the first row of the cohort trace
  m_M_SoC[1, , ] <- v_m_init
  
  ### Initialize cohort trace for competing strategies ----
  #* Structure and initial states are the same as for SoC
  m_M_strA  <- m_M_SoC # Strategy A
  
  
  
  ## Create transition probability arrays for strategy SoC ----
  ### Initialize transition probability array for strategy SoC ----
  #* All transitions to a non-death state are assumed to be conditional on survival
  a_P_SoC <- array(0,
                   dim  = c(n_states, n_states, n_cycles, n_sim),
                   dimnames = list(v_names_states, 
                                   v_names_states, 
                                   0:(n_cycles - 1),
                                   1:n_sim))
  
  # ### Fill in array
  ## From R
  a_P_SoC["R", "R", ,]   <- (1 - (v_p_HDage + score_p_CVD[,])) * (1 - (score_p_MI1[,] + score_p_S1[,]))
  a_P_SoC["R", "MI1", , ]  <- (1 - (v_p_HDage + score_p_CVD[,])) * score_p_MI1[,] 
  a_P_SoC["R", "S1", ,]  <- (1 - (v_p_HDage + score_p_CVD[,])) * score_p_S1[,] 
  a_P_SoC["R", "CVD", ,] <- score_p_CVD[,] 
  a_P_SoC["R", "D", ,]   <- v_p_HDage 
  ## From MI1
  a_P_SoC["MI1", "MI1", ,]  <- (1 - (smart_p_MI1CVD[,] + v_p_HDage)) * (1- (smart_p_MI1S1[,] + smart_p_MI1MI2[,]))
  a_P_SoC["MI1", "MI2", ,]  <- (1 - (smart_p_MI1CVD[,] + v_p_HDage)) * smart_p_MI1MI2[,]
  a_P_SoC["MI1", "S1", ,]  <-  (1 - (smart_p_MI1CVD[,] + v_p_HDage)) * smart_p_MI1S1[,] 
  a_P_SoC["MI1", "CVD", ,]  <- smart_p_MI1CVD[,] 
  a_P_SoC["MI1", "D", ,]  <-  v_p_HDage 
  ## From MI2
  a_P_SoC["MI2", "MI2", ,]  <- (1 - (smart_p_MI2CVD[,] + v_p_HDage)) * (1-smart_p_MI2S1[,]) 
  a_P_SoC["MI2", "S1", ,]  <- (1 - (smart_p_MI2CVD[,] + v_p_HDage)) * smart_p_MI2S1[,] 
  a_P_SoC["MI2", "CVD", ,]  <- smart_p_MI2CVD[,] 
  a_P_SoC["MI2", "D", ,]  <-  v_p_HDage 
  
  ## From S1
  a_P_SoC["S1", "S1", ,] <- (1 - (v_p_HDage + smart_p_S1CVD[,])) * (1-smart_p_S1S2[,]) 
  a_P_SoC["S1", "S2", ,] <- (1 - (v_p_HDage + smart_p_S1CVD[,])) * smart_p_S1S2[,]
  a_P_SoC["S1", "CVD", ,] <- smart_p_S1CVD[,]
  a_P_SoC["S1", "D", ,] <- v_p_HDage 
  
  ## From S2
  a_P_SoC["S2", "S2", ,] <- (1 - (v_p_HDage + smart_p_S2CVD[,])) 
  a_P_SoC["S2", "CVD", ,] <- smart_p_S2CVD[,]
  a_P_SoC["S2", "D", ,] <- v_p_HDage 
  
  ## From CVD
  a_P_SoC["CVD", "CVD", ,]   <- 1
  
  ## From D
  a_P_SoC["D", "D", ,]   <- 1
  
  ### Transition probability array for strategy: CVRM-Box ----
  #a_P_strA <- a_P_SoC
  a_P_strA <- array(0,
                    dim  = c(n_states, n_states, n_cycles, n_sim),
                    dimnames = list(v_names_states, 
                                    v_names_states, 
                                    0:(n_cycles - 1),
                                    1:n_sim))
  ## From R
  a_P_strA["R", "R", ,]   <- (1 - (v_p_HDage + box_score_p_CVD[,])) * (1 - (box_score_p_MI1[,] + box_score_p_S1[,]))
  a_P_strA["R", "MI1", ,]  <- (1 - (v_p_HDage + box_score_p_CVD[,])) * box_score_p_MI1[,] 
  a_P_strA["R", "S1", ,]  <- (1 - (v_p_HDage + box_score_p_CVD[,])) * box_score_p_S1[,] 
  a_P_strA["R", "CVD", ,] <- box_score_p_CVD[,] 
  a_P_strA["R", "D", ,]   <- v_p_HDage 
  ## From MI1
  a_P_strA["MI1", "MI1", ,]  <- (1 - (box_smart_p_MI1CVD[,] + v_p_HDage)) * (1- (box_smart_p_MI1S1[,] + box_smart_p_MI1MI2[,]))
  a_P_strA["MI1", "MI2", ,]  <- (1 - (box_smart_p_MI1CVD[,] + v_p_HDage)) * box_smart_p_MI1MI2[,]
  a_P_strA["MI1", "S1", ,]  <-  (1 - (box_smart_p_MI1CVD[,] + v_p_HDage)) * box_smart_p_MI1S1[,] 
  a_P_strA["MI1", "CVD", ,]  <- box_smart_p_MI1CVD[,] 
  a_P_strA["MI1", "D", ,]  <-  v_p_HDage 
  ## From MI2
  a_P_strA["MI2", "MI2", ,]  <- (1 - (box_smart_p_MI2CVD[,] + v_p_HDage)) * (1-box_smart_p_MI2S1[,]) 
  a_P_strA["MI2", "S1", ,]  <- (1 - (box_smart_p_MI2CVD[,] + v_p_HDage)) * box_smart_p_MI2S1[,] 
  a_P_strA["MI2", "CVD", ,]  <- box_smart_p_MI2CVD[,] 
  a_P_strA["MI2", "D", ,]  <-  v_p_HDage 
  
  ## From S1
  a_P_strA["S1", "S1", ,] <- (1 - (v_p_HDage + box_smart_p_S1CVD[,])) * (1-box_smart_p_S1S2[,]) 
  a_P_strA["S1", "S2", ,] <- (1 - (v_p_HDage + box_smart_p_S1CVD[,])) * box_smart_p_S1S2[,]
  a_P_strA["S1", "CVD", ,] <- box_smart_p_S1CVD[,]
  a_P_strA["S1", "D", ,] <- v_p_HDage 
  
  ## From S2
  a_P_strA["S2", "S2", ,] <- (1 - (v_p_HDage + box_smart_p_S2CVD[,])) 
  a_P_strA["S2", "CVD", ,] <- box_smart_p_S2CVD[,]
  a_P_strA["S2", "D", ,] <- v_p_HDage 
  
  ## From CVD
  a_P_strA["CVD", "CVD", ,]   <- 1
  
  ## From D
  a_P_strA["D", "D", ,]   <- 1
  
  ## Check if transition probability arrays are valid ----
  #* Functions included in "R/Functions.R". The latest version can be found in `darthtools` package
  ### Check that transition probabilities are [0, 1] ----
  for(sim in 1:n_sim) {
    #print(sim)
    check_transition_probability(a_P_SoC[,,,sim], verbose = TRUE)
    check_transition_probability(a_P_strA[,,,sim], verbose = TRUE)
    # Check that all rows for each slice of the array sum to 1
    sum_soc_tmp <- sum(rowSums(a_P_SoC[,,,sim]))
    if (sum_soc_tmp != n_states*n_cycles) {print("Invalid")}
    sum_a_tmp <- sum(rowSums(a_P_strA[,,,sim]))
    if (sum_a_tmp != n_states*n_cycles) {print("Invalid")}
  }
  
  ## Create transition dynamics arrays ----
  #* These arrays will capture transitions from each state to another over time 
  ### Initialize transition dynamics array for strategy SoC ----
  a_A_SoC <- array(0,
                   dim      = c(n_states, n_states, n_cycles + 1, n_sim),
                   dimnames = list(v_names_states, v_names_states, 0:n_cycles, 1:n_sim))
  a_A_strA  <- a_A_SoC #Structure and initial states are the same as for SoC
  
  
  #  Run Markov model ----
  #* Iterative solution of age-dependent cSTM
  for(i in 1:n_sim) {
    #* Set first slice of a_A_SoC and a_A_strA with the initial state vector in its diagonal
    diag(a_A_SoC[, , 1,i]) <- v_m_init
    diag(a_A_strA[, , 1,i]) <- v_m_init
    for(t in 1:n_cycles){
      ## Fill in cohort trace
      # For SoC
      m_M_SoC[t + 1, ,i]  <- m_M_SoC[t, ,i]  %*% a_P_SoC[, , t,i]
      # for strategy: CVRM-Box
      m_M_strA[t + 1, ,i] <- m_M_strA[t,,i ] %*% a_P_strA[, , t,i]
      
      ## Fill in transition-dynamics array
      # For SoC
      a_A_SoC[, , t + 1,i]  <- diag(m_M_SoC[t, ,i]) %*% a_P_SoC[, , t,i]
      # for strategy: CVRM-Box
      a_A_strA[, , t + 1,i] <- diag(m_M_strA[t, ,i]) %*% a_P_strA[, , t,i]
    }
  }
  ## Store the cohort traces in a list ----
  l_m_M <- list(SoC =  m_M_SoC,
                A   =  m_M_strA) 
  
  names(l_m_M) <- v_names_str
  
  ## Store the transition dynamics array for each strategy in a list ----
  l_a_A <- list(SoC =  a_A_SoC,
                A   =  a_A_strA) 
  names(l_a_A) <- v_names_str
  
  # Plot Outputs ----
  #* (Functions included in "R/Functions.R"; depends on the `ggplot2` package)
  
  # Plot the cohort trace for strategy SoC ----
  mean_trace_SoC <- apply(m_M_SoC, c(1:2), mean)
  trace_soc <- plot_trace(mean_trace_SoC)
  
  mean_trace_strA <- apply(m_M_strA, c(1:2), mean)
  trace_box <- plot_trace(mean_trace_strA)
  ## Plot the cohort trace for all strategies ----
  trace_all <- plot_trace_strategy(l_m_M)
  
  #time spent in states
  time_in_states_tmp <- array(NA, dim=c(n_str, n_states, n_sim), dimnames = list(v_names_str, v_names_states, 1:n_sim))
  #for SOC, there is no variation in the risks
  time_in_states_tmp["Standard of care",,] <- colMeans(apply(m_M_SoC,c(1,2), mean))*100
  time_in_states_tmp["Strategy A",,] <- colMeans(m_M_strA[,,])*100
  
  time_in_states <- matrix(nrow = length(v_names_str), ncol = n_states+1)
  rownames(time_in_states) <- v_names_str
  colnames(time_in_states) <- c("Option", "At risk", "Post-MI", "Post-recurrent MI", "Post-Stroke", "Post-recurrent Stroke", "CVD Death", "All-cause Death")
  time_in_states["Standard of care",] <- c("No treatment", 
                                           round(mean(time_in_states_tmp["Standard of care","R",]),1),
                                           round(mean(time_in_states_tmp["Standard of care","MI1",]),1),
                                           round(mean(time_in_states_tmp["Standard of care","MI2",]),1),
                                           round(mean(time_in_states_tmp["Standard of care","S1",]),1),
                                           round(mean(time_in_states_tmp["Standard of care","S2",]),1),
                                           round(mean(time_in_states_tmp["Standard of care","CVD",]),1),
                                           round(mean(time_in_states_tmp["Standard of care","D",]),1))
  
  time_in_states["Strategy A",] <- c("Treatment",
                                     round(mean(time_in_states_tmp["Strategy A","R",]),1),
                                     round(mean(time_in_states_tmp["Strategy A","MI1",]),1),
                                     round(mean(time_in_states_tmp["Strategy A","MI2",]),1),
                                     round(mean(time_in_states_tmp["Strategy A","S1",]),1),
                                     round(mean(time_in_states_tmp["Strategy A","S2",]),1),
                                     round(mean(time_in_states_tmp["Strategy A","CVD",]),1),
                                     round(mean(time_in_states_tmp["Strategy A","D",]),1))
  
  #Life expectancy output and costs of added life years
  LEs_full <- matrix(NA,nrow=n_sim,ncol=n_str)
  PAID_full_SOC <- matrix(NA,nrow=n_sim,ncol=n_cycles+1)
  PAID_full_strA <- matrix(NA,nrow=n_sim,ncol=n_cycles+1)
  
  
  colnames(LEs_full) <- v_names_str
  colnames(PAID_full_SOC) <- c(0:n_cycles)
  colnames(PAID_full_strA) <- c(0:n_cycles)
  
  
  for(i in 1:n_sim){
    LEs_full[i,"Standard of care"] <- sum(rowSums(m_M_SoC[, -which(v_names_states == "D" | v_names_states == "CVD"),i])*v_wcc)
    LEs_full[i,"Strategy A"] <- sum(rowSums(m_M_strA[, -which(v_names_states == "D" | v_names_states == "CVD"),i])*v_wcc)
    PAID_full_SOC[i,] <- as.numeric(rowSums(m_M_SoC[, -which(v_names_states == "D" | v_names_states == "CVD"),i]))
    PAID_full_strA[i,] <- as.numeric(rowSums(m_M_strA[, -which(v_names_states == "D" | v_names_states == "CVD"),i]))
    
  }
  PAID_final <- get_paid(intervention = PAID_full_strA, control = PAID_full_SOC) 
  
  LEs <- data.frame(Option = c("No treatment", "Treatment"),
                    LE = c(round(mean(LEs_full[,"Standard of care"]),2),
                           paste0(
                             round(mean(LEs_full[,"Strategy A"]),2),
                             " (",
                             round(quantile(LEs_full[,"Strategy A"], probs = 0.025),2),
                             ", ",
                             round(quantile(LEs_full[,"Strategy A"], probs = 0.975),2),
                             ")"
                           )))
  
  ## Plot the epidemiology outcomes ----
  ### Survival ----
  
  df_surv <- calc_surv(l_m_M, v_names_death_states= c("CVD", "D"))
  #print(df_surv)
  survival_plot <- ggplot(df_surv,
                          aes(x = Cycle, y = Survival, group = as.factor(Strategy))) +
    geom_line(aes(linetype = Strategy), size = 0.8) +
    
    xlab("Cycle") +
    ylab("Proportion") +
    ylim(0,1) +
    ggtitle("Survival probabilities") +
    theme_bw(base_size = 14) +
    theme()

  # State Rewards ----
  v_u_SoC    <- cbind(u_R_sgCN,  
                      u_R_sgCN - u_MI1,  
                      u_R_sgCN - u_S1, 
                      u_R_sgCN - u_MI1 - u_MI2, 
                      u_R_sgCN - u_S1 - u_S2, 
                      u_D,
                      u_CVD)
  colnames(v_u_SoC) <- v_names_states 
  
  #* Vector of state costs under strategy SoC
  v_c_SoC    <- cbind(c_SOC_controlled ,  
                      c_SOC_controlled +  c_MI1, 
                      c_SOC_controlled +  c_S1,  
                      c_SOC_controlled +  c_MI1 + c_MI2,  
                      c_SOC_controlled +  c_S1 + c_S2,  
                      c_D,
                      c_CVD)
  colnames(v_c_SoC) <- v_names_states 
  #* Vector of state utilities under strategy A
  v_u_strA   <- v_u_SoC
  
  #* Vector of state costs under strategy A
  v_c_strA   <- cbind(c_strA_controlled, 
                      c_MI1+ c_strA_controlled,
                      c_S1+ c_strA_controlled, 
                      c_MI2+ c_MI1+ c_strA_controlled, 
                      c_S2+ c_S1 + c_strA_controlled, 
                      c_D,
                      c_CVD) 
  colnames(v_c_strA) <- v_names_states 
  
  
  ## Store state rewards ----
  #* Store the vectors of state utilities for each strategy in a list 
  l_u <- list(SoC = v_u_SoC,
              A   = v_u_strA)
  #* Store the vectors of state cost for each strategy in a list 
  l_c <- list(SoC =  v_c_SoC,
              A   =  v_c_strA)
  
  #* assign strategy names to matching items in the lists
  names(l_u) <- names(l_c) <- v_names_str
  
  # Compute expected outcomes ----
  #* Create empty vectors to store total utilities and costs 
  v_tot_qaly <- v_tot_cost <- vector(mode = "numeric", length = n_str)
  names(v_tot_qaly) <- names(v_tot_cost) <- v_names_str
  
  
  
  total_costs_sims <- as.data.frame(matrix(NA, nrow=n_sim, ncol=n_str))
  colnames(total_costs_sims) <- v_names_str
  total_QALY_sims <- as.data.frame(matrix(NA, nrow=n_sim, ncol=n_str))
  colnames(total_costs_sims) <- v_names_str
  
  cycle_costs_strA <- as.data.frame(matrix(data=NA, nrow = n_sim, ncol=n_cycles+1))
  cycle_costs_SOC<- as.data.frame(matrix(data=NA, nrow = n_sim, ncol=n_cycles+1))
  
  ## Loop through each strategy and calculate total utilities and costs ----
  for (sim in 1:n_sim){
    for (i in 1:n_str) { # i <- 1
      v_u_str <- l_u[[i]][sim,]   # select the vector of state utilities for the i-th strategy
      v_c_str <- l_c[[i]][sim,]   # select the vector of state costs for the i-th strategy
      a_A_str <- l_a_A[[i]][,,,sim] # select the transition array for the i-th strategy, simulation
      ##* Array of state rewards 
      #* Create transition matrices of state utilities and state costs for the i-th strategy 
      m_u_str   <- matrix(v_u_str, nrow = n_states, ncol = n_states, byrow = T)
      m_c_str   <- matrix(v_c_str, nrow = n_states, ncol = n_states, byrow = T)
      #* Expand the transition matrix of state utilities across cycles to form a transition array of state utilities
      a_R_u_str <- array(m_u_str, 
                         dim      = c(n_states, n_states, n_cycles + 1),
                         dimnames = list(v_names_states, v_names_states, 0:n_cycles))
      # Expand the transition matrix of state costs across cycles to form a transition array of state costs
      a_R_c_str <- array(m_c_str, 
                         dim      = c(n_states, n_states, n_cycles + 1),
                         dimnames = list(v_names_states, v_names_states, 0:n_cycles))
      
      
      ##* Apply transition rewards
      #* Apply disutility due to transition from H to S1
      a_R_u_str["R", "MI1", ]  <- 0 #
      a_R_u_str["MI1", "MI2", ] <-  a_R_u_str["MI1", "MI2", ]  - du_MI2[sim] #
      a_R_u_str["MI2", "S1", ]  <- a_R_u_str["MI2", "S1", ]  - du_MItoS[sim] #
      a_R_u_str["R", "S1", ]  <- 0 #
      a_R_u_str["MI1", "S1", ]  <- a_R_u_str["MI1", "S1", ]  - du_MItoS[sim] #
      a_R_u_str["S1", "S2", ] <-  a_R_u_str["S1", "S2", ]  -  du_S2[sim] #
      
      
      #* Add transition cost per cycle due to transition from H to S1
      a_R_c_str["R", "MI1", ]      <- 0 #
      a_R_c_str["MI1", "MI2", ]      <- a_R_c_str["MI1", "MI2", ]    +    ic_MI2[sim] # #
      if(n_age_init<=66){a_R_c_str["MI1", "MI2", 1:(67-n_age_init+1)]      <- a_R_c_str["MI1", "MI2", 1:(67-n_age_init+1)]    +    ic_fric[sim]} #
      a_R_c_str["MI1", "S1", ]      <- a_R_c_str["MI1", "S1", ]       + ic_S1[sim]  #
      a_R_c_str["R", "S1", ]      <- 0 #
      a_R_c_str["S1", "S2", ]      <- a_R_c_str["S1", "S2", ]      +  ic_S2[sim] # friction costs should only be applied once #
      if(n_age_init<=66){a_R_c_str["S1", "S2", 1:(67-n_age_init+1)]      <- a_R_c_str["S1", "S2", 1:(67-n_age_init+1)]      +  ic_fric[sim]} # friction costs should only be applied once #
      a_R_c_str["MI2", "CVD", ]      <- a_R_c_str["MI2", "CVD", ]       + ic_CVD[sim]
      a_R_c_str["MI2", "S1", ]      <- a_R_c_str["MI2", "S1", ]       + ic_S2[sim]
      a_R_c_str["S2", "CVD", ]      <- a_R_c_str["S2", "CVD", ]       + ic_CVD[sim]
      a_R_c_str["R", "CVD", ]      <- 0
      
      
      
      
      ###* Expected QALYs and costs for all transitions per cycle
      #* QALYs = life years x QoL
      
      a_Y_c_str <- a_A_str * a_R_c_str
      a_Y_u_str <- a_A_str * a_R_u_str 
      
      ###* Expected QALYs and costs per cycle
      ##* Vector of QALYs and costs
      v_qaly_str <- apply(a_Y_u_str, 3, sum) # sum the proportion of the cohort across transitions 
      v_cost_str <- apply(a_Y_c_str, 3, sum) # sum the proportion of the cohort across transitions
      
      
      #* QALYs
      v_tot_qaly[i] <- t(v_qaly_str) %*% (v_dwe * v_wcc)
      #* Costs
      v_tot_cost[i] <- t(v_cost_str) %*% (v_dwc * v_wcc)
      
      ## Vector with discounted values
      if(i==1){
        total_QALY_sims[sim,i] <- v_tot_qaly[i]
        total_costs_sims[sim,i] <- v_tot_cost[i]
        cycle_costs_SOC[sim,] <- v_cost_str
      }
      if(i==2){
        total_QALY_sims[sim,i] <- v_tot_qaly[i]
        total_costs_sims[sim,i] <- v_tot_cost[i] + c_init_strA + (PAID_final$intervention[sim] - PAID_final$control[sim]) # note here we add the initial intervention costs and costs of added life years
        cycle_costs_strA[sim,] <- v_cost_str
      }
    }
  }
  
  ## Visualize PSA results for CEA ----
  ### Create PSA object ----
  #* Function included in "R/Functions.R" The latest version can be found in `dampack` package
  l_psa <- make_psa_obj(cost          = total_costs_sims, 
                        effectiveness = total_QALY_sims, 
                        parameters    = df_psa_input, 
                        strategies    = v_names_str)
  l_psa$strategies <- v_names_str
  colnames(l_psa$effectiveness) <- v_names_str
  colnames(l_psa$cost) <- v_names_str
  
  #* Vector with willingness-to-pay (WTP) thresholds.
  v_wtp <- seq(0, 80000, by = 1000)
  
  ### Cost-Effectiveness Scatter plot ----
  txtsize <- 13

  
  gg_scatter <- inc_scatter(l_psa, wtp_line = 20000, txtsize=txtsize)+
    theme(legend.position = c(0.8, 0.48),
          axis.title.x = element_text(size=14),
          axis.title.y = element_text(size=14),
          axis.text.x = element_text(size=14),
          axis.text.y = element_text(size=14)
    )
  
  ### Incremental cost-effectiveness ratios (ICERs) with probabilistic output ----
  #* Compute expected costs and effects for each strategy from the PSA
  #* Function included in "R/Functions.R". The latest version can be found in `dampack` package
  df_out_ce_psa <- summary(l_psa)
  
  #* Function included in "R/Functions.R"; depends on the `dplyr` package
  #* The latest version can be found in `dampack` package
  df_cea_psa <- calculate_icers(cost       = df_out_ce_psa$meanCost, 
                                effect     = df_out_ce_psa$meanEffect,
                                strategies = df_out_ce_psa$Strategy)
  df_cea_psa
  
  
  ### Cost-effectiveness acceptability curves (CEACs) and frontier (CEAF) ---
  #* Functions included in "R/Functions.R". The latest versions can be found in `dampack` package
  ceac_obj <- ceac(wtp = v_wtp, psa = l_psa)

  #* Regions of highest probability of cost-effectiveness for each strategy
  summary(ceac_obj)
  #* CEAC & CEAF plot
  gg_ceac <- plot.ceac(ceac_obj, frontier = F, points = F, ylim = c(0,1), txtsize = txtsize, xlim = c(0, NA), n_x_ticks = 14) +
    ggthemes::scale_color_colorblind() +
    ggthemes::scale_fill_colorblind() +
    theme(legend.position = c(0.8, 0.48),
          axis.title.x = element_text(size=14),
          axis.title.y = element_text(size=14),
          axis.text.x = element_text(size=14),
          axis.text.y = element_text(size=14)
    )
  gg_ceac
  
  return(list(l_psa = l_psa, 
              df_out_ce_psa = df_out_ce_psa, 
              df_cea_psa = df_cea_psa,  
              ceac_obj = ceac_obj,
              gg_ceac = gg_ceac, 
              gg_scatter = gg_scatter,  
              mean_trace_SoC = mean_trace_SoC,
              mean_trace_strA = mean_trace_strA,
              trace_all = trace_all,
              time_in_states = time_in_states, 
              LEs = LEs,
              LEs_full = LEs_full,
              PAID_final = PAID_final
  ))
}


#### 8. Model for subgroup UY ####

#For subgroup uncontrolled BP, with a CVD history

run_model_uncontrolled_yeshis <- function() {
  
  
  ### Risk prediction model inputs (SOC) ----
  
  #The effect of the intervention on the transitions is due to the difference on systolic blood pressure 'sbp_diff'
  
  
  ### Transition rates (annual) ----
  #SCORE2 and SCORE2-OP
  score_tprobs <- gen_score_tprobs(intervention = "SOC",
                                   subgroup = "uncontrolled_nohis") #this will not be used in this run because people start in MI or Stroke
  
  score_p_MI1 <- as.matrix(score_tprobs$score_MI1)
  score_p_S1 <- as.matrix(score_tprobs$score_S1)
  score_p_CVD <- as.matrix(score_tprobs$score_CVD)
  
  
  box_score_tprobs <- gen_score_tprobs(intervention = "Box",
                                       subgroup = "uncontrolled_nohis") #this will not be used in this run because people start in MI or Stroke
  
  box_score_p_MI1 <- as.matrix(box_score_tprobs$score_MI1)
  box_score_p_S1 <- as.matrix(box_score_tprobs$score_S1)
  box_score_p_CVD <- as.matrix(box_score_tprobs$score_CVD)
  
  
  #SMART2
  smart_tprobs_fromMI <- gen_smart_tprobs(from_state = "MI",
                                          intervention = "SOC",
                                          subgroup = "uncontrolled_yeshis")
  
  smart_p_MI1MI2 <- as.matrix(smart_tprobs_fromMI$smart_MI)
  smart_p_MI1S1 <- as.matrix(smart_tprobs_fromMI$smart_S)
  smart_p_MI2S1 <- as.matrix(smart_tprobs_fromMI$smart_S) # 
  smart_p_MI1CVD <- as.matrix(smart_tprobs_fromMI$smart_CVD)
  smart_p_MI2CVD <- as.matrix(smart_tprobs_fromMI$smart_CVD) # 
  
  
  smart_tprobs_fromS <- gen_smart_tprobs(from_state = "S",
                                         intervention = "SOC",
                                         subgroup = "uncontrolled_yeshis")
  
  smart_p_S1S2 <- as.matrix(smart_tprobs_fromS$smart_S)
  smart_p_S1CVD <- as.matrix(smart_tprobs_fromS$smart_CVD)
  smart_p_S2CVD <- as.matrix(smart_tprobs_fromS$smart_CVD) #
  
  
  
  box_smart_tprobs_fromMI <- gen_smart_tprobs(from_state = "MI",
                                              intervention = "Box",
                                              subgroup = "uncontrolled_yeshis")
  
  box_smart_p_MI1MI2 <- as.matrix(box_smart_tprobs_fromMI$smart_MI)
  box_smart_p_MI1S1 <- as.matrix(box_smart_tprobs_fromMI$smart_S)
  box_smart_p_MI2S1 <- as.matrix(box_smart_tprobs_fromMI$smart_S) # 
  box_smart_p_MI1CVD <- as.matrix(box_smart_tprobs_fromMI$smart_CVD)
  box_smart_p_MI2CVD <- as.matrix(box_smart_tprobs_fromMI$smart_CVD) # 
  
  
  box_smart_tprobs_fromS <- gen_smart_tprobs(from_state = "S",
                                             intervention = "Box",
                                             subgroup = "uncontrolled_yeshis")
  
  box_smart_p_S1S2 <- as.matrix(box_smart_tprobs_fromS$smart_S)
  box_smart_p_S1CVD <- as.matrix(box_smart_tprobs_fromS$smart_CVD)
  box_smart_p_S2CVD <- as.matrix(box_smart_tprobs_fromS$smart_CVD) #
  
  ## Age-dependent mortality rates ----
  lt_NL_2021 <- nl_mort()   
  #* Extract age-specific all-cause mortality for ages in model time horizon
  v_r_mort_by_age <- lt_NL_2021 %>% 
    dplyr::filter(Age >= n_age_init & Age < n_age_max) %>%
    dplyr::select(Female, Male) 
  v_r_mort_by_age$Total <- v_r_mort_by_age$Female*(1-prop_male)+v_r_mort_by_age$Male*prop_male
  v_r_mort_by_age <- as.matrix(v_r_mort_by_age$Total)
  
  #Also extract age-specific scaling values for all-cause mortality to account for CVD deaths
  df_mort_p_scaling <- mort_p_scaling()
  v_p_mort_scale <- df_mort_p_scaling %>% 
    dplyr::filter(Age >= n_age_init & Age < n_age_max) %>%
    dplyr::select(Female, Male) 
  v_p_mort_scale$Total <- v_p_mort_scale$Female*(1-prop_male)+v_p_mort_scale$Male*prop_male
  v_p_mort_scale <- as.numeric(v_p_mort_scale$Total)
  
  ### Discount weight for costs and effects ----
  v_dwc  <- 1 / ((1 + (d_c * cycle_length)) ^ (0:n_cycles))
  v_dwe  <- 1 / ((1 + (d_e * cycle_length)) ^ (0:n_cycles))
  
  # Process model inputs ----
  ## Age-specific transition rates to the Dead state for all cycles ----
  v_r_HDage  <- rep(v_r_mort_by_age, each = 1/cycle_length)
  #* Name age-specific mortality vector 
  names(v_r_HDage) <- v_age_names
  
  #* Function included in "R/Functions.R". The latest version can be found in `darthtools` package
  v_p_HDage  <- rate_to_prob(v_r_HDage, t = cycle_length)  # Age-specific mortality risk in the Healthy state 
  v_p_HDage <- v_p_HDage*(1-v_p_mort_scale) 
  
  # Construct state-transition models ----
  ## Initial state vector ----
  #* All starting healthy
  v_m_init <- c(R = 0, MI1=0.64, MI2=0, S1 = 0.36, S2 = 0, CVD=0 , D = 0) 
  ## Initialize cohort traces ----
  ### Initialize cohort trace under SoC ----
  m_M_SoC <- array(NA, 
                   dim = c((n_cycles + 1), n_states, n_sim), 
                   dimnames = list(0:n_cycles, v_names_states, 1:n_sim))
  #* Store the initial state vector in the first row of the cohort trace
  m_M_SoC[1, , ] <- v_m_init
  
  ### Initialize cohort trace for competing strategies ----
  #* Structure and initial states are the same as for SoC
  m_M_strA  <- m_M_SoC # Strategy A
  
  
  
  ## Create transition probability arrays for strategy SoC ----
  ### Initialize transition probability array for strategy SoC ----
  #* All transitions to a non-death state are assumed to be conditional on survival
  a_P_SoC <- array(0,
                   dim  = c(n_states, n_states, n_cycles, n_sim),
                   dimnames = list(v_names_states, 
                                   v_names_states, 
                                   0:(n_cycles - 1),
                                   1:n_sim))
  
  # ### Fill in array
  ## From R
  a_P_SoC["R", "R", ,]   <- (1 - (v_p_HDage + score_p_CVD[,])) * (1 - (score_p_MI1[,] + score_p_S1[,]))
  a_P_SoC["R", "MI1", , ]  <- (1 - (v_p_HDage + score_p_CVD[,])) * score_p_MI1[,] 
  a_P_SoC["R", "S1", ,]  <- (1 - (v_p_HDage + score_p_CVD[,])) * score_p_S1[,] 
  a_P_SoC["R", "CVD", ,] <- score_p_CVD[,] 
  a_P_SoC["R", "D", ,]   <- v_p_HDage 
  ## From MI1
  a_P_SoC["MI1", "MI1", ,]  <- (1 - (smart_p_MI1CVD[,] + v_p_HDage)) * (1- (smart_p_MI1S1[,] + smart_p_MI1MI2[,]))
  a_P_SoC["MI1", "MI2", ,]  <- (1 - (smart_p_MI1CVD[,] + v_p_HDage)) * smart_p_MI1MI2[,]
  a_P_SoC["MI1", "S1", ,]  <-  (1 - (smart_p_MI1CVD[,] + v_p_HDage)) * smart_p_MI1S1[,] 
  a_P_SoC["MI1", "CVD", ,]  <- smart_p_MI1CVD[,] 
  a_P_SoC["MI1", "D", ,]  <-  v_p_HDage 
  ## From MI2
  a_P_SoC["MI2", "MI2", ,]  <- (1 - (smart_p_MI2CVD[,] + v_p_HDage)) * (1-smart_p_MI2S1[,]) 
  a_P_SoC["MI2", "S1", ,]  <- (1 - (smart_p_MI2CVD[,] + v_p_HDage)) * smart_p_MI2S1[,] 
  a_P_SoC["MI2", "CVD", ,]  <- smart_p_MI2CVD[,] 
  a_P_SoC["MI2", "D", ,]  <-  v_p_HDage 
  
  ## From S1
  a_P_SoC["S1", "S1", ,] <- (1 - (v_p_HDage + smart_p_S1CVD[,])) * (1-smart_p_S1S2[,]) 
  a_P_SoC["S1", "S2", ,] <- (1 - (v_p_HDage + smart_p_S1CVD[,])) * smart_p_S1S2[,]
  a_P_SoC["S1", "CVD", ,] <- smart_p_S1CVD[,]
  a_P_SoC["S1", "D", ,] <- v_p_HDage 
  
  ## From S2
  a_P_SoC["S2", "S2", ,] <- (1 - (v_p_HDage + smart_p_S2CVD[,])) 
  a_P_SoC["S2", "CVD", ,] <- smart_p_S2CVD[,]
  a_P_SoC["S2", "D", ,] <- v_p_HDage 
  
  ## From CVD
  a_P_SoC["CVD", "CVD", ,]   <- 1
  
  ## From D
  a_P_SoC["D", "D", ,]   <- 1
  
  ### Transition probability array for strategy: CVRM-Box ----
  #a_P_strA <- a_P_SoC
  a_P_strA <- array(0,
                    dim  = c(n_states, n_states, n_cycles, n_sim),
                    dimnames = list(v_names_states, 
                                    v_names_states, 
                                    0:(n_cycles - 1),
                                    1:n_sim))
  ## From R
  a_P_strA["R", "R", ,]   <- (1 - (v_p_HDage + box_score_p_CVD[,])) * (1 - (box_score_p_MI1[,] + box_score_p_S1[,]))
  a_P_strA["R", "MI1", ,]  <- (1 - (v_p_HDage + box_score_p_CVD[,])) * box_score_p_MI1[,] 
  a_P_strA["R", "S1", ,]  <- (1 - (v_p_HDage + box_score_p_CVD[,])) * box_score_p_S1[,] 
  a_P_strA["R", "CVD", ,] <- box_score_p_CVD[,] 
  a_P_strA["R", "D", ,]   <- v_p_HDage 
  ## From MI1
  a_P_strA["MI1", "MI1", ,]  <- (1 - (box_smart_p_MI1CVD[,] + v_p_HDage)) * (1- (box_smart_p_MI1S1[,] + box_smart_p_MI1MI2[,]))
  a_P_strA["MI1", "MI2", ,]  <- (1 - (box_smart_p_MI1CVD[,] + v_p_HDage)) * box_smart_p_MI1MI2[,]
  a_P_strA["MI1", "S1", ,]  <-  (1 - (box_smart_p_MI1CVD[,] + v_p_HDage)) * box_smart_p_MI1S1[,] 
  a_P_strA["MI1", "CVD", ,]  <- box_smart_p_MI1CVD[,] 
  a_P_strA["MI1", "D", ,]  <-  v_p_HDage 
  ## From MI2
  a_P_strA["MI2", "MI2", ,]  <- (1 - (box_smart_p_MI2CVD[,] + v_p_HDage)) * (1-box_smart_p_MI2S1[,]) 
  a_P_strA["MI2", "S1", ,]  <- (1 - (box_smart_p_MI2CVD[,] + v_p_HDage)) * box_smart_p_MI2S1[,] 
  a_P_strA["MI2", "CVD", ,]  <- box_smart_p_MI2CVD[,] 
  a_P_strA["MI2", "D", ,]  <-  v_p_HDage 
  
  ## From S1
  a_P_strA["S1", "S1", ,] <- (1 - (v_p_HDage + box_smart_p_S1CVD[,])) * (1-box_smart_p_S1S2[,]) 
  a_P_strA["S1", "S2", ,] <- (1 - (v_p_HDage + box_smart_p_S1CVD[,])) * box_smart_p_S1S2[,]
  a_P_strA["S1", "CVD", ,] <- box_smart_p_S1CVD[,]
  a_P_strA["S1", "D", ,] <- v_p_HDage 
  
  ## From S2
  a_P_strA["S2", "S2", ,] <- (1 - (v_p_HDage + box_smart_p_S2CVD[,])) 
  a_P_strA["S2", "CVD", ,] <- box_smart_p_S2CVD[,]
  a_P_strA["S2", "D", ,] <- v_p_HDage 
  
  ## From CVD
  a_P_strA["CVD", "CVD", ,]   <- 1
  
  ## From D
  a_P_strA["D", "D", ,]   <- 1
  
  ## Check if transition probability arrays are valid ----
  #* Functions included in "R/Functions.R". The latest version can be found in `darthtools` package
  ### Check that transition probabilities are [0, 1] ----
  for(sim in 1:n_sim) {
    #print(sim)
    check_transition_probability(a_P_SoC[,,,sim], verbose = TRUE)
    check_transition_probability(a_P_strA[,,,sim], verbose = TRUE)
    #Check that all rows for each slice of the array sum to 1
    sum_soc_tmp <- sum(rowSums(a_P_SoC[,,,sim]))
    if (sum_soc_tmp != n_states*n_cycles) {print("Invalid")}
    sum_a_tmp <- sum(rowSums(a_P_strA[,,,sim]))
    if (sum_a_tmp != n_states*n_cycles) {print("Invalid")}
  }
  
  ## Create transition dynamics arrays ----
  #* These arrays will capture transitions from each state to another over time 
  ### Initialize transition dynamics array for strategy SoC ----
  a_A_SoC <- array(0,
                   dim      = c(n_states, n_states, n_cycles + 1, n_sim),
                   dimnames = list(v_names_states, v_names_states, 0:n_cycles, 1:n_sim))
  a_A_strA  <- a_A_SoC #Structure and initial states are the same as for SoC
  
  
  #  Run Markov model ----
  #* Iterative solution of age-dependent cSTM
  for(i in 1:n_sim) {
    #* Set first slice of a_A_SoC and a_A_strA with the initial state vector in its diagonal
    diag(a_A_SoC[, , 1,i]) <- v_m_init
    diag(a_A_strA[, , 1,i]) <- v_m_init
    for(t in 1:n_cycles){
      ## Fill in cohort trace
      # For SoC
      m_M_SoC[t + 1, ,i]  <- m_M_SoC[t, ,i]  %*% a_P_SoC[, , t,i]
      # for strategy: CVRM-Box
      m_M_strA[t + 1, ,i] <- m_M_strA[t,,i ] %*% a_P_strA[, , t,i]
      
      ## Fill in transition-dynamics array
      # For SoC
      a_A_SoC[, , t + 1,i]  <- diag(m_M_SoC[t, ,i]) %*% a_P_SoC[, , t,i]
      # for strategy: CVRM-Box
      a_A_strA[, , t + 1,i] <- diag(m_M_strA[t, ,i]) %*% a_P_strA[, , t,i]
    }
  }
  ## Store the cohort traces in a list ----
  l_m_M <- list(SoC =  m_M_SoC,
                A   =  m_M_strA) 
  
  names(l_m_M) <- v_names_str
  
  ## Store the transition dynamics array for each strategy in a list ----
  l_a_A <- list(SoC =  a_A_SoC,
                A   =  a_A_strA) 
  names(l_a_A) <- v_names_str
  
  # Plot Outputs ----
  #* (Functions included in "R/Functions.R"; depends on the `ggplot2` package)
  
  # Plot the cohort trace for strategy SoC ----
  mean_trace_SoC <- apply(m_M_SoC, c(1:2), mean)
  trace_soc <- plot_trace(mean_trace_SoC)
  
  mean_trace_strA <- apply(m_M_strA, c(1:2), mean)
  trace_box <- plot_trace(mean_trace_strA)
  ## Plot the cohort trace for all strategies ----
  
  trace_all <- plot_trace_strategy(l_m_M)
  
  #time spent in states
  time_in_states_tmp <- array(NA, dim=c(n_str, n_states, n_sim), dimnames = list(v_names_str, v_names_states, 1:n_sim))
  #for SOC, there is no variation in the risks
  time_in_states_tmp["Standard of care",,] <- colMeans(apply(m_M_SoC,c(1,2), mean))*100
  time_in_states_tmp["Strategy A",,] <- colMeans(m_M_strA[,,])*100
  
  time_in_states <- matrix(nrow = length(v_names_str), ncol = n_states+1)
  rownames(time_in_states) <- v_names_str
  colnames(time_in_states) <- c("Option", "At risk", "Post-MI", "Post-recurrent MI", "Post-Stroke", "Post-recurrent Stroke", "CVD Death", "All-cause Death")
  time_in_states["Standard of care",] <- c("No treatment", 
                                           round(mean(time_in_states_tmp["Standard of care","R",]),1),
                                           round(mean(time_in_states_tmp["Standard of care","MI1",]),1),
                                           round(mean(time_in_states_tmp["Standard of care","MI2",]),1),
                                           round(mean(time_in_states_tmp["Standard of care","S1",]),1),
                                           round(mean(time_in_states_tmp["Standard of care","S2",]),1),
                                           round(mean(time_in_states_tmp["Standard of care","CVD",]),1),
                                           round(mean(time_in_states_tmp["Standard of care","D",]),1))
  
  time_in_states["Strategy A",] <- c("Treatment",
                                     round(mean(time_in_states_tmp["Strategy A","R",]),1),
                                     round(mean(time_in_states_tmp["Strategy A","MI1",]),1),
                                     round(mean(time_in_states_tmp["Strategy A","MI2",]),1),
                                     round(mean(time_in_states_tmp["Strategy A","S1",]),1),
                                     round(mean(time_in_states_tmp["Strategy A","S2",]),1),
                                     round(mean(time_in_states_tmp["Strategy A","CVD",]),1),
                                     round(mean(time_in_states_tmp["Strategy A","D",]),1))
  
  #Life expectancy output and costs of added life years
  LEs_full <- matrix(NA,nrow=n_sim,ncol=n_str)
  PAID_full_SOC <- matrix(NA,nrow=n_sim,ncol=n_cycles+1)
  PAID_full_strA <- matrix(NA,nrow=n_sim,ncol=n_cycles+1)
  
  
  colnames(LEs_full) <- v_names_str
  colnames(PAID_full_SOC) <- c(0:n_cycles)
  colnames(PAID_full_strA) <- c(0:n_cycles)
  
  
  for(i in 1:n_sim){
    LEs_full[i,"Standard of care"] <- sum(rowSums(m_M_SoC[, -which(v_names_states == "D" | v_names_states == "CVD"),i])*v_wcc)
    LEs_full[i,"Strategy A"] <- sum(rowSums(m_M_strA[, -which(v_names_states == "D" | v_names_states == "CVD"),i])*v_wcc)
    PAID_full_SOC[i,] <- as.numeric(rowSums(m_M_SoC[, -which(v_names_states == "D" | v_names_states == "CVD"),i]))
    PAID_full_strA[i,] <- as.numeric(rowSums(m_M_strA[, -which(v_names_states == "D" | v_names_states == "CVD"),i]))
    
  }
  PAID_final <- get_paid(intervention = PAID_full_strA, control = PAID_full_SOC) 
  
  LEs <- data.frame(Option = c("No treatment", "Treatment"),
                    LE = c(round(mean(LEs_full[,"Standard of care"]),2),
                           paste0(
                             round(mean(LEs_full[,"Strategy A"]),2),
                             " (",
                             round(quantile(LEs_full[,"Strategy A"], probs = 0.025),2),
                             ", ",
                             round(quantile(LEs_full[,"Strategy A"], probs = 0.975),2),
                             ")"
                           )))
  
  
  ## Plot the epidemiology outcomes ----
  ### Survival ----
  
  df_surv <- calc_surv(l_m_M, v_names_death_states= c("CVD", "D"))
  #print(df_surv)
  survival_plot <- ggplot(df_surv,
                          aes(x = Cycle, y = Survival, group = as.factor(Strategy))) +
    geom_line(aes(linetype = Strategy), size = 0.8) +
    
    xlab("Cycle") +
    ylab("Proportion") +
    ylim(0,1) +
    ggtitle("Survival probabilities") +
    theme_bw(base_size = 14) +
    theme()

  # State Rewards ----
  v_u_SoC    <- cbind(u_R_sgCN - u_hypertension,  
                      u_R_sgCN - u_hypertension - u_MI1,  
                      u_R_sgCN - u_hypertension - u_S1, 
                      u_R_sgCN - u_hypertension - u_MI1 - u_MI2, 
                      u_R_sgCN - u_hypertension - u_S1 - u_S2, 
                      u_D,
                      u_CVD)
  colnames(v_u_SoC) <- v_names_states 
  
  #* Vector of state costs under strategy SoC
  v_c_SoC    <- cbind(c_SOC_uncontrolled ,  
                      c_SOC_uncontrolled  + c_MI1, 
                      c_SOC_uncontrolled  + c_S1,  
                      c_SOC_uncontrolled  + c_MI1 + c_MI2,  
                      c_SOC_uncontrolled  + c_S1 + c_S2,  
                      c_D,
                      c_CVD)
  colnames(v_c_SoC) <- v_names_states 
  #* Vector of state utilities under strategy A
  v_u_strA   <- v_u_SoC
  
  #* Vector of state costs under strategy A
  v_c_strA   <- cbind(c_strA_uncontrolled, 
                      c_MI1+ c_strA_uncontrolled,
                      c_S1+ c_strA_uncontrolled, 
                      c_MI2+ c_MI1+ c_strA_uncontrolled, 
                      c_S2+ c_S1 + c_strA_uncontrolled, 
                      c_D,
                      c_CVD) 
  colnames(v_c_strA) <- v_names_states 
  
  
  ## Store state rewards ----
  #* Store the vectors of state utilities for each strategy in a list 
  l_u <- list(SoC = v_u_SoC,
              A   = v_u_strA)
  #* Store the vectors of state cost for each strategy in a list 
  l_c <- list(SoC =  v_c_SoC,
              A   =  v_c_strA)
  
  #* assign strategy names to matching items in the lists
  names(l_u) <- names(l_c) <- v_names_str
  
  # Compute expected outcomes ----
  #* Create empty vectors to store total utilities and costs 
  v_tot_qaly <- v_tot_cost <- vector(mode = "numeric", length = n_str)
  names(v_tot_qaly) <- names(v_tot_cost) <- v_names_str
  
  
  
  total_costs_sims <- as.data.frame(matrix(NA, nrow=n_sim, ncol=n_str))
  colnames(total_costs_sims) <- v_names_str
  total_QALY_sims <- as.data.frame(matrix(NA, nrow=n_sim, ncol=n_str))
  colnames(total_costs_sims) <- v_names_str
  
  cycle_costs_strA <- as.data.frame(matrix(data=NA, nrow = n_sim, ncol=n_cycles+1))
  cycle_costs_SOC<- as.data.frame(matrix(data=NA, nrow = n_sim, ncol=n_cycles+1))
  
  ## Loop through each strategy and calculate total utilities and costs ----
  for (sim in 1:n_sim){
    for (i in 1:n_str) { # i <- 1
      v_u_str <- l_u[[i]][sim,]   # select the vector of state utilities for the i-th strategy
      v_c_str <- l_c[[i]][sim,]   # select the vector of state costs for the i-th strategy
      a_A_str <- l_a_A[[i]][,,,sim] # select the transition array for the i-th strategy, simulation
      ##* Array of state rewards 
      #* Create transition matrices of state utilities and state costs for the i-th strategy 
      m_u_str   <- matrix(v_u_str, nrow = n_states, ncol = n_states, byrow = T)
      m_c_str   <- matrix(v_c_str, nrow = n_states, ncol = n_states, byrow = T)
      #* Expand the transition matrix of state utilities across cycles to form a transition array of state utilities
      a_R_u_str <- array(m_u_str, 
                         dim      = c(n_states, n_states, n_cycles + 1),
                         dimnames = list(v_names_states, v_names_states, 0:n_cycles))
      # Expand the transition matrix of state costs across cycles to form a transition array of state costs
      a_R_c_str <- array(m_c_str, 
                         dim      = c(n_states, n_states, n_cycles + 1),
                         dimnames = list(v_names_states, v_names_states, 0:n_cycles))

      ##* Apply transition rewards
      #* Apply disutility due to transition from H to S1
      a_R_u_str["R", "MI1", ]  <- 0 #
      a_R_u_str["MI1", "MI2", ] <-  a_R_u_str["MI1", "MI2", ]  - du_MI2[sim] #
      a_R_u_str["MI2", "S1", ]  <- a_R_u_str["MI2", "S1", ]  - du_MItoS[sim] #
      a_R_u_str["R", "S1", ]  <- 0 #
      a_R_u_str["MI1", "S1", ]  <- a_R_u_str["MI1", "S1", ]  - du_MItoS[sim] #
      a_R_u_str["S1", "S2", ] <-  a_R_u_str["S1", "S2", ]  -  du_S2[sim] #
      
      
      #* Add transition cost per cycle due to transition from H to S1
      a_R_c_str["R", "MI1", ]      <- 0 #
      a_R_c_str["MI1", "MI2", ]      <- a_R_c_str["MI1", "MI2", ]    +    ic_MI2[sim] # #
      if(n_age_init<=66){a_R_c_str["MI1", "MI2", 1:(67-n_age_init+1)]      <- a_R_c_str["MI1", "MI2", 1:(67-n_age_init+1)]    +    ic_fric[sim]} #
      a_R_c_str["MI1", "S1", ]      <- a_R_c_str["MI1", "S1", ]       + ic_S1[sim]  #
      a_R_c_str["R", "S1", ]      <- 0 #
      a_R_c_str["S1", "S2", ]      <- a_R_c_str["S1", "S2", ]      +  ic_S2[sim] # friction costs should only be applied once #
      if(n_age_init<=66){a_R_c_str["S1", "S2", 1:(67-n_age_init+1)]      <- a_R_c_str["S1", "S2", 1:(67-n_age_init+1)]      +  ic_fric[sim]} # friction costs should only be applied once #
      a_R_c_str["MI2", "CVD", ]      <- a_R_c_str["MI2", "CVD", ]       + ic_CVD[sim]
      a_R_c_str["MI2", "S1", ]      <- a_R_c_str["MI2", "S1", ]       + ic_S2[sim]
      a_R_c_str["S2", "CVD", ]      <- a_R_c_str["S2", "CVD", ]       + ic_CVD[sim]
      a_R_c_str["R", "CVD", ]      <- 0
      
      
      
      
      
      ###* Expected QALYs and costs for all transitions per cycle
      #* QALYs = life years x QoL
      
      a_Y_c_str <- a_A_str * a_R_c_str
      a_Y_u_str <- a_A_str * a_R_u_str 
      
      ###* Expected QALYs and costs per cycle
      ##* Vector of QALYs and costs
      v_qaly_str <- apply(a_Y_u_str, 3, sum) # sum the proportion of the cohort across transitions 
      v_cost_str <- apply(a_Y_c_str, 3, sum) # sum the proportion of the cohort across transitions
      
      
      #* QALYs
      v_tot_qaly[i] <- t(v_qaly_str) %*% (v_dwe * v_wcc)
      #* Costs
      v_tot_cost[i] <- t(v_cost_str) %*% (v_dwc * v_wcc)
      
      ## Vector with discounted values
      if(i==1){
        total_QALY_sims[sim,i] <- v_tot_qaly[i]
        total_costs_sims[sim,i] <- v_tot_cost[i]
        cycle_costs_SOC[sim,] <- v_cost_str
      }
      if(i==2){
        total_QALY_sims[sim,i] <- v_tot_qaly[i]
        total_costs_sims[sim,i] <- v_tot_cost[i] + c_init_strA + (PAID_final$intervention[sim] - PAID_final$control[sim]) # note here we add the initial intervention costs and costs of added life years
        cycle_costs_strA[sim,] <- v_cost_str
      }
    }
  }
  
  
  ## Visualize PSA results for CEA ----
  ### Create PSA object ----
  #* Function included in "R/Functions.R" The latest version can be found in `dampack` package
  l_psa <- make_psa_obj(cost          = total_costs_sims, 
                        effectiveness = total_QALY_sims, 
                        parameters    = df_psa_input, 
                        strategies    = v_names_str)
  l_psa$strategies <- v_names_str
  colnames(l_psa$effectiveness) <- v_names_str
  colnames(l_psa$cost) <- v_names_str
  
  #* Vector with willingness-to-pay (WTP) thresholds.
  v_wtp <- seq(0, 80000, by = 1000)
  
  ### Cost-Effectiveness Scatter plot ----
  txtsize <- 13
  
  gg_scatter <- inc_scatter(l_psa, wtp_line = 20000, txtsize=txtsize)+
    theme(legend.position = c(0.8, 0.48),
          axis.title.x = element_text(size=14),
          axis.title.y = element_text(size=14),
          axis.text.x = element_text(size=14),
          axis.text.y = element_text(size=14)
    )
    
  
  ### Incremental cost-effectiveness ratios (ICERs) with probabilistic output ----
  #* Compute expected costs and effects for each strategy from the PSA
  #* Function included in "R/Functions.R". The latest version can be found in `dampack` package
  df_out_ce_psa <- summary(l_psa)
  
  #* Function included in "R/Functions.R"; depends on the `dplyr` package
  #* The latest version can be found in `dampack` package
  df_cea_psa <- calculate_icers(cost       = df_out_ce_psa$meanCost, 
                                effect     = df_out_ce_psa$meanEffect,
                                strategies = df_out_ce_psa$Strategy)
  df_cea_psa
  
  
  ### Cost-effectiveness acceptability curves (CEACs) and frontier (CEAF) ---
  #* Functions included in "R/Functions.R". The latest versions can be found in `dampack` package
  ceac_obj <- ceac(wtp = v_wtp, psa = l_psa)

  #* Regions of highest probability of cost-effectiveness for each strategy
  summary(ceac_obj)
  #* CEAC & CEAF plot
  gg_ceac <- plot.ceac(ceac_obj, frontier = F, points = F, ylim = c(0,1), txtsize = txtsize, xlim = c(0, NA), n_x_ticks = 14) +
    ggthemes::scale_color_colorblind() +
    ggthemes::scale_fill_colorblind() +
    theme(legend.position = c(0.8, 0.48),
          axis.title.x = element_text(size=14),
          axis.title.y = element_text(size=14),
          axis.text.x = element_text(size=14),
          axis.text.y = element_text(size=14)
    )
  gg_ceac
  
  return(list(l_psa = l_psa, 
              df_out_ce_psa = df_out_ce_psa, 
              df_cea_psa = df_cea_psa,  
              ceac_obj = ceac_obj,
              gg_ceac = gg_ceac, 
              gg_scatter = gg_scatter,  
              mean_trace_SoC = mean_trace_SoC,
              mean_trace_strA = mean_trace_strA,
              trace_all = trace_all,
              time_in_states = time_in_states, 
              LEs = LEs,
              LEs_full = LEs_full,
              PAID_final = PAID_final
  ))
}


#### 9. Model for pooled analyses ####

run_model_pooled <- function(){
  
  output_CN <- run_model_controlled_nohis()
  print("ran SG 1")
  output_UN <- run_model_uncontrolled_nohis()
  print("ran SG 2")
  output_CY <- run_model_controlled_yeshis()
  print("ran SG 3")
  output_UY <- run_model_uncontrolled_yeshis()
  print("ran SG 4")

  #Plot the (pooled) cohort trace for strategy SoC ----
  mean_trace_SoC_pooled <- (output_CN$mean_trace_SoC*sg_prop_CN +
                              output_UN$mean_trace_SoC*sg_prop_UN +
                              output_CY$mean_trace_SoC*sg_prop_CY +
                              output_UY$mean_trace_SoC*sg_prop_UY)

  
  mean_trace_SoC_pooled_df <- as.data.frame(mean_trace_SoC_pooled)
  mean_trace_SoC_pooled_df$Strategy <- "Standard of care"
  mean_trace_SoC_pooled_df$Cycle <- c(0:n_cycles)
  
  mean_trace_strA_pooled <- (output_CN$mean_trace_strA*sg_prop_CN +
                               output_UN$mean_trace_strA*sg_prop_UN +
                               output_CY$mean_trace_strA*sg_prop_CY +
                               output_UY$mean_trace_strA*sg_prop_UY)
  mean_trace_strA_pooled_df <- as.data.frame(mean_trace_strA_pooled)
  mean_trace_strA_pooled_df$Strategy <- "Strategy A"
  mean_trace_strA_pooled_df$Cycle <- c(0:n_cycles)
  
  
  trace_all_pooled_df <- rbind(mean_trace_SoC_pooled_df, mean_trace_strA_pooled_df)

  # Convert to long format
  m_M_plot <- gather(trace_all_pooled_df, key = `Health State`, value = value, R:D)
  m_M_plot$`Health State` <- factor(m_M_plot$`Health State`, levels = c("R", "MI1", "MI2", "S1", "S2", "CVD", "D"))
  m_M_plot$Strategy <- factor(m_M_plot$Strategy, levels = c("Standard of care", "Strategy A")) 
  
  # Create the plot
  trace_all_pooled <- ggplot(m_M_plot, aes(x = Cycle, y = value, color = `Health State`, linetype = Strategy)) +
    geom_line(size = 0.5) +
    xlab("Cycle") +
    ylab("Proportion of the cohort") +
    scale_linetype_manual(values = c("solid", "dotted")) +
    theme_bw(base_size = 14) +
    theme(legend.position = "bottom", 
          legend.background = element_rect(fill = NA),
          legend.text = element_text(size=14))
  
  
  
  time_in_states_pooled <- matrix(nrow = length(v_names_str), ncol = n_states+1)
  rownames(time_in_states_pooled) <- v_names_str
  colnames(time_in_states_pooled) <- c("Option", "At risk", "Post-MI", "Post-recurrent MI", "Post-Stroke", "Post-recurrent Stroke", "CVD Death", "All-cause Death")
  time_in_states_pooled["Standard of care",] <- c("No treatment",
                                                  round(colMeans(mean_trace_SoC_pooled)*100,1))
  time_in_states_pooled["Strategy A",] <- c("Treatment",
                                            round(colMeans(mean_trace_strA_pooled)*100,1))
  
  LEs_full_pooled <- (output_CN$LEs_full*sg_prop_CN +
                        output_UN$LEs_full*sg_prop_UN +
                        output_CY$LEs_full*sg_prop_CY +
                        output_UY$LEs_full*sg_prop_UY)

  LEs_pooled <- data.frame(Option = c("No treatment", "Treatment"),
                           LE = c(round(mean(LEs_full_pooled[,"Standard of care"]),2),
                                  paste0(
                                    round(mean(LEs_full_pooled[,"Strategy A"]),2),
                                    " (",
                                    round(quantile(LEs_full_pooled[,"Strategy A"], probs = 0.025),2),
                                    ", ",
                                    round(quantile(LEs_full_pooled[,"Strategy A"], probs = 0.975),2),
                                    ")"
                                  )))

  total_costs_sims_pooled <- (output_CN$l_psa$cost*sg_prop_CN +
                                output_UN$l_psa$cost*sg_prop_UN +
                                output_CY$l_psa$cost*sg_prop_CY +
                                output_UY$l_psa$cost*sg_prop_UY)
  total_QALY_sims_pooled <- (output_CN$l_psa$effectiveness*sg_prop_CN +
                               output_UN$l_psa$effectiveness*sg_prop_UN +
                               output_CY$l_psa$effectiveness*sg_prop_CY +
                               output_UY$l_psa$effectiveness*sg_prop_UY)
  df_psa_input_pooled <- (output_CN$l_psa$parameters*sg_prop_CN +
                            output_UN$l_psa$parameters*sg_prop_UN +
                            output_CY$l_psa$parameters*sg_prop_CY +
                            output_UY$l_psa$parameters*sg_prop_UY)
  
  PAID_final_pooled_intervention <- (output_CN$PAID_final$intervention*sg_prop_CN +
                                      output_UN$PAID_final$intervention*sg_prop_UN +
                                      output_CY$PAID_final$intervention*sg_prop_CY +
                                      output_UY$PAID_final$intervention*sg_prop_UY)
  
  PAID_final_pooled_control <- (output_CN$PAID_final$control*sg_prop_CN +
                                       output_UN$PAID_final$control*sg_prop_UN +
                                       output_CY$PAID_final$control*sg_prop_CY +
                                       output_UY$PAID_final$control*sg_prop_UY)
  
  PAID_final_pooled <- list(intervention = PAID_final_pooled_intervention, control = PAID_final_pooled_control)
  
  ### Create (pooled) PSA object ----
  #* Function included in "R/Functions.R" The latest version can be found in `dampack` package
  l_psa <- make_psa_obj(cost          = total_costs_sims_pooled, 
                        effectiveness = total_QALY_sims_pooled, 
                        parameters    = df_psa_input_pooled, 
                        strategies    = v_names_str)
  l_psa$strategies <- v_names_str
  colnames(l_psa$effectiveness) <- v_names_str
  colnames(l_psa$cost) <- v_names_str
  
  
  
  #* Vector with willingness-to-pay (WTP) thresholds.
  v_wtp <- seq(0, 80000, by = 1000)
  
  ### Cost-Effectiveness Scatter plot ----
  txtsize <- 13
  
  gg_scatter <- inc_scatter(l_psa, wtp_line = 20000, txtsize=txtsize)+
    theme(legend.position = c(0.8, 0.48),
          axis.title.x = element_text(size=14),
          axis.title.y = element_text(size=14),
          axis.text.x = element_text(size=14),
          axis.text.y = element_text(size=14)
    )
  
  ### Incremental cost-effectiveness ratios (ICERs) with probabilistic output ----
  #* Compute expected costs and effects for each strategy from the PSA
  #* Function included in "R/Functions.R". The latest version can be found in `dampack` package
  df_out_ce_psa <- summary(l_psa)
  
  #* Function included in "R/Functions.R"; depends on the `dplyr` package
  #* The latest version can be found in `dampack` package
  df_cea_psa <- calculate_icers(cost       = df_out_ce_psa$meanCost, 
                                effect     = df_out_ce_psa$meanEffect,
                                strategies = df_out_ce_psa$Strategy)
  df_cea_psa
  
  ### Cost-effectiveness acceptability curves (CEACs) and frontier (CEAF) ---
  #* Functions included in "R/Functions.R". The latest versions can be found in `dampack` package
  ceac_obj <- ceac(wtp = v_wtp, psa = l_psa)
  
  
  #* Regions of highest probability of cost-effectiveness for each strategy
  summary(ceac_obj)
  #* CEAC & CEAF plot
  gg_ceac <- plot.ceac(ceac_obj, frontier = F, points = F, ylim = c(0,1), txtsize = txtsize, xlim = c(0, NA), n_x_ticks = 14) +
    ggthemes::scale_color_colorblind() +
    ggthemes::scale_fill_colorblind() +
    theme(legend.position = c(0.8, 0.48),
          axis.title.x = element_text(size=14),
          axis.title.y = element_text(size=14),
          axis.text.x = element_text(size=14),
          axis.text.y = element_text(size=14)
    )
  gg_ceac
  
  
  
  return(list(l_psa = l_psa, 
              df_out_ce_psa = df_out_ce_psa, 
              df_cea_psa = df_cea_psa,  
              ceac_obj = ceac_obj,
              gg_ceac = gg_ceac, 
              gg_scatter = gg_scatter,  
              mean_trace_SoC = mean_trace_SoC_pooled,
              mean_trace_strA = mean_trace_strA_pooled,
              trace_all = trace_all_pooled,
              time_in_states = time_in_states_pooled,
              LEs = LEs_pooled,
              LEs_full = LEs_full_pooled,
              PAID_final = PAID_final_pooled))
  
}

#### 10. BI Model for subgroup CN ####

#For subgroup controlled BP, no CVD history

run_BI_model_controlled_nohis <- function() {
  
  
  ### Risk prediction model inputs (SOC) ----
  
  #The effect of the intervention on the transitions is due to the difference on systolic blood pressure 'sbp_diff'
  
  
  ### Transition rates (annual) ----
  #SCORE2 and SCORE2-OP
  score_tprobs <- gen_score_tprobs(intervention = "SOC",
                                   subgroup = "controlled_nohis")
  
  score_p_MI1 <- as.matrix(score_tprobs$score_MI1)
  score_p_S1 <- as.matrix(score_tprobs$score_S1)
  score_p_CVD <- as.matrix(score_tprobs$score_CVD)
  
  
  box_score_tprobs <- gen_score_tprobs(intervention = "Box",
                                       subgroup = "controlled_nohis")
  
  box_score_p_MI1 <- as.matrix(box_score_tprobs$score_MI1)
  box_score_p_S1 <- as.matrix(box_score_tprobs$score_S1)
  box_score_p_CVD <- as.matrix(box_score_tprobs$score_CVD)
  
  
  #SMART2
  smart_tprobs_fromMI <- gen_smart_tprobs(from_state = "MI",
                                          intervention = "SOC",
                                          subgroup = "controlled_nohis")
  
  smart_p_MI1MI2 <- as.matrix(smart_tprobs_fromMI$smart_MI)
  smart_p_MI1S1 <- as.matrix(smart_tprobs_fromMI$smart_S)
  smart_p_MI2S1 <- as.matrix(smart_tprobs_fromMI$smart_S) # 
  smart_p_MI1CVD <- as.matrix(smart_tprobs_fromMI$smart_CVD)
  smart_p_MI2CVD <- as.matrix(smart_tprobs_fromMI$smart_CVD) # 
  
  
  smart_tprobs_fromS <- gen_smart_tprobs(from_state = "S",
                                         intervention = "SOC",
                                         subgroup = "controlled_nohis")
  
  smart_p_S1S2 <- as.matrix(smart_tprobs_fromS$smart_S)
  smart_p_S1CVD <- as.matrix(smart_tprobs_fromS$smart_CVD)
  smart_p_S2CVD <- as.matrix(smart_tprobs_fromS$smart_CVD) #
  
  
  
  box_smart_tprobs_fromMI <- gen_smart_tprobs(from_state = "MI",
                                              intervention = "Box",
                                              subgroup = "controlled_nohis")
  
  box_smart_p_MI1MI2 <- as.matrix(box_smart_tprobs_fromMI$smart_MI)
  box_smart_p_MI1S1 <- as.matrix(box_smart_tprobs_fromMI$smart_S)
  box_smart_p_MI2S1 <- as.matrix(box_smart_tprobs_fromMI$smart_S) # 
  box_smart_p_MI1CVD <- as.matrix(box_smart_tprobs_fromMI$smart_CVD)
  box_smart_p_MI2CVD <- as.matrix(box_smart_tprobs_fromMI$smart_CVD) # 
  
  
  box_smart_tprobs_fromS <- gen_smart_tprobs(from_state = "S",
                                             intervention = "Box",
                                             subgroup = "controlled_nohis")
  
  box_smart_p_S1S2 <- as.matrix(box_smart_tprobs_fromS$smart_S)
  box_smart_p_S1CVD <- as.matrix(box_smart_tprobs_fromS$smart_CVD)
  box_smart_p_S2CVD <- as.matrix(box_smart_tprobs_fromS$smart_CVD) #
  
  ## Age-dependent mortality rates ----
  lt_NL_2021 <- nl_mort()   
  #* Extract age-specific all-cause mortality for ages in model time horizon
  v_r_mort_by_age <- lt_NL_2021 %>% 
    dplyr::filter(Age >= n_age_init & Age < n_age_max) %>%
    dplyr::select(Female, Male) 
  v_r_mort_by_age$Total <- v_r_mort_by_age$Female*(1-prop_male)+v_r_mort_by_age$Male*prop_male
  v_r_mort_by_age <- as.matrix(v_r_mort_by_age$Total)
  
  #Also extract age-specific scaling values for all-cause mortality to account for CVD deaths
  df_mort_p_scaling <- mort_p_scaling()
  v_p_mort_scale <- df_mort_p_scaling %>% 
    dplyr::filter(Age >= n_age_init & Age < n_age_max) %>%
    dplyr::select(Female, Male) 
  v_p_mort_scale$Total <- v_p_mort_scale$Female*(1-prop_male)+v_p_mort_scale$Male*prop_male
  v_p_mort_scale <- as.numeric(v_p_mort_scale$Total)
  
  ### Discount weight for costs and effects ----
  v_dwc  <- 1 / ((1 + (d_c * cycle_length)) ^ (0:n_cycles))
  v_dwe  <- 1 / ((1 + (d_e * cycle_length)) ^ (0:n_cycles))
  
  # Process model inputs ----
  ## Age-specific transition rates to the Dead state for all cycles ----
  v_r_HDage  <- rep(v_r_mort_by_age, each = 1/cycle_length)
  #* Name age-specific mortality vector 
  names(v_r_HDage) <- v_age_names
  
  #* Function included in "R/Functions.R". The latest version can be found in `darthtools` package
  v_p_HDage  <- rate_to_prob(v_r_HDage, t = cycle_length)  # Age-specific mortality risk in the Healthy state 
  v_p_HDage <- v_p_HDage*(1-v_p_mort_scale) 
  
  # Construct state-transition models ----
  ## Initial state vector ----
  #* All starting healthy
  v_m_init <- c(R = 1, MI1=0, MI2=0, S1 = 0, S2 = 0, CVD=0 , D = 0) 
  ## Initialize cohort traces ----
  ### Initialize cohort trace under SoC ----
  m_M_SoC <- array(NA, 
                   dim = c((n_cycles + 1), n_states, n_sim), 
                   dimnames = list(0:n_cycles, v_names_states, 1:n_sim))
  #* Store the initial state vector in the first row of the cohort trace
  m_M_SoC[1, , ] <- v_m_init
  ### Initialize cohort trace for competing strategies ----
  #* Structure and initial states are the same as for SoC
  m_M_strA  <- m_M_SoC # Strategy A
  
  
  
  ## Create transition probability arrays for strategy SoC ----
  ### Initialize transition probability array for strategy SoC ----
  #* All transitions to a non-death state are assumed to be conditional on survival
  a_P_SoC <- array(0,
                   dim  = c(n_states, n_states, n_cycles, n_sim),
                   dimnames = list(v_names_states, 
                                   v_names_states, 
                                   0:(n_cycles - 1),
                                   1:n_sim))
  
  # ### Fill in array
  ## From R
  a_P_SoC["R", "R", ,]   <- (1 - (v_p_HDage + score_p_CVD[,])) * (1 - (score_p_MI1[,] + score_p_S1[,]))
  a_P_SoC["R", "MI1", , ]  <- (1 - (v_p_HDage + score_p_CVD[,])) * score_p_MI1[,] 
  a_P_SoC["R", "S1", ,]  <- (1 - (v_p_HDage + score_p_CVD[,])) * score_p_S1[,] 
  a_P_SoC["R", "CVD", ,] <- score_p_CVD[,] 
  a_P_SoC["R", "D", ,]   <- v_p_HDage 
  ## From MI1
  a_P_SoC["MI1", "MI1", ,]  <- (1 - (smart_p_MI1CVD[,] + v_p_HDage)) * (1- (smart_p_MI1S1[,] + smart_p_MI1MI2[,]))
  a_P_SoC["MI1", "MI2", ,]  <- (1 - (smart_p_MI1CVD[,] + v_p_HDage)) * smart_p_MI1MI2[,]
  a_P_SoC["MI1", "S1", ,]  <-  (1 - (smart_p_MI1CVD[,] + v_p_HDage)) * smart_p_MI1S1[,] 
  a_P_SoC["MI1", "CVD", ,]  <- smart_p_MI1CVD[,] 
  a_P_SoC["MI1", "D", ,]  <-  v_p_HDage 
  ## From MI2
  a_P_SoC["MI2", "MI2", ,]  <- (1 - (smart_p_MI2CVD[,] + v_p_HDage)) * (1-smart_p_MI2S1[,]) 
  a_P_SoC["MI2", "S1", ,]  <- (1 - (smart_p_MI2CVD[,] + v_p_HDage)) * smart_p_MI2S1[,] 
  a_P_SoC["MI2", "CVD", ,]  <- smart_p_MI2CVD[,] 
  a_P_SoC["MI2", "D", ,]  <-  v_p_HDage 
  
  ## From S1
  a_P_SoC["S1", "S1", ,] <- (1 - (v_p_HDage + smart_p_S1CVD[,])) * (1-smart_p_S1S2[,]) 
  a_P_SoC["S1", "S2", ,] <- (1 - (v_p_HDage + smart_p_S1CVD[,])) * smart_p_S1S2[,]
  a_P_SoC["S1", "CVD", ,] <- smart_p_S1CVD[,]
  a_P_SoC["S1", "D", ,] <- v_p_HDage 
  
  ## From S2
  a_P_SoC["S2", "S2", ,] <- (1 - (v_p_HDage + smart_p_S2CVD[,])) 
  a_P_SoC["S2", "CVD", ,] <- smart_p_S2CVD[,]
  a_P_SoC["S2", "D", ,] <- v_p_HDage 
  
  ## From CVD
  a_P_SoC["CVD", "CVD", ,]   <- 1
  
  ## From D
  a_P_SoC["D", "D", ,]   <- 1
  
  ### Transition probability array for strategy: CVRM-Box ----
  #a_P_strA <- a_P_SoC
  a_P_strA <- array(0,
                    dim  = c(n_states, n_states, n_cycles, n_sim),
                    dimnames = list(v_names_states, 
                                    v_names_states, 
                                    0:(n_cycles - 1),
                                    1:n_sim))
  ## From R
  a_P_strA["R", "R", ,]   <- (1 - (v_p_HDage + box_score_p_CVD[,])) * (1 - (box_score_p_MI1[,] + box_score_p_S1[,]))
  a_P_strA["R", "MI1", ,]  <- (1 - (v_p_HDage + box_score_p_CVD[,])) * box_score_p_MI1[,] 
  a_P_strA["R", "S1", ,]  <- (1 - (v_p_HDage + box_score_p_CVD[,])) * box_score_p_S1[,] 
  a_P_strA["R", "CVD", ,] <- box_score_p_CVD[,] 
  a_P_strA["R", "D", ,]   <- v_p_HDage 
  ## From MI1
  a_P_strA["MI1", "MI1", ,]  <- (1 - (box_smart_p_MI1CVD[,] + v_p_HDage)) * (1- (box_smart_p_MI1S1[,] + box_smart_p_MI1MI2[,]))
  a_P_strA["MI1", "MI2", ,]  <- (1 - (box_smart_p_MI1CVD[,] + v_p_HDage)) * box_smart_p_MI1MI2[,]
  a_P_strA["MI1", "S1", ,]  <-  (1 - (box_smart_p_MI1CVD[,] + v_p_HDage)) * box_smart_p_MI1S1[,] 
  a_P_strA["MI1", "CVD", ,]  <- box_smart_p_MI1CVD[,] 
  a_P_strA["MI1", "D", ,]  <-  v_p_HDage 
  ## From MI2
  a_P_strA["MI2", "MI2", ,]  <- (1 - (box_smart_p_MI2CVD[,] + v_p_HDage)) * (1-box_smart_p_MI2S1[,]) 
  a_P_strA["MI2", "S1", ,]  <- (1 - (box_smart_p_MI2CVD[,] + v_p_HDage)) * box_smart_p_MI2S1[,] 
  a_P_strA["MI2", "CVD", ,]  <- box_smart_p_MI2CVD[,] 
  a_P_strA["MI2", "D", ,]  <-  v_p_HDage 
  
  ## From S1
  a_P_strA["S1", "S1", ,] <- (1 - (v_p_HDage + box_smart_p_S1CVD[,])) * (1-box_smart_p_S1S2[,]) 
  a_P_strA["S1", "S2", ,] <- (1 - (v_p_HDage + box_smart_p_S1CVD[,])) * box_smart_p_S1S2[,]
  a_P_strA["S1", "CVD", ,] <- box_smart_p_S1CVD[,]
  a_P_strA["S1", "D", ,] <- v_p_HDage 
  
  ## From S2
  a_P_strA["S2", "S2", ,] <- (1 - (v_p_HDage + box_smart_p_S2CVD[,])) 
  a_P_strA["S2", "CVD", ,] <- box_smart_p_S2CVD[,]
  a_P_strA["S2", "D", ,] <- v_p_HDage 
  
  ## From CVD
  a_P_strA["CVD", "CVD", ,]   <- 1
  
  ## From D
  a_P_strA["D", "D", ,]   <- 1
  
  
  ## Check if transition probability arrays are valid ----
  #* Functions included in "R/Functions.R". The latest version can be found in `darthtools` package
  ### Check that transition probabilities are [0, 1] ----
  for(sim in 1:n_sim) {
    #print(sim)
    check_transition_probability(a_P_SoC[,,,sim], verbose = TRUE)
    check_transition_probability(a_P_strA[,,,sim], verbose = TRUE)
    ##Check that all rows for each slice of the array sum to 1
    sum_soc_tmp <- sum(rowSums(a_P_SoC[,,,sim]))
    if (sum_soc_tmp != n_states*n_cycles) {print("Invalid")}
    sum_a_tmp <- sum(rowSums(a_P_strA[,,,sim]))
    if (sum_a_tmp != n_states*n_cycles) {print("Invalid")}
  }
  
  ## Create transition dynamics arrays ----
  #* These arrays will capture transitions from each state to another over time 
  ### Initialize transition dynamics array for strategy SoC ----
  a_A_SoC <- array(0,
                   dim      = c(n_states, n_states, n_cycles + 1, n_sim),
                   dimnames = list(v_names_states, v_names_states, 0:n_cycles, 1:n_sim))
  a_A_strA  <- a_A_SoC #Structure and initial states are the same as for SoC
  
  
  #  Run Markov model ----
  #* Iterative solution of age-dependent cSTM
  for(i in 1:n_sim) {
    #* Set first slice of a_A_SoC and a_A_strA with the initial state vector in its diagonal
    diag(a_A_SoC[, , 1,i]) <- v_m_init
    diag(a_A_strA[, , 1,i]) <- v_m_init
    for(t in 1:n_cycles){
      ## Fill in cohort trace
      # For SoC
      m_M_SoC[t + 1, ,i]  <- m_M_SoC[t, ,i]  %*% a_P_SoC[, , t,i]
      # for strategy: CVRM-Box
      m_M_strA[t + 1, ,i] <- m_M_strA[t,,i ] %*% a_P_strA[, , t,i]
      
      ## Fill in transition-dynamics array
      # For SoC
      a_A_SoC[, , t + 1,i]  <- diag(m_M_SoC[t, ,i]) %*% a_P_SoC[, , t,i]
      # for strategy: CVRM-Box
      a_A_strA[, , t + 1,i] <- diag(m_M_strA[t, ,i]) %*% a_P_strA[, , t,i]
    }
  }
  ## Store the cohort traces in a list ----
  l_m_M <- list(SoC =  m_M_SoC,
                A   =  m_M_strA) 
  
  names(l_m_M) <- v_names_str
  
  ## Store the transition dynamics array for each strategy in a list ----
  l_a_A <- list(SoC =  a_A_SoC,
                A   =  a_A_strA) 
  names(l_a_A) <- v_names_str
  
  
  # State Rewards ----
  v_u_SoC    <- cbind(u_R_sgCN,  
                      u_R_sgCN - u_MI1,  
                      u_R_sgCN - u_S1, 
                      u_R_sgCN - u_MI1 - u_MI2, 
                      u_R_sgCN - u_S1 - u_S2, 
                      u_D,
                      u_CVD)
  colnames(v_u_SoC) <- v_names_states 
  
  #* Vector of state costs under strategy SoC
  v_c_SoC    <- cbind(c_SOC_controlled,  
                      c_SOC_controlled + c_MI1, 
                      c_SOC_controlled + c_S1,  
                      c_SOC_controlled + c_MI1 + c_MI2,  
                      c_SOC_controlled + c_S1 + c_S2,  
                      c_D,
                      c_CVD)
  colnames(v_c_SoC) <- v_names_states 
  #* Vector of state utilities under strategy A
  v_u_strA   <- v_u_SoC
  
  #* Vector of state costs under strategy A
  v_c_strA   <- cbind(c_strA_controlled, 
                      c_MI1+ c_strA_controlled,
                      c_S1+ c_strA_controlled, 
                      c_MI2+ c_MI1+ c_strA_controlled, 
                      c_S2+ c_S1 + c_strA_controlled, 
                      c_D,
                      c_CVD) 
  colnames(v_c_strA) <- v_names_states 
  
  
  ## Store state rewards ----
  #* Store the vectors of state utilities for each strategy in a list 
  l_u <- list(SoC = v_u_SoC,
              A   = v_u_strA)
  #* Store the vectors of state cost for each strategy in a list 
  l_c <- list(SoC =  v_c_SoC,
              A   =  v_c_strA)
  
  #* assign strategy names to matching items in the lists
  names(l_u) <- names(l_c) <- v_names_str
  
  # Compute expected outcomes ----
  #* Create empty vectors to store total utilities and costs 
  v_tot_qaly <- v_tot_cost <- vector(mode = "numeric", length = n_str)
  names(v_tot_qaly) <- names(v_tot_cost) <- v_names_str
  
  
  
  total_costs_sims <- as.data.frame(matrix(NA, nrow=n_sim, ncol=n_str))
  colnames(total_costs_sims) <- v_names_str
  total_QALY_sims <- as.data.frame(matrix(NA, nrow=n_sim, ncol=n_str))
  colnames(total_costs_sims) <- v_names_str
  
  cycle_costs_strA <- as.data.frame(matrix(data=NA, nrow = n_sim, ncol=n_cycles+1))
  cycle_costs_SOC<- as.data.frame(matrix(data=NA, nrow = n_sim, ncol=n_cycles+1))
  
  ## Loop through each strategy and calculate total utilities and costs ----
  for (sim in 1:n_sim){
    for (i in 1:n_str) { # i <- 1
      v_u_str <- l_u[[i]][sim,]   # select the vector of state utilities for the i-th strategy
      v_c_str <- l_c[[i]][sim,]   # select the vector of state costs for the i-th strategy
      a_A_str <- l_a_A[[i]][,,,sim] # select the transition array for the i-th strategy, simulation
      ##* Array of state rewards 
      #* Create transition matrices of state utilities and state costs for the i-th strategy 
      m_u_str   <- matrix(v_u_str, nrow = n_states, ncol = n_states, byrow = T)
      m_c_str   <- matrix(v_c_str, nrow = n_states, ncol = n_states, byrow = T)
      #* Expand the transition matrix of state utilities across cycles to form a transition array of state utilities
      a_R_u_str <- array(m_u_str, 
                         dim      = c(n_states, n_states, n_cycles + 1),
                         dimnames = list(v_names_states, v_names_states, 0:n_cycles))
      # Expand the transition matrix of state costs across cycles to form a transition array of state costs
      a_R_c_str <- array(m_c_str, 
                         dim      = c(n_states, n_states, n_cycles + 1),
                         dimnames = list(v_names_states, v_names_states, 0:n_cycles))

      
      ##* Apply transition rewards
      #* Apply disutility due to transition from H to S1
      a_R_u_str["R", "MI1", ]  <- a_R_u_str["R", "MI1", ]      - du_MI1[sim] #
      a_R_u_str["MI1", "MI2", ] <-  a_R_u_str["MI1", "MI2", ]  - du_MI2[sim] #
      a_R_u_str["R", "S1", ]  <- a_R_u_str["R", "S1", ]      - du_S1[sim] #
      a_R_u_str["MI1", "S1", ]  <- a_R_u_str["MI1", "S1", ]  - du_MItoS[sim] #
      a_R_u_str["MI2", "S1", ]  <- a_R_u_str["MI2", "S1", ]  - du_MItoS[sim] #
      a_R_u_str["S1", "S2", ] <-  a_R_u_str["S1", "S2", ]  - du_S2[sim] #
      
      
      #* Add transition cost per cycle due to transition from H to S1
      a_R_c_str["R", "MI1",]      <- a_R_c_str["R", "MI1",] + ic_MI1[sim] 
      a_R_c_str["MI1", "MI2", ]      <- a_R_c_str["MI1", "MI2", ]       + ic_MI2[sim] #
      a_R_c_str["MI1", "S1", ]      <- a_R_c_str["MI1", "S1", ]       + ic_S1[sim] #
      a_R_c_str["R", "S1", ]      <- a_R_c_str["R", "S1", ]       + ic_S1[sim]  #
      a_R_c_str["S1", "S2", ]      <- a_R_c_str["S1", "S2", ]       + ic_S2[sim] #
      a_R_c_str["MI2", "CVD", ]      <- a_R_c_str["MI2", "CVD", ]       + ic_CVD[sim]
      a_R_c_str["MI2", "S1", ]      <- a_R_c_str["MI2", "S1", ]       + ic_S2[sim]
      a_R_c_str["S2", "CVD", ]      <- a_R_c_str["S2", "CVD", ]       + ic_CVD[sim]
      a_R_c_str["R", "CVD", ]      <- a_R_c_str["R", "CVD", ]       + ic_CVD[sim]
      
      
      
      
      ###* Expected QALYs and costs for all transitions per cycle
      #* QALYs = life years x QoL
      
      a_Y_c_str <- a_A_str * a_R_c_str
      a_Y_u_str <- a_A_str * a_R_u_str 
      
      ###* Expected QALYs and costs per cycle
      ##* Vector of QALYs and costs
      v_qaly_str <- apply(a_Y_u_str, 3, sum) # sum the proportion of the cohort across transitions 
      v_cost_str <- apply(a_Y_c_str, 3, sum) # sum the proportion of the cohort across transitions
      
      #* QALYs
      v_tot_qaly[i] <- t(v_qaly_str) %*% (v_dwe * v_wcc)
      #* Costs
      v_tot_cost[i] <- t(v_cost_str) %*% (v_dwc * v_wcc)
      
      ## Vector with discounted values
      if(i==1){
        total_QALY_sims[sim,i] <- v_tot_qaly[i]
        total_costs_sims[sim,i] <- v_tot_cost[i]
        cycle_costs_SOC[sim,] <- v_cost_str 
      }
      if(i==2){
        total_QALY_sims[sim,i] <- v_tot_qaly[i]
        total_costs_sims[sim,i] <- v_tot_cost[i] + c_init_strA #
        cycle_costs_strA[sim,] <- v_cost_str
      }
    }
  }
  
  #BUDGED IMPACT, regional 
  #SOC BI
  df_BI_reg_SOC <- cycle_costs_SOC[,1:horizon_BI]
  
  df_BI_reg_SOC$first_run <- df_BI_reg_SOC$V1+
    df_BI_reg_SOC$V2+
    df_BI_reg_SOC$V3+
    df_BI_reg_SOC$V4+
    df_BI_reg_SOC$V5
  df_BI_reg_SOC$first_run <- df_BI_reg_SOC$first_run*BI_reg_n_yr1  
  
  df_BI_reg_SOC$second_run <- df_BI_reg_SOC$V1+
    df_BI_reg_SOC$V2+
    df_BI_reg_SOC$V3+
    df_BI_reg_SOC$V4
  df_BI_reg_SOC$second_run <- df_BI_reg_SOC$second_run*BI_reg_n_yr2
  
  df_BI_reg_SOC$third_run <- df_BI_reg_SOC$V1+
    df_BI_reg_SOC$V2+
    df_BI_reg_SOC$V3
  df_BI_reg_SOC$third_run <- df_BI_reg_SOC$third_run*BI_reg_n_yr3
  
  df_BI_reg_SOC$fourth_run <- df_BI_reg_SOC$V1+
    df_BI_reg_SOC$V2
  df_BI_reg_SOC$fourth_run <- df_BI_reg_SOC$fourth_run*BI_reg_n_yr4
  
  df_BI_reg_SOC$fifth_run <- df_BI_reg_SOC$V1
  df_BI_reg_SOC$fifth_run <- df_BI_reg_SOC$fifth_run*BI_reg_n_yr5
  
  df_BI_reg_SOC$total_BI <- df_BI_reg_SOC$first_run + df_BI_reg_SOC$second_run + df_BI_reg_SOC$third_run + df_BI_reg_SOC$fourth_run + df_BI_reg_SOC$fifth_run
  
  
  #intervention BI
  df_BI_reg_strA <- cycle_costs_strA[,1:horizon_BI]
  
  df_BI_reg_strA$first_run <- df_BI_reg_strA$V1+
    df_BI_reg_strA$V2+
    df_BI_reg_strA$V3+
    df_BI_reg_strA$V4+
    df_BI_reg_strA$V5
  df_BI_reg_strA$first_run <- df_BI_reg_strA$first_run*BI_reg_n_yr1 + c_init_strA*BI_reg_n_yr1
  
  df_BI_reg_strA$second_run <- df_BI_reg_strA$V1+
    df_BI_reg_strA$V2+
    df_BI_reg_strA$V3+
    df_BI_reg_strA$V4
  df_BI_reg_strA$second_run <- df_BI_reg_strA$second_run*BI_reg_n_yr2  + c_init_strA*BI_reg_n_yr2
  
  df_BI_reg_strA$third_run <- df_BI_reg_strA$V1+
    df_BI_reg_strA$V2+
    df_BI_reg_strA$V3
  df_BI_reg_strA$third_run <- df_BI_reg_strA$third_run*BI_reg_n_yr3  + c_init_strA*BI_reg_n_yr3
  
  df_BI_reg_strA$fourth_run <- df_BI_reg_strA$V1+
    df_BI_reg_strA$V2
  df_BI_reg_strA$fourth_run <- df_BI_reg_strA$fourth_run*BI_reg_n_yr4 + c_init_strA*BI_reg_n_yr4
  
  df_BI_reg_strA$fifth_run <- df_BI_reg_strA$V1
  df_BI_reg_strA$fifth_run <- df_BI_reg_strA$fifth_run*BI_reg_n_yr5 + c_init_strA*BI_reg_n_yr5
  
  df_BI_reg_strA$total_BI <- df_BI_reg_strA$first_run + df_BI_reg_strA$second_run + df_BI_reg_strA$third_run + df_BI_reg_strA$fourth_run + df_BI_reg_strA$fifth_run  
  
  
  #BUDGED IMPACT, national 
  #SOC BI
  df_BI_nat_SOC <- cycle_costs_SOC[,1:horizon_BI]
  
  df_BI_nat_SOC$first_run <- df_BI_nat_SOC$V1+
    df_BI_nat_SOC$V2+
    df_BI_nat_SOC$V3+
    df_BI_nat_SOC$V4+
    df_BI_nat_SOC$V5
  df_BI_nat_SOC$first_run <- df_BI_nat_SOC$first_run*BI_nat_n_yr1  
  
  df_BI_nat_SOC$second_run <- df_BI_nat_SOC$V1+
    df_BI_nat_SOC$V2+
    df_BI_nat_SOC$V3+
    df_BI_nat_SOC$V4
  df_BI_nat_SOC$second_run <- df_BI_nat_SOC$second_run*BI_nat_n_yr2
  
  df_BI_nat_SOC$third_run <- df_BI_nat_SOC$V1+
    df_BI_nat_SOC$V2+
    df_BI_nat_SOC$V3
  df_BI_nat_SOC$third_run <- df_BI_nat_SOC$third_run*BI_nat_n_yr3
  
  df_BI_nat_SOC$fourth_run <- df_BI_nat_SOC$V1+
    df_BI_nat_SOC$V2
  df_BI_nat_SOC$fourth_run <- df_BI_nat_SOC$fourth_run*BI_nat_n_yr4
  
  df_BI_nat_SOC$fifth_run <- df_BI_nat_SOC$V1
  df_BI_nat_SOC$fifth_run <- df_BI_nat_SOC$fifth_run*BI_nat_n_yr5
  
  df_BI_nat_SOC$total_BI <- df_BI_nat_SOC$first_run + df_BI_nat_SOC$second_run + df_BI_nat_SOC$third_run + df_BI_nat_SOC$fourth_run + df_BI_nat_SOC$fifth_run
  
  
  #intervention BI
  df_BI_nat_strA <- cycle_costs_strA[,1:horizon_BI]
  
  df_BI_nat_strA$first_run <- df_BI_nat_strA$V1+
    df_BI_nat_strA$V2+
    df_BI_nat_strA$V3+
    df_BI_nat_strA$V4+
    df_BI_nat_strA$V5
  df_BI_nat_strA$first_run <- df_BI_nat_strA$first_run*BI_nat_n_yr1 + c_init_strA*BI_nat_n_yr1
  
  df_BI_nat_strA$second_run <- df_BI_nat_strA$V1+
    df_BI_nat_strA$V2+
    df_BI_nat_strA$V3+
    df_BI_nat_strA$V4
  df_BI_nat_strA$second_run <- df_BI_nat_strA$second_run*BI_nat_n_yr2  + c_init_strA*BI_nat_n_yr2
  
  df_BI_nat_strA$third_run <- df_BI_nat_strA$V1+
    df_BI_nat_strA$V2+
    df_BI_nat_strA$V3
  df_BI_nat_strA$third_run <- df_BI_nat_strA$third_run*BI_nat_n_yr3  + c_init_strA*BI_nat_n_yr3
  
  df_BI_nat_strA$fourth_run <- df_BI_nat_strA$V1+
    df_BI_nat_strA$V2
  df_BI_nat_strA$fourth_run <- df_BI_nat_strA$fourth_run*BI_nat_n_yr4 + c_init_strA*BI_nat_n_yr4
  
  df_BI_nat_strA$fifth_run <- df_BI_nat_strA$V1
  df_BI_nat_strA$fifth_run <- df_BI_nat_strA$fifth_run*BI_nat_n_yr5 + c_init_strA*BI_nat_n_yr5
  
  df_BI_nat_strA$total_BI <- df_BI_nat_strA$first_run + df_BI_nat_strA$second_run + df_BI_nat_strA$third_run + df_BI_nat_strA$fourth_run + df_BI_nat_strA$fifth_run  
  
  
  
  return(list(df_BI_reg_SOC = df_BI_reg_SOC,
              df_BI_reg_strA = df_BI_reg_strA,
              df_BI_nat_SOC = df_BI_nat_SOC,
              df_BI_nat_strA = df_BI_nat_strA))
}


#### 11. BI Model for subgroup UN ####

#For subgroup uncontrolled BP, no CVD history

run_BI_model_uncontrolled_nohis <- function() {
  
  
  ### Risk prediction model inputs (SOC) ----
  
  #The effect of the intervention on the transitions is due to the difference on systolic blood pressure 'sbp_diff'
  
  
  ### Transition rates (annual) ----
  #SCORE2 and SCORE2-OP
  score_tprobs <- gen_score_tprobs(intervention = "SOC",
                                   subgroup = "uncontrolled_nohis")
  
  score_p_MI1 <- as.matrix(score_tprobs$score_MI1)
  score_p_S1 <- as.matrix(score_tprobs$score_S1)
  score_p_CVD <- as.matrix(score_tprobs$score_CVD)
  
  
  box_score_tprobs <- gen_score_tprobs(intervention = "Box",
                                       subgroup = "uncontrolled_nohis")
  
  box_score_p_MI1 <- as.matrix(box_score_tprobs$score_MI1)
  box_score_p_S1 <- as.matrix(box_score_tprobs$score_S1)
  box_score_p_CVD <- as.matrix(box_score_tprobs$score_CVD)
  
  
  #SMART2
  smart_tprobs_fromMI <- gen_smart_tprobs(from_state = "MI",
                                          intervention = "SOC",
                                          subgroup = "uncontrolled_nohis")
  
  smart_p_MI1MI2 <- as.matrix(smart_tprobs_fromMI$smart_MI)
  smart_p_MI1S1 <- as.matrix(smart_tprobs_fromMI$smart_S)
  smart_p_MI2S1 <- as.matrix(smart_tprobs_fromMI$smart_S) # 
  smart_p_MI1CVD <- as.matrix(smart_tprobs_fromMI$smart_CVD)
  smart_p_MI2CVD <- as.matrix(smart_tprobs_fromMI$smart_CVD) # 
  
  
  smart_tprobs_fromS <- gen_smart_tprobs(from_state = "S",
                                         intervention = "SOC",
                                         subgroup = "uncontrolled_nohis")
  
  smart_p_S1S2 <- as.matrix(smart_tprobs_fromS$smart_S)
  smart_p_S1CVD <- as.matrix(smart_tprobs_fromS$smart_CVD)
  smart_p_S2CVD <- as.matrix(smart_tprobs_fromS$smart_CVD) #
  
  
  
  box_smart_tprobs_fromMI <- gen_smart_tprobs(from_state = "MI",
                                              intervention = "Box",
                                              subgroup = "uncontrolled_nohis")
  
  box_smart_p_MI1MI2 <- as.matrix(box_smart_tprobs_fromMI$smart_MI)
  box_smart_p_MI1S1 <- as.matrix(box_smart_tprobs_fromMI$smart_S)
  box_smart_p_MI2S1 <- as.matrix(box_smart_tprobs_fromMI$smart_S) # 
  box_smart_p_MI1CVD <- as.matrix(box_smart_tprobs_fromMI$smart_CVD)
  box_smart_p_MI2CVD <- as.matrix(box_smart_tprobs_fromMI$smart_CVD) # 
  
  
  box_smart_tprobs_fromS <- gen_smart_tprobs(from_state = "S",
                                             intervention = "Box",
                                             subgroup = "uncontrolled_nohis")
  
  box_smart_p_S1S2 <- as.matrix(box_smart_tprobs_fromS$smart_S)
  box_smart_p_S1CVD <- as.matrix(box_smart_tprobs_fromS$smart_CVD)
  box_smart_p_S2CVD <- as.matrix(box_smart_tprobs_fromS$smart_CVD) #
  
  ## Age-dependent mortality rates ----
  lt_NL_2021 <- nl_mort()   
  #* Extract age-specific all-cause mortality for ages in model time horizon
  v_r_mort_by_age <- lt_NL_2021 %>% 
    dplyr::filter(Age >= n_age_init & Age < n_age_max) %>%
    dplyr::select(Female, Male) 
  v_r_mort_by_age$Total <- v_r_mort_by_age$Female*(1-prop_male)+v_r_mort_by_age$Male*prop_male
  v_r_mort_by_age <- as.matrix(v_r_mort_by_age$Total)
  
  #Also extract age-specific scaling values for all-cause mortality to account for CVD deaths
  df_mort_p_scaling <- mort_p_scaling()
  v_p_mort_scale <- df_mort_p_scaling %>% 
    dplyr::filter(Age >= n_age_init & Age < n_age_max) %>%
    dplyr::select(Female, Male) 
  v_p_mort_scale$Total <- v_p_mort_scale$Female*(1-prop_male)+v_p_mort_scale$Male*prop_male
  v_p_mort_scale <- as.numeric(v_p_mort_scale$Total)
  
  ### Discount weight for costs and effects ----
  v_dwc  <- 1 / ((1 + (d_c * cycle_length)) ^ (0:n_cycles))
  v_dwe  <- 1 / ((1 + (d_e * cycle_length)) ^ (0:n_cycles))
  
  # Process model inputs ----
  ## Age-specific transition rates to the Dead state for all cycles ----
  v_r_HDage  <- rep(v_r_mort_by_age, each = 1/cycle_length)
  #* Name age-specific mortality vector 
  names(v_r_HDage) <- v_age_names
  
  #* Function included in "R/Functions.R". The latest version can be found in `darthtools` package
  v_p_HDage  <- rate_to_prob(v_r_HDage, t = cycle_length)  # Age-specific mortality risk in the Healthy state 
  v_p_HDage <- v_p_HDage*(1-v_p_mort_scale) 
  
  # Construct state-transition models ----
  ## Initial state vector ----
  #* All starting healthy
  v_m_init <- c(R = 1, MI1=0, MI2=0, S1 = 0, S2 = 0, CVD=0 , D = 0) 
  ## Initialize cohort traces ----
  ### Initialize cohort trace under SoC ----
  m_M_SoC <- array(NA, 
                   dim = c((n_cycles + 1), n_states, n_sim), 
                   dimnames = list(0:n_cycles, v_names_states, 1:n_sim))
  #* Store the initial state vector in the first row of the cohort trace
  m_M_SoC[1, , ] <- v_m_init
  
  ### Initialize cohort trace for competing strategies ----
  #* Structure and initial states are the same as for SoC
  m_M_strA  <- m_M_SoC # Strategy A
  
  
  
  ## Create transition probability arrays for strategy SoC ----
  ### Initialize transition probability array for strategy SoC ----
  #* All transitions to a non-death state are assumed to be conditional on survival
  a_P_SoC <- array(0,
                   dim  = c(n_states, n_states, n_cycles, n_sim),
                   dimnames = list(v_names_states, 
                                   v_names_states, 
                                   0:(n_cycles - 1),
                                   1:n_sim))
  
  # ### Fill in array
  ## From R
  a_P_SoC["R", "R", ,]   <- (1 - (v_p_HDage + score_p_CVD[,])) * (1 - (score_p_MI1[,] + score_p_S1[,]))
  a_P_SoC["R", "MI1", , ]  <- (1 - (v_p_HDage + score_p_CVD[,])) * score_p_MI1[,] 
  a_P_SoC["R", "S1", ,]  <- (1 - (v_p_HDage + score_p_CVD[,])) * score_p_S1[,] 
  a_P_SoC["R", "CVD", ,] <- score_p_CVD[,] 
  a_P_SoC["R", "D", ,]   <- v_p_HDage 
  ## From MI1
  a_P_SoC["MI1", "MI1", ,]  <- (1 - (smart_p_MI1CVD[,] + v_p_HDage)) * (1- (smart_p_MI1S1[,] + smart_p_MI1MI2[,]))
  a_P_SoC["MI1", "MI2", ,]  <- (1 - (smart_p_MI1CVD[,] + v_p_HDage)) * smart_p_MI1MI2[,]
  a_P_SoC["MI1", "S1", ,]  <-  (1 - (smart_p_MI1CVD[,] + v_p_HDage)) * smart_p_MI1S1[,] 
  a_P_SoC["MI1", "CVD", ,]  <- smart_p_MI1CVD[,] 
  a_P_SoC["MI1", "D", ,]  <-  v_p_HDage 
  ## From MI2
  a_P_SoC["MI2", "MI2", ,]  <- (1 - (smart_p_MI2CVD[,] + v_p_HDage)) * (1-smart_p_MI2S1[,]) 
  a_P_SoC["MI2", "S1", ,]  <- (1 - (smart_p_MI2CVD[,] + v_p_HDage)) * smart_p_MI2S1[,] 
  a_P_SoC["MI2", "CVD", ,]  <- smart_p_MI2CVD[,] 
  a_P_SoC["MI2", "D", ,]  <-  v_p_HDage 
  
  ## From S1
  a_P_SoC["S1", "S1", ,] <- (1 - (v_p_HDage + smart_p_S1CVD[,])) * (1-smart_p_S1S2[,]) 
  a_P_SoC["S1", "S2", ,] <- (1 - (v_p_HDage + smart_p_S1CVD[,])) * smart_p_S1S2[,]
  a_P_SoC["S1", "CVD", ,] <- smart_p_S1CVD[,]
  a_P_SoC["S1", "D", ,] <- v_p_HDage 
  
  ## From S2
  a_P_SoC["S2", "S2", ,] <- (1 - (v_p_HDage + smart_p_S2CVD[,])) 
  a_P_SoC["S2", "CVD", ,] <- smart_p_S2CVD[,]
  a_P_SoC["S2", "D", ,] <- v_p_HDage 
  
  ## From CVD
  a_P_SoC["CVD", "CVD", ,]   <- 1
  
  ## From D
  a_P_SoC["D", "D", ,]   <- 1
  
  ### Transition probability array for strategy: CVRM-Box ----
  #a_P_strA <- a_P_SoC
  a_P_strA <- array(0,
                    dim  = c(n_states, n_states, n_cycles, n_sim),
                    dimnames = list(v_names_states, 
                                    v_names_states, 
                                    0:(n_cycles - 1),
                                    1:n_sim))
  ## From R
  a_P_strA["R", "R", ,]   <- (1 - (v_p_HDage + box_score_p_CVD[,])) * (1 - (box_score_p_MI1[,] + box_score_p_S1[,]))
  a_P_strA["R", "MI1", ,]  <- (1 - (v_p_HDage + box_score_p_CVD[,])) * box_score_p_MI1[,] 
  a_P_strA["R", "S1", ,]  <- (1 - (v_p_HDage + box_score_p_CVD[,])) * box_score_p_S1[,] 
  a_P_strA["R", "CVD", ,] <- box_score_p_CVD[,] 
  a_P_strA["R", "D", ,]   <- v_p_HDage 
  ## From MI1
  a_P_strA["MI1", "MI1", ,]  <- (1 - (box_smart_p_MI1CVD[,] + v_p_HDage)) * (1- (box_smart_p_MI1S1[,] + box_smart_p_MI1MI2[,]))
  a_P_strA["MI1", "MI2", ,]  <- (1 - (box_smart_p_MI1CVD[,] + v_p_HDage)) * box_smart_p_MI1MI2[,]
  a_P_strA["MI1", "S1", ,]  <-  (1 - (box_smart_p_MI1CVD[,] + v_p_HDage)) * box_smart_p_MI1S1[,] 
  a_P_strA["MI1", "CVD", ,]  <- box_smart_p_MI1CVD[,] 
  a_P_strA["MI1", "D", ,]  <-  v_p_HDage 
  ## From MI2
  a_P_strA["MI2", "MI2", ,]  <- (1 - (box_smart_p_MI2CVD[,] + v_p_HDage)) * (1-box_smart_p_MI2S1[,]) 
  a_P_strA["MI2", "S1", ,]  <- (1 - (box_smart_p_MI2CVD[,] + v_p_HDage)) * box_smart_p_MI2S1[,] 
  a_P_strA["MI2", "CVD", ,]  <- box_smart_p_MI2CVD[,] 
  a_P_strA["MI2", "D", ,]  <-  v_p_HDage 
  
  ## From S1
  a_P_strA["S1", "S1", ,] <- (1 - (v_p_HDage + box_smart_p_S1CVD[,])) * (1-box_smart_p_S1S2[,]) 
  a_P_strA["S1", "S2", ,] <- (1 - (v_p_HDage + box_smart_p_S1CVD[,])) * box_smart_p_S1S2[,]
  a_P_strA["S1", "CVD", ,] <- box_smart_p_S1CVD[,]
  a_P_strA["S1", "D", ,] <- v_p_HDage 
  
  ## From S2
  a_P_strA["S2", "S2", ,] <- (1 - (v_p_HDage + box_smart_p_S2CVD[,])) 
  a_P_strA["S2", "CVD", ,] <- box_smart_p_S2CVD[,]
  a_P_strA["S2", "D", ,] <- v_p_HDage 
  
  ## From CVD
  a_P_strA["CVD", "CVD", ,]   <- 1
  
  ## From D
  a_P_strA["D", "D", ,]   <- 1
  
  ## Check if transition probability arrays are valid ----
  #* Functions included in "R/Functions.R". The latest version can be found in `darthtools` package
  ### Check that transition probabilities are [0, 1] ----
  for(sim in 1:n_sim) {
    #print(sim)
    check_transition_probability(a_P_SoC[,,,sim], verbose = TRUE)
    check_transition_probability(a_P_strA[,,,sim], verbose = TRUE)
    ## Check that all rows for each slice of the array sum to 1 
    sum_soc_tmp <- sum(rowSums(a_P_SoC[,,,sim]))
    if (sum_soc_tmp != n_states*n_cycles) {print("Invalid")}
    sum_a_tmp <- sum(rowSums(a_P_strA[,,,sim]))
    if (sum_a_tmp != n_states*n_cycles) {print("Invalid")}
  }
  
  ## Create transition dynamics arrays ----
  #* These arrays will capture transitions from each state to another over time 
  ### Initialize transition dynamics array for strategy SoC ----
  a_A_SoC <- array(0,
                   dim      = c(n_states, n_states, n_cycles + 1, n_sim),
                   dimnames = list(v_names_states, v_names_states, 0:n_cycles, 1:n_sim))
  a_A_strA  <- a_A_SoC #Structure and initial states are the same as for SoC
  
  
  #  Run Markov model ----
  #* Iterative solution of age-dependent cSTM
  for(i in 1:n_sim) {
    #* Set first slice of a_A_SoC and a_A_strA with the initial state vector in its diagonal
    diag(a_A_SoC[, , 1,i]) <- v_m_init
    diag(a_A_strA[, , 1,i]) <- v_m_init
    for(t in 1:n_cycles){
      ## Fill in cohort trace
      # For SoC
      m_M_SoC[t + 1, ,i]  <- m_M_SoC[t, ,i]  %*% a_P_SoC[, , t,i]
      # for strategy: CVRM-Box
      m_M_strA[t + 1, ,i] <- m_M_strA[t,,i ] %*% a_P_strA[, , t,i]
      
      ## Fill in transition-dynamics array
      # For SoC
      a_A_SoC[, , t + 1,i]  <- diag(m_M_SoC[t, ,i]) %*% a_P_SoC[, , t,i]
      # for strategy: CVRM-Box
      a_A_strA[, , t + 1,i] <- diag(m_M_strA[t, ,i]) %*% a_P_strA[, , t,i]
    }
  }
  ## Store the cohort traces in a list ----
  l_m_M <- list(SoC =  m_M_SoC,
                A   =  m_M_strA) 
  
  names(l_m_M) <- v_names_str
  
  ## Store the transition dynamics array for each strategy in a list ----
  l_a_A <- list(SoC =  a_A_SoC,
                A   =  a_A_strA) 
  names(l_a_A) <- v_names_str
  
  
  # State Rewards ----
  v_u_SoC    <- cbind(u_R_sgCN - u_hypertension,  
                      u_R_sgCN - u_hypertension - u_MI1,  
                      u_R_sgCN - u_hypertension - u_S1, 
                      u_R_sgCN - u_hypertension - u_MI1 - u_MI2, 
                      u_R_sgCN - u_hypertension - u_S1 - u_S2, 
                      u_D,
                      u_CVD)
  colnames(v_u_SoC) <- v_names_states 
  
  #* Vector of state costs under strategy SoC
  v_c_SoC    <- cbind(c_SOC_uncontrolled ,  
                      c_SOC_uncontrolled  + c_MI1, 
                      c_SOC_uncontrolled  + c_S1,  
                      c_SOC_uncontrolled  + c_MI1 + c_MI2,  
                      c_SOC_uncontrolled  + c_S1 + c_S2,  
                      c_D,
                      c_CVD)
  colnames(v_c_SoC) <- v_names_states 
  #* Vector of state utilities under strategy A
  v_u_strA   <- v_u_SoC
  
  #* Vector of state costs under strategy A
  v_c_strA   <- cbind(c_strA_uncontrolled , 
                      c_MI1  + c_strA_uncontrolled,
                      c_S1  + c_strA_uncontrolled, 
                      c_MI2 + c_MI1+ c_strA_uncontrolled, 
                      c_S2 + c_S1 + c_strA_uncontrolled, 
                      c_D,
                      c_CVD) 
  colnames(v_c_strA) <- v_names_states 
  
  
  ## Store state rewards ----
  #* Store the vectors of state utilities for each strategy in a list 
  l_u <- list(SoC = v_u_SoC,
              A   = v_u_strA)
  #* Store the vectors of state cost for each strategy in a list 
  l_c <- list(SoC =  v_c_SoC,
              A   =  v_c_strA)
  
  #* assign strategy names to matching items in the lists
  names(l_u) <- names(l_c) <- v_names_str
  
  # Compute expected outcomes ----
  #* Create empty vectors to store total utilities and costs 
  v_tot_qaly <- v_tot_cost <- vector(mode = "numeric", length = n_str)
  names(v_tot_qaly) <- names(v_tot_cost) <- v_names_str
  
  
  
  total_costs_sims <- as.data.frame(matrix(NA, nrow=n_sim, ncol=n_str))
  colnames(total_costs_sims) <- v_names_str
  total_QALY_sims <- as.data.frame(matrix(NA, nrow=n_sim, ncol=n_str))
  colnames(total_costs_sims) <- v_names_str
  
  cycle_costs_strA <- as.data.frame(matrix(data=NA, nrow = n_sim, ncol=n_cycles+1))
  cycle_costs_SOC<- as.data.frame(matrix(data=NA, nrow = n_sim, ncol=n_cycles+1))
  
  ## Loop through each strategy and calculate total utilities and costs ----
  for (sim in 1:n_sim){
    for (i in 1:n_str) { # i <- 1
      v_u_str <- l_u[[i]][sim,]   # select the vector of state utilities for the i-th strategy
      v_c_str <- l_c[[i]][sim,]   # select the vector of state costs for the i-th strategy
      a_A_str <- l_a_A[[i]][,,,sim] # select the transition array for the i-th strategy, simulation
      ##* Array of state rewards 
      #* Create transition matrices of state utilities and state costs for the i-th strategy 
      m_u_str   <- matrix(v_u_str, nrow = n_states, ncol = n_states, byrow = T)
      m_c_str   <- matrix(v_c_str, nrow = n_states, ncol = n_states, byrow = T)
      #* Expand the transition matrix of state utilities across cycles to form a transition array of state utilities
      a_R_u_str <- array(m_u_str, 
                         dim      = c(n_states, n_states, n_cycles + 1),
                         dimnames = list(v_names_states, v_names_states, 0:n_cycles))
      # Expand the transition matrix of state costs across cycles to form a transition array of state costs
      a_R_c_str <- array(m_c_str, 
                         dim      = c(n_states, n_states, n_cycles + 1),
                         dimnames = list(v_names_states, v_names_states, 0:n_cycles))

      ##* Apply transition rewards
      #* Apply disutility due to transition from H to S1
      a_R_u_str["R", "MI1", ]  <- a_R_u_str["R", "MI1", ]      - du_MI1[sim] #
      a_R_u_str["MI1", "MI2", ] <-  a_R_u_str["MI1", "MI2", ]  - du_MI2[sim] #
      a_R_u_str["MI2", "S1", ]  <- a_R_u_str["MI2", "S1", ]  - du_MItoS[sim] #
      a_R_u_str["R", "S1", ]  <- a_R_u_str["R", "S1", ]      - du_S1[sim] #
      a_R_u_str["MI1", "S1", ]  <- a_R_u_str["MI1", "S1", ]  - du_MItoS[sim] #
      a_R_u_str["S1", "S2", ] <-  a_R_u_str["S1", "S2", ]  - du_S2[sim] #
      
      
      #* Add transition cost per cycle due to transition from H to S1
      a_R_c_str["R", "MI1",]      <- a_R_c_str["R", "MI1",] + ic_MI1[sim] #
      a_R_c_str["MI1", "MI2", ]      <- a_R_c_str["MI1", "MI2", ]       + ic_MI2[sim] #
      a_R_c_str["MI1", "S1", ]      <- a_R_c_str["MI1", "S1", ]       + ic_S1[sim] #
      a_R_c_str["R", "S1", ]      <- a_R_c_str["R", "S1", ]       + ic_S1[sim]  #
      a_R_c_str["S1", "S2", ]      <- a_R_c_str["S1", "S2", ]       + ic_S2[sim] #
      a_R_c_str["MI2", "CVD", ]      <- a_R_c_str["MI2", "CVD", ]       + ic_CVD[sim]
      a_R_c_str["MI2", "S1", ]      <- a_R_c_str["MI2", "S1", ]       + ic_S2[sim]
      a_R_c_str["S2", "CVD", ]      <- a_R_c_str["S2", "CVD", ]       + ic_CVD[sim]
      a_R_c_str["R", "CVD", ]      <- a_R_c_str["R", "CVD", ]       + ic_CVD[sim]
      
      
      
      
      ###* Expected QALYs and costs for all transitions per cycle
      #* QALYs = life years x QoL
      
      a_Y_c_str <- a_A_str * a_R_c_str
      a_Y_u_str <- a_A_str * a_R_u_str 
      
      ###* Expected QALYs and costs per cycle
      ##* Vector of QALYs and costs
      v_qaly_str <- apply(a_Y_u_str, 3, sum) # sum the proportion of the cohort across transitions 
      v_cost_str <- apply(a_Y_c_str, 3, sum) # sum the proportion of the cohort across transitions
      
      
      #* QALYs
      v_tot_qaly[i] <- t(v_qaly_str) %*% (v_dwe * v_wcc)
      #* Costs
      v_tot_cost[i] <- t(v_cost_str) %*% (v_dwc * v_wcc)
      
      ## Vector with discounted values
      if(i==1){
        total_QALY_sims[sim,i] <- v_tot_qaly[i]
        total_costs_sims[sim,i] <- v_tot_cost[i]
        cycle_costs_SOC[sim,] <- v_cost_str
      }
      if(i==2){
        total_QALY_sims[sim,i] <- v_tot_qaly[i]
        total_costs_sims[sim,i] <- v_tot_cost[i] + c_init_strA #
        cycle_costs_strA[sim,] <- v_cost_str
      }
    }
  }
  
  
  #BUDGET IMPACT, regional
  #SOC BI
  df_BI_reg_SOC <- cycle_costs_SOC[,1:horizon_BI]
  
  df_BI_reg_SOC$first_run <- df_BI_reg_SOC$V1+
    df_BI_reg_SOC$V2+
    df_BI_reg_SOC$V3+
    df_BI_reg_SOC$V4+
    df_BI_reg_SOC$V5
  df_BI_reg_SOC$first_run <- df_BI_reg_SOC$first_run*BI_reg_n_yr1 
  
  df_BI_reg_SOC$second_run <- df_BI_reg_SOC$V1+
    df_BI_reg_SOC$V2+
    df_BI_reg_SOC$V3+
    df_BI_reg_SOC$V4
  df_BI_reg_SOC$second_run <- df_BI_reg_SOC$second_run*BI_reg_n_yr2
  
  df_BI_reg_SOC$third_run <- df_BI_reg_SOC$V1+
    df_BI_reg_SOC$V2+
    df_BI_reg_SOC$V3
  df_BI_reg_SOC$third_run <- df_BI_reg_SOC$third_run*BI_reg_n_yr3
  
  df_BI_reg_SOC$fourth_run <- df_BI_reg_SOC$V1+
    df_BI_reg_SOC$V2
  df_BI_reg_SOC$fourth_run <- df_BI_reg_SOC$fourth_run*BI_reg_n_yr4 
  
  df_BI_reg_SOC$fifth_run <- df_BI_reg_SOC$V1
  df_BI_reg_SOC$fifth_run <- df_BI_reg_SOC$fifth_run*BI_reg_n_yr5
  
  df_BI_reg_SOC$total_BI <- df_BI_reg_SOC$first_run + df_BI_reg_SOC$second_run + df_BI_reg_SOC$third_run + df_BI_reg_SOC$fourth_run + df_BI_reg_SOC$fifth_run
  
  
  #intervention BI
  df_BI_reg_strA <- cycle_costs_strA[,1:horizon_BI]
  
  df_BI_reg_strA$first_run <- df_BI_reg_strA$V1+
    df_BI_reg_strA$V2+
    df_BI_reg_strA$V3+
    df_BI_reg_strA$V4+
    df_BI_reg_strA$V5
  df_BI_reg_strA$first_run <- df_BI_reg_strA$first_run*BI_reg_n_yr1 + c_init_strA*BI_reg_n_yr1 
  
  df_BI_reg_strA$second_run <- df_BI_reg_strA$V1+
    df_BI_reg_strA$V2+
    df_BI_reg_strA$V3+
    df_BI_reg_strA$V4
  df_BI_reg_strA$second_run <- df_BI_reg_strA$second_run*BI_reg_n_yr2  + c_init_strA*BI_reg_n_yr2
  
  df_BI_reg_strA$third_run <- df_BI_reg_strA$V1+
    df_BI_reg_strA$V2+
    df_BI_reg_strA$V3
  df_BI_reg_strA$third_run <- df_BI_reg_strA$third_run*BI_reg_n_yr3  + c_init_strA*BI_reg_n_yr3
  
  df_BI_reg_strA$fourth_run <- df_BI_reg_strA$V1+
    df_BI_reg_strA$V2
  df_BI_reg_strA$fourth_run <- df_BI_reg_strA$fourth_run*BI_reg_n_yr4 + c_init_strA*BI_reg_n_yr4
  
  df_BI_reg_strA$fifth_run <- df_BI_reg_strA$V1
  df_BI_reg_strA$fifth_run <- df_BI_reg_strA$fifth_run*BI_reg_n_yr5 + c_init_strA*BI_reg_n_yr5
  
  df_BI_reg_strA$total_BI <- df_BI_reg_strA$first_run + df_BI_reg_strA$second_run + df_BI_reg_strA$third_run + df_BI_reg_strA$fourth_run + df_BI_reg_strA$fifth_run  
  
  
  #BUDGET IMPACT, national
  #SOC BI
  df_BI_nat_SOC <- cycle_costs_SOC[,1:horizon_BI]
  
  df_BI_nat_SOC$first_run <- df_BI_nat_SOC$V1+
    df_BI_nat_SOC$V2+
    df_BI_nat_SOC$V3+
    df_BI_nat_SOC$V4+
    df_BI_nat_SOC$V5
  df_BI_nat_SOC$first_run <- df_BI_nat_SOC$first_run*BI_nat_n_yr1 
  
  df_BI_nat_SOC$second_run <- df_BI_nat_SOC$V1+
    df_BI_nat_SOC$V2+
    df_BI_nat_SOC$V3+
    df_BI_nat_SOC$V4
  df_BI_nat_SOC$second_run <- df_BI_nat_SOC$second_run*BI_nat_n_yr2
  
  df_BI_nat_SOC$third_run <- df_BI_nat_SOC$V1+
    df_BI_nat_SOC$V2+
    df_BI_nat_SOC$V3
  df_BI_nat_SOC$third_run <- df_BI_nat_SOC$third_run*BI_nat_n_yr3
  
  df_BI_nat_SOC$fourth_run <- df_BI_nat_SOC$V1+
    df_BI_nat_SOC$V2
  df_BI_nat_SOC$fourth_run <- df_BI_nat_SOC$fourth_run*BI_nat_n_yr4 
  
  df_BI_nat_SOC$fifth_run <- df_BI_nat_SOC$V1
  df_BI_nat_SOC$fifth_run <- df_BI_nat_SOC$fifth_run*BI_nat_n_yr5
  
  df_BI_nat_SOC$total_BI <- df_BI_nat_SOC$first_run + df_BI_nat_SOC$second_run + df_BI_nat_SOC$third_run + df_BI_nat_SOC$fourth_run + df_BI_nat_SOC$fifth_run
  
  
  #intervention BI
  df_BI_nat_strA <- cycle_costs_strA[,1:horizon_BI]
  
  df_BI_nat_strA$first_run <- df_BI_nat_strA$V1+
    df_BI_nat_strA$V2+
    df_BI_nat_strA$V3+
    df_BI_nat_strA$V4+
    df_BI_nat_strA$V5
  df_BI_nat_strA$first_run <- df_BI_nat_strA$first_run*BI_nat_n_yr1 + c_init_strA*BI_nat_n_yr1 
  
  df_BI_nat_strA$second_run <- df_BI_nat_strA$V1+
    df_BI_nat_strA$V2+
    df_BI_nat_strA$V3+
    df_BI_nat_strA$V4
  df_BI_nat_strA$second_run <- df_BI_nat_strA$second_run*BI_nat_n_yr2  + c_init_strA*BI_nat_n_yr2
  
  df_BI_nat_strA$third_run <- df_BI_nat_strA$V1+
    df_BI_nat_strA$V2+
    df_BI_nat_strA$V3
  df_BI_nat_strA$third_run <- df_BI_nat_strA$third_run*BI_nat_n_yr3  + c_init_strA*BI_nat_n_yr3
  
  df_BI_nat_strA$fourth_run <- df_BI_nat_strA$V1+
    df_BI_nat_strA$V2
  df_BI_nat_strA$fourth_run <- df_BI_nat_strA$fourth_run*BI_nat_n_yr4 + c_init_strA*BI_nat_n_yr4
  
  df_BI_nat_strA$fifth_run <- df_BI_nat_strA$V1
  df_BI_nat_strA$fifth_run <- df_BI_nat_strA$fifth_run*BI_nat_n_yr5 + c_init_strA*BI_nat_n_yr5
  
  df_BI_nat_strA$total_BI <- df_BI_nat_strA$first_run + df_BI_nat_strA$second_run + df_BI_nat_strA$third_run + df_BI_nat_strA$fourth_run + df_BI_nat_strA$fifth_run  
  
  
  return(list(df_BI_reg_SOC = df_BI_reg_SOC,
              df_BI_reg_strA = df_BI_reg_strA,
              df_BI_nat_SOC = df_BI_nat_SOC,
              df_BI_nat_strA = df_BI_nat_strA))
}


#### 12. BI Model for subgroup CY ####

#For subgroup controlled BP, with a CVD history

run_BI_model_controlled_yeshis <- function() {
  
  
  ### Risk prediction model inputs (SOC) ----
  
  #The effect of the intervention on the transitions is due to the difference on systolic blood pressure 'sbp_diff'
  
  
  ### Transition rates (annual) ----
  #SCORE2 and SCORE2-OP
  score_tprobs <- gen_score_tprobs(intervention = "SOC",
                                   subgroup = "controlled_nohis") #this will not be used in this run because people start in MI or Stroke
  
  score_p_MI1 <- as.matrix(score_tprobs$score_MI1)
  score_p_S1 <- as.matrix(score_tprobs$score_S1)
  score_p_CVD <- as.matrix(score_tprobs$score_CVD)
  
  
  box_score_tprobs <- gen_score_tprobs(intervention = "Box",
                                       subgroup = "controlled_nohis") #this will not be used in this run because people start in MI or Stroke
  
  box_score_p_MI1 <- as.matrix(box_score_tprobs$score_MI1)
  box_score_p_S1 <- as.matrix(box_score_tprobs$score_S1)
  box_score_p_CVD <- as.matrix(box_score_tprobs$score_CVD)
  
  
  #SMART2
  smart_tprobs_fromMI <- gen_smart_tprobs(from_state = "MI",
                                          intervention = "SOC",
                                          subgroup = "controlled_yeshis")
  
  smart_p_MI1MI2 <- as.matrix(smart_tprobs_fromMI$smart_MI)
  smart_p_MI1S1 <- as.matrix(smart_tprobs_fromMI$smart_S)
  smart_p_MI2S1 <- as.matrix(smart_tprobs_fromMI$smart_S) # 
  smart_p_MI1CVD <- as.matrix(smart_tprobs_fromMI$smart_CVD)
  smart_p_MI2CVD <- as.matrix(smart_tprobs_fromMI$smart_CVD) # 
  
  
  smart_tprobs_fromS <- gen_smart_tprobs(from_state = "S",
                                         intervention = "SOC",
                                         subgroup = "controlled_yeshis")
  
  smart_p_S1S2 <- as.matrix(smart_tprobs_fromS$smart_S)
  smart_p_S1CVD <- as.matrix(smart_tprobs_fromS$smart_CVD)
  smart_p_S2CVD <- as.matrix(smart_tprobs_fromS$smart_CVD) #
  
  
  
  box_smart_tprobs_fromMI <- gen_smart_tprobs(from_state = "MI",
                                              intervention = "Box",
                                              subgroup = "controlled_yeshis")
  
  box_smart_p_MI1MI2 <- as.matrix(box_smart_tprobs_fromMI$smart_MI)
  box_smart_p_MI1S1 <- as.matrix(box_smart_tprobs_fromMI$smart_S)
  box_smart_p_MI2S1 <- as.matrix(box_smart_tprobs_fromMI$smart_S) # 
  box_smart_p_MI1CVD <- as.matrix(box_smart_tprobs_fromMI$smart_CVD)
  box_smart_p_MI2CVD <- as.matrix(box_smart_tprobs_fromMI$smart_CVD) # 
  
  
  box_smart_tprobs_fromS <- gen_smart_tprobs(from_state = "S",
                                             intervention = "Box",
                                             subgroup = "controlled_yeshis")
  
  box_smart_p_S1S2 <- as.matrix(box_smart_tprobs_fromS$smart_S)
  box_smart_p_S1CVD <- as.matrix(box_smart_tprobs_fromS$smart_CVD)
  box_smart_p_S2CVD <- as.matrix(box_smart_tprobs_fromS$smart_CVD) #
  
  ## Age-dependent mortality rates ----
  lt_NL_2021 <- nl_mort()   
  #* Extract age-specific all-cause mortality for ages in model time horizon
  v_r_mort_by_age <- lt_NL_2021 %>% 
    dplyr::filter(Age >= n_age_init & Age < n_age_max) %>%
    dplyr::select(Female, Male) 
  v_r_mort_by_age$Total <- v_r_mort_by_age$Female*(1-prop_male)+v_r_mort_by_age$Male*prop_male
  v_r_mort_by_age <- as.matrix(v_r_mort_by_age$Total)
  
  #Also extract age-specific scaling values for all-cause mortality to account for CVD deaths
  df_mort_p_scaling <- mort_p_scaling()
  v_p_mort_scale <- df_mort_p_scaling %>% 
    dplyr::filter(Age >= n_age_init & Age < n_age_max) %>%
    dplyr::select(Female, Male) 
  v_p_mort_scale$Total <- v_p_mort_scale$Female*(1-prop_male)+v_p_mort_scale$Male*prop_male
  v_p_mort_scale <- as.numeric(v_p_mort_scale$Total)
  
  ### Discount weight for costs and effects ----
  v_dwc  <- 1 / ((1 + (d_c * cycle_length)) ^ (0:n_cycles))
  v_dwe  <- 1 / ((1 + (d_e * cycle_length)) ^ (0:n_cycles))
  
  # Process model inputs ----
  ## Age-specific transition rates to the Dead state for all cycles ----
  v_r_HDage  <- rep(v_r_mort_by_age, each = 1/cycle_length)
  #* Name age-specific mortality vector 
  names(v_r_HDage) <- v_age_names
  
  #* Function included in "R/Functions.R". The latest version can be found in `darthtools` package
  v_p_HDage  <- rate_to_prob(v_r_HDage, t = cycle_length)  # Age-specific mortality risk in the Healthy state 
  v_p_HDage <- v_p_HDage*(1-v_p_mort_scale) 
  
  # Construct state-transition models ----
  ## Initial state vector ----
  #* All starting healthy
  v_m_init <- c(R = 0, MI1=0.64, MI2=0, S1 = 0.36, S2 = 0, CVD=0 , D = 0) 
  ## Initialize cohort traces ----
  ### Initialize cohort trace under SoC ----
  m_M_SoC <- array(NA, 
                   dim = c((n_cycles + 1), n_states, n_sim), 
                   dimnames = list(0:n_cycles, v_names_states, 1:n_sim))
  #* Store the initial state vector in the first row of the cohort trace
  m_M_SoC[1, , ] <- v_m_init
  
  ### Initialize cohort trace for competing strategies ----
  #* Structure and initial states are the same as for SoC
  m_M_strA  <- m_M_SoC # Strategy A
  
  
  
  ## Create transition probability arrays for strategy SoC ----
  ### Initialize transition probability array for strategy SoC ----
  #* All transitions to a non-death state are assumed to be conditional on survival
  a_P_SoC <- array(0,
                   dim  = c(n_states, n_states, n_cycles, n_sim),
                   dimnames = list(v_names_states, 
                                   v_names_states, 
                                   0:(n_cycles - 1),
                                   1:n_sim))
  
  # ### Fill in array
  ## From R
  a_P_SoC["R", "R", ,]   <- (1 - (v_p_HDage + score_p_CVD[,])) * (1 - (score_p_MI1[,] + score_p_S1[,]))
  a_P_SoC["R", "MI1", , ]  <- (1 - (v_p_HDage + score_p_CVD[,])) * score_p_MI1[,] 
  a_P_SoC["R", "S1", ,]  <- (1 - (v_p_HDage + score_p_CVD[,])) * score_p_S1[,] 
  a_P_SoC["R", "CVD", ,] <- score_p_CVD[,] 
  a_P_SoC["R", "D", ,]   <- v_p_HDage 
  ## From MI1
  a_P_SoC["MI1", "MI1", ,]  <- (1 - (smart_p_MI1CVD[,] + v_p_HDage)) * (1- (smart_p_MI1S1[,] + smart_p_MI1MI2[,]))
  a_P_SoC["MI1", "MI2", ,]  <- (1 - (smart_p_MI1CVD[,] + v_p_HDage)) * smart_p_MI1MI2[,]
  a_P_SoC["MI1", "S1", ,]  <-  (1 - (smart_p_MI1CVD[,] + v_p_HDage)) * smart_p_MI1S1[,] 
  a_P_SoC["MI1", "CVD", ,]  <- smart_p_MI1CVD[,] 
  a_P_SoC["MI1", "D", ,]  <-  v_p_HDage 
  ## From MI2
  a_P_SoC["MI2", "MI2", ,]  <- (1 - (smart_p_MI2CVD[,] + v_p_HDage)) * (1-smart_p_MI2S1[,]) 
  a_P_SoC["MI2", "S1", ,]  <- (1 - (smart_p_MI2CVD[,] + v_p_HDage)) * smart_p_MI2S1[,] 
  a_P_SoC["MI2", "CVD", ,]  <- smart_p_MI2CVD[,] 
  a_P_SoC["MI2", "D", ,]  <-  v_p_HDage 
  
  ## From S1
  a_P_SoC["S1", "S1", ,] <- (1 - (v_p_HDage + smart_p_S1CVD[,])) * (1-smart_p_S1S2[,]) 
  a_P_SoC["S1", "S2", ,] <- (1 - (v_p_HDage + smart_p_S1CVD[,])) * smart_p_S1S2[,]
  a_P_SoC["S1", "CVD", ,] <- smart_p_S1CVD[,]
  a_P_SoC["S1", "D", ,] <- v_p_HDage 
  
  ## From S2
  a_P_SoC["S2", "S2", ,] <- (1 - (v_p_HDage + smart_p_S2CVD[,])) 
  a_P_SoC["S2", "CVD", ,] <- smart_p_S2CVD[,]
  a_P_SoC["S2", "D", ,] <- v_p_HDage 
  
  ## From CVD
  a_P_SoC["CVD", "CVD", ,]   <- 1
  
  ## From D
  a_P_SoC["D", "D", ,]   <- 1
  
  ### Transition probability array for strategy: CVRM-Box ----
  #a_P_strA <- a_P_SoC
  a_P_strA <- array(0,
                    dim  = c(n_states, n_states, n_cycles, n_sim),
                    dimnames = list(v_names_states, 
                                    v_names_states, 
                                    0:(n_cycles - 1),
                                    1:n_sim))
  ## From R
  a_P_strA["R", "R", ,]   <- (1 - (v_p_HDage + box_score_p_CVD[,])) * (1 - (box_score_p_MI1[,] + box_score_p_S1[,]))
  a_P_strA["R", "MI1", ,]  <- (1 - (v_p_HDage + box_score_p_CVD[,])) * box_score_p_MI1[,] 
  a_P_strA["R", "S1", ,]  <- (1 - (v_p_HDage + box_score_p_CVD[,])) * box_score_p_S1[,] 
  a_P_strA["R", "CVD", ,] <- box_score_p_CVD[,] 
  a_P_strA["R", "D", ,]   <- v_p_HDage 
  ## From MI1
  a_P_strA["MI1", "MI1", ,]  <- (1 - (box_smart_p_MI1CVD[,] + v_p_HDage)) * (1- (box_smart_p_MI1S1[,] + box_smart_p_MI1MI2[,]))
  a_P_strA["MI1", "MI2", ,]  <- (1 - (box_smart_p_MI1CVD[,] + v_p_HDage)) * box_smart_p_MI1MI2[,]
  a_P_strA["MI1", "S1", ,]  <-  (1 - (box_smart_p_MI1CVD[,] + v_p_HDage)) * box_smart_p_MI1S1[,] 
  a_P_strA["MI1", "CVD", ,]  <- box_smart_p_MI1CVD[,] 
  a_P_strA["MI1", "D", ,]  <-  v_p_HDage 
  ## From MI2
  a_P_strA["MI2", "MI2", ,]  <- (1 - (box_smart_p_MI2CVD[,] + v_p_HDage)) * (1-box_smart_p_MI2S1[,]) 
  a_P_strA["MI2", "S1", ,]  <- (1 - (box_smart_p_MI2CVD[,] + v_p_HDage)) * box_smart_p_MI2S1[,] 
  a_P_strA["MI2", "CVD", ,]  <- box_smart_p_MI2CVD[,] 
  a_P_strA["MI2", "D", ,]  <-  v_p_HDage 
  
  ## From S1
  a_P_strA["S1", "S1", ,] <- (1 - (v_p_HDage + box_smart_p_S1CVD[,])) * (1-box_smart_p_S1S2[,]) 
  a_P_strA["S1", "S2", ,] <- (1 - (v_p_HDage + box_smart_p_S1CVD[,])) * box_smart_p_S1S2[,]
  a_P_strA["S1", "CVD", ,] <- box_smart_p_S1CVD[,]
  a_P_strA["S1", "D", ,] <- v_p_HDage 
  
  ## From S2
  a_P_strA["S2", "S2", ,] <- (1 - (v_p_HDage + box_smart_p_S2CVD[,])) 
  a_P_strA["S2", "CVD", ,] <- box_smart_p_S2CVD[,]
  a_P_strA["S2", "D", ,] <- v_p_HDage 
  
  ## From CVD
  a_P_strA["CVD", "CVD", ,]   <- 1
  
  ## From D
  a_P_strA["D", "D", ,]   <- 1
  
  ## Check if transition probability arrays are valid ----
  #* Functions included in "R/Functions.R". The latest version can be found in `darthtools` package
  ### Check that transition probabilities are [0, 1] ----
  for(sim in 1:n_sim) {
    #print(sim)
    check_transition_probability(a_P_SoC[,,,sim], verbose = TRUE)
    check_transition_probability(a_P_strA[,,,sim], verbose = TRUE)
    ## Check that all rows for each slice of the array sum to 1 
    sum_soc_tmp <- sum(rowSums(a_P_SoC[,,,sim]))
    if (sum_soc_tmp != n_states*n_cycles) {print("Invalid")}
    sum_a_tmp <- sum(rowSums(a_P_strA[,,,sim]))
    if (sum_a_tmp != n_states*n_cycles) {print("Invalid")}
  }
  
  ## Create transition dynamics arrays ----
  #* These arrays will capture transitions from each state to another over time 
  ### Initialize transition dynamics array for strategy SoC ----
  a_A_SoC <- array(0,
                   dim      = c(n_states, n_states, n_cycles + 1, n_sim),
                   dimnames = list(v_names_states, v_names_states, 0:n_cycles, 1:n_sim))
  a_A_strA  <- a_A_SoC #Structure and initial states are the same as for SoC
  
  
  #  Run Markov model ----
  #* Iterative solution of age-dependent cSTM
  for(i in 1:n_sim) {
    #* Set first slice of a_A_SoC and a_A_strA with the initial state vector in its diagonal
    diag(a_A_SoC[, , 1,i]) <- v_m_init
    diag(a_A_strA[, , 1,i]) <- v_m_init
    for(t in 1:n_cycles){
      ## Fill in cohort trace
      # For SoC
      m_M_SoC[t + 1, ,i]  <- m_M_SoC[t, ,i]  %*% a_P_SoC[, , t,i]
      # for strategy: CVRM-Box
      m_M_strA[t + 1, ,i] <- m_M_strA[t,,i ] %*% a_P_strA[, , t,i]
      
      ## Fill in transition-dynamics array
      # For SoC
      a_A_SoC[, , t + 1,i]  <- diag(m_M_SoC[t, ,i]) %*% a_P_SoC[, , t,i]
      # for strategy: CVRM-Box
      a_A_strA[, , t + 1,i] <- diag(m_M_strA[t, ,i]) %*% a_P_strA[, , t,i]
    }
  }
  ## Store the cohort traces in a list ----
  l_m_M <- list(SoC =  m_M_SoC,
                A   =  m_M_strA) 
  
  names(l_m_M) <- v_names_str
  
  ## Store the transition dynamics array for each strategy in a list ----
  l_a_A <- list(SoC =  a_A_SoC,
                A   =  a_A_strA) 
  names(l_a_A) <- v_names_str
  
  
  # State Rewards ----
  v_u_SoC    <- cbind(u_R_sgCN,  
                      u_R_sgCN - u_MI1,  
                      u_R_sgCN - u_S1, 
                      u_R_sgCN - u_MI1 - u_MI2, 
                      u_R_sgCN - u_S1 - u_S2, 
                      u_D,
                      u_CVD)
  colnames(v_u_SoC) <- v_names_states 
  
  #* Vector of state costs under strategy SoC
  v_c_SoC    <- cbind(c_SOC_controlled ,  
                      c_SOC_controlled +  c_MI1, 
                      c_SOC_controlled +  c_S1,  
                      c_SOC_controlled +  c_MI1 + c_MI2,  
                      c_SOC_controlled +  c_S1 + c_S2,  
                      c_D,
                      c_CVD)
  colnames(v_c_SoC) <- v_names_states 
  #* Vector of state utilities under strategy A
  v_u_strA   <- v_u_SoC
  
  #* Vector of state costs under strategy A
  v_c_strA   <- cbind(c_strA_controlled, 
                      c_MI1+ c_strA_controlled,
                      c_S1+ c_strA_controlled, 
                      c_MI2+ c_MI1+ c_strA_controlled, 
                      c_S2+ c_S1 + c_strA_controlled, 
                      c_D,
                      c_CVD) 
  colnames(v_c_strA) <- v_names_states 
  
  
  ## Store state rewards ----
  #* Store the vectors of state utilities for each strategy in a list 
  l_u <- list(SoC = v_u_SoC,
              A   = v_u_strA)
  #* Store the vectors of state cost for each strategy in a list 
  l_c <- list(SoC =  v_c_SoC,
              A   =  v_c_strA)
  
  #* assign strategy names to matching items in the lists
  names(l_u) <- names(l_c) <- v_names_str
  
  # Compute expected outcomes ----
  #* Create empty vectors to store total utilities and costs 
  v_tot_qaly <- v_tot_cost <- vector(mode = "numeric", length = n_str)
  names(v_tot_qaly) <- names(v_tot_cost) <- v_names_str
  
  
  
  total_costs_sims <- as.data.frame(matrix(NA, nrow=n_sim, ncol=n_str))
  colnames(total_costs_sims) <- v_names_str
  total_QALY_sims <- as.data.frame(matrix(NA, nrow=n_sim, ncol=n_str))
  colnames(total_costs_sims) <- v_names_str
  
  cycle_costs_strA <- as.data.frame(matrix(data=NA, nrow = n_sim, ncol=n_cycles+1))
  cycle_costs_SOC<- as.data.frame(matrix(data=NA, nrow = n_sim, ncol=n_cycles+1))
  
  ## Loop through each strategy and calculate total utilities and costs ----
  for (sim in 1:n_sim){
    for (i in 1:n_str) { # i <- 1
      v_u_str <- l_u[[i]][sim,]   # select the vector of state utilities for the i-th strategy
      v_c_str <- l_c[[i]][sim,]   # select the vector of state costs for the i-th strategy
      a_A_str <- l_a_A[[i]][,,,sim] # select the transition array for the i-th strategy, simulation
      ##* Array of state rewards 
      #* Create transition matrices of state utilities and state costs for the i-th strategy 
      m_u_str   <- matrix(v_u_str, nrow = n_states, ncol = n_states, byrow = T)
      m_c_str   <- matrix(v_c_str, nrow = n_states, ncol = n_states, byrow = T)
      #* Expand the transition matrix of state utilities across cycles to form a transition array of state utilities
      a_R_u_str <- array(m_u_str, 
                         dim      = c(n_states, n_states, n_cycles + 1),
                         dimnames = list(v_names_states, v_names_states, 0:n_cycles))
      # Expand the transition matrix of state costs across cycles to form a transition array of state costs
      a_R_c_str <- array(m_c_str, 
                         dim      = c(n_states, n_states, n_cycles + 1),
                         dimnames = list(v_names_states, v_names_states, 0:n_cycles))
      

      ##* Apply transition rewards
      #* Apply disutility due to transition from H to S1
      a_R_u_str["R", "MI1", ]  <- 0 #
      a_R_u_str["MI1", "MI2", ] <-  a_R_u_str["MI1", "MI2", ]  - du_MI2[sim] #
      a_R_u_str["MI2", "S1", ]  <- a_R_u_str["MI2", "S1", ]  - du_MItoS[sim] #
      a_R_u_str["R", "S1", ]  <- 0 #
      a_R_u_str["MI1", "S1", ]  <- a_R_u_str["MI1", "S1", ]  - du_MItoS[sim] #
      a_R_u_str["S1", "S2", ] <-  a_R_u_str["S1", "S2", ]  -  du_S2[sim] #
      
      
      #* Add transition cost per cycle due to transition from H to S1
      a_R_c_str["R", "MI1", ]      <- 0 #
      a_R_c_str["MI1", "MI2", ]      <- a_R_c_str["MI1", "MI2", ]    +    ic_MI2[sim] # #
      a_R_c_str["MI1", "S1", ]      <- a_R_c_str["MI1", "S1", ]       + ic_S1[sim]  #
      a_R_c_str["R", "S1", ]      <- 0 #
      a_R_c_str["S1", "S2", ]      <- a_R_c_str["S1", "S2", ]      +  ic_S2[sim] 
      a_R_c_str["MI2", "CVD", ]      <- a_R_c_str["MI2", "CVD", ]       + ic_CVD[sim]
      a_R_c_str["MI2", "S1", ]      <- a_R_c_str["MI2", "S1", ]       + ic_S2[sim]
      a_R_c_str["S2", "CVD", ]      <- a_R_c_str["S2", "CVD", ]       + ic_CVD[sim]
      a_R_c_str["R", "CVD", ]      <- 0
      
      
      
      
      ###* Expected QALYs and costs for all transitions per cycle
      #* QALYs = life years x QoL
      
      a_Y_c_str <- a_A_str * a_R_c_str
      a_Y_u_str <- a_A_str * a_R_u_str 
      
      ###* Expected QALYs and costs per cycle
      ##* Vector of QALYs and costs
      v_qaly_str <- apply(a_Y_u_str, 3, sum) # sum the proportion of the cohort across transitions 
      v_cost_str <- apply(a_Y_c_str, 3, sum) # sum the proportion of the cohort across transitions
      
      
      #* QALYs
      v_tot_qaly[i] <- t(v_qaly_str) %*% (v_dwe * v_wcc)
      #* Costs
      v_tot_cost[i] <- t(v_cost_str) %*% (v_dwc * v_wcc)
      
      ## Vector with discounted values
      if(i==1){
        total_QALY_sims[sim,i] <- v_tot_qaly[i]
        total_costs_sims[sim,i] <- v_tot_cost[i]
        cycle_costs_SOC[sim,] <- v_cost_str
      }
      if(i==2){
        total_QALY_sims[sim,i] <- v_tot_qaly[i]
        total_costs_sims[sim,i] <- v_tot_cost[i] + c_init_strA #
        cycle_costs_strA[sim,] <- v_cost_str
      }
    }
  }
  
  #BUDGET IMPACT, regional
  #SOC BI
  df_BI_reg_SOC <- cycle_costs_SOC[,1:horizon_BI]
  
  df_BI_reg_SOC$first_run <- df_BI_reg_SOC$V1+
    df_BI_reg_SOC$V2+
    df_BI_reg_SOC$V3+
    df_BI_reg_SOC$V4+
    df_BI_reg_SOC$V5
  df_BI_reg_SOC$first_run <- df_BI_reg_SOC$first_run*BI_reg_n_yr1  
  
  df_BI_reg_SOC$second_run <- df_BI_reg_SOC$V1+
    df_BI_reg_SOC$V2+
    df_BI_reg_SOC$V3+
    df_BI_reg_SOC$V4
  df_BI_reg_SOC$second_run <- df_BI_reg_SOC$second_run*BI_reg_n_yr2  
  
  df_BI_reg_SOC$third_run <- df_BI_reg_SOC$V1+
    df_BI_reg_SOC$V2+
    df_BI_reg_SOC$V3
  df_BI_reg_SOC$third_run <- df_BI_reg_SOC$third_run*BI_reg_n_yr3  
  
  df_BI_reg_SOC$fourth_run <- df_BI_reg_SOC$V1+
    df_BI_reg_SOC$V2
  df_BI_reg_SOC$fourth_run <- df_BI_reg_SOC$fourth_run*BI_reg_n_yr4  
  
  df_BI_reg_SOC$fifth_run <- df_BI_reg_SOC$V1
  df_BI_reg_SOC$fifth_run <- df_BI_reg_SOC$fifth_run*BI_reg_n_yr5  
  
  df_BI_reg_SOC$total_BI <- df_BI_reg_SOC$first_run + df_BI_reg_SOC$second_run + df_BI_reg_SOC$third_run + df_BI_reg_SOC$fourth_run + df_BI_reg_SOC$fifth_run
  
  
  #intervention BI
  df_BI_reg_strA <- cycle_costs_strA[,1:horizon_BI]
  
  df_BI_reg_strA$first_run <- df_BI_reg_strA$V1+
    df_BI_reg_strA$V2+
    df_BI_reg_strA$V3+
    df_BI_reg_strA$V4+
    df_BI_reg_strA$V5
  df_BI_reg_strA$first_run <- df_BI_reg_strA$first_run*BI_reg_n_yr1 + c_init_strA*BI_reg_n_yr1  
  
  df_BI_reg_strA$second_run <- df_BI_reg_strA$V1+
    df_BI_reg_strA$V2+
    df_BI_reg_strA$V3+
    df_BI_reg_strA$V4
  df_BI_reg_strA$second_run <- df_BI_reg_strA$second_run*BI_reg_n_yr2  + c_init_strA*BI_reg_n_yr2
  
  df_BI_reg_strA$third_run <- df_BI_reg_strA$V1+
    df_BI_reg_strA$V2+
    df_BI_reg_strA$V3
  df_BI_reg_strA$third_run <- df_BI_reg_strA$third_run*BI_reg_n_yr3  + c_init_strA*BI_reg_n_yr3
  
  df_BI_reg_strA$fourth_run <- df_BI_reg_strA$V1+
    df_BI_reg_strA$V2
  df_BI_reg_strA$fourth_run <- df_BI_reg_strA$fourth_run*BI_reg_n_yr4 + c_init_strA*BI_reg_n_yr4
  
  df_BI_reg_strA$fifth_run <- df_BI_reg_strA$V1
  df_BI_reg_strA$fifth_run <- df_BI_reg_strA$fifth_run*BI_reg_n_yr5 + c_init_strA*BI_reg_n_yr5
  
  df_BI_reg_strA$total_BI <- df_BI_reg_strA$first_run + df_BI_reg_strA$second_run + df_BI_reg_strA$third_run + df_BI_reg_strA$fourth_run + df_BI_reg_strA$fifth_run  
  
  
  #BUDGET IMPACT, national
  #SOC BI
  df_BI_nat_SOC <- cycle_costs_SOC[,1:horizon_BI]
  
  df_BI_nat_SOC$first_run <- df_BI_nat_SOC$V1+
    df_BI_nat_SOC$V2+
    df_BI_nat_SOC$V3+
    df_BI_nat_SOC$V4+
    df_BI_nat_SOC$V5
  df_BI_nat_SOC$first_run <- df_BI_nat_SOC$first_run*BI_nat_n_yr1  
  
  df_BI_nat_SOC$second_run <- df_BI_nat_SOC$V1+
    df_BI_nat_SOC$V2+
    df_BI_nat_SOC$V3+
    df_BI_nat_SOC$V4
  df_BI_nat_SOC$second_run <- df_BI_nat_SOC$second_run*BI_nat_n_yr2  
  
  df_BI_nat_SOC$third_run <- df_BI_nat_SOC$V1+
    df_BI_nat_SOC$V2+
    df_BI_nat_SOC$V3
  df_BI_nat_SOC$third_run <- df_BI_nat_SOC$third_run*BI_nat_n_yr3  
  
  df_BI_nat_SOC$fourth_run <- df_BI_nat_SOC$V1+
    df_BI_nat_SOC$V2
  df_BI_nat_SOC$fourth_run <- df_BI_nat_SOC$fourth_run*BI_nat_n_yr4  
  
  df_BI_nat_SOC$fifth_run <- df_BI_nat_SOC$V1
  df_BI_nat_SOC$fifth_run <- df_BI_nat_SOC$fifth_run*BI_nat_n_yr5  
  
  df_BI_nat_SOC$total_BI <- df_BI_nat_SOC$first_run + df_BI_nat_SOC$second_run + df_BI_nat_SOC$third_run + df_BI_nat_SOC$fourth_run + df_BI_nat_SOC$fifth_run
  
  
  #intervention BI
  df_BI_nat_strA <- cycle_costs_strA[,1:horizon_BI]
  
  df_BI_nat_strA$first_run <- df_BI_nat_strA$V1+
    df_BI_nat_strA$V2+
    df_BI_nat_strA$V3+
    df_BI_nat_strA$V4+
    df_BI_nat_strA$V5
  df_BI_nat_strA$first_run <- df_BI_nat_strA$first_run*BI_nat_n_yr1 + c_init_strA*BI_nat_n_yr1  
  
  df_BI_nat_strA$second_run <- df_BI_nat_strA$V1+
    df_BI_nat_strA$V2+
    df_BI_nat_strA$V3+
    df_BI_nat_strA$V4
  df_BI_nat_strA$second_run <- df_BI_nat_strA$second_run*BI_nat_n_yr2  + c_init_strA*BI_nat_n_yr2
  
  df_BI_nat_strA$third_run <- df_BI_nat_strA$V1+
    df_BI_nat_strA$V2+
    df_BI_nat_strA$V3
  df_BI_nat_strA$third_run <- df_BI_nat_strA$third_run*BI_nat_n_yr3  + c_init_strA*BI_nat_n_yr3
  
  df_BI_nat_strA$fourth_run <- df_BI_nat_strA$V1+
    df_BI_nat_strA$V2
  df_BI_nat_strA$fourth_run <- df_BI_nat_strA$fourth_run*BI_nat_n_yr4 + c_init_strA*BI_nat_n_yr4
  
  df_BI_nat_strA$fifth_run <- df_BI_nat_strA$V1
  df_BI_nat_strA$fifth_run <- df_BI_nat_strA$fifth_run*BI_nat_n_yr5 + c_init_strA*BI_nat_n_yr5
  
  df_BI_nat_strA$total_BI <- df_BI_nat_strA$first_run + df_BI_nat_strA$second_run + df_BI_nat_strA$third_run + df_BI_nat_strA$fourth_run + df_BI_nat_strA$fifth_run  
  
  
  return(list(df_BI_reg_SOC = df_BI_reg_SOC,
              df_BI_reg_strA = df_BI_reg_strA,
              df_BI_nat_SOC = df_BI_nat_SOC,
              df_BI_nat_strA = df_BI_nat_strA))
}


#### 13. BI Model for subgroup UY ####

#For subgroup uncontrolled BP, with a CVD history

run_BI_model_uncontrolled_yeshis <- function() {
  
  
  ### Risk prediction model inputs (SOC) ----
  
  #The effect of the intervention on the transitions is due to the difference on systolic blood pressure 'sbp_diff'
  
  
  ### Transition rates (annual) ----
  #SCORE2 and SCORE2-OP
  score_tprobs <- gen_score_tprobs(intervention = "SOC",
                                   subgroup = "uncontrolled_nohis") #this will not be used in this run because people start in MI or Stroke
  
  score_p_MI1 <- as.matrix(score_tprobs$score_MI1)
  score_p_S1 <- as.matrix(score_tprobs$score_S1)
  score_p_CVD <- as.matrix(score_tprobs$score_CVD)
  
  
  box_score_tprobs <- gen_score_tprobs(intervention = "Box",
                                       subgroup = "uncontrolled_nohis") #this will not be used in this run because people start in MI or Stroke
  
  box_score_p_MI1 <- as.matrix(box_score_tprobs$score_MI1)
  box_score_p_S1 <- as.matrix(box_score_tprobs$score_S1)
  box_score_p_CVD <- as.matrix(box_score_tprobs$score_CVD)
  
  
  #SMART2
  smart_tprobs_fromMI <- gen_smart_tprobs(from_state = "MI",
                                          intervention = "SOC",
                                          subgroup = "uncontrolled_yeshis")
  
  smart_p_MI1MI2 <- as.matrix(smart_tprobs_fromMI$smart_MI)
  smart_p_MI1S1 <- as.matrix(smart_tprobs_fromMI$smart_S)
  smart_p_MI2S1 <- as.matrix(smart_tprobs_fromMI$smart_S) # 
  smart_p_MI1CVD <- as.matrix(smart_tprobs_fromMI$smart_CVD)
  smart_p_MI2CVD <- as.matrix(smart_tprobs_fromMI$smart_CVD) # 
  
  
  smart_tprobs_fromS <- gen_smart_tprobs(from_state = "S",
                                         intervention = "SOC",
                                         subgroup = "uncontrolled_yeshis")
  
  smart_p_S1S2 <- as.matrix(smart_tprobs_fromS$smart_S)
  smart_p_S1CVD <- as.matrix(smart_tprobs_fromS$smart_CVD)
  smart_p_S2CVD <- as.matrix(smart_tprobs_fromS$smart_CVD) #
  
  
  
  box_smart_tprobs_fromMI <- gen_smart_tprobs(from_state = "MI",
                                              intervention = "Box",
                                              subgroup = "uncontrolled_yeshis")
  
  box_smart_p_MI1MI2 <- as.matrix(box_smart_tprobs_fromMI$smart_MI)
  box_smart_p_MI1S1 <- as.matrix(box_smart_tprobs_fromMI$smart_S)
  box_smart_p_MI2S1 <- as.matrix(box_smart_tprobs_fromMI$smart_S) # 
  box_smart_p_MI1CVD <- as.matrix(box_smart_tprobs_fromMI$smart_CVD)
  box_smart_p_MI2CVD <- as.matrix(box_smart_tprobs_fromMI$smart_CVD) # 
  
  
  box_smart_tprobs_fromS <- gen_smart_tprobs(from_state = "S",
                                             intervention = "Box",
                                             subgroup = "uncontrolled_yeshis")
  
  box_smart_p_S1S2 <- as.matrix(box_smart_tprobs_fromS$smart_S)
  box_smart_p_S1CVD <- as.matrix(box_smart_tprobs_fromS$smart_CVD)
  box_smart_p_S2CVD <- as.matrix(box_smart_tprobs_fromS$smart_CVD) #
  
  ## Age-dependent mortality rates ----
  lt_NL_2021 <- nl_mort()   
  #* Extract age-specific all-cause mortality for ages in model time horizon
  v_r_mort_by_age <- lt_NL_2021 %>% 
    dplyr::filter(Age >= n_age_init & Age < n_age_max) %>%
    dplyr::select(Female, Male) 
  v_r_mort_by_age$Total <- v_r_mort_by_age$Female*(1-prop_male)+v_r_mort_by_age$Male*prop_male
  v_r_mort_by_age <- as.matrix(v_r_mort_by_age$Total)
  
  #Also extract age-specific scaling values for all-cause mortality to account for CVD deaths
  df_mort_p_scaling <- mort_p_scaling()
  v_p_mort_scale <- df_mort_p_scaling %>% 
    dplyr::filter(Age >= n_age_init & Age < n_age_max) %>%
    dplyr::select(Female, Male) 
  v_p_mort_scale$Total <- v_p_mort_scale$Female*(1-prop_male)+v_p_mort_scale$Male*prop_male
  v_p_mort_scale <- as.numeric(v_p_mort_scale$Total)
  
  ### Discount weight for costs and effects ----
  v_dwc  <- 1 / ((1 + (d_c * cycle_length)) ^ (0:n_cycles))
  v_dwe  <- 1 / ((1 + (d_e * cycle_length)) ^ (0:n_cycles))
  
  # Process model inputs ----
  ## Age-specific transition rates to the Dead state for all cycles ----
  v_r_HDage  <- rep(v_r_mort_by_age, each = 1/cycle_length)
  #* Name age-specific mortality vector 
  names(v_r_HDage) <- v_age_names
  
  #* Function included in "R/Functions.R". The latest version can be found in `darthtools` package
  v_p_HDage  <- rate_to_prob(v_r_HDage, t = cycle_length)  # Age-specific mortality risk in the Healthy state 
  v_p_HDage <- v_p_HDage*(1-v_p_mort_scale) 
  
  # Construct state-transition models ----
  ## Initial state vector ----
  #* All starting healthy
  v_m_init <- c(R = 0, MI1=0.64, MI2=0, S1 = 0.36, S2 = 0, CVD=0 , D = 0) 
  ## Initialize cohort traces ----
  ### Initialize cohort trace under SoC ----
  m_M_SoC <- array(NA, 
                   dim = c((n_cycles + 1), n_states, n_sim), 
                   dimnames = list(0:n_cycles, v_names_states, 1:n_sim))
  #* Store the initial state vector in the first row of the cohort trace
  m_M_SoC[1, , ] <- v_m_init
  
  ### Initialize cohort trace for competing strategies ----
  #* Structure and initial states are the same as for SoC
  m_M_strA  <- m_M_SoC # Strategy A
  
  
  
  ## Create transition probability arrays for strategy SoC ----
  ### Initialize transition probability array for strategy SoC ----
  #* All transitions to a non-death state are assumed to be conditional on survival
  a_P_SoC <- array(0,
                   dim  = c(n_states, n_states, n_cycles, n_sim),
                   dimnames = list(v_names_states, 
                                   v_names_states, 
                                   0:(n_cycles - 1),
                                   1:n_sim))
  
  # ### Fill in array
  ## From R
  a_P_SoC["R", "R", ,]   <- (1 - (v_p_HDage + score_p_CVD[,])) * (1 - (score_p_MI1[,] + score_p_S1[,]))
  a_P_SoC["R", "MI1", , ]  <- (1 - (v_p_HDage + score_p_CVD[,])) * score_p_MI1[,] 
  a_P_SoC["R", "S1", ,]  <- (1 - (v_p_HDage + score_p_CVD[,])) * score_p_S1[,] 
  a_P_SoC["R", "CVD", ,] <- score_p_CVD[,] 
  a_P_SoC["R", "D", ,]   <- v_p_HDage 
  ## From MI1
  a_P_SoC["MI1", "MI1", ,]  <- (1 - (smart_p_MI1CVD[,] + v_p_HDage)) * (1- (smart_p_MI1S1[,] + smart_p_MI1MI2[,]))
  a_P_SoC["MI1", "MI2", ,]  <- (1 - (smart_p_MI1CVD[,] + v_p_HDage)) * smart_p_MI1MI2[,]
  a_P_SoC["MI1", "S1", ,]  <-  (1 - (smart_p_MI1CVD[,] + v_p_HDage)) * smart_p_MI1S1[,] 
  a_P_SoC["MI1", "CVD", ,]  <- smart_p_MI1CVD[,] 
  a_P_SoC["MI1", "D", ,]  <-  v_p_HDage 
  ## From MI2
  a_P_SoC["MI2", "MI2", ,]  <- (1 - (smart_p_MI2CVD[,] + v_p_HDage)) * (1-smart_p_MI2S1[,]) 
  a_P_SoC["MI2", "S1", ,]  <- (1 - (smart_p_MI2CVD[,] + v_p_HDage)) * smart_p_MI2S1[,] 
  a_P_SoC["MI2", "CVD", ,]  <- smart_p_MI2CVD[,] 
  a_P_SoC["MI2", "D", ,]  <-  v_p_HDage 
  
  ## From S1
  a_P_SoC["S1", "S1", ,] <- (1 - (v_p_HDage + smart_p_S1CVD[,])) * (1-smart_p_S1S2[,]) 
  a_P_SoC["S1", "S2", ,] <- (1 - (v_p_HDage + smart_p_S1CVD[,])) * smart_p_S1S2[,]
  a_P_SoC["S1", "CVD", ,] <- smart_p_S1CVD[,]
  a_P_SoC["S1", "D", ,] <- v_p_HDage 
  
  ## From S2
  a_P_SoC["S2", "S2", ,] <- (1 - (v_p_HDage + smart_p_S2CVD[,])) 
  a_P_SoC["S2", "CVD", ,] <- smart_p_S2CVD[,]
  a_P_SoC["S2", "D", ,] <- v_p_HDage 
  
  ## From CVD
  a_P_SoC["CVD", "CVD", ,]   <- 1
  
  ## From D
  a_P_SoC["D", "D", ,]   <- 1
  
  ### Transition probability array for strategy: CVRM-Box ----
  #a_P_strA <- a_P_SoC
  a_P_strA <- array(0,
                    dim  = c(n_states, n_states, n_cycles, n_sim),
                    dimnames = list(v_names_states, 
                                    v_names_states, 
                                    0:(n_cycles - 1),
                                    1:n_sim))
  ## From R
  a_P_strA["R", "R", ,]   <- (1 - (v_p_HDage + box_score_p_CVD[,])) * (1 - (box_score_p_MI1[,] + box_score_p_S1[,]))
  a_P_strA["R", "MI1", ,]  <- (1 - (v_p_HDage + box_score_p_CVD[,])) * box_score_p_MI1[,] 
  a_P_strA["R", "S1", ,]  <- (1 - (v_p_HDage + box_score_p_CVD[,])) * box_score_p_S1[,] 
  a_P_strA["R", "CVD", ,] <- box_score_p_CVD[,] 
  a_P_strA["R", "D", ,]   <- v_p_HDage 
  ## From MI1
  a_P_strA["MI1", "MI1", ,]  <- (1 - (box_smart_p_MI1CVD[,] + v_p_HDage)) * (1- (box_smart_p_MI1S1[,] + box_smart_p_MI1MI2[,]))
  a_P_strA["MI1", "MI2", ,]  <- (1 - (box_smart_p_MI1CVD[,] + v_p_HDage)) * box_smart_p_MI1MI2[,]
  a_P_strA["MI1", "S1", ,]  <-  (1 - (box_smart_p_MI1CVD[,] + v_p_HDage)) * box_smart_p_MI1S1[,] 
  a_P_strA["MI1", "CVD", ,]  <- box_smart_p_MI1CVD[,] 
  a_P_strA["MI1", "D", ,]  <-  v_p_HDage 
  ## From MI2
  a_P_strA["MI2", "MI2", ,]  <- (1 - (box_smart_p_MI2CVD[,] + v_p_HDage)) * (1-box_smart_p_MI2S1[,]) 
  a_P_strA["MI2", "S1", ,]  <- (1 - (box_smart_p_MI2CVD[,] + v_p_HDage)) * box_smart_p_MI2S1[,] 
  a_P_strA["MI2", "CVD", ,]  <- box_smart_p_MI2CVD[,] 
  a_P_strA["MI2", "D", ,]  <-  v_p_HDage 
  
  ## From S1
  a_P_strA["S1", "S1", ,] <- (1 - (v_p_HDage + box_smart_p_S1CVD[,])) * (1-box_smart_p_S1S2[,]) 
  a_P_strA["S1", "S2", ,] <- (1 - (v_p_HDage + box_smart_p_S1CVD[,])) * box_smart_p_S1S2[,]
  a_P_strA["S1", "CVD", ,] <- box_smart_p_S1CVD[,]
  a_P_strA["S1", "D", ,] <- v_p_HDage 
  
  ## From S2
  a_P_strA["S2", "S2", ,] <- (1 - (v_p_HDage + box_smart_p_S2CVD[,])) 
  a_P_strA["S2", "CVD", ,] <- box_smart_p_S2CVD[,]
  a_P_strA["S2", "D", ,] <- v_p_HDage 
  
  ## From CVD
  a_P_strA["CVD", "CVD", ,]   <- 1
  
  ## From D
  a_P_strA["D", "D", ,]   <- 1
  
  ## Check if transition probability arrays are valid ----
  #* Functions included in "R/Functions.R". The latest version can be found in `darthtools` package
  ### Check that transition probabilities are [0, 1] ----
  for(sim in 1:n_sim) {
    #print(sim)
    check_transition_probability(a_P_SoC[,,,sim], verbose = TRUE)
    check_transition_probability(a_P_strA[,,,sim], verbose = TRUE)
    ## Check that all rows for each slice of the array sum to 1
    sum_soc_tmp <- sum(rowSums(a_P_SoC[,,,sim]))
    if (sum_soc_tmp != n_states*n_cycles) {print("Invalid")}
    sum_a_tmp <- sum(rowSums(a_P_strA[,,,sim]))
    if (sum_a_tmp != n_states*n_cycles) {print("Invalid")}
  }
  
  ## Create transition dynamics arrays ----
  #* These arrays will capture transitions from each state to another over time 
  ### Initialize transition dynamics array for strategy SoC ----
  a_A_SoC <- array(0,
                   dim      = c(n_states, n_states, n_cycles + 1, n_sim),
                   dimnames = list(v_names_states, v_names_states, 0:n_cycles, 1:n_sim))
  a_A_strA  <- a_A_SoC #Structure and initial states are the same as for SoC
  
  
  #  Run Markov model ----
  #* Iterative solution of age-dependent cSTM
  for(i in 1:n_sim) {
    #* Set first slice of a_A_SoC and a_A_strA with the initial state vector in its diagonal
    diag(a_A_SoC[, , 1,i]) <- v_m_init
    diag(a_A_strA[, , 1,i]) <- v_m_init
    for(t in 1:n_cycles){
      ## Fill in cohort trace
      # For SoC
      m_M_SoC[t + 1, ,i]  <- m_M_SoC[t, ,i]  %*% a_P_SoC[, , t,i]
      # for strategy: CVRM-Box
      m_M_strA[t + 1, ,i] <- m_M_strA[t,,i ] %*% a_P_strA[, , t,i]
      
      ## Fill in transition-dynamics array
      # For SoC
      a_A_SoC[, , t + 1,i]  <- diag(m_M_SoC[t, ,i]) %*% a_P_SoC[, , t,i]
      # for strategy: CVRM-Box
      a_A_strA[, , t + 1,i] <- diag(m_M_strA[t, ,i]) %*% a_P_strA[, , t,i]
    }
  }
  ## Store the cohort traces in a list ----
  l_m_M <- list(SoC =  m_M_SoC,
                A   =  m_M_strA) 
  
  names(l_m_M) <- v_names_str
  
  ## Store the transition dynamics array for each strategy in a list ----
  l_a_A <- list(SoC =  a_A_SoC,
                A   =  a_A_strA) 
  names(l_a_A) <- v_names_str
  
  
  # State Rewards ----
  v_u_SoC    <- cbind(u_R_sgCN - u_hypertension,  
                      u_R_sgCN - u_hypertension - u_MI1,  
                      u_R_sgCN - u_hypertension - u_S1, 
                      u_R_sgCN - u_hypertension - u_MI1 - u_MI2, 
                      u_R_sgCN - u_hypertension - u_S1 - u_S2, 
                      u_D,
                      u_CVD)
  colnames(v_u_SoC) <- v_names_states 
  
  #* Vector of state costs under strategy SoC
  v_c_SoC    <- cbind(c_SOC_uncontrolled ,  
                      c_SOC_uncontrolled  + c_MI1, 
                      c_SOC_uncontrolled  + c_S1,  
                      c_SOC_uncontrolled  + c_MI1 + c_MI2,  
                      c_SOC_uncontrolled  + c_S1 + c_S2,  
                      c_D,
                      c_CVD)
  colnames(v_c_SoC) <- v_names_states 
  #* Vector of state utilities under strategy A
  v_u_strA   <- v_u_SoC
  
  #* Vector of state costs under strategy A
  v_c_strA   <- cbind(c_strA_uncontrolled, 
                      c_MI1+ c_strA_uncontrolled,
                      c_S1+ c_strA_uncontrolled, 
                      c_MI2+ c_MI1+ c_strA_uncontrolled, 
                      c_S2+ c_S1 + c_strA_uncontrolled, 
                      c_D,
                      c_CVD) 
  colnames(v_c_strA) <- v_names_states 
  
  
  ## Store state rewards ----
  #* Store the vectors of state utilities for each strategy in a list 
  l_u <- list(SoC = v_u_SoC,
              A   = v_u_strA)
  #* Store the vectors of state cost for each strategy in a list 
  l_c <- list(SoC =  v_c_SoC,
              A   =  v_c_strA)
  
  #* assign strategy names to matching items in the lists
  names(l_u) <- names(l_c) <- v_names_str
  
  # Compute expected outcomes ----
  #* Create empty vectors to store total utilities and costs 
  v_tot_qaly <- v_tot_cost <- vector(mode = "numeric", length = n_str)
  names(v_tot_qaly) <- names(v_tot_cost) <- v_names_str
  
  
  
  total_costs_sims <- as.data.frame(matrix(NA, nrow=n_sim, ncol=n_str))
  colnames(total_costs_sims) <- v_names_str
  total_QALY_sims <- as.data.frame(matrix(NA, nrow=n_sim, ncol=n_str))
  colnames(total_costs_sims) <- v_names_str
  
  cycle_costs_strA <- as.data.frame(matrix(data=NA, nrow = n_sim, ncol=n_cycles+1))
  cycle_costs_SOC<- as.data.frame(matrix(data=NA, nrow = n_sim, ncol=n_cycles+1))
  
  ## Loop through each strategy and calculate total utilities and costs ----
  for (sim in 1:n_sim){
    for (i in 1:n_str) { # i <- 1
      v_u_str <- l_u[[i]][sim,]   # select the vector of state utilities for the i-th strategy
      v_c_str <- l_c[[i]][sim,]   # select the vector of state costs for the i-th strategy
      a_A_str <- l_a_A[[i]][,,,sim] # select the transition array for the i-th strategy, simulation
      ##* Array of state rewards 
      #* Create transition matrices of state utilities and state costs for the i-th strategy 
      m_u_str   <- matrix(v_u_str, nrow = n_states, ncol = n_states, byrow = T)
      m_c_str   <- matrix(v_c_str, nrow = n_states, ncol = n_states, byrow = T)
      #* Expand the transition matrix of state utilities across cycles to form a transition array of state utilities
      a_R_u_str <- array(m_u_str, 
                         dim      = c(n_states, n_states, n_cycles + 1),
                         dimnames = list(v_names_states, v_names_states, 0:n_cycles))
      # Expand the transition matrix of state costs across cycles to form a transition array of state costs
      a_R_c_str <- array(m_c_str, 
                         dim      = c(n_states, n_states, n_cycles + 1),
                         dimnames = list(v_names_states, v_names_states, 0:n_cycles))

      ##* Apply transition rewards
      #* Apply disutility due to transition from H to S1
      a_R_u_str["R", "MI1", ]  <- 0 #
      a_R_u_str["MI1", "MI2", ] <-  a_R_u_str["MI1", "MI2", ]  - du_MI2[sim] #
      a_R_u_str["MI2", "S1", ]  <- a_R_u_str["MI2", "S1", ]  - du_MItoS[sim] #
      a_R_u_str["R", "S1", ]  <- 0 #
      a_R_u_str["MI1", "S1", ]  <- a_R_u_str["MI1", "S1", ]  - du_MItoS[sim] #
      a_R_u_str["S1", "S2", ] <-  a_R_u_str["S1", "S2", ]  -  du_S2[sim] #
      
      
      #* Add transition cost per cycle due to transition from H to S1
      a_R_c_str["R", "MI1", ]      <- 0 #
      a_R_c_str["MI1", "MI2", ]      <- a_R_c_str["MI1", "MI2", ]    +    ic_MI2[sim] # #
      a_R_c_str["MI1", "S1", ]      <- a_R_c_str["MI1", "S1", ]       + ic_S1[sim]  #
      a_R_c_str["R", "S1", ]      <- 0 #
      a_R_c_str["S1", "S2", ]      <- a_R_c_str["S1", "S2", ]      +  ic_S2[sim] 
      a_R_c_str["MI2", "CVD", ]      <- a_R_c_str["MI2", "CVD", ]       + ic_CVD[sim]
      a_R_c_str["MI2", "S1", ]      <- a_R_c_str["MI2", "S1", ]       + ic_S2[sim]
      a_R_c_str["S2", "CVD", ]      <- a_R_c_str["S2", "CVD", ]       + ic_CVD[sim]
      a_R_c_str["R", "CVD", ]      <- 0
      
      
      
      
      
      ###* Expected QALYs and costs for all transitions per cycle
      #* QALYs = life years x QoL
      
      a_Y_c_str <- a_A_str * a_R_c_str
      a_Y_u_str <- a_A_str * a_R_u_str 
      
      ###* Expected QALYs and costs per cycle
      ##* Vector of QALYs and costs
      v_qaly_str <- apply(a_Y_u_str, 3, sum) # sum the proportion of the cohort across transitions 
      v_cost_str <- apply(a_Y_c_str, 3, sum) # sum the proportion of the cohort across transitions
      
      
      #* QALYs
      v_tot_qaly[i] <- t(v_qaly_str) %*% (v_dwe * v_wcc)
      #* Costs
      v_tot_cost[i] <- t(v_cost_str) %*% (v_dwc * v_wcc)
      
      ## Vector with discounted values
      if(i==1){
        total_QALY_sims[sim,i] <- v_tot_qaly[i]
        total_costs_sims[sim,i] <- v_tot_cost[i]
        cycle_costs_SOC[sim,] <- v_cost_str
      }
      if(i==2){
        total_QALY_sims[sim,i] <- v_tot_qaly[i]
        total_costs_sims[sim,i] <- v_tot_cost[i] + c_init_strA #
        cycle_costs_strA[sim,] <- v_cost_str
      }
    }
  }
  
  
  #BUDGET IMPACT, regional
  #SOC BI
  df_BI_reg_SOC <- cycle_costs_SOC[,1:horizon_BI]
  
  df_BI_reg_SOC$first_run <- df_BI_reg_SOC$V1+
    df_BI_reg_SOC$V2+
    df_BI_reg_SOC$V3+
    df_BI_reg_SOC$V4+
    df_BI_reg_SOC$V5
  df_BI_reg_SOC$first_run <- df_BI_reg_SOC$first_run*BI_reg_n_yr1  
  
  df_BI_reg_SOC$second_run <- df_BI_reg_SOC$V1+
    df_BI_reg_SOC$V2+
    df_BI_reg_SOC$V3+
    df_BI_reg_SOC$V4
  df_BI_reg_SOC$second_run <- df_BI_reg_SOC$second_run*BI_reg_n_yr2  
  
  df_BI_reg_SOC$third_run <- df_BI_reg_SOC$V1+
    df_BI_reg_SOC$V2+
    df_BI_reg_SOC$V3
  df_BI_reg_SOC$third_run <- df_BI_reg_SOC$third_run*BI_reg_n_yr3  
  
  df_BI_reg_SOC$fourth_run <- df_BI_reg_SOC$V1+
    df_BI_reg_SOC$V2
  df_BI_reg_SOC$fourth_run <- df_BI_reg_SOC$fourth_run*BI_reg_n_yr4  
  
  df_BI_reg_SOC$fifth_run <- df_BI_reg_SOC$V1
  df_BI_reg_SOC$fifth_run <- df_BI_reg_SOC$fifth_run*BI_reg_n_yr5  
  
  df_BI_reg_SOC$total_BI <- df_BI_reg_SOC$first_run + df_BI_reg_SOC$second_run + df_BI_reg_SOC$third_run + df_BI_reg_SOC$fourth_run + df_BI_reg_SOC$fifth_run
  
  
  #intervention BI
  df_BI_reg_strA <- cycle_costs_strA[,1:horizon_BI]
  
  df_BI_reg_strA$first_run <- df_BI_reg_strA$V1+
    df_BI_reg_strA$V2+
    df_BI_reg_strA$V3+
    df_BI_reg_strA$V4+
    df_BI_reg_strA$V5
  df_BI_reg_strA$first_run <- df_BI_reg_strA$first_run*BI_reg_n_yr1 + c_init_strA*BI_reg_n_yr1  
  
  df_BI_reg_strA$second_run <- df_BI_reg_strA$V1+
    df_BI_reg_strA$V2+
    df_BI_reg_strA$V3+
    df_BI_reg_strA$V4
  df_BI_reg_strA$second_run <- df_BI_reg_strA$second_run*BI_reg_n_yr2  + c_init_strA*BI_reg_n_yr2
  
  df_BI_reg_strA$third_run <- df_BI_reg_strA$V1+
    df_BI_reg_strA$V2+
    df_BI_reg_strA$V3
  df_BI_reg_strA$third_run <- df_BI_reg_strA$third_run*BI_reg_n_yr3  + c_init_strA*BI_reg_n_yr3
  
  df_BI_reg_strA$fourth_run <- df_BI_reg_strA$V1+
    df_BI_reg_strA$V2
  df_BI_reg_strA$fourth_run <- df_BI_reg_strA$fourth_run*BI_reg_n_yr4 + c_init_strA*BI_reg_n_yr4
  
  df_BI_reg_strA$fifth_run <- df_BI_reg_strA$V1
  df_BI_reg_strA$fifth_run <- df_BI_reg_strA$fifth_run*BI_reg_n_yr5 + c_init_strA*BI_reg_n_yr5
  
  df_BI_reg_strA$total_BI <- df_BI_reg_strA$first_run + df_BI_reg_strA$second_run + df_BI_reg_strA$third_run + df_BI_reg_strA$fourth_run + df_BI_reg_strA$fifth_run  
  
  #BUDGET IMPACT, national
  #SOC BI
  df_BI_nat_SOC <- cycle_costs_SOC[,1:horizon_BI]
  
  df_BI_nat_SOC$first_run <- df_BI_nat_SOC$V1+
    df_BI_nat_SOC$V2+
    df_BI_nat_SOC$V3+
    df_BI_nat_SOC$V4+
    df_BI_nat_SOC$V5
  df_BI_nat_SOC$first_run <- df_BI_nat_SOC$first_run*BI_nat_n_yr1  
  
  df_BI_nat_SOC$second_run <- df_BI_nat_SOC$V1+
    df_BI_nat_SOC$V2+
    df_BI_nat_SOC$V3+
    df_BI_nat_SOC$V4
  df_BI_nat_SOC$second_run <- df_BI_nat_SOC$second_run*BI_nat_n_yr2  
  
  df_BI_nat_SOC$third_run <- df_BI_nat_SOC$V1+
    df_BI_nat_SOC$V2+
    df_BI_nat_SOC$V3
  df_BI_nat_SOC$third_run <- df_BI_nat_SOC$third_run*BI_nat_n_yr3  
  
  df_BI_nat_SOC$fourth_run <- df_BI_nat_SOC$V1+
    df_BI_nat_SOC$V2
  df_BI_nat_SOC$fourth_run <- df_BI_nat_SOC$fourth_run*BI_nat_n_yr4  
  
  df_BI_nat_SOC$fifth_run <- df_BI_nat_SOC$V1
  df_BI_nat_SOC$fifth_run <- df_BI_nat_SOC$fifth_run*BI_nat_n_yr5  
  
  df_BI_nat_SOC$total_BI <- df_BI_nat_SOC$first_run + df_BI_nat_SOC$second_run + df_BI_nat_SOC$third_run + df_BI_nat_SOC$fourth_run + df_BI_nat_SOC$fifth_run
  
  
  #intervention BI
  df_BI_nat_strA <- cycle_costs_strA[,1:horizon_BI]
  
  df_BI_nat_strA$first_run <- df_BI_nat_strA$V1+
    df_BI_nat_strA$V2+
    df_BI_nat_strA$V3+
    df_BI_nat_strA$V4+
    df_BI_nat_strA$V5
  df_BI_nat_strA$first_run <- df_BI_nat_strA$first_run*BI_nat_n_yr1 + c_init_strA*BI_nat_n_yr1  
  
  df_BI_nat_strA$second_run <- df_BI_nat_strA$V1+
    df_BI_nat_strA$V2+
    df_BI_nat_strA$V3+
    df_BI_nat_strA$V4
  df_BI_nat_strA$second_run <- df_BI_nat_strA$second_run*BI_nat_n_yr2  + c_init_strA*BI_nat_n_yr2
  
  df_BI_nat_strA$third_run <- df_BI_nat_strA$V1+
    df_BI_nat_strA$V2+
    df_BI_nat_strA$V3
  df_BI_nat_strA$third_run <- df_BI_nat_strA$third_run*BI_nat_n_yr3  + c_init_strA*BI_nat_n_yr3
  
  df_BI_nat_strA$fourth_run <- df_BI_nat_strA$V1+
    df_BI_nat_strA$V2
  df_BI_nat_strA$fourth_run <- df_BI_nat_strA$fourth_run*BI_nat_n_yr4 + c_init_strA*BI_nat_n_yr4
  
  df_BI_nat_strA$fifth_run <- df_BI_nat_strA$V1
  df_BI_nat_strA$fifth_run <- df_BI_nat_strA$fifth_run*BI_nat_n_yr5 + c_init_strA*BI_nat_n_yr5
  
  df_BI_nat_strA$total_BI <- df_BI_nat_strA$first_run + df_BI_nat_strA$second_run + df_BI_nat_strA$third_run + df_BI_nat_strA$fourth_run + df_BI_nat_strA$fifth_run  
  
  
  return(list(df_BI_reg_SOC = df_BI_reg_SOC,
              df_BI_reg_strA = df_BI_reg_strA,
              df_BI_nat_SOC = df_BI_nat_SOC,
              df_BI_nat_strA = df_BI_nat_strA))
}


#### 14. BI Model for pooled analyses ####

run_BI_model_pooled <- function(){
  

  output_CN <- run_BI_model_controlled_nohis()

  output_UN <- run_BI_model_uncontrolled_nohis()

  output_CY <- run_BI_model_controlled_yeshis()

  output_UY <- run_BI_model_uncontrolled_yeshis()

  
  #pooled, regional
  df_BI_reg_SOC_pooled <- (output_CN$df_BI_reg_SOC*sg_prop_CN +
                             output_UN$df_BI_reg_SOC*sg_prop_UN +
                             output_CY$df_BI_reg_SOC*sg_prop_CY +
                             output_UY$df_BI_reg_SOC*sg_prop_UY)
  
  df_BI_reg_strA_pooled <- (output_CN$df_BI_reg_strA*sg_prop_CN +
                              output_UN$df_BI_reg_strA*sg_prop_UN +
                              output_CY$df_BI_reg_strA*sg_prop_CY +
                              output_UY$df_BI_reg_strA*sg_prop_UY)
  
  #pooled, national
  df_BI_nat_SOC_pooled <- (output_CN$df_BI_nat_SOC*sg_prop_CN +
                             output_UN$df_BI_nat_SOC*sg_prop_UN +
                             output_CY$df_BI_nat_SOC*sg_prop_CY +
                             output_UY$df_BI_nat_SOC*sg_prop_UY)
  
  df_BI_nat_strA_pooled <- (output_CN$df_BI_nat_strA*sg_prop_CN +
                              output_UN$df_BI_nat_strA*sg_prop_UN +
                              output_CY$df_BI_nat_strA*sg_prop_CY +
                              output_UY$df_BI_nat_strA*sg_prop_UY)
  

  return(list(df_BI_reg_SOC = df_BI_reg_SOC_pooled,
              df_BI_reg_strA = df_BI_reg_strA_pooled,
              df_BI_nat_SOC = df_BI_nat_SOC_pooled,
              df_BI_nat_strA = df_BI_nat_strA_pooled))
  
}

#### 15. Wrapper for CE model ####


f_wrapper <- function(
    
  #-- User adjustable inputs --#
  
  n_age_init,
  perspective, 
  prop_male,
  c_init_strA, 
  c_strA_controlled, 
  c_strA_uncontrolled, 
  c_SOC_controlled, 
  c_SOC_uncontrolled, 
  sbp_diff_controlled, 
  sbp_diff_uncontrolled, 
  subgroup,
  year_reduction_effect, 
  percentage_reduction_effect,
  d_c, 
  d_e 

  
){
  # need to specify environment of inner functions (to use outer function enviroment)
  # alternatively - define functions within the wrapper function.
  environment(add_common_aes)                <- environment()
  environment(calc_evpi)                     <- environment()
  environment(calc_exp_loss)                 <- environment()
  environment(calc_prevalence)               <- environment()
  environment(calc_prop_sicker)              <- environment()
  environment(calc_sick)                     <- environment()
  environment(calc_surv)                     <- environment()
  environment(calculate_icers)               <- environment()
  environment(calculate_icers_psa)           <- environment()
  environment(calculate_outcome)             <- environment()
  environment(ceac)                          <- environment()
  environment(check_df_and_coerce)           <- environment()
  environment(check_psa_object)              <- environment()
  environment(check_sum_of_transition_array) <- environment()
  environment(check_transition_probability)  <- environment()
  environment(compute_icers)                 <- environment()
  environment(create_sa)                     <- environment()
  environment(data_summary)                  <- environment()
  environment(format_table_cea)              <- environment()
  environment(gen_score_tprobs)              <- environment()
  environment(gen_smart_tprobs)              <- environment()
  environment(gen_wcc)                        <- environment()
  environment(get_os)                        <- environment()
  environment(get_paid)                      <- environment()
  environment(labfun)                        <- environment()
  environment(make_psa_obj)                  <- environment()
  environment(mort_p_scaling)               <- environment()
  environment(nl_mort)                      <- environment()
  environment(number_ticks)                  <- environment()
  environment(plot.ceac)                     <- environment()
  environment(plot.evpi)                     <- environment()
  environment(plot.exp_loss)                 <- environment()
  environment(plot.icers)                    <- environment()
  environment(inc_scatter)                   <- environment()
  environment(plot_prevalence)               <- environment()
  environment(plot_proportion_sicker)        <- environment()
  environment(plot_trace)                    <- environment()
  environment(plot_trace_strategy)           <- environment()
  environment(print.sa)                      <- environment()
  environment(rate_to_prob)                  <- environment()
  environment(run_model_controlled_nohis)    <- environment()
  environment(run_model_controlled_yeshis)   <- environment()
  environment(run_model_uncontrolled_nohis)  <- environment()
  environment(run_model_uncontrolled_yeshis) <- environment()
  environment(run_model_pooled)              <- environment()
  environment(summary.ceac)                  <- environment()
  environment(summary.psa)                    <- environment()
  environment(update_param_list)              <- environment()
  environment(rnormA)                         <- environment()
  environment(rtriA)                         <- environment()
  environment(rbetaA)                         <- environment()
  
  
  
  
  #-- Unadjustable inputs --#
  
  
  # Model input ----
  ## General setup ----
  cycle_length <- 1 # cycle length equal to one year
  
  n_age_max  <- 100 # maximum age of follow up
  n_cycles <- (n_age_max - n_age_init)/cycle_length # time horizon, number of cycles
  n_sim <- 1000 # number of simulations (for probabilistic analysis)
  wtp <- 30000

  #* Age labels 
  v_age_names <- paste(rep(n_age_init:(n_age_max-1), each = 1/cycle_length), 
                       1:(1/cycle_length), 
                       sep = ".")

  v_names_states <- c("R",  # Healthy (H)
                      "MI1", # post-MI
                      "MI2", # post-recurrent-MI
                      "S1", # post-stroke
                      "S2", # post-recurrent-stroke
                      "CVD", # CVD related death
                      "D")  # Dead (D)
  
  n_states <- length(v_names_states)   # number of health states 
  
  ### Strategies ----
  v_names_str <- c("Standard of care",      # store the strategy names
                   "Strategy A") 
  n_str       <- length(v_names_str)        # number of strategies
  
  ## Within-cycle correction (WCC) using Simpson's 1/3 rule ----
  v_wcc <- gen_wcc(n_cycles = n_cycles,  # Function included in "R/Functions.R". The latest version can be found in `darthtools` package
                   method = "half-cycle") # vector of wcc
  
  #population subgroup proportions (from prospective CVRM-Box)
  sg_prop_CN <- 0.482 
  sg_prop_UN <- 0.345 
  sg_prop_CY <- 0.124 
  sg_prop_UY <- 0.049 
  
  
  #from CBS
  prob_nonfatal_MI_men_20	<-	0.544492844
  prob_nonfatal_MI_men_45	<-	0.579836322
  prob_nonfatal_MI_men_65	<-	0.394545871
  prob_nonfatal_MI_men_80	<-	0.213132115
  prob_nonfatal_stroke_men_20	<-	0.34847542
  prob_nonfatal_stroke_men_45	<-	0.310813164
  prob_nonfatal_stroke_men_65	<-	0.386525094
  prob_nonfatal_stroke_men_80	<-	0.314800106
  prob_fatal_event_men_20	<-	0.107031736
  prob_fatal_event_men_45	<-	0.109350514
  prob_fatal_event_men_65	<-	0.218929035
  prob_fatal_event_men_80	<-	0.472067779
  prob_nonfatal_MI_women_20	<-	0.348338692
  prob_nonfatal_MI_women_45	<-	0.44973545
  prob_nonfatal_MI_women_65	<-	0.314247156
  prob_nonfatal_MI_women_80	<-	0.150135524
  prob_nonfatal_stroke_women_20	<-	0.584137192
  prob_nonfatal_stroke_women_45	<-	0.442077416
  prob_nonfatal_stroke_women_65	<-	0.464944412
  prob_nonfatal_stroke_women_80	<-	0.328646451
  prob_fatal_event_women_20	<-	0.067524116
  prob_fatal_event_women_45	<-	0.108187135
  prob_fatal_event_women_65	<-	0.220808431
  prob_fatal_event_women_80	<-	0.521218025
  

  stop_smoke <- rtriA(n=n_sim, mode=0.13, min=0.00, max=0.2) #
  
  ### State rewards (general) ----

  #### State Costs ----
  c_MI1 <- rtriA(n = n_sim, mode=1800, min = 1400, max = 2069) 
  c_S1 <- rtriA(n = n_sim, mode=8000, min = 7731, max = 12959) 
  c_MI2 <- rtriA(n = n_sim, mode=1000, min = 0, max = 2122) 
  c_S2 <- rtriA(n = n_sim, mode=1500, min = 0, max = 2851) 
  c_D <- rep(0, 1000)
  c_CVD <- rep(0, 1000)
  ### Utilities ----
  
  u_R_sgCN <- 0.83 #from prospective CVRM-Box data
  u_R_sgCN <- rbetaA(n = n_sim, mean=u_R_sgCN, sd=0.02)
  
  #these are decrements to the u_R_sgCN utility, subtraction occurs later
  u_hypertension <- rtriA(n = n_sim, mode=0.05, min = 0.0246, max = 0.11)
  u_MI1 <- rtriA(n = n_sim, mode=0.06, min = 0.011, max = 0.163) 
  u_S1 <- rtriA(n = n_sim, mode=0.08, min = 0.04, max = 0.23)
  u_MI2 <- rtriA(n = n_sim, mode=0.07, min = 0.0048, max = 0.16)
  u_S2 <- rtriA(n = n_sim, mode=0.068, min = 0.00, max = 0.118)
  u_D <- rep(0, n_sim)
  u_CVD <- rep(0, n_sim)
  
  ### Transition rewards ----
  du_MI1 <- rtriA(n = n_sim, mode=0.1, min = 0.0325, max = 0.196)  
  du_MI1 <- du_MI1 - u_MI1
  du_S1 <- rtriA(n = n_sim, mode=0.15, min = 0, max = 0.26) # 
  du_S1 <- du_S1 - u_S1
  du_MI2 <- du_MI1  # 
  du_S2 <- du_S1  # 
  du_MItoS <- rbetaA(n = n_sim, mean=0.34, sd=(0.35/sqrt(200))) 
  du_MItoS <- du_MItoS - u_MI1 - u_S1

  ic_MI1 <- rtriA(n = n_sim, mode=10000, min = 6484, max = 24847)   # 
  ic_MI1 <- ic_MI1 - c_MI1
  ic_S1 <- rtriA(n = n_sim, mode=17000, min = 6928, max = 25408)   # 
  ic_S1 <- ic_S1 - c_S1

  ic_MI2 <- ic_MI1  # 
  ic_S2 <- ic_S1  # 
  ic_D <- rep(0, n_sim)
  ic_CVD <- ic_MI1 #assuming lowest of non-fatal cost categories (i.e. MI)
  if(perspective == "societal") {
    ic_fric <- rtriA(n = n_sim, mode=19828, min = 17978, max = 21679)
  }else{
    ic_fric <- rep(0, n_sim)
  }
  
  
  #copy to df
  df_psa_input <- data.frame(
    c_CVD	,   c_D	,   c_init_strA	, c_MI1	, c_MI2	,  c_S1	,  c_S2	,  c_strA_controlled	, c_strA_uncontrolled, c_SOC_controlled, c_SOC_uncontrolled,
    du_MI1	,  du_MI2	,  du_MItoS	,  du_S1	,  du_S2	,
    ic_CVD	,  ic_D	,  ic_MI1	,  ic_MI2	,  ic_S1	,  ic_S2	,
    sbp_diff_controlled, sbp_diff_uncontrolled	,  stop_smoke	,
    u_CVD	,  u_D	,  u_hypertension	,  u_MI1	,  u_MI2	,  u_R_sgCN	,  u_S1	,  u_S2	,
    ic_fric
  )
  
  if(subgroup == "pooled"){
    output <- run_model_pooled()
    return(list(output = output))
  }else{
    if(subgroup == "CN"){
      output <- run_model_controlled_nohis()
      
      return(list(output = output))
    }else{
      if(subgroup == "UN"){
        output <- run_model_uncontrolled_nohis()
        return(list(output = output))
      }else{
        if(subgroup == "CY"){
          output <- run_model_controlled_yeshis()
          return(list(output = output))
        }else{
          if(subgroup == "UY"){
            output <- run_model_uncontrolled_yeshis()
            return(list(output = output))
          }
        }
      }
    }
  }
}


# } # end of function


#### 16. Wrapper for BI model ####


f_wrapper_BI <- function(
    
  #-- User adjustable inputs --#
  
  n_age_init,
  perspective, 
  prop_male, 
  c_init_strA, 
  c_strA_controlled, 
  c_strA_uncontrolled, 
  c_SOC_controlled, 
  c_SOC_uncontrolled,
  sbp_diff_controlled, 
  sbp_diff_uncontrolled, 
  subgroup, 
  BI_reg_n_yr1, 
  BI_reg_n_yr2, 
  BI_reg_n_yr3, 
  BI_reg_n_yr4, 
  BI_reg_n_yr5, 
  BI_nat_n_yr1, 
  BI_nat_n_yr2, 
  BI_nat_n_yr3, 
  BI_nat_n_yr4, 
  BI_nat_n_yr5, 
  year_reduction_effect, 
  percentage_reduction_effect

  
  
){
  # need to specify environment of inner functions (to use outer function enviroment)
  # alternatively - define functions within the wrapper function.
  environment(add_common_aes)                <- environment()
  environment(calc_evpi)                     <- environment()
  environment(calc_exp_loss)                 <- environment()
  environment(calc_prevalence)               <- environment()
  environment(calc_prop_sicker)              <- environment()
  environment(calc_sick)                     <- environment()
  environment(calc_surv)                     <- environment()
  environment(calculate_icers)               <- environment()
  environment(calculate_icers_psa)           <- environment()
  environment(calculate_outcome)             <- environment()
  environment(ceac)                          <- environment()
  environment(check_df_and_coerce)           <- environment()
  environment(check_psa_object)              <- environment()
  environment(check_sum_of_transition_array) <- environment()
  environment(check_transition_probability)  <- environment()
  environment(compute_icers)                 <- environment()
  environment(create_sa)                     <- environment()
  environment(data_summary)                  <- environment()
  environment(format_table_cea)              <- environment()
  environment(gen_score_tprobs)              <- environment()
  environment(gen_smart_tprobs)              <- environment()
  environment(gen_wcc)                        <- environment()
  environment(get_os)                        <- environment()
  environment(labfun)                        <- environment()
  environment(make_psa_obj)                  <- environment()
  environment(mort_p_scaling)               <- environment()
  environment(nl_mort)                      <- environment()
  environment(number_ticks)                  <- environment()
  environment(plot.ceac)                     <- environment()
  environment(plot.evpi)                     <- environment()
  environment(plot.exp_loss)                 <- environment()
  environment(plot.icers)                    <- environment()
  environment(inc_scatter)                   <- environment()
  environment(plot_prevalence)               <- environment()
  environment(plot_proportion_sicker)        <- environment()
  environment(plot_trace)                    <- environment()
  environment(plot_trace_strategy)           <- environment()
  environment(print.sa)                      <- environment()
  environment(rate_to_prob)                  <- environment()
  environment(run_BI_model_controlled_nohis)    <- environment()
  environment(run_BI_model_controlled_yeshis)   <- environment()
  environment(run_BI_model_uncontrolled_nohis)  <- environment()
  environment(run_BI_model_uncontrolled_yeshis) <- environment()
  environment(run_BI_model_pooled)              <- environment()
  environment(summary.ceac)                  <- environment()
  environment(summary.psa)                    <- environment()
  environment(update_param_list)              <- environment()
  environment(rnormA)                         <- environment()
  environment(rtriA)                         <- environment()
  environment(rbetaA)                         <- environment()
  
  
  
  
  #-- Unadjustable inputs --#
  
  ## General setup ----
  cycle_length <- 1 # cycle length equal to one year
  
  n_age_max  <- n_age_init + 5 # maximum age of follow up
  n_cycles <- (n_age_max - n_age_init)/cycle_length # 5 # time horizon, number of cycles
  n_sim <- 1000 # number of simulations (for probabilistic analysis)
  wtp <- 30000

  v_age_names <- paste(rep(n_age_init:(n_age_max-1), each = 1/cycle_length), 
                       1:(1/cycle_length), 
                       sep = ".")

  v_names_states <- c("R",  # Healthy (H)
                      "MI1", # post-MI
                      "MI2", # post-recurrent-MI
                      "S1", # post-stroke
                      "S2", # post-recurrent-stroke
                      "CVD", # CVD related death
                      "D")  # Dead (D)
  
  n_states <- length(v_names_states)   # number of health states 
  
  ### Discounting factors ----
  d_c <- 0.0 # annual discount rate for costs 
  d_e <- 0.0 # annual discount rate for QALYs
  
  ### Strategies ----
  v_names_str <- c("Standard of care",      # store the strategy names
                   "Strategy A") 
  n_str       <- length(v_names_str)        # number of strategies
  
  ## Within-cycle correction (WCC) using Simpson's 1/3 rule ----
  v_wcc <- gen_wcc(n_cycles = n_cycles,  # Function included in "R/Functions.R". The latest version can be found in `darthtools` package
                   method = "half-cycle") # vector of wcc
  
  #population subgroup proportions (from prospective CVRM-Box)
  sg_prop_CN <- 0.482 
  sg_prop_UN <- 0.345 
  sg_prop_CY <- 0.124 
  sg_prop_UY <- 0.049 
  
  #from CBS
  prob_nonfatal_MI_men_20	<-	0.544492844
  prob_nonfatal_MI_men_45	<-	0.579836322
  prob_nonfatal_MI_men_65	<-	0.394545871
  prob_nonfatal_MI_men_80	<-	0.213132115
  prob_nonfatal_stroke_men_20	<-	0.34847542
  prob_nonfatal_stroke_men_45	<-	0.310813164
  prob_nonfatal_stroke_men_65	<-	0.386525094
  prob_nonfatal_stroke_men_80	<-	0.314800106
  prob_fatal_event_men_20	<-	0.107031736
  prob_fatal_event_men_45	<-	0.109350514
  prob_fatal_event_men_65	<-	0.218929035
  prob_fatal_event_men_80	<-	0.472067779
  prob_nonfatal_MI_women_20	<-	0.348338692
  prob_nonfatal_MI_women_45	<-	0.44973545
  prob_nonfatal_MI_women_65	<-	0.314247156
  prob_nonfatal_MI_women_80	<-	0.150135524
  prob_nonfatal_stroke_women_20	<-	0.584137192
  prob_nonfatal_stroke_women_45	<-	0.442077416
  prob_nonfatal_stroke_women_65	<-	0.464944412
  prob_nonfatal_stroke_women_80	<-	0.328646451
  prob_fatal_event_women_20	<-	0.067524116
  prob_fatal_event_women_45	<-	0.108187135
  prob_fatal_event_women_65	<-	0.220808431
  prob_fatal_event_women_80	<-	0.521218025
  
  stop_smoke <- rtriA(n=n_sim, mode=0.13, min=0.00, max=0.2) #
  
  ### State rewards (general) ----
  
  #### State Costs ----
  c_MI1 <- rtriA(n = n_sim, mode=1800, min = 1400, max = 2069) 
  c_S1 <- rtriA(n = n_sim, mode=8000, min = 7731, max = 12959) 
  c_MI2 <- rtriA(n = n_sim, mode=1000, min = 0, max = 2122) 
  c_S2 <- rtriA(n = n_sim, mode=1500, min = 0, max = 2851) 
  c_D <- rep(0, 1000)
  c_CVD <- rep(0, 1000)
  ### Utilities ----
  
  u_R_sgCN <- 0.83 #from prospective CVRM-Box data
  u_R_sgCN <- rbetaA(n = n_sim, mean=u_R_sgCN, sd=0.02)
  
  #these are decrements to the u_R_sgCN utility, subtraction occurs later
  u_hypertension <- rtriA(n = n_sim, mode=0.05, min = 0.0246, max = 0.11)
  u_MI1 <- rtriA(n = n_sim, mode=0.06, min = 0.011, max = 0.163) 
  u_S1 <- rtriA(n = n_sim, mode=0.08, min = 0.04, max = 0.23)
  u_MI2 <- rtriA(n = n_sim, mode=0.07, min = 0.0048, max = 0.16)
  u_S2 <- rtriA(n = n_sim, mode=0.068, min = 0.00, max = 0.118)
  u_D <- rep(0, n_sim)
  u_CVD <- rep(0, n_sim)
  
  ### Transition rewards ----
  du_MI1 <- rtriA(n = n_sim, mode=0.1, min = 0.0325, max = 0.196)  
  du_MI1 <- du_MI1 - u_MI1
  du_S1 <- rtriA(n = n_sim, mode=0.15, min = 0, max = 0.26) # 
  du_S1 <- du_S1 - u_S1
  du_MI2 <- du_MI1  # 
  du_S2 <- du_S1  # 
  du_MItoS <- rbetaA(n = n_sim, mean=0.34, sd=(0.35/sqrt(200))) 
  du_MItoS <- du_MItoS - u_MI1 - u_S1

  ic_MI1 <- rtriA(n = n_sim, mode=10000, min = 6484, max = 24847)   # 
  ic_MI1 <- ic_MI1 - c_MI1
  ic_S1 <- rtriA(n = n_sim, mode=17000, min = 6928, max = 25408)   # 
  ic_S1 <- ic_S1 - c_S1

  ic_MI2 <- ic_MI1  # 
  ic_S2 <- ic_S1  # 
  ic_D <- rep(0, n_sim)
  ic_CVD <- ic_MI1 #assuming lowest of non-fatal cost categories (i.e. MI)
  if(perspective == "societal") {
    ic_fric <- rtriA(n = n_sim, mode=19828, min = 17978, max = 21679)
  }else{
    ic_fric <- rep(0, n_sim)
  }
  
  #for BI, regional perspective
  horizon_BI <- 5
  
  #copy to df
  df_psa_input <- data.frame(
    c_CVD	,   c_D	,   c_init_strA	, c_MI1	, c_MI2	,  c_S1	,  c_S2	,  c_strA_controlled	, c_strA_uncontrolled, c_SOC_controlled, c_SOC_uncontrolled,
    du_MI1	,  du_MI2	,  du_MItoS	,  du_S1	,  du_S2	,
    ic_CVD	,  ic_D	,  ic_MI1	,  ic_MI2	,  ic_S1	,  ic_S2	,
    sbp_diff_controlled, sbp_diff_uncontrolled	,  stop_smoke	,
    u_CVD	,  u_D	,  u_hypertension	,  u_MI1	,  u_MI2	,  u_R_sgCN	,  u_S1	,  u_S2	,
    ic_fric	,
    horizon_BI, BI_reg_n_yr1, BI_reg_n_yr2, BI_reg_n_yr3, BI_reg_n_yr4, BI_reg_n_yr5, 
    BI_nat_n_yr1, BI_nat_n_yr2, BI_nat_n_yr3, BI_nat_n_yr4, BI_nat_n_yr5
  )
  
  
  if(subgroup == "pooled"){
    output <- run_BI_model_pooled()
    return(list(output = output))
  }else{
    if(subgroup == "CN"){
      output <- run_BI_model_controlled_nohis()
      return(list(output = output))
    }else{
      if(subgroup == "UN"){
        output <- run_BI_model_uncontrolled_nohis()
        return(list(output = output))
      }else{
        if(subgroup == "CY"){
          output <- run_BI_model_controlled_yeshis()
          return(list(output = output))
        }else{
          if(subgroup == "UY"){
            output <- run_BI_model_uncontrolled_yeshis()
            return(list(output = output))
          }
        }
      }
    }
  }
}


# } # end of function


#### 17. Functions (some of which modified slightly) from tutorial ####

#----------------------------------------------------------------------------#
####                    Function to plot cohort trace                     ####
#----------------------------------------------------------------------------#
#' Plot cohort trace
#'
#' \code{plot_trace} plots the cohort trace.
#'
#' @param m_M a cohort trace matrix
#' @return a ggplot object - plot of the cohort trace
#' 
plot_trace <- function(m_M) {
  df_M      <- data.frame(Cycle = 0:n_cycles, m_M, check.names = F)
  df_M_long <- tidyr::gather(df_M, key = `Health State`, value, 2:ncol(df_M))
  df_M_long$`Health State` <- factor(df_M_long$`Health State`, levels = v_names_states)
  gg_trace <- ggplot(df_M_long, aes(x = Cycle, y = value, 
                                    color = `Health State`, linetype = `Health State`)) +
    geom_line(size = 1) +
    xlab("Cycle") +
    ylab("Proportion of the cohort") +
    scale_x_continuous(breaks = number_ticks(8)) + 
    theme_bw(base_size = 14) +
    theme(legend.position  = "bottom", 
          legend.background = element_rect(fill = NA)) 
  
  return(gg_trace) 
}

#----------------------------------------------------------------------------#
####                    Function to plot cohort trace per strategy        ####
#----------------------------------------------------------------------------#
#' Plot cohort trace per strategy
#'
#' \code{plot_trace} plots the cohort trace for each strategy, split by health state.
#'
#' @param l_m_M a list containing cohort trace matrices
#' @return a ggplot object - plot of the cohort trace for each strategy split by health state.
#' 
# plot_trace_strategy <- function(l_m_M) {
#   n_str <- length(l_m_M) 
#   for (i in seq_along(l_m_M)){
#     arr_3d <- l_m_M[[i]]
#     mat_2d <- apply(arr_3d, c(1,2), mean)
#     l_m_M[[i]] <- mat_2d
#   }
#   l_df_M <- lapply(l_m_M, as.data.frame)
#   df_M_strategies <- data.table::rbindlist(l_df_M, use.names = T, 
#                                            idcol = "Strategy")
#   df_M_strategies$Cycle <- rep(0:n_cycles, n_str)
#   m_M_plot <- tidyr::gather(df_M_strategies, key = `Health State`, value, 
#                             2:(ncol(df_M_strategies)-1))
#   m_M_plot$`Health State`    <- factor(m_M_plot$`Health State`, levels = v_names_states)
#   m_M_plot$Strategy <- factor(m_M_plot$Strategy, levels = v_names_str)
#   
#   p <- ggplot(m_M_plot, aes(x = Cycle, y = value, 
#                             color = Strategy, linetype = Strategy)) +
#     geom_line(size = 0.5) +
#     # scale_color_brewer(palette="RdBu") +
#     xlab("Cycle") +
#     ylab("Proportion of the cohort") +
#     theme_bw(base_size = 14) +
#     theme(legend.position  = "bottom", 
#           legend.background = element_rect(fill = NA)) +
#     facet_wrap(~ `Health State`)
#   
#   return(p) 
# }

#below the adapted plot_trace_strategy function
plot_trace_strategy <- function(l_m_M) {
  n_str <- length(l_m_M)
  for (i in seq_along(l_m_M)) {
    arr_3d <- l_m_M[[i]]
    mat_2d <- apply(arr_3d, c(1,2), mean)
    l_m_M[[i]] <- mat_2d
  }
  l_df_M <- lapply(l_m_M, as.data.frame)
  df_M_strategies <- data.table::rbindlist(l_df_M, use.names = TRUE, idcol = "Strategy")
  df_M_strategies$Cycle <- rep(0:n_cycles, n_str)
  m_M_plot <- tidyr::gather(df_M_strategies, key = `Health State`, value, 2:(ncol(df_M_strategies)-1))
  m_M_plot$`Health State` <- factor(m_M_plot$`Health State`, levels = v_names_states)
  m_M_plot$Strategy <- factor(m_M_plot$Strategy, levels = v_names_str)
  
  p <- ggplot(m_M_plot, aes(x = Cycle, y = value, color = `Health State`, linetype = Strategy)) +
    geom_line(size = 0.5) +
    xlab("Cycle") +
    ylab("Proportion of the cohort") +
    scale_linetype_manual(values = c("solid", "dotted")) +
    theme_bw(base_size = 14) +
    theme(legend.position = "bottom", 
          legend.background = element_rect(fill = NA),
          legend.text = element_text(size = 14))
  
  return(p)
}

#----------------------------------------------------------------------------#
####             Function to calculate survival probabilities             ####
#----------------------------------------------------------------------------#
#' Calculate survival probabilities
#'
#' \code{calc_surv} calculates the survival probabilities.
#'
#' @param l_m_M a list containing cohort trace matrices
#' @return a dataframe containing survival probabilities for each strategy
#' 
calc_surv <- function(l_m_M, v_names_death_states) {
  for (i in seq_along(l_m_M)){
    arr_3d <- l_m_M[[i]]
    mat_2d <- apply(arr_3d, c(1,2), mean)
    l_m_M[[i]] <- mat_2d
  }
  df_surv <- as.data.frame(lapply(l_m_M, 
                                  function(x) {
                                    rowSums(x[, !colnames(x) %in% v_names_death_states])
                                  }
  ))
  colnames(df_surv) <- v_names_str
  df_surv$Cycle     <- 0:n_cycles
  df_surv_long      <- tidyr::gather(df_surv, key = Strategy, Survival, 1:n_str)
  df_surv_long$Strategy <- ordered(df_surv_long$Strategy, levels = v_names_str)
  df_surv_long <- df_surv_long %>% 
    select(Strategy, Cycle, Survival)
  
  return(df_surv_long)
}

#----------------------------------------------------------------------------#
####                Function to calculate state proportions               ####
#----------------------------------------------------------------------------#
#' Calculate state proportions
#'
#' \code{calc_surv} calculates the proportions of the cohort in specified states
#'
#' @param l_m_M a list containing cohort trace matrices
#' @return a dataframe containing proportions in specified states for each strategy
#' 
calc_sick <- function(l_m_M, v_names_sick_states) {
  n_sick_states <- length(v_names_sick_states)
  df_sick <- as.data.frame(lapply(l_m_M,
                                  function(x) {
                                    if (n_sick_states == 1) {
                                      x[, colnames(x) %in% v_names_sick_states]
                                    } else {
                                      rowSums(x[, colnames(x) %in% v_names_sick_states])
                                    }
                                  }
  ))
  colnames(df_sick) <- v_names_str
  df_sick$Cycle     <- 0:n_cycles
  df_sick_long      <- tidyr::gather(df_sick, key = Strategy, Sick, 1:n_str)
  df_sick_long$Strategy <- ordered(df_sick_long$Strategy, levels = v_names_str)
  df_sick_long <- df_sick_long %>% 
    select(Strategy, Cycle, Sick)
  
  return(df_sick_long)
}

#----------------------------------------------------------------------------#
####                   Function to calculate prevalence                   ####
#----------------------------------------------------------------------------#
#' Calculate prevalence
#'
#' \code{plot_prevalence} calculate the prevalence for different health states.
#'
#' @param l_m_M a list containing cohort trace matrices
#' @return a dataframe containing prevalence of specified health states for each strategy
#' 
calc_prevalence <- function(l_m_M, v_names_sick_states, v_names_dead_states) {
  for (i in seq_along(l_m_M)){
    arr_3d <- l_m_M[[i]]
    mat_2d <- apply(arr_3d, c(1,2), mean)
    l_m_M[[i]] <- mat_2d
  }
  df_alive      <- calc_surv(l_m_M, v_names_dead_states)
  df_prop_sick  <- calc_sick(l_m_M, v_names_sick_states)
  df_prevalence <- data.frame(Strategy   = df_alive$Strategy, 
                              Cycle      = df_alive$Cycle,
                              Prevalence = df_prop_sick$Sick / df_alive$Survival)
  return(df_prevalence) 
}

#----------------------------------------------------------------------------#
####           Function to calculate state-in-state proportions           ####
#----------------------------------------------------------------------------#
#' Calculate state-in-state proportions
#'
#' \code{plot_prevalence} calculates the proportion of a speciefied subset of states among a set of specified states
#'
#' @param l_m_M a list containing cohort trace matrices
#' @return a dataframe containing state-in-state proportions of specified health states for each strategy
#' 
calc_prop_sicker <- function(l_m_M, v_names_sick_states, v_names_sicker_states) {
  for (i in seq_along(l_m_M)){
    arr_3d <- l_m_M[[i]]
    mat_2d <- apply(arr_3d, c(1,2), mean)
    l_m_M[[i]] <- mat_2d
  }
  df_prop_sick   <- calc_sick(l_m_M, v_names_sick_states)
  df_prop_sicker <- calc_sick(l_m_M, v_names_sicker_states)
  df_prop_sick_sicker <- data.frame(Strategy   = df_prop_sick$Strategy, 
                                    Cycle      = df_prop_sick$Cycle,
                                    `Proportion Sicker` = 
                                      df_prop_sicker$Sick / 
                                      (df_prop_sick$Sick + df_prop_sicker$Sick))
  
  return(df_prop_sick_sicker) 
}

#----------------------------------------------------------------------------#
####                   Function to plot survival curve                    ####
#----------------------------------------------------------------------------#
#' Plot survival curve
#'
#' \code{plot_surv} plots the survival probability curve.
#'
#' @param l_m_M a list containing cohort trace matrices
#' @return a ggplot object - plot of the survival curve
#' 
# plot_surv <- function(l_m_M, v_names_death_states) {
#   df_surv <- calc_surv(l_m_M, v_names_death_states)
#   df_surv$Strategy <- factor(df_surv$Strategy, levels = v_names_str)
# #  df_surv$Survival <- round(df_surv$Survival, 2)
#   
#   p <- ggplot(df_surv, 
#               aes(x = Cycle, y = Survival, group = Strategy)) +
#     geom_line(aes(linetype = Strategy, col = Strategy), size = 0.8) +
#    # scale_color_brewer(palette="RdBu") +
#     xlab("Cycle") +
#     ylab("Proportion") +
#     ggtitle("Survival probabilities") +
#     theme_bw(base_size = 14) +
#     theme()
#   
#   return(p) 
# }

#----------------------------------------------------------------------------#
####                   Function to plot prevalence curve                  ####
#----------------------------------------------------------------------------#
#' Plot prevalence curve
#'
#' \code{plot_prevalence} plots the prevalence curve for specified health states.
#'
#' @param l_m_M a list containing cohort trace matrices
#' @return a ggplot object - plot of the prevalence curve
#' 
plot_prevalence <- function(l_m_M, v_names_sick_states, v_names_dead_states) {
  df_prevalence <- calc_prevalence(l_m_M, v_names_sick_states, v_names_dead_states)
  df_prevalence$Strategy <- factor(df_prevalence$Strategy, levels = v_names_str)
  #df_prevalence$Proportion.Sicker <- round(df_prevalence$Prevalence, 2)
  
  p <- ggplot(df_prevalence, 
              aes(x = Cycle, y = Prevalence, group = Strategy)) +
    geom_line(aes(linetype = Strategy), size = 1.2) +
    # scale_color_brewer(palette = "RdBu") +
    xlab("Cycle") +
    ylab("Proportion") + 
    ggtitle(paste("Prevalence", "of", paste(v_names_sick_states, collapse = " "))) + 
    theme_bw(base_size = 14) +
    theme()
  
  return(p) 
}

#----------------------------------------------------------------------------#
####           Function to plot state-in-state proportion curve           ####
#----------------------------------------------------------------------------#
#' Plot state-in-state proportion curve
#'
#' \code{plot_prevalence} plots the 
#'
#' @param l_m_M a list containing cohort trace matrices
#' @return a ggplot object - plot of state-in-state proportion curve
#' 
plot_proportion_sicker <- function(l_m_M, v_names_sick_states, v_names_sicker_states) {
  df_proportion_sicker <- calc_prop_sicker(l_m_M, v_names_sick_states, v_names_sicker_states)
  df_proportion_sicker$Strategy <- factor(df_proportion_sicker$Strategy, levels = v_names_str)
  #df_proportion_sicker$Proportion.Sicker <- round(df_proportion_sicker$Proportion.Sicker, 2)
  
  p <- ggplot(df_proportion_sicker, 
              aes(x = Cycle, y = Proportion.Sicker, group = Strategy)) +
    geom_line(aes(linetype = Strategy), size = 1.2, na.rm = T) +
    #scale_color_brewer(palette = "RdBu") +
    xlab("Cycle") +
    ylab("Proportion") +
    ggtitle(paste(paste("Proportion of", v_names_sicker_states), 
                  paste(c("among", v_names_sick_states), collapse = " "))) + 
    theme_bw(base_size = 14) +
    theme()
  
  return(p) 
}

#----------------------------------------------------------------------------#
####                     Function to format CEA table                     ####
#----------------------------------------------------------------------------#
#' Format CEA table
#'
#' \code{format_table_cea} formats the CEA table.
#'
#' @param table_cea a dataframe object - table with CEA results
#' @return a dataframe object - formatted CEA table
#' 
format_table_cea <- function(table_cea) {
  colnames(table_cea)[colnames(table_cea) 
                      %in% c("Cost", 
                             "Effect", 
                             "Inc_Cost", 
                             "Inc_Effect",
                             "ICER")] <- 
    
    c("Costs ($)", 
      "QALYs", 
      "Incremental Costs ($)", 
      "Incremental QALYs", 
      "ICER ($/QALY)") 
  
  table_cea$`Costs ($)` <- comma(round(table_cea$`Costs ($)`, 0))
  table_cea$`Incremental Costs ($)` <- comma(round(table_cea$`Incremental Costs ($)`, 0))
  table_cea$QALYs <- round(table_cea$QALYs, 2)
  table_cea$`Incremental QALYs` <- round(table_cea$`Incremental QALYs`, 2)
  table_cea$`ICER ($/QALY)` <- comma(round(table_cea$`ICER ($/QALY)`, 0))
  return(table_cea)
}

#' Update parameters
#'
#' \code{update_param_list} is used to update list of all parameters with new 
#' values for specific parameters.
#'
#' @param l_params_all List with all parameters of decision model
#' @param params_updated Parameters for which values need to be updated
#' @return 
#' A list with all parameters updated.
#' @export
update_param_list <- function(l_params_all, params_updated){
  
  if (typeof(params_updated)!="list"){
    params_updated <- split(unname(params_updated),names(params_updated)) #converte the named vector to a list
  }
  l_params_all <- modifyList(l_params_all, params_updated) #update the values
  return(l_params_all)
}

################################################################################
################# FUNCTIONS INCLUDED IN DARTHTOOLS #############################
################################################################################
#' Within-cycle correction (WCC)
#'
#' \code{gen_wcc} generates a vector of within-cycle corrections (WCC).
#'
#' @param n_cycles number of cycles
#' @param method The method to be used for within-cycle correction.
#'
#' @return A vector of length \code{n_cycles + 1} with within-cycle corrections
#'
#' @details
#' The default method is an implementation of Simpson's 1/3rd rule that
#' generates a vector with the first and last entry with 1/3 and the odd and
#' even entries with 4/3 and 2/3, respectively.
#'
#' Method "\code{half-cycle}" is the half-cycle correction method that
#' generates a vector with the first and last entry with 1/2 and the rest equal
#' to 1.
#'
#' Method "\code{none}" does not implement any within-cycle correction and
#' generates a vector with ones.
#'
#' @references
#' \enumerate{
#' \item Elbasha EH, Chhatwal J. Myths and misconceptions of within-cycle
#' correction: a guide for modelers and decision makers. Pharmacoeconomics.
#' 2016;34(1):13-22.
#' \item Elbasha EH, Chhatwal J. Theoretical foundations and practical
#' applications of within-cycle correction methods. Med Decis Mak.
#' 2016;36(1):115-131.
#' }
#'
#' @examples
#' # Number of cycles
#' n_cycles <- 10
#' gen_wcc(n_cycles = n_cycles, method = "Simpson1/3")
#' gen_wcc(n_cycles = n_cycles, method = "half-cycle")
#' gen_wcc(n_cycles = n_cycles, method = "none")
#'
#' @export
gen_wcc <- function (n_cycles, method = c("Simpson1/3", "half-cycle", "none")) 
{
  if (n_cycles <= 0) {
    stop("Number of cycles should be positive")
  }
  method <- match.arg(method)
  n_cycles <- as.integer(n_cycles)
  if (method == "Simpson1/3") {
    v_cycles <- seq(1, n_cycles + 1)
    v_wcc <- ((v_cycles%%2) == 0) * (2/3) + ((v_cycles%%2) != 
                                               0) * (4/3)
    v_wcc[1] <- v_wcc[n_cycles + 1] <- 1/3
  }
  if (method == "half-cycle") {
    v_wcc <- rep(1, n_cycles + 1)
    v_wcc[1] <- v_wcc[n_cycles + 1] <- 0.5
  }
  if (method == "none") {
    v_wcc <- rep(1, n_cycles + 1)
  }
  return(v_wcc)
}

function (r, t = 1) 
{
  if ((sum(r < 0) > 0)) {
    stop("rate not greater than or equal to 0")
  }
  p <- 1 - exp(-r * t)
  return(p)
}

#' Convert a rate to a probability
#'
#' \code{rate_to_prob} convert a rate to a probability.
#'
#' @param r rate
#' @param t time/ frequency
#' @return a scalar or vector with probabilities
#' @examples
#' # Annual rate to monthly probability
#' r_year  <- 0.3
#' r_month <- rate_to_prob(r = r_year, t = 1/12)
#' r_month
#' @export
rate_to_prob <- function(r, t = 1){
  if ((sum(r < 0) > 0)){
    stop("rate not greater than or equal to 0")
  }
  p <- 1 - exp(- r * t)
  return(p)
}

#' Check if transition array is valid
#'
#' \code{check_transition_probability} checks if transition probabilities are in \[0, 1\].
#'
#' @param a_P A transition probability array/ matrix.
#' @param err_stop Logical variable to stop model run if set up as TRUE. Default = FALSE.
#' @param verbose Logical variable to indicate print out of messages.
#' Default = FALSE
#'
#' @return
#' This function stops if transition probability array is not valid and shows
#' what are the entries that are not valid
#' @export
check_transition_probability <- function(a_P,
                                         err_stop = FALSE,
                                         verbose = FALSE) {
  
  a_P <- as.array(a_P)
  
  # Verify if a_P is 2D or 3D matrix
  n_dim <- length(dim(a_P))
  # If a_P is a 2D matrix, convert to a 3D array
  if (n_dim < 3){
    a_P <- array(a_P, dim = list(nrow(a_P), ncol(a_P), 1),
                 dimnames = list(rownames(a_P), colnames(a_P), "Time independent"))
  }
  # Check which entries are not valid
  m_indices_notvalid <- arrayInd(which(a_P < 0 | a_P > 1),
                                 dim(a_P))
  
  if(dim(m_indices_notvalid)[1] != 0){
    v_rows_notval   <- rownames(a_P)[m_indices_notvalid[, 1]]
    v_cols_notval   <- colnames(a_P)[m_indices_notvalid[, 2]]
    v_cycles_notval <- dimnames(a_P)[[3]][m_indices_notvalid[, 3]]
    
    df_notvalid <- data.frame(`Transition probabilities not valid:` =
                                matrix(paste0(paste(v_rows_notval, v_cols_notval, sep = "->"),
                                              "; at cycle ",
                                              v_cycles_notval), ncol = 1),
                              check.names = FALSE)
    
    if(err_stop) {
      stop("Not valid transition probabilities\n",
           paste(capture.output(df_notvalid), collapse = "\n"))
    }
    
    if(verbose){
      warning("Not valid transition probabilities\n",
              paste(capture.output(df_notvalid), collapse = "\n"))
    }
  }
}

#' Check if the sum of transition probabilities equal to one.
#'
#' \code{check_sum_of_transition_array} checks if each of the rows of the
#' transition matrices sum to one.
#'
#' @param a_P A transition probability array/ matrix.
#' @param n_states Number of health states in a Markov trace, appropriate for Markov models.
#' @param n_rows Number of rows (individuals), appropriate for microsimulation models.
#' @param n_cycles Number of cycles.
#' @param err_stop Logical variable to stop model run if set up as TRUE.
#' Default = TRUE.
#' @param verbose Logical variable to indicate print out of messages.
#' Default = TRUE
#' @return
#' The transition probability array and the cohort trace matrix.
#' @export
check_sum_of_transition_array <- function(a_P,
                                          n_rows = NULL,
                                          n_states = NULL,
                                          n_cycles,
                                          err_stop = TRUE,
                                          verbose  = TRUE) {
  
  if (!is.null(n_rows) & !is.null(n_states)) {
    stop("Pick either n_rows or n_states, not both.")
  }
  
  if (is.null(n_rows) & is.null(n_states)) {
    stop("Need to specify either n_rows or n_states, but not both.")
  }
  
  if (!is.null(n_rows)) {
    n_states <- n_rows
  }
  
  a_P <- as.array(a_P)
  d <- length(dim(a_P))
  # For matrix
  if (d == 2) {
    valid <- sum(rowSums(a_P))
    if (abs(valid - n_states) > 0.01) {
      if(err_stop) {
        stop("This is not a valid transition matrix")
      }
      
      if(verbose){
        warning("This is not a valid transition matrix")
      }
    }
  } else {
    # For array
    valid <- (apply(a_P, d, function(x) sum(rowSums(x))) == n_states)
    if (!isTRUE(all.equal(as.numeric(sum(valid)), as.numeric(n_cycles)))) {
      if(err_stop) {
        stop("This is not a valid transition array")
      }
      
      if(verbose){
        warning("This is not a valid transition array")
      }
    }
  }
}

################################################################################
################## FUNCTIONS INCLUDED IN DAMPACK ###############################
################################################################################

# For a more detailed description and current versions of the functions above, 
# please go to dampack's package Github repo (https://github.com/DARTH-git/dampack)

#' Cost-Effectiveness Acceptability Curve (CEAC)
#'
#' \code{ceac} is used to compute and plot the cost-effectiveness acceptability
#' curves (CEAC) from a probabilistic sensitivity analysis (PSA) dataset.
#'
#' @param wtp numeric vector with willingness-to-pay (WTP) thresholds
#' @param psa psa object from \code{\link{make_psa_obj}}
#' @keywords cost-effectiveness acceptability curves
#' @details
#' \code{ceac} computes the probability of each of the strategies being
#' cost-effective at each \code{wtp} threshold. The returned object has classes
#' \code{ceac} and \code{data.frame}, and has its own plot method (\code{\link{plot.ceac}}).
#'
#' @return An object of class \code{ceac} that can be visualized with \code{plot}. The \code{ceac}
#' object is a data.frame that shows the proportion of PSA samples for which each strategy at each
#' WTP threshold is cost-effective. The final column indicates whether or not the strategy at a
#' particular WTP is on the cost-efficient frontier.
#'
#' @examples
#' # psa input provided with package
#' data("example_psa")
#' example_psa_obj <- make_psa_obj(example_psa$cost, example_psa$effectiveness,
#'                     example_psa$parameters, example_psa$strategies)
#'
#' # define wtp threshold vector (can also use a single wtp)
#' wtp <- seq(1e4, 1e5, by = 1e4)
#' ceac_obj <- ceac(wtp, example_psa_obj)
#' plot(ceac_obj) # see ?plot.ceac for options
#'
#' # this is most useful when there are many strategies
#' # warnings are printed to describe strategies that
#' # have been filtered out
#' plot(ceac_obj, min_prob = 0.5)
#'
#' # standard ggplot layers can be used
#' plot(ceac_obj) +
#'     labs(title = "CEAC", y = "Pr(Cost-effective) at WTP")
#'
#' # the ceac object is also a data frame
#' head(ceac_obj)
#'
#' # summary() tells us the regions of cost-effectiveness for each strategy.
#' # Note that the range_max column is an open parenthesis, meaning that the
#' # interval over which that strategy is cost-effective goes up to but does not include
#' # the value in the range_max column.
#' summary(ceac_obj)
#'
#' @seealso
#' \code{\link{plot.ceac}}, \code{\link{summary.ceac}}
#'
#'
#' @importFrom tidyr pivot_longer
#' @export
ceac <- function(wtp, psa) {
  # check that psa has class 'psa'
  check_psa_object(psa)
  
  # define needed variables
  strategies <- psa$strategies
  n_strategies <- psa$n_strategies
  effectiveness <- psa$effectiveness
  cost <- psa$cost
  n_sim <- psa$n_sim
  
  # number of willingness to pay thresholds
  n_wtps <- length(wtp)
  
  # matrix to store probability optimal for each strategy
  cea <- matrix(0, nrow = n_wtps, ncol = n_strategies)
  colnames(cea) <- strategies
  
  # vector to store strategy at the cost-effectiveness acceptability frontier
  frontv <- rep(0, n_wtps)
  
  for (l in 1:n_wtps) {
    # calculate net monetary benefit at wtp[l]
    lth_wtp <- wtp[l]
    nmb <-  calculate_outcome("nmb", cost, effectiveness, lth_wtp)
    
    # find the distribution of optimal strategies
    max.nmb <- max.col(nmb)
    opt <- table(max.nmb)
    cea[l, as.numeric(names(opt))] <- opt / n_sim
    
    # calculate point on CEAF
    # the strategy with the highest expected nmb
    frontv[l] <- which.max(colMeans(nmb))
  }
  
  # make cea df
  cea_df <- data.frame(wtp, cea, strategies[frontv],
                       stringsAsFactors = FALSE)
  colnames(cea_df) <- c("WTP", strategies, "fstrat")
  
  # Reformat df to long format
  ceac <- tidyr::pivot_longer(
    data = cea_df,
    cols = !c("WTP", "fstrat"),
    names_to = "Strategy",
    values_to = "Proportion"
  )
  
  # boolean for on frontier or not
  ceac$On_Frontier <- (ceac$fstrat == ceac$Strategy)
  
  # drop fstrat column
  ceac$fstrat <- NULL
  
  # order by WTP
  ceac <- ceac[order(ceac$WTP), ]
  
  # remove rownames
  rownames(ceac) <- NULL
  
  # make strategies in ceac object into ordered factors
  ceac$Strategy <- factor(ceac$Strategy, levels = strategies, ordered = TRUE)
  
  # define classes
  # defining data.frame as well allows the object to use print.data.frame, for example
  class(ceac) <- c("ceac", "data.frame")
  
  return(ceac)
}

#' Plot of Cost-Effectiveness Acceptability Curves (CEAC)
#'
#' Plots the CEAC, using the object created by \code{\link{ceac}}.
#'
#' @param x object of class \code{ceac}.
#' @param frontier whether to plot acceptability frontier (TRUE) or not (FALSE)
#' @param points whether to plot points (TRUE) or not (FALSE)
#' @param currency string with currency used in the cost-effectiveness analysis (CEA).
#'Defaults to \code{$}, but can be any currency symbol or word (e.g., £, €, peso)
#' @param min_prob minimum probability to show strategy in plot.
#' For example, if the min_prob is 0.05, only strategies that ever
#' exceed Pr(Cost Effective) = 0.05 will be plotted. Most useful in situations
#' with many strategies.
#' @inheritParams add_common_aes
#'
#' @keywords internal
#'
#' @details
#' \code{ceac} computes the probability of each of the strategies being
#' cost-effective at each \code{wtp} value.
#' @return A \code{ggplot2} plot of the CEAC.
#'
#' @import ggplot2
#' @import dplyr
#'
#' @export
plot.ceac <- function(x,
                      frontier = TRUE,
                      points = TRUE,
                      currency = "€",
                      min_prob = 0,
                      txtsize = 12,
                      n_x_ticks = 10,
                      n_y_ticks = 8,
                      xbreaks = NULL,
                      ybreaks = NULL,
                      ylim = NULL,
                      xlim = c(0, NA),
                      col = c("full", "bw"),
                      ...) {
  wtp_name <- "WTP"
  prop_name <- "Proportion"
  strat_name <- "Strategy"
  x$WTP_thou <- x[, wtp_name] / 1000
  
  # removing strategies with probabilities always below `min_prob`
  # get group-wise max probability
  if (min_prob > 0) {
    max_prob <- x %>%
      group_by(.data$Strategy) %>%
      summarize(maxpr = max(.data$Proportion)) %>%
      filter(.data$maxpr >= min_prob)
    strat_to_keep <- max_prob$Strategy
    if (length(strat_to_keep) == 0) {
      stop(
        paste("no strategies remaining. you may want to lower your min_prob value (currently ",
              min_prob, ")", sep = "")
      )
    }
    # report filtered out strategies
    old_strat <- unique(x$Strategy)
    diff_strat <- setdiff(old_strat, strat_to_keep)
    n_diff_strat <- length(diff_strat)
    if (n_diff_strat > 0) {
      # report strategies filtered out
      cat("filtered out ", n_diff_strat, " strategies with max prob below ", min_prob, ":\n",
          paste(diff_strat, collapse = ","), "\n", sep = "")
      
      # report if any filtered strategies are on the frontier
      df_filt <- filter(x, .data$Strategy %in% diff_strat & .data$On_Frontier)
      if (nrow(df_filt) > 0) {
        cat(paste0("WARNING - some strategies that were filtered out are on the frontier:\n",
                   paste(unique(df_filt$Strategy), collapse = ","), "\n"))
      }
    }
    
    # filter dataframe
    x <- filter(x, .data$Strategy %in% strat_to_keep)
  }
  
  # Drop unused strategy names
  x$Strategy <- droplevels(x$Strategy)
  
  #JH modification to exclude SOC from CEAC, and change name
  x <- x %>% filter(Strategy == "Strategy A")
  #
  p <- ggplot(data = x, aes_(x = as.name("WTP_thou"),
                             y = as.name(prop_name),
                             color = as.name(strat_name))) +
    geom_line(show.legend = F) +  #JH added to omit legend since we only show 1 strategy
    xlab(paste("Willingness to Pay (Thousand ", currency, " / QALY)", sep = "")) +
    ylab("Pr Intervention is Cost-Effective")
  
  if (points) {
    p <- p + geom_point(aes_(color = as.name(strat_name)))
  }
  if (frontier) {
    front <- x[x$On_Frontier, ]
    p <- p + geom_point(data = front, aes_(x = as.name("WTP_thou"),
                                           y = as.name(prop_name),
                                           shape = as.name("On_Frontier")),
                        size = 3, stroke = 1, color = "black") +
      scale_shape_manual(name = NULL, values = 0, labels = "Frontier") +
      guides(color = guide_legend(order = 1),
             shape = guide_legend(order = 2))
  }
  col <- match.arg(col)
  add_common_aes(p, txtsize, col = col, col_aes = "color",
                 continuous = c("x", "y"), n_x_ticks = n_x_ticks, n_y_ticks = n_y_ticks,
                 xbreaks = xbreaks, ybreaks = ybreaks,
                 ylim = ylim, xlim = xlim)
}


#' Summarize a ceac
#'
#' Describes cost-effective strategies and their
#' associated intervals of cost-effectiveness
#'
#' @param object object returned from the \code{ceac} function
#' @param ... further arguments (not used)
#' @return data frame showing the interval of cost effectiveness for each
#' interval. The intervals are open on the right endpoint -
#' i.e., [\code{range_min}, \code{range_max})
#'
#' @keywords internal
#'
#' @export
summary.ceac <- function(object, ...) {
  front <- object[object$On_Frontier == TRUE, ]
  front$Strategy <- as.character(front$Strategy)
  wtp <- front$WTP
  wtp_range <- range(wtp)
  n_wtps <- length(wtp)
  
  # get the indices where the CE strategy isn't the same as the following CE strategy
  strat_on_front <- front$Strategy
  lagged_strat <- c(strat_on_front[-1], strat_on_front[n_wtps])
  switches <- which(strat_on_front != lagged_strat) + 1
  n_switches <- length(switches)
  # strat_on_front[switches] are the optimal strategies at wtp[switches]
  if (n_switches == 0) {
    wtp_min <- wtp_range[1]
    wtp_max <- wtp_range[2]
    one_strat <- unique(front$Strategy)
    sum_df <- data.frame(wtp_min,
                         wtp_max,
                         one_strat)
  } else {
    # build up summary data frame
    sum_df <- NULL
    for (i in 1:n_switches) {
      if (i == 1) {
        sum_df_row_first <- data.frame(wtp_range[1],
                                       wtp[switches],
                                       strat_on_front[switches - 1],
                                       fix.empty.names = FALSE,
                                       stringsAsFactors = FALSE)
        sum_df <- rbind(sum_df, sum_df_row_first)
      }
      if (i == n_switches) {
        sum_df_row_last <- data.frame(wtp[switches],
                                      wtp_range[2],
                                      strat_on_front[switches],
                                      fix.empty.names = FALSE,
                                      stringsAsFactors = FALSE)
        sum_df <- rbind(sum_df, sum_df_row_last)
      }
      if (i > 1) {
        sum_df_row_middle <- data.frame(wtp[switches[i]],
                                        wtp[switches[i + 1]],
                                        strat_on_front[switches[i]],
                                        fix.empty.names = FALSE,
                                        stringsAsFactors = FALSE)
        sum_df <- rbind(sum_df, sum_df_row_middle)
      }
    }
  }
  names(sum_df) <- c("range_min", "range_max", "cost_eff_strat")
  sum_df
}

#' Calculate incremental cost-effectiveness ratios (ICERs)
#'
#' @description
#' This function takes in strategies and their associated cost and effect, assigns them
#' one of three statuses (non-dominated, extended dominated, or dominated), and
#' calculates the incremental cost-effectiveness ratios for the non-dominated strategies
#'
#' The cost-effectiveness frontier can be visualized with \code{plot}, which calls \code{\link{plot.icers}}.
#'
#' An efficent way to get from a probabilistic sensitivity analysis to an ICER table
#' is by using \code{summary} on the PSA object and then using its columns as
#' inputs to \code{calculate_icers}.
#'
#' @param cost vector of cost for each strategy
#' @param effect vector of effect for each strategy
#' @param strategies string vector of strategy names
#' With the default (NULL), there is no reference strategy, and the strategies
#' are ranked in ascending order of cost.
#'
#' @return A data frame and \code{icers} object of strategies and their associated
#' status, incremental cost, incremental effect, and ICER.
#'
#' @seealso \code{\link{plot.icers}}
#'
#' @examples
#' ## Base Case
#' # if you have a base case analysis, can use calculate_icers on that
#' data(hund_strat)
#' hund_icers <- calculate_icers(hund_strat$Cost,
#'                               hund_strat$QALYs,
#'                               hund_strat$Strategy)
#'
#' plot(hund_icers)
#' # we have so many strategies that we may just want to plot the frontier
#' plot(hund_icers, plot_frontier_only = TRUE)
#' # see ?plot.icers for more options
#'
#' ## Using a PSA object
#' data(psa_cdiff)
#'
#' # summary() gives mean cost and effect for each strategy
#' sum_cdiff <- summary(psa_cdiff)
#'
#' # calculate icers
#' icers <- calculate_icers(sum_cdiff$meanCost,
#'                          sum_cdiff$meanEffect,
#'                          sum_cdiff$Strategy)
#' icers
#'
#' # visualize
#' plot(icers)
#'
#' # by default, only the frontier is labeled
#' # if using a small number of strategies, you can label all the points
#' # note that longer strategy names will get truncated
#' plot(icers, label = "all")
#' @export
calculate_icers <- function(cost, effect, strategies) {
  # checks on input
  n_cost <- length(cost)
  n_eff <- length(effect)
  n_strat <- length(strategies)
  if (n_cost != n_eff | n_eff != n_strat) {
    stop("cost, effect, and strategies must all be vectors of the same length", call. = FALSE)
  }
  
  # coerce to character, in case they are provided as numeric
  char_strat <- as.character(strategies)
  
  # create data frame to hold data
  df <- data.frame("Strategy" = char_strat,
                   "Cost" = cost,
                   "Effect" = effect,
                   stringsAsFactors = FALSE)
  nstrat <- nrow(df)
  
  # if only one strategy was provided, return df with NAs for incremental
  if (nstrat == 1) {
    df[, c("ICER", "Inc_Cost", "Inc_Effect")] <- NA
    return(df)
  }
  
  # three statuses: dominated, extended dominated, and non-dominated
  d <- NULL
  
  # detect dominated strategies
  # dominated strategies have a higher cost and lower effect
  df <- df %>%
    arrange(.data$Cost, desc(.data$Effect))
  
  # iterate over strategies and detect (strongly) dominated strategies
  # those with higher cost and equal or lower effect
  for (i in 1:(nstrat - 1)) {
    ith_effect <- df[i, "Effect"]
    for (j in (i + 1):nstrat) {
      jth_effect <- df[j, "Effect"]
      if (jth_effect <= ith_effect) {
        # append dominated strategies to vector
        d <- c(d, df[j, "Strategy"])
      }
    }
  }
  
  # detect weakly dominated strategies (extended dominance)
  # this needs to be repeated until there are no more ED strategies
  ed <- vector()
  continue <- TRUE  # ensure that the loop is run at least once
  while (continue) {
    # vector of all dominated strategies (strong or weak)
    dom <- union(d, ed)
    
    # strategies declared to be non-dominated at this point
    nd <- setdiff(strategies, dom)
    
    # compute icers for nd strategies
    nd_df <- df[df$Strategy %in% nd, ] %>%
      compute_icers()
    
    # number non-d
    n_non_d <- nrow(nd_df)
    
    # if only two strategies left, we're done
    if (n_non_d <= 2) {
      break
    }
    
    # strategy identifiers for non-d
    nd_strat <- nd_df$Strategy
    
    # now, go through non-d strategies and detect any
    # with higher ICER than following strategy
    ## keep track of whether any ED strategies are picked up
    # if not, we're done - exit the loop
    new_ed <- 0
    for (i in 2:(n_non_d - 1)) {
      if (nd_df[i, "ICER"] > nd_df[i + 1, "ICER"]) {
        ed <- c(ed, nd_strat[i])
        new_ed <- new_ed + 1
      }
    }
    if (new_ed == 0) {
      continue <- FALSE
    }
  }
  
  # recompute icers without weakly dominated strategies
  nd_df_icers <- nd_df[!(nd_df$Strategy %in% dom), ] %>%
    mutate(Status = "ND") %>%
    compute_icers()
  
  # dominated and weakly dominated
  d_df <- df[df$Strategy %in% d, ] %>%
    mutate(ICER = NA, Status = "D")
  
  ed_df <- df[df$Strategy %in% ed, ] %>%
    mutate(ICER = NA, Status = "ED")
  
  # when combining, sort so we have ref,ND,ED,D
  results <- bind_rows(d_df, ed_df, nd_df_icers) %>%
    arrange(desc(.data$Status), .data$Cost, desc(.data$Effect))
  
  # re-arrange columns
  results <- results %>%
    select(.data$Strategy, .data$Cost, .data$Effect,
           .data$Inc_Cost, .data$Inc_Effect, .data$ICER, .data$Status)
  
  # declare class of results
  class(results) <- c("icers", "data.frame")
  return(results)
}

#' Calculate incremental cost-effectiveness ratios from a \code{psa} object.
#'
#' @description The mean costs and QALYs for each strategy in a PSA are used
#' to conduct an incremental cost-effectiveness analysis. \code{\link{calculate_icers}} should be used
#' if costs and QALYs for each strategy need to be specified manually, whereas \code{calculate_icers_psa}
#' can be used if mean costs and mean QALYs from the PSA are assumed to represent a base case scenario for
#' calculation of ICERS.
#'
#' Optionally, the \code{uncertainty} argument can be used to provide the 2.5th and 97.5th
#' quantiles for each strategy's cost and QALY outcomes based on the variation present in the PSA.
#' Because the dominated vs. non-dominated status and the ordering of strategies in the ICER table are
#' liable to change across different samples of the PSA, confidence intervals are not provided for the
#' incremental costs and QALYs along the cost-effectiveness acceptability frontier.
#' \code{link{plot.psa}} does not show the confidence intervals in the resulting plot
#' even if present in the ICER table.
#'
#' @param psa \code{psa} object from \code{link{make_psa_object}}
#' @param uncertainty whether or not 95% quantiles for the cost and QALY outcomes should be included
#' in the resulting ICER table. Defaults to \code{FALSE}.
#'
#' @return A data frame and \code{icers} object of strategies and their associated
#' status, cost, effect, incremental cost, incremental effect, and ICER. If \code{uncertainty} is
#' set to \code{TRUE}, four additional columns are provided for the 2.5th and 97.5th quantiles for
#' each strategy's cost and effect.
#' @seealso \code{\link{plot.icers}}
#' @seealso \code{\link{calculate_icers}}
#' @importFrom tidyr pivot_longer
#' @export
calculate_icers_psa <- function(psa, uncertainty = FALSE) {
  
  # check that psa has class 'psa'
  check_psa_object(psa)
  
  # Calculate mean outcome values
  psa_sum <- summary(psa)
  
  # Supply mean outcome values to calculate_icers
  icers <- calculate_icers(cost = psa_sum$meanCost,
                           effect = psa_sum$meanEffect,
                           strategies = psa_sum$Strategy)
  
  if (uncertainty == TRUE) {
    
    # extract cost and effect data.frames from psa object
    cost <- psa$cost
    effect <- psa$effectiveness
    
    # Calculate quantiles across costs and effects
    cost_bounds <- cost %>%
      pivot_longer(cols = everything(), names_to = "Strategy") %>%
      group_by(.data$Strategy) %>%
      summarize(Lower_95_Cost = quantile(.data$value, probs = 0.025, names = FALSE),
                Upper_95_Cost = quantile(.data$value, probs = 0.975, names = FALSE))
    
    effect_bounds <- effect %>%
      pivot_longer(cols = everything(), names_to = "Strategy") %>%
      group_by(.data$Strategy) %>%
      summarize(Lower_95_Effect = quantile(.data$value, probs = 0.025, names = FALSE),
                Upper_95_Effect = quantile(.data$value, probs = 0.975, names = FALSE))
    
    # merge bound data.frames into icers data.frame
    icers <- icers %>%
      left_join(cost_bounds, by = "Strategy") %>%
      left_join(effect_bounds, by = "Strategy") %>%
      select(.data$Strategy, .data$Cost, .data$Lower_95_Cost, .data$Upper_95_Cost,
             .data$Effect, .data$Lower_95_Effect, .data$Upper_95_Effect,
             .data$Inc_Cost, .data$Inc_Effect, .data$ICER, .data$Status)
  }
  
  return(icers)
}

#' compute icers for non-dominated strategies
#'
#' @param non_d a data frame of non-dominated strategies, with columns
#' "Strategy", "Cost", and "Effect"
#'
#' @return the input dataframe with columns "Inc_Cost",
#' "Inc_Effect", and "ICER" appended
#'
#' @keywords internal
compute_icers <- function(non_d) {
  if (nrow(non_d) > 1) {
    non_d[1, "ICER"] <- NA
    for (i in 2:nrow(non_d)) {
      inc_cost <- (non_d[i, "Cost"] - non_d[i - 1, "Cost"])
      inc_effect <- (non_d[i, "Effect"] - non_d[i - 1, "Effect"])
      non_d[i, "Inc_Cost"] <- inc_cost
      non_d[i, "Inc_Effect"] <- inc_effect
      non_d[i, "ICER"] <- inc_cost / inc_effect
    }
  } else {
    # don't calculate ICER if only one strategy
    non_d[1, c("ICER", "Inc_Cost", "Inc_Effect")] <- NA
  }
  return(non_d)
}

#' Plot of ICERs
#'
#' Plots the cost-effectiveness plane for a ICER object, calculated with \code{\link{calculate_icers}}
#' @param x Object of class \code{icers}.
#' @inheritParams add_common_aes
#' @param currency string. with currency used in the cost-effectiveness analysis (CEA).
#' @param effect_units string. unit of effectiveness
#' @param label whether to label strategies on the efficient frontier, all strategies, or none.
#' defaults to frontier.
#' @param label_max_char max number of characters to label the strategies - if not NULL (the default)
#' longer strategies are truncated to save space.
#' @param plot_frontier_only only plot the efficient frontier
#' @param alpha opacity of points
#' @inheritParams ggrepel::geom_label_repel
#'
#' @return a ggplot2 object which can be modified by adding additional geoms
#'
#' @importFrom stringr str_sub
#' @importFrom ggrepel geom_label_repel
#' @export
plot.icers <- function(x,
                       txtsize = 12,
                       currency = "€",
                       effect_units = "QALYs",
                       label = c("frontier", "all", "none"),
                       label_max_char = NULL,
                       plot_frontier_only = FALSE,
                       alpha = 1,
                       n_x_ticks = 6,
                       n_y_ticks = 6,
                       xbreaks = NULL,
                       ybreaks = NULL,
                       xlim = NULL,
                       ylim = NULL,
                       xexpand = expansion(0.1),
                       yexpand = expansion(0.1),
                       max.iter = 20000,
                       ...) {
  if (ncol(x) > 7) {
    # reformat icers class object if uncertainty bounds are present
    x <- x %>%
      select(.data$Strategy, .data$Cost, .data$Effect,
             .data$Inc_Cost, .data$Inc_Effect,
             .data$ICER, .data$Status)
  }
  
  # type checking
  label <- match.arg(label)
  
  # this is so non-dominated strategies are plotted last (on top)
  x <- arrange(x, .data$Status)
  
  # change status text in data frame for plotting
  d_name <- "Dominated"
  ed_name <- "Weakly Dominated"
  nd_name <- "Efficient Frontier"
  
  status_expand <- c("D" = d_name, "ED" = ed_name,
                     "ND" = nd_name, "ref" = nd_name)
  x$Status <- factor(status_expand[x$Status], ordered = FALSE,
                     levels = c(d_name, ed_name, nd_name))
  
  # linetype
  plot_lines <- c("Dominated" = "blank",
                  "Weakly Dominated" = "blank",
                  "Efficient Frontier" = "solid")
  
  # names to refer to in aes_
  stat_name <- "Status"
  strat_name <- "Strategy"
  eff_name <- "Effect"
  cost_name <- "Cost"
  
  # frontier only
  if (plot_frontier_only) {
    plt_data <- x[x$Status == nd_name, ]
  } else {
    plt_data <- x
  }
  
  # make plot
  icer_plot <- ggplot(plt_data, aes_(x = as.name(eff_name), y = as.name(cost_name),
                                     shape = as.name(stat_name))) +
    geom_point(alpha = alpha, size = 2) +
    geom_line(aes_(linetype = as.name(stat_name), group = as.name(stat_name))) +
    scale_linetype_manual(name = NULL, values = plot_lines) +
    scale_shape_discrete(name = NULL) +
    labs(x = paste0("Effect (", effect_units, ")"),
         y = paste0("Cost (", currency, ")"))
  
  icer_plot <- add_common_aes(icer_plot, txtsize, col = "none",
                              continuous = c("x", "y"),
                              n_x_ticks = n_x_ticks, n_y_ticks = n_y_ticks,
                              xbreaks = xbreaks, ybreaks = ybreaks,
                              xlim = xlim, ylim = ylim,
                              xexpand = xexpand, yexpand = yexpand)
  
  # labeling
  if (label != "none") {
    if (!is.null(label_max_char)) {
      plt_data[, strat_name] <- str_sub(plt_data[, strat_name],
                                        start = 1L, end = label_max_char)
    }
    if (label == "all") {
      lab_data <- plt_data
    }
    if (label == "frontier") {
      lab_data <- plt_data[plt_data$Status == nd_name, ]
    }
    
    icer_plot <- icer_plot +
      ggrepel::geom_label_repel(data = lab_data,
                                aes_(label = as.name(strat_name)),
                                size = 3,
                                show.legend = FALSE,
                                max.iter = max.iter,
                                direction = "both")
  }
  return(icer_plot)
}

#' Create a PSA object
#'
#' @description
#' Creates an object to hold probabilistic sensitivity analysis data,
#' while checking the data for validity. The object can then be
#' used for many standard cost-effectiveness analyses (see Details below).
#'
#' @param parameters Data frame with values for each simulation (rows) and parameter (columns).
#' The column names should be the parameter names.
#' @param cost For the data.frame, each simulation should be a row and each strategy should be a column.
#' Naming the columns of the data frames is not necessary, as they will be renamed with
#' the \code{strategies} vector.
#' @param effectiveness For the data.frame, each simulation should be a row and each strategy should be a column.
#' Naming the columns of the data frames is not necessary, as they will be renamed with
#' the \code{strategies} vector.
#' @param other_outcome data.frame containing values for another user-defined outcome.
#' Each simulation should be a row of the data frame, and each strategy should be a column.
#' Naming the columns of the data frames is not necessary, as they will be renamed with
#' the \code{strategies} vector.
#' @param strategies vector with the names of the strategies. Due to requirements in
#' certain uses of this vector, this function uses \code{\link{make.names}} to modify
#' strategy names as necessary. It is strongly suggested that you follow the rules
#' in the \code{\link{make.names}} help page, to avoid unexpected errors.
#'
#' @param currency symbol for the currency being used (ex. "$", "£")
#'
#' @details
#' The PSA object forms the backbone of one part of the \code{dampack} package.
#'
#' A scatterplot of the cost-effectiveness plane may be shown by running \code{plot}
#' on the output of \code{make_psa_obj}.
#'
#' Using this object, you may calculate:
#' \itemize{
#'   \item Cost-effectiveness acceptability curves (\code{\link{ceac}})
#'   \item Expected value of perfect information (\code{\link{calc_evpi}})
#'   \item Expected loss (\code{\link{calc_exp_loss}})
#'   \item One-way sensitivity analysis (\code{\link{owsa}})
#'   \item Two-way sensitivity analysis (\code{\link{twsa}})
#'   \item Metamodels (\code{\link{metamodel}})
#' }
#'
#' In addition, the PSA may be converted to a base-case analysis by using \code{summary}
#' on the PSA object. The output of \code{summary} can be used in \code{\link{calculate_icers}}.
#'
#'
#' @return An object of class \code{psa}
#'
#' @seealso \code{\link{summary.psa}}, \code{\link{plot.psa}}
#'
#' @examples
#' # psa input provided with package
#' data("example_psa")
#' psa <- make_psa_obj(example_psa$cost, example_psa$effectiveness,
#'                     example_psa$parameters, example_psa$strategies)
#'
#' # custom print and summary methods
#' print(psa)
#' summary(psa)
#'
#' # custom plot method; see ?plot.psa for options
#' plot(psa)
#'
#' @importFrom stringr str_replace
#' @export
make_psa_obj <- function(cost, effectiveness, parameters = NULL,
                         strategies = NULL, currency = "€", other_outcome = NULL) {
  
  # parameter names
  parnames <- names(parameters)
  
  # define psa as a named list
  psa_obj <- create_sa(parameters, parnames, effectiveness, strategies,
                       cost, currency, other_outcome)
  
  # give classes "psa" and "sa"
  class(psa_obj) <- c("psa", class(psa_obj))
  return(psa_obj)
}

check_psa_object <- function(psa) {
  if (!inherits(psa, "psa")) {
    stop(paste0("The psa results parameter must be an object of class `psa`.\n",
                "Please run the make_psa() function to create this object."))
  }
}

check_df_and_coerce <- function(obj) {
  obj_name <- deparse(substitute(obj))
  if (!inherits(obj, "data.frame")) {
    warning(paste0("\'", obj_name, "\'", " is not a data frame. coercing to data frame"))
    df <- as.data.frame(obj)
  } else {
    df <- as.data.frame(obj)
  }
  return(df)
}

#' summarize a psa object across all simulations
#'
#' @param object the psa object
#' @param calc_sds whether or not to calculate the standard deviations. Defaults to FALSE
#' @param ... further arguments to summary (not used)
#'
#' @importFrom stats sd
#' @return a \code{data.frame} containing the mean cost and effectiveness for each strategy and, if requested,
#' the standard deviations of the cost and effectiveness for each strategy.
#' @export
summary.psa <- function(object, calc_sds = FALSE, ...) {
  
  mean_cost <- colMeans(object$cost)
  mean_effect <- colMeans(object$effectiveness)
  strat <- object$strategies
  sum_psa <- data.frame("Strategy" = strat,
                        "meanCost" = mean_cost,
                        "meanEffect" = mean_effect,
                        stringsAsFactors = FALSE)
  if (calc_sds) {
    sd_cost <- apply(object$cost, 2, sd)
    sd_effect <- apply(object$effectiveness, 2, sd)
    sum_psa[, "sdCost"] <- sd_cost
    sum_psa[, "sdEffect"] <- sd_effect
  }
  rownames(sum_psa) <- seq_len(nrow(sum_psa))
  sum_psa
}

#' Plot the psa object
#'
#' @param x the psa object
#' @param center plot the mean cost and effectiveness for each strategy. defaults to TRUE
#' @param ellipse plot an ellipse around each strategy. defaults to TRUE
#' @param alpha opacity of the scatterplot points.
#' 0 is completely transparent, 1 is completely opaque
#' @inheritParams add_common_aes
#'
#' @importFrom ellipse ellipse
#' @import dplyr
#' @import ggplot2
#' @importFrom scales dollar_format
#' @return A \code{ggplot2} plot of the PSA, showing the distribution of each PSA sample and strategy
#' on the cost-effectiveness plane.
#' @importFrom tidyr pivot_longer
#' @export
plot.psa <- function(x,
                     center = TRUE, ellipse = TRUE,
                     alpha = 0.4, txtsize = 12, col = c("full", "bw"),
                     n_x_ticks = 6, n_y_ticks = 6,
                     xbreaks = NULL,
                     ybreaks = NULL,
                     xlim = NULL,
                     ylim = NULL,
                     ...) {
  
  effectiveness <- x$effectiveness
  cost <- x$cost
  strategies <- x$strategies
  currency <- x$currency
  
  # expect that effectiveness and costs have strategy column names
  # removes confusing 'No id variables; using all as measure variables'
  df_cost <- suppressMessages(
    pivot_longer(cost,
                 everything(),
                 names_to = "Strategy",
                 values_to = "Cost")
  )
  df_effect <- suppressMessages(
    pivot_longer(effectiveness,
                 cols = everything(),
                 names_to = "Strategy",
                 values_to = "Effectiveness")
  )
  ce_df <- data.frame("Strategy" = df_cost$Strategy,
                      "Cost" = df_cost$Cost,
                      "Effectiveness" = df_effect$Effectiveness)
  
  # make strategies in psa object into ordered factors
  ce_df$Strategy <- factor(ce_df$Strategy, levels = strategies, ordered = TRUE)
  
  psa_plot <- ggplot(ce_df, aes_string(x = "Effectiveness", y = "Cost", color = "Strategy")) +
    geom_point(size = 0.7, alpha = alpha, shape = 21) +
    ylab(paste("Cost (", currency, ")", sep = ""))
  
  # define strategy-specific means for the center of the ellipse
  if (center) {
    strat_means <- ce_df %>%
      group_by(.data$Strategy) %>%
      summarize(Cost.mean = mean(.data$Cost),
                Eff.mean = mean(.data$Effectiveness))
    # make strategies in psa object into ordered factors
    strat_means$Strategy <- factor(strat_means$Strategy, levels = strategies, ordered = TRUE)
    psa_plot <- psa_plot +
      geom_point(data = strat_means,
                 aes_string(x = "Eff.mean", y = "Cost.mean", fill = "Strategy"),
                 size = 3, shape = 21, color = "black")
  }
  
  if (ellipse) {
    # make points for ellipse plotting
    df_list_ell <- lapply(strategies, function(s) {
      strat_specific_df <- ce_df[ce_df$Strategy == s, ]
      els <-  with(strat_specific_df,
                   ellipse::ellipse(cor(Effectiveness, Cost),
                                    scale = c(sd(Effectiveness), sd(Cost)),
                                    centre = c(mean(Effectiveness), mean(Cost))))
      data.frame(els, group = s, stringsAsFactors = FALSE)
    })
    df_ell <- bind_rows(df_list_ell)
    # draw ellipse lines
    psa_plot <- psa_plot + geom_path(data = df_ell,
                                     aes_string(x = "x", y = "y", colour = "group"),
                                     size = 1, linetype = 2, alpha = 1)
  }
  
  # add common theme
  col <- match.arg(col)
  add_common_aes(psa_plot, txtsize, col = col, col_aes = c("color", "fill"),
                 continuous = c("x", "y"),
                 n_x_ticks = n_x_ticks, n_y_ticks = n_y_ticks,
                 xbreaks = xbreaks, ybreaks = ybreaks,
                 xlim = xlim, ylim = ylim)
}



##modified scatter plot function
inc_scatter <- function(data, wtp_line, txtsize){
  for_scatter <- as.data.frame(matrix(data=data$cost$`Strategy A`-data$cost$`Standard of care`))
  for_scatter$inc.eff <- data$effectiveness$`Strategy A`-data$effectiveness$`Standard of care`
  colnames(for_scatter) <- c("icost", "iqaly")
  
  plotOb <- ggplot(for_scatter, aes(x = iqaly, y = icost)) +
    geom_point(color = "navy", size = 0.9, alpha = 0.5, shape=21) +  # Customize point color and size
    labs(title = "", x = "Incremental QALYs", y = "Incremental costs (Thousand €)") + # Add titles for the plot and axes
    geom_point(data = for_scatter,
               aes_string(x = mean(for_scatter$iqaly), y = mean(for_scatter$icost)),
               size = 3, shape = 21, color = "black", fill="gray") +
    theme_minimal() + 
    theme(title = element_text(face = "bold", size = (txtsize + 2)),
          axis.title.x = element_text(face = "bold", size = txtsize - 1),
          axis.title.y = element_text(face = "bold", size = txtsize - 1),
          axis.text.y = element_text(size = txtsize - 2),
          axis.text.x = element_text(size = txtsize - 2)) +
    coord_cartesian(xlim = c(-max(abs(for_scatter$iqaly)), max(abs(for_scatter$iqaly))), # Set x-axis limits
                    ylim = c(-max(abs(for_scatter$icost)), max(abs(for_scatter$icost)))) +       # Set y-axis limits
    geom_vline(xintercept = 0, color = "black" ) +        # Add vertical line at x = 0
    geom_hline(yintercept = 0, color = "black") +        # Add horizontal line at y = 0
    geom_abline(intercept = 0, slope = wtp_line/1, color = "darkgoldenrod")       # Add diagonal line
  
  return(plotOb)
}


' A generic sensitivity analysis object
#'
#' @description This function is called by \code{\link{make_psa_obj}},
#' \code{\link{create_dsa_oneway}},
#' and \code{\link{create_dsa_oneway}}, and checks the structure of
#' each of the arguments before creating an SA object.
#'
#' @param parameters a data frame with parameter values for each model run. Each
#' column should represent a different parameter, and each row should represent a
#' simulation (in the same order as \code{cost} and \code{effectiveness})
#' @param parnames names for the parameters.
#' @param cost,effectiveness,other_outcome data frames containing data for costs,
#' effectiveness or another outcome (user-defined), respectively.
#' Each simulation should be a row of the data frame, and each strategy should be a column.
#' Naming the columns of the data frames is not necessary, as they will be renamed with
#' the \code{strategies} vector.
#' @param strategies vector with the names of the strategies. Due to requirements in
#' certain uses of this vector, this function uses \code{\link{make.names}} to modify
#' strategy names as necessary. It is strongly suggested that you follow the rules
#' in the \code{\link{make.names}} help page, to avoid unexpected errors.
#'
#' @param currency symbol for the currency being used (ex. "$", "£")
#' @return returns "sa" sensitivity analysis object.
#' @keywords internal
create_sa <- function(parameters, parnames, effectiveness, strategies,
                      cost, currency, other_outcome) {
  # checks that each is a dataframe
  if (!is.null(cost)) {
    cost <- check_df_and_coerce(cost)
  }
  
  if (!is.null(other_outcome)) {
    other_outcome <- check_df_and_coerce(other_outcome)
  }
  
  if (!is.null(effectiveness)) {
    effectiveness <- check_df_and_coerce(effectiveness)
  }
  
  if (!is.null(parameters)) {
    parameters <- check_df_and_coerce(parameters)
  }
  
  ### argument checks and definitions of other variables ###
  
  # costs, effectiveness, and parameters have same number of rows
  n_sim_ls <- list(effectiveness, cost, parameters, other_outcome)
  if (length(unique(unlist(lapply(n_sim_ls[!unlist(lapply(n_sim_ls, is.null))], nrow)))) != 1) {
    stop("Among those provided, the cost, effectiveness, parameter,
         and other_outcome dataframes must all have the same number of rows.")
  }
  
  # define n_sim
  n_sim <- unique(unlist(lapply(n_sim_ls[!unlist(lapply(n_sim_ls, is.null))], nrow)))
  
  # costs and effectiveness have same number of columns (strategies)
  n_strategies_ls <- list(effectiveness, cost, other_outcome)
  if (length(unique(unlist(lapply(n_strategies_ls[!unlist(lapply(n_strategies_ls, is.null))], ncol)))) != 1) {
    stop("Among those provided, the cost, effectiveness,
         and other_outcome dataframes must all have the same number of columns.")
  }
  
  # define n_strategies
  n_strategies <- unique(unlist(lapply(n_strategies_ls[!unlist(lapply(n_strategies_ls, is.null))], ncol)))
  
  # If the strategy names are not provided, generate a generic vector
  # with strategy names
  if (is.null(strategies)) {
    strategies <- paste(rep("Strategy_", n_strategies), seq(1, n_strategies), sep = "")
  } else {
    # correct strategy names. they are used as data.frame column names and in lm()
    # so they need to be syntactically valid
    new_strategies <- make.names(strategies, unique = TRUE)
    
    # write warning to console, so user knows that strategy name was changed
    for (i in 1:n_strategies) {
      old_strat <- strategies[i]
      new_strat <- new_strategies[i]
      if (new_strat != old_strat) {
        warning(paste0("strategy name '", old_strat, "' was converted to '", new_strat,
                       "' for compatibility. See ?make.names"), call. = FALSE)
      }
    }
    # update strategies
    strategies <- new_strategies
    
    # make sure strategies is the same length as the number of columns
    if (n_strategies != length(strategies)) {
      stop(
        paste0("The number of columns in the cost and effectiveness",
               "matrices is different from the number of strategies provided"))
    }
  }
  
  # define cost and effectiveness column names using strategies
  if (!is.null(cost)) {
    names(cost) <- strategies
  }
  if (!is.null(effectiveness)) {
    names(effectiveness) <- strategies
  }
  
  # define sa as a named list
  sa <- list("n_strategies" = n_strategies,
             "strategies" = strategies,
             "n_sim" = n_sim,
             "cost" = cost,
             "effectiveness" = effectiveness,
             "other_outcome" = other_outcome,
             "parameters" = parameters,
             "parnames" = parnames,
             "currency" = currency)
  class(sa) <- "sa"
  return(sa)
}

#' print a psa object
#'
#' @param x the psa object
#' @param all_strat whether or not to print the full list of strategies. defaults to FALSE, which truncates
#' the strategy list to 5
#' @param ... further arguments to print (not used)
#'
#' @return None (invisible NULL).
#' @export
print.sa <- function(x, all_strat = FALSE, ...) {
  xclass <- class(x)
  is_ow_dsa <- "dsa_oneway" %in% xclass
  is_tw_dsa <- "dsa_twoway" %in% xclass
  is_psa <- "psa" %in% xclass
  cat("\n")
  if (is_ow_dsa) {
    cat("One-way Deterministic SA Object", "\n")
  }
  if (is_tw_dsa) {
    cat("Two-way Deterministic SA Object", "\n")
  }
  if (is_psa) {
    cat("PSA object", "\n")
  }
  cat("-------------------------------------------------", "\n")
  
  # cost
  cat("number of strategies (n_strategies):", x$n_strategies, "\n")
  n_trunc <- 5
  if (all_strat | (x$n_strategies <= n_trunc)) {
    s2print <- x$strategies
    msg <- ""
  } else {
    s2print <- c(x$strategies[1:n_trunc], "...")
    msg <- paste("(truncated at", n_trunc, ")")
  }
  s_collapsed <- paste(s2print, collapse = ", ")
  cat("strategies:", s_collapsed, msg, "\n")
  if (is_psa) {
    cat("number of simulations (n_sim):", x$n_sim, "\n")
  }
  cat("cost: a data frame with", nrow(x$cost), "rows and", ncol(x$cost), "columns.", "\n")
  cat("effectiveness: a data frame with",
      nrow(x$effectiveness), "rows and",
      ncol(x$effectiveness), "columns.", "\n")
  cat("parameters: a data frame with",
      nrow(x$parameters), "rows and",
      ncol(x$parameters), "columns", "\n")
  cat("parameter names (parnames): ", paste(x$parnames, collapse = ", "), "\n")
  cat("currency:", x$currency, "\n")
}

#' A function that is used to calculate all outcomes
#'
#' @param outcome choice of outcome
#' @param cost data frame with costs
#' @param effect data frame with effects
#' @param wtp willingness-to-pay threshold
#' @return a data.frame of the desired outcome values for each strategy
#' @keywords internal
calculate_outcome <- function(outcome = c("nhb", "nmb", "eff", "cost", "nhb_loss",
                                          "nmb_loss", "nhb_loss_voi", "nmb_loss_voi"),
                              cost, effect, wtp) {
  outcome <- match.arg(outcome)
  n_sim <- nrow(cost)
  if (outcome == "eff") {
    y <- effect
  } else if (outcome == "cost") {
    y <- cost
  } else {
    if (is.null(wtp)) {
      # the call. = FALSE makes the error message more clear
      stop("wtp must be provided for NHB and NMB",  call. = FALSE)
    }
    if (is.null(cost)) {
      stop("must provide cost for NHB and NMB.",  call. = FALSE)
    }
    if (outcome == "nhb") {
      y <- effect - cost / wtp
    }
    if (outcome == "nmb") {
      y <- effect * wtp - cost
    }
    if (outcome == "nhb_loss" | outcome == "nmb_loss") {
      if (outcome == "nhb_loss") {
        net_outcome <- "nhb"
      }
      if (outcome == "nmb_loss") {
        net_outcome <- "nmb"
      }
      netben <- calculate_outcome(net_outcome, cost, effect, wtp)
      max_str_rowwise <- max.col(netben)
      y <-  netben[cbind(1:n_sim, max_str_rowwise)] - netben
    }
    if (outcome == "nhb_loss_voi" | outcome == "nmb_loss_voi") {
      if (outcome == "nhb_loss_voi") {
        net_outcome <- "nhb"
      }
      if (outcome == "nmb_loss_voi") {
        net_outcome <- "nmb"
      }
      netben <- calculate_outcome(net_outcome, cost, effect, wtp)
      max_str <- which.max(colMeans(netben))
      y <- netben - netben[cbind(1:n_sim), max_str]
    }
  }
  return(y)
}

#' Calculate the expected loss at a range of willingness-to-pay thresholds
#'
#' @description
#' The expected loss is the quantification of the foregone benefits
#' when choosing a suboptimal strategy given current evidence.
#'
#' @param wtp vector of willingness to pay thresholds
#' @param psa object of class \code{psa}, produced by function
#' \code{\link{make_psa_obj}}
#'
#' @details
#' Visualize the expected loss at a variety of WTP thresholds using \code{\link{plot.exp_loss}}.
#'
#' @return object with classes \code{exp_loss} and \code{data.frame}
#'
#' @seealso \code{\link{plot.exp_loss}}, \code{\link{make_psa_obj}}
#'
#' @references
#' \enumerate{
#' \item Alarid-Escudero F, Enns EA, Kuntz KM, Michaud TL, Jalal H.
#' "Time Traveling Is Just Too Dangerous" But Some Methods Are Worth Revisiting:
#' The Advantages of Expected Loss Curves Over Cost-Effectiveness Acceptability
#' Curves and Frontier. Value Health. 2019;22(5):611-618.
#' \item Eckermann S, Briggs A, Willan AR. Health technology assessment in the
#' cost- disutility plane. Med Decis Making. 2008;28(2):172–181.
#' }
#' @examples
#' data("example_psa_obj")
#' wtp <- seq(1e4, 1e5, by = 1e4)
#' exp_loss <- calc_exp_loss(example_psa_obj, wtp)
#'
#' # can use head(), summary(), print(), etc.
#' head(exp_loss)
#'
#' # plot an expected loss curve (ELC)
#' plot(exp_loss)
#'
#' # the y axis is on a log scale by default
#' plot(exp_loss, log_y = FALSE)
#' @importFrom tidyr pivot_longer
#' @export
calc_exp_loss <- function(psa, wtp) {
  check_psa_object(psa)
  cost <- psa$cost
  effectiveness <- psa$effectiveness
  strategies <- psa$strategies
  n_str  <- psa$n_strategies
  exp_loss <- matrix(0, nrow = length(wtp), ncol = n_str)
  for (i in seq_len(length(wtp))) {
    ith_wtp <- wtp[i]
    loss <- calculate_outcome("nmb_loss", cost, effectiveness, ith_wtp)
    exp_loss[i, ] <- colMeans(loss)
  }
  # optimal strategy based on lowest expected loss (max of negative expected loss)
  # this was done because min.col isn't a function
  optimal_str <- max.col(-exp_loss)
  
  # Format expected loss for plotting
  exp_loss_df <- data.frame(wtp, exp_loss, strategies[optimal_str])
  colnames(exp_loss_df) <- c("WTP", strategies, "fstrat")
  
  # Reformat df to long format
  exp_loss_df_melt <- tidyr::pivot_longer(
    data = exp_loss_df,
    cols = !c("WTP", "fstrat"),
    names_to = "Strategy",
    values_to = "Expected_Loss"
  )
  
  # boolean for on frontier or not
  exp_loss_df_melt$On_Frontier <- (exp_loss_df_melt$fstrat == exp_loss_df_melt$Strategy)
  
  # drop fstrat column
  exp_loss_df_melt$fstrat <- NULL
  
  # order by WTP
  exp_loss_df_melt <- exp_loss_df_melt[order(exp_loss_df_melt$WTP), ]
  
  # remove rownames
  rownames(exp_loss_df_melt) <- NULL
  
  # make strategies in exp_loss object into ordered factors
  exp_loss_df_melt$Strategy <- factor(exp_loss_df_melt$Strategy, levels = strategies, ordered = TRUE)
  
  class(exp_loss_df_melt) <- c("exp_loss", "data.frame")
  return(exp_loss_df_melt)
}


#' Plot of Expected Loss Curves (ELC)
#'
#' @param x object of class \code{exp_loss}, produced by function
#'  \code{\link{calc_exp_loss}}
#' @param currency string with currency used in the cost-effectiveness analysis (CEA).
#'  Default: $, but it could be any currency symbol or word (e.g., £, €, peso)
#' @param effect_units units of effectiveness. Default: QALY
#' @param log_y take the base 10 log of the y axis
#' @param frontier indicate the frontier (also the expected value of perfect information).
#' To only plot the EVPI see \code{\link{calc_evpi}}.
#' @param points whether to plot points on the curve (TRUE) or not (FALSE)
#' @param lsize line size. defaults to 1.
#' @inheritParams add_common_aes
#'
#' @return A \code{ggplot2} object with the expected loss
#' @import ggplot2
#' @importFrom scales comma
#' @export
plot.exp_loss <- function(x,
                          log_y = TRUE,
                          frontier = TRUE,
                          points = TRUE,
                          lsize = 1,
                          txtsize = 12,
                          currency = "€",
                          effect_units = "QALY",
                          n_y_ticks = 8,
                          n_x_ticks = 20,
                          xbreaks = NULL,
                          ybreaks = NULL,
                          xlim = c(0, NA),
                          ylim = NULL,
                          col = c("full", "bw"),
                          ...) {
  wtp_name <- "WTP_thou"
  loss_name <- "Expected_Loss"
  strat_name <- "Strategy"
  x[, wtp_name] <- x$WTP / 1000
  
  # split into on frontier and not on frontier
  nofront <- x
  front <- x[x$On_Frontier, ]
  
  # Drop unused levels from strategy names
  nofront$Strategy <- droplevels(nofront$Strategy)
  front$Strategy <- droplevels(front$Strategy)
  # formatting if logging the y axis
  if (log_y) {
    tr <- "log10"
  } else {
    tr <- "identity"
  }
  
  p <- ggplot(data = nofront, aes_(x = as.name(wtp_name),
                                   y = as.name(loss_name))) +
    xlab(paste0("Willingness to Pay (Thousand ", currency, "/", effect_units, ")")) +
    ylab(paste0("Expected Loss (", currency, ")"))
  
  # color
  col <- match.arg(col)
  ## change linetype too if color is black and white
  if (col == "full") {
    if (points) {
      p <- p + geom_point(aes_(color = as.name(strat_name)))
    }
    p <- p +
      geom_line(size = lsize, aes_(color = as.name(strat_name)))
    
  }
  if (col == "bw") {
    if (points) {
      p <- p + geom_point()
    }
    p <- p +
      geom_line(aes_(linetype = as.name(strat_name)))
  }
  
  p <- add_common_aes(p, txtsize, col = col, col_aes = c("color", "line"),
                      continuous = c("x", "y"),
                      n_x_ticks = n_x_ticks, n_y_ticks = n_y_ticks,
                      xbreaks = xbreaks, ybreaks = ybreaks,
                      xlim = xlim, ylim = ylim,
                      ytrans = tr)
  if (frontier) {
    p <- p + geom_point(data = front, aes_(x = as.name(wtp_name),
                                           y = as.name(loss_name),
                                           shape = as.name("On_Frontier")),
                        size = 3, stroke = 1, color = "black") +
      scale_shape_manual(name = NULL, values = 0, labels = "Frontier & EVPI") +
      guides(color = guide_legend(order = 1),
             linetype = guide_legend(order = 1),
             shape = guide_legend(order = 2))
  }
  return(p)
}

#' Expected Value of Perfect Information (EVPI)
#'
#' \code{calc_evpi} is used to compute the expected value of perfect information
#' (EVPI) from a probabilistic sensitivity analysis (PSA) dataset.
#' @param wtp numeric vector with willingness-to-pay (WTP) thresholds
#' @param psa psa object from \code{\link{make_psa_obj}}
#' @param pop scalar that corresponds to the total population
#' @keywords expected value of perfect information; net monetary benefit
#' @section Details:
#' \code{evpi} calculates the value of eliminating all the uncertainty of a
#' cost-effectiveness analysis at each WTP threshold.
#' @return A data frame and \code{evpi} object with the EVPI at each WTP threshold.
#' @seealso \code{\link{plot.evpi}}, \code{\link{make_psa_obj}}
#' @examples
#' # load psa object provided with package
#' data("example_psa_obj")
#'
#' # define wtp threshold vector (can also use a single wtp)
#' wtp <- seq(1e4, 1e5, by = 1e4)
#' evpi <- calc_evpi(example_psa_obj, wtp)
#' plot(evpi) # see ?plot.evpi for options
#'
#' # can use plot options (# see ?plot.evpi for details)
#' plot(evpi, effect_units = "QALE")
#'
#' # or can use ggplot layers
#' plot(evpi) + ggtitle("Expected Value of Perfect Information")
#' @export
calc_evpi <- function(psa, wtp, pop = 1) {
  check_psa_object(psa)
  cost <- psa$cost
  effectiveness <- psa$effectiveness
  if (ncol(effectiveness) < 2) {
    stop("You need at least two different strategies to compute EVPI.")
  }
  # number of wtp thresholds
  n_wtps <- length(wtp)
  # vector to store evpi
  evpi <- rep(0, n_wtps)
  # Estimate the Loss matrix and EVPI at each WTP threshold
  for (l in 1:n_wtps) {
    ## Calculate the opportunity loss from choosing d.star for each strategy
    loss <- calculate_outcome("nmb_loss", cost, effectiveness, wtp[l])
    
    ## Compute EVPI
    evpi[l] <- min(apply(loss, 2, mean)) * pop
  }
  
  # Data frame to store EVPI for each WTP threshold
  df_evpi <- data.frame("WTP" = wtp, "EVPI" = evpi)
  
  # declare class as both evpi (plotting) and data.frame (printing)
  class(df_evpi) <- c("evpi", "data.frame")
  return(df_evpi)
}

#' Plot of Expected Value of Perfect Information (EVPI)
#'
#' @description
#' Plots the \code{evpi} object created by \code{\link{calc_evpi}}.
#'
#' @param x object of class \code{evpi}, produced by function
#'  \code{\link{calc_evpi}}
#' @param currency string with currency used in the cost-effectiveness analysis (CEA).
#'  Default: $, but it could be any currency symbol or word (e.g., £, €, peso)
#' @param effect_units units of effectiveness. Default: QALY
#' @inheritParams add_common_aes
#' @keywords expected value of perfect information
#' @return A \code{ggplot2} plot with the EVPI
#' @seealso \code{\link{calc_evpi}}
#' @import ggplot2
#' @importFrom scales comma
#' @export
plot.evpi <- function(x,
                      txtsize = 12,
                      currency = "€",
                      effect_units = "QALY",
                      n_y_ticks = 8,
                      n_x_ticks = 20,
                      xbreaks = NULL,
                      ybreaks = NULL,
                      xlim = c(0, NA),
                      ylim = NULL,
                      ...) {
  x$WTP_thou <- x$WTP / 1000
  g <- ggplot(data = x,
              aes_(x = as.name("WTP_thou"), y = as.name("EVPI"))) +
    geom_line() +
    xlab(paste("Willingness to Pay (Thousand ", currency, "/", effect_units, ")", sep = "")) +
    ylab(paste("EVPI (", currency, ")", sep = ""))
  add_common_aes(g, txtsize, continuous = c("x", "y"),
                 n_x_ticks = n_x_ticks, n_y_ticks = n_y_ticks,
                 xbreaks = xbreaks, ybreaks = ybreaks,
                 xlim = xlim, ylim = ylim)
}

#' Adds aesthetics to all plots to reduce code duplication
#'
#' @param gplot a ggplot object
#' @param txtsize base text size
#' @param scale_name how to name scale. Default inherits from variable name.
#' @param col either none, full color, or black and white
#' @param col_aes which aesthetics to modify with \code{col}
#' @param lval color lightness - 0 to 100
#' @param greystart between 0 and 1. used in greyscale only. smaller numbers are lighter
#' @param greyend between 0 and 1, greater than greystart.
#' @param continuous which axes are continuous and should be modified by this function
#' @param n_x_ticks,n_y_ticks number of axis ticks
#' @param xbreaks,ybreaks vector of axis breaks.
#' will override \code{n_x_ticks} and/or \code{n_y_ticks} if provided.
#' @param facet_lab_txtsize text size for plot facet labels
#' @param xlim,ylim vector of axis limits, or NULL, which sets limits automatically
#' @param xtrans,ytrans transformations for the axes. See \code{\link[ggplot2]{scale_continuous}} for details.
#' @param xexpand,yexpand Padding around data. See \code{\link[ggplot2]{scale_continuous}} for details.
#' The default behavior in ggplot2 is \code{expansion(0.05)}. See \code{\link[ggplot2]{expansion}}
#' for how to modify this.
#' @param ... further arguments to plot.
#' This is not used by \code{dampack} but required for generic consistency.
#' @return a \code{ggplot2} plot updated with a common aesthetic
#'
#' @import ggplot2
#' @keywords internal
add_common_aes <- function(gplot, txtsize, scale_name = waiver(),
                           col = c("none", "full", "bw"),
                           col_aes = c("fill", "color"),
                           lval = 50,
                           greystart = 0.2,
                           greyend = 0.8,
                           continuous = c("none", "x", "y"),
                           n_x_ticks = 6,
                           n_y_ticks = 6,
                           xbreaks = NULL,
                           ybreaks = NULL,
                           xlim = NULL,
                           ylim = NULL,
                           xtrans = "identity",
                           ytrans = "identity",
                           xexpand = waiver(),
                           yexpand = waiver(),
                           facet_lab_txtsize = NULL,
                           ...) {
  p <- gplot +
    theme_bw() +
    theme(legend.title = element_text(size = txtsize),
          legend.text = element_text(size = txtsize - 3),
          title = element_text(face = "bold", size = (txtsize + 2)),
          axis.title.x = element_text(face = "bold", size = txtsize - 1),
          axis.title.y = element_text(face = "bold", size = txtsize - 1),
          axis.text.y = element_text(size = txtsize - 2),
          axis.text.x = element_text(size = txtsize - 2),
          strip.text.x = element_text(size = facet_lab_txtsize),
          strip.text.y = element_text(size = facet_lab_txtsize))
  
  col <- match.arg(col)
  col_aes <- match.arg(col_aes, several.ok = TRUE)
  if (col == "full") {
    if ("color" %in% col_aes) {
      p <- p +
        scale_color_discrete(name = scale_name, l = lval,
                             aesthetics = "color",
                             drop = FALSE)
    }
    if ("fill" %in% col_aes) {
      p <- p +
        scale_fill_discrete(name = scale_name, l = lval,
                            aesthetics = "fill",
                            drop = FALSE)
    }
  }
  if (col == "bw") {
    if ("color" %in% col_aes) {
      p <- p +
        scale_color_grey(name = scale_name, start = greystart, end = greyend,
                         aesthetics = "color",
                         drop = FALSE)
    }
    if ("fill" %in% col_aes) {
      p <- p +
        scale_fill_grey(name = scale_name, start = greystart, end = greyend,
                        aesthetics = "fill",
                        drop = FALSE)
    }
  }
  
  # axes and axis ticks
  continuous <- match.arg(continuous, several.ok = TRUE)
  
  if ("x" %in% continuous) {
    if (!is.null(xbreaks)) {
      xb <- xbreaks
    } else {
      xb <- number_ticks(n_x_ticks)
    }
    p <- p +
      scale_x_continuous(breaks = xb,
                         labels = labfun,
                         limits = xlim,
                         trans = xtrans,
                         expand = xexpand)
  }
  if ("y" %in% continuous) {
    if (!is.null(ybreaks)) {
      yb <- ybreaks
    } else {
      yb <- number_ticks(n_y_ticks)
    }
    p <- p +
      scale_y_continuous(breaks = yb,
                         labels = labfun,
                         limits = ylim,
                         trans = ytrans,
                         expand = yexpand)
  }
  return(p)
}

#' used to automatically label continuous scales
#' @keywords internal
#' @param x axis breaks
#' @return  a character vector giving a label for each input value
labfun <- function(x) {
  if (any(x > 999, na.rm = TRUE)) {
    scales::comma(x)
  } else {
    x
  }
}

#' Number of ticks for \code{ggplot2} plots
#'
#' Function for determining number of ticks on axis of \code{ggplot2} plots.
#' @param n integer giving the desired number of ticks on axis of
#' \code{ggplot2} plots. Non-integer values are rounded down.
#' @section Details:
#' Based on function \code{pretty}.
#' @return a vector of axis-label breaks
#' @export
number_ticks <- function(n) {
  function(limits) {
    pretty(limits, n + 1)
  }
}

# Function that returns the lower case name of the operating system we’re running on
# Source: https://www.r-bloggers.com/identifying-the-os-from-r/
get_os <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}

#---------------------------------------------------# 
##### Function to summarize posterior output  #######
#---------------------------------------------------# 
data_summary <- function(data, varname, groupnames){
  #+++++++++++++++++++++++++
  # Function to calculate the mean, standard deviation and 95% Credible Interval
  # for each group
  # Source: http://www.sthda.com/english/wiki/ggplot2-error-bars-quick-start-guide-r-software-and-data-visualization
  #+++++++++++++++++++++++++
  # data : a data frame
  # varname : the name of a column containing the variable
  #to be summariezed
  # groupnames : vector of column names to be used as
  # grouping variables
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm = TRUE),
      median = quantile(x[[col]], probs = 0.5, names = FALSE),
      sd = sd(x[[col]], na.rm = TRUE),
      lb = quantile(x[[col]], probs = 0.025, names = FALSE),
      ub = quantile(x[[col]], probs = 0.975, names = FALSE))
  }
  data_sum <- ddply(data, groupnames, .fun = summary_func, 
                    varname)
  data_sum <- plyr::rename(data_sum, c("mean" = varname))
  return(data_sum)
}

### PAID3 costs in added life years ----
get_paid <- function(intervention, control){
  #load data
  ref_data_PAID <- data.frame(
    Age =	c(30, 	31,	32,	33,	34,	35,	36,	37,	38,	39,	40,	41,	42,	43,	44,	45,	46,	47,	48,	49,	50,	51,	52,	53,	54,	55,	56,	57,	58,	59,	60,	61,	62,	63,	64,	65,	66,	67,	68,	69,	70,	71,	72,	73,	74,	75,	76,	77,	78,	79,	80,	81,	82,	83,	84,	85,	86,	87,	88,	89,	90,	91,	92,	93,	94,	95,	96,	97,	98,	99),
    Last.year.of.life.men =	c(18752, 	19313,	19921,	20594,	21341,	22146,	22991,	23859,	24739,	25629,	26534,	27467,	28455,	29525,	30693,	31980,	33401,	34973,	36702,	38553,	40375,	42075,	43585,	44908,	46062,	47099,	48065,	48928,	49610,	50007,	50092,	49995,	49870,	49776,	49709,	49610,	49433,	49149,	48778,	48359,	47904,	47429,	46983,	46610,	46339,	46112,	45824,	45379,	44755,	44090,	43512,	43168,	43085,	43172,	43347,	43509,	43648,	43837,	44257,	45020,	46143,	47598,	49307,	51094,	52828,	54474,	55716,	52653,	49340,	45894),
    Last.year.of.life.women =	c(33736, 	34782,	35474,	35834,	35976,	36013,	36030,	36111,	36313,	36654,	37146,	37835,	38794,	40031,	41480,	43050,	44725,	46539,	48582,	50880,	53161,	55183,	56695,	57659,	58201,	58509,	58751,	58880,	58774,	58300,	57452,	56465,	55585,	54909,	54399,	53927,	53372,	52661,	51821,	50932,	50083,	49346,	48766,	48374,	48177,	48148,	48236,	48390,	48623,	48997,	49556,	50348,	51402,	52631,	53913,	55112,	56154,	57072,	58078,	59320,	60859,	62708,	64805,	66973,	69098,	71260,	73033,	69275,	64866,	59905),
    Other.years.of.life.men =	c(2584, 	2599,	2618,	2641,	2665,	2689,	2711,	2730,	2747,	2764,	2783,	2807,	2838,	2879,	2927,	2981,	3041,	3106,	3176,	3250,	3324,	3399,	3474,	3549,	3626,	3707,	3795,	3891,	3995,	4106,	4222,	4342,	4466,	4594,	4729,	4870,	5020,	5180,	5354,	5548,	5767,	6018,	6306,	6635,	7005,	7412,	7851,	8315,	8806,	9346,	9962,	10682,	11533,	12512,	13589,	14735,	15927,	17166,	18487,	19940,	21551,	23341,	25314,	27435,	29704,	32126,	34910,	35660,	36927,	38972),
    Other.years.of.life.women =	c(3768, 	3806,	3811,	3785,	3733,	3662,	3577,	3483,	3391,	3306,	3238,	3194,	3182,	3200,	3242,	3300,	3369,	3443,	3522,	3600,	3678,	3751,	3817,	3878,	3935,	3991,	4050,	4115,	4187,	4265,	4350,	4442,	4546,	4663,	4795,	4939,	5095,	5263,	5448,	5657,	5903,	6195,	6544,	6956,	7433,	7977,	8590,	9273,	10041,	10920,	11937,	13118,	14488,	16038,	17721,	19493,	21312,	23155,	25050,	27037,	29148,	31410,	33835,	36400,	39136,	42073,	45477,	46367,	47603,	49303)
  )
  
  v_dwc_tmp  <- 1 / ((1 + (d_c * cycle_length)) ^ (0:n_cycles))

  #get only necessary values depending on n_cycles
  ref_data_PAID[nrow(ref_data_PAID)+1,] <- ref_data_PAID[nrow(ref_data_PAID),] #copy last row to enable max age = 100
  ref_data_PAID$Age[nrow(ref_data_PAID)] = 100
  ref_data_PAID <- subset(ref_data_PAID, Age >= n_age_init-1 & Age <= n_age_max)

  v_PAID_values_other_men <- ref_data_PAID$Other.years.of.life.men*1.24 # 1.24 = CPI adjustment from 2017-2023
  v_PAID_values_last_men <- ref_data_PAID$Last.year.of.life.men*1.24 # 1.24 = CPI adjustment from 2017-2023
  v_PAID_values_other_women <- ref_data_PAID$Other.years.of.life.women*1.24 # 1.24 = CPI adjustment from 2017-2023
  v_PAID_values_last_women <- ref_data_PAID$Last.year.of.life.women*1.24 # 1.24 = CPI adjustment from 2017-2023
  
  df_unrelated_strA <- intervention 
  df_unrelated_SOC <- control 
  
  df_unrelated_strA[,"0"] <- v_PAID_values_other_men[1]*prop_male + v_PAID_values_other_women[1]*(1-prop_male)
  df_unrelated_SOC[,"0"] <- v_PAID_values_other_men[1]*prop_male + v_PAID_values_other_women[1]*(1-prop_male)
  
  for(col in 2:(n_cycles+1)){
    df_unrelated_strA[,col] <- (intervention[,col]*v_PAID_values_other_men[col]*prop_male + ((intervention[,col-1] - intervention[,col])*prop_male)*v_PAID_values_last_men[col] +
      intervention[,col]*v_PAID_values_other_women[col]*(1-prop_male) + ((intervention[,col-1] - intervention[,col])*(1-prop_male))*v_PAID_values_last_women[col])*v_dwc_tmp[col]
    
    df_unrelated_SOC[,col] <- (control[,col]*v_PAID_values_other_men[col]*prop_male + ((control[,col-1] - control[,col])*prop_male)*v_PAID_values_last_men[col] +
      control[,col]*v_PAID_values_other_women[col]*(1-prop_male) + ((control[,col-1] - control[,col])*(1-prop_male))*v_PAID_values_last_women[col])*v_dwc_tmp[col]
  }
  
  v_PAID_discounted_intervention <- rowSums(df_unrelated_strA)
  v_PAID_discounted_control <- rowSums(df_unrelated_SOC)

  return(list(intervention = v_PAID_discounted_intervention,
              control = v_PAID_discounted_control))
}


### Repeatable Rgens for reproducibility ----
rnormA <- function(n, mean, sd){
  set.seed(seed)
  rnorm_out <- rnorm(n=n, mean=mean, sd=sd)
  return(rnorm_out)
}

rtriA <- function(n, mode, min, max){
  set.seed(seed)
  rtri_out <- rtri(n=n, mode=mode, min=min, max=max)
  return(rtri_out)
} 


rbetaA <- function(n, mean, sd){
  set.seed(seed)
  alpha <- (mean ^ 2 * (1 - mean)/sd ^ 2) - mean
  beta <- (alpha / mean) - alpha
  rbeta_out <- rbeta(n=n, shape1=alpha, shape2=beta)
  return(rbeta_out)
} 

rgammaA <- function(n,mean,sd){
  set.seed(seed)
  alpha <- (mean/sd)^2
  beta <- (sd^2)/mean
  rgamma_out <- rgamma(n=n, shape=alpha, scale=beta)
  return(rgamma_out)
}



### function for workload reductions ----
fun_efficiency_gains <- function(controlled_prop,
                                 uncontrolled_prop,
                                 EG_duration_GP_long,
                                 EG_duration_GP_short,
                                 EG_duration_POH_long,
                                 EG_duration_POH_short,
                                 cvd_pop_catchment, 
                                 n_gp_ref,
                                 n_POH_ref
                                 ){
  
  # Create the dataframe
  df_usage <- data.frame(
    strategy = c(rep("Intervention", 3), rep("SOC", 3)),
    measure = rep(c("number of long contacts per patient per year",
                    "number of short contacts per patient per year",
                    "number of remote consultations per patient per year"), 2),
    GP_controlled = c(0.06, 0.07, 0.03, 0.05, 0.08, 0.04),
    GP_uncontrolled = c(0.06, 0.08, 0.03, 0.06, 0.12, 0.05),
    POH_controlled = c(1.09, 1.25, 0.6, 1.03, 1.6, 0.67),
    POH_uncontrolled = c(1.2, 1.53, 0.6, 1.1, 2.32, 0.90)
  )
  
      tot_GP_long_intervention_controlled <- (cvd_pop_catchment*controlled_prop*(df_usage$GP_controlled[df_usage$strategy=="Intervention" & df_usage$measure == "number of long contacts per patient per year"]*(EG_duration_GP_long/60)))/n_gp_ref
      tot_GP_long_soc_controlled <-  (cvd_pop_catchment*controlled_prop*(df_usage$GP_controlled[df_usage$strategy=="SOC" & df_usage$measure == "number of long contacts per patient per year"]*(EG_duration_GP_long/60)))/n_gp_ref
      diff_GP_long_controlled <- tot_GP_long_intervention_controlled-tot_GP_long_soc_controlled
      
      tot_GP_short_intervention_controlled <- (cvd_pop_catchment*controlled_prop*(df_usage$GP_controlled[df_usage$strategy=="Intervention" & df_usage$measure == "number of short contacts per patient per year"]*(EG_duration_GP_short/60)))/n_gp_ref
      tot_GP_short_soc_controlled <-  (cvd_pop_catchment*controlled_prop*(df_usage$GP_controlled[df_usage$strategy=="SOC" & df_usage$measure == "number of short contacts per patient per year"]*(EG_duration_GP_short/60)))/n_gp_ref
      diff_GP_short_controlled <- tot_GP_short_intervention_controlled-tot_GP_short_soc_controlled
      
      tot_eGP_intervention_controlled <- (cvd_pop_catchment*controlled_prop*df_usage$GP_controlled[df_usage$strategy=="Intervention" & df_usage$measure == "number of remote consultations per patient per year"])/n_gp_ref
      tot_eGP_soc_controlled <-  (cvd_pop_catchment*controlled_prop*df_usage$GP_controlled[df_usage$strategy=="SOC" & df_usage$measure == "number of remote consultations per patient per year"])/n_gp_ref
      diff_eGP_controlled <- tot_eGP_intervention_controlled-tot_eGP_soc_controlled
      
      tot_POH_long_intervention_controlled <- (cvd_pop_catchment*controlled_prop*(df_usage$POH_controlled[df_usage$strategy=="Intervention" & df_usage$measure == "number of long contacts per patient per year"]*(EG_duration_POH_long/60)))/n_POH_ref
      tot_POH_long_soc_controlled <-  (cvd_pop_catchment*controlled_prop*(df_usage$POH_controlled[df_usage$strategy=="SOC" & df_usage$measure == "number of long contacts per patient per year"]*(EG_duration_POH_long/60)))/n_POH_ref
      diff_POH_long_controlled <- tot_POH_long_intervention_controlled-tot_POH_long_soc_controlled
      
      tot_POH_short_intervention_controlled <- (cvd_pop_catchment*controlled_prop*(df_usage$POH_controlled[df_usage$strategy=="Intervention" & df_usage$measure == "number of short contacts per patient per year"]*(EG_duration_POH_short/60)))/n_POH_ref
      tot_POH_short_soc_controlled <-  (cvd_pop_catchment*controlled_prop*(df_usage$POH_controlled[df_usage$strategy=="SOC" & df_usage$measure == "number of short contacts per patient per year"]*(EG_duration_POH_short/60)))/n_POH_ref
      diff_POH_short_controlled <- tot_POH_short_intervention_controlled-tot_POH_short_soc_controlled
      
      tot_ePOH_intervention_controlled <- (cvd_pop_catchment*controlled_prop*df_usage$POH_controlled[df_usage$strategy=="Intervention" & df_usage$measure == "number of remote consultations per patient per year"])/n_POH_ref
      tot_ePOH_soc_controlled <-  (cvd_pop_catchment*controlled_prop*df_usage$POH_controlled[df_usage$strategy=="SOC" & df_usage$measure == "number of remote consultations per patient per year"])/n_POH_ref
      diff_ePOH_controlled <-  tot_ePOH_intervention_controlled - tot_ePOH_soc_controlled
    
      tot_GP_long_intervention_uncontrolled <- (cvd_pop_catchment*uncontrolled_prop*(df_usage$GP_uncontrolled[df_usage$strategy=="Intervention" & df_usage$measure == "number of long contacts per patient per year"]*(EG_duration_GP_long/60)))/n_gp_ref
      tot_GP_long_soc_uncontrolled <-  (cvd_pop_catchment*uncontrolled_prop*(df_usage$GP_uncontrolled[df_usage$strategy=="SOC" & df_usage$measure == "number of long contacts per patient per year"]*(EG_duration_GP_long/60)))/n_gp_ref
      diff_GP_long_uncontrolled <- tot_GP_long_intervention_uncontrolled-tot_GP_long_soc_uncontrolled
      
      tot_GP_short_intervention_uncontrolled <- (cvd_pop_catchment*uncontrolled_prop*(df_usage$GP_uncontrolled[df_usage$strategy=="Intervention" & df_usage$measure == "number of short contacts per patient per year"]*(EG_duration_GP_short/60)))/n_gp_ref
      tot_GP_short_soc_uncontrolled <-  (cvd_pop_catchment*uncontrolled_prop*(df_usage$GP_uncontrolled[df_usage$strategy=="SOC" & df_usage$measure == "number of short contacts per patient per year"]*(EG_duration_GP_short/60)))/n_gp_ref
      diff_GP_short_uncontrolled <- tot_GP_short_intervention_uncontrolled-tot_GP_short_soc_uncontrolled
      
      tot_eGP_intervention_uncontrolled <- (cvd_pop_catchment*uncontrolled_prop*df_usage$GP_uncontrolled[df_usage$strategy=="Intervention" & df_usage$measure == "number of remote consultations per patient per year"])/n_gp_ref
      tot_eGP_soc_uncontrolled <-  (cvd_pop_catchment*uncontrolled_prop*df_usage$GP_uncontrolled[df_usage$strategy=="SOC" & df_usage$measure == "number of remote consultations per patient per year"])/n_gp_ref
      diff_eGP_uncontrolled <- tot_eGP_intervention_uncontrolled-tot_eGP_soc_uncontrolled
      
      tot_POH_long_intervention_uncontrolled <- (cvd_pop_catchment*uncontrolled_prop*(df_usage$POH_uncontrolled[df_usage$strategy=="Intervention" & df_usage$measure == "number of long contacts per patient per year"]*(EG_duration_POH_long/60)))/n_POH_ref
      tot_POH_long_soc_uncontrolled <-  (cvd_pop_catchment*uncontrolled_prop*(df_usage$POH_uncontrolled[df_usage$strategy=="SOC" & df_usage$measure == "number of long contacts per patient per year"]*(EG_duration_POH_long/60)))/n_POH_ref
      diff_POH_long_uncontrolled <- tot_POH_long_intervention_uncontrolled-tot_POH_long_soc_uncontrolled
      
      tot_POH_short_intervention_uncontrolled <- (cvd_pop_catchment*uncontrolled_prop*(df_usage$POH_uncontrolled[df_usage$strategy=="Intervention" & df_usage$measure == "number of short contacts per patient per year"]*(EG_duration_POH_short/60)))/n_POH_ref
      tot_POH_short_soc_uncontrolled <-  (cvd_pop_catchment*uncontrolled_prop*(df_usage$POH_uncontrolled[df_usage$strategy=="SOC" & df_usage$measure == "number of short contacts per patient per year"]*(EG_duration_POH_short/60)))/n_POH_ref
      diff_POH_short_uncontrolled <- tot_POH_short_intervention_uncontrolled-tot_POH_short_soc_uncontrolled
      
      tot_ePOH_intervention_uncontrolled <- (cvd_pop_catchment*uncontrolled_prop*df_usage$POH_uncontrolled[df_usage$strategy=="Intervention" & df_usage$measure == "number of remote consultations per patient per year"])/n_POH_ref
      tot_ePOH_soc_uncontrolled <-  (cvd_pop_catchment*uncontrolled_prop*df_usage$POH_uncontrolled[df_usage$strategy=="SOC" & df_usage$measure == "number of remote consultations per patient per year"])/n_POH_ref
      diff_ePOH_uncontrolled <-  tot_ePOH_intervention_uncontrolled - tot_ePOH_soc_uncontrolled
  
  
  return(list(tot_GP_long_intervention_controlled = round( tot_GP_long_intervention_controlled,2), 
              tot_GP_long_soc_controlled = round( tot_GP_long_soc_controlled,2), 
              diff_GP_long_controlled = round( diff_GP_long_controlled ,2),
              tot_GP_short_intervention_controlled = round(  tot_GP_short_intervention_controlled,2),
              tot_GP_short_soc_controlled = round( tot_GP_short_soc_controlled,2), 
              diff_GP_short_controlled = round( diff_GP_short_controlled,2),
              tot_eGP_intervention_controlled= round(tot_eGP_intervention_controlled,2),
              tot_eGP_soc_controlled= round(tot_eGP_soc_controlled,2),
              diff_eGP_controlled= round(diff_eGP_controlled,2),
              tot_POH_long_intervention_controlled = round( tot_POH_long_intervention_controlled,2), 
              tot_POH_long_soc_controlled = round( tot_POH_long_soc_controlled,2), 
              diff_POH_long_controlled = round( diff_POH_long_controlled ,2),
              tot_POH_short_intervention_controlled = round(  tot_POH_short_intervention_controlled,2),
              tot_POH_short_soc_controlled = round( tot_POH_short_soc_controlled,2), 
              diff_POH_short_controlled = round( diff_POH_short_controlled,2),
              tot_ePOH_intervention_controlled= round(tot_ePOH_intervention_controlled,2),
              tot_ePOH_soc_controlled= round(tot_ePOH_soc_controlled,2),
              diff_ePOH_controlled= round(diff_ePOH_controlled,2),
                               
                               tot_GP_long_intervention_uncontrolled = round( tot_GP_long_intervention_uncontrolled,2), 
                               tot_GP_long_soc_uncontrolled = round( tot_GP_long_soc_uncontrolled,2), 
                               diff_GP_long_uncontrolled = round( diff_GP_long_uncontrolled ,2),
                               tot_GP_short_intervention_uncontrolled = round(  tot_GP_short_intervention_uncontrolled,2),
                               tot_GP_short_soc_uncontrolled = round( tot_GP_short_soc_uncontrolled,2), 
                               diff_GP_short_uncontrolled = round( diff_GP_short_uncontrolled,2),
                               tot_eGP_intervention_uncontrolled= round(tot_eGP_intervention_uncontrolled,2),
                               tot_eGP_soc_uncontrolled= round(tot_eGP_soc_uncontrolled,2),
                               diff_eGP_uncontrolled= round(diff_eGP_uncontrolled,2),
                               tot_POH_long_intervention_uncontrolled = round( tot_POH_long_intervention_uncontrolled,2), 
                               tot_POH_long_soc_uncontrolled = round( tot_POH_long_soc_uncontrolled,2), 
                               diff_POH_long_uncontrolled = round( diff_POH_long_uncontrolled ,2),
                               tot_POH_short_intervention_uncontrolled = round(  tot_POH_short_intervention_uncontrolled,2),
                               tot_POH_short_soc_uncontrolled = round( tot_POH_short_soc_uncontrolled,2), 
                               diff_POH_short_uncontrolled = round( diff_POH_short_uncontrolled,2),
                               tot_ePOH_intervention_uncontrolled= round(tot_ePOH_intervention_uncontrolled,2),
                               tot_ePOH_soc_uncontrolled= round(tot_ePOH_soc_uncontrolled,2),
                               diff_ePOH_uncontrolled= round(diff_ePOH_uncontrolled,2)
              ))
}
