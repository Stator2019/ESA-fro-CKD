
  summary_function <- 
    function(x){
      stat_result <- 
        data.table(
          unique = unique(x) %>% length(),
          range = max(x) - min(x),
          mean = mean(x) %>% round(2),
          std = var(x) %>% sqrt() %>% round(2),
          min = min(x),
          Q5 = quantile(x, 0.05),
          Q10 = quantile(x, 0.1),
          Q25 = quantile(x, 0.25),
          median = quantile(x, 0.5),
          Q75 = quantile(x, 0.75),
          Q90 = quantile(x, 0.9),
          Q95 = quantile(x, 0.95),
          max = max(x)
        )
      
      return(write_xlsx(stat_result,
                        path = 'data/result.xlsx'))
    }
# demographic -------------------------------------------------------------
  correct_chr_no_cohort <- 
    sqlQuery(
      ckd_epo_jerry,
      'select *
        from ckd_epo_jerry.dbo.chr_no_cohort;'
    ) %>% 
    filter(chr_no %chin% chr_no_cohort$chr_no) %>% 
    setDT(key = c('chr_no', 'hospital'))
  
  summary_function(correct_chr_no_cohort$age)


# 585% 586% 次數統計 -----------------------------------------------------------
  ckd_result_longer <- 
    sqlQuery(
      ckd_epo_jerry,
      'select *
        from ckd_epo_jerry.dbo.result_longer'
    )
  
  ckd_stat <- 
    result_longer %>% 
    group_by(hospital, chr_no) %>% 
    summarise(
      icd_freq = sum(!is.na(icd_code))
    )
  
  ckd_stat <- 
    oie_longer_cohort %>%
    filter(
      (icd_type == 'icd9_code' & grepl(icd_code, pattern = '58[56].*')) |
      (icd_type == 'icd10_code' & grepl(icd_code, pattern = 'N18.[4569]|N19'))
    ) %>% 
    group_by(hospital, chr_no) %>% 
    summarise(
      icd_freq = sum(!is.na(icd_code))
    )
  
  
  summary_function(ckd_stat$icd_freq)

# ckd 次數統計 ----------------------------------------------------------------
  ckd_result_longer <- 
    sqlQuery(
      ckd_epo_jerry,
      'select * 
        from ckd_epo_jerry.dbo.result_longer;'
    )


# CKD 後骨折 -----------------------------------------------------------------
  fracture_first_date <- 
    sqlQuery(
      ckd_epo_jerry,
      'select *
        from ckd_epo_jerry.dbo.fracture_first_date;'
    )
  
  summary_function(fracture_first_date$freq)
  
# 洗腎後 fracture 次數統計 -------------------------------------------------------
  oie_longer_cohort <- 
    sqlQuery(
      'select * from ckd_epo_jerry.dbo.oie_longer_cohort',
      ckd_epo_jerry
    )
  
  fracture_after_dialysis <- 
    inner_join(
      oie_longer_cohort[
        (icd_type == 'icd9_code' &
           icd_code %chin% unique(fracture_icd_code$`ICD-9-CM`)) |
        (icd_type == 'icd10_code' &
           icd_code %chin% unique(fracture_icd_code$`ICD-10-CM`)), ],
      dialysis_first_date,
      by = c('hospital' = 'hospital',
             'chr_no' = 'chr_no')
    ) %>% 
    filter(ymd(drug_date + 19110000) > ymd(dialysis_start_date)) %>% 
    select(
      hospital, chr_no, drug_date, icd_code
    )
  
  
  fracture_after_dialysis_result <- 
    fracture_after_dialysis %>% 
    group_by(hospital, chr_no) %>% 
    summarise(
      fracture_freq = n()
    )
  
  summary_function(fracture_after_dialysis_result$fracture_freq)


# dialysis patients  ------------------------------------------------

  summary_function(dialysis_first_date$freq)



# dialysis vintage 年資--------------------------------------------------------
  summary_function(dialysis_first_date$diff_month)

# 用藥次數 --------------------------------------------------------------------

  opd_med_atc <- 
    sqlQuery(
      ckd_epo_jerry,
      'select * from ckd_epo_jerry.dbo.opd_med_atc;'
    ) %>% 
    setDT(key = c('hospital', 'CHR_NO', 'FEE_NO', 
                  'ATC_CODE', 'MED_CODE'))
  
  ipd_med_atc <- 
    sqlQuery(
      ckd_epo_jerry,
      'select * from ckd_epo_jerry.dbo.ipd_med_atc;'
    ) %>% 
    setDT(key = c('hospital', 'CHR_NO', 'FEE_NO', 
                  'ATC_CODE', 'MED_CODE'))
  
  med_atc <- 
    bind_rows(
      opd_med_atc %>% 
        select(hospital,CHR_NO, ATC_CODE) %>% 
        mutate(atc_code = str_sub(ATC_CODE, 1, 5)) %>% 
        select(-ATC_CODE) %>% 
        filter(atc_code != ''),
      ipd_med_atc %>% 
        select(hospital,CHR_NO, ATC_CODE) %>% 
        mutate(atc_code = str_sub(ATC_CODE, 1, 5)) %>% 
        select(-ATC_CODE) %>% 
        filter(atc_code != '')
    ) %>% 
    group_by(hospital, CHR_NO, atc_code) %>%
    mutate(freq = row_number()) %>% 
    summarise(freq = max(freq)) %>% 
    arrange(desc(freq))
  
  med_person <- 
    med_atc %>% 
    group_by(atc_code, .drop = T) %>% 
    summarise(
      count_person = n_distinct(hospital, CHR_NO),
      unique_num = unique(freq) %>% length(),
      range = max(freq, na.rm = T) - min(freq, na.rm = T),
      stat_mean = mean(freq, na.rm = T),
      stat_std = var(freq, na.rm = T) %>% sqrt(),
      stat_min = min(freq, na.rm = T),
      Q5 = quantile(freq, 0.05, na.rm = T),
      Q10 = quantile(freq, 0.1, na.rm = T),
      Q25 = quantile(freq, 0.25, na.rm = T),
      median = quantile(freq, 0.5, na.rm = T),
      Q75 = quantile(freq, 0.75, na.rm = T),
      Q90 = quantile(freq, 0.9, na.rm = T),
      Q95 = quantile(freq, 0.95, na.rm = T),
      stat_max = max(freq, na.rm = T)
    ) %>% 
    arrange(desc(count_person))
  
  write_xlsx(
    med_person,
    'data/med_result.xlsx'
  )
  

# ESA 次數 ------------------------------------------------------------------
  
  esa_type <- 
    esa_used_result %>% 
    group_by(hospital, chr_no, esa_type) %>% 
    mutate(freq = row_number()) %>% 
    summarise(freq = max(freq)) %>% 
    arrange(desc(freq))
  
  esa_person <- 
    esa_type %>% 
    group_by(esa_type) %>% 
    summarise(
      count_person = n_distinct(hospital, chr_no),
      unique_num = unique(freq) %>% length(),
      range = max(freq, na.rm = T) - min(freq, na.rm = T),
      stat_mean = mean(freq, na.rm = T),
      stat_std = var(freq, na.rm = T) %>% sqrt(),
      stat_min = min(freq, na.rm = T),
      Q5 = quantile(freq, 0.05, na.rm = T),
      Q10 = quantile(freq, 0.1, na.rm = T),
      Q25 = quantile(freq, 0.25, na.rm = T),
      median = quantile(freq, 0.5, na.rm = T),
      Q75 = quantile(freq, 0.75, na.rm = T),
      Q90 = quantile(freq, 0.9, na.rm = T),
      Q95 = quantile(freq, 0.95, na.rm = T),
      stat_max = max(freq, na.rm = T)
    ) %>% 
    arrange(desc(count_person))
  
  write_xlsx(
    esa_person,
    'data/esa_person.xlsx'
  )
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  



# 檢驗檢查 --------------------------------------------------------------------
  exper_sign <- 
    sqlQuery(
      ckd_epo_jerry,
      'select *
        from ckd_epo_jerry.dbo.w_lab_results;'
    ) %>% 
    setDT()
  tmp <- 
    bind_rows(
      exper_sign %>% 
        group_by(hospital, chr_no, exam_item) %>% 
        mutate(freq = row_number()) %>% 
        summarise(freq = max(freq)) %>% 
        arrange(desc(freq)),
      labresult_st %>% 
        group_by(hospital, chr_no, exam_item) %>% 
        mutate(freq = row_number()) %>% 
        summarise(freq = max(freq)) %>% 
        arrange(desc(freq))
    )
    
  exam_person <- 
    tmp %>% 
    group_by(exam_item, .drop = T) %>% 
    summarise(
      count_person = n_distinct(hospital, chr_no),
      unique_num = unique(freq) %>% length(),
      range = max(freq, na.rm = T) - min(freq, na.rm = T),
      stat_mean = mean(freq, na.rm = T),
      stat_std = var(freq, na.rm = T) %>% sqrt(),
      stat_min = min(freq, na.rm = T),
      Q5 = quantile(freq, 0.05, na.rm = T),
      Q10 = quantile(freq, 0.1, na.rm = T),
      Q25 = quantile(freq, 0.25, na.rm = T),
      median = quantile(freq, 0.5, na.rm = T),
      Q75 = quantile(freq, 0.75, na.rm = T),
      Q90 = quantile(freq, 0.9, na.rm = T),
      Q95 = quantile(freq, 0.95, na.rm = T),
      stat_max = max(freq, na.rm = T)
    ) %>% 
    arrange(desc(count_person))
  
  write_xlsx(
    exam_person,
    'data/exam_person.xlsx'
  )
    
  