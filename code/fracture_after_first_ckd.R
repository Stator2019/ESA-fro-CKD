  fracture_icd_code <- 
    sqlQuery(
      ckd_fracture_Grace,
      "select distinct [ICD-9-CM], [ICD-10-CM]
  	      from ckd_fracture_Grace.dbo.fracture_icd_code;"
    ) %>% 
    mutate(`ICD-9-CM` = as.character(`ICD-9-CM`)) %>% 
    setDT()

  oie_longer_cohort <-
    sqlQuery(
      ckd_epo_jerry,
      'select *
        from ckd_epo_jerry.dbo.oie_longer_cohort;'
    ) %>% 
    setDT(key = c('chr_no', 'hospital', 'data_type'))
  
  ckd_first_date <- 
    sqlQuery(
      ckd_epo_jerry,
      'select *
        from ckd_epo_jerry.dbo.ckd_first_date;'
    ) %>% 
    setDT(key = c('chr_no', 'hospital'))
  
  fracture_first_date <- 
    left_join(
      oie_longer_cohort,
      ckd_first_date,
      by = c('hospital' = 'hospital',
             'chr_no' = 'chr_no')
    ) %>% 
    filter(drug_date > ckd_start_date) %>% 
    filter(
      (icd_type == 'icd9_code' & 
         icd_code %chin% fracture_icd_code$`ICD-9-CM`) |
        (icd_type == 'icd10_code' & 
           icd_code %chin% fracture_icd_code$`ICD-10-CM`)
    ) %>% 
    group_by(hospital, chr_no) %>% 
    summarise(
      fracture_start_date = 
        (min(drug_date) + 19110000) %>% 
        as.character() ,
      fracture_end_date = 
        (max(drug_date) + 19110000) %>% 
        as.character(),
      freq = sum(!is.na(drug_date))
    ) %>% 
    group_by(.drop = T) %>% 
    mutate(
      diff_month = 
        12*(as.integer(str_sub(fracture_end_date, 1, 4)) - 
              as.integer(str_sub(fracture_start_date, 1, 4))) +
        as.integer(str_sub(fracture_end_date, 5, 6)) -
        as.integer(str_sub(fracture_start_date, 5, 6))
    ) %>% 
    setDT(key = c('chr_no', 'hospital'))
  