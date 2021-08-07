  dialysis_fee_code <- 
    sqlQuery(
      ckd_fracture_Grace,
      "select * 
            from ckd_fracture_Grace.dbo.dialysis_fee_basic;"
    ) %>% 
    setDT()
  
  oie_longer_cohort <- 
    sqlQuery(
      ckd_epo_jerry,
      'select * from ckd_epo_jerry.dbo.oie_longer_cohort'
      
    ) %>% 
    setDT(key = c('hospital', 'chr_no', 'drug_date'))
  
  dialysis <- 
    map_dfr(
      c('s', 't', 'w'),
      function(hospital){
        map_dfr(
          c('opd', 'ipd'),
          function(dept){
            sqlQuery(
              db_hospital,
              paste0(
                'select distinct fee_no, fee_code, ',
                if_else(dept == 'opd', 'opd_date', 'ipd_date'),
                ' from DB_Hospital.dbo.v_',
                dept,
                '_fee_',
                hospital,
                ' where fee_code in ( ',
                str_c("'", 
                      dialysis_fee_code$FEE_CODE,
                      "'",
                      collapse = ', '),
                ') ;')
            ) %>% 
              filter(
                fee_no %chin% distinct(oie_longer_cohort[fee_no != ''] %>%
                                         filter(data_type == dept),
                                       fee_no)$fee_no
              ) %>%
              mutate(hospital = hospital,
                     data_type = dept)
          }
        )
      }
    ) %>% 
    mutate(drug_date = coalesce(opd_date, ipd_date)) %>% 
    select(-ipd_date, -opd_date) %>% 
    distinct()
  
  dialysis_first_date <- 
    inner_join(
      oie_longer_cohort[fee_no %chin% dialysis$fee_no,] %>% 
        select(-contains('icd')) %>% 
        distinct(),
      dialysis,
      by = c('fee_no' = 'fee_no',
             'hospital' = 'hospital',
             'drug_date' = 'drug_date',
             'data_type' = 'data_type')
    ) %>% 
    distinct() %>% 
    group_by(hospital, chr_no) %>% 
    summarise(
      dialysis_start_date = 
        (min(drug_date) + 19110000) %>% 
        as.character() ,
      dialysis_end_date = 
        (max(drug_date) + 19110000) %>% 
        as.character(),
      freq = sum(!is.na(drug_date))
    ) %>% 
    group_by(.drop = T) %>% 
    mutate(
      diff_month = 
        12*(as.integer(str_sub(dialysis_end_date, 1, 4)) - 
              as.integer(str_sub(dialysis_start_date, 1, 4))) +
        as.integer(str_sub(dialysis_end_date, 5, 6)) -
        as.integer(str_sub(dialysis_start_date, 5, 6))
    )