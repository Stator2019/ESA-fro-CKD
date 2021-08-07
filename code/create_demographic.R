  
  tmp_chr_no <- 
    sqlQuery(
      ckd_epo_jerry,
      'select distinct chr_no, hospital
        from ckd_epo_jerry.dbo.result_longer;'
    ) %>% 
    setDT(chr_no, hospital)
  
  chr_basic <- 
    