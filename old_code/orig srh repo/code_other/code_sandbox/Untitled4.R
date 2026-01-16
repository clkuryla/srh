# Checking EHR outputs

# Upload each 

pt_cat <- read_csv("../../../_Research/all3_categor_CLK.csv")

pt_cat_unique <- pt_cat %>% 
  unique()

pt_cat_agreement <- read_csv("../../../_Research/merged_patient_responses_with_agreement (1).csv")

pt_cat_agreement <- pt_cat_agreement %>% 
  unique() 
  
pt_cat_agreement %>% 
  summarize(n_all3 = count(all_3_agree))

sum(pt_cat_agreement$all_3_agree)
sum(pt_cat_agreement$claude_4o)
sum(pt_cat_agreement$claude_o1pro)
sum(pt_cat_agreement$o1pro_4o)

