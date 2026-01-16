#https://wwwn.cdc.gov/nchs/data/nhanes3/1a/adult.dat
#https://wwwn.cdc.gov/nchs/data/nhanes3/1a/adult.sas

library(SAScii)
# nhanes3.tf <- tempfile()
daturl <- "https://wwwn.cdc.gov/nchs/data/nhanes3/1a/adult.dat"
code_url ="https://wwwn.cdc.gov/nchs/data/nhanes3/1a/adult.sas"
# Sas_code <- url(code_url)
# writeLines ( readLines(Sas_code) , con = nhanes3.tf )
# nhanes3.fwf.parameters <- parse.SAScii( nhanes3.tf , beginline = 5 )
# str( nhanes3.fwf.parameters )
# #-----
# 'data.frame':   90 obs. of  4 variables:
#   $ varname: chr  "SEQN" "HYK1A" "HYK1B" "HYK2A" ...
# $ width  : num  5 1 1 2 2 2 2 4 4 2 ...
# $ char   : logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
# $ divisor: num  1 1 1 1 1 1 1 1 1 1 ...
# #------

daturl <- "https://wwwn.cdc.gov/nchs/data/nhanes3/1a/adult.dat"
in.nhanes3 <- read.fwf(daturl, widths=nhanes3.fwf.parameters$width,
                     col.names= nhanes3.fwf.parameters$varname)

in2 <- read.SAScii( daturl, code_url)

write_csv(in2, "big_data/NHANES/nhanes_3/nhanes3.csv")

nhanes3_data <- read_csv("big_data/NHANES/nhanes_3/nhanes3.csv")

nhanes3_selected <- nhanes3_data %>% 
  select(SEQN, 
         DMPFSEQ, 
         HSAGEIR, # age in years
         HAB1, # self-rated health: 1:excellent, very good, good, fair, 5: poor (get rid of 6 and 7)
         HSSEX, # 1 male, 2 female
         SDPPHASE, # 1 1988-1991, 2 1991-1994 
         HSDOIMO, # date of screener (month)
         HSAGEU, # age unit
         HSAITMOR # age in months at interview (screener)
         ) %>% 
  filter(HAB1 %in% 1:5) %>% 
  mutate(age = HSAGEIR,
         sex = ifelse(HSSEX == 1, "Male", "Female"),
         year = ifelse(SDPPHASE == 1, 1989.5, 1992.5),
         srh = 6 - HAB1)

glimpse(nhanes3_selected)

write_csv(nhanes3_selected, "big_data/NHANES/nhanes_3/nhanes3_selected.csv")
