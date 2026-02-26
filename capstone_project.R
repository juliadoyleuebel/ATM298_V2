# when we want to commit changes 
# git add capstone_project.R
# git commit -m "Work on capstone project"
# git push

# read in the data
install.packages("readxl")
library(readxl)
policy_list = read_excel('/Users/jdoyleue/ATM298_V2/data/CARB - Global Policies.xlsx')
policy_list


