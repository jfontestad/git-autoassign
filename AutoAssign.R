#autoassign.R

###logon and external packages
# install package AzureGraph if not yet installed
if (!require(AzureGraph)) install.packages('AzureGraph')
library(AzureGraph)
if (!require(httpuv)) install.packages('httpuv')
library(httpuv)
if (!require(httr)) install.packages('httr')
library(httr)

if (!exists("me")) {
  gr=create_graph_login(auth_type="device_code")
  me <- gr$get_user()
}

if (!require(jsonlite)) install.packages('jsonlite')
library(jsonlite)
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)
if (!require(bizdays)) install.packages('bizdays')
library(bizdays)
if (!require(functional)) install.packages('functional')
library(functional)
if (!require(gtools)) install.packages('gtools')
library(gtools)
if (!require(stringi)) install.packages('stringi')
library (stringi)
if (!require(lubridate)) install.packages('lubridate')
library(lubridate)

###configuration
#list of plans to be taken into consideration for automated assignment
plans_and_groups = list(c('Tech Invoices 2020','Tech Invoices'), 
                        c('Transformation Assurance','Transformation Assurance'),
                        c('Tech Specific Contracts 2020','Tech Specific Contract'),
                        c('Tech Forecast','Tech Forecast'),
                        c('Tech Order Forms 2020','Tech Order Forms'),
                        c('Tech DS 2020','Tech Delivery Slips'),
                        c('TECH EC Inter-institutional', 'Tech Order Forms'),
                        c('Tech Management Follow up', 'Tech Management Follow up'))

#fixed configuration parameters
user_list = list(user_names = c('AG', 'MN', 'DP', 'ZV', 'BG', 'unassigned'),
                 user_ids = c('3963a97a-43e7-43e7-b71a-a710b7bbc4fa','7900481f-40d8-4e73-868b-6db453b53aa4','0db6b33a-4615-43af-85de-cab76eb2628c','37a32389-1aac-481c-a1fc-e80ab2e5aa34', 'ab4bbfc2-6d90-44f6-b832-86ae7c6464a2', 'NA'))
user_tib = as_tibble(user_list)

priority_list = list(priority_names = c('Low', 'Medium', 'Important', 'Urgent'),
                     priority_ids = c('9','5','3','1'))
priority_tib = as_tibble(priority_list)

category_list = list(categories = c("Problem", "Huge Effort", "High Effort", "ISO ok", "New Message", "Waiting"),
                     category_ids = sapply(seq(6), function(x) paste("category", x, sep="")))
category_tib = as_tibble(category_list)


my_teams = call_graph_url(me$token,"https://graph.microsoft.com/v1.0/me/joinedTeams")

### functions
#read csv file with parameter information
read_parameter_table = function(fileName, teamName) {
  #search for my teams
  TSTA_team = my_teams$value[lapply(my_teams$value, function(x) x$displayName) %in% teamName == TRUE]
  TSTA_team_id = TSTA_team[[1]]$id
  
  # id of the team transformation assurance 58398e3c-899e-431f-99d4-207f15012489
  #call_graph_url(me$token,paste("https://graph.microsoft.com/beta/groups/58398e3c-899e-431f-99d4-207f15012489/drive"))
  group_drive = call_graph_url(me$token,paste("https://graph.microsoft.com/beta/groups/",TSTA_team_id,"/drive", sep=""))
  group_drive_id = group_drive$id
  
  search_csv = call_graph_url(me$token,
                              paste("https://graph.microsoft.com/v1.0/drives/",group_drive_id,"/root/search(q='{",fileName,"}')", sep="")) 
  csv_id = search_csv$value[[1]]$id
  
  #read the file
  csv_param = call_graph_url(me$token,paste("https://graph.microsoft.com/v1.0/drives/",group_drive_id,"/items/",csv_id,"/content", sep=""))
  
  # convert a sequence of unicode hex to a vector or charachters 
  csvchars = chr(csv_param)
  
  # convert a vector of charachters to a string
  csvtable = paste(csvchars, collapse="")
  csvLongStr = toString (csvtable)
  csv_fine_str = stri_enc_toutf8(stri_trans_nfd(csvLongStr)) #this final command eliminates the special charachters
  
  # create a tibble from the csv data read from the file and converted using the previous snippets
  read_csv(csv_fine_str)
}

#read_presence_sheet
# function that reads the TSTA presence sheet; it comes in 
# monthly blocks; one column per team member, one row per day of the month
# months are side to side
read_presence_sheet = function (fileName, teamName) {
  #read the presence matrix
  TSTA_team = my_teams$value[lapply(my_teams$value, function(x) x$displayName) %in% teamName == TRUE]
  TSTA_team_id = TSTA_team[[1]]$id
  
  # id of the team transformation assurance 58398e3c-899e-431f-99d4-207f15012489
  #call_graph_url(me$token,paste("https://graph.microsoft.com/beta/groups/58398e3c-899e-431f-99d4-207f15012489/drive"))
  group_drive = call_graph_url(me$token,paste("https://graph.microsoft.com/beta/groups/",TSTA_team_id,"/drive", sep=""))
  group_drive_id = group_drive$id
  
  search_presence_sheet = call_graph_url(me$token,
                                         paste("https://graph.microsoft.com/v1.0/drives/",group_drive_id,"/root/search(q='{",fileName,"}')", sep="")) 
  presence_sheet_id = search_presence_sheet$value[[1]]$id
  
  presence_data = call_graph_url(me$token,
                                 paste("https://graph.microsoft.com/v1.0/drives/",
                                       group_drive_id,
                                       "/items/",
                                       presence_sheet_id, 
                                       "/workbook/worksheets/LeavePlan/range(address='B37:DZ68')",
                                       sep="")) 
  #presence_data from the excel comes in monthly block, one row per day of the month
  header = presence_data$values[[1]]
  block_col_num = which(sapply(header, function(x) x == "February")) - 1
  team_names = header[2:block_col_num]
  
  #we transpose the presence_data to have one column for each day of the month; one row per team member
  # months are one below the other
  presence_matrix = as_tibble(matrix(unlist(presence_data$values), nrow=length(unlist(presence_data$values[1]))), .name_repair = "minimal")
  
  #function that determines how many days each month has based on the 
  # presence_matrix data
  det_mon_days = function (mon_num) {
    header_row = presence_matrix[(mon_num-1) * block_col_num +1,]
    det_mon_days = max(which(sapply(header_row, function(x) x != "")))-1
  }
  
  #iteration over the blocks to put them side by side so that we obtain
  # a matrix with one row per team member and 365 columns, one for each day of the year
  horizontal_presence_matrix = presence_matrix[2:block_col_num ,1:det_mon_days(1)]
  num_days_in_year = det_mon_days(1)
  
  for (mon_num in seq(11)+1) {
    start_row = ((mon_num - 1) * block_col_num + 2)
    end_row = (mon_num * block_col_num)
    num_days = det_mon_days(mon_num)
    temp = presence_matrix[start_row:end_row,1:num_days + 1]
    horizontal_presence_matrix <- bind_cols(horizontal_presence_matrix, temp)
    num_days_in_year <- num_days_in_year + num_days
  }
  
  colnames(horizontal_presence_matrix) = unlist(list("Member", seq(num_days_in_year-1)))
  
  today_n = yday(today())
  
  #we cut out the portion from today to today+time window for analysis
  presence_decision_tb = as_tibble(cbind(horizontal_presence_matrix[,1], horizontal_presence_matrix[,today_n:(today_n + avail_window-1)]))
  
  colnames(presence_decision_tb) = unlist(list("Member", seq(avail_window)))
  
  #we find out how many leave days in the window by compressing the information
  # to one long string full of X or F (full day leave), P (PM leave) and A (AM leave); 
  # we count those to compute the amount of leave in the time window
  presence_decision_tb %>% 
    unite("pres", 2:avail_window) %>%
    select(c("Member", "pres")) %>%
    mutate(pres = toupper( pres)) %>%
    mutate(full_day_abs_X = str_count(pres, "X")) %>%
    mutate(full_day_abs_F = str_count(pres, "F")) %>%
    mutate(am_abs = str_count(pres, "A")) %>%
    mutate(pm_abs = str_count(pres, "P")) %>%
    mutate(abs_days = full_day_abs_X + full_day_abs_F + 0.5 * am_abs + 0.5 *pm_abs) %>%
    mutate(pres_days = 15 - abs_days ) %>% #todo: 15 comes from 21/7*5 converting calendar days to working days
    select("Member", "pres_days", "abs_days")
}

retrieve_plan_id = function (plan_and_group) {
  plan_name = plan_and_group[[1]]
  group_name = plan_and_group[[2]]
  print("in retrieve_plan_id")
  print(plan_name)
  print(group_name)
  group_id = my_teams$value[unlist(map(my_teams$value, function(x) x$displayName == group_name))][[1]]$id
  plan_data = call_graph_url(me$token,paste("https://graph.microsoft.com/v1.0/groups/",group_id,"/planner/plans",sep=""))
  plan_id = plan_data$value[unlist(map(plan_data$value, function(x) x$title == plan_name))][[1]]$id
  plan_id
}

det_effort = function (category_list) {
  #include only effort categories
  if (length(category_list[category_list %in% 'category2'] ) == 1) {
    'category2'
  } else {
    if (length(category_list[category_list %in% 'category3'] ) == 1) {
      'category3'
    } else {
      'NA'
    }
  }
}

det_problem = function (category_list) {
  if (length(category_list[category_list %in% 'category1'] ) == 1) {
    'category1'
  } else {
    'NA'
  }  
}

det_waiting = function (category_list) {
  if (length(category_list[category_list %in% 'category6'] ) == 1) {
    'category6'
  } else {
    'NA'
  } 
}

#det_new_message
det_new_message = function (category_list) {
  if (length(category_list[category_list %in% 'category5'] ) == 1) {
    'category5'
  } else {
    'NA'
  }  
}

ensure_value = function (uncertain_val) {
  if (is.null(uncertain_val)) {
    'na'
  } else {
    uncertain_val 
  }
}

cut_date_time = function (date_time_string) {
  if (nchar(date_time_string) >= 10) {
    temp_date_time = substring(date_time_string, 1, 10)
  } else {
    temp_date_time = date_time_string
  }    
  #  format(strptime(temp_date_time, format="%Y-%m-%d"), format="%d/%m/%Y")
  strptime(temp_date_time, format="%Y-%m-%d")
}

#det_effort_points
# calls det_effort_points_single for a vector of categories
det_effort_points = function (effort_category_list) {
  unlist(lapply(effort_category_list, det_effort_points_single))
}

#def_effort_points_single
# reads the categories to determine the effort for one 
# category entry
det_effort_points_single = function (effort_category) {
  switch(
    effort_category,
    "Huge Effort" = 4,
    "High Effort" = 2,
    "Medium Effort" = 0,
    "Low Effort" = 0, #low effort is not used anymore
    "NA" = 0)
}

# function that reads all tasks for a plan (identified by its id)
#  and extracts selected columns
extract_task_tbl = function(plan_name_and_id) {
  plan_name = plan_name_and_id[1]
  plan_id = plan_name_and_id[2]
  inv_tasks = call_graph_url(me$token,paste("https://graph.microsoft.com/beta/planner/plans/",plan_id,"/tasks", sep=""))
  num_rows = length(inv_tasks$value)
  #extract assignments (that are nested)
  inv_tasks_assignments = lapply(inv_tasks$value, function(x) x$assignments)
  inv_tasks_assignments_ids = lapply(inv_tasks_assignments, names)
  inv_tasks_assignments_ids = lapply(inv_tasks_assignments_ids, function (xx) if (length(xx) == 0) "NA" else xx[[1]])  
  inv_tasks_categories = lapply(inv_tasks$value, function(x) names(x$appliedCategories))
  inv_tasks_effort = lapply(inv_tasks_categories, function(x) det_effort(x))
  inv_tasks_problem = lapply(inv_tasks_categories, function(x) det_problem(x))
  inv_tasks_waiting = lapply(inv_tasks_categories, function(x) det_waiting(x))
  inv_tasks_new_message = lapply(inv_tasks_categories, function(x) det_new_message(x))
  #compose the initial tibble that contains only the plan name repeated for each row
  tib_inv_tasks = as_tibble(list(plan=rep(plan_name, num_rows)))
  # compose the lists and clean the values (they are all lists and need sapply fun=paste to become char)
  tib_inv_tasks = tib_inv_tasks %>%
    mutate(p_etag = sapply(lapply(inv_tasks$value, function(x) x$'@odata.etag'), FUN=paste)) %>%
    mutate(title=sapply(lapply(inv_tasks$value, function(x) x$title), FUN=paste)) %>%
    mutate(bucketId = sapply(lapply(inv_tasks$value, function(x) x$bucketId), FUN=paste)) %>%
    mutate(createdDateTime = sapply(lapply(inv_tasks$value, function(x) cut_date_time(x$createdDateTime)), FUN=paste)) %>% 
    mutate(startDateTime = sapply(lapply(inv_tasks$value, function(x) cut_date_time(ensure_value(x$startDateTime))), FUN=paste)) %>%
    mutate(dueDateTime = sapply(lapply(inv_tasks$value, function(x) cut_date_time(ensure_value(x$dueDateTime))), FUN=paste)) %>%
    mutate(completedDateTime = sapply(lapply(inv_tasks$value, function(x) cut_date_time(ensure_value(x$completedDateTime))), FUN=paste)) %>%
    mutate(id=sapply(lapply(inv_tasks$value, function(x) x$id), FUN=paste)) %>%
    mutate(priority=sapply(lapply(inv_tasks$value, function(x) x$priority), FUN=paste)) %>%
    mutate(assignments = unlist(inv_tasks_assignments_ids)) %>%
    mutate(effort = sapply(inv_tasks_effort, FUN=paste)) %>%
    mutate(problem = sapply(inv_tasks_problem, FUN=paste)) %>%
    mutate(waiting = sapply(inv_tasks_waiting, FUN=paste)) %>%
    mutate(new_message = sapply(inv_tasks_new_message, FUN=paste))
  
  tib_inv_tasks
}

# list the buckets by ID
extract_buckets = function (plan_name_and_id) {
  plan_name = plan_name_and_id[1]
  plan_id = plan_name_and_id[2]
  buckets = call_graph_url(me$token,paste("https://graph.microsoft.com/v1.0/planner/plans/",plan_id,"/buckets", sep=""))
  num_rows = length(buckets$value)
  tib_buckets = as_tibble(list(b_plan = rep(plan_name, num_rows)))
  tib_buckets = tib_buckets %>%
    mutate(b_name=sapply(lapply(buckets$value, function(x) x$name), FUN=paste)) %>%
    mutate(b_id=sapply(lapply(buckets$value, function(x) x$id), FUN=paste))
}

create_decision_table = function() {
  assigned_tasks <<- tasks_with_bucket_names %>%
    filter(Assigned == "AG" | Assigned == "MN" | Assigned == "DP" | Assigned == "ZV"  | Assigned == "BG") %>%
    filter(Completed == "")
  
  load_matrix <<- assigned_tasks %>%
    select(Assigned, Points) %>%
    filter(!is.na(Points)) %>%
    group_by(Assigned) %>%
    summarise_if(is.numeric, sum) %>%
    rename(Member = Assigned)
  
  #join the tables together
  full_matrix <<- load_matrix %>%
    left_join(skill_matrix,by = c('Member' = 'Member')) %>%
    left_join(availability,by = c('Member' = 'Member')) %>%
    left_join(preferences, by = c('Member' = 'Member', 'TaskType'='Plan'), suffix = c(".skill", ".pref")) %>%
    left_join(presence_sum_tb,by = c('Member' = 'Member')) %>%
    filter (SkillBucket == PrefBucket | SkillBucket == "Other" | PrefBucket == "Other") %>%
    mutate(pref_load = round(Points/(Preference*Availability), 2))
}

det_sub_type_single = function(thePlan, theTitle) {
  relevant_identifiers = sub_type_identifiers %>% filter(Plan == thePlan)
  if (dim(relevant_identifiers)[[1]] > 0) {
    matches = sapply(relevant_identifiers$Keyword , function(x) grepl(x, theTitle, ignore.case = TRUE))
    if (max(matches) > 0) { #at least one keyword was found
      relevant_identifiers$Name[[(which(matches)[[1]])]] #we use the first one
    } else {
      "Other" #no match: category "Other" applies
    }
  } else {
    "Other" #no match: category "Other" applies
  }
}

#this function searches the subtype over a vector
# todo: rewrite using mapply
det_sub_type = function (plan_vec, title_vec) {
  result = c()
  for (count in seq(length(plan_vec))) {
    next_result = det_sub_type_single(plan_vec[[count]], title_vec[[count]]) 
    result = c(result, next_result)
  }
  result
}

update_inv_task = function(p_id, p_etag, new_assignee) {
  print ("going to update and assign")
  print(new_assignee)
  http_body = list(assignments = list (nass = list('@odata.type' = "microsoft.graph.plannerAssignment", orderHint="N9917 U2883!")))
  body_json = toJSON (http_body, auto_unbox = TRUE)
  body_json = gsub("nass", new_assignee, body_json  )
  call_graph_url(me$token,paste ("https://graph.microsoft.com/beta/planner/tasks/",
                                 p_id,
                                 sep=""),
                 add_headers  ("If-Match"=p_etag),
                 body=body_json, 
                 http_verb=c ("PATCH"),encode = "raw")
}

correct_task_data = function(p_id, selected) {
  tasks_with_bucket_names <<- tasks_with_bucket_names %>%
    mutate(Assigned=replace(Assigned, id==p_id, selected))
  create_decision_table()
}

decide_and_update = function (x) {
  p_id = x["id"][[1]]
  Plan = x["Plan"][[1]]
  p_etag = x["p_etag"][[1]]
  Category = x["categ"][[1]]
  Bucket = x["Bucket"][[1]]
  print("*** going to decide for task")
  print(x["Title"][[1]])
  selected = decide_task(p_id, Plan, Bucket, Category)
  if (mode_assign == "prod") {
    if (selected != "") {
      print("going to call update function")
      update_inv_task(p_id, p_etag, (user_tib %>% filter(user_names == selected) %>% select (user_ids))[[1]])
    }
  }
  correct_task_data(p_id, selected)
  print("*** assignment decision")
  print(x["Title"][[1]])
  print(selected)
}

decide_task = function(id, thePlan, theBucket, theCategory) {
  print(paste("Plan=", thePlan, sep=""))
  print(paste("Bucket=", theBucket, sep=""))
  print(paste("Category=", theCategory, sep=""))
  candidates = full_matrix %>%
    filter(TaskType == thePlan) 

  candidates = candidates %>%
    filter(SkillBucket == theBucket | SkillBucket == "Other") 

  candidates = candidates %>%
    filter(PrefBucket == theBucket | PrefBucket == "Other") 
  
  print("candidates after plan and bucket filter")
  print(candidates)
  
  candidates = candidates %>%
    filter(TaskSubType == theCategory | TaskSubType == "Other") 

  print("candidates after category filter")
  print(candidates)
  
  candidates = candidates %>%
    filter(Skill > skill_min) 

  print("candidates after skill filter")
  print(candidates)
  

  candidates = candidates %>%
    filter(abs_days <= absence_max) 

  print("candidates after leave filter")
  print(candidates)
  
  candidates = candidates %>%
    filter(pref_load == min(pref_load))
  
  print("winner")
  print(candidates)
  
  if(dim(candidates)[[1]] > 0) {
    candidates[[1,1]]
  } else {
    "" #problem! nobody available to do the job
  }
}

#allows to exclude and include Plans, Buckets and Categories from automated assignment
det_exclusion_list = function () {
#  auto_assign_control <<- read_parameter_table("AutoAssignControl.csv", "Transformation Assurance")
  print("auto_assign_control")
  print(auto_assign_control)

  exclude_set_precise = unassigned_tasks %>%
    left_join(auto_assign_control, 
              by=c('Plan' = 'Plan', 'Bucket' = 'Bucket', 'categ' = 'Category')) %>%
    filter(Autoassign == "FALSE") %>%
    group_by(id) %>%
    select(id, Autoassign) 
  
  print("exclude_set_precise")
  print(exclude_set_precise)
  
  protect_set_precise = unassigned_tasks %>%
    left_join(auto_assign_control, 
              by=c('Plan' = 'Plan', 'Bucket' = 'Bucket', 'categ' = 'Category')) %>%
    filter(Autoassign == "TRUE") %>%
    group_by(id) %>%
    select(id, Autoassign) 
  
  print("protect_set_precise")
  print(protect_set_precise)
  
  #filter out those excluded by assignment_control based on Plan and Bucket
  exclude_set_bckt =  unassigned_tasks %>%
    left_join(auto_assign_control, 
              by=c('Plan' = 'Plan', 'Bucket' = 'Bucket')) %>%
    filter(Category == "All") %>%
    filter(Autoassign == "FALSE") %>%
    select(id, Autoassign) %>%
    group_by(id)
  
  print("exclude_set_bckt")
  print(exclude_set_bckt)
  
  protect_set_bckt =  unassigned_tasks %>%
    left_join(auto_assign_control, 
              by=c('Plan' = 'Plan', 'Bucket' = 'Bucket')) %>%
    filter(Category == "All") %>%
    filter(Autoassign == "TRUE") %>%
    select(id, Autoassign) %>%
    group_by(id)
  
  print("protect_set_bckt")
  print(protect_set_bckt)
  
  #filter out those excluded by assignment_control based on Plan and Bucket
  exclude_set_cat =  unassigned_tasks %>%
    left_join(auto_assign_control, 
              by=c('Plan' = 'Plan', 'categ' = 'Category')) %>%
    filter(Bucket.y == "All") %>%
    filter(Autoassign == "FALSE") %>%
    select(id, Autoassign) %>%
    group_by(id)
  
  print("exclude_set_cat")
  print(exclude_set_cat)
  
  protect_set_cat =  unassigned_tasks %>%
    left_join(auto_assign_control, 
              by=c('Plan' = 'Plan', 'categ' = 'Category')) %>%
    filter(categ == "All") %>%
    filter(Autoassign == "TRUE") %>%
    select(id, Autoassign) %>%
    group_by(id)
  
  print("protect_set_cat")
  print(protect_set_cat)

  #filter out those excluded by assignment_control based on Plan and Bucket
  exclude_set_plan =  unassigned_tasks %>%
    left_join(auto_assign_control, 
              by=c('Plan' = 'Plan')) %>%
    filter(Bucket.y == "All") %>%
    filter(Category == "All") %>%
    filter(Autoassign == "FALSE") %>%
    select(id, Autoassign) %>%
    group_by(id)
  
  print("exclude_set_plan")
  print(exclude_set_plan)
  
  exclude_set_weak = bind_rows(exclude_set_plan, exclude_set_bckt, exclude_set_cat) %>%
    group_by(id)
  
  print("exclude_set_weak")
  print(exclude_set_weak)
  
  protect_set_total = bind_rows(protect_set_bckt, protect_set_cat, protect_set_precise)
  print("protect_set_total")
  print(protect_set_total)
  
  exclude_set_intermediate = exclude_set_weak %>%
    left_join(protect_set_total, by=('id' = 'id')) %>%
    filter(is.na(Autoassign.y))
  
  print("exclude_set_intermediate")
  print(exclude_set_intermediate)
  
  exclude_set_final = bind_rows(exclude_set_intermediate, exclude_set_precise) %>%
    group_by(id)
  
  print("exclude_set_final")
  print(exclude_set_final)
  
  exclude_set_final
}

###main program
#load the parameter tables
skill_matrix = read_parameter_table("SkillMatrix.csv", "Transformation Assurance")
preferences_and_availability = read_parameter_table("Preferences.csv", "Transformation Assurance")
preferences = preferences_and_availability %>% 
  filter(Plan != "Availability")
availability = preferences_and_availability %>% 
  filter(Plan == "Availability") %>%
  rename(Availability = Preference,AvailBucket = PrefBucket)
sub_type_identifiers = read_parameter_table("TaskTypeIdentification.csv", "Transformation Assurance")
auto_assign_control = read_parameter_table("AutoAssignControl.csv", "Transformation Assurance")

#load the presence sheet
presence_sum_tb = read_presence_sheet("2020_TA_Team_Presence", "Transformation Assurance")

print("finished loading parameter tables; going to read tasks")

#extract tasks from relevant planner plans
planner_plan_ids = map(plans_and_groups, function(x) c(x[1], retrieve_plan_id(x)))

#repeat gathering tasks for each plan in the list planner_plan_ids
integrated_tbl = tibble()
integrated_tbl = map(planner_plan_ids, function(x) extract_task_tbl(x))
integrated_tbl = bind_rows(integrated_tbl)

#gather buckets for each plan
integrated_buckets = tibble()
integrated_buckets = map(planner_plan_ids, function(x) extract_buckets(x))
integrated_buckets = bind_rows(integrated_buckets)

#eliminate the completed tasks to speed up computation
all_tasks_tbl <- integrated_tbl %>%
  filter(completedDateTime == 'NA')

#read the efforts per task based on buckets
tib_param = read_parameter_table("TSTAServicesTaskEfforts", "Transformation Assurance") %>%
  rename(Param_Plan = Plan,
         Param_Bucket = 'Bucket Name')

#read the tasks
tasks_with_bucket_names = all_tasks_tbl %>% left_join(integrated_buckets, by=c("bucketId" = "b_id")) %>%
  left_join(user_tib, by=c('assignments' = 'user_ids')) %>%
  left_join(priority_tib, by=c('priority' = 'priority_ids')) %>%
  left_join(category_tib, by=c('effort' = 'category_ids')) %>%
  left_join(category_tib, by=c('problem' = 'category_ids')) %>%
  left_join(category_tib, by=c('waiting' = 'category_ids')) %>%
  left_join(category_tib, by=c('new_message' = 'category_ids')) %>%
  left_join(tib_param, by=c('plan' = 'Param_Plan', 'b_name' = 'Param_Bucket')) %>%
  mutate(eff_points = Points + det_effort_points(categories.x)) %>%
  mutate(startDateTime = sapply(lapply(startDateTime, function(x) {if (x == "NA") {""} else {x}}), FUN=paste))  %>%   #mutate(dueDateTime = format(dueDateTime, format="%Y-%m-%d")) %>%
  mutate(dueDateTime = sapply(lapply(dueDateTime, function(x) {if (x == "NA") {""} else {x}}), FUN=paste))  %>%   #mutate(dueDateTime = format(dueDateTime, format="%Y-%m-%d")) %>%
  mutate(completedDateTime = sapply(lapply(completedDateTime, function(x) {if (x == "NA") {""} else {x}}), FUN=paste))  %>%   #mutate(dueDateTime = format(dueDateTime, format="%Y-%m-%d")) %>%
  mutate(eff_points = Points + det_effort_points(categories.x)) %>%
  mutate(StepDeadline = "=IFERROR(IF(OFFSET(INDIRECT(ADDRESS(ROW(), COLUMN())),0,-1)<>\"\", WORKDAY.INTL(OFFSET(INDIRECT(ADDRESS(ROW(), COLUMN())),0,-1),ROUNDDOWN(NETWORKDAYS.INTL(OFFSET(INDIRECT(ADDRESS(ROW(), COLUMN())),0,-1),OFFSET(INDIRECT(ADDRESS(ROW(), COLUMN())),0,1))*VLOOKUP(OFFSET(INDIRECT(ADDRESS(ROW(), COLUMN())),0,11),models!$A$1:$D$200,4,FALSE),0)-1),\"\"),\"\")") %>%
  mutate(url = paste("https://tasks.office.com/EFSA815.onmicrosoft.com/en-gb/Home/Task/", id, sep="")) %>%
  mutate(link = paste("=hyperlink(offset(indirect(address(row(), column())), 0, 17),\"Link\")",sep = "")) %>%
  mutate(ModelKey = paste(plan, b_name, sep = "")) %>%
  select(id, p_etag,plan, title, b_name, createdDateTime, startDateTime, StepDeadline, dueDateTime, completedDateTime, priority_names, user_names, categories.x, categories.y, categories.x.x, categories.y.y, eff_points, ModelKey, url,link) %>%
  rename(Plan = plan,
         Title = title,
         Bucket = b_name,
         Created = createdDateTime,
         Start = startDateTime, 
         StepDue = StepDeadline,
         Due = dueDateTime,
         Completed = completedDateTime,
         Priority = priority_names,
         Assigned = user_names,
         Effort = categories.x,
         Problem = categories.y,
         Waiting = categories.x.x,
         NewMessage = categories.y.y,
         Points = eff_points)

print("finished loading tasks; going to prepare decision table")

#use the parameters and the tasks to create the table that 
# gathers all data relevant for decision (load, skill, category, preferences)
create_decision_table()

print("finished to prepare decision table; going to filter the tasks to be assigned")

#select the unassigned tasks to be assigned
unassigned_tasks = tasks_with_bucket_names %>%
  filter(Assigned == "unassigned") %>%
  filter(Completed == "") %>%
  arrange(Due) 

if (unassigned_tasks %>% count() > 0) {
  #add a column with the category that is extracted from the title
  unassigned_tasks = unassigned_tasks %>%
    mutate(categ = det_sub_type(Plan, Title))

  #filter out those excluded by assignment_control exactly
  exclusion_set = det_exclusion_list()
  
  if(dim(exclusion_set)[[1]]>0) {
    unassigned_tasks = unassigned_tasks %>%
      left_join(exclusion_set, by=c('id' = 'id')) %>%
      filter(is.na(Autoassign.x))
  }
  
  if (unassigned_tasks %>% count() > 0) {
    print("finished to filter the tasks to be assigned; going to assign")
    print(unassigned_tasks %>% count())
    
    #assign; after each assignment the load is recomputed for the next decision
    apply(unassigned_tasks, 1, decide_and_update)
  } else {
    print("nothing left to assign")
  }
} else {
  print("nothing left to assign")
  
}

