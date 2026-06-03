#cleaning

library(haven)
library(dplyr)
library(stringr)
library(readr)

setwd("")
list.files() %>% dput()

# Get all .sav files in the folder
files <- list.files(pattern = "\\.sav$", full.names = TRUE)

files

# ===============================================
# NSSO Data Cleaning & Merging Workflow
# ===============================================



# -------------------------------
# Step 1: Define needed variables
# -------------------------------
vars_needed <- c(
    # Identification & Weighting
    "ENTID", "State", "State_District",
    "Weight_SC", "Weight_SS", "MLT", "NSC", "NSS",
    
    # Firm Characteristics (Block 2)
    "b2_q201","b2_q202","b2_q203","b2_q204","b2_q205","b2_q207",
    "b2_q210","b2_q211","b2_q212","b2_q213","b2_q214","b2_q215",
    "b2_q216","b2_q217","b2_q218","b2_q219",
    paste0("b2_q",220:246), # b2_q220–b2_q246
    
    # Sector Activity Variables (Block 2 pt1)
    paste0("b2pt1_q",251:264),
    
    # Employment (Block 8)
    "b8_q2", paste0("b8_q",3:12),
    
    # Receipts & Expenses (Blocks 3–6)
    "b3_q4","b4_q2","b4_q3","b5_q2","b5_q3","b5_q4","b6_q2","b6_q3",
    
    # GVA (Block 7)
    "b7_q2","b7_q3",
    
    # Compensation to Workers (Block 9)
    "b9_q2","b9_q3",
    
    # Capital Stock (Block 10 & 10pt1)
    "b10_q3","b10_q4","b10_q5","b10_q6",
    "b10pt1_q1011_3","b10pt1_q1012_3",
    
    # Inventories (Block 13)
    "b13_q2","b13_q3","b13_q4",
    
    # ICT (Block 14)
    "b14_q1423","b14_q1424"
)



library(dplyr)
library(haven)

clean_block <- function(file_name, block_vars) {
    block <- read_sav(file_name) %>%
        select(any_of(block_vars))
    return(block)
}
setwd("")
my_dir <- ""
files <- list.files(my_dir, pattern = "Block 1.*\\.sav$", full.names = TRUE)

block1_clean <- clean_block(
    file_name = files[1],
    block_vars = vars_needed
)

library(dplyr)
library(haven)

# Directory
my_dir <- ""

# Function to extract variables from a block
clean_block <- function(file_path, block_vars) {
    read_sav(file_path) %>%
        select(any_of(block_vars))
}

# Define patterns for each block (adjust if needed)
block_patterns_SR1 <- c(
    "Block 1.*\\.sav$",  # Identifiers
    "Block 2.*\\.sav$",  # Firm characteristics
    "Block 2pt1.*\\.sav$", # Sector activity
    "Block 3.*\\.sav$",  # Principal operating expenses
    "Block 4.*\\.sav$",  # Other operating expenses
    "Block 5.*\\.sav$",  # Principal receipts
    "Block 6.*\\.sav$",  # Other receipts
    "Block 7.*\\.sav$",  # GVA
    "Block 8.*\\.sav$",  # Employment
    "Block 9.*\\.sav$",  # Compensation
    "Block 10.*\\.sav$", # Capital stock
    "Block 10pt1.*\\.sav$", # Capital stock pt1
    "Block 13.*\\.sav$", # Inventories
    "Block 14.*\\.sav$"  # ICT
)

# Loop to read all blocks for SR1
SR1_clean <- lapply(block_patterns_SR1, function(pat) {
    files <- list.files(my_dir, pattern = pat, full.names = TRUE)
    if(length(files) == 0) stop(paste("No file found for pattern:", pat))
    clean_block(files[1], vars_needed)  # Take the first match
})


# Repeat for SR2
block_patterns_SR2 <- sub("Semi-Round-1", "Semi-Round-2", block_patterns_SR1)

SR2_clean <- lapply(block_patterns_SR2, function(pat) {
    files <- list.files(my_dir, pattern = pat, full.names = TRUE)
    if(length(files) == 0) stop(paste("No file found for pattern:", pat))
    clean_block(files[1], vars_needed)
})

# Save cleaned SR1 
names(SR1_clean) <- c("SR1_block1","SR1_block2","SR1_block2pt1","SR1_block3","SR1_block4","SR1_block5","SR1_block6", "SR1_block7","SR1_block8","SR1_block9","SR1_block10","SR1_block10pt1","SR1_block13","SR1_block14")
names(SR2_clean) <- c("SR2_block1","SR2_block2","SR2_block2pt1","SR2_block3","SR2_block4","SR2_block5","SR2_block6", "SR2_block7","SR2_block8","SR2_block9","SR2_block10","SR2_block10pt1","SR2_block13","SR2_block14")


names(SR1_clean)  # should be like Block1, Block2, etc.

#save_dir <- ""

# Directory to save cleaned blocks
save_dir <- ""
if(!dir.exists(save_dir)) dir.create(save_dir)

# Function to save blocks one by one
save_blocks_individually <- function(block_list, prefix) {
    for(i in seq_along(block_list)) {
        block_name <- names(block_list)[i]           # e.g., "SR1_block1"
        block_data <- block_list[[i]]                # the actual data
        
        # Save as RDS
        saveRDS(block_data, 
                file = file.path(save_dir, paste0(prefix, "_", block_name, "_cleaned.rds")))
        
        # Optional: Save as CSV (comment out if not needed)
        #write.csv(block_data, 
        #  file = file.path(save_dir, paste0(prefix, "_", block_name, "_cleaned.csv")),
        # row.names = FALSE)
        
        # Print progress to monitor
        message("Saved ", prefix, "_", block_name)
    }
}

# --- Save SR1 blocks ---
save_blocks_individually(SR1_clean, prefix = "SR1")

# --- Save SR2 blocks ---
save_blocks_individually(SR2_clean, prefix = "SR2")


#HYPOTHESIS 1 

library(dplyr)

clean_dir <- ""

# Load blocks
block2_sr2 <- readRDS(file.path(clean_dir, "SR2_SR2_block2_cleaned.rds"))
block3_sr2 <- readRDS(file.path(clean_dir, "SR2_SR2_block3_cleaned.rds"))
block8_sr2 <- readRDS(file.path(clean_dir, "SR2_SR2_block8_cleaned.rds"))
block1_sr2 <- readRDS(file.path(clean_dir, "SR2_SR2_block1_cleaned.rds"))  # for weights

library(dplyr)

# Set directory where cleaned blocks are saved
clean_dir <- ""

# ----------------------------
# Load only required SR2 blocks
# ----------------------------
block1_sr2 <- readRDS(file.path(clean_dir, "SR2_SR2_block1_cleaned.rds"))   # weights
block2_sr2 <- readRDS(file.path(clean_dir, "SR2_SR2_block2_cleaned.rds"))   # informal, sector
block3_sr2 <- readRDS(file.path(clean_dir, "SR2_SR2_block3_cleaned.rds"))   # state
block8_sr2 <- readRDS(file.path(clean_dir, "SR2_SR2_block8_cleaned.rds"))   # employment

# ----------------------------
# Step 1: Aggregate numeric and categorical variables per ENTID
# ----------------------------
# Employment: sum total workers
block8_agg <- block8_sr2 %>%
    group_by(ENTID) %>%
    summarise(b8_q9 = sum(b8_q9, na.rm = TRUE), .groups = "drop")

# Informal indicators: take first non-NA or majority
block2_agg <- block2_sr2 %>%
    group_by(ENTID) %>%
    summarise(
        b2_q227 = first(b2_q227),  # Registration
        b2_q216 = first(b2_q216),  # Accounts maintained
        b2_q204 = first(b2_q204),  # Ownership type
        b2_q202 = first(b2_q202),  # Sector NIC
        .groups = "drop"
    )

# State info: take first occurrence per ENTID
block3_agg <- block3_sr2 %>%
    group_by(ENTID) %>%
    summarise(
        State = first(State),
        State_District = first(State_District),
        .groups = "drop"
    )

# Weights: take first occurrence per ENTID
block1_agg <- block1_sr2 %>%
    group_by(ENTID) %>%
    summarise(
        Weight_SS = first(Weight_SS),
        Weight_SC = first(Weight_SC),
        .groups = "drop"
    )

# ----------------------------
# Step 2: Merge only necessary blocks for H1

block1_agg <- block1_sr2 %>%
    group_by(ENTID) %>%
    summarise(
        Weight_SS = first(Weight_SS),
        Weight_SC = first(Weight_SC),
        .groups = "drop"
    )

# ----------------------------
h1_data <- block8_agg %>%
    left_join(block2_agg, by = "ENTID") %>%
    left_join(block3_agg, by = "ENTID") %>%
    left_join(block1_agg, by = "ENTID")

# ----------------------------
# Step 3: Create H1 variables
# ----------------------------






h1_data <- h1_data %>%
    mutate(
        b2_q202 = as.numeric(as.character(b2_q202))
        
    )

h1_data <- h1_data %>%
    mutate(
        # Informal dummy
        informal = ifelse(b2_q227 == 2 | b2_q216 == 2 | b2_q204 %in% c("Proprietary", "Partnership"), 1, 0),
        
        # Small firm dummy
        small = case_when(
            b2_q202 >= 1 & b2_q202 <= 3999 & b8_q9 < 20 ~ 1,  # Manufacturing
            b2_q202 >= 4000 & b8_q9 < 10 ~ 1,                # Services/Trade
            TRUE ~ 0
        )
    )


# ----------------------------
# Step 4: Calculate employment shares (descriptive)
# ----------------------------
informal_summary <- h1_data %>%
    summarise(
        total_emp = sum(b8_q9, na.rm = TRUE),
        informal_emp = sum(b8_q9 * informal, na.rm = TRUE),
        informal_share = informal_emp / total_emp
    )

informal_summary

# ----------------------------
# Step 5: Weighted employment shares
# ----------------------------
weighted_summary <- h1_data %>%
    summarise(
        total_emp_w = sum(b8_q9 * Weight_SS, na.rm = TRUE),
        informal_emp_w = sum(b8_q9 * informal * Weight_SS, na.rm = TRUE),
        informal_share_w = informal_emp_w / total_emp_w
    )

weighted_summary


library(dplyr)

# ----------------------------
# Step 0: Assume h1_data already exists from previous script
# h1_data contains: ENTID, b8_q9, informal, small, b2_q202 (sector), State, State_District, Weight_SS
# ----------------------------

# ----------------------------
# Step 1: Employment by informal / small firm
# ----------------------------
employment_summary <- h1_data %>%
    group_by(informal, small) %>%
    summarise(
        total_emp = sum(b8_q9, na.rm = TRUE),
        weighted_emp = sum(b8_q9 * Weight_SS, na.rm = TRUE),
        .groups = "drop"
    )

employment_summary

# ----------------------------
# Step 2: Employment by sector and informal status
# ----------------------------
sector_summary <- h1_data %>%
    group_by(b2_q202, informal) %>%
    summarise(
        total_emp = sum(b8_q9, na.rm = TRUE),
        weighted_emp = sum(b8_q9 * Weight_SS, na.rm = TRUE),
        .groups = "drop"
    )

sector_summary

# ----------------------------
# Step 3: Employment by state and informal status
# ----------------------------
state_summary <- h1_data %>%
    group_by(State, informal) %>%
    summarise(
        total_emp = sum(b8_q9, na.rm = TRUE),
        weighted_emp = sum(b8_q9 * Weight_SS, na.rm = TRUE),
        .groups = "drop"
    )

state_summary

# ----------------------------
# Step 4: Employment by informal × small × sector (optional detailed cross-tab)
# ----------------------------
cross_tab_summary <- h1_data %>%
    group_by(informal, small, b2_q202) %>%
    summarise(
        total_emp = sum(b8_q9, na.rm = TRUE),
        weighted_emp = sum(b8_q9 * Weight_SS, na.rm = TRUE),
        .groups = "drop"
    )

cross_tab_summary

# ----------------------------
# Step 5: Create final H1 table for reporting
# Optional: calculate shares
# ----------------------------
h1_final <- h1_data %>%
    group_by(informal) %>%
    summarise(
        total_emp = sum(b8_q9, na.rm = TRUE),
        weighted_emp = sum(b8_q9 * Weight_SS, na.rm = TRUE),
        share_emp = total_emp / sum(h1_data$b8_q9, na.rm = TRUE),
        weighted_share = weighted_emp / sum(h1_data$b8_q9 * h1_data$Weight_SS, na.rm = TRUE),
        .groups = "drop"
    )

h1_final

library(dplyr)
library(tidyr)

# ----------------------------
# Step 0: Prepare sector groups
# ----------------------------
h1_data <- h1_data %>%
    mutate(
        sector_group = case_when(
            b2_q202 >= 1 & b2_q202 <= 3999 ~ "Manufacturing",
            b2_q202 >= 4000 ~ "Services/Trade",
            TRUE ~ "Other"
        ),
        small = ifelse(is.na(small), 0, small),  # ensure no NA in small
        informal = ifelse(is.na(informal), 0, informal)
    )

# ----------------------------
# Step 1: Summarise employment by informal, small, sector
# ----------------------------
h1_summary <- h1_data %>%
    group_by(sector_group, informal, small) %>%
    summarise(
        total_emp = sum(b8_q9, na.rm = TRUE),
        weighted_emp = sum(b8_q9 * Weight_SS, na.rm = TRUE),
        .groups = "drop"
    )

# ----------------------------
# Step 2: Calculate shares
# ----------------------------
h1_summary <- h1_summary %>%
    group_by(sector_group) %>%
    mutate(
        share_emp = total_emp / sum(total_emp) * 100,
        weighted_share = weighted_emp / sum(weighted_emp) * 100
    ) %>%
    ungroup()

# ----------------------------
# Step 3: Pivot for compact wide table
# ----------------------------
h1_table <- h1_summary %>%
    unite("Informal_Small", informal, small, sep = "_") %>% 
    pivot_wider(
        names_from = Informal_Small,
        values_from = c(total_emp, weighted_emp, share_emp, weighted_share)
    )

# ----------------------------
# Step 4: Optional: keep only major sector groups
# ----------------------------
h1_table <- h1_table %>% filter(sector_group %in% c("Manufacturing", "Services/Trade"))

# ----------------------------
# Step 5: Save to CSV for paper/Excel
# ----------------------------
write.csv(h1_table, "H1_compact_summary.csv", row.names = FALSE)

# ----------------------------
# View final table
# ----------------------------
h1_table

View(as.data.frame(h1_table))
print(h1_table, n = Inf, width = Inf)

h1_summary_long <- h1_summary %>%
    mutate(informal_label = ifelse(informal==1, "Informal","Formal"),
           small_label = ifelse(small==1, "Small","Large")) %>%
    select(sector_group, informal_label, small_label, total_emp, weighted_emp, share_emp, weighted_share)



# View the table in console
print(h1_summary_long, n = Inf)   # show all rows




# Keep only informal firms

library(dplyr)

# Step 1: Make sure informal_label exists in h1_data
h1_data <- h1_data %>%
    mutate(
        informal_label = ifelse(informal == 1, "Informal", "Formal"),
        small_label = ifelse(small == 1, "Small", "Large"),
        sector_group = case_when(
            b2_q202 >= 1 & b2_q202 <= 3999 ~ "Manufacturing",
            b2_q202 >= 4000 ~ "Services/Trade",
            TRUE ~ "Other"
        )
    )

# Step 2: Summarise weighted employment only for Informal firms
h1_informal <- h1_data %>%
    filter(informal == 1) %>%   # Keep only informal
    group_by(sector_group, small_label) %>%
    summarise(
        weighted_emp = sum(b8_q9 * Weight_SS, na.rm = TRUE),
        weighted_share = weighted_emp / sum(weighted_emp) * 100,
        .groups = "drop"
    ) %>%
    mutate(weighted_emp_k = weighted_emp / 1000)  # in thousands

# Step 3: View
print(h1_informal,n = Inf)
library(dplyr)
library(ggplot2)

# ----------------------------
# Step 1: Prepare data for plotting
# ----------------------------
h1_plot_data <- h1_data %>%
    filter(informal == 1) %>%  # Only informal firms
    mutate(
        small_label = ifelse(small == 1, "Small", "Large"),
        sector_group = case_when(
            b2_q202 >= 1 & b2_q202 <= 3999 ~ "Manufacturing",
            b2_q202 >= 4000 ~ "Services/Trade",
            TRUE ~ "Other"
        )
    ) %>%
    group_by(sector_group, small_label) %>%
    summarise(
        weighted_emp = sum(b8_q9 * Weight_SS, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    group_by(sector_group) %>%
    mutate(
        weighted_share = weighted_emp / sum(weighted_emp) * 100
    ) %>%
    ungroup()

# ----------------------------
# Step 2: Plot
# ----------------------------
ggplot(h1_plot_data, aes(x = sector_group, y = weighted_share, fill = small_label)) +
    geom_bar(stat = "identity") +
    labs(
        title = "Weighted Employment Share (%) by Informal Firms and Size",
        x = "Sector",
        y = "Weighted Employment Share (%)",
        fill = "Firm Size"
    ) +
    scale_fill_manual(values = c("Small" = "#1f78b4", "Large" = "#33a02c")) +
    scale_y_continuous(labels = scales::percent_format(scale = 1)) +  # show % nicely
    theme_minimal(base_size = 14) +
    theme(
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, hjust = 0.5)
    )

print(h1_table, n = Inf, width = Inf)
print(h1_summary, n = Inf, width = Inf)


#Hypothesis 2


library(dplyr)
clean_dir <- ""

block2_sr1 <- readRDS(file.path(clean_dir, "SR1_SR1_block2_cleaned.rds"))
block7_sr1 <- readRDS(file.path(clean_dir, "SR1_SR1_block7_cleaned.rds"))
block10pt1_sr1 <- readRDS(file.path(clean_dir, "SR1_SR1_block10pt1_cleaned.rds"))
block8_sr2 <- readRDS(file.path(clean_dir, "SR2_SR2_block8_cleaned.rds"))

# ==========================================================
# 1. AGGREGATE EACH BLOCK BY ENTID
# ==========================================================

# ---- Block 7: GVA ----
gva <- block7_sr1 %>%
    group_by(ENTID) %>%
    summarise(gva = sum(b7_q3, na.rm = TRUE), .groups = "drop")

# ---- Block 10.1: Capital ----
#capital <- block10pt1_sr1 %>%
# group_by(ENTID) %>%
# summarise(capital = sum(b10pt1_q1011_3 + b10pt1_q1012_3, na.rm = TRUE),
#  .groups = "drop")

# ---- Block 8 SR2: Employment ----
employment <- block8_sr2 %>%
    group_by(ENTID) %>%
    summarise(emp = sum(b8_q9, na.rm = TRUE), .groups = "drop")

# ---- Block 2: Ownership + Hours + Months + Informality ----
owner <- block2_sr1 %>%
    select(ENTID, b2_q204, b2_q215, b2_q214, b2_q227)
# b2_q204 = ownership
# b2_q215 = avg_hours
# b2_q214 = avg_months
# b2_q227 = informal status

# ==========================================================
# 2. MERGE ALL BLOCKS SAFELY (no many-to-many)
# ==========================================================

h2 <- owner %>%
    left_join(gva, by = "ENTID") %>%
    
    left_join(employment, by = "ENTID")

# ==========================================================
# 3. CREATE CLEAN VARIABLES
# ==========================================================

h2 <- h2 %>%
    mutate(
        avg_hours = as.numeric(b2_q215),
        avg_months = as.numeric(b2_q214),
        informal = ifelse(b2_q227 == "2", 1, 0),
        labor_prod = gva / emp,
        ownership = as.factor(b2_q204)
    ) %>%
    filter(
        !is.na(labor_prod),
        is.finite(labor_prod),
        emp > 0,
        !is.na(avg_hours),
        !is.na(avg_months)
    )

# ==========================================================
# 4. ADD STATE (from Block 7, which contains ENTID + State)
# ==========================================================

state_df <- block7_sr1 %>% 
    select(ENTID, State) %>% 
    distinct()   # critical to avoid many-many join

h2 <- left_join(h2, state_df, by = "ENTID")

# ==========================================================
# 5. CLEAN FACTORS
# ==========================================================

h2$ownership <- as.factor(h2$ownership)
h2$State <- as.factor(h2$State)


# ==========================================================
# 6. FINAL REGRESSION WITH STATE FIXED EFFECTS
# ==========================================================

model <- lm(
    labor_prod ~ avg_hours + avg_months +
        informal + ownership + State,
    data = h2
)

summary(model)



#log Model
# avoid log(0) errors using +1
h2$log_lp <- log(h2$labor_prod + 1)
names(h2)
summary(h2$labor_prod)
min(h2$labor_prod, na.rm = TRUE)
sum(h2$labor_prod < 0, na.rm = TRUE)
h2 <- h2 %>% filter(labor_prod > 0)
h2$log_lp <- log(h2$labor_prod)

sum(is.na(h2$log_lp))

model_log <- lm(
    log_lp ~  avg_hours + avg_months +
        informal + ownership + State,
    data = h2
)

summary(model_log)


# ==========================================================
# H3: SECTORAL VARIATION — CLEAN SCRIPT
# ==========================================================

library(dplyr)

# ----------------------------------------------------------
# 1. LOAD CLEAN BLOCKS
# ----------------------------------------------------------
clean_dir <- ""

block2_sr1      <- readRDS(file.path(clean_dir, "SR1_SR1_block2_cleaned.rds"))
block7_sr1      <- readRDS(file.path(clean_dir, "SR1_SR1_block7_cleaned.rds"))
block8_sr2      <- readRDS(file.path(clean_dir, "SR2_SR2_block8_cleaned.rds"))

# ----------------------------------------------------------
# 2. AGGREGATE BASIC VARIABLES
# ----------------------------------------------------------

gva <- block7_sr1 %>% 
    group_by(ENTID) %>% 
    summarise(gva = sum(b7_q3, na.rm = TRUE), .groups = "drop")

emp <- block8_sr2 %>% 
    group_by(ENTID) %>% 
    summarise(emp = sum(b8_q9, na.rm = TRUE), .groups = "drop")

owner_ops <- block2_sr1 %>%
    select(ENTID, b2_q202, b2_q204, b2_q205, b2_q207, 
           b2_q214, b2_q215, b2_q217)

# ----------------------------------------------------------
# 3. MERGE EVERYTHING INTO ONE DATASET
# ----------------------------------------------------------

h3 <- owner_ops %>%
    left_join(gva, by = "ENTID") %>%
    left_join(emp, by = "ENTID")

# ----------------------------------------------------------
# 4. CREATE PRODUCTIVITY
# ----------------------------------------------------------

h3 <- h3 %>%
    mutate(
        productivity = gva / emp
    ) %>%
    filter(
        productivity > 0,
        emp > 0
    )

# ----------------------------------------------------------
# 5. CLEAN NIC CODE FOR SECTOR CLASSIFICATION
# ----------------------------------------------------------

# Convert NIC to character
h3$nic <- as.character(h3$b2_q202)

# Extract 2-digit NIC
h3$nic2 <- substr(h3$nic, 1, 2)

# Convert safely to numeric
h3$nic2_num <- suppressWarnings(as.numeric(h3$nic2))

# ----------------------------------------------------------
# 6. DEFINE SECTOR CLASSIFICATION
# ----------------------------------------------------------

# Manufacturing: 10–33, 35
manufacturing_codes <- c(10:33, 35)

# Trade: 45–47
trade_codes <- c(45, 46, 47)

# Services: 37–39, 50–63, 68–75, 85–93
service_codes <- c(37:39, 50:63, 68, 69:75, 85:93)

# Special 5-digit service codes
special_service <- c(
    "49211","49219","4922","4923",
    "64193","643","64309","6491","64920",
    "64921","64929","6499",
    "771","772","773",
    "561","562","563"
)

# ----------------------------------------------------------
# 7. CREATE SERVICE DUMMY
# ----------------------------------------------------------

h3$service <- ifelse(
    h3$nic %in% special_service |
        h3$nic2_num %in% service_codes |
        (h3$nic2_num >= 50 & h3$nic2_num <= 63),
    1, 0
)

# ----------------------------------------------------------
# 8. ADD STATE (FROM BLOCK 7)
# ----------------------------------------------------------

state_df <- block7_sr1 %>% select(ENTID, State) %>% distinct()
h3 <- left_join(h3, state_df, by = "ENTID")
h3$State <- as.factor(h3$State)

# ----------------------------------------------------------
# 9. MODEL READY VARIABLES
# ----------------------------------------------------------

h3 <- h3 %>%
    mutate(
        firm_size = emp,
        ownership = as.factor(b2_q204),
        informal = ifelse(b2_q217 == "2", 1, 0)
    )

# ----------------------------------------------------------
# 10. FINAL H3 REGRESSION (FAST & CLEAN)
# ----------------------------------------------------------

model_h3 <- lm(
    log(productivity + 1) ~ service * firm_size + ownership + informal + State,
    data = h3
)

summary(model_h3)

# ----------------------------------------------------------
# 11. CHECK SECTOR DISTRIBUTION
# ----------------------------------------------------------

table(h3$service)

h3 %>%
    distinct(ENTID, service) %>%
    count(ENTID) %>%
    filter(n > 1)

#HYP0THESIS 4
# =========================
# H4: Regional Variation 
# =========================

# Required packages
library(dplyr)
library(haven)
library(broom)
library(lmtest)
library(sandwich)
library(ggplot2)
library(scales)

# -------------------------
# 0. Directory: change if needed
# -------------------------
clean_dir <- ""

# -------------------------
# 1. Load cleaned SR1 & SR2 blocks (use your exact file names)
# -------------------------
block2_sr1  <- readRDS(file.path(clean_dir, "SR1_SR1_block2_cleaned.rds"))    # sector, ownership, op chars
block7_sr1  <- readRDS(file.path(clean_dir, "SR1_SR1_block7_cleaned.rds"))    # GVA items
block8_sr2  <- readRDS(file.path(clean_dir, "SR2_SR2_block8_cleaned.rds"))    # Employment (SR2)
block3_sr2  <- readRDS(file.path(clean_dir, "SR2_SR2_block3_cleaned.rds"))    # State (SR2 Block3)

# Basic checks
cat("Rows: block2 =", nrow(block2_sr1), "block7 =", nrow(block7_sr1),
    "block8 =", nrow(block8_sr2), "block3 =", nrow(block3_sr2), "\n")

# -------------------------
# 2. Aggregate GVA (Block 7) by ENTID
#    b7_q3 is the value; sum across rows per ENTID
# -------------------------
gva <- block7_sr1 %>%
    mutate(b7_q3_num = suppressWarnings(as.numeric(b7_q3))) %>%
    group_by(ENTID) %>%
    summarise(gva = sum(b7_q3_num, na.rm = TRUE), .groups = "drop")

# -------------------------
# 3. Aggregate Employment (Block 8 SR2) by ENTID
#    b8_q9 is total workers (ensure numeric)
# -------------------------
emp <- block8_sr2 %>%
    mutate(b8_q9_num = suppressWarnings(as.numeric(b8_q9))) %>%
    group_by(ENTID) %>%
    summarise(emp = sum(b8_q9_num, na.rm = TRUE), .groups = "drop")

# -------------------------
# 4. Get State from Block 3 (SR2 Block3 cleaned) - you confirmed column "State"
# -------------------------
state_df <- block3_sr2 %>%
    select(ENTID, State, State_District) %>%
    distinct()

# -------------------------
# 5. Select block2 variables we need and ensure NIC is character
#    b2_q202 = NIC-2008; b2_q204 = ownership; b2_q214, b2_q215 = months/hours; b2_q217 informal
# -------------------------
b2 <- block2_sr1 %>%
    select(ENTID, b2_q202, b2_q204, b2_q214, b2_q215, b2_q217) %>%
    mutate(b2_q202 = as.character(b2_q202))

# -------------------------
# 6. Merge into analysis dataset (left joins)
# -------------------------
h4 <- b2 %>%
    left_join(gva, by = "ENTID") %>%
    left_join(emp, by = "ENTID") %>%
    left_join(state_df, by = "ENTID")



# Quick missingness check
cat("N rows after merge:", nrow(h4), "\n")
cat("N missing gva:", sum(is.na(h4$gva)), "N missing emp:", sum(is.na(h4$emp)), "\n")

# -------------------------
# 7. Clean NIC codes robustly and classify into broad 3 sectors (Manufacturing / Trade / Services)
#    This handles 5-digit codes like '49211' and 2-digit numeric codes.
# -------------------------
h4 <- h4 %>%
    mutate(
        nic_raw = as.character(b2_q202),
        nic2 = suppressWarnings(as.numeric(substr(nic_raw, 1, 2))),    # first 2 digits numeric, if possible
        # For safety: numeric NA might occur for weird strings; keep nic_raw for special matches
        # Broad sector classification (Option A):
        sector_group = case_when(
            !is.na(nic2) & nic2 >= 10 & nic2 <= 33 ~ "Manufacturing",            # 10-33
            !is.na(nic2) & nic2 >= 45 & nic2 <= 47 ~ "Trade",                    # 45-47
            !is.na(nic2) & nic2 >= 50 & nic2 <= 99 ~ "Services",                 # 50-99
            # Additional special check: some service NICs may start with '49' (transport) or are 5-digit codes
            nic_raw %in% c("49211","49219","4922","4923","64193","643","64309","6491","64920","64921","64929","6499") ~ "Services",
            TRUE ~ "Other"
        ),
        sector_group = factor(sector_group, levels = c("Manufacturing","Trade","Services","Other"))
    )

# Verify sector counts
print(table(h4$sector_group, useNA = "ifany"))

# -------------------------
# 8. Derive productivity and logs; controls


# -------------------------
h4 <- h4 %>%
    mutate(
        gva = as.numeric(gva),
        emp = as.numeric(emp),
        productivity = ifelse(!is.na(emp) & emp > 0, gva / emp, NA_real_),
        log_prod = ifelse(!is.na(productivity) & productivity > 0, log(productivity), NA_real_),
        log_emp = ifelse(!is.na(emp) & emp > 0, log(emp), NA_real_),
        avg_months = suppressWarnings(as.numeric(b2_q214)),
        avg_hours  = suppressWarnings(as.numeric(b2_q215)),
        informal   = ifelse(as.character(b2_q217) == "2", 1, 0),
        ownership  = as.factor(b2_q204),
        State = as.factor(State)
    ) %>%
    # keep only observations with essential info
    filter(!is.na(log_prod), !is.na(log_emp), !is.na(sector_group), !is.na(State))

h4$State <- relevel(h4$State, ref = "27")   # Maharashtra as baseline

cat("Final observations for H4:", nrow(h4), "\n")

# -------------------------
# 9. State-level summary (for reporting & plotting)
# -------------------------
state_summary <- h4 %>%
    group_by(State) %>%
    summarise(
        n = n(),
        mean_prod = mean(productivity, na.rm = TRUE),
        median_prod = median(productivity, na.rm = TRUE),
        mean_emp = mean(emp, na.rm = TRUE),
        median_emp = median(emp, na.rm = TRUE)
    ) %>%
    arrange(desc(mean_prod))

print(head(state_summary, 12))

# -------------------------
# 10. Regression: Productivity (log) with State fixed effects
#     We'll run OLS and compute cluster-robust SE clustered by State
# -------------------------
spec_prod <- log_prod ~ sector_group + log_emp + ownership + informal + avg_hours + avg_months + State

prod_model <- lm(spec_prod, data = h4)

# cluster-robust covariance matrix by State
vcov_prod_cl <- vcovCL(prod_model, cluster = ~ State)
prod_tidy <- coeftest(prod_model, vcov = vcov_prod_cl)

cat("\n=== Productivity model (cluster-robust SE by State) ===\n")
print(prod_tidy)

# -------------------------
# 11. Regression: Employment (log_emp) with State fixed effects
# -------------------------
spec_emp <- log_emp ~ sector_group + ownership + informal + avg_hours + avg_months + State

emp_model <- lm(spec_emp, data = h4)
vcov_emp_cl <- vcovCL(emp_model, cluster = ~ State)
emp_tidy <- coeftest(emp_model, vcov = vcov_emp_cl)

cat("\n=== Employment model (cluster-robust SE by State) ===\n")
print(emp_tidy)

# -------------------------
# 12. Quick plots for the paper
# -------------------------
# Top 20 states by mean productivity
top_states <- state_summary %>% top_n(20, mean_prod) %>% arrange(mean_prod)

p1 <- ggplot(top_states, aes(x = reorder(State, mean_prod), y = mean_prod)) +
    geom_col() + coord_flip() +
    labs(title = "Top 20 States by Mean Enterprise Productivity", x = "State", y = "Mean productivity (GVA per worker)") +
    scale_y_continuous(labels = scales::comma)


print(p1)


# Top 20 states by mean employment
top_states_emp <- state_summary %>% top_n(20, mean_emp) %>% arrange(mean_emp)
p2 <- ggplot(top_states_emp, aes(x = reorder(State, mean_emp), y = mean_emp)) +
    geom_col() + coord_flip() +
    labs(title = "Top 20 States by Mean Enterprise Employment", x = "State", y = "Mean employment (workers)") +
    scale_y_continuous(labels = scales::comma)


print(p2)

# -------------------------
# 13. Save cleaned H4 dataset and model objects (optional)
# -------------------------
saveRDS(h4, file = file.path(clean_dir, "H4_regression_data.rds"))
saveRDS(prod_model, file = file.path(clean_dir, "H4_prod_model.rds"))
saveRDS(emp_model, file = file.path(clean_dir, "H4_emp_model.rds"))

cat("H4 dataset and models saved to:", clean_dir, "\n")
# =========================
# End of H4 script
# =========================
ls()
sort(unique(h4$State))








