###############################################################################
# Mortality one class classification example ##################################
###############################################################################
# This script builds an example of a one class classification model to identify
# possibly erroneous mortality table inputs. Based on SOA mortality tables.



# Install any missing packages
source("prepMachine.R")

# Load up packages used in script
library(tidyverse)
library(factoextra)
library(readxl)
library(e1071)
library(glue)


###############################################################################
# Read and treat SOA mortality tables #########################################
###############################################################################

# Read all mortality tables.
# Used CSO/CET with aggregate format
files.list <- list.files("data/", recursive = TRUE, full.names = TRUE)

# Take only ages 15 to 99
names.vector <- c("Table.Name.", paste0("X", 15:99))


# Loop through and load all tables
for(i in 1:length(files.list)) {
  curr <- read_excel(files.list[i], col_names = FALSE) %>%
    filter(!(row_number() %in% c(2:24))) %>%
    spread(...1, ...2) %>%
    data.frame() %>%
    select(one_of(names.vector))
  
  if(i == 1) {
    total <- curr 
  } else {
    total <- bind_rows(total, curr)
  }
}


# # Save dataset to avoid full loop running in demo
# saveRDS(total, "cache/PostLoop.rds")
# # Readback saved RDS
# total <- readRDS("cache/PostLoop.rds")

# Drop any extra years
total <- total %>%
  select(one_of(names.vector))

# Recast characters as numbers, caused by Excel formatting
for(cols in 2:(length(names(total)))) {
  total[, cols] <- as.numeric(total[, cols])
}

total.model <- total
row.names(total.model) <- total.model$Table.Name.

###############################################################################
# Build and visualize mortality table principal components analysis ###########
###############################################################################

# Create PCA model for mortality tables
formula.pr <- paste0("~ ", glue_collapse(names(total)[2:86], " + "))


model.pca <- prcomp(formula(formula.pr),
                    data = total.model,
                    scale = TRUE, # Centering and scaling is key for a PCA
                    center = TRUE) # Centering and scaling is key for a PCA


# Print summary information on the model
summary(model.pca)


# Visualization of PCA model
fviz_pca_ind(model.pca, 
             col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = FALSE
)

preds.pca <- predict(model.pca,
                     newdata = total.model)

# Print summary information on the PCs
summary(preds.pca)


# Plot and visualize most extreme PC1 and PC2
data.plot <- total.model[c(which.max((preds.pca %>% data.frame)$PC1),
                           which.max((preds.pca %>% data.frame)$PC2),
                           which.min((preds.pca %>% data.frame)$PC1),
                           which.min((preds.pca %>% data.frame)$PC2)),] %>%
  gather(key = "Age", 
         value = "Qx", 
         -Table.Name.) %>%
  mutate(Age = as.numeric(gsub("X", "", Age)))

ggplot(data = data.plot) +
  geom_line(aes(x = Age, y = Qx, col = Table.Name.)) +
  theme_minimal() +
  theme(legend.position = c(.4, .85)) +
  ggtitle("Mortality table shape for most extreme tables")

ggplot(data = data.plot) +
  geom_line(aes(x = Age, y = log(Qx), col = Table.Name.)) +
  theme_minimal() +
  theme(legend.position = c(.4, .85)) +
  ggtitle("Log of mortality table shape for most extreme tables")


# Numeric summary information for each of the extreme tables
data.plot %>% 
  split(.$Table.Name.) %>%
  map(~ lm(Qx ~ Age, data = .)) %>%
  map_dfr(~ broom::tidy(.), id = "source") %>%
  filter(term == "Age") %>%
  select(SlopeQx = estimate) %>%
  bind_cols(data.plot %>% 
          group_by(Table.Name.) %>% 
          summarize(MeanQx = mean(Qx)), .)

###############################################################################
# Prepare data for one-class SVM ##############################################
###############################################################################

# Pull out 2 test records for later unit testing
fake <- preds.pca[c(1:2), c("PC1", "PC2")]
fake[1, "PC1"] <- 25 # Create extreme values for one record
fake[1, "PC2"] <- 7
row.names(fake) <- c("FakeRecord", "NormalRecord")


# Create partition for test/train split
smp_size <- floor(0.7 * nrow(preds.pca))

set.seed(123)
train_ind <- sample(seq_len(nrow(preds.pca)), size = smp_size)

train <- preds.pca[train_ind, c("PC1", "PC2")]
test <- preds.pca[-train_ind, c("PC1", "PC2")]

###############################################################################
# Build one-class SVMs for combinations of hyper parameters ###################
###############################################################################

# Grid of parameters to test different models
hyper_grid <- expand.grid(
  nu = seq(from = .01, to = .5, by = .05),
  gamma = seq(from = .01, to = 1, by = .05),
  perf = 0,
  hittest = 0
)

# Test each combination of parameters
for(k in 1:nrow(hyper_grid)) {
  print(k)
  model <- svm(x = train, 
               type = "one-classification",
               nu = hyper_grid$nu[k],
               gamma = hyper_grid$gamma[k])
  preds <- predict(model, newdata = test)
  hyper_grid$perf[k] <- length(which(preds == FALSE))/length(preds)
  hyper_grid$hittest[k] <- all(
    ifelse(predict(model, newdata = fake) == c(FALSE, TRUE), 
           TRUE,
           FALSE))
}


# saveRDS(hyper_grid, "cache/ModelResults.rds")
# hyper_grid <- readRDS("cache/ModelResults.rds")

# Examine best candidate models
hyper_grid %>% filter(perf > 0) %>% arrange(perf) %>% head(20)
