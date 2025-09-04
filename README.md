# Human-Capital-Analytics Project

## 📌 Business Goal
Minimize employee turnover by identifying key factors influencing attrition and building predictive models.

## 🎯 Analytical Approach
- Data preprocessing and descriptive analysis
- Hypothesis testing
- Predictive modeling (Logistic Regression, Decision Tree, Naïve Bayes)
- Model evaluation and tuning
- Clustering (K-means) to segment employees

## 🛠️ Tech Stack
- **Language:** R
- **Libraries:** dplyr, ggplot2, tidyr, corrplot, Boruta, caret, e1071, cluster, rpart, rpart.plot, pROC, gridExtra, reshape2

## 📂 Repository Structure

- Human-Capital-Analytics/
- ├── data/ # Sample data to download
- ├── scripts/ # R scripts for preprocessing, modeling, clustering
- ├── docs/ # Project report in PDF
- ├── figures/ # Visualization outputs
- └── README.md # Project overview

## 📊 Results
- Logistic Regression: Accuracy 74.33%, Sensitivity 73.41%  
- Classification Tree: Accuracy 97.31%, Sensitivity 92.73%  
- Naïve Bayes: Accuracy 84.08%, Sensitivity 55.11%  
- K-means Clustering: Segmented employees into 4 groups for deeper insights

> ⚠️ **Caution**  
> Model results may vary slightly between runs due to randomized data partitioning.  
> Use `set.seed(****)` to ensure reproducibility.

## 📖 Documentation
The full project report is available in [docs/Human_Capital_Analytics_Report.pdf](docs/Human_Capital_Analytics_Report.pdf).

## 👤 Author
Shanmugan Kukatla
