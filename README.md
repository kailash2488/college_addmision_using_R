# College Admission Prediction in R

An end-to-end classification project that explores factors associated with US college admission and compares multiple machine-learning models on a consistent train/test split.

> **Project status:** the analysis code is repaired and portable. The original `College_admission.csv` is not included in this repository, so add the dataset locally before running it. No performance scores are claimed until they can be reproduced from that source data.

## What this project demonstrates

- Data validation, missing-value reporting, and type conversion
- Exploratory analysis of GRE, GPA, and admission outcomes
- Reproducible, stratified training/test sampling
- Logistic regression, decision tree, and radial SVM comparison
- Accuracy, precision, recall, specificity, and F1 evaluation
- Exported reports, plots, predictions, and trained models

## Dataset

The script expects these fields. Column names are normalized to lowercase, so common capitalization differences are accepted.

| Column | Meaning |
| --- | --- |
| `admit` | Binary outcome: `1` admitted, `0` not admitted |
| `gre` | Graduate Record Examination score |
| `gpa` | Grade point average |
| `ses` | Socioeconomic status: 1 low, 2 medium, 3 high |
| `gender_male` | Gender indicator: 1 male, 0 female |
| `race` | Encoded race category from the coursework dataset |
| `rank` | Institution prestige: 1 highest, 4 lowest |

Place an appropriately licensed copy at `data/College_admission.csv`, or pass another path. Because `race` and `gender_male` are sensitive attributes, treat this as an educational analysis—not a production admissions system.

## Run the analysis

Requirements: R 4.1+, plus `rpart` and `e1071` for all three models.

```r
install.packages(c("rpart", "e1071"))
```

```bash
Rscript solve.R data/College_admission.csv
```

Optionally choose another results directory:

```bash
Rscript solve.R path/to/College_admission.csv results
```

Logistic regression and reporting use base R. If an optional package is unavailable, its model is skipped with a clear message.

## Generated outputs

| File | Description |
| --- | --- |
| `model_metrics.csv` | Classification metrics ranked by accuracy |
| `test_predictions.csv` | Actual labels and per-model predictions |
| `data_summary.txt` | Row counts, missing values, admission rate, and summary |
| `admission_by_gre.csv` | Admission rate for low, medium, and high GRE groups |
| `admission_by_gpa.png` | GPA distribution by admission outcome |
| `admission_rate_by_gre.png` | Admission percentage across GRE categories |
| `*_model.rds` | Saved fitted models |

## Methodology

1. Normalize and validate the input schema.
2. Convert invalid numeric values to missing and report them.
3. Use complete cases and record how many observations remain.
4. Treat SES, gender, race, and rank as categorical predictors.
5. Create a reproducible 75/25 stratified split (`set.seed(123)`).
6. Train available classifiers on identical training rows.
7. Evaluate every model against the same held-out test set.

Outliers are retained. Automatically deleting legitimate high or low scores can bias a small admissions dataset; diagnostics and domain knowledge should guide future treatment.

## Repository layout

```text
.
├── solve.R                 # Reproducible analysis pipeline
├── README.md               # Project documentation
├── AfterOutliears.png      # Historical coursework screenshot
├── GpaOutliear.png         # Historical coursework screenshot
└── WithOutliears.png       # Historical coursework screenshot
```

## Known limitation

The repository cannot reproduce final metrics by itself because the source CSV is missing. Once it is added, run the command above and publish the generated charts and metrics here.

## License

No license has been specified. Add one before distributing or reusing the code publicly.
