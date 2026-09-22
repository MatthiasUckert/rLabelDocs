# rLabelDocs

A powerful, flexible **R Shiny application** for multi-schema document classification with team collaboration, complete audit trails, and advanced analytics.

## 📋 Overview

rLabelDocs enables teams to classify documents using multiple independent schemas simultaneously. Built with modern R packages (Shiny, Arrow, SQLite), it provides a production-ready solution for document tagging workflows with full history tracking and flexible validation.

### Key Features

- **🎯 Multi-Schema Support**: Classify documents using multiple independent schemas (e.g., Production, Test, Legal)
- **🏷️ Flexible Tagging**: Each document can receive multiple classification values per class
- **📊 Real-time Analytics**: Track progress with interactive dashboards and visualizations
- **👥 Multi-User Ready**: Full audit trail with user IDs and timestamps
- **📝 Document Notes**: Add contextual notes to any document
- **🔍 Advanced Filtering**: Browse and filter documents by any combination of attributes
- **📤 Data Export**: Export current state or full history in CSV or Parquet format
- **⏱️ Timeline View**: Navigate through classification history with interactive timeline
- **🚀 Fast & Efficient**: Arrow/Parquet for blazing-fast data operations
- **💾 Append-Only Logging**: SQLite backend ensures no data is ever lost

## 🚀 Quick Start

### Prerequisites

- R (>= 4.0.0)
- Required R packages:
  ```r
  install.packages(c(
    "shiny",
    "arrow",
    "dplyr",
    "readr",
    "ggplot2",
    "DT",
    "RSQLite",
    "DBI"
  ))
  ```

### Installation

```r
# install.packages("pak")
pak::pak("MatthiasUckert/rLabelDocs")
library(rLabelDocs)
```

**Launch the app**
   ```r
   classification_app(
     .dir = "path/to/your/project",
     .user_id = "your_username"
   )
   ```

The app will open in your default browser. If your project doesn't have the required files yet, use the Introduction tab to upload them.

## 📁 Project Structure

### Required Files

Your project directory needs two files to get started:

#### 1. Documents.parquet
Contains your documents in Parquet format with columns:
- `DocID` (character): Unique document identifier
- `HTML` (character): Document content in HTML format

Example in R:
```r
library(arrow)

documents <- data.frame(
  DocID = c("doc001", "doc002", "doc003"),
  HTML = c(
    "<h1>Document 1</h1><p>Content...</p>",
    "<h1>Document 2</h1><p>Content...</p>",
    "<h1>Document 3</h1><p>Content...</p>"
  )
)

write_parquet(documents, "Documents.parquet")
```

#### 2. Schema.csv
Defines your classification schemas with columns:
- `Schema` (integer): Schema ID (e.g., 1, 2, 3)
- `SchemaName` (character): Human-readable name (e.g., "Production")
- `Class` (character): Classification class (e.g., "DocType")
- `Value` (character): Possible values (e.g., "Contract")

Example Schema.csv:
```csv
Schema,SchemaName,Class,Value
1,Production,DocType,Contract
1,Production,DocType,Invoice
1,Production,DocType,Report
1,Production,Status,Active
1,Production,Status,Archived
2,Test,Category,SampleA
2,Test,Category,SampleB
```

### Auto-Generated Files

The system automatically creates:
- `classification_data.db`: SQLite database with append-only logs
- `.schema_hash`: Schema change detection
- `exports/`: Folder for exported data files

## 🎨 User Interface

### 1. Introduction Tab
- **Upload files** if setting up a new project
- **View project overview** with key statistics
- **Schema visualization** showing all classes and values
- **Help documentation** with usage tips

### 2. Classification Tab
- **Main workflow** for document classification
- **Filtering options**: All, Classified, Unclassified, or Marked documents
- **Selection modes**: Single (radio) or Multi (checkbox) behavior
- **Document navigation**: Prev/Next buttons or direct search
- **Document notes**: Add contextual information
- **Visual feedback**: Schema tabs show completion status

### 3. Overview Tab
- **Key metrics**: Total documents, classified, remaining, % complete
- **Progress charts**: Donut chart, user activity, distributions
- **Timeline visualization**: Classification activity over time
- **Recent classifications**: Sortable table of latest work

### 4. Browser Tab
- **Advanced filtering**: By Schema → Class → Value
- **Notes filter**: Show only documents with notes
- **Document marking**: Batch select documents for workflow
- **Read-only viewer**: Inspect documents and classifications

### 5. Export Data Tab
- **Export formats**: CSV or Parquet
- **Export scopes**: Current state or full history
- **Content selection**: Classifications only, notes only, or both
- **Timeline export**: Export data as of any historical point
- **File management**: View and delete previous exports

## 🔧 Usage Examples

### Basic Classification Workflow

1. **Start the app**
   ```r
   classification_app(.dir = "my_project", .user_id = "analyst1")
   ```

2. **Navigate to Classification tab**
   - Select "Unclassified" filter to focus on new documents
   - Choose "Multi" selection mode for faster tagging
   - Select appropriate values for each class in each schema
   - Add notes if needed
   - Click "Save" to record classifications

3. **Monitor progress** in Overview tab

4. **Export results** when complete

### Advanced Filtering

In the Browser tab:
1. Select a Schema (e.g., "Production")
2. Select a Class (e.g., "DocType")
3. Select specific Values (e.g., "Contract", "Invoice")
4. Optionally filter for documents with notes
5. Click "Apply Filter"
6. Browse filtered documents in the middle panel

### Exporting Data

1. Navigate to Export Data tab
2. Optional: Use timeline slider to view historical state
3. Click "Export Current State" or "Export Full History"
4. Choose format (CSV or Parquet)
5. Select content (classifications, notes, or both)
6. Download the generated file

## 🏗️ Architecture

### Technology Stack

- **Frontend**: Shiny (R's reactive web framework)
- **Data Storage**: 
  - SQLite (classification logs, append-only)
  - Parquet (document storage, efficient I/O)
- **Data Processing**: dplyr, Arrow
- **Visualization**: ggplot2
- **UI Components**: Custom CSS, DT (DataTables)

### Data Model

**Normalized Storage**: One row per classification value
```
DocID | UserID | Timestamp | Schema | Class | Value
------|--------|-----------|--------|-------|------
doc001| Alice  | 2024-...  | 1      | Type  | Contract
doc001| Alice  | 2024-...  | 1      | Type  | Legal
doc001| Alice  | 2024-...  | 1      | Status| Active
```

**Benefits**:
- Unlimited values per class
- Flexible schema changes
- Complete history tracking
- Easy filtering and aggregation

### Module Structure

```
R/
├── app.R                    # Application entry point
├── utils.R                  # Helper functions
├── validation.R             # Validation logic
├── data_io.R               # Database and file I/O
├── classification_logic.R   # Business logic
├── ui_helpers.R            # UI generation functions
├── mod_intro.R             # Introduction/Setup module
├── mod_classification.R     # Main classification module
├── mod_overview.R          # Analytics dashboard module
├── mod_browser.R           # Advanced filtering module
└── mod_export.R            # Data export module
```

## 📊 Analytics & Reporting

The Overview tab provides comprehensive analytics:

- **Progress Metrics**: Real-time completion statistics
- **User Activity**: Track individual contributions
- **Distribution Charts**: Visualize classification patterns
- **Timeline Analysis**: Understand classification velocity
- **Export Capabilities**: Generate reports in multiple formats

## 🤝 Contributing

Contributions are welcome! Please feel free to submit a Pull Request. For major changes:

1. Fork the repository
2. Create your feature branch (`git checkout -b feature/AmazingFeature`)
3. Commit your changes (`git commit -m 'Add some AmazingFeature'`)
4. Push to the branch (`git push origin feature/AmazingFeature`)
5. Open a Pull Request

### Development Guidelines

- Follow tidyverse style guide for R code
- Add roxygen2 documentation for all functions
- Include examples in function documentation
- Test thoroughly before submitting PR
- Update README if adding new features

## 📝 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## 🐛 Bug Reports & Feature Requests

Please use the [GitHub Issues](https://github.com/MatthiasUckert/rLabelDocs/issues) page to report bugs or request features.

When reporting bugs, please include:
- R version and platform
- Steps to reproduce
- Expected vs actual behavior
- Relevant screenshots or error messages

## 📚 Additional Resources

### Example Workflows

**Single-User Quick Classification**:
```r
# Setup
classification_app(.dir = "contracts_2024", .user_id = "lawyer1")

# Filter unclassified → Classify → Export
```

**Team Collaboration**:
```r
# User 1
classification_app(.dir = "shared_project", .user_id = "analyst1")

# User 2 (same project)
classification_app(.dir = "shared_project", .user_id = "analyst2")

# Track contributions in Overview tab
```

**Quality Review**:
```r
# Use Browser tab to filter specific categories
# Mark documents needing review
# Use Marked filter in Classification tab
# Add notes during review
```

### Schema Design Tips

1. **Keep schemas focused**: One schema per use case (e.g., Legal, Financial, Operational)
2. **Use clear names**: Make class and value names self-explanatory
3. **Avoid overlap**: Values within a class should be mutually exclusive if using single-select mode
4. **Plan for growth**: Add new values easily without disrupting existing classifications

### Performance Tips

- For large document sets (>10,000), consider pagination in custom views
- Export data regularly to maintain backups
- Use Parquet format for exports when working with large datasets
- Monitor SQLite database size; vacuum periodically if needed

## 🙏 Acknowledgments

Built with:
- [Shiny](https://shiny.rstudio.com/) - Web application framework
- [Arrow](https://arrow.apache.org/docs/r/) - Fast data I/O
- [dplyr](https://dplyr.tidyverse.org/) - Data manipulation
- [ggplot2](https://ggplot2.tidyverse.org/) - Data visualization
- [DT](https://rstudio.github.io/DT/) - Interactive tables
- [SQLite](https://www.sqlite.org/) - Database engine

---

**Maintainer**: [Matthias Uckert](https://github.com/MatthiasUckert)  
**Project Link**: [https://github.com/MatthiasUckert/rLabelDocs](https://github.com/MatthiasUckert/rLabelDocs)
