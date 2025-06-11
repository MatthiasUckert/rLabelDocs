
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rLabelDocs

\<img src=“man/figures/logo.png” align=“right” height=“139” /\>

<!-- badges: start -->

[<img
src="https://github.com/yourusername/rLabelDocs/workflows/R-CMD-check/badge.svg"
alt="R-CMD-check" />](https://github.com/yourusername/rLabelDocs/actions)
[<img src="https://www.r-pkg.org/badges/version/rLabelDocs"
alt="CRAN status" />](https://cran.r-project.org/package=rLabelDocs)
[<img
src="https://img.shields.io/badge/lifecycle-experimental-orange.svg"
alt="Lifecycle: experimental" />](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[<img src="https://img.shields.io/badge/License-MIT-yellow.svg"
alt="License: MIT" />](https://opensource.org/licenses/MIT)

<!-- badges: end -->

> **Interactive Document Classification and Labeling System**

rLabelDocs provides a comprehensive Shiny-based application for document
classification and labeling workflows. It’s designed for research teams,
legal document review, content analysis, and any scenario requiring
systematic document categorization with quality control and progress
monitoring.

## ✨ Features

- **🎯 Interactive Classification Interface**: User-friendly buttons for
  quick document labeling

- **📊 Real-time Progress Analytics**: Track completion rates and
  classification statistics

- **👥 Multi-user Support**: User activity tracking and collaborative
  workflows

- **🔍 Advanced Filtering & Search**: Find documents by ID or filter by
  classification criteria

- **📈 Comprehensive Dashboard**: Visual analytics with charts and
  progress indicators

- **💾 Robust Data Storage**: Uses Apache Parquet format for efficient
  data handling

- **🔄 Flexible Schema Support**: Define custom classification
  categories and values

- **📱 Responsive Design**: Works seamlessly across different screen
  sizes

## 🚀 Installation

### Development Version (Recommended)

    # Install from GitHub
    if (!require(devtools)) install.packages("devtools")
    devtools::install_github("yourusername/rLabelDocs")

### Dependencies

rLabelDocs requires several packages that will be automatically
installed:

    # Core dependencies
    install.packages(c(
      "shiny", "arrow", "dplyr", "ggplot2", 
      "DT", "readr", "tibble", "purrr", "magrittr"
    ))

## 📋 Quick Start

### 1. Prepare Your Data

Create a directory with the required structure:

    your_project/
    ├── Documents.parquet     # Document content with DocID and HTML columns
    ├── Schema.csv           # Classification schema definition
    └── Classifications.parquet  # Classification results (auto-created)

#### Documents.parquet

Contains your documents to be classified:

    # Example structure
    documents <- data.frame(
      DocID = c("doc_001", "doc_002", "doc_003"),
      HTML = c("<h1>Document 1</h1><p>Content...</p>", 
               "<h1>Document 2</h1><p>Content...</p>",
               "<h1>Document 3</h1><p>Content...</p>")
    )
    arrow::write_parquet(documents, "your_project/Documents.parquet")

#### Schema.csv

Defines your classification categories:

    # Example schema
    schema <- data.frame(
      Class = c("DocumentType", "DocumentType", "Priority", "Priority", "Priority"),
      Value = c("Contract", "Invoice", "High", "Medium", "Low")
    )
    write.csv(schema, "your_project/Schema.csv", row.names = FALSE)

### 2. Launch the Application

    library(rLabelDocs)

    # Launch the classification app
    classification_app(
      .dir = "path/to/your_project",
      .user_id = "your_username"
    )

The application will open in your default web browser with three main
tabs:

## 📖 User Guide

### Classification Tab

The main interface for document labeling:

- **Document Filter**: Choose between All, Unclassified, or Classified
  documents

- **Search Function**: Jump directly to specific document IDs

- **Classification Buttons**: Click to assign categories to documents

- **Navigation**: Move between documents with Previous/Next buttons

- **Progress Tracking**: See real-time completion statistics

### Overview Tab

Comprehensive analytics dashboard:

- **Key Metrics**: Total documents, classified count, completion
  percentage

- **Progress Visualization**: Donut chart showing classification
  progress

- **User Activity**: Bar charts of classification activity by user

- **Timeline Analysis**: Cumulative progress over time

- **Distribution Charts**: Breakdown of classifications by category

- **Recent Activity**: Table of the most recent classifications

### Browser Tab

Advanced document exploration:

- **Advanced Filtering**: Filter documents by specific classification
  values

- **Document Navigation**: Browse filtered results with
  First/Previous/Next/Last controls

- **Document Viewer**: Full HTML rendering of document content

- **Filter Management**: Apply and clear classification-based filters

## 🛠️ Advanced Usage

### Custom Data Validation

    # Check if your directory structure is valid
    rLabelDocs::check_input_directory("path/to/your_project")

### Programmatic Access

    # Read classification schema
    schema <- read_schema("path/to/your_project")

    # Get document statistics
    stats <- get_filter_counts("path/to/your_project")
    progress <- get_progress_stats("path/to/your_project")

    # Check if specific document is classified
    is_classified <- is_document_classified("path/to/your_project", "doc_001")

### Custom Plotting

    # Generate standalone plots
    progress_plot <- plot_progress_overview("path/to/your_project")
    timeline_plot <- plot_classification_timeline("path/to/your_project")
    user_plot <- plot_user_activity("path/to/your_project")

    # Display or save plots
    print(progress_plot)
    ggsave("progress.png", progress_plot, width = 8, height = 6)

## 📊 Data Structure Details

### Required Files

| File | Format | Description | Required Columns |
|----|----|----|----|
| `Documents.parquet` | Parquet | Document content | `DocID`, `HTML` |
| `Schema.csv` | CSV | Classification schema | `Class`, `Value` |
| `Classifications.parquet` | Parquet | Results (auto-created) | `DocID`, `UserID`, `Timestamp`, `{ClassNames}` |

### Schema Definition

The `Schema.csv` file defines your classification categories:

    Class,Value
    DocumentType,Contract
    DocumentType,Invoice
    DocumentType,Receipt
    Priority,High
    Priority,Medium
    Priority,Low
    Status,Pending
    Status,Complete

This creates:

- **DocumentType** with values: Contract, Invoice, Receipt

- **Priority** with values: High, Medium, Low

- **Status** with values: Pending, Complete

### Output Data

Classifications are automatically saved to `Classifications.parquet`
with:

    # Example output structure
    classifications <- data.frame(
      DocID = "doc_001",
      UserID = "analyst_1", 
      Timestamp = Sys.time(),
      DocumentType = "Contract",
      Priority = "High",
      Status = "Complete"
    )

## 🎯 Use Cases

### Legal Document Review

- Classify contracts by type, priority, and review status

- Track reviewer progress and workload distribution

- Generate reports on classification patterns

### Research Data Analysis

- Label research documents by methodology, topic, and relevance

- Monitor annotation quality across team members

- Export classified data for statistical analysis

### Content Moderation

- Categorize user-generated content by type and priority

- Track moderation progress and team performance

- Filter and review specific content categories

### Compliance Documentation

- Classify regulatory documents by compliance area

- Monitor document processing backlogs

- Generate compliance reporting metrics

## 🔧 Configuration

### Performance Optimization

For large document collections:

    # Adjust chunk sizes for better performance
    options(DT.options = list(pageLength = 25, scrollX = TRUE))

    # For very large datasets, consider filtering documents first
    subset_docs <- documents[sample(nrow(documents), 1000), ]
    arrow::write_parquet(subset_docs, "subset_Documents.parquet")

### Custom Styling

Modify the application appearance by customizing CSS in the module UI
functions or create custom themes.

## 🤝 Contributing

We welcome contributions! Please see our [Contributing
Guidelines](https://claude.ai/chat/CONTRIBUTING.md) for details.

### Development Setup

    # Clone the repository
    git clone https://github.com/yourusername/rLabelDocs.git
    cd rLabelDocs

    # Install development dependencies
    devtools::install_dev_deps()

    # Run tests
    devtools::test()

    # Check package
    devtools::check()

### Reporting Issues

Please report bugs and feature requests on our [GitHub
Issues](https://github.com/yourusername/rLabelDocs/issues) page.

## 📚 Citation

If you use rLabelDocs in your research, please cite:

    @Manual{rLabelDocs,
      title = {rLabelDocs: Interactive Document Classification and Labeling System},
      author = {Your Name},
      year = {2024},
      note = {R package version 0.1.0},
      url = {https://github.com/yourusername/rLabelDocs},
    }

## 📄 License

This project is licensed under the MIT License - see the
[LICENSE](https://claude.ai/chat/LICENSE) file for details.

## 🙏 Acknowledgments

- Built with [Shiny](https://shiny.rstudio.com/) for interactive web
  applications

- Uses [Apache Arrow](https://arrow.apache.org/docs/r/) for efficient
  data storage

- Visualization powered by [ggplot2](https://ggplot2.tidyverse.org/)

- Data manipulation with the [tidyverse](https://www.tidyverse.org/)

## 📞 Support

- 📖 **Documentation**: [Package
  Documentation](https://yourusername.github.io/rLabelDocs/)

- 💬 **Discussions**: [GitHub
  Discussions](https://github.com/yourusername/rLabelDocs/discussions)

- 🐛 **Bug Reports**: [GitHub
  Issues](https://github.com/yourusername/rLabelDocs/issues)

- 📧 **Email**: your.email@example.com

------------------------------------------------------------------------

**Happy Labeling! 🏷️**
