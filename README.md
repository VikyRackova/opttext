opttext
================

**opttext** is an R package for high-performance text preprocessing,
document-term matrix construction, and exploratory text analysis. The
package is designed for workflows that need efficient handling of large
text corpora while minimizing memory overhead and unnecessary
intermediate objects.

## Motivation and problem scope

Text analysis in R often relies on token-based workflows that are
flexible but can be computationally expensive for large corpora.
`opttext` addresses this problem by operating directly on character
vectors whenever possible and by using sparse matrix representations for
downstream analysis.

The package is intended to provide:

- Fast preprocessing of raw text.
- Direct construction of document-term matrices.
- Sparse matrix trimming for feature reduction.
- Exploratory visualizations for text data.
- A lightweight workflow that is efficient, modular, and reproducible.

## Core features

`opttext` currently includes four main components:

1.  Fast text preprocessing.
2.  Fast document-term matrix construction.
3.  Sparse matrix trimming.
4.  Exploratory text visualizations.

## Installation

``` r
devtools::install_github("VikyRackova/opttext")
```

## Package structure

The package is organized around separate tasks:

- Preprocessing functions for common text cleaning operations.
- Matrix construction functions for creating sparse document-term
  matrices.
- Trimming functions for reducing sparse noise and memory usage.
- Visualization functions for exploratory analysis.
- Included datasets for examples and testing.

This structure reflects a modular design where each component has a
clear role in the overall workflow.

## Data processing approach

`opttext` is designed to reduce unnecessary overhead in text workflows
by:

- Processing character vectors directly where possible.
- Avoiding intermediate token objects in the main pipeline.
- Using sparse matrix storage for document-term representations.
- Supporting parallel processing with `RcppParallel`.
- Using compiled code with `Rcpp` for performance-critical steps.

These implementation choices are intended to make the package suitable
for larger corpora and computationally intensive text analysis tasks.

## Object design

The main document-term matrix object returned by `fast_dtm()` is a
custom S4 object that extends `dgCMatrix`. This supports compatibility
with sparse matrix workflows while also storing corpus-specific
information such as:

- Vocabulary.
- Term frequency.
- Document frequency.
- Document-level metadata.

## Example use cases

`opttext` can be used in workflows such as:

- Cleaning and transforming raw text corpora.
- Creating sparse matrices for statistical or machine learning models.
- Removing rare terms before topic modeling or classification.
- Exploring term frequencies across documents or groups.
- Preparing text data for downstream sparse-matrix-based methods.

The following workflow shows the intended use of the package: clean
text, build a sparse representation, reduce uninformative features, and
explore the corpus visually.

``` r
texts <- c(
  "Inflation increased during this quarter.",
  "Economic growth remained stable.",
  "Inflation growth belongs to important indicators."
)

clean_texts <- lowercase(texts)
dtm <- fast_dtm(clean_texts)
dtm_small <- dtm_trim(dtm, min_docfreq = 2, min_termfreq = 2)

plot_top_terms(dtm_small)
```

The output of `fast_dtm()` can be also used directly with topic modeling
methods that accept sparse matrices.

``` r
topicmodels::LDA(dtm, k = 10)
```

## Included datasets

The package includes curated text datasets that serve both as examples
and as practical test cases for evaluating preprocessing performance,
document-term matrix construction, and exploratory text-analysis
techniques.

#### FED_Minutes

Federal Open Market Committee meeting minutes from March 1994 to
November 2024.

``` r
data("FED_Minutes")
```

Variables:

- Date
- Text

#### The_Times

Articles from *The Times* discussing government and public
administration between 2020 and 2025.

``` r
data("The_Times")
```

Variables:

- Date
- Text

## Relationship to other packages

`opttext` is designed to build upon established text analysis workflows
rather than replace them. Its functions follow familiar logic from
widely used packages such as `quanteda`, `tidytext`, and `tm`, but are
implemented with a stronger focus on computational efficiency and
scalability.

For example, `lowercase()` follows the same basic text-normalization
logic used in existing workflows, but extends it with parallel
processing to make it more suitable for large corpora. Similarly,
`fast_dtm()` builds on the logic of existing document-term matrix
constructors while returning an S4 object that remains directly
compatible with downstream methods such as latent Dirichlet allocation.

A separate benchmark file `benchmark.R` in the repository compares
`opttext` with established packages and shows that the package performs
competitively, and in many cases faster, for the supported operations.

## Documentation

Each exported function includes documentation with arguments, return
values, and examples. For usage details, see the package help pages
after installation.

## Author

Viktoria Račková  
Master’s Programme in Financial and Economic Research specialization
Econometrics  
Maastricht University
