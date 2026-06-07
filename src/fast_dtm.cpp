// [[Rcpp::depends(RcppParallel)]]
// [[Rcpp::plugins(cpp17)]]

#include <Rcpp.h>
#include <RcppParallel.h>
#include <unordered_map>
#include <unordered_set>
#include <vector>
#include <string>
#include <algorithm>
#include <cctype>

using namespace Rcpp;

struct DtmWorker : public RcppParallel::Worker {
  const std::vector<std::string>& docs;
  const std::unordered_set<std::string>& stopwords;
  bool remove_stopwords;
  std::vector<std::unordered_map<std::string, int>>& doc_counts;

  DtmWorker(const std::vector<std::string>& docs,
            const std::unordered_set<std::string>& stopwords,
            bool remove_stopwords,
            std::vector<std::unordered_map<std::string, int>>& doc_counts)
    : docs(docs),
      stopwords(stopwords),
      remove_stopwords(remove_stopwords),
      doc_counts(doc_counts) {}

  void operator()(std::size_t begin, std::size_t end) {
    for (std::size_t d = begin; d < end; ++d) {
      const std::string& text = docs[d];
      std::size_t start = 0;
      std::size_t n = text.size();

      while (start < n) {
        while (start < n && std::isspace(static_cast<unsigned char>(text[start]))) {
          ++start;
        }

        std::size_t stop = start;

        while (stop < n && !std::isspace(static_cast<unsigned char>(text[stop]))) {
          ++stop;
        }

        if (stop > start) {
          std::string word = text.substr(start, stop - start);

          if (!remove_stopwords || stopwords.find(word) == stopwords.end()) {
            doc_counts[d][word] += 1;
          }
        }

        start = stop;
      }
    }
  }
};

// [[Rcpp::export]]
Rcpp::List fast_dtm_cpp(Rcpp::CharacterVector text,
                        Rcpp::CharacterVector stopwords,
                        bool remove_stopwords = true) {

  int n_docs = text.size();

  std::vector<std::string> docs(n_docs);

  for (int i = 0; i < n_docs; ++i) {
    if (CharacterVector::is_na(text[i])) {
      docs[i] = "";
    } else {
      docs[i] = Rcpp::as<std::string>(text[i]);
    }
  }

  std::unordered_set<std::string> stopword_set;

  for (int i = 0; i < stopwords.size(); ++i) {
    if (!CharacterVector::is_na(stopwords[i])) {
      stopword_set.insert(Rcpp::as<std::string>(stopwords[i]));
    }
  }

  std::vector<std::unordered_map<std::string, int>> doc_counts(n_docs);

  DtmWorker worker(docs, stopword_set, remove_stopwords, doc_counts);
  RcppParallel::parallelFor(0, n_docs, worker);

  std::vector<std::string> vocab;

  {
    std::unordered_set<std::string> vocab_set;

    for (int d = 0; d < n_docs; ++d) {
      for (const auto& item : doc_counts[d]) {
        vocab_set.insert(item.first);
      }
    }

    vocab.assign(vocab_set.begin(), vocab_set.end());
    std::sort(vocab.begin(), vocab.end());
  }

  std::unordered_map<std::string, int> vocab_id;

  for (int j = 0; j < static_cast<int>(vocab.size()); ++j) {
    vocab_id[vocab[j]] = j + 1; // R uses 1-based indexing
  }

  std::vector<int> i_vec;
  std::vector<int> j_vec;
  std::vector<int> x_vec;

  for (int d = 0; d < n_docs; ++d) {
    for (const auto& item : doc_counts[d]) {
      i_vec.push_back(d + 1);
      j_vec.push_back(vocab_id[item.first]);
      x_vec.push_back(item.second);
    }
  }

  return Rcpp::List::create(
    Rcpp::Named("i") = i_vec,
    Rcpp::Named("j") = j_vec,
    Rcpp::Named("x") = x_vec,
    Rcpp::Named("vocab") = vocab
  );
}
