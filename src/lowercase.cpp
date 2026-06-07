// [[Rcpp::depends(RcppParallel)]]
#include <Rcpp.h>
#include <RcppParallel.h>

#include <string>
#include <vector>
#include <unordered_map>

#include <unicode/unistr.h>
#include <unicode/locid.h>

using namespace Rcpp;

inline void lower_inplace(std::string& x) {
  bool ascii = true;

  for (std::size_t i = 0; i < x.size(); i++) {
    unsigned char c = x[i];

    if (c > 127) {
      ascii = false;
      break;
    }

    if (c >= 'A' && c <= 'Z') {
      x[i] = c + 32;
    }
  }

  if (!ascii) {
    icu::UnicodeString u = icu::UnicodeString::fromUTF8(x);
    u.toLower(icu::Locale::getRoot());

    std::string out;
    u.toUTF8String(out);

    x.swap(out);
  }
}

struct ReconstructWorker : public RcppParallel::Worker {

  const std::vector<int>& ids;
  const std::vector<std::size_t>& doc_start;
  const std::vector<std::size_t>& doc_length;
  const std::vector<std::string>& vocabulary;
  std::vector<std::string>& result;

  ReconstructWorker(const std::vector<int>& ids,
                    const std::vector<std::size_t>& doc_start,
                    const std::vector<std::size_t>& doc_length,
                    const std::vector<std::string>& vocabulary,
                    std::vector<std::string>& result)
    : ids(ids),
      doc_start(doc_start),
      doc_length(doc_length),
      vocabulary(vocabulary),
      result(result) {}

  void operator()(std::size_t begin, std::size_t end) {

    for (std::size_t i = begin; i < end; i++) {

      std::size_t start = doc_start[i];
      std::size_t len   = doc_length[i];

      if (len == 0) {
        result[i] = "";
        continue;
      }

      std::size_t out_size = 0;

      for (std::size_t j = 0; j < len; j++) {
        out_size += vocabulary[ids[start + j]].size();
      }

      out_size += len - 1;

      std::string output;
      output.reserve(out_size);

      for (std::size_t j = 0; j < len; j++) {
        if (j > 0) output.push_back(' ');
        output += vocabulary[ids[start + j]];
      }

      result[i] = output;
    }
  }
};


// [[Rcpp::export]]
CharacterVector cpp_lowercase(CharacterVector texts) {

  int n = texts.size();

  std::unordered_map<std::string, int> word_to_id;
  word_to_id.reserve(200000);

  std::vector<std::string> vocabulary;
  vocabulary.reserve(200000);

  std::vector<int> ids;
  ids.reserve(2000000);

  std::vector<std::size_t> doc_start(n);
  std::vector<std::size_t> doc_length(n);
  std::vector<int> is_na(n, 0);

  int next_id = 0;

  for (int i = 0; i < n; i++) {

    doc_start[i] = ids.size();
    doc_length[i] = 0;

    if (CharacterVector::is_na(texts[i])) {
      is_na[i] = 1;
      continue;
    }

    std::string text = Rcpp::as<std::string>(texts[i]);

    std::size_t token_start = 0;
    std::size_t text_size = text.size();

    for (std::size_t j = 0; j <= text_size; j++) {

      if (j == text_size || text[j] == ' ') {

        if (j > token_start) {

          std::string word = text.substr(token_start, j - token_start);

          auto it = word_to_id.find(word);

          if (it == word_to_id.end()) {

            int id = next_id++;

            word_to_id.emplace(word, id);

            lower_inplace(word);
            vocabulary.push_back(word);

            ids.push_back(id);

          } else {
            ids.push_back(it->second);
          }

          doc_length[i]++;
        }

        token_start = j + 1;
      }
    }
  }

  std::vector<std::string> result(n);

  ReconstructWorker worker(ids, doc_start, doc_length, vocabulary, result);
  RcppParallel::parallelFor(0, n, worker);

  CharacterVector out(n);

  for (int i = 0; i < n; i++) {
    if (is_na[i]) {
      out[i] = NA_STRING;
    } else {
      out[i] = result[i];
    }
  }

  return out;
}
