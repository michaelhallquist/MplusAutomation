// [[Rcpp::depends(BH)]]
// [[Rcpp::plugins(cpp11)]]

#include <Rcpp.h>
#include <boost/algorithm/string.hpp>

using namespace Rcpp;

// [[Rcpp::export]]
Rcpp::List parseMplusCpp(CharacterVector instring) {
  List outlist(instring.size());
  
  for(int i = 0; i < instring.size(); i++) {
    std::string text = Rcpp::as<std::string>(instring[i]);
    boost::trim(text);
    std::istringstream iss(text);
    std::vector<std::string> results((std::istream_iterator<std::string>(iss)),
                                     std::istream_iterator<std::string>());
    boost::split(results, text, [](char c){return c == ' ';}, boost::token_compress_on);
    outlist[i] = results;
  }
  return outlist;
}