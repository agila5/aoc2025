#include <Rcpp.h>
#include <ranges>

int compute_nDigits(long long int x) {
  int digits = 0; 
  while (x) {
    x /= 10; 
    digits++; 
  }
  return digits; 
}

long long int pow10(int n) {
  static const long long int pow10_lookup[10] = {
    1LL, 10LL, 100LL, 1000LL, 10000LL, 
    100000LL, 1000000LL, 10000000LL, 100000000LL, 1000000000LL
  };
  return pow10_lookup[n]; 
}

// [[Rcpp::export]]
Rcpp::NumericVector find_IDs(std::string start, std::string end) {
  auto start_int = std::stoll(start); 
  auto end_int = std::stoll(end);
  Rcpp::NumericVector out; 
  
  for (auto i = start_int; i <= end_int; i++) {
    auto nDigits = compute_nDigits(i); 
    if (nDigits % 2 != 0) continue; 

    auto ten_exp = pow10(nDigits / 2); 
    
    auto first_half = i / ten_exp; 
    auto second_half = i % ten_exp; 
    if (first_half == second_half) {
      out.push_back(static_cast<double>(i)); 
    }
  }
  
  return out; 
}

Rcpp::IntegerVector compute_divisors(int x) {
  Rcpp::IntegerVector out {1}; 
  
  if (x <= 2) {
    return out; 
  }

  for (int i = 2; i < x; i++) {
    if ((x % i) == 0) {
      out.push_back(i); 
    }
  }
  
  return out; 
}

// [[Rcpp::export]]
Rcpp::NumericVector find_all_silly_patterns(
    std::string start, 
    std::string end
) {
  auto start_int = std::stoll(start); 
  auto end_int = std::stoll(end);
  Rcpp::NumericVector out; 
  
  for (auto num = start_int; num <= end_int; num++) {
    // Since 1..10 cannot have any sequence
    if (num < 11) continue; 
    auto num_string = std::to_string(num); // This is important for inner loop
    auto nDigits = compute_nDigits(num); 
    
    Rcpp::IntegerVector divisors {compute_divisors(nDigits)}; 

    for (auto seq_size : divisors) {
       
      std::vector<std::string> num_substrings; 
      // Split the string into pieces of size seq_size
      for (std::size_t step = 0; step < num_string.length(); step = step + seq_size) {
        std::string num_substring {num_string.substr(step, seq_size)}; 
        num_substrings.push_back(num_substring); 
      }
      
      // Check if all elements are equal
      if (std::all_of(
          num_substrings.begin(), 
          num_substrings.end(), 
          [&](std::string s) {return s == num_substrings[0];}
      )) {
        out.push_back(static_cast<double>(num)); 
        break;
      }
    }
  }
  
  return out; 
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
find_all_silly_patterns("998", "1012")
*/
