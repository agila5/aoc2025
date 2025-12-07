#include <Rcpp.h>

// [[Rcpp::export]]
int rotate_left_cpp(int point, int value) {
  int out {point}; 
  Rcpp::Environment env = Rcpp::Environment::global_env();
  int ct {env["counter"]};
  
  for (auto i = 1; i <= value; i++) {
    out -= 1; 
    if (out < 0) {
      out += 100; 
    }
    if (out == 0) {
      ct += 1;
    }
  }
  env.assign("counter", ct); 
  return out;
}

// [[Rcpp::export]]
int rotate_right_cpp(int point, int value) {
  int out {point};
  Rcpp::Environment env = Rcpp::Environment::global_env();
  int ct {env["counter"]};
  
  for (auto i = 1; i <= value; i++) {
    out += 1; 
    if (out > 99) {
      out -= 100; 
    }
    if (out == 0) {
      ct += 1; 
    }
  }
  env.assign("counter", ct);
  return out; 
}

/*** R
rotate_left_cpp(50, 68)
rotate_left_cpp(82, 30)
rotate_right_cpp(52, 48)
rotate_right_cpp(95, 60)
*/
