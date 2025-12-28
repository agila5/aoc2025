#include <Rcpp.h>

double compute_area(Rcpp::NumericVector point1, Rcpp::NumericVector point2) {
  // Assuming point1 and point2 are of length 2 representing (x, y)
  double width = std::abs(point2[0] - point1[0]) + 1;
  double height = std::abs(point2[1] - point1[1]) + 1;
  return width * height;
}
  
// [[Rcpp::export]]
double compute_max_area(Rcpp::List input) {
  /* Input is a list of numeric vectors representing coordinates (like (x, y)). 
  I need to compute the area between each pair of coordinates. */
  double max_area {0.0}; 
  for (int i = 0; i < input.size() - 1; ++i) {
    for (int j = i + 1; j < input.size(); ++j) {
      Rcpp::NumericVector point1 = input[i];
      Rcpp::NumericVector point2 = input[j];
      double area = compute_area(point1, point2);
      if (area > max_area) {
        max_area = area;
      }
    }
  }
  
  return max_area;
}


/*** R
compute_max_area(list(c(2, 5), c(9, 7)))
compute_max_area(list(c(7, 1), c(11, 7)))
compute_max_area(list(c(7, 3), c(2, 3)))
compute_max_area(list(c(2, 5), c(11, 1)))
*/
