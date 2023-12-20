#include <Rcpp.h>

double SeedFromLoc(const Rcpp::NumericMatrix &map,
                   const std::vector<int> &st,
                   double id, int i) {

    std::vector<int> idx;

    for (int k = st[i], e = st[i + 1L]; k < e; ++k) {
        if (map(k, 0) <= id) idx.push_back(k);
    }

    if (idx.size()) {
        double my_max  = -1;
        int max_idx = 0;

        for (auto k: idx) {
            if (map(k, 0) > my_max) {
                my_max  = map(k, 0);
                max_idx = k;
            }
        }

        if ((map(max_idx, 0) + map(max_idx, 2)) > id) {
            id += (map(max_idx, 1) - map(max_idx, 0));
            return (i == 0) ? id : SeedFromLoc(map, st, id, i - 1);
        }
    }

    return SeedFromLoc(map, st, id, i - 1);
}

// [[Rcpp::export]]
double FindMinLoc(
    const Rcpp::NumericMatrix &map, const std::vector<int> &start,
    const std::vector<double> &seed_low, const std::vector<double> &seed_high,
    double low, double high, int length_map
) {

    for (double i = low; i <= high; ++i) {
        const double id = SeedFromLoc(map, start, i, length_map);

        const bool check_1 = std::any_of(
            seed_low.begin(), seed_low.end(), [id](double s) {
                return id >= s;
            }
        );

        if (check_1) {
            for (int j = 0; j < seed_low.size(); ++j) {
                if (id >= seed_low[j]) {
                    if (id <= seed_high[j]) {
                        return(i);
                    }
                }
            }
        }
    }

    return -1;
}
