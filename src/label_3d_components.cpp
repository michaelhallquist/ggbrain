#include "ggbrain.h"

#include <vector>

namespace {
struct Offset3D {
  int di;
  int dj;
  int dk;

  Offset3D(int di_value, int dj_value, int dk_value)
    : di(di_value), dj(dj_value), dk(dk_value) {}
};
}

//' Label connected components in a 3D logical array
//'
//' @name label_3d_components_cpp
//' @param mask A 3D logical array with TRUE for foreground voxels.
//' @param nn Connectivity level: 1 for faces, 2 for faces and edges, or 3 for
//'   faces, edges, and corners.
//' @param min_size Minimum component size to include in the returned statistics.
//' @param return_labels Whether to return the labeled volume.
//' @return A list containing component statistics and, when requested, labels.
//' @keywords internal
// [[Rcpp::export]]
Rcpp::List label_3d_components_cpp(
    const Rcpp::LogicalVector& mask,
    int nn = 3,
    int min_size = 1,
    bool return_labels = false) {
  if (nn < 1 || nn > 3) {
    Rcpp::stop("nn must be 1, 2, or 3");
  }
  if (min_size < 1) {
    Rcpp::stop("min_size must be at least 1");
  }

  Rcpp::IntegerVector dims = mask.attr("dim");
  if (dims.size() != 3) {
    Rcpp::stop("mask must be a 3D array");
  }

  const int ni = dims[0];
  const int nj = dims[1];
  const int nk = dims[2];
  const R_xlen_t n_voxels = mask.size();

  for (R_xlen_t index = 0; index < n_voxels; ++index) {
    if (mask[index] == NA_LOGICAL) {
      Rcpp::stop("mask cannot contain missing values");
    }
  }

  std::vector<Offset3D> offsets;
  offsets.reserve(26);
  for (int dk = -1; dk <= 1; ++dk) {
    for (int dj = -1; dj <= 1; ++dj) {
      for (int di = -1; di <= 1; ++di) {
        const int distance = std::abs(di) + std::abs(dj) + std::abs(dk);
        if (distance > 0 && distance <= nn) {
          offsets.push_back(Offset3D(di, dj, dk));
        }
      }
    }
  }

  Rcpp::IntegerVector labels(n_voxels);
  labels.attr("dim") = dims;

  std::vector<int> cluster_ids;
  std::vector<int> cluster_sizes;
  std::vector<double> com_i;
  std::vector<double> com_j;
  std::vector<double> com_k;
  std::vector<R_xlen_t> queue;
  std::vector<bool> keep_label(1, false);
  int next_label = 1;

  for (R_xlen_t seed = 0; seed < n_voxels; ++seed) {
    if (mask[seed] == FALSE || labels[seed] != 0) {
      continue;
    }

    queue.clear();
    queue.push_back(seed);
    labels[seed] = next_label;

    R_xlen_t head = 0;
    int size = 0;
    double sum_i = 0.0;
    double sum_j = 0.0;
    double sum_k = 0.0;

    while (head < static_cast<R_xlen_t>(queue.size())) {
      const R_xlen_t index = queue[head++];
      const int i = index % ni;
      const R_xlen_t plane_index = index / ni;
      const int j = plane_index % nj;
      const int k = plane_index / nj;

      ++size;
      sum_i += i + 1;
      sum_j += j + 1;
      sum_k += k + 1;

      for (std::vector<Offset3D>::const_iterator offset = offsets.begin();
           offset != offsets.end(); ++offset) {
        const int neighbor_i = i + offset->di;
        const int neighbor_j = j + offset->dj;
        const int neighbor_k = k + offset->dk;

        if (neighbor_i < 0 || neighbor_i >= ni ||
            neighbor_j < 0 || neighbor_j >= nj ||
            neighbor_k < 0 || neighbor_k >= nk) {
          continue;
        }

        const R_xlen_t neighbor_index =
          neighbor_i + static_cast<R_xlen_t>(ni) *
            (neighbor_j + static_cast<R_xlen_t>(nj) * neighbor_k);

        if (mask[neighbor_index] == TRUE && labels[neighbor_index] == 0) {
          labels[neighbor_index] = next_label;
          queue.push_back(neighbor_index);
        }
      }
    }

    if (size >= min_size) {
      cluster_ids.push_back(next_label);
      cluster_sizes.push_back(size);
      com_i.push_back(sum_i / size);
      com_j.push_back(sum_j / size);
      com_k.push_back(sum_k / size);
      keep_label.push_back(true);
    } else {
      keep_label.push_back(false);
    }

    ++next_label;
  }

  if (return_labels && min_size > 1) {
    for (R_xlen_t index = 0; index < n_voxels; ++index) {
      const int label = labels[index];
      if (label > 0 && !keep_label[label]) {
        labels[index] = 0;
      }
    }
  }

  Rcpp::DataFrame clusters = Rcpp::DataFrame::create(
    Rcpp::Named("cluster_id") = Rcpp::wrap(cluster_ids),
    Rcpp::Named("size") = Rcpp::wrap(cluster_sizes),
    Rcpp::Named("com_i") = Rcpp::wrap(com_i),
    Rcpp::Named("com_j") = Rcpp::wrap(com_j),
    Rcpp::Named("com_k") = Rcpp::wrap(com_k)
  );
  SEXP label_result = return_labels ? static_cast<SEXP>(labels) : R_NilValue;

  return Rcpp::List::create(
    Rcpp::Named("clusters") = clusters,
    Rcpp::Named("labels") = label_result
  );
}
