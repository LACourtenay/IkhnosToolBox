
library(IkhnosToolBox)

# load data ---------------------------------------

data(right_femur)
load_bone(right_femur)

data("femur_right_circular1")
data("femur_right_linear1")
data("femur_right_linear2")

# if file path is unknown:

#example_c <- load_marks("femur_right_circular1", plot = TRUE)
#example_l <- load_marks("linear", plot = TRUE, colour_value = "red")

# if file path is known:

example_1 <- load_marks(femur_right_circular1,
  mark_type = "circular",
  plot = TRUE
)

example_2 <- load_marks(
  femur_right_linear1,
  mark_type = "linear",
  plot = TRUE,
  colour_value = "red"
)

save_3d_image("femur_R_large_captive")

#

# Statistics ----------------------------

example_circular <- extract_spatial_data(example_1, mark_type = "circular")
example_linear <- extract_spatial_data(example_2, "linear")

perform_CSR_analyses(example_circular, n_permutations = 101)
perform_CSR_analyses(example_linear, n_permutations = 101)

sp_distance_matrix(example_circular)
sp_distance_matrices(example_circular, example_linear,
                     name_1 = "circular", name_2 = "linear")

#

# tSNE ------------------------------------

sample1_coords <- as.matrix(example_circular)
sample2_coords <- as.matrix(example_linear)
sample1_sample2 <- rbind(sample1_coords, sample2_coords)
group_labels <- as.factor(c(
  rep("circular", nrow(sample1_coords)),
  rep("linear", nrow(sample2_coords))
))

tsne_calculation(sample1_sample2, group_labels, bone = right_femur)

tsne_calculation(sample1_sample2, bone = right_femur)

#

# Wavelet analysis ------------------------

example_time_series <- two_sample_histogram_distributions(
  example_circular, example_linear,
  "circular", "linear",
  "x", "femur"
)

wavelet_analysis(
  example_time_series$first_sample_ts,
  example_time_series$second_sample_ts,
  "circular", "linear"
)

#

# Calculate orientations ------------------

data("right_femur")
data("femur_right_linear1")

example_lin_1 <- load_marks(
  femur_right_linear1,
  mark_type = "linear",
  plot = FALSE
)
example_lin_2 <- load_marks(
  femur_right_linear2,
  mark_type = "linear",
  plot = FALSE
)

right_1 <- calculate_orientations(example_lin_1, right_femur)
right_2 <- calculate_orientations(example_lin_2, right_femur)

descriptive_circular_analysis(right_1)
descriptive_circular_analysis(right_2)

preferential_orientation_test(right_1)
preferential_orientation_test(right_2)

d1 <- compare_two_sample_orientations(right_1, right_2)

#

# skeletal profile analysis ------------------

data("humerus_right_circular1")
data("humerus_right_circular2")
data("humerus_right_circular3")
data("femur_right_circular1")
data("femur_right_circular2")
data("femur_right_circular3")
data("tibia_right_circular1")
data("tibia_right_circular2")
data("tibia_right_circular3")
data("radius_right_circular1")
data("radius_right_circular2")
data("radius_right_circular3")
data("metatarsus_right_circular1")
data("metatarsus_right_circular2")
data("metatarsus_right_circular3")

data("metacarpus_right_circular1")

# series 1

h1 <- load_marks(
  humerus_right_circular1,
  mark_type = "circular", plot = FALSE
)
f1 <- load_marks(
  femur_right_circular1,
  mark_type = "circular", plot = FALSE
)
r1 <- load_marks(
  radius_right_circular1,
  mark_type = "circular", plot = FALSE
)
t1 <- load_marks(
  tibia_right_circular1,
  mark_type = "circular", plot = FALSE
)
mt1 <- load_marks(
  metatarsus_right_circular1,
  mark_type = "circular", plot = FALSE
)
mc1 <- load_marks(
  metacarpus_right_circular1,
  mark_type = "circular", plot = FALSE
)

# series 2

h2 <- load_marks(
  humerus_right_circular2,
  mark_type = "circular", plot = FALSE
)
f2 <- load_marks(
  femur_right_circular2,
  mark_type = "circular", plot = FALSE
)
r2 <- load_marks(
  radius_right_circular2,
  mark_type = "circular", plot = FALSE
)
t2 <- load_marks(
  tibia_right_circular2,
  mark_type = "circular", plot = FALSE
)
mt2 <- load_marks(
  metatarsus_right_circular2,
  mark_type = "circular", plot = FALSE
)

# series 3

h3 <- load_marks(
  humerus_right_circular3,
  mark_type = "circular", plot = FALSE
)
f3 <- load_marks(
  femur_right_circular3,
  mark_type = "circular", plot = FALSE
)
r3 <- load_marks(
  radius_right_circular3,
  mark_type = "circular", plot = FALSE
)
t3 <- load_marks(
  tibia_right_circular3,
  mark_type = "circular", plot = FALSE
)
mt3 <- load_marks(
  metatarsus_right_circular3,
  mark_type = "circular", plot = FALSE
)


time_series_1 <- create_time_series(h = h1, f = f1, r = r1, t = t1, mt = mt1)
time_series_2 <- add_time_series(h = h2, f = f2, r = r2, t = t2, mt = mt2, colour = "red")
time_series_3 <- add_time_series(h = h3, f = f3, r = r3, t = t3, mt = mt3, colour = "blue")

#series_database <- rbind(time_series_1, time_series_2, time_series_3)
#series_database$Sample <- as.factor(
#  c(
#    rep("Series_1", nrow(time_series_1)),
#    rep("Series_2", nrow(time_series_2)),
#    rep("Series_3", nrow(time_series_3))
#  )
#)

# time_series_clustering(series_database)

#


