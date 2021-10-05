
library(IkhnosToolBox)

# load data ---------------------------------------

data(right_femur)
load_bone(right_femur)

# if file path is unknown:

#example_c <- load_marks("circular", plot = TRUE)
#example_l <- load_marks("linear", plot = TRUE, colour_value = "red")

# if file path is known:

example_1 <- load_marks(
  "C:\\Users\\Lloyd\\Desktop\\TIDOP\\IKHNOS\\prueba ikhnos -R\\DATOS PRUEBA FEMUR\\B_femur R_A_medium.txt",
  mark_type = "circular",
  plot = TRUE
)
example_2 <- load_marks(
  "C:\\Users\\Lloyd\\Desktop\\TIDOP\\IKHNOS\\prueba ikhnos -R\\DATOS PRUEBA FEMUR\\E_femur R_A&SA_medium.txt",
  mark_type = "linear",
  plot = TRUE,
  colour_value = "red"
)

save_3d_image("trial")

#

# Statistics ----------------------------

example_circular <- extract_spatial_data(example_1, mark_type = "circular", plot_results = TRUE)
example_linear <- extract_spatial_data(example_2, "linear", plot_results = TRUE)

perform_CSR_analyses(example_circular, n_permutations = 200)
perform_CSR_analyses(example_linear, n_permutations = 200)

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

tsne_calculation(sample1_sample2, group_labels)

# 1st note - X11 is platform specific!
# 2nd not - unsupervised approaches for tsne?????

tsne_calculation(sample1_sample2)

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

# skeletal profile analysis ------------------

# series 1

h1 <- load_marks(
  "C:\\Users\\Lloyd\\Desktop\\TIDOP\\IKHNOS\\prueba ikhnos -R\\DATOS PRUEBA FEMUR\\H_humerus L&R_A&SA_medium.txt",
  "circular", plot = FALSE
)
f1 <- load_marks(
  "C:\\Users\\Lloyd\\Desktop\\TIDOP\\IKHNOS\\prueba ikhnos -R\\DATOS PRUEBA FEMUR\\F_femur L&R_A&SA_medium.txt",
  "circular", plot = FALSE
)
r1 <- load_marks(
  "C:\\Users\\Lloyd\\Desktop\\TIDOP\\IKHNOS\\prueba ikhnos -R\\DATOS PRUEBA FEMUR\\R_radius L&R_A&SA_medium.txt",
  "circular", plot = FALSE
)
t1 <- load_marks(
  "C:\\Users\\Lloyd\\Desktop\\TIDOP\\IKHNOS\\prueba ikhnos -R\\DATOS PRUEBA FEMUR\\T_tibia L&R_A&SA_medium.txt",
  "circular", plot = FALSE
)

# series 2

h2 <- load_marks(
  "C:\\Users\\Lloyd\\Desktop\\TIDOP\\IKHNOS\\prueba ikhnos -R\\DATOS PRUEBA FEMUR\\H2_humerus L&R_A&SA_small.txt",
  "circular", plot = FALSE
)
f2 <- load_marks(
  "C:\\Users\\Lloyd\\Desktop\\TIDOP\\IKHNOS\\prueba ikhnos -R\\DATOS PRUEBA FEMUR\\F2_femur L&R_A&SA_small.txt",
  "circular", plot = FALSE
)
r2 <- load_marks(
  "C:\\Users\\Lloyd\\Desktop\\TIDOP\\IKHNOS\\prueba ikhnos -R\\DATOS PRUEBA FEMUR\\R2_radius L&R_A&SA_small.txt",
  "circular", plot = FALSE
)
t2 <- load_marks(
  "C:\\Users\\Lloyd\\Desktop\\TIDOP\\IKHNOS\\prueba ikhnos -R\\DATOS PRUEBA FEMUR\\T2_tibia L&R_A&SA_small.txt",
  "circular", plot = FALSE
)

# series 3

h3 <- load_marks(
  "C:\\Users\\Lloyd\\Desktop\\TIDOP\\IKHNOS\\prueba ikhnos -R\\DATOS PRUEBA FEMUR\\series 3\\humerus L&R_A_large.txt",
  "circular", plot = FALSE
)
f3 <- load_marks(
  "C:\\Users\\Lloyd\\Desktop\\TIDOP\\IKHNOS\\prueba ikhnos -R\\DATOS PRUEBA FEMUR\\series 3\\femur L&R_A_large.txt",
  "circular", plot = FALSE
)
r3 <- load_marks(
  "C:\\Users\\Lloyd\\Desktop\\TIDOP\\IKHNOS\\prueba ikhnos -R\\DATOS PRUEBA FEMUR\\series 3\\radius R_A_large.txt",
  "circular", plot = FALSE
)
t3 <- load_marks(
  "C:\\Users\\Lloyd\\Desktop\\TIDOP\\IKHNOS\\prueba ikhnos -R\\DATOS PRUEBA FEMUR\\series 3\\tibia L&R_A_large.txt",
  "circular", plot = FALSE
)

time_series_1 <- create_time_series(h = h1, f = f1, r = r1, t = t1)
time_series_2 <- add_time_series(h = h2, f = f2, r = r2, t = t2)
time_series_3 <- add_time_series(h = h3, f = f3, r = r3, t = t3)

series_database <- rbind(time_series_1, time_series_2, time_series_3)
series_database$Sample <- as.factor(
  c(
    rep("Series_1", nrow(time_series_1)),
    rep("Series_2", nrow(time_series_2)),
    rep("Series_3", nrow(time_series_3))
  )
)

time_series_clustering(series_database)


#

