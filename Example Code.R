
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

#

