
library(IkhnosToolBox)

# load data ---------------------------------------

data(right_femur)
load_bone(right_femur)

# if file path is unknown:

#example_c <- load_marks("circular", plot = TRUE)
#example_l <- load_marks("linear", plot = TRUE, colour_value = "red")

# if file path is known:

example_c <- load_marks(
  "C:\\Users\\Lloyd\\Desktop\\TIDOP\\IKHNOS\\prueba ikhnos -R\\DATOS PRUEBA FEMUR\\B_femur R_A_medium.txt",
  mark_type = "circular",
  plot = TRUE
)
example_l <- load_marks(
  "C:\\Users\\Lloyd\\Desktop\\TIDOP\\IKHNOS\\prueba ikhnos -R\\DATOS PRUEBA FEMUR\\E_femur R_A&SA_medium.txt",
  mark_type = "linear",
  plot = TRUE,
  colour_value = "red"
)

save_3d_image("trial")

#

# Statistics ----------------------------

example_circular <- extract_spatial_data(example_c, mark_type = "circular", plot_results == TRUE)
example_linear <- extract_spatial_data(example_l, "linear")

#extract_spatial_data(data = example_c,
#                     marK_type = "circular",
#                     plot_results == TRUE)
