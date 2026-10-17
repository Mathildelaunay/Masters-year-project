#How to save plots in different ways:

#A singular plot------------------------------------------
ggsave(
  filename = "visualisation_1.pdf",
  plot = plot_name,
  width = 10,
  height = 6,
  units = "in"
)

#Several plots--------------------------------------------
pdf(
  "all_visualisations.pdf",
  width = 10,
  height = 6
)

print(p1)
print(p2)
print(p3)

dev.off()

#Save several individual PDFs automatically----------------
dir.create("visualisations", showWarnings = FALSE)

ggsave("visualisations/visualisation_1.pdf", p1,
       width = 10, height = 6)

ggsave("visualisations/visualisation_2.pdf", p2,
       width = 10, height = 6)

ggsave("visualisations/visualisation_3.pdf", p3,
       width = 10, height = 6)