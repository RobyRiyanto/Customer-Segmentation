# Reads csv data and inserts it into customer variables
customer <- read.csv("https://academy.dqlab.id/dataset/customer_segments.txt", sep="\t")

# Data conversion with data.matrix
customer_matrix <- data.matrix(customer[c("Jenis.Kelamin", "Profesi", "Tipe.Residen")])

# Merge the data back into our original variables
customer <- data.frame(customer, customer_matrix)

# Normalize Shopping Value
customer$NilaiBelanjaSetahun <- customer$NilaiBelanjaSetahun/1000000

# Fill in master data
Profesi <- unique(customer[c("Profesi", "Profesi.1")])
Jenis.Kelamin <- unique(customer[c("Jenis.Kelamin","Jenis.Kelamin.1")])
Tipe.Residen <- unique(customer[c("Tipe.Residen","Tipe.Residen.1")])

# Create a variable field that is used
field_used = c("Jenis.Kelamin.1","Umur","Profesi.1", "Tipe.Residen.1", "NilaiBelanjaSetahun")


# Clustering and K-Means Algorithms
# K-Means section
set.seed (100)

# function kmeans to form 5 clusters with 25 random scenarios and save them into segmentation variables
segmentation <- kmeans(x=customer[field_used], centers=5, nstart=25)



# Elbow CUrve
# Function to call the kmeans function for a range of number of clusters
sse <- sapply(1:10,
              function(param_k)
              {
                kmeans(customer[field_used], param_k, nstart=25)$tot.withinss
              }
)

# Plot
total_cluster_max <- 10
ssdata = data.frame(cluster=c(1:total_cluster_max),sse)
ggplot(ssdata, aes(x=cluster,y=sse)) +
  geom_line(color="red") +
  geom_point() +
  ylab("Within Cluster Sum of Squares") + 
  xlab("Number of Cluster") +
  geom_text(aes(label=format(round(sse, 2), nsmall = 2)),hjust=-0.2, vjust=-0.5) +
  scale_x_discrete(limits=c(1:total_cluster_max))


# Create Model
# Naming Segments
Customer.Segment <- data.frame(cluster=c(1,2,3,4,5), Segmen.Name=c("Silver Youth Gals", "Diamond Professional", "Silver Mid Professional", "Gold Young Professional", "Diamond Senior Member"))

# Combining all assets into the Identity cluster variable
identity_cluster <- list(Profesi=Profesi, Jenis.Kelamin=Jenis.Kelamin, Tipe.Residen=Tipe.Residen, segmentation=segmentation, Customer.Segment=Customer.Segment, field_used=field_used)

# Saving Objects Model in File
saveRDS(identity_cluster,"cluster.rds")
