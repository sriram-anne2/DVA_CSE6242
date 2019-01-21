#GTUsername: sanne31@gatech.edu
edg_fb <- read.table("0.edges")
feat_fb <- read.table("0.feat")
fb_network <- graph_from_data_frame(d = edg_fb, vertices = feat_fb, directed = FALSE) 
names_fb <- read.table("0.featnames")

#using V80 because that is the gender feature as obtained in the names_fb table having all feature names for this particular node
V(fb_network)$V80=as.character(feat_fb$V80[match(V(fb_network)$name,feat_fb$V1)])
V(fb_network)$color=V(fb_network)$V80
V(fb_network)$color=gsub("0","green",V(fb_network)$color)
V(fb_network)$color=gsub("1","red",V(fb_network)$color)
V(fb_network)$size=3

plot.igraph(fb_network, vertex.label=NA,layout=layout.fruchterman.reingold)
