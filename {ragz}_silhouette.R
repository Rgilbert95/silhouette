#####function to get the indexes of the vector points in a cluster
inde_cluster= function(labels, cluster){
  output=numeric()
  for(i in labels)
  {
    for(j in i)
      output= which(labels %in% c(cluster))
  }
  return(output)
}

#Function to get the distance in 2 multi-dimensional points
get_distance= function(el1, el2){
  if(length(el1)!=length(el2)){
    print("The dimensions are not equal")
  }
  dime= length(el1)
  s=0
  for (i in seq(length(el1))) {
    s=s+(el1[i]-el2[i])^2
  }
  dist= sqrt(s)
  return(dist)
}

# "a" intracluster distances, using the distance matrix
#Input: labels, distance matrix
#Output: all a values
find_all_a= function(labels, d){
  n_centroids= length(unique(labels))
  a= replicate(nrow(data), 0)
  for (c in seq((n_centroids))) {
    idx= inde_cluster(labels, c)
    for (p in idx) {
      sum=0
      for (q in idx) {
        sum=sum+d[p,q]
      }
      a[p]=sum/(length(idx)-1)
    }
  }
  return(a)
}

#####distance between a point and all points in a cluster
#input point's:point index, cluster index, distance matrix, labels
#output: distance "a" point and a cluster
pt_dist= function(pt,oth, d,labels){
  idx=inde_cluster(labels, oth)
  sum=0
  result=numeric()
  for (i in (idx)) {
    sum=sum+d[pt, i]
  }
  result=sum/length(idx)
  return(result)
}

find_all_b=function(labels, d){
  b=replicate(length(labels), 0)
  n_centroids=length(unique(labels))
  for (c in seq((n_centroids))) {
    other_centroids= seq(n_centroids)[-c]
    idx=inde_cluster(labels, c)
    for (p in idx) {
      t=replicate((n_centroids)-1, 0)
      for (c_o in seq(length(other_centroids))) {
        t[c_o]=pt_dist(p, other_centroids[c_o], d, labels)
      }
      b[p]=min(t)
    }
  }
  return(b)
}


#Calculate silhouette values
#input, a intracluster, b intercluster, labels

find_all_s=function(a, b, labels){
  if(length(a)!=length(b)){
    print("The dimensions are not equal")
  }
  s=replicate(length(a), 0)
    for (p in seq(length(a))) {
        s[p]=(b[p]-a[p])/max(a[p],b[p])
  }
  return(s)
}


#SILHOUETTE SCORE
coef_sil=function(X, labels){
  d0= (dist(X, diag= TRUE,upper= TRUE, method = "euclidean"))
  d= as.matrix(d0)
  a=find_all_a(labels, d)
  b= find_all_b(labels, d)
  s=find_all_s(a, b, labels)
  SC=mean(s)
  return(SC)
}

#NEW TEST, #####ONLY ONCE, problem here
cluster=3
library(synthesis)
blobs=data.gen.blobs(
  nobs = 1000,
  features = 2,
  centers = cluster,
  sd = 1,
  bbox = c(-10, 10),
  do.plot = TRUE)


datat= blobs$x
labelst= as.vector(unlist(kmeans(datat, cluster)[1]))

pt=2
oth=2
d0t= (dist(datat, diag= TRUE,upper= TRUE, method = "euclidean"))
dt= as.matrix(d0t) #####distance matrix between all points
a_all= find_all_a(labelst, dt)
b_all= find_all_b(labelst, dt)
sil=find_all_s(a_all, b_all, labelst)
sil
score_sil=coef_sil(datat, labelst)
score_sil

####PLOT

names(sil) = labelst
color=names(sil)
#sf=sort(sil)
sil_s=c()

for (i in seq(1:cluster)) {
  si=sil[labelst==i]
  fs=sort(si)
  sil_s=c(sil_s, fs)
}
sil_s

barplot(sil_s, horiz = TRUE, xlim =c(-1, 1), 
        col= as.integer(names(sil_s))+1, width = 1, border = NA)




