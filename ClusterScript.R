
segment = Single %>% 
  select(Alcohol, Contributed.To.Accident, Work.Zone, 
         Fatal, Property.Damage, Personal.Injury, 
         Belts, Male = Gender)

segment$Male = gsub("M", "1", segment$Male)
segment$Male = gsub("F", "0", segment$Male)

dist = dist(segment, method = "binary")

cluster = hclust(dist, method = "complete")

sil_width = map_dbl(2:40, function(k){
  model = pam(segment, k = k)
  model$silinfo$avg.width
})

df = data.frame(
  k = 2:40,
  sil_width = sil_width
)

ggplot(df, aes(k, sil_width)) +
  geom_line() +
  scale_x_continuous(breaks = 2:40)

assign = cutree(cluster, k = ## whatever optimal k happens to be)
                  
segment = mutate(segment, cluster = assign)