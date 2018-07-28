# Driver tree for MTS

library(collapsibleTree)

Metal<-data.frame(
  Upper=c(
    NA,"Metal","Metal","Con Tonnage","Con Tonnage","Con Tonnage","Mill Feed Tonnage","Mill Feed Tonnage",
    "Mill Feed Tonnage","Mill Feed Tonnage","Mill Feed Tonnage","Crusher Feed Tonnage","Crusher Feed Tonnage","Crusher Feed Tonnage",
    "Crusher Operating Hours","Crusher Operating Hours","Crusher Operating Hours","Head Grade","Head Grade","Mining Location",
    "Mining Location","Recovery","Con Grade","Con Grade","Con Grade","Mill Operating Hours","Mill Operating Hours","Mill Operating Hours",
    "Mill Throughput Rate","Mill Throughput Rate","Crusher Throughput Rate","Crusher Throughput Rate","Ore Type","Ore Type"
  ),
  Lower=c("Metal","Con Tonnage","Con Grade","Mill Feed Tonnage","Head Grade","Recovery","Crusher Feed Tonnage","COS Inventory Change",
          "Mill Operating Hours","Calendar Time","Mill Throughput Rate","Crusher Operating Hours","Crusher Throughput Rate","Ore Type",
          "Crusher PA","Crusher UoA", "Crusher OE","Block Model Variance","Mining Location","Behind of Plan","Ahead of Plan","Oxidization",
          "ConCu Limit", "ConAs Limit","ConF Limit","Mill PA","Mill UoA","Mill OE","Ore Hardness","Volumetric Tput Limit",
          "Ore Fragmentation","Ore Hardness","Central","South West"
  )
)

# Add in colors and sizes
Metal$Color <- Metal$Lower
levels(Metal$Color) <- colorspace::rainbow_hcl(34)

collapsibleTreeNetwork(
  Metal,
  fill = "Color",
  nodeSize = "leafCount"
)
