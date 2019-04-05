# Function to convert an arc into line segments
arc2line <- function(center, r, vector, theta){
  require(alphahull)
  require(sp)
  angles <- anglesArc(vector, theta)
  seqang <- seq(angles[1], angles[2], length = 100)
  x <- center[1] + r * cos(seqang)
  y <- center[2] + r * sin(seqang)
  coords.xy <- cbind(x,y)
  line <- Line(coords = coords.xy)
  return(line)
}


# Function to convert alpha hull arcs into SpatialLines
ahull2lines <- function(hull){
  arclist <- hull$arcs
  lines <- list()
  for (i in 1:nrow(arclist)) {
    line_i <- arc2line(center = arclist[i, 1:2], r = arclist[i, 3], vector = arclist[i, 4:5], theta = arclist[i, 6])
    list_length <- length(lines)
    if(list_length > 0){
      last_line_coords <- lines[[list_length]]@coords
    }
    if(i == 1){
      lines[[i]] <- line_i
    } else if(all.equal(line_i@coords[1,], last_line_coords[nrow(last_line_coords),])){
      lines[[list_length]]@coords <- rbind(last_line_coords, line_i@coords[2:nrow(line_i@coords),])
    } else {
      lines[[length(lines) + 1]] <- line_i
    }
  }
  lines <- Lines(lines, ID = 'l')
  sp_lines <- SpatialLines(list(lines))
  return(sp_lines)
}

# Function to convert SpatialLines to SpatialPolygon
spLines2poly <- function(sp_lines){
  lines_slot <- sp_lines@lines[[1]]
  poly_bool <- sapply(lines_slot@Lines, function(x){
    coords <- lines_slot@Lines[[1]]@coords
    all.equal(coords[1,], coords[nrow(coords),])
  })
  poly_lines <- sp_lines[poly_bool]
  poly_lines_slot <- poly_lines@lines
  sp_polys <- SpatialPolygons(list(Polygons(lapply(poly_lines_slot, function(x) {
    Polygon(slot(slot(x, "Lines")[[1]], "coords")) 
    }), ID = "1")))
  return(sp_polys)
}

# Function to convert alpha hull into SpatialPolygon
ahull2poly <- function(hull){
  hull2SpatialLines <- ahull2lines(hull)
  SpatialLines2SpatialPolygon <- spLines2poly(hull2SpatialLines)
  return(SpatialLines2SpatialPolygon)
}