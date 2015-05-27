# theta is side-to side rotation
# phi is elevation
scatterStereo <- function(
  x
 ,y
 ,z
 ,xlim = range(x)
 ,ylim = range(y)
 ,zlim = range(z)
 ,pch=1
 ,main=''
 ,theta = 30
 ,phi = 0
) {

# Greater parallax angle => greater 3d effect
PARALLAX_ANGLE = 3.0
COL1 = rgb(1.0,0.0,0.0,0.5)
COL2 = rgb(0.0,1.0,1.0,0.5)

# plot black axes and box
# Currently not showing any bounding box
persp(
  x=0:1
 ,y=0:1
 ,z=matrix(NA,2,2)
 ,xlim = xlim
 ,ylim = ylim
 ,zlim = zlim
 ,xlab = 'X'
 ,ylab = 'Y'
 ,zlab = 'Z'
 ,theta = theta
 ,phi = phi
 ,box = FALSE
# ,ticktype = 'detailed'
)

par(new=TRUE)

# plot col1
pcol1 <- persp(
  x=0:1
 ,y=0:1
 ,z=matrix(NA,2,2)
 ,xlim = xlim
 ,ylim = ylim
 ,zlim = zlim
 ,xlab = ''
 ,ylab = ''
 ,zlab = ''
 ,theta = theta + PARALLAX_ANGLE
 ,phi = phi
 ,box = FALSE
)

par(new=TRUE)

# plot col2
pcol2 <- persp(
  x=0:1
 ,y=0:1
 ,z=matrix(NA,2,2)
 ,xlim = xlim
 ,ylim = ylim
 ,zlim = zlim
 ,xlab = ''
 ,ylab = ''
 ,zlab = ''
 ,theta = theta - PARALLAX_ANGLE
 ,phi = phi
 ,box = FALSE
)

# calculate divergences between persp #1 and #2
# a simple metric of how "close" a point is
# Scale size accordingly
# Scaling size doesn't correspond to apparent distance!
# I think there's some complex perceptual interaction occurring.
coord1 <- trans3d(
  x,y,z
 ,pmat = pcol1
)
coord2 <- trans3d(
  x,y,z
 ,pmat = pcol2
)
divergence <- sqrt(
  (coord1$x - coord2$x)^2
 +(coord1$y - coord2$y)^2
)
divergenceScaled <- (divergence - min(divergence)) / (max(divergence) - min(divergence)) / 1 + 1

# flip large/small
divergenceScaled <- 3.0 - divergenceScaled

print(summary(divergenceScaled))
print(range(divergenceScaled))

points(
  trans3d(
    x,y,z
   ,pmat = pcol1
  )
 ,pch=pch
 ,cex = 1 #divergenceScaled
 ,col = COL1
)

points(
  trans3d(
    x,y,z
   ,pmat = pcol2
  )
 ,pch=pch
 ,cex = 1 #divergenceScaled
 ,col = COL2
)

par(new=FALSE)
  
}
