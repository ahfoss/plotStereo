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

PARALLAX_ANGLE = 3
COL1 = rgb(1,0,0)
COL2 = rgb(0,0,1)

# plot black axes and box
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
points(
  trans3d(
    x,y,z
   ,pmat = pcol1
  )
 ,pch=pch
 ,col = COL1
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
points(
  trans3d(
    x,y,z
   ,pmat = pcol2
  )
 ,pch=pch
 ,col = COL2
)

par(new=FALSE)
  
}
