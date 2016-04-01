gnuplot <<_end
set pm3d
# set parametric
# set contour base
set contour
set key outside
#set view 0,0,1
set view map
unset surface
set cntrparam cubicspline  # smooth out the lines
set cntrparam levels 50
#set pm3d interpolate 20,20 # interpolate the color (takes long time)

set xlabel 'x'
set ylabel 'y'
set format x '%.0f'
set format y '%.0f'
set format z '%.2f'

splot 'He_100_M2.proj' u 1:2:3 notitle w l
pause 5
_end
