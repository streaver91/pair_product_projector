gnuplot <<_end
set pm3d
# unset surface  # don't need surface grid
set view map # Do contours rather than an elevation plot
set contour
set key outside
set cntrparam cubicspline  # smooth out the lines
set cntrparam levels 50    # sets the num of contour lines
# set pm3d interpolate 20,20 # interpolate the color
 
# Set a nice color palette
set palette model RGB defined

set xlabel 'x'
set ylabel 'y'
set format x '%.0f'
set format y '%.0f'
set format z '%.2f'
 
splot 'He_100_M1.proj' using 1:2:3 notitle with lines lt 1

set size 1.,1.; set term post eps enhanced color solid "Times-Roman" 24 ; set output 'He_excit_proj2.eps' ; replot
pause 5
_end
