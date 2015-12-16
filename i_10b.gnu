gnuplot <<_end
set terminal wxt size 1000,1000
set pm3d
unset surface  # don't need surface grid
set view map # Do contours rather than an elevation plot
set contour
set key outside
# set cntrparam cubicspline  # smooth out the lines
set cntrparam levels 20    # sets the num of contour lines
# set pm3d interpolate 20,20 # interpolate the color
 
 # Set a nice color palette
 # set palette model RGB defined

# Set margins so that plots show up at the same position 
set lmargin at screen 0.00
set rmargin at screen 1.00
set bmargin at screen 0.15
set tmargin at screen 0.95
# Set square
set size square

set xlabel 'x'
set ylabel 'y'
set format x '%.0f'
set format y '%.0f'
set format z '%.2f'

splot 'He_10_M1.proj' u 1:2:3 notitle w l lt 1, 'He_10_M2.proj' u 1:2:3 notitle w l lt 2 lw 2, 'He_10_M3.proj' u 1:2:3 notitle w l lt 3 lw 3
set size 1.,1.; set term post eps enhanced color solid "Times-Roman" 24 ; set output 'He_excit_tau=10.eps' ; replot
pause 18
_end
