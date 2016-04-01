gnuplot <<_end
#set terminal x11 size 700,700
set terminal wxt size 1000, 1000
set termoption dash
#set pm3d
unset surface  # don't need surface grid
set view map # Do contours rather than an elevation plot
set contour
#set key outside
# set cntrparam cubicspline  # smooth out the lines
set cntrparam levels 5    # sets the num of contour lines
# set pm3d interpolate 20,20 # interpolate the color
 
 # Set a nice color palette
 # set palette model RGB defined

set style increment userstyles 
# Set margins so that plots show at the same position 
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
set palette color
#set style line 1 lt 1 lw 2
#set style line 2 lt 3 lw 2
#set style line 3 lt 5 lw 2
splot 'He_1_M1.proj' u 1:2:3 w l lt 1 lc 1 lw 1, 'He_1_M2.proj' u 1:2:3 w l lt 3 lc 2 lw 2, 'He_1_M3.proj' u 1:2:3 w l lt 5 lc 3 lw 3
#splot 'He_1_M1.proj' u 1:2:3 w l ls 1, 'He_1_M2.proj' u 1:2:3 w l ls 2, 'He_1_M3.proj' u 1:2:3 w l ls 3
#splot 'He_1_M1.proj' u 1:2:3 w l lw 1, 'He_1_M2.proj' u 1:2:3 w l lw 2, 'He_1_M3.proj' u 1:2:3 w l lw 3
set size 1.,1.;
set term post eps enhanced color solid "Times-Roman" 24 ; 
set output 'He_excit_tau=1.eps' ; 
replot
pause 68
_end
