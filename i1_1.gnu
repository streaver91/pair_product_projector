set parametric
set contour base
set view 0,0,1
unset surface
set cntrparam levels 50
splot 'He_1_M1.proj' u 1:2:3 w l
