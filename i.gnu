set cntrparam levels 10
set contour both
splot [:][:][:] 'He_1_M1.proj' using 1:2:3 w l lt 1 notitle
