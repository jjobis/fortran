set title "linear regression analysis (Global ocean Temperature Anomalies)"
set nokey
set grid
set ylabel "anomaly"
set xlabel "year/month count"
m="data_all.txt"
set style line 11 lc rgb "#808080" lt 1
set border 3 back ls 11
set tics nomirror
set style line 12 lc rgb "#808080" lt 0 lw 1
set grid back ls 12
set term png size 1000,600
set output "output_all.jpg"
set xrange [0:1200]
plot m using 1:2 pt 1 ps 1 lt 1 lw 1,[0:1200]-0.3423+0.00057004*x lc 2 lw 2,[348:1200] -0.2829+0.00050293*x lc 4 lw 2,\
[558:1200]-0.5110+0.00073586*x lc 6 lw 2,[828:1200]-0.8635+0.00107574*x lc 8 lw 2, [948:1200]-0.4313+0.00068261*x lc 7 lw 2,\
[1068:1200]-0.5683 + 0.00080398*x lc 11 lw 2