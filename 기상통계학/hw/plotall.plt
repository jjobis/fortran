set title "linear regression analysis (Global Land and Ocean Temperature Anomalies)"
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
plot m using 1:2 pt 1 ps 1 lt 1 lw 1,-0.3739+0.00062258*x lc 2 lw 2, -0.3387+0.00058372*x lc 4 lw 2,\
-0.6548+0.00090850*x lc 6 lw 2,-1.1242+0.00136069*x lc 8 lw 2, -0.7480+0.00101930*x lc 7 lw 2,\
-0.9849 + 0.00122977*x lc 5 lw 2