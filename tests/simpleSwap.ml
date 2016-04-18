tau;
tau;
tau;
spn (B101);
tau;
spn (B118);
tau;
spn (B134)

Cswap1' ~ ? int ! int end,
Cswap2' ~ ? int ! int end,
Cswap3 ~ ! int ? int end,
tau < B41,
tau < B57,
tau < B85,
psh ($l1$, ? int ! int end) < B108,
psh ($l2$, ? int ! int end) < B110,
psh ($l3$, ! int ? int end) < B128,
psh ($l3$, ! int ? int end) < B144,
R116 ! int < B112,
R117 ! int < B113,
R131 ! int < B129,
R147 ! int < B145,
R114 ? int < B109,
R115 ? int < B111,
R132 ? int < B130,
R148 ? int < B146,
tau;
tau;
B123;
tau < B118,
tau;
tau;
B128;
tau;
tau;
tau;
B129;
tau;
tau;
B130 < B123,
tau;
tau;
B139;
tau < B134,
tau;
tau;
B144;
tau;
tau;
tau;
B145;
tau;
tau;
B146 < B139,
rec B101(tau;
tau;
B108;
tau;
tau;
B109;
tau;
tau;
B110;
tau;
tau;
B111;
tau;
tau;
tau;
B112;
tau;
tau;
tau;
B113;
tau;
tau;
B101) < B101,
$l1$ ~ R114,
$l1$ ~ R117,
$l2$ ~ R115,
$l2$ ~ R116,
$l3$ ~ R131,
$l3$ ~ R132,
$l3$ ~ R147,
$l3$ ~ R148