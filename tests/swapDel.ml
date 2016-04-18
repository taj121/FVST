tau;
tau;
tau;
spn (B118);
tau;
spn (B132);
tau;
spn (B163)

Cswap1' ~ (+) [($SWAP$; ? int ! int end ), ($LEAD$; ! ? int ! int end end)],
Cswap2' ~ (+) [($SWAP$; ? int ! int end ), ($LEAD$; ! ? int ! int end end)],
Cswap3 ~ + [($SWAP$; ! int ? int end), ($LEAD$; ? ? int ! int end end)] [],
tau < B38,
tau < B68,
tau < B102,
psh ($l1$, (+) [($SWAP$; ? int ! int end ), ($LEAD$; ! ? int ! int end end )]) < B123,
psh ($l2$, (+) [($SWAP$; ? int ! int end ), ($LEAD$; ! ? int ! int end end )]) < B125,
psh ($l3$, + [($SWAP$; ! int ? int end), ($LEAD$; ? ? int ! int end end)] []) < B144,
psh ($l3$, + [($SWAP$; ! int ? int end), ($LEAD$; ? ? int ! int end end)] []) < B175,
R151 ! int < B145,
R155 ! int < B149,
R182 ! int < B176,
R186 ! int < B180,
R152 ? int < B146,
R154 ? int < B148,
R183 ? int < B177,
R185 ? int < B179,
R130 ! R131 < B127,
R153 ? $l4$ < B147,
R184 ? $l4$ < B178,
R128 ! $SWAP$ < B124,
R129 ! $LEAD$ < B126,
tau;
tau;
B137;
tau < B132,
tau;
tau;
B144;
tau;
R150? optn [($SWAP$; tau;
tau;
tau;
B145;
tau;
tau;
B146), ($LEAD$; tau;
tau;
B147;
tau;
tau;
B148;
tau;
tau;
tau;
B149;
tau)] < B137,
tau;
tau;
B168;
tau < B163,
tau;
tau;
B175;
tau;
R181? optn [($SWAP$; tau;
tau;
tau;
B176;
tau;
tau;
B177), ($LEAD$; tau;
tau;
B178;
tau;
tau;
B179;
tau;
tau;
tau;
B180;
tau)] < B168,
rec B118(tau;
tau;
B123;
tau;
B124;
tau;
tau;
B125;
tau;
B126;
tau;
tau;
tau;
B127;
tau;
tau;
B118) < B118,
$l1$ ~ R128,
$l1$ ~ R131,
$l2$ ~ R129,
$l2$ ~ R130,
$l3$ ~ R150,
$l3$ ~ R151,
$l3$ ~ R152,
$l3$ ~ R153,
$l3$ ~ R181,
$l3$ ~ R182,
$l3$ ~ R183,
$l3$ ~ R184,
$l4$ ~ R154,
$l4$ ~ R155,
$l4$ ~ R185,
$l4$ ~ R186