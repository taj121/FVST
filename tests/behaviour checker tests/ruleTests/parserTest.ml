B123;
tau;
chc(tau,B12);
rec B13 (tau);
spn(B57);
psh($t$, end);
psh($t$, ! unit end);
psh($t$, ? bool end);
psh($t$, ? end end);
psh($t$, S12);
psh($t$, +[($test$; end),($test1$;S13)][($test$;S14),($test1$;S15)]);
R1 ! unit;
R2 ! bool;
R3 ? int;
R4 ? pair(unit;int);
R5 ! funct int -> bool -B4;
R6 ! R15;
R7 ? T1;
R8 ? $hi$;
R9 ! $bye$;
R666 ? optn [($1$;B12),($2$;B15),($3$;B13)]

unit <  int,
C1~end,
CN1~end
