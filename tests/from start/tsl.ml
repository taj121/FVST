let client = fun z =>
  (let p1 = req-tsl () in
    ((send (p1, 1));
     (recv p1);
      (send (p1, 2))
     )) in

 let server = fun z =>
   ( let p2 = acc-tsl () in
    (recv p2);
     (send (p2, 3));
     (recv p2)
       )
   in (spawn client);
 (spawn server)