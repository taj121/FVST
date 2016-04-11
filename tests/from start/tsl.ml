let client = 
  (let p1 = req-tsl () in 
      (send (p1, 1); 
            send (p1, 200);
            send (p1, 1);
            send (p1, 1);
            send (p1, 2);
            let selVNum = recv p1 in
            let rand = recv p1 in
            let selSuit = recv p1 in
            let selComp = recv p1 in
            let sesId = recv p1 in
            recv p1;
            recv p1;
            recv p1;
            send (p1, 1))) in 

let server =
  (let myVNum = 2 in
    let p2 = acc-tsl () in
      let vNum = recv p2 in
      let rNum = recv p2 in
      let sugSuit = recv p2 in
      let sugComp = recv p2 in
      let sesId = recv p2 in 
      send (p2, vNum);
      send (p2, 12);
      send (p2, sugSuit);
      send (p2, sugComp);
      send (p2, 201);
      send (p2, 1);
      send (p2, 1);
      send (p2, 1);
      recv p2)

in (spawn client);
(spawn server)