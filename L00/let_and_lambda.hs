letOverwrite x = let x = 2
              in
               let x = 3
               in
                let x = 4
                in
                 x

lamOverwrite x = (\x -> (\x -> (\x -> x))) 2 3 4


counter x = let x = x + 1
            in
             let x = x + 1
             in
              x

lamCounter x = (\x -> (\x -> x + 1) + 1) x
