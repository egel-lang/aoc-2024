# Advent of Code 2024 - Egel Solutions

Marco Devillers

## Day 1 

```
    # Advent of Code (AoC) - day 1, task 2

    import "prelude.eg"
    using System, OS, List

    def input =
        let L = read_line stdin in if eof stdin then {} else {L | input}

    def parse = 
        do Regex::matches (Regex::compile "[0-9]+") |> map to_int

    def tally =
        do map Dict::count |> reduce Dict::inner_join |> Dict::to_list

    def main =
        input |> map parse |> transpose |> tally |> map [(X,(Y,Z)) -> X*Y*Z] |> sum
```

## Day 2 

```
    # Advent of Code (AoC) - day 2, task 2

    import "prelude.eg"

    using System, OS, List

    def input =
        let L = read_line stdin in if eof stdin then {} else {L | input}

    def parse = 
        do Regex::matches (Regex::compile "[0-9]+")  |> map to_int

    def safe =
        [ XX ->
        [ XX -> or (all (flip elem {-1,-2,-3}) XX) (all (flip elem {1,2,3}) XX) ] 
                (zip_with (-) XX (tail XX)) ]

    def dampened =
        [ XX -> zip_with (++) (inits XX) (tail (tails XX)) ]

    def main =
        input |> map parse |> map dampened |> map (any safe) 
              |> filter id |> length
```

## Day 3 

```
    # Advent of Code (AoC) - day 3, task 2

    import "prelude.eg"

    using System, OS, List

    def input =
        let L = read_line stdin in if eof stdin then {} else {L | input}

    def parse = 
        do Regex::matches (Regex::compile "mul\\([0-9]+,[0-9]+\\)|do(n't)?\\(\\)")

    def args =
        do Regex::matches (Regex::compile "[0-9]+") |> map to_int

    def calc =
        [ _ {}             -> 0
        | _ {"don't()"|XX} -> calc false XX
        | _ {"do()"|XX}    -> calc true XX
        | true {X|XX}      -> product (args X) + calc true XX 
        | false {X|XX}     -> calc false XX ]

    def main =
        input |> foldl (+) "" |> parse |> calc true
```

## Day 4 

```
    # Advent of Code (AoC) - day 4, task 2

    import "prelude.eg"

    using System, OS, List, String (to_chars, from_chars)

    val star = {(-1, -1), (0,0), (1, 1), (1, -1), (0,0), (-1,1)}

    def words = 
        [D -> map (flip map star . add) (Dict::keys D) |> map (map (Dict::get_with_default '.' D))]

    def main =
        read_lines stdin |> map to_chars |> Dict::from_lists |> words |> map from_chars
        |> filter (flip elem {"MASMAS", "MASSAM", "SAMMAS", "SAMSAM"}) |> length

```

## Day 5 

```
    # Advent of Code (AoC) - day 5, task 2

    import "prelude.eg"

    using System, OS, List

    def parse = 
        do Regex::matches (Regex::compile "[0-9]+") |> map to_int

    def order =
        foldl [P {X,Y} -> [A B -> (not (and (X == B) (Y == A))) && [_ -> P A B]]] [_ _ -> true]

    def main =
        read_lines stdin |> map parse |> span (do length |> ((==) 2)) |> proj_update 1 tail
        |> [(PP,XX) -> filter (join ((/=) . sort_by (order PP))) XX |> map (sort_by (order PP))]
        |> map (join (nth . flip (/) 2 . length)) |> sum
```

## Day 6 

```
    # Advent of Code (AoC) - day 6, task 2

    import "prelude.eg"

    using System, OS, List, String (to_chars, from_chars)

    def dirs = {(-1, 0), (0, 1), (1, 0), (0,-1)}

    def start_pos = 
        [D -> foldl [P0 P1 -> if Dict::get D P1 == '^' then P1 else P0] (0,0) (Dict::keys D)]

    def track =
        [D -> trace_while 
            [(P,N) -> Dict::has D P] 
            [(P,N) -> [Q -> if Dict::has D Q && [_ -> Dict::get D Q == '#'] then (P,(N+1)%4) else (Q,N)] (add (nth N dirs) P)]
            (start_pos D,0) ]

    def loops =
        [D -> let V = Dict::dict in iter_while
            [(P,N) -> and (not (Dict::has V (P,N))) (Dict::has D P)] 
            [(P,N) -> Dict::set V (P,N) 0; [Q -> if Dict::has D Q && [_ -> Dict::get D Q == '#'] then (P,(N+1)%4) else (Q,N)] (add (nth N dirs) P)]
            (start_pos D,0) |> Dict::has V]

    def solve =
        [D -> foldl [N P -> if loops (Dict::set (Dict::copy D) P '#') then printf "{} {}\n" N P; N+1 else N] 0 
              (map fst (track D) |> tail |> unique)]

    def main =
        read_lines stdin |> map to_chars |> Dict::from_lists |> solve

```

## Day 7 

```
    # Advent of Code (AoC) - day 7, task 2

    import "prelude.eg"

    using System, OS, List

    def parse = 
        do Regex::matches (Regex::compile "[0-9]+") |> map to_int

    def conc =
        [X Y -> to_int (to_text X + to_text Y)]

    def solutions =
        foldl [{} X -> {X} |XX X -> map ((*) X) XX ++ map ((+) X) XX ++ map (flip conc X) XX] {}

    def main =
        read_lines stdin |> map parse |> map [XX -> (head XX, solutions (tail XX))] 
        |> filter [(X,XX) -> (filter ((==) X) XX) /= {}] |> map fst |> sum
```

## Day 8 

```
    # Advent of Code (AoC) - day 8, task 2

    import "prelude.eg"

    using System, OS, List, String (to_chars, from_chars)

    def antennas =
        do Dict::to_list |> filter [(_,'.') -> false | _ -> true]

    def combs =
        do fix [F {} -> {} |F {X|XX} -> map [Y -> (X,Y)] XX ++ F XX]
           |> filter [((P0,A0),(P1,A1)) -> (A0 == A1)]

    def cast =
        [D P V -> if Dict::has D P then {P|cast D (add P V) V} else {}]

    def antinodes =
        [ D -> do combs
           |> foldl [AA ((P0, A0),(P1,A1)) -> cast D P0 (sub P0 P1) ++ cast D P0 (sub P1 P0) ++ AA] {}
           |> unique ]

    def main =
        read_lines stdin |> map to_chars |> Dict::from_lists 
        |> [ D -> antennas D |> antinodes D ]
        |> length
```

## Day 9 

```
    # Advent of Code (AoC) - day 9, task 2

    import "prelude.eg"

    using System, OS, List, String (to_chars, from_chars)

    def to_fs =
        do foldl_state [(0, N) XX X -> ((1, N+1), {(X, N)|XX})
                       |(1, N) XX X -> ((0, N), {(X, none)|XX})] (0,0) {}
           |> reverse

    def wipe =
        [I {(J,Y)|XX} -> if I == Y then {(J,none)|XX} else {(J,Y)|wipe I XX}]

    def place = 
        [(I,X) {(J,none)|XX} -> if I <= J then {(I,X),(J-I,none)|wipe X XX} else {(J,none)|place (I,X) XX}
        |(I,X) {(J,Y)|XX} -> if X == Y then {(J,Y)|XX} else {(J,Y)|place (I,X) XX}]

    def compact =
        [XX -> foldl [XX (_,none) -> XX|XX F -> place F XX] XX (reverse XX)]

    def main =
        read_line stdin |> to_chars |> map to_int
        |> to_fs |> compact 
        |> foldl_state [N M (L,none) -> (N+L,M)|N M (L,F) -> (N+L, M + F*((N+L)*(N+L- 1)/2-(N*(N- 1))/2))] 0 0

```

## Day 10 

```
    # Advent of Code (AoC) - day 10, task 2

    import "prelude.eg"

    using System, OS, List, String (to_chars, from_chars)

    def dirs = {(-1,0),(1,0),(0,-1),(0,1)}

    def start = do Dict::to_list |> filter (do snd |> ((==)'0')) |> map fst

    def trails =
        [D -> flatmap [PP -> 
            if Dict::get D (head PP) == '9' then {PP}
            else map (add (head PP)) dirs |> map (flip cons PP)
                |> filter [{P,Q|PP} -> Dict::get_with_default '0' D P == succ (Dict::get D Q)]
                |> trails D]]

    def main =
        read_lines stdin |> map to_chars |> Dict::from_lists
        |> [D -> start D |> map (flip cons nil) |> trails D ]
        |> length

```

## Day 11 

```
    # Advent of Code (AoC) - day 10, task 2

    import "prelude.eg"

    using System, OS, List, String (to_chars, from_chars)

    def dirs = {(-1,0),(1,0),(0,-1),(0,1)}

    def start = do Dict::to_list |> filter (do snd |> ((==)'0')) |> map fst

    def trails =
        [D -> flatmap [PP -> 
            if Dict::get D (head PP) == '9' then {PP}
            else map (add (head PP)) dirs |> map (flip cons PP)
                |> filter [{P,Q|PP} -> Dict::get_with_default '0' D P == succ (Dict::get D Q)]
                |> trails D]]

    def main =
        read_lines stdin |> map to_chars |> Dict::from_lists
        |> [D -> start D |> map (flip cons nil) |> trails D ]
        |> length

```

## Day 12 

```
    # Advent of Code (AoC) - day 12, task 2

    import "prelude.eg"

    using System, OS, List, String (to_chars), D = Dict

    def dirs = {(-1,0),(1,0),(0,-1),(0,1)}

    def regions0 =
        [D C PP {} RR -> (PP, RR)
        |D C PP QQ RR -> 
            let QQ = flatmap [P -> map (add P) QQ] dirs |> unique |> filter (D::has D) in
            let (PP0, RR) = split [P -> (D::get D P == C) && [_ -> elem P QQ]] RR in
                regions0 D C (PP0++PP) PP0 RR ]

    def regions =
        [D {} -> {}
        |D {P|PP} -> [(PP,QQ) -> {PP|regions D QQ}] (regions0 D (D::get D P) {P} {P} PP)]

    def perimeter =
        [D PP -> filter (flip not_elem PP) (flatmap [P -> map (add P) dirs] PP)]

    def sides0 =
        [PP -> map (flip tuple 0) PP |> D::from_list |> [D -> regions D PP]]

    def sides =
        [PP -> flatmap [P -> map (add P) PP |> filter (flip not_elem PP) |> sides0] dirs]

    def main =
        read_lines stdin |> map to_chars |> D::from_lists
        |> [D -> regions D (D::keys D) |> map [PP -> (PP, sides PP)]]
        |> map [(PP0,PP1) -> (length PP0) * (length PP1)] |> sum
```

## Day 13 

```
    # Advent of Code (AoC) - day 13, task 2

    import "prelude.eg"

    using System, OS, List, String (to_chars, from_chars)

    def parse = do Regex::matches (Regex::compile "[0-9]+") |> map to_int |> list_to_tuple

    def solve =
        [O {(AX,AY), (BX,BY), (PX,PY)} ->
            let (PX,PY) = add O (PX,PY) in
            let M = ((PX * BY) - (PY * BX)) / ((AX * BY) - (AY * BX)) in
            let N = (PY - AY * M) / BY in
            if (PX,PY) == add (mul M (AX,AY)) (mul N (BX,BY)) then (M,N) else none]

    def main =
        read_lines stdin |> map parse |> split_on tuple 
        |> map (solve (10000000000000, 10000000000000))
        |> filter ((/=) none) |> map [(M,N) -> 3 * M + N] |> sum

```

## Day 14 

```
    # Advent of Code (AoC) - day 14, task 2

    import "prelude.eg"

    using System, OS, List, String (to_chars, from_chars), D = Dict

    def parse = do Regex::matches (Regex::compile "-?[0-9]+") |> map to_int
                |> chunks 2 |> map list_to_tuple |> list_to_tuple

    def size = (101,103)

    def walk = [N (P,V) -> add P (mul N V)]

    def mod = [N M -> ((N%M)+M)%M]

    def clip = [(BX, BY) (PX,PY) -> (mod PX BX, mod PY BY)]

    def display =
        [(PX,PY) N PP ->
            let PP = map (clip size . walk N) PP in
            let D = D::from_list (map (flip tuple 1) PP) in
            map [X -> map [Y -> print (if D::has D (X,Y) then "x" else ".")] (from_to 0 PY); print "\n"] (from_to 0 PX);
            printf "seconds passed: {}\n" N]

    def main =
        read_lines stdin |> map parse |> [PP -> map [N -> display size N PP] (from_to 0 (uncurry (*) size))];
        none

```

## Day 15 

```
    # Advent of Code (AoC) - day 15, task 2

    import "prelude.eg"

    using System, OS, List, String (to_chars), D = Dict

    def parse = do map to_chars |> split_on {} |> [{XX,YY} -> (XX, reduce (++) YY)]

    def dir = ['^' -> (-1,0) |'v' -> (1,0)|'<' -> (0,-1)|'>' -> (0,1)]

    def expand = flatmap ['@' -> {'@','.'}|'O' -> {'[',']'}|'.' -> {'.','.'}|'#'->{'#','#'}]

    def start = do D::to_list |> filter ((==) '@' . snd) |> head |> fst

    def cat = [none F -> none | XX F -> [none -> none| YY -> XX++YY] (F none)]

    def region = [D P V -> let Q = add P V in
        [(_,0) '[' -> cat {P, add (0,1) P} [_ -> cat (region D Q V) [_ -> region D (add (0,1) Q) V]]
        |(_,0) ']' -> cat {P, add (0,-1) P} [_ -> cat (region D Q V) [_ -> region D (add (0,-1) Q) V]]
        |(0,_) '[' -> cat {P, Q} [_ -> region D (add (0,1) Q) V]
        |(0,_) ']' -> cat {P, Q} [_ -> region D (add (0,-1) Q) V]
        |_     '@' -> cat {P} [_ -> region D (add P V) V]
        |_     '#' -> none 
        |_     _   -> {}] V (D::get D P)]

    def shove = 
        [D PP V -> foldl [D (P,X) -> D::set D P X] D
                (map [P -> (P,'.')] PP ++ map [P -> (add P V, D::get D P)] PP)]

    def step = [D P V -> [none -> (D,P)|PP -> (shove D PP V, add P V)] (region D P V)]

    def main =
        read_lines stdin |> parse |> [(XX,VV) ->(D::from_lists (map expand XX), map dir VV)]
        |> [(D,VV) -> foldl [(D,P) V -> print P "\n"; step D P V] (D, start D) VV] |> fst
        |> D::to_list |> foldl [N ((X,Y),'[') -> N + 100 * X + Y |N _ -> N] 0

```

## Day 16 

```
    # Advent of Code (AoC) - day 16, task 2

    import "prelude.eg"

    using System, OS, List, String (to_chars, from_chars), D = Dict

    def pos = [C -> do D::to_list |> filter ((==) C . snd) |> head |> fst]

    def dirs = {(0,1),(1,0),(0,-1),(-1,0)}

    def rotate = [(0,Y) -> {(Y,0),(-Y,0)} | (X,0) -> {(0,X),(0,-X)}]

    def insort = [P {} -> {P}|P {Q|QQ} -> if proj 0 P <= proj 0 Q then {P,Q|QQ} else {Q|insort P QQ}]

    def dijkstra0 = 
        [ G {} (D0,D1) -> (D0,D1)
        | G {(N,P)|QQ} (D0,D1) ->
            printf "N P {} {}\n" N P;
            let ADJ = Dict::get G P in
            let (D0,D1,QQ) = foldl [(D0,D1,QQ) (M,Q) ->
                            let ALT = N + M in
                            if ALT < D::get_with_default max_int D0 Q then 
                            (D::set D0 Q ALT, D::set D1 Q {P}, insort (ALT,Q) QQ)
                            else if ALT == D::get D0 Q then 
                            (D::set D0 Q ALT, D::set D1 Q (unique {P|D::get D1 Q}), QQ)
                            else (D0,D1,QQ)] (D0,D1,QQ) ADJ
             in dijkstra0 G QQ (D0,D1)]

    def dijkstra = [G P -> dijkstra0 G {(0,P)} (D::set D::dict P 0, D::set D::dict P {})]

    def adj =
        [D (P,V) -> {(1,(add P V,V))} ++ map [V -> (1001, (add P V,V))] (rotate V)
                |> filter ((/=) '#' . D::get D . fst . snd)]

    def to_graph =
        [D -> foldl [G (P,'#') -> G 
                    |G (P,_) -> foldl [G (P,V) -> D::set G (P,V) (adj D (P,V))] G 
                                      (map (tuple P) dirs)] D::dict (D::to_list D)] 

    def nodes = 
        [D PP {} -> PP
        |D PP {Q|QQ} -> nodes D {Q|PP} (D::get_with_default {} D Q ++ QQ)]

    def main =
        read_lines stdin |> map to_chars |> D::from_lists 
        |> [D -> let S = pos 'S' D in let E = pos 'E' D in
            to_graph D |> [G -> dijkstra G (S,(0,1))]
            |> [(D0,D1) -> 
                   map [P -> (Dict::get_with_default max_int D0 P,P)] (map (tuple E) dirs)
                   |> [PP -> filter ((==) (minimum (map fst PP)) . fst) PP |> map snd]
                   |> nodes D1 {} ]
            |> map fst |> unique |> length]

```

## Day 17 

```
    # Advent of Code (AoC) - day 17, task 2

    import "prelude.eg"

    using System, OS, List

    def parse = do foldl (+) "" |> (do Regex::matches (Regex::compile "-?[0-9]+") |> map to_int) 
                |> [{A,B,C|P} -> ((A,B,C,0),P)]

    def op = [N RR -> if N < 4 then N else proj (N - 4) RR]

    def ins =
        [0 N (A,B,C,P) -> (A/(Math::pow_int 2 (op N (A,B,C,P))),B,C,P+2) 
        |1 N (A,B,C,P) -> (A,N^B,C,P+2)
        |2 N (A,B,C,P) -> (A,(op N (A,B,C,P))%8,C,P+2)
        |3 N (0,B,C,P) -> (0,B,C,P+2)
        |3 N (A,B,C,P) -> (A,B,C,N)
        |4 N (A,B,C,P) -> (A,B^C,C,P+2)
        |5 N (A,B,C,P) -> ((A,B,C,P+2),(op N (A,B,C,P))%8) 
        |6 N (A,B,C,P) -> (A,A/(Math::pow_int 2 (op N (A,B,C,P))),C,P+2)
        |7 N (A,B,C,P) -> (A,B,A/(Math::pow_int 2 (op N (A,B,C,P))),P+2) ]

    def fetch = [_ {} -> none|0 {X|_} -> X|N {_|XX} -> fetch (N - 1) XX]

    def step = [((A,B,C,P),PP,OO) -> 
            [(none,_) -> none |(_,none) -> none
            |(OP,N) -> [(RR,O) -> (RR,PP,{O|OO})|RR -> (RR,PP,OO)] (ins OP N (A,B,C,P))]
            (fetch P PP, fetch (P+1) PP)]

    def run = [(RR,PP) -> while step (RR,PP,{}) |> proj 2 |> reverse]

    def iter_with = [0 F X -> X|N F X -> iter_with (N - 1) F (F N X)]

    def find = [PP -> iter_with (length PP) [L NN -> flatmap
                [(N,I) -> if run ((8*N+I,0,0,0),PP) == (drop (L - 1) PP) then {8*N+I} else {}] 
                (flatmap [N -> map (tuple N) (from_to 0 7)] NN) ] {0}] 

    def main = read_lines stdin |> parse |> [(RR,PP) -> find PP] |> minimum
```

## Day 18 

```
    # Advent of Code (AoC) - day 18, task 2

    import "prelude.eg"

    using System, OS, List, String (to_chars, from_chars), D = Dict

    def dirs = {(0,1),(1,0),(0,-1),(-1,0)}

    def insort = [P {} -> {P} |P {Q|QQ} -> if proj 0 P <= proj 0 Q then {P,Q|QQ} else {Q|insort P QQ}]

    def dijkstra0 = 
        [ G {} D -> D
        | G {(N,P)|QQ} D ->
            let ADJ = Dict::get G P in
            let (D,QQ) = foldl [(D,QQ) (M,Q) ->
                            let ALT = N + M in
                            if ALT < D::get_with_default max_int D Q then 
                            (D::set D Q ALT, insort (ALT,Q) QQ)
                            else (D,QQ)] (D,QQ) ADJ in
                dijkstra0 G QQ D ]

    def dijkstra = [G P -> dijkstra0 G {(0,P)} (D::set D::dict P 0)]

    def board =
        [(X,Y) PP ->
            let F = [C -> foldl [D P -> D::set D P C]] in
            F '#' (F '.' D::dict (flatmap [X -> map (tuple X) (from_to 0 Y)] (from_to 0 X))) PP]

    def adj =
        [D P -> map (add P) dirs |> filter [P -> D::has D P && [_ -> D::get D P /= '#']]]

    def to_graph =
        [D -> foldl [G (P,'#') -> G |G (P,_) -> D::set G P (map (tuple 1) (adj D P))] D::dict (D::to_list D)]

    def find =
        [B S E -> do take B |> board E |> to_graph |> [G -> dijkstra G S] |> D::to_list |> filter (((==) E) . fst)]

    def bin0 =
        [L R J P PP -> if L > R then J else let M = (L+R)/2 in 
            if P (nth M PP) then bin0 (M+1) R M P PP else bin0 L (M - 1) J P PP]

    def bin = [P PP -> bin0 0 (length PP - 1) none P PP]

    def main =
        let S = (0,0) in let E = (70,70) in
        read_lines stdin |> map (list_to_tuple . map to_int . Regex::matches (Regex::compile "[0-9]+"))
        |> [PP -> bin [N -> find N S E PP /= {}] (from_to 0 (length PP - 1)) |> flip nth PP]
```

## Day 19 

```
    # Advent of Code (AoC) - day 19, task 2

    import "prelude.eg"

    using System, OS, List, String (starts_with, remove, count, split_pattern), D = Dict

    def match =
        [X Y -> if starts_with X Y then remove 0 (count X) Y else none]

    def solve =
        [XX _  D "" -> 1
        |XX {} D Z -> 0
        |XX {Y|YY} D Z -> [none -> solve XX YY D Z |Z0 -> (D::memo D (solve XX XX) Z0) + (solve XX YY D Z)] (match Y Z)]

    def main =
        read_lines stdin |> split_on "" |> [{{XX},YY} -> (split_pattern ", " XX, YY)]
        |> [(XX, YY) -> map [Y -> solve XX XX D::dict Y] YY |> sum]

```

## References

[1] Devillers, Marco. The Egel Language. Github, [Egel Language](https://egel-lang.github.io/),
    2024.

Copyright 2024 Marco Devillers, MIT licence
