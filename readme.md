```
--łaczenie par bez powrótrzeń
pary :: [a] -> [(a, a)]
pary xs = [(x, y) | (x:ys) <- tails xs, y <- ys]
  where tails [] = []
        tails l@(_:xs) = l : tails xs
main :: IO ()
main = print (pary [1, 2, 3, 4])

```

```
% --- Definicja grafu ---
% Lista wszystkich wierzchołków
nodes([a,b,c,d]).

% Lista krawędzi (graf nieskierowany)
edge(a,b).
edge(a,c).
edge(b,c).
edge(c,d).

% Sąsiedztwo jako relacja symetryczna
neighbor(X,Y) :-
    edge(X,Y) ;
    edge(Y,X).

% --- Główny predykat kolorujący ---
% coloring(+Nodes, +Colors, -Coloring)
%   Nodes     – lista wierzchołków do pokolorowania
%   Colors    – lista dostępnych kolorów (termów, np. red, green…)
%   Coloring  – wynikowa lista par Wierzchołek–Kolor, np. [a-red,b-green,…]
coloring(Nodes, Colors, Coloring) :-
    color_nodes(Nodes, Colors, [], Coloring).

% color_nodes(+ToDo, +Colors, +Acc, -Coloring)
%   ToDo      – wierzchołki jeszcze do pokolorowania
%   Colors    – dostępne kolory
%   Acc       – dotychczasowe przypisania [W-K|…]
%   Coloring  – finalne przypisania
color_nodes([], _Colors, Acc, Acc).
color_nodes([V|Vs], Colors, Acc, Coloring) :-
    member(K, Colors),
    \+ conflict(V-K, Acc),
    color_nodes(Vs, Colors, [V-K|Acc], Coloring).

% conflict(+V-Kolor, +Acc)
%   prawdziwe, jeśli istnieje w Acc wierzchołek U o tym samym kolorze
%   i jednocześnie (U,V) sąsiedzi
conflict(V-K, Acc) :-
    member(U-K, Acc),
    neighbor(U, V).

% --- Przykładowe wywołanie ---
% ?- nodes(N), coloring(N, [red,green,blue], C).
% C = [d-red, c-green, b-red, a-blue] ;
% C = [d-red, c-green, b-red, a-green] ;
% … i tak dalej (wszystkie możliwe rozwiązania).

```
