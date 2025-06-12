# Haskell
Online IDE
https://play.haskell.org
## Komentarze
```
-- Komentarz jednolinijkowy

{-
    To jest
    komentarz
    wielolinijkowy
-}
```

## Funkcje
```
addMe :: Integer -> Integer -> Integer
addMe x y = x + y

main :: IO ()
main = do
    putStr "Sum of x + y = "
    print (addMe 10 25)
```

## Listy
### Listy to jednorodne kolekcje elementów.

```
xs = [1,2,3,4,5]
head xs     -- 1
tail xs     -- [2,3,4,5]
length xs   -- 5
sum xs      -- 15
```
#### Zakresy i leniwość
```
[1..10]           -- [1,2,3,...,10]
[1,3..9]          -- [1,3,5,7,9]
take 5 [1..]      -- [1,2,3,4,5] (lista nieskończona)
```
#### Łączenie list
```
[1,2] ++ [3,4]         -- [1,2,3,4]
'a' : "bc"             -- "abc"
```
#### Dostęp do elementów
```
[1,2,3] !! 0           -- 1
[1,2,3] !! 2           -- 3
```
#### Listy wyrozumowane (list comprehensions)
```
[ <wyrażenie> | <zmienna> <- <lista>, <warunek1>, <warunek2>, ... ]

[x * 2 | x <- [1..5]]
-- Wynik: [2,4,6,8,10]

[x | x <- [1..10], even x]
-- Wynik: [2,4,6,8,10]

[(x, y) | x <- [1,2], y <- [3,4]]
-- Wynik: [(1,3),(1,4),(2,3),(2,4)]

take 5 [x^2 | x <- [1..]]
-- Wynik: [1,4,9,16,25]
```

## Funkcje Wyższego Rzędu (Higher-Order Functions)
### Przyjmuje inną funkcję jako argument lub zwraca funkcję jako wynik.
`map` stosuje daną funkcję do każdego elementu listy.
```
map (*2) [1,2,3]
-- Wynik: [2,4,6]
```

## Krotki (Tuples)
### Tuples przechowują stałą liczbę elementów różnych typów.
```
mojaKrotka :: (Int, String, Bool)
mojaKrotka = (1, "Haskell", True)

-- Dostęp do elementów krotki
-- fst (1, "Hello") -- zwróci 1
-- snd (1, "Hello") -- zwróci "Hello"
```
#### Krotki jako elementy list
```
[(1, "a"), (2, "b"), (3, "c")]

map fst [(1, "a"), (2, "b")]   -- [1,2]
map snd [(1, "a"), (2, "b")]   -- ["a","b"]
```

## Wzorce (Pattern matching)
### Wzorce pozwalają na definiowanie funkcji w zależności od struktury danych wejściowych.
`x` – reprezentuje pierwszy element listy (głowę),
`xs` – reprezentuje pozostałe elementy listy (ogon).
```
-- Funkcja obliczająca sumę elementów listy
sumList :: [Int] -> Int
sumList [] = 0                -- przypadek pustej listy
sumList (x:xs) = x + sumList xs  -- x to pierwszy element, xs to reszta listy
```
`foldr` (reduce / right fold) "zwija" listę do pojedynczej wartości, stosując funkcję od prawej do lewej.
```
-- Przykład: suma elementów listy
sumaElementow :: [Int] -> Int
sumaElementow lista = foldr (+) 0 lista
--foldr (+) 0 [1,2,3] działa tak: 1 + (2 + (3 + 0))
-- Użycie:
-- sumaElementow [1,2,3] -- zwróci 6
```
`foldl` działa od lewej do prawej.

# Prolog
Online IDE
https://swish.swi-prolog.org
### Komentarze
```
% To jest komentarz
```
### Fakty - stwierdzenia o świecie.
```
kot(mruczek).
pies(reksio).
lubi(ala, koty).
```
### Reguły - warunkowe zależności
```
zwierze(X) :- kot(X).
zwierze(X) :- pies(X).
```
### Zapytania - pytania o fakty i reguły.
```
?- kot(mruczek).         % true
?- zwierze(reksio).      % true
?- lubi(ala, X).         % X = koty
```
### Zmienne i stałe
Stała - zaczyna się małą literą (ala, pies)
Zmienna - zaczyna się dużą literą (X, Kto)
```
przyjaciel(ala, basia).
przyjaciel(basia, kasia).

?- przyjaciel(ala, Kto).  % Kto = basia
```

### Rekurencje
Prolog nie ma pętli, używa rekurencji
```
dlugosc([], 0).
dlugosc([_|Ogon], Dlugosc) :-
    dlugosc(Ogon, D),
    Dlugosc is D + 1.
```
### Operacje na listach
```
czlonek(X, [X|_]).         % X jest na początku listy
czlonek(X, [_|Ogon]) :-    % lub w ogonie
    czlonek(X, Ogon).
```
```
?- czlonek(3, [1,2,3,4]).  % true
```
### Zagadki logiczne
```
rodzic(anna, jan).
rodzic(jan, tomek).

dziadek(X, Y) :- 
    rodzic(X, Z), 
    rodzic(Z, Y).
```
```
?- dziadek(anna, tomek).   % true
```
