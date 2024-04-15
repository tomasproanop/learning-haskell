{-8. Übungszettel
Funktionale Programmierung 
Winter 2022/2023
Dozentin: Katharina Klost
Tutor: Lennart Schütte
Arbeitsgruppe: Pascal Druwe und Tomás Proaño


1. Algebraische Datentypen (3+3+6+4 Punkte)

https://dev.to/louy2/generalizing-tree-traversal-in-haskell-with-typeclass-4egb

https://blog.moertel.com/posts/2012-01-26-the-inner-beauty-of-tree-traversals.html

https://www.google.com/search?q=Tree+Traversal+in+Haskell&oq=Tree+Traversal+in+Haskell&aqs=chrome..69i57j69i60.385j0j4&sourceid=chrome&ie=UTF-8 

Ein Goldschmied verfügt über Kettenglieder aus Gold, Silber und Platin und kann daraus Ketten
formen.

Für die Ketten gilt das Folgende:
i) Jede Kette hat einen Anfang (linke Seite) und ein Ende (rechte Seite)
ii) Eine leere Kette ist eine Kette
iii) Das Ende einer Kette ch1 kann durch ein Kettenglied l mit dem Anfang einer Kette ch verbunden
werden.

Ein passender algebraischer Datentyp kann wie folgt aussehen:

data Link = G | S | P deriving (Eq, Show)
data Chain = Empty | Join Chain Link Chain

(a) Implementieren Sie eine Funktion list::Chain -> [Link] zur Bestimmung der Materialfolge
von links nach rechts.

(b) Realisieren Sie Chain als Instanz der Klass Eq, ohne deriving zu verwenden. Hierbei sind zwei
Ketten gleich, wenn sie die gleiche Materialfolge haben.

(c) Sei eine Kostenfunktion p :: Link -> Int für die Preise von Kettengliedern gegeben. Der Preis
einer Kette ergibt sich als Summe der Preise der verwendeten Kettenglieder. Implementieren Sie
price:: (Link -> Int) -> Chain -> Int auf zwei verschiedene Arten:

(1) direkt mit den Mustern von Chain
(2) mit Hilfe von map oder eine Faltung

(d) Definieren Sie eine Funktion firstSplit::Link -> Chain -> (Chain,Chain), so dass durch
firstSplit m ch aus der Kette ch das erste Kettenglied aus dem Material m entfernt wird und
das Ergebnis die Teilketten links und rechts von dem entfernten Kettenglied repräsentiert.
Wenn ch mit m beginnt, ist die erste Teilkette leer. Wenn ch kein Kettenglied aus Material m
enthält oder das erste Kettenglied aus Material m am Ende liegt, dann ist die zweite Teilkette
leer.

Hinweis: Die Funktionen list und elem können hilfreich sein um den Punkt an dem getrennt
wird zu finden.

2. Traversierung von Bäumen (4 Punkte)
Schreiben Sie eine Funktion, welche die Elemente eine Binärbaums in Levelorderreihenfolge in einer
Liste ausgibt.

Hinweis: Verwenden Sie eine Liste um sich zu merken, welche Teilbäume auf den Ebenen noch
betrachtet werden müssen.

3. Struktuelle Induktion auf Bäumen (3 Punkte)
Betrachten Sie die folgenden Funktionsdefinitionen.


data Tree a = Leaf a | Node a (Tree a) (Tree a)
sumLeaves :: Tree a -> Integer
sumLeaves (Leaf x) = 1 --sL.1
sumLeaves (Node _ lt rt) = sumLeaves lt + sumLeaves rt --sL.2
sumNodes :: Tree a -> Integer
sumNodes (Leaf x) = 0 --sN.1
sumNodes (Node _ lt rt) = 1 + sumNodes lt + sumNodes rt --sN.2

Beweisen Sie mittels struktureller Induktion, dass für alle endlichen Bäume t :: Tree a gilt:

sumLeaves t = sumNodes t + 1

4. Foldable (4+3 Punkte)

(a) Recherchieren Sie welche Funktionen von der Typklasse Foldable zur Verfügung gestellt werden
und erklären Sie zwei Funktionen die nicht in der minimalen vollständigen Definition sind
im Detail. Geben Sie eine Implementierung dieser zwei Funktionen mit Hilfe der minimalen
vollständigen Signatur an. Sie müssen in der Lage sein die Implementierungen auf Nachfrage zu
erklären. Denken Sie daran Ihre Quellen anzugeben.

(b) Machen Sie BTree zu einer Instanz von Foldable (ohne deriving zu verwenden) und
identifizieren Sie die Traversierungsfunktion, welche Äquivalent zum Ausdruck foldr (:) [] ist.




*****



--1. Allgemeine Bäume (3+3+3+2+2 Punkte)

--(a) Geben Sie die Definition eines algebraischen Datentypes GTree für Bäume an, in denen jeder
--innere Knoten beliebig viele Kinder haben kann.

data GTree a = GLeaf
               | GNode a [GTree a] [GTree a]
               deriving (Eq, Show)

--(b) Schreiben Sie eine Funktion postOrderG, welche die Postorder-Reihenfolge eines allgemeinen 
--Baumes in eine Liste schreibt.

postorderG :: GTree a -> [a]
postorderG (GNode x kind) =  concatMap postorderG kind 
                            ++ [x]         

--(c) Bestimmen Sie die Laufzeit der Funktion postOrderG.
{-Die Laufzeit ist 𝑂(𝑛).

Bei Postorder wird jeder Knoten im Binärbaum besucht:

a) 1 mal, wenn es sich um einen Leaf handelt.

b) 1 mal wenn es ein Knoten mit nur einem Kind ist (entweder links oder rechts).

c) 2 mal, wenn der Knoten sowohl ein linkes als auch ein rechtes Kind hat. Hier werden ggf. links und rechts die Kinder 
verarbeitet, aber kein Knoten im Baum mehr als zweimal besucht. Wenn n die Anzahl der Knoten ist, ist die 
Worst-Case-Komplexität 𝑂(2𝑛) im Fall eines vollständigen Binärbaums und im besten Fall ist es 𝑂(𝑛)/

Wenn man die Konstanten ignoriert:

Best-Case-Zeit: 𝑂(𝑛)
Worst-Case-Zeit: 𝑂(𝑛) 
-}

--2. 
--(b) Schreiben Sie eine Funktion binaryTreeToSorted :: (Ord a) => BTree a -> [a], die die Einträge eines binären 
--Suchbaumes in sortierter Reihenfolge zurückgibt.

inorder :: (Ord a) => BTree a -> [a]
inorder Nil = []
inorder (Node t1 a t2) = inorder t1 ++ [a] ++ inorder t2

binaryTreeToSorted :: Ord a => [a] -> [a]
binaryTreeToSorted = inorder . addList 

--3.
--(a)

countIf :: (Int->Bool) -> Tree -> Int
countIf con (Leaf x) = if con x then 1 else 0
countIf con (Node left x right) = (if con x then 1 else 0) + (countIf con right) + (countIf con left) 

