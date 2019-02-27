module Main where
 
import Graphics.Gloss
type Symbole  = Char
type Mot      = [Symbole]
type Axiome   = Mot
type Regles   = Symbole -> Mot
type LSysteme = [Mot]
type EtatTortue = (Point, Float)
type EtatDessin = (EtatTortue, Path)

type Config = (EtatTortue -- État initial de la tortue
              ,Float      -- Longueur initiale d’un pas
              ,Float      -- Facteur d’échelle
              ,Float      -- Angle pour les rotations de la tortue
              ,[Symbole]) -- Liste des symboles compris par la tortue

regle :: Regles
regle 'F' = "F-F++F-F"
regle '-' = "-"
regle '+' = "+"
regle '[' = "["
regle ']' = "]"
regle _ = error "Symbol not in alphabet"
--- motSuivant regle ['F','-','F','+','+','F','-','F']
--Simo
etatInitial :: Config -> EtatTortue
etatInitial (x,_,_,_,_) = x

longueurPas :: Config -> Float
longueurPas (_,y,_,_,_) = y

facteurEchelle :: Config -> Float
facteurEchelle (_,_,z,_,_) = z

angle :: Config -> Float
angle (_,_,_,z,_) = z

symbolesTortue :: Config -> [Symbole]
symbolesTortue (_,_,_,_,a) = a

avance :: Config -> EtatTortue -> EtatTortue
avance t ((x,y),f) = ((x+d*cos(f), y+d*sin(f)),f)
            where d = longueurPas t

tourneAGauche :: Config -> EtatTortue -> EtatTortue
tourneAGauche t ((x,y),cap) = ((x,y),cap')
            where cap' = cap + a
                  a = angle t

tourneADroite :: Config -> EtatTortue -> EtatTortue
tourneADroite t ((x,y),cap) = ((x,y),cap')
            where cap' = cap - a
                  a = angle t 
---Victor
filtreSymbolesTortue :: Config -> Mot -> Mot                
filtreSymbolesTortue (_,_,_,_, alphabet) word = filter (\sym -> sym `elem` alphabet) word

---Simo

motSuivant :: Regles -> Mot -> Mot 
motSuivant r [] = []
motSuivant r (x:xs) =  r x ++ motSuivant r xs

motSuivant' :: Regles -> Mot -> Mot 
motSuivant' r [] = []
motSuivant' r m =  head (map r m )

---[r x | x <- xs]

lsysteme :: Axiome -> Regles -> LSysteme
lsysteme a r = iterate  (motSuivant r) a


interpreteSymbole :: Config -> EtatDessin -> Symbole -> EtatDessin
interpreteSymbole config (t,path) 'F' = ((p,f),p:path)
                                       where (p,f) = avance config t
interpreteSymbole config (t,path) '-' = ((p,f),path)
                                       where (p,f) = tourneADroite config t
interpreteSymbole config (t,path) '+' = ((p,f),path)
                                       where (p,f) = tourneAGauche config t
interpreteSymbole config (t,path) _ = error "Symbol not in alphabet"

interpreteMotInterm :: Config ->EtatDessin -> Mot -> EtatDessin
interpreteMotInterm config eD [] = eD
interpreteMotInterm config eD (x:xs) =interpreteMotInterm config eD' xs
                                      where eD' = interpreteSymbole config eD x 

interpreteMot :: Config -> Mot -> Picture
interpreteMot config mot = line(path)
                         where (x,path) = (interpreteMotInterm config eD mot)
                               eD =((p,f),[p])
                               (p,f) = etatInitial config


---dessine le flocon direcetment
---dessin = interpreteMot (((-150,0),0),100,1,pi/3,"F+-") "F+F--F+F"
---main = display (InWindow "L-système" (1000, 1000) (0, 0)) white dessin

toInt :: Float -> Int
toInt x = round x 

lsystemeAnime :: LSysteme -> Config -> Float -> Picture
lsystemeAnime lS config f = interpreteMot config (lS !! (toInt f))



dessinAnime:: Float -> Picture
dessinAnime instant = interpreteMot (((-150,0),0),100,1,pi/3,"F+-") ( take enieme "F+F--F+F" )
                       where enieme = round instant `mod` 20

--main = animate (InWindow "L-système" (1000, 1000) (0, 0)) white dessinAnime

vonKoch1 :: LSysteme
vonKoch1 = lsysteme "F" regles
    where regles 'F' = "F-F++F-F"
          regles  s  = [s]

vonKoch2 :: LSysteme
vonKoch2 = lsysteme "F++F++F++" regles
    where regles 'F' = "F-F++F-F"
          regles  s  = [s]

hilbert :: LSysteme
hilbert = lsysteme "X" regles
    where regles 'X' = "+YF-XFX-FY+"
          regles 'Y' = "-XF+YFY+FX-"
          regles  s  = [s]

dragon :: LSysteme
dragon = lsysteme "FX" regles
    where regles 'X' = "X+YF+"
          regles 'Y' = "-FX-Y"
          regles  s  = [s]

vonKoch1Anime :: Float -> Picture
vonKoch1Anime = lsystemeAnime vonKoch1 (((-400, 0), 0), 800, 1/3, pi/3, "F+-")

vonKoch2Anime :: Float -> Picture
vonKoch2Anime = lsystemeAnime vonKoch2 (((-400, -250), 0), 800, 1/3, pi/3, "F+-")

hilbertAnime :: Float -> Picture
hilbertAnime = lsystemeAnime hilbert (((-400, -400), 0), 800, 1/2, pi/2, "F+-")

dragonAnime :: Float -> Picture
dragonAnime = lsystemeAnime dragon (((0, 0), 0), 50, 1, pi/2, "F+-")


main = animate (InWindow "L-système" (1000, 1000) (0, 0)) white vonKoch1Anime