module Main where

import Graphics.Gloss
type Symbole  = Char
type Mot      = [Symbole]
type Axiome   = Mot
type Regles   = Symbole -> Mot
type LSysteme = [Mot]
type EtatTortue = (Point, Float)
type EtatDessin = ([EtatTortue], [Path])

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

pushEtat :: Config -> EtatDessin -> EtatDessin
pushEtat config ((p,cap):ts, paths) = ((p,cap):((p,cap):ts),([p]:paths) )


popEtat :: Config -> EtatDessin -> EtatDessin
popEtat  config (t:ts,path:pathN:paths) = (ts,((reverse path)++ path ++ pathN):paths)

interpreteSymbole :: Config -> EtatDessin -> Symbole -> EtatDessin
interpreteSymbole config (t:ts,path:paths) 'F' = ((p,f):ts,(p:path):paths)
                                       where (p,f) = avance config t
interpreteSymbole config (t:ts,paths) '-' = ((p,f):ts,paths)
                                       where (p,f) = tourneADroite config t
interpreteSymbole config (t:ts,paths) '+' = ((p,f):ts,paths)
                                       where (p,f) = tourneAGauche config t
interpreteSymbole config (t,path) '[' = pushEtat config (t,path)

interpreteSymbole config (t,path) ']' = popEtat config (t,path)
interpreteSymbole config (t,path) _ = error "Symbol not in alphabet"


interpreteMotInterm :: Config ->EtatDessin -> Mot -> EtatDessin
interpreteMotInterm config eD [] = eD
interpreteMotInterm config eD (x:xs) =interpreteMotInterm config eD' xs
                                      where eD' = interpreteSymbole config eD x 


interpreteMot :: Config -> Mot -> Picture
interpreteMot config mot = line( path)
                         where (x:xs,path:paths) = (interpreteMotInterm config eD mot)
                               eD =([(p,f)],[[p]])
                               (p,f) = etatInitial config
toInt :: Float -> Int
toInt x = round x 

lsystemeAnime :: LSysteme -> Config -> Float -> Picture
lsystemeAnime lS config f = interpreteMot config (lS !! (toInt f))


dessinAnime:: Float -> Picture
dessinAnime instant = interpreteMot (((-150,0),0),100,1,pi/3,"F+-") ( take enieme "F+F--F+F" )
                       where enieme = round instant `mod` 20
motSuivant :: Regles -> Mot -> Mot 
motSuivant r [] = []
motSuivant r (x:xs) =  r x ++ motSuivant r xs

lsysteme :: Axiome -> Regles -> LSysteme
lsysteme a r = iterate  (motSuivant r) a


brindille :: LSysteme
brindille = lsysteme "F" regles
    where regles 'F' = "F[-F]F[+F]F"
          regles  s  = [s]

broussaille :: LSysteme
broussaille = lsysteme "F" regles
    where regles 'F' = "FF-[-F+F+F]+[+F-F-F]"
          regles  s  = [s]

brindilleAnime :: Float -> Picture
brindilleAnime = lsystemeAnime brindille (((0, -400), pi/2), 800, 1/3, 25*pi/180, "F+-[]")

broussailleAnime :: Float -> Picture
broussailleAnime = lsystemeAnime broussaille (((0, -400), pi/2), 500, 2/5, 25*pi/180, "F+-[]")

main = animate (InWindow "L-système" (1000, 1000) (0, 0)) white broussailleAnime



dessin = interpreteMot (((0, -400), pi/2), 800, 1/3, 25*pi/180, "F+-[]") "F[-F]F[+F]F"
---main = display (InWindow "L-système" (1000, 1000) (0, 0)) white dessin


s = paths where (x:xs,paths)=(interpreteMotInterm (((0, -400), pi/2), 800, 1/3, 25*pi/180, "F+-[]") ([((0,-400),pi/2)],[[(0,-400)]]) "F[-F]F[+F")