# Installieren

1. ghci & cabal
2. opencv installieren
3. CV clonen und installiern (siehe unten)
4. cabal install cryptohash
5. cabal install plugins

## Cave

beim cryptohash SO das -ghc... im Dateinamen in ~/.cabal/lib/.../ entfernen.

## CV

aktueller fork von CV, der builded:

<https://github.com/TomMD/CV>

[mein fork mit implementiertem compareHistogram](git@github.com:m0ru/CV.git)

^ funktioniert mit:

* c2hs-0.17.2
* ghci-7.8.2
* cabal-1.20.0.1
* opencv-2.4.8-2

Beim installieren sollte das flag für OpenCV-2.4 mit angegeben werden

    cabal install path/to/CV-clone/CV.cabal -fopencv24

