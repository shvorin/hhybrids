# What hhybrids is
This is an implementation of Hybrid Chess rewritten in Haskell. For rules, details, etc see [original Python implementation](https://github.com/shvorin/hybrids).
# Build
hhybrids GUI is based on Graphics.UI.WX, so some stuff is required (TODO: the list of GHC libraries is not complete):

    sudo apt-get install ghc libwxgtk-media3.0-dev
    cabal  install wx

Build hhboard:

    ghc -main-is GUIBoard GUIBoard.hs
# Run
    ./GUIBoard
