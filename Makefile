
xmonad : xmonad.hs
	ghc -o xmonad-i386-linux xmonad.hs

clean :
	rm -f xmonad xmonad.hi xmonad.o
