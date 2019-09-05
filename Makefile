xmonad-64 : xmonad.hs
	/usr/bin/ghc -o xmonad-x86_64-linux -O2 xmonad.hs

xmonad-32 : xmonad.hs
	ghc -o xmonad-i386-linux xmonad.hs

clean :
	rm -f xmonad xmonad.hi xmonad.o
