all: main plugins

main: Main.hs PluginAPI.hs
	ghc --make -dynamic Main.hs

plugins: MD5ComparePlugin.hs TestPlugin.hs PluginAPI.hs
	ghc --make -dynamic $^

clean:
	rm -f *.o
	rm -f *.hi
	rm -f Main
