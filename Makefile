all: main plugins

main: Main.hs PluginAPI.hs Sensing.hs Strings.hs
	ghc --make -dynamic Main.hs Sensing.hs Strings.hs

plugins: MD5ComparePlugin.hs TestPlugin.hs PluginAPI.hs
	ghc --make -dynamic $^

clean:
	rm -f *.o
	rm -f *.hi
	rm -f Main
	rm -f *~
