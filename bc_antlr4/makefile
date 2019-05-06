MAKEFLAGS += Rr

ANTLR4 := java -jar /usr/local/lib/antlr-4.7.2-complete.jar
GRUN := java org.antlr.v4.gui.TestRig   bc parseAll input.txt

ANTLR_OUTPUTS := bcLexer.java bcParser.java bcBaseListener.java bcListener.java

.PHONY: clean build run

build:
	$(ANTLR4) bc.g4 && javac *.java

clean:
	rm -f *.class *.interp *.tokens $(ANTLR_OUTPUTS)

run: build
	java Program

tree: build
	$(GRUN) -tree

gui: build
	$(GRUN) -gui
