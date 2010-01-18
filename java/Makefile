SOURCES = \
  RWekaInterfaces.java
OBJECTS = $(SOURCES:.java=.class)
JARS = RWeka.jar

JAVAC = javac
JAR = jar

.SUFFIXES:
.SUFFIXES: .class .java

## <NOTE>
## Keeps this in sync with the requirements as documented in
## http://www.cs.waikato.ac.nz/~ml/weka/index_requirements.html.
## As of 2006-06-22, the current release (3.4.7) only requires 1.4, but
## the developer branch ("HEAD") from the CVS needs Java 1.5 to compile
## and run.  Note that we ship with Weka 3.5.x which requires 1.5.
## To ensure that a jar possibly built with Sun javac 1.6.0 works a with
## a 1.5 runtime, use
##   $(JAVAC) -target 1.5 -source 1.5
## for compilation.
.java.class:
	$(JAVAC) -target 1.5 -source 1.5 -cp ../inst/java/weka.jar $<
## </NOTE>

all: $(JARS)

RWeka.jar: $(OBJECTS)
	$(JAR) cvf $@ $(OBJECTS)
$(OBJECTS): ../inst/java/weka.jar

## <ARGH>
## Ensure that dynamic class discovery is off: otherwise, the Weka class
## loader is unhappy and writes 
##   [GenericPropertiesCreator]
##   classloader in use is not the system classloader:
##   using static entries in weka/gui/GenericObjectEditor.props rather
##   than dynamic class discovery
## to stdout which in turn makes everyone else unhappy ...
../inst/java/weka.jar: FORCE
	@echo -n "updating $@ ..."
	@(pfile="weka/gui/GenericPropertiesCreator.props"; \
	  $(JAR) xf $@ $${pfile} ; \
	  mv $${pfile} $${pfile}.in ; \
	  sed 's/^UseDynamic=true/UseDynamic=false/' $${pfile}.in \
	    > $${pfile}; \
	  $(JAR) uf $@ $${pfile} ; \
	  rm -rf weka)
	@echo " done"
FORCE:

install: all
	@mv $(JARS) ../inst/java
clean:
	-@rm -f $(OBJECTS) $(JARS)
