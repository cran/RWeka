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
## As of 2006-04-20, the current release (3.4.7) only requires 1.4, but
## the developer branch ("HEAD") from the CVS needs Java 1.5 to compile
## and run.  To ensure that a jar built with e.g. Sun javac 1.5.0_06 and
## Weka 3.4.x works with 1.4, we would use
##   $(JAVAC) -target 1.4 -source 1.4
## as otherwise we get
##   javac -target 1.4 -cp ../inst/jar/weka.jar RWekaInterfaces.java
##   javac: target release 1.4 conflicts with default source release 1.5
## However, we now ship with Weka 3.5.x which requires 1.5 anyway ...
.java.class:
	$(JAVAC) -cp ../inst/jar/weka.jar $<
## </NOTE>

all: $(JARS)

RWeka.jar: $(OBJECTS)
	$(JAR) cvf $@ $(OBJECTS)
$(OBJECTS): ../inst/jar/weka.jar

install: all
	@mv $(JARS) ../inst/jar
clean:
	-@rm -f $(OBJECTS) $(JARS)
