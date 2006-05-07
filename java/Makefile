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
## and run.
## Now, if we use Sun javac 1.5.0_06 with -target 1.4, we get
##   javac -target 1.4 -cp ../inst/jar/weka.jar RWekaInterfaces.java
##   javac: target release 1.4 conflicts with default source release 1.5
## hence we try what we use below.  If this does not work, we could
## hard-wire /usr/lib/j2sdk1.4-sun/bin/javac for JAVAC.
.java.class:
	$(JAVAC) -target 1.4 -source 1.4 -cp ../inst/jar/weka.jar $<
## </NOTE>

all: $(JARS)

RWeka.jar: $(OBJECTS)
	$(JAR) cvf $@ $(OBJECTS)
$(OBJECTS): ../inst/jar/weka.jar

install: all
	@mv $(JARS) ../inst/jar
clean:
	-@rm -f $(OBJECTS) $(JARS)
