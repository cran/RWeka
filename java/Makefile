SOURCES = \
  RWekaInterfaces.java
OBJECTS = $(SOURCES:.java=.class)
JARS = RWeka.jar

JAVAC = javac
JAR = jar
WEKAJAR = ../../RWekajars/inst/java/weka.jar

.SUFFIXES:
.SUFFIXES: .class .java

## <NOTE>
## Keeps this in sync with the requirements as documented in
## <http://www.cs.waikato.ac.nz/~ml/weka/index_requirements.html>.
.java.class:
	$(JAVAC) -target 1.6 -source 1.6 -cp $(WEKAJAR) $<
## </NOTE>

all: $(JARS)

RWeka.jar: $(OBJECTS)
	$(JAR) cvf $@ $(OBJECTS)
$(OBJECTS): $(WEKAJAR)

install: all
	@mv $(JARS) ../inst/java
clean:
	-@rm -f $(OBJECTS) $(JARS)
