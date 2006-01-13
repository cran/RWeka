SOURCES = \
  RWekaInterfaces.java
OBJECTS = $(SOURCES:.java=.class)
JARS = RWeka.jar

.SUFFIXES:
.SUFFIXES: .class .java

.java.class:
	javac -cp ../inst/jar/weka.jar $<

all: $(JARS)

RWeka.jar: $(OBJECTS)
	jar cvf $@ $(OBJECTS)

install: all
	@mv $(JARS) ../inst/jar
clean:
	-@rm -f $(OBJECTS) $(JARS)
