clean:
	fsc -shutdown
	rm -f *.class
	rm -f *.jar
	
dist: build
	jar cfm regex_keyspace.jar manifest.txt *.class

build:
	fsc *.scala
	
