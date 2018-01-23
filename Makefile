ML=gmlray.ml
EXE=$(ML:.ml=)

#OUT=gmlray_output.pbm

default: run

exe: $(EXE)

run: $(EXE)
	time ./$< >glmray.log

#out : $(OUT)

#$(OUT) : $(EXE)
#	./$< >$@

%:%.ml
	ocamlc -dtypes -o $@ $<
# | sed 's/File "\(.*\)", line \([0-9]*\), characters \([0-9]*\):/\1 \2 \3/'

%.opt:%.ml
	ocamlopt -o $@ $<



#voir: $(OUT)
#	display $<

png:
	( for a in *.pgm ; do convert -verbose "$$a" "$${a/.pgm/.png}" ; done ; \
	for a in *.pbm ; do convert -verbose "$$a" "$${a/.pbm/.png}" ; done ; )

clean:
	rm -f *.o *.cmx *.cmi *.cmo

arc:
	(set -x ; \
	DIRNAME=$$(date +archives/%Y_%m_%d/) ; mkdir -p "$$DIRNAME" ; \
	FILENAME=$$(date +archives/%Y_%m_%d/$(ML)_%Y_%m_%d_%Hh%Mm%Ss) ; \
	cp -av $(ML) $$FILENAME ; \
	)
