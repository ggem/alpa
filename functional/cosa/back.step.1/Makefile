PARAMS = 
SGEN = sgen
SGENFLAGS = $(PARAMS)

PAT_FILES = lex.ssl \
	ids.ssl   exps.ssl   funDefs.ssl   conDefs.ssl \
	ids_f.ssl exps_f.ssl funDefs_f.ssl conDefs_f.ssl \
	int.ssl bool.ssl primFun_f.ssl funCall_f.ssl let_f.ssl \
	idExpPairList.ssl newName.ssl substitution.ssl match.ssl \
	prog.ssl transformation.ssl util.ssl \
	lists.ssl stepcount.ssl timebound.ssl

EDITORS = timebound
all: $(EDITORS)

$(EDITORS): $(PAT_FILES) 
	$(SGEN) -o $@ $(SGENFLAGS) $(PAT_FILES) 

clean:	
	$(SGEN) -clean
	-rm -f $(EDITORS)
