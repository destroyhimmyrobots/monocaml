OC=ocamlc
CFLAGS=-safe-string
LIB_DIR=src/lib
OBJ_DIR=src/obj
OSC_DIR=src/osc
COL_DIR=src/collections
MONOME_DIR=src/monome
OSC_OBJ_FILES=$(OSC_DIR)/OscMatcher.cmo \
			  $(OSC_DIR)/OscMessageBuilder.cmo
COLLECTIONS_OBJ_FILES=$(COL_DIR)FixedSizeList.cmo
MBASE_OBJ_FILES=$(MONOME_DIR)/exceptions.cmo \
				$(MONOME_DIR)/types.cmo \
				$(MONOME_DIR)/utils.cmo 

.PHONY: clean setup

setup: 
	mkdir -p {bin,lib,obj/{$(COL_DIR),$(MONOME_DIR),$(OSC_DIR)}}

clean: 
	rm -rf {bin,lib,obj}

%.cmo: %.ml
	$(OC) $(CFLAGS) -c -o $(OBJ_DIR)/$@ $< 

osc: $(OSC_OBJ_FILES)

collections: $(COLLECTIONS_OBJ_FILES)

serialosc: $(MONOME_DIR)/serialosc/SerialOscApi.cmo

monome_base: $(MBASE_OBJ_FILES)

monome: monome_base serialosc

all: clean setup osc monome
