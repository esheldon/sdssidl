# system dependent stuff is in the config
include ../make.conf

INCLUDE	= -I${IDL_DIR}/external/include
LOCAL_CCFLAGS = $(INCLUDE) $(OS_CFLAGS) $(SDSS_CFLAGS)
LDFLAGS = ${OS_LDFLAGS}
LD=gcc

LIBOBJS = \
          atlasImages.o\
          dataIo.o\
          dervish.o\
          main.o\
          main_mask.o\
          phSpanUtil.o\
          read.o\
          rice.o\
          write.o\
	  read_atlasUtil.o

LIBOBJS_ALL = $(LIBOBJS) read_atlasIDL.o read_objmaskIDL.o recframeIDL.o

LIBRARIES = read_atlasIDL.so read_objmaskIDL.so recframeIDL.so
DLMS = read_atlasIDL.dlm read_objmaskIDL.dlm recframeIDL.dlm

# build and install library

all: $(LIBRARIES)
	cp -f $(LIBRARIES) ../DLM
	cp -f $(DLMS) ../DLM

# Rules for building library

read_atlasIDL.so: $(LIBOBJS) read_atlasIDL.o
	@ echo; echo linking $@
	$(LD) $(LDFLAGS) -o $@ $(LIBOBJS) read_atlasIDL.o

read_objmaskIDL.so: $(LIBOBJS) read_objmaskIDL.o
	@ echo; echo linking $@
	$(LD) $(LDFLAGS) -o $@ $(LIBOBJS) read_objmaskIDL.o

recframeIDL.so: $(LIBOBJS) recframeIDL.o
	@ echo; echo linking $@
	$(LD) $(LDFLAGS) -o $@ $(LIBOBJS) recframeIDL.o

clean: 
	- rm -f $(LIBOBJS_ALL) $(LIBRARIES) 
	- cd ../DLM/; rm -f $(LIBRARIES); rm -f $(DLMS)

.SUFFIXES:
.SUFFIXES: .o .c

.c.o:
	@ echo; echo compiling $<
	$(CC) -c $(LOCAL_CCFLAGS) $<
