include ../../make.conf

CC = gcc

CFLAGS = ${OS_CFLAGS} ${GSL_CFLAGS} -O6
LDFLAGS = ${OS_LDFLAGS} ${GSL_LDFLAGS} -lm -lgsl -lgslcblas

EXECUTABLES = jackknife_stripe calculate_area
OBJS = jackknife_stripe.o calculate_area.o

all: ${EXECUTABLES}

clean: 
	rm -f ${EXECUTABLES} ${OBJS}

jackknife_stripe: jackknife_stripe.o
calculate_area: calculate_area.o
