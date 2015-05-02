TESTS = check_table check_mem_intern
check_PROGRAMS = check_table check_mem_intern

check_table_SOURCES = check_table.c table.c table.h
check_table_CFLAGS = @CHECK_CFLAGS@
check_table_LDADD = @CHECK_LIBS@ -lcheck

check_mem_intern_SOURCES = check_mem_intern.c mem_intern.c mem_intern.h
check_mem_intern_CFLAGS = @CHECK_CFLAGS@
check_mem_intern_LDADD = @CHECK_LIBS@ -lcheck

noinst_LIBRARIES = libnsys.a
libnsys_a_SOURCES = table.c table.h mem_intern.c mem_intern.h \
	ptr_ocaml.c table_ocaml.c mem_intern_ocaml.c
