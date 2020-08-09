PREFIX = /usr/local
exec_prefix = $(PREFIX)
bindir = $(exec_prefix)/bin

INSTALL = install
INSTALL_PROGRAM = $(INSTALL) -m 0755 -D

retry: retry.adb
	gnatmake retry.adb

clean:
	gnatclean retry

install: retry
	$(INSTALL_PROGRAM) retry $(DESTDIR)$(bindir)/retry

uninstall:
	rm -f $(DESTDIR)$(bindir)/retry
