#$Id: Makefile,v 1.4 2001-11-18 13:20:09 gotm Exp $
#
# Makefile for making new release of GOTM.
#
# Before doing - make release - be sure to commit all files.

# Remember to update  - VERSION - for each new release


# 20010531
VERSION=2.3.5
# 20010531
VERSION=2.3.6
# 20010613
VERSION=2.3.7

export CVSROOT=gotm@gotm.net:/cvsroot

TAGNAME	= v$(shell cat VERSION | tr . _)
RELEASE	= gotm-$(VERSION)
OLDDIR	= $(HOME)/old_gotm/Unstable
TARFILE	= $(RELEASE).tar.gz
RHOST	= gotm.net
RDIR	= src/devel

SCP	= /usr/bin/scp
SSH	= /usr/bin/ssh

all: release

new_version:
	@if [ -d $(OLDDIR)/$(RELEASE) ]; then		\
	   echo "$(RELEASE) is already released";	\
	   echo "update VERSION in Makefile";		\
	   exit 99;					\
	fi;
	echo $(VERSION) > VERSION

unstable: new_version
	echo $(TAGNAME)
	cvs tag $(TAGNAME)	
	(cd src/ ; make ../include/version.h)
	cvs2cl
	cvs export -r $(TAGNAME) -d $(RELEASE) gotm
	mv $(RELEASE) $(OLDDIR)
	cp ChangeLog $(OLDDIR)/$(RELEASE)

tarfile: unstable
	(cd $(OLDDIR); tar -cvzf $(TARFILE) $(RELEASE) )

release: tarfile
	$(SCP) $(OLDDIR)/$(TARFILE) $(RHOST):$(RDIR)
	$(SSH) $(RHOST) \( cd $(RDIR) \; ln -sf $(TARFILE) gotm-devel.tar.gz \) 

diff:
	( cvs diff > cvs.diff ; vi cvs.diff )
	
update:
	( cvs update > cvs.update ; vi cvs.update )
	
#-----------------------------------------------------------------------
# Copyright (C) 2001 - Hans Burchard and Karsten Bolding (BBH)         !
#-----------------------------------------------------------------------
