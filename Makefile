install:
	[ ! -f ~/.emacs.d/init.el ] || mv ~/.emacs.d/init.el ~/.emacs.d/init.el.old
	ln -s $(PWD)/init.el ~/.emacs.d/init.el
