# `100ms_dot_emacs`

Is it possible to start emacs in just 100 milliseconds? Yes.

But how? The key is to cache everything at compile time, no `load-path` is
needed.

I DO use `straight.el` and `use-package` for package management, but
only for compile time. At runtime, all required packages are compiled
and the path of each packages are also cached in the compiled byte
codes. `straight.el` and `use-package` are not _required_ at runtime.


At compile time, everything is loaded including `straight.el` and
`use-package`. All paths of installed packages are added into
`load-path`. When compile a `init.el`, all required packages are
compiled, thanks to `straight.el`.

But during the compilation, I hijack the special handler for compiler
`require` form. Usually, `require` form leave `FILENAME` empty,
i.e. emacs searches `FILENAME` at runtime, it is not so fast,
especially you have many elements in `load-path`, and usually emacs
ends with the same path. So, why not cache the search result at
compile time. The hijacked version of handler replaces the searched
FILENAME in the compiled byte codes, i.e. no load path is needed at
runtime.

A similiar trick is applied for autoload. In `autoload` form, usually
the third element `FILE` is a bare filename without directory name,
after compiling all required packages, I dump all autoloads forms
discovered by `straight.el` into a dedicated file,
e.g. `~/.emacs.d/.autoloads.el` and compile it into a 'elc' file, all
`FILE` are replaced with corresponding full path.

In the sample `init.el`, I can start emacs in less than 100ms and I
still uses many packages.