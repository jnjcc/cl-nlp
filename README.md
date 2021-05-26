cl-nlp
=====================
Natural Language Processing in Common Lisp

Unfortunately, this package is _NOT_ production ready.

For one thing, most of the linear algebra stuff is written from scratch,
which is definitely _NOT_ efficient enough.

Usage
---------

Tests
---------
```sh
cd ~/quicklisp/local-projects/
git clone https://github.com/jnjcc/cl-dataset
git clone https://github.com/jnjcc/cl-ml
git clone https://github.com/jnjcc/cl-nlp
ln -s cl-dataset/nlp cl-nlp/test/dataset
```

```lisp
(ql:quickload :cl-nlp)
(ql:quickload :cl-nlp-test)
(cl-nlp/test:run-all-tests)
```

COPYING
---------
Copyright (c) 2012-2015 jnjcc, [Yste.org](http://www.yste.org)

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.
