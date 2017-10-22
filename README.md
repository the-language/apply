与大部分Scheme方言的不同
=====================
只有`begin`中的非`define`部分是一定会求值,按顺序求值的

没有`set!`,用`(atom! value)` `(atom-get atom)` `(atom-set! atom value)` `(atom-map! f atom)` `(atom? value)`
