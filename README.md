# bptr-plist

## Intro

Functions to operate on property lists.  
Trying to handle property lists with 'empty' keys:

```
 (:a 1 :b 2 :c :d 4)
              ^ <-- this is the 'empty' key
```
So the keywords can't be the values of keys.  

It's used in [`bptr-html`](https://github.com/Bad-ptr/bptr-html) and looks like working well.  
But I think it's very buggy beyond functions that i used in bptr-html ;p.
