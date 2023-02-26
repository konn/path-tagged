# path-tagged - thin wrapper around `path` library augmented with the target entity

This package provides a thin wrapper type `PathTo entity b t` of `Path b t` from [`path`][pathlib] library.

[pathlib]: https://hackage.haskell.org/package/path

The following hopefully describes the image:

```hs
(</>) :: PathTo e b Dir -> PathTo e' (RelTo e) t -> PathTo e' b t
parent :: PathTo e b t -> PathTo (Parent e) b Dir
```
