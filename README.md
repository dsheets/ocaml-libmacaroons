# ocaml-libmacaroons

[Ctypes](https://github.com/ocamllabs/ocaml-ctypes) bindings to
[libmacaroons](https://github.com/rescrv/libmacaroons).

``` ocaml
(* example libmacaroons use *)
```

## Considerations

**This binding has not been thoroughly and independently audited so your
use case must be able to tolerate this uncertainty.**

Despite ocaml-libmacaroons' thin interface on top of *libmacaroons*, it is
still important to be mindful of security invariants. In particular, you
should ensure that nonces used for cryptographic operations are
**never** repeated with the same key set.

## Tests

Internal consistency tests may be found in `lib_test`.
