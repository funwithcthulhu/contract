# Release Checklist

This project uses `contract` as both the opam package name and the public Dune
library name.

Before tagging a release:

```sh
opam install . --deps-only --with-test -y
opam exec -- dune build @all
opam exec -- dune runtest
opam exec -- dune build -p contract
opam exec -- dune runtest -p contract
opam lint contract.opam
opam exec -- dune exec examples/users_api.exe
```

Check that the example prints OpenAPI JSON with:

- `"openapi": "3.0.3"`
- `/users/{id}`
- `GET /users/{id}`
- `POST /users`

For 0.1.0:

```sh
git tag -a v0.1.0 -m "Release 0.1.0"
git push origin v0.1.0
opam publish
```

Use the tag created from the checked release commit. If `opam publish` cannot be
used from this machine, open an opam-repository pull request for `packages/contract/contract.0.1.0/opam`.
