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

For 0.2.0:

```sh
git tag -a 0.2.0 -m "Release 0.2.0"
git push origin 0.2.0
opam publish --tag 0.2.0 -v 0.2.0 .
```

Use the tag created from the checked release commit. If `opam publish` cannot be
used from this machine, open an opam-repository pull request for `packages/contract/contract.0.2.0/opam`.
