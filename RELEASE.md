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
opam exec -- dune exec examples/http_style_app.exe
```

Check that the examples print OpenAPI JSON with:

- `"openapi": "3.0.3"`
- `/users/{id}`
- `GET /users/{id}`
- `POST /users`

When cutting a release:

```sh
git tag -a <version> -m "Release <version>"
git push origin <version>
opam publish --tag <version> -v <version> .
```

Use the tag created from the checked release commit. If `opam publish` cannot be
used from this machine, open an opam-repository pull request for the package
version being released.
