{ pkgs ? import <nixpkgs> { } }:
with pkgs;
mkShell {
  packages = [
    zlib
    ghc
    cabal-install
    haskell-language-server
    haskellPackages.ormolu
    haskellPackages.cabal2nix
    haskellPackages.haskell-dap
    haskellPackages.haskell-debug-adapter
    haskellPackages.ghci-dap
    haskellPackages.cabal-fmt
  ];
  shellHook = ''
    alias b="cabal build"
    alias c="cabal clean"
    alias fmt="ormolu -i ./**/*.hs"
    alias repl="cabal repl"
    alias run="cabal run --"
    alias t="cabal test"
    alias fmt:cabal="cabal-fmt -i ./dl-pixiv-fanbox.cabal"
    alias pack="cabal2nix . > nix/dl-pixiv-fanbox.nix"
  '';
}
