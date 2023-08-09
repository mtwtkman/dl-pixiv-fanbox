{ nixpkgs ? import <nixpkgs> { } }:
nixpkgs.pkgs.haskell.compiler.ghcHEAD.callPackage ./nix/dl-pixiv-fanbox.nix { }
