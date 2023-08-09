{ mkDerivation, base, lib, req }:
mkDerivation {
  pname = "dl-pixiv-fanbox";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base req ];
  executableHaskellDepends = [ base ];
  doHaddock = false;
  license = "unknown";
  mainProgram = "dl-pixiv-fanbox";
}
