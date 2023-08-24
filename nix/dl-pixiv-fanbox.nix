{ mkDerivation, aeson, async, base, brick, bytestring, containers
, directory, exceptions, lib, microlens, microlens-mtl
, microlens-th, modern-uri, mtl, regex-tdfa, req, scalpel, text
, time, transformers, vector, vty
}:
mkDerivation {
  pname = "dl-pixiv-fanbox";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson async base brick bytestring containers directory exceptions
    microlens microlens-mtl microlens-th modern-uri mtl regex-tdfa req
    scalpel text time transformers vector vty
  ];
  executableHaskellDepends = [ base ];
  license = "unknown";
  mainProgram = "dl-pixiv-fanbox";
}
