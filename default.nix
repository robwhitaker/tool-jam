{ mkDerivation, base, containers, data-default, fsnotify, lib, path
, path-io, relude, text
}:
mkDerivation {
  pname = "taskd";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base containers data-default fsnotify path path-io relude text
  ];
  description = "Daemon that handles changes in your ~/tools directory";
  license = lib.licenses.bsd3;
}
