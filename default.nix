{ mkDerivation, base, containers, directory, filepath, fsnotify
, lib, path, path-io, record-dot-preprocessor, record-hasfield
, relude, text
}:
mkDerivation {
  pname = "taskd";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base containers directory filepath fsnotify path path-io
    record-dot-preprocessor record-hasfield relude text
  ];
  description = "Daemon that handles changes in your ~/tools directory";
  license = lib.licenses.bsd3;
}
