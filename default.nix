{ mkDerivation, ghc, base, stm, hashable, bytestring, containers, pure-auth, pure-bloom, pure-default, pure-elm, pure-hooks, pure-json, pure-marker, pure-maybe, pure-render, pure-router, pure-sync, pure-websocket, pure-websocket-cache, pure-time, pure-txt, pure-sorcerer, text, iproute, directory, filepath, stdenv }:
mkDerivation {
  pname = "pure-conjurer";
  version = "0.8.0.0";
  src = ./.;
  libraryHaskellDepends = 
    [ base stm hashable bytestring containers pure-auth pure-bloom pure-default pure-elm pure-hooks pure-json pure-marker pure-maybe pure-render pure-router pure-sync pure-websocket pure-websocket-cache pure-time pure-txt pure-sorcerer text directory filepath ]
    ++ (if ghc.isGhcjs or false then [ ] else [ iproute ]);
  license = stdenv.lib.licenses.bsd3;
}
