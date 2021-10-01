{ mkDerivation, base, hashable, containers, pure-auth, pure-elm, pure-hooks, pure-json, pure-marker, pure-maybe, pure-router, pure-sync, pure-websocket, pure-txt, pure-sorcerer, text, stdenv }:
mkDerivation {
  pname = "pure-conjurer";
  version = "0.8.0.0";
  src = ./.;
  libraryHaskellDepends = 
    [ base hashable containers pure-auth pure-elm pure-hooks pure-json pure-marker pure-maybe pure-router pure-sync pure-websocket pure-txt pure-sorcerer text ];
  license = stdenv.lib.licenses.bsd3;
}
