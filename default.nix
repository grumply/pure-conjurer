{ mkDerivation, ghc, base, hashable, containers, pure-auth, pure-elm, pure-hooks, pure-json, pure-marker, pure-maybe, pure-router, pure-sync, pure-websocket, pure-time, pure-txt, pure-sorcerer, text, iproute, stdenv }:
mkDerivation {
  pname = "pure-conjurer";
  version = "0.8.0.0";
  src = ./.;
  libraryHaskellDepends = 
    [ base hashable containers pure-auth pure-elm pure-hooks pure-json pure-marker pure-maybe pure-router pure-sync pure-websocket pure-time pure-txt pure-sorcerer text ]
    ++ (if ghc.isGhcjs or false then [ ] else [ iproute ]);
  license = stdenv.lib.licenses.bsd3;
}
