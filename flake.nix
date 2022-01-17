{
  description = "touchpage";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    # let
    #   makeElmPkg = { pkgs, additionalInputs ? [ ], pythonPackages ? (ps: [ ]) }:
    #     pkgs.stdenv.mkDerivation {
    #       name = "touchpage-elm";
    #       src = ./.;
    #       buildPhase = pkgs.elmPackages.fetchElmDeps {
    #         elmPackages = import ./elm/elm-srcs.nix;
    #         elmVersion = "0.19.1";
    #         registryDat = ./elm/registry.dat;
    #       } + ''
    #         cd elm
    #        	elm-optimize-level-2 src/Main.elm --output=dist/main.js
    #       '';
    #       installPhase = ''
    #         mkdir $out
    #         cp -r dist/* $out
    #       '';
    #       buildInputs = with pkgs;
    #          [
    #           elmPackages.elm
    #           elmPackages.elm-optimize-level-2
    #         ] ++ additionalInputs;
    #     };
    # in
    flake-utils.lib.eachDefaultSystem (
      system: let
        pname = "touchpage-controls";
        pkgs = nixpkgs.legacyPackages."${system}";
      in
        rec {
          inherit pname;
          # `nix build`

          # `nix run`

          # `nix develop`
          devShell = pkgs.mkShell {
            nativeBuildInputs = with pkgs; [
              elmPackages.elm
              elmPackages.elm-analyse
              elmPackages.elm-doc-preview
              elmPackages.elm-format
              elmPackages.elm-live
              elmPackages.elm-test
              elmPackages.elm-upgrade
              elmPackages.elm-xref
              elmPackages.elm-language-server
              elmPackages.elm-verify-examples
              elmPackages.elmi-to-json
              elmPackages.elm-optimize-level-2
            ];
          };
        }
    );
}

