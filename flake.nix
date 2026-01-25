{
  description = "typefuckery";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };
        ocamlPkgs = pkgs.ocamlPackages;

        projectSrc = pkgs.lib.cleanSourceWith {
          src = ./.;
          filter =
            path: type:
            pkgs.lib.cleanSourceFilter path type
            && builtins.baseNameOf path != "_build"
            && builtins.baseNameOf path != "result";
        };

        typefuckeryPackage = ocamlPkgs.buildDunePackage {
          pname = "typefuckery";
          version = "0.1.0";
          src = projectSrc;
          duneVersion = "3";
          duneBuildFlags = [ "--ignore-lock-dir" ];
          duneInstallFlags = [ "--ignore-lock-dir" ];
        };
      in
      {
        packages.default = typefuckeryPackage;

        checks.default = typefuckeryPackage.overrideAttrs (old: {
          pname = "typefuckery-tests";
          doCheck = true;
          checkPhase = ''
            runHook preCheck
            dune runtest --ignore-lock-dir
            runHook postCheck
          '';
        });

        devShells.default = pkgs.mkShell {
          inputsFrom = [ self.packages.${system}.default ];
          packages =
            (with ocamlPkgs; [
              ocaml-lsp
              utop
              ocamlformat
            ])
            ++ [
              pkgs.git
              pkgs.cacert
            ];
        };
      }
    );
}
