{
  description = "A flake for TC-ML";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils }:
    let
      systems = [ "x86_64-linux" ];
    in
    flake-utils.lib.eachSystem systems (system:
      let
        pkgs = import nixpkgs { inherit system; };
        ocamlPackages = pkgs.ocaml-ng.ocamlPackages_4_14;
      in
      {
        formatter = pkgs.nixpkgs-fmt;
        packages = {
          tcml = pkgs.stdenv.mkDerivation {
            pname = "tcml";
            version = "0.0.0";
            src = self;
            nativeBuildInputs = (
              with pkgs;
              [ ]
            ) ++ (
              with ocamlPackages;
              [
                ocaml
                dune_3
                ocaml-lsp
                ocamlformat
              ]
            );
            buildPhase = ''
              dune build
            '';
            installPhase = ''
              cp -r _build/default/ $out/
              cp $out/bin/main.exe $out/bin/${self.packages.${system}.tcml.pname}
            '';
          };
          default = self.packages.${system}.tcml;
        };
      }
    );
}
