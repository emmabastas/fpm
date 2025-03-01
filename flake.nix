{
  description = "";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let pkgs = nixpkgs.legacyPackages.${system}; in
        {
          devShells.default = pkgs.mkShell {
            packages = with pkgs; [
              gfortran9
              (python3.withPackages (python-pkgs: [
                python-pkgs.pip
              ]))
            ];

            shellHook = ''
              python -m venv .venv
              source .venv/bin/activate
              pip install -r requirements.txt
            '';
          };
        }
      );
}
