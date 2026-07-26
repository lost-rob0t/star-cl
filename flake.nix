{
  description = "Star-cl: StarIntel v0.9.0 document implementation";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
      ];

      forAllSystems = nixpkgs.lib.genAttrs systems;

      mkPackageSet = pkgs:
        let
          sbcl' = pkgs.sbcl.withOverrides (lispFinal: _lispPrev: {
            cms-ulid = pkgs.sbcl.buildASDFSystem {
              pname = "cms-ulid";
              version = "0-unstable-2024-04-20";

              src = pkgs.fetchgit {
                url = "https://gitlab.com/colinstrickland/cms-ulid.git";
                rev = "fff84302dee5db42fb90aafd834af3ffbfd6c2bb";
                hash = "sha256-B5rekME60bWHk47kDepQpOr9drjgXjZBiRpA+Ob1CuU=";
              };

              lispLibs = [
                lispFinal.local-time
                lispFinal.ironclad
                lispFinal.bit-smasher
                lispFinal.serapeum
              ];

              dontStrip = true;
            };

            starintel-v090 = pkgs.sbcl.buildASDFSystem {
              pname = "starintel-v090";
              version = "0.9.0";
              src = self;

              lispLibs = [
                lispFinal.jzon
                lispFinal.cl-ppcre
              ];

              systems = [ "starintel-v090" ];
              asdFilesToKeep = [ "starintel-v090.asd" ];
              dontStrip = true;
            };

            starintel = pkgs.sbcl.buildASDFSystem {
              pname = "starintel";
              version = "0.9.0";
              src = self;

              lispLibs = [
                lispFinal.jsown
                lispFinal.jzon
                lispFinal.cl-ppcre
                lispFinal.ironclad
                lispFinal.local-time
                lispFinal.cms-ulid
                lispFinal.str
                lispFinal.closer-mop
              ];

              systems = [ "starintel" ];
              asdFilesToKeep = [
                "src/starintel.asd"
                "starintel-v090.asd"
                "starintel-test.asd"
              ];
              dontStrip = true;
            };

            starintel-test = pkgs.sbcl.buildASDFSystem {
              pname = "starintel-test";
              version = "0.9.0";
              src = self;

              lispLibs = [
                lispFinal.starintel
                lispFinal.fiveam
              ];

              systems = [ "starintel-test" ];
              dontStrip = true;
            };
          });

          cms-ulid = sbcl'.pkgs.cms-ulid;
          starintel-v090 = sbcl'.pkgs.starintel-v090;
          starintel = sbcl'.pkgs.starintel;
          starintel-test = sbcl'.pkgs.starintel-test;

          sbcl-wrapped = sbcl'.withPackages (ps: [
            ps.starintel
            ps.starintel-v090
          ]);

          sbcl-test-wrapped = sbcl'.withPackages (ps: [
            ps.starintel-test
          ]);
        in
        {
          inherit
            cms-ulid
            starintel-v090
            starintel
            starintel-test
            sbcl-wrapped
            sbcl-test-wrapped
            ;
        };
    in
    {
      packages = forAllSystems (system:
        let
          packageSet = mkPackageSet nixpkgs.legacyPackages.${system};
        in
        {
          default = packageSet.starintel;
          inherit (packageSet)
            cms-ulid
            starintel-v090
            starintel
            starintel-test
            sbcl-wrapped
            sbcl-test-wrapped
            ;
        });

      checks = forAllSystems (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          packageSet = mkPackageSet pkgs;

          testCheck = pkgs.stdenvNoCC.mkDerivation {
            pname = "starintel-tests-check";
            version = "0.9.0";
            src = self;

            nativeBuildInputs = [ packageSet.sbcl-test-wrapped ];
            dontConfigure = true;

            buildPhase = ''
              runHook preBuild

              export HOME="$TMPDIR"
              export XDG_CACHE_HOME="$HOME/.cache"

              cp -r "$src" "$TMPDIR/source"
              chmod -R u+w "$TMPDIR/source"
              cd "$TMPDIR/source"

              ${packageSet.sbcl-test-wrapped}/bin/sbcl \
                --non-interactive \
                --no-userinit \
                --no-sysinit \
                --eval "(require :asdf)" \
                --eval "(push (truename \".\") asdf:*central-registry*)" \
                --eval "(asdf:load-system :starintel-test)" \
                --eval "(handler-case
                          (progn
                            (asdf:test-system :starintel-test)
                            (uiop:quit 0))
                          (error (e)
                            (format t \"~%Test error: ~a~%\" e)
                            (uiop:quit 1)))" \
                2>&1 | tee "$TMPDIR/test-output.log"

              test_exit_code="''${PIPESTATUS[0]}"
              if [ "$test_exit_code" -ne 0 ]; then
                exit "$test_exit_code"
              fi

              runHook postBuild
            '';

            installPhase = ''
              runHook preInstall

              mkdir -p "$out"
              cp "$TMPDIR/test-output.log" "$out/test-results.log"

              runHook postInstall
            '';
          };
        in
        {
          default = testCheck;
          starintel-tests = testCheck;
        });

      devShells = forAllSystems (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          packageSet = mkPackageSet pkgs;
        in
        {
          default = pkgs.mkShell {
            packages = [
              packageSet.sbcl-test-wrapped
              pkgs.pkg-config
            ];

            shellHook = ''
              echo "StarIntel v0.9.0 development environment ready"
              echo "Test with: nix flake check"
            '';
          };
        });

      overlays.default = final: _prev:
        let
          packageSet = mkPackageSet final;
        in
        {
          inherit (packageSet)
            cms-ulid
            starintel-v090
            starintel
            starintel-test
            ;
        };
    };
}
