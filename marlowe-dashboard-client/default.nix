{ pkgs, gitignore-nix, haskell, webCommon, webCommonMarlowe, buildPursPackage, buildNodeModules, filterNpm, plutus-pab, marlowe-app, marlowe-companion-app, marlowe-follow-app }:
let
  server-invoker = haskell.packages.marlowe-dashboard-server.components.exes.marlowe-dashboard-server;

  build-server-invoker = "$(nix-build --quiet --no-build-output ../default.nix -A plutus.haskell.packages.marlowe-dashboard-server.components.exes.marlowe-dashboard-server)";

  # output containing the purescript bridge code
  generated-purescript = pkgs.runCommand "marlowe-dashboard-purescript" { } ''
    mkdir $out
    ${server-invoker}/bin/marlowe-dashboard-server psgenerator $out
    ${plutus-pab.server-invoker}/bin/plutus-pab psgenerator $out
  '';

  # generate-purescript: script to create purescript bridge code
  generate-purescript = pkgs.writeShellScriptBin "marlowe-dashboard-generate-purs" ''
    rm -rf ./generated
    ${build-server-invoker}/bin/marlowe-dashboard-server psgenerator generated
    ${plutus-pab.build-server-invoker}/bin/plutus-pab psgenerator generated
  '';

  # start-backend: script to start the plutus-pab server
  # note this should be changed soon - see the README
  start-backend = pkgs.writeShellScriptBin "marlowe-pab-server" ''
    echo "marlowe-pab-server: for development use only"
    ${plutus-pab.server-invoker}/bin/plutus-pab --config=marlowe-pab.yaml -m all-servers
  '';

  cleanSrc = gitignore-nix.gitignoreSource ./.;

  nodeModules = buildNodeModules {
    projectDir = filterNpm cleanSrc;
    packageJson = ./package.json;
    packageLockJson = ./package-lock.json;
    githubSourceHashMap = { };
  };

  contractsJSON = pkgs.writeTextDir "contracts.json" (builtins.toJSON {
    marlowe = "${marlowe-app}/bin/marlowe-app";
    walletCompanion = "${marlowe-companion-app}/bin/marlowe-companion-app";
    walletFollower = "${marlowe-follow-app}/bin/marlowe-follow-app";
  });

  client = buildPursPackage {
    inherit pkgs nodeModules;
    src = cleanSrc;
    checkPhase = ''
      node -e 'require("./output/Test.Main").main()'
    '';
    name = "marlowe-dashboard-client";
    extraSrcs = {
      web-common = webCommon;
      web-common-marlowe = webCommonMarlowe;
      generated = plutus-pab.generated-purescript;
      contracts = contractsJSON;
    };
    packages = pkgs.callPackage ./packages.nix { };
    spagoPackages = pkgs.callPackage ./spago-packages.nix { };
  };

  install-marlowe-contracts = pkgs.writeShellScriptBin "install-marlowe-contracts" ''
    ${plutus-pab.server-invoker}/bin/plutus-pab contracts install --path ${marlowe-app}/bin/marlowe-app
    ${plutus-pab.server-invoker}/bin/plutus-pab contracts install --path ${marlowe-companion-app}/bin/marlowe-companion-app
    ${plutus-pab.server-invoker}/bin/plutus-pab contracts install --path ${marlowe-follow-app}/bin/marlowe-follow-app
  '';
in
{
  inherit client contractsJSON generate-purescript install-marlowe-contracts start-backend;
}
