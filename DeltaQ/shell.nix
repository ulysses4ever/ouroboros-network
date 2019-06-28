let
  jupyter = import (builtins.fetchGit {
    url = https://github.com/tweag/jupyterWith;
    rev = "";
    }) { };

  iPython = jupyter.kernels.iPythonWith {
    name = "python";
    packages = p: with p; [ numpy ];
  };

  iHaskell = jupyter.kernels.iHaskellWith {
    name = "haskell";
    packages = p: with p;
      let  dq1 = callCabal2nix "DeltaQIllustration"  ./packages/DeltaQIllustration {Chart=Chart;};
           dq2 = callCabal2nix "DeltaQIHaskell"      ./packages/DeltaQIHaskell {DeltaQIllustration = dq1;};
           Chart = (callHackageDirect {pkg    = "Chart";
                                       ver    = "1.9.1";     
                                       sha256 = "0aiq9r78yhma1xzhwl2xlzwqb2c59k6a4jjvdpninwm6lngac3s7";}) {};
      
           Chart-cairo = (callHackageDirect {pkg    = "Chart-cairo";
                                       ver    = "1.9.1";     
                                       sha256 = "0ambbkxbndjqxr0c4pysn87b3z0f8zpgvldj4qjbyazadna9k807";}) {Chart=Chart;};
          # ihaskell-charts needs some special treatment to get around broken packages
          ih-charts = callHackage "ihaskell-charts" "0.3.0.1" {Chart=Chart;Chart-cairo=Chart-cairo;};
      in [
    # the pretty renders 
    ihaskell-hatex ihaskell-diagrams ihaskell-graphviz ihaskell-aeson ihaskell-hvega
    ihaskell-widgets
    #  ihaskell-plot ihaskell-gnuplot  ihaskell-magic
    # current special case for Chart and its dependencies
    Chart ih-charts
    formatting hvega
    dq1
    dq2

   ];
  };

  jupyterEnvironment =
    jupyter.jupyterlabWith {
      kernels = [ iPython iHaskell ];
    };
in
  jupyterEnvironment.env