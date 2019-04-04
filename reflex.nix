let rev = "173643c312bc2a3032cfa9b9e47d1a7c3659500e";
in import (builtins.fetchTarball {
  url = "https://github.com/reflex-frp/reflex-platform/archive/${rev}.tar.gz";
  sha256 = "1b57j97zlv47cyh2pkyprr049p0zjqh5bvcshqb491w2l4xjyfbh";
}) {}
