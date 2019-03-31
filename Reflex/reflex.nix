let rev = "4a95515b9b28a69e86af45bcc80339ee7efae8fc";
in import (builtins.fetchTarball {
  url = "https://github.com/reflex-frp/reflex-platform/archive/${rev}.tar.gz";
  sha256 = "01ackv0zsaf05zn8x4dkwhm9xsg8yy95k104c2nq1zcmmq8gkpxc";
}) {}
