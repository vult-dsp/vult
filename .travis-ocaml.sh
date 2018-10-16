## basic OCaml and opam installation

full_apt_version () {
  package=$1
  version=$2
  case "${version}" in
      latest) echo -n "${package}" ;;
      *) echo -n "${package}="
         apt-cache show "$package" \
             | sed -n "s/^Version: \(${version}\)/\1/p" \
             | head -1
  esac
}

set -uex


# the ocaml version to test
OCAML_VERSION=${OCAML_VERSION:-latest}
OPAM_VERSION=${OPAM_VERSION:-2.0.0}
OPAM_INIT=${OPAM_INIT:-true}

if [ "${INSTALL_LOCAL+x}" = x ] ; then
  if [ "$TRAVIS_OS_NAME" = osx ] ; then
    echo INSTALL_LOCAL not permitted for macOS targets
    exit 1
  fi

  case ${OPAM_VERSION} in
      2.0.0)
          if [ "${OPAM_SWITCH:=ocaml-system}" != ocaml-system ] ; then
              echo "INSTALL_LOCAL requires OPAM_SWITCH=ocaml-system (or unset/null)"
              exit 1
          fi ;;
      *)
          if [ "${OPAM_SWITCH:=system}" != system ] ; then
              echo "INSTALL_LOCAL requires OPAM_SWITCH=system (or unset/null)"
              exit 1
          fi ;;
  esac
fi

# the base opam repository to use for bootstrapping and catch-all namespace
case $OPAM_VERSION in
    2.0.0) BASE_REMOTE=${BASE_REMOTE:-git://github.com/ocaml/opam-repository} ;;
    *) BASE_REMOTE=${BASE_REMOTE:-git://github.com/ocaml/opam-repository#1.2} ;;
esac

# whether we need a new gcc and binutils
UPDATE_GCC_BINUTILS=${UPDATE_GCC_BINUTILS:-"0"}

# Install Trusty remotes
UBUNTU_TRUSTY=${UBUNTU_TRUSTY:-"0"}

# Install XQuartz on OSX
INSTALL_XQUARTZ=${INSTALL_XQUARTZ:-"false"}

install_opam2 () {
    case $TRAVIS_OS_NAME in
        linux)
            sudo add-apt-repository --yes ppa:ansible/bubblewrap
            sudo apt-get update -qq
            sudo apt-get install -y bubblewrap
            sudo wget https://github.com/ocaml/opam/releases/download/2.0.0/opam-2.0.0-x86_64-linux -O /usr/local/bin/opam
            sudo chmod +x /usr/local/bin/opam ;;
        osx)
            sudo curl -sL https://github.com/ocaml/opam/releases/download/2.0.0/opam-2.0.0-x86_64-darwin -o /usr/local/bin/opam
            sudo chmod +x /usr/local/bin/opam ;;
    esac
}

install_ppa () {
  ppa=$1
  sudo add-apt-repository --yes ppa:${ppa}
  sudo apt-get update -qq
  if [ "${INSTALL_LOCAL:=0}" = 0 ] ; then
    sudo apt-get install -y \
       "$(full_apt_version ocaml $OCAML_VERSION)" \
       "$(full_apt_version ocaml-base $OCAML_VERSION)" \
       "$(full_apt_version ocaml-native-compilers $OCAML_VERSION)" \
       "$(full_apt_version ocaml-compiler-libs $OCAML_VERSION)" \
       "$(full_apt_version ocaml-interp $OCAML_VERSION)" \
       "$(full_apt_version ocaml-base-nox $OCAML_VERSION)" \
       "$(full_apt_version ocaml-nox $OCAML_VERSION)" \
       "$(full_apt_version camlp4 $OCAML_VERSION)" \
       "$(full_apt_version camlp4-extra $OCAML_VERSION)" \
       opam
  else
    sudo apt-get install -y opam
  fi
}

install_ocaml () {
    sudo apt-get install -y  \
         ocaml ocaml-base ocaml-native-compilers ocaml-compiler-libs \
         ocaml-interp ocaml-base-nox ocaml-nox \
         camlp4 camlp4-extra
}

install_on_linux () {
  case "$OCAML_VERSION,$OPAM_VERSION" in
    3.12,1.2.2)
        OCAML_VERSION=4.02
        OCAML_FULL_VERSION=3.12.1
        install_ppa avsm/ocaml42+opam12 ;;
    3.12,2.0.0)
        OCAML_VERSION=4.01
        OCAML_FULL_VERSION=3.12.1
        install_opam2 ;;
    4.00,1.2.2)
        OCAML_VERSION=4.02
        OCAML_FULL_VERSION=4.00.1
        install_ppa avsm/ocaml42+opam12 ;;
    4.00,2.0.0)
        OCAML_VERSION=4.01
        OCAML_FULL_VERSION=4.00.1
        install_opam2 ;;
    4.01,1.2.2)
        OCAML_VERSION=4.02
        OCAML_FULL_VERSION=4.01.0
        install_ppa avsm/ocaml42+opam12 ;;
    4.01,2.0.0)
        OCAML_FULL_VERSION=4.01.0
        OPAM_SWITCH=${OPAM_SWITCH:-ocaml-system}
        install_ocaml ;
        install_opam2 ;;
    4.02,1.1.2)
        OCAML_FULL_VERSION=4.02.3
        OPAM_SWITCH=${OPAM_SWITCH:-system}
        install_ppa avsm/ocaml42+opam11 ;;
    4.02,1.2.0)
        OCAML_FULL_VERSION=4.02.3
        OPAM_SWITCH=${OPAM_SWITCH:-system}
        install_ppa avsm/ocaml42+opam120 ;;
    4.02,1.2.1)
        OCAML_FULL_VERSION=4.02.3
        OPAM_SWITCH=${OPAM_SWITCH:-system}
        install_ppa avsm/ocaml42+opam121 ;;
    4.02,1.2.2)
        OCAML_FULL_VERSION=4.02.3
        OPAM_SWITCH=${OPAM_SWITCH:-system}
        install_ppa avsm/ocaml42+opam12 ;;
    4.02,2.0.0)
        OCAML_VERSION=4.01
        OCAML_FULL_VERSION=4.02.3
        install_opam2 ;;
    4.03,1.2.2)
        OCAML_VERSION=4.02
        OCAML_FULL_VERSION=4.03.0
        install_ppa avsm/ocaml42+opam12 ;;
    4.03,2.0.0)
        OCAML_VERSION=4.01
        OCAML_FULL_VERSION=4.03.0
        install_opam2 ;;
    4.04,1.2.2)
        OCAML_VERSION=4.02
        OCAML_FULL_VERSION=4.04.2
        install_ppa avsm/ocaml42+opam12 ;;
    4.04,2.0.0)
        OCAML_VERSION=4.01
        OCAML_FULL_VERSION=4.04.2
        install_opam2 ;;
    4.05,1.2.2)
        OCAML_VERSION=4.02
        OCAML_FULL_VERSION=4.05.0
        install_ppa avsm/ocaml42+opam12 ;;
    4.05,2.0.0)
        OCAML_VERSION=4.01
        OCAML_FULL_VERSION=4.05.0
        install_opam2 ;;
    4.06,1.2.2)
        OCAML_VERSION=4.02
        OCAML_FULL_VERSION=4.06.1
        install_ppa avsm/ocaml42+opam12 ;;
    4.06,2.0.0)
        OCAML_VERSION=4.01
        OCAML_FULL_VERSION=4.06.1
        install_opam2 ;;
    4.07,1.2.2)
        OCAML_VERSION=4.02
        OCAML_FULL_VERSION=4.07.0
        install_ppa avsm/ocaml42+opam12 ;;
    4.07,2.0.0)
        OCAML_VERSION=4.01
        OCAML_FULL_VERSION=4.07.0
        install_opam2 ;;
    *) echo "Unknown OCAML_VERSION=$OCAML_VERSION OPAM_VERSION=$OPAM_VERSION"
       echo "(An unset OCAML_VERSION used to default to \"latest\", but you must now specify it."
       echo "Try something like \"OCAML_VERSION=3.12\", \"OCAML_VERSION=4.07\", or see README-travis.md at https://github.com/ocaml/ocaml-ci-scripts )"
       exit 1 ;;
  esac

  TRUSTY="deb mirror://mirrors.ubuntu.com/mirrors.txt trusty main restricted universe"

  if [ "$UPDATE_GCC_BINUTILS" != "0" ] ; then
    echo "installing a recent gcc and binutils (mainly to get mirage-entropy-xen working!)"
    sudo add-apt-repository "${TRUSTY}"
    sudo add-apt-repository --yes ppa:ubuntu-toolchain-r/test
    sudo apt-get -qq update
    sudo apt-get install -y gcc-4.8
    sudo update-alternatives --install /usr/bin/gcc gcc /usr/bin/gcc-4.8 90
    sudo add-apt-repository -r "${TRUSTY}"
  fi

  if [ "$UBUNTU_TRUSTY" != "0" ] ; then
    echo "Adding Ubuntu Trusty mirrors"
    sudo add-apt-repository "${TRUSTY}"
    sudo apt-get -qq update
  fi

  if [ "${INSTALL_LOCAL:=0}" != 0 ] ; then
    echo -en "travis_fold:start:build.ocaml\r"
    echo "Building a local OCaml; this may take a few minutes..."
    wget "http://caml.inria.fr/pub/distrib/ocaml-${OCAML_FULL_VERSION%.*}/ocaml-$OCAML_FULL_VERSION.tar.gz"
    tar -xzf "ocaml-$OCAML_FULL_VERSION.tar.gz"
    cd "ocaml-$OCAML_FULL_VERSION"
    ./configure -prefix /usr/local ${OCAML_CONFIGURE_ARGS:=--with-debug-runtime}
    make world.opt
    sudo make install
    cd ..
    echo -en "travis_fold:end:build.ocaml\r"
  fi
}

install_on_osx () {
  case $INSTALL_XQUARTZ in
      true)
        curl -OL "http://xquartz.macosforge.org/downloads/SL/XQuartz-2.7.6.dmg"
        sudo hdiutil attach XQuartz-2.7.6.dmg
        sudo installer -verbose -pkg /Volumes/XQuartz-2.7.6/XQuartz.pkg -target /
        ;;
  esac
  brew update &> /dev/null
  brew upgrade python || true
  case "$OCAML_VERSION,$OPAM_VERSION" in
    3.12,1.2.2) OCAML_FULL_VERSION=3.12.1; brew install opam ;;
    3.12,2.0.0) OCAML_FULL_VERSION=3.12.1; install_opam2 ;;
    4.00,1.2.2) OCAML_FULL_VERSION=4.00.1; brew install opam ;;
    4.00,2.0.0) OCAML_FULL_VERSION=4.00.1; install_opam2 ;;
    4.01,1.2.2) OCAML_FULL_VERSION=4.01.0; brew install opam ;;
    4.01,2.0.0) OCAML_FULL_VERSION=4.01.0; install_opam2 ;;
    4.02,1.2.2) OCAML_FULL_VERSION=4.02.3; brew install opam ;;
    4.02,2.0.0) OCAML_FULL_VERSION=4.02.3; install_opam2 ;;
    4.03,1.2.2) OCAML_FULL_VERSION=4.03.0; brew install opam ;;
    4.03,2.0.0) OCAML_FULL_VERSION=4.03.0; install_opam2 ;;
    4.04,1.2.2) OCAML_FULL_VERSION=4.04.2; brew install opam ;;
    4.04,2.0.0) OCAML_FULL_VERSION=4.04.2; install_opam2 ;;
    4.05,1.2.2) OCAML_FULL_VERSION=4.05.0; brew install opam ;;
    4.05,2.0.0) OCAML_FULL_VERSION=4.05.0; install_opam2 ;;
    4.06,1.2.2) OCAML_FULL_VERSION=4.06.1; brew install opam ;;
    4.06,2.0.0) OCAML_FULL_VERSION=4.06.1; install_opam2 ;;
    4.07,1.2.2) OCAML_FULL_VERSION=4.07.0;
                OPAM_SWITCH=${OPAM_SWITCH:-system};
                brew install ocaml;
                brew install opam ;;
    4.07,2.0.0) OCAML_FULL_VERSION=4.07.0;
                OPAM_SWITCH=${OPAM_SWITCH:-ocaml-system};
                brew install ocaml;
                install_opam2 ;;
    *) echo "Unknown OCAML_VERSION=$OCAML_VERSION OPAM_VERSION=$OPAM_VERSION"
       exit 1 ;;
  esac
}

case $TRAVIS_OS_NAME in
    osx) install_on_osx ;;
    linux) install_on_linux ;;
esac

OPAM_SWITCH=${OPAM_SWITCH:-$OCAML_FULL_VERSION}

export OPAMYES=1

case $OPAM_INIT in
  true)
      opam init -a "$BASE_REMOTE" --comp="$OPAM_SWITCH"
      eval $(opam config env)
      ;;
esac

echo OCAML_VERSION=$OCAML_VERSION >  .travis-ocaml.env
echo OPAM_SWITCH=$OPAM_SWITCH     >> .travis-ocaml.env

if [ -x "$(command -v ocaml)" ]; then
    ocaml -version
else
    echo "OCaml is not yet installed"
fi

opam --version
opam --git-version