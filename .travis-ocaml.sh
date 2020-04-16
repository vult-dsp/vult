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


OCAML_VERSION=${OCAML_VERSION:-latest}
SYS_OCAML_VERSION=4.05
# Default opam is the latest release of opam 2
OPAM_VERSION=${OPAM_VERSION:-2}
OPAM_INIT=${OPAM_INIT:-true}
OCAML_BETA=${OCAML_BETA:-disable}

OPAM_LATEST_RELEASE=2.0.6

case $OPAM_VERSION in
    2|2.0) OPAM_VERSION=$OPAM_LATEST_RELEASE;;
    1.*) echo "Opam version '$OPAM_VERSION' is not supported"; exit 1;;
esac

if [ "$TRAVIS_OS_NAME" = "osx" ] ; then
    brew update &> /dev/null
    BREW_OPAM_VERSION=$(brew info opam --json=v1 | sed -e 's/.*"versions":{[^}]*"stable":"//' -e 's/".*//')
    if [ "$OPAM_VERSION" != "$BREW_OPAM_VERSION" ] ; then
        set +x
        echo -e "[\e[0;31mWARNING\e[0m] Ignored OPAM_VERSION=$OPAM_VERSION; interpreted as \"$BREW_OPAM_VERSION\"" >&2
        echo -e "[\e[0;31mWARNING\e[0m] opam 2 is installed via Homebrew" >&2
        set -x
    fi
    OPAM_VERSION="$BREW_OPAM_VERSION"
fi

if [ "$OPAM_VERSION" != "$OPAM_LATEST_RELEASE" ] ; then
    set +x
    echo -e "[\e[0;31mWARNING\e[0m] Out-of-date opam $OPAM_VERSION requested" >&2
    echo -e "[\e[0;31mWARNING\e[0m] Latest release is $OPAM_LATEST_RELEASE" >&2
    set -x
fi

if [ "${INSTALL_LOCAL+x}" = x ] ; then
  if [ "$TRAVIS_OS_NAME" = osx ] ; then
    echo INSTALL_LOCAL not permitted for macOS targets
    exit 1
  fi

  if [ "${OPAM_SWITCH:=ocaml-system}" != ocaml-system ] ; then
    echo "INSTALL_LOCAL requires OPAM_SWITCH=ocaml-system (or unset/null)"
    exit 1
  fi
fi

# the base opam repository to use for bootstrapping and catch-all namespace
BASE_REMOTE=${BASE_REMOTE:-git://github.com/ocaml/opam-repository}

# whether we need a new gcc and binutils
UPDATE_GCC_BINUTILS=${UPDATE_GCC_BINUTILS:-"0"}

# Install Xenial remotes
UBUNTU_XENIAL=${UBUNTU_XENIAL:-"0"}

# Install XQuartz on OSX
INSTALL_XQUARTZ=${INSTALL_XQUARTZ:-"false"}

APT_UPDATED=0

add_ppa () {
    if [ "$TRAVIS_OS_NAME" = "linux" ] ; then
        APT_UPDATED=0
        sudo add-apt-repository --yes ppa:$1
    fi
}

apt_install () {
    if [ "$TRAVIS_OS_NAME" = "linux" ] ; then
        if [ "$APT_UPDATED" -eq 0 ] ; then
            APT_UPDATED=1
            sudo apt-get update -qq
        fi
        sudo apt-get install --no-install-recommends -y "$@"
    fi
}

install_ocaml () {
    apt_install \
         ocaml ocaml-base ocaml-native-compilers ocaml-compiler-libs \
         ocaml-interp ocaml-base-nox ocaml-nox
}

install_opam2 () {
    case $TRAVIS_OS_NAME in
        linux)
            case $TRAVIS_DIST in
                precise|trusty|xenial)
                    add_ppa ansible/bubblewrap ;;
            esac
            if [ "${INSTALL_LOCAL:=0}" = 0 ] ; then
                install_ocaml
            fi
            apt_install bubblewrap
            sudo wget https://github.com/ocaml/opam/releases/download/$OPAM_VERSION/opam-$OPAM_VERSION-x86_64-linux -O /usr/local/bin/opam
            sudo chmod +x /usr/local/bin/opam ;;
        osx)
            if [ "${INSTALL_LOCAL:=0}" = 0 ] ; then
                brew install ocaml
            fi
            sudo curl -fsSL https://github.com/ocaml/opam/releases/download/$OPAM_VERSION/opam-$OPAM_VERSION-x86_64-macos -o /usr/local/bin/opam
            sudo chmod +x /usr/local/bin/opam ;;
    esac
}

install_ppa () {
  add_ppa $1
  if [ "${INSTALL_LOCAL:=0}" = 0 ] ; then
    sudo apt-get -qq update
    APT_UPDATED=1
    apt_install \
       "$(full_apt_version ocaml $SYS_OCAML_VERSION)" \
       "$(full_apt_version ocaml-base $SYS_OCAML_VERSION)" \
       "$(full_apt_version ocaml-native-compilers $SYS_OCAML_VERSION)" \
       "$(full_apt_version ocaml-compiler-libs $SYS_OCAML_VERSION)" \
       "$(full_apt_version ocaml-interp $SYS_OCAML_VERSION)" \
       "$(full_apt_version ocaml-base-nox $SYS_OCAML_VERSION)" \
       "$(full_apt_version ocaml-nox $SYS_OCAML_VERSION)"
  fi
  apt_install opam
}

install_on_linux () {
  case "$OCAML_VERSION" in
    3.12) OCAML_FULL_VERSION=3.12.1; install_opam2 ;;
    4.00) OCAML_FULL_VERSION=4.00.1; install_opam2 ;;
    4.01) OCAML_FULL_VERSION=4.01.0; install_opam2 ;;
    4.02) OCAML_FULL_VERSION=4.02.3; install_opam2 ;;
    4.03) OCAML_FULL_VERSION=4.03.0; install_opam2 ;;
    4.04) OCAML_FULL_VERSION=4.04.2; install_opam2 ;;
    4.05) OCAML_FULL_VERSION=4.05.0; install_opam2 ;;
    4.06) OCAML_FULL_VERSION=4.06.1; install_opam2 ;;
    4.07) OCAML_FULL_VERSION=4.07.1; install_opam2 ;;
    4.08) OCAML_FULL_VERSION=4.08.1; install_opam2 ;;
    4.09) OCAML_FULL_VERSION=4.09.1; install_opam2 ;;
    4.10) OCAML_FULL_VERSION=4.10.0; install_opam2 ;;
    *)
        if [ "$OCAML_BETA" != "enable" ]; then
            echo "Unknown OCAML_VERSION=$OCAML_VERSION"
            echo "(An unset OCAML_VERSION used to default to \"latest\", but you must now specify it."
            echo "Try something like \"OCAML_VERSION=3.12\", \"OCAML_VERSION=4.10\", or see README-travis.md at https://github.com/ocaml/ocaml-ci-scripts )"
            exit 1
        fi
        OCAML_FULL_VERSION="${OCAML_VERSION}"
        install_opam2 ;;
  esac

  XENIAL="deb mirror://mirrors.ubuntu.com/mirrors.txt xenial main restricted universe"

  if [ "$UPDATE_GCC_BINUTILS" != "0" ] ; then
    echo "installing a recent gcc and binutils (mainly to get mirage-entropy-xen working!)"
    sudo add-apt-repository "${XENIAL}"
    sudo add-apt-repository --yes ppa:ubuntu-toolchain-r/test
    sudo apt-get -qq update
    sudo apt-get install -y gcc-5
    sudo update-alternatives --install /usr/bin/gcc gcc /usr/bin/gcc-5 90
    sudo add-apt-repository -r "${XENIAL}"
  fi

  if [ "$UBUNTU_XENIAL" != "0" ] ; then
    echo "Adding Ubuntu Xenial mirrors"
    sudo add-apt-repository "${XENIAL}"
    sudo apt-get -qq update
    APT_UPDATED=1
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
  case "$OCAML_VERSION" in
    3.12) OCAML_FULL_VERSION=3.12.1; install_opam2 ;;
    4.00) OCAML_FULL_VERSION=4.00.1; install_opam2 ;;
    4.01) OCAML_FULL_VERSION=4.01.0; install_opam2 ;;
    4.02) OCAML_FULL_VERSION=4.02.3; install_opam2 ;;
    4.03) OCAML_FULL_VERSION=4.03.0; install_opam2 ;;
    4.04) OCAML_FULL_VERSION=4.04.2; install_opam2 ;;
    4.05) OCAML_FULL_VERSION=4.05.0; install_opam2 ;;
    4.06) OCAML_FULL_VERSION=4.06.1; install_opam2 ;;
    4.07) OCAML_FULL_VERSION=4.07.1; install_opam2 ;;
    4.08) OCAML_FULL_VERSION=4.08.1; install_opam2 ;;
    4.09) OCAML_FULL_VERSION=4.09.0;
          OPAM_SWITCH=${OPAM_SWITCH:-ocaml-system};
          brew install ocaml;
          install_opam2 ;;
    4.10) OCAML_FULL_VERSION=4.10.0; install_opam2 ;;
    *)
        if [ "$OCAML_BETA" != "enable" ]; then
            echo "Unknown OCAML_VERSION=$OCAML_VERSION"
            exit 1
        fi
        OCAML_FULL_VERSION="${OCAML_VERSION}"
        install_opam2 ;;
  esac
}

case $TRAVIS_OS_NAME in
    osx) install_on_osx ;;
    linux) install_on_linux ;;
esac

ocaml_package=ocaml-base-compiler
if [ "$OCAML_BETA" = "enable" ]; then
    ocaml_package=ocaml-variants
fi

OPAM_SWITCH=${OPAM_SWITCH:-$ocaml_package.$OCAML_FULL_VERSION}

export OPAMYES=1

case $OPAM_INIT in
  true)
      opam init -a --bare "$BASE_REMOTE"
      opam_repo_selection=
      if [ "$OCAML_BETA" = "enable" ]; then
          opam repo add --dont-select beta git://github.com/ocaml/ocaml-beta-repository.git
          opam_repo_selection="--repo=default,beta"
      fi
      opam switch "$OPAM_SWITCH" || opam switch create $opam_repo_selection "$OPAM_SWITCH"
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
