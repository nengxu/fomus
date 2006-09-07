#!/bin/sh

if [[ ! -e "fomus.asd" ]]
then
    echo "You must run this script from the FOMUS directory"
    exit 1
fi

for A in $*
do
  case $A in
      --uninstall)
	  UNINST=1
	  ;;
      --bindir=)
	  BINDIR=`echo $A | sed -e "s/^--bindir=//"`
	  ;;
      --libdir=)
	  LIBDIR=`echo $A | sed -e "s/^--libdir=//"`
	  ;;
      --sbcl)
	  LISP=sbcl
	  ;;
      --cmucl)
	  LISP=cmucl
	  ;;
      --openmcl)
	  LISP=openmcl
	  ;;
      --cm=*)
	  CMDIR=`echo $A | sed -e "s/^--cm=//"`
	  ;;
      --cmn=*)
	  CMNDIR=`echo $A | sed -e "s/^--cmn=//"`
	  ;;
      --prefix=*)
	  PREFIX=`echo $A | sed -e "s/^--prefix=//"`
	  ;;
      *)
	  echo
	  echo "This installs FOMUS as a command-line executable."
	  echo
	  echo "Usage Examples:"
	  echo "./install.sh --sbcl                                                     --install using SBCL into /usr/local (you need to be root)"
	  echo "./install.sh --sbcl --cm=/mylispdir/cm                                  --install using SBCL and include CM"
	  echo "./install.sh --sbcl --cmn=/mylispdir/cmn                                --install using SBCL and include CMN"
	  echo "./install.sh --cmucl --prefix=/mybasedir                                --install using CMUCL into /mybaseinstalldir"
	  echo "./install.sh --cmucl --prefix=/mybasedir --bindir=/mybasedir/mybin      --install using CMUCL with special bin directory"
	  echo "./install.sh --openmcl --prefix=/mybasedir --libdir=/mybasedir/mylib    --install using OpenMCL with special lib directory"
	  echo "./install.sh --uninstall                                                --uninstall from /usr/local"
	  echo "./install.sh --uninstall --prefix=/mybasedir                            --uninstall from /mybaseinstalldir"
	  echo "./install.sh --uninstall --prefix=/mybasedir --bindir=/mybasedir/mybin  --uninstall from /mybaseinstalldir and special bin directory"
	  echo
	  echo "Lisp options are --sbcl, --cmucl and --openmcl"
	  echo
	  echo "If you get stuck in Lisp while compiling, try the following command:"
	  echo "(cl-user::quit)"
	  exit 1
	  ;;
  esac
done

if [[ -z "$PREFIX" ]]
then
    PREFIX="/usr/local"
fi
if [[ -z "$BINDIR" ]]
then
    BINDIR="$PREFIX/bin"
fi
if [[ -z "$LIBDIR" ]]
then
    LIBDIR="$PREFIX/lib"
fi

if [[ "$UNINST" = "1" ]]
then
    echo
    echo "Uninstalling..."
    echo "rm -f $LIBDIR/fomus.img"
    rm -f $LIBDIR/fomus.img
    echo "rm -f $BINDIR/fomus"
    rm -f $BINDIR/fomus
    echo
    echo "Done!"
    exit 0
fi

if [[ -z "$LISP" ]]
then
    echo "No Lisp was specified (use --sbcl, --cmucl or --openmcl, or type --help for more options)"
    exit 1
fi

LOADCM="src/cm.lisp"
LOADCMN="cmn-all.lisp"

case $LISP in
    sbcl)
	LISPEXE=sbcl
	LOADARG="--load"
	EVALARG="--eval"
	EXITCMD="(quit)"
	COREARG="--core"
	EXTRAARG="--noinform"
	DUMPCMD='(sb-ext:save-lisp-and-die "fomus.img" :purify t)'
	;;
    cmucl)
	LISPEXE=lisp
	LOADARG="-load"
	EVALARG="-eval"
	EXITCMD="(quit)"
	COREARG="-core"
	EXTRAARG="-quiet"
	DUMPCMD='(ext:save-lisp "fomus.img" :purify t)'
	;;
    openmcl)
	LISPEXE=openmcl
	LOADARG="-l"
	EVALARG="-e"
	EXITCMD="(quit)"
	COREARG="-I"
	DUMPCMD='(ccl:save-application "fomus.img" :purify t)'
	;;
esac

if [[ -e fomus.img ]]
then
    rm fomus.img
fi

if [[ -n "$CMDIR" ]]
then
    $LISPEXE $EXTRAARG $LOADARG "$CMDIR/$LOADCM" $EVALARG $EXITCMD
    INCCM1=$LOADARG
    INCCM2="$CMDIR/$LOADCM"
fi
if [[ -n "$CMNDIR" ]]
then
    $LISPEXE $EXTRAARG $LOADARG "$CMNDIR/$LOADCMN" $EVALARG $EXITCMD
    INCCMN1=$LOADARG
    INCCMN2="$CMNDIR/$LOADCMN"
fi
INSTFLAG='(intern "INSTALL" :common-lisp-user)'
$LISPEXE $EXTRAARG $EVALARG "$INSTFLAG" $LOADARG "load.lisp" $EVALARG $EXITCMD

$LISPEXE $EXTRAARG $INCCM1 $INCCM2 $INCCMN1 $INCCMN2 $EVALARG "$INSTFLAG" $LOADARG "load.lisp" $EVALARG "$DUMPCMD"

if [[ ! -e "fomus.img" ]]
then
    echo
    echo "Couldn't create FOMUS Lisp image :("
    exit 1
fi

echo '#!/bin/sh' > fomus.sh
echo 'us="Usage: fomus [-lxfscmv] [-q val] [-o basefilename] filename"' >> fomus.sh
echo 'while getopts lxfscmvq:o: opt; do' >> fomus.sh
echo '    case $opt in' >> fomus.sh
echo '        l) o="l$o";;' >> fomus.sh
echo '        x) o="x$o";;' >> fomus.sh
echo '        f) o="f$o";;' >> fomus.sh
echo '        s) o="s$o";;' >> fomus.sh
echo '        c) o="c$o";;' >> fomus.sh
echo '        m) o="m$o";;' >> fomus.sh
echo '        v) o="v$o";;' >> fomus.sh
echo '        o) n="$OPTARG";;' >> fomus.sh
echo '        q) q="$OPTARG";;' >> fomus.sh
echo '        ?) echo $us; exit 2;;' >> fomus.sh
echo '    esac' >> fomus.sh
echo 'done' >> fomus.sh
echo 'shift $(($OPTIND - 1))' >> fomus.sh
echo 'if [[ $# -ne 1 ]]; then echo $us; exit 2; fi' >> fomus.sh
echo "$LISPEXE $COREARG \"$LIBDIR/fomus.img\" $EXTRAARG $EVALARG \"(fm::fomus-exe \\\"\$1\\\" \\\"\$HOME/.fomus\\\" \\\"\$o\\\" \\\"\$n\\\" \\\"\$q\\\")\"" >> fomus.sh

echo
echo "Installing..."
echo install -d $BINDIR
install -d $BINDIR
echo install -m 755 fomus.sh $BINDIR/fomus
install -m 755 fomus.sh $BINDIR/fomus
echo install -d $LIBDIR
install -d $LIBDIR
echo install -m 644 fomus.img $LIBDIR
install -m 644 fomus.img $LIBDIR

rm fomus.sh
rm fomus.img

echo
echo "Done!"
