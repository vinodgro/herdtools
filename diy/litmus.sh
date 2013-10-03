set -e
DIR=`dirname $0`
REPOS=svn+ssh://secsvn@svn-sem.cl.cam.ac.uk/WeakMemory
TMP=/var/tmp
. $DIR/version.sh
. $DIR/funs.sh
EXPORT=$TMP/exp.$$
mkdir $EXPORT
( cd $EXPORT && svn export -N $REPOS/mem.new && \
  svn export -N $REPOS/litmus.new/plumbing && \
  mkdir litmus.new && mv plumbing litmus.new )
NAME=litmus-$V
FINAL=$TMP/$NAME
/bin/rm -rf $FINAL && mkdir $FINAL
( cd $EXPORT/litmus.new/plumbing && tar chf - . ) | \
( cd $FINAL && tar xmf - . ) && \
/bin/rm -rf $EXPORT
TMPF=/tmp/$$.txt
( cd $FINAL && sed -e 's|MCYCLES=.*|MCYCLES=|' Makefile > $TMPF && mv $TMPF Makefile )
( cleandir $FINAL )

( cd $TMP && tar zcf $NAME.tar.gz $NAME )
( mv $TMP/$NAME.tar.gz . && /bin/rm -rf $FINAL )