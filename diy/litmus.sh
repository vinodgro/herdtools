set -e
DIR=`dirname $0`
REPOS=svn+ssh://secsvn@svn-sem.cl.cam.ac.uk/WeakMemory
TMP=/var/tmp
. $DIR/version.herd.sh
. $DIR/funs.sh
NAME=litmus-$V
EXPORT=$TMP/export.$$
FINAL=$EXPORT/$NAME
mkdir -p $EXPORT
( cd $EXPORT &&
  svn export -N $REPOS/litmus.herd $NAME && \
  svn export -N $REPOS/diy/LICENSE.txt && \
  ( cd $NAME && /bin/rm lib &&  svn export -N $REPOS/lib && svn export -N $REPOS/litmus.herd/generated ) && \
  true )
#TMPF=/tmp/$$.txt
#( cd $FINAL && sed -e 's|MCYCLES=.*|MCYCLES=|' Makefile > $TMPF && mv $TMPF Makefile )
( cleandir $FINAL )
( cd $EXPORT && tar zcf $NAME.tar.gz $NAME )
( mv $EXPORT/$NAME.tar.gz . && /bin/rm -rf $EXPORT )