set -e
DIR=`dirname $0`
REPOS=svn+ssh://secsvn@svn-sem.cl.cam.ac.uk/WeakMemory
. version.herd.sh
. $DIR/funs.sh
TMP=/var/tmp
FINAL=$TMP/diy-$V
/bin/rm -rf $FINAL && mkdir $FINAL
( cd $FINAL && \
  svn export -N $REPOS/litmus.herd litmus && \
  svn export -N $REPOS/gen.herd gen && \
  svn export -N $REPOS/herd herd && \
  svn export -N $REPOS/tools tools && \
  svn export -N $REPOS/lib lib && \
  svn export -N $REPOS/diy/Makefile.herd Makefile && \
  svn export -N $REPOS/diy/README.herd.txt README.txt && \
  svn export -N $REPOS/diy/INSTALL.herd.txt INSTALL.txt && \
  svn export -N $REPOS/diy/LICENSE.txt  && \
  true
)
( cleandir $FINAL/litmus ) && \
( cleandir $FINAL/gen ) && \
( cleandir $FINAL/herd ) && \
( cleandir $FINAL/tools )

# add example and documentation
sh $DIR/installdoc.sh $FINAL
NAME=`basename $FINAL`
( cd $TMP && tar cf - $NAME ) | gzip --best > $NAME.tar.gz
/bin/rm -rf $FINAL
