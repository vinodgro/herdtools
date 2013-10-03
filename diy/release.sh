set -e
DIR=`dirname $0`
REPOS=svn+ssh://secsvn@svn-sem.cl.cam.ac.uk/WeakMemory
. $DIR/version.sh
. $DIR/funs.sh
TMP=/var/tmp
EXPORT=$TMP/exp.$$
mkdir $EXPORT
( cd $EXPORT && \
  svn export -N $REPOS/mem.new && \
  mkdir -p litmus.new && \
  svn export -N $REPOS/litmus.new/plumbing litmus.new/plumbing && \
  svn export -N $REPOS/gen.new && \
  svn export -N $REPOS/diy )
FINAL=$TMP/diy-$V
/bin/rm -rf $FINAL && mkdir $FINAL
( cd $EXPORT && tar chf - litmus.new gen.new ) | \
( cd $FINAL && tar xf - ) && \
( cd $FINAL && mv gen.new gen && \
  mv litmus.new/plumbing litmus && rmdir litmus.new ) && \
( cd $EXPORT && cp diy/*.txt diy/Makefile $FINAL )
/bin/rm -rf $EXPORT
( cleandir $FINAL/litmus ) && ( cleandir $FINAL/gen )

# add example and documentation
sh $DIR/installdoc.sh $FINAL
NAME=`basename $FINAL`
( cd $TMP && tar cf - $NAME ) | gzip --best > $NAME.tar.gz
/bin/rm -rf $FINAL
