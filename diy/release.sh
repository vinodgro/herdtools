set -e
DIR=`dirname $0`
REPOS=svn+ssh://secsvn@svn-rsem019.cl.cam.ac.uk/herdtools
. $DIR/version.sh
. $DIR/funs.sh
TMP=/var/tmp
FINAL=$TMP/diy-$V
/bin/rm -rf $FINAL && mkdir $FINAL
( cd $FINAL && \
  svn export -N $REPOS/litmus litmus && \
  svn export -N $REPOS/gen gen && \
  svn export -N $REPOS/herd herd && \
  svn export -N $REPOS/tools tools && \
  svn export -N $REPOS/lib lib && \
  svn export -N $REPOS/diy/Makefile.herd Makefile && \
  svn export -N $REPOS/diy/README.txt README.txt && \
  svn export -N $REPOS/diy/INSTALL.txt INSTALL.txt && \
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
