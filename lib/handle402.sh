DIR=$(dirname $0)

if [ $(ocaml $DIR/check402.ml) = ok ]; then
    rm -f $DIR/bytes.ml $DIR/bytes.mli
else
    cp $DIR/_bytes.ml $DIR/bytes.ml
    cp $DIR/_bytes.mli $DIR/bytes.mli
fi
