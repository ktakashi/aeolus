#!/usr/bin/env bash

# Testing implementations
SAGITTARIUS=${SAGITTARIUS:-"sagittarius"}
GAUCHE=${GAUCHE:-"gosh"}
CHIBI=${CHIBI:-"chibi-scheme"}
LARCENY=${LARCENY:-"larceny"}
PICRIN=${PICRIN:-"picrin"}

run_tests()
{
    name=$1
    log="$name-test.log"
    shift
    echo `date` > "$log"
    sleep 1
    {
	for f in tests/*; do
	    echo "$name $@ $f"
	    "$name" $@ "$f"
	done
    } 2>&1 | tee -a "$log" 
}

add_load()
{
    dir=$1
    load=$2
    for f in $dir; do
	echo "Adding $f"
	echo "(load \"$f\")" >> "$load"
    done
}

prepare_pircin()
{
    # there is no option to add library search path on picrin
    # so use load
    echo "Preparing load file"
    load=picrin_load.scm
    if [ -f $load ]; then 
	rm $load
    fi
    echo '(import (scheme base) (scheme load) (scheme write))' > "$load"
    add_load 'lib/aeolus/misc/*.sld' $load
    add_load 'lib/aeolus/cipher/*.sld' $load
    add_load 'lib/aeolus/modes/*.sld' $load
    add_load 'lib/aeolus/*.sld' $load

    # now testing
    add_load 'test-lib/*.sld' $load

    echo '(display "loaded") (newline)' >> $load
    log="pircin-test.log"
    sleep 1
    {
	for f in tests/*; do
	    testfile="$f.picrin.scm"
	    cat "$load" > "$testfile"
	    cat "$f" >> "$testfile"
	    echo "$PICRIN $testfile"
	    "$PICRIN" "$testfile"
	    rm "$testfile"
	done
    } 2>&1 | tee -a "$log" 
    # rm "$load"
}

error()
{
    echo $1
    echo "$0 systems ..."
}

if [ "$#" -lt 1 ]; then
    systems="$SAGITTARIUS $GAUCHE $CHIBI $LARCENY"
else
    systems="$@"
fi

for system in $systems; do
    echo "Testing $system"
    case "$system" in
	sagittarius) run_tests $SAGITTARIUS "-r7" "-Llib" "-Ltest-lib" ;;
	gosh) run_tests $GAUCHE "-r7" "-Ilib" "-Itest-lib" ;;
	chibi-scheme) run_tests $CHIBI "-Ilib" "-Itest-lib" ;;
	larceny) 
	    run_tests $LARCENY "-r7rs" "-path" "lib:test-lib" "-program" ;;
	picrin) prepare_pircin ;;
	*) error ">>> Given implementations is not supported: $system" ;;
    esac
done
