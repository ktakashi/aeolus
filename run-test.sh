#!/usr/bin/env bash

# Testing implementations
SAGITTARIUS=${SAGITTARIUS:-"sagittarius"}
GAUCHE=${GAUCHE:-"gosh"}
CHIBI=${CHIBI:-"chibi-scheme"}
LARCENY=${LARCENY:-"larceny"}

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
	*) error ">>> Given implementations is not supported: $system" ;;
    esac
done
