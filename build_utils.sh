#! /bin/bash -e

declare -A OTP_VSNS=( ["25"]="25.3.2.21" ["26"]="26.2.5.12" ["27"]="27.3.3" )

build_project() {
    REBAR=$1
    PRJ=$2
    VSN=$3
    shift
    shift
    shift

    echo ""
    echo "Building --$PRJ-- with OTP ${OTP_VSNS[$VSN]}..."
    rm -rf ebin
    rm -rf apps/*/ebin
    ~/erlide_tools/${OTP_VSNS[$VSN]}/bin/escript $REBAR "$@"
}

get_feature_vsn() {
    x=`cat $1/feature.xml | grep "version=" | head -n 2 | tail -n 1 | cut -d '"' -f 2`
    echo "${x%.qualifier}"
}
