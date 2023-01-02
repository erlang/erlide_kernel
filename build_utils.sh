#! /bin/bash -e

declare -A OTP_VSNS=( ["23"]="23.3.4.18" ["24"]="24.3.4.7" ["25"]="25.2" )

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

build_projects() {
    build_project erlide_common 23 "$@"
    build_project erlide_debugger_23 23 "$@"
    build_project erlide_debugger_24 24 "$@"
    build_project erlide_debugger_25 25 "$@"
    build_project erlide_debugger 23 "$@"
    build_project erlide_tools 23 "$@"

    build_project erlide_ide 23 "$@"
}

assemble_eclipse_plugin() {
    echo "Assemble eclipse plugin"
    mkdir -p org.erlide.kernel/common
    cp erlide_common/_build/default/lib/*/ebin/*.* org.erlide.kernel/common
    cp erlide_tools/_build/default/lib/*/ebin/*.* org.erlide.kernel/common

    mkdir -p org.erlide.kernel/ide
    cp erlide_ide/_build/default/lib/*/ebin/*.* org.erlide.kernel/ide

    mkdir -p org.erlide.kernel/debugger
    cp erlide_debugger/_build/default/lib/*/ebin/*.* org.erlide.kernel/debugger

    mkdir -p org.erlide.kernel/debugger/23
    cp erlide_debugger_23/_build/default/lib/*/ebin/*.* org.erlide.kernel/debugger/23
    mkdir -p org.erlide.kernel/debugger/24
    cp erlide_debugger_24/_build/default/lib/*/ebin/*.* org.erlide.kernel/debugger/24
    mkdir -p org.erlide.kernel/debugger/25
    cp erlide_debugger_25/_build/default/lib/*/ebin/*.* org.erlide.kernel/debugger/25

    cd org.erlide.kernel
    rm -f org.erlide.kernel_*.zip
    VSN=`get_plugin_vsn .`
    zip -r org.erlide.kernel_$VSN.zip * > /dev/null
    cd ..

    mkdir -p _build
    rm -f _build/org.erlide.kernel_*.zip
    mv org.erlide.kernel/org.erlide.kernel_*.zip _build
    echo "Created _build/org.erlide.kernel_$VSN.zip"
}

get_plugin_vsn() {
    x=`cat $1/META-INF/MANIFEST.MF | grep "Bundle-Version:" | cut -d " " -f 2`
    echo "${x%.qualifier}"
}

get_feature_vsn() {
    x=`cat $1/feature.xml | grep "version=" | head -n 2 | tail -n 1 | cut -d '"' -f 2`
    echo "${x%.qualifier}"
}

assemble_language_server() {
    echo "Assemble language_server"
    #cd
    VSN=`get_server_vsn`

    #echo "Created _build/erlide_server_$VSN.zip"
}

get_server_vsn() {
    # FIXME
    #x=`cat META-INF/MANIFEST.MF | grep "Bundle-Version:" | cut -d " " -f 2`
    #echo "${x%.qualifier}"
    echo "x.x.x"
}

