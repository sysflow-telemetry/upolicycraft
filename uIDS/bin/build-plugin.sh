PLUGIN=primus_uids
plugin=bap_plugin_${PLUGIN}
TMPDIR=`mktemp -d`
cd $TMPDIR
touch $plugin.ml
bapbuild -package bap-plugin-${PLUGIN} $plugin.plugin
DESC=`ocamlfind query -format "%D" bap-plugin-${PLUGIN}`
CONS=`ocamlfind query -format "%(constraints)" bap-plugin-${PLUGIN}`
TAGS=`ocamlfind query -format "%(tags)" bap-plugin-${PLUGIN}`
if [ ! -z "$CONS" ]; then
bapbundle update -cons "$CONS" $plugin.plugin
fi
if [ ! -z "$TAGS" ]; then
bapbundle update -tags "$TAGS" $plugin.plugin
fi
bapbundle update -desc "$DESC" $plugin.plugin
bapbundle update -name ${PLUGIN} $plugin.plugin

bapbundle install $plugin.plugin
cd -
rm -rf $TMPDIR

