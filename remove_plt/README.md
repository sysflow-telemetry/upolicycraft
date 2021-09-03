PLT Remover Pass
================

This analysis pass attempts to rewrite binaries so that they do not
call out to the ".plt" function which we observed halts micro-execution
in a recent version of BAP.

Build:

    bapbuild remove_plt.plugin
    bapbundle install remove_plt.plugin

