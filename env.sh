#!/bin/sh -e

#
# add_dir_component_to_var
#
# Function for adding a dir comp to a path list env var as
# PATH, LD_LIBRARY_PATH, or MANPATH only if necessary.
# This function takes care to add colon if necesssary and
# manage empty vars.
# If the var is empty, only add the dir comp. If the env var
# only contains one comp, add a colon to separate the new
# dir comp from the other already availables comps.
#
# args:
#  1: directory component to add to a path list env var
#  2: env var name to which to add the dir comp
#
function add_dir_component_to_var {
    dir_comp=$1
    env_var=$2
    new_val=$dir_comp$(echo "\${$env_var:+:}\$$env_var")
    grep_cmd="echo \$$env_var | grep -q $dir_comp"
    if ! $(eval $grep_cmd); then
	eval $env_var=$new_val
    fi
}

#
# dell_dir_component_to_var
#
# Function for removing a dir comp to a path list env var as
# PATH, LD_LIBRARY_PATH, or MANPATH.
# This function takes care of removing leading colon if some
# remains.
#
# args:
#  1: directory component to add to a path list env var
#  2: env var name to which to add the dir comp
#
function del_dir_component_to_var {
    dir_comp=$1
    env_var=$2
    new_val="$env_var=\$(echo \$${env_var} | sed -e \"s|$dir_comp||g\")"
    eval $(echo $new_val)
    new_val="$env_var=\$(echo \$${env_var} | sed -e \"s|::|:|g\")"
    eval $(echo $new_val)
    new_val="$env_var=\$(echo \$${env_var} | sed -e \"s|^:||g\")"
    eval $(echo $new_val)
}

## If no args or 'add', add dir comps
if test -z "$1" -o "x$1" == "xadd"; then
    action=add
else
    # If arg 'del', delete dir comp
    if test "x$1" == "xdel"; then
	action=del
    fi
fi

##
## PATH
##

# Add local bin to path
eval ${action}_dir_component_to_var /usr/local/sbin PATH
# Add local sbin to path
eval ${action}_dir_component_to_var /usr/local/bin PATH

##
## LD_LIBRARY_PATH
##

# Add local lib to ld library path
eval ${action}_dir_component_to_var /usr/local/lib LD_LIBRARY_PATH

##
## LD_RUN_PATH
##

# Add local lib to ld run path
eval ${action}_dir_component_to_var /usr/local/lib LD_RUN_PATH

##
## PKG_CONFIG_PATH
##

# Add lib pkgconfig dir to pkg config path
eval ${action}_dir_component_to_var /usr/lib/pkgconfig PKG_CONFIG_PATH
# Add local lib pkgconfig dir to pkg config path
eval ${action}_dir_component_to_var /usr/local/lib/pkgconfig PKG_CONFIG_PATH
# Add local x86_64 lib pkgconfig dir to pkg config path
eval ${action}_dir_component_to_var /usr/lib/x86_64-linux-gnu/pkgconfig PKG_CONFIG_PATH

##
## MANPATH
##

# Add local man fr_FR.UTF-8 to man path
eval ${action}_dir_component_to_var /usr/local/share/man/fr_FR.UTF-8 MANPATH
# Add local man fr.UTF-8 to man path
eval ${action}_dir_component_to_var /usr/local/share/man/fr.UTF-8 MANPATH
# Add local man fr_FR.ISO8859-1 to man path
eval ${action}_dir_component_to_var /usr/local/share/man/fr_FR.ISO8859-1 MANPATH
# Add local man fr.ISO8859-1 to man path
eval ${action}_dir_component_to_var /usr/local/share/man/fr.ISO8859-1 MANPATH
# Add local man to man path
eval ${action}_dir_component_to_var /usr/local/share/man MANPATH

echo "==================================================================================="
echo "PATH=$PATH"
echo "LD_LIBRARY_PATH=$LD_LIBRARY_PATH"
echo "LD_RUN_PATH=$LD_RUN_PATH"
echo "PKG_CONFIG_PATH=$PKG_CONFIG_PATH"
echo "MANPATH=$MANPATH"
echo "==================================================================================="

# Export needed env vars
export PKG_CONFIG_PATH LD_LIBRARY_PATH LD_RUN_PATH MANPATH
