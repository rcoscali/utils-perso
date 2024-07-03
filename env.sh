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
# At last the env var is exported.
#
# Args:
#  $1: directory component to add to a path list env var
#  $2: env var name to which to add the dir comp
#
# Returns:
#  Nothing
#
function add_dir_component_to_var {
    dir_comp=$1
    env_var=$2
    new_val=$dir_comp$(echo "\${$env_var:+:}\$$env_var")
    grep_cmd="echo \$$env_var | grep -q :$dir_comp:"
    if ! $(eval $grep_cmd); then
	eval $env_var=$new_val
    fi
    eval export $env_var
}

#
# del_dir_component_from_var
#
# Function for removing a dir comp from a path list env var as
# PATH, LD_LIBRARY_PATH, or MANPATH.
# If the env var is not empty, it process the env var.
# This function takes care of removing leading colon if some
# remains.
# At last, if empty, the environment variable is unset.
#
# Args:
#  $1: directory component to add to a path list env var
#  $2: env var name to which to add the dir comp
#
# Returns:
#  Nothing
#
function del_dir_component_from_var {
    dir_comp=$1
    env_var=$2
    if $(test -n "$env_var"); then
	new_val="$env_var=\$(echo \$${env_var} | sed -e \"s|$dir_comp||g\" -e \"s|::|:|g\" -e \"s|^:||g\")"
	eval $(echo $new_val)
	if $(test -z $env_var); then
	    unset $(echo $env_var)
	fi
    fi
}

## If no args or 'add', add dir comps
if test -z "$1" -o "x$1" == "xadd"; then
    action=add_dir_component_to_var
else
    # If arg 'del', delete dir comp
    if test "x$1" == "xdel"; then
	action=del_dir_component_from_var
    fi
fi

##
## PATH
##

# Add local bin to path
eval ${action} /usr/local/sbin PATH
# Add local sbin to path
eval ${action} /usr/local/bin PATH

##
## LD_LIBRARY_PATH
##

# Add local lib to ld library path
eval ${action} /usr/local/lib LD_LIBRARY_PATH

##
## LD_RUN_PATH
##

# Add local lib to ld run path
eval ${action} /usr/local/lib LD_RUN_PATH

##
## PKG_CONFIG_PATH
##

# Add usr lib pkgconfig dir to pkg config path
eval ${action} /usr/lib/pkgconfig PKG_CONFIG_PATH
# Add usr lib x86_64 pkgconfig dir to pkg config path
eval ${action} /usr/lib/x86_64-linux-gnu/pkgconfig PKG_CONFIG_PATH
# Add usr local lib pkgconfig dir to pkg config path
eval ${action} /usr/local/lib/pkgconfig PKG_CONFIG_PATH

##
## MANPATH
##

# Add usr local share man fr_FR.UTF-8 to man path
eval ${action} /usr/local/share/man/fr_FR.UTF-8 MANPATH
# Add usr local share man fr.UTF-8 to man path
eval ${action} /usr/local/share/man/fr.UTF-8 MANPATH
# Add usr local share man fr_FR.ISO8859-1 to man path
eval ${action} /usr/local/share/man/fr_FR.ISO8859-1 MANPATH
# Add usr local share man fr.ISO8859-1 to man path
eval ${action} /usr/local/share/man/fr.ISO8859-1 MANPATH
# Add usr local share man to man path
eval ${action} /usr/local/share/man MANPATH

test -z $LD_LIBRARY_PATH && unset LD_LIBRARY_PATH
test -z $LD_RUN_PATH && unset LD_RUN_PATH
test -z $PKG_CONFIG_PATH && unset PKG_CONFIG_PATH
test -z $MANPATH && unset MANPATH

echo "==================================================================================="
echo "PATH=$PATH"
env | egrep ^LD_LIBRARY_PATH
env | egrep ^LD_RUN_PATH
env | egrep ^PKG_CONFIG_PATH
env | egrep ^MANPATH
echo "==================================================================================="
