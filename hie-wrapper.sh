#!/usr/bin/env bash

DEBUG=1
indent=""
function debug {
  if [[ $DEBUG == 1 ]]; then
    echo "$indent$@" >> /tmp/hie-wrapper.log
  fi
}

curDir=`pwd`
debug "Launching HIE for project located at $curDir"
indent="  "

GHCBIN='ghc'
# If a .stack-work exists, assume we are using stack.
if [ -d ".stack-work" ]; then
  debug "Using stack GHC version"
  GHCBIN='stack ghc --'
else
  debug "Using plain GHC version"
fi
versionNumber=`$GHCBIN --version`
debug $versionNumber

HIEBIN='hie'
BACKUP_HIEBIN='hie'
# Match the version number with a HIE version, and provide a fallback without
# the patch number.
if [[ $versionNumber = *"8.0.1"* ]]; then
  debug "Project is using GHC 8.0.1"
  HIEBIN='hie-8.0.1'
  BACKUP_HIEBIN='hie-8.0'
elif [[ $versionNumber = *"8.0.2"* ]]; then
  debug "Project is using GHC 8.0.2"
  HIEBIN='hie-8.0.2'
  BACKUP_HIEBIN='hie-8.0'
elif [[ $versionNumber = *"8.0"* ]]; then
  debug "Project is using GHC 8.0.*"
  HIEBIN='hie-8.0'
elif [[ $versionNumber = *"8.2.1"* ]]; then
  debug "Project is using GHC 8.2.1"
  HIEBIN='hie-8.2.1'
  BACKUP_HIEBIN='hie-8.2'
elif [[ $versionNumber = *"8.2.2"* ]]; then
  debug "Project is using GHC 8.2.2"
  HIEBIN='hie-8.2.2'
  BACKUP_HIEBIN='hie-8.2'
elif [[ $versionNumber = *"8.2"* ]]; then
  debug "Project is using GHC 8.2.*"
  HIEBIN='hie-8.2'
else
  debug "WARNING: GHC version does not match any of the checked ones."
fi

if [ -x "$(command -v $HIEBIN)" ]; then
  debug "$HIEBIN was found on path"
elif [ -x "$(command -v $BACKUP_HIEBIN)" ]; then
  debug "Backup $BACKUP_HIEBIN was found on path"
  HIEBIN=$BACKUP_HIEBIN
else
  debug "Falling back to plain hie"
  HIEBIN='hie'
fi

$HIEBIN $@
