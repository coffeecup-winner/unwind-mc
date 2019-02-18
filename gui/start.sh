#! /bin/bash

CONFIG=${1-debug}

LD_LIBRARY_PATH=../lib/target/$CONFIG:$LD_LIBRARY_PATH npm start
