#!/bin/bash

export RUN_DBNAME="pocbanking@127.0.0.1:5432"
export RUN_USERNAME="postgres"
export RUN_PASSWD="postgres"
export COB_LIBRARY_PATH=./lib
bin/DISPATCHER "00001 PUT     ACCOUNT   00001 0000002575"
