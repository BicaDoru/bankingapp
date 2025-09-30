#!/bin/bash
export COB_LIBRARY_PATH=lib
export COB_FILE_PATH=$PWD
export RUN_DBNAME="pocbanking@127.0.0.1:5432"
export RUN_USERNAME="postgres"
export RUN_PASSWD="postgres"
bin/DISPATCHER "      GET     CUSTOMERS"