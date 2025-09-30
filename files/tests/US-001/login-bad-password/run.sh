#!/bin/bash
export COB_LIBRARY_PATH=lib
export COB_FILE_PATH=$PWD
export RUN_DBNAME="pocbanking@127.0.0.1:5432"
export RUN_USERNAME="postgres"
export RUN_PASSWD="postgres"

. $PWD/scripts/dispatcher-utils

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ACTUAL_OUTPUT_FILE="${SCRIPT_DIR}/actual-output.txt"
rm -f "$ACTUAL_OUTPUT_FILE"

input_value="      POST    LOGIN     client1                                           client"
$(run_dispatcher_clean "$ACTUAL_OUTPUT_FILE" "$input_value" "")