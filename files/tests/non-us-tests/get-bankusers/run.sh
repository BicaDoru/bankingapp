#!/bin/bash

export RUN_DBNAME="pocbanking@127.0.0.1:5432"
export RUN_USERNAME="postgres"
export RUN_PASSWD="postgres"
export COB_LIBRARY_PATH=./lib

. $PWD/scripts/dispatcher-utils

SQL_INSERT_DATA_PATH1="$PWD/files/tests/non-us-tests/get-bankusers/populate-busers1.sql"
SQL_INSERT_DATA_PATH2="$PWD/files/tests/non-us-tests/get-bankusers/populate-busers2.sql"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ACTUAL_OUTPUT_FILE="${SCRIPT_DIR}/actual-output.txt"
rm -f "$ACTUAL_OUTPUT_FILE"

DB_NAME="pocbanking"
PG_USER="postgres"

# --------------------------------------- Enough accounts for second page ------------------------------------------------------------
sudo -u "$PG_USER" psql -d "$DB_NAME" -f "$SQL_INSERT_DATA_PATH1"


# --------------------------------------- Second page, 1 user ------------------------------------------------------------
input_value="00001 GET     USERS     00002"
$(run_dispatcher_clean "$ACTUAL_OUTPUT_FILE" "$input_value")
./scripts/sql/reset-db || { echo "DB reset failed"; exit 1; }

# --------------------------------------- First page ------------------------------------------------------------
input_value="00001 GET     USERS     00001"
$(run_dispatcher_clean "$ACTUAL_OUTPUT_FILE" "$input_value")
./scripts/sql/reset-db || { echo "DB reset failed"; exit 1; }

# --------------------------------------- First page by default ------------------------------------------------------------
sudo -u "$PG_USER" psql -d "$DB_NAME" -f "$SQL_INSERT_DATA_PATH1"
input_value="00001 GET     USERS"
$(run_dispatcher_clean "$ACTUAL_OUTPUT_FILE" "$input_value")
./scripts/sql/reset-db || { echo "DB reset failed"; exit 1; }

# --------------------------------------- Second page, 3 users ------------------------------------------------------------
sudo -u "$PG_USER" psql -d "$DB_NAME" -f "$SQL_INSERT_DATA_PATH2"
input_value="00001 GET     USERS     00002"
$(run_dispatcher_clean "$ACTUAL_OUTPUT_FILE" "$input_value")
./scripts/sql/reset-db || { echo "DB reset failed"; exit 1; }

# --------------------------------------- Second page, no users ------------------------------------------------------------
input_value="00001 GET     USERS     00002"
$(run_dispatcher_clean "$ACTUAL_OUTPUT_FILE" "$input_value")
./scripts/sql/reset-db || { echo "DB reset failed"; exit 1; }

# --------------------------------------- Permission denied: teller ------------------------------------------------------------
input_value="00002 GET     USERS     00001"
$(run_dispatcher_clean "$ACTUAL_OUTPUT_FILE" "$input_value")
./scripts/sql/reset-db || { echo "DB reset failed"; exit 1; }

# --------------------------------------- Permission denied: client ------------------------------------------------------------
input_value="00005 GET     USERS     00001"
$(run_dispatcher_clean "$ACTUAL_OUTPUT_FILE" "$input_value")
./scripts/sql/reset-db || { echo "DB reset failed"; exit 1; }

