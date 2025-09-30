#!/bin/bash

export RUN_DBNAME="pocbanking@127.0.0.1:5432"
export RUN_USERNAME="postgres"
export RUN_PASSWD="postgres"
export COB_LIBRARY_PATH=./lib

DB_NAME="pocbanking"
PG_USER="postgres"

. $PWD/scripts/dispatcher-utils

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ACTUAL_OUTPUT_FILE="${SCRIPT_DIR}/actual-output.txt"
rm -f "$ACTUAL_OUTPUT_FILE"

# --------------------------------------- Enough accounts for second page ------------------------------------------------------------
input_value="00001 GET     TRANS     0000100002 00002"
$(run_dispatcher_clean "$ACTUAL_OUTPUT_FILE" "$input_value")
./scripts/sql/reset-db || { echo "DB reset failed"; exit 1; }

# --------------------------------------- Test case for 11 records(1 record only on 2nd page) ---------------------------------------------------------
SQL_CLEAR_PATH="$PWD/files/tests/non-us-tests/get-transactions/test-11-records.sql"
sudo -u "$PG_USER" psql -d "$DB_NAME" -f "$SQL_CLEAR_PATH"

input_value="00001 GET     TRANS     0000100002 00002"
$(run_dispatcher_clean "$ACTUAL_OUTPUT_FILE" "$input_value")
./scripts/sql/reset-db || { echo "DB reset failed"; exit 1; }

# --------------------------------------- Test case for empty table(no records) ----------------------------------------------------------------
SQL_CLEAR_PATH="$PWD/files/tests/non-us-tests/get-transactions/clear-transaction.sql"
sudo -u "$PG_USER" psql -d "$DB_NAME" -f "$SQL_CLEAR_PATH"


input_value="00001 GET     TRANS"
$(run_dispatcher_clean "$ACTUAL_OUTPUT_FILE" "$input_value")
./scripts/sql/reset-db || { echo "DB reset failed"; exit 1; }

# --------------------------------------- Test case for Teller permission deny -------------------------------------------------------------------
input_value="00002 GET     TRANS"
$(run_dispatcher_clean "$ACTUAL_OUTPUT_FILE" "$input_value")
./scripts/sql/reset-db || { echo "DB reset failed"; exit 1; }

# --------------------------------------- Test case for Client permission deny -------------------------------------------------------------------
input_value="00004 GET     TRANS"
$(run_dispatcher_clean "$ACTUAL_OUTPUT_FILE" "$input_value")
./scripts/sql/reset-db || { echo "DB reset failed"; exit 1; }

# --------------------------------------- Test case for empty BUSRID -------------------------------------------------------------------
input_value="      GET     TRANS"
$(run_dispatcher_clean "$ACTUAL_OUTPUT_FILE" "$input_value")
./scripts/sql/reset-db || { echo "DB reset failed"; exit 1; }

# --------------------------------------- Test case to get transactions for an account on 1st page ------------------------------------------------------------
input_value="00001 GET     TRANS     0000100002 00001"
$(run_dispatcher_clean "$ACTUAL_OUTPUT_FILE" "$input_value")
./scripts/sql/reset-db || { echo "DB reset failed"; exit 1; }

# --------------------------------------- Test case to get transactions for an account on 2nd page ------------------------------------------------------------
input_value="00001 GET     TRANS     0000100002 00002"
$(run_dispatcher_clean "$ACTUAL_OUTPUT_FILE" "$input_value")
./scripts/sql/reset-db || { echo "DB reset failed"; exit 1; }

# --------------------------------------- Test case for only 1 transaction for 2nd page for account 2 ------------------------------------------------------------
SQL_CLEAR_PATH="$PWD/files/tests/non-us-tests/get-transactions/test-11-records.sql"
sudo -u "$PG_USER" psql -d "$DB_NAME" -f "$SQL_CLEAR_PATH"

input_value="00001 GET     TRANS     0000100002 00002"
$(run_dispatcher_clean "$ACTUAL_OUTPUT_FILE" "$input_value")
./scripts/sql/reset-db || { echo "DB reset failed"; exit 1; }

# --------------------------------------- Test case for no transactions for an account(no records) ----------------------------------------------------------------
SQL_CLEAR_PATH="$PWD/files/tests/non-us-tests/get-transactions/clear-transaction.sql"
sudo -u "$PG_USER" psql -d "$DB_NAME" -f "$SQL_CLEAR_PATH"

input_value="00001 GET     TRANS     0000100002 00001"
$(run_dispatcher_clean "$ACTUAL_OUTPUT_FILE" "$input_value")
./scripts/sql/reset-db || { echo "DB reset failed"; exit 1; }
# --------------------------------------- Test case for teller on client ----------------------------------------------------------------
input_value="00002 GET     TRANS     0000400005 00001"
$(run_dispatcher_clean "$ACTUAL_OUTPUT_FILE" "$input_value")
./scripts/sql/reset-db || { echo "DB reset failed"; exit 1; }

# --------------------------------------- Test case for teller on another teller ----------------------------------------------------------------
SQL_INSERT1="$PWD/files/tests/non-us-tests/get-transactions/new-acc-transaction.sql"
sudo -u "$PG_USER" psql -d "$DB_NAME" -f "$SQL_INSERT1"

input_value="00002 GET     TRANS     0000200008 00001"
$(run_dispatcher_clean "$ACTUAL_OUTPUT_FILE" "$input_value")
./scripts/sql/reset-db || { echo "DB reset failed"; exit 1; }

# --------------------------------------- Test case for teller on self ----------------------------------------------------------------
input_value="00002 GET     TRANS     0000100002 00001"
$(run_dispatcher_clean "$ACTUAL_OUTPUT_FILE" "$input_value")
./scripts/sql/reset-db || { echo "DB reset failed"; exit 1; }

# --------------------------------------- Test case for client on teller ----------------------------------------------------------------
input_value="00004 GET     TRANS     0000100002 00001"
$(run_dispatcher_clean "$ACTUAL_OUTPUT_FILE" "$input_value")
./scripts/sql/reset-db || { echo "DB reset failed"; exit 1; }

# --------------------------------------- Test case for client on another client ----------------------------------------------------------------
input_value="00004 GET     TRANS     0000400005 00001"
$(run_dispatcher_clean "$ACTUAL_OUTPUT_FILE" "$input_value")
./scripts/sql/reset-db || { echo "DB reset failed"; exit 1; }

# --------------------------------------- Test case for client on self ----------------------------------------------------------------
input_value="00005 GET     TRANS     0000400005 00001"
$(run_dispatcher_clean "$ACTUAL_OUTPUT_FILE" "$input_value")
./scripts/sql/reset-db || { echo "DB reset failed"; exit 1; }