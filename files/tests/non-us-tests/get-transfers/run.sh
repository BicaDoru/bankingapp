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

SQL_INSERT_DATA_PATH="$PWD/files/tests/non-us-tests/get-transfers/populate-transfer.sql"

# --------------------------------------- First page, all transfers ------------------------------------------------------------
sudo -u "$PG_USER" psql -d "$DB_NAME" -f "$SQL_INSERT_DATA_PATH"
input_value="00001 GET     TRANSFER  00000 00000 00001"
$(run_dispatcher_clean "$ACTUAL_OUTPUT_FILE" "$input_value")
./scripts/sql/reset-db || { echo "DB reset failed"; exit 1; }

# --------------------------------------- First page by default, all transfers ------------------------------------------------------------
input_value="00001 GET     TRANSFER  00000 00000"
$(run_dispatcher_clean "$ACTUAL_OUTPUT_FILE" "$input_value")
./scripts/sql/reset-db || { echo "DB reset failed"; exit 1; }

# --------------------------------------- Second page, all transfers ------------------------------------------------------------
sudo -u "$PG_USER" psql -d "$DB_NAME" -f "$SQL_INSERT_DATA_PATH"
input_value="00001 GET     TRANSFER  00000 00000 00002"
$(run_dispatcher_clean "$ACTUAL_OUTPUT_FILE" "$input_value")
./scripts/sql/reset-db || { echo "DB reset failed"; exit 1; }

# --------------------------------------- Filter by ACCID = 2 ------------------------------------------------------------
input_value="00001 GET     TRANSFER  00000 00002"
$(run_dispatcher_clean "$ACTUAL_OUTPUT_FILE" "$input_value")
./scripts/sql/reset-db || { echo "DB reset failed"; exit 1; }

# --------------------------------------- Filter by CUSTID = 4 ------------------------------------------------------------
input_value="00001 GET     TRANSFER  00004 00000"
$(run_dispatcher_clean "$ACTUAL_OUTPUT_FILE" "$input_value")
./scripts/sql/reset-db || { echo "DB reset failed"; exit 1; }

# --------------------------------------- Filter by BOTH (CUSTID IS IGNORED) ------------------------------------------------------------
input_value="00001 GET     TRANSFER  00004 00002"
$(run_dispatcher_clean "$ACTUAL_OUTPUT_FILE" "$input_value")
./scripts/sql/reset-db || { echo "DB reset failed"; exit 1; }

# --------------------------------------- BaTe to BaTe account - fail ------------------------------------------------------------
input_value="00003 GET     TRANSFER  00000 00002 00001"
$(run_dispatcher_clean "$ACTUAL_OUTPUT_FILE" "$input_value")
./scripts/sql/reset-db || { echo "DB reset failed"; exit 1; }

# --------------------------------------- BaTe to his account - success ------------------------------------------------------------
input_value="00002 GET     TRANSFER  00000 00002 00001"
$(run_dispatcher_clean "$ACTUAL_OUTPUT_FILE" "$input_value")
./scripts/sql/reset-db || { echo "DB reset failed"; exit 1; }

# --------------------------------------- BaCl to BaTe account - fail ------------------------------------------------------------
input_value="00004 GET     TRANSFER  00000 00002 00001"
$(run_dispatcher_clean "$ACTUAL_OUTPUT_FILE" "$input_value")
./scripts/sql/reset-db || { echo "DB reset failed"; exit 1; }

# --------------------------------------- BaTe to BaCl customer - success ------------------------------------------------------------
input_value="00003 GET     TRANSFER  00004 00000 00001"
$(run_dispatcher_clean "$ACTUAL_OUTPUT_FILE" "$input_value")
./scripts/sql/reset-db || { echo "DB reset failed"; exit 1; }

# --------------------------------------- BaTe to itself - no data ------------------------------------------------------------
input_value="00003 GET     TRANSFER  00002 00000 00001"
$(run_dispatcher_clean "$ACTUAL_OUTPUT_FILE" "$input_value")
./scripts/sql/reset-db || { echo "DB reset failed"; exit 1; }

# --------------------------------------- BaTe all transfers - no permission ------------------------------------------------------------
input_value="00002 GET     TRANSFER  00000 00000 00001"
$(run_dispatcher_clean "$ACTUAL_OUTPUT_FILE" "$input_value")
./scripts/sql/reset-db || { echo "DB reset failed"; exit 1; }

# --------------------------------------- Filter by CUSTID = 2 - no data  ------------------------------------------------------------
input_value="00001 GET     TRANSFER  00002 00000"
$(run_dispatcher_clean "$ACTUAL_OUTPUT_FILE" "$input_value")
./scripts/sql/reset-db || { echo "DB reset failed"; exit 1; }
