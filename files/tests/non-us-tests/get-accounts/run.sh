#!/bin/bash

export RUN_DBNAME="pocbanking@127.0.0.1:5432"
export RUN_USERNAME="postgres"
export RUN_PASSWD="postgres"
export COB_LIBRARY_PATH=./lib

. $PWD/scripts/dispatcher-utils

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ACTUAL_OUTPUT_FILE="${SCRIPT_DIR}/actual-output.txt"
rm -f "$ACTUAL_OUTPUT_FILE"

# --------------------------------------- Enough accounts for second page ------------------------------------------------------------
inputs=(
"00001 POST    ACCOUNT   00002 RO94PORL9647798828952396       USD"
"00001 POST    ACCOUNT   00003 RO87PORL1987826528687392       USD"
"00001 POST    ACCOUNT   00001 RO79PORL9890475718607553       USD"
"00001 POST    ACCOUNT   00003 RO45PORL4306942192473149       USD"
"00001 POST    ACCOUNT   00002 RO74PORL3978468720008965       USD"
"00001 POST    ACCOUNT   00002 RO20PORL8738915090568311       USD"
"00001 POST    ACCOUNT   00001 RO02PORL2970073188350410       USD"
"00001 POST    ACCOUNT   00004 RO05PORL7175326800736108       USD"
"00001 POST    ACCOUNT   00003 RO09PORL7522532601455006       USD"
"00001 POST    ACCOUNT   00001 RO59PORL4206151379855566       USD"
)

for i in "${inputs[@]}"; do
  run_dispatcher_quiet "$i"
done

input_value="00001 GET     ACCOUNTS        00002"
$(run_dispatcher_clean "$ACTUAL_OUTPUT_FILE" "$input_value")
./scripts/sql/reset-db || { echo "DB reset failed"; exit 1; }

# --------------------------------------- Test case for 11 records(1 record only on 2nd page) ---------------------------------------------------------
inputs=(
"00001 POST    ACCOUNT   00002 RO94PORL9647798828952396       USD"
"00001 POST    ACCOUNT   00003 RO87PORL1987826528687392       USD"
"00001 POST    ACCOUNT   00001 RO79PORL9890475718607553       USD"
"00001 POST    ACCOUNT   00003 RO45PORL4306942192473149       USD"
)

for i in "${inputs[@]}"; do
  run_dispatcher_quiet "$i"
done

input_value="00001 GET     ACCOUNTS        00002"
$(run_dispatcher_clean "$ACTUAL_OUTPUT_FILE" "$input_value")
./scripts/sql/reset-db || { echo "DB reset failed"; exit 1; }

# --------------------------------------- Test case for empty table(no records) ----------------------------------------------------------------
inputs=(
"00001 DELETE  ACCOUNT   00001"
"00001 DELETE  ACCOUNT   00002"
"00001 DELETE  ACCOUNT   00003"
"00001 DELETE  ACCOUNT   00004"
"00001 DELETE  ACCOUNT   00005"
"00001 DELETE  ACCOUNT   00006"
"00001 DELETE  ACCOUNT   00007"
)

for i in "${inputs[@]}"; do
  run_dispatcher_quiet "$i"
done

input_value="00001 GET     ACCOUNTS"
$(run_dispatcher_clean "$ACTUAL_OUTPUT_FILE" "$input_value")
./scripts/sql/reset-db || { echo "DB reset failed"; exit 1; }

# --------------------------------------- Test case for Teller permission deny -------------------------------------------------------------------
input_value="00002 GET     ACCOUNTS"
$(run_dispatcher_clean "$ACTUAL_OUTPUT_FILE" "$input_value")
./scripts/sql/reset-db || { echo "DB reset failed"; exit 1; }

# --------------------------------------- Test case for Client permission deny -------------------------------------------------------------------
input_value="00004 GET     ACCOUNTS"
$(run_dispatcher_clean "$ACTUAL_OUTPUT_FILE" "$input_value")
./scripts/sql/reset-db || { echo "DB reset failed"; exit 1; }

# --------------------------------------- Test case for empty BUSRID -------------------------------------------------------------------
input_value="      GET     ACCOUNTS"
$(run_dispatcher_clean "$ACTUAL_OUTPUT_FILE" "$input_value")
./scripts/sql/reset-db || { echo "DB reset failed"; exit 1; }

# --------------------------------------- Test case to get accounts for a customer on 1st page ------------------------------------------------------------
input_value="00001 GET     ACCOUNTS  00001"
$(run_dispatcher_clean "$ACTUAL_OUTPUT_FILE" "$input_value")
./scripts/sql/reset-db || { echo "DB reset failed"; exit 1; }

# --------------------------------------- Test case to get accounts for a customer on 2nd page ------------------------------------------------------------
inputs=(
"00001 POST    ACCOUNT   00002 RO94PORL9647798828952396       USD"
"00001 POST    ACCOUNT   00002 RO87PORL1987826528687392       USD"
"00001 POST    ACCOUNT   00002 RO79PORL9890475718607553       USD"
"00001 POST    ACCOUNT   00002 RO45PORL4306942192473149       USD"
"00001 POST    ACCOUNT   00002 RO74PORL3978468720008965       USD"
"00001 POST    ACCOUNT   00002 RO20PORL8738915090568311       USD"
"00001 POST    ACCOUNT   00002 RO02PORL2970073188350410       USD"
"00001 POST    ACCOUNT   00002 RO05PORL7175326800736108       USD"
"00001 POST    ACCOUNT   00002 RO09PORL7522532601455006       USD"
"00001 POST    ACCOUNT   00002 RO59PORL4206151379855566       USD"
"00001 POST    ACCOUNT   00002 RO59PORL8203454372853174       USD"
"00001 POST    ACCOUNT   00002 RO15PORL1938475620192837       USD"
"00001 POST    ACCOUNT   00002 RO33PORL5647382910456728       USD"
"00001 POST    ACCOUNT   00002 RO77PORL9081726354091827       USD"
)

for i in "${inputs[@]}"; do
  run_dispatcher_quiet "$i"
done

input_value="00001 GET     ACCOUNTS  00002 00002"
$(run_dispatcher_clean "$ACTUAL_OUTPUT_FILE" "$input_value")
./scripts/sql/reset-db || { echo "DB reset failed"; exit 1; }

# --------------------------------------- Test case for only 1 account for 2nd page for customer 2 ------------------------------------------------------------
inputs=(
"00001 POST    ACCOUNT   00002 RO94PORL9647798828952396       USD"
"00001 POST    ACCOUNT   00002 RO87PORL1987826528687392       USD"
"00001 POST    ACCOUNT   00002 RO79PORL9890475718607553       USD"
"00001 POST    ACCOUNT   00002 RO45PORL4306942192473149       USD"
"00001 POST    ACCOUNT   00002 RO74PORL3978468720008965       USD"
"00001 POST    ACCOUNT   00002 RO20PORL8738915090568311       USD"
"00001 POST    ACCOUNT   00002 RO02PORL2970073188350410       USD"
"00001 POST    ACCOUNT   00002 RO05PORL7175326800736108       USD"
"00001 POST    ACCOUNT   00002 RO09PORL7522532601455006       USD"
"00001 POST    ACCOUNT   00002 RO59PORL4206151379855566       USD"
"00001 POST    ACCOUNT   00002 RO59PORL8203454372853174       USD"
)

for i in "${inputs[@]}"; do
  run_dispatcher_quiet "$i"
done

input_value="00001 GET     ACCOUNTS  00002 00002"
$(run_dispatcher_clean "$ACTUAL_OUTPUT_FILE" "$input_value")
./scripts/sql/reset-db || { echo "DB reset failed"; exit 1; }

# --------------------------------------- Test case for no accounts for a customer(no records) ----------------------------------------------------------------
input_value="00001 GET     ACCOUNTS  00002"
$(run_dispatcher_clean "$ACTUAL_OUTPUT_FILE" "$input_value")
./scripts/sql/reset-db || { echo "DB reset failed"; exit 1; }

# --------------------------------------- Test case for teller on client ----------------------------------------------------------------
input_value="00002 GET     ACCOUNTS  00003"
$(run_dispatcher_clean "$ACTUAL_OUTPUT_FILE" "$input_value")
./scripts/sql/reset-db || { echo "DB reset failed"; exit 1; }

# --------------------------------------- Test case for teller on another teller ----------------------------------------------------------------
input_value="00002 GET     ACCOUNTS  00002"
$(run_dispatcher_clean "$ACTUAL_OUTPUT_FILE" "$input_value")
./scripts/sql/reset-db || { echo "DB reset failed"; exit 1; }

# --------------------------------------- Test case for teller on self ----------------------------------------------------------------
input_value="00002 GET     ACCOUNTS  00001"
$(run_dispatcher_clean "$ACTUAL_OUTPUT_FILE" "$input_value")
./scripts/sql/reset-db || { echo "DB reset failed"; exit 1; }

# --------------------------------------- Test case for client on teller ----------------------------------------------------------------
input_value="00004 GET     ACCOUNTS  00001"
$(run_dispatcher_clean "$ACTUAL_OUTPUT_FILE" "$input_value")
./scripts/sql/reset-db || { echo "DB reset failed"; exit 1; }

# --------------------------------------- Test case for client on another client ----------------------------------------------------------------
input_value="00005 GET     ACCOUNTS  00003"
$(run_dispatcher_clean "$ACTUAL_OUTPUT_FILE" "$input_value")
./scripts/sql/reset-db || { echo "DB reset failed"; exit 1; }

# --------------------------------------- Test case for client on self ----------------------------------------------------------------
input_value="00005 GET     ACCOUNTS  00004"
$(run_dispatcher_clean "$ACTUAL_OUTPUT_FILE" "$input_value")
./scripts/sql/reset-db || { echo "DB reset failed"; exit 1; }