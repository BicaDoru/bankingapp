export RUN_DBNAME="pocbanking@127.0.0.1:5432"
export RUN_USERNAME="postgres"
export RUN_PASSWD="postgres"
export COB_LIBRARY_PATH=./lib

. $PWD/scripts/dispatcher-utils

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ACTUAL_OUTPUT_FILE="${SCRIPT_DIR}/actual-output.txt"
rm -f "$ACTUAL_OUTPUT_FILE"

input_value="00001 GET     TRANS     0000000002      2025-09-04"
$(run_dispatcher_clean "$ACTUAL_OUTPUT_FILE" "$input_value")