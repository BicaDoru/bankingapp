export RUN_DBNAME="pocbanking@127.0.0.1:5432"
export RUN_USERNAME="postgres"
export RUN_PASSWD="postgres"
export COB_LIBRARY_PATH=./lib

. $PWD/scripts/dispatcher-utils

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ACTUAL_OUTPUT_FILE="${SCRIPT_DIR}/actual-output.txt"
rm -f "$ACTUAL_OUTPUT_FILE"
 #---------------------------------------------enough customers for a second page
 inputs=(
"00001 POST    CUSTOMER  00003client3                                          Zorilor1"
"00001 POST    CUSTOMER  00001client4                                          Zorilor2"
"00001 POST    CUSTOMER  00002client5                                          Zorilor3"
"00001 POST    CUSTOMER  00004client6                                          Zorilor2"
"00001 POST    CUSTOMER  00001client7                                          Zorilor3"
"00001 POST    CUSTOMER  00001client8                                          Zorilor2"
"00001 POST    CUSTOMER  00001teller3                                          Zorilor9"
"00001 POST    CUSTOMER  00001teller4                                          Zorilor8"
"00001 POST    CUSTOMER  00001teller5                                          Zorilor7"
"00001 POST    CUSTOMER  00001teller6                                          Zorilor6"
"00001 POST    CUSTOMER  00001teller7                                          Zorilor5"

 )

 for i in "${inputs[@]}"; do
  run_dispatcher_quiet "$i"
done
 input_value="00001 GET     CUSTOMERS"
 $(run_dispatcher_clean "$ACTUAL_OUTPUT_FILE" "$input_value")
./scripts/sql/reset-db || { echo "DB reset failed"; exit 1; }

#------------------------------------------------Test case for multiple customers on page2

 inputs=(
"00001 POST    CUSTOMER  00003client3                                          Zorilor1"
"00001 POST    CUSTOMER  00001client4                                          Zorilor2"
"00001 POST    CUSTOMER  00002client5                                          Zorilor3"
"00001 POST    CUSTOMER  00004client6                                          Zorilor2"
"00001 POST    CUSTOMER  00001client7                                          Zorilor3"
"00001 POST    CUSTOMER  00001client8                                          Zorilor2"
"00001 POST    CUSTOMER  00001teller3                                          Zorilor9"
"00001 POST    CUSTOMER  00001teller4                                          Zorilor8"
"00001 POST    CUSTOMER  00001teller5                                          Zorilor7"
"00001 POST    CUSTOMER  00001teller6                                          Zorilor6"
"00001 POST    CUSTOMER  00001teller7                                          Zorilor5"

 )

 for i in "${inputs[@]}"; do
  run_dispatcher_quiet "$i"
done
 input_value="00001 GET     CUSTOMERS 00002"
 $(run_dispatcher_clean "$ACTUAL_OUTPUT_FILE" "$input_value")
./scripts/sql/reset-db || { echo "DB reset failed"; exit 1; }
#------------------------------------------------Test case for one customer on page2
 inputs=(
"00001 POST    CUSTOMER  00003client3                                          Zorilor1"
"00001 POST    CUSTOMER  00001client4                                          Zorilor2"
"00001 POST    CUSTOMER  00002client5                                          Zorilor3"
"00001 POST    CUSTOMER  00004client6                                          Zorilor2"
"00001 POST    CUSTOMER  00001client7                                          Zorilor3"
"00001 POST    CUSTOMER  00001client8                                          Zorilor2"
"00001 POST    CUSTOMER  00001teller3                                          Zorilor9"

 )

 for i in "${inputs[@]}"; do
  run_dispatcher_quiet "$i"
done
 input_value="00001 GET     CUSTOMERS 00002"
 $(run_dispatcher_clean "$ACTUAL_OUTPUT_FILE" "$input_value")
./scripts/sql/reset-db || { echo "DB reset failed"; exit 1; }
#-------------------------------------------------------------------------Test case for updating one customer on page 1
inputs=(
 "00001 PUT     CUSTOMER  00004client6                                           AI4LYF4"
)
 for i in "${inputs[@]}"; do
  run_dispatcher_quiet "$i"
done
 input_value="00001 GET     CUSTOMERS"
 $(run_dispatcher_clean "$ACTUAL_OUTPUT_FILE" "$input_value")
./scripts/sql/reset-db || { echo "DB reset failed"; exit 1; }
#-------------------------------------------------------------------------Test case for an empty page
inputs=(
  "00001 DELETE  CUSTOMER  00001"
  "00001 DELETE  CUSTOMER  00002"
  "00001 DELETE  CUSTOMER  00003"
  "00001 DELETE  CUSTOMER  00004"
)
for i in "${inputs[@]}"; do
  run_dispatcher_quiet "$i"
done
 input_value="00001 GET     CUSTOMERS"
 $(run_dispatcher_clean "$ACTUAL_OUTPUT_FILE" "$input_value")
./scripts/sql/reset-db || { echo "DB reset failed"; exit 1; }
#----------------------------------------------------------------------Test case for deleting a customer
inputs=(
  "00001 DELETE  CUSTOMER  00001"
)
for i in "${inputs[@]}"; do
  run_dispatcher_quiet "$i"
done
 input_value="00001 GET     CUSTOMERS"
 $(run_dispatcher_clean "$ACTUAL_OUTPUT_FILE" "$input_value")
./scripts/sql/reset-db || { echo "DB reset failed"; exit 1; }
#------------------------------------------------------------Test case for teller getting the list of customers
input_value="00002 GET     CUSTOMERS"
$(run_dispatcher_clean "$ACTUAL_OUTPUT_FILE" "$input_value")
./scripts/sql/reset-db || { echo "DB reset failed"; exit 1; }
#------------------------------------------------------------Test case for customer getting the list of customers
input_value="00004 GET     CUSTOMERS"
$(run_dispatcher_clean "$ACTUAL_OUTPUT_FILE" "$input_value")
./scripts/sql/reset-db || { echo "DB reset failed"; exit 1; }