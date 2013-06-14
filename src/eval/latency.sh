#! /bin/bash

CLIENT="./client_nigori.byte"
SERVER="./server_nigori.byte --sql"
PREPEND=get.output.
DB=nigori.db
server_pid=""

start_server()
{
  output=$1
  if [ -z "$output" ];
  then
    output=/dev/null
  fi
  $SERVER 1>/dev/null  2>>$output &
  server_pid=$!
}

stop_server()
{
  kill $server_pid
  wait $server_pid
}

setup_db()
{
  rm $DB >& /dev/null
  ln $1 $DB >& /dev/null
}

clean_db()
{
  rm $DB >& /dev/null
}

register()
{
  $CLIENT register >& /dev/null
}

test_one()
{
  samples=50
  key="-----test-key-000000"
  rev="-----test-rev-000001"

  db=$1
  output="$PREPEND$db"
  echo $output
  setup_db $db
  start_server $output
  for j in $(seq $samples)
  do
    $CLIENT get $key $rev 1>/dev/null 2>>$output
  done
  stop_server
  clean_db
}

test_latency()
{
  for i in nigori.db.*
  do
    db=$(basename $i)
    test_one $db
  done
}
##############################################################################
if [ -z "$1" ];
then
  test_latency
else
  test_one $1
fi
