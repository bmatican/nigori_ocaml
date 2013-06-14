#! /bin/bash

CLIENT="./../client_nigori.byte"
SERVER="./../server_nigori.byte --sql"
server_pid=""

total_done_batches=1
waitall() # PIDS
{
  for pid in "$@";
  do
    wait $pid
  done
  echo "done batch $total_done_batches"
  ((++total_done_batches))
}

start_server()
{
  $SERVER 2>&1 >/dev/null &
  server_pid=$!
}

stop_server()
{
  kill $server_pid
  wait $server_pid
}

move_db()
{
  mv ../nigori.db nigori.db.n$1.v$2
}

register()
{
  $CLIENT register 2>&1 >/dev/null
}

send_batch() # $1=store_size; $2=val_size
{
  local i=1
  local store_size=$1
  local batch=1
  local batch_limit=10
  local pids=""
  local key="-----test-key-000000"
  local prepend_size=$(($2 - 20)) # 20 fixed chars, rest is fluff
  local prepend=$(python -c "print '0' * $prepend_size")

  register
  for line in $(cat suffixes.data)
  do
    rev="-----test-rev-$line"
    val="-----test-val-$line"
    val="$prepend$val"
    $CLIENT put $key $rev $val 2>&1 >/dev/null &
    pid=$!
    pids="$pids $pid"
    ((++batch))
    if [ "$batch" -gt "$batch_limit" ];
    then
      waitall $pids
      batch=1
      pids=""
    fi
    ((++i))
    if [ "$i" -gt "$store_size" ];
    then
      break
    fi
  done
  waitall $pids
}

do_load()
{
  start_server
  register
  send_batch $1 $2
  stop_server
  move_db
}
##############################################################################
do_load $@
