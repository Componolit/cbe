#!/bin/bash

echo "--- Automated CBE testing ---"

RW_TEST=0
KEY_TEST=1
INFO_TEST=1
CREATE_SNAPSHOT_TEST=1
CREATE_SNAPSHOT_DIRTY_TEST=1
WRITE_1_BLOCK_TEST=0
WRITE_CREATE_TEST=0

test_key() {
	echo "Write key to CBE"
	echo "All your base are belong to us" > /dev/cbe/control/key
}

test_info() {
	echo "Dump CBE info"
	cat /dev/cbe/control/info
}

test_create_snapshot() {
	echo "Create snapshot"
	echo true > /dev/cbe/control/create_snapshot
}

test_rw() {
	local data_file="$1"
	[ "$data_file" = "" ] && exit 1

	# read complete current snapshot
	echo "Read '$data_file'"
	dd if=$data_file of=/dev/null bs=4096

	# write complete current snapshot
	echo "Write '$data_file'"
	dd if=$data_file of=/dev/null bs=4096
}

produce_pattern() {
	[ "$pattern" = "" ] && exit 1

	seq -s "$pattern" 4096
}

test_write_1() {
	local data_file="$1"
	local offset=$2

	local pattern="1"
	local pattern_file="/tmp/pattern.$pattern"
	# create pattern file because using '| dd' leads to a one byte short write
	produce_pattern "$pattern" > $pattern_file
	dd bs=4096 count=1 if=/tmp/pattern.$pattern of=$data_file seek=$offset || exit 1
	rm $pattern_file
}

main() {
	local data_file="/dev/cbe/current/data"

	if [ $KEY_TEST -eq 1 ]; then
		test_key
	fi

	if [ $INFO_TEST -eq 1 ]; then
		test_info
	fi

	if [ $CREATE_SNAPSHOT_TEST -eq 1 ]; then
		test_create_snapshot
	fi

	if [ $CREATE_SNAPSHOT_DIRTY_TEST -eq 1 ]; then
		test_create_snapshot
		test_write_1 "$data_file" "$((512 * 1024 / 4096))"
		test_create_snapshot
		test_write_1 "$data_file" "$((128 * 1024 / 4096))"
		test_create_snapshot
	fi

	if [ $RW_TEST -eq 1 ]; then
		test_rw "$data_file"
	fi

	if [ $WRITE_1_BLOCK_TEST -eq 1 ]; then
		test_write_1 "$data_file" "$((512 * 1024 / 4096))"
	fi

	if [ $WRITE_CREATE_TEST -eq 1 ]; then
		test_write_1 "$data_file" "$((512 * 1024 / 4096))"
		test_create_snapshot
		test_info
	fi

	ls -l /dev/cbe/snapshots
	ls -l /dev/cbe/snapshots/3
}

main "$@"

#exit 0
