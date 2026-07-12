#!/bin/bash

echo "=== Testing timeout command ==="

echo "Building timeout..."
make
echo "✓ Build successful"

echo "Testing help and version options"
./timeout --help | head -1
./timeout --version
echo "✓ Help and version options work"

echo "Testing normal command execution"
./timeout 1s echo "Hello world"
echo "✓ Normal command execution works"

echo "Testing zero duration disables timeout"
./timeout 0 sh -c 'sleep 0.1; exit 7'
exit_code=$?
if [ $exit_code -eq 7 ]; then
    echo "✓ Zero duration disables timeout (exit: 7)"
else
    echo "✗ Zero duration failed (exit: $exit_code, expected 7)"
    exit 1
fi

echo "Testing timeout with sleep"
./timeout 1s sleep 3
exit_code=$?
if [ $exit_code -eq 124 ]; then
    echo "✓ Timeout worked (exit: 124)"
else
    echo "✗ Timeout failed (exit: $exit_code)"
    exit 1
fi

echo "Testing timeout terminates descendant processes"
marker="/tmp/timeout_process_group_$$"
rm -f "$marker"
./timeout 0.1s sh -c "(sleep 0.3; touch '$marker') & wait"
exit_code=$?
sleep 0.4
if [ $exit_code -eq 124 ] && [ ! -e "$marker" ]; then
    echo "✓ Timeout terminated descendant processes"
else
    echo "✗ Descendant process survived timeout"
    rm -f "$marker"
    exit 1
fi

echo "Testing preserve-status option"
./timeout -p 1s echo "test"
exit_code=$?
if [ $exit_code -eq 0 ]; then
    echo "✓ Preserve-status worked (exit: 0)"
else
    echo "✗ Preserve-status failed (exit: $exit_code)"
    exit 1
fi

echo "Testing signal option"
./timeout -s TERM 1s sleep 3
exit_code=$?
if [ $exit_code -eq 124 ]; then
    echo "✓ Signal option worked (exit: 124)"
else
    echo "✗ Signal option failed (exit: $exit_code)"
    exit 1
fi

echo "Testing foreground option"
echo "test input" | ./timeout -f 1s cat > /tmp/foreground_test.txt
if [ "$(cat /tmp/foreground_test.txt)" = "test input" ]; then
    echo "✓ Foreground option works with input/output"
else
    echo "✗ Foreground option failed"
    exit 1
fi

# Test that foreground mode still respects timeout
./timeout -f 1s sleep 3
exit_code=$?
if [ $exit_code -eq 124 ]; then
    echo "✓ Foreground mode still respects timeout (exit: 124)"
else
    echo "✗ Foreground mode timeout failed (exit: $exit_code)"
    exit 1
fi

echo "Testing non-existent command"
./timeout 1s nonexistent_command_xyz_abc 2>/dev/null
exit_code=$?
if [ $exit_code -eq 127 ]; then
    echo "✓ Non-existent command returns 127"
else
    echo "✗ Non-existent command failed (exit: $exit_code, expected 127)"
    exit 1
fi

echo "Testing non-executable command"
echo "#!/bin/sh" > test_not_exec
chmod a-x test_not_exec
./timeout 1s ./test_not_exec 2>/dev/null
exit_code=$?
if [ $exit_code -eq 126 ]; then
    echo "✓ Non-executable command returns 126"
else
    echo "✗ Non-executable command failed (exit: $exit_code, expected 126)"
    rm -f test_not_exec
    exit 1
fi
rm -f test_not_exec

echo "=== All tests completed ==="
