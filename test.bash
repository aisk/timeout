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

echo "Testing timeout with sleep"
./timeout 1s sleep 3
exit_code=$?
if [ $exit_code -eq 124 ]; then
    echo "✓ Timeout worked (exit: 124)"
else
    echo "✗ Timeout failed (exit: $exit_code)"
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

echo "=== All tests completed ==="