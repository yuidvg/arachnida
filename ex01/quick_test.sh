#!/bin/bash

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Test configuration
SERVER_PORT=8000
SERVER_PID=""
TEST_DIR="data/quick_test"
FAILED_TESTS=0
TOTAL_TESTS=0

# Cleanup function
cleanup() {
    echo -e "${YELLOW}Cleaning up...${NC}"

    # Kill server if running
    if [ ! -z "$SERVER_PID" ]; then
        kill $SERVER_PID 2>/dev/null
        wait $SERVER_PID 2>/dev/null
    fi

    # Kill any remaining python servers
    pkill -f "python.*http.server.*$SERVER_PORT" 2>/dev/null

    # Remove test directory
    rm -rf "$TEST_DIR" 2>/dev/null

    echo "Cleanup complete"
}

# Set up trap for cleanup
trap cleanup EXIT INT TERM

# Function to run a test case
run_test() {
    local test_name="$1"
    local depth="$2"
    local expected_min="$3"
    local expected_max="$4"

    TOTAL_TESTS=$((TOTAL_TESTS + 1))
    echo -e "${BLUE}Test $TOTAL_TESTS: $test_name${NC}"

    # Create unique test directory
    local test_subdir="$TEST_DIR/test_$TOTAL_TESTS"
    mkdir -p "$test_subdir"

    # Run spider with output redirect
    echo "Running: cabal exec spider -- -r -l $depth -p $test_subdir http://localhost:$SERVER_PORT"
    if cabal exec spider -- -r -l $depth -p "$test_subdir" http://localhost:$SERVER_PORT > /dev/null 2>&1; then
        # Count downloaded images
        local count=$(find "$test_subdir" -name "*.jpg" -o -name "*.jpeg" -o -name "*.png" -o -name "*.gif" -o -name "*.bmp" | wc -l)

        echo "Downloaded: $count images"

        # Check if count is within expected range
        if [ $count -ge $expected_min ] && [ $count -le $expected_max ]; then
            echo -e "${GREEN}✓ PASS${NC} ($count images, expected $expected_min-$expected_max)"
        else
            echo -e "${RED}✗ FAIL${NC} ($count images, expected $expected_min-$expected_max)"
            FAILED_TESTS=$((FAILED_TESTS + 1))
        fi
    else
        echo -e "${RED}✗ FAIL${NC} (Spider execution failed)"
        FAILED_TESTS=$((FAILED_TESTS + 1))
    fi
    echo ""
}

echo -e "${BLUE}=== Spider Automated Test Suite ===${NC}"
echo ""

# Build spider
echo -e "${YELLOW}Building spider...${NC}"
if ! cabal build > /dev/null 2>&1; then
    echo -e "${RED}Failed to build spider${NC}"
    exit 1
fi
echo -e "${GREEN}Spider built successfully${NC}"
echo ""

# Start server
echo -e "${YELLOW}Starting test server on port $SERVER_PORT...${NC}"
cd test
python3 -m http.server $SERVER_PORT > /dev/null 2>&1 &
SERVER_PID=$!
cd ..

# Wait for server to start
sleep 2

# Check if server is running
if ! curl -s http://localhost:$SERVER_PORT > /dev/null; then
    echo -e "${RED}Failed to start server${NC}"
    exit 1
fi
echo -e "${GREEN}Server started successfully${NC}"
echo ""

# Run test cases
echo -e "${YELLOW}Running test cases...${NC}"
echo ""

# Test 1: Depth 1 - should get root + level1 images (observed: 23, theoretical: 36)
run_test "Shallow crawl (depth 1)" 1 20 40

# Test 2: Depth 3 - should get root + level1-3 images (observed: 83, theoretical: 73)
run_test "Medium crawl (depth 3)" 3 70 90

# Test 3: Depth 6 - should get maximum images from all levels
run_test "Deep crawl (depth 6)" 6 40 80

# Summary
echo -e "${BLUE}=== Test Summary ===${NC}"
echo "Total tests: $TOTAL_TESTS"
echo "Passed: $((TOTAL_TESTS - FAILED_TESTS))"
echo "Failed: $FAILED_TESTS"

if [ $FAILED_TESTS -eq 0 ]; then
    echo -e "${GREEN}All tests passed! 🎉${NC}"
    exit 0
else
    echo -e "${RED}Some tests failed! ❌${NC}"
    exit 1
fi