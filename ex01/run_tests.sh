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
TEST_BASE_DIR="data/test_runs"
FAILED_TESTS=0
TOTAL_TESTS=0

# Cleanup function
cleanup() {
    echo -e "${YELLOW}Cleaning up...${NC}"

    # Kill server if running
    if [ ! -z "$SERVER_PID" ]; then
        echo "Stopping server (PID: $SERVER_PID)"
        kill $SERVER_PID 2>/dev/null
        wait $SERVER_PID 2>/dev/null
    fi

    # Kill any remaining python servers on our port
    pkill -f "python.*http.server.*$SERVER_PORT" 2>/dev/null

    # Remove test directories
    if [ -d "$TEST_BASE_DIR" ]; then
        echo "Removing test directories: $TEST_BASE_DIR"
        rm -rf "$TEST_BASE_DIR"
    fi

    echo -e "${GREEN}Cleanup complete${NC}"
}

# Set up trap for cleanup on exit
trap cleanup EXIT

# Function to start server
start_server() {
    echo -e "${BLUE}Starting test server on port $SERVER_PORT...${NC}"
    cd test
    python3 -m http.server $SERVER_PORT &
    SERVER_PID=$!
    cd ..

    # Wait for server to start
    echo "Waiting for server to start..."
    for i in {1..10}; do
        if curl -s "http://localhost:$SERVER_PORT" > /dev/null 2>&1; then
            echo -e "${GREEN}Server started successfully (PID: $SERVER_PID)${NC}"
            return 0
        fi
        sleep 1
    done

    echo -e "${RED}Failed to start server${NC}"
    return 1
}

# Function to run a test case
run_test() {
    local test_name="$1"
    local depth="$2"
    local expected_min_images="$3"
    local expected_extensions="$4"
    local description="$5"

    TOTAL_TESTS=$((TOTAL_TESTS + 1))
    echo -e "\n${BLUE}=== Test $TOTAL_TESTS: $test_name ===${NC}"
    echo "Description: $description"
    echo "Depth: $depth, Expected min images: $expected_min_images"

    # Create unique test directory
    local test_dir="$TEST_BASE_DIR/test_${TOTAL_TESTS}_$(echo $test_name | tr ' ' '_')"
    mkdir -p "$test_dir"

    # Run spider with custom data directory
    echo "Running: cabal exec spider -- -r -l $depth http://localhost:$SERVER_PORT"

    # Temporarily change data directory by creating symlink
    if [ -d "data" ]; then
        mv data data_backup
    fi
    ln -s "$test_dir" data

    # Run the spider
    local start_time=$(date +%s)
    if cabal exec spider -- -r -l $depth http://localhost:$SERVER_PORT > /dev/null 2>&1; then
        local end_time=$(date +%s)
        local duration=$((end_time - start_time))

        # Restore original data directory
        rm data
        if [ -d "data_backup" ]; then
            mv data_backup data
        fi

        # Check results
        local image_count=$(ls "$test_dir" 2>/dev/null | wc -l)
        echo "Downloaded $image_count images in ${duration}s"

        # Check if we have minimum expected images
        if [ "$image_count" -ge "$expected_min_images" ]; then
            echo -e "${GREEN}✓ Image count check passed ($image_count >= $expected_min_images)${NC}"

            # Check extensions
            local extensions_found=""
            for ext in jpg jpeg png gif bmp; do
                local count=$(ls "$test_dir"/*.$ext 2>/dev/null | wc -l)
                if [ "$count" -gt 0 ]; then
                    extensions_found="$extensions_found $ext($count)"
                fi
            done

            echo "Extensions found:$extensions_found"

            # Check if all expected extensions are present
            local all_extensions_found=true
            for ext in $expected_extensions; do
                if ! ls "$test_dir"/*.$ext > /dev/null 2>&1; then
                    all_extensions_found=false
                    echo -e "${RED}✗ Missing extension: $ext${NC}"
                fi
            done

            if [ "$all_extensions_found" = true ]; then
                echo -e "${GREEN}✓ All expected extensions found${NC}"
                echo -e "${GREEN}✓ Test $test_name PASSED${NC}"
            else
                echo -e "${RED}✗ Test $test_name FAILED (missing extensions)${NC}"
                FAILED_TESTS=$((FAILED_TESTS + 1))
            fi
        else
            echo -e "${RED}✗ Test $test_name FAILED (insufficient images: $image_count < $expected_min_images)${NC}"
            FAILED_TESTS=$((FAILED_TESTS + 1))
        fi
    else
        # Restore original data directory on failure
        rm data
        if [ -d "data_backup" ]; then
            mv data_backup data
        fi

        echo -e "${RED}✗ Test $test_name FAILED (spider execution failed)${NC}"
        FAILED_TESTS=$((FAILED_TESTS + 1))
    fi
}

# Main execution
echo -e "${BLUE}=== Spider Automated Test Suite ===${NC}"
echo "Building spider..."

# Build the project
if ! cabal build; then
    echo -e "${RED}Failed to build spider${NC}"
    exit 1
fi

# Start server
if ! start_server; then
    exit 1
fi

# Create test base directory
mkdir -p "$TEST_BASE_DIR"

# Test Cases
echo -e "\n${YELLOW}Running test cases...${NC}"

# Test 1: Shallow crawl (depth 1)
run_test "Shallow Crawl" 1 8 "jpg jpeg png gif bmp" "Test basic functionality with depth 1"

# Test 2: Medium crawl (depth 3)
run_test "Medium Crawl" 3 25 "jpg jpeg png gif bmp" "Test recursive crawling with depth 3"

# Test 3: Deep crawl (depth 6) - Full test
run_test "Deep Crawl" 6 80 "jpg jpeg png gif bmp" "Test maximum depth crawling through all 6 levels"

# Test 4: Root only (depth 0)
run_test "Root Only" 0 5 "jpg jpeg png gif bmp" "Test root level only (no recursion)"

# Test 5: Very shallow (depth 2)
run_test "Level 2 Crawl" 2 15 "jpg jpeg png gif bmp" "Test crawling to level 2"

# Summary
echo -e "\n${BLUE}=== Test Summary ===${NC}"
echo "Total tests: $TOTAL_TESTS"
echo "Passed: $((TOTAL_TESTS - FAILED_TESTS))"
echo "Failed: $FAILED_TESTS"

if [ "$FAILED_TESTS" -eq 0 ]; then
    echo -e "${GREEN}🎉 All tests passed!${NC}"
    exit 0
else
    echo -e "${RED}❌ $FAILED_TESTS test(s) failed${NC}"
    exit 1
fi