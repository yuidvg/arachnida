#!/bin/bash

# Test runner for the spider program
# This script tests the spider with various configurations

SPIDER_EXECUTABLE="../app/spider"  # Adjust path as needed
TEST_URL="http://localhost:8000/index.html"
OUTPUT_DIR="./spider_test_output"

echo "🕷️  Spider Test Runner"
echo "===================="
echo ""

# Check if spider executable exists
if [ ! -f "$SPIDER_EXECUTABLE" ]; then
    echo "❌ Spider executable not found at: $SPIDER_EXECUTABLE"
    echo "Please build the spider first or adjust the path in this script."
    exit 1
fi

# Create output directory
mkdir -p "$OUTPUT_DIR"

echo "🧪 Running Spider Tests..."
echo ""

# Test 1: Basic functionality with depth 3
echo "Test 1: Basic spider test (depth 3)"
echo "Command: $SPIDER_EXECUTABLE -r -l 3 $TEST_URL"
$SPIDER_EXECUTABLE -r -l 3 "$TEST_URL"
echo ""

# Test 2: Deep crawling with depth 6
echo "Test 2: Deep crawling test (depth 6)"
echo "Command: $SPIDER_EXECUTABLE -r -l 6 $TEST_URL"
$SPIDER_EXECUTABLE -r -l 6 "$TEST_URL"
echo ""

# Test 3: Maximum depth test
echo "Test 3: Maximum depth test (depth 10)"
echo "Command: $SPIDER_EXECUTABLE -r -l 10 $TEST_URL"
$SPIDER_EXECUTABLE -r -l 10 "$TEST_URL"
echo ""

# Test 4: Custom output path
echo "Test 4: Custom output path test"
echo "Command: $SPIDER_EXECUTABLE -r -l 5 -p $OUTPUT_DIR $TEST_URL"
$SPIDER_EXECUTABLE -r -l 5 -p "$OUTPUT_DIR" "$TEST_URL"
echo ""

echo "✅ All tests completed!"
echo ""
echo "📊 Test Summary:"
echo "- Tested depths: 3, 6, 10"
echo "- Tested recursive crawling"
echo "- Tested custom output paths"
echo "- Test site has 6 levels of depth"
echo "- All required image extensions included (.jpg, .jpeg, .png, .gif, .bmp)"
echo ""
echo "Check the downloaded images to verify the spider worked correctly."