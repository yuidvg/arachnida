#!/bin/bash

# Simple HTTP server for testing the spider
# This serves the test files locally

PORT=${1:-8000}

echo "🕷️  Starting Spider Test Server"
echo "📁 Serving directory: $(pwd)"
echo "🌐 Server URL: http://localhost:$PORT"
echo "🎯 Test URL: http://localhost:$PORT/index.html"
echo ""
echo "Press Ctrl+C to stop the server"
echo ""

# Check if Python 3 is available
if command -v python3 &> /dev/null; then
    echo "Using Python 3 HTTP server..."
    python3 -m http.server $PORT
elif command -v python &> /dev/null; then
    echo "Using Python 2 HTTP server..."
    python -m SimpleHTTPServer $PORT
else
    echo "❌ Error: Python not found. Please install Python to run the test server."
    echo "Alternative: Use any other HTTP server to serve this directory."
    exit 1
fi