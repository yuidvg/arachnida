#!/bin/bash

# Script to analyze the test structure and show expected image counts
# This helps set proper test expectations

echo "🔍 Spider Test Structure Analysis"
echo "================================="
echo ""

# Count images per level
echo "📊 Images per level:"
echo "Level 0 (Root):  $(ls test/images/ 2>/dev/null | wc -l) images"
echo "Level 1:         $(find test/level1 -name "*.jpg" -o -name "*.jpeg" -o -name "*.png" -o -name "*.gif" -o -name "*.bmp" 2>/dev/null | wc -l) images"
echo "Level 2:         $(find test/level2 -name "*.jpg" -o -name "*.jpeg" -o -name "*.png" -o -name "*.gif" -o -name "*.bmp" 2>/dev/null | wc -l) images"
echo "Level 3:         $(find test/level3 -name "*.jpg" -o -name "*.jpeg" -o -name "*.png" -o -name "*.gif" -o -name "*.bmp" 2>/dev/null | wc -l) images"
echo "Level 4:         $(find test/level4 -name "*.jpg" -o -name "*.jpeg" -o -name "*.png" -o -name "*.gif" -o -name "*.bmp" 2>/dev/null | wc -l) images"
echo "Level 5:         $(find test/level5 -name "*.jpg" -o -name "*.jpeg" -o -name "*.png" -o -name "*.gif" -o -name "*.bmp" 2>/dev/null | wc -l) images"
echo "Level 6:         $(find test/level6 -name "*.jpg" -o -name "*.jpeg" -o -name "*.png" -o -name "*.gif" -o -name "*.bmp" 2>/dev/null | wc -l) images"
echo ""

# Calculate cumulative counts
ROOT=$(ls test/images/ 2>/dev/null | wc -l)
L1=$(find test/level1 -name "*.jpg" -o -name "*.jpeg" -o -name "*.png" -o -name "*.gif" -o -name "*.bmp" 2>/dev/null | wc -l)
L2=$(find test/level2 -name "*.jpg" -o -name "*.jpeg" -o -name "*.png" -o -name "*.gif" -o -name "*.bmp" 2>/dev/null | wc -l)
L3=$(find test/level3 -name "*.jpg" -o -name "*.jpeg" -o -name "*.png" -o -name "*.gif" -o -name "*.bmp" 2>/dev/null | wc -l)
L4=$(find test/level4 -name "*.jpg" -o -name "*.jpeg" -o -name "*.png" -o -name "*.gif" -o -name "*.bmp" 2>/dev/null | wc -l)
L5=$(find test/level5 -name "*.jpg" -o -name "*.jpeg" -o -name "*.png" -o -name "*.gif" -o -name "*.bmp" 2>/dev/null | wc -l)
L6=$(find test/level6 -name "*.jpg" -o -name "*.jpeg" -o -name "*.png" -o -name "*.gif" -o -name "*.bmp" 2>/dev/null | wc -l)

echo "📈 Theoretical cumulative counts (if all images are found):"
echo "Depth 1: $((ROOT + L1)) images (Root + Level1)"
echo "Depth 2: $((ROOT + L1 + L2)) images (Root + Level1-2)"
echo "Depth 3: $((ROOT + L1 + L2 + L3)) images (Root + Level1-3)"
echo "Depth 4: $((ROOT + L1 + L2 + L3 + L4)) images (Root + Level1-4)"
echo "Depth 5: $((ROOT + L1 + L2 + L3 + L4 + L5)) images (Root + Level1-5)"
echo "Depth 6: $((ROOT + L1 + L2 + L3 + L4 + L5 + L6)) images (Root + Level1-6)"
echo ""

echo "⚠️  Note: Actual spider results may differ due to:"
echo "   - Unreachable pages (broken links)"
echo "   - Duplicate images (same image referenced multiple times)"
echo "   - External images (from picsum.photos, etc.)"
echo "   - Missing image files"
echo "   - URL resolution issues"
echo ""

echo "🎯 Recommended test expectations (with tolerance):"
echo "Depth 1: 20-40 images   (theoretical: $((ROOT + L1)))"
echo "Depth 2: 45-65 images   (theoretical: $((ROOT + L1 + L2)))"
echo "Depth 3: 65-85 images   (theoretical: $((ROOT + L1 + L2 + L3)))"
echo "Depth 4: 85-105 images  (theoretical: $((ROOT + L1 + L2 + L3 + L4)))"
echo "Depth 5: 110-130 images (theoretical: $((ROOT + L1 + L2 + L3 + L4 + L5)))"
echo "Depth 6: 135-155 images (theoretical: $((ROOT + L1 + L2 + L3 + L4 + L5 + L6)))"