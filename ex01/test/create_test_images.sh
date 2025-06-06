#!/bin/bash

# Script to create placeholder images for spider test
# This creates simple text files with appropriate extensions for testing

echo "Creating placeholder images for spider test..."

# Function to create images for a level
create_level_images() {
    local level=$1
    local dir="level$level/images"

    if [ ! -d "$dir" ]; then
        mkdir -p "$dir"
    fi

    cd "$dir"

    # Basic level images
    echo "Level $level Main JPG" > "level$level-main.jpg"
    echo "Level $level Secondary JPEG" > "level$level-secondary.jpeg"
    echo "Level $level Graphic PNG" > "level$level-graphic.png"
    echo "Level $level Motion GIF" > "level$level-motion.gif"
    echo "Level $level Raw BMP" > "level$level-raw.bmp"

    # Alternative images
    echo "Level $level Alt 1 JPG" > "level$level-alt1.jpg"
    echo "Level $level Alt 2 JPEG" > "level$level-alt2.jpeg"
    echo "Level $level Icon PNG" > "level$level-icon.png"
    echo "Level $level Animation GIF" > "level$level-anim.gif"
    echo "Level $level Bitmap BMP" > "level$level-bitmap.bmp"

    # Gallery images
    for i in {01..06}; do
        case $((i % 5)) in
            1) echo "Gallery Level $level Image $i" > "gallery$level-$i.jpg";;
            2) echo "Gallery Level $level Image $i" > "gallery$level-$i.jpeg";;
            3) echo "Gallery Level $level Image $i" > "gallery$level-$i.png";;
            4) echo "Gallery Level $level Image $i" > "gallery$level-$i.gif";;
            0) echo "Gallery Level $level Image $i" > "gallery$level-$i.bmp";;
        esac
    done

    cd ../..
}

# Create Level 2-6 images
for level in {2..6}; do
    create_level_images $level
done

# Special images for deeper levels
cd level3/images
echo "Level 3 Deep 1" > level3-deep1.jpg
echo "Level 3 Deep 2" > level3-deep2.jpeg
echo "Level 3 Deep 3" > level3-deep3.png
echo "Level 3 Deep 4" > level3-deep4.gif
echo "Level 3 Deep 5" > level3-deep5.bmp
cd ../..

cd level4/images
echo "Level 4 Ultra 1" > level4-ultra1.jpg
echo "Level 4 Ultra 2" > level4-ultra2.jpeg
echo "Level 4 Ultra 3" > level4-ultra3.png
echo "Level 4 Ultra 4" > level4-ultra4.gif
echo "Level 4 Ultra 5" > level4-ultra5.bmp
cd ../..

cd level5/images
echo "Level 5 Extreme 1" > level5-extreme1.jpg
echo "Level 5 Extreme 2" > level5-extreme2.jpeg
echo "Level 5 Extreme 3" > level5-extreme3.png
echo "Level 5 Extreme 4" > level5-extreme4.gif
echo "Level 5 Extreme 5" > level5-extreme5.bmp

echo "Level 5 Final 1" > level5-final1.jpg
echo "Level 5 Final 2" > level5-final2.jpeg
echo "Level 5 Final 3" > level5-final3.png
echo "Level 5 Final 4" > level5-final4.gif
echo "Level 5 Final 5" > level5-final5.bmp

for i in {01..06}; do
    case $((i % 5)) in
        1) echo "Gallery Level 5 Image $i" > "gallery5-$i.jpg";;
        2) echo "Gallery Level 5 Image $i" > "gallery5-$i.jpeg";;
        3) echo "Gallery Level 5 Image $i" > "gallery5-$i.png";;
        4) echo "Gallery Level 5 Image $i" > "gallery5-$i.gif";;
        0) echo "Gallery Level 5 Image $i" > "gallery5-$i.bmp";;
    esac
done
cd ../..

cd level6/images
echo "Level 6 Ultimate 1" > level6-ultimate1.jpg
echo "Level 6 Ultimate 2" > level6-ultimate2.jpeg
echo "Level 6 Ultimate 3" > level6-ultimate3.png
echo "Level 6 Ultimate 4" > level6-ultimate4.gif
echo "Level 6 Ultimate 5" > level6-ultimate5.bmp

for i in {01..10}; do
    case $((i % 5)) in
        1) echo "Ultimate Gallery Image $i" > "ultimate-$i.jpg";;
        2) echo "Ultimate Gallery Image $i" > "ultimate-$i.jpeg";;
        3) echo "Ultimate Gallery Image $i" > "ultimate-$i.png";;
        4) echo "Ultimate Gallery Image $i" > "ultimate-$i.gif";;
        0) echo "Ultimate Gallery Image $i" > "ultimate-$i.bmp";;
    esac
done
cd ../..

echo "✅ All placeholder images created successfully!"
echo "📁 Test structure ready with 6 levels of depth"
echo "🖼️  All required image extensions (.jpg, .jpeg, .png, .gif, .bmp) included"
echo "🔗 Multiple linking paths and cross-references created"
echo ""
echo "To test your spider, run it against: http://localhost:8000/index.html"
echo "Or use the file:// protocol with the full path to index.html"