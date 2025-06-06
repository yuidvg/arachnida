# Spider Test Suite

This directory contains a comprehensive test environment for the `spider` program.

## 🎯 Test Features

- **✅ Depth > 5**: The test site has **6 levels** of depth (Level 0 to Level 6)
- **✅ All Extensions**: Contains all required image extensions:
  - `.jpg` - JPEG images
  - `.jpeg` - JPEG images (alternative extension)
  - `.png` - PNG images
  - `.gif` - GIF images
  - `.bmp` - BMP images
- **✅ Local Testing**: Can be run completely locally
- **✅ Complex Structure**: Multiple paths, cross-references, and recursive links

## 📁 Directory Structure

```
test/
├── index.html              # Level 0 (Root)
├── level1/                 # Level 1
│   ├── page1.html
│   ├── page2.html
│   ├── gallery.html
│   └── images/
├── level2/                 # Level 2
│   ├── page1.html
│   ├── page2.html
│   ├── gallery.html
│   └── images/
├── level3/                 # Level 3
│   ├── deep.html
│   ├── gallery.html
│   └── images/
├── level4/                 # Level 4
│   ├── deep.html
│   ├── gallery.html
│   └── images/
├── level5/                 # Level 5
│   ├── deep.html
│   ├── final.html
│   ├── gallery.html
│   └── images/
├── level6/                 # Level 6 (Deepest)
│   ├── final.html
│   ├── ultimate.html
│   └── images/
└── images/                 # Root level images
```

## 🚀 Quick Start

### 1. Setup Test Images
```bash
./create_test_images.sh
```

### 2. Start Local Server
```bash
./start_server.sh [port]
# Default port is 8000
# Access at: http://localhost:8000/index.html
```

### 3. Run Spider Tests
```bash
./test_spider.sh
```

## 🧪 Manual Testing

You can also test manually:

```bash
# Basic test (depth 3)
../app/spider -r -l 3 http://localhost:8000/index.html

# Deep test (depth 6)
../app/spider -r -l 6 http://localhost:8000/index.html

# Maximum depth test
../app/spider -r -l 10 http://localhost:8000/index.html

# Custom output directory
../app/spider -r -l 5 -p ./output http://localhost:8000/index.html
```

## 📊 Expected Results

When running with depth 6 or higher, your spider should:

1. **Download images from all levels** (0-6)
2. **Find all image extensions** (.jpg, .jpeg, .png, .gif, .bmp)
3. **Follow recursive links** between pages
4. **Handle cross-references** and multiple paths to the same content
5. **Respect depth limits** when specified

## 🔍 Verification

After running the spider, check:

- [ ] Images downloaded from all depth levels
- [ ] All 5 required extensions present
- [ ] No duplicate downloads (if implemented)
- [ ] Proper directory structure (if using -p option)
- [ ] Depth limiting works correctly

## 🌐 Alternative Testing

If you prefer not to use the local server, you can also test with file:// URLs:

```bash
../app/spider -r -l 6 file://$(pwd)/index.html
```

## 📝 Notes

- The test uses placeholder text files with appropriate extensions for simplicity
- Each level contains multiple pages with different linking patterns
- External URLs (picsum.photos) are included for additional testing
- The structure is designed to test edge cases and complex navigation patterns