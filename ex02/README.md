# Scorpion - Image Metadata Extractor

Scorpion is a command-line tool that extracts and displays EXIF and metadata from image files.

## Features

- Extracts basic file information (name, size, type, dimensions)
- Parses EXIF data from JPEG files
- Supports multiple image formats: JPEG, PNG, GIF, BMP
- Displays metadata in a human-readable format
- Functional programming approach with immutable data structures

## Usage

```bash
./scorpion FILE1 [FILE2 ...]
```

### Examples

```bash
# Analyze a single image
./scorpion photo.jpg

# Analyze multiple images
./scorpion photo1.jpg photo2.png image.gif

# Analyze all images in current directory
./scorpion *.jpg *.png
```

## Supported Formats

- **JPEG** (.jpg, .jpeg) - Full EXIF support
- **PNG** (.png) - Basic metadata
- **GIF** (.gif) - Basic metadata
- **BMP** (.bmp) - Basic metadata

## Building

```bash
cd ex02
cabal build
cabal run scorpion -- [files...]
```

## Output Format

The tool displays:

1. **File Information**
   - File name
   - File size (human-readable)
   - File type
   - Image dimensions

2. **EXIF Data** (when available)
   - Camera make and model
   - Date/time information
   - Camera settings (ISO, aperture, etc.)
   - GPS coordinates (if present)

3. **Raw Metadata**
   - File signature
   - Additional technical details

## Architecture

The project follows functional programming principles:

- **Types.hs** - Immutable data structures
- **Args.hs** - Command line argument parsing
- **ExifParser.hs** - Binary EXIF data parsing
- **MetadataExtractor.hs** - File metadata extraction
- **Core.hs** - Business logic and formatting
- **Main.hs** - Entry point

## Dependencies

- `base` - Core Haskell libraries
- `optparse-applicative` - Command line parsing
- `text` - Text processing
- `bytestring` - Binary data handling
- `binary` - Binary parsing
- `filepath` - File path utilities
- `directory` - File system operations
- `time` - Date/time handling
- `containers` - Data structures
- `vector` - Efficient arrays