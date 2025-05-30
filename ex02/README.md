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

- **JPEG** (.jpg, .jpeg) - EXIF metadata and dimensions.
- **PNG** (.png) - Basic metadata (dimensions).
- **GIF** (.gif) - Basic metadata (dimensions).
- **BMP** (.bmp) - Basic metadata (dimensions).

## Building

```bash
cd ex02
cabal build
cabal run scorpion -- [files...]
```

## Output Format

The tool displays:

1.  **File Information**
    -   File name
    -   File size (human-readable)
    -   File type
    -   Image dimensions

2.  **EXIF Data** (when available, primarily for JPEG)
    -   Camera make and model
    -   Date/time information (requires full implementation)
    -   Camera settings (ISO, aperture, etc.)
    -   GPS coordinates (requires full GPS IFD parsing)

3.  **Raw Metadata**
    -   File signature (first 8 bytes)
    -   Additional technical details (e.g., raw file size)

## Architecture

The project follows functional programming principles and utilizes standard Haskell libraries (as per the Haskell 98 Report and common packages like `bytestring` and `binary`):

-   **Types.hs**: Defines immutable data structures (algebraic data types) for `ImageMetadata`, `FileMetadata`, and `ExifData`.
-   **Args.hs**: Handles command-line argument parsing using `optparse-applicative`.
-   **ExifParser.hs**: Implements binary EXIF data parsing using `Data.Binary.Get` for monadic parsing of `ByteString` data.
-   **MetadataExtractor.hs**: Extracts general file metadata and orchestrates format-specific dimension parsing.
-   **Core.hs**: Contains the main business logic, including file processing and formatting the output.
-   **Main.hs**: Serves as the entry point for the executable.

## Dependencies

-   `base` - Core Haskell libraries (conforming to Haskell 98 Standard Prelude, e.g., `Data.Char`, `System.IO`).
-   `optparse-applicative` - Command line parsing.
-   `text` - For `Data.Text` type.
-   `bytestring` - For strict and lazy `ByteString` types.
-   `binary` - For `Data.Binary.Get` monadic parser.
-   `filepath` - File path manipulation utilities.
-   `directory` - File system operations (e.g., `getFileSize`, part of Haskell 98 libraries).
-   `time` - For `UTCTime` (part of Haskell 98 libraries).
-   `containers` - For `Data.Map`.
-   `vector` - (Not directly used in core metadata parsing, but listed as a general dependency).

## Metadata Implementation Details

The `scorpion` program extracts metadata by parsing the binary structure of image files. This process is primarily handled by `MetadataExtractor.hs` and `ExifParser.hs`.

### General File Metadata (`MetadataExtractor.hs`)

For all supported image formats (JPEG, PNG, GIF, BMP), the following basic metadata is extracted:

-   **File Name**: Derived from the input `FilePath` using `System.FilePath.takeFileName`.
-   **File Size**: Queried using `System.Directory.getFileSize`.
-   **File Type**: Inferred from the file extension.
-   **Image Dimensions (Width x Height)**: Each format requires a specific parsing strategy to locate width and height information, typically stored in its header.
-   **Raw Metadata**: Currently includes the initial bytes of the file (often containing the magic number or signature) and the raw file size.

### EXIF Data Parsing (`ExifParser.hs` - Primarily for JPEG)

EXIF (Exchangeable image file format) is a standard for storing metadata in digital image files, especially JPEGs. `ExifParser.hs` implements the logic to decode this information.

1.  **EXIF Segment Identification**: JPEGs store EXIF data within an **APP1 Application Segment** (marker `0xFFE1`). The parser specifically looks for this segment starting with the ASCII identifier "Exif\0\0".
2.  **TIFF Header and Byte Order**: The EXIF data within the APP1 segment follows a TIFF (Tagged Image File Format) structure. The first two bytes of this structure indicate the **byte order** (endianness): `II` for Little Endian or `MM` for Big Endian. This is crucial for correctly interpreting multi-byte values.
3.  **Image File Directory (IFD)**: The core of TIFF/EXIF data is the IFD, a table of metadata entries. Each entry, or **tag**, consists of:
    -   A 2-byte **Tag ID** (e.g., `0x0110` for Camera Model).
    -   A 2-byte **Data Type** (e.g., ASCII, SHORT, LONG, RATIONAL, UNDEFINED).
    -   A 4-byte **Count** of values of the specified data type.
    -   A 4-byte **Value Offset** which either contains the value itself (if it fits) or an offset within the EXIF data block where the value is stored.
4.  **IFD Chaining**: An IFD can point to other IFDs (e.g., Exif SubIFD, GPS Info IFD). The current parser reads IFD0 and recognizes some common tags.
5.  **Tag Interpretation**: The `updateExifData` function maps known Tag IDs to fields in the `ExifData` structure. This involves interpreting the data type and count to correctly parse values (e.g., integers, or simplified rationals).

### Format-Specific Dimension Extraction (`MetadataExtractor.hs`)

The `extractDimensions` function routes to format-specific parsers based on file signatures (magic numbers):

-   **JPEG (`extractJPEGDimensions`)**: Identifies a **Start of Frame (SOF)** marker (typically `SOF0` - `0xFFC0`). Image height and width are read from fixed offsets relative to this marker.
-   **PNG (`extractPNGDimensions`)**: Parses the **IHDR (Image Header) chunk**, which is the first chunk after the 8-byte PNG signature. Width and height are 4-byte big-endian integers within this chunk.
-   **GIF (`extractGIFDimensions`)**: Reads the 6-byte header ("GIF87a" or "GIF89a"). Width and height are 2-byte little-endian values at offsets 6 and 8 respectively.
-   **BMP (`extractBMPDimensions`)**: Checks for the "BM" signature. Width and height are typically 4-byte little-endian values within the **DIB (Device-Independent Bitmap) header** (commonly BITMAPINFOHEADER), at offsets +18 and +22 from the file start.

### Current Limitations and Adherence to Specifications

While `scorpion` aims to extract useful metadata, a full, conformant implementation of specifications like EXIF is extensive. Key areas of simplification or partial implementation include:

-   **EXIF Data Types**: Full parsing of all EXIF data types (e.g., `SRATIONAL`, `ASCII` strings with varying lengths and offsets, arrays of values) is complex. The current `extractString` is a placeholder, and rational parsing is simplified.
-   **EXIF Date/Time**: EXIF timestamps require parsing specific string formats (e.g., `YYYY:MM:DD HH:MM:SS`) into a Haskell `UTCTime` or similar, which is not yet fully implemented.
-   **GPS Info IFD**: Complete parsing of GPS data involves reading and interpreting all tags within the GPS Info IFD, including converting rational values for latitude/longitude and handling reference fields. This is currently simplified to only recognizing a pointer to this IFD.
-   **MakerNotes and Interoperability IFD**: Many cameras store vendor-specific data in MakerNotes, and an Interoperability IFD can link to further information. These are not currently parsed.
-   **Color Depth & Compression**: These `FileMetadata` fields are not yet populated.

Future development could involve more comprehensive parsing of these elements by more closely following the detailed EXIF specification (e.g., CIPA DC-008) and image format standards.

*(This documentation refers to concepts and libraries consistent with the Haskell 98 Report and common Haskell development practices. For official Haskell language and library definitions, see [https://www.haskell.org/onlinereport/](https://www.haskell.org/onlinereport/)).*