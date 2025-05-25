# Spider - Web Image Scraper

Spider is a command-line tool that extracts images from websites, with support for recursive crawling.

## Features

- Download images from a specified URL
- Recursive downloading with configurable depth
- Support for common image formats (JPG, JPEG, PNG, GIF, BMP)
- Customizable download location

## Building

To build the spider program, you need GHC (Glasgow Haskell Compiler) and Cabal.

```bash
cd ex01
cabal build
```

## Usage

```
spider [-r] [-l N] [-p PATH] URL
```

### Options

- `URL`: The website URL to scrape
- `-r`: Enable recursive downloading of images
- `-l N`: Set the maximum depth level for recursive download (default: 5)
- `-p PATH`: Specify the path where downloaded files will be saved (default: ./data/)

### Examples

Download images from a single page:
```bash
./spider https://example.com
```

Download images recursively with default depth (5):
```bash
./spider -r https://example.com
```

Download images recursively with custom depth (3):
```bash
./spider -r -l 3 https://example.com
```

Download images to a custom directory:
```bash
./spider -p ./images/ https://example.com
```

## Implementation

Spider is implemented in Haskell, using a functional programming approach. It uses:

- `http-conduit` for HTTP requests
- `tagsoup` for HTML parsing
- `optparse-applicative` for command-line argument parsing
- Pure functions and immutability where possible