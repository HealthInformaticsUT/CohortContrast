#!/bin/bash
# =============================================================
# Offline Installation Script for CohortContrast Viewer Python Dependencies
# =============================================================
#
# Usage:
#   1. Copy this entire 'packages' folder to your offline server
#   2. Run: bash packages/INSTALL.sh
#
# Requirements:
#   - Python 3.9+ on Linux x86_64
#   - pip installed
#
# Supports both:
#   - Compressed: linux_x86_64.zip
#   - Uncompressed: linux_x86_64/ folder with .whl files
# =============================================================

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CLEANUP_TEMP=false
PACKAGES_DIR=""

# Detect platform
PLATFORM=$(python3 -c "import sysconfig; print(sysconfig.get_platform())" 2>/dev/null || echo "unknown")
PYTHON_VERSION=$(python3 -c "import sys; print(f'{sys.version_info.major}.{sys.version_info.minor}')" 2>/dev/null || echo "unknown")

echo "=============================================="
echo "CohortContrast Viewer Offline Package Installer"
echo "=============================================="
echo "Platform: $PLATFORM"
echo "Python:   $PYTHON_VERSION"
echo ""

# Function to cleanup on exit
cleanup() {
    if [ "$CLEANUP_TEMP" = true ] && [ -n "$PACKAGES_DIR" ] && [ -d "$PACKAGES_DIR" ]; then
        echo ""
        echo "Cleaning up temporary files..."
        rm -rf "$PACKAGES_DIR"
        echo "Cleanup complete."
    fi
}
trap cleanup EXIT

# Check for packages - prefer zip, fallback to folder
if [ -f "$SCRIPT_DIR/linux_x86_64.zip" ]; then
    echo "Found compressed packages: linux_x86_64.zip"
    echo "Extracting..."
    
    # Create temp directory for extraction
    PACKAGES_DIR="$SCRIPT_DIR/.temp_packages_$$"
    mkdir -p "$PACKAGES_DIR"
    unzip -q "$SCRIPT_DIR/linux_x86_64.zip" -d "$PACKAGES_DIR"
    CLEANUP_TEMP=true
    
    echo "Extracted $(ls "$PACKAGES_DIR"/*.whl 2>/dev/null | wc -l) wheel files"
    
elif [ -d "$SCRIPT_DIR/linux_x86_64" ]; then
    PACKAGES_DIR="$SCRIPT_DIR/linux_x86_64"
    echo "Found uncompressed packages: linux_x86_64/"
    
elif ls "$SCRIPT_DIR"/*.whl 1>/dev/null 2>&1; then
    PACKAGES_DIR="$SCRIPT_DIR"
    echo "Found wheel files in current directory"
    
else
    echo "ERROR: No wheel packages found!"
    echo ""
    echo "Expected one of:"
    echo "  - $SCRIPT_DIR/linux_x86_64.zip"
    echo "  - $SCRIPT_DIR/linux_x86_64/"
    echo ""
    echo "Download packages with:"
    echo "  pip download -d linux_x86_64 --platform manylinux2014_x86_64 \\"
    echo "      --python-version 39 --only-binary=:all: \\"
    echo "      dash dash-bootstrap-components pandas numpy scipy scikit-learn"
    exit 1
fi

echo "Packages: $PACKAGES_DIR"
echo "Found $(ls "$PACKAGES_DIR"/*.whl 2>/dev/null | wc -l) wheel files"
echo ""

# Install packages
echo "Installing packages..."
pip3 install --user --no-index --find-links="$PACKAGES_DIR" \
    dash dash-bootstrap-components dash-ag-grid \
    pandas pyarrow numpy scipy \
    plotly diskcache psutil multiprocess \
    scikit-learn scikit-learn-extra

echo ""
echo "=============================================="
echo "Installation complete!"
echo "=============================================="
echo ""
echo "Next steps in R:"
echo "  library(CohortContrast)"
echo "  configurePython(python_path = '/usr/bin/python3', create_venv = FALSE)"
echo "  checkPythonDeps()  # Verify installation"
echo "  runCohortContrastViewer(data_dir = 'results_parquet')"
echo ""
