#!/bin/bash
# =============================================================
# Create Compressed Package Archives for CohortContrast Viewer
# =============================================================
#
# This script compresses the wheel files into zip archives for
# more efficient distribution. Run this before committing to git.
#
# Usage:
#   cd packages/
#   bash create_zip.sh
#
# Output:
#   - linux_x86_64.zip (compressed wheel files)
#   - Original wheel files can then be removed from git
# =============================================================

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

echo "=============================================="
echo "Creating Package Archives"
echo "=============================================="

# Create zip for linux_x86_64
if [ -d "linux_x86_64" ] && ls linux_x86_64/*.whl 1>/dev/null 2>&1; then
    echo "Compressing linux_x86_64..."
    cd linux_x86_64
    zip -9 ../linux_x86_64.zip *.whl
    cd ..
    echo "Created: linux_x86_64.zip ($(du -h linux_x86_64.zip | cut -f1))"
    
    # Show size comparison
    ORIG_SIZE=$(du -sh linux_x86_64 | cut -f1)
    ZIP_SIZE=$(du -h linux_x86_64.zip | cut -f1)
    echo "Original folder size: $ORIG_SIZE"
    echo "Compressed size: $ZIP_SIZE"
    echo ""
    echo "You can now remove the linux_x86_64/ folder from git:"
    echo "  git rm -r linux_x86_64/"
    echo "  git add linux_x86_64.zip"
else
    echo "No linux_x86_64 wheel files found."
fi

# Add support for other platforms here if needed
# Example for macOS:
# if [ -d "macos_arm64" ] && ls macos_arm64/*.whl 1>/dev/null 2>&1; then
#     echo "Compressing macos_arm64..."
#     cd macos_arm64
#     zip -9 ../macos_arm64.zip *.whl
#     cd ..
#     echo "Created: macos_arm64.zip"
# fi

echo ""
echo "=============================================="
echo "Done!"
echo "=============================================="
