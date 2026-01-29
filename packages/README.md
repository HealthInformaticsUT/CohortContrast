# Offline Python Packages for CohortContrast Viewer

This directory contains pre-downloaded Python wheel files for offline installation of the CohortContrast Viewer dashboard.

## For Package Maintainers

### Creating Compressed Archives

To reduce repository size, compress the wheel files before committing:

```bash
cd packages/
bash create_zip.sh
```

This creates `linux_x86_64.zip` from the wheel files. You can then remove the folder:

```bash
git rm -r linux_x86_64/
git add linux_x86_64.zip
git commit -m "Compress offline packages"
```

### Downloading New Packages

To download/update packages for a platform:
```bash
pip download -d linux_x86_64 \
    --platform manylinux2014_x86_64 \
    --python-version 39 \
    --only-binary=:all: \
    dash dash-bootstrap-components dash-ag-grid \
    pandas pyarrow numpy scipy \
    plotly diskcache psutil multiprocess \
    scikit-learn scikit-learn-extra
```

## For End Users

### Option 1: Install from R (Recommended)

```r
library(CohortContrast)

# Configure Python (no virtual environment for offline)
configurePython(python_path = "/usr/bin/python3", create_venv = FALSE)

# Install from bundled packages
installPythonDepsOffline()

# Verify installation
checkPythonDeps()
```

### Option 2: Install from Command Line

```bash
cd /path/to/CohortContrast/packages
bash INSTALL.sh
```

## Supported Platforms

- `linux_x86_64`: Linux on x86_64 architecture (Python 3.9+)

Additional platforms can be added by downloading wheels for that platform and creating a corresponding zip file.
