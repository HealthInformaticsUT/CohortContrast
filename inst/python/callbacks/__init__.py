"""
Callbacks package for ContrastViewer.

Due to the nature of Dash callbacks requiring tight coupling with
app state and stores, the callbacks remain in app.py for now.

This package provides callback utilities and helper functions.
"""

from .register import (
    setup_clientside_callbacks,
    get_callback_dependencies,
)

__all__ = [
    'setup_clientside_callbacks',
    'get_callback_dependencies',
]
