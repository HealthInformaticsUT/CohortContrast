"""Session initialization and idle-cleanup callbacks."""

import os
import time
import uuid
from typing import Dict, Optional, Set

from dash import Input, Output, State, no_update

SESSION_IDLE_TIMEOUT_SECONDS = int(
    os.environ.get("CONTRAST_VIEWER_SESSION_IDLE_TIMEOUT_SECONDS", 3 * 60 * 60)
)
SESSION_CLEANUP_INTERVAL_SECONDS = int(
    os.environ.get("CONTRAST_VIEWER_SESSION_CLEANUP_INTERVAL_SECONDS", 5 * 60)
)

_session_last_seen: Dict[str, float] = {}
_last_cleanup_ts: float = 0.0
_sessions_seen_since_restart: Set[str] = set()


def _log_session_stats(logger_obj, *, background: bool) -> None:
    """Log current session counters."""
    if not background or logger_obj is None:
        return
    logger_obj.info(
        "Session stats: active_sessions=%d, clients_seen_since_restart=%d",
        len(_session_last_seen),
        len(_sessions_seen_since_restart),
    )


def _evict_idle_sessions(
    dashboard_show_state_store: Dict[str, Dict[int, bool]],
    *,
    now_ts: float,
    logger_obj=None,
    background: bool,
) -> None:
    """Evict session-scoped state for sessions idle past timeout."""
    global _last_cleanup_ts
    if now_ts - _last_cleanup_ts < SESSION_CLEANUP_INTERVAL_SECONDS:
        return
    _last_cleanup_ts = now_ts

    expired_sessions = [
        session_id
        for session_id, last_seen_ts in _session_last_seen.items()
        if now_ts - last_seen_ts > SESSION_IDLE_TIMEOUT_SECONDS
    ]
    if not expired_sessions:
        _log_session_stats(logger_obj, background=background)
        return

    for session_id in expired_sessions:
        _session_last_seen.pop(session_id, None)
        prefix = f"{session_id}::"
        stale_keys = [k for k in dashboard_show_state_store if k.startswith(prefix)]
        for key in stale_keys:
            dashboard_show_state_store.pop(key, None)

    if background and logger_obj is not None:
        logger_obj.info(
            "Session cleanup evicted %d idle session(s)", len(expired_sessions)
        )
        _log_session_stats(logger_obj, background=background)


def touch_session(
    session_id: Optional[str],
    dashboard_show_state_store: Dict[str, Dict[int, bool]],
    *,
    logger_obj=None,
    background: bool = True,
) -> None:
    """Mark a session active and opportunistically run idle cleanup."""
    now_ts = time.time()
    if session_id:
        is_new_session = session_id not in _sessions_seen_since_restart
        _sessions_seen_since_restart.add(session_id)
        _session_last_seen[session_id] = now_ts
        if is_new_session and background and logger_obj is not None:
            logger_obj.info("New client session connected: %s", session_id)
            _log_session_stats(logger_obj, background=background)
    _evict_idle_sessions(
        dashboard_show_state_store,
        now_ts=now_ts,
        logger_obj=logger_obj,
        background=background,
    )


def register_session_callbacks(
    app,
    *,
    dashboard_show_state_store: Dict[str, Dict[int, bool]],
    logger_obj=None,
    background: bool = True,
) -> None:
    """Ensure each browser session gets a stable ID and is touched periodically."""

    @app.callback(
        Output("session-id-store", "data"),
        Input("session-id-store", "modified_timestamp"),
        State("session-id-store", "data"),
        prevent_initial_call=False,
    )
    def ensure_session_id(_modified_ts, current_session_id):
        if current_session_id:
            touch_session(
                current_session_id,
                dashboard_show_state_store,
                logger_obj=logger_obj,
                background=background,
            )
            return no_update
        new_session_id = str(uuid.uuid4())
        touch_session(
            new_session_id,
            dashboard_show_state_store,
            logger_obj=logger_obj,
            background=background,
        )
        return new_session_id

    @app.callback(
        Output("session-heartbeat-store", "data"),
        Input("session-heartbeat-interval", "n_intervals"),
        State("session-id-store", "data"),
        prevent_initial_call=False,
    )
    def session_heartbeat(_n_intervals, session_id):
        touch_session(
            session_id,
            dashboard_show_state_store,
            logger_obj=logger_obj,
            background=background,
        )
        return time.time()
