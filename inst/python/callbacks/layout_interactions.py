"""Layout interaction callbacks for sidebar and clientside UI behavior."""

from typing import Optional

from dash import Input, Output, State, clientside_callback


def register_layout_interaction_callbacks(app) -> None:
    """Register layout interaction callbacks (sidebar toggle and clientside UI updates)."""

    @app.callback(
        [Output("sidebar", "style"), Output("main-content", "style")],
        Input("sidebar-toggle", "n_clicks")
    )
    def toggle_sidebar(n_clicks: Optional[int]):
        if n_clicks is None:
            n_clicks = 0

        base_sidebar_style = {
            "width": "450px",
            "height": "100vh",
            "position": "fixed",
            "left": "0",
            "top": "0",
            "backgroundColor": "#f8f9fa",
            "borderRight": "1px solid #ddd",
            "boxShadow": "2px 0 5px rgba(0,0,0,0.1)",
            "zIndex": "1000",
            "transition": "transform 0.3s ease",
            "overflowY": "auto",
            "overflowX": "hidden",
        }

        base_main_style = {
            "marginLeft": "450px",
            "padding": "0",
            "transition": "margin-left 0.3s ease",
            "width": "calc(100% - 450px)",
            "boxSizing": "border-box",
        }

        if n_clicks % 2 == 1:
            base_sidebar_style["transform"] = "translateX(-100%)"
            base_main_style["marginLeft"] = "0"
            base_main_style["width"] = "100%"
        else:
            base_sidebar_style["transform"] = "translateX(0)"
            base_main_style["marginLeft"] = "450px"
            base_main_style["width"] = "calc(100% - 450px)"

        return base_sidebar_style, base_main_style

    clientside_callback(
        """
        function(tabValue) {
            if (tabValue === "dashboard") {
                var attempts = [50, 150, 300, 500, 800];
                attempts.forEach(function(delay, index) {
                    setTimeout(function() {
                        var plotDiv = document.getElementById("composite-plot");
                        var containerDiv = document.getElementById("composite-plot-container");
                        if (plotDiv && window.Plotly) {
                            var rect = plotDiv.getBoundingClientRect();
                            var isVisible = rect.width > 0 && rect.height > 0;
                            var isInViewport = rect.top < window.innerHeight && rect.bottom > 0;

                            if (isVisible || isInViewport) {
                                var figure = plotDiv.data || null;
                                if (figure) {
                                    var layout = plotDiv.layout || {};
                                    var plotHeight = layout.height || plotDiv.offsetHeight || plotDiv.clientHeight;
                                    window.Plotly.Plots.resize(plotDiv);

                                    if (index >= attempts.length - 2) {
                                        window.Plotly.redraw(plotDiv);
                                        if (plotHeight > 0 && layout.height) {
                                            window.Plotly.relayout(plotDiv, {height: plotHeight});
                                        }
                                    }

                                    if (containerDiv && plotHeight > 0) {
                                        containerDiv.style.height = plotHeight + "px";
                                        containerDiv.style.minHeight = plotHeight + "px";
                                    }
                                } else {
                                    window.Plotly.Plots.resize(plotDiv);
                                    if (containerDiv) {
                                        var currentHeight = plotDiv.offsetHeight || plotDiv.clientHeight;
                                        if (currentHeight > 0) {
                                            containerDiv.style.height = currentHeight + "px";
                                        }
                                    }
                                }
                            }
                        }
                    }, delay);
                });
            }
            return window.dash_clientside.no_update;
        }
        """,
        Output("composite-plot", "id"),
        Input("main-tabs", "value")
    )

    clientside_callback(
        """
        function(message) {
            const baseStyle = {
                "position": "fixed",
                "top": "60%",
                "left": "50%",
                "transform": "translateX(-50%)",
                "zIndex": "10001",
                "color": "#2c3e50",
                "fontSize": "18px",
                "fontWeight": "500",
                "textAlign": "center",
                "pointerEvents": "none"
            };

            if (!window.__ccvLoadingState) {
                window.__ccvLoadingState = {
                    lastMessage: "",
                    waitingAfterPlot: false
                };
            }
            const state = window.__ccvLoadingState;
            const show = (text) => [text, {...baseStyle, "display": "block"}];
            const hide = () => ["", {...baseStyle, "display": "none"}];

            if (message && message.trim() !== "") {
                state.lastMessage = message;
                state.waitingAfterPlot = false;
                return show(message);
            }

            if ((state.lastMessage || "").includes("Rendering dashboard plot")) {
                const graphEl = document.getElementById("composite-plot");
                if (graphEl && typeof graphEl.on === "function") {
                    if (!state.waitingAfterPlot) {
                        state.waitingAfterPlot = true;
                        const onAfterPlot = function() {
                            const msgEl = document.getElementById("loading-message-display");
                            if (msgEl) {
                                msgEl.style.display = "none";
                                msgEl.textContent = "";
                            }
                            state.lastMessage = "";
                            state.waitingAfterPlot = false;
                            if (typeof graphEl.removeListener === "function") {
                                graphEl.removeListener("plotly_afterplot", onAfterPlot);
                            }
                        };
                        graphEl.on("plotly_afterplot", onAfterPlot);
                    }
                    return show("Rendering dashboard plot...");
                }
            }

            state.lastMessage = "";
            state.waitingAfterPlot = false;
            return hide();
        }
        """,
        [Output("loading-message-display", "children"),
         Output("loading-message-display", "style")],
        Input("loading-message-store", "data")
    )

    clientside_callback(
        """
        function(nClicks, selectedStudy, clusterView, figureState) {
            if (!nClicks) {
                return window.dash_clientside.no_update;
            }

            const safeStudy = (selectedStudy || "study")
                .toString()
                .replace(/[^a-zA-Z0-9_-]/g, "_");
            const safeCluster = (clusterView || "all")
                .toString()
                .replace(/[^a-zA-Z0-9_-]/g, "_");
            const timestamp = new Date().toISOString()
                .replace(/[:.-]/g, "")
                .replace("T", "_")
                .slice(0, 15);

            const filename = `${safeStudy}_composite_${safeCluster}_${timestamp}`;

            if (!window.Plotly) {
                return "plotly_missing";
            }

            const plotContainer = document.getElementById("composite-plot");
            let livePlotDiv = null;
            if (plotContainer) {
                // dcc.Graph id points to a wrapper; Plotly APIs need the inner
                // .js-plotly-plot node that holds the rendered figure.
                if (plotContainer.classList && plotContainer.classList.contains("js-plotly-plot")) {
                    livePlotDiv = plotContainer;
                } else {
                    livePlotDiv = plotContainer.querySelector(".js-plotly-plot");
                }
            }

            const hasFigureState = figureState && Array.isArray(figureState.data) && figureState.data.length > 0;
            if (hasFigureState) {
                const fallbackWidth = Math.max(
                    800,
                    Math.round(
                        (figureState.layout && figureState.layout.width) ||
                        (livePlotDiv ? livePlotDiv.clientWidth : 0) ||
                        1200
                    )
                );
                const fallbackHeight = Math.max(
                    500,
                    Math.round(
                        (figureState.layout && figureState.layout.height) ||
                        (livePlotDiv ? livePlotDiv.clientHeight : 0) ||
                        700
                    )
                );

                const offscreen = document.createElement("div");
                offscreen.style.position = "fixed";
                offscreen.style.left = "-10000px";
                offscreen.style.top = "-10000px";
                offscreen.style.width = `${fallbackWidth}px`;
                offscreen.style.height = `${fallbackHeight}px`;
                document.body.appendChild(offscreen);

                const exportLayout = Object.assign({}, figureState.layout || {}, {
                    width: fallbackWidth,
                    height: fallbackHeight,
                });
                const exportConfig = {displayModeBar: false, responsive: false, staticPlot: true};

                window.Plotly.newPlot(offscreen, figureState.data, exportLayout, exportConfig)
                    .then(function() {
                        return window.Plotly.downloadImage(offscreen, {
                            format: "png",
                            filename: filename,
                            width: fallbackWidth,
                            height: fallbackHeight,
                            scale: 2
                        });
                    })
                    .catch(function() {
                        // no-op: status output remains filename for callback success
                    })
                    .finally(function() {
                        setTimeout(function() {
                            if (offscreen && offscreen.parentNode) {
                                offscreen.parentNode.removeChild(offscreen);
                            }
                        }, 150);
                    });
                return filename;
            }

            const hasLiveFigure =
                livePlotDiv &&
                livePlotDiv.layout &&
                Array.isArray(livePlotDiv.data) &&
                livePlotDiv.data.length > 0;
            if (!hasLiveFigure) {
                return "plot_not_ready";
            }

            const fullLayout = livePlotDiv._fullLayout || livePlotDiv.layout || {};
            const width = Math.max(800, Math.round(fullLayout.width || livePlotDiv.clientWidth || 1200));
            const height = Math.max(500, Math.round(fullLayout.height || livePlotDiv.clientHeight || 700));
            window.Plotly.downloadImage(livePlotDiv, {
                format: "png",
                filename: filename,
                width: width,
                height: height,
                scale: 2
            });
            return filename;
        }
        """,
        Output("composite-image-download-status", "children"),
        Input("download-composite-png-btn", "n_clicks"),
        State("selected-study-store", "data"),
        State("cluster-view-store", "data"),
        State("composite-plot", "figure"),
        prevent_initial_call=True
    )
