# CohortContrast Viewer Python Layout

This folder is organized by responsibility.

- `app.py`: application entrypoint.
- `config/`: constants and runtime configuration.
- `data/`: parquet loading, preprocessing, caching and filtering.
- `layout/`: Dash/Plotly UI layout composition.
- `callbacks/`: callback handlers split by feature area.
- `plots/`: plot building helpers.
- `clustering/`: clustering-specific computation.
- `precompute/`: summary precomputation pipeline.
- `models/`: state/registry models.
- `utils/`: generic utility helpers.

Guideline:
- If code reads/writes study data, put it in `data/`.
- If code creates UI nodes, put it in `layout/`.
- If code wires UI interactions, put it in `callbacks/`.
- If code only draws figures, put it in `plots/`.
