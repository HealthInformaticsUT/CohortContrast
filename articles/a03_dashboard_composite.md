# Dashboard Composite Plot

## Introduction

The **Dashboard** is the primary analysis surface in CohortContrast. It
combines effect size, prevalence, timing, and demographics for active
concepts in one plot and one table.

![Dashboard composite
plot](../reference/figures/a03_dashboard_composite.png)

Dashboard composite plot

## What the composite shows

The chart is concept-centric (one row per concept) and combines:

- **Event occurrences**: distribution of first occurrence timing.
- **Prevalence/Enrichment**: target prevalence and prevalence difference
  ratio.
- **Age**: concept-level age estimate relative to cohort context.
- **Male proportion**: concept-level sex distribution.
- **Cluster columns**: per-cluster prevalence summaries for the active
  clustering.

## How to use it

1.  Apply sidebar filters and inspect which concepts remain active.
2.  Switch cluster view (`All`, `Cluster 1`, `Cluster 2`, …) to compare
    within-cluster behavior.
3.  Use table filters and the **Show** column for manual
    inclusion/exclusion.
4.  Click **Apply Table Selection** to persist manual visibility
    updates.

![Dashboard composite
plot](../reference/figures/a03_dashboard_composite_cluster1.png) 5.
Enable ordinal rows for concept’s where on average \>2 occurrences
happen. ![Dashboard composite
plot](../reference/figures/a03_dashboard_composite_ordinal.png)

## Interpretation guidance

- High prevalence difference ratio with solid target prevalence often
  indicates robust cohort contrast.
- Cluster-specific prevalence columns help distinguish global signals
  from cluster-local signals.
- Age/sex columns can expose clinically meaningful subgroup effects and
  potential confounding patterns.
