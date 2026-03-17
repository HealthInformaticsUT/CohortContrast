# Mappings Tab

## Introduction

The **Mappings** tab is used to combine concepts and audit mapping
decisions. Merge actions are available in **patient mode**.

## Manual Merge

Workflow:

1.  In **Manual Merge**, select concepts using the `Map` column (main
    concepts only).
2.  Click **Merge Selected Concepts**.
3.  Confirm `New Concept ID`, `New Concept Name`, and `Heritage`.
4.  Click **Apply Merge**.

Defaults in the merge form are prefilled from the most prevalent
selected concept.

![Manual merge workflow](../reference/figures/a02_mappings_manual.png)

Manual merge workflow

## Hierarchy Suggestions

Use hierarchy-based candidate merges:

- Set `Minimum hierarchy depth`.
- Optionally keep `Allow only minors` enabled.
- Click **Update Hierarchy Suggestions**.
- Review rows and click **Merge Selected Hierarchy Suggestions**.

![Hierarchy
suggestions](../reference/figures/a02_mappings_suggestions.png)

Hierarchy suggestions

## Correlation Suggestions

Use correlation-based candidate merges:

- Set `Minimum correlation`.
- Set `Max median days in between`.
- Optionally allow `heritage drift`.
- Click **Update Correlation Suggestions**.
- Review rows and click **Merge Selected Correlation Suggestions**.

![Correlation
suggestions](../reference/figures/a06_mappings_correlation.png)

Correlation suggestions

## Mapping History

The history table records applied mappings, including source concept
IDs/names, mapped concept IDs/names, mapping type, and heritage. Use it
as an audit trail for post-processing decisions.
