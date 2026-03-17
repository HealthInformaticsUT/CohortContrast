"""Statistics helpers for composite plot callbacks."""

from typing import Dict, List, Optional, Tuple

import numpy as np
import pandas as pd

from callbacks.composite_helpers import (
    count_occurrences as _count_occurrences,
    count_unique_occurrences as _count_unique_occurrences,
    expand_id_set as _expand_id_set,
    match_series_id as _match_series_id,
    normalize_id_loose as _normalize_id_loose,
)
from utils.helpers import normalize_concept_id as _normalize_concept_id


def build_summary_cluster_overlay_data(
    *,
    clustering_summary_matrix: pd.DataFrame,
    selected_cluster: Optional[str],
    concept_summaries: pd.DataFrame,
    ordinal_summaries: pd.DataFrame,
) -> Tuple[Dict, Dict, Dict]:
    from scipy import stats as scipy_stats

    cluster_prevalence_data = {}
    cluster_age_stats_summary = {}
    cluster_male_prop_stats_summary = {}

    if clustering_summary_matrix.empty or selected_cluster is None:
        return (
            cluster_prevalence_data,
            cluster_age_stats_summary,
            cluster_male_prop_stats_summary,
        )

    concept_data_lookup = {}
    for row in concept_summaries.to_dict("records"):
        cid = str(row.get("CONCEPT_ID", "")).replace(".0", "")
        concept_data_lookup[cid] = row
    if ordinal_summaries is not None and not ordinal_summaries.empty:
        for row in ordinal_summaries.to_dict("records"):
            cid = str(row.get("CONCEPT_ID", "")).replace(".0", "")
            concept_data_lookup[cid] = row

    filtered_cluster_rows = clustering_summary_matrix[
        clustering_summary_matrix["cluster"] == selected_cluster
    ].to_dict("records")

    for row in filtered_cluster_rows:
        cid = str(row.get("CONCEPT_ID", "")).replace(".0", "")
        is_ordinal = row.get("IS_ORDINAL", False)
        ordinal_raw = row.get("ORDINAL")
        ordinal = (
            int(ordinal_raw)
            if ordinal_raw is not None and not pd.isna(ordinal_raw)
            else 0
        )
        original_concept_id = row.get("ORIGINAL_CONCEPT_ID")

        if is_ordinal and original_concept_id is not None:
            lookup_concept_id_str = _normalize_concept_id(original_concept_id)
        else:
            lookup_concept_id_str = _normalize_concept_id(cid)

        try:
            lookup_concept_id_int = int(lookup_concept_id_str)
        except (ValueError, TypeError):
            lookup_concept_id_int = lookup_concept_id_str

        cluster_prev = row.get("prevalence", 0)
        concept_data = concept_data_lookup.get(cid, {})

        control_prevalence = concept_data.get("COMPARATOR_SUBJECT_PREVALENCE", 0)
        target_prevalence = concept_data.get("TARGET_SUBJECT_PREVALENCE", 0)
        overall_ratio = concept_data.get("PREVALENCE_DIFFERENCE_RATIO", 1.0)

        if (
            (not control_prevalence or control_prevalence <= 0)
            and target_prevalence
            and overall_ratio
            and overall_ratio > 0
        ):
            control_prevalence = target_prevalence / overall_ratio

        cluster_enrichment = (
            cluster_prev / control_prevalence
            if control_prevalence and control_prevalence > 0
            else 1.0
        )

        heritage = row.get("HERITAGE")
        if heritage is None or pd.isna(heritage):
            lookup_cid = (
                str(original_concept_id).replace(".0", "")
                if is_ordinal and original_concept_id
                else cid
            )
            heritage = (
                concept_data_lookup.get(lookup_cid, {}).get("HERITAGE", "unknown")
                or "unknown"
            )

        key_str = (heritage, lookup_concept_id_str, ordinal)
        cluster_prevalence_data[key_str] = {
            "prevalence": cluster_prev,
            "enrichment": cluster_enrichment,
        }
        key_int = (heritage, lookup_concept_id_int, ordinal)

        cluster_age_mean = row.get("age_mean")
        cluster_age_std = row.get("age_std")
        cluster_male_prop = row.get("male_proportion")
        patient_count = row.get("patient_count", 0) or 1

        if cluster_age_mean is not None and not pd.isna(cluster_age_mean):
            n = patient_count
            if n > 1 and cluster_age_std is not None and not pd.isna(cluster_age_std):
                t_critical = scipy_stats.t.ppf(0.975, n - 1)
                margin = t_critical * (cluster_age_std / np.sqrt(n))
                cluster_age_stats_summary[key_int] = {
                    "mean_age": cluster_age_mean,
                    "ci_low": cluster_age_mean - margin,
                    "ci_high": cluster_age_mean + margin,
                    "n": n,
                }
            else:
                cluster_age_stats_summary[key_int] = {
                    "mean_age": cluster_age_mean,
                    "ci_low": cluster_age_mean,
                    "ci_high": cluster_age_mean,
                    "n": n,
                }

        if cluster_male_prop is not None and not pd.isna(cluster_male_prop):
            n = patient_count
            if n > 1:
                se = np.sqrt(cluster_male_prop * (1 - cluster_male_prop) / n)
                ci_low = max(0, cluster_male_prop - 1.96 * se)
                ci_high = min(1, cluster_male_prop + 1.96 * se)
            else:
                ci_low = ci_high = cluster_male_prop
            cluster_male_prop_stats_summary[key_int] = {
                "mean_male_prop": cluster_male_prop,
                "ci_low": ci_low,
                "ci_high": ci_high,
                "n": n,
            }

    return (
        cluster_prevalence_data,
        cluster_age_stats_summary,
        cluster_male_prop_stats_summary,
    )


def build_summary_age_stats(
    active_concepts: List[Dict],
    concept_summaries: pd.DataFrame,
    ordinal_summaries: pd.DataFrame,
) -> Dict:
    from scipy import stats as scipy_stats

    age_stats = {}
    active_concept_ids = set()
    for concept in active_concepts:
        cid = concept.get("CONCEPT_ID") or concept.get("_concept_id")
        if cid is None:
            continue
        active_concept_ids.add(str(cid).replace(".0", ""))
        try:
            active_concept_ids.add(int(float(str(cid).replace(".0", ""))))
        except (ValueError, TypeError):
            pass

    all_summaries = (
        pd.concat([concept_summaries, ordinal_summaries], ignore_index=True)
        if ordinal_summaries is not None and not ordinal_summaries.empty
        else concept_summaries
    )
    if all_summaries.empty or not active_concept_ids:
        return age_stats

    all_summaries = all_summaries.copy()

    def normalize_id(value):
        if pd.isna(value):
            return None
        try:
            return int(float(str(value).replace(".0", "")))
        except (ValueError, TypeError):
            return str(value).replace(".0", "")

    all_summaries["_check_id_norm"] = all_summaries["CONCEPT_ID"].apply(normalize_id)
    filtered = all_summaries[all_summaries["_check_id_norm"].isin(active_concept_ids)]

    for row in filtered.to_dict("records"):
        age_mean = row.get("age_mean")
        if age_mean is None or pd.isna(age_mean):
            continue

        heritage = row.get("HERITAGE", "ALL") or "unknown"
        is_ordinal = (
            bool(row.get("IS_ORDINAL"))
            if row.get("IS_ORDINAL") is not None and not pd.isna(row.get("IS_ORDINAL"))
            else False
        )
        ordinal = (
            int(row.get("ORDINAL"))
            if row.get("ORDINAL") is not None and not pd.isna(row.get("ORDINAL"))
            else 0
        )
        original_concept_id = row.get("ORIGINAL_CONCEPT_ID")
        concept_id = row.get("CONCEPT_ID")
        lookup_concept_id = (
            original_concept_id
            if is_ordinal and pd.notna(original_concept_id)
            else concept_id
        )

        age_std = row.get("age_std")
        n_ages = row.get("n_ages") or row.get("patient_count", 0) or 1
        male_proportion = row.get("male_proportion")

        if n_ages > 1 and age_std and not pd.isna(age_std):
            se = age_std / np.sqrt(n_ages)
            t_critical = scipy_stats.t.ppf(0.975, df=n_ages - 1)
            ci_low = age_mean - t_critical * se
            ci_high = age_mean + t_critical * se
        else:
            ci_low = ci_high = age_mean

        try:
            normalized_concept_id = int(float(str(lookup_concept_id).replace(".0", "")))
        except (ValueError, TypeError):
            normalized_concept_id = str(lookup_concept_id).replace(".0", "")

        age_stats[(heritage, normalized_concept_id, ordinal)] = {
            "mean_age": age_mean,
            "ci_low": ci_low,
            "ci_high": ci_high,
            "n": n_ages,
            "male_proportion": male_proportion,
        }

    return age_stats


def build_patient_cluster_prevalence_data(
    *,
    clustering_results: Dict,
    data_patients: pd.DataFrame,
    dashboard_data: List[Dict],
    active_concepts: List[Dict],
    selected_cluster: Optional[str],
) -> Dict:
    cluster_prevalence_data = {}

    patient_assignments = clustering_results.get("patient_assignments", [])
    if isinstance(patient_assignments, list):
        df_assignments = pd.DataFrame(patient_assignments)
    else:
        df_assignments = patient_assignments

    cluster_patients_set = set()
    if not df_assignments.empty and "cluster" in df_assignments.columns:
        cluster_patients_set = set(
            df_assignments[df_assignments["cluster"] == selected_cluster][
                "patient_id"
            ].tolist()
        )

    df_target_for_calc = data_patients[
        data_patients["COHORT_DEFINITION_ID"] == "target"
    ].copy()

    active_concept_ids = set()
    for concept in active_concepts:
        cid = concept.get("_concept_id") or concept.get("CONCEPT_ID")
        if cid:
            active_concept_ids.add(_normalize_concept_id(cid))

    for item in dashboard_data:
        item_concept_id = item.get("_concept_id") or item.get("CONCEPT_ID")
        if item_concept_id is None:
            continue
        if _normalize_concept_id(item_concept_id) not in active_concept_ids:
            continue

        heritage = item.get("HERITAGE", "unknown")
        is_ordinal = item.get("IS_ORDINAL", False)
        ordinal_num = item.get("ORDINAL", 0)
        original_concept_id = item.get("ORIGINAL_CONCEPT_ID")

        if is_ordinal:
            lookup_concept_id = _normalize_concept_id(original_concept_id)
        else:
            lookup_concept_id = _normalize_concept_id(item_concept_id)

        cluster_prev = 0.0
        if is_ordinal and ordinal_num > 0 and original_concept_id:
            norm_original_id = _normalize_concept_id(original_concept_id)
            concept_data_calc = df_target_for_calc[
                df_target_for_calc["CONCEPT_ID"]
                .astype(str)
                .str.replace(".0", "", regex=False)
                == norm_original_id
            ].copy()
            if not concept_data_calc.empty:
                concept_data_calc["OCC_COUNT"] = concept_data_calc["TIME_TO_EVENT"].apply(
                    _count_unique_occurrences
                )
                patient_occ_counts = concept_data_calc.groupby("PERSON_ID")[
                    "OCC_COUNT"
                ].sum()
                patients_with_ordinal = set(
                    patient_occ_counts[patient_occ_counts >= ordinal_num].index.tolist()
                )
                patients_in_cluster = patients_with_ordinal & cluster_patients_set
                if cluster_patients_set:
                    cluster_prev = len(patients_in_cluster) / len(cluster_patients_set)
        else:
            norm_concept_id = _normalize_concept_id(item_concept_id)
            concept_data_calc = pd.DataFrame()
            for try_id in [item_concept_id, norm_concept_id, str(item_concept_id)]:
                if try_id is None:
                    continue
                try_id_str = str(try_id).replace(".0", "")
                concept_data_calc = df_target_for_calc[
                    df_target_for_calc["CONCEPT_ID"]
                    .astype(str)
                    .str.replace(".0", "", regex=False)
                    == try_id_str
                ]
                if not concept_data_calc.empty:
                    break
            if not concept_data_calc.empty:
                patients_with_concept = set(concept_data_calc["PERSON_ID"].unique())
                patients_in_cluster = patients_with_concept & cluster_patients_set
                if cluster_patients_set:
                    cluster_prev = len(patients_in_cluster) / len(cluster_patients_set)

        control_prevalence = item.get("COMPARATOR_SUBJECT_PREVALENCE", 0)
        target_prevalence = item.get("TARGET_SUBJECT_PREVALENCE", 0)
        overall_ratio = item.get("PREVALENCE_DIFFERENCE_RATIO", 1.0)
        if (
            (not control_prevalence or control_prevalence <= 0)
            and target_prevalence
            and overall_ratio
            and overall_ratio > 0
        ):
            control_prevalence = target_prevalence / overall_ratio

        cluster_enrichment = (
            cluster_prev / control_prevalence
            if control_prevalence and control_prevalence > 0
            else 1.0
        )

        key = (heritage, lookup_concept_id, ordinal_num)
        cluster_prevalence_data[key] = {
            "prevalence": cluster_prev,
            "enrichment": cluster_enrichment,
        }
        direct_key = (heritage, _normalize_concept_id(item_concept_id), ordinal_num)
        if direct_key != key:
            cluster_prevalence_data[direct_key] = {
                "prevalence": cluster_prev,
                "enrichment": cluster_enrichment,
            }

    return cluster_prevalence_data


def build_patient_cluster_age_stats(
    *,
    heritage_groups_order: Dict,
    data_patients: pd.DataFrame,
    data_initial: pd.DataFrame,
    data_person: pd.DataFrame,
    cluster_patient_ids,
) -> Dict:
    from scipy import stats as scipy_stats

    if (
        data_patients.empty
        or data_initial.empty
        or data_person.empty
        or not cluster_patient_ids
    ):
        return {}

    cluster_patient_ids_normalized = _expand_id_set(cluster_patient_ids)

    initial_map = {}
    for sid, date in zip(
        data_initial["SUBJECT_ID"].values, data_initial["COHORT_START_DATE"].values
    ):
        initial_map[sid] = date
        initial_map[str(sid)] = date
        try:
            initial_map[int(sid)] = date
        except (ValueError, TypeError):
            pass

    person_map_birth = {}
    for pid, year in zip(
        data_person["PERSON_ID"].values, data_person["YEAR_OF_BIRTH"].values
    ):
        person_map_birth[pid] = year
        person_map_birth[str(pid)] = year
        try:
            person_map_birth[int(pid)] = year
        except (ValueError, TypeError):
            pass

    df_cluster = data_patients[
        (data_patients["COHORT_DEFINITION_ID"] == "target")
        & (data_patients["PERSON_ID"].isin(cluster_patient_ids_normalized))
    ].copy()

    cluster_age_stats = {}
    for heritage_key, items in heritage_groups_order.items():
        for item in items:
            is_ordinal_item = item.get("is_ordinal", False)
            if is_ordinal_item:
                concept_id_key = _normalize_id_loose(item.get("original_concept_id"))
                ordinal_num = item.get("ordinal", 0)
            else:
                concept_id_key = _normalize_id_loose(
                    item.get("concept_id") or item.get("_concept_id")
                )
                ordinal_num = 0
            lookup_key = (heritage_key or "unknown", concept_id_key, ordinal_num)

            if is_ordinal_item:
                original_concept_id = item.get("original_concept_id")
                concept_data = df_cluster[
                    _match_series_id(df_cluster["CONCEPT_ID"], original_concept_id)
                ].copy()
                if not concept_data.empty and ordinal_num > 0:
                    concept_data["OCC_COUNT"] = concept_data["TIME_TO_EVENT"].apply(
                        _count_occurrences
                    )
                    concept_data = concept_data[
                        concept_data["OCC_COUNT"] >= ordinal_num
                    ].copy()
            else:
                concept_id = (
                    item.get("concept_id")
                    or item.get("_concept_id")
                    or item.get("CONCEPT_ID")
                )
                concept_data = df_cluster[
                    _match_series_id(df_cluster["CONCEPT_ID"], concept_id)
                ].copy()

            if concept_data.empty:
                continue

            ages = []
            for pid in concept_data["PERSON_ID"].unique():
                birth_year = person_map_birth.get(pid)
                cohort_start = initial_map.get(pid)
                if (
                    birth_year is None
                    or pd.isna(birth_year)
                    or cohort_start is None
                    or pd.isna(cohort_start)
                ):
                    continue
                try:
                    cohort_date = pd.to_datetime(cohort_start)
                    age = cohort_date.year - int(birth_year)
                    if 0 <= age <= 150:
                        ages.append(age)
                except (ValueError, TypeError, AttributeError):
                    continue

            if len(ages) >= 2:
                mean_age = np.mean(ages)
                std_age = np.std(ages, ddof=1)
                n = len(ages)
                t_critical = scipy_stats.t.ppf(0.975, n - 1)
                margin = t_critical * (std_age / np.sqrt(n))
                cluster_age_stats[lookup_key] = {
                    "mean_age": mean_age,
                    "ci_low": mean_age - margin,
                    "ci_high": mean_age + margin,
                    "n": n,
                }
            elif len(ages) == 1:
                cluster_age_stats[lookup_key] = {
                    "mean_age": ages[0],
                    "ci_low": ages[0],
                    "ci_high": ages[0],
                    "n": 1,
                }

    return cluster_age_stats


def build_patient_cluster_male_prop_stats(
    *,
    heritage_groups_order: Dict,
    data_patients: pd.DataFrame,
    data_person: pd.DataFrame,
    cluster_patient_ids,
) -> Dict:
    from scipy import stats as scipy_stats

    if data_patients.empty or data_person.empty or not cluster_patient_ids:
        return {}

    person_gender_map = {}
    for pid, gender in zip(
        data_person["PERSON_ID"].values, data_person["GENDER_CONCEPT_ID"].values
    ):
        person_gender_map[pid] = gender
        person_gender_map[str(pid)] = gender
        try:
            person_gender_map[int(pid)] = gender
        except (ValueError, TypeError):
            pass

    cluster_patient_ids_normalized = _expand_id_set(cluster_patient_ids)
    df_cluster = data_patients[
        (data_patients["COHORT_DEFINITION_ID"] == "target")
        & (data_patients["PERSON_ID"].isin(cluster_patient_ids_normalized))
    ].copy()

    cluster_male_prop_stats = {}
    for heritage_key, items in heritage_groups_order.items():
        for item in items:
            is_ordinal_item = item.get("is_ordinal", False)
            if is_ordinal_item:
                concept_id_key = _normalize_id_loose(item.get("original_concept_id"))
                ordinal_num = item.get("ordinal", 0)
            else:
                concept_id_key = _normalize_id_loose(
                    item.get("concept_id") or item.get("_concept_id")
                )
                ordinal_num = 0
            lookup_key = (heritage_key or "unknown", str(concept_id_key), ordinal_num)

            if is_ordinal_item:
                original_concept_id = item.get("original_concept_id")
                concept_data = df_cluster[
                    _match_series_id(df_cluster["CONCEPT_ID"], original_concept_id)
                ].copy()
                if not concept_data.empty and ordinal_num > 0:
                    concept_data["OCC_COUNT"] = concept_data["TIME_TO_EVENT"].apply(
                        _count_occurrences
                    )
                    concept_data = concept_data[
                        concept_data["OCC_COUNT"] >= ordinal_num
                    ].copy()
            else:
                concept_id = (
                    item.get("concept_id")
                    or item.get("_concept_id")
                    or item.get("CONCEPT_ID")
                )
                concept_data = df_cluster[
                    _match_series_id(df_cluster["CONCEPT_ID"], concept_id)
                ].copy()

            if concept_data.empty:
                continue

            male_indicators = []
            for pid in concept_data["PERSON_ID"].unique():
                gender_concept_id = person_gender_map.get(pid)
                if gender_concept_id is None:
                    gender_concept_id = person_gender_map.get(str(pid))
                if gender_concept_id is None:
                    try:
                        gender_concept_id = person_gender_map.get(int(pid))
                    except (ValueError, TypeError):
                        pass
                if gender_concept_id is None or pd.isna(gender_concept_id):
                    continue
                male_indicators.append(1 if gender_concept_id == 8507 else 0)

            if len(male_indicators) >= 2:
                mean_male = np.mean(male_indicators)
                n = len(male_indicators)
                se = np.sqrt(mean_male * (1 - mean_male) / n)
                z_critical = scipy_stats.norm.ppf(0.975)
                ci_low = max(0, mean_male - z_critical * se)
                ci_high = min(1, mean_male + z_critical * se)
                cluster_male_prop_stats[lookup_key] = {
                    "mean_male_prop": mean_male,
                    "ci_low": ci_low,
                    "ci_high": ci_high,
                    "n": n,
                }
            elif len(male_indicators) == 1:
                mean_male = male_indicators[0]
                cluster_male_prop_stats[lookup_key] = {
                    "mean_male_prop": mean_male,
                    "ci_low": mean_male,
                    "ci_high": mean_male,
                    "n": 1,
                }

    return cluster_male_prop_stats
