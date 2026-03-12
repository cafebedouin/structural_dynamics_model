#!/usr/bin/env python3
"""
Cross-tab theater_ratio against coalition type within the tangled_rope band.

Distinguishes structural blindness (institutional_dissent, low theater)
from strategic classification (institutional_dissent, high theater).

Usage:
    python3 theater_coalition_crosstab.py

Expects enriched_pipeline.json and corpus_data.json in the same
output directory as query.py uses (shared/loader.py OUTPUT_DIR).
"""

import json
import sys
from pathlib import Path

import pandas as pd

# -- Reuse the loader from the existing query infrastructure --
try:
    from shared.loader import load_json, OUTPUT_DIR
except ImportError:
    # Fallback: look for JSON files in current directory
    OUTPUT_DIR = Path(".")
    def load_json(path, label):
        with open(path) as f:
            return json.load(f)


VALID_PERSPECTIVES = ["powerless", "moderate", "institutional", "analytical"]

# PSI band thresholds (must match tangled_decomposition.py)
PSI_ROPE_LEANING  = 0.30
PSI_SNARE_LEANING = 0.70

# Theater bands for interpretation
THEATER_LOW  = 0.15
THEATER_HIGH = 0.40


def compute_psi(perspectives: dict) -> float | None:
    """
    Replicate the PSI formula:
        psi = P(snare) / (P(rope) + P(snare) + 0.001)

    Perspectives dict maps power-level -> type string.
    We count how many positions classify as snare vs rope.
    For a simple proxy, we treat the MaxEnt probabilities as
    the fraction of positions voting each way.
    """
    if not perspectives:
        return None
    types = list(perspectives.values())
    n_snare = sum(1 for t in types if t == "snare")
    n_rope  = sum(1 for t in types if t == "rope")
    n_total = len(types)
    if n_total == 0:
        return None
    p_snare = n_snare / n_total
    p_rope  = n_rope  / n_total
    return p_snare / (p_rope + p_snare + 0.001)


def classify_coalition(perspectives: dict) -> str:
    """
    Classify the coalition type from perspective assignments.
    Mirrors the logic from tangled_decomposition.py.
    """
    if not perspectives:
        return "other"
    types = list(perspectives.values())
    unique = set(types)

    # All agree on tangled_rope
    if unique == {"tangled_rope"}:
        return "uniform_tangled"

    inst = perspectives.get("institutional")
    others = [perspectives.get(p) for p in ("powerless", "moderate", "analytical")
              if p in perspectives]

    # Institutional sees rope/scaffold; others see tangled_rope or snare
    if inst in ("rope", "scaffold"):
        others_tangled_or_snare = all(t in ("tangled_rope", "snare") for t in others if t)
        if others_tangled_or_snare:
            return "institutional_dissent"

    # Analytical sees differently from powerless+moderate consensus
    anal = perspectives.get("analytical")
    pw   = perspectives.get("powerless")
    mod  = perspectives.get("moderate")
    if pw and mod and pw == mod and anal and anal != pw:
        return "analytical_dissent"

    # 3+ distinct types
    if len(unique) >= 3:
        return "split_field"

    return "other"


def psi_band(psi: float | None) -> str:
    if psi is None:
        return "unknown"
    if psi < PSI_ROPE_LEANING:
        return "rope_leaning"
    if psi > PSI_SNARE_LEANING:
        return "snare_leaning"
    return "genuinely_tangled"


def theater_band(tr: float | None) -> str:
    if tr is None:
        return "unknown"
    if tr < THEATER_LOW:
        return "low"
    if tr < THEATER_HIGH:
        return "medium"
    return "high"


def build_dataframe() -> pd.DataFrame:
    pipeline_raw = load_json(OUTPUT_DIR / "enriched_pipeline.json", "enriched_pipeline")
    per_constraint = pipeline_raw.get("per_constraint", [])

    rows = []
    for entry in per_constraint:
        cid   = entry.get("id")
        ctype = entry.get("claimed_type")
        if ctype != "tangled_rope":
            continue

        persp = entry.get("perspectives", {})
        psi   = compute_psi(persp)
        coalition = classify_coalition(persp)
        tr    = entry.get("theater_ratio")
        eps   = entry.get("extractiveness") or entry.get("metrics", {}).get("extractiveness")

        rows.append({
            "id":           cid,
            "theater_ratio": tr,
            "theater_band":  theater_band(tr),
            "psi":           psi,
            "psi_band":      psi_band(psi),
            "coalition":     coalition,
            "extractiveness": eps,
        })

    return pd.DataFrame(rows)


def main():
    df = build_dataframe()
    total = len(df)
    print(f"Tangled_rope constraints loaded: {total}\n")

    # ── 1. Theater by coalition: mean, median, std ──
    print("=" * 60)
    print("1. THEATER RATIO BY COALITION TYPE")
    print("=" * 60)
    stats = (
        df.groupby("coalition")["theater_ratio"]
        .agg(n="count", mean="mean", median="median", std="std")
        .sort_values("mean", ascending=False)
    )
    print(stats.to_string(float_format="{:.3f}".format))
    print()

    # ── 2. Theater by PSI band ──
    print("=" * 60)
    print("2. THEATER RATIO BY PSI BAND")
    print("=" * 60)
    stats2 = (
        df.groupby("psi_band")["theater_ratio"]
        .agg(n="count", mean="mean", median="median", std="std")
        .sort_values("mean", ascending=False)
    )
    print(stats2.to_string(float_format="{:.3f}".format))
    print()

    # ── 3. Core cross-tab: PSI band × coalition × theater band ──
    print("=" * 60)
    print("3. PSI BAND × COALITION × THEATER BAND (counts)")
    print("=" * 60)
    crosstab = pd.crosstab(
        [df["psi_band"], df["coalition"]],
        df["theater_band"],
        margins=True
    )
    print(crosstab.to_string())
    print()

    # ── 4. The key diagnostic: institutional_dissent split ──
    print("=" * 60)
    print("4. INSTITUTIONAL DISSENT: THEATER DISTRIBUTION BY PSI BAND")
    print("   (structural blindness vs. strategic classification)")
    print("=" * 60)
    inst = df[df["coalition"] == "institutional_dissent"].copy()
    if inst.empty:
        print("  No institutional_dissent constraints found.")
    else:
        sub = (
            inst.groupby(["psi_band", "theater_band"])
            .agg(n=("id", "count"), mean_theater=("theater_ratio", "mean"),
                 mean_extraction=("extractiveness", "mean"))
            .reset_index()
        )
        print(sub.to_string(index=False, float_format="{:.3f}".format))
        print()
        print("  Interpretation guide:")
        print("    rope_leaning  + low theater   → structural blindness")
        print("                                    (institution can't find the snare)")
        print("    snare_leaning + high theater  → strategic classification")
        print("                                    (institution sees it, calls it coordination)")
        print("    snare_leaning + low theater   → ambiguous (genuine coordination claim)")

    print()

    # ── 5. High-theater institutional_dissent constraints ──
    print("=" * 60)
    print("5. HIGH-THEATER INSTITUTIONAL_DISSENT CONSTRAINTS (theater > 0.40)")
    print("   (candidates for strategic classification)")
    print("=" * 60)
    candidates = df[
        (df["coalition"] == "institutional_dissent") &
        (df["theater_ratio"] > THEATER_HIGH)
    ][["id", "psi_band", "theater_ratio", "extractiveness"]].sort_values(
        "theater_ratio", ascending=False
    )
    if candidates.empty:
        print("  None found.")
    else:
        print(candidates.to_string(index=False, float_format="{:.3f}".format))

    print()

    # ── 6. Low-theater snare-leaning split_field ──
    print("=" * 60)
    print("6. LOW-THEATER SNARE-LEANING SPLIT_FIELD CONSTRAINTS (theater < 0.15)")
    print("   (genuine disagreement, not performance)")
    print("=" * 60)
    genuine = df[
        (df["coalition"] == "split_field") &
        (df["psi_band"] == "snare_leaning") &
        (df["theater_ratio"] < THEATER_LOW)
    ][["id", "psi", "theater_ratio", "extractiveness"]].sort_values(
        "extractiveness", ascending=False
    )
    if genuine.empty:
        print("  None found.")
    else:
        print(genuine.to_string(index=False, float_format="{:.3f}".format))


if __name__ == "__main__":
    main()
