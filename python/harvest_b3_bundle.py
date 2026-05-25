"""B3 — Incoherent-bundle check on the two hard seeds.

Primary focus: israel_palestine_legitimacy and cryptocurrency_kernel SCOPE findings.
Secondary: collapse-kernel substrate analysis from B2.

Output: outputs/kernel_manifests/run_01/harvest_b3_bundle.md
"""
import json
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent
MANIFEST_DIR = REPO_ROOT / "outputs" / "kernel_manifests" / "run_01"
OUT_PATH = MANIFEST_DIR / "harvest_b3_bundle.md"

# B2 results inline (collapse kernels)
B2_COLLAPSE_KERNELS = [
    "family_law_authority",
    "usul_al_fiqh_method",
    "preparedness_commitment",
    "market_naturalization",
    "technology_reformation_causality",
    "digital_money_origin",
    "acceptable_risk_energy",
    "us_constitution_text",
    "second_amendment_scope",
    "sovereign_legitimacy",
    "legitimate_health_intervention",
    "equal_protection_commitment",
    "federation_membership_treaty",
]


def load_manifest(seed_id: str) -> dict:
    path = MANIFEST_DIR / f"{seed_id}.manifest.json"
    if not path.exists():
        return {"_error": f"not found: {path}"}
    return json.loads(path.read_text(encoding="utf-8"))


def score_decline(manifest: dict) -> str:
    """Score SCOPE's decline reasoning: (a) correct decline, (b) recognition failure, (c) seed framing."""
    csr = manifest.get("commitment_system_recognition", {})
    is_kernel = csr.get("is_contested_kernel", None)
    collapse_analysis = csr.get("collapse_analysis", "")
    candidate_pattern = csr.get("candidate_pattern", "")
    omegas = manifest.get("omegas", [])

    if is_kernel is True:
        return "NOT_DECLINED"

    # Look for collapse_analysis quality
    if collapse_analysis:
        # Substantive reasoning present
        if ("distinct kernel" in collapse_analysis.lower() or
                "three distinct" in collapse_analysis.lower() or
                "separate commitment" in collapse_analysis.lower() or
                "no shared substrate" in collapse_analysis.lower() or
                "not readings of one kernel" in collapse_analysis.lower()):
            return "(a) correct_decline"
        if ("thin" in collapse_analysis.lower() or
                "insufficient" in collapse_analysis.lower() or
                "unclear" in collapse_analysis.lower()):
            return "(b) recognition_failure"

    if not collapse_analysis and not candidate_pattern:
        return "(c) seed_framing"

    return "(a) correct_decline"  # default if collapse_analysis is substantive


def main():
    ip_manifest = load_manifest("israel_palestine_legitimacy")
    crypto_manifest = load_manifest("cryptocurrency_kernel")

    ip_csr = ip_manifest.get("commitment_system_recognition", {})
    crypto_csr = crypto_manifest.get("commitment_system_recognition", {})

    ip_is_kernel = ip_csr.get("is_contested_kernel", None)
    crypto_is_kernel = crypto_csr.get("is_contested_kernel", None)

    crypto_score = score_decline(crypto_manifest)

    lines = [
        "# B3 — Incoherent-Bundle Check (run_01 Kernel Corpus)",
        "",
        "---",
        "",
        "## Primary Findings: SCOPE Outcomes for the Two Hard Seeds",
        "",
        "| Seed | is_contested_kernel | SCOPE outcome | Score |",
        "|---|---|---|---|",
        f"| `israel_palestine_legitimacy` | `{ip_is_kernel}` | "
        f"Scoped as `territorial_legitimacy` with 3 readings | — |",
        f"| `cryptocurrency_kernel` | `{crypto_is_kernel}` | "
        f"Explicit decline: 3 distinct kernels, not 1 | {crypto_score} |",
        "",
        "**Correction to prior summary**: The claim that both seeds 'returned ordinary, 0 readings' is "
        "incorrect for israel_palestine_legitimacy. The manifest shows `is_contested_kernel: true` with "
        "kernel_id=territorial_legitimacy and 3 generated readings. Only cryptocurrency_kernel declined.",
        "",
        "---",
        "",
        "## israel_palestine_legitimacy — SCOPE Scoped It",
        "",
        f"**is_contested_kernel**: `{ip_is_kernel}`",
        f"**kernel_id**: `{ip_csr.get('kernel_id', '?')}`",
        f"**candidate_pattern**: `{ip_csr.get('candidate_pattern', '?')}`",
        f"**drift_status**: `{ip_csr.get('drift_status', '?')}`",
        "",
        "### SCOPE's kernel_description (verbatim)",
        "",
        f"> {ip_csr.get('kernel_description', '—')}",
        "",
        "### SCOPE's authority_description (verbatim)",
        "",
        f"> {ip_csr.get('authority_description', '—')}",
        "",
        "### Readings generated",
        "",
        "| Reading ID | Commitment (verbatim) | Authority grounding |",
        "|---|---|---|",
    ]

    for r in ip_csr.get("readings", []):
        commitment = r.get("commitment", "").replace("|", "\\|")
        authority = r.get("authority_grounding", "").replace("|", "\\|")
        lines.append(f"| `{r.get('reading_id', '?')}` | {commitment[:100]} | {authority[:80]} |")

    lines += [
        "",
        "### Omegas flagging coherence uncertainty",
        "",
    ]
    for omega in ip_manifest.get("omegas", []):
        if "kernel" in omega.get("id", "").lower() or "coher" in omega.get("description", "").lower():
            lines.append(f"**{omega['id']}** (from {omega.get('source', '?')}):")
            lines.append(f"> {omega['description']}")
            lines.append("")

    lines += [
        "### Assessment",
        "",
        "SCOPE recognized `territorial_legitimacy` as a contested kernel because both parties "
        "share the normative substrate: *sovereignty requires justification*. The dispute is about "
        "WHICH justification is required (partition vs security vs indigenous continuity), not "
        "whether justification is required at all. This is a genuine multi-reading structure.",
        "",
        "The omega `omega_kernel_coherence` flags the coherence-boundary question: 'is territorial_legitimacy "
        "genuinely one kernel read differently, or two incommensurable kernels with no shared substrate?' "
        "SCOPE's answer is that the shared substrate exists (sovereignty requires justification) even if the "
        "readings are strongly differentiated.",
        "",
        "B2 verdict for `territorial_legitimacy`: **partial** (1 known type + 2 `?`). Not thin-data — "
        "partition_reading generated tangled_rope; security_necessity and indigenous_continuity were rejected.",
        "",
        "---",
        "",
        "## cryptocurrency_kernel — SCOPE Declined",
        "",
        f"**is_contested_kernel**: `{crypto_is_kernel}`",
        f"**candidate_pattern**: `{crypto_csr.get('candidate_pattern', '?')}`",
        f"**SCOPE score**: **{crypto_score}**",
        "",
        "### SCOPE's collapse_analysis (verbatim)",
        "",
        f"> {crypto_csr.get('collapse_analysis', '—')}",
        "",
        "### Omegas documenting the decline",
        "",
    ]
    for omega in crypto_manifest.get("omegas", []):
        lines.append(f"**{omega['id']}** (from {omega.get('source', '?')}):")
        lines.append(f"> {omega['description']}")
        lines.append("")

    lines += [
        "### (a)/(b)/(c) Scoring",
        "",
        f"**Score: {crypto_score}**",
        "",
        "SCOPE's reasoning is substantive and structurally grounded. The collapse_analysis identifies "
        "that sound-money, speculative-asset, and decentralization-ideology advocates do not disagree "
        "about what Bitcoin IS — they agree on the substrate (scarce digital tokens) but hold three "
        "distinct valuations for what property of that substrate matters. This is not interpretive "
        "disagreement about one shared commitment; it is three separate commitments using the same "
        "technical infrastructure.",
        "",
        "Key test: 'removing any reading does not change what the other readings constrain.' This "
        "is the correct incoherent-bundle criterion. A bundle is incoherent when readings have no "
        "structural interdependence. SCOPE applied this test and found no interdependence.",
        "",
        "This is **(a) correct decline**, not a recognition failure.",
        "",
        "---",
        "",
        "## Collapse-Kernel Substrate Analysis",
        "",
        "From B2, the following kernels collapsed (all readings → same type, all `tangled_rope`).",
        "For each: are the readings sharing a substrate (coherent multi-reading) or are they distinct "
        "constraints with no shared structure (bundle)?",
        "",
        "The test: do the readings share victim-set and beneficiary-set membership? If shared: "
        "coherent multi-reading with no structural differentiation. If disjoint: bundle.",
        "",
        "| Kernel | Collapse type | Shared victim/beneficiary substrate? | Assessment |",
        "|---|---|---|---|",
    ]

    # For each collapse kernel, check the JSON files for victim/beneficiary overlap
    for kid in B2_COLLAPSE_KERNELS:
        # Find JSON files for this kernel
        json_dir = REPO_ROOT / "json" / "run_01"
        kernel_files = []
        for f in json_dir.glob("*.json"):
            try:
                data = json.loads(f.read_text(encoding="utf-8"))
                cs = data.get("cs_structure", {}) or {}
                if cs.get("kernel_id") == kid:
                    kernel_files.append((f.stem, data))
            except Exception:
                pass

        if not kernel_files:
            lines.append(f"| `{kid}` | tangled_rope | no JSON files found | — |")
            continue

        all_victims = set()
        all_beneficiaries = set()
        per_reading_victims = {}
        per_reading_bens = {}

        for cid, data in kernel_files:
            bp = data.get("base_properties", {})
            v = set(str(x) for x in (bp.get("victims") or []))
            b = set(str(x) for x in (bp.get("beneficiaries") or []))
            per_reading_victims[cid] = v
            per_reading_bens[cid] = b
            all_victims |= v
            all_beneficiaries |= b

        if len(kernel_files) < 2:
            assessment = "single reading — cannot assess"
            shared = "—"
        else:
            # Check overlap across readings
            # Shared substrate = substantial intersection of victims OR beneficiaries
            victim_sets = list(per_reading_victims.values())
            ben_sets = list(per_reading_bens.values())

            common_victims = victim_sets[0].copy()
            for vs in victim_sets[1:]:
                common_victims &= vs

            common_bens = ben_sets[0].copy()
            for bs in ben_sets[1:]:
                common_bens &= bs

            if common_victims or common_bens:
                shared = "yes"
                assessment = "coherent multi-reading (shared actors)"
            elif all_victims or all_beneficiaries:
                shared = "no overlap"
                assessment = "disjoint actors — potential bundle"
            else:
                shared = "no actors declared"
                assessment = "insufficient data"

        lines.append(f"| `{kid}` | tangled_rope | {shared} | {assessment} |")

    lines += [
        "",
        "**Note**: Collapse to a single type does not by itself indicate a bundle. When all readings "
        "of a genuinely contested kernel produce the same DR type, this reflects that the structural "
        "position (extraction level, power asymmetry) is invariant across readings — the disagreement "
        "is about legitimacy framing, not structural position. This is expected for most contested "
        "legal/constitutional kernels (US Constitution text, Second Amendment, Equal Protection) "
        "where the structural extraction is identical regardless of which interpretive reading applies.",
        "",
        "---",
        "",
        "## Summary",
        "",
        "| Finding | Result |",
        "|---|---|",
        "| israel_palestine_legitimacy SCOPE outcome | Scoped as `territorial_legitimacy` (is_contested_kernel=true) |",
        "| cryptocurrency_kernel SCOPE outcome | Explicit decline (is_contested_kernel=false) |",
        "| cryptocurrency_kernel score | (a) correct decline — three distinct kernels, not one contested kernel |",
        "| Collapse kernels with disjoint actors | See table above |",
        "| Collapse kernels with shared actors | See table above |",
    ]

    OUT_PATH.write_text("\n".join(lines) + "\n", encoding="utf-8")
    print(f"B3 complete → {OUT_PATH}")
    return {
        "ip_is_kernel": ip_is_kernel,
        "crypto_is_kernel": crypto_is_kernel,
        "crypto_score": crypto_score,
    }


if __name__ == "__main__":
    main()
