#!/usr/bin/env python3
"""Generate the OQ-93 dedicated grid batch (Stage C, N=10 operator-ruled).

Assembles the LIVE prompt + prompts/grid_batch_addendum.md at call time (no
prompt fork — the live prompt stays canonical and unchanged until the
operator-gated opt-in flip). Stories are written to the AUDIT directory, NOT
prolog/testsets/ or json/: no consumer reads generated grids before the kappa
plausibility gate passes (prevalence sequencing, PREREGISTRATION.md).

Usage (from repo root):
    python3 -m agent.generate_grid_batch [--limit N] [--model M] [--dry-run]
"""

import argparse
import json
import sys
from pathlib import Path

import anthropic

from agent.story_generator_base import (
    PROMPT_PATH,
    SCHEMA_PATH,
    EXAMPLE_PATH,
    _SYSTEM_INSTRUCTION,
    _load_context_file,
)

REPO_ROOT = Path(__file__).resolve().parent.parent
ADDENDUM_PATH = REPO_ROOT / "prompts" / "grid_batch_addendum.md"
OUT_DIR = REPO_ROOT / "audits" / "2026-06-11_oq93_grid_migration" / "grid_batch"
DEFAULT_MODEL = "claude-haiku-4-5-20251001"

sys.path.insert(0, str(REPO_ROOT / "python"))
from generate_constraint_pl import validate_json, generate_pl  # noqa: E402

# N=10 seeds (operator-ruled batch size). Topics chosen so level-resolved
# coercion dynamics is the SUBJECT (the opt-in-by-story-focus criterion the
# live prompt will use after the gate) — topics only; every grid VALUE is the
# model's, derived from its own story (no seeded constants, OQ-70 discipline).
SEEDS = [
    ("union_decertification_campaign", "A coordinated union decertification campaign: structural and organizational pressure intensify while individual workers are offered carrots, and class-level resistance is delegitimated"),
    ("gig_platform_algorithmic_management", "Algorithmic management on a gig platform tightening over five years: individual drivers' exits close as the platform's structural position consolidates"),
    ("company_town_scrip_economy", "A company town moving wages into scrip and company stores: stakes rise for individuals while class-level alternatives are dismantled"),
    ("adjunctification_of_university_teaching", "A university system converting faculty lines to adjunct contracts: organizational suppression of organizing, individual precarity, structural normalization"),
    ("medical_debt_collection_escalation", "A hospital system's debt-collection pipeline hardening into wage garnishment and liens: individual stakes inflate fastest, structural machinery follows"),
    ("probation_supervision_intensification", "Probation supervision intensifying via electronic monitoring and fee stacking: individual-level coercion saturates while structural visibility falls"),
    ("eldercare_guardianship_capture", "Professional guardianship capturing elders' estates: individual accessibility collapses totally while organizational and class levels barely register the change"),
    ("tenant_displacement_renovation_eviction", "Renoviction waves in a gentrifying district: class-level displacement pressure rises while individual cases are settled quietly"),
    ("agricultural_contract_grower_lockin", "Poultry contract-grower lock-in deepening: organizational dependence on integrators rises, individual growers' debt forecloses exit, class resistance suppressed"),
    ("garment_supplychain_audit_theater", "A garment brand's supplier code-of-conduct regime: structural extraction steady or rising while audit theater lowers the visible individual-level coercion"),
]


def build_messages(source_desc):
    static_content = (
        f"=== GENERATION PROMPT ===\n{_load_context_file(PROMPT_PATH)}\n\n"
        f"=== GRID BATCH ADDENDUM ===\n{_load_context_file(ADDENDUM_PATH)}\n\n"
        f"=== JSON SCHEMA ===\n{_load_context_file(SCHEMA_PATH)}\n\n"
        f"=== EXAMPLE JSON (note: the example predates coercion_grid and does "
        f"not author one; YOUR story must) ===\n{_load_context_file(EXAMPLE_PATH)}"
    )
    task = (
        f"=== YOUR TASK ===\nGenerate a complete constraint story JSON for: {source_desc}\n"
        f"Follow the schema exactly, INCLUDING the full 32-point coercion_grid "
        f"per the GRID BATCH ADDENDUM. Output ONLY valid JSON."
    )
    return [{
        "role": "user",
        "content": [
            {"type": "text", "text": static_content,
             "cache_control": {"type": "ephemeral"}},
            {"type": "text", "text": task},
        ],
    }]


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--limit", type=int, default=0)
    ap.add_argument("--model", default=DEFAULT_MODEL)
    ap.add_argument("--dry-run", action="store_true")
    args = ap.parse_args()

    seeds = SEEDS[: args.limit] if args.limit else SEEDS
    (OUT_DIR / "json").mkdir(parents=True, exist_ok=True)
    (OUT_DIR / "pl").mkdir(parents=True, exist_ok=True)
    (OUT_DIR / "raw").mkdir(parents=True, exist_ok=True)

    if args.dry_run:
        for cid, desc in seeds:
            print(f"  would generate: {cid}")
        return

    client = anthropic.Anthropic()
    ledger = []
    for cid, desc in seeds:
        source = f"TOPIC: {desc}\nCONSTRAINT_ID: {cid}"
        print(f"[gen] {cid} ...", flush=True)
        try:
            resp = client.messages.create(
                model=args.model,
                max_tokens=12288,
                system=[{"type": "text", "text": _SYSTEM_INSTRUCTION,
                         "cache_control": {"type": "ephemeral"}}],
                messages=build_messages(source),
            )
            text = "".join(b.text for b in resp.content if b.type == "text")
        except Exception as e:
            ledger.append({"cid": cid, "status": "api_error", "detail": repr(e)[:300]})
            print(f"  API ERROR: {e}")
            continue
        (OUT_DIR / "raw" / f"{cid}.txt").write_text(text)
        # strip accidental fences
        body = text.strip()
        if body.startswith("```"):
            body = body.split("```", 2)[1]
            body = body.split("\n", 1)[1] if "\n" in body else body
        try:
            data = json.loads(body)
        except json.JSONDecodeError as e:
            ledger.append({"cid": cid, "status": "json_error", "detail": str(e)[:200]})
            print(f"  JSON ERROR: {e}")
            continue
        data.setdefault("header", {})["constraint_id"] = cid
        errors = validate_json(data)
        if errors:
            ledger.append({"cid": cid, "status": "schema_invalid",
                           "detail": errors[:5]})
            print(f"  SCHEMA INVALID ({len(errors)} errors): {errors[:2]}")
            (OUT_DIR / "json" / f"{cid}.json").write_text(json.dumps(data, indent=2))
            continue
        (OUT_DIR / "json" / f"{cid}.json").write_text(json.dumps(data, indent=2))
        try:
            pl = generate_pl(data)
        except ValueError as e:
            ledger.append({"cid": cid, "status": "compiler_rejected", "detail": str(e)[:300]})
            print(f"  COMPILER REJECTED: {e}")
            continue
        (OUT_DIR / "pl" / f"{cid}.pl").write_text(pl)
        has_grid = bool(data.get("coercion_grid"))
        n_pts = len((data.get("coercion_grid") or {}).get("points") or [])
        ledger.append({"cid": cid, "status": "ok", "grid_points": n_pts})
        print(f"  ok (grid points: {n_pts}, grid present: {has_grid})")

    (OUT_DIR / "generation_ledger.json").write_text(json.dumps(ledger, indent=2))
    n_ok = sum(1 for e in ledger if e["status"] == "ok")
    print(f"\n{n_ok}/{len(seeds)} generated clean; ledger at {OUT_DIR}/generation_ledger.json")


if __name__ == "__main__":
    main()
