#!/usr/bin/env python3
"""Stage A witness — OQ-92 gain_flow/fixing_cost schema fields.

Validates the three provenance shapes + four negative controls, each negative
required to bite at its INTENDED guard (error path checked, not just invalidity).
Validator: Draft7 (the pipeline's actual validator). Base story: a real corpus
JSON with stakeholders[] (json/demographic_skill_mismatch.json — incidentally one
of the 7 live CI_Rope certifications, the future uncaptured-side Stage-D control).
Run from repo root: python3 audits/2026-06-10_oq92_step3_preregistration/stage_a_schema_witness.py
"""
import copy
import json
import sys
from pathlib import Path

from jsonschema import Draft7Validator

ROOT = Path(__file__).resolve().parents[2]
SCHEMA = json.load(open(ROOT / "schemas/constraint_story_schema.json"))
BASE = json.load(open(ROOT / "json/demographic_skill_mismatch.json"))
V = Draft7Validator(SCHEMA)

SEAT = BASE["stakeholders"][0]["name"]
results = []


def err_path(e):
    # jsonschema 3.2.0 (pipeline's version) has no json_path
    return "$." + ".".join(str(p) for p in e.absolute_path)


def check(label, doc, expect_valid, must_mention=None):
    errs = list(V.iter_errors(doc))
    valid = not errs
    ok = valid == expect_valid
    detail = ""
    if not expect_valid:
        if valid:
            ok = False
            detail = "EXPECTED INVALID but validated"
        else:
            joined = " | ".join(err_path(e) + ": " + e.message[:90] for e in errs)
            if must_mention and not any(must_mention in err_path(e) + e.message for e in errs):
                ok = False
                detail = f"invalid but NOT at intended guard ({must_mention}); got: {joined}"
            else:
                detail = f"bit at intended guard: {joined[:160]}"
    results.append((label, "PASS" if ok else "FAIL", detail))
    return ok


# 1-3: the three provenance shapes
d = copy.deepcopy(BASE); d["gain_flow"] = SEAT; d["fixing_cost"] = "prohibitive"
check("1 named-seat + prohibitive (authored-gain)", d, True)

d = copy.deepcopy(BASE); d["gain_flow"] = "diffuse"; d["fixing_cost"] = "cheap"
check("2 explicit-diffuse + cheap", d, True)

d = copy.deepcopy(BASE)
d.pop("gain_flow", None); d.pop("fixing_cost", None)
check("3 absent (fields omitted; pre-migration shape)", d, True)

# 4-7: negative controls, each at its intended guard
d = copy.deepcopy(BASE); d["gain_flow"] = 42
check("4 gain_flow wrong type (42)", d, False, must_mention="gain_flow")

d = copy.deepcopy(BASE); d["gain_flow"] = SEAT; d["fixing_cost"] = "moderate"
check("5 fixing_cost bad enum ('moderate')", d, False, must_mention="fixing_cost")

d = copy.deepcopy(BASE); d.pop("stakeholders"); d["gain_flow"] = "diffuse"
check("6 gain_flow without stakeholders (dependency rider)", d, False, must_mention="stakeholders")

d = copy.deepcopy(BASE); d.pop("stakeholders"); d["fixing_cost"] = "cheap"
check("7 fixing_cost without stakeholders (dependency rider)", d, False, must_mention="stakeholders")

# Note: gain_flow naming a NONEXISTENT seat is schema-VALID by design — referential
# integrity is compiler-enforced (Draft7 cannot express it). Witnessed at Stage B.
d = copy.deepcopy(BASE); d["gain_flow"] = "ghost_seat_zz"
check("8 ghost seat is schema-valid BY DESIGN (compiler's job, Stage B)", d, True)

print(f"base story: json/demographic_skill_mismatch.json  seat used: {SEAT}")
for label, verdict, detail in results:
    print(f"  [{verdict}] {label}" + (f" — {detail}" if detail else ""))
n_fail = sum(1 for _, v, _ in results if v == "FAIL")
print(f"{len(results) - n_fail}/{len(results)} witness cases pass")
sys.exit(1 if n_fail else 0)
