#!/usr/bin/env python3
"""OQ-277 threshold-comparability probe — PRIMARY-SIDE DRAW.

Queue item 1 of PREREGISTRATION_threshold_calibration.md. Executed by the assembler
(the escape extractor must not see the primary sample and does not run this).

Method is deliberately IDENTICAL to the escape-side draw recorded in the prereg:
    r = random.Random(SEED); sorted pool; r.choice(subject) then r.choice(self_audit)
so neither side gets a different sampling discipline.

Prereg rules discharged here:
  1. known-positives only  -> asserts every pool member is an extracted unit with all
     four coder-facing fields non-empty (a NO-UNIT in this slot destroys the arm)
  2. matched on incident_location to the escape draws (`subject`, `self_audit_subsection`)
     -> asserts both values are present in the primary stratum; if either were absent the
     script FAILS rather than silently substituting (an undeclared substitution voids the pass)
  3. seed stated before anything is assembled -> SEED below, printed, and written to the
     held record

Writes the identities to a HELD file. Prints only the seed, counts, and a confirmation.
"""
import glob, json, random, pathlib, sys

SEED = 20260811
HERE = pathlib.Path(__file__).resolve().parent
POOL_DIR = HERE.parent / "our_units"
OUT = HERE / "_held_primary_draw.json"

FIELDS = ["symptom", "mechanism_as_described", "detection_path", "consequence"]
WANT = ["subject", "self_audit_subsection"]   # the escape draws' incident_location values

units = {}
for f in sorted(glob.glob(str(POOL_DIR / "*.json"))):
    d = json.load(open(f))
    name = pathlib.Path(f).name
    # rule 1: known-positive check
    for k in FIELDS:
        assert isinstance(d.get(k), str) and d[k].strip(), f"{name}: empty {k} — not a known-positive"
    units[name] = d

strata = {w: sorted(n for n, d in units.items() if d["metadata"]["incident_location"] == w)
          for w in WANT}

# rule 2: matching, fail-closed on absence
missing = [w for w in WANT if not strata[w]]
if missing:
    sys.exit(f"MISMATCH: primary stratum has no unit with incident_location in {missing}. "
             "Prereg requires the substitution be DECLARED — stopping rather than silently substituting.")

r = random.Random(SEED)
drawn = {w: r.choice(strata[w]) for w in WANT}   # subject first, then self_audit_subsection

OUT.write_text(json.dumps({
    "seed": SEED,
    "method": "random.Random(SEED); sorted pool per incident_location; r.choice(subject) then r.choice(self_audit_subsection)",
    "pool_sizes": {w: len(strata[w]) for w in WANT},
    "location_match": "EXACT — both escape-side incident_location values present in the primary stratum; no substitution",
    "drawn": drawn,
}, indent=2) + "\n")

print(f"primary-side draw EXECUTED  seed={SEED}")
print(f"  pool sizes by incident_location: " + ", ".join(f"{w}={len(strata[w])}" for w in WANT))
print(f"  location match: EXACT (no substitution, nothing to declare)")
print(f"  known-positive assertion: PASS on all {len(units)} pool members (4/4 fields non-empty)")
print(f"  identities written to {OUT.name} (HELD — not printed)")
