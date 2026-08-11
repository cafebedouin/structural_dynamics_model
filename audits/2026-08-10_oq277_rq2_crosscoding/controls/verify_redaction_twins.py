#!/usr/bin/env python3
"""Two-sided sanity check on control (c)'s redaction twins.

HANDOFF_TWINS_AND_DRIVER.md §1, stated as a pre-committed assertion rather than an
inspection:

    the unredacted arm MUST FAIL the leak sweep (it is exempt, not clean) and the
    redacted arm MUST PASS it. A pair where BOTH arms pass is a pair where you did not
    actually un-redact anything, and it would report a floor of zero — "redaction costs
    nothing" — by construction.

Both directions are asserted. Checking only that the unredacted arm has hits would pass a
pair whose redacted arm was never redacted either; checking only the redacted arm would pass
a pair whose unredacted arm restored nothing. The defect this control can produce is a floor
of zero, and a floor of zero is what BOTH one-sided checks report as healthy.

A pair is also required to actually DIFFER in restored vocabulary, not merely in hit count:
the restored terms are printed so the reader can see what the delta is made of.

Run:  python3 controls/verify_redaction_twins.py
Exit: 0 iff every declared pair exists and satisfies both directions.
"""
from __future__ import annotations
import json
import pathlib
import sys

HERE = pathlib.Path(__file__).resolve().parent
AUDIT = HERE.parent
REPO = HERE.parents[2]
sys.path.insert(0, str(REPO / "python" / "audits"))
import oq277_lexicon as L  # noqa: E402

FIELDS = L.CODER_FACING_FIELDS
failures: list[str] = []
checks = 0


def check(cond: bool, label: str) -> None:
    global checks
    checks += 1
    print(f"        {'PASS' if cond else 'FAIL'}  {label}")
    if not cond:
        failures.append(label)


def blob(d: dict) -> str:
    return " ".join(str(d.get(f, "")) for f in FIELDS)


def terms(text: str, direction: str) -> list[str]:
    return sorted({m for _g, _p, m, _c in L.scan(text, direction)})


def verify_direction(direction: str, twins_path: pathlib.Path, redacted_lookup) -> None:
    print(f"\n[{direction}] redaction twins — {twins_path.name}")
    if not twins_path.exists():
        check(False, f"{twins_path.name} exists (direction ({direction}) twins NOT WRITTEN YET)")
        return
    pairs = json.load(open(twins_path))["pairs"]
    for p in pairs:
        uid = p["unit_id"]
        print(f"    {p['pair_id']} — {uid}")
        un = blob(p["unredacted"])
        red = redacted_lookup(uid)
        if red is None:
            check(False, f"{uid}: redacted arm found in its source packet")
            continue
        red_hits = terms(red, direction)
        un_hits = terms(un, direction)

        check(len(red_hits) == 0,
              f"{uid}: REDACTED arm passes the sweep (0 hits, got {len(red_hits)}{': ' + str(red_hits) if red_hits else ''})")
        check(len(un_hits) > 0,
              f"{uid}: UNREDACTED arm FAILS the sweep as required ({len(un_hits)} distinct banned terms)")
        check(all(f in p["unredacted"] and str(p["unredacted"][f]).strip() for f in FIELDS),
              f"{uid}: all four coder-facing fields present and non-empty")
        if un_hits:
            print(f"          restored: {un_hits[:12]}{' …' if len(un_hits) > 12 else ''}")


def main() -> int:
    wu = {u["id"]: u for u in json.load(open(AUDIT / "packets" / "wu_units.json"))["units"]}
    verify_direction(
        "i", HERE / "redaction_twins_direction_i.json",
        lambda uid: blob(wu[uid]) if uid in wu else None,
    )

    our = {}
    for f in sorted((AUDIT / "packets" / "our_units").glob("*.json")):
        u = json.load(open(f))
        our[f.stem] = u
    verify_direction(
        "ii", HERE / "redaction_twins_direction_ii.json",
        lambda uid: blob(our[uid]) if uid in our else None,
    )

    print()
    if failures:
        print(f"{len(failures)} of {checks} checks FAILED:")
        for f in failures:
            print(f"  - {f}")
        return 1
    print(f"ALL {checks} TWIN CHECKS PASS")
    return 0


if __name__ == "__main__":
    sys.exit(main())
