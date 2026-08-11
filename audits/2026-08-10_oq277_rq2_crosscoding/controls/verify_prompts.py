#!/usr/bin/env python3
"""Standing check on the coder prompts — the only artifact in this design with no witness.

A prompt cannot be validated by running it; running it is the spend. What CAN be checked is
that it satisfies the constraints C4/C5 fixed before it was written, and those constraints
are exactly the ones whose violation would be invisible afterwards:

  - `other` reachability is a property of the WORDING. If it is phrased as a trailing "if
    none of the above apply" it becomes a residual, is under-selected, and nothing
    downstream can distinguish that from real coverage. The both-residue row and the escape
    check both depend on it.
  - a second requested signal (confidence, rationale) becomes an unpreregistered weight that
    an adjudicator will read. k=3 unanimity IS the churn instrument.
  - source-identifying vocabulary in a prompt is a leak through the weights that the payload
    grep cannot catch — the coder recalls the published taxonomy instead of reasoning from
    the definition.

NOTE ON SWEEP SCOPE. A prompt necessarily contains its own direction's class definitions, so
it can NEVER be clean under its own direction's full lexicon and is not swept that way. What
is asserted is that it is clean under the `source_identifying` group of BOTH directions —
the part that must hold regardless of which taxonomy the prompt is teaching.

Run:  python3 controls/verify_prompts.py
Exit: 0 iff every prompt satisfies every stated constraint.
"""
from __future__ import annotations
import pathlib
import re
import sys

HERE = pathlib.Path(__file__).resolve().parent
AUDIT = HERE.parent
REPO = HERE.parents[2]
sys.path.insert(0, str(REPO / "python" / "audits"))
import oq277_lexicon as L  # noqa: E402

PROMPTS = AUDIT / "prompts"
FIELDS = L.CODER_FACING_FIELDS

# leg -> (prompt file, own sweep direction, answer tokens)
LEGS = {
    "direction_i": ("direction_i.md", "i", ["P1", "P2", "P3", "P4", "P5", "P6", "other"]),
    "direction_ii": ("direction_ii.md", "ii", ["A", "B", "C", "D", "E", "other"]),
    # (iii') deliberately shares direction (i)'s prompt — see prompts/README.md. A separate
    # file would be a byte-copy with no queryable fact saying which is canonical: P2 inside
    # the experiment that measures P2.
    "iii_prime": ("direction_i.md", "ii", ["P1", "P2", "P3", "P4", "P5", "P6", "other"]),
}

failures: list[str] = []
checks = 0


def check(cond: bool, label: str) -> None:
    global checks
    checks += 1
    print(f"        {'PASS' if cond else 'FAIL'}  {label}")
    if not cond:
        failures.append(label)


def norm(s: str) -> str:
    """Collapse whitespace. Prompt prose is hard-wrapped, so a literal substring test on a
    phrase that spans a line break fails for a formatting reason and reads as a content
    defect — which is itself the kind of false signal this audit is about."""
    return re.sub(r"\s+", " ", s)


def si_group(direction: str) -> dict:
    return {direction: {"source_identifying":
                        L.LEXICON_DETECT[direction]["source_identifying"]}}


def main() -> int:
    print("OQ-277 coder-prompt constraint check (C4/C5)\n")

    seen_files: dict[str, str] = {}
    for leg, (fname, own, tokens) in LEGS.items():
        path = PROMPTS / fname
        print(f"\n[{leg}] — {fname}, sweep direction ({own}), {len(tokens)} answer tokens")
        if not path.exists():
            check(False, f"{fname} exists")
            continue
        raw = path.read_text()
        flat = norm(raw)
        seen_files.setdefault(fname, leg)

        # 1. source-identifying clean in BOTH directions
        for d in ("i", "ii"):
            hits = L.scan(raw, d, si_group(d))
            check(not hits, f"clean under direction ({d}) source_identifying"
                            + (f" — LEAKED {sorted({m for _g, _p, m, _c in hits})}" if hits else ""))

        # 2. exactly the four coder-facing placeholders, each once
        for f in FIELDS:
            check(raw.count("{" + f + "}") == 1, f"placeholder {{{f}}} appears exactly once")
        stray = set(re.findall(r"\{([a-z_]+)\}", raw)) - set(FIELDS)
        check(not stray, f"no placeholder outside the four coder-facing fields"
                         + (f" — found {sorted(stray)}" if stray else ""))

        # 3. every answer token is offered on the final answer line
        tail = raw.rsplit("---", 1)[-1]
        missing = [t for t in tokens
                   if not re.search(rf"(?<![A-Za-z]){re.escape(t)}(?![A-Za-z])", tail)]
        check(not missing, f"answer line offers all {len(tokens)} tokens"
                           + (f" — MISSING {missing}" if missing else ""))

        # 4. `other` is first-class, not a residual
        check("This is a substantive answer, not a leftover" in flat,
              "'other' carries a positive definition ('a substantive answer, not a leftover')")
        check(not re.search(r"if none of the (above|these)|otherwise,? (choose|use|pick)",
                            flat, re.I),
              "no trailing 'if none apply' residual phrasing")
        # `other` must sit in the same list shape as the classes: introduced at line-start
        # with the same em-dash form the lettered/numbered classes use.
        klass = len(re.findall(r"(?m)^(?:P[1-6]|[A-E]) —", raw))
        oth = len(re.findall(r"(?m)^other —", raw))
        check(klass == len(tokens) - 1 and oth == 1,
              f"'other' is introduced in the SAME list shape as the classes "
              f"({klass} class entries + {oth} 'other' entry)")

        # 5. exactly one label, no second signal
        check("nothing else" in flat, "demands one token and nothing else")
        check(re.search(r"no confidence", flat, re.I) is not None,
              "explicitly forbids a confidence field")
        check(re.search(r"no explanation|no rationale", flat, re.I) is not None,
              "explicitly forbids an explanation/rationale")

        # 6. no provenance pin in the payload (it belongs in the preregistration)
        check(not re.search(r"\b[0-9a-f]{7,40}\b", raw),
              "carries no commit hash — the pin lives in the preregistration, because a "
              "hash in a payload identifies our source through the weights")

    # 7. the shared-prompt fact, asserted rather than conventional
    print("\n[shared prompt] — (iii') and direction (i) must use ONE file")
    check(LEGS["iii_prime"][0] == LEGS["direction_i"][0],
          "(iii') and direction (i) resolve to the same prompt file")
    check(not (PROMPTS / "iii_prime.md").exists(),
          "no iii_prime.md fork exists on disk")
    check(LEGS["iii_prime"][2] == LEGS["direction_i"][2],
          "(iii') and direction (i) share one label space")
    check(LEGS["iii_prime"][1] != LEGS["direction_i"][1],
          "(iii') sweeps under the OTHER direction than direction (i) — its units are ours "
          "while its answers are in our index; this crossing is the easy thing to get backwards")

    print()
    if failures:
        print(f"{len(failures)} of {checks} checks FAILED:")
        for f in failures:
            print(f"  - {f}")
        return 1
    print(f"ALL {checks} PROMPT CHECKS PASS")
    print("\nNOTE: passing here means the prompts satisfy their PRE-REGISTERED constraints.")
    print("It does not mean the wording is good. That judgement is the operator's at freeze")
    print("(C4/C5) and it is the only control this artifact has.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
