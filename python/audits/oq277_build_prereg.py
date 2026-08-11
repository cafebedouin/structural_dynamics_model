#!/usr/bin/env python3
"""Assemble (and re-verify) OQ-277's PREREGISTRATION.md.

WHY THIS IS A BUILD STEP AND NOT A HAND-WRITTEN FILE.

The handoff requires the preregistration to incorporate `verdict_grammar_amendment.md`
VERBATIM, plus the prompts and the banned lexicons. Copying 850+ lines by hand creates a
second copy of each with no queryable fact saying which is canonical — Pattern 2, inside
the experiment that measures Pattern 2. Pattern 2's own rule says what to do about it:

    one canonical location per thing, and canonicity must be a CHECKED FACT
    (a path in docs, a CI check), not a memory.

So the sources stay canonical, the preregistration is assembled from them, and `--check`
asserts the shipped document is byte-identical to a fresh assembly. Duplication with
checked canonicity, rather than duplication on trust.

The prose body is hand-authored in `PREREGISTRATION_body.md` — a pre-registration is a
document someone wrote, not a report someone generated. Only the appendices and the
computed call table are mechanical.

DETERMINISM. No timestamps, no randomness: the same sources must always produce the same
bytes, or the md5 that anchors the freeze would drift on every rebuild and mean nothing.

Usage:
  python3 python/audits/oq277_build_prereg.py --write     # assemble + print md5
  python3 python/audits/oq277_build_prereg.py --check     # verify shipped == fresh
"""
from __future__ import annotations
import argparse, glob, hashlib, json, pathlib, sys

REPO = pathlib.Path(__file__).resolve().parents[2]
AUDIT = REPO / "audits" / "2026-08-10_oq277_rq2_crosscoding"
OUT = AUDIT / "PREREGISTRATION.md"
BODY = AUDIT / "PREREGISTRATION_body.md"

# Everything the preregistration pins by md5. A reader can verify each independently.
PINNED = [
    ("CLAUDE.md", REPO / "CLAUDE.md",
     "the published six; Build Discipline block, lines 472-540"),
    ("docs/technical/build_discipline.md", REPO / "docs" / "technical" / "build_discipline.md",
     "mechanism text behind the six"),
    ("packets/wu_source/failure_modes_catalog.md",
     AUDIT / "packets" / "wu_source" / "failure_modes_catalog.md", "Wu's A-E, as fetched"),
    ("packets/wu_source/llm_observer_ground_truth.yaml",
     AUDIT / "packets" / "wu_source" / "llm_observer_ground_truth.yaml", "Wu's dataset rows"),
    ("python/audits/oq277_lexicon.py", REPO / "python" / "audits" / "oq277_lexicon.py",
     "the single leak matcher, both pinned versions"),
    ("prompts/direction_i.md", AUDIT / "prompts" / "direction_i.md",
     "coder prompt: directions (i) and (iii')"),
    ("prompts/direction_ii.md", AUDIT / "prompts" / "direction_ii.md",
     "coder prompt: direction (ii)"),
    ("verdict_grammar_amendment.md", AUDIT / "verdict_grammar_amendment.md",
     "incorporated verbatim as Appendix D"),
    ("controls/anchors.json", AUDIT / "controls" / "anchors.json", "anchor set, both directions"),
    ("controls/decoys.json", AUDIT / "controls" / "decoys.json", "decoys"),
    ("controls/planted.json", AUDIT / "controls" / "planted.json",
     "planted leak fixtures + planted broken unit"),
    ("controls/redaction_pairs_predeclared.json",
     AUDIT / "controls" / "redaction_pairs_predeclared.json", "the pre-declared pair selection"),
    ("controls/redaction_twins_direction_i.json",
     AUDIT / "controls" / "redaction_twins_direction_i.json", "3 unredacted arms"),
    ("controls/redaction_twins_direction_ii.json",
     AUDIT / "controls" / "redaction_twins_direction_ii.json", "5 unredacted arms, 2 sets"),
    ("RULING_2026-08-11_freeze_scope.md", AUDIT / "RULING_2026-08-11_freeze_scope.md",
     "(iii') population and the freeze scope"),
    ("controls/redaction_pair_selection_defect.md",
     AUDIT / "controls" / "redaction_pair_selection_defect.md", "option-C ruling"),
]

LEGS = [("direction_i", "coder_direction_i", "direction (i) — 22 units + 3 anchors + 2 decoys "
                                             "+ 3 twin arms"),
        ("direction_ii", "coder_direction_ii", "direction (ii) — 26 units + 3 anchors + 2 decoys "
                                               "+ 5 twin arms"),
        ("iii_prime", "coder_iii_prime", "(iii') — 7 new units (3 anchor members reuse their "
                                         "direction-(i) calls)")]
K = 3
DESIGN_TOTAL_ITEMS = 73


def md5_file(p: pathlib.Path) -> str:
    return hashlib.md5(p.read_bytes()).hexdigest() if p.exists() else "ABSENT"


def call_table() -> str:
    rows, total, missing = [], 0, []
    for leg, stem, label in LEGS:
        pkt = AUDIT / "packets" / "run" / f"{stem}.json"
        if pkt.exists():
            n = len(json.load(open(pkt)))
            rows.append(f"| {label} | {n} | {n * K} |")
            total += n
        else:
            missing.append(leg)
            rows.append(f"| {label} | **not built** | **0** |")
    rows.append("| escape units | **0** | **0** |")
    out = ["| leg | items | calls at k=3 |", "|---|---|---|", *rows,
           f"| **assembled total** | **{total}** | **{total * K}** |",
           f"| **design total** | **{DESIGN_TOTAL_ITEMS}** | "
           f"**{DESIGN_TOTAL_ITEMS * K}** |"]
    if missing:
        out += ["", f"> **INCOMPLETE.** {missing} not built. The assembled total is a PARTIAL, "
                    f"not the expected call count. The freeze completes only when the assembled "
                    f"total equals the design total of {DESIGN_TOTAL_ITEMS} items / "
                    f"{DESIGN_TOTAL_ITEMS * K} calls."]
    else:
        out += ["", "> Assembled total equals the design total. The call-count precondition for "
                    "the freeze is satisfied."]
    return "\n".join(out)


def fence(path: pathlib.Path, lang: str = "") -> str:
    """Inline a file verbatim. Uses a 5-backtick fence so any 3-backtick block inside the
    source survives intact — a markdown document inlined in a 3-backtick fence would
    terminate at its own first code block and silently truncate the 'verbatim' copy."""
    return f"`````{lang}\n{path.read_text()}\n`````"


def assemble() -> str:
    parts = [BODY.read_text().replace("<!--CALLTABLE-->", call_table())]

    parts.append("\n---\n\n## Appendix A — pinned sources (md5)\n")
    parts.append("Every artifact this preregistration depends on, pinned so a reader can verify "
                 "the versions in force. **The pins live here and never in a payload**: a commit "
                 "hash inside a payload identifies our source to the coder through the weights, "
                 "where a payload grep cannot reach.\n")
    parts.append("| artifact | md5 | what it is |\n|---|---|---|")
    for name, path, what in PINNED:
        parts.append(f"| `{name}` | `{md5_file(path)}` | {what} |")
    parts.append("\n**Wu's two source files were fetched 2026-08-10** from "
                 "`bisdom-cell/openclaw-model-bridge` (public, accompanying the arXiv paper); "
                 "the fetch manifest carries the same md5s "
                 "(`packets/wu_source/FETCH_MANIFEST.txt`).")

    parts.append("\n---\n\n## Appendix B — the coder prompts, verbatim\n")
    parts.append("These are the exact templates the driver formats with each item's four fields. "
                 "**They are the only artifact in this design with no witness** — a prompt cannot "
                 "be validated by running it, because running it is the spend. "
                 "`controls/verify_prompts.py` checks them against their pre-registered "
                 "constraints (49/49); the wording judgement is the operator's, at freeze "
                 "(C4/C5), and is the only control this artifact has.\n")
    parts.append("### B.1 `prompts/direction_i.md` — directions (i) and (iii')\n")
    parts.append(fence(AUDIT / "prompts" / "direction_i.md"))
    parts.append("\n### B.2 `prompts/direction_ii.md` — direction (ii)\n")
    parts.append(fence(AUDIT / "prompts" / "direction_ii.md"))
    parts.append("\n### B.3 Prompt design notes\n")
    parts.append(fence(AUDIT / "prompts" / "README.md"))

    parts.append("\n---\n\n## Appendix C — the banned lexicons, verbatim\n")
    parts.append("One matcher, two pinned versions, one module. `LEXICON_DETECT` is the live "
                 "leak-grep; `LEXICON_SELECTION_20260811` is frozen at pre-declaration and "
                 "widening it is prohibited. **Editing either list after the md5 below is "
                 "recorded invalidates the freeze.**\n")
    parts.append(fence(REPO / "python" / "audits" / "oq277_lexicon.py", "python"))

    parts.append("\n---\n\n## Appendix D — `verdict_grammar_amendment.md`, incorporated VERBATIM\n")
    parts.append("Binding pre-registration content in its entirety, including §Q and the two "
                 "entries added 2026-08-11 (§L.4 — a ruling made on evidence that had not been "
                 "gathered; §L.5 — the second matcher defect and the role split). Inlined by "
                 "`oq277_build_prereg.py` from the canonical file and asserted byte-identical to "
                 "it by `--check`, so this copy cannot drift from its source.\n")
    parts.append(fence(AUDIT / "verdict_grammar_amendment.md"))

    parts.append("\n---\n\n## Appendix E — frozen unit lists\n")
    parts.append("Opaque ids as assembled, in emitted order. Non-coder-facing detail (role, true "
                 "label, `matrix_unit`, source id) lives in the sibling `*_map.json` files.\n")
    for leg, stem, label in LEGS:
        pkt = AUDIT / "packets" / "run" / f"{stem}.json"
        mp = AUDIT / "packets" / "run" / f"{stem}_map.json"
        parts.append(f"\n### {label}\n")
        if not pkt.exists():
            parts.append("**NOT BUILT** — pending hand-back. Declared as ABSENT, never emitted "
                         "as an empty packet: an empty packet written without comment is the "
                         "absence-satisfies-the-gate shape, and a downstream expected-call total "
                         "computed from it would be smaller and self-consistent.\n")
            continue
        ids = [x["id"] for x in json.load(open(pkt))]
        m = json.load(open(mp))
        parts.append(f"- items: **{len(ids)}** · matrix cells: **{m['n_matrix_cells']}** · "
                     f"quarantined: **{m['n_quarantined']}** · unit-sweep direction: "
                     f"**({m['sweep_direction']})**")
        parts.append(f"- leak-exempt (MUST fire): `{'`, `'.join(m['leak_exempt_ids'])}`"
                     if m["leak_exempt_ids"] else "- leak-exempt: none")
        parts.append(f"- emitted order: `{' '.join(ids)}`")
        parts.append(f"- packet md5: `{md5_file(pkt)}` · map md5: `{md5_file(mp)}`")

    parts.append("\n---\n\n*End of preregistration.*\n")
    return "\n".join(parts)


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--write", action="store_true")
    ap.add_argument("--check", action="store_true")
    a = ap.parse_args()
    fresh = assemble()

    if a.check:
        if not OUT.exists():
            print("RED — PREREGISTRATION.md does not exist")
            return 1
        shipped = OUT.read_text()
        if shipped != fresh:
            print("RED — shipped PREREGISTRATION.md differs from a fresh assembly.")
            print("      A source changed after the document was built. Re-run --write, and if "
                  "an md5\n      is already recorded in audit_log.md, the freeze is INVALIDATED "
                  "and must be re-stamped.")
            print(f"      shipped md5 {hashlib.md5(shipped.encode()).hexdigest()}")
            print(f"      fresh   md5 {hashlib.md5(fresh.encode()).hexdigest()}")
            return 1
        print(f"GREEN — PREREGISTRATION.md is byte-identical to a fresh assembly")
        print(f"        md5 {hashlib.md5(shipped.encode()).hexdigest()}")
        print(f"        every verbatim appendix matches its canonical source")
        return 0

    if a.write:
        OUT.write_text(fresh)
        digest = hashlib.md5(fresh.encode()).hexdigest()
        print(f"wrote {OUT.relative_to(REPO)}  ({len(fresh.splitlines())} lines)")
        print(f"md5 {digest}")
        built = sum(1 for _l, s, _lab in LEGS
                    if (AUDIT / "packets" / "run" / f"{s}.json").exists())
        if built < len(LEGS):
            print(f"\nNOTE: {len(LEGS) - built} leg(s) not built. This md5 is a DRAFT stamp, "
                  f"NOT the freeze.\n      Re-assemble and re-stamp once every leg lands.")
        return 0

    ap.print_help()
    return 2


if __name__ == "__main__":
    sys.exit(main())
