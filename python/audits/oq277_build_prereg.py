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
import argparse, glob, hashlib, json, pathlib, re, sys

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


#: Machine-readable freeze marker in audit_log.md. Prose said "FROZEN" and "DRAFT" in a
#: table; a checker cannot key on that without guessing, so the stamp is explicit.
FREEZE_MARK = "<!--OQ277-FREEZE-STAMP:"


def recorded_freeze_stamps() -> list[str]:
    """Every md5 audit_log.md declares as a FREEZE stamp (not a draft)."""
    log = AUDIT / "audit_log.md"
    if not log.exists():
        return []
    return re.findall(re.escape(FREEZE_MARK) + r"\s*([0-9a-f]{32})\s*-->", log.read_text())


def drifted_sources(shipped: str) -> list[tuple[str, str]]:
    """Which pinned sources have changed since the document was assembled.

    Compares each source's CURRENT md5 against the md5 recorded for it in the shipped
    document's own Appendix A pin table. That is what "pinned" means here, and it covers
    both the sources inlined verbatim and the ones pinned by md5 only.

    The first version of this function tested whether each source's text appeared verbatim
    in the document. It reported 12 drifted sources when 2 had changed: every md5-only pin
    is absent from the document by design, so the test could not distinguish "moved" from
    "never inlined". A drift list padded with false positives is worse than none — it
    trains the reader to skip the block, which is how the permanently-red check this
    function exists to replace failed in the first place.
    """
    recorded = dict(re.findall(r"^\| `([^`]+)` \| `([0-9a-f]{32}|ABSENT)` \|", shipped, re.M))
    out = []
    for label, path, why in PINNED:
        was = recorded.get(label)
        if was is None:                       # not in the shipped pin table at all
            out.append((label, f"{why} — NOT PINNED in the shipped document"))
        elif md5_file(path) != was:
            out.append((label, why))
    return out


def check() -> int:
    """Verify the shipped preregistration — with the POST-FREEZE case separated out.

    ADDED 2026-08-11. The original check asked one question, "is the shipped document
    byte-identical to a fresh assembly?", and treated any difference as RED with the remedy
    "re-run --write and re-stamp". That is right BEFORE a freeze and wrong after one.

    After a stamp exists, a difference has two incompatible meanings wearing one colour:

      * the shipped document was ALTERED  -> fatal, the freeze is broken;
      * the pinned SOURCES moved on       -> expected, because a frozen document is a
                                             snapshot and the repository keeps working.

    The old form reported the second as RED and prescribed a remedy that would DESTROY the
    frozen record — re-writing produces a different document, and the stamp under which a
    run was made would no longer name anything on disk. It went red three times in this arc
    and, post-spend, could never go green again: a permanently-red check trains its reader
    to route around it, which is worse than no check.

    So: frozen documents are verified against their RECORDED md5 (fatal on mismatch), and
    source drift is reported separately as informational, naming which sources moved.
    """
    if not OUT.exists():
        print("RED — PREREGISTRATION.md does not exist")
        return 1
    shipped = OUT.read_text()
    shipped_md5 = hashlib.md5(shipped.encode()).hexdigest()
    stamps = recorded_freeze_stamps()
    # assemble() is deliberately NOT called yet: the frozen path does not need a fresh
    # assembly, and post-freeze a pinned source may legitimately have moved or gone.

    # --- POST-FREEZE: the document is a snapshot; the sources are allowed to move on.
    if stamps:
        if shipped_md5 not in stamps:
            print("RED — a FREEZE stamp is recorded and the shipped PREREGISTRATION.md does "
                  "NOT match it.")
            print("      The frozen document has been altered. This is the fatal case: a run "
                  "may have\n      been made under the recorded stamp, and that stamp no "
                  "longer names what is on disk.")
            print(f"      shipped  md5 {shipped_md5}")
            print(f"      recorded    {', '.join(stamps)}")
            print("      Restore the frozen document from git; do NOT re-write to make this "
                  "pass.")
            return 1
        print(f"GREEN — PREREGISTRATION.md matches its recorded FREEZE stamp")
        print(f"        md5 {shipped_md5}  (frozen; verified against audit_log.md)")
        drift = drifted_sources(shipped)
        if drift:
            print(f"\n  INFO — {len(drift)} pinned source(s) have moved since the freeze. This "
                  f"is EXPECTED\n         and is not a failure: the document is a snapshot of "
                  f"what was preregistered.")
            for label, why in drift:
                print(f"           · {label}  ({why})")
            print("         The frozen document must NOT be rebuilt from these. A new "
                  "--write\n         produces a DIFFERENT experiment record needing its own "
                  "stamp and spend-go.")
        else:
            print("        every pinned source is still byte-identical to its inlined copy")
        return 0

    # --- PRE-FREEZE: no stamp yet, so byte-identity to a fresh assembly is the invariant.
    try:
        fresh = assemble()
    except FileNotFoundError as exc:
        print("RED — cannot assemble: a pinned source is missing.")
        print(f"      {exc}")
        print("      Pre-freeze, byte-identity to a fresh assembly IS the invariant, so an "
              "unbuildable\n      assembly is a failure, not an exemption.")
        return 1
    if shipped != fresh:
        print("RED — shipped PREREGISTRATION.md differs from a fresh assembly (no freeze "
              "stamp recorded).")
        print("      A source changed after the document was built. Re-run --write.")
        print(f"      shipped md5 {shipped_md5}")
        print(f"      fresh   md5 {hashlib.md5(fresh.encode()).hexdigest()}")
        for label, why in drifted_sources(shipped):
            print(f"        · {label}  ({why})")
        return 1
    print(f"GREEN — PREREGISTRATION.md is byte-identical to a fresh assembly (DRAFT — no "
          f"freeze stamp recorded)")
    print(f"        md5 {shipped_md5}")
    print(f"        every verbatim appendix matches its canonical source")
    return 0


def selftest() -> int:
    """Two-sided controls on the mode split. Relaxing a check owes a demonstration that
    what remains still bites — the whole point is that the fatal case stayed fatal."""
    ok = True

    def chk(label, cond):
        nonlocal ok
        print(f"  {'PASS' if cond else 'FAIL'}  {label}")
        ok = ok and cond

    import tempfile, shutil
    global AUDIT, OUT
    real_audit, real_out = AUDIT, OUT

    def under(log_text: str, doc_text: str) -> int:
        d = pathlib.Path(tempfile.mkdtemp())
        try:
            globals()["AUDIT"] = d
            globals()["OUT"] = d / "PREREGISTRATION.md"
            (d / "PREREGISTRATION.md").write_text(doc_text)
            (d / "audit_log.md").write_text(log_text)
            return check()
        finally:
            globals()["AUDIT"], globals()["OUT"] = real_audit, real_out
            shutil.rmtree(d, ignore_errors=True)

    doc = "a frozen preregistration\n"
    doc_md5 = hashlib.md5(doc.encode()).hexdigest()
    print("freeze-mode controls — the fatal case must stay fatal:\n")
    chk("FROZEN + document matches its stamp -> GREEN",
        under(f"{FREEZE_MARK} {doc_md5} -->\n", doc) == 0)
    chk("FROZEN + document ALTERED -> RED (the case that must never be relaxed)",
        under(f"{FREEZE_MARK} {doc_md5} -->\n", doc + "tampered\n") == 1)
    chk("a DRAFT-only log is not read as a freeze stamp",
        under(f"draft md5 {doc_md5}, not the freeze\n", doc) != 0)
    chk("no log at all -> falls back to pre-freeze byte-identity",
        under("", doc) != 0)
    # Drift-list accuracy. Added after the first implementation reported 12 drifted sources
    # when 2 had changed; a list padded with false positives trains the reader to skip it.
    print("\ndrift-list controls — must name the moved source and ONLY the moved source:")
    real = PINNED[0][1]                                   # any pinned file that exists
    label0 = PINNED[0][0]
    doc_ok = f"| `{label0}` | `{md5_file(real)}` | why |\n"
    doc_stale = f"| `{label0}` | `{'0' * 32}` | why |\n"
    doc_absent = "| `something-else` | `" + "0" * 32 + "` | why |\n"
    chk("a source whose md5 still matches is NOT listed",
        label0 not in [d[0] for d in drifted_sources(doc_ok)])
    chk("a source whose md5 has changed IS listed",
        label0 in [d[0] for d in drifted_sources(doc_stale)])
    chk("a source absent from the pin table is flagged as NOT PINNED",
        any(d[0] == label0 and "NOT PINNED" in d[1] for d in drifted_sources(doc_absent)))

    print(f"\n{'GREEN — the mode split discriminates' if ok else 'RED'}")
    return 0 if ok else 1


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--write", action="store_true")
    ap.add_argument("--check", action="store_true")
    ap.add_argument("--selftest", action="store_true",
                    help="two-sided controls on the freeze/pre-freeze mode split")
    a = ap.parse_args()

    if a.selftest:
        return selftest()
    if a.check:
        return check()

    if a.write:
        if recorded_freeze_stamps() and OUT.exists() and \
                hashlib.md5(OUT.read_text().encode()).hexdigest() in recorded_freeze_stamps():
            print("REFUSED: PREREGISTRATION.md matches a recorded FREEZE stamp. Re-writing "
                  "would\n         replace the record of what was preregistered — a run may "
                  "have been made\n         under it. A new experiment record needs a new "
                  "stamp and its own spend-go.")
            return 1
        fresh = assemble()
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
