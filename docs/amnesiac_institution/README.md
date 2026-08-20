# `docs/amnesiac_institution/` — canonical version

**Canonical: `amnesiac_institution_v0_6.md`.** Seven versions live here; only this one is current.

A *checked fact, not a memory* (Build Discipline Pattern 2): the citations in v0.6 are verified by
`python3 python/claim_cite_check.py --check` in `scripts/gate.sh`, and
`audits/2026-08-13_oq287_defork/checks.sh` asserts v0.6's post-OQ-287 structure.

| file | status |
|---|---|
| `amnesiac_institution_v0_6.md` | **CANONICAL** |
| `_v0_5.md`, `v0.4`, `v0.3`, `v0.2`, `amnesiac_institution.md` | superseded; point-in-time, never edited |
| `V04_CONSOLIDATION_MANIFEST.md`, `literature_verification_memo.md` | build records |
| `*.pdf` | source literature |

**This paper is canonical for the institution** — the case study, differential amnesia, the memory
economy, the organizational form, self-instrumentation. It is **not** canonical for the derivation:
that is `../concealment/concealment_without_a_concealer_v0_4.md` (ISSUES OQ-287, ruled 2026-08-13).
§2.1–2.7 were vacated 2026-08-13; the numbers are deliberately empty and are never reused.

**Editing policy — CORRECTED 2026-08-20, because the stated policy was not the practised one.**
This line read *"v0.6 admits **pointer-only** edits — forward pointers and redirect notes — and no
content edits"* from `96db0124` (2026-08-14) until today, and **nine content-edit commits landed
against it in the six days since** (`98d0996a` … `da6de5b2`), including the withdrawal of §5.4's
pooled scalar, two new sub-sections, and this pass's marked corrections. A policy sentence that no
edit ever consulted is a success-shaped token for a governance step nobody performed — the paper's
own P8 — so it is restated as what is actually enforced rather than as an aspiration:

**v0.6 admits content edits, and they are MARKED, never silent.** A correction goes in the house
form (a dated `> **Correction marked (…)**` blockquote, or a `[COST CORRECTED]` / `[RETIRED]` box),
leaving the corrected text intact above it; a new as-of date gets its **own** dated block in the
front-matter in-place-corrections list and never extends an older one; and carriage invariants are
machine-checked by `python/amnesiac_carriage_check.py --check` in `scripts/gate.sh`, which goes red
if a carriage site moves without its expectation moving too.

§2.8 and §2.9 are marked **declared temporary** with the
practice paper as canonical destination; §2.9(b) is cited in correspondence already sent, so their
redirect is owed at sub-item granularity. That redirect and the extraction are OQ-287's two open
limbs.
