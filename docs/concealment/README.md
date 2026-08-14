# `docs/concealment/` — canonical version

**Canonical: `concealment_without_a_concealer_v0_4.md`.** Everything else here is superseded.

This is a *checked fact, not a memory* (Build Discipline Pattern 2): `scripts/gate.sh` runs
`python3 python/claim_cite_check.py --check`, which resolves every cross-document claim citation
against **this file's Appendix A**, so a rename or a stale pointer turns the gate red.

| file | status |
|---|---|
| `concealment_without_a_concealer_v0_4.md` | **CANONICAL** |
| `_v0_3.md`, `_v0_2.md`, `_v0_1.md` | superseded; point-in-time, never edited |
| `theory_v02_to_v03.diff`, `theory_v03_to_v04.diff` | build record |

**This paper is canonical for the derivation** (ISSUES OQ-287, ruled 2026-08-13).
`../amnesiac_institution/amnesiac_institution_v0_6.md` cites it and does not restate it; it is
canonical for the institution. The ordering is acyclic: this paper never cites v0.6 for the
derivation.

**Citing a claim from here.** Use `CWC:<label>@<digest>` — e.g. `CWC:A2@31548228`. The label set is
Appendix A's and is **open** (`A`, `E`, `P`, and the corollary `C1`); the Preface's summary of it is
a summary and has been stale once, so Appendix A is authoritative. The digest covers the **whole
row**, so editing a row's kill condition moves the pin and fires every citing site — that is the
mechanism working. Recipe and rationale:
`../../audits/2026-08-13_oq287_defork/claim_digest.sh`.
