# `docs/practice/` — canonical version

**Canonical: `practice_paper_v0_1.md`.** It is currently the only version.

A *checked fact, not a memory* (Build Discipline Pattern 2), by the same mechanism as its two
siblings: the cross-document claim citations in this paper resolve against
`../concealment/concealment_without_a_concealer_v0_4.md`'s Appendix A and are verified by
`python3 python/claim_cite_check.py --check` in `scripts/gate.sh`, so a rename or a stale pointer
turns the gate red. A new directory without a README is the defect OQ-287 closed; this file exists
before the paper needs it.

| file | status |
|---|---|
| `practice_paper_v0_1.md` | **CANONICAL** |

## What this paper is canonical for

**The discipline documents as a development practice** — `CLAUDE.md`, `build_discipline.md`,
`design_discipline.md`, `ISSUES.md`, `KNOWN_STATE.md` read as a working method for research whose
workers do not persist, and machine-enforced rather than described.

It is **not** canonical for:

| topic | canonical home |
|---|---|
| the derivation (compression, framing non-identifiability, warrant transfer) | `../concealment/concealment_without_a_concealer_v0_4.md` |
| the institution (the case study, differential amnesia, the memory economy, the organizational form, self-instrumentation) | `../amnesiac_institution/amnesiac_institution_v0_6.md` |

**The ordering is acyclic and is the whole point of OQ-287.** This paper cites both and restates
neither. Neither cites this paper for what it owns. Where this paper needed the derivation it pinned
a claim (`CWC:<label>@<digest>`); where it needed the institution it cited v0.6 as authority.

## What MOVED here, and what did not

**Moved — v0.6 is now the superseded side for exactly two subsections**, which keep their numbers
there and carry forward pointers:

| from | to |
|---|---|
| `amnesiac_institution_v0_6.md` §2.8 — the unmarked perturbation | **§III** |
| `amnesiac_institution_v0_6.md` §2.9 — the negative control | **§V** |

**Did NOT move.** v0.6 §7, §7.4, §7.8, §8, §9, §9.4, §10 remain canonical *there*. This paper draws
on them and cites them as authority. Confusing the two directions would manufacture the duplication
OQ-287 exists to have closed — see `../../audits/2026-08-13_oq287_defork/EXTRACTION_PROMPT.md` §3,
whose two tables are the operative distinction.

## Citing a claim from the derivation

Use `CWC:<label>@<digest>` — e.g. `CWC:A2@31548228`. The digest covers the whole Appendix A row, kill
condition included, so editing a row fires every citing site: **on a fire, re-read the site and
decide; never bump the hex.** Recipe: `../../audits/2026-08-13_oq287_defork/claim_digest.sh`.

**Aptness is not checked and cannot be.** `claim_cite_check` verifies that a pin matches its row; it
is silent on whether that row is the *right* one to cite. Every citation in this paper therefore
carries a one-line aptness note in `../../audits/2026-08-20_oq287_limb1_extraction/APTNESS.md`.
The gate's green tick does not discharge that and will read as though it does.
