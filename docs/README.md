# `docs/` — the papers, and which one owns what

Two bodies of work live here. The **framework** (Deferential Realism: the seat theorem, the engine,
the corpus) is what the repository computes. The **method trio** below is what the repository
learned about producing research with workers that do not persist — a separate argument, cut into
three papers whose dependency is acyclic and machine-checked.

## The framework

**Start at `deferential_realism_paper_v8.md`** — entry point and canonical vocabulary; its closing
Appendix states the current state plainly. Detailed records: `deferential_realism_paper_v7.md`
(committer axis), `deferential_realism_paper_v6.13.1.md` (observer axis). The law:
`seat-theorem-v1.md`. Formal rules: `logic.md`, matched to `prolog/config.pl`.

## The method trio

Read them in this order. Each directory carries a `README.md` naming its canonical file; nothing
here is authoritative over those.

| # | paper | what it owns | size |
|---|---|---|---|
| 1 | [`concealment/concealment_without_a_concealer_v0_4.md`](concealment/) | **the derivation** — compression × framing Π × boundary; claims A1–A5 + C1 with kill conditions. Substrate-independent; the cases are Fogbank, scurvy, hunger stones | ~13k words |
| 2 | [`amnesiac_institution/amnesiac_institution_v0_6.md`](amnesiac_institution/) | **the institution** — the case study, differential amnesia, the five records, the memory economy, the organizational form, self-instrumentation, the measured incidence rate | ~44k words |
| 3 | [`practice/practice_paper_v0_1.md`](practice/) | **the practice** — the discipline documents read as a working method, machine-enforced rather than described | ~5.5k words |

**The ordering is the point, and it is enforced.** (1) cites neither sibling. (2) cites (1) for the
derivation and restates none of it. (3) cites both and restates neither. Cross-document claims are
pinned as `CWC:<label>@<digest>` over the whole source row, resolved by
`python3 python/claim_cite_check.py --check` in `scripts/gate.sh` — so editing a cited row moves its
digest and fires every citing site. Canonicity is a **checked fact, not a memory** (ISSUES OQ-287,
ruled 2026-08-13; both limbs discharged 2026-08-20).

**Shortest useful path in:** paper 3. It is the tightest of the three and derives the practice
rather than listing it; paper 1 is the argument underneath it, paper 2 the evidence base.

**What none of them claims.** All three are **n = 1, self-observed, undenominated**. There is no
comparison arm: whether the discipline *reduces* silent defects or only makes them visible is RQ1
(paper 2, §14), and it is unrun. Read every rate as a property of this record, not of the method.

## Everything else

`docs/` also holds working notes, sketches, results, and superseded drafts — several hundred files,
point-in-time and not maintained. `technical/` (wiring notes that caused real bugs), `design/`
(design intent + declared gaps), and `commitment_systems/` (the CS spec) are live and are named
from `CLAUDE.md`. If a file here is not reachable from `CLAUDE.md`, `README.md`, or this page,
treat it as archival.
