# OQ-277 — RQ2 two-directional blind cross-coding (Wu A–E × published P1–P6): IN PROGRESS, pre-spend

**Executed:** 2026-08-10
**OQ:** OQ-277 (experiment), OQ-278 (fork-residue row), OQ-280 (this run produces the corpus §2.3 never produced)
**Verdict:** **NO RESULTS YET.** This directory is at Phase 1 (extraction). Nothing has been
coded, no model call has been made, and no matrix exists. The pre-registration is not yet
frozen and the operator spend-go has not been requested. Any reader arriving here before
`PREREGISTRATION.md` carries an md5 line in `audit_log.md` should treat every file present as
input material, not evidence.
**Substrate:** no pipeline run — this audit's substrate is the audit record itself
(`audits/`, 174 directories as of 2026-08-10) plus Wu, **arXiv:2606.14589v1**. The paper is
cited by its permanent arXiv identifier, not by a repository path: a local convenience copy
sits under `docs/amnesiac_institution/` but `*.pdf` is gitignored, so a fresh clone would not
have it and a path citation would be unreproducible. The material this audit actually codes —
Wu's failure-modes catalog and his labeled incident dataset — is frozen inside this directory
under `packets/wu_source/` with md5s, and IS tracked.
**Fired:** live — two plan-stage findings were independently verified before any spend and
are already minted: the pattern taxonomy is FORKED between `CLAUDE.md` and
`docs/technical/build_discipline.md` (OQ-278), and v0.3 §2.3 describes a P1–P6 classification
that produced no artifact (OQ-280). A third, smaller catch landed during step 1: Appendix B's
§4.5 denominator included an empty placeholder directory (73/175 → 73/174; headline 42%
unchanged). None of these required the experiment to run — the experiment's *design* surfaced
them.
**Evidence map:**
- `frame/` — the frozen frame: full directory listing, the census split (incident-bearing vs
  non-census), the pinned regeneration command, and the seeded samples. Witnesses that the
  sample was drawn before any coding, from a stated population.
- `packets/` — redacted incident units (both directions), the coder-facing material.
- `payloads/` — every exact assembled API payload, written BEFORE send. Witnesses H2 (leak).
- `responses/` — raw model responses, one file per call. The matrices must be reproducible
  from these alone.
- `controls/` — anchors, decoys, planted-leak payload, planted-broken unit, redaction-bias
  pairs. Each carries its pre-registered want=fire / want=pass.
- `matrices/` — confusion matrices and churn tables (Phase 4 output; absent until then).
- `PREREGISTRATION.md` — frozen at spend. `audit_log.md` — its md5, above the first result line.

*This file is rewritten at close with the real verdict. Its current content is the honest
state: extraction underway, nothing measured.*
