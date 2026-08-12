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
- `LETTER_2026-08-11_wu.md` — outbound correspondence to the source author, reporting the
  direction-(i) reconstruction's incidental finding: his failure-modes catalog and his labeled
  dataset assign **different classes to 10 of the same 22 incidents** (55% self-agreement, single
  author, own incidents, own taxonomy — the most favorable conditions a taxonomy will ever face).
  Witnesses that the comparison set is **unstable under its own author's hand**, which scopes every
  cross-coding claim this audit can make: a confusion matrix against a 55%-self-agreeing reference
  cannot separate *our taxonomy disagrees with Wu's* from *Wu's disagrees with itself*, and any
  effect-size floor here must be set against that, not against the published Table 1. Carries a
  repository annotation header (not part of the letter) recording reply status. **Not a
  dependency** — OQ-277 does not block on a reply.
- `SPEC_next_preregistration.md` — the specification governing the next stamp (see §"WHAT THIS
  AUDIT IS BLOCKED ON" in ISSUES OQ-277); no spend is requested by it.
- `responses/EMPTY_BY_DEFECT.md` — **the empty `responses/` directory IS the artifact of the live
  run.** Git does not track empty directories, so without this marker the directory vanishes from a
  fresh clone and "made 219 calls, persisted nothing" becomes indistinguishable from "never reached
  this phase." Witnesses the failure that `edc90409` records.
- `payloads_stub/`, `responses_stub/`, `STUB_RUN_README.md` — the **synthetic** full-scale stub run
  (`mode: stub`, 219 units, stamped 34 minutes AFTER the failed live run). Witnesses §2.1 row 6 of
  the SPEC — that gate 4, the output-side gate, is built and closes write-then-verify at production
  scale — which is otherwise a claim a cold reader cannot check. **Not results**; the README states
  the fail-closed `mode == "live"` requirement now carried as SPEC §3 item 6.

*This file is rewritten at close with the real verdict. Its current content is the honest
state: extraction underway, nothing measured.*
