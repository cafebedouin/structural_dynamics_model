# Investigations ledger — register the decision to LOOK, before the outcome is known

**Adopted:** 2026-08-19 (OQ-276 ruling, operator, second-instance reviewed). This file exists to
make `Fired: no` REACHABLE: audit directories are created on suspicion, so the Fired: tally is a
yield over audits-that-got-written (0 of 53 WRITEUPs conclude pure confirmation, measured
2026-08-13) — publication bias, with the Fired: rate as its impact factor. Registration is the
fix, and this is the registration.

**OPENING CRITERION (the ledger's value is entirely determined by this line):** open a line
whenever you are about to CHECK something you do not already know the answer to, BEFORE running
the check — not when you decide it is worth an audit dir. Small, quick, mostly-negative checks
belong here deliberately: hunches that dissolve in five minutes are exactly the `no` population
this ledger exists to recover. If the author decides at hunch-time whether something "counts as
an inquiry," the same selection that produced 0-of-53 operates one level up and the ledger
measures nothing.

**CLOSING:** attach the Fired: bit (`live | latent | no`) to the line at close REGARDLESS of
outcome, with a one-clause result. A line that graduates to an audit dir also notes the dir; its
WRITEUP bit and ledger bit must agree.

**Open lines are INFORMATION, never a hygiene metric:** `apparatus_instrument.py` reports the
open count alongside the tally (reporting only — a rising open count is not by itself a defect,
and pressure to close lines is pressure to write whatever bit clears them fastest).

**Format:** one line per inquiry, prepended (newest first):

```
- [ ] YYYY-MM-DD — <the question, one clause>
- [x] YYYY-MM-DD — <the question> → Fired: no — <one-clause result> (closed YYYY-MM-DD)
```

---

- [x] 2026-08-21 — do all 21 Assumed-substrate lines of the OQ-306 plan (let-s-try-reviewing-306-vectorized-kernighan) hold against the repo at HEAD e873b1ba → Fired: live — 19/21 hold as written (live leg 279 = 253 story + 26 axiom_contradiction, disjoint and total, subject==basename on all 253 and on all 3926 twin-leg files); two plan claims flipped: S16's golden baseline `outputs/golden_classifications.json` is ABSENT (M7/P8 anchor unavailable — C3's witness must be chosen before C1, not discovered after) and S17's control (a) cites `audits/oq140_divergence_extract.py` whose real path is `python/audits/...`, losing the deliberate outside-the-narrowest-tree property (closed 2026-08-21)
- [x] 2026-08-21 — is `clause(narrative_ontology:cs_axiom_contradiction(_,_), true, Ref)` accessible on the loaded live corpus (S20), and does `clause_property(Ref, source(F))` vs `file(F)` differ for consulted testsets → Fired: no — accessible with no throw despite the predicate being static (`multifile`, not `dynamic`); `source/1` and `file/1` return identical full paths (no include indirection); 92 clauses over exactly 26 distinct source basenames (closed 2026-08-21)
- [x] 2026-08-21 — is `python/*.json` gitignored, i.e. can `corpus_census_baseline.json` actually be committed (S21) → Fired: no — not ignored; `git ls-files 'python/*.json'` already tracks several, so the D3 pin is genuinely shareable (closed 2026-08-21)
- [x] 2026-08-21 — does `load_warning_gate.py` capture `[corpus]`-prefixed info lines, and is there any last-line parser of loader stderr the census line would break (S10 + S13) → Fired: latent — the gate captures NONE of the three proposed lines for two independent reasons (`collect_warnings()` runs `swipl -g "[stack], halt"` and never loads the corpus; its regex is `^(Warning|ERROR):`, which `[corpus] WARNING:` also fails), so no allowlist entry is needed and `load_warning_allowlist.txt` drops off C1's file list — but the same fact makes D4 arm 3 an unenforced stderr line rather than a gate arm (Pattern 6 shape), a defect conditional on arm 3 being built; separately all four `[corpus]` consumers are human-progress echo filters that parse no value, and the only `tail -1` is `gate.sh` over checker output, so no last-line parser breaks (closed 2026-08-21)
- [x] 2026-08-20 — step-0 sole-writer re-check at the harmonic-launching-spark checkpoint: is this session still the only writer → Fired: live — HEAD had moved c81bc4bb→b316c273 (5 commits, the OQ-332/OQ-276 ruling session) with 3 other live claude processes; execution held until operator confirmed sole-writer; registered retroactively — the check ran before the same session's new open-a-line rule reached this instance (closed 2026-08-20)
- [x] 2026-08-20 — do all Assumed-substrate lines of plan harmonic-launching-spark hold against the repo → Fired: no — every line confirmed at c81bc4bb (papers/commits/OQ-334 content/gate GREEN; the fixer figure's exactly-two records and OQ-293's missing E1 pin match the plan's ⊗ corrections); registered retroactively, same reason as above (closed 2026-08-20)
- [x] 2026-08-20 — what mechanism routes OQ-276 into the resolver's BLOCKED ON YOU section (Ω_P type line, empty Deps, or something else), and what edit routes it out → Fired: live — omega_resolver.py:406-408: every LEAF OQ whose Ω-type line reads Ω_P routes to blocked_on_human; no waiting-on-condition relator exists, so routing OQ-276 out requires either a resolver change or a type-line reorder, neither free (closed 2026-08-20)
