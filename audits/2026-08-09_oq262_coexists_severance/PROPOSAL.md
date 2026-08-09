# PROPOSAL — OQ-262 severance/intrinsicness audit on authored `coexists_with` edges

**Date:** 2026-08-09 (Phase B; Phase A recon committed at `7de8e5f9`)
**OQ:** OQ-262 (open, P3, splits_from OQ-259)
**Plan of record:** `~/.claude/plans/review-oq-262-from-issues-md-ancient-stonebraker.md`
**Grammar:** `PREREGISTRATION.md` (this directory) — DRAFT until R2 sign-off; frozen after.

## What runs (after R2 signs)

**Phase C — execution, in order:**

1. **Corpus freeze witness:** md5-fingerprint `prolog/testsets/` and
   `prolog/archives/datasets/kernel_test/` at Phase C START and END — the freeze
   spans the whole phase (the judged tier spans turns). The fingerprint pair is what
   the writeup cites; a mismatch invalidates the crossing rows (operator topic runs
   land stories mid-session — witnessed 2×, 2026-07-23).
2. **Mechanical tier** (Prolog probe in this directory, logs saved): M1/M2 per
   PREREGISTRATION §D over fiat (live leg) and kernel_test (overlay), controls pasted.
   - Verdict constitution: M1/M2 pass iff they fire on exactly the named must-fire
     targets and nowhere else on their substrate. A miss either direction = the
     mechanical tier is broken; fix and rerun before the judged tier (implementation
     controls — prereg §G).
3. **Judged tier, CP first:** blind subagent(s) per prereg §E — one per kernel
   family, each receiving only its three `.pl` files + the §J instrument text.
   A5 leak check performed and pasted BEFORE launch. Verdicts + mutation text +
   quotes recorded verbatim in `READOUT_cp_blind.md` before comparison. Gate
   evaluated per §E outcome semantics; any HALT outcome stops here and the writeup
   records the miscalibration (still a valid closure input).
4. **Fiat judging** (only if the gate passes): main instance judges the 13 fiat
   pairs under the frozen grammar, one row per pair:
   {pair, directed relations, mechanical flags, verdict class, mutation text,
   witness quotes, RULED|INFERRED}. Artifacts: `edge_audit.json` (the annotation
   surface for this closure — audit-dir artifact, cited from the OQ) +
   human-readable `FINDINGS_fiat.md`. Promoting annotations into an authored
   in-corpus field is OUT of scope (schema change → test-bed posture, separate
   decision; goes to writeup Residue).

**Phase D — residue routing (separate commits, output-changing discipline):**

Pre-derived movement (RECON §1): routing changes can move output ONLY on the live
leg (18 forecloses / 100 coexists prefixed_to_bare edges); twins and kernel_test
predict byte-identical output. Per-consumer prediction finalized against the
Phase-A census before each edit; the diff must match it.

1. `cs_pattern_detection.pl:355` (`cs_displaced_beneficiary/1`): resolve `Sibling`
   through `cs_edge_target_member/4` (kernel + pairs join — mirror the
   `json_report.pl:1964–1990` routed-consumer pattern). Witness: same-session
   clean-vs-edited pipeline diff (exit 0 + output mtime advanced + per-constraint
   compare + corpus md5 identical across both halves).
2. `cs_corpus_analysis.pl:131–149`: same routing. Output is console-only — witness
   via before/after console capture over the same frozen corpus. **If no
   witnessable surface exists, DROP the edit** (operator condition) and record as
   declared residue.
3. `drl_composition.pl:122` (`detect_necessity_inheritance/2`): NO edit. Mint a new
   OQ (authored Priority + Deps): raw-match + `Source` bound to a constraint id
   where `cs_reading_relation/3` is UID-keyed — the predicate can essentially never
   fire on real corpus data; the fix requires deciding what Source should bind to
   (engine design). Note the allowlist entry (`axis_boundary_allowlist.txt:29`,
   verify at edit time).

Commit split: behavior-preserving (probe scripts, docs) separate from
output-changing (the two routings), each routing its own commit with pasted witness.

**Phase E — WRITEUP.md + close:** per `audits/README.md` format (header, evidence
map, Residue); correct the OQ entry's stale premises (OQ-23 status, quarantine-file
semantics, CP divergence 253/468 → live 164, 2-vs-3 un-routed consumers); OQ-262 →
resolved (compress-on-close); mint the drl_composition OQ; audits/README.md index
row; regenerate `issues/INDEX.md`; KNOWN_STATE.md entry + promotion test; `[GATE]`.

## What constitutes each verdict

- **Per-pair verdicts:** constituted by the frozen grammar (prereg §A–§C) applied to
  the authored facts, each row carrying its witness quotes and footing. Never a rate
  (prereg §I).
- **Control pass/fail:** prereg §E outcome semantics, written before the run.
- **Routing success:** diff matches the pre-derived prediction over a
  fingerprint-identical corpus; exit 0 + mtime advanced (the false-pass guard).
- **Overall closure verdict altitude:** "N pairs audited under the frozen grammar,
  per-pair verdicts recorded" — with the prereg §H downgrade if RULED is a minority.

## Evidence plan (artifact → claim)

| artifact | witnesses |
|---|---|
| `recon_probe.pl` + 6 logs | Phase A substrate claims (committed `7de8e5f9`) |
| `RECON.md` | edge tables, inventory, census, movement pre-derivation |
| `PREREGISTRATION.md` (signed) | the frozen grammar + controls + declarations |
| fingerprint files (Phase C) | corpus freeze across the judged tier |
| mechanical-tier log | M1/M2 fire/no-fire on named targets |
| A5 leak-check paste | blinding-through-instrument verified |
| `READOUT_cp_blind.md` | CP verdicts verbatim, pre-comparison |
| `edge_audit.json` + `FINDINGS_fiat.md` | the 13 per-pair annotations (deliverable) |
| routing diffs + console captures | Phase D behavior change matches prediction |
| `WRITEUP.md` | closure verdict at scoped altitude + Residue |

## R2 decision points (blocking; the operator's seat)

1. **Sign PREREGISTRATION.md as written** (§K) — or edit freely first; the freeze
   starts at the signature.
2. **Arm A vs Arm B** (prereg §E): the plan pinned the CP triplet (arm A); recon
   found the triplet authors zero axioms (rider 3 confirmed) and found an
   axiom-bearing sibling family (`state_killing_authority`) in the same archive.
   **Recommendation: Arm B** — it adds (a) a fourth expected-nonzero gate row whose
   evidence base matches fiat's (axioms + declared contradiction), and (b) the
   expected-`genuine` pair the fiat family cannot supply (prereg §F Branch 2 fired:
   every fiat foundational is a rival exclusive-location claim), i.e. the two-sided
   calibration amendment A1 asks for. Cost: one extra blind subagent. Both arms
   keep the plan's triplet gate intact; arm B only adds rows.

3. (Non-blocking, flagged per the one-sentence rule) `cs_corpus_analysis.pl`'s only
   witnessable surface appears to be its own console output; if the operator regards
   a console before/after capture as insufficient witness for an output-changing
   edit, say so at R2 and Phase D step 2 becomes a DROP + declared residue.

## Stop

Per the plan: this session stops here. `blocked_on_human oq262-r2-grammar-signoff`
is authored on the OQ-262 Deps line; Phases C–E run only after the operator's R2.
