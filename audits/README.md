# audits/ — consolidated audit archive

**Mandate (2026-06-04): every completed audit lives here, in one subdirectory per audit,
named `<YYYY-MM-DD>_<slug>/`.** The subdirectory holds the writeup AND its evidence
artifacts (probe scripts, raw JSON/TSV/logs) together. Do not scatter audit writeups into
`docs/` or leave findings only in `outputs/` — `outputs/` is gitignored, so findings left
there are unversioned and disappear on a fresh clone.

Conventions:

- **`outputs/` is the live workspace; `audits/` is the dated archive.** Audit *scripts* stay
  in `python/audits/` (or `prolog/`) and keep reading/writing `outputs/` — re-running a
  producer regenerates the workspace copy. When an audit pass completes, move (or copy) the
  writeup + evidence into its `audits/<date>_<slug>/` directory. Probe `.pl`/`.py` snippets
  written for one audit are archived in its subdirectory.
- **Date** = the audit's execution date (from the writeup header), not the consolidation
  date. **Slug** = short kebab/snake topic name.
- **Point-in-time documents are not retro-edited.** Archived writeups may reference paths as
  they were at execution time (e.g. `outputs/...` footers); only *live* pointers (ISSUES.md,
  KNOWN_STATE.md, code comments, orientation docs) are kept current.
- Audits follow the methodology in `CLAUDE.md` → Audit Methodology (recon → proposal →
  execution → writeup) and cite the pipeline manifest of the run they measured.

Consolidated 2026-06-04 from: `docs/*.md` audit writeups, `docs/audits/`,
`docs/technical/schema_drift_audit.md`, `outputs/` audit families, root `audit/`,
`audit_data/`, `audit_proposal/` (+ its `audit/agy/` variant), and `phase1/`
(see KNOWN_STATE.md 2026-06-04 entry for the move map and fork notes).

## Citing third-party sources (operator ruling, 2026-08-10)

**Do not track third-party PDFs.** Cite an external paper by its **permanent identifier**
(arXiv id, DOI) — never by a repository path to a local convenience copy. `*.pdf` is
gitignored, so a path citation is unresolvable for anyone who clones: a citation that does
not carry its own retrieval path is not a citation. `python/audit_citation_status.py`
gates on exactly this (`untracked-frozen-evidence`), and it is right to.

If offline access genuinely matters for reproducibility, the answer is **a fetch script
carrying the identifier and a checksum**, not the binary. Material actually *analysed*
(extracted tables, fetched catalogs, datasets) is different: freeze it inside the audit
directory, tracked, with md5s — that is evidence, not a reference. Worked example:
`audits/2026-08-10_oq277_rq2_crosscoding/` cites the paper as `arXiv:2606.14589v1` while
freezing the two artifacts it codes under `packets/wu_source/` with `FETCH_MANIFEST.txt`.

## Replicating a cross-story finding (runbook, 2026-08-15)

If the finding you are auditing composes **more than one generated story** — a kernel's pairwise
Jaccard, an H¹-across-readings, any "reading A relates to reading B like X" — its stability is the
product of its components', not any one story's, and a single draw is an observation. Runbook:
**`docs/technical/codraw_replication.md`** (pre-registration first, read-path controls before
spend, k draws into run-tagged subdirs that never touch the live corpus). Worked example:
`2026-08-14_cheap_confession_codraw_replication/`, where a structure held its per-pair counts
across all four runs and still failed to replicate.

## Writeup format (adopted 2026-08-06; forward-only)

Codifies the majority practice found by the 2026-08-06 index build: 57/168 directories already
used `WRITEUP.md`/`writeup.md`, but four rival entry-point names (WRITEUP/FINDINGS/README/
bespoke) and three pre-registration spellings left several directories mechanically
unindexable — a Pattern-2 silent fork on the prose substrate (`build_discipline.md` → *An audit
directory has one entry point*). Existing directories are point-in-time and are **not
renamed**; every NEW audit follows this.

**Machine-checked (gate-wired):** `python3 python/audit_writeup_gate.py --check` runs in
`scripts/gate.sh` — dirs dated >= 2026-08-06 must carry `WRITEUP.md` and no rival
pre-registration spelling; malformed directory names fail closed; pre-adoption dirs are exempt.
The checker runs its own 8-fixture selftest (6 violation shapes flagged, 2 conforming shapes
clean) before every live sweep, so a green line is never a didn't-look.

**One entry point: `WRITEUP.md` (uppercase), exactly one per audit directory.** It is the file
the index reads and a cold reader opens first, and the only file that must exist — a HALT, an
abandoned probe, or an evidence dump still gets a short WRITEUP.md saying exactly that. Phase
files keep reserved names, and WRITEUP.md links each one present:

- `RECON.md` — what data exists, what is answerable (read-only pass).
- `PROPOSAL.md` — exactly what will run and what would constitute each verdict.
- `PREREGISTRATION.md` — this single spelling (not `PRE_REGISTRATION.md`/`PREREG.md`).
  **Frozen at spend time**: never retro-edited; the writeup quotes it, never amends it.
  **And the freeze must be WITNESSABLE, not narrated** (operator, 2026-08-09): record the
  file's md5 into the audit log BEFORE the first run, so the md5 line sits physically above
  the first result line — a prereg file merely sitting next to results doesn't witness the
  ordering. If the prereg is amended after a first run, log the new md5 at its position;
  that history is the honest record. Template:
  `audits/2026-08-09_oq151_dual_gauge/audit_log.md`.

**HEAD STAMP PAIR — `audit_log.md` records `git rev-parse HEAD` at OPEN and again at CLOSE, and
the writeup states the comparison** (operator ruling, 2026-08-17). One stamp detects nothing; two
bracket the session and turn *"was there a concurrent writer?"* from an inference into a diff. If
they differ, name the intervening commits and state the blast radius **on the audit's own
read-set** — `git diff --stat <open> <close> -- <paths the probes touched>`; an empty diff over
those paths is the clean result, and it is a different thing from never having looked. **This is
DETECTION, not prevention** — prevention needs a lock file, which this workflow deliberately does
not have. Motivating episode: `audits/2026-08-17_oq251_natural_law_reachability/`, where an
executor read a dirty→clean `git status` transition as a stale index stat while another instance
was committing between the two commands. **A dirty→clean transition is affirmative evidence OF a
writer, not of its absence.** Ruling, and the declined `.claude/settings.json` hook alternative
(a hook that fails open is worse than the prose rule it replaces): **OQ-297**.

**PRIOR-ART GREP, same pass as the finding** (operator ruling, 2026-08-17). Before a finding is
written down, grep `docs/technical/build_discipline.md` for its predicate / atom / mechanism name,
and **record the hit-or-no-hit in the finding itself** — one line, e.g. *"prior art: BD-P3
(`build_discipline.md:601`, 2026-05-30) — this is a RE-DISCOVERY"* or *"prior art: none
(grepped `has_viable_alternatives`, `intent_viable_alternative`)"*.

**Why it is a step and not a habit:** the obvious falsifier for whether documentation is reaching
instances — *count re-discoveries* — **has the same defect as the thing it measures.** It counts
only the re-discoveries somebody noticed were re-discoveries; an undetected one is
indistinguishable from a novel finding, so the metric drifts toward *"routing is fine"* exactly as
routing gets worse. Making the grep a same-pass step converts detection from luck into a checklist
item, and only then does the count mean anything. Witnessed: `TRIPWIRE 5` in KNOWN_STATE 2026-08-17
was published as a discovery and was BD-P3, documented 2026-05-30 with the same worked example —
caught by operator challenge, which is not a mechanism.

**COMMIT `audit_log.md` FIRST** (same ruling) — before any code commit, then append as the audit
proceeds. **Commit order is a primitive; a blob comparison after the fact is a reconstruction.**
This bites on pre-edit reads: an audit that pastes a file's current text before correcting it *has*
witnessed that text, but if the log lands in a later commit than the edit, git cannot testify to
the ordering and the witness must be rebuilt by diffing against the edit's parent. Committing the
log first makes the ordering free.
- `FINDINGS_<leg>.md` / `READOUT_<arm>.md` — per-leg or per-arm interim results in multi-phase
  audits; at close, WRITEUP.md is written (or promoted from the final findings file) and links
  them all.

**Required WRITEUP.md header block:**

```
# <slug or OQ-NN> — <verdict-bearing headline at its scoped altitude>

**Executed:** YYYY-MM-DD (= directory date)
**OQ:** OQ-NN[, OQ-MM] — or `none` + why this ran without one
**Verdict:** one sentence: the claim, its scope, the caveat the body carries
**Substrate:** corpus leg(s) + pipeline-manifest cite (n_constraints, code_commit,
  code_dirty) — or `no pipeline run`
**Fired:** live — <what fired/flipped> | latent — <hazard found, conditional on
  unproduced input> | no
**Evidence map:** one line per artifact in this directory — what it is, which claim
  it witnesses
```

**The `Fired:` bit (adopted 2026-08-10, forward-only; dirs dated > 2026-08-10).** The
apparatus's own catch-rate instrument (operator ruling 2026-08-10; the "one-fix-bite"
question from `build_discipline.md` → *Don't answer "does the apparatus pay for itself?"
by producing more apparatus*, installed in substrate). `live` = a control fired, a claim
flipped, or a verdict a consumer actually saw changed; `latent` = a real defect found but
conditional on an input the system does not currently produce; `no` = pure confirmation.
Machine-checked by `python3 python/apparatus_instrument.py --check` in `scripts/gate.sh`
(selftest rides every run); the rolling rate is REPORTED, never gated — reading it is the
operator's seat. Pre-adoption dirs are exempt; a voluntarily backfilled `Fired:` line still
counts toward the rate.

Body sections are flexible; the reader contract is not: the **headline carries the verdict**
("proxy only" in the body means "proxy only" in the title — build_discipline → *over-confident
moves* #1), **results paste their witnesses** (paste-or-untag), and a closing **Residue**
section names what changed in substrate (OQ status edits, KNOWN_STATE entries, next steps) so
the cold read finds the handoff.

**The evidence map is mandatory, not decorative.** Every artifact in the directory appears in
it; an artifact no claim consumes is either deleted or declared dead there (Pattern 1 applied
to audit dirs). Evidence-only directories whose findings live wholly in ISSUES.md/KNOWN_STATE.md
are a legacy shape — new audits always carry the writeup, however short.

## Index (as of 2026-08-06 — 168 audits)

One row per audit directory: the main writeup file and its headline. Derived by reading each
directory's writeup title on 2026-08-06 — **append a row when landing a new audit**; rows for
existing audits are point-in-time and not retro-edited. Directories marked *(evidence only)*
hold raw artifacts with no markdown writeup (their findings live in ISSUES.md / KNOWN_STATE.md).

| Directory | Writeup | Headline |
|---|---|---|
| `2025-02-23_gap_tests` | `(evidence only)` | Gap-test testset variants (117 .pl files; u1/u2 exemption perturbation series) — no writeup |
| `2025-05-15_recon_2` | `recon_2_inventory.md` | Recon-2 Inventory — Phase 1 Output |
| `2026-02-23_false_ci_rope` | `false_ci_rope_audit.md` | false_ci_rope Internal Structure Audit |
| `2026-02-23_report_generator` | `report_generator_audit.md` | Report Generator Audit |
| `2026-02-23_scaffold_piton_gate` | `scaffold_piton_gate_audit.md` | Scaffold & Piton Gate Trace Audit |
| `2026-02-25_spectral_laplacian` | `(evidence only)` | Spectral Laplacian probe scripts + outputs (phase0–2, haiku leg) — no writeup |
| `2026-02-28_codebase_audit_data` | `mandatrophy_divergence_analysis.md` | MANDATROPHY x Large-Divergence Overlap Analysis |
| `2026-03-07_logic_divergence` | `logic_divergence_audit.md` | Logic Divergence Audit |
| `2026-04-14_blocking_gate` | `blocking_gate_audit_20260414.md` | Blocking Gate Audit — 2026-04-14 |
| `2026-04-14_ccdp` | `ccdp_audit_report.md` | CCDP Audit Report |
| `2026-05-02_trifurcation_mapping` | `trifurcation_mapping_audit.md` | Trifurcation-to-Apparatus Mapping Audit |
| `2026-05-07_cluster_space` | `cluster_space_audit_phase1.md` | Cluster-Space Audit — Phase 1 Results |
| `2026-05-08_metric_two_hub` | `metric_audit_writeup.md` | When T Isn't the Boundary: Implementation Structure and Position-Space Geometry in the Deferential Realism Apparatus |
| `2026-05-08_sheaf` | `sheaf_audit_writeup.md` | When the Site Changes the Boundary: Scope Modifier Mechanics and the Limits of Site-Stability |
| `2026-05-15_repo_reorg_proposal` | `(evidence only)` | Repo reorganization proposal (proposal/ + agy_variant/) — no writeup |
| `2026-05-17_audit3_profile_accumulation` | `audit3_report.md` | Audit 3 Report — Profile Accumulation Impact on Corpus |
| `2026-05-20_ab_test` | `(evidence only)` | A/B threshold-stripping generation test — prompts, run logs, score harness (finding: mountain fails stripped arm; see KNOWN_STATE) |
| `2026-05-29_bc_coupling` | `bc_coupling_audit.md` | B/C Coupling Audit |
| `2026-05-29_position_geometry` | `position_geometry_audit.md` | Position-Space Geometry Audit |
| `2026-05-30_authoring_closure_fabricated_defaults` | `audit_authoring_closure_fabricated_defaults.md` | Audit: Authoring-Closure + Fabricated-Default Census — Findings |
| `2026-05-30_schema_drift` | `schema_drift_audit.md` | Schema Drift Audit — 2026-05-30 |
| `2026-05-31_wiring_gap_census` | `wiring_gap_census.md` | Wiring-Gap Census — prompt ↔ schema ↔ engine disagreements |
| `2026-06-03_purity` | `purity_audit_20260603.md` | Purity Audit — is each purity notion correct for its application? (2026-06-03) |
| `2026-06-03_reading_diff_census` | `reading_diff_census.md` | reading_diff within-kernel census (OQ-59 #3) |
| `2026-06-04_fnl_bait_confound` | `fnl_bait_confound_audit_20260604.md` | FNL bait-confound audit — is the disguise-signature dominance substantive or a generator artifact? |
| `2026-06-04_oq65_bait_census` | `writeup.md` | OQ-65 Detector-Bait Census — Writeup (2026-06-04) |
| `2026-06-04_oq71_depth_lineage` | `(evidence only)` | OQ-71 depth-lineage evidence (six-dim richness results, gate2 captures) — writeup lives in ISSUES OQ-71 |
| `2026-06-05_flat_control_generate_both` | `writeup.md` | Generate-Both Landed: forced-flat control on every kernel, with a mechanical alignment key (OQ-76 primary fix) |
| `2026-06-05_generation_pipeline_deleak` | `writeup.md` | Generation-Pipeline De-Leak — Execution Writeup |
| `2026-06-05_kernel_gate_replication` | `writeup.md` | K1 — Kernel-Gate Replication Probe: a real, topic-classed boundary band; gate noise localized to the binary decision |
| `2026-06-05_scope_count_distribution` | `writeup.md` | SCOPE Count-Distribution Probe — the 7-7-7 watch is RESOLVED (coincidence + run noise, not an implicit target) |
| `2026-06-06_kernel_first_phase0` | `PHASE0_READOUT.md` | Phase 0 (Step 0a) readout — kernel-first auto-routing: self-classifier accuracy |
| `2026-06-07_stakeholder_layer_migration` | `AUDIT.md` | Stakeholder-Layer Migration — Pass 1 audit + pilot steps 0/1/1b (committer-axis existence-proof prerequisite; 9 writeups) |
| `2026-06-08_coordination_washing_clean_pass` | `FINDINGS.md` | Does the DR classifier ever return a clean pass, or is coordination-washing its default? |
| `2026-06-08_typea_template_extensibility` | `AUDIT.md` | Pre-Rebuild Audit — Template Extensibility & Type A (Temporal) Authoring |
| `2026-06-09_capture_axis_cut_control` | `FINDINGS.md` | Step 1 capturer-cut discriminating control — FINDINGS (HALT) |
| `2026-06-09_imputation_shim_census` | `census.md` | Imputation-Shim Blast-Radius Census |
| `2026-06-09_oq07_mismatch_runtime_probe` | `WRITEUP.md` | OQ-07 runtime probe — hand-traced mismatch candidate verified at runtime |
| `2026-06-10_external_review_vote_market` | `README.md` | External-review triage — vote-market run (2026-06-10) |
| `2026-06-10_external_review_xprize` | `README.md` | External-review triage — XPrize / rejuvenation run (2026-06-10) |
| `2026-06-10_gain_flow_prototype` | `FINDINGS.md` | Findings — OQ-92 step-2 gain-flow prototype (run 2026-06-10) |
| `2026-06-10_oq57_live_rewitness` | `FINDINGS.md` | OQ-57 re-witness — live + archive corpora (2026-06-10) |
| `2026-06-10_oq77_serial_kill_condition` | `writeup.md` | OQ-77 kill-condition execution: giant_component_analysis SIGSEGV is not serially reproducible |
| `2026-06-10_oq81_reading_upstream_recon` | `AB_RESULTS.md` | OQ-81 A/B results — 2026-06-10 |
| `2026-06-10_oq92_step3_preregistration` | `FINDINGS.md` | Findings — OQ-92 step-3 build stages (running log; one section per landed stage) |
| `2026-06-10_oq93_grid_viability_probe` | `FINDINGS.md` | Findings — OQ-93 grid-viability probe (run 2026-06-10; pre-fix + post-fix) |
| `2026-06-10_oq94_readsite_pass` | `READSITE_PASS.md` | OQ-94 read-site pass — sorting the beneficiary/coordination consumer surface by the ruled per-site rule |
| `2026-06-10_oq94_row2_cirope_reachability` | `FINDINGS.md` | Findings — Q2 row-2 CI_Rope reachability control (run 2026-06-10) |
| `2026-06-10_oq95_phantom_node_fix` | `writeup.md` | OQ-95 fix: phantom network nodes — fail-closed neighbor filter + scoped giant_comp edges |
| `2026-06-10_signature_liveness_crosscorpus` | `MATRIX.md` | Cross-corpus signature-liveness sweep (2026-06-10) |
| `2026-06-11_oq105_row_sweep` | `WRITEUP.md` | OQ-105 per-row sweep — the PREDICTED bucket discharged |
| `2026-06-11_oq109_phase_b` | `UNANIMITY_ADJUDICATION.md` | Unanimity-guard adjudication under the pinned criterion (operator, 2026-06-12) |
| `2026-06-11_oq110_residual_join` | `WRITEUP.md` | OQ-110 — Offline residual join + D-fork decision package |
| `2026-06-11_oq112_item4_sentinel_trace` | `WRITEUP.md` | OQ-112 item 4 — `unknown`-sentinel trace: reachability, sink, and absorption boundaries |
| `2026-06-11_oq33_close` | `writeup.md` | OQ-33 close attempt — re-witness of the classify_at_time fail-close on current substrate |
| `2026-06-11_oq44_policy_close` | `writeup.md` | OQ-44 policy close — fail-closed-on-absence ruled; the two remaining engine sites dispositioned |
| `2026-06-11_oq46_backed_reconciliation` | `writeup.md` | OQ-46 Backed reconciliation — bucketed semantics, explicit sanction marker, OQ-105 spin-off |
| `2026-06-11_oq46_close` | `writeup.md` | OQ-46 close — the scalar suppression fallback is sanctioned, not a stopgap (operator ruling 2026-06-11) |
| `2026-06-11_oq83_close` | `STEP1_REPORT.md` | Step 1 — classifier-sync nb_setval witness (OQ-83 entry item 5) |
| `2026-06-11_oq90_piton_refinement` | `README.md` | OQ-90 — capture-keyed piton refinement (RESOLVED 2026-06-11) |
| `2026-06-11_oq93_grid_migration` | `FINDINGS.md` | Findings — OQ-93 grid migration (executed 2026-06-11) |
| `2026-06-11_oq97_pattern6_census` | `WRITEUP.md` | OQ-97 close-out — Pattern-6 success-shaped-absorption census (bounded grep, class-based triage) |
| `2026-06-11_oq98_verdict_join` | `writeup.md` | OQ-98 close — the verdict banner becomes a join over the report's own evidence |
| `2026-06-12_cohort_zero` | `WRITEUP.md` | OQ-109 Phase C — close-out (analytical tail + population correction) |
| `2026-06-12_gate_partial_fix` | `gate_partial_fix_witness.txt` | Gate partial-fix witness (single .txt witness file) |
| `2026-06-12_oq105_alignment_gate` | `WRITEUP.md` | OQ-105 alignment rule landed — ruling (a) implemented as prompt rule + fail-closed compiler gate |
| `2026-06-12_oq106_retire` | `README.md` | OQ-106 retire — `structural_coercive_intent` top verdict deleted (destructive-replace witness) |
| `2026-06-12_oq114_archive_probe` | `WRITEUP.md` | OQ-114 archive divergence probe — executed under the FROZEN criterion |
| `2026-06-12_oq78_dead_bin_channel` | `README.md` | OQ-78 bin-withdrawal probe: HALTED pre-spend — the epsilon_bin channel is dead at the generation interface |
| `2026-06-12_signature_identity_witness` | `WRITEUP.md` | Signature-identity witness — does the engine's structural typing carry story identity across generation draws? |
| `2026-06-13_oq117_within_arm_proxy` | `WRITEUP.md` | OQ-117(b) — within-arm proxy + matched-fed-arm positive control |
| `2026-06-13_oq122_retype_discriminator` | `FINDINGS.md` | OQ-122 — Re-type test is confirmatory, not discriminating; the cap is claim-driven (two mechanisms), not extraction-driven |
| `2026-06-13_oq50_power_scaling_residue` | `FINDINGS.md` | OQ-50 power-scaling residue — census, the dead-restoration mechanism, and the joint witness |
| `2026-06-13_twin_comparison` | `FINDINGS.md` | Twin-model comparison — FINDINGS (2026-06-13) |
| `2026-06-14_corpus_omega_soundness_poc` | `README.md` | Corpus omega soundness POC (OQ-130 scale arm) — adjudication |
| `2026-06-14_extraction_blindness_existential_label` | `README.md` | extraction_blindness is an existential-labeling artifact (2026-06-14) |
| `2026-06-14_omega_gap_reconstruction` | `README.md` | Omega-gap feeder reconstruction — rewire `detect_gap_pattern` onto authored seats |
| `2026-06-14_omega_resolver_pilot` | `README.md` | Omega-resolver pilot on ISSUES.md (2026-06-14) |
| `2026-06-14_omega_type_diagnostic_poc` | `README.md` | Ω-type diagnostic POC (OQ-130 child) — 2026-06-14 |
| `2026-06-14_oq116_mmc_engine_witness` | `WITNESS.md` | OQ-116 MMC engine witness — what the engine actually does with a claimed-mountain / high-ε firing |
| `2026-06-14_oq122_fixture_triage` | `FINDINGS.md` | OQ-122 fixture-migration triage — RE-MEASURED ON LIVE; the 36-fixture blocker is STALE |
| `2026-06-14_oq49_remeasure` | `README.md` | OQ-49 re-measure — signature-override prevalence on the live corpora (post-reset, post-OQ-70) |
| `2026-06-15_oq131_six_observer` | `FINDINGS.md` | FINDINGS — OQ-131 Q1: six-vs-four observer site cohomology |
| `2026-06-16_census_sweep` | `WRITEUP.md` | Census × perturb adapter — config-sensitivity of the commentary census |
| `2026-06-16_oq121_totalization` | `WRITEUP.md` | OQ-121 — Totalize the commentary family + domain-relative census coverage |
| `2026-06-16_oq134_commentary_census` | `WRITEUP.md` | OQ-134 — Generic commentary-grade corpus census (build + witnesses) |
| `2026-06-16_partial_silent_totalization` | `WRITEUP.md` | Partial-silent totalization — `consensus_provenance/2` + `seat_perceived_vs_real/4` |
| `2026-06-16_q6_crosscheck_completion` | `WRITEUP.md` | Q6 crosscheck completion — audit & witnesses |
| `2026-06-16_seat_invariant_vs_prolog` | `REPORT.md` | CC Audit — Does the Prolog substrate confirm or falsify the seat/orientation invariant? |
| `2026-06-17_mountain_authoring_sweep` | `ROUTING_SINK_DESIGN.md` | Routing Sink Design — the natural_law author↔engine diff |
| `2026-06-18_oq01_rope_bypass_twins` | `WRITEUP.md` | OQ-01 grounding on the twin corpora — the A3 collapse does not reproduce |
| `2026-06-18_oq104_citation_checker` | `FINDINGS.md` | OQ-104 — Audit-citation integrity checker (build + drive-M-down + controls + disposition) |
| `2026-06-18_oq48_recalibration` | `WRITEUP.md` | OQ-48 — Recalibration-readiness audit against the twin corpora |
| `2026-06-18_oq56_twin_within_kernel_perturbation` | `FINDINGS.md` | Findings — within-kernel reading perturbation on the twins (OQ-56 D1) |
| `2026-06-20_kernel_reading_orbits` | `FINDINGS.md` | Findings — kernel/reading orbit discovery (OQ-150) |
| `2026-06-20_oq58_cross_corpus_incompleteness` | `README.md` | OQ-58 — Cross-corpus dangling-`cs_reading_relation` incompleteness census |
| `2026-06-20_oq69_ledger_drain` | `WRITEUP.md` | OQ-69 ledger drain — 2026-06-20 |
| `2026-06-21_maxent_seat_aware` | `FINDINGS.md` | FINDINGS — MaxEnt signature-override made seat-aware (OQ-138 maxent residual) |
| `2026-06-21_oq119` | `WRITEUP.md` | OQ-119 — Does feeding move the join? WRITEUP |
| `2026-06-21_oq119_gate0` | `GATE0_FINDINGS.md` | OQ-119 Gate 0 — three-axis substrate re-witness (NO SPEND) |
| `2026-06-21_oq138_fsm_route_conversion` | `CONSTRUCTED3_FINDINGS.md` | OQ-138 constructed-3 conversion — constructed_high_extraction unknown→snare routed (seat-aware, CLAIM discriminant) |
| `2026-06-21_oq35_field_counterfactual` | `writeup.md` | OQ-35 — cruft-vs-wire adjudication of 6 authored-field wiring gaps (census rows 1–6) |
| `2026-06-22_oq112_round1` | `WRITEUP.md` | OQ-112 Round 1 — C4a (item 1) resolved + full 8-class member sort |
| `2026-06-22_oq112_round2` | `WRITEUP.md` | OQ-112 Round 2 — Round 0 (re-witness on 92) + the witness-truth control |
| `2026-06-22_oq20_dr_baseline_diff` | `WRITEUP.md` | OQ-20 — Did CS-era *code* perturb DR output? Baseline code/data diff audit |
| `2026-06-23_oq06_offcase_fixtures` | `WRITEUP.md` | OQ-06 — Off-case fixtures for `cs_drift_unacknowledged` / `cs_axiom_foreclosed` |
| `2026-06-23_oq112_closeout` | `BITE_RULING.md` | OQ-112 close-out — pre-registered bite-definition ruling (recorded BEFORE the data) |
| `2026-06-23_oq112_round3` | `WRITEUP.md` | OQ-112 Round 3 — Round 0 (re-witness on 92 + reader recon) |
| `2026-06-23_oq15_crossaxis_witnesses` | `WRITEUP.md` | OQ-15 cross-axis surface — Phase 0a substrate witnesses |
| `2026-06-24_oq41_basex_t0` | `FINDINGS.md` | OQ-41 rows 24–25 — the `BaseX=0.5` default at Time=0 is OFF-GRID PROBING, not absence |
| `2026-06-25_oq182_trajectory_revive` | `c2_domain_finding.md` | OQ-182 trajectory revive — C0/C2/C-gen/C-null/C-prov finding series (9 writeups; C2 blocker: cross_domain_twins "domain" is a name-prefix heuristic) |
| `2026-06-25_oq18_temporal_reduction` | `README.md` | OQ-18 — temporal first/last reduction: witness set |
| `2026-06-26_oq91_repair` | `b1scan_finding.md` | B1-scan — real-corpus upward-run scan (cheapest falsifier; selects the close-branch) |
| `2026-06-27_oq118_reprobe` | `README.md` | OQ-118 re-probe (2026-06-27) — witness behind the ruling object |
| `2026-06-27_oq124_oq149_committer_convention_control` | `FINDINGS.md` | FINDINGS — OQ-124 + OQ-149 committer-axis convention control |
| `2026-06-27_oq182_trajectory_serialization` | `mechanism_witness_finding.md` | OQ-182 — Trajectory/giant_comp serialization: MECHANISM WITNESS — PASS |
| `2026-06-28_oq22_hub_starvation` | `FINDINGS.md` | OQ-22 Hub-1/Hub-2 starvation — findings (evidence-first); VERDICT B |
| `2026-06-29_oq23_coexists_fpn_canary` | `HOLD_FINDINGS.md` | OQ-23 HOLD deliverables (operator ruled option 3, 2026-06-29) |
| `2026-06-30_oq37_census_redispose` | `writeup.md` | OQ-37 census re-disposition — writeup |
| `2026-06-30_oq38_orphan_xref` | `WRITEUP.md` | OQ-38 — Reproducible export-vs-caller orphan census |
| `2026-07-01_oq197_acceptance_controls` | `README.md` | OQ-197 acceptance controls — kernel_v1 (i) + twins (ii), 2026-07-01 |
| `2026-07-01_oq197_r4_recompute` | `README.md` | R4 recompute on the fixed detector — read-only, 2026-07-01 |
| `2026-07-01_oq197_source_h1_crosstab` | `README.md` | OQ-197 (a)/(b) source cross-tab vs h1_band — 2026-07-01 |
| `2026-07-01_oq41_row26_expansion` | `FINDINGS.md` | OQ-41 row-26 expansion — five-site branch-shape resolution + disposition |
| `2026-07-01_oq45_oq52_hidden_winners` | `WRITEUP.md` | OQ-45 + OQ-52 — the presents-as-natural / hidden-winner pair |
| `2026-07-02_oq136_census_bucket_provenance` | `WRITEUP.md` | OQ-136 writeup — census absence buckets × generation provenance |
| `2026-07-02_oq137_reading_totality` | `classification_table.md` | OQ-137 classification table — reading layer vs the typed-absence convention (§5) |
| `2026-07-02_oq138_fnl_evidence` | `FNL_CONVERSION_DIFF.md` | OQ-138 FNL conversion — twin pipeline OLD-vs-NEW diff reconciliation (2026-07-03) |
| `2026-07-02_oq193_giant_comp_ruling` | `RULING_EVIDENCE.md` | OQ-193 giant_comp sibling edges — ruling evidence (2026-07-02) |
| `2026-07-02_oq195_general_n_gap` | `WRITEUP.md` | OQ-195 — General-n H¹ Gap Spectrum: Verification Writeup |
| `2026-07-02_oq75_stage2_preflight` | `PREFLIGHT.md` | OQ-75 Stage-2 go — preflight checklist, verified live (2026-07-02) |
| `2026-07-03_oq03_self_application` | `README.md` | OQ-03 self-application datum + closing ruling (2026-07-03) |
| `2026-07-03_oq205_build` | `README.md` | OQ-205 build-phase graduation audit (2026-07-03) |
| `2026-07-03_oq205_epsilon_census` | `WRITEUP.md` | ε census (OQ-205 spec recon) — read-only, 2026-07-03 |
| `2026-07-03_oq72_concept_key_pilot` | `WRITEUP.md` | OQ-72 concept-key pilot — WRITEUP |
| `2026-07-03_oq87_twins_ca3` | `FINDINGS.md` | OQ-87 twins characterization — FINDINGS |
| `2026-07-04_oq140_divergence_characterization` | `WRITEUP.md` | OQ-140 WRITEUP — characterizing the `author_engine_divergence` population |
| `2026-07-04_oq75b_grain_probe` | `WRITEUP.md` | OQ-75(b) grain-sensitivity precursor probe — WRITEUP |
| `2026-07-04_oq88_false_mountain_detector` | `FINDINGS.md` | OQ-88 FINDINGS — false-mountain as kernel-false-negative detector |
| `2026-07-04_twin_conditioned` | `FINDINGS.md` | FINDINGS — Twin conditioned re-analyses (OQ-125 resolved; OQ-123 mitigated) |
| `2026-07-06_oq213a_twin_sonnet_leg` | `RESULTS.md` | Twin-model cross-classification — RESULTS |
| `2026-07-11_oq186_oq188_readsite` | `README.md` | OQ-188 + OQ-186 read-site audit — Phase-1 evidence and decisions |
| `2026-07-12_oq207_stakeholder_h1` | `README.md` | OQ-207 — Stakeholder-frame H¹ census (three live legs + kernel_v1) |
| `2026-07-12_oq215_arm2_d9_control` | `READOUT.md` | OQ-215 arm 2 — D9 discrimination control (READOUT) |
| `2026-07-12_oq215_arm3_variance` | `READOUT.md` | OQ-215 arm 3 — five-run variance (READOUT) |
| `2026-07-12_oq217_consensus_tightening` | `README.md` | OQ-217 — consensus_provenance/2 tightening: movement census (2026-07-12/13) |
| `2026-07-12_oq218_scored_snare` | `READOUT_STAGE2.md` | OQ-218 Stage 2 — Batch READOUT (2026-07-12) |
| `2026-07-13_oq214_theme_meter` | `WRITEUP.md` | OQ-214 — deterministic theme-naming meter (`_theme_inventory`): Phase A writeup |
| `2026-07-13_oq219_missing_floor` | `READOUT_datum_stone.md` | OQ-219 — Datum Stone (floor-PRIMARY) READOUT: routing outcome (a) |
| `2026-07-14_oq138_residual_rewitness` | `README.md` | OQ-138 residual re-witness + certify pass (2026-07-14) |
| `2026-07-16_oq221_meter_partition` | `WRITEUP.md` | OQ-221 — Firing-condition partition of the defect/gate set: WRITEUP |
| `2026-07-17_oq60_purity_absence` | `PHASE2_CLOSE_2026-07-23.md` | Phase-2 consolidation + Phase-3 close (2026-07-23) |
| `2026-07-20_five_leg_twin_comparison` | `BATTERY_WRITEUP.md` | Stance profiling battery — the convergence test (2026-07-20) |
| `2026-07-23_oq232_falsifier_redesign` | `README.md` | OQ-232: Axiom 2's falsifier redesigned as two scoped discriminating perturbations |
| `2026-07-24_oq152_seat_crosssection` | `FINDINGS.md` | OQ-152 — per-seat naturalization-collapse cross-section: answered with a negative |
| `2026-07-24_oq153_step3_blind_pass` | `RESULTS.md` | OQ-153 step 3 — blind pass RESULTS (verdicts UNRULED, for operator) |
| `2026-07-24_oq153_update_authority_step2` | `enum_controls.pl` | OQ-153 update-authority step 2 — enumeration controls (evidence only) |
| `2026-07-24_oq61_header_purity_cascade` | `README.md` | OQ-61 — Corpus header purity/cascade line: three operator rulings implemented |
| `2026-07-25_oq216_contract_extractor_redesign` | `WITNESS.md` | OQ-216 stage-2 contract extractor redesign — witness record |
| `2026-07-25_oq254_q_provenance` | `WRITEUP.md` | OQ-254 — Q-provenance of the topic decomposition: recon + minimal wiring (RESOLVED) |
| `2026-07-25_oq255_seat_cost_measure` | `WRITEUP.md` | OQ-255 — The seat-cost measure: gate + grade, defined and controlled |
| `2026-07-25_oq62_band_vocabulary_fork` | `CALL_SITE_CENSUS.md` | OQ-62 — call-site census for the purity-bander negative guard |
| `2026-07-25_oq66_nlwb_filter_cutover` | `FINDINGS.md` | OQ-66 — `natural_law_without_beneficiary/1` agent-filter cutover |
| `2026-07-25_oq67_legacy_chi_retire` | `WRITEUP.md` | OQ-67 — Retiring the legacy power-modifier χ path by value |
| `2026-07-27_cross_author_epsilon_probe` | `README.md` | Cross-author ε probe — step 1 of the reader-profile plan (Claude-web, 2026-07-27) |
| `2026-08-03_kritik_ingest` | `WRITEUP.md` | Kritik ingestion probe — WRITEUP |
| `2026-08-03_oq258_referent_discriminator` | `WRITEUP.md` | OQ-258 referent discriminator — WRITEUP (executed 2026-08-03 → 2026-08-04) |
| `2026-08-05_oq259_emphasis_discriminator` | `ARM0_HALT_REPORT.md` | Arm 0 result: pre-registered HALT rule FIRED — no Arm-1 spend |
| `2026-08-09_oq262_coexists_severance` | `WRITEUP.md` | OQ-262 — severance/intrinsicness on coexists edges: 18 pairs judged under a frozen grammar; 2 raw-match consumers routed |
| `2026-08-12_oq283_framing_boundary_discrimination` | `WRITEUP.md` | OQ-283 — boundary framing-loss SEPARATES from seat-theorem Corollary 2a (3 NOT-HELD / 2 declines / 1 seam); explicitly NOT a third-axis result |
| `2026-08-17_oq285_mode3_measurement_arm` | `WRITEUP.md` | OQ-285 — recommendation (D): the FAILS/`unknown` axis is empty (0/1333) and registered-by-design; no instrument separates expressive capacity from coordinate position (max population 8 seats) → proposed GAP-36 |
| `2026-08-17_oq251_natural_law_reachability` | `WRITEUP.md` | OQ-251 — no path to `natural_law` certification exists (unsatisfiable BY CONSTRUCTION, one blocker), and the binding commit is `8b5a34b8` not OQ-70: three-point bisect corrects a mis-attribution in `narrative_ontology.pl` + `GATE2_REWITNESS.md`; OQ-248 kill condition did not trip; OQ-296 minted |
| `2026-08-17_giant_comp_segv_hang` | `WRITEUP.md` | PARTIAL — round-2 preregistered, arms unrun; owner session closed mid-flight (OQ-301) |
| `2026-08-17_bound_dispatch_hardening` | `WRITEUP.md` | Bound-dispatch pilot: fresh-variable heads + unify-after-cut on classify_from_metrics/6 + constraint_signature/2 + classify_by_signature/3; zero-diff on six legs (semantics-changing by construction); gate row `dispatch head`; three checker-only finds → OQ-302/OQ-303 |
