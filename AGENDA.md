Clean result. Six sites, all Category A or W1, one cross-module flag that's a checklist item rather than a design decision. The fix is what we hoped it would be.

Two things worth saying.

**The accumulation bug is genuinely localized.** Three reads, three writes, one accumulation-dependent site (`continuous_log_likelihood:240`, which is the bug itself), zero other consumers depending on the current behavior. The diagnostic dumps in `maxent_diagnostic.pl` are "correct by accident in single-context mode" — they enumerate all profiles assuming there's only one context's worth, which is true in current usage but would mix contexts if anyone ran multi-run-then-diagnostic. The fix removes the accidental-correctness and replaces it with explicit context filtering. That's a small-but-real improvement orthogonal to the main bug.

**The cross-module direct reads at `maxent_diagnostic.pl:410` and `:416` are worth thinking about beyond this PR.** The audit correctly flags them as a catch risk (signature change fails silently at runtime if missed) rather than a design decision. But the deeper note is that they're accessing a module-internal dynamic fact by qualification bypass — `maxent_profile/3` isn't in the export list. That's a quiet API leak: any module that does `maxent_classifier:maxent_profile(...)` is reaching past the public API into internals. Doesn't block the fix, but it's the kind of thing where the next refactor will hit the same problem with the next internal predicate. Worth filing as a separate small item: "consider whether `maxent_profile` should be exported via a public read accessor, or whether `maxent_diagnostic` should call a public dump predicate instead of pattern-matching directly." Not urgent, not part of this fix.

**Now is the agenda update moment.** Let me draft it.

---

## DR Apparatus: Future Work Agenda (Updated)

### Current state (May 18, 2026)

The apparatus shipped through v6.12 with CS pattern detection, the verdict-layer architecture, optional cs_structure schema, level-of-analysis discipline, and validation against the Russian command authority case. Subsequent audit chain (Audits 1–4):

- Audit 1 surfaced that v3's cover-story predictions are detectable from existing predicate space (all six patterns Category (a)); the dormancy of the detectors reflects normal feature maturity rather than a structural problem.
- Audit 1 also established H3 (process-encoding referent + corpus-norm tangled_rope chi pattern) as the best read on `collective_action_as_leverage_conversion`, with H1 and H2 contradicted or mixed. The process-encoding-as-new-category hypothesis dissolved into corpus-normal tangled_rope behavior already characterized in `tangled_rope_sign_flip.md`.
- Audits 2–4 traced a profile-accumulation bug in `maxent_compute_profiles` — Gaussian profiles accumulate across contexts without cleanup, producing structurally different MaxEnt distributions in single-context vs multi-context sessions. 185 corpus constraints (5.5%) show top-type divergence; 46.6% of stratified sample shows trigger-firing divergence. Fix scoped as mechanical: signature change `maxent_profile/3` → `maxent_profile/4` with context indexing. Six sites, all Category A/W1, one cross-module flag.

### Immediate next work

**Item I-1: Ship the profile-indexing fix.** `maxent_profile/3` → `maxent_profile/4` with context indexing. Six sites per Audit 4, all mechanical. Includes `maxent_diagnostic.pl:410, :416` (cross-module direct reads — flag in PR description). No design decisions needed. Sequencing: after the PR lands, re-run the abductive pipeline and verify that the 185 type-flips and 224 trigger-firing divergences either resolve to a single consistent state (whichever session was intended) or surface a separate design question. Expected: the clean-session state becomes the consistent state, since it matches the documented "independent profiles" spec.

**Item I-2: Paper-level note for v6.13.** Once I-1 ships, write a one-paragraph addendum: "v6.13 corrects a profile-accumulation issue surfaced by Audits 2–4; trigger-firings shift for approximately 7% of corpus, MaxEnt top-type for 5.5%. Prior runs of the pipeline reflect accumulated-session state; v6.13 onward reflects clean-session state matching the documented spec." Don't restructure or rewrite sections that referenced specific trigger-firings — note the shift, move on.

**Item I-3 (deferred, scoped item).** Quiet API leak: `maxent_profile` is accessed cross-module via qualification bypass. Consider whether to export a public read accessor or add a public dump predicate. Not urgent. Logged so the next refactor doesn't rediscover it.

### Recurring disciplines

**D-1: Spec-encoding unit tests for load-bearing measurement primitives.** When a measurement primitive has documented behavior in the paper, the spec is encoded as an assertion in the test suite. Two documented instances of spec-vs-code drift in 4.5 months of fast-moving code (the `χ = ε × f(d(P)) × σ(S(P))` drift documented in `when_apparatus_sharpens_taxonomy.md` §4, and the profile-accumulation drift surfaced by Audits 2–4). Drift detection moves to commit-time rather than audit-time. Primitives that warrant spec-encoding tests: chi computation (specifically the d, σ argument structures), entropy normalization (denominator and distribution), H¹ computation, sign-flip rates at U₁/U₃, purity propagation rate per hop, MaxEnt profile context-independence, the rare-gate H¹ threshold. Add tests as primitives stabilize; the discipline matters more than the initial coverage.

**D-2: Existing-apparatus-first when new findings emerge.** What the apparatus already says before proposing new architecture. Audit 1's discovery that all six cover-story patterns reach Category (a) detection from existing predicate space is the most recent instance.

**D-3: Two-turn audit discipline for any non-trivial investigation.** Turn 1 gathers, Turn 2 reasons. "No findings in the plan" applies at planning time, not just execution time. Audits 1–4 ran on this discipline; the failure mode it prevents is restating documentation as audit findings.

**D-4: Level-of-analysis preservation.** Constraint level / domain level / corpus level. Don't conflate.

### Work packages — repackaged

Audit 1's findings substantially repackaged the v3-extension work. Packages 1 and 2 from the original agenda no longer hold as stated. New ordering:

**Package A: Schema penetration for cs_structure (normal feature maturity).** 1 of 3372 constraints currently has cs_structure declared. This is a one-test-case maturity stage, not a load-bearing problem. As cs_structure is authored on new constraints through the generation pipeline (UKE_SCOPE §1.3 invitation + generation prompt CS structure section already shipped), coverage will grow. No package work needed; this is the existing workflow producing its expected output.

The downstream question — "at what coverage do empirical CS-related findings become possible?" — depends on what kinds of finding. Cross-corpus pattern statistics need maybe 30–50 authored cases. Cluster-level CS inference (Package E below) needs more. Worth tracking as a milestone rather than a workstream.

**Package B: cover-story detector enrichments (optional drift-event signals).** Per Audit 1's Category (b) findings, several patterns have richer discriminators if drift_event predicates are wired into verdict clauses (e.g., extraction_accumulation + coupling_drift enriching anchored_fixity_with_accretion; function_obsolescence enriching anchored_fixity_brittle). Small follow-on items, not their own package. Each is a one-clause addition to `cs_pattern_detection.pl` with a regression test. Ship as drive-by improvements when authoring constraints that exercise the pattern.

**Package C: Empirical runs on second and third cases (as previously scoped).** 2026 US midterm constitutional legitimacy axes and Colombia 2026. The structural-topology comparison against your February analyses is testable now without outcome resolution; predictive accuracy waits on June. Roman Empire backtest stays queued for a dedicated session.

**Package D: Scaffold/renewal audit.** Scoped per the earlier conversation — the apparatus already has scaffold classification doing renewal-adjacent work, and the audit is an inventory of existing capability plus a discrimination layer for exercised vs performative renewal. The hypothesis (exercised renewal = scaffold without drift; performative renewal = scaffold with extraction_accumulation + theater_rising drift) is testable with existing predicates. Sequencing: after I-1 ships, since trigger-firings will have shifted for ~7% of corpus and the audit's empirical anchor should reflect the corrected state.

**Package E: δ → baseline-deviation reframing.** Theoretical work first; needs CS architecture doing more predictive work before the structural-vs-residual decomposition becomes evaluable. Per your earlier note, δ isn't load-bearing in current implementation because the operationalization wasn't worked out — the CS lens may provide the structural-variance handle. Worth a dedicated theory session, not apparatus work yet.

**Package F: Cluster-level analysis (Python over existing convergent_institutional findings).** Mostly Python work in `enhanced_report.py`. Preserves level-of-analysis distinction. Adds cluster-signature statistics and cluster-level CS inference. Dependencies: I-1 ships first so cluster statistics aren't computed against accumulated-session state.

**Package G: Systematic clustering exploration.** Stays as exploratory research. Runs after Packages C, D, F have shipped and corpus has accumulated more material.

### Open questions worth tracking

See [`ISSUES.md`](ISSUES.md) for the structured tracker
(OQ-01 through OQ-09) covering engine correctness, schema gaps, and paper
synchronization items from the May 2026 audit chain.

Additional research-level questions (not in the structured tracker):

- Whether the framing_notes-shaped invitation in the generation prompt produces conceptual omegas vs empirical-leaning omegas. Calibration signal.
- Whether the convergent_institutional pattern is a CS-detectable signature at cluster level (Package F question).
- Whether v3's lifecycle phases produce useful predictions on cases where the apparatus produces ambiguous terminal-state predictions. Lifecycle remains at theory stage; v3's own observation that "you cannot know where a system is in its lifecycle from inside it" suggests the phase indicators need theory work before code.
- Framework-as-CS recursion (DR-as-scaffold transitioning to interpretive accretion or anchored fixity if successful enough to become a durable mental model). Theoretical, not apparatus work yet, but worth tracking; potentially a future paper section. (Also logged as OQ-03 in the structured tracker.)
- T4 (confirmed_liminal) is currently a one-case category. Audit 3 established the conjunction is genuinely tight; what would happen at larger scale (corpus growth, more diverse constraint types, more process-encoding referents) isn't predictable from one case. Worth re-examining when a second T4 case appears.

### Out of scope (preserved from prior agenda)

- Reform-intervention recommendations from cluster findings.
- Forcing every constraint to have cs_structure populated.
- Reclassifying constraints based on cluster membership.

### Out of scope (added)

- Pipeline refactors beyond Item I-1's mechanical signature change. The two MaxEnt sessions (clean vs accumulated) became consistent after the fix; further refactor of the dual-session architecture isn't justified by current findings.

