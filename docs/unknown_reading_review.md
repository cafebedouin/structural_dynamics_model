# Architectural Review: Schema vs. Implementation

*What the documents ask for, what the engine provides, and the gap between them*

**Scope:** High-level read-only architectural review. No code changed.  
**Produced from:** Reading the schema documents and the engine code; reports at `outputs/constraint_reports/`.

---

## 1. What the Schema Asks For

The documents you've pointed to describe a coherent system, not a collection of separate ideas. Reading them in order surfaces a layered set of requirements.

### Layer 1 — The Seat Theorem and Declaration Discipline

The Seat Theorem (`docs/seat-theorem-v1.md`) establishes the engine's fundamental epistemic claim: every contentful verdict is seat-dependent. The only coherent response to this is **declaration** — showing which standpoint issued the verdict — because concealment is the unique inconsistent position (claiming content while denying a seat).

What this asks of an implementation:
- An engine that evaluates a constraint should know and report which seat it occupies during evaluation.
- Findings should be annotated as issued-from-seat-X, not issued-from-nowhere.
- The distinction "declared vs. concealed" should be a first-class concept.
- The no-seat pose — asserting content while denying standpoint — should be detectable in the objects the engine classifies.

The t3 corollary adds a temporal dimension: a commitment made from one seat at t1 can be confronted at t2 from another seat. The confrontation is the only accountability mechanism the framework permits. This implies the engine needs some way to make prior commitments confrontable by later states.

### Layer 2 — Disparity as Signal, Not Noise

The Cyclopean Point essay (`docs/cyclopean-point.md`) translates the Seat Theorem into a concrete epistemology. The manufactured neutral center is the institutional observer position claiming to be nowhere. Depth is readable only from the gap between positions, not from any single position or their average. The epistemic failure mode is fusing two perspectives into a felt middle — the third standpoint that has forgotten it is a standpoint.

What this asks:
- Multi-observer classification, where the *disagreement pattern* across observers is the primary output, not any single observer's verdict.
- Ability to distinguish structured disagreement (disparity = depth signal) from convergent disagreement (noise = situation-fixed parameter).
- Ability to detect when the institutional position claims coordination while other positions see extraction.

### Layer 3 — Few Seats Worth Choosing

The few-seats essay (`docs/the_few_seats_worth_choosing.md`) identifies the apparatus-as-substitute-for-stance failure mode: an elaborate examined-life framework that runs its examinations inside cover-story-approved parameters, generating the *experience* of having understood without the *cost* of having staked anything confrontable. Applied to the engine: the engine can become a sophisticated apparatus for generating analysis that substitutes for the harder work of declaring a reading and exposing it to confrontation.

What this asks:
- Findings should be confrontable — pointing toward stakes that reality can settle.
- The engine should generate results that can be wrong in a way the author would recognize.
- Omega variables are the current implementation of this; they are the engine's version of staked predictions.

### Layer 4 — The Kernel-Reading Architecture

The Altar essay (`docs/altar-to-the-unknown-reading.md`) is the document that most directly describes what the engine should be capable of — and what it cannot be. Its central claim: the unit of analysis is a **kernel-reading pair**, not a kernel alone. The engine always evaluates one kernel under one reading. What it finds is bounded to that reading.

This draws a five-way distinction among alternative readings:
1. **Same-task alternative readings** (Bayesian): engine could run them if differently authored, findings would be comparable
2. **Kernel-reinterpreting readings** (standpoint-epistemology, pragmatist): engine could run them with modified authoring, but what the kernel says changes
3. **Kernel-dissolving readings** (Foucauldian, Wittgensteinian): reject the engine's presuppositions (constraints exist, beneficiaries exist, drift is measurable, remediation is meaningful); *cannot be run in this architecture without rebuilding it*
4. **The sixth seat**: the reading no one has named from where the author stands

What this asks:
- The engine should declare which reading it's in.
- Reports should annotate findings as reading-bounded or reading-robust.
- The architecture should know which of its presuppositions (constraints exist, etc.) would be rejected by which class of reading.
- A seat-map structure: which readings have been run, which are unbuilt, which are unrunnable.

### Layer 5 — The Debugging Trifurcation

The debugging philosophy (`docs/debugging_philosophy.md`) provides a diagnostic framework: problems are Type A (drift, fixable by frame-fixing), Type B (structure, requires axiom revision), or Type C (indexical, requires index specification). The Cyclopean Point essay explicitly classifies its central claim as **Type B** — the neutral viewpoint is structurally impossible, not just imperfectly constructed. Type A solutions (adjust the frame, redo the combination) don't apply.

What this asks:
- The engine's findings should carry a type annotation: is this finding Type A (frame drift), Type B (structural impossibility), or Type C (index underspecification)?
- Omega variables are embryonically doing this (type_class: conceptual vs. empirical), but findings themselves lack the distinction.

### Layer 6 — Commitment Systems as Seat Infrastructure

The commitment systems sketch (`docs/commitment_systems/commitment_systems_sketch_v6.md`) provides the authority-grounding taxonomy. The key extension it offers: distinguishing authority groundings by whether they declare or conceal their standpoint. The nine patterns give structural vocabulary for how constraints maintain or drift from their stated basis. The new diagnostics (`cs_authority_masking/3`, `cs_cover_story_active/2`, `cs_displaced_beneficiary/1`) fire when computed structural signals disagree with asserted CS fields.

What this asks:
- A seat-declaration axis in the CS authority grounding: authority that claims a standpoint vs. authority that claims no standpoint while making contentful assertions.
- The `false_natural_law_constraint` pattern already detects the no-seat pose structurally (self-enforcing claim with beneficiaries). This is the key CS-layer implementation of the Seat Theorem's inconsistency detection.

---

## 2. What the Engine Has

Reading the testsets, reports, and key Prolog modules, the engine currently delivers the following — all verified against actual output:

### 2a. Multi-Observer Classification with H¹ Cohomology

The 4D context tuple `(P, T, E, S)` — agent_power, time_horizon, exit_options, spatial_scope — drives per-index classification. The H¹ cohomology measure reports how topologically non-trivial the classification sheaf is over the observer positions. H¹=3 means discrete blocs that can't be smoothly deformed into each other. This directly implements the disparity-as-signal requirement: the disagreement pattern is a primary output.

The T1-T6 theorem instantiation is a significant capability. T4 (Oracle Gap): MaxEnt is confident from one position but H¹>0 means cross-position comparison reveals invisible structure. T1 (Cover Story): at least one observer sees coordination while another sees extraction. T6 (Hub Correspondence): H¹ ≥ 5 maps to both hubs contributing, three or more distinct types across four observers. These theorems operationalize the cyclopean-point claim inside the engine's verification layer.

### 2b. Structural Signature Detection

The signature vocabulary — `false_natural_law`, `false_ci_rope`, `false_summit_mountain`, `constructed_high_extraction`, `coupling_invariant_rope` — is the engine's implementation of detecting the no-seat pose in the objects it classifies. The `false_summit_mountain` signature is the clearest: a constraint meeting all natural-law thresholds that has identifiable beneficiaries. Genuine natural laws have zero beneficiaries. Finding `false_summit_mountain` on `disparity_as_depth_signal` is precisely the engine detecting that the geometric principle has been naturalized — its constructed origin rendered invisible.

### 2c. CS Layer with Kernel-Reading Infrastructure

The CS layer has predicates that directly implement the kernel-reading architecture the altar essay requires:
- `cs_kernel_id/2`: names the kernel a constraint story is a reading of
- `cs_reading_relation/3`: relates readings of the same kernel (`coexists_with`, and presumably `conflicts_with`)
- `cs_axiom/3`, `cs_axiom_status/2`, `cs_axiom_grounding/3`: formalizes what axioms a reading holds and their status

This is well-implemented in the commitment-systems corpus (e.g., `autonomy_reading.pl` correctly declares `cs_kernel_id(autonomy_reading, end_of_life_decision_authority)` and relates it to `sanctity_reading` and `vulnerability_protection_reading`). The Prolog predicate infrastructure is there.

### 2d. Drift Event System

The drift event detection — `extraction_accumulation`, `metric_substitution`, `coupling_drift`, `purity_drift` — implements the temporal dimension the Seat Theorem's t3 corollary requires. Critical drift (extraction rising 0.48→0.68, theater rising 0.55→0.78 over the 30-step interval) represents the kind of temporal trajectory where a commitment made at t0 can be confronted at t30. The `cs_drift_state/3` predicate captures gap(practice_drift, substantial, true) in the CS layer.

### 2e. Omega Variables as Confrontable Stakes

The omega variable system is the engine's implementation of the few-seats essay's discipline of staking views specifically in a form reality can settle. Each omega has a question, a resolution mechanism, and an impact statement. The three omega types (conceptual, empirical, structural) gesture at the Type A/B/C debugging distinction.

### 2f. Nine CS Patterns

The nine commitment-system patterns (`marked_revision`, `interpretive_accretion`, `diffuse_reconstruction`, `implicit_practice`, `anchored_fixity_with_accretion`, `anchored_fixity_brittle`, `natural_law_constraint`, `epistemic_consensus`, `no_pattern_match`) provide structural vocabulary for how authority and kernel relate and drift. The three new diagnostics in v5.2 fire on disagreement between asserted CS fields and computed structural signals — implementing "agreement is noise, only disagreement surfaces" directly in Prolog.

---

## 3. The Gaps

### Gap A — Critical: The Cyclopean Point Testsets Have No CS Fields

The three cyclopean-point constraints (`disparity_as_depth_signal`, `cyclopean_point_as_manufactured_center`, `power_asymmetry_in_legibility`) are authored without CS fields. They have no `cs_kernel_id`, no `cs_reading_relation`, no `cs_axiom` facts. This is a significant omission because:

1. These three constraints ARE multiple readings of a single kernel (call it `the_cyclopean_point_epistemic_claim`). The `disparity_as_depth_signal` constraint is a reading of that kernel from a natural-law frame; `cyclopean_point_as_manufactured_center` is the extracted-construction reading; `power_asymmetry_in_legibility` is the power-mechanism reading. They belong in a `cs_reading_relation` graph the way `autonomy_reading`, `sanctity_reading`, and `vulnerability_protection_reading` do.

2. Without CS fields, the engine can't fire CS verdicts on these constraints — including `false_natural_law_constraint`, which is the most relevant pattern for `disparity_as_depth_signal` (a constraint claiming `self_enforcing` authority but having three beneficiaries).

3. The altar essay's central argument is precisely about the kernel-reading structure. Running the cyclopean-point analysis without kernel-reading metadata is running the analysis inside the structure it's supposed to detect.

**What Prolog supports:** The infrastructure is complete. Adding CS fields to these testsets requires authoring the JSON with `cs_kernel_codification`, `cs_authority_grounding`, `cs_interpretation_layer_present`, `cs_kernel_id`, `cs_reading_relation`, and `cs_axiom` fields — exactly as `autonomy_reading.json` does. The JSON schema and Prolog templates already handle this.

### Gap B — Critical: Reports Don't Declare the Reading

The enhanced_report.py output never says "this analysis was produced from reading X of kernel Y." The cyclopean_point_as_manufactured_center report says "the engine computed snare (analytical), rope (institutional)" but doesn't say "within the analytical-observer reading." The altar essay's load-bearing sentence is: "What the engine has to say about other readings of the same kernel is precisely nothing, because it was not asked to run them." The report never says this.

This is not a minor omission. Without reading declaration in the report, the findings appear to be verdicts about the kernel rather than verdicts about one reading of the kernel. The reader does what the altar essay warns against: takes the engine's finding as a conclusion about the thing itself rather than about the thing-as-seen-from-here.

**What Prolog supports:** The CS fields, once added (Gap A), would give the engine enough information to emit reading-boundary text in the report. The enhanced_report.py would need a new section, inserted near the omega context, that reads: "Reading: [cs_reading from cs_kernel_id/cs_reading_relation] of Kernel: [cs_kernel_id]. Findings bounded to this reading. For cross-reading robustness, run [cs_reading_relation coexists_with targets]."

**What Prolog cannot support:** Automatic determination of whether a specific finding is reading-robust. That requires actually running the engine under alternative readings and comparing outputs. The engine can note which alternative readings exist (from cs_reading_relation) and which are unrunnable (because they dissolve the task's presuppositions), but it cannot compute robustness from a single run.

### Gap C — Significant: No Seat-Declaration Status in CS Authority Grounding

The CS authority grounding taxonomy (`lineage`, `practice`, `self_enforcing`, `expertise`, `diffuse_epistemic`, `extraction`) lacks a dimension for whether the authority declares or conceals its standpoint. The `false_natural_law_constraint` verdict fires when `self_enforcing` authority has beneficiaries — that's detecting the no-seat pose structurally. But there's no positive representation for a constraint that *correctly* declares its seat.

The Seat Theorem's discipline of declaration asks for a distinction: declared vs. concealed. In the CS layer, this would look like:
- `seat_declared`: authority explicitly claims a standpoint as part of its operation (the autonomy reading of end-of-life authority claims a standpoint; it names whose autonomy, from which tradition, under which philosophical framework)
- `seat_concealed`: authority claims no standpoint while making contentful assertions (the manufactured center operating as neutral ground)

Currently these aren't distinguished. `seat_declared` constraints would be the models of what the Seat Theorem recommends; `seat_concealed` constraints would be candidates for the no-seat pose verdict. The existing `false_ci_rope` and `false_summit_mountain` signatures detect some instances of `seat_concealed`, but through structural inference rather than first-class representation.

**What Prolog supports:** Adding `seat_declaration_status/2` as a new CS field (declared | concealed | ambiguous) is straightforward. The pattern matching in `cs_pattern_detection.pl` could then fire a `false_seat_concealment` verdict when declared=ambiguous or declared=concealed combined with high theater_ratio and beneficiary presence. This would be a new path in the verdict predicate, not a structural change.

### Gap D — Moderate: Type A/B/C Classification Not Propagated to Findings

The debugging philosophy framework distinguishes drift problems (Type A), structural impossibility (Type B), and index underspecification (Type C). The omega variables have `type_class` (conceptual, empirical) but this doesn't map cleanly onto A/B/C. More importantly, the engine's *findings* — false_ci_rope, critical drift, false_summit_mountain — aren't annotated with the debugging type.

A `false_summit_mountain` finding is a Type B problem: the constraint's natural-law claim is structurally impossible (beneficiaries exist, ergo not natural law). No amount of frame-fixing resolves it; the axioms need revision. A `critical_drift: extraction_accumulation` finding is a Type A problem: the constraint's extraction is rising over time; the drift could in principle be arrested with frame-fixing (acknowledgment + revision). A finding whose variability across observer positions exceeds H¹=3 is a Type C problem: the type depends on which seat you specify, and the problem dissolves under index specification (not by converging on one answer, but by correctly attributing different answers to different indices).

Without this classification, findings appear to be on a single axis (more or less serious) rather than on a three-way axis where each type requires a different response.

**What Prolog supports:** The existing theorem infrastructure could emit Type annotations alongside theorem instantiations. T1 (Cover Story) is intrinsically Type C — the type depends on observer index. T2 (Discrete Blocs) and T4 (Oracle Gap) are Type C. The drift events are Type A. The false_summit and false_ci_rope signatures are Type B. This mapping already exists implicitly; making it explicit requires adding a `paradox_type` annotation to the theorem and signature output.

### Gap E — Structural: The Engine Doesn't Report Its Own Seat

The most self-referential gap: when the engine classifies `disparity_as_depth_signal` as `false_summit_mountain`, it finds that the principle of reading depth off disparity between positions has been naturalized — its constructed origin rendered invisible through repeated invocation. But the engine's own epistemic architecture is built on that principle. H¹ cohomology is a disparity-reading instrument. Multi-observer classification is disparity-as-signal implemented in Prolog.

The altar essay names this as the place the diptych "is willing to be wrong about" from the standpoint-epistemology direction. From the analytical-observer position, the false_summit finding applies to the principle the engine uses to generate the finding. The engine is the cyclopean point looking at the cyclopean point.

The report doesn't note this. It could. A note at the bottom of the `disparity_as_depth_signal` report — "this engine's classification architecture (H¹ cohomology, multi-observer disparity-reading) operates on the principle this constraint was found to have naturalized" — would be the engine practicing what the diptych preaches: declaring the seat.

**What Prolog supports:** This is largely a report-layer addition. The enhanced_report.py, when it encounters a constraint with `false_summit_mountain` signature and whose kernel relates to the engine's core epistemic principles, could emit this note. The trigger condition would need to be authored (probably as a special case for the disparity-related constraints, or more generally when a false_summit_mountain finding is on a constraint the engine itself relies on). Prolog can't derive this automatically — it requires someone to mark which constraints the engine's architecture is downstream of.

---

## 4. What Cannot Be Delivered by This Architecture

These are not gaps to close. They are structural limits.

**Cross-reading comparison without multiple runs.** The engine can note which alternative readings of a kernel exist (via `cs_reading_relation`). It cannot compute whether its finding on reading X would hold under reading Y without actually running the engine on a Y-authored testset. The altar essay's Ω_E — whether the three verdicts are reading-robust — requires authoring five additional constraint sets (one per alternative reading) and comparing outputs. The engine provides the infrastructure for this comparison but cannot perform it in a single run.

**Evaluation of kernel-dissolving readings.** The Foucauldian and Wittgensteinian readings reject the engine's presuppositions: that constraints exist as objects of analysis, that beneficiaries are identifiable, that drift is measurable, that remediation is meaningful. An engine that requires these presuppositions to function cannot evaluate readings that deny them. This isn't a deficiency; it's the engine having a seat. The repair is declaration, not removal.

**Automatic seat-freedom certification.** The Seat Theorem says a contentful verdict system cannot certify its own seat-freedom from within. The engine cannot produce a report saying "this analysis is seat-free." That would be the no-seat pose. What it can do is declare its reading — which it currently doesn't.

**The sixth seat.** The reading that neither the author nor the engine can name. This is architecturally unfillable by definition. The altar essay's response is correct: reserve a place in the seat-map, note the finitude of the enumeration, and decline to claim the six named readings are exhaustive. The engine can mark its omega variables as "there are readings of this kernel not listed here" without specifying them.

---

## 5. The Most Tractable Improvements

In decreasing order of tractability:

**1. Add CS fields to the cyclopean-point testsets.** The JSON schema and Prolog multifile declarations already exist. Author `cs_kernel_id`, `cs_reading_relation`, and `cs_axiom` facts for the three cyclopean-point constraints, pointing to a shared kernel. This unlocks the CS verdict machinery on the constraints where it matters most, and enables the report to say which reading is being run.

**2. Report reading boundary in enhanced_report.py.** Once Gap A is closed, emit a "Reading Declaration" section near the omega context. Text: which kernel, which reading, which co-existing readings are unrun, which are unrunnable (if any are tagged as such). This is mostly a Python change on top of the Prolog CS infrastructure.

**3. Add `seat_declaration_status` to the CS authority grounding.** A new field: `declared | concealed | ambiguous`. Fire a verdict when asserted `concealed` or `ambiguous` disagrees with a low theater_ratio (the constraint claims concealment but exhibits minimal theater). Conversely, fire a different verdict when a high theater_ratio constraint claims `declared` (theater may be substituting for actual declaration). This requires a modest addition to `cs_pattern_detection.pl` and the JSON schema.

**4. Add paradox type (A/B/C) to theorem and finding annotations.** The mapping from findings to debugging types exists implicitly in the theorem structure. Making it explicit adds a column to the abductive flags table and theorem instantiation output. Mostly a labeling change in the reporting layer.

**5. Self-referential note in disparity_as_depth_signal report.** A special case in enhanced_report.py: when a constraint has `false_summit_mountain` and its kernel_id or topic_domain relates to the engine's core epistemic architecture, emit a declared-seat note. Requires a manual annotation (which constraints the engine's architecture is downstream of), not automatic detection.

---

## 6. The Deeper Structural Observation

The documents form a recursive structure that the engine is currently executing from the outside. The Seat Theorem says declaration is the only coherent residue of neutrality. The Cyclopean Point says the manufactured center is the institutional position claiming to be nowhere. The Altar says the engine evaluates one kernel under one reading and is silent on the others. The Few Seats essay says the apparatus can substitute for the harder work of staking something confrontable.

The engine successfully detects these patterns *in the constraints it analyzes*. It classifies `cyclopean_point_as_manufactured_center` as `false_ci_rope` and finds the institutional observer position claiming coordination while other positions see extraction. It fires T4 (Oracle Gap) because MaxEnt is confident from one position but H¹>0 reveals structure invisible from that position. It generates omega variables pointing toward confrontable stakes.

What the engine does not do is apply this apparatus to *itself*, which is precisely what the diptych asks. The altar essay exists because the engine ran the cyclopean-point analysis and the author noticed: the engine evaluated one reading and produced findings bounded to that reading, and the report presented them without that boundary visible. The second panel is the boundary declaration the first panel's engine output needed but didn't include.

The gaps described above are, in this sense, a single gap: the engine does not yet declare the seat from which its findings are issued. The CS infrastructure for doing so (kernel_id, reading_relation, axioms) is mostly built. What's missing is wiring it to the report.

---

*Read-only. No code changed. The above is structural analysis, not implementation instructions. All findings cite specific files, predicates, or report output.*
