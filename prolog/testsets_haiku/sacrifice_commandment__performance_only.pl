% ============================================================================
% CONSTRAINT STORY: sacrifice_commandment__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_commandment__performance_only, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sacrifice_commandment__performance_only
 *   human_readable: Sacrifice Commandment as Physical Performance Requirement (Temple Dependency Reading)
 *   domain: religious/halakhic
 *
 * SUMMARY:
 *   The performance_only reading of the sacrifice commandment asserts that
 *   the obligation to perform animal sacrifice remains binding in its
 *   literal, physical form—and that without the Temple, the commandment is
 *   structurally suspended (not fulfilled, not abrogated, but held in a
 *   permanent state of non-performance). This reading has dominated
 *   mainstream rabbinic authority for 1,900 years since the Temple's
 *   destruction in 70 CE. The constraint functions as a snare because it
 *   sustains a directed flow of scholarly labor and communal attention into
 *   detailed technical study of unperformable acts, while simultaneously
 *   suppressing alternative readings (study-as-performance,
 *   archive-maintenance) that would dissolve the non-compliance state and
 *   redirect resources. The performance_only reading is one of three
 *   structurally distinct readings of the same kernel (the sacrificial
 *   commandments themselves)—each reading instantiates a different constraint
 *   with different beneficiary structures and different extraction profiles.
 *
 * KEY AGENTS:
 *   - Halakhic scholars: identity-locked payers whose professional existence is constituted by interpretive engagement with the performance_only framework; exit requires abandoning scholarly identity
 *   - Jewish practitioners: identity-locked payers bearing permanent non-compliance; structural access to commandment fulfillment is impossible
 *   - Temple restoration movement: beneficiary collecting legitimacy and resource flow from the perpetual 'suspended' state
 *   - Rabbinic authority structure: agenda-setter enforcing performance_only reading through institutional weight and suppression of competing readings
 *   - Study-as-performance advocates: excluded; their reading would dissolve the extraction but is suppressed as heresy
 *   - Archive-maintenance advocates: excluded; would reframe study as custodial rather than compensatory
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_commandment__performance_only, 0.82).
domain_priors:suppression_score(sacrifice_commandment__performance_only, 0.71).
domain_priors:theater_ratio(sacrifice_commandment__performance_only, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, extractiveness, 0.82).
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, resistance, 0.34).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_commandment__performance_only, snare).
narrative_ontology:human_readable(sacrifice_commandment__performance_only, "Sacrifice Commandment as Physical Performance Requirement (Temple Dependency Reading)").
narrative_ontology:topic_domain(sacrifice_commandment__performance_only, "religious/halakhic").

domain_priors:requires_active_enforcement(sacrifice_commandment__performance_only).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_commandment__performance_only, '50e3866d-b625-46d3-96ad-4a2c4929c34c').
narrative_ontology:cs_kernel_codification('50e3866d-b625-46d3-96ad-4a2c4929c34c', fixed_text).
narrative_ontology:cs_authority_grounding('50e3866d-b625-46d3-96ad-4a2c4929c34c', lineage).
narrative_ontology:cs_interpretation_layer_present('50e3866d-b625-46d3-96ad-4a2c4929c34c').
narrative_ontology:cs_reading_relation('50e3866d-b625-46d3-96ad-4a2c4929c34c', sacrifice_commandment__study_as_performance, forecloses).
narrative_ontology:cs_reading_relation('50e3866d-b625-46d3-96ad-4a2c4929c34c', sacrifice_commandment__archive_maintenance, coexists_with).
narrative_ontology:cs_axiom('50e3866d-b625-46d3-96ad-4a2c4929c34c', foundational, literal_physical_performance_required).
narrative_ontology:cs_axiom_status(literal_physical_performance_required, holdable).
narrative_ontology:cs_axiom_grounding('50e3866d-b625-46d3-96ad-4a2c4929c34c', literal_physical_performance_required, deontological).
narrative_ontology:cs_axiom('50e3866d-b625-46d3-96ad-4a2c4929c34c', secondary, temple_restoration_messianic_condition).
narrative_ontology:cs_axiom_status(temple_restoration_messianic_condition, holdable).
narrative_ontology:cs_axiom_grounding('50e3866d-b625-46d3-96ad-4a2c4929c34c', temple_restoration_messianic_condition, theological).
narrative_ontology:cs_reference_frame('50e3866d-b625-46d3-96ad-4a2c4929c34c', commandment_performance_binding_post_temple_destruction).
narrative_ontology:cs_drift_state('50e3866d-b625-46d3-96ad-4a2c4929c34c', contemporary_jewish_diaspora, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('50e3866d-b625-46d3-96ad-4a2c4929c34c', '').
narrative_ontology:cs_kernel_id(sacrifice_commandment__performance_only, sacrifice_commandment).

% --- Structural relationships ---
narrative_ontology:constraint_victim(sacrifice_commandment__performance_only, halakhic_scholars).
narrative_ontology:constraint_victim(sacrifice_commandment__performance_only, jewish_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sacrifice_commandment__performance_only, temple_restoration_movement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Directed into 1,900+ years of detailed technical study of unperformable sacrifice procedures—animal selection, preparation, Temple altar layout, priestly ritual sequences. Their scholarly attention and institutional resources are diverted from obligations the reading permits to be observed without the Temple (prayer, study of other commandments, community ethics). Exit would require abandoning professional identity as Talmudic interpreter and accepting the sibling reading's legitimacy, which the performance_only framework treats as heresy.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, halakhic_scholars, payer,
    organized, generational, identity_locked, global).

% Hold the obligation to perform sacrifice commandments, yet structural access to fulfillment is impossible—the Temple does not exist and is not rebuilding in the foreseeable historical moment. The reading creates permanent non-compliance: the commandment cannot be fulfilled, only studied. Exit from the reading itself (adopting study-as-performance) requires rejecting the framework their community and childhood education installed; remaining within it produces perpetual failure.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, jewish_practitioners, payer,
    powerless, biographical, identity_locked, global).

% Benefits from the constraint's perpetual framing of Temple as necessary. The performance_only reading sustains the narrative that the commandments remain unfulfilled and will remain so until the Temple is rebuilt—a claim that justifies the movement's existence and directs resources toward restoration. The constraint legitimizes the messianic aspiration as the only resolution to the commandment's unfulfillability.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, temple_restoration_movement, beneficiary,
    moderate, civilizational, constrained, regional).

% Interprets and enforces the performance_only reading through textual commentary, responsa (legal opinions), and communal education. Maintains the technical apparatus of sacrifice law study despite its operational impossibility. The structure resists and suppresses the study_as_performance reading through institutional weight and heresy-designation, treating literal performance as the binding framework.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, rabbinic_authority_structure, agenda_setter,
    institutional, generational, constrained, global).

% Hold that sacrifice study preserves technical knowledge for possible future Temple restoration without claiming the current obligation to perform is suspended. They would argue study is custodial, not compensatory, and would redirect some scholarly resources toward living law. They are marginalized in mainstream halakhic discourse and treated as inadequate interpreters of the commandment's binding force.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, archive_maintenance_advocates, excluded,
    moderate, civilizational, constrained, global).

% Hold that intellectual engagement with sacrifice law fulfills the commandment in the Temple's absence—that study IS the performance, not a substitute. This reading would dissolve the permanent non-compliance and permit resources to flow to other commandments. They are institutionally marginalized, treated as reducing the commandment's gravity, and their interpretive tradition is suppressed within mainstream rabbinic authority structures.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, study_as_performance_advocates, excluded,
    organized, generational, constrained, global).

% Analyze how the performance_only reading sustains a 1,900-year extractive arrangement where study labor is directed at unperformable acts. They note the constraint functions as a snare not because it prohibits study (which is continuous) but because it redirects scholarly and communal resources toward an eternally unreachable target while suppressing alternative readings that would dissolve the entrapment.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, observer_comparative_religionists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains continuity of the Jewish legal tradition and communal identity across the diaspora separation from the Temple—preserves technical knowledge and interpretive practice tied to sacrifice law as expressions of binding obligation, rather than as historical artifact or optional archive.
% TRANSFER_FUNCTION: Directs scholarly attention, institutional resources, and practiced interpretive labor from the Jewish community (particularly halakhic scholars and advanced practitioners) into detailed technical study of unperformable sacrifice procedures—study that does not alter behavior (sacrifice cannot be performed) and does not permit alternative fulfillment readings (study-as-performance is suppressed as heresy).
% ABSENT_VOICES: Study-as-performance advocates are excluded from mainstream halakhic authority structures—their reading would dissolve the permanent non-compliance state and permit resources to redirect toward living law and community ethics. Archive-maintenance advocates are marginalized; they would reframe study as custodial rather than obligatory-compensatory. Both excluded positions would reduce the constraint's extractiveness but are treated as inadequate interpretations of binding law.
% DISAPPEARANCE_RATIONALE: If the performance_only reading disappeared and study-as-performance became canonical, the Jewish communal obligation structure would reorganize substantially: study would be recognized as fulfilling the commandment itself; the permanent non-compliance state would dissolve; scholarly resources could redirect toward living law; the messianic framing tied to Temple restoration would lose its central interpretive anchor; the Temple restoration movement would lose its legitimizing narrative for why restoration is necessary to fulfill binding obligations. The performance_only advocates and restoration movement would say the world rearranges fundamentally. The study-as-performance advocates would say the world corrects itself. The disappearance verdict is contested because the constraint's very existence depends on which reading is adopted—there is no neutral 'world' independent of the reading.
% FOUNDING_PROBLEM: After the Roman destruction of the Second Temple (70 CE), Jews in diaspora faced an unprecedented problem: a core commandment (to bring animal sacrifices) could no longer be performed by any mechanism available to them. The founding problem was: what is the status and binding force of an obligation that cannot be fulfilled?
% FOUNDING_PROBLEM_CORROBORATION: Mainstream rabbinic tradition attests the problem is still live—the commandment remains binding and suspended, not abrogated or reinterpreted. Medieval and modern authorities cite this status continuously. However, from outside the performance_only framework, independent legal historians and study-as-performance advocates attest the founding problem has been solved by alternative readings: study-as-performance solves it (study IS the performance); archive-maintenance solves it (study preserves knowledge without claiming current obligation to perform); even accepting the commandment as abrogated or inapplicable would solve it. The fact that these solutions are suppressed rather than adopted is evidence that the performance_only reading maintains the founding problem deliberately—it keeps alive the question 'what is the status of an unperformable obligation' when alternative readings would resolve it. No neutral external source corroborates that the performance_only resolution is the only or best answer to the founding problem; testimony divides along reading-lines.
narrative_ontology:disappearance_verdict(sacrifice_commandment__performance_only, contested).
narrative_ontology:founding_problem_status(sacrifice_commandment__performance_only, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_commandment__performance_only, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(sacrifice_commandment__performance_only, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_commandment__performance_only, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_commandment__performance_only_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sacrifice_commandment__performance_only, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sacrifice_commandment__performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82 at interval end) because the constraint directs substantial scholarly and communal resources toward acts that definitionally cannot be performed—the study is perpetually non-fulfilling by the reading's own logic, yet remains obligatory. Suppression is high (0.71) because the constraint's persistence depends on suppressing the study-as-performance reading, which would dissolve the non-compliance and permit resource redistribution. Theater is very high (0.68) because the dominant activity—detailed technical study of Temple procedures that will never be executed—is scholarly performance of tradition rather than functional preparation or living-law engagement. Accessibility_collapse is near-maximal (0.92) because once the performance_only framework is internalized (which happens through centuries of education and community membership), the alternative readings become nearly unthinkable within the tradition—the framework is presented as settled law, not as one interpretive option among others. Resistance is low (0.34) because the reading is deeply institutionalized and identity-fusion for scholars makes organized opposition structurally costly. The measurement series tracks the hardening of the performance_only reading's institutional dominance and the corresponding rise in suppression of alternatives (post-Talmudic codification, medieval responsaism, modernity). Early in the interval, alternative readings had more institutional space; by the interval end, the reading is nearly hegemonic in mainstream rabbinic discourse.
 *
 * PERSPECTIVAL GAP:
 *   From the rabbinic authority seat, the performance_only reading is correct interpretation of binding law—study preserves tradition, maintains readiness for restoration, and maintains the commandment's dignity by refusing to reinterpret it away. From the scholar seat (deeply educated in the framework), the reading feels necessary and binding, even though it produces perpetual non-compliance. From the study-as-performance advocates' seat, the same reading is an extractive snare that diverts resources and sustains false obligation. From the comparative religionists' seat, the constraint functions as an institutional machine that generates scholarly labor by making fulfillment impossible while suppressing the readings that would make it possible. The engine computes these divergences from the structural data: beneficiary/victim declarations, power differentials, exit options, institutional enforcement capacity.
 *
 * DIRECTIONALITY LOGIC:
 *   The halakhic scholars and practitioners are structural targets (d near 1.0): they bear perpetual non-compliance and identity-lock that makes exit nearly impossible. The rabbinic authority structure sits at d~0.4-0.5 (mixed: it administers the constraint and derives institutional legitimacy from it, but also bears some of the epistemic burden of sustaining an unperformable commandment). The Temple restoration movement is a beneficiary (d near 0.0): it collects justification and resource flow from the perpetual 'suspended' state. Study-as-performance and archive-maintenance advocates are excluded rather than economically positioned within the constraint—their exclusion is the enforcement object itself. The scholarly victims are those most deeply educated in the performance_only framework, because their identity and expertise are most locked into it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to treat an unperformable commandment after Temple destruction) has a clear answer under the study-as-performance reading: study IS performance. The performance_only reading refuses this answer, insisting that performance requires physical execution and that the commandment is therefore suspended. By the mandatrophy lens, the performance_only reading sustains the founding problem deliberately—it keeps alive the question 'what is the status of an unperformable obligation' when alternative readings would resolve it. The reading produces mandatrophy in the strict sense: the interpretive structure (performance_only framing) persists long after the problem it was meant to address is solvable through other means. The Temple could theoretically be rebuilt (it is possible, if unlikely); but the study-as-performance reading would solve the commandment problem without rebuilding—and it is suppressed. This is mandatrophy: a framework that keeps alive a non-performance state and directs resources toward its continuation, when alternative readings would dissolve the state. The constraint is mandatrophic because its primary function (maintaining the commandment's binding force while it cannot be performed) has outlived its necessity (study-as-performance is available and would fulfill the obligation), yet the constraint persists due to institutional investment in the performance_only framing and identity-fusion of scholars.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    performance_definition_ambiguity,
    'Does the performance_only reading''s definition of ''performance'' as necessarily physical execution have textual or logical grounding, or is it one interpretive choice among others presented as settled law?',
    'Genealogical-textual analysis: when did the performance_only reading emerge in the halakhic tradition? What textual basis do early sources cite? How did medieval and modern authorities cement it as binding? Alternative: legal-philosophical analysis of why ''performance'' must mean physical rather than intellectual engagement.',
    'If the definition is one interpretive choice, the study-as-performance reading gains textual legitimacy and the performance_only reading appears as a constructed constraint, not an inevitable reading. Extraction classification would shift from snare (suppressed alternatives) to false-summit (mountain masquerading as natural law). If the definition has solid textual grounding, the readings are genuinely distinct and the performance_only reading''s type remains snare (high extraction + suppression).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(performance_definition_ambiguity, conceptual, 'Whether ''performance'' definition is textually necessitated or interpretively chosen.').

omega_variable(
    study_labor_fungibility,
    'Could the scholarly resources currently directed into sacrifice-law study be redirected toward other living obligations if the performance_only reading were displaced?',
    'Analysis of communal resource allocation: what fraction of halakhic scholarship and yeshiva curriculum is devoted to sacrifice law? What would the distribution look like under study-as-performance (where study fulfills the obligation and resources could redirect)? Empirical check: in communities where study-as-performance is more accepted, do resources actually redistribute?',
    'If resources are truly fungible and would redirect, the performance_only reading''s extraction victim class is halakhic scholars and the broader Jewish community (resource opportunity cost is measurable). If resources are locked into tradition by institutional inertia independent of the reading''s logic, the extraction is lower—the constraint is maintaining scholarly institutions regardless of its framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(study_labor_fungibility, empirical, 'Whether scholarly resources diverted by the reading are truly extractive or locked in independently.').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Is the suppression of study-as-performance reading maintained primarily through institutional barriers (institutional control, social sanction) or through internalized belief that the performance_only reading is obviously correct?',
    'Post-exit trajectory analysis: if a scholar or community adopted study-as-performance and external institutional pressure were removed, would the internalized belief in performance_only persist or erode? What ratio of suppression persists after institutional barriers fall?',
    'If suppression is primarily structural (institutional), removing the institutional dominance of the performance_only reading would permit study-as-performance to flourish and the constraint''s type could shift. If suppression is primarily internalized, the reading''s dominance persists even without institutional enforcement—the constraint becomes more entrenched and identity-fusion is the primary mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Structural vs. internalized suppression mechanism.').

omega_variable(
    temple_restoration_probability,
    'How do changes in the probability or timeline of Temple restoration affect the performance_only reading''s extractiveness and classification?',
    'Scenario analysis: if Temple restoration became imminent (suddenly probable within a generation), would study-labor directed at sacrifice procedures shift from extractive to preparatory (reducing theater_ratio, shifting from snare toward scaffold)? Conversely, if Temple restoration became widely considered metaphorical/impossible, would the reading''s hold weaken and study-as-performance gain ground?',
    'If the reading''s extractiveness is coupled to Temple-restoration probability, then the performance_only reading is not stable across changing historical conditions—it would shift from snare (when Temple restoration is distant/unlikely) toward scaffold (if it became imminent). This would affect whether the reading is a stable constraint or a temporary institutional arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(temple_restoration_probability, empirical, 'Coupling between Temple-restoration probability and the reading''s extractiveness.').

omega_variable(
    committer_kernel_framing,
    'Is the sacrifice commandment kernel itself (the textual obligation to bring sacrifices) interpreted differently by each reading, or do the readings all interpret the same fixed textual object differently?',
    'Careful textual analysis: does performance_only cite different biblical/Talmudic passages than study-as-performance, or do both readings cite the same passages and disagree on interpretation? If they cite the same passages, is the disagreement purely semantic (what counts as ''performance'') or do they fundamentally locate the kernel differently?',
    'If the readings interpret the same kernel object differently, they are competitors for the same interpretive authority—one is right and the others are wrong (or all are live options). If they interpret different textual objects, they are not truly competitors—the kernel itself is under-specified. This affects whether the performance_only reading is a chosen constraint or the only correct interpretation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_kernel_framing, conceptual, 'Whether the readings compete for authority over the same kernel or interpret different kernels.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_commandment__performance_only, 0, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_commandment__performance_only, theater_ratio, 0, 0.35).
narrative_ontology:measurement(sacr_tr_t250, sacrifice_commandment__performance_only, theater_ratio, 250, 0.42).
narrative_ontology:measurement(sacr_tr_t600, sacrifice_commandment__performance_only, theater_ratio, 600, 0.52).
narrative_ontology:measurement(sacr_tr_t1000, sacrifice_commandment__performance_only, theater_ratio, 1000, 0.61).
narrative_ontology:measurement(sacr_tr_t1500, sacrifice_commandment__performance_only, theater_ratio, 1500, 0.65).
narrative_ontology:measurement(sacr_tr_t1900, sacrifice_commandment__performance_only, theater_ratio, 1900, 0.68).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_commandment__performance_only, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(sacr_be_t250, sacrifice_commandment__performance_only, base_extractiveness, 250, 0.58).
narrative_ontology:measurement(sacr_be_t600, sacrifice_commandment__performance_only, base_extractiveness, 600, 0.68).
narrative_ontology:measurement(sacr_be_t1000, sacrifice_commandment__performance_only, base_extractiveness, 1000, 0.75).
narrative_ontology:measurement(sacr_be_t1500, sacrifice_commandment__performance_only, base_extractiveness, 1500, 0.79).
narrative_ontology:measurement(sacr_be_t1900, sacrifice_commandment__performance_only, base_extractiveness, 1900, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_commandment__performance_only, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(sacr_su_t250, sacrifice_commandment__performance_only, suppression_requirement, 250, 0.51).
narrative_ontology:measurement(sacr_su_t600, sacrifice_commandment__performance_only, suppression_requirement, 600, 0.58).
narrative_ontology:measurement(sacr_su_t1000, sacrifice_commandment__performance_only, suppression_requirement, 1000, 0.65).
narrative_ontology:measurement(sacr_su_t1500, sacrifice_commandment__performance_only, suppression_requirement, 1500, 0.68).
narrative_ontology:measurement(sacr_su_t1900, sacrifice_commandment__performance_only, suppression_requirement, 1900, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_commandment__performance_only, identity_coordination).
narrative_ontology:boltzmann_floor_override(sacrifice_commandment__performance_only, 0.16).
narrative_ontology:affects_constraint(sacrifice_commandment__performance_only, sacrifice_commandment__study_as_performance).
narrative_ontology:affects_constraint(sacrifice_commandment__performance_only, sacrifice_commandment__archive_maintenance).

% DUAL FORMULATION NOTE:
% The sacrifice commandment kernel decomposes into three structurally distinct constraints, one per reading: performance_only (this file), study_as_performance, archive_maintenance. They share a common kernel (the biblical/Talmudic commandment) but instantiate different constraints because the readings differ on what counts as fulfilling the obligation. ε values differ by magnitude: performance_only (ε≈0.82, high extractiveness) vs. study_as_performance (lower extractiveness, obligation fulfilled through study) vs. archive_maintenance (moderate extractiveness, study as custodial). The readings coexist in Jewish tradition but are not equally endorsed; performance_only has institutional dominance. Each reading's constraint models the extraction structure peculiar to that reading's interpretation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
