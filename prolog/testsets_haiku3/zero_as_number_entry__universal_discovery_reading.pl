% ============================================================================
% CONSTRAINT STORY: zero_as_number_entry__universal_discovery_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_zero_as_number_entry__universal_discovery_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: zero_as_number_entry__universal_discovery_reading
 *   human_readable: Zero-as-Number: Universal Mathematical Discovery Reading
 *   domain: mathematics/philosophy/conceptual_history
 *
 * SUMMARY:
 *   This reading instantiates the universal-discovery thesis: zero-as-number
 *   is a mathematical necessity that flows logically from positional notation
 *   and arithmetic operations. It was always available to any mathematical
 *   tradition sufficiently developed to use positional notation. Indian
 *   mathematicians formalized it first (7th–12th centuries), Europeans later
 *   (12th–17th centuries), via transmission or independent discovery. The
 *   priority of discovery is a historical fact; the ontological status is
 *   independent of priority. Under this reading, there are no victims
 *   (mathematics does not lose from universal truths), no monopoly benefit
 *   (all mathematics benefits equally from correctness), and no extraction
 *   dynamic. The constraint is a mountain: an irreducible feature of
 *   mathematical logic, not an arrangement held in place by enforcement or
 *   choice.
 *
 * KEY AGENTS:
 *   - mathematical_community — all beneficiaries of universal mathematical truth
 *   - indian_mathematical_tradition — first to formalize the discovery
 *   - european_mathematical_tradition — later discovery path
 *   - mathematical_universality_doctrine — vindicated by this reading's core claim
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zero_as_number_entry__universal_discovery_reading, 0.05).
domain_priors:suppression_score(zero_as_number_entry__universal_discovery_reading, 0.02).
domain_priors:theater_ratio(zero_as_number_entry__universal_discovery_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zero_as_number_entry__universal_discovery_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(zero_as_number_entry__universal_discovery_reading, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(zero_as_number_entry__universal_discovery_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zero_as_number_entry__universal_discovery_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(zero_as_number_entry__universal_discovery_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zero_as_number_entry__universal_discovery_reading, mountain).
narrative_ontology:human_readable(zero_as_number_entry__universal_discovery_reading, "Zero-as-Number: Universal Mathematical Discovery Reading").
narrative_ontology:topic_domain(zero_as_number_entry__universal_discovery_reading, "mathematics/philosophy/conceptual_history").

domain_priors:emerges_naturally(zero_as_number_entry__universal_discovery_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zero_as_number_entry__universal_discovery_reading, '4239db5d-e584-4a36-98ae-f62e06ad182a').
narrative_ontology:cs_kernel_codification('4239db5d-e584-4a36-98ae-f62e06ad182a', distributed).
narrative_ontology:cs_authority_grounding('4239db5d-e584-4a36-98ae-f62e06ad182a', expertise).
narrative_ontology:cs_interpretation_layer_present('4239db5d-e584-4a36-98ae-f62e06ad182a').
narrative_ontology:cs_reading_relation('4239db5d-e584-4a36-98ae-f62e06ad182a', zero_as_number_entry__contingent_thinkability_reading, coexists_with).
narrative_ontology:cs_reading_relation('4239db5d-e584-4a36-98ae-f62e06ad182a', zero_as_number_entry__hybrid_scaffolding_reading, coexists_with).
narrative_ontology:cs_axiom('4239db5d-e584-4a36-98ae-f62e06ad182a', foundational, mathematical_universality_doctrine).
narrative_ontology:cs_axiom_status(mathematical_universality_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('4239db5d-e584-4a36-98ae-f62e06ad182a', mathematical_universality_doctrine, deontological).
narrative_ontology:cs_axiom('4239db5d-e584-4a36-98ae-f62e06ad182a', foundational, mathematical_truth_independence_from_discovery_order).
narrative_ontology:cs_axiom_status(mathematical_truth_independence_from_discovery_order, holdable).
narrative_ontology:cs_axiom_grounding('4239db5d-e584-4a36-98ae-f62e06ad182a', mathematical_truth_independence_from_discovery_order, deontological).
narrative_ontology:cs_axiom('4239db5d-e584-4a36-98ae-f62e06ad182a', secondary, logical_necessity_of_zero_in_positional_systems).
narrative_ontology:cs_axiom_status(logical_necessity_of_zero_in_positional_systems, holdable).
narrative_ontology:cs_axiom_grounding('4239db5d-e584-4a36-98ae-f62e06ad182a', logical_necessity_of_zero_in_positional_systems, empirically_contingent).
narrative_ontology:cs_reference_frame('4239db5d-e584-4a36-98ae-f62e06ad182a', mathematical_realism_framework).
narrative_ontology:cs_drift_state('4239db5d-e584-4a36-98ae-f62e06ad182a', contemporary_mathematical_discourse, gap(stable, minor, false)).
narrative_ontology:cs_created_at('4239db5d-e584-4a36-98ae-f62e06ad182a', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(zero_as_number_entry__universal_discovery_reading, zero_as_number_entry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zero_as_number_entry__universal_discovery_reading, mathematical_community).
narrative_ontology:constraint_vindicates(zero_as_number_entry__universal_discovery_reading, mathematical_universality_doctrine).
narrative_ontology:constraint_vindicates(zero_as_number_entry__universal_discovery_reading, logic_independence_from_culture).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The entire mathematical tradition benefits from the truth that zero-as-number is a universal necessity rather than a cultural contingency. This reading affirms that mathematical discovery converges on pre-existing truths regardless of the discovering culture's path or timing.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__universal_discovery_reading, mathematical_community, beneficiary,
    institutional, civilizational, analytical, universal).

% First to formalize zero-as-number in operational systems (Aryabhata, Brahmagupta, et al.). Under this reading, their achievement is the discovery of a universal mathematical truth, not the invention of a culturally-contingent concept.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__universal_discovery_reading, indian_mathematical_tradition, observer,
    institutional, civilizational, analytical, regional).

% Discovered zero-as-number later, either through transmission or independently. Under this reading, their path was slower but led to the same universal truth; priority does not affect ontological status.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__universal_discovery_reading, european_mathematical_tradition, observer,
    institutional, civilizational, analytical, regional).

% The proposition that mathematical truths exist independent of human discovery or cultural context. This reading vindicates this doctrine.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__universal_discovery_reading, mathematical_universality_doctrine, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(zero_as_number_entry__universal_discovery_reading, mathematical_universality_doctrine).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None. Zero-as-number is not a coordinating arrangement but a mathematical truth. The 'coordination' is purely logical: the necessity of zero follows from positional notation + arithmetic operations.
% TRANSFER_FUNCTION: None. No transfer of resources or obligation occurs. This is a natural law, not an arrangement with winners and losers.
% ABSENT_VOICES: Philosophers and historians of mathematics who argue the concept is historically contingent or culturally embedded would dispute this reading's universality claim. They are not excluded from the conversation but are unpersuaded by this frame.
% DISAPPEARANCE_RATIONALE: If this constraint 'disappeared' (i.e., if zero-as-number ceased to be a mathematical necessity), mathematics itself would be logically inconsistent. Zero-as-number does not depend on human recognition or institutional maintenance — it is a feature of the structure of positional notation and arithmetic, which are themselves necessary features of any workable positional system.
% FOUNDING_PROBLEM: How is mathematical truth related to discovery? Is zero-as-number a human invention, a culturally contingent concept, or a discovery of a pre-existing mathematical necessity?
% FOUNDING_PROBLEM_CORROBORATION: This reading is supported by mathematical logicians and philosophy of mathematics scholars who argue for mathematical realism and universality (e.g., Quine, Gödel's tradition). It is contested by historians of mathematics and postcolonial scholars who emphasize cultural contingency and the role of specific traditions in making concepts thinkable. External corroboration from realist philosophers of mathematics is strong; corroboration from historians remains contested.
narrative_ontology:disappearance_verdict(zero_as_number_entry__universal_discovery_reading, world_unchanged).
narrative_ontology:founding_problem_status(zero_as_number_entry__universal_discovery_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zero_as_number_entry__universal_discovery_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(zero_as_number_entry__universal_discovery_reading, 'none', 1).
narrative_ontology:epsilon_provenance(zero_as_number_entry__universal_discovery_reading, 0.05, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(zero_as_number_entry__universal_discovery_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(zero_as_number_entry__universal_discovery_reading, ExtMetricName, E),
    domain_priors:suppression_score(zero_as_number_entry__universal_discovery_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(zero_as_number_entry__universal_discovery_reading),
    narrative_ontology:constraint_metric(zero_as_number_entry__universal_discovery_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(zero_as_number_entry__universal_discovery_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(zero_as_number_entry__universal_discovery_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is near-zero (0.05) because this reading asserts zero-as-number is not a choice point or an arrangement that could be otherwise; it flows from necessity. Suppression is negligible (0.02) because mathematical truth requires no enforcement — it is true whether anyone recognizes it or not. Theater ratio is zero: there is no performative maintenance of a mathematical law. Accessibility collapse is very high (0.92): once the logical necessity of zero is understood, alternatives collapse completely — one cannot coherently deny it without rejecting positional notation or arithmetic itself. Resistance is very low (0.08): even those who contest the reading's universality claim do not mount active resistance to the mathematical fact; they contest the philosophical interpretation. The measurement series run flat across the 2000-year interval because the constraint's structural properties do not change — mathematical necessity is timeless.
 *
 * PERSPECTIVAL GAP:
 *   There is minimal perspectival gap on the technical question (all mathematical seats compute the same classification: mountain). The gap lies in the philosophical interpretation: the Indian tradition formalized zero earlier; the European tradition arrived later. Under this reading, both discovered the same truth. Under the contingent_thinkability_reading, the European tradition would not have arrived independently (counterfactual: contingent on transmission). Under the hybrid_scaffolding_reading, both had access to the latent structure but differed in their conceptual frameworks for recognizing it. The engine computes the per-seat classification from metrics alone; the perspectival gap is philosophical (which reading captures the true nature of mathematical discovery), not technical.
 *
 * DIRECTIONALITY LOGIC:
 *   All stakeholders sit at d ≈ 0.0 (full beneficiary pole) because they do not bear costs from this constraint; they benefit from its truth. There are no payers, no targets. The mathematical truth yields no extraction. The 'beneficiaries' are listed only to satisfy the FSM schema (a mountain with declared beneficiaries triggers False Summit evaluation). The presence of beneficiaries here does not indicate asymmetric extraction — it indicates that the truth of mathematics benefits the mathematical community universally.
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy is present. The constraint's founding problem (the nature of mathematical truth) is live and remains the subject of active philosophical debate. The constraint has not outlived its function; it persists because it is true, not because it persists out of institutional inertia. The reading affirms the original mandate: mathematical universality is a feature of mathematical logic itself, not a contingent institutional arrangement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mathematical_realism_vs_constructivism,
    'Is zero-as-number a discovery of a pre-existing mathematical structure, or a human construction that becomes true once institutionalized?',
    'Philosophical analysis of mathematical ontology and epistemology; comparison of discovery patterns across cultures; examination of counterfactuals (would zero arise in all sufficiently developed positional systems, or only in those with specific conceptual frameworks).',
    'If realism is correct, this reading''s universality thesis holds: zero is a mountain regardless of discovery order. If constructivism is correct, zero-as-number is more like a scaffold or artifact, and its status is historical/cultural rather than timeless.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mathematical_realism_vs_constructivism, conceptual, 'Ontological status of mathematical objects: discovered or constructed.').

omega_variable(
    transmission_vs_independent_discovery,
    'Did European mathematics discover zero independently, or was the discovery transmitted (directly or indirectly) from Indian/Islamic sources?',
    'Detailed historiographical analysis of manuscripts and transmission paths; examination of temporal gaps and evidence of contact; linguistic and notational analysis of adoption patterns.',
    'If independent: this reading''s universality claim is strongly supported (two independent routes to the same necessity). If transmitted: the reading remains consistent but raises secondary questions about the contingency of transmission itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transmission_vs_independent_discovery, empirical, 'Historical factual question about the route of zero''s entry into European mathematics.').

omega_variable(
    conceptual_barriers_in_greek_framework,
    'Would Greek/Aristotelian metaphysics have blocked the recognition of zero-as-number even if positional notation had been available within that tradition?',
    'Philosophical reconstruction of Aristotelian metaphysics relative to zero (potentiality, actuality, the void); historical counterfactual analysis; examination of late-Hellenistic and Byzantine mathematics for any emergent recognition of zero-like concepts.',
    'If metaphysical barriers were decisive: this reading''s universality claim weakens (zero would have been unavailable to Greeks even given the structural prerequisites). If barriers were negligible: this reading''s universality claim strengthens (Greek traditions could have discovered zero given different historical circumstances).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(conceptual_barriers_in_greek_framework, conceptual, 'Whether conceptual frameworks determine mathematical availability, or whether structural prerequisites alone suffice.').

omega_variable(
    false_summit_natural_law_status,
    'Is zero-as-number a genuine natural law (mathematical necessity independent of human cognition), or a constructed constraint that benefits the mathematical community by affirming their universality doctrine?',
    'Philosophical examination of mathematical Platonism vs. anti-realism; analysis of whether the mathematical community has institutional interest in claiming universality; consideration of counterfactuals (would mathematics work without zero, or is it truly necessary).',
    'If genuinely natural: the mountain classification holds. If constructed-but-beneficial: FSM fires and reclassification to tangled_rope or false_summit occurs; zero-as-number would then be understood as a coordinating narrative that benefits mathematicians by affirming their discipline''s transcultural authority.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(false_summit_natural_law_status, preference, 'Whether the constraint is a mathematical necessity or a beneficiary-serving narrative about mathematical universality.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zero_as_number_entry__universal_discovery_reading, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zero_tr_t0, zero_as_number_entry__universal_discovery_reading, theater_ratio, 0, 0.0).
narrative_ontology:measurement_basis(zero_tr_t0, observed).
narrative_ontology:measurement(zero_tr_t500, zero_as_number_entry__universal_discovery_reading, theater_ratio, 500, 0.0).
narrative_ontology:measurement_basis(zero_tr_t500, observed).
narrative_ontology:measurement(zero_tr_t1000, zero_as_number_entry__universal_discovery_reading, theater_ratio, 1000, 0.0).
narrative_ontology:measurement_basis(zero_tr_t1000, observed).
narrative_ontology:measurement(zero_tr_t1500, zero_as_number_entry__universal_discovery_reading, theater_ratio, 1500, 0.0).
narrative_ontology:measurement_basis(zero_tr_t1500, observed).
narrative_ontology:measurement(zero_tr_t2000, zero_as_number_entry__universal_discovery_reading, theater_ratio, 2000, 0.0).
narrative_ontology:measurement_basis(zero_tr_t2000, observed).

% Extraction over time
narrative_ontology:measurement(zero_be_t0, zero_as_number_entry__universal_discovery_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement_basis(zero_be_t0, observed).
narrative_ontology:measurement(zero_be_t500, zero_as_number_entry__universal_discovery_reading, base_extractiveness, 500, 0.05).
narrative_ontology:measurement_basis(zero_be_t500, observed).
narrative_ontology:measurement(zero_be_t1000, zero_as_number_entry__universal_discovery_reading, base_extractiveness, 1000, 0.05).
narrative_ontology:measurement_basis(zero_be_t1000, observed).
narrative_ontology:measurement(zero_be_t1500, zero_as_number_entry__universal_discovery_reading, base_extractiveness, 1500, 0.05).
narrative_ontology:measurement_basis(zero_be_t1500, observed).
narrative_ontology:measurement(zero_be_t2000, zero_as_number_entry__universal_discovery_reading, base_extractiveness, 2000, 0.05).
narrative_ontology:measurement_basis(zero_be_t2000, observed).

% Suppression requirement over time
narrative_ontology:measurement(zero_su_t0, zero_as_number_entry__universal_discovery_reading, suppression_requirement, 0, 0.0).
narrative_ontology:measurement_basis(zero_su_t0, observed).
narrative_ontology:measurement(zero_su_t500, zero_as_number_entry__universal_discovery_reading, suppression_requirement, 500, 0.0).
narrative_ontology:measurement_basis(zero_su_t500, observed).
narrative_ontology:measurement(zero_su_t1000, zero_as_number_entry__universal_discovery_reading, suppression_requirement, 1000, 0.0).
narrative_ontology:measurement_basis(zero_su_t1000, observed).
narrative_ontology:measurement(zero_su_t1500, zero_as_number_entry__universal_discovery_reading, suppression_requirement, 1500, 0.0).
narrative_ontology:measurement_basis(zero_su_t1500, observed).
narrative_ontology:measurement(zero_su_t2000, zero_as_number_entry__universal_discovery_reading, suppression_requirement, 2000, 0.0).
narrative_ontology:measurement_basis(zero_su_t2000, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zero_as_number_entry__universal_discovery_reading, information_standard).
narrative_ontology:affects_constraint(zero_as_number_entry__universal_discovery_reading, zero_as_number_entry__contingent_thinkability_reading).
narrative_ontology:affects_constraint(zero_as_number_entry__universal_discovery_reading, zero_as_number_entry__hybrid_scaffolding_reading).

% DUAL FORMULATION NOTE:
% The zero_as_number_entry kernel decomposes into three constraint stories corresponding to three competing readings of the kernel's meaning: universal_discovery_reading (this file), contingent_thinkability_reading, and hybrid_scaffolding_reading. Each reading instantiates a different constraint because each asserts a different ε (extractiveness from transmission contingency), a different beneficiary structure (universal mathematics vs. specific traditions vs. those with scaffolding frameworks), and a different type (mountain vs. tangled_rope vs. scaffold). The stories are linked via network.affects_constraints because they share a kernel (the disputed nature of zero's ontological and historical status) but diverge in their structural classification. The universal_discovery reading asserts zero is a mountain; the contingent reading asserts it is more contingent (tangled_rope or snare); the hybrid reading asserts it is a scaffold requiring specific conceptual prerequisites. All three readings converge on the same historical facts (India discovered/formalized first, Europe later) but diverge on the philosophical interpretation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
