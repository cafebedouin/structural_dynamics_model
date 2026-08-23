% ============================================================================
% CONSTRAINT STORY: legitimacy_of_imposed_practice__hybrid_scaffolding_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_imposed_practice__hybrid_scaffolding_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: legitimacy_of_imposed_practice__hybrid_scaffolding_reading
 *   human_readable: Hybrid Scaffolding of Imposed Cultural Practice Legitimacy
 *   domain: political/historical/cultural
 *
 * SUMMARY:
 *   This constraint models the hybrid_scaffolding_reading of the
 *   legitimacy_of_imposed_practice kernel: a state authority imposes cultural
 *   practices (calendar, dress, language) via top-down mandate, but pure
 *   decree fails (calendar reform collapses) while scaffolded imposition —
 *   elite modeling plus ideological messaging that frames the new practices
 *   as modern, scientific, and nationally authentic — achieves partial
 *   displacement with hybrid practices (Western dress adopted, traditional
 *   elements retained). The beneficiaries are urban elites who gain status
 *   and access by adopting Western identity markers through the scaffolding
 *   infrastructure. The victims are rural populations excluded from that
 *   infrastructure, whose traditional practices are delegitimized while they
 *   lack pathways to adopt the new markers on favorable terms. The constraint
 *   is claimed as tangled_rope: genuine coordination (shared cultural
 *   framework for bureaucracy/commerce) coexisting with asymmetric extraction
 *   (urban capture, rural exclusion), requiring active enforcement (state
 *   mandate + ideological apparatus).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0.65).
domain_priors:suppression_score(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0.7).
domain_priors:theater_ratio(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, tangled_rope).
narrative_ontology:human_readable(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, "Hybrid Scaffolding of Imposed Cultural Practice Legitimacy").
narrative_ontology:topic_domain(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, "political/historical/cultural").

domain_priors:requires_active_enforcement(legitimacy_of_imposed_practice__hybrid_scaffolding_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, '6b825a19-3119-4441-91ee-5c9fddbbd0f8').
narrative_ontology:cs_kernel_codification('6b825a19-3119-4441-91ee-5c9fddbbd0f8', formalized).
narrative_ontology:cs_authority_grounding('6b825a19-3119-4441-91ee-5c9fddbbd0f8', extraction).
narrative_ontology:cs_interpretation_layer_present('6b825a19-3119-4441-91ee-5c9fddbbd0f8').
narrative_ontology:cs_reading_relation('6b825a19-3119-4441-91ee-5c9fddbbd0f8', legitimacy_of_imposed_practice__endogenous_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('6b825a19-3119-4441-91ee-5c9fddbbd0f8', legitimacy_of_imposed_practice__exogenous_override_reading, coexists_with).
narrative_ontology:cs_axiom('6b825a19-3119-4441-91ee-5c9fddbbd0f8', foundational, ideological_messaging_generates_quasi_endogenous_pull).
narrative_ontology:cs_axiom_status(ideological_messaging_generates_quasi_endogenous_pull, holdable).
narrative_ontology:cs_axiom_grounding('6b825a19-3119-4441-91ee-5c9fddbbd0f8', ideological_messaging_generates_quasi_endogenous_pull, empirically_contingent).
narrative_ontology:cs_axiom('6b825a19-3119-4441-91ee-5c9fddbbd0f8', foundational, scaffolded_imposition_achieves_partial_displacement).
narrative_ontology:cs_axiom_status(scaffolded_imposition_achieves_partial_displacement, holdable).
narrative_ontology:cs_axiom_grounding('6b825a19-3119-4441-91ee-5c9fddbbd0f8', scaffolded_imposition_achieves_partial_displacement, empirically_contingent).
narrative_ontology:cs_reference_frame('6b825a19-3119-4441-91ee-5c9fddbbd0f8', state_led_cultural_standardization).
narrative_ontology:cs_drift_state('6b825a19-3119-4441-91ee-5c9fddbbd0f8', contemporary_postcolonial_critique, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6b825a19-3119-4441-91ee-5c9fddbbd0f8', '').
narrative_ontology:cs_kernel_id(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, legitimacy_of_imposed_practice).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, urban_elites_adopting_western_identity).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, rural_populations_excluded_from_scaffolding).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, ideological_apparatus).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, traditional_practice_adherents).
narrative_ontology:constraint_vindicates(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, hybrid_scaffolding_legitimacy_thesis).
narrative_ontology:constraint_vindicates(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, ideological_messaging_generates_quasi_endogenous_pull).
narrative_ontology:constraint_vindicates(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, scaffolded_imposition_achieves_partial_displacement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adopt Western dress, calendar, and cultural markers as status signals and pathways to state employment and commercial networks. Gain cultural capital and material advantage from the scaffolding infrastructure (schools, media, bureaucratic pathways) that validates their adopted identity. Can exit by reverting to traditional markers but lose the accumulated advantages.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, urban_elites_adopting_western_identity, beneficiary,
    organized, biographical, mobile, national).

% Bear the costs of cultural displacement without access to the scaffolding infrastructure (elite schools, state media, bureaucratic patronage) that would allow them to adopt the new identity markers on favorable terms. Their traditional practices are delegitimized by state mandate while the ideological messaging frames their exclusion as backwardness. Exit requires geographic migration to urban centers or cultural assimilation under disadvantageous terms.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, rural_populations_excluded_from_scaffolding, payer,
    powerless, generational, trapped, regional).

% Issues top-down mandates (calendar reform, dress codes, language standardization) and deploys ideological messaging (nationalist education, state media, ceremonial displays) to generate quasi-endogenous pull. The authority's legitimacy depends on the hybrid scaffolding succeeding — pure decree failed (calendar), pure climb was too slow. The state extracts compliance and cultural uniformity but also bears the cost of maintaining the ideological apparatus.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, state_authority_imposing_practice, agenda_setter,
    institutional, generational, analytical, national).

% Operates the messaging infrastructure (education curriculum, propaganda, cultural institutions) that frames the imposed practice as modern, scientific, and nationally authentic. Gains institutional resources, professional status, and narrative control from the scaffolding role. Partly captured by the state authority but also develops independent interest in the scaffolding's continuation.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, ideological_apparatus, agenda_setter,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, ideological_apparatus, beneficiary).

% Maintain pre-imposition cultural practices (traditional calendar, dress, rituals) as core identity. Experience the scaffolding as active suppression of their lifeworld — not merely exclusion but delegitimization. Their resistance is structurally constrained by state enforcement but sustained by identity fusion: abandoning the practices would constitute existential loss of self and community.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, traditional_practice_adherents, payer,
    moderate, generational, identity_locked, local).

% Analyze the constraint from outside: historians, anthropologists, political scientists studying state-led cultural imposition. They see the full structure — the failed pure decree (calendar), the partial hybrid success (dress), the quasi-endogenous pull generated by ideology, the urban/rural divergence. Their readings feed back into the kernel contest (endogenous vs exogenous vs hybrid).
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, external_scholars_observers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creates a shared cultural framework across a diverse population by combining state mandate with ideological framing that makes the imposed practice appear as natural modernization rather than foreign imposition, enabling coordination of bureaucracy, commerce, and national identity.
% TRANSFER_FUNCTION: Moves cultural capital, state employment access, commercial network entry, and status recognition from rural/traditional populations to urban/Westernized elites via the scaffolding infrastructure (elite schools, state media, bureaucratic pathways) that validates the new identity markers.
% ABSENT_VOICES: Rural traditional leaders, minority cultural groups maintaining alternative practices, diaspora communities preserving pre-imposition traditions, and women in traditional households who bear disproportionate cultural transmission burdens — all structurally excluded from the scaffolding infrastructure and the ideological messaging that defines legitimacy.
% DISAPPEARANCE_RATIONALE: If the hybrid scaffolding vanished overnight, the imposed practice would lose its legitimacy veneer and revert to perceived foreign imposition. Urban elites would lose the cultural capital that distinguishes them, rural populations would lose the delegitimizing framework but gain no immediate alternative, the state authority would face a legitimacy crisis, and the ideological apparatus would lose its mandate. Hybrid practices (Western dress with traditional elements) would collapse into either full reversion or intensified conflict.
% FOUNDING_PROBLEM: How to modernize and standardize cultural practices across a heterogeneous population to enable bureaucratic coordination, national defense, and commercial integration — while maintaining state legitimacy and avoiding the instability of pure coercion or the slowness of organic evolution.
% FOUNDING_PROBLEM_CORROBORATION: Historical sociologists of state formation (Gellner, Anderson, Tilly) corroborate that cultural standardization was a functional requirement for modern statehood. Postcolonial critics (Chakrabarty, Said, Scott) corroborate that the 'modernization' framing manufactured a problem to justify extraction and that organic pathways existed but were suppressed. The parties dispute whether the problem was real or constructed.
narrative_ontology:disappearance_verdict(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0.65, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_imposed_practice__hybrid_scaffolding_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimacy_of_imposed_practice__hybrid_scaffolding_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) reflects the net transfer of cultural capital and material access from rural to urban populations via the scaffolding. Suppression (0.7) is high initially (pure decree enforcement) then declines as ideological messaging generates quasi-endogenous pull, but stabilizes as the scaffolding infrastructure itself becomes a structural barrier. Theater ratio (0.4) rises over time as the ideological apparatus increasingly performs legitimacy maintenance rather than functional coordination. Accessibility collapse (0.55) is partial — hybrid practices persist, traditional practices survive in pockets, alternatives don't fully collapse. Resistance (0.5) is moderate — rural resistance is sustained but structurally constrained; urban elites don't resist (they benefit).
 *
 * PERSPECTIVAL GAP:
 *   The urban elite seat experiences the constraint as rope/coordination (genuine shared framework enabling their advancement). The rural payer seat experiences it as snare/extraction (delegitimization without access). The state authority seat experiences it as scaffold (transitional structure meant to achieve modernization). The engine computes these divergences from the structural data — the claimed_type (tangled_rope) is the generating model's structural judgment, not a reconciliation of seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Urban elites are structural beneficiaries (d near 0.0): they collect cultural capital and material advantages, have mobile exit, and the scaffolding is built for them. Rural populations are structural targets (d near 1.0): they bear the costs of delegitimization and exclusion, are trapped/identity-locked, and the scaffolding infrastructure actively filters against them. State authority is agenda_setter with analytical exit (it doesn't exit the constraint it administers) but experiences d ~ 0.5 — it extracts compliance but bears maintenance costs. Ideological apparatus is dual-positioned: agenda_setter (administers messaging) and beneficiary (gains resources/status), d ~ 0.2. Traditional adherents are identity-locked payers (d ~ 0.9) — exit means existential loss.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (modernization/standardization for state functionality) is contested: state builders say it was real and live; critics say it was manufactured to justify extraction. The constraint shows mandatrophy dynamics — the original mandate (pure decree) failed, the scaffolding was added as a fix, but the scaffolding itself became a vehicle for urban elite capture. The mandate has outlived its coordinating function (bureaucracy now functions with hybrid practices) but persists due to the ideological apparatus's institutional inertia and the urban elites' interest in maintaining the status distinction. The theater ratio rise captures this.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine structural reading of the legitimacy_of_imposed_practice kernel, or does it collapse the kernel''s contest into a single authorized interpretation?',
    'Compare the structural predictions of this reading (urban elite capture, rural exclusion, partial hybrid success) against the sibling readings'' predictions: endogenous_climb_reading predicts total failure of imposition without bottom-up pathways; exogenous_override_reading predicts total success of decree regardless of scaffolding. Empirical adjudication via historical case comparison (Meiji Japan, Atatürk Turkey, Pahlavi Iran, Soviet cultural revolution).',
    'If this reading forecloses the siblings, the kernel is not genuinely contested — one reading has captured the classification. If all three coexist as live positions, the kernel contest is real and each reading must be a separate constraint story.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the kernel contest is structurally real or an artifact of reading selection.').

omega_variable(
    scaffolding_coordination_vs_extraction,
    'Does the ideological messaging and elite modeling constitute genuine coordination (solving a collective action problem of cultural standardization) or is the coordination story cover for extraction (urban elite status capture at rural expense)?',
    'Measure whether the scaffolding infrastructure (schools, media, bureaucratic pathways) provides net benefits to rural populations who access it, or whether the infrastructure is designed to filter for pre-selected urban elites. Compare outcomes for rural migrants who do access the scaffolding vs. those who cannot.',
    'If genuine coordination, the constraint is a tangled_rope with real coordination function. If cover for extraction, it trends toward snare. The hybrid_scaffolding_reading''s claimed partial success with hybrid practices hangs on this distinction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scaffolding_coordination_vs_extraction, empirical, 'Whether the scaffolding''s coordination function is real or ideological cover.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the rural populations'' exclusion maintained by structural barriers (lack of schools, media access, bureaucratic pathways) or by internalized suppression (internalized inferiority, identity fusion with ''backwardness'' narrative)?',
    'Post-exit suppression trajectory: track rural migrants who access urban scaffolding — if they still experience cultural illegitimacy despite structural access, internalized suppression is operative. Compare with populations where scaffolding was never imposed (control cases).',
    'If internalized, the constraint''s effective suppression is higher than structural measures suggest — the target carries the suppression after exit. This would increase the effective extraction for the payer seat beyond the base suppression metric.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for rural exclusion.').

omega_variable(
    calendar_dress_divergence_causality,
    'Why did pure decree fail for calendar but scaffolded imposition partially succeed for dress? Is the difference in the practices themselves (calendar = abstract/systemic, dress = visible/performative) or in the scaffolding deployment?',
    'Compare scaffolding intensity: were elite modeling and ideological messaging deployed equally for both? Historical record suggests calendar reform had less ceremonial/ideological scaffolding than dress reform. Test by examining state media, education curriculum, and ceremonial emphasis for each domain.',
    'If the divergence is due to scaffolding intensity, the hybrid_scaffolding_reading''s causal claim (scaffolding enables success) is supported. If due to practice-type, the reading overstates scaffolding''s general efficacy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(calendar_dress_divergence_causality, empirical, 'Causal attribution for the calendar/dress outcome divergence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legitimacy_imposed_practice_hybrid_tr_t0, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(legitimacy_imposed_practice_hybrid_tr_t0, observed).
narrative_ontology:measurement(legitimacy_imposed_practice_hybrid_tr_t10, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement_basis(legitimacy_imposed_practice_hybrid_tr_t10, observed).
narrative_ontology:measurement(legitimacy_imposed_practice_hybrid_tr_t20, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement_basis(legitimacy_imposed_practice_hybrid_tr_t20, observed).
narrative_ontology:measurement(legitimacy_imposed_practice_hybrid_tr_t30, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement_basis(legitimacy_imposed_practice_hybrid_tr_t30, observed).
narrative_ontology:measurement(legitimacy_imposed_practice_hybrid_tr_t40, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 40, 0.39).
narrative_ontology:measurement_basis(legitimacy_imposed_practice_hybrid_tr_t40, observed).
narrative_ontology:measurement(legitimacy_imposed_practice_hybrid_tr_t50, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 50, 0.4).
narrative_ontology:measurement_basis(legitimacy_imposed_practice_hybrid_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(legitimacy_imposed_practice_hybrid_be_t0, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(legitimacy_imposed_practice_hybrid_be_t0, observed).
narrative_ontology:measurement(legitimacy_imposed_practice_hybrid_be_t10, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement_basis(legitimacy_imposed_practice_hybrid_be_t10, observed).
narrative_ontology:measurement(legitimacy_imposed_practice_hybrid_be_t20, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement_basis(legitimacy_imposed_practice_hybrid_be_t20, observed).
narrative_ontology:measurement(legitimacy_imposed_practice_hybrid_be_t30, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 30, 0.6).
narrative_ontology:measurement_basis(legitimacy_imposed_practice_hybrid_be_t30, observed).
narrative_ontology:measurement(legitimacy_imposed_practice_hybrid_be_t40, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 40, 0.63).
narrative_ontology:measurement_basis(legitimacy_imposed_practice_hybrid_be_t40, observed).
narrative_ontology:measurement(legitimacy_imposed_practice_hybrid_be_t50, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 50, 0.65).
narrative_ontology:measurement_basis(legitimacy_imposed_practice_hybrid_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(legitimacy_imposed_practice_hybrid_su_t0, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement_basis(legitimacy_imposed_practice_hybrid_su_t0, observed).
narrative_ontology:measurement(legitimacy_imposed_practice_hybrid_su_t10, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 10, 0.75).
narrative_ontology:measurement_basis(legitimacy_imposed_practice_hybrid_su_t10, observed).
narrative_ontology:measurement(legitimacy_imposed_practice_hybrid_su_t20, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(legitimacy_imposed_practice_hybrid_su_t20, observed).
narrative_ontology:measurement(legitimacy_imposed_practice_hybrid_su_t30, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement_basis(legitimacy_imposed_practice_hybrid_su_t30, observed).
narrative_ontology:measurement(legitimacy_imposed_practice_hybrid_su_t40, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 40, 0.69).
narrative_ontology:measurement_basis(legitimacy_imposed_practice_hybrid_su_t40, observed).
narrative_ontology:measurement(legitimacy_imposed_practice_hybrid_su_t50, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 50, 0.7).
narrative_ontology:measurement_basis(legitimacy_imposed_practice_hybrid_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0.08).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, state_formation_bureaucratic_standardization).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, national_identity_construction).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, cultural_assimilation_policy).

% DUAL FORMULATION NOTE:
% This constraint is one member of the legitimacy_of_imposed_practice constraint family. The sibling readings are endogenous_climb_reading and exogenous_override_reading. All three share the kernel (state-imposed cultural practice legitimacy) but differ structurally: endogenous_reading has no beneficiaries (coordination failure), exogenous_reading has no victims (decree works for all), hybrid_reading has both (tangled_rope). Their ε values differ substantially: endogenous ~0.1 (failed coordination), exogenous ~0.3 (decree cost only), hybrid ~0.65 (active extraction via scaffolding).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
