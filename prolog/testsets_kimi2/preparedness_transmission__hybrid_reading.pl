% ============================================================================
% CONSTRAINT STORY: preparedness_transmission__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_transmission__hybrid_reading, []).

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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: preparedness_transmission__hybrid_reading
 *   human_readable: Stratified Preparedness Transmission: Physical Infrastructure High, Civilian Coordination Decayed
 *   domain: disaster_risk_management/institutional_memory
 *
 * SUMMARY:
 *   This constraint story models one reading (hybrid_reading) of the
 *   preparedness_transmission kernel. The reading holds that disaster
 *   preparedness transmission has stratified: physical infrastructure and
 *   engineering competence remain high and functional, while civilian
 *   coordination knowledge has decayed. The result is a system that presents
 *   as readyâinfrastructure passes inspection, drills are performedâbut
 *   fails at the social coordination layer under stress, producing evacuation
 *   confusion and mutual-aid breakdown. Key agents include institutional
 *   emergency managers who administer the hardware-biased system,
 *   infrastructure vendors who benefit from procurement, and civilian
 *   residents who bear the coordination deficit. Other readings of the same
 *   kernel (husk_reading, competence_reading) treat the label as different
 *   structural constraints.
 *
 * KEY AGENTS:
 *   - emergency_management_agencies (agenda_setter/beneficiary, institutional/constrained) â administers metrics, collects legitimacy and budgetary stability
 *   - infrastructure_vendors (beneficiary, powerful/mobile) â supplies and maintains physical systems
 *   - civilian_residents (payer, powerless/constrained) â lacks coordination knowledge under stress despite functional infrastructure
 *   - grassroots_preparedness_networks (excluded, moderate/constrained) â maintains living knowledge outside official channels
 *   - disaster_researchers (observer, analytical) â documents the physical-social gap
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_transmission__hybrid_reading, 0.68).
domain_priors:suppression_score(preparedness_transmission__hybrid_reading, 0.55).
domain_priors:theater_ratio(preparedness_transmission__hybrid_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_transmission__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(preparedness_transmission__hybrid_reading, "Stratified Preparedness Transmission: Physical Infrastructure High, Civilian Coordination Decayed").
narrative_ontology:topic_domain(preparedness_transmission__hybrid_reading, "disaster_risk_management/institutional_memory").

domain_priors:requires_active_enforcement(preparedness_transmission__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_transmission__hybrid_reading, '2bf57b3d-b76d-456b-abab-503f83b89c04').
narrative_ontology:cs_kernel_codification('2bf57b3d-b76d-456b-abab-503f83b89c04', distributed).
narrative_ontology:cs_authority_grounding('2bf57b3d-b76d-456b-abab-503f83b89c04', practice).
narrative_ontology:cs_interpretation_layer_present('2bf57b3d-b76d-456b-abab-503f83b89c04').
narrative_ontology:cs_reading_relation('2bf57b3d-b76d-456b-abab-503f83b89c04', preparedness_transmission__husk_reading, coexists_with).
narrative_ontology:cs_reading_relation('2bf57b3d-b76d-456b-abab-503f83b89c04', preparedness_transmission__competence_reading, coexists_with).
narrative_ontology:cs_axiom('2bf57b3d-b76d-456b-abab-503f83b89c04', foundational, preparedness_transmission_is_stratified).
narrative_ontology:cs_axiom_status(preparedness_transmission_is_stratified, holdable).
narrative_ontology:cs_axiom_grounding('2bf57b3d-b76d-456b-abab-503f83b89c04', preparedness_transmission_is_stratified, empirically_contingent).
narrative_ontology:cs_axiom('2bf57b3d-b76d-456b-abab-503f83b89c04', foundational, social_coordination_decays_under_hardware_priority).
narrative_ontology:cs_axiom_status(social_coordination_decays_under_hardware_priority, holdable).
narrative_ontology:cs_axiom_grounding('2bf57b3d-b76d-456b-abab-503f83b89c04', social_coordination_decays_under_hardware_priority, empirically_contingent).
narrative_ontology:cs_reference_frame('2bf57b3d-b76d-456b-abab-503f83b89c04', integrated_preparedness_system).
narrative_ontology:cs_drift_state('2bf57b3d-b76d-456b-abab-503f83b89c04', contemporary_stress_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2bf57b3d-b76d-456b-abab-503f83b89c04', '').
narrative_ontology:cs_kernel_id(preparedness_transmission__hybrid_reading, preparedness_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_transmission__hybrid_reading, emergency_management_agencies).
narrative_ontology:constraint_beneficiary(preparedness_transmission__hybrid_reading, infrastructure_vendors).
narrative_ontology:constraint_victim(preparedness_transmission__hybrid_reading, civilian_residents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer civil defense budgets, set preparedness metrics centered on infrastructure readiness and drill compliance, and report readiness to political oversight. Collect stable funding and institutional legitimacy from demonstrable physical assets. Face political and bureaucratic constraints that make shifting investment toward diffuse social coordination costly and reputationally risky.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, emergency_management_agencies, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(preparedness_transmission__hybrid_reading, emergency_management_agencies, beneficiary).

% Design, build, and maintain physical disaster infrastructure such as warning systems, shelters, and emergency communications networks. Collect revenue from procurement and maintenance contracts tied to institutional readiness metrics. Able to shift business to other jurisdictions or technical sectors.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, infrastructure_vendors, beneficiary,
    powerful, biographical, mobile, national).

% Live in disaster-prone areas and depend on official preparedness systems. Have lost inter-generational coordination knowledge including evacuation route familiarity, neighborhood mutual aid protocols, and ad-hoc communication practices because institutional transmission focused on hardware. Under stress, struggle to self-coordinate despite functional sirens and buildings.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, civilian_residents, payer,
    powerless, biographical, constrained, local).

% Maintain living community-based coordination practices and informal warning systems at the neighborhood level. Excluded from official preparedness metrics, funding streams, and institutional planning tables; their knowledge is treated as auxiliary rather than core to readiness.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, grassroots_preparedness_networks, excluded,
    moderate, biographical, constrained, local).

% Study disaster outcomes and repeatedly document gaps between infrastructure performance and social coordination under stress. Publish findings showing that coordination knowledge decay explains evacuation failures, but compete with hardware-focused policy narratives for institutional attention.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, disaster_researchers, observer,
    analytical, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains functional physical disaster infrastructure and institutional response protocols; coordinates capital investment, engineering standards, and asset maintenance across jurisdictions so that hardware performs as designed during emergencies.
% TRANSFER_FUNCTION: Moves public funds and institutional legitimacy toward physical infrastructure procurement and maintenance; moves risk exposure and coordination burden onto civilian populations who no longer possess practiced mutual-aid and evacuation knowledge.
% ABSENT_VOICES: Grassroots preparedness networks and community mutual-aid organizations maintain living coordination knowledge but are excluded from official metrics, funding, and planning tables; they would argue for social-capital investment and participatory drills but are structurally absent from the conversation.
% DISAPPEARANCE_RATIONALE: If the stratified transmission arrangement vanished, the institutional preference for hardware metrics would lose its enforcement; funds would shift toward social coordination or preparedness would visibly fragment, and civilian outcomes under stress would change because the current false-confidence structure would collapse.
% FOUNDING_PROBLEM: How to maintain societal disaster readiness across long periods without frequent live disasters to validate and refresh knowledge and infrastructure.
% FOUNDING_PROBLEM_CORROBORATION: Emergency management agencies claim the problem remains live and is addressed by infrastructure readiness. Disaster researchers and grassroots networks attest that the social-coordination half of the founding problem is unaddressed; post-disaster forensic reviews from independent investigative bodies repeatedly corroborate coordination failure despite functional infrastructure.
narrative_ontology:disappearance_verdict(preparedness_transmission__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_transmission__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_transmission__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(preparedness_transmission__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_transmission__hybrid_reading, 0.68, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_transmission__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(preparedness_transmission__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(preparedness_transmission__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high because the constraint substitutes hardware readiness for genuine societal readiness, extracting safety from civilians who are left with deteriorated coordination capacity. Suppression (0.55) is moderate: alternatives such as community-based coordination and social-capital investment are structurally defunded and excluded from metrics but not violently repressed. Theater_ratio (0.45) reflects the growing performative component of drills and inspections that certify readiness without ensuring coordination. Accessibility_collapse (0.60) captures how civilian alternativesâinter-generational knowledge and local mutual aidâhave atrophied under institutional crowding-out. Resistance (0.40) is moderate: researchers and grassroots groups document the gap but lack leverage to shift budgets. The temporal series show extraction and theater rising together over the interval as hardware investment deepened and social coordination was progressively neglected.
 *
 * PERSPECTIVAL GAP:
 *   The emergency management agency seat experiences the constraint as genuine coordination: infrastructure works, drills occur, metrics are met, and assets perform. The civilian resident seat experiences the same constraint as extraction or abandonment: the official system claims readiness but does not transmit the coordination knowledge they need under stress. The engine computes this divergence from identical structural data via directionalityâagencies are beneficiaries (low d), civilians are victims (high d)âwithout requiring the author to reconcile the two perspectives.
 *
 * DIRECTIONALITY LOGIC:
 *   Emergency management agencies and infrastructure vendors are declared beneficiaries: they collect budgetary flows, legitimacy, and maintenance contracts from the hardware-centric arrangement, placing them toward the beneficiary end of directionality. Civilian residents are declared victims: they pay through unpriced risk exposure and coordination incapacity, placing them toward the target end. Researchers are analytical observers with neutral directionality. Grassroots networks are excluded, relevant to the suppression picture but not directly to extraction arithmetic.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling this as pure coordination (Rope) because the victim set is non-empty and extraction is substantial: civilians genuinely lack coordination knowledge that the system implicitly promises. It prevents mislabeling as pure extraction (Snare) because the physical infrastructure layer is structurally functional and provides genuine coordination value during emergencies. The Tangled Rope classification captures the hybrid: real coordination in one layer, asymmetric extraction in another, held together by active institutional enforcement of hardware-biased metrics and drill rituals.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    social_decay_mechanism,
    'Is civilian coordination decay a deliberate cost-avoidance by institutions favoring measurable hardware, or an emergent byproduct of urbanization and social change?',
    'Comparative historical analysis of preparedness systems with varying hardware/social investment balances; institutional budget-process ethnography tracing decision criteria over time.',
    'If deliberate, extraction is higher and directionality should shift toward institutional beneficiaries; if emergent, the constraint is more inertial and piton-like in character.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(social_decay_mechanism, empirical, 'Whether coordination decay is intentional institutional optimization or emergent social drift.').

omega_variable(
    kernel_reading_validity,
    'Does the stratified reading correctly identify a separable physical-social layer divergence, or does it mistakenly ascribe intentionality to a uniformly hollowed system?',
    'Comparison with husk_reading: if physical infrastructure also fails under deeper operational inspection, the stratified reading is false; if the physical layer genuinely performs while coordination fails, the stratified reading is supported.',
    'Would reclassify the constraint toward piton (uniform decay) or confirm tangled_rope (genuine coordination in one layer, extraction in another).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_validity, conceptual, 'Whether this reading''s layer-separation claim is structurally sound.').

omega_variable(
    suppression_ambiguity_social_layer,
    'Is the collapse of civilian coordination alternatives structural (funding and institutional recognition withdrawn) or internalized (populations believe infrastructure substitutes for social readiness)?',
    'Attitude surveys and behavioral drills: if civilians exposed to coordination training recover capabilities quickly, suppression is structural; if they resist or dismiss training, suppression is partially internalized.',
    'Internalized suppression raises effective extraction because the target carries the constraint after any structural reform, complicating remediation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_ambiguity_social_layer, empirical, 'Structural versus internalized suppression in the social coordination layer.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_transmission__hybrid_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_transmission__hybrid_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(prep_tr_t10, preparedness_transmission__hybrid_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(prep_tr_t20, preparedness_transmission__hybrid_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(prep_tr_t30, preparedness_transmission__hybrid_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement(prep_tr_t40, preparedness_transmission__hybrid_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement(prep_tr_t50, preparedness_transmission__hybrid_reading, theater_ratio, 50, 0.45).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_transmission__hybrid_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(prep_be_t10, preparedness_transmission__hybrid_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(prep_be_t20, preparedness_transmission__hybrid_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(prep_be_t30, preparedness_transmission__hybrid_reading, base_extractiveness, 30, 0.52).
narrative_ontology:measurement(prep_be_t40, preparedness_transmission__hybrid_reading, base_extractiveness, 40, 0.6).
narrative_ontology:measurement(prep_be_t50, preparedness_transmission__hybrid_reading, base_extractiveness, 50, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_transmission__hybrid_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(prep_su_t10, preparedness_transmission__hybrid_reading, suppression_requirement, 10, 0.42).
narrative_ontology:measurement(prep_su_t20, preparedness_transmission__hybrid_reading, suppression_requirement, 20, 0.45).
narrative_ontology:measurement(prep_su_t30, preparedness_transmission__hybrid_reading, suppression_requirement, 30, 0.5).
narrative_ontology:measurement(prep_su_t40, preparedness_transmission__hybrid_reading, suppression_requirement, 40, 0.52).
narrative_ontology:measurement(prep_su_t50, preparedness_transmission__hybrid_reading, suppression_requirement, 50, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_transmission__hybrid_reading, resource_allocation).
narrative_ontology:affects_constraint(preparedness_transmission__hybrid_reading, preparedness_transmission__husk_reading).
narrative_ontology:affects_constraint(preparedness_transmission__hybrid_reading, preparedness_transmission__competence_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the preparedness_transmission kernel, decomposed per the epsilon-invariance principle. The hybrid reading claims physical infrastructure competence and civilian coordination decay are separable layers; the husk reading claims uniform hollowing; the competence reading claims both layers are live and revalidated through practice.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
