% ============================================================================
% CONSTRAINT STORY: imposition_mechanism_kernel__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imposition_mechanism_kernel__exogenous_override_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: imposition_mechanism_kernel__exogenous_override_reading
 *   human_readable: State-Coerced Norm Imposition (Exogenous Override Reading)
 *   domain: political/sociological
 *
 * SUMMARY:
 *   The exogenous override reading frames state-imposed normative change as
 *   coercive substitution: a ruling authority displaces distributed, locally
 *   legitimate norms with a unified regime backed by violence rather than
 *   cultural acceptance. The new norms serve state consolidation, resource
 *   extraction, and territorial control. Legitimacy is explicitly NOT derived
 *   from populations' adoption of the norms as culturally sound, but from the
 *   state's demonstrated monopoly on violence — the capacity to punish
 *   noncompliance and eliminate alternatives. This reading applies to
 *   normative shifts imposed by conquest, colonial authority, authoritarian
 *   consolidation, or centralization wars. Compliance is conditional on
 *   enforcement presence; when the state's enforcement apparatus weakens or
 *   withdraws, the older norms rapidly re-emerge.
 *
 * KEY AGENTS:
 *   - State apparatus: designs and enforces new norms; legitimacy rests on coercive capacity
 *   - Ruling authority: benefits from norm unification for resource extraction and administrative control
 *   - Subject populations: comply through threat of punishment; exit is blocked (trapped)
 *   - Cultural resistors: defend pre-existing norms; suppressed by force (payer + excluded)
 *   - Enforcement apparatus: executes punishment; benefits from institutional survival and authority
 *   - Intellectual legitimators: craft post-hoc narratives of divine or natural justification (high theater)
 *   - External observers: document the constraint's structure from outside the coercive system
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_mechanism_kernel__exogenous_override_reading, 0.82).
domain_priors:suppression_score(imposition_mechanism_kernel__exogenous_override_reading, 0.89).
domain_priors:theater_ratio(imposition_mechanism_kernel__exogenous_override_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 0.89).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_mechanism_kernel__exogenous_override_reading, tangled_rope).
narrative_ontology:human_readable(imposition_mechanism_kernel__exogenous_override_reading, "State-Coerced Norm Imposition (Exogenous Override Reading)").
narrative_ontology:topic_domain(imposition_mechanism_kernel__exogenous_override_reading, "political/sociological").

domain_priors:requires_active_enforcement(imposition_mechanism_kernel__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_mechanism_kernel__exogenous_override_reading, '0703bb8b-a2bb-49c5-b4cd-56abb0a62fe0').
narrative_ontology:cs_kernel_codification('0703bb8b-a2bb-49c5-b4cd-56abb0a62fe0', formalized).
narrative_ontology:cs_authority_grounding('0703bb8b-a2bb-49c5-b4cd-56abb0a62fe0', extraction).
narrative_ontology:cs_interpretation_layer_present('0703bb8b-a2bb-49c5-b4cd-56abb0a62fe0').
narrative_ontology:cs_reading_relation('0703bb8b-a2bb-49c5-b4cd-56abb0a62fe0', imposition_mechanism_kernel__endogenous_climb_reading, forecloses).
narrative_ontology:cs_reading_relation('0703bb8b-a2bb-49c5-b4cd-56abb0a62fe0', imposition_mechanism_kernel__hybrid_legitimation_reading, coexists_with).
narrative_ontology:cs_axiom('0703bb8b-a2bb-49c5-b4cd-56abb0a62fe0', foundational, legitimacy_derived_from_coercive_capacity).
narrative_ontology:cs_axiom_status(legitimacy_derived_from_coercive_capacity, holdable).
narrative_ontology:cs_axiom_grounding('0703bb8b-a2bb-49c5-b4cd-56abb0a62fe0', legitimacy_derived_from_coercive_capacity, empirically_contingent).
narrative_ontology:cs_axiom('0703bb8b-a2bb-49c5-b4cd-56abb0a62fe0', foundational, compliance_conditional_on_enforcement_monitoring).
narrative_ontology:cs_axiom_status(compliance_conditional_on_enforcement_monitoring, holdable).
narrative_ontology:cs_axiom_grounding('0703bb8b-a2bb-49c5-b4cd-56abb0a62fe0', compliance_conditional_on_enforcement_monitoring, empirically_contingent).
narrative_ontology:cs_axiom('0703bb8b-a2bb-49c5-b4cd-56abb0a62fe0', secondary, pre_existing_norms_suppressed_not_displaced).
narrative_ontology:cs_axiom_status(pre_existing_norms_suppressed_not_displaced, holdable).
narrative_ontology:cs_axiom_grounding('0703bb8b-a2bb-49c5-b4cd-56abb0a62fe0', pre_existing_norms_suppressed_not_displaced, empirically_contingent).
narrative_ontology:cs_reference_frame('0703bb8b-a2bb-49c5-b4cd-56abb0a62fe0', coercive_unification_regime).
narrative_ontology:cs_drift_state('0703bb8b-a2bb-49c5-b4cd-56abb0a62fe0', generational_maturation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0703bb8b-a2bb-49c5-b4cd-56abb0a62fe0', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(imposition_mechanism_kernel__exogenous_override_reading, imposition_mechanism_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__exogenous_override_reading, state_apparatus).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__exogenous_override_reading, ruling_authority).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__exogenous_override_reading, subject_populations).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__exogenous_override_reading, cultural_resistors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__exogenous_override_reading, enforcement_apparatus).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__exogenous_override_reading, intellectual_legitimators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and enforces new behavioral norms through law, punishment, and surveillance machinery. The norms serve state consolidation, revenue extraction, or territorial control. Legitimacy is NOT derived from cultural acceptance but from the state's demonstrated capacity to punish non-compliance. Administrators of the constraint; they collect compliance through force, not persuasion.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Centralizes control and extracts resources (taxes, labor, loyalty) by replacing older distributed norms with state-mandated ones. Benefits from the substitution even as resistance remains high; the arrangement persists because exit is blocked, not because the arrangement is accepted as legitimate.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, ruling_authority, beneficiary,
    powerful, generational, arbitrage, national).

% Comply with new norms because noncompliance triggers punishment — loss of property, bodily harm, social exclusion, or death. Their previous norms are delegitimized through violent enforcement of the new ones. Exit means fleeing the territory entirely, which most cannot do. Compliance is conditional on state monitoring; it evaporates when enforcement presence is absent.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, subject_populations, payer,
    powerless, biographical, trapped, national).

% Actively contest the new norms from pre-existing cultural or religious authority. They would argue for their own legitimacy claims but are overridden by state force. Some resist openly (and face punishment); most resist covertly, maintaining older practices where enforcement cannot reach. Their voice in the conversation is actively suppressed.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, cultural_resistors, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(imposition_mechanism_kernel__exogenous_override_reading, cultural_resistors, excluded).

% Executes and perpetuates the constraint through police, military, and judicial machinery. Their institutional survival depends on the constraint's persistence; they benefit from the authority it grants them and the resources it channels to enforcement. High theater ratio: visible punishment ceremonies reinforce the constraint far beyond what direct enforcement requires.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, enforcement_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(imposition_mechanism_kernel__exogenous_override_reading, enforcement_apparatus, beneficiary).

% Clergy, philosophers, scribes who craft post-hoc legitimacy narratives for the coerced norms — framing state mandate as divine will, natural order, or cultural necessity. They benefit from patronage and authority but are constrained: their narratives must never admit the underlying violence or they lose the fiction-maintaining function. Theater is high: the entire legitimacy apparatus is performative cover for coercion.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, intellectual_legitimators, beneficiary,
    organized, biographical, constrained, national).

% Historians, anthropologists, comparative analysts who document the imposition. They see the constraint clearly: compliance is conditional on enforcement, legitimacy is absent, resistance is suppressed but continues under the surface. Their position outside the constraint allows them to report its structure.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, external_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(imposition_mechanism_kernel__exogenous_override_reading, ruling_authority).
narrative_ontology:fixing_cost_class(imposition_mechanism_kernel__exogenous_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Unifies a heterogeneous population under a single normative regime, making future state control (tax collection, military conscription, law enforcement) administratively possible. Replaces distributed, locally legitimate norms with centrally controllable ones. The coordination IS for the benefit of the state, not the population; this reading denies the population experienced it as coordination.
% TRANSFER_FUNCTION: Moves behavioral compliance, cultural authority, and material resources (through taxation enabled by norm compliance) from subject populations and cultural resistors to the state apparatus and ruling authority. The transfer is sustained by state violence, not by reciprocal benefit or mutual agreement.
% ABSENT_VOICES: Pre-existing cultural and religious authorities whose norms are being supplanted. They would articulate an alternative legitimacy claim but are excluded from the norm-setting conversation by definition — the constraint exists to silence them. Diaspora and exiled populations who rejected the new norms and left also have no voice in the internal conversation (though their absence validates the constraint's suppressive function).
% DISAPPEARANCE_RATIONALE: If state enforcement of the new norms ceased, older cultural and religious norms would re-emerge within days or weeks — populations would revert to practices suppressed only by threat of punishment. The state's administrative capacity would degrade without the unified normative regime. The constraint's disappearance would represent a collapse of the centralizing state project.
% FOUNDING_PROBLEM: Fragmented, heterogeneous population with incompatible norms and authorities; no unified tax base or military conscription possible; territorial control threatened by rival polities or internal authority competition.
% FOUNDING_PROBLEM_CORROBORATION: Historians of state formation document the pattern across multiple cases: normative unification through violence precedes both taxation infrastructure and military capacity (Weber, Poggi, Tilly on European state formation; accounts of Islamic expansion, Ottoman consolidation, Chinese imperial unification). Accounts from contemporary observers during the imposition (chronicles, diplomatic records) attest the pattern. This corroboration comes from outside the state apparatus—external and retrospective analysts, not state legitimators.
narrative_ontology:disappearance_verdict(imposition_mechanism_kernel__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(imposition_mechanism_kernel__exogenous_override_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_mechanism_kernel__exogenous_override_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(imposition_mechanism_kernel__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imposition_mechanism_kernel__exogenous_override_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imposition_mechanism_kernel__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(imposition_mechanism_kernel__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(imposition_mechanism_kernel__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82 at interval end) and rising because the constraint transfers behavioral compliance and cultural authority from distributed populations to the centralized state; this transfer is a form of extraction — populations lose autonomy and older sources of meaning. Suppression is highest of all metrics (0.89 at interval end) because the constraint's persistence depends entirely on active force: without enforcement, populations revert to prior norms. Theater is substantial (0.62 at interval end and plateauing) because legitimacy narratives — religious justification, claims of natural order, cultural inevitability — must be performed constantly to maintain the fiction that the norms are accepted rather than imposed. The accessibility-collapse metric (0.71) reflects that alternatives (the pre-existing norms) remain cognitively available to populations and are actively maintained covertly; the constraint collapses exit routes but not the awareness of what exit would mean. Resistance (0.73) is high throughout because populations and cultural resistors actively maintain older practices where enforcement cannot reach and resist openly when the cost is bearable. The measurement series models a state imposition in its first generation (0-40 time units): extractiveness and suppression rise as enforcement machinery hardens, then plateau as the system reaches steady-state oppression; theater plateaus once the legitimacy narrative is fully formed and internalized by enforcement staff. The rising slopes document the buildup of enforcement infrastructure and the solidification of coercive control.
 *
 * PERSPECTIVAL GAP:
 *   From the state apparatus and ruling authority seats, the constraint is presented as coordination—bringing order and stability to fragmented populations. From the subject population seats and cultural resistor seats, the same constraint appears as pure extraction and suppression. The engine computes divergent classifications from these structural differences: the agenda-setter (state) perceives coordination benefits and sees the constraint as rope or tangled-rope; the payer seats perceive coercion and conditional compliance, computing snare or tangled-rope from their position. The divergence is not reconcilable—it reflects the fundamental asymmetry of the imposition reading: legitimacy is contested, and that contestation is itself suppressed by force.
 *
 * DIRECTIONALITY LOGIC:
 *   The state apparatus and ruling authority are beneficiaries (d near 0.0-0.2): they collect compliance, extract resources, consolidate authority, and face no threat of exit (they command the territory). Subject populations are targets (d near 0.85-1.0): they pay behavioral compliance and cultural surrender, face harsh punishment for exit, and have no voice in the rules' design. Cultural resistors occupy a middle position (d near 0.65-0.75) because they bear costs (suppression, punishment) but retain some agency (covert resistance, exit via diaspora, theological counter-narratives). Enforcement apparatus and intellectual legitimators are secondary beneficiaries (d near 0.1-0.3): they benefit from institutional authority and patronage but are constrained by their dependence on the state's continuation—their exit options are real but costly.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint explicitly rejects the mandate-legitimacy synthesis: new norms are imposed by violence, not adopted because their merit is recognized. The founding problem (fragmented population, rival authorities) is real, but the constraint's solution (coercive unification) does not earn legitimacy through solving it. The imposition reading explicitly asserts that mandatrophy—the death of founding justification—is either present from the start (if populations never accepted the norms) or arrives quickly (once enforcement intensity makes clear the norms are imposed, not consensual). The constraint persists not because its mandate outlives its justification, but because its mandate was never grounded in acceptance; persistence is explained by enforcement capacity, not by faded legitimacy. Declaring this constraint's claimed_type as tangled_rope rather than snare reflects that a genuine coordination function exists (unification of the population enables subsequent state projects) alongside the extraction; but the tangled-rope reading is the state's frame, not an objective one. The payer seats and external observers would read it as snare (extraction with coordination cover).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_internalization_depth,
    'To what extent have subject populations internalized the imposed norms as genuinely legitimate (rather than merely complying under threat of punishment)? What is the ratio of internalized acceptance to suppression-conditional compliance?',
    'Post-withdrawal observation: if enforcement is removed, do populations retain the imposed norms (deep internalization) or revert to pre-imposition norms within a generation? Also survey/ethnographic data on whether populations describe compliance as ''moral duty'' vs. ''fear of punishment.''',
    'If internalization is deep, the constraint is moving toward tangled-rope or rope; if shallow, it is more securely snare. High internalization would suggest the imposition reading has partly transitioned toward the hybrid or climb reading over time. Low internalization confirms the exogenous override reading''s persistence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_internalization_depth, empirical, 'Depth of legitimacy internalization vs. coercion-conditional compliance').

omega_variable(
    cultural_resistor_coherence,
    'Are the pre-existing norms that resist the imposed ones still coherent and capable of re-organizing populations, or have they degraded under suppression to the point where they cannot serve as live alternatives?',
    'Documentation of covert practice, transmission to next generation, theological/intellectual defense in exile communities, capacity for rapid re-emergence when enforcement weakens (historical record of norm reversions).',
    'If pre-existing norms are coherent and transmissible, the imposition is contingent on ongoing enforcement (high suppression is permanent). If they have degraded, the imposition may be approaching durability independent of enforcement (the reading transitions toward rope or piton over generational scales).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cultural_resistor_coherence, empirical, 'Viability of suppressed pre-existing norms as alternatives to imposed norms').

omega_variable(
    coercion_mechanism_substitution,
    'As the state''s enforcement apparatus matures, does the direct coercion (visible punishment) decrease while the theater (legitimation narrative, institutional performance) increases? Or does raw coercion remain necessary throughout the constraint''s life?',
    'Measurement of enforcement-apparatus intensity and visibility over time; changes in punishment frequency, severity, and public performance; growth of administrative capacity without police presence.',
    'If coercion decreases as theater increases, the constraint is shifting toward tangled_rope (coordination function becomes partially real as legitimacy is partly internalized). If raw coercion remains high, the exogenous override reading persists and the constraint is snare, not tangled-rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_mechanism_substitution, empirical, 'Whether enforcement intensity decreases as legitimacy narratives mature').

omega_variable(
    founding_problem_persistence_independent_of_constraint,
    'Does the founding problem (fragmented authority, incompatible norms) persist as an independent structural fact, or is it an artifact of the imposition itself—i.e., does fragmentation recur only when the centralizing constraint weakens?',
    'Comparative analysis of pre-imposition authority structures (historical record, oral tradition) vs. post-withdrawal fragmentation patterns. Theoretical analysis of whether the original norms would have eventually reached compatibility without intervention.',
    'If fragmentation is independent, the constraint solves a real problem and is tangled-rope at minimum. If fragmentation is an artifact of the imposition (suppressed alternatives would have generated their own coordination once differentiation was allowed), the founding-problem justification is circular and the constraint is pure snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_problem_persistence_independent_of_constraint, conceptual, 'Whether the founding problem is an independent structural fact or an artifact of the constraint itself').

omega_variable(
    sibling_reading_empirical_signals,
    'What observable patterns would distinguish this reading (exogenous override) from the sibling endogenous_climb_reading in real historical cases? How would the measurement grid look different if populations had genuinely adopted the norms bottom-up before state codification?',
    'Contrast historical cases: imposition cases should show high early suppression with rising internalization over generations; climb cases should show low early suppression with rapid voluntary adoption. Examine the temporal order in historical records: did state enforcement precede cultural practice, or follow it?',
    'This omega clarifies the empirical boundary between readings. If the case examined shows state enforcement PRECEDING cultural practice, it is exogenous override. If cultural practice PRECEDES state codification, it is endogenous climb or hybrid.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_empirical_signals, empirical, 'Empirical signals distinguishing exogenous override from endogenous climb').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_mechanism_kernel__exogenous_override_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impo_tr_t0, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement(impo_tr_t5, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 5, 0.5).
narrative_ontology:measurement(impo_tr_t10, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 10, 0.54).
narrative_ontology:measurement(impo_tr_t15, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 15, 0.57).
narrative_ontology:measurement(impo_tr_t20, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 20, 0.59).
narrative_ontology:measurement(impo_tr_t25, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 25, 0.61).
narrative_ontology:measurement(impo_tr_t30, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 30, 0.62).
narrative_ontology:measurement(impo_tr_t35, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 35, 0.62).
narrative_ontology:measurement(impo_tr_t40, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 40, 0.62).

% Extraction over time
narrative_ontology:measurement(impo_be_t0, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(impo_be_t5, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 5, 0.71).
narrative_ontology:measurement(impo_be_t10, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 10, 0.74).
narrative_ontology:measurement(impo_be_t15, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 15, 0.76).
narrative_ontology:measurement(impo_be_t20, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 20, 0.78).
narrative_ontology:measurement(impo_be_t25, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 25, 0.8).
narrative_ontology:measurement(impo_be_t30, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 30, 0.81).
narrative_ontology:measurement(impo_be_t35, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 35, 0.82).
narrative_ontology:measurement(impo_be_t40, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 40, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(impo_su_t0, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 0, 0.81).
narrative_ontology:measurement(impo_su_t5, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 5, 0.83).
narrative_ontology:measurement(impo_su_t10, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 10, 0.85).
narrative_ontology:measurement(impo_su_t15, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 15, 0.86).
narrative_ontology:measurement(impo_su_t20, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 20, 0.87).
narrative_ontology:measurement(impo_su_t25, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 25, 0.88).
narrative_ontology:measurement(impo_su_t30, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 30, 0.88).
narrative_ontology:measurement(impo_su_t35, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 35, 0.89).
narrative_ontology:measurement(impo_su_t40, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 40, 0.89).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imposition_mechanism_kernel__exogenous_override_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(imposition_mechanism_kernel__exogenous_override_reading, 0.12).
narrative_ontology:affects_constraint(imposition_mechanism_kernel__exogenous_override_reading, imposition_mechanism_kernel__endogenous_climb_reading).
narrative_ontology:affects_constraint(imposition_mechanism_kernel__exogenous_override_reading, imposition_mechanism_kernel__hybrid_legitimation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the imposition_mechanism_kernel. The exogenous override reading instantiates the position that new norms are imposed by state violence and legitimacy is derived from monopoly on coercive capacity, not cultural acceptance. Sibling readings: endogenous_climb_reading (norms adopted bottom-up before state codification) and hybrid_legitimation_reading (norms achieved legitimacy through symbolic authority transfer + institutional incentives, neither pure coercion nor pure adoption). All three readings share the same referent (the historical phenomenon of norm change in state formation) and the same founding-problem domain, but differ in the structural explanation of HOW legitimacy is achieved. Each reading has distinct beneficiary/victim structures, distinct suppression mechanisms, and distinct epsilon values. The three readings should be linked in the consuming analysis to model the contested kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(imposition_mechanism_kernel__exogenous_override_reading, powerless, 0.88).
constraint_indexing:directionality_override(imposition_mechanism_kernel__exogenous_override_reading, moderate, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
