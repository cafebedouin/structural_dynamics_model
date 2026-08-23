% ============================================================================
% CONSTRAINT STORY: legitimacy_of_practice_standardization__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_practice_standardization__exogenous_override_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: legitimacy_of_practice_standardization__exogenous_override_reading
 *   human_readable: State-Decreed Practice Standardization (Exogenous Override Reading)
 *   domain: political_history/modernization_studies/institutional_change
 *
 * SUMMARY:
 *   This constraint story captures the exogenous override reading of practice
 *   standardization legitimacy: state authority decrees calendar, dress, and
 *   measurement changes for claimed collective benefits (modernization,
 *   fiscal stability, international alignment). The structural reality is
 *   abrupt legal imposition with enforcement mechanisms producing surface
 *   compliance while rural populations maintain lunar calendar and
 *   traditional practices underground for decades — a 'double life' that is a
 *   stable equilibrium, not a transitional phase. The constraint is claimed
 *   as a coordination mechanism (rope-like) but operates with substantial
 *   extraction from rural/traditional populations (snare-like), requiring
 *   active enforcement to sustain — hence tangled_rope.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_practice_standardization__exogenous_override_reading, 0.75).
domain_priors:suppression_score(legitimacy_of_practice_standardization__exogenous_override_reading, 0.8).
domain_priors:theater_ratio(legitimacy_of_practice_standardization__exogenous_override_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_practice_standardization__exogenous_override_reading, tangled_rope).
narrative_ontology:human_readable(legitimacy_of_practice_standardization__exogenous_override_reading, "State-Decreed Practice Standardization (Exogenous Override Reading)").
narrative_ontology:topic_domain(legitimacy_of_practice_standardization__exogenous_override_reading, "political_history/modernization_studies/institutional_change").

domain_priors:requires_active_enforcement(legitimacy_of_practice_standardization__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_practice_standardization__exogenous_override_reading, 'a70adeec-43b4-45a6-8163-0930f8729af0').
narrative_ontology:cs_kernel_codification('a70adeec-43b4-45a6-8163-0930f8729af0', formalized).
narrative_ontology:cs_authority_grounding('a70adeec-43b4-45a6-8163-0930f8729af0', extraction).
narrative_ontology:cs_interpretation_layer_present('a70adeec-43b4-45a6-8163-0930f8729af0').
narrative_ontology:cs_reading_relation('a70adeec-43b4-45a6-8163-0930f8729af0', legitimacy_of_practice_standardization__endogenous_displacement_reading, coexists_with).
narrative_ontology:cs_reading_relation('a70adeec-43b4-45a6-8163-0930f8729af0', legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, influences).
narrative_ontology:cs_axiom('a70adeec-43b4-45a6-8163-0930f8729af0', foundational, state_decree_sufficient_for_legitimacy).
narrative_ontology:cs_axiom_status(state_decree_sufficient_for_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('a70adeec-43b4-45a6-8163-0930f8729af0', state_decree_sufficient_for_legitimacy, conventional).
narrative_ontology:cs_axiom('a70adeec-43b4-45a6-8163-0930f8729af0', secondary, collective_benefit_justifies_override).
narrative_ontology:cs_axiom_status(collective_benefit_justifies_override, holdable).
narrative_ontology:cs_axiom_grounding('a70adeec-43b4-45a6-8163-0930f8729af0', collective_benefit_justifies_override, instrumental).
narrative_ontology:cs_reference_frame('a70adeec-43b4-45a6-8163-0930f8729af0', state_decree_legitimacy_framework).
narrative_ontology:cs_drift_state('a70adeec-43b4-45a6-8163-0930f8729af0', contemporary_post_modernization_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('a70adeec-43b4-45a6-8163-0930f8729af0', '').
narrative_ontology:cs_kernel_id(legitimacy_of_practice_standardization__exogenous_override_reading, legitimacy_of_practice_standardization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__exogenous_override_reading, state_modernizing_apparatus).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__exogenous_override_reading, international_alignment_beneficiaries).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__exogenous_override_reading, urban_elite_modernizers).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__exogenous_override_reading, rural_traditional_practitioners).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__exogenous_override_reading, lunar_calendar_communities).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__exogenous_override_reading, underground_practice_maintainers).
narrative_ontology:constraint_vindicates(legitimacy_of_practice_standardization__exogenous_override_reading, state_authority_legitimacy_doctrine).
narrative_ontology:constraint_vindicates(legitimacy_of_practice_standardization__exogenous_override_reading, modernization_collective_benefit_thesis).
narrative_ontology:constraint_vindicates(legitimacy_of_practice_standardization__exogenous_override_reading, fiscal_stability_through_standardization).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decrees calendar, dress, and measurement standardization by law; maintains enforcement bureaucracy (inspectors, courts, penalties); justifies imposition as necessary for fiscal administration, treaty compliance, and bureaucratic interoperability. Collects administrative efficiency and international legitimacy; bears cost of enforcement machinery.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, state_modernizing_apparatus, agenda_setter,
    institutional, generational, arbitrage, global).

% Face legal penalties for maintaining ancestral calendar and dress; comply superficially in market towns and government offices while continuing lunar calendar for agriculture, rituals, and kinship reckoning in villages. Bear the cognitive and social cost of 'double life'; no viable exit from state jurisdiction; underground practice persists for decades as stable equilibrium.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, rural_traditional_practitioners, payer,
    powerless, generational, trapped, local).

% Organize communal life around lunar calendar (festivals, marriages, agricultural cycles); state decree renders their temporal framework illegible to bureaucracy. Maintain parallel calendrical system; identity fused to lunar reckoning makes exit unthinkable. Some communities negotiate limited recognition for ritual purposes while accepting solar calendar for tax purposes.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, lunar_calendar_communities, payer,
    moderate, generational, identity_locked, regional).

% Adopt solar calendar and Western dress voluntarily as status markers; gain access to state bureaucracy, international commerce, and modern professions. Benefit from administrative standardization they helped design; cost of compliance is near-zero for them. Act as cultural intermediaries enforcing the new norms on rural populations.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, urban_elite_modernizers, beneficiary,
    powerful, biographical, mobile, national).

% Foreign investors, treaty partners, and international organizations gain predictable legal and commercial interface with the state. Their benefit is indirect but structural: standardized timekeeping, contract dates, and measurement reduce transaction costs. No enforcement burden; they capture the coordination surplus.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, international_alignment_beneficiaries, beneficiary,
    institutional, generational, arbitrage, global).

% Specialists (astrologers, ritual calendar keepers, traditional tailors) whose livelihood depends on the suppressed practice. Operate in semi-clandestine networks; state tolerance fluctuates between suppression and pragmatic neglect. Their expertise sustains the 'double life' equilibrium; they are both victims of extraction and keepers of the alternative system.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, underground_practice_maintainers, payer,
    powerless, biographical, identity_locked, local).

% Historian or political scientist analyzing the constraint from outside; sees the full structure of imposition, surface compliance, and persistent underground practice. Neither collects nor pays; observes that the 'double life' is not a transitional phase but a stable equilibrium maintained by the constraint's own enforcement logic.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: State standardization solves coordination problems of fiscal administration, international treaty compliance, and modern bureaucratic interoperability across diverse local practices.
% TRANSFER_FUNCTION: Moves compliance burden and cultural adaptation costs from state apparatus to rural/traditional populations; moves legitimacy and administrative efficiency to state.
% ABSENT_VOICES: Rural traditional practitioners and lunar calendar communities who would object to abrupt imposition but are structurally excluded from the decree process; their objection is expressed through persistent underground practice.
% DISAPPEARANCE_RATIONALE: If the decree vanished, rural populations would openly revert to traditional practices, state would lose administrative standardization, international alignment would fracture; the constraint actively structures the 'double life' equilibrium.
% FOUNDING_PROBLEM: The problem of administrative illegibility and fiscal instability caused by diverse local calendars, dress codes, and measurement systems under pre-modern state capacity.
% FOUNDING_PROBLEM_CORROBORATION: Historical administrative records show fiscal improvement claims; anthropological studies of rural resistance document the 'double life' as persistent equilibrium not transitional phase; no corroboration from rural populations themselves for the modernization benefit claim.
narrative_ontology:disappearance_verdict(legitimacy_of_practice_standardization__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_practice_standardization__exogenous_override_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_practice_standardization__exogenous_override_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legitimacy_of_practice_standardization__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_practice_standardization__exogenous_override_reading, 0.75, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_practice_standardization__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimacy_of_practice_standardization__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimacy_of_practice_standardization__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.75) because the compliance burden falls asymmetrically on populations who did not choose the change and maintain parallel systems at significant cost. Suppression is very high (0.8) because the constraint's persistence depends on active legal enforcement against underground practice. Theater ratio is moderate-high (0.6) because the performative surface compliance layer grows over time while the coordination function (administrative standardization) becomes partially genuine as state capacity builds. Accessibility collapse is high (0.7) because legal penalties make open alternatives nearly inaccessible, yet resistance remains high (0.75) because the underground practice is identity-fused and persists for decades. The measurement series shows initial high extraction and suppression at decree moment (T=0), gradual decline as state capacity absorbs some coordination function, then a late uptick as enforcement hardens against persistent non-compliance.
 *
 * PERSPECTIVAL GAP:
 *   From the state_modernizing_apparatus seat, the constraint is genuine coordination solving fiscal and international interoperability problems — the enforcement cost is the price of modernization. From the rural_traditional_practitioners seat, the same structure is enforced extraction imposing a 'double life' that persists for generations — the coordination benefit accrues entirely to the state and urban elites. The engine computes this seat divergence from the structural data; the authored claim (tangled_rope) reflects the analytical_observer's reading that both coordination and extraction are structurally real.
 *
 * DIRECTIONALITY LOGIC:
 *   The state_modernizing_apparatus is the agenda_setter and structural beneficiary (d near 0.0) — it sets the rules, collects administrative efficiency, and controls enforcement. Rural_traditional_practitioners and underground_practice_maintainers are full targets (d near 1.0) — they bear the costs, have trapped/identity_locked exit, and face active suppression. Lunar_calendar_communities are targets with identity_locked exit (d ~0.85) — their communal identity fuses with the suppressed practice, making exit structurally impossible. Urban_elite_modernizers and international_alignment_beneficiaries are beneficiaries with mobile/arbitrage exit (d ~0.1-0.2) — they gain coordination surplus with minimal cost. The analytical_observer sits at d=0.5 (symmetric).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (administrative illegibility under pre-modern state capacity) is contested: state claims it remains live; rural populations and historical evidence suggest it is dead (modern administrative capacity exists without imposition). The constraint persists because the state apparatus extracts legitimacy and control from maintaining the standardization decree — mandatrophy is unresolved. The 'double life' equilibrium is not transitional; it is the constraint's stable operating mode.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_extraction_boundary,
    'Is the administrative standardization function genuinely necessary for fiscal stability and international alignment, or is it a cover for state control over rural populations?',
    'Counterfactual analysis: if state capacity were built without practice imposition (e.g., bilingual/bicalendar administration), would fiscal and international outcomes differ?',
    'If coordination is genuine and necessary, the constraint is tangled_rope with irreducible coordination cost; if cover, it is snare with coordination as pure pretext.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, empirical, 'Whether the coordination function is structurally necessary or extractive pretext.').

omega_variable(
    double_life_stability,
    'Is the ''double life'' equilibrium (surface compliance + persistent underground practice) a stable attractor or a transitional phase toward full internalization?',
    'Longitudinal study of communities over 50+ years: measure rate of underground practice abandonment vs. reproduction across generations.',
    'If stable, the constraint is a persistent tangled_rope; if transitional, it may be a scaffold with very long horizon or a piton in late stage.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(double_life_stability, empirical, 'Whether the observed equilibrium is stable or transient.').

omega_variable(
    kernel_reading_disagreement_location,
    'Where exactly do the three readings of the legitimacy_of_practice_standardization kernel disagree structurally?',
    'Map each reading''s beneficiary/victim structure, enforcement requirement, and claimed coordination function; identify the minimal structural delta.',
    'Clarifies whether the readings are genuinely distinct constraints (per ε-invariance) or measurement variants of one constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, conceptual, 'Structural disagreement locus among kernel readings: exogenous override vs endogenous displacement vs dual equilibrium.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal penalties, inspectorates) or internalized (rural populations believing the new practices are superior)?',
    'Post-reform suppression trajectory: if underground practice persists after legal penalties are removed, internalized component is significant.',
    'If internalized, effective suppression is higher than structural measure; the constraint''s extraction persists even without active enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the rural populations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_practice_standardization__exogenous_override_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(legi_tr_t10, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 10, 0.5).
narrative_ontology:measurement(legi_tr_t20, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 20, 0.55).
narrative_ontology:measurement(legi_tr_t30, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 30, 0.6).
narrative_ontology:measurement(legi_tr_t40, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 40, 0.6).
narrative_ontology:measurement(legi_tr_t50, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 50, 0.6).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 0, 0.85).
narrative_ontology:measurement(legi_be_t10, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 10, 0.8).
narrative_ontology:measurement(legi_be_t20, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 20, 0.75).
narrative_ontology:measurement(legi_be_t30, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 30, 0.72).
narrative_ontology:measurement(legi_be_t40, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 40, 0.7).
narrative_ontology:measurement(legi_be_t50, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 50, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 0, 0.9).
narrative_ontology:measurement(legi_su_t10, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 10, 0.85).
narrative_ontology:measurement(legi_su_t20, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 20, 0.8).
narrative_ontology:measurement(legi_su_t30, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 30, 0.78).
narrative_ontology:measurement(legi_su_t40, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 40, 0.75).
narrative_ontology:measurement(legi_su_t50, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 50, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_practice_standardization__exogenous_override_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(legitimacy_of_practice_standardization__exogenous_override_reading, 0.1).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__exogenous_override_reading, legitimacy_of_practice_standardization__endogenous_displacement_reading).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__exogenous_override_reading, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the colloquial label 'legitimacy of practice standardization' into three structurally distinct readings with different ε values and beneficiary/victim structures. The exogenous override reading (this story) has high ε (0.75) because the standing arrangement under contest is state imposition on rural populations. The endogenous displacement reading would have lower ε (voluntary adoption). The dual equilibrium reading would have partitioned ε (high in public domain, low in private). All three share the kernel_id but instantiate different constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legitimacy_of_practice_standardization__exogenous_override_reading, moderate, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
