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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: State Coercive Norm Imposition (Exogenous Override Reading)
 *   domain: historical_sociology/state_formation
 *
 * SUMMARY:
 *   This constraint story instantiates the exogenous_override reading of the
 *   imposition_mechanism_kernel: the claim that new norms achieve legitimacy
 *   through state coercion backed by monopoly on violence, rather than
 *   through bottom-up cultural acceptance. The kernel's sibling readings
 *   (endogenous_climb, hybrid_legitimation) are treated as separate
 *   constraints. The standing arrangement under contest is the coercive
 *   imposition apparatus itself â tax collection, legal imposition,
 *   conscription, and cultural standardization â as assessed from the
 *   perspective that such coercion is the primary engine of normative order.
 *
 * KEY AGENTS:
 *   - state_elites: Primary agenda-setter (powerful/constrained) â impose norms and extract surplus
 *   - bureaucratic_apparatus: Primary beneficiary (organized/constrained) â enforces and administers
 *   - subject_populations: Primary payer (powerless/trapped) â comply under coercion
 *   - displaced_local_elites: Secondary payer (moderate/identity_locked) â lose authority to state
 *   - historical_sociologists: Analytical observer (analytical/analytical) â document the contest
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_mechanism_kernel__exogenous_override_reading, 0.78).
domain_priors:suppression_score(imposition_mechanism_kernel__exogenous_override_reading, 0.87).
domain_priors:theater_ratio(imposition_mechanism_kernel__exogenous_override_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 0.87).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_mechanism_kernel__exogenous_override_reading, tangled_rope).
narrative_ontology:human_readable(imposition_mechanism_kernel__exogenous_override_reading, "State Coercive Norm Imposition (Exogenous Override Reading)").
narrative_ontology:topic_domain(imposition_mechanism_kernel__exogenous_override_reading, "historical_sociology/state_formation").

domain_priors:requires_active_enforcement(imposition_mechanism_kernel__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_mechanism_kernel__exogenous_override_reading, '565862f8-54ab-4a0c-a4c0-6ec1a7852de6').
narrative_ontology:cs_kernel_codification('565862f8-54ab-4a0c-a4c0-6ec1a7852de6', formalized).
narrative_ontology:cs_authority_grounding('565862f8-54ab-4a0c-a4c0-6ec1a7852de6', extraction).
narrative_ontology:cs_interpretation_layer_present('565862f8-54ab-4a0c-a4c0-6ec1a7852de6').
narrative_ontology:cs_reading_relation('565862f8-54ab-4a0c-a4c0-6ec1a7852de6', imposition_mechanism_kernel__endogenous_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('565862f8-54ab-4a0c-a4c0-6ec1a7852de6', imposition_mechanism_kernel__hybrid_legitimation_reading, influences).
narrative_ontology:cs_axiom('565862f8-54ab-4a0c-a4c0-6ec1a7852de6', foundational, coercive_imposition_suffices_for_legitimacy).
narrative_ontology:cs_axiom_status(coercive_imposition_suffices_for_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('565862f8-54ab-4a0c-a4c0-6ec1a7852de6', coercive_imposition_suffices_for_legitimacy, empirically_contingent).
narrative_ontology:cs_axiom('565862f8-54ab-4a0c-a4c0-6ec1a7852de6', foundational, state_mandate_precedes_popular_acceptance).
narrative_ontology:cs_axiom_status(state_mandate_precedes_popular_acceptance, holdable).
narrative_ontology:cs_axiom_grounding('565862f8-54ab-4a0c-a4c0-6ec1a7852de6', state_mandate_precedes_popular_acceptance, conventional).
narrative_ontology:cs_reference_frame('565862f8-54ab-4a0c-a4c0-6ec1a7852de6', monopoly_violence_authority).
narrative_ontology:cs_drift_state('565862f8-54ab-4a0c-a4c0-6ec1a7852de6', post_weberian_critique, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('565862f8-54ab-4a0c-a4c0-6ec1a7852de6', '').
narrative_ontology:cs_kernel_id(imposition_mechanism_kernel__exogenous_override_reading, imposition_mechanism_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__exogenous_override_reading, state_elites).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__exogenous_override_reading, bureaucratic_apparatus).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__exogenous_override_reading, subject_populations).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__exogenous_override_reading, displaced_local_elites).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Impose new norms through decrees, legal codes, and enforcement apparatus. Derive legitimacy from monopoly on violence rather than popular acceptance. Must maintain coercive capacity and monitoring infrastructure or face fragmentation and loss of authority.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, state_elites, agenda_setter,
    powerful, generational, constrained, national).

% Administer and enforce centrally imposed norms, collecting salaries, positions, and institutional survival from the enforcement structure. Their occupational existence depends on the persistence of top-down imposition.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, bureaucratic_apparatus, beneficiary,
    organized, biographical, constrained, national).

% Comply with new norms under explicit or implicit threat of state violence. Bear the costs of taxation, conscription, cultural suppression, and loss of local customary practice. Geographic and economic exit is blocked by border controls and land tenure systems.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, subject_populations, payer,
    powerless, immediate, trapped, national).

% Lose traditional authority and norm-setting power to the central state. Their pre-state legitimacy derived from local cultural acceptance is overridden by coercive state mandate. Identity is fused with the displaced customary order, making adaptation costly and resistance identity-defining.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, displaced_local_elites, payer,
    moderate, biographical, identity_locked, regional).

% Observe the contest between coercive and cultural legitimation mechanisms across state-formation cases. Document enforcement costs, resistance patterns, and the divergence between state and subject narratives of the same imposition events.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, historical_sociologists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(imposition_mechanism_kernel__exogenous_override_reading, state_elites).
narrative_ontology:fixing_cost_class(imposition_mechanism_kernel__exogenous_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes uniform behavioral norms across a territory by centralizing rule-making and eliminating competing local authorities, enabling large-scale resource extraction and military coordination.
% TRANSFER_FUNCTION: Moves compliance, tax revenue, labor (conscription), and cultural deference from subject populations and local elites to the central state apparatus and its elite beneficiaries; moves normative authority from local customary sources to state decrees.
% ABSENT_VOICES: Subject populations experiencing everyday coercion, indigenous norm-keepers, and local elders whose authority is displaced are structurally excluded from the legitimation discourse; their objections are recorded only in resistance movements or fugitive practices.
% DISAPPEARANCE_RATIONALE: If state coercive imposition vanished, local normative orders would resurface, tax and conscription flows would halt, territorial fragmentation would reassert, and the central state's extractive capacity would collapse â the social order would rearrange around regional and customary authorities.
% FOUNDING_PROBLEM: Political fragmentation and the absence of a unified coercive authority made large-scale public goods (defense, law enforcement, infrastructure) impossible and allowed local strongmen to dominate.
% FOUNDING_PROBLEM_CORROBORATION: State chroniclers and later nationalist historiography attest to the fragmentation problem. However, subject populations and displaced local elites attest that the specific coercive solution created new problems worse than fragmentation; anthropological and subaltern studies from outside the state-beneficiary camp corroborate the high costs of imposition.
narrative_ontology:disappearance_verdict(imposition_mechanism_kernel__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(imposition_mechanism_kernel__exogenous_override_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_mechanism_kernel__exogenous_override_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(imposition_mechanism_kernel__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imposition_mechanism_kernel__exogenous_override_reading, 0.78, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is high (0.78) because the state extracts revenue, labor, and deference through centrally imposed norms; suppression is higher still (0.87) because compliance is conditional on continuous monitoring and active neutralization of resistance. Theater_ratio rises from 0.20 to 0.55 as enforcement becomes increasingly performative â public displays of monopoly violence substitute for functional integration. Accessibility_collapse is high (0.72) because state norms crowd out local customary alternatives once the apparatus is understood. Resistance is moderate (0.60) because subject populations and displaced elites resist but are systematically overpowered. The measurement series share a single time grid to prevent misaligned substitution artifacts.
 *
 * PERSPECTIVAL GAP:
 *   The state elite seat experiences the constraint as necessary coordination â territorial integration, resource mobilization, and order maintenance. The subject population seat experiences the identical structure as extractive domination. The engine computes this divergence from the structural data (beneficiary/victim declarations, exit options, power levels); the authored claim does not adjudicate the divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   State elites and the bureaucratic apparatus are structural beneficiaries: they collect revenue, authority, and positional rents from the imposition mechanism, yielding directionality near the beneficiary end (low d). Subject populations and displaced local elites are structural victims: they bear the costs of extraction, suppression, and identity displacement, with near-total exit failure (trapped and identity_locked), yielding directionality near the target end (high d). The national spatial scope amplifies effective extraction for the trapped victim seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â political fragmentation and local strongman rule â may have been genuinely severe. However, the R5 genealogy flags a potential mandatrophy: the founding_problem_status is contested, and if the state's coercive apparatus outlives the fragmentation threat, the constraint persists as extraction rather than coordination. The theater_ratio trajectory supports this concern, showing rising performative maintenance relative to functional delivery. A founding_problem_status of contested paired with a world_rearranges disappearance_verdict indicates the arrangement is still load-bearing for some parties even if its original justification is disputed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_source_ambiguity,
    'Does sustained compliance with state-imposed norms reflect internalized legitimation or merely the continuous presence of coercive threat?',
    'Comparative analysis of compliance persistence during periods of state capacity collapse; if compliance evaporates when enforcement falters, the mechanism is structural coercion.',
    'If internalized, the constraint''s effective extraction is lower than measured and directionality shifts toward symmetric; if purely structural, extraction remains high and the exogenous_override reading is vindicated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_source_ambiguity, empirical, 'Whether compliance is driven by internalized belief or ongoing coercion').

omega_variable(
    coercion_coordination_separability,
    'Can the state''s genuine coordination function (territorial integration, dispute resolution) be disentangled from its extractive coercion?',
    'Historical comparison with stateless societies and with states that transitioned to consent-based governance; if coordination persists after coercive extraction is removed, the functions are separable.',
    'If inseparable, the constraint is a more entrenched tangled_rope; if separable, the coercive component is a snare riding on coordination cover.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coercion_coordination_separability, conceptual, 'Whether coordination and extraction are structurally separable in state imposition').

omega_variable(
    kernel_contest_boundary,
    'Does the exogenous_override reading foreclose the endogenous_climb reading, or do they describe different phases of a single legitimation process?',
    'Genealogical analysis of specific norm adoptions: determining whether state imposition ever produces bottom-up acceptance, or whether acceptance and coercion remain distinct causal pathways.',
    'If endogenous acceptance never follows exogenous imposition, the readings are rival ontologies; if acceptance routinely follows coercion, the kernel may require a sequential or hybrid reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_contest_boundary, conceptual, 'Relationship between coercion and climb readings of the kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_mechanism_kernel__exogenous_override_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impo_tr_t0, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(impo_tr_t10, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(impo_tr_t20, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(impo_tr_t30, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 30, 0.48).
narrative_ontology:measurement(impo_tr_t40, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 40, 0.52).
narrative_ontology:measurement(impo_tr_t50, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 50, 0.55).

% Extraction over time
narrative_ontology:measurement(impo_be_t0, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(impo_be_t10, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(impo_be_t20, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 20, 0.72).
narrative_ontology:measurement(impo_be_t30, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 30, 0.75).
narrative_ontology:measurement(impo_be_t40, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 40, 0.77).
narrative_ontology:measurement(impo_be_t50, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 50, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(impo_su_t0, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement(impo_su_t10, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 10, 0.78).
narrative_ontology:measurement(impo_su_t20, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 20, 0.82).
narrative_ontology:measurement(impo_su_t30, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 30, 0.85).
narrative_ontology:measurement(impo_su_t40, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 40, 0.86).
narrative_ontology:measurement(impo_su_t50, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 50, 0.87).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imposition_mechanism_kernel__exogenous_override_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
