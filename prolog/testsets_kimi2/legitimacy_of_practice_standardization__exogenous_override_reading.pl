% ============================================================================
% CONSTRAINT STORY: legitimacy_of_practice_standardization__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-11
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   human_readable: State Exogenous Practice Standardization Legitimacy
 *   domain: political/modernization
 *
 * SUMMARY:
 *   This is the exogenous_override_reading of the
 *   legitimacy_of_practice_standardization kernel. It treats state decree as
 *   sufficient and necessary for legitimate practice change, producing abrupt
 *   legal imposition of calendar and dress standards enforced by
 *   administrative penalties. The expected structural signature is surface
 *   compliance masking persistent underground practice: a stable 'double
 *   life' equilibrium rather than a transitional phase, with rural
 *   populations maintaining lunar calendars and traditional dress for
 *   decades. Sibling readings include endogenous_displacement_reading
 *   (legitimacy from voluntary adoption) and
 *   dual_practice_equilibrium_reading (domain-partitioned authority). This
 *   reading is decomposed from the colloquial label because its epsilon and
 *   enforcement profile differ structurally from the siblings.
 *
 * KEY AGENTS:
 *   - state_modernizers: Primary agenda-setter and beneficiary (institutional/arbitrage) â decrees and enforces standardization, collects sovereign authority.
 *   - urban_administrative_class: Primary beneficiary (organized/constrained) â gains administrative friction reduction.
 *   - rural_populations: Primary payer (powerless/trapped) â bears compliance costs and cultural disruption, maintains underground practice.
 *   - traditional_communities: Secondary payer (moderate/identity_locked) â ritual identity fused with suppressed traditional practice.
 *   - religious_authorities: Excluded seat (moderate/constrained) â previously arbitrated temporal and sartorial norms, now superseded.
 *   - modernization_analysts: Analytical observer (analytical/analytical) â measures the compliance-theater gap.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_practice_standardization__exogenous_override_reading, 0.62).
domain_priors:suppression_score(legitimacy_of_practice_standardization__exogenous_override_reading, 0.75).
domain_priors:theater_ratio(legitimacy_of_practice_standardization__exogenous_override_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_practice_standardization__exogenous_override_reading, tangled_rope).
narrative_ontology:human_readable(legitimacy_of_practice_standardization__exogenous_override_reading, "State Exogenous Practice Standardization Legitimacy").
narrative_ontology:topic_domain(legitimacy_of_practice_standardization__exogenous_override_reading, "political/modernization").

domain_priors:requires_active_enforcement(legitimacy_of_practice_standardization__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_practice_standardization__exogenous_override_reading, '01cddbb9-6bc5-4c7f-9808-67e8a1a15ca9').
narrative_ontology:cs_kernel_codification('01cddbb9-6bc5-4c7f-9808-67e8a1a15ca9', formalized).
narrative_ontology:cs_authority_grounding('01cddbb9-6bc5-4c7f-9808-67e8a1a15ca9', extraction).
narrative_ontology:cs_interpretation_layer_present('01cddbb9-6bc5-4c7f-9808-67e8a1a15ca9').
narrative_ontology:cs_reading_relation('01cddbb9-6bc5-4c7f-9808-67e8a1a15ca9', legitimacy_of_practice_standardization__endogenous_displacement_reading, influences).
narrative_ontology:cs_reading_relation('01cddbb9-6bc5-4c7f-9808-67e8a1a15ca9', legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, influences).
narrative_ontology:cs_axiom('01cddbb9-6bc5-4c7f-9808-67e8a1a15ca9', foundational, state_decree_confers_practice_legitimacy).
narrative_ontology:cs_axiom_status(state_decree_confers_practice_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('01cddbb9-6bc5-4c7f-9808-67e8a1a15ca9', state_decree_confers_practice_legitimacy, conventional).
narrative_ontology:cs_axiom('01cddbb9-6bc5-4c7f-9808-67e8a1a15ca9', foundational, collective_modernization_outweighs_local_custom).
narrative_ontology:cs_axiom_status(collective_modernization_outweighs_local_custom, holdable).
narrative_ontology:cs_axiom_grounding('01cddbb9-6bc5-4c7f-9808-67e8a1a15ca9', collective_modernization_outweighs_local_custom, instrumental).
narrative_ontology:cs_reference_frame('01cddbb9-6bc5-4c7f-9808-67e8a1a15ca9', centralized_modernizing_state).
narrative_ontology:cs_drift_state('01cddbb9-6bc5-4c7f-9808-67e8a1a15ca9', post_reform_stabilization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('01cddbb9-6bc5-4c7f-9808-67e8a1a15ca9', '').
narrative_ontology:cs_kernel_id(legitimacy_of_practice_standardization__exogenous_override_reading, legitimacy_of_practice_standardization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__exogenous_override_reading, state_modernizers).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__exogenous_override_reading, urban_administrative_class).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__exogenous_override_reading, rural_populations).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__exogenous_override_reading, traditional_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decree calendar and dress reforms through legal codes, enforce compliance via administrative penalties and education campaigns, and justify the constraints as modernization, fiscal stability, and international alignment. Collect consolidated sovereign authority and diplomatic recognition.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, state_modernizers, agenda_setter,
    institutional, generational, arbitrage, national).

% Operates within unified calendars and standardized dress codes that streamline tax collection, scheduling, and professional coordination. Benefits from reduced friction but does not control the rules; bears minor conformity costs.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, urban_administrative_class, beneficiary,
    organized, biographical, constrained, national).

% Legally required to adopt the state calendar and dress for official interactions but maintain traditional lunar calendars and garments in private. Bear fines, scheduling confusion, and cultural discontinuity; geographic and economic immobility prevents leaving the enforcement zone.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, rural_populations, payer,
    powerless, biographical, trapped, regional).

% Organize ritual life around traditional temporal and sartorial systems. State imposition disrupts communal coherence and inter-generational transmission. Ritual identity is fused with traditional practice, making open abandonment psychologically and socially costly even when legal penalties are absent.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, traditional_communities, payer,
    moderate, generational, identity_locked, local).

% Previously arbitrated ritual time, seasonal observance, and dress norms. State decrees have superseded this arbitration role. Would argue for the legitimacy of traditional temporal authority but are excluded from policy design and formal negotiations.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, religious_authorities, excluded,
    moderate, biographical, constrained, regional).

% Document the gap between official compliance statistics and persistent underground practice. Analyze whether standardization produces genuine administrative integration or performative conformity with stable dual-life equilibria.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, modernization_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimacy_of_practice_standardization__exogenous_override_reading, state_modernizers).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: State-led unification of calendars, dress codes, and administrative practices to enable centralized governance, fiscal regularity, and international diplomatic recognition.
% TRANSFER_FUNCTION: Moves temporal and sartorial autonomy from rural and traditional communities to the state administrative apparatus; moves legitimacy and enforcement capacity from religious and local authorities to state modernizers.
% ABSENT_VOICES: Religious authorities who previously governed ritual time and dress are excluded from policy design; rural populations are consulted only nominally if at all; traditional elites lose arbitration role.
% DISAPPEARANCE_RATIONALE: If the state's override authority vanished, official administrative standards would lose enforcement backing; rural populations would revert openly to traditional practice, urban administration would face coordination friction, and the state's international alignment narrative would weaken.
% FOUNDING_PROBLEM: Fragmented calendars and dress standards impeded centralized tax collection, military mobilization, and diplomatic coordination with Western powers.
% FOUNDING_PROBLEM_CORROBORATION: State modernizers and urban administrators claim the problem remains live for global integration. Rural populations and social historians attest that traditional systems functioned adequately for local coordination and that centralization was partly manufactured; independent ethnographic records from outside the beneficiary set support the shifted-function reading.
narrative_ontology:disappearance_verdict(legitimacy_of_practice_standardization__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_practice_standardization__exogenous_override_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_practice_standardization__exogenous_override_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legitimacy_of_practice_standardization__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_practice_standardization__exogenous_override_reading, 0.62, 'kimi-k2.6', 'none', direct).

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
 *   Base extractiveness (0.62) is substantial because the constraint imposes non-trivial cultural and administrative costs on rural and traditional payers while the coordination benefit (unified administration) is real but asymmetrically distributed. Suppression (0.75) is high because persistence depends on legal penalties, monitoring, and education campaigns rather than spontaneous adoption. Theater ratio (0.55) is elevated because the 'double life' equilibrium means a significant share of observed compliance is performative â surface adherence that masks continued traditional practice. Accessibility collapse (0.45) is moderate: legal alternatives are barred but practical alternatives persist underground. Resistance (0.6) reflects chronic underground persistence as passive resistance. The measurement series share one time grid so temporal analysis is aligned.
 *
 * PERSPECTIVAL GAP:
 *   The state_modernizer seat computes the constraint as necessary coordination for national integration and global legitimacy. The rural_population and traditional_community seats compute it as imposed cultural extraction backed by enforcement. The urban_administrative_class sits nearer symmetric: genuine daily benefit from standardization, but limited ability to alter the rules. The engine derives this divergence from beneficiary declarations, victim declarations, and exit options rather than from any authored classification.
 *
 * DIRECTIONALITY LOGIC:
 *   State_modernizers are beneficiaries with arbitrage-grade exit (they can revise the legal code) and therefore derive low directionality toward the target pole. Urban_administrative_class are beneficiaries with constrained exit, sitting slightly higher but still on the beneficiary side. Rural_populations are declared victims with trapped exit and low power, producing directionality near the full-target pole. Traditional_communities are declared victims with identity_locked exit, producing similarly high directionality. Religious_authorities are excluded and bear no extracted flow directly; their exclusion is structural but not captured in the chi computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â fragmented administration impeding centralized state functions â was genuine, but its resolution has partly given way to authority consolidation and international signaling. The constraint persists beyond strict functional necessity because it vindicates state sovereignty and modernizing legitimacy. It is not yet a piton because the coordination function (unified tax and military administration) remains partially real, but the rising theater ratio indicates growing performative maintenance. The R5 genealogy interview captures this tension: founding_problem_status is contested, corroborated by voices outside the beneficiary set.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_contest_endogenous_alternative,
    'If practice legitimacy were instead grounded in voluntary endogenous adoption, would the same state-imposed standards have emerged, or does the exogenous reading foreclose the necessary conditions for authentic cultural change?',
    'Comparative historical analysis of modernization campaigns that succeeded through diffusion versus imposition, measuring persistence and internalization rates across generations.',
    'If endogenous adoption produces more durable legitimacy, this constraint''s extraction component dominates its coordination component and the classification shifts toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_contest_endogenous_alternative, conceptual, 'Whether exogenous decree is compatible with endogenous legitimacy').

omega_variable(
    domain_partition_feasibility,
    'Could state authority govern public/administrative practice while traditional authority governs private/ritual practice without undermining the coordination function?',
    'Comparative legal pluralism studies examining whether dual-calendar or dual-dress systems achieve comparable administrative efficiency.',
    'If domain partition is feasible, the constraint''s totalizing imposition is extractive overreach rather than coordination necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domain_partition_feasibility, empirical, 'Whether public/private domain partition satisfies the coordination need').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal penalties, enforcement patrols) or internalized (self-policing of public behavior to avoid stigma or official sanction)?',
    'Post-regime-change observational studies: if traditional practice resurges immediately in public, suppression was structural; if it stays hidden, internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests â the target carries the suppression with them after formal enforcement lapses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_practice_standardization__exogenous_override_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(legi_tr_t8, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 8, 0.38).
narrative_ontology:measurement(legi_tr_t16, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 16, 0.45).
narrative_ontology:measurement(legi_tr_t24, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 24, 0.5).
narrative_ontology:measurement(legi_tr_t32, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 32, 0.53).
narrative_ontology:measurement(legi_tr_t40, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 40, 0.55).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(legi_be_t8, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 8, 0.55).
narrative_ontology:measurement(legi_be_t16, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(legi_be_t24, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 24, 0.6).
narrative_ontology:measurement(legi_be_t32, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 32, 0.61).
narrative_ontology:measurement(legi_be_t40, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(legi_su_t8, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 8, 0.65).
narrative_ontology:measurement(legi_su_t16, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 16, 0.7).
narrative_ontology:measurement(legi_su_t24, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 24, 0.73).
narrative_ontology:measurement(legi_su_t32, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 32, 0.74).
narrative_ontology:measurement(legi_su_t40, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 40, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_practice_standardization__exogenous_override_reading, identity_coordination).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__exogenous_override_reading, endogenous_displacement_reading).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__exogenous_override_reading, dual_practice_equilibrium_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the legitimacy_of_practice_standardization kernel, instantiating the exogenous_override position. It is decomposed from the colloquial label because its epsilon (substantial extraction through active enforcement) differs structurally from the endogenous_displacement reading (lower extraction, voluntary adoption) and the dual_practice_equilibrium reading (domain-partitioned legitimacy with minimal enforcement).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
