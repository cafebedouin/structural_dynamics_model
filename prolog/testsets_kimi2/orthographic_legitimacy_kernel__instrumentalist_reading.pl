% ============================================================================
% CONSTRAINT STORY: orthographic_legitimacy_kernel__instrumentalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_orthographic_legitimacy_kernel__instrumentalist_reading, []).

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
 *   constraint_id: orthographic_legitimacy_kernel__instrumentalist_reading
 *   human_readable: Instrumentalist Orthographic Reform (Script as Literacy/Administration Tool)
 *   domain: political_linguistics/state_formation
 *
 * SUMMARY:
 *   This is the instrumentalist reading of the orthographic legitimacy
 *   kernel: the claim that a state's choice of writing system is legitimate
 *   insofar as it maximizes literacy acquisition and administrative
 *   efficiency. The constraint is instantiated by twentieth-century
 *   state-building reforms that replaced a classical script (often Arabic or
 *   Perso-Arabic) with a Latinized or simplified orthography. The reform
 *   coordinates mass schooling and bureaucratic standardization while
 *   asymmetrically extracting from the established classical-script elite by
 *   devaluing their linguistic capital. It is structurally distinct from the
 *   continuity reading, which grounds legitimacy in historical and religious
 *   textual access, and from the modernist reading, which grounds legitimacy
 *   in civilizational rupture with the Islamic/Ottoman past.
 *
 * KEY AGENTS:
 *   - reformist_state_bureaucracy (institutional/arbitrage) â sets and enforces the new script through education and official documentation
 *   - newly_literate_citizens (powerless/constrained) â primary beneficiary of expanded access
 *   - classical_script_elite (powerful/identity_locked) â bears the cost of devalued cultural capital
 *   - language_policy_researchers (analytical/analytical) â observer seat tracking literacy metrics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(orthographic_legitimacy_kernel__instrumentalist_reading, 0.52).
domain_priors:suppression_score(orthographic_legitimacy_kernel__instrumentalist_reading, 0.6).
domain_priors:theater_ratio(orthographic_legitimacy_kernel__instrumentalist_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__instrumentalist_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__instrumentalist_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__instrumentalist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(orthographic_legitimacy_kernel__instrumentalist_reading, tangled_rope).
narrative_ontology:human_readable(orthographic_legitimacy_kernel__instrumentalist_reading, "Instrumentalist Orthographic Reform (Script as Literacy/Administration Tool)").
narrative_ontology:topic_domain(orthographic_legitimacy_kernel__instrumentalist_reading, "political_linguistics/state_formation").

domain_priors:requires_active_enforcement(orthographic_legitimacy_kernel__instrumentalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(orthographic_legitimacy_kernel__instrumentalist_reading, 'd734802e-4b3b-4fbf-8c52-5d03d17f0fa0').
narrative_ontology:cs_kernel_codification('d734802e-4b3b-4fbf-8c52-5d03d17f0fa0', formalized).
narrative_ontology:cs_authority_grounding('d734802e-4b3b-4fbf-8c52-5d03d17f0fa0', expertise).
narrative_ontology:cs_interpretation_layer_present('d734802e-4b3b-4fbf-8c52-5d03d17f0fa0').
narrative_ontology:cs_reading_relation('d734802e-4b3b-4fbf-8c52-5d03d17f0fa0', orthographic_legitimacy_kernel__continuity_reading, influences).
narrative_ontology:cs_reading_relation('d734802e-4b3b-4fbf-8c52-5d03d17f0fa0', orthographic_legitimacy_kernel__modernist_reading, coexists_with).
narrative_ontology:cs_axiom('d734802e-4b3b-4fbf-8c52-5d03d17f0fa0', foundational, literacy_maximization_mandate).
narrative_ontology:cs_axiom_status(literacy_maximization_mandate, holdable).
narrative_ontology:cs_axiom_grounding('d734802e-4b3b-4fbf-8c52-5d03d17f0fa0', literacy_maximization_mandate, instrumental).
narrative_ontology:cs_reference_frame('d734802e-4b3b-4fbf-8c52-5d03d17f0fa0', pragmatic_administrative_efficiency).
narrative_ontology:cs_drift_state('d734802e-4b3b-4fbf-8c52-5d03d17f0fa0', post_reform_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d734802e-4b3b-4fbf-8c52-5d03d17f0fa0', '').
narrative_ontology:cs_kernel_id(orthographic_legitimacy_kernel__instrumentalist_reading, orthographic_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__instrumentalist_reading, newly_literate_citizens).
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__instrumentalist_reading, reformist_state_bureaucracy).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__instrumentalist_reading, classical_script_elite).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Implements script reform through national education curricula and official documentation standards; justifies the new orthography via literacy statistics and administrative cost-benefit analyses; enforces compliance by controlling teacher training, examination systems, and state publishing; gains bureaucratic efficiency and political legitimacy from modernization metrics.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, reformist_state_bureaucracy, agenda_setter,
    institutional, generational, arbitrage, national).

% Gain access to mass literacy and state services through the simplified or Latinized script; children are educated exclusively in the new orthography; cannot opt out of the state school system or official script without exiting civic participation; experience expanded opportunity but remain dependent on the state-defined standard.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, newly_literate_citizens, beneficiary,
    powerless, generational, constrained, national).

% Bear devaluation of specialized cultural capital in the classical script; lose bureaucratic and educational gatekeeping roles as state institutions switch orthographies; face high retraining costs and status erosion; their professional and social identity is fused with the displaced writing system, making exit psychologically and economically costly.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, classical_script_elite, payer,
    powerful, biographical, identity_locked, national).

% Study literacy acquisition rates, administrative efficiency outcomes, and distributional effects of the reform; publish comparative data on script learnability and state capacity; neither collect from nor pay into the constraint; their findings may be cited by any seat.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, language_policy_researchers, observer,
    analytical, generational, analytical, national).

narrative_ontology:fixing_cost_class(orthographic_legitimacy_kernel__instrumentalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a simplified or Latinized script that reduces learning time for mass literacy acquisition and streamlines bureaucratic record-keeping, lowering the cost of state-citizen interaction and standardized education.
% TRANSFER_FUNCTION: Moves cultural capital and bureaucratic access from the classical-script-literate elite to the mass population and the state apparatus; devalues old-script expertise while creating new-script human capital aligned with the reform.
% ABSENT_VOICES: Religious scholars and traditional literary elites who anchor legitimacy in continuity with the Arabic-script heritage; they would argue that the reform severs access to historical and theological sources but are sidelined by the instrumentalist efficiency calculus and excluded from curriculum design.
% DISAPPEARANCE_RATIONALE: If the instrumentalist justification vanished and the state no longer tied legitimacy to literacy maximization and administrative efficiency, the educational system and bureaucracy would face reorganization; the classical-script elite's cultural capital would regain relative value, and the reform coalition would lose its primary pragmatic mandate.
% FOUNDING_PROBLEM: Low literacy rates and high administrative friction in a society where the classical script has a steep learning curve, limiting mass participation in state communication, education, and civic life.
% FOUNDING_PROBLEM_CORROBORATION: International development agencies and literacy NGOs from outside the state attest to low baseline literacy and administrative inefficiency. Domestic classical-script elites contest that the problem was pedagogical and institutional rather than script-based, arguing that expanded schooling could have achieved the same ends without orthographic replacement. The reformist state bureaucracy is a beneficiary-party and its self-assertion is not independent corroboration.
narrative_ontology:disappearance_verdict(orthographic_legitimacy_kernel__instrumentalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(orthographic_legitimacy_kernel__instrumentalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(orthographic_legitimacy_kernel__instrumentalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(orthographic_legitimacy_kernel__instrumentalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(orthographic_legitimacy_kernel__instrumentalist_reading, 0.52, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(orthographic_legitimacy_kernel__instrumentalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(orthographic_legitimacy_kernel__instrumentalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(orthographic_legitimacy_kernel__instrumentalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.52) because the arrangement is genuinely coordinating: it expands literacy and streamlines administration. However, the same mechanism extracts from the classical-script elite by annihilating the scarcity premium of their skills. Suppression (0.60) reflects the active enforcement required to shift the educational and bureaucratic apparatus to a new script and to marginalize the old in official contexts. Theater ratio is low-moderate (0.25) because the instrumentalist frame is pragmatic and data-oriented, though some modernization theater may attach to it. Accessibility collapse (0.65) captures how alternative script choices become impractical once the state commits its entire educational infrastructure. Resistance (0.55) reflects pushback from the displaced elite and traditionalist sectors. The measurement series run on one shared time grid so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The newly literate population experiences the reform as liberation from exclusion; the state experiences it as successful modernization; the classical elite experiences it as structural dispossession of their human capital. These divergent computed types are driven by the same structural data â directionality splits along beneficiary/victim lines.
 *
 * DIRECTIONALITY LOGIC:
 *   The reformist state bureaucracy and newly literate citizens sit near the beneficiary pole: the state gains administrative efficiency and statistical legitimacy; the masses gain access. The classical-script elite sits near the target pole: their skills are devalued, exit is identity-locked, and they bear the effective extraction. The engine derives this split from the structural declarations rather than from any tuned metric.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids snare classification because the coordination function â mass literacy and state capacity â is structurally genuine and not merely cover. It avoids rope classification because the victim group is identifiable and the extraction is asymmetric, requiring active enforcement to maintain the new script's dominance in official and educational spheres. The Tangled Rope gate captures this hybridity precisely.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary,
    'Does the instrumentalist reading represent a distinct constraint, or is it a pragmatic veneer for the modernist reading''s identity-rupture project?',
    'Historical archival analysis of state deliberations: if efficiency arguments were decisive in private planning while public rhetoric emphasized modernity, the readings are separable; if efficiency was always subordinate to rupture, the instrumentalist reading is a cover story.',
    'If subordinate to modernism, the constraint should merge with the modernist reading and epsilon would rise (more extractive, less coordination). If distinct, the instrumentalist reading stands as a separate tangled_rope with its own coordination floor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether instrumentalism is a distinct reading or a modernist cover story').

omega_variable(
    elite_cost_nature,
    'Is the cost to the classical-script elite a transitional friction or a permanent structural devaluation?',
    'Longitudinal wage, status, and employment data for classical-script experts across multiple post-reform generations.',
    'If permanent, extraction is higher than transitional estimates suggest and the constraint remains tangled_rope indefinitely; if temporary, the constraint may trend toward rope as the elite adapts or exits.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elite_cost_nature, empirical, 'Duration of elite human-capital devaluation').

omega_variable(
    literacy_causation,
    'Does the script reform actually cause higher literacy, or does literacy rise from expanded schooling regardless of script?',
    'Controlled comparison with regions or cohorts that expanded schooling without script change, isolating the orthographic treatment effect.',
    'If literacy gains are script-independent, the coordination function is weaker and the constraint shifts toward snare; if script-dependent, the coordination function is stronger and the tangled_rope classification is reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(literacy_causation, empirical, 'Whether script change itself drives literacy or merely correlates with schooling expansion').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(orthographic_legitimacy_kernel__instrumentalist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(orth_tr_t0, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(orth_tr_t8, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(orth_tr_t16, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 16, 0.25).
narrative_ontology:measurement(orth_tr_t24, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 24, 0.22).
narrative_ontology:measurement(orth_tr_t32, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 32, 0.2).
narrative_ontology:measurement(orth_tr_t40, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 40, 0.18).

% Extraction over time
narrative_ontology:measurement(orth_be_t0, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(orth_be_t8, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(orth_be_t16, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 16, 0.48).
narrative_ontology:measurement(orth_be_t24, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 24, 0.52).
narrative_ontology:measurement(orth_be_t32, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 32, 0.55).
narrative_ontology:measurement(orth_be_t40, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(orth_su_t0, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(orth_su_t8, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 8, 0.65).
narrative_ontology:measurement(orth_su_t16, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 16, 0.6).
narrative_ontology:measurement(orth_su_t24, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 24, 0.58).
narrative_ontology:measurement(orth_su_t32, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 32, 0.55).
narrative_ontology:measurement(orth_su_t40, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 40, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(orthographic_legitimacy_kernel__instrumentalist_reading, information_standard).
narrative_ontology:affects_constraint(orthographic_legitimacy_kernel__instrumentalist_reading, orthographic_legitimacy_kernel__continuity_reading).
narrative_ontology:affects_constraint(orthographic_legitimacy_kernel__instrumentalist_reading, orthographic_legitimacy_kernel__modernist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the orthographic_legitimacy_kernel. The instrumentalist reading (efficiency/literacy) is structurally distinct from the continuity reading (tradition) and the modernist reading (Western rupture). Each reading carries a different epsilon, beneficiary/victim structure, and normative grounding, and should be compiled as a separate constraint story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
