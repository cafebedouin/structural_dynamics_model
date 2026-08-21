% ============================================================================
% CONSTRAINT STORY: imposition_mechanism_kernel__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   This constraint describes the imposition of new social norms by a central
 *   state through coercive means, where the legitimacy of these norms is
 *   derived primarily from the state's monopoly on violence rather than from
 *   pre-existing cultural acceptance. This is the 'exogenous override'
 *   reading of the imposition mechanism kernel, emphasizing top-down
 *   coercion. The metrics reflect high extraction and suppression, consistent
 *   with a Snare, as the state actively overrides existing social structures.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_mechanism_kernel__exogenous_override_reading, 0.85).
domain_priors:suppression_score(imposition_mechanism_kernel__exogenous_override_reading, 0.92).
domain_priors:theater_ratio(imposition_mechanism_kernel__exogenous_override_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_mechanism_kernel__exogenous_override_reading, snare).
narrative_ontology:human_readable(imposition_mechanism_kernel__exogenous_override_reading, "State-Coerced Norm Imposition (Exogenous Override Reading)").
narrative_ontology:topic_domain(imposition_mechanism_kernel__exogenous_override_reading, "historical_sociology/state_formation/cultural_authority").

domain_priors:requires_active_enforcement(imposition_mechanism_kernel__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_mechanism_kernel__exogenous_override_reading, '8b4e871d-f20d-4ead-a3d4-d42527835cd4').
narrative_ontology:cs_kernel_codification('8b4e871d-f20d-4ead-a3d4-d42527835cd4', formalized).
narrative_ontology:cs_authority_grounding('8b4e871d-f20d-4ead-a3d4-d42527835cd4', extraction).
narrative_ontology:cs_reading_relation('8b4e871d-f20d-4ead-a3d4-d42527835cd4', imposition_mechanism_kernel__endogenous_climb_reading, forecloses).
narrative_ontology:cs_reading_relation('8b4e871d-f20d-4ead-a3d4-d42527835cd4', imposition_mechanism_kernel__hybrid_legitimation_reading, influences).
narrative_ontology:cs_axiom('8b4e871d-f20d-4ead-a3d4-d42527835cd4', foundational, legitimacy_from_coercion).
narrative_ontology:cs_axiom_status(legitimacy_from_coercion, holdable).
narrative_ontology:cs_axiom_grounding('8b4e871d-f20d-4ead-a3d4-d42527835cd4', legitimacy_from_coercion, conventional).
narrative_ontology:cs_axiom('8b4e871d-f20d-4ead-a3d4-d42527835cd4', foundational, state_as_sole_norm_giver).
narrative_ontology:cs_axiom_status(state_as_sole_norm_giver, holdable).
narrative_ontology:cs_axiom_grounding('8b4e871d-f20d-4ead-a3d4-d42527835cd4', state_as_sole_norm_giver, instrumental).
narrative_ontology:cs_reference_frame('8b4e871d-f20d-4ead-a3d4-d42527835cd4', state_monopoly_on_violence).
narrative_ontology:cs_drift_state('8b4e871d-f20d-4ead-a3d4-d42527835cd4', contemporary_postcolonial_critique, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('8b4e871d-f20d-4ead-a3d4-d42527835cd4', '').
narrative_ontology:cs_kernel_id(imposition_mechanism_kernel__exogenous_override_reading, imposition_mechanism_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__exogenous_override_reading, state_apparatus).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__exogenous_override_reading, ruling_elite).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__exogenous_override_reading, local_communities).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__exogenous_override_reading, traditional_elites).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__exogenous_override_reading, subaltern_groups).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The central authority that promulgates new norms, enforces them through its monopoly on violence (military, police, judiciary), and benefits from the consolidation of its power and the standardization of social behavior. It actively suppresses dissent and alternative normative systems.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% The political and economic beneficiaries of the new norms, often aligned with the state apparatus. They gain from the stability, predictability, and new opportunities created by the imposed order, even if the norms lack deep cultural roots among the populace.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, ruling_elite, beneficiary,
    powerful, biographical, mobile, national).

% Bear the direct costs of compliance with norms that often contradict their established customs, traditions, and social structures. They face coercion, fines, imprisonment, or violence for non-compliance, with little to no voice in the norm-setting process. Their options are resistance or grudging compliance.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, local_communities, payer,
    powerless, generational, trapped, local).

% Leaders of pre-existing social structures (e.g., tribal chiefs, religious authorities) whose authority is undermined or directly challenged by the new state-imposed norms. They lose status, influence, and often economic resources, and may engage in covert or overt resistance.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, traditional_elites, payer,
    moderate, biographical, constrained, regional).

% Marginalized populations who are particularly vulnerable to the enforcement of new norms, often experiencing disproportionate punishment due to their lack of resources, social capital, and historical disadvantage. Their identity is often tied to pre-existing cultural practices that are now criminalized or devalued.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, subaltern_groups, payer,
    powerless, immediate, identity_locked, local).

% Scholars who study the historical processes of state formation and norm imposition, analyzing primary sources and archaeological evidence to reconstruct the mechanisms of legitimation and coercion. They seek to understand the long-term impacts and contested narratives of such processes.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, historical_analysts, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(imposition_mechanism_kernel__exogenous_override_reading, state_apparatus).
narrative_ontology:fixing_cost_class(imposition_mechanism_kernel__exogenous_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a uniform legal and social order across a diverse territory, enabling centralized governance, taxation, and resource mobilization that would be impossible under fragmented traditional norms.
% TRANSFER_FUNCTION: Transfers social and cultural authority from local, traditional institutions to the centralized state, along with resources (taxes, labor) and compliance with state-defined behaviors, from local communities and traditional elites to the state apparatus and ruling elite.
% ABSENT_VOICES: The voices of local communities and subaltern groups, whose traditional norms are being overridden, are largely absent from the formal legitimation discourse. Their objections are expressed through resistance, non-compliance, or cultural preservation efforts, rather than through recognized channels.
% DISAPPEARANCE_RATIONALE: If the state's coercive power and its imposed norms vanished, the centralized order would collapse, leading to a resurgence of local and traditional normative systems, potential fragmentation of the territory, and a re-negotiation of social hierarchies. The state's ability to extract resources and enforce its will would cease.
% FOUNDING_PROBLEM: The problem of governing a diverse and often resistant population, integrating disparate territories, and consolidating state power in the face of competing local authorities and normative systems.
% FOUNDING_PROBLEM_CORROBORATION: The state apparatus and ruling elite attest that the problem of maintaining order and central authority is perpetually live, justifying ongoing enforcement. Historical analysts, from outside the benefiting parties, corroborate that the problem of integrating diverse populations was indeed a central challenge for nascent states, though they may dispute the necessity or legitimacy of the coercive methods employed.
narrative_ontology:disappearance_verdict(imposition_mechanism_kernel__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(imposition_mechanism_kernel__exogenous_override_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_mechanism_kernel__exogenous_override_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(imposition_mechanism_kernel__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imposition_mechanism_kernel__exogenous_override_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high because the state re-directs resources and labor from local communities to its own projects, and imposes behaviors that serve its interests. Suppression is very high because the state actively uses force (military, police, legal sanctions) to compel compliance and eliminate alternative normative systems. Theater ratio is low because the state's enforcement is direct and overt, with little need for performative legitimation beyond the display of power itself. Accessibility collapse is moderate-high as alternatives are actively suppressed but not entirely eradicated, leading to ongoing resistance. Resistance is high, reflecting the continuous struggle against the imposed order.
 *
 * PERSPECTIVAL GAP:
 *   From the state's perspective, this is a necessary coordination mechanism for governance and development. From the perspective of local communities, it is a coercive imposition that extracts resources and undermines their way of life. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The state apparatus and ruling elite are clear beneficiaries, gaining power, resources, and control. Local communities, traditional elites, and subaltern groups are victims, bearing the costs of compliance, loss of autonomy, and cultural disruption. The directionality for victims is high, reflecting their lack of exit and the direct extraction they face.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling coercion as coordination by emphasizing the active enforcement and suppression required to maintain the norms. The high extractiveness and suppression, coupled with ongoing resistance, clearly indicate a Snare, not a Rope or Scaffold, as the coordination story is a cover for power consolidation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_source_ambiguity,
    'Is the observed compliance primarily due to state coercion, or is there an underlying, unacknowledged process of cultural acceptance or instrumental adoption at play?',
    'Longitudinal ethnographic studies of local communities post-imposition, analyzing shifts in internalized norms versus overt compliance, and the persistence of norms after periods of state weakness or withdrawal.',
    'If significant cultural acceptance is found, the constraint might be reclassified as a Tangled Rope or even a Rope over time, as the ''exogenous override'' framing would be incomplete. If compliance remains purely coercive, the Snare classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_source_ambiguity, empirical, 'Ambiguity regarding the true source of norm legitimacy: coercion vs. acceptance.').

omega_variable(
    resistance_measurement_bias,
    'Does the measurement of ''resistance'' accurately capture covert, passive, or cultural forms of resistance, or is it biased towards overt, confrontational acts?',
    'Development of new historical and sociological methodologies for detecting and quantifying ''everyday forms of resistance'' (e.g., foot-dragging, sabotage, cultural preservation) that do not appear in official records of rebellion.',
    'If covert resistance is significantly higher than currently measured, the effective suppression might be lower than perceived, or the state''s control less absolute, potentially shifting the classification towards a more contested Snare or even a Tangled Rope if some coordination function is revealed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(resistance_measurement_bias, conceptual, 'Bias in measuring resistance, potentially underestimating non-overt forms.').

omega_variable(
    kernel_framing_choice,
    'Is the ''exogenous override'' reading the most appropriate framing for this historical process, or would an ''endogenous climb'' or ''hybrid legitimation'' reading better capture the dynamics?',
    'Comparative historical analysis across multiple case studies of state formation, evaluating which reading''s explanatory power best accounts for the observed patterns of norm adoption and state-society relations.',
    'Adopting an ''endogenous climb'' reading would drastically lower extractiveness and suppression, likely reclassifying the constraint as a Rope. A ''hybrid legitimation'' reading would suggest a Tangled Rope, acknowledging both coercive and consensual elements. This omega highlights the conceptual choice inherent in interpreting historical processes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_choice, conceptual, 'The choice of kernel reading (exogenous override vs. endogenous climb vs. hybrid legitimation) fundamentally alters the constraint''s classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_mechanism_kernel__exogenous_override_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impo_tr_t0, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(impo_tr_t20, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(impo_tr_t40, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 40, 0.15).
narrative_ontology:measurement(impo_tr_t60, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 60, 0.12).
narrative_ontology:measurement(impo_tr_t80, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 80, 0.13).
narrative_ontology:measurement(impo_tr_t100, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(impo_be_t0, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(impo_be_t20, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 20, 0.8).
narrative_ontology:measurement(impo_be_t40, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 40, 0.85).
narrative_ontology:measurement(impo_be_t60, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 60, 0.88).
narrative_ontology:measurement(impo_be_t80, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 80, 0.87).
narrative_ontology:measurement(impo_be_t100, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 100, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(impo_su_t0, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(impo_su_t20, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 20, 0.85).
narrative_ontology:measurement(impo_su_t40, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 40, 0.9).
narrative_ontology:measurement(impo_su_t60, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 60, 0.95).
narrative_ontology:measurement(impo_su_t80, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 80, 0.93).
narrative_ontology:measurement(impo_su_t100, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 100, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
