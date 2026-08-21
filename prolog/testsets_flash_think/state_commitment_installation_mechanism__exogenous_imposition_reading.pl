% ============================================================================
% CONSTRAINT STORY: state_commitment_installation_mechanism__exogenous_imposition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_commitment_installation_mechanism__exogenous_imposition_reading, []).

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
 *   constraint_id: state_commitment_installation_mechanism__exogenous_imposition_reading
 *   human_readable: State-Imposed Commitment Installation (Exogenous Imposition Reading)
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   This constraint describes the process by which new social or cultural
 *   commitments (laws, norms, symbols) are installed top-down by a central
 *   authority, often a state, holding a mandate for transformation. It is a
 *   specific reading of the 'state_commitment_installation_mechanism' kernel,
 *   focusing on exogenous imposition rather than bottom-up or hybrid
 *   processes. The constraint is characterized by high extraction and
 *   suppression, as the new order is enforced against existing traditions and
 *   power structures, leading to identifiable victims and resistance at the
 *   base.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_commitment_installation_mechanism__exogenous_imposition_reading, 0.85).
domain_priors:suppression_score(state_commitment_installation_mechanism__exogenous_imposition_reading, 0.9).
domain_priors:theater_ratio(state_commitment_installation_mechanism__exogenous_imposition_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_commitment_installation_mechanism__exogenous_imposition_reading, snare).
narrative_ontology:human_readable(state_commitment_installation_mechanism__exogenous_imposition_reading, "State-Imposed Commitment Installation (Exogenous Imposition Reading)").
narrative_ontology:topic_domain(state_commitment_installation_mechanism__exogenous_imposition_reading, "historical_sociology/state_formation/cultural_authority").

domain_priors:requires_active_enforcement(state_commitment_installation_mechanism__exogenous_imposition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_commitment_installation_mechanism__exogenous_imposition_reading, '442319a9-7dd7-4387-ae02-2be66c82a046').
narrative_ontology:cs_kernel_codification('442319a9-7dd7-4387-ae02-2be66c82a046', formalized).
narrative_ontology:cs_authority_grounding('442319a9-7dd7-4387-ae02-2be66c82a046', extraction).
narrative_ontology:cs_reading_relation('442319a9-7dd7-4387-ae02-2be66c82a046', state_commitment_installation_mechanism__endogenous_climb_reading, forecloses).
narrative_ontology:cs_reading_relation('442319a9-7dd7-4387-ae02-2be66c82a046', state_commitment_installation_mechanism__hybrid_cascade_reading, forecloses).
narrative_ontology:cs_axiom('442319a9-7dd7-4387-ae02-2be66c82a046', foundational, state_sovereignty_as_sole_legitimator).
narrative_ontology:cs_axiom_status(state_sovereignty_as_sole_legitimator, holdable).
narrative_ontology:cs_axiom_grounding('442319a9-7dd7-4387-ae02-2be66c82a046', state_sovereignty_as_sole_legitimator, deontological).
narrative_ontology:cs_axiom('442319a9-7dd7-4387-ae02-2be66c82a046', foundational, legitimacy_flows_from_decree).
narrative_ontology:cs_axiom_status(legitimacy_flows_from_decree, holdable).
narrative_ontology:cs_axiom_grounding('442319a9-7dd7-4387-ae02-2be66c82a046', legitimacy_flows_from_decree, conventional).
narrative_ontology:cs_reference_frame('442319a9-7dd7-4387-ae02-2be66c82a046', absolute_state_authority).
narrative_ontology:cs_drift_state('442319a9-7dd7-4387-ae02-2be66c82a046', post_globalization_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('442319a9-7dd7-4387-ae02-2be66c82a046', '').
narrative_ontology:cs_kernel_id(state_commitment_installation_mechanism__exogenous_imposition_reading, state_commitment_installation_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__exogenous_imposition_reading, transformative_state_authority).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__exogenous_imposition_reading, traditional_elites).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__exogenous_imposition_reading, local_communities).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__exogenous_imposition_reading, displaced_factions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The central state apparatus, often led by a revolutionary or modernizing elite, that issues decrees and uses its coercive power to install new commitments (laws, norms, symbols) across the territory. It benefits from the consolidation of power and the creation of a unified national identity.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, transformative_state_authority, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Hereditary or customary leaders (e.g., tribal chiefs, religious authorities, landed gentry) whose authority and social capital are directly challenged and often dismantled by the state's new commitments. They face a choice between co-optation, marginalization, or open rebellion.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, traditional_elites, payer,
    powerful, generational, constrained, national).

% Populations living under traditional customs and local governance structures, who are forced to adopt new state-mandated norms, languages, and identities. Their resistance is often localized and met with state repression; exit means displacement or internal exile.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, local_communities, payer,
    powerless, biographical, trapped, local).

% Organized groups or movements that resist the state's imposition of new commitments, often advocating for alternative visions of society or the preservation of existing traditions. They bear the costs of repression, imprisonment, or exile.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, displaced_factions, payer,
    organized, biographical, constrained, national).

% Academics who analyze the processes of state formation and cultural change, documenting the mechanisms of commitment installation, the resistance encountered, and the long-term consequences. They provide an external, analytical perspective on the constraint's operation.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, historical_sociologists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To rapidly establish a new, unified social and cultural order across a diverse population, replacing fragmented traditional systems with a centralized state-sanctioned framework.
% TRANSFER_FUNCTION: Transfers legitimacy, cultural authority, and social control from traditional, local, or competing institutions to the central state. It extracts compliance, resources, and loyalty from the populace.
% ABSENT_VOICES: Traditional religious leaders, customary law practitioners, regional autonomists, and any groups whose identity or power is rooted in the pre-existing order. Their voices are actively suppressed or ignored in the state's narrative of transformation.
% DISAPPEARANCE_RATIONALE: If the state's imposed commitments vanished, the centralized order would likely fragment, leading to a resurgence of traditional practices, local governance, or new forms of social organization, potentially accompanied by conflict over the vacuum of authority.
% FOUNDING_PROBLEM: To overcome political fragmentation, consolidate state power, and forge a new national identity or ideology necessary for modernization or revolutionary transformation.
% FOUNDING_PROBLEM_CORROBORATION: The transformative state authority and its proponents assert the founding problem (fragmentation, backwardness) was live and required radical solutions. Independent historians and anthropologists often corroborate the existence of fragmentation but contest the necessity or legitimacy of the top-down imposition, highlighting the coercive aspects and the suppression of viable alternatives.
narrative_ontology:disappearance_verdict(state_commitment_installation_mechanism__exogenous_imposition_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_commitment_installation_mechanism__exogenous_imposition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_commitment_installation_mechanism__exogenous_imposition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(state_commitment_installation_mechanism__exogenous_imposition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_commitment_installation_mechanism__exogenous_imposition_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_commitment_installation_mechanism__exogenous_imposition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_commitment_installation_mechanism__exogenous_imposition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_commitment_installation_mechanism__exogenous_imposition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.85) reflects the transfer of authority and resources from traditional institutions to the central state, and the imposition of costs (compliance, loss of autonomy) on local populations. Suppression (0.90) is severe because the state actively dismantles or co-opts alternative sources of legitimacy and enforces the new order through coercive means. The low theater ratio (0.10) indicates that the constraint's operation is direct and coercive, with little performative maintenance; its function is primarily to impose and extract. Resistance (0.80) is high due to the direct challenge to established orders.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the transformative state_authority, this mechanism is a necessary act of modernization or nation-building, a 'rope' for societal progress. From the perspective of the traditional_elites and local_communities, it is a 'snare' designed to dismantle their way of life and extract their autonomy. The engine's classification will reflect the latter, given the high extraction and suppression metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   The transformative state_authority is the clear beneficiary and agenda-setter, gaining consolidated power and legitimacy. Traditional_elites, local_communities, and displaced_factions are the targets, bearing the costs of lost autonomy, cultural disruption, and direct repression. Their exit options are severely constrained or trapped, amplifying the effective extraction they experience.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading highlights that the 'mandate for transformation' often serves as a cover story for pure extraction. The initial problem (fragmentation) may be real, but the chosen solution (exogenous imposition) quickly becomes a mechanism for state consolidation and rent-seeking, rather than genuine coordination. The high suppression and extractiveness, coupled with persistent resistance, indicate that the coordination story is secondary to the extractive function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint accurately described as the ''exogenous_imposition_reading'' of the ''state_commitment_installation_mechanism'' kernel?',
    'Comparative historical analysis against empirical cases of state formation, evaluating the primary mechanism of commitment installation (top-down decree vs. bottom-up adoption vs. hybrid).',
    'If the primary mechanism is found to be more ''endogenous_climb'' or ''hybrid_cascade'', the classification of this specific constraint (and its siblings) would shift to reflect different beneficiary/victim structures and power dynamics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Confirms this constraint as a specific reading of the state commitment kernel.').

omega_variable(
    legitimacy_source_ambiguity,
    'Does the ''transformation mandate'' genuinely derive from a broad societal consensus or is it primarily a self-proclaimed justification by the state authority?',
    'Analysis of historical records, popular movements, and alternative political discourses to assess the breadth and depth of support for the state''s transformative agenda at the time of imposition.',
    'If the mandate is found to be self-proclaimed with limited popular backing, the ''snare'' classification is strongly reinforced, as the coordination story loses its foundation. If broad consensus is demonstrated, the constraint might lean more towards a ''tangled_rope'' with a stronger, albeit still extractive, coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_source_ambiguity, empirical, 'Ambiguity regarding the true source of the state''s transformative mandate.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_commitment_installation_mechanism__exogenous_imposition_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(stat_tr_t20, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 20, 0.08).
narrative_ontology:measurement(stat_tr_t40, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 40, 0.07).
narrative_ontology:measurement(stat_tr_t60, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 60, 0.08).
narrative_ontology:measurement(stat_tr_t80, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 80, 0.09).
narrative_ontology:measurement(stat_tr_t100, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 0, 0.8).
narrative_ontology:measurement(stat_be_t20, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 20, 0.85).
narrative_ontology:measurement(stat_be_t40, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 40, 0.87).
narrative_ontology:measurement(stat_be_t60, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 60, 0.85).
narrative_ontology:measurement(stat_be_t80, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 80, 0.83).
narrative_ontology:measurement(stat_be_t100, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 100, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(stat_su_t20, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 20, 0.9).
narrative_ontology:measurement(stat_su_t40, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 40, 0.92).
narrative_ontology:measurement(stat_su_t60, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 60, 0.9).
narrative_ontology:measurement(stat_su_t80, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 80, 0.88).
narrative_ontology:measurement(stat_su_t100, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 100, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_commitment_installation_mechanism__exogenous_imposition_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
