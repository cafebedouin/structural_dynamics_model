% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_substrate__composite_overdetermined_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_substrate__composite_overdetermined_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: honor_satisfaction_substrate__composite_overdetermined_reading
 *   human_readable: Decline of Dueling: Composite Overdetermined Reading
 *   domain: historical_sociology/cultural_anthropology/legal_history
 *
 * SUMMARY:
 *   This constraint describes the decline of dueling as a composite
 *   phenomenon, where both exogenous legal and institutional suppression and
 *   endogenous transformation of the honor code operated simultaneously and
 *   with non-independent causal pathways. It is a reading of the
 *   'honor_satisfaction_substrate' kernel, emphasizing the entanglement of
 *   these forces rather than their separate or additive effects. The
 *   constraint is claimed as a Tangled Rope because it involved both a
 *   coordination function (maintaining social order through honor) and
 *   asymmetric extraction (the state's increasing power over individual
 *   honor, and the rising costs for those who adhered to the old code).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_substrate__composite_overdetermined_reading, 0.4).
domain_priors:suppression_score(honor_satisfaction_substrate__composite_overdetermined_reading, 0.7).
domain_priors:theater_ratio(honor_satisfaction_substrate__composite_overdetermined_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_substrate__composite_overdetermined_reading, tangled_rope).
narrative_ontology:human_readable(honor_satisfaction_substrate__composite_overdetermined_reading, "Decline of Dueling: Composite Overdetermined Reading").
narrative_ontology:topic_domain(honor_satisfaction_substrate__composite_overdetermined_reading, "historical_sociology/cultural_anthropology/legal_history").

domain_priors:requires_active_enforcement(honor_satisfaction_substrate__composite_overdetermined_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_substrate__composite_overdetermined_reading, 'bc4639d1-2965-4634-b88e-c837771ea535').
narrative_ontology:cs_kernel_codification('bc4639d1-2965-4634-b88e-c837771ea535', implicit).
narrative_ontology:cs_authority_grounding('bc4639d1-2965-4634-b88e-c837771ea535', distributed).
narrative_ontology:cs_reading_relation('bc4639d1-2965-4634-b88e-c837771ea535', honor_satisfaction_substrate__practice_decline_reading, influences).
narrative_ontology:cs_reading_relation('bc4639d1-2965-4634-b88e-c837771ea535', honor_satisfaction_substrate__cultural_contraction_reading, influences).
narrative_ontology:cs_axiom('bc4639d1-2965-4634-b88e-c837771ea535', foundational, decline_is_overdetermined_by_entangled_causes).
narrative_ontology:cs_axiom_status(decline_is_overdetermined_by_entangled_causes, holdable).
narrative_ontology:cs_axiom_grounding('bc4639d1-2965-4634-b88e-c837771ea535', decline_is_overdetermined_by_entangled_causes, empirically_contingent).
narrative_ontology:cs_reference_frame('bc4639d1-2965-4634-b88e-c837771ea535', honor_code_as_self_regulating_social_order).
narrative_ontology:cs_drift_state('bc4639d1-2965-4634-b88e-c837771ea535', late_18th_early_19th_century, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('bc4639d1-2965-4634-b88e-c837771ea535', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_substrate__composite_overdetermined_reading, honor_satisfaction_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__composite_overdetermined_reading, state_legal_apparatus).
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__composite_overdetermined_reading, emerging_bourgeois_class).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__composite_overdetermined_reading, honor_bound_aristocracy).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__composite_overdetermined_reading, dueling_seconds).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a ritualized mechanism for resolving disputes among gentlemen, maintaining social hierarchy, and upholding personal honor, thereby coordinating social expectations around status and grievance.
% TRANSFER_FUNCTION: Transferred social status and honor (or its loss) between individuals, and over time, transferred authority over personal conduct from individual honor codes to state legal systems.
% ABSENT_VOICES: Women, lower classes, and religious dissenters were largely excluded from the honor code and its dueling practices; they would have argued for alternative, less violent, and more equitable forms of dispute resolution and social status.
% DISAPPEARANCE_RATIONALE: The disappearance of dueling fundamentally rearranged the social landscape of honor, masculinity, and state authority. Without it, new mechanisms for dispute resolution, legal enforcement, and the construction of gentlemanly identity emerged, leading to a different social order.
% FOUNDING_PROBLEM: The problem of maintaining personal honor and resolving grave insults among a specific social class (aristocracy/gentry) in a manner that preserved social standing and prevented endless feuds.
% FOUNDING_PROBLEM_CORROBORATION: Historians and sociologists widely corroborate that the specific social problem dueling addressed (honor satisfaction through ritual combat) is no longer 'live' in Western societies, having been superseded by legal systems and changed cultural norms. Contemporary legal scholars and cultural anthropologists also attest to this shift, noting the problem's resolution through alternative means.
narrative_ontology:disappearance_verdict(honor_satisfaction_substrate__composite_overdetermined_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_satisfaction_substrate__composite_overdetermined_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_substrate__composite_overdetermined_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(honor_satisfaction_substrate__composite_overdetermined_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_substrate__composite_overdetermined_reading_tests).
:- end_tests(honor_satisfaction_substrate__composite_overdetermined_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.4) reflects the diminishing, but still present, social capital and status derived from dueling, offset by the increasing costs of legal penalties. Suppression (0.7) is high due to active legal prohibitions and institutional disincentives (e.g., military courts-martial). Theater ratio (0.1) is low, as the practice became genuinely dangerous and less about pure performance, though some performative elements remained. Accessibility collapse (0.75) is high because the social and legal environment made dueling increasingly difficult and unthinkable. Resistance (0.2) is low, reflecting the declining adherence to the honor code and the increasing power of the state.
 *
 * PERSPECTIVAL GAP:
 *   For the honor-bound aristocracy, the constraint was increasingly a Snare, as their traditional means of honor satisfaction were criminalized and delegitimized. For the state legal apparatus and the emerging bourgeois class, it was a Rope or even a Mountain, as it solidified their authority and a new social order. This reading emphasizes the simultaneous operation and entanglement of these forces.
 *
 * DIRECTIONALITY LOGIC:
 *   The state legal apparatus and the emerging bourgeois class are beneficiaries (d near 0.0) as they gain authority and consolidate a new social order. The honor-bound aristocracy and dueling seconds are victims (d near 1.0) as their traditional practices are suppressed and delegitimized. The entanglement means that even as the state suppressed, the cultural substrate was shifting, making the 'victim' position less about pure coercion and more about a changing social reality.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (maintaining social order and honor) was not resolved but transformed. The 'mandatrophy' here is not a simple decay but a re-channeling of the underlying social function. The classification as Tangled Rope prevents mislabeling it as a pure Snare (ignoring the cultural transformation) or a pure Rope (ignoring the coercive elements). The entanglement of exogenous and endogenous factors means the 'mandate' itself was a moving target.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint primarily a result of exogenous legal suppression or endogenous cultural transformation?',
    'Detailed historical counterfactual analysis: what would have happened if legal prohibitions were absent but cultural shifts occurred, and vice versa?',
    'If primarily exogenous, the constraint is more of a Snare; if primarily endogenous, it''s closer to a Mountain (cultural norm). This reading asserts both are causally entangled.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is the ''composite_overdetermined_reading'' of the ''honor_satisfaction_substrate'' kernel, asserting entangled exogenous and endogenous causes for dueling''s decline.').

omega_variable(
    causal_pathway_independence,
    'To what extent were the legal/institutional suppression and the honor code transformation causally independent?',
    'Historical analysis tracing specific instances of legal action influencing cultural perception, and cultural shifts enabling legal enforcement.',
    'Higher independence would suggest two distinct constraints operating in parallel; lower independence reinforces the ''composite'' nature of this single constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_pathway_independence, empirical, 'Assesses the degree of causal entanglement between exogenous and endogenous factors in dueling''s decline.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_substrate__composite_overdetermined_reading, 1750, 1850).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t0, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(hono_tr_t10, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(hono_tr_t20, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement(hono_tr_t30, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 30, 0.1).

% Extraction over time
narrative_ontology:measurement(hono_be_t0, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(hono_be_t10, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(hono_be_t20, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(hono_be_t30, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 30, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t0, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(hono_su_t10, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(hono_su_t20, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(hono_su_t30, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 30, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_substrate__composite_overdetermined_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
