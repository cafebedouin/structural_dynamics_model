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
 *   constraint_id: honor_satisfaction_substrate__composite_overdetermined_reading
 *   human_readable: Dueling's Decline: Composite Overdetermined Reading
 *   domain: historical_sociology/cultural_anthropology/legal_history
 *
 * SUMMARY:
 *   This constraint represents the composite, overdetermined reading of
 *   dueling's decline, where both exogenous legal/institutional suppression
 *   and endogenous cultural delegitimation (honor code transformation)
 *   operated simultaneously with causally entangled pathways. It is a
 *   tangled_rope because it involved active enforcement by the state
 *   (coordination for social order) but extracted from the traditional
 *   aristocracy and honor-bound individuals (asymmetric cost). The claimed
 *   type 'tangled_rope' reflects the active enforcement and coordination
 *   function, while the metrics capture the substantial suppression and the
 *   costs borne by those whose honor system was being dismantled.
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
narrative_ontology:human_readable(honor_satisfaction_substrate__composite_overdetermined_reading, "Dueling's Decline: Composite Overdetermined Reading").
narrative_ontology:topic_domain(honor_satisfaction_substrate__composite_overdetermined_reading, "historical_sociology/cultural_anthropology/legal_history").

domain_priors:requires_active_enforcement(honor_satisfaction_substrate__composite_overdetermined_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_substrate__composite_overdetermined_reading, '382db960-8142-4fed-815b-e9563ab931dc').
narrative_ontology:cs_kernel_codification('382db960-8142-4fed-815b-e9563ab931dc', implicit).
narrative_ontology:cs_authority_grounding('382db960-8142-4fed-815b-e9563ab931dc', practice).
narrative_ontology:cs_interpretation_layer_present('382db960-8142-4fed-815b-e9563ab931dc').
narrative_ontology:cs_reading_relation('382db960-8142-4fed-815b-e9563ab931dc', honor_satisfaction_substrate__practice_decline_reading, influences).
narrative_ontology:cs_reading_relation('382db960-8142-4fed-815b-e9563ab931dc', honor_satisfaction_substrate__cultural_contraction_reading, influences).
narrative_ontology:cs_axiom('382db960-8142-4fed-815b-e9563ab931dc', foundational, decline_is_overdetermined).
narrative_ontology:cs_axiom_status(decline_is_overdetermined, holdable).
narrative_ontology:cs_axiom_grounding('382db960-8142-4fed-815b-e9563ab931dc', decline_is_overdetermined, empirically_contingent).
narrative_ontology:cs_axiom('382db960-8142-4fed-815b-e9563ab931dc', foundational, exogenous_endogenous_causal_entanglement).
narrative_ontology:cs_axiom_status(exogenous_endogenous_causal_entanglement, holdable).
narrative_ontology:cs_axiom_grounding('382db960-8142-4fed-815b-e9563ab931dc', exogenous_endogenous_causal_entanglement, empirically_contingent).
narrative_ontology:cs_reference_frame('382db960-8142-4fed-815b-e9563ab931dc', dueling_as_legitimate_honor_satisfaction).
narrative_ontology:cs_drift_state('382db960-8142-4fed-815b-e9563ab931dc', late_19th_century_europe, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('382db960-8142-4fed-815b-e9563ab931dc', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_substrate__composite_overdetermined_reading, honor_satisfaction_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__composite_overdetermined_reading, state_legal_apparatus).
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__composite_overdetermined_reading, emerging_bourgeois_class).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__composite_overdetermined_reading, traditional_aristocracy).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__composite_overdetermined_reading, honor_bound_individuals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively enforced laws against dueling, imposing penalties and delegitimizing the practice as a challenge to state monopoly on violence. Benefited from increased social order and centralized authority.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, state_legal_apparatus, agenda_setter,
    institutional, generational, mobile, national).

% Benefited from the decline of dueling as it removed a costly and risky aristocratic practice that was incompatible with their commercial and industrial values. Their social norms shifted towards 'dignity' and legal recourse.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, emerging_bourgeois_class, beneficiary,
    powerful, biographical, mobile, national).

% Saw dueling as a core component of their honor system and social status. Faced legal penalties and social pressure to abandon the practice, leading to a loss of a traditional means of satisfaction and status maintenance.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, traditional_aristocracy, payer,
    moderate, generational, identity_locked, regional).

% Individuals who felt compelled by the honor code to duel, but faced increasing legal and social repercussions. Their options were to risk legal action and social ostracization, or to internalize the changing norms and find alternative means of satisfaction.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, honor_bound_individuals, payer,
    powerless, immediate, constrained, local).

% Analyze the complex interplay of legal, social, and cultural factors that led to the decline of dueling, seeking to understand the causal pathways and the transformation of honor codes.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, cultural_historians, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint coordinated the shift from a private, violent system of honor satisfaction (dueling) to a public, legal system of dispute resolution, reducing social disorder and consolidating state authority.
% TRANSFER_FUNCTION: Transferred the right to adjudicate grievances and enforce social norms from individuals and aristocratic codes to the state legal system, along with the associated social capital and power.
% ABSENT_VOICES: Those who clung to the traditional honor code, particularly in regions where state authority was weaker or aristocratic traditions more entrenched, were increasingly marginalized and their voices suppressed by legal and social pressures.
% DISAPPEARANCE_RATIONALE: If the legal and cultural constraints against dueling vanished, it is unlikely dueling would immediately return to its historical prevalence due to the deep-seated cultural shifts. However, the absence of these constraints would necessitate a re-evaluation of dispute resolution mechanisms and honor satisfaction, potentially leading to new forms of extra-legal violence or a resurgence of similar practices in certain subcultures.
% FOUNDING_PROBLEM: The problem of maintaining social order and resolving disputes in a society where private violence (dueling) was a legitimate means of honor satisfaction, challenging state authority and leading to unpredictable social costs.
% FOUNDING_PROBLEM_CORROBORATION: The state legal apparatus and the emerging bourgeois class attested to the problem's live status, seeking to consolidate power and establish new social norms. Cultural historians corroborate that the problem of private violence challenging state authority is a recurring theme in legal history, even if dueling itself is no longer the primary manifestation.
narrative_ontology:disappearance_verdict(honor_satisfaction_substrate__composite_overdetermined_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_satisfaction_substrate__composite_overdetermined_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_substrate__composite_overdetermined_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(honor_satisfaction_substrate__composite_overdetermined_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_substrate__composite_overdetermined_reading, 0.4, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is moderate (0.4) because the state gained social order and consolidated authority, but the direct financial extraction was not the primary mechanism. Suppression is high (0.7) due to active legal prohibition and institutional pressure. Theater ratio is low (0.1) as the decline was a genuine, functional shift, not merely performative maintenance. Accessibility collapse is high (0.75) because both legal barriers and cultural shifts made dueling increasingly unthinkable. Resistance is low (0.2) because the forces of change were overwhelming and diffuse.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the state and the rising bourgeois class, the decline of dueling was a positive development, a move towards a more rational and orderly society. From the perspective of the traditional aristocracy, it was a loss of status, tradition, and a fundamental aspect of their identity. This reading acknowledges both perspectives as contributing to the overdetermined decline.
 *
 * DIRECTIONALITY LOGIC:
 *   The state legal apparatus and the emerging bourgeois class were beneficiaries, gaining social order and a more compatible normative framework. The traditional aristocracy and honor-bound individuals were payers, losing a means of honor satisfaction and facing legal/social costs. The decline was not a simple imposition but a complex process where the 'rope' of dueling was broken by external force while its 'mountain' of honor was eroded from within.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (social order, state authority) remained live, but the means of achieving it shifted. The classification as tangled_rope prevents mislabeling it as a pure snare, acknowledging the genuine coordination function of reducing private violence, while also recognizing the asymmetric extraction from those whose honor system was dismantled. The overdetermined nature of the decline means that even if legal suppression had been less effective, the internal erosion of the honor code would have contributed to its decline.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_pathway_weighting,
    'What was the relative causal weight of exogenous legal suppression versus endogenous cultural delegitimation in dueling''s decline?',
    'Comparative historical analysis across different national contexts with varying legal enforcement strengths and cultural trajectories, or counterfactual modeling.',
    'If legal suppression was dominant, the constraint leans more towards a Snare; if cultural delegitimation was dominant, it leans more towards a Mountain erosion. This reading asserts entanglement, not dominance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_pathway_weighting, empirical, 'Determining the primary driver of dueling''s decline.').

omega_variable(
    honor_code_transformation_mechanism,
    'What were the specific mechanisms by which the honor code transformed from a ''culture of honor'' to a ''culture of dignity''?',
    'Detailed micro-historical studies of individual and group responses to legal changes and social pressures, analyzing shifts in language, rituals, and self-perception.',
    'A clearer understanding of the internal transformation would strengthen the ''mountain erosion'' aspect of this composite reading, showing how the substrate itself changed, rather than just being externally suppressed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(honor_code_transformation_mechanism, empirical, 'Understanding the internal dynamics of honor code change.').

omega_variable(
    entanglement_vs_additivity,
    'Were the causal pathways of legal suppression and cultural delegitimation truly entangled, or were they largely additive and independent?',
    'Process tracing and qualitative comparative analysis to identify feedback loops and synergistic effects between legal changes and cultural shifts. If legal changes accelerated cultural shifts, and vice versa, entanglement is supported.',
    'If additive, the composite reading might be better represented as two distinct, co-occurring constraints. If entangled, this reading''s core premise of non-independent pathways is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(entanglement_vs_additivity, conceptual, 'Clarifying the relationship between the two causal forces.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_substrate__composite_overdetermined_reading, 1700, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1700, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 1700, 0.2).
narrative_ontology:measurement(hono_tr_t1750, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 1750, 0.15).
narrative_ontology:measurement(hono_tr_t1800, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 1800, 0.1).
narrative_ontology:measurement(hono_tr_t1850, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 1850, 0.08).
narrative_ontology:measurement(hono_tr_t1900, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 1900, 0.1).

% Extraction over time
narrative_ontology:measurement(hono_be_t1700, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 1700, 0.3).
narrative_ontology:measurement(hono_be_t1750, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 1750, 0.35).
narrative_ontology:measurement(hono_be_t1800, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 1800, 0.4).
narrative_ontology:measurement(hono_be_t1850, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 1850, 0.38).
narrative_ontology:measurement(hono_be_t1900, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 1900, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1700, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 1700, 0.4).
narrative_ontology:measurement(hono_su_t1750, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 1750, 0.55).
narrative_ontology:measurement(hono_su_t1800, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 1800, 0.7).
narrative_ontology:measurement(hono_su_t1850, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 1850, 0.65).
narrative_ontology:measurement(hono_su_t1900, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 1900, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_substrate__composite_overdetermined_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'honor_satisfaction_substrate' kernel, focusing on the composite, overdetermined nature of dueling's decline, where both legal suppression and cultural transformation were entangled.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
