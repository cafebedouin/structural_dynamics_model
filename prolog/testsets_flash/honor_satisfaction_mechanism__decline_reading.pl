% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_mechanism__decline_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_mechanism__decline_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: honor_satisfaction_mechanism__decline_reading
 *   human_readable: Honor Satisfaction Mechanism (Decline Reading)
 *   domain: historical_sociology/legal_history/normative_systems
 *
 * SUMMARY:
 *   This constraint describes the honor satisfaction mechanism, specifically
 *   through dueling, as it underwent a period of decline from widespread
 *   practice to fringe status. It is a reading of the
 *   'honor_satisfaction_mechanism' kernel, focusing on the gradual erosion of
 *   its social and legal legitimacy. While dueling persisted, its frequency
 *   decreased, and the social and legal costs for participants increased,
 *   pushing it towards a degraded, performative state.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_mechanism__decline_reading, 0.3).
domain_priors:suppression_score(honor_satisfaction_mechanism__decline_reading, 0.6).
domain_priors:theater_ratio(honor_satisfaction_mechanism__decline_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_mechanism__decline_reading, piton).
narrative_ontology:human_readable(honor_satisfaction_mechanism__decline_reading, "Honor Satisfaction Mechanism (Decline Reading)").
narrative_ontology:topic_domain(honor_satisfaction_mechanism__decline_reading, "historical_sociology/legal_history/normative_systems").

domain_priors:requires_active_enforcement(honor_satisfaction_mechanism__decline_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_mechanism__decline_reading, '383cca82-f616-401b-8c0c-9506cfa3d3a4').
narrative_ontology:cs_kernel_codification('383cca82-f616-401b-8c0c-9506cfa3d3a4', implicit).
narrative_ontology:cs_authority_grounding('383cca82-f616-401b-8c0c-9506cfa3d3a4', practice).
narrative_ontology:cs_interpretation_layer_present('383cca82-f616-401b-8c0c-9506cfa3d3a4').
narrative_ontology:cs_reading_relation('383cca82-f616-401b-8c0c-9506cfa3d3a4', honor_satisfaction_mechanism__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('383cca82-f616-401b-8c0c-9506cfa3d3a4', honor_satisfaction_mechanism__composite_reading, coexists_with).
narrative_ontology:cs_axiom('383cca82-f616-401b-8c0c-9506cfa3d3a4', foundational, honor_requires_physical_satisfaction).
narrative_ontology:cs_axiom_status(honor_requires_physical_satisfaction, holdable).
narrative_ontology:cs_axiom_grounding('383cca82-f616-401b-8c0c-9506cfa3d3a4', honor_requires_physical_satisfaction, conventional).
narrative_ontology:cs_axiom('383cca82-f616-401b-8c0c-9506cfa3d3a4', secondary, state_monopoly_on_violence_is_supreme).
narrative_ontology:cs_axiom_status(state_monopoly_on_violence_is_supreme, holdable).
narrative_ontology:cs_axiom_grounding('383cca82-f616-401b-8c0c-9506cfa3d3a4', state_monopoly_on_violence_is_supreme, conventional).
narrative_ontology:cs_reference_frame('383cca82-f616-401b-8c0c-9506cfa3d3a4', honor_code_supremacy).
narrative_ontology:cs_drift_state('383cca82-f616-401b-8c0c-9506cfa3d3a4', late_19th_century, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('383cca82-f616-401b-8c0c-9506cfa3d3a4', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_mechanism__decline_reading, honor_satisfaction_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__decline_reading, honor_seekers).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__decline_reading, families_of_duelists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__decline_reading, social_elites).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals who felt their honor was impugned and sought satisfaction through dueling. They bore the risk of death or injury, and increasingly, legal penalties and social ostracization. Their options were to duel, accept perceived dishonor, or seek alternative, less violent forms of redress.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, honor_seekers, payer,
    moderate, biographical, constrained, local).

% Governments and legal systems that increasingly criminalized dueling, imposing fines, imprisonment, or exile. They sought to establish a monopoly on violence and maintain public order, gradually eroding the social legitimacy of dueling.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, state_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Families of duelists bore the social stigma, financial costs, and emotional trauma associated with dueling, whether their member was killed, injured, or prosecuted. They had little direct agency to prevent duels once challenged, often being bound by the same honor code.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, families_of_duelists, payer,
    moderate, biographical, trapped, local).

% While some participated in duels, the broader elite benefited from the decline of dueling as it reduced internal violence and allowed for more stable social and political order. They gradually shifted towards other mechanisms for resolving disputes and maintaining status.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, social_elites, beneficiary,
    powerful, generational, mobile, national).

% The evolving collective sentiment that increasingly viewed dueling as barbaric, anachronistic, and a threat to civil society. This shift in public perception contributed to the social costs borne by duelists and supported state enforcement efforts.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, public_opinion, observer,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_satisfaction_mechanism__decline_reading, diffuse).
narrative_ontology:fixing_cost_class(honor_satisfaction_mechanism__decline_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a formalized, if violent, mechanism for individuals to resolve disputes over honor and reputation, coordinating social expectations around masculine codes of conduct.
% TRANSFER_FUNCTION: Transferred social status and perceived honor to the victor (or survivor) of a duel, while transferring risk of death, injury, and legal/social penalties to both participants, and ultimately, to their families and society.
% ABSENT_VOICES: The victims of dueling (those killed or permanently injured) and their immediate families, who bore the ultimate costs, were largely excluded from the 'honor' discourse that perpetuated the practice. They would argue for the inherent barbarity and social destructiveness of dueling.
% DISAPPEARANCE_RATIONALE: If the mechanism for honor satisfaction via dueling vanished overnight, individuals would be forced to find alternative, non-violent means to resolve disputes and defend their reputation, leading to a reorganization of social norms around conflict resolution and a strengthening of state legal authority.
% FOUNDING_PROBLEM: The need for a formalized mechanism to resolve disputes over personal honor and reputation among social elites, where state legal systems were perceived as inadequate or too slow for immediate satisfaction.
% FOUNDING_PROBLEM_CORROBORATION: Historians and legal scholars widely corroborate that while honor disputes persist, the specific problem of needing dueling for 'satisfaction' is dead, having been superseded by stronger state legal systems and evolving social norms. Contemporary public opinion also views dueling as an anachronism.
narrative_ontology:disappearance_verdict(honor_satisfaction_mechanism__decline_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_satisfaction_mechanism__decline_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_mechanism__decline_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(honor_satisfaction_mechanism__decline_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_mechanism__decline_reading_tests).
:- end_tests(honor_satisfaction_mechanism__decline_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.3) reflects the declining but still present costs borne by duelists and their families, even as the practice became less common. Suppression (0.6) is moderate, reflecting the increasing, but not absolute, state and social pressure against dueling. The theater ratio (0.4) indicates that by the end of the period, dueling was often more about performing adherence to an outdated code than genuinely resolving disputes, with many duels being ritualized or avoided. Resistance (0.7) is high, reflecting the active efforts by state authorities and public opinion to suppress the practice. Accessibility collapse (0.4) is low, as alternatives for dispute resolution (legal systems, social mediation) were increasingly available.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of a duelist in the early period, the mechanism might have felt like a necessary, if risky, coordination for maintaining honor. By the later period, from the perspective of state authorities or the general public, it was an anachronistic, extractive practice that needed to be suppressed. The decline reading emphasizes this shift in perception and the increasing costs for participants.
 *
 * DIRECTIONALITY LOGIC:
 *   Honor seekers (payers) bore the direct costs and risks, placing them at the target end. State authorities (agenda setters) actively suppressed the practice, benefiting from increased social order and a monopoly on violence. Families of duelists (payers) bore significant indirect costs. Social elites (beneficiaries) benefited from the overall decline in violence and the shift to more 'civilized' forms of dispute resolution. Public opinion (observer) shifted to condemn dueling, reinforcing its decline.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits clear mandatrophy: the founding problem (resolving honor disputes when state law was weak) became 'dead' as state authority strengthened and social norms evolved. The persistence of dueling, even at a declining rate, became increasingly performative (rising theater ratio) and extractive, sustained by inertia and a dwindling adherence to an outdated code. This classification as a Piton reflects that the primary function atrophied, but the constraint remained due to institutional inertia and theatrical maintenance, extracting diffuse costs without concentrated benefits.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    decline_vs_contraction_mechanism,
    'Did dueling decline due to a gradual erosion of social and legal support (decline_reading), or did it become cognitively unthinkable, a category-level impossibility (contraction_reading)?',
    'Analysis of primary sources (diaries, legal records, philosophical treatises) for explicit statements on the ''thinkability'' of dueling, and the mechanisms of social sanction vs. cognitive impossibility. The decline reading emphasizes external pressures; the contraction reading emphasizes internal cognitive shifts.',
    'If contraction is true, the constraint''s suppression and accessibility collapse would be higher, reflecting a more fundamental shift in social cognition rather than mere legal/social pressure. This would push the classification towards a Mountain of social cognition, rather than a Piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decline_vs_contraction_mechanism, conceptual, 'Distinguishing between gradual social decline and a fundamental cognitive shift in the status of dueling.').

omega_variable(
    composite_vs_decline_causality,
    'Was the decline of dueling driven by a single, overarching process of erosion (decline_reading), or by multiple distinct, interacting mechanisms (state monopoly, bourgeois norms, insurance, category-shift) as proposed by the composite_reading?',
    'Detailed historical-sociological analysis disentangling the causal pathways and their relative contributions. The decline reading offers a simpler, more linear narrative; the composite reading posits a more complex, multi-causal explanation.',
    'If the composite reading is true, this single constraint story would be an oversimplification, and the phenomenon would be better modeled as a family of linked constraints, each representing a distinct mechanism, with different extractiveness and suppression profiles.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(composite_vs_decline_causality, empirical, 'Whether the decline was a singular process or a result of multiple interacting factors.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_mechanism__decline_reading, 1700, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1700, honor_satisfaction_mechanism__decline_reading, theater_ratio, 1700, 0.1).
narrative_ontology:measurement(hono_tr_t1750, honor_satisfaction_mechanism__decline_reading, theater_ratio, 1750, 0.2).
narrative_ontology:measurement(hono_tr_t1800, honor_satisfaction_mechanism__decline_reading, theater_ratio, 1800, 0.3).
narrative_ontology:measurement(hono_tr_t1850, honor_satisfaction_mechanism__decline_reading, theater_ratio, 1850, 0.35).
narrative_ontology:measurement(hono_tr_t1900, honor_satisfaction_mechanism__decline_reading, theater_ratio, 1900, 0.4).

% Extraction over time
narrative_ontology:measurement(hono_be_t1700, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 1700, 0.6).
narrative_ontology:measurement(hono_be_t1750, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 1750, 0.5).
narrative_ontology:measurement(hono_be_t1800, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 1800, 0.4).
narrative_ontology:measurement(hono_be_t1850, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 1850, 0.35).
narrative_ontology:measurement(hono_be_t1900, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 1900, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1700, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 1700, 0.3).
narrative_ontology:measurement(hono_su_t1750, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 1750, 0.4).
narrative_ontology:measurement(hono_su_t1800, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 1800, 0.5).
narrative_ontology:measurement(hono_su_t1850, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 1850, 0.55).
narrative_ontology:measurement(hono_su_t1900, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 1900, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_mechanism__decline_reading, identity_coordination).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__decline_reading, honor_satisfaction_mechanism__contraction_reading).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__decline_reading, honor_satisfaction_mechanism__composite_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading ('decline_reading') of the 'honor_satisfaction_mechanism' kernel. It focuses on the gradual erosion of dueling's social and legal legitimacy, leading to its decline. Sibling readings (contraction_reading, composite_reading) offer alternative explanations for the end of dueling.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
