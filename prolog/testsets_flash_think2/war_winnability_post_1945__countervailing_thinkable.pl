% ============================================================================
% CONSTRAINT STORY: war_winnability_post_1945__countervailing_thinkable
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_winnability_post_1945__countervailing_thinkable, []).

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
 *   constraint_id: war_winnability_post_1945__countervailing_thinkable
 *   human_readable: Nuclear War Winnability (Countervailing Strategy Reading)
 *   domain: strategic_studies/nuclear_deterrence/international_relations
 *
 * SUMMARY:
 *   This constraint represents the 'countervailing_thinkable' reading of the
 *   'war_winnability_post_1945' kernel. It posits that while nuclear weapons
 *   impose severe constraints, limited victory in a nuclear exchange remains
 *   achievable through doctrines like counterforce targeting. This reading
 *   underpins continuous strategic planning for nuclear warfighting and
 *   justifies significant investment in nuclear arsenals, contrasting with
 *   views that declare nuclear war categorically unwinnable or that emphasize
 *   rhetorical de-escalation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_winnability_post_1945__countervailing_thinkable, 0.68).
domain_priors:suppression_score(war_winnability_post_1945__countervailing_thinkable, 0.75).
domain_priors:theater_ratio(war_winnability_post_1945__countervailing_thinkable, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__countervailing_thinkable, extractiveness, 0.68).
narrative_ontology:constraint_metric(war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(war_winnability_post_1945__countervailing_thinkable, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__countervailing_thinkable, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(war_winnability_post_1945__countervailing_thinkable, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_winnability_post_1945__countervailing_thinkable, tangled_rope).
narrative_ontology:human_readable(war_winnability_post_1945__countervailing_thinkable, "Nuclear War Winnability (Countervailing Strategy Reading)").
narrative_ontology:topic_domain(war_winnability_post_1945__countervailing_thinkable, "strategic_studies/nuclear_deterrence/international_relations").

domain_priors:requires_active_enforcement(war_winnability_post_1945__countervailing_thinkable).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_winnability_post_1945__countervailing_thinkable, '870e4cd3-4280-4853-94f9-c776debd46a2').
narrative_ontology:cs_kernel_codification('870e4cd3-4280-4853-94f9-c776debd46a2', formalized).
narrative_ontology:cs_authority_grounding('870e4cd3-4280-4853-94f9-c776debd46a2', lineage).
narrative_ontology:cs_interpretation_layer_present('870e4cd3-4280-4853-94f9-c776debd46a2').
narrative_ontology:cs_reading_relation('870e4cd3-4280-4853-94f9-c776debd46a2', war_winnability_post_1945__deterrence_unthinkable, forecloses).
narrative_ontology:cs_reading_relation('870e4cd3-4280-4853-94f9-c776debd46a2', war_winnability_post_1945__rhetorical_contraction, coexists_with).
narrative_ontology:cs_axiom('870e4cd3-4280-4853-94f9-c776debd46a2', foundational, nuclear_war_is_fightable).
narrative_ontology:cs_axiom_status(nuclear_war_is_fightable, holdable).
narrative_ontology:cs_axiom_grounding('870e4cd3-4280-4853-94f9-c776debd46a2', nuclear_war_is_fightable, empirically_contingent).
narrative_ontology:cs_axiom('870e4cd3-4280-4853-94f9-c776debd46a2', foundational, escalation_is_controllable).
narrative_ontology:cs_axiom_status(escalation_is_controllable, holdable).
narrative_ontology:cs_axiom_grounding('870e4cd3-4280-4853-94f9-c776debd46a2', escalation_is_controllable, empirically_contingent).
narrative_ontology:cs_reference_frame('870e4cd3-4280-4853-94f9-c776debd46a2', flexible_response_doctrine).
narrative_ontology:cs_drift_state('870e4cd3-4280-4853-94f9-c776debd46a2', contemporary_strategic_environment, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('870e4cd3-4280-4853-94f9-c776debd46a2', '').
narrative_ontology:cs_kernel_id(war_winnability_post_1945__countervailing_thinkable, war_winnability_post_1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__countervailing_thinkable, military_industrial_complex).
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__countervailing_thinkable, strategic_planners).
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__countervailing_thinkable, nuclear_powers).
narrative_ontology:constraint_victim(war_winnability_post_1945__countervailing_thinkable, arms_control_regimes).
narrative_ontology:constraint_victim(war_winnability_post_1945__countervailing_thinkable, peace_advocates).
narrative_ontology:constraint_victim(war_winnability_post_1945__countervailing_thinkable, taxpayers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the continuous investment in research, development, and deployment of counterforce nuclear capabilities and associated command, control, communications, and intelligence (C3I) systems. This doctrine provides a rationale for mission continuity and budget allocation.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, military_industrial_complex, agenda_setter,
    institutional, generational, arbitrage, global).

% Responsible for developing and maintaining war plans that incorporate counterforce targeting and limited victory scenarios. Their professional identity and career paths are often tied to the continued relevance of such strategic thinking.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, strategic_planners, agenda_setter,
    institutional, biographical, constrained, national).

% Maintain their strategic leverage and perceived security by possessing a doctrine that suggests options beyond pure annihilation, thereby enhancing deterrence credibility and bargaining power in international relations.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, nuclear_powers, beneficiary,
    institutional, generational, constrained, global).

% Bear the cost of undermined efforts to reduce nuclear arsenals and prevent proliferation. The pursuit of 'winnable' scenarios often necessitates capabilities that complicate arms control verification and trust-building.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, arms_control_regimes, payer,
    organized, generational, constrained, global).

% Bear the cost of increased perceived risk of nuclear war and the diversion of resources from social welfare to military spending. Their efforts to promote disarmament and de-escalation are directly challenged by this doctrine.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, peace_advocates, payer,
    organized, biographical, constrained, global).

% Fund the substantial costs associated with maintaining and modernizing nuclear arsenals, strategic planning, and the military-industrial complex, often without direct input or clear understanding of the strategic rationale.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, taxpayers, payer,
    powerless, immediate, constrained, national).

% Proponents of the view that nuclear war is categorically unwinnable are structurally excluded from mainstream strategic planning and resource allocation debates, as their core premise directly contradicts the operational assumptions of countervailing strategy.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, deterrence_unthinkable_theorists, excluded,
    analytical, biographical, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains strategic stability by ensuring a credible retaliatory and warfighting capability, preventing an adversary from believing a first strike could achieve decisive victory without unacceptable costs, thereby coordinating the actions of nuclear powers towards mutual restraint.
% TRANSFER_FUNCTION: Transfers vast resources (funding, intellectual capital, scientific talent) from public welfare, civilian research, and arms control initiatives to military research, development, and strategic planning, in exchange for perceived national security and strategic leverage.
% ABSENT_VOICES: Proponents of 'deterrence unthinkable' and 'rhetorical contraction' readings are sidelined, as their arguments for the categorical unwinnability of nuclear war or the need for discursive de-escalation are incompatible with the operational planning for limited victory. Their perspectives are actively suppressed in official strategic discourse.
% DISAPPEARANCE_RATIONALE: If the belief in limited nuclear victory vanished overnight, strategic doctrines would shift dramatically towards pure deterrence or disarmament, arms control would gain significant traction, and the military-industrial complex would face a crisis of mission, leading to a major reorganization of global security architecture and resource allocation.
% FOUNDING_PROBLEM: How to maintain credible deterrence and prevent nuclear blackmail in a world where nuclear weapons exist, without resorting to a 'mutual assured destruction' (MAD) doctrine that might be perceived as lacking flexible response options or inviting preemption.
% FOUNDING_PROBLEM_CORROBORATION: Strategic planners and defense establishments within nuclear powers corroborate the problem's ongoing relevance, citing evolving threats and the need for flexible response options. Independent academic analyses and historical records from outside the benefiting parties also attest to the initial problem, though they often contest its current status or the efficacy of the countervailing solution.
narrative_ontology:disappearance_verdict(war_winnability_post_1945__countervailing_thinkable, world_rearranges).
narrative_ontology:founding_problem_status(war_winnability_post_1945__countervailing_thinkable, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_winnability_post_1945__countervailing_thinkable, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(war_winnability_post_1945__countervailing_thinkable, 'none', 1).
narrative_ontology:epsilon_provenance(war_winnability_post_1945__countervailing_thinkable, 0.68, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_winnability_post_1945__countervailing_thinkable_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_winnability_post_1945__countervailing_thinkable, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(war_winnability_post_1945__countervailing_thinkable_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) because the doctrine demands continuous resource allocation to maintain complex and costly nuclear forces, diverting funds from other sectors. Suppression is also high (0.75) as this reading actively marginalizes alternative strategic paradigms (e.g., pure deterrence, disarmament) that challenge its core assumptions. Theater ratio is moderate (0.40); while there's genuine strategic analysis, some aspects of 'winnability' rhetoric serve to justify military budgets and maintain institutional relevance. Accessibility collapse is high (0.80) because this reading collapses the alternative of a world without nuclear war planning, and the alternative of total victory without nuclear exchange. Resistance is high (0.70) due to strong opposition from arms control and peace movements.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of strategic planners and nuclear powers, this doctrine is a necessary coordination mechanism for maintaining deterrence and national security in a dangerous world. From the perspective of arms control advocates and peace movements, it is an extractive mechanism that perpetuates an arms race and increases the risk of catastrophe, while diverting resources from more pressing societal needs. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The military-industrial complex, strategic planners, and nuclear powers are clear beneficiaries, gaining mission continuity, resources, and strategic leverage. Arms control regimes, peace advocates, and taxpayers are victims, bearing the costs of increased militarization and reduced security. The 'deterrence unthinkable' theorists are excluded, as their foundational premise is incompatible with this reading's operational logic.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    strategic_necessity_vs_institutional_imperative,
    'Is the ''countervailing_thinkable'' doctrine a genuine strategic necessity for national security, or is its persistence primarily driven by institutional imperatives of the military-industrial complex and strategic planning communities?',
    'Independent, declassified analysis of strategic alternatives and their efficacy, coupled with a cost-benefit analysis that includes opportunity costs and long-term risks, conducted by entities without direct financial or professional stake in the doctrine''s perpetuation.',
    'If primarily institutional, the constraint''s extractiveness and theater_ratio would be re-evaluated upwards, and its coordination function downwards, potentially reclassifying it closer to a Snare. If genuine strategic necessity, its coordination function would be affirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strategic_necessity_vs_institutional_imperative, conceptual, 'Distinguishing between genuine strategic need and self-serving institutional perpetuation.').

omega_variable(
    escalation_control_feasibility,
    'Is escalation control in a limited nuclear exchange, as posited by counterforce targeting, empirically feasible, or is it a theoretical construct that would collapse in practice?',
    'Historical analysis of crisis escalation dynamics (though limited for nuclear scenarios), wargaming simulations with high fidelity to human and systemic factors, and expert consensus from diverse strategic communities (including those skeptical of control).',
    'If escalation control is empirically infeasible, the doctrine''s coordination function (maintaining stability through flexible response) would be severely undermined, increasing its perceived risk and extractiveness from a global security perspective, potentially shifting its classification towards a Snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(escalation_control_feasibility, empirical, 'The empirical feasibility of controlling escalation in a nuclear conflict.').

omega_variable(
    suppression_mechanism_ideological_lockin,
    'Is the suppression of alternative strategic views (e.g., ''deterrence unthinkable'') structural (institutional power, funding biases) or internalized (ideological lock-in within strategic communities)?',
    'Analysis of career trajectories and funding patterns for strategic thinkers who advocate alternative views, combined with qualitative studies of decision-making processes within defense establishments. If alternative views gain traction when structural barriers are lowered, it suggests structural suppression. If they remain marginalized even with open discourse, it suggests internalized lock-in.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as the target (alternative ideas) carries the suppression with them even in theoretically open forums. This would amplify the constraint''s extractive nature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ideological_lockin, empirical, 'Structural vs. internalized suppression of alternative strategic paradigms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_winnability_post_1945__countervailing_thinkable, 1960, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t1960, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 1960, 0.2).
narrative_ontology:measurement(war__tr_t1975, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 1975, 0.3).
narrative_ontology:measurement(war__tr_t1990, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 1990, 0.35).
narrative_ontology:measurement(war__tr_t2005, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 2005, 0.38).
narrative_ontology:measurement(war__tr_t2025, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 2025, 0.4).

% Extraction over time
narrative_ontology:measurement(war__be_t1960, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 1960, 0.55).
narrative_ontology:measurement(war__be_t1975, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 1975, 0.6).
narrative_ontology:measurement(war__be_t1990, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 1990, 0.65).
narrative_ontology:measurement(war__be_t2005, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 2005, 0.67).
narrative_ontology:measurement(war__be_t2025, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(war__su_t1960, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 1960, 0.6).
narrative_ontology:measurement(war__su_t1975, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 1975, 0.68).
narrative_ontology:measurement(war__su_t1990, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 1990, 0.72).
narrative_ontology:measurement(war__su_t2005, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 2005, 0.74).
narrative_ontology:measurement(war__su_t2025, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 2025, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_winnability_post_1945__countervailing_thinkable, enforcement_mechanism).
narrative_ontology:affects_constraint(war_winnability_post_1945__countervailing_thinkable, arms_race_dynamics).
narrative_ontology:affects_constraint(war_winnability_post_1945__countervailing_thinkable, nuclear_proliferation_risk).
narrative_ontology:affects_constraint(war_winnability_post_1945__countervailing_thinkable, deterrence_unthinkable).
narrative_ontology:affects_constraint(war_winnability_post_1945__countervailing_thinkable, rhetorical_contraction).

% DUAL FORMULATION NOTE:
% This constraint is one reading ('countervailing_thinkable') of the 'war_winnability_post_1945' kernel. It structurally influences the 'arms_race_dynamics' and 'nuclear_proliferation_risk' constraints by providing a rationale for continuous arms development and perceived utility of nuclear weapons. It is linked to its sibling readings ('deterrence_unthinkable', 'rhetorical_contraction') as part of a contested conceptual space.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
