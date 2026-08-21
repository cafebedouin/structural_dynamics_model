% ============================================================================
% CONSTRAINT STORY: total_war_possibility_space__deterrence_equilibrium_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_possibility_space__deterrence_equilibrium_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: total_war_possibility_space__deterrence_equilibrium_reading
 *   human_readable: Deterrence Equilibrium Reading of Total War Possibility Space
 *   domain: international_relations_theory/strategic_studies
 *
 * SUMMARY:
 *   This constraint story instantiates the 'deterrence equilibrium' reading
 *   of the 'total_war_possibility_space' kernel. It posits that total war
 *   remains a strategically reachable option, but its actualization is
 *   deterred by the mutual vulnerability created by nuclear weapons. The
 *   constraint is maintained through continuous investment in war-fighting
 *   capabilities and credible threats of retaliation, leading to a stable but
 *   costly equilibrium. This reading predicts ongoing doctrine development,
 *   counterforce targeting, and theorization of escalation ladders.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_possibility_space__deterrence_equilibrium_reading, 0.65).
domain_priors:suppression_score(total_war_possibility_space__deterrence_equilibrium_reading, 0.9).
domain_priors:theater_ratio(total_war_possibility_space__deterrence_equilibrium_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_possibility_space__deterrence_equilibrium_reading, tangled_rope).
narrative_ontology:human_readable(total_war_possibility_space__deterrence_equilibrium_reading, "Deterrence Equilibrium Reading of Total War Possibility Space").
narrative_ontology:topic_domain(total_war_possibility_space__deterrence_equilibrium_reading, "international_relations_theory/strategic_studies").

domain_priors:requires_active_enforcement(total_war_possibility_space__deterrence_equilibrium_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_possibility_space__deterrence_equilibrium_reading, '823cfaa9-6af4-4c28-8bbf-e29cace34044').
narrative_ontology:cs_kernel_codification('823cfaa9-6af4-4c28-8bbf-e29cace34044', formalized).
narrative_ontology:cs_authority_grounding('823cfaa9-6af4-4c28-8bbf-e29cace34044', practice).
narrative_ontology:cs_interpretation_layer_present('823cfaa9-6af4-4c28-8bbf-e29cace34044').
narrative_ontology:cs_reading_relation('823cfaa9-6af4-4c28-8bbf-e29cace34044', total_war_possibility_space__nuclear_taboo_reading, coexists_with).
narrative_ontology:cs_reading_relation('823cfaa9-6af4-4c28-8bbf-e29cace34044', total_war_possibility_space__space_contraction_reading, forecloses).
narrative_ontology:cs_axiom('823cfaa9-6af4-4c28-8bbf-e29cace34044', foundational, rational_actor_calculus).
narrative_ontology:cs_axiom_status(rational_actor_calculus, holdable).
narrative_ontology:cs_axiom_grounding('823cfaa9-6af4-4c28-8bbf-e29cace34044', rational_actor_calculus, empirically_contingent).
narrative_ontology:cs_axiom('823cfaa9-6af4-4c28-8bbf-e29cace34044', foundational, mutual_vulnerability_ensures_deterrence).
narrative_ontology:cs_axiom_status(mutual_vulnerability_ensures_deterrence, holdable).
narrative_ontology:cs_axiom_grounding('823cfaa9-6af4-4c28-8bbf-e29cace34044', mutual_vulnerability_ensures_deterrence, empirically_contingent).
narrative_ontology:cs_reference_frame('823cfaa9-6af4-4c28-8bbf-e29cace34044', cold_war_mad_doctrine).
narrative_ontology:cs_drift_state('823cfaa9-6af4-4c28-8bbf-e29cace34044', contemporary_multi_polar_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('823cfaa9-6af4-4c28-8bbf-e29cace34044', '').
narrative_ontology:cs_kernel_id(total_war_possibility_space__deterrence_equilibrium_reading, total_war_possibility_space).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_possibility_space__deterrence_equilibrium_reading, nuclear_powers).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__deterrence_equilibrium_reading, global_population).
narrative_ontology:constraint_victim(total_war_possibility_space__deterrence_equilibrium_reading, taxpayers).
narrative_ontology:constraint_victim(total_war_possibility_space__deterrence_equilibrium_reading, populations_under_threat).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__deterrence_equilibrium_reading, non_nuclear_states).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__deterrence_equilibrium_reading, military_industrial_complex).
narrative_ontology:constraint_victim(total_war_possibility_space__deterrence_equilibrium_reading, non_nuclear_states).
narrative_ontology:constraint_victim(total_war_possibility_space__deterrence_equilibrium_reading, global_population).
narrative_ontology:constraint_vindicates(total_war_possibility_space__deterrence_equilibrium_reading, mutually_assured_destruction_doctrine).
narrative_ontology:constraint_vindicates(total_war_possibility_space__deterrence_equilibrium_reading, rational_deterrence_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain and develop nuclear arsenals, strategic doctrines, and command-and-control systems. They benefit from perceived security and global influence, but bear the immense costs of maintaining deterrence and the risk of accidental escalation. Their exit options are limited by the perceived need to maintain a credible deterrent.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, nuclear_powers, agenda_setter,
    institutional, generational, constrained, global).

% Benefit from the absence of total war between major powers, but often face regional instability, proxy conflicts, and the pressure to develop their own deterrents or align with nuclear powers. They bear indirect costs through global military spending and the constant threat of escalation.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, non_nuclear_states, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(total_war_possibility_space__deterrence_equilibrium_reading, non_nuclear_states, payer).

% Are the ultimate beneficiaries of total war being deterred, as their survival is at stake. However, they bear the costs through taxation for military budgets, live under the psychological burden of nuclear threat, and have no direct exit from the system of deterrence.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, global_population, beneficiary,
    powerless, generational, trapped, universal).
narrative_ontology:stakeholder_secondary_role(total_war_possibility_space__deterrence_equilibrium_reading, global_population, payer).

% Profits immensely from the continuous investment in war-fighting capabilities, research, and development required to maintain deterrence. They have significant influence on policy and benefit from the perpetuation of the deterrence paradigm.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, military_industrial_complex, beneficiary,
    institutional, generational, arbitrage, global).

% Argue for the abolition of nuclear weapons and alternative security paradigms. They are largely excluded from the core strategic decision-making processes of nuclear powers, but can mobilize public opinion and influence international treaties.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, disarmament_advocates, excluded,
    moderate, generational, mobile, global).

% Analyze, model, and debate the dynamics of deterrence, escalation, and strategic stability. They provide intellectual frameworks for policy-makers but do not directly control the levers of power.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, strategic_theorists, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents total war between major powers by establishing a mutual vulnerability that makes aggression prohibitively costly, thereby coordinating states towards non-direct conflict.
% TRANSFER_FUNCTION: Transfers vast resources (financial, intellectual, human capital) from national economies and global populations to the maintenance and development of military capabilities, intelligence, and strategic infrastructure, in exchange for perceived strategic stability and the avoidance of total war.
% ABSENT_VOICES: Future generations, who inherit the risks and costs of nuclear deterrence without having consented to it; populations in regions used for proxy conflicts or nuclear testing; and those who advocate for non-military security paradigms, are largely excluded from the core strategic discourse.
% DISAPPEARANCE_RATIONALE: If the mutual vulnerability and the deterrence it creates vanished overnight, the strategic calculus of major powers would fundamentally shift. The likelihood of large-scale conventional or even nuclear conflict would increase dramatically, leading to a rapid and profound reorganization of international relations, alliances, and national security priorities.
% FOUNDING_PROBLEM: Preventing a repeat of the devastating world wars of the 20th century, particularly after the advent of nuclear weapons made such conflicts potentially existential for humanity.
% FOUNDING_PROBLEM_CORROBORATION: International relations scholars, historians of the Cold War, and contemporary policy statements from non-nuclear states and international bodies (e.g., UN resolutions on disarmament) corroborate that the threat of total war, though deterred, remains a live concern, and the founding problem persists.
narrative_ontology:disappearance_verdict(total_war_possibility_space__deterrence_equilibrium_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_possibility_space__deterrence_equilibrium_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_possibility_space__deterrence_equilibrium_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(total_war_possibility_space__deterrence_equilibrium_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_possibility_space__deterrence_equilibrium_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_possibility_space__deterrence_equilibrium_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(total_war_possibility_space__deterrence_equilibrium_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(total_war_possibility_space__deterrence_equilibrium_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.65) reflects the immense and continuous costs of maintaining nuclear arsenals, delivery systems, and associated intelligence and command structures. Suppression (0.90) is very high because the threat of unacceptable retaliation effectively suppresses the alternative of total war. Theater ratio (0.40) is moderate; while there is genuine capability, much of the public display (military exercises, rhetoric) serves a performative signaling function to reinforce deterrence. Accessibility collapse (0.80) is high because the consequences of total war make it an effectively collapsed alternative for rational actors, though physically possible. Resistance (0.30) is moderate-low, as the system is largely accepted as a necessary evil, despite calls for disarmament.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of nuclear powers, this constraint is a necessary, albeit costly, mechanism for global stability. From the perspective of the global population or disarmament advocates, it is a dangerous, extractive system that perpetuates risk and diverts resources. The engine's per-seat classification will reflect these divergent experiences based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear powers act as agenda-setters, benefiting from perceived security and global influence, but also bearing the direct costs and risks. Non-nuclear states and the global population are beneficiaries of avoided total war, but also bear indirect costs and live under constant threat. The military-industrial complex is a clear beneficiary, profiting from the continuous arms race. Disarmament advocates are excluded from the core decision-making, and strategic theorists act as observers.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem of preventing total war remains 'live,' indicating that the constraint's mandate has not atrophied. However, the 'contested' status of the founding problem's solution (whether deterrence is the *best* or *only* solution) suggests that while the function persists, its justification is under continuous debate, preventing it from being a pure Rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_framing_ambiguity,
    'Is total war truly strategically reachable (as this reading claims), or has its possibility space contracted, or has it become normatively taboo?',
    'Historical analysis of near-misses, declassified strategic planning documents, and cross-cultural studies of normative constraints on warfare. If total war is found to be truly unthinkable or taboo, the constraint shifts from a Tangled Rope of active deterrence to a Mountain (space contraction) or Rope (nuclear taboo) of inherent impossibility or strong normative coordination.',
    'If total war is found to be truly unthinkable or taboo, the constraint shifts from a Tangled Rope of active deterrence to a Mountain (space contraction) or Rope (nuclear taboo) of inherent impossibility or strong normative coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_ambiguity, conceptual, 'Ambiguity regarding the true nature of total war''s possibility space.').

omega_variable(
    deterrence_stability_vs_arms_race,
    'Does the continuous investment in war-fighting capability, inherent to maintaining deterrence, lead to an unstable arms race that increases the risk of accidental war, or does it genuinely stabilize the system?',
    'Empirical studies of arms race dynamics, game theory modeling of escalation, and historical analysis of military-technical competition.',
    'If it leads to instability, the extractiveness and suppression metrics are understated, and the constraint''s classification leans more heavily towards Snare due to the self-perpetuating, high-risk costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_stability_vs_arms_race, empirical, 'Whether deterrence maintenance leads to stability or increased risk.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_possibility_space__deterrence_equilibrium_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 1945, 0.2).
narrative_ontology:measurement(tota_tr_t1960, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 1960, 0.3).
narrative_ontology:measurement(tota_tr_t1980, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 1980, 0.45).
narrative_ontology:measurement(tota_tr_t2000, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 2000, 0.4).
narrative_ontology:measurement(tota_tr_t2015, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 2015, 0.35).
narrative_ontology:measurement(tota_tr_t2025, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 2025, 0.4).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 1945, 0.5).
narrative_ontology:measurement(tota_be_t1960, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 1960, 0.6).
narrative_ontology:measurement(tota_be_t1980, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 1980, 0.7).
narrative_ontology:measurement(tota_be_t2000, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 2000, 0.68).
narrative_ontology:measurement(tota_be_t2015, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 2015, 0.63).
narrative_ontology:measurement(tota_be_t2025, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 2025, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1945, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 1945, 0.75).
narrative_ontology:measurement(tota_su_t1960, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 1960, 0.85).
narrative_ontology:measurement(tota_su_t1980, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 1980, 0.9).
narrative_ontology:measurement(tota_su_t2000, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 2000, 0.88).
narrative_ontology:measurement(tota_su_t2015, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 2015, 0.89).
narrative_ontology:measurement(tota_su_t2025, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 2025, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_possibility_space__deterrence_equilibrium_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(total_war_possibility_space__deterrence_equilibrium_reading, arms_control_treaties).
narrative_ontology:affects_constraint(total_war_possibility_space__deterrence_equilibrium_reading, non_proliferation_regime).
narrative_ontology:affects_constraint(total_war_possibility_space__deterrence_equilibrium_reading, proxy_wars_legitimacy).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
