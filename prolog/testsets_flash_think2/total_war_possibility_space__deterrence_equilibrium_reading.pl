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
 *   constraint_id: total_war_possibility_space__deterrence_equilibrium_reading
 *   human_readable: Deterrence Equilibrium for Total War
 *   domain: international_relations_theory/strategic_studies
 *
 * SUMMARY:
 *   This constraint describes the strategic reality where total war, while
 *   technically feasible, is prevented by the mutual vulnerability of major
 *   powers to catastrophic destruction. It is a
 *   'deterrence_equilibrium_reading' of the broader
 *   'total_war_possibility_space' kernel, emphasizing the rational
 *   calculation of costs and benefits that underpins strategic stability.
 *   This reading posits that the constraint is maintained through continuous
 *   investment in military capabilities and credible threats of retaliation,
 *   rather than through normative shifts or a fundamental alteration of the
 *   strategic space itself.
 *
 * KEY AGENTS:
 *   - nuclear_powers: Agenda-setter/Beneficiary (institutional/constrained)
 *   - taxpayers_of_nuclear_states: Payer (powerless/trapped)
 *   - populations_at_risk: Payer (powerless/trapped)
 *   - military_industrial_complexes: Beneficiary (institutional/arbitrage)
 *   - strategic_analysts: Observer (analytical/analytical)
 *   - disarmament_advocates: Excluded (moderate/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_possibility_space__deterrence_equilibrium_reading, 0.78).
domain_priors:suppression_score(total_war_possibility_space__deterrence_equilibrium_reading, 0.92).
domain_priors:theater_ratio(total_war_possibility_space__deterrence_equilibrium_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_possibility_space__deterrence_equilibrium_reading, tangled_rope).
narrative_ontology:human_readable(total_war_possibility_space__deterrence_equilibrium_reading, "Deterrence Equilibrium for Total War").
narrative_ontology:topic_domain(total_war_possibility_space__deterrence_equilibrium_reading, "international_relations_theory/strategic_studies").

domain_priors:requires_active_enforcement(total_war_possibility_space__deterrence_equilibrium_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_possibility_space__deterrence_equilibrium_reading, '7833d355-f031-40ac-bc1b-4cec8948b890').
narrative_ontology:cs_kernel_codification('7833d355-f031-40ac-bc1b-4cec8948b890', implicit).
narrative_ontology:cs_authority_grounding('7833d355-f031-40ac-bc1b-4cec8948b890', self_enforcing).
narrative_ontology:cs_reading_relation('7833d355-f031-40ac-bc1b-4cec8948b890', total_war_possibility_space__nuclear_taboo_reading, coexists_with).
narrative_ontology:cs_reading_relation('7833d355-f031-40ac-bc1b-4cec8948b890', total_war_possibility_space__space_contraction_reading, coexists_with).
narrative_ontology:cs_axiom('7833d355-f031-40ac-bc1b-4cec8948b890', foundational, rational_actor_maximization).
narrative_ontology:cs_axiom_status(rational_actor_maximization, holdable).
narrative_ontology:cs_axiom_grounding('7833d355-f031-40ac-bc1b-4cec8948b890', rational_actor_maximization, empirically_contingent).
narrative_ontology:cs_axiom('7833d355-f031-40ac-bc1b-4cec8948b890', foundational, mutual_assured_destruction_credible).
narrative_ontology:cs_axiom_status(mutual_assured_destruction_credible, holdable).
narrative_ontology:cs_axiom_grounding('7833d355-f031-40ac-bc1b-4cec8948b890', mutual_assured_destruction_credible, empirically_contingent).
narrative_ontology:cs_reference_frame('7833d355-f031-40ac-bc1b-4cec8948b890', cold_war_strategic_stability).
narrative_ontology:cs_drift_state('7833d355-f031-40ac-bc1b-4cec8948b890', post_cold_war_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('7833d355-f031-40ac-bc1b-4cec8948b890', '').
narrative_ontology:cs_kernel_id(total_war_possibility_space__deterrence_equilibrium_reading, total_war_possibility_space).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_possibility_space__deterrence_equilibrium_reading, nuclear_powers).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__deterrence_equilibrium_reading, military_industrial_complexes).
narrative_ontology:constraint_victim(total_war_possibility_space__deterrence_equilibrium_reading, taxpayers_of_nuclear_states).
narrative_ontology:constraint_victim(total_war_possibility_space__deterrence_equilibrium_reading, populations_at_risk).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__deterrence_equilibrium_reading, non_nuclear_states).
narrative_ontology:constraint_vindicates(total_war_possibility_space__deterrence_equilibrium_reading, global_stability_doctrine).
narrative_ontology:constraint_vindicates(total_war_possibility_space__deterrence_equilibrium_reading, peace_through_strength_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States possessing nuclear weapons that actively maintain and modernize their arsenals, articulate deterrence doctrines, and engage in strategic signaling. They benefit from avoiding total war but bear the immense costs and risks of maintaining the deterrence system.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, nuclear_powers, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(total_war_possibility_space__deterrence_equilibrium_reading, nuclear_powers, beneficiary).

% States without nuclear weapons that benefit from the global stability provided by deterrence, but are also subject to the strategic dynamics and risks of nuclear confrontation. They have limited agency in shaping the constraint.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, non_nuclear_states, beneficiary,
    organized, biographical, constrained, global).

% Bear the direct financial costs of maintaining nuclear arsenals and associated military infrastructure through taxation. They have no direct exit from these costs.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, taxpayers_of_nuclear_states, payer,
    powerless, immediate, trapped, national).

% Live under the constant existential threat of nuclear war, bearing the psychological and social costs of mutual vulnerability. They have no direct exit from this condition.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, populations_at_risk, payer,
    powerless, biographical, trapped, global).

% Industries and institutions that profit from the continuous development, production, and maintenance of nuclear weapons and conventional military capabilities essential for deterrence. They are a primary recipient of the financial extraction.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, military_industrial_complexes, beneficiary,
    institutional, generational, arbitrage, national).

% Academics, think-tank researchers, and government advisors who study, theorize, and model the dynamics of deterrence, arms control, and strategic stability. They analyze the constraint's operation without directly participating in its enforcement or bearing its primary costs.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, strategic_analysts, observer,
    analytical, generational, analytical, global).

% Individuals and organizations who argue for the abolition of nuclear weapons and the dismantling of deterrence structures. They are largely excluded from the core decision-making processes of nuclear powers but exert moral and political pressure.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, disarmament_advocates, excluded,
    moderate, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(total_war_possibility_space__deterrence_equilibrium_reading, military_industrial_complexes).
narrative_ontology:fixing_cost_class(total_war_possibility_space__deterrence_equilibrium_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To prevent total war, particularly nuclear war, by establishing a credible threat of unacceptable retaliation, thereby making such a conflict strategically irrational for all parties.
% TRANSFER_FUNCTION: Transfers vast financial resources from national taxpayers to military-industrial complexes for the maintenance and modernization of nuclear arsenals. It also transfers the psychological burden of existential risk to global populations and limits the strategic autonomy of states.
% ABSENT_VOICES: Disarmament advocates, future generations, and populations in potential target zones are largely excluded from the strategic calculus, though their concerns are sometimes acknowledged rhetorically. They would argue for alternative security paradigms not based on mutual threat.
% DISAPPEARANCE_RATIONALE: If the deterrence equilibrium vanished overnight (e.g., through a sudden, universal loss of nuclear capability or credibility), the international strategic landscape would fundamentally reorganize. It could lead to rapid conventional arms races, new forms of total war, or a scramble for new deterrents, as the primary constraint on large-scale conflict would be removed.
% FOUNDING_PROBLEM: The problem of preventing a repeat of the devastating total wars of the 20th century, especially after the advent of nuclear weapons made such conflicts potentially civilization-ending.
% FOUNDING_PROBLEM_CORROBORATION: International relations scholars, military strategists, and historical analyses widely corroborate the founding problem's existence and its ongoing relevance. While the specific dynamics of deterrence evolve, the core problem of preventing total war remains central to strategic thought, attested by numerous academic publications, government white papers, and expert consensus.
narrative_ontology:disappearance_verdict(total_war_possibility_space__deterrence_equilibrium_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_possibility_space__deterrence_equilibrium_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_possibility_space__deterrence_equilibrium_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(total_war_possibility_space__deterrence_equilibrium_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_possibility_space__deterrence_equilibrium_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

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
 *   Base extractiveness is high (0.78) due to the immense financial and opportunity costs of maintaining nuclear arsenals and the constant psychological burden of existential threat. Suppression is very high (0.92) because the threat of mutual annihilation is the ultimate coercive force, effectively collapsing the rational alternative of total war. Theater ratio is moderate (0.40); while much of deterrence involves signaling and posturing, the underlying destructive capability and the credible will to use it (if deterrence fails) are very real. Accessibility collapse is high (0.90) as the catastrophic consequences make total war an irrational option. Resistance is low (0.15) because, while there is resistance to the *costs* of deterrence (e.g., arms races), the principle of deterrence itself is widely accepted by states as a necessary evil for survival.
 *
 * PERSPECTIVAL GAP:
 *   Nuclear powers and military-industrial complexes perceive this constraint as a necessary, albeit costly, mechanism for peace and security, a 'tangled rope' that coordinates away from catastrophe. For taxpayers and populations at risk, it functions more as a 'snare,' extracting resources and imposing existential threat with little agency or direct benefit. Strategic analysts view it as a complex, dynamic system, while disarmament advocates see it as an inherently flawed and dangerous 'snare' that should be dismantled.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear powers are beneficiaries as they avoid total war and maintain their security, but also agenda-setters as they actively manage the system. Military-industrial complexes are clear beneficiaries, receiving the financial transfers. Taxpayers and populations at risk are primary targets/payers, bearing the financial and existential costs. Non-nuclear states are diffuse beneficiaries of stability but also subject to the risks. Disarmament advocates are excluded, as their proposals challenge the core premise of the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a Tangled Rope prevents mislabeling it as a pure Rope (which would ignore the massive extraction and active enforcement) or a pure Snare (which would ignore its genuine, albeit costly, coordination function in preventing total war). It acknowledges the dual nature: a system that coordinates global powers away from catastrophic conflict, but does so through a highly extractive and suppressive mechanism that benefits specific institutional actors while imposing costs on others.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_stability_ambiguity,
    'Is the deterrence equilibrium truly stable and robust, or is it inherently fragile and prone to collapse under specific conditions (e.g., irrational actors, technological shifts, miscalculation)?',
    'Empirical analysis of historical crises, game-theoretic modeling under various assumptions, and observation of state behavior in emerging strategic environments. Resolution would clarify the true risk profile.',
    'If fragile, the constraint''s effective suppression and extractiveness are higher than measured, as the system is constantly on the brink of catastrophic failure. If robust, the current metrics accurately reflect a stable, albeit costly, coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_stability_ambiguity, empirical, 'Uncertainty regarding the long-term stability of the deterrence equilibrium.').

omega_variable(
    rational_actor_assumption_validity,
    'Is the foundational assumption of rational actor behavior, central to deterrence theory, universally valid across all state and non-state actors, and under all conditions (e.g., extreme stress, regime change)?',
    'Historical case studies of decision-making under crisis, psychological studies of leadership, and analysis of non-state actor motivations. Resolution would challenge or affirm the core premise.',
    'If the rational actor assumption is frequently violated, the constraint''s coordination function is weaker and its persistence relies more heavily on pure suppression, potentially shifting its classification towards a Snare for certain actors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rational_actor_assumption_validity, conceptual, 'Validity of the rational actor assumption in deterrence theory.').

omega_variable(
    suppression_mechanism_ambiguity,
    'To what extent is the suppression of total war purely external (the threat of retaliation) versus internalized (a normative taboo against using nuclear weapons)?',
    'Comparative analysis with the ''nuclear_taboo_reading'' and ''space_contraction_reading'' of the same kernel. If the constraint persists even when material threats are ambiguous, internalized suppression is stronger. If it weakens with perceived threat, external suppression dominates.',
    'If suppression is significantly internalized, the constraint might be more stable and less reliant on active enforcement, potentially reducing its effective extractiveness over time. If purely external, it remains a high-cost, high-enforcement Tangled Rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, conceptual, 'Structural vs. internalized suppression mechanism for total war.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_possibility_space__deterrence_equilibrium_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 1945, 0.2).
narrative_ontology:measurement(tota_tr_t1965, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 1965, 0.3).
narrative_ontology:measurement(tota_tr_t1985, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 1985, 0.45).
narrative_ontology:measurement(tota_tr_t2005, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 2005, 0.35).
narrative_ontology:measurement(tota_tr_t2025, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 2025, 0.4).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 1945, 0.6).
narrative_ontology:measurement(tota_be_t1965, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 1965, 0.75).
narrative_ontology:measurement(tota_be_t1985, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 1985, 0.85).
narrative_ontology:measurement(tota_be_t2005, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 2005, 0.7).
narrative_ontology:measurement(tota_be_t2025, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 2025, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1945, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 1945, 0.7).
narrative_ontology:measurement(tota_su_t1965, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 1965, 0.85).
narrative_ontology:measurement(tota_su_t1985, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 1985, 0.95).
narrative_ontology:measurement(tota_su_t2005, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 2005, 0.88).
narrative_ontology:measurement(tota_su_t2025, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 2025, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_possibility_space__deterrence_equilibrium_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(total_war_possibility_space__deterrence_equilibrium_reading, nuclear_proliferation_regime).
narrative_ontology:affects_constraint(total_war_possibility_space__deterrence_equilibrium_reading, arms_control_treaties).
narrative_ontology:affects_constraint(total_war_possibility_space__deterrence_equilibrium_reading, total_war_possibility_space__nuclear_taboo_reading).
narrative_ontology:affects_constraint(total_war_possibility_space__deterrence_equilibrium_reading, total_war_possibility_space__space_contraction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'total_war_possibility_space' kernel, focusing on deterrence through mutual vulnerability. It is linked to sibling readings that emphasize normative taboo or strategic space contraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
