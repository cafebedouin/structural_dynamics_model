% ============================================================================
% CONSTRAINT STORY: total_war_possibility_space__nuclear_taboo_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_possibility_space__nuclear_taboo_reading, []).

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
 *   constraint_id: total_war_possibility_space__nuclear_taboo_reading
 *   human_readable: Nuclear Taboo on Total War
 *   domain: international_relations/strategic_studies
 *
 * SUMMARY:
 *   This constraint describes the 'nuclear taboo' reading of the total war
 *   possibility space, where total war involving nuclear weapons became
 *   normatively prohibited through a constructed taboo, independent of
 *   material capability. While the material possibility of total war remains,
 *   the normative barrier makes it unthinkable for most state actors. This
 *   reading emphasizes the role of norm entrepreneurs and international
 *   institutions in maintaining this constraint. The claimed type is 'rope'
 *   because it genuinely coordinates global behavior for collective benefit,
 *   but the metrics reflect the ongoing effort and cost of maintaining the
 *   taboo against material possibility.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_possibility_space__nuclear_taboo_reading, 0.4).
domain_priors:suppression_score(total_war_possibility_space__nuclear_taboo_reading, 0.7).
domain_priors:theater_ratio(total_war_possibility_space__nuclear_taboo_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_possibility_space__nuclear_taboo_reading, rope).
narrative_ontology:human_readable(total_war_possibility_space__nuclear_taboo_reading, "Nuclear Taboo on Total War").
narrative_ontology:topic_domain(total_war_possibility_space__nuclear_taboo_reading, "international_relations/strategic_studies").

domain_priors:requires_active_enforcement(total_war_possibility_space__nuclear_taboo_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_possibility_space__nuclear_taboo_reading, '45e682cb-2d99-4a76-b226-17947bad82a0').
narrative_ontology:cs_kernel_codification('45e682cb-2d99-4a76-b226-17947bad82a0', distributed).
narrative_ontology:cs_authority_grounding('45e682cb-2d99-4a76-b226-17947bad82a0', practice).
narrative_ontology:cs_interpretation_layer_present('45e682cb-2d99-4a76-b226-17947bad82a0').
narrative_ontology:cs_reading_relation('45e682cb-2d99-4a76-b226-17947bad82a0', total_war_possibility_space__deterrence_equilibrium_reading, coexists_with).
narrative_ontology:cs_reading_relation('45e682cb-2d99-4a76-b226-17947bad82a0', total_war_possibility_space__space_contraction_reading, coexists_with).
narrative_ontology:cs_axiom('45e682cb-2d99-4a76-b226-17947bad82a0', foundational, nuclear_weapons_are_unusable).
narrative_ontology:cs_axiom_status(nuclear_weapons_are_unusable, holdable).
narrative_ontology:cs_axiom_grounding('45e682cb-2d99-4a76-b226-17947bad82a0', nuclear_weapons_are_unusable, deontological).
narrative_ontology:cs_axiom('45e682cb-2d99-4a76-b226-17947bad82a0', foundational, normative_restraint_is_causal).
narrative_ontology:cs_axiom_status(normative_restraint_is_causal, holdable).
narrative_ontology:cs_axiom_grounding('45e682cb-2d99-4a76-b226-17947bad82a0', normative_restraint_is_causal, empirically_contingent).
narrative_ontology:cs_reference_frame('45e682cb-2d99-4a76-b226-17947bad82a0', post_hiroshima_normative_shock).
narrative_ontology:cs_drift_state('45e682cb-2d99-4a76-b226-17947bad82a0', contemporary_geopolitical_tensions, gap(repudiation_pressure, minor, true)).
narrative_ontology:cs_created_at('45e682cb-2d99-4a76-b226-17947bad82a0', '').
narrative_ontology:cs_kernel_id(total_war_possibility_space__nuclear_taboo_reading, total_war_possibility_space).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, global_population).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, non_nuclear_states).
narrative_ontology:constraint_victim(total_war_possibility_space__nuclear_taboo_reading, military_planners_of_nuclear_powers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the absence of total war, which would entail catastrophic human cost. Has no direct agency in maintaining the taboo but is the ultimate beneficiary of its persistence.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, global_population, beneficiary,
    powerless, generational, trapped, global).

% Benefits from the normative constraint on nuclear powers, reducing the risk of existential conflict. Actively participates in non-proliferation regimes and diplomatic efforts to reinforce the taboo.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, non_nuclear_states, beneficiary,
    organized, generational, constrained, global).

% Are constrained in their strategic options, unable to credibly plan for or execute total war scenarios involving nuclear weapons due to the normative prohibition. Their professional identity is shaped by this constraint, making exit from the taboo difficult.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, military_planners_of_nuclear_powers, payer,
    institutional, biographical, identity_locked, global).

% Actively construct, maintain, and enforce the nuclear taboo through international treaties, diplomatic pressure, and public advocacy. Their careers and institutional legitimacy are tied to the persistence of this normative framework.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, norm_entrepreneurs_and_diplomats, agenda_setter,
    institutional, biographical, constrained, global).

% Are outside the normative framework of the taboo and may seek to acquire or use nuclear weapons, challenging the global consensus. Their actions are met with international condemnation and sanctions, reinforcing the taboo for others.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, rogue_states_and_non_state_actors, excluded,
    moderate, immediate, mobile, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global state behavior by establishing a shared normative boundary against the use of nuclear weapons in total war, preventing escalation and promoting non-proliferation.
% TRANSFER_FUNCTION: Transfers the cost of foregone strategic options from nuclear powers to the global population and non-nuclear states, who receive the benefit of reduced existential risk.
% ABSENT_VOICES: Rogue states and non-state actors who do not adhere to the taboo would argue for the right to use all available means for self-preservation or achieving their objectives, but they are excluded from the normative discourse.
% DISAPPEARANCE_RATIONALE: If the nuclear taboo vanished overnight, the strategic landscape would fundamentally shift. Nuclear weapons would become thinkable as first-strike or war-winning instruments, leading to a rapid arms race, increased proliferation, and a much higher probability of catastrophic conflict. Global security architectures would collapse.
% FOUNDING_PROBLEM: The existential threat posed by nuclear weapons after Hiroshima and Nagasaki, where the material capability for total annihilation outpaced any normative or strategic framework for restraint.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the Cold War, international relations scholars, and former diplomats widely corroborate the emergence of the taboo as a response to the unique destructive power of nuclear weapons, distinct from mere deterrence. The ongoing non-proliferation efforts and the lack of nuclear use since 1945 serve as empirical corroboration.
narrative_ontology:disappearance_verdict(total_war_possibility_space__nuclear_taboo_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_possibility_space__nuclear_taboo_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_possibility_space__nuclear_taboo_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-04',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(total_war_possibility_space__nuclear_taboo_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_possibility_space__nuclear_taboo_reading, 0.4, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_possibility_space__nuclear_taboo_reading_tests).
:- end_tests(total_war_possibility_space__nuclear_taboo_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.4) is moderate, representing the cost to military planners of foregone strategic options and the resources invested in non-proliferation. Suppression (0.7) is high, as the taboo requires active enforcement through diplomatic pressure, sanctions, and the non-proliferation regime to prevent its erosion. Theater ratio (0.2) is low, indicating that the efforts to maintain the taboo are largely functional, though some performative aspects exist in diplomatic rhetoric. Accessibility collapse (0.6) is moderate, as the material possibility of total war still exists, but the normative barrier makes it difficult to access. Resistance (0.1) is low from within the normative framework, but high from excluded actors.
 *
 * PERSPECTIVAL GAP:
 *   Military planners, particularly those in nuclear states, experience this as a significant constraint on their freedom of action, while the global population experiences it as a vital safeguard. The 'nuclear taboo' reading highlights this normative imposition, whereas a 'deterrence equilibrium' reading would frame it as a rational strategic choice.
 *
 * DIRECTIONALITY LOGIC:
 *   The global population and non-nuclear states are beneficiaries, as they are protected from existential threat. Military planners of nuclear powers are payers, as their strategic options are constrained. Norm entrepreneurs and diplomats are agenda-setters, actively shaping and enforcing the taboo. Rogue states and non-state actors are excluded, as they operate outside the normative framework and challenge its legitimacy.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (preventing nuclear total war) remains highly live. The classification as 'rope' prevents mislabeling it as pure extraction, acknowledging its genuine coordination function, while the metrics capture the ongoing effort and cost of maintaining this normative barrier. If the taboo were to weaken, the extractiveness might decrease for military planners (more options) but the overall global cost would skyrocket, indicating a shift towards a 'snare' for the global population.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    taboo_strength_measurement,
    'How can the strength of the nuclear taboo be empirically measured, beyond the absence of nuclear use?',
    'Analysis of state rhetoric, military doctrine, public opinion surveys, and diplomatic negotiations for shifts in normative language and commitment to non-use principles.',
    'A robust measurement would allow for more precise tracking of the taboo''s persistence and potential erosion, informing policy interventions to reinforce it. If the taboo is weaker than assumed, the effective extractiveness on military planners might be lower, but the global risk higher.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(taboo_strength_measurement, empirical, 'Empirical indicators for the normative strength of the nuclear taboo.').

omega_variable(
    taboo_vs_deterrence_causality,
    'To what extent is the absence of total war due to the nuclear taboo, versus the material reality of deterrence?',
    'Counterfactual historical analysis, comparative case studies of non-nuclear existential threats, and expert elicitation on decision-making processes during crises.',
    'If deterrence is the primary driver, this constraint might be reclassified closer to a ''mountain'' (material reality) or a ''tangled_rope'' (strategic coordination with coercive elements). If the taboo is primary, the ''rope'' classification is reinforced, emphasizing the constructed normative element.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(taboo_vs_deterrence_causality, conceptual, 'Distinguishing the causal weight of normative taboo versus material deterrence in preventing total war.').

omega_variable(
    non_nuclear_powers_taboo_adherence,
    'Does the nuclear taboo apply symmetrically to non-nuclear states considering nuclear acquisition, or is it primarily a constraint on existing nuclear powers?',
    'Examination of the non-proliferation regime''s enforcement mechanisms and the normative justifications for preventing proliferation versus preventing use by established powers.',
    'If the taboo is primarily a constraint on existing nuclear powers, non-nuclear states might face a different, more extractive constraint (e.g., a ''snare'' of non-proliferation enforced by nuclear powers), leading to a decomposition of this constraint into multiple, linked stories.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_nuclear_powers_taboo_adherence, empirical, 'Symmetry of the nuclear taboo''s application across nuclear and non-nuclear states.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_possibility_space__nuclear_taboo_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 1945, 0.1).
narrative_ontology:measurement(tota_tr_t1960, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 1960, 0.15).
narrative_ontology:measurement(tota_tr_t1980, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 1980, 0.2).
narrative_ontology:measurement(tota_tr_t2000, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 2000, 0.2).
narrative_ontology:measurement(tota_tr_t2010, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 2010, 0.18).
narrative_ontology:measurement(tota_tr_t2024, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 1945, 0.3).
narrative_ontology:measurement(tota_be_t1960, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 1960, 0.4).
narrative_ontology:measurement(tota_be_t1980, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 1980, 0.45).
narrative_ontology:measurement(tota_be_t2000, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 2000, 0.4).
narrative_ontology:measurement(tota_be_t2010, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 2010, 0.38).
narrative_ontology:measurement(tota_be_t2024, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 2024, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1945, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 1945, 0.5).
narrative_ontology:measurement(tota_su_t1960, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 1960, 0.65).
narrative_ontology:measurement(tota_su_t1980, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 1980, 0.75).
narrative_ontology:measurement(tota_su_t2000, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 2000, 0.7).
narrative_ontology:measurement(tota_su_t2010, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 2010, 0.68).
narrative_ontology:measurement(tota_su_t2024, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_possibility_space__nuclear_taboo_reading, identity_coordination).
narrative_ontology:affects_constraint(total_war_possibility_space__nuclear_taboo_reading, non_proliferation_treaty_regime).
narrative_ontology:affects_constraint(total_war_possibility_space__nuclear_taboo_reading, arms_control_agreements).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'total_war_possibility_space' kernel. It focuses on the normative prohibition (taboo) as the primary constraint, distinct from material deterrence or strategic space contraction. Sibling readings (deterrence_equilibrium_reading, space_contraction_reading) offer alternative explanations for the absence of total war.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
