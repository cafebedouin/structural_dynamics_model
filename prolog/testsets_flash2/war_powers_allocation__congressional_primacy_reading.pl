% ============================================================================
% CONSTRAINT STORY: war_powers_allocation__congressional_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_powers_allocation__congressional_primacy_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: war_powers_allocation__congressional_primacy_reading
 *   human_readable: Congressional Primacy in War Powers Authorization
 *   domain: constitutional_law/separation_of_powers/war_powers
 *
 * SUMMARY:
 *   This constraint represents the 'congressional primacy' reading of US war
 *   powers, asserting that military force beyond immediate defense requires
 *   explicit congressional authorization as a constitutional necessity. This
 *   reading views executive unilateral action as an extraction from
 *   congressional war power and emphasizes the legislative branch's role as a
 *   check on executive authority. The metrics reflect the ongoing struggle to
 *   enforce this reading against executive assertions of inherent authority.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_powers_allocation__congressional_primacy_reading, 0.7).
domain_priors:suppression_score(war_powers_allocation__congressional_primacy_reading, 0.85).
domain_priors:theater_ratio(war_powers_allocation__congressional_primacy_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_powers_allocation__congressional_primacy_reading, tangled_rope).
narrative_ontology:human_readable(war_powers_allocation__congressional_primacy_reading, "Congressional Primacy in War Powers Authorization").
narrative_ontology:topic_domain(war_powers_allocation__congressional_primacy_reading, "constitutional_law/separation_of_powers/war_powers").

domain_priors:requires_active_enforcement(war_powers_allocation__congressional_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_powers_allocation__congressional_primacy_reading, '3a495257-526f-4700-ab3f-8832d2467765').
narrative_ontology:cs_kernel_codification('3a495257-526f-4700-ab3f-8832d2467765', fixed_text).
narrative_ontology:cs_authority_grounding('3a495257-526f-4700-ab3f-8832d2467765', lineage).
narrative_ontology:cs_interpretation_layer_present('3a495257-526f-4700-ab3f-8832d2467765').
narrative_ontology:cs_reading_relation('3a495257-526f-4700-ab3f-8832d2467765', war_powers_allocation__inherent_executive_reading, coexists_with).
narrative_ontology:cs_reading_relation('3a495257-526f-4700-ab3f-8832d2467765', war_powers_allocation__functional_accommodation_reading, coexists_with).
narrative_ontology:cs_axiom('3a495257-526f-4700-ab3f-8832d2467765', foundational, congressional_declaration_prerequisite).
narrative_ontology:cs_axiom_status(congressional_declaration_prerequisite, holdable).
narrative_ontology:cs_axiom_grounding('3a495257-526f-4700-ab3f-8832d2467765', congressional_declaration_prerequisite, deontological).
narrative_ontology:cs_axiom('3a495257-526f-4700-ab3f-8832d2467765', foundational, executive_power_limited_to_defense).
narrative_ontology:cs_axiom_status(executive_power_limited_to_defense, holdable).
narrative_ontology:cs_axiom_grounding('3a495257-526f-4700-ab3f-8832d2467765', executive_power_limited_to_defense, deontological).
narrative_ontology:cs_reference_frame('3a495257-526f-4700-ab3f-8832d2467765', founding_constitutional_intent).
narrative_ontology:cs_drift_state('3a495257-526f-4700-ab3f-8832d2467765', contemporary_post_911_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3a495257-526f-4700-ab3f-8832d2467765', '').
narrative_ontology:cs_kernel_id(war_powers_allocation__congressional_primacy_reading, war_powers_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_powers_allocation__congressional_primacy_reading, congress).
narrative_ontology:constraint_beneficiary(war_powers_allocation__congressional_primacy_reading, constitutional_order).
narrative_ontology:constraint_victim(war_powers_allocation__congressional_primacy_reading, executive_branch).
narrative_ontology:constraint_victim(war_powers_allocation__congressional_primacy_reading, military_personnel).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the body constitutionally vested with the power to declare war, Congress is the primary beneficiary of this reading. It seeks to assert its authority over military deployments beyond immediate defense, viewing executive unilateral action as an infringement on its constitutional role. Its power is constrained by political will and the executive's ability to act unilaterally and present faits accomplis.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, congress, agenda_setter,
    institutional, generational, constrained, national).

% The executive branch, particularly the President as Commander-in-Chief, bears the cost of this constraint by being required to seek explicit authorization for military force. This reading limits the President's perceived flexibility and speed in responding to international crises, forcing a slower, more deliberative process. Exit options are constrained by constitutional norms and potential political backlash.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, executive_branch, payer,
    institutional, biographical, constrained, global).

% Military personnel are deployed under orders, regardless of the authorization source. This reading, when enforced, ensures their deployments are backed by a broader political consensus, theoretically reducing the risk of prolonged, unsupported conflicts. However, when the constraint is violated, they bear the direct costs of potentially unauthorized actions.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, military_personnel, payer,
    powerless, immediate, trapped, global).

% The abstract principle of separation of powers and checks and balances is reinforced by this reading. It ensures that the grave decision to commit military force is shared, preventing the concentration of war-making power in a single branch, thereby preserving the integrity of the constitutional framework.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, constitutional_order, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(war_powers_allocation__congressional_primacy_reading, constitutional_order).

% International allies observe the internal US debate on war powers. A clear congressional authorization provides greater legitimacy and stability to US military engagements, making alliances more predictable. Unilateral executive action, from this perspective, can create uncertainty and strain alliances.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, international_allies, observer,
    organized, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures that the decision to commit the nation to military force, a matter of profound national consequence, is made through a deliberative process involving both the legislative and executive branches, coordinating national will and resources.
% TRANSFER_FUNCTION: Transfers the authority to initiate military force from the executive's unilateral discretion to a shared power requiring explicit legislative consent, thereby transferring political risk and accountability to Congress.
% ABSENT_VOICES: Future generations, who bear the long-term costs of war, are structurally absent from the immediate decision-making process. Their interests are theoretically represented by Congress, but often overridden by short-term political expediency or executive urgency.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, the executive branch would likely assert even broader inherent authority, leading to more frequent and potentially less scrutinized military interventions. Congress would lose a critical check on executive power, fundamentally altering the balance of power and the nature of US foreign policy.
% FOUNDING_PROBLEM: The framers of the Constitution sought to prevent the concentration of war-making power in a single individual, fearing the abuses of monarchical power and ensuring that the decision to go to war reflected the will of the people through their representatives.
% FOUNDING_PROBLEM_CORROBORATION: Historians and constitutional scholars widely corroborate the framers' intent to divide war powers. Congressional leaders and legal experts outside the executive branch consistently argue that the founding problem remains live, citing ongoing executive assertions of unilateral authority as evidence of its persistence.
narrative_ontology:disappearance_verdict(war_powers_allocation__congressional_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(war_powers_allocation__congressional_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_powers_allocation__congressional_primacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(war_powers_allocation__congressional_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(war_powers_allocation__congressional_primacy_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_powers_allocation__congressional_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_powers_allocation__congressional_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(war_powers_allocation__congressional_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.7) because the executive frequently bypasses or reinterprets congressional authorization, effectively extracting the power to deploy force. Suppression is very high (0.85) due to the executive's institutional capacity to act quickly and unilaterally, often presenting Congress with a fait accompli, thereby suppressing its deliberative role. Theater ratio is moderate (0.4) as Congress often engages in performative debates or passes non-binding resolutions without effectively halting executive action, maintaining the appearance of oversight without full functional control.
 *
 * PERSPECTIVAL GAP:
 *   From Congress's perspective, this constraint is a vital Rope, ensuring proper constitutional process. From the executive's perspective, it is a Snare, unduly restricting its ability to protect national interests. The engine's classification will reflect the structural realities of extraction and suppression, which this reading asserts are high.
 *
 * DIRECTIONALITY LOGIC:
 *   Congress and the constitutional order are beneficiaries, as this reading aims to preserve their authority and the system of checks and balances. The executive branch is the primary target, as its power to act unilaterally is constrained. Military personnel are also targets, as they are deployed regardless of the authorization source, bearing the direct costs of potentially unauthorized actions. International allies are observers, affected by the legitimacy and stability of US foreign policy.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    executive_unilateralism_legitimacy,
    'To what extent do historical precedents and evolving international threats legitimize executive unilateral action in the absence of explicit congressional authorization?',
    'Judicial review of specific executive actions, or a constitutional amendment clarifying war powers. Absent these, ongoing political contestation and public opinion shifts.',
    'If executive unilateralism is deemed legitimate, the extractiveness of this constraint (from Congress) would decrease, potentially reclassifying it towards a more balanced Rope or even a Mountain (if inherent executive power is seen as natural law). If illegitimate, the current high extractiveness and suppression are further validated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(executive_unilateralism_legitimacy, conceptual, 'Ambiguity regarding the constitutional legitimacy of executive unilateral action in war powers.').

omega_variable(
    congressional_will_vs_capacity,
    'Is the observed ''suppression'' of congressional war powers due to executive overreach, or to Congress''s own institutional reluctance and lack of political will to assert its authority?',
    'Analysis of congressional voting records on war authorizations, legislative efforts to reclaim war powers, and internal congressional debates on asserting authority. Compare with periods of strong congressional assertion.',
    'If congressional reluctance is the primary driver, the ''suppression'' metric might be reinterpreted as a form of self-imposed constraint or a Piton, where Congress allows its power to atrophy. If executive overreach is dominant, the current classification as a Tangled Rope (or Snare) is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(congressional_will_vs_capacity, empirical, 'Distinguishing between executive suppression and congressional self-limitation in war powers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_powers_allocation__congressional_primacy_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t0, war_powers_allocation__congressional_primacy_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(war__tr_t10, war_powers_allocation__congressional_primacy_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(war__tr_t20, war_powers_allocation__congressional_primacy_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(war__tr_t30, war_powers_allocation__congressional_primacy_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement(war__tr_t40, war_powers_allocation__congressional_primacy_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement(war__tr_t50, war_powers_allocation__congressional_primacy_reading, theater_ratio, 50, 0.4).

% Extraction over time
narrative_ontology:measurement(war__be_t0, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(war__be_t10, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(war__be_t20, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(war__be_t30, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(war__be_t40, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 40, 0.7).
narrative_ontology:measurement(war__be_t50, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 50, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(war__su_t0, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(war__su_t10, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 10, 0.75).
narrative_ontology:measurement(war__su_t20, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 20, 0.8).
narrative_ontology:measurement(war__su_t30, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 30, 0.83).
narrative_ontology:measurement(war__su_t40, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 40, 0.85).
narrative_ontology:measurement(war__su_t50, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 50, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_powers_allocation__congressional_primacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(war_powers_allocation__congressional_primacy_reading, war_powers_allocation__inherent_executive_reading).
narrative_ontology:affects_constraint(war_powers_allocation__congressional_primacy_reading, war_powers_allocation__functional_accommodation_reading).
narrative_ontology:affects_constraint(war_powers_allocation__congressional_primacy_reading, presidential_emergency_powers).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'war_powers_allocation' kernel. This 'congressional primacy' reading emphasizes legislative control, contrasting with the 'inherent executive' and 'functional accommodation' readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
