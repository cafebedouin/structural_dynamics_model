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
 *   constraint_id: war_powers_allocation__congressional_primacy_reading
 *   human_readable: Congressional Primacy in War Powers Authorization
 *   domain: Constitutional Law/Separation of Powers/War Powers
 *
 * SUMMARY:
 *   This constraint story instantiates the 'congressional primacy' reading of
 *   the war powers allocation kernel. It asserts that military force beyond
 *   immediate defense constitutionally requires explicit congressional
 *   authorization. This reading views executive unilateral action as an
 *   extraction of power from the legislative branch, necessitating high
 *   suppression of executive claims to inherent authority. The metrics
 *   reflect the ongoing struggle to enforce this constitutional principle
 *   against executive actions that often bypass formal authorization.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_powers_allocation__congressional_primacy_reading, 0.78).
domain_priors:suppression_score(war_powers_allocation__congressional_primacy_reading, 0.85).
domain_priors:theater_ratio(war_powers_allocation__congressional_primacy_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_powers_allocation__congressional_primacy_reading, tangled_rope).
narrative_ontology:human_readable(war_powers_allocation__congressional_primacy_reading, "Congressional Primacy in War Powers Authorization").
narrative_ontology:topic_domain(war_powers_allocation__congressional_primacy_reading, "Constitutional Law/Separation of Powers/War Powers").

domain_priors:requires_active_enforcement(war_powers_allocation__congressional_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_powers_allocation__congressional_primacy_reading, '12804237-c6cf-4bac-b12e-59119031dfd5').
narrative_ontology:cs_kernel_codification('12804237-c6cf-4bac-b12e-59119031dfd5', fixed_text).
narrative_ontology:cs_authority_grounding('12804237-c6cf-4bac-b12e-59119031dfd5', lineage).
narrative_ontology:cs_interpretation_layer_present('12804237-c6cf-4bac-b12e-59119031dfd5').
narrative_ontology:cs_reading_relation('12804237-c6cf-4bac-b12e-59119031dfd5', war_powers_allocation__inherent_executive_reading, forecloses).
narrative_ontology:cs_reading_relation('12804237-c6cf-4bac-b12e-59119031dfd5', war_powers_allocation__functional_accommodation_reading, influences).
narrative_ontology:cs_axiom('12804237-c6cf-4bac-b12e-59119031dfd5', foundational, congressional_declaration_of_war_is_sole_legitimate_basis_for_offensive_force).
narrative_ontology:cs_axiom_status(congressional_declaration_of_war_is_sole_legitimate_basis_for_offensive_force, holdable).
narrative_ontology:cs_axiom_grounding('12804237-c6cf-4bac-b12e-59119031dfd5', congressional_declaration_of_war_is_sole_legitimate_basis_for_offensive_force, deontological).
narrative_ontology:cs_axiom('12804237-c6cf-4bac-b12e-59119031dfd5', secondary, executive_power_as_commander_in_chief_is_subordinate_to_congressional_war_declaration).
narrative_ontology:cs_axiom_status(executive_power_as_commander_in_chief_is_subordinate_to_congressional_war_declaration, holdable).
narrative_ontology:cs_axiom_grounding('12804237-c6cf-4bac-b12e-59119031dfd5', executive_power_as_commander_in_chief_is_subordinate_to_congressional_war_declaration, deontological).
narrative_ontology:cs_reference_frame('12804237-c6cf-4bac-b12e-59119031dfd5', constitutional_original_intent_framing).
narrative_ontology:cs_drift_state('12804237-c6cf-4bac-b12e-59119031dfd5', contemporary_executive_action_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('12804237-c6cf-4bac-b12e-59119031dfd5', '').
narrative_ontology:cs_kernel_id(war_powers_allocation__congressional_primacy_reading, war_powers_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_powers_allocation__congressional_primacy_reading, congress).
narrative_ontology:constraint_beneficiary(war_powers_allocation__congressional_primacy_reading, american_public).
narrative_ontology:constraint_victim(war_powers_allocation__congressional_primacy_reading, executive_branch).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(war_powers_allocation__congressional_primacy_reading, american_public).
narrative_ontology:constraint_victim(war_powers_allocation__congressional_primacy_reading, military_personnel).
narrative_ontology:constraint_vindicates(war_powers_allocation__congressional_primacy_reading, separation_of_powers_doctrine).
narrative_ontology:constraint_vindicates(war_powers_allocation__congressional_primacy_reading, checks_and_balances_principle).
narrative_ontology:constraint_vindicates(war_powers_allocation__congressional_primacy_reading, democratic_accountability_for_war).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the body constitutionally vested with the power to declare war, Congress is meant to authorize military force beyond immediate defense. When the executive acts unilaterally, Congress's power is bypassed, and its constitutional role is diminished. Its 'exit' is to assert its authority, which often leads to political conflict.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, congress, agenda_setter,
    institutional, generational, constrained, national).

% The executive branch, particularly the President as Commander-in-Chief, bears the 'cost' of needing explicit congressional authorization for military actions. From this reading's perspective, unilateral executive action constitutes an extraction of power from Congress. The executive's 'exit' is to claim inherent authority, which is often met with legal and political challenge.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, executive_branch, payer,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(war_powers_allocation__congressional_primacy_reading, executive_branch, agenda_setter).

% Benefits from the constitutional check on war powers, ensuring democratic accountability for military engagements. However, the public also bears the ultimate costs of war (lives, treasure) regardless of authorization, and may feel disempowered when executive action bypasses Congress. Their 'exit' is through electoral politics and protest.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, american_public, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(war_powers_allocation__congressional_primacy_reading, american_public, payer).

% The courts are theoretically positioned to adjudicate disputes over war powers but have historically been reluctant to intervene, often deferring to the political branches. They observe the contest without actively enforcing this reading's strictures.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, judiciary, observer,
    institutional, civilizational, analytical, national).

% Bear the most direct and severe costs of military action, regardless of its constitutional authorization. They are bound by orders and have virtually no exit options once deployed, making them ultimate payers in any conflict.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, military_personnel, payer,
    powerless, immediate, trapped, global).

% Analyze and interpret the constitutional framework for war powers, often advocating for this congressional primacy reading. Their influence is through academic discourse, legal briefs, and public commentary, but they lack direct enforcement power.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, constitutional_scholars, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures that the nation's decision to engage in military force, particularly offensive actions, is a collective one, reflecting the will of the people through their elected representatives, thereby coordinating national resources and will behind a legitimate cause.
% TRANSFER_FUNCTION: Transfers the authority to initiate large-scale military force from the executive branch to the legislative branch, requiring a formal declaration or authorization from Congress before such force can be deployed beyond immediate self-defense.
% ABSENT_VOICES: Future generations, who will bear the long-term consequences of war, are absent from the immediate decision-making process. Their interests would strongly advocate for strict adherence to constitutional checks on war powers.
% DISAPPEARANCE_RATIONALE: If the constitutional necessity of congressional authorization for war vanished, the executive branch would gain unchecked power to deploy military force, fundamentally altering the balance of power, increasing the likelihood of unilateral military interventions, and eroding democratic accountability for war. The entire constitutional order regarding foreign policy and national security would reorganize.
% FOUNDING_PROBLEM: The constraint was built to prevent the executive from unilaterally engaging in costly and potentially tyrannical wars, ensuring that the decision to commit the nation to war rested with the representative body closest to the people, preventing abuses seen under monarchical systems.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars, historical records of the Constitutional Convention debates, and public opinion polls consistently corroborate the founding problem's intent to prevent executive overreach in war. Legislative efforts to reassert war powers also attest to its live status, even when executive actions challenge it.
narrative_ontology:disappearance_verdict(war_powers_allocation__congressional_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(war_powers_allocation__congressional_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_powers_allocation__congressional_primacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(war_powers_allocation__congressional_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(war_powers_allocation__congressional_primacy_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.78) because the executive branch frequently deploys military force without explicit congressional authorization, effectively extracting the power of war declaration from Congress. Suppression is very high (0.85) as this reading actively suppresses claims of inherent executive authority for offensive military action, requiring constant assertion by Congress and legal challenges. Theater ratio is low (0.15) because this reading is about genuine constitutional adherence, not performative maintenance; any theatricality comes from executive attempts to frame unauthorized actions as legitimate. Resistance is high (0.70) due to the executive branch's consistent pushback against strict congressional oversight.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Congress (and this reading), the constraint is a vital check on executive power, ensuring collective decision-making for war. From the executive's perspective, this constraint is an impediment to swift and decisive action in national security, often leading to claims of inherent authority or interpretations that minimize the need for explicit authorization. The engine's computation of per-seat types will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Congress and the American public are the primary beneficiaries of this reading, as it vests war-making authority in the representative body and ensures democratic accountability. The executive branch is the primary target/payer, as it is constrained by the requirement for authorization and its unilateral actions are deemed unconstitutional extractions. Military personnel are payers who bear the direct costs of war, regardless of authorization.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a Tangled Rope (rather than a Snare) acknowledges the genuine coordination function of legitimate war authorization, while highlighting the asymmetric extraction that occurs when the executive bypasses Congress. It prevents mislabeling the constitutional framework as purely extractive, while still identifying the extractive dynamics of executive overreach. The 'live' status of the founding problem, despite substantial practice drift, indicates an ongoing contest, not a defunct mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definition_of_immediate_defense,
    'What constitutes ''immediate defense'' that would permit unilateral executive action without prior congressional authorization?',
    'Judicial clarification or legislative definition of ''imminent threat'' and ''defensive action'' in specific contexts, or a constitutional amendment.',
    'A narrow definition would strengthen congressional primacy, increasing executive extractiveness. A broad definition would weaken it, reducing executive extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definition_of_immediate_defense, conceptual, 'Ambiguity in the scope of executive self-defense powers.').

omega_variable(
    enforceability_of_congressional_will,
    'To what extent can Congress practically enforce its constitutional primacy in war powers against a determined executive, given the judiciary''s reluctance to intervene?',
    'Historical analysis of legislative tools (e.g., War Powers Resolution, funding restrictions) and their effectiveness, or a shift in judicial willingness to adjudicate.',
    'If enforcement mechanisms are weak, the effective suppression of executive unilateralism is lower than measured, and the constraint operates more as a Snare. If strong, it reinforces the Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforceability_of_congressional_will, empirical, 'Practical limits on congressional enforcement of war powers.').

omega_variable(
    legitimacy_of_unilateral_action_claims,
    'Are executive claims of inherent authority for military action genuinely rooted in constitutional text/history, or are they primarily instrumental justifications for bypassing congressional oversight?',
    'Comprehensive historical and legal scholarship, potentially informed by declassified executive branch legal opinions, to assess the original intent and evolution of executive war powers.',
    'If claims are found to be primarily instrumental, the ''inherent_executive_reading'' is further delegitimized, strengthening the ''forecloses'' relation and increasing the perceived extraction from Congress. If some historical basis is found, it might slightly soften the ''forecloses'' relation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimacy_of_unilateral_action_claims, conceptual, 'The true constitutional grounding of executive claims to unilateral war powers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_powers_allocation__congressional_primacy_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t1945, war_powers_allocation__congressional_primacy_reading, theater_ratio, 1945, 0.1).
narrative_ontology:measurement(war__tr_t1965, war_powers_allocation__congressional_primacy_reading, theater_ratio, 1965, 0.12).
narrative_ontology:measurement(war__tr_t1985, war_powers_allocation__congressional_primacy_reading, theater_ratio, 1985, 0.13).
narrative_ontology:measurement(war__tr_t2005, war_powers_allocation__congressional_primacy_reading, theater_ratio, 2005, 0.14).
narrative_ontology:measurement(war__tr_t2024, war_powers_allocation__congressional_primacy_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(war__be_t1945, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 1945, 0.5).
narrative_ontology:measurement(war__be_t1965, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 1965, 0.65).
narrative_ontology:measurement(war__be_t1985, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 1985, 0.7).
narrative_ontology:measurement(war__be_t2005, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 2005, 0.75).
narrative_ontology:measurement(war__be_t2024, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(war__su_t1945, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 1945, 0.6).
narrative_ontology:measurement(war__su_t1965, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 1965, 0.7).
narrative_ontology:measurement(war__su_t1985, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 1985, 0.75).
narrative_ontology:measurement(war__su_t2005, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 2005, 0.8).
narrative_ontology:measurement(war__su_t2024, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 2024, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_powers_allocation__congressional_primacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(war_powers_allocation__congressional_primacy_reading, war_powers_allocation__inherent_executive_reading).
narrative_ontology:affects_constraint(war_powers_allocation__congressional_primacy_reading, war_powers_allocation__functional_accommodation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'war_powers_allocation' kernel, focusing on congressional primacy. Its structural claims directly contest the 'inherent_executive_reading' and influence the 'functional_accommodation_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
