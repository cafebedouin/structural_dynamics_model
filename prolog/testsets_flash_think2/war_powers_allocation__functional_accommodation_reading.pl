% ============================================================================
% CONSTRAINT STORY: war_powers_allocation__functional_accommodation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_powers_allocation__functional_accommodation_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: war_powers_allocation__functional_accommodation_reading
 *   human_readable: Functional Accommodation Reading of War Powers Allocation
 *   domain: constitutional_law/separation_of_powers/war_powers
 *
 * SUMMARY:
 *   This constraint instantiates the 'functional accommodation' reading of
 *   the war powers allocation kernel, which posits a flexible,
 *   context-dependent distribution of authority between the executive and
 *   legislative branches. This reading contrasts with the 'congressional
 *   primacy' reading (emphasizing strict legislative control) and the
 *   'inherent executive' reading (asserting broad presidential authority).
 *   The functional accommodation allows for executive action in imminent
 *   threats but theoretically requires congressional authorization for
 *   prolonged campaigns, creating an ambiguity zone where both branches claim
 *   authority and categorical rules are often suppressed.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_powers_allocation__functional_accommodation_reading, 0.65).
domain_priors:suppression_score(war_powers_allocation__functional_accommodation_reading, 0.55).
domain_priors:theater_ratio(war_powers_allocation__functional_accommodation_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_powers_allocation__functional_accommodation_reading, tangled_rope).
narrative_ontology:human_readable(war_powers_allocation__functional_accommodation_reading, "Functional Accommodation Reading of War Powers Allocation").
narrative_ontology:topic_domain(war_powers_allocation__functional_accommodation_reading, "constitutional_law/separation_of_powers/war_powers").

domain_priors:requires_active_enforcement(war_powers_allocation__functional_accommodation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_powers_allocation__functional_accommodation_reading, '831f2a9b-431b-462e-8c74-cc11a5377783').
narrative_ontology:cs_kernel_codification('831f2a9b-431b-462e-8c74-cc11a5377783', formalized).
narrative_ontology:cs_authority_grounding('831f2a9b-431b-462e-8c74-cc11a5377783', lineage).
narrative_ontology:cs_interpretation_layer_present('831f2a9b-431b-462e-8c74-cc11a5377783').
narrative_ontology:cs_reading_relation('831f2a9b-431b-462e-8c74-cc11a5377783', war_powers_allocation__congressional_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('831f2a9b-431b-462e-8c74-cc11a5377783', war_powers_allocation__inherent_executive_reading, coexists_with).
narrative_ontology:cs_axiom('831f2a9b-431b-462e-8c74-cc11a5377783', foundational, executive_flexibility_in_crisis).
narrative_ontology:cs_axiom_status(executive_flexibility_in_crisis, holdable).
narrative_ontology:cs_axiom_grounding('831f2a9b-431b-462e-8c74-cc11a5377783', executive_flexibility_in_crisis, instrumental).
narrative_ontology:cs_axiom('831f2a9b-431b-462e-8c74-cc11a5377783', foundational, congressional_deliberation_for_sustained_force).
narrative_ontology:cs_axiom_status(congressional_deliberation_for_sustained_force, holdable).
narrative_ontology:cs_axiom_grounding('831f2a9b-431b-462e-8c74-cc11a5377783', congressional_deliberation_for_sustained_force, deontological).
narrative_ontology:cs_reference_frame('831f2a9b-431b-462e-8c74-cc11a5377783', post_vietnam_war_balance).
narrative_ontology:cs_drift_state('831f2a9b-431b-462e-8c74-cc11a5377783', post_9_11_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('831f2a9b-431b-462e-8c74-cc11a5377783', '').
narrative_ontology:cs_kernel_id(war_powers_allocation__functional_accommodation_reading, war_powers_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_powers_allocation__functional_accommodation_reading, executive_branch).
narrative_ontology:constraint_beneficiary(war_powers_allocation__functional_accommodation_reading, military_command).
narrative_ontology:constraint_victim(war_powers_allocation__functional_accommodation_reading, congressional_oversight).
narrative_ontology:constraint_victim(war_powers_allocation__functional_accommodation_reading, public_accountability).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(war_powers_allocation__functional_accommodation_reading, congress).
narrative_ontology:constraint_beneficiary(war_powers_allocation__functional_accommodation_reading, international_allies).
narrative_ontology:constraint_victim(war_powers_allocation__functional_accommodation_reading, congress).
narrative_ontology:constraint_victim(war_powers_allocation__functional_accommodation_reading, public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Asserts the need for flexibility and swift action in national security, interpreting its commander-in-chief powers broadly for imminent threats. Benefits from operational discretion and the ability to initiate military action without prior, explicit congressional approval in certain contexts.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, executive_branch, agenda_setter,
    institutional, generational, constrained, national).

% Holds the constitutional power to declare war and raise armies, but often cedes initial authority to the executive, especially for short-term engagements. Bears the political cost of prolonged, unauthorized conflicts but also benefits from avoiding direct responsibility for unpopular military actions. Engages in oversight and funding debates.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, congress, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(war_powers_allocation__functional_accommodation_reading, congress, beneficiary).

% Benefits from the operational flexibility afforded by executive discretion, allowing for rapid deployment and response to evolving threats. Operates under the command of the executive, but relies on congressional funding and support for sustained operations.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, military_command, beneficiary,
    institutional, biographical, constrained, global).

% Generally defers to the political branches on war powers, often declining to hear cases on 'political questions.' Provides a theoretical check but rarely intervenes directly in the allocation of war powers in practice.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, judiciary, observer,
    institutional, civilizational, analytical, national).

% Bears the human and economic costs of military conflicts, often without direct input or clear accountability mechanisms for decisions made in the 'gray area' of war powers. Experiences the consequences of both executive action and congressional inaction.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, public, payer,
    powerless, immediate, trapped, national).

% Benefits from the perceived ability of the US executive to act swiftly in crises, providing a degree of stability or rapid response in shared security concerns. May also bear costs if US actions are perceived as unilateral or destabilizing.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, international_allies, beneficiary,
    organized, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(war_powers_allocation__functional_accommodation_reading, diffuse).
narrative_ontology:fixing_cost_class(war_powers_allocation__functional_accommodation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To enable the United States government to respond effectively and flexibly to national security threats, balancing the executive's need for swift action with the legislative branch's role in authorizing prolonged engagements and funding.
% TRANSFER_FUNCTION: Transfers the authority to initiate and sustain military force, along with the associated political risks, resource allocation, and accountability, between the executive and legislative branches, often with the executive gaining initial discretion.
% ABSENT_VOICES: Strict constitutionalists (from both congressional primacy and inherent executive camps) who advocate for clear, categorical rules rather than flexible accommodation; the broader public, which often lacks direct input on war decisions and bears the ultimate costs.
% DISAPPEARANCE_RATIONALE: If this functional accommodation vanished, the US would be forced into a rigid interpretation of war powers (either strict congressional primacy or inherent executive authority), fundamentally altering its foreign policy, military engagement, and the balance of power between branches, potentially leading to gridlock or unchecked presidential power.
% FOUNDING_PROBLEM: The framers of the Constitution sought to create a government capable of both swift defense against external threats and deliberative decision-making regarding the commitment of the nation to war, balancing efficiency with democratic accountability.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars, historical analyses of war powers debates, and ongoing political discourse from non-partisan observers consistently corroborate the enduring tension between executive efficiency and legislative deliberation in matters of war.
narrative_ontology:disappearance_verdict(war_powers_allocation__functional_accommodation_reading, world_rearranges).
narrative_ontology:founding_problem_status(war_powers_allocation__functional_accommodation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_powers_allocation__functional_accommodation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(war_powers_allocation__functional_accommodation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(war_powers_allocation__functional_accommodation_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_powers_allocation__functional_accommodation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_powers_allocation__functional_accommodation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(war_powers_allocation__functional_accommodation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) reflects the executive's ability to leverage the ambiguity for unilateral action, potentially leading to mission creep or prolonged engagements without explicit congressional buy-in. Suppression (0.55) arises from the active political and legal maneuvering that prevents clear, categorical rules from being established, maintaining the 'gray area.' Theater ratio (0.40) indicates that while some oversight and deliberation are genuine, a significant portion of inter-branch activity serves to assert or defend claims of authority rather than resolve the underlying tension. The metrics show a peak in extractiveness and suppression during the post-9/11 era of prolonged interventions, with a slight decline by 2020 as some of these conflicts wound down or faced increased scrutiny.
 *
 * PERSPECTIVAL GAP:
 *   From the executive's perspective, this accommodation is a necessary and efficient mechanism for national security. From the perspective of congressional oversight and public accountability, it can appear as a mechanism for executive overreach and a dilution of democratic checks. The engine's per-seat classification will highlight this divergence, showing the constraint as more extractive for the public and congressional oversight, and more coordinative for the executive and military.
 *
 * DIRECTIONALITY LOGIC:
 *   The executive branch and military command are primary beneficiaries, gaining operational flexibility and discretion (low directionality). Congress is a complex seat, acting as a payer by ceding authority but also a beneficiary by avoiding direct responsibility for unpopular actions (mid-range directionality). The public bears the costs of conflicts and lack of accountability (high directionality). The judiciary acts as an observer, often deferring to the political branches.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    accommodation_vs_overreach,
    'Is the ''functional accommodation'' a genuine, necessary balance for national security, or has it become a de facto mechanism for executive overreach and rent-seeking of authority?',
    'Comparative analysis of military interventions under different war powers regimes (e.g., pre- and post-War Powers Resolution), assessing outcomes, costs, and public support, alongside legal scholarship on constitutional interpretation.',
    'If primarily overreach, the constraint''s effective extractiveness is higher than currently assessed, and its classification shifts closer to a Snare for the legislative and public seats. If a genuine balance, the coordination function is stronger, supporting a Rope or Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(accommodation_vs_overreach, conceptual, 'Ambiguity between functional balance and executive overreach.').

omega_variable(
    imminent_threat_definition_ambiguity,
    'How is the distinction between ''imminent threat'' (permitting unilateral action) and ''prolonged campaign'' (requiring authorization) defined in practice, and by which branch?',
    'Analysis of executive branch legal opinions, congressional debates, and judicial rulings (where available) regarding specific military actions, tracing how the definitional boundary shifts over time and in response to political pressure.',
    'If the executive consistently defines ''imminent threat'' broadly to encompass prolonged actions, the constraint''s suppression of congressional authority is higher, and its extractiveness for the legislative branch increases. If Congress effectively asserts its definitional power, the constraint is more coordinative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(imminent_threat_definition_ambiguity, empirical, 'Definitional ambiguity of operational contexts.').

omega_variable(
    congressional_authorization_impact,
    'What is the actual impact of congressional authorization on the conduct and duration of military operations, versus its symbolic or political signaling role?',
    'Empirical study comparing military campaigns with and without explicit congressional authorization, analyzing differences in resource allocation, strategic objectives, public support, and exit strategies.',
    'If authorization has minimal practical effect, the constraint''s theater ratio is higher, indicating performative rather than functional coordination. If authorization significantly alters operations, the coordination function is stronger, reducing the effective extractiveness for the public and legislative seats.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(congressional_authorization_impact, empirical, 'Actual vs. symbolic impact of congressional authorization.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_powers_allocation__functional_accommodation_reading, 1990, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t1990, war_powers_allocation__functional_accommodation_reading, theater_ratio, 1990, 0.3).
narrative_ontology:measurement(war__tr_t1995, war_powers_allocation__functional_accommodation_reading, theater_ratio, 1995, 0.32).
narrative_ontology:measurement(war__tr_t2000, war_powers_allocation__functional_accommodation_reading, theater_ratio, 2000, 0.35).
narrative_ontology:measurement(war__tr_t2005, war_powers_allocation__functional_accommodation_reading, theater_ratio, 2005, 0.45).
narrative_ontology:measurement(war__tr_t2010, war_powers_allocation__functional_accommodation_reading, theater_ratio, 2010, 0.48).
narrative_ontology:measurement(war__tr_t2015, war_powers_allocation__functional_accommodation_reading, theater_ratio, 2015, 0.45).
narrative_ontology:measurement(war__tr_t2020, war_powers_allocation__functional_accommodation_reading, theater_ratio, 2020, 0.4).

% Extraction over time
narrative_ontology:measurement(war__be_t1990, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 1990, 0.55).
narrative_ontology:measurement(war__be_t1995, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 1995, 0.58).
narrative_ontology:measurement(war__be_t2000, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 2000, 0.6).
narrative_ontology:measurement(war__be_t2005, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 2005, 0.68).
narrative_ontology:measurement(war__be_t2010, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 2010, 0.7).
narrative_ontology:measurement(war__be_t2015, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 2015, 0.68).
narrative_ontology:measurement(war__be_t2020, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 2020, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(war__su_t1990, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 1990, 0.45).
narrative_ontology:measurement(war__su_t1995, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 1995, 0.48).
narrative_ontology:measurement(war__su_t2000, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 2000, 0.5).
narrative_ontology:measurement(war__su_t2005, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 2005, 0.6).
narrative_ontology:measurement(war__su_t2010, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 2010, 0.62).
narrative_ontology:measurement(war__su_t2015, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 2015, 0.58).
narrative_ontology:measurement(war__su_t2020, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 2020, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_powers_allocation__functional_accommodation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(war_powers_allocation__functional_accommodation_reading, military_budget_allocation).
narrative_ontology:affects_constraint(war_powers_allocation__functional_accommodation_reading, foreign_policy_doctrine).
narrative_ontology:affects_constraint(war_powers_allocation__functional_accommodation_reading, war_powers_allocation__congressional_primacy_reading).
narrative_ontology:affects_constraint(war_powers_allocation__functional_accommodation_reading, war_powers_allocation__inherent_executive_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'war_powers_allocation' kernel, focusing on the functional accommodation between executive and legislative branches. It is linked to the 'congressional_primacy_reading' and 'inherent_executive_reading' siblings, which offer alternative interpretations of war powers.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
