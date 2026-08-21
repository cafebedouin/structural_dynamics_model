% ============================================================================
% CONSTRAINT STORY: second_amendment_text__collective_security_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_text__collective_security_reading, []).

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
 *   constraint_id: second_amendment_text__collective_security_reading
 *   human_readable: Second Amendment: Collective Security Reading
 *   domain: constitutional_law/political_theory/firearms_policy
 *
 * SUMMARY:
 *   This constraint represents the 'collective security' reading of the
 *   Second Amendment, which interprets the 'well-regulated militia' clause as
 *   conditioning the right to bear arms on its utility for organized civic
 *   defense. Under this reading, the state retains significant power to
 *   regulate firearms to serve collective security, making licensing and
 *   permit regimes permissible. This reading is often contrasted with
 *   interpretations emphasizing an unconditioned individual right to
 *   self-defense. The claim/metric gap is deliberate: the constraint is
 *   CLAIMED as a Tangled Rope (coordinating collective security while
 *   extracting from individuals) while the authored metrics describe its
 *   substantially extractive and suppressive operation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_text__collective_security_reading, 0.65).
domain_priors:suppression_score(second_amendment_text__collective_security_reading, 0.75).
domain_priors:theater_ratio(second_amendment_text__collective_security_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_text__collective_security_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_text__collective_security_reading, "Second Amendment: Collective Security Reading").
narrative_ontology:topic_domain(second_amendment_text__collective_security_reading, "constitutional_law/political_theory/firearms_policy").

domain_priors:requires_active_enforcement(second_amendment_text__collective_security_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_text__collective_security_reading, '344fc857-b3f6-4b0c-8666-9c0061d94725').
narrative_ontology:cs_kernel_codification('344fc857-b3f6-4b0c-8666-9c0061d94725', fixed_text).
narrative_ontology:cs_authority_grounding('344fc857-b3f6-4b0c-8666-9c0061d94725', lineage).
narrative_ontology:cs_interpretation_layer_present('344fc857-b3f6-4b0c-8666-9c0061d94725').
narrative_ontology:cs_reading_relation('344fc857-b3f6-4b0c-8666-9c0061d94725', second_amendment_text__individual_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('344fc857-b3f6-4b0c-8666-9c0061d94725', second_amendment_text__originalist_civic_virtue_reading, coexists_with).
narrative_ontology:cs_axiom('344fc857-b3f6-4b0c-8666-9c0061d94725', foundational, militia_clause_conditions_right).
narrative_ontology:cs_axiom_status(militia_clause_conditions_right, holdable).
narrative_ontology:cs_axiom_grounding('344fc857-b3f6-4b0c-8666-9c0061d94725', militia_clause_conditions_right, conventional).
narrative_ontology:cs_axiom('344fc857-b3f6-4b0c-8666-9c0061d94725', foundational, state_police_power_for_public_safety).
narrative_ontology:cs_axiom_status(state_police_power_for_public_safety, holdable).
narrative_ontology:cs_axiom_grounding('344fc857-b3f6-4b0c-8666-9c0061d94725', state_police_power_for_public_safety, conventional).
narrative_ontology:cs_reference_frame('344fc857-b3f6-4b0c-8666-9c0061d94725', post_brady_era_regulatory_framework).
narrative_ontology:cs_drift_state('344fc857-b3f6-4b0c-8666-9c0061d94725', post_heller_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('344fc857-b3f6-4b0c-8666-9c0061d94725', '').
narrative_ontology:cs_kernel_id(second_amendment_text__collective_security_reading, second_amendment_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_text__collective_security_reading, state_regulatory_apparatus).
narrative_ontology:constraint_beneficiary(second_amendment_text__collective_security_reading, general_public).
narrative_ontology:constraint_victim(second_amendment_text__collective_security_reading, individual_gun_owners).
narrative_ontology:constraint_victim(second_amendment_text__collective_security_reading, firearms_manufacturers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers and enforces firearms regulations, including licensing, background checks, and restrictions on certain weapon types, to ensure public safety and a 'well-regulated militia'. Benefits from expanded police power and public trust in security.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, state_regulatory_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Benefits from perceived enhanced collective security and reduced gun violence due to state regulation. May also bear indirect costs through restricted access to firearms for self-defense or sport.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, general_public, beneficiary,
    organized, biographical, constrained, national).

% Subject to state licensing, registration, and use restrictions. Bears the costs of compliance and potential loss of access to desired firearms, viewing these as infringements on a fundamental right.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, individual_gun_owners, payer,
    moderate, biographical, constrained, national).

% Affected by regulations on firearm types, sales, and distribution, which can impact market demand and product development. Engages in lobbying and and litigation to influence policy.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, firearms_manufacturers, payer,
    powerful, biographical, constrained, national).

% Strongly oppose this reading, arguing it misinterprets the Second Amendment by subordinating individual rights to state control. Actively lobby, litigate, and organize to promote an individual-right interpretation.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, individual_rights_advocates, excluded,
    organized, generational, constrained, national).

% Analyze the historical context, textual meaning, and legal precedents of the Second Amendment, often debating the merits and implications of the collective security interpretation.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, constitutional_scholars, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate individual arms bearing with the collective security needs of the state, ensuring a 'well-regulated militia' capable of civic defense, while allowing for state regulation of arms.
% TRANSFER_FUNCTION: Transfers some individual liberty and unrestricted access to firearms to the state's power to regulate arms for the collective good and public safety.
% ABSENT_VOICES: Individual rights advocates who argue for an unconditioned right to bear arms, and those who believe the militia clause is obsolete or merely prefatory, are structurally excluded from this reading's internal logic.
% DISAPPEARANCE_RATIONALE: If this reading vanished overnight, the state's power to regulate firearms for collective security would be severely curtailed, leading to a dramatic shift in firearms policy, public safety debates, and potentially an increase in unregulated arms. The legal landscape would reorganize around a more expansive individual right.
% FOUNDING_PROBLEM: Balancing the necessity of an armed citizenry for defense (the militia) with the potential dangers of unregulated private arms and the need for public order.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the founding era, political theorists, and some public safety advocates corroborate the historical context of collective security concerns and the ongoing debate over balancing these interests. Legislative hearing testimony and academic legal scholarship also support the persistence of this tension.
narrative_ontology:disappearance_verdict(second_amendment_text__collective_security_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_text__collective_security_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_text__collective_security_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(second_amendment_text__collective_security_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_text__collective_security_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_text__collective_security_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_text__collective_security_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_text__collective_security_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) and suppression (0.75) are high because this reading grants the state substantial power to restrict individual firearms ownership and use, imposing costs and limiting access for individuals. The state's regulatory actions are seen as genuinely aimed at public safety, hence the low theater ratio (0.20). Resistance is very high (0.80) due to intense opposition from individual rights advocates. The increasing extractiveness and suppression over time reflect the historical trend of expanding state regulatory power in response to public safety concerns.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the state and the general public, this reading provides a necessary framework for public safety and order. From the perspective of individual gun owners and their advocates, it is an illegitimate infringement on a fundamental right. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The state regulatory apparatus is a primary beneficiary, gaining power and legitimacy from its role in ensuring collective security. The general public is also a beneficiary, experiencing perceived safety benefits. Individual gun owners and firearms manufacturers are targets, bearing the costs of regulation and restrictions. Individual rights advocates are excluded from this reading's internal logic, as their core premise is rejected by it.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    second_amendment_interpretive_ambiguity,
    'Is the ''well-regulated militia'' clause a condition on the right to bear arms, or merely a prefatory statement of purpose that does not limit the individual right?',
    'Further historical scholarship on founding-era intent, or a definitive Supreme Court ruling that explicitly resolves the conditioning vs. prefatory debate.',
    'If prefatory, the constraint''s extractiveness and suppression from individual owners would be reclassified as illegitimate; if conditioning, the current classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(second_amendment_interpretive_ambiguity, conceptual, 'Whether the militia clause conditions the right or is merely prefatory.').

omega_variable(
    effectiveness_of_firearms_regulation,
    'Does the state''s regulatory apparatus, as justified by this reading, genuinely enhance collective security and reduce violence, or does it primarily disarm law-abiding citizens without deterring criminals?',
    'Empirical studies on the causal impact of specific firearms regulations on crime rates, public safety, and defensive gun use, controlling for confounding factors.',
    'If ineffective, the justification for the constraint''s extractiveness and suppression would be undermined, potentially leading to reclassification towards a Snare; if effective, the Rope aspect is strengthened.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(effectiveness_of_firearms_regulation, empirical, 'Empirical efficacy of firearms regulation for collective security.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_text__collective_security_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_text__collective_security_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(seco_tr_t10, second_amendment_text__collective_security_reading, theater_ratio, 10, 0.19).
narrative_ontology:measurement(seco_tr_t20, second_amendment_text__collective_security_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(seco_tr_t30, second_amendment_text__collective_security_reading, theater_ratio, 30, 0.19).
narrative_ontology:measurement(seco_tr_t40, second_amendment_text__collective_security_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement(seco_tr_t50, second_amendment_text__collective_security_reading, theater_ratio, 50, 0.2).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_text__collective_security_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(seco_be_t10, second_amendment_text__collective_security_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(seco_be_t20, second_amendment_text__collective_security_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement(seco_be_t30, second_amendment_text__collective_security_reading, base_extractiveness, 30, 0.63).
narrative_ontology:measurement(seco_be_t40, second_amendment_text__collective_security_reading, base_extractiveness, 40, 0.64).
narrative_ontology:measurement(seco_be_t50, second_amendment_text__collective_security_reading, base_extractiveness, 50, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_text__collective_security_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(seco_su_t10, second_amendment_text__collective_security_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(seco_su_t20, second_amendment_text__collective_security_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(seco_su_t30, second_amendment_text__collective_security_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(seco_su_t40, second_amendment_text__collective_security_reading, suppression_requirement, 40, 0.74).
narrative_ontology:measurement(seco_su_t50, second_amendment_text__collective_security_reading, suppression_requirement, 50, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_text__collective_security_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'second_amendment_text' kernel, each representing a distinct interpretation of the right to bear arms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
