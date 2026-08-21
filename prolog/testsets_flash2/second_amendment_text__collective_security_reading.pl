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
 *   constraint_id: second_amendment_text__collective_security_reading
 *   human_readable: Second Amendment: Collective Security Reading
 *   domain: constitutional_law/political_theory/firearms_policy
 *
 * SUMMARY:
 *   This constraint represents the 'collective security' reading of the
 *   Second Amendment, where the right to bear arms is understood as
 *   conditioned by the necessity of a well-regulated militia for state
 *   security. This interpretation grants the state significant power to
 *   regulate firearms to serve collective safety, making state regulatory
 *   bodies beneficiaries and individual gun owners payers. The metrics
 *   reflect a moderately extractive and suppressive constraint, as it imposes
 *   costs and limits on individuals for a collective good.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_text__collective_security_reading, 0.45).
domain_priors:suppression_score(second_amendment_text__collective_security_reading, 0.3).
domain_priors:theater_ratio(second_amendment_text__collective_security_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_text__collective_security_reading, rope).
narrative_ontology:human_readable(second_amendment_text__collective_security_reading, "Second Amendment: Collective Security Reading").
narrative_ontology:topic_domain(second_amendment_text__collective_security_reading, "constitutional_law/political_theory/firearms_policy").

domain_priors:requires_active_enforcement(second_amendment_text__collective_security_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_text__collective_security_reading, 'e1cf8fc0-5835-4eb5-903e-6e8ee9aef12c').
narrative_ontology:cs_kernel_codification('e1cf8fc0-5835-4eb5-903e-6e8ee9aef12c', fixed_text).
narrative_ontology:cs_authority_grounding('e1cf8fc0-5835-4eb5-903e-6e8ee9aef12c', lineage).
narrative_ontology:cs_interpretation_layer_present('e1cf8fc0-5835-4eb5-903e-6e8ee9aef12c').
narrative_ontology:cs_reading_relation('e1cf8fc0-5835-4eb5-903e-6e8ee9aef12c', second_amendment_text__individual_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('e1cf8fc0-5835-4eb5-903e-6e8ee9aef12c', second_amendment_text__originalist_civic_virtue_reading, coexists_with).
narrative_ontology:cs_axiom('e1cf8fc0-5835-4eb5-903e-6e8ee9aef12c', foundational, militia_clause_conditions_operative_clause).
narrative_ontology:cs_axiom_status(militia_clause_conditions_operative_clause, holdable).
narrative_ontology:cs_axiom_grounding('e1cf8fc0-5835-4eb5-903e-6e8ee9aef12c', militia_clause_conditions_operative_clause, conventional).
narrative_ontology:cs_axiom('e1cf8fc0-5835-4eb5-903e-6e8ee9aef12c', foundational, state_has_police_power_over_arms).
narrative_ontology:cs_axiom_status(state_has_police_power_over_arms, holdable).
narrative_ontology:cs_axiom_grounding('e1cf8fc0-5835-4eb5-903e-6e8ee9aef12c', state_has_police_power_over_arms, conventional).
narrative_ontology:cs_reference_frame('e1cf8fc0-5835-4eb5-903e-6e8ee9aef12c', well_regulated_militia_framework).
narrative_ontology:cs_drift_state('e1cf8fc0-5835-4eb5-903e-6e8ee9aef12c', contemporary_judicial_interpretations, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('e1cf8fc0-5835-4eb5-903e-6e8ee9aef12c', '').
narrative_ontology:cs_kernel_id(second_amendment_text__collective_security_reading, second_amendment_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_text__collective_security_reading, state_regulatory_apparatus).
narrative_ontology:constraint_beneficiary(second_amendment_text__collective_security_reading, general_public).
narrative_ontology:constraint_victim(second_amendment_text__collective_security_reading, individual_gun_owners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(second_amendment_text__collective_security_reading, firearms_manufacturers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the Second Amendment as primarily concerned with collective security, allowing for robust state regulation of firearms to maintain public order and safety. Benefits from the ability to implement licensing, registration, and other control measures.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, state_regulatory_apparatus, agenda_setter,
    institutional, generational, mobile, national).

% Benefits from perceived enhanced public safety and reduced gun violence due to state regulation. Their interest is in collective well-being, which this reading prioritizes.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, general_public, beneficiary,
    organized, biographical, constrained, national).

% Experience the right to bear arms as conditioned by state-imposed regulations (e.g., permits, waiting periods, restrictions on certain types of firearms). They bear the costs of compliance and limitations on their choices of arms.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, individual_gun_owners, payer,
    moderate, biographical, constrained, national).

% Face market restrictions and compliance costs due to state regulations on firearm types, sales, and distribution. Their business model is directly impacted by the scope of permissible state control.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, firearms_manufacturers, payer,
    powerful, biographical, constrained, national).

% Analyze the balance between collective security and individual liberties, often challenging regulations that they perceive as overly restrictive of individual rights, even within a collective security framework.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, civil_liberties_advocates, observer,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the state's authority to maintain public order and safety through firearm regulation with the citizenry's right to bear arms, ensuring that arms are available for collective defense while minimizing risks to the public.
% TRANSFER_FUNCTION: Transfers a degree of individual autonomy over firearm ownership to the state in exchange for enhanced collective security and public order.
% ABSENT_VOICES: Those who believe in an absolute individual right to bear arms, free from state interference, are largely excluded from the policy-making process under this reading. They would argue that the state's regulatory power is an infringement on fundamental liberty.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the state's power to regulate firearms for collective security would be severely curtailed, leading to a rapid deregulation of firearms. This would likely result in a significant increase in firearm availability and potentially a rise in gun violence, forcing society to reorganize its approach to public safety.
% FOUNDING_PROBLEM: The founding problem was to balance the necessity of a well-regulated militia for the security of a free state with the individual's right to possess arms, ensuring both collective defense and preventing tyranny, while also managing the inherent dangers of widespread firearm ownership.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, historians, and public safety experts outside of direct gun rights advocacy corroborate that the tension between collective security and individual arms possession remains a live and complex problem, requiring ongoing interpretation and policy solutions.
narrative_ontology:disappearance_verdict(second_amendment_text__collective_security_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_text__collective_security_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_text__collective_security_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(second_amendment_text__collective_security_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_text__collective_security_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_text__collective_security_reading_tests).
:- end_tests(second_amendment_text__collective_security_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45) because individual gun owners bear the costs of regulation (licensing, restrictions) for the benefit of collective security. Suppression is also moderate (0.30) as the state actively enforces these regulations, but alternatives (e.g., unregulated black markets) are not entirely eliminated. Theater ratio is low (0.10) as the regulatory function is genuinely active and not merely performative. Resistance is moderate (0.50) due to ongoing legal and political challenges from individual rights advocates.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the state and the general public, this reading is a necessary coordination mechanism for public safety. From the perspective of individual gun owners, it is an extractive and suppressive constraint on a fundamental right. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The state regulatory apparatus is a clear beneficiary (d near 0.0) as it gains power and legitimacy from this reading. The general public is also a beneficiary (d near 0.15) due to perceived safety. Individual gun owners and firearms manufacturers are payers (d near 0.7-0.8) as they bear the direct costs and restrictions. Civil liberties advocates are observers (d near 0.5) as they analyze the constraint without direct benefit or cost.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_impact_of_regulation,
    'What is the demonstrable empirical impact of state firearm regulations (under this reading) on collective security and public safety outcomes?',
    'Longitudinal studies and meta-analyses comparing public safety metrics (e.g., gun violence rates, accidental deaths) across jurisdictions with varying regulatory stringency, controlling for confounding socioeconomic factors.',
    'Strong empirical evidence of positive impact would strengthen the legitimacy of this reading and its classification as a Rope; weak or negative evidence would push it towards a Snare or Tangled Rope, suggesting the coordination story is cover for extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(empirical_impact_of_regulation, empirical, 'Assesses whether the claimed benefits of collective security are empirically realized by the regulations.').

omega_variable(
    scope_of_militia_definition,
    'How broadly or narrowly should ''well-regulated militia'' be interpreted in contemporary society, and does this interpretation align with the original intent or evolving societal needs?',
    'Historical and legal scholarship on the evolving definition of ''militia'' from the founding era to present, combined with public discourse analysis on the role of organized civic defense in modern states.',
    'A narrow, historically specific definition might limit the state''s regulatory scope, pushing the constraint towards an individual right. A broad, evolving definition would reinforce the state''s power, maintaining the current classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_militia_definition, conceptual, 'Examines the conceptual boundaries of the ''militia'' clause and its implications for state power.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal barriers, economic costs) or internalized (social norms, fear of legal repercussions) for individual gun owners?',
    'Post-regulatory change behavior: if gun ownership patterns persist after specific regulations are removed, reclassify as partially internalized. Surveys on attitudes towards compliance and perceived risks.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as individuals self-regulate beyond explicit legal requirements. This would amplify the perceived cost for individual gun owners.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for gun owners.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_text__collective_security_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_text__collective_security_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(seco_be_t10, second_amendment_text__collective_security_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(seco_be_t20, second_amendment_text__collective_security_reading, base_extractiveness, 20, 0.43).
narrative_ontology:measurement(seco_be_t30, second_amendment_text__collective_security_reading, base_extractiveness, 30, 0.44).
narrative_ontology:measurement(seco_be_t40, second_amendment_text__collective_security_reading, base_extractiveness, 40, 0.45).
narrative_ontology:measurement(seco_be_t50, second_amendment_text__collective_security_reading, base_extractiveness, 50, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_text__collective_security_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(seco_su_t10, second_amendment_text__collective_security_reading, suppression_requirement, 10, 0.27).
narrative_ontology:measurement(seco_su_t20, second_amendment_text__collective_security_reading, suppression_requirement, 20, 0.28).
narrative_ontology:measurement(seco_su_t30, second_amendment_text__collective_security_reading, suppression_requirement, 30, 0.29).
narrative_ontology:measurement(seco_su_t40, second_amendment_text__collective_security_reading, suppression_requirement, 40, 0.3).
narrative_ontology:measurement(seco_su_t50, second_amendment_text__collective_security_reading, suppression_requirement, 50, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_text__collective_security_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_text__collective_security_reading, second_amendment_text__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_text__collective_security_reading, second_amendment_text__originalist_civic_virtue_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the Second Amendment text, each with different structural properties and stakeholder dynamics. This 'collective security' reading emphasizes state regulatory power for public safety.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
