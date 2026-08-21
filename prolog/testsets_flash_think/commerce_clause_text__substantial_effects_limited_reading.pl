% ============================================================================
% CONSTRAINT STORY: commerce_clause_text__substantial_effects_limited_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_text__substantial_effects_limited_reading, []).

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
 *   constraint_id: commerce_clause_text__substantial_effects_limited_reading
 *   human_readable: Commerce Clause: Substantial Effects Doctrine (Limited Reading)
 *   domain: constitutional_law/federalism
 *
 * SUMMARY:
 *   This constraint represents a specific reading of the U.S. Constitution's
 *   Commerce Clause, which holds that federal power extends to intrastate
 *   activity only if it has a 'substantial effect' on interstate commerce AND
 *   the regulation is genuinely economic and not a pretext for general police
 *   power. This reading emerged as a counter-balance to more expansive
 *   interpretations, particularly after cases like United States v. Lopez
 *   (1995) and United States v. Morrison (2000). It aims to preserve a
 *   meaningful sphere of state sovereignty while acknowledging federal
 *   authority over national economic issues. The constraint is claimed as a
 *   Tangled Rope because it coordinates federal and state power but involves
 *   ongoing contestation and extraction from both levels of government
 *   depending on the specific application.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_text__substantial_effects_limited_reading, 0.55).
domain_priors:suppression_score(commerce_clause_text__substantial_effects_limited_reading, 0.6).
domain_priors:theater_ratio(commerce_clause_text__substantial_effects_limited_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_text__substantial_effects_limited_reading, tangled_rope).
narrative_ontology:human_readable(commerce_clause_text__substantial_effects_limited_reading, "Commerce Clause: Substantial Effects Doctrine (Limited Reading)").
narrative_ontology:topic_domain(commerce_clause_text__substantial_effects_limited_reading, "constitutional_law/federalism").

domain_priors:requires_active_enforcement(commerce_clause_text__substantial_effects_limited_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_text__substantial_effects_limited_reading, 'fbf226ca-e4c6-4021-97dd-2d7acf162b36').
narrative_ontology:cs_kernel_codification('fbf226ca-e4c6-4021-97dd-2d7acf162b36', fixed_text).
narrative_ontology:cs_authority_grounding('fbf226ca-e4c6-4021-97dd-2d7acf162b36', lineage).
narrative_ontology:cs_interpretation_layer_present('fbf226ca-e4c6-4021-97dd-2d7acf162b36').
narrative_ontology:cs_reading_relation('fbf226ca-e4c6-4021-97dd-2d7acf162b36', commerce_clause_text__expansive_federal_reading, coexists_with).
narrative_ontology:cs_reading_relation('fbf226ca-e4c6-4021-97dd-2d7acf162b36', commerce_clause_text__originalist_narrow_reading, coexists_with).
narrative_ontology:cs_axiom('fbf226ca-e4c6-4021-97dd-2d7acf162b36', foundational, economic_activity_nexus_required).
narrative_ontology:cs_axiom_status(economic_activity_nexus_required, holdable).
narrative_ontology:cs_axiom_grounding('fbf226ca-e4c6-4021-97dd-2d7acf162b36', economic_activity_nexus_required, empirically_contingent).
narrative_ontology:cs_axiom('fbf226ca-e4c6-4021-97dd-2d7acf162b36', foundational, non_pretextual_regulation).
narrative_ontology:cs_axiom_status(non_pretextual_regulation, holdable).
narrative_ontology:cs_axiom_grounding('fbf226ca-e4c6-4021-97dd-2d7acf162b36', non_pretextual_regulation, conventional).
narrative_ontology:cs_reference_frame('fbf226ca-e4c6-4021-97dd-2d7acf162b36', post_new_deal_balancing).
narrative_ontology:cs_drift_state('fbf226ca-e4c6-4021-97dd-2d7acf162b36', contemporary_judicial_review, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('fbf226ca-e4c6-4021-97dd-2d7acf162b36', '').
narrative_ontology:cs_kernel_id(commerce_clause_text__substantial_effects_limited_reading, commerce_clause_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_text__substantial_effects_limited_reading, state_governments).
narrative_ontology:constraint_beneficiary(commerce_clause_text__substantial_effects_limited_reading, federal_government).
narrative_ontology:constraint_victim(commerce_clause_text__substantial_effects_limited_reading, federal_government).
narrative_ontology:constraint_victim(commerce_clause_text__substantial_effects_limited_reading, state_governments).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(commerce_clause_text__substantial_effects_limited_reading, economic_actors).
narrative_ontology:constraint_beneficiary(commerce_clause_text__substantial_effects_limited_reading, civil_society_advocates).
narrative_ontology:constraint_victim(commerce_clause_text__substantial_effects_limited_reading, economic_actors).
narrative_ontology:constraint_victim(commerce_clause_text__substantial_effects_limited_reading, civil_society_advocates).
narrative_ontology:constraint_vindicates(commerce_clause_text__substantial_effects_limited_reading, federalism_principle).
narrative_ontology:constraint_vindicates(commerce_clause_text__substantial_effects_limited_reading, limited_government_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The ultimate arbiter of the Commerce Clause's scope, interpreting the 'substantial effects' test and the limits of federal power. Its rulings define the boundaries for both federal and state action.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, supreme_court, agenda_setter,
    institutional, generational, analytical, national).

% Seeks to enact legislation addressing national problems, often relying on the Commerce Clause. Benefits when its economic regulations are upheld, but pays when its attempts to regulate non-economic or pretextual activity are struck down by the courts.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, federal_government, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(commerce_clause_text__substantial_effects_limited_reading, federal_government, payer).

% Benefits from the protection of their traditional police powers against federal encroachment. Pays when federal economic regulation is upheld and preempts state law, or when they must conform to federal standards.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, state_governments, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(commerce_clause_text__substantial_effects_limited_reading, state_governments, payer).

% Seek predictable regulatory environments. Benefit from clear federal authority over genuinely interstate commerce, but pay compliance costs for both federal and state regulations, and bear the uncertainty of ongoing litigation.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, economic_actors, beneficiary,
    powerful, immediate, constrained, national).
narrative_ontology:stakeholder_secondary_role(commerce_clause_text__substantial_effects_limited_reading, economic_actors, payer).

% Analyze and critique the Supreme Court's Commerce Clause jurisprudence, influencing legal discourse and potential future interpretations. They do not directly benefit or pay but shape the intellectual landscape.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, legal_scholars, observer,
    analytical, generational, analytical, national).

% Advocate for specific policy outcomes, often through litigation or lobbying. Pay the costs of legal challenges and political engagement, but benefit when court rulings align with their policy goals, whether expanding or limiting federal power.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, civil_society_advocates, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(commerce_clause_text__substantial_effects_limited_reading, civil_society_advocates, beneficiary).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To define and coordinate the legitimate boundaries of federal and state regulatory authority over economic activity, preventing both federal overreach into traditional state police powers and state-level balkanization of national markets.
% TRANSFER_FUNCTION: Transfers regulatory authority and economic impact between the federal and state governments, depending on whether an intrastate activity is deemed to have a 'substantial effect' on interstate commerce and whether the regulation is genuinely economic.
% ABSENT_VOICES: Advocates for a purely localist vision of economic regulation (where federal power is almost entirely absent) or a purely nationalist vision (where federal power is virtually unlimited) are often marginalized in the balancing act this reading represents.
% DISAPPEARANCE_RATIONALE: If this limited reading of the Commerce Clause vanished, the balance of power between federal and state governments would fundamentally shift. Either federal power would expand dramatically (as in the expansive reading), or contract severely (as in the originalist reading), leading to a complete reorganization of regulatory authority and economic governance.
% FOUNDING_PROBLEM: The original problem was to grant Congress sufficient power to regulate a national economy while preserving a meaningful sphere of state sovereignty, avoiding the weaknesses of the Articles of Confederation without creating an omnipotent federal government.
% FOUNDING_PROBLEM_CORROBORATION: The ongoing litigation before the Supreme Court, the continuous debate in legal scholarship, and the legislative efforts by both federal and state governments to assert their authority all corroborate that the founding problem of balancing federal and state power remains live and contested. This is attested by legal historians and political scientists outside the direct beneficiaries of any specific outcome.
narrative_ontology:disappearance_verdict(commerce_clause_text__substantial_effects_limited_reading, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_text__substantial_effects_limited_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_text__substantial_effects_limited_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(commerce_clause_text__substantial_effects_limited_reading, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_text__substantial_effects_limited_reading, 0.55, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_text__substantial_effects_limited_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(commerce_clause_text__substantial_effects_limited_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(commerce_clause_text__substantial_effects_limited_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.55) because while this reading limits federal power, it still allows significant federal intervention, leading to ongoing costs for both federal and state actors in defining and defending their respective spheres. Suppression (0.60) is moderate-high, reflecting the active judicial enforcement required to maintain these boundaries against legislative and executive pressures from both federal and state levels. Resistance (0.70) is high due to the constant litigation and political debate surrounding the Commerce Clause's scope. Theater ratio is low (0.10) as the judicial function is genuinely active and consequential, not merely performative. Accessibility collapse is moderate (0.50) as it collapses the alternative of unlimited federal power but preserves state power, leaving a complex set of alternatives for regulatory action.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of federal officials seeking to address national problems, this reading can be seen as an arbitrary limitation on necessary governmental power. From the perspective of state officials, it can be seen as a vital safeguard of federalism. The engine's computation of per-seat classification will reflect these divergent experiences, with the federal government experiencing higher extraction when its laws are struck down, and states experiencing higher extraction when federal laws are upheld.
 *
 * DIRECTIONALITY LOGIC:
 *   The Supreme Court acts as the primary agenda-setter, defining the terms of the constraint. The federal government benefits when its economic regulations are upheld but is a target when its overreach is curtailed. Conversely, state governments benefit from the protection of their police powers but are targets when federal economic regulation preempts their authority. Economic actors seek predictability and national market access, benefiting from clear rules but bearing compliance costs. This dynamic creates a hybrid beneficiary/victim structure where both federal and state entities can be either depending on the specific context, characteristic of a Tangled Rope.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    economic_vs_non_economic_ambiguity,
    'How clear is the distinction between ''economic'' and ''non-economic'' activity for the purpose of Commerce Clause regulation?',
    'Further Supreme Court rulings providing clearer definitions or a legislative consensus on what constitutes ''economic'' activity in a modern context.',
    'If the distinction remains ambiguous, the constraint''s application will be inconsistent, leading to higher litigation costs and regulatory uncertainty. If clarified, it could reduce extractiveness and suppression by providing clearer boundaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_vs_non_economic_ambiguity, conceptual, 'Ambiguity in defining ''economic'' activity under the Commerce Clause.').

omega_variable(
    pretextual_regulation_detection,
    'How effectively can courts discern whether a federal regulation is genuinely economic or merely a pretext for exercising general police powers reserved to the states?',
    'Empirical analysis of judicial review outcomes and legislative intent, or the development of more robust legal tests for identifying pretextual legislation.',
    'If pretextual regulation is difficult to detect, federal power may effectively expand beyond the intended limits of this reading, increasing extraction from states. If detection is effective, the constraint''s limiting function is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pretextual_regulation_detection, empirical, 'Judicial capacity to detect pretextual federal regulation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_text__substantial_effects_limited_reading, 1995, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1995, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 1995, 0.1).
narrative_ontology:measurement(comm_tr_t2000, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(comm_tr_t2005, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 2005, 0.1).
narrative_ontology:measurement(comm_tr_t2010, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(comm_tr_t2015, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 2015, 0.1).
narrative_ontology:measurement(comm_tr_t2020, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 2020, 0.1).
narrative_ontology:measurement(comm_tr_t2025, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 2025, 0.1).

% Extraction over time
narrative_ontology:measurement(comm_be_t1995, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 1995, 0.6).
narrative_ontology:measurement(comm_be_t2000, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 2000, 0.58).
narrative_ontology:measurement(comm_be_t2005, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 2005, 0.55).
narrative_ontology:measurement(comm_be_t2010, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 2010, 0.53).
narrative_ontology:measurement(comm_be_t2015, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 2015, 0.54).
narrative_ontology:measurement(comm_be_t2020, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 2020, 0.55).
narrative_ontology:measurement(comm_be_t2025, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 2025, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1995, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 1995, 0.65).
narrative_ontology:measurement(comm_su_t2000, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 2000, 0.7).
narrative_ontology:measurement(comm_su_t2005, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 2005, 0.72).
narrative_ontology:measurement(comm_su_t2010, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 2010, 0.7).
narrative_ontology:measurement(comm_su_t2015, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 2015, 0.68).
narrative_ontology:measurement(comm_su_t2020, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 2020, 0.69).
narrative_ontology:measurement(comm_su_t2025, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 2025, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_text__substantial_effects_limited_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(commerce_clause_text__substantial_effects_limited_reading, commerce_clause_text__expansive_federal_reading).
narrative_ontology:affects_constraint(commerce_clause_text__substantial_effects_limited_reading, commerce_clause_text__originalist_narrow_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the Commerce Clause text, each with different structural implications for federal and state power. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
