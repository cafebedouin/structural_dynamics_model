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
 *   constraint_id: commerce_clause_text__substantial_effects_limited_reading
 *   human_readable: Commerce Clause: Substantial Effects Doctrine (Limited Reading)
 *   domain: constitutional_law/federalism
 *
 * SUMMARY:
 *   This constraint represents a specific reading of the U.S. Constitution's
 *   Commerce Clause, asserting that federal power extends to intrastate
 *   activities with a 'substantial effect' on interstate commerce, but with
 *   crucial limitations: it must involve genuinely economic activity and
 *   require a jurisdictional nexus, preventing federal overreach into
 *   traditional state police powers. This reading attempts to balance
 *   national economic needs with federalism principles, distinguishing itself
 *   from both more expansive and more restrictive interpretations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_text__substantial_effects_limited_reading, 0.45).
domain_priors:suppression_score(commerce_clause_text__substantial_effects_limited_reading, 0.6).
domain_priors:theater_ratio(commerce_clause_text__substantial_effects_limited_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_text__substantial_effects_limited_reading, tangled_rope).
narrative_ontology:human_readable(commerce_clause_text__substantial_effects_limited_reading, "Commerce Clause: Substantial Effects Doctrine (Limited Reading)").
narrative_ontology:topic_domain(commerce_clause_text__substantial_effects_limited_reading, "constitutional_law/federalism").

domain_priors:requires_active_enforcement(commerce_clause_text__substantial_effects_limited_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_text__substantial_effects_limited_reading, '71896e87-a176-43e5-9a93-123f2a0b2c2b').
narrative_ontology:cs_kernel_codification('71896e87-a176-43e5-9a93-123f2a0b2c2b', fixed_text).
narrative_ontology:cs_authority_grounding('71896e87-a176-43e5-9a93-123f2a0b2c2b', lineage).
narrative_ontology:cs_interpretation_layer_present('71896e87-a176-43e5-9a93-123f2a0b2c2b').
narrative_ontology:cs_reading_relation('71896e87-a176-43e5-9a93-123f2a0b2c2b', commerce_clause_text__expansive_federal_reading, coexists_with).
narrative_ontology:cs_reading_relation('71896e87-a176-43e5-9a93-123f2a0b2c2b', commerce_clause_text__originalist_narrow_reading, coexists_with).
narrative_ontology:cs_axiom('71896e87-a176-43e5-9a93-123f2a0b2c2b', foundational, federal_power_limited_to_economic_activity).
narrative_ontology:cs_axiom_status(federal_power_limited_to_economic_activity, holdable).
narrative_ontology:cs_axiom_grounding('71896e87-a176-43e5-9a93-123f2a0b2c2b', federal_power_limited_to_economic_activity, conventional).
narrative_ontology:cs_axiom('71896e87-a176-43e5-9a93-123f2a0b2c2b', foundational, jurisdictional_nexus_required).
narrative_ontology:cs_axiom_status(jurisdictional_nexus_required, holdable).
narrative_ontology:cs_axiom_grounding('71896e87-a176-43e5-9a93-123f2a0b2c2b', jurisdictional_nexus_required, conventional).
narrative_ontology:cs_reference_frame('71896e87-a176-43e5-9a93-123f2a0b2c2b', post_new_deal_limited_federalism).
narrative_ontology:cs_drift_state('71896e87-a176-43e5-9a93-123f2a0b2c2b', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('71896e87-a176-43e5-9a93-123f2a0b2c2b', '').
narrative_ontology:cs_kernel_id(commerce_clause_text__substantial_effects_limited_reading, commerce_clause_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_text__substantial_effects_limited_reading, federal_government).
narrative_ontology:constraint_beneficiary(commerce_clause_text__substantial_effects_limited_reading, national_economic_actors).
narrative_ontology:constraint_victim(commerce_clause_text__substantial_effects_limited_reading, states_police_power).
narrative_ontology:constraint_victim(commerce_clause_text__substantial_effects_limited_reading, intrastate_non_economic_actors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exercises power over intrastate economic activities that substantially affect interstate commerce, but must demonstrate a jurisdictional nexus and avoid pretextual regulation of non-economic matters. Benefits from the ability to address national economic problems.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, federal_government, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from a uniform national regulatory environment for genuinely economic activities, reducing compliance costs across state lines. They are subject to federal regulation but prefer it to a patchwork of state laws.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, national_economic_actors, beneficiary,
    powerful, biographical, mobile, national).

% Bears the cost of federal preemption in areas where intrastate economic activity is regulated. Must demonstrate that its own regulations are not unduly burdensome on interstate commerce and are not disguised economic regulation.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, states_police_power, payer,
    institutional, generational, constrained, national).

% Are vulnerable to federal overreach if their non-economic activities are recharacterized as having a substantial effect on interstate commerce. They bear the cost of defending against federal regulation in areas traditionally reserved for state police power.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, intrastate_non_economic_actors, payer,
    moderate, biographical, trapped, local).

% Acts as the ultimate arbiter of the Commerce Clause's scope, defining the boundaries between federal and state power. Its rulings enforce the jurisdictional nexus and non-pretextual economic regulation requirements.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, supreme_court, agenda_setter,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates federal and state regulatory authority by allowing federal intervention in genuinely economic intrastate activities that have a substantial effect on interstate commerce, while preserving a sphere for state police power over non-economic matters.
% TRANSFER_FUNCTION: Transfers regulatory authority from states to the federal government for certain economic activities, and from individuals/states to the federal government for compliance with national economic policy.
% ABSENT_VOICES: Advocates for a more expansive federal power (e.g., to address social issues with indirect economic links) are constrained by the 'economic activity' and 'jurisdictional nexus' requirements. Conversely, strict originalists who would limit federal power to direct interstate trade are also excluded from the current interpretive consensus.
% DISAPPEARANCE_RATIONALE: If this limited reading of the Commerce Clause vanished, the balance of federal and state power would be fundamentally altered. Either federal power would expand unchecked into traditional state domains (if the 'substantial effects' test became unlimited) or contract severely (if only direct interstate trade was regulated), leading to a complete reorganization of regulatory authority and economic governance.
% FOUNDING_PROBLEM: The original Commerce Clause was intended to prevent states from erecting trade barriers and to ensure a unified national market, but its precise scope regarding intrastate activity was ambiguous.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars and constitutional historians attest to the ongoing tension between federal and state power, and the Supreme Court's continued engagement with Commerce Clause cases corroborates the live status of the problem. The federal government and national economic actors also attest to the need for national economic regulation.
narrative_ontology:disappearance_verdict(commerce_clause_text__substantial_effects_limited_reading, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_text__substantial_effects_limited_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_text__substantial_effects_limited_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(commerce_clause_text__substantial_effects_limited_reading, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_text__substantial_effects_limited_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_text__substantial_effects_limited_reading_tests).
:- end_tests(commerce_clause_text__substantial_effects_limited_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) is moderate because while federal power is significant, it is not unlimited, and the 'economic activity' and 'jurisdictional nexus' requirements provide some checks. Suppression (0.6) is moderate-high due to the Supreme Court's active role in policing the boundaries of federal power, requiring states and non-economic actors to conform to these distinctions. Theater ratio (0.2) is low, as the distinction between economic and non-economic activity, while sometimes difficult to apply, is a genuine attempt to define the scope of federal power, not merely a performance.
 *
 * PERSPECTIVAL GAP:
 *   From the federal government's perspective, this reading provides necessary tools for national economic governance. From the states' perspective, it represents a constant tension and potential encroachment on their reserved powers, requiring active defense of their jurisdictional boundaries. The classification as a Tangled Rope reflects this inherent tension between coordination and extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal government and national economic actors are beneficiaries, gaining from the ability to regulate and operate in a unified national market for economic activities. States and intrastate non-economic actors are payers, bearing the cost of federal preemption and the need to justify their own regulations within the defined boundaries. The Supreme Court acts as an agenda-setter, defining and enforcing these boundaries.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    economic_vs_non_economic_ambiguity,
    'Is the distinction between ''economic'' and ''non-economic'' activity a stable, empirically verifiable category, or is it a conceptual distinction subject to judicial reinterpretation and political contestation?',
    'Analysis of Supreme Court jurisprudence over time: if the distinction consistently shifts based on judicial philosophy rather than clear empirical criteria, it suggests conceptual ambiguity. Longitudinal studies of legislative intent and impact on state police powers.',
    'If the distinction is unstable, the constraint''s effective suppression on states and non-economic actors is higher, as the boundary of federal power becomes less predictable, potentially pushing the classification closer to a Snare. If stable, it reinforces the Tangled Rope classification by confirming a genuine coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_vs_non_economic_ambiguity, conceptual, 'Ambiguity in the ''economic vs. non-economic'' distinction for Commerce Clause cases.').

omega_variable(
    jurisdictional_nexus_pretext_detection,
    'How effectively can courts detect and prevent federal regulation that is pretextually framed as economic but is actually an attempt to regulate non-economic activity traditionally reserved to the states?',
    'Empirical study of federal legislation and subsequent judicial review: track the success rate of challenges to federal laws on Commerce Clause grounds, specifically those alleging pretextual economic regulation. Analyze dissenting opinions for patterns of concern.',
    'If pretextual regulation is frequently upheld, the constraint''s suppression is effectively higher, as the ''limited'' aspect of the reading becomes performative, increasing extraction from states. If courts consistently strike down such laws, it reinforces the constraint''s function in preserving federalism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(jurisdictional_nexus_pretext_detection, empirical, 'Effectiveness of courts in preventing pretextual federal regulation under the Commerce Clause.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_text__substantial_effects_limited_reading, 1995, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1995, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 1995, 0.15).
narrative_ontology:measurement(comm_tr_t2005, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 2005, 0.18).
narrative_ontology:measurement(comm_tr_t2015, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 2015, 0.19).
narrative_ontology:measurement(comm_tr_t2024, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(comm_be_t1995, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 1995, 0.4).
narrative_ontology:measurement(comm_be_t2005, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 2005, 0.42).
narrative_ontology:measurement(comm_be_t2015, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 2015, 0.44).
narrative_ontology:measurement(comm_be_t2024, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1995, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 1995, 0.55).
narrative_ontology:measurement(comm_su_t2005, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 2005, 0.57).
narrative_ontology:measurement(comm_su_t2015, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 2015, 0.59).
narrative_ontology:measurement(comm_su_t2024, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
