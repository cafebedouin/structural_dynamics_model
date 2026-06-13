% ============================================================================
% CONSTRAINT STORY: second_amendment_boundary__militia_conditioned_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_boundary__militia_conditioned_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: second_amendment_boundary__militia_conditioned_reading
 *   human_readable: Second Amendment: Militia-Conditioned Right to Bear Arms
 *   domain: constitutional_law/political_theory/firearms_policy
 *
 * SUMMARY:
 *   This constraint represents the 'militia-conditioned' reading of the
 *   Second Amendment, where the prefatory clause ('A well regulated Militia,
 *   being necessary to the security of a free State') defines the scope of
 *   the operative clause ('the right of the people to keep and bear Arms').
 *   Under this reading, the right to bear arms is primarily tied to the
 *   collective defense function of a militia, allowing for substantial state
 *   regulation of private firearm possession. It presumes legitimate state
 *   authority to regulate firearms for public safety.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_boundary__militia_conditioned_reading, 0.3).
domain_priors:suppression_score(second_amendment_boundary__militia_conditioned_reading, 0.4).
domain_priors:theater_ratio(second_amendment_boundary__militia_conditioned_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_boundary__militia_conditioned_reading, rope).
narrative_ontology:human_readable(second_amendment_boundary__militia_conditioned_reading, "Second Amendment: Militia-Conditioned Right to Bear Arms").
narrative_ontology:topic_domain(second_amendment_boundary__militia_conditioned_reading, "constitutional_law/political_theory/firearms_policy").

domain_priors:requires_active_enforcement(second_amendment_boundary__militia_conditioned_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_boundary__militia_conditioned_reading, '960e9ba8-361d-4e09-bc2d-0da74b36fd70').
narrative_ontology:cs_kernel_codification('960e9ba8-361d-4e09-bc2d-0da74b36fd70', fixed_text).
narrative_ontology:cs_authority_grounding('960e9ba8-361d-4e09-bc2d-0da74b36fd70', lineage).
narrative_ontology:cs_interpretation_layer_present('960e9ba8-361d-4e09-bc2d-0da74b36fd70').
narrative_ontology:cs_reading_relation('960e9ba8-361d-4e09-bc2d-0da74b36fd70', second_amendment_boundary__individual_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('960e9ba8-361d-4e09-bc2d-0da74b36fd70', second_amendment_boundary__insurrectionist_reading, coexists_with).
narrative_ontology:cs_axiom('960e9ba8-361d-4e09-bc2d-0da74b36fd70', foundational, militia_clause_defines_scope).
narrative_ontology:cs_axiom_status(militia_clause_defines_scope, holdable).
narrative_ontology:cs_axiom_grounding('960e9ba8-361d-4e09-bc2d-0da74b36fd70', militia_clause_defines_scope, conventional).
narrative_ontology:cs_axiom('960e9ba8-361d-4e09-bc2d-0da74b36fd70', foundational, collective_defense_primary_purpose).
narrative_ontology:cs_axiom_status(collective_defense_primary_purpose, holdable).
narrative_ontology:cs_axiom_grounding('960e9ba8-361d-4e09-bc2d-0da74b36fd70', collective_defense_primary_purpose, deontological).
narrative_ontology:cs_reference_frame('960e9ba8-361d-4e09-bc2d-0da74b36fd70', original_collective_right_framing).
narrative_ontology:cs_drift_state('960e9ba8-361d-4e09-bc2d-0da74b36fd70', contemporary_judicial_interpretations, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('960e9ba8-361d-4e09-bc2d-0da74b36fd70', '').
narrative_ontology:cs_kernel_id(second_amendment_boundary__militia_conditioned_reading, second_amendment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_boundary__militia_conditioned_reading, state_legislatures).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__militia_conditioned_reading, public_safety_advocates).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__militia_conditioned_reading, general_public).
narrative_ontology:constraint_victim(second_amendment_boundary__militia_conditioned_reading, gun_owners_in_high_regulation_jurisdictions).
narrative_ontology:constraint_victim(second_amendment_boundary__militia_conditioned_reading, firearms_collectors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(second_amendment_boundary__militia_conditioned_reading, firearms_manufacturers_and_retailers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Empowered to enact comprehensive firearms regulations based on this reading, balancing public safety with the collective defense purpose of a militia. They benefit from the flexibility to address local public safety concerns.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, state_legislatures, agenda_setter,
    institutional, generational, mobile, national).

% Benefit from the legal framework that supports stricter gun control measures, aligning with their goals of reducing gun violence. They actively lobby for and defend such regulations.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, public_safety_advocates, beneficiary,
    organized, generational, mobile, national).

% Benefits from the perceived and actual reduction in gun violence and enhanced public safety due to regulations. However, individual members may also be subject to restrictions on firearm ownership.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, general_public, beneficiary,
    moderate, biographical, constrained, national).

% Bear the direct costs of compliance with strict firearm laws, including restrictions on types of weapons, magazine capacity, and licensing requirements. Their ability to own firearms for self-defense or sport is significantly curtailed.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, gun_owners_in_high_regulation_jurisdictions, payer,
    powerless, biographical, constrained, local).

% Face restrictions on acquiring and possessing certain historical or rare firearms, as regulations often focus on functional characteristics rather than historical value. This impacts their hobby and investment.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, firearms_collectors, payer,
    moderate, biographical, constrained, national).

% Experience reduced market access and sales for certain products due to bans or restrictions. They must adapt their product lines and distribution strategies to comply with diverse state laws, increasing operational costs.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, firearms_manufacturers_and_retailers, payer,
    powerful, generational, constrained, national).

% Actively challenge this reading in courts and legislatures, advocating for an individual right interpretation. While they participate in the political process, their preferred interpretation is largely excluded from the legal framework this reading establishes.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, gun_rights_advocacy_groups, excluded,
    organized, generational, mobile, national).

% Analyze the historical context, textual meaning, and legal implications of this reading, contributing to the ongoing academic and public debate without directly benefiting or paying from its operation.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_boundary__militia_conditioned_reading, diffuse).
narrative_ontology:fixing_cost_class(second_amendment_boundary__militia_conditioned_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates public safety by enabling state governments to regulate firearms, ensuring that the right to bear arms serves the collective purpose of a 'well regulated Militia' rather than unrestricted individual possession.
% TRANSFER_FUNCTION: Transfers regulatory authority over firearms from individuals to state governments, and potentially transfers a sense of security to the general public at the cost of restricted firearm access for some gun owners.
% ABSENT_VOICES: Advocates for an unconditioned individual right to bear arms are present in the political discourse but are structurally excluded from the legal framework established by this reading. They would argue that the right is fundamental and not subject to the militia clause's conditioning.
% DISAPPEARANCE_RATIONALE: If this reading disappeared, state governments would lose a primary legal basis for firearms regulation, leading to a rapid deregulation of gun ownership. This would likely result in a significant increase in firearm availability and potentially gun violence, fundamentally altering public safety and the balance of power between individuals and the state.
% FOUNDING_PROBLEM: The founding problem was how to balance the need for a citizen militia for collective defense with concerns about individual liberty and potential misuse of arms, particularly in a post-revolutionary context with a nascent federal government.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars and historians attest to the historical context of the militia's importance for state security. Public safety advocates and state legislatures corroborate that the problem of balancing collective security with individual arms possession remains live, citing ongoing gun violence and the need for regulatory flexibility. Gun rights groups contest the 'live' status, arguing the original intent was broader individual liberty.
narrative_ontology:disappearance_verdict(second_amendment_boundary__militia_conditioned_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_boundary__militia_conditioned_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_boundary__militia_conditioned_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(second_amendment_boundary__militia_conditioned_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_boundary__militia_conditioned_reading_tests).
:- end_tests(second_amendment_boundary__militia_conditioned_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.3) is moderate, as it restricts certain types of firearm ownership or use, but does not prohibit all possession. Suppression (0.4) is also moderate, reflecting the active enforcement of state-level regulations (e.g., licensing, bans on certain weapons). Theater ratio (0.1) is low, as the regulatory actions are genuinely aimed at public safety, not merely performative. Accessibility collapse (0.6) is moderate, as alternatives (e.g., non-firearm self-defense, participation in state-regulated militias) exist but are not always preferred. Resistance (0.5) is moderate, reflecting ongoing legal and political challenges from gun rights advocates.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state legislatures and public safety advocates, this reading is a legitimate and necessary coordination mechanism for public safety. From the perspective of gun owners whose rights are restricted, it is an extractive constraint that infringes on a fundamental right. The engine's per-seat classification will reflect this divergence based on the declared roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   State legislatures and public safety advocates are primary beneficiaries (d near 0.0), as this reading empowers them to enact and enforce regulations for collective safety. The general public also benefits from perceived increased safety. Gun owners in high-regulation jurisdictions and firearms collectors are victims (d near 1.0), as their ability to acquire and possess certain firearms is restricted. Their exit options are constrained by legal frameworks and the high cost of non-compliance.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling legitimate public safety regulation as pure extraction by grounding the right in a collective purpose. It acknowledges that while some individuals bear costs, the overall structure aims to coordinate collective security. The ongoing contestation with other readings, however, means its mandate is constantly re-evaluated.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    militia_definition_ambiguity,
    'What constitutes a ''well regulated Militia'' in the contemporary context, and does it include all able-bodied citizens or only organized state-controlled forces?',
    'Further judicial clarification or legislative definition of ''militia'' in modern terms.',
    'A broad definition could expand the scope of who is considered part of the ''militia,'' potentially shifting the balance of regulatory power. A narrow definition reinforces state control.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(militia_definition_ambiguity, conceptual, 'Ambiguity in the definition of ''militia'' affects the scope of the right.').

omega_variable(
    reading_impact_on_individual_right,
    'To what extent does this militia-conditioned reading foreclose or merely constrain the individual_right_reading?',
    'Judicial precedent explicitly stating the relationship between the prefatory and operative clauses, or a constitutional amendment clarifying the right''s scope.',
    'If it forecloses, the individual right is nullified; if it merely constrains, the individual right exists but is subject to significant state regulation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_impact_on_individual_right, conceptual, 'The degree to which this reading negates or limits the individual right interpretation.').

omega_variable(
    natural_law_vs_constructed_right,
    'Is the right to bear arms a natural, pre-existing right, or a constructed right granted and conditioned by the state?',
    'Philosophical consensus on the origin of rights, or a clear statement within the constitutional text itself.',
    'If natural, state regulation is inherently suspect; if constructed, state regulation is a legitimate exercise of democratic power.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_right, conceptual, 'The fundamental nature of the right to bear arms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_boundary__militia_conditioned_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(seco_be_t10, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 10, 0.25).
narrative_ontology:measurement(seco_be_t20, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 20, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(seco_su_t10, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 10, 0.35).
narrative_ontology:measurement(seco_su_t20, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 20, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_boundary__militia_conditioned_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_boundary__militia_conditioned_reading, second_amendment_boundary__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_boundary__militia_conditioned_reading, second_amendment_boundary__insurrectionist_reading).
narrative_ontology:affects_constraint(second_amendment_boundary__militia_conditioned_reading, firearms_market_regulation).
narrative_ontology:affects_constraint(second_amendment_boundary__militia_conditioned_reading, public_safety_legislation).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the Second Amendment boundary kernel. Each reading has a different structural impact on regulatory authority and individual rights, leading to different extraction profiles and classifications. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
