% ============================================================================
% CONSTRAINT STORY: second_amendment_scope__individual_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_scope__individual_right_reading, []).

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
 *   constraint_id: second_amendment_scope__individual_right_reading
 *   human_readable: Second Amendment: Individual Right to Firearms Ownership
 *   domain: constitutional_law/political_theory/rights_jurisprudence
 *
 * SUMMARY:
 *   This constraint represents the 'individual right' reading of the Second
 *   Amendment, which interprets the right to bear arms as belonging to
 *   individuals for self-defense, largely unconnected to militia service.
 *   This reading, solidified by Supreme Court decisions like Heller (2008)
 *   and McDonald (2010), significantly constrains the ability of state and
 *   local governments to regulate firearms. It is a contested interpretation
 *   of a constitutional kernel, with other readings (collective right, civic
 *   right) offering alternative structural implications.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_scope__individual_right_reading, 0.65).
domain_priors:suppression_score(second_amendment_scope__individual_right_reading, 0.4).
domain_priors:theater_ratio(second_amendment_scope__individual_right_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_scope__individual_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_scope__individual_right_reading, "Second Amendment: Individual Right to Firearms Ownership").
narrative_ontology:topic_domain(second_amendment_scope__individual_right_reading, "constitutional_law/political_theory/rights_jurisprudence").

domain_priors:requires_active_enforcement(second_amendment_scope__individual_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_scope__individual_right_reading, '2d449084-d748-4ff6-b36e-d754d3eafd62').
narrative_ontology:cs_kernel_codification('2d449084-d748-4ff6-b36e-d754d3eafd62', fixed_text).
narrative_ontology:cs_authority_grounding('2d449084-d748-4ff6-b36e-d754d3eafd62', lineage).
narrative_ontology:cs_interpretation_layer_present('2d449084-d748-4ff6-b36e-d754d3eafd62').
narrative_ontology:cs_reading_relation('2d449084-d748-4ff6-b36e-d754d3eafd62', second_amendment_scope__collective_right_reading, forecloses).
narrative_ontology:cs_reading_relation('2d449084-d748-4ff6-b36e-d754d3eafd62', second_amendment_scope__civic_right_reading, forecloses).
narrative_ontology:cs_axiom('2d449084-d748-4ff6-b36e-d754d3eafd62', foundational, individual_right_unconnected_to_militia).
narrative_ontology:cs_axiom_status(individual_right_unconnected_to_militia, holdable).
narrative_ontology:cs_axiom_grounding('2d449084-d748-4ff6-b36e-d754d3eafd62', individual_right_unconnected_to_militia, deontological).
narrative_ontology:cs_axiom('2d449084-d748-4ff6-b36e-d754d3eafd62', secondary, self_defense_is_fundamental_right).
narrative_ontology:cs_axiom_status(self_defense_is_fundamental_right, holdable).
narrative_ontology:cs_axiom_grounding('2d449084-d748-4ff6-b36e-d754d3eafd62', self_defense_is_fundamental_right, deontological).
narrative_ontology:cs_reference_frame('2d449084-d748-4ff6-b36e-d754d3eafd62', post_heller_jurisprudence).
narrative_ontology:cs_drift_state('2d449084-d748-4ff6-b36e-d754d3eafd62', contemporary_public_discourse, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('2d449084-d748-4ff6-b36e-d754d3eafd62', '').
narrative_ontology:cs_kernel_id(second_amendment_scope__individual_right_reading, second_amendment_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_scope__individual_right_reading, individual_gun_owners).
narrative_ontology:constraint_beneficiary(second_amendment_scope__individual_right_reading, firearms_manufacturers).
narrative_ontology:constraint_beneficiary(second_amendment_scope__individual_right_reading, gun_rights_advocacy_groups).
narrative_ontology:constraint_victim(second_amendment_scope__individual_right_reading, state_legislatures).
narrative_ontology:constraint_victim(second_amendment_scope__individual_right_reading, local_governments).
narrative_ontology:constraint_victim(second_amendment_scope__individual_right_reading, gun_violence_victims_and_families).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_scope__individual_right_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(second_amendment_scope__individual_right_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_scope__individual_right_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_scope__individual_right_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_scope__individual_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it provides a coordination function (ensuring individual access to firearms for self-defense, as interpreted) but also involves significant asymmetric extraction. Extractiveness is high (0.65) due to the broad scope of individual rights and the strict scrutiny applied to gun control laws, which effectively extracts regulatory capacity from state and local governments. Suppression (0.40) is moderate, reflecting the active legal and political enforcement required to maintain this interpretation against legislative efforts to impose stricter controls. Theater ratio is low (0.10) as the enforcement is direct and functional, not performative. The increasing extractiveness over time reflects the expansion of this reading's scope and the corresponding reduction in state regulatory power.
 *
 * PERSPECTIVAL GAP:
 *   Individual gun owners and gun rights advocacy groups experience this as a protective Rope, safeguarding a fundamental right. State and local governments, along with gun violence victims and their families, experience it as a Snare or highly extractive Tangled Rope, as it severely limits their ability to enact public safety measures and imposes significant social costs.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual gun owners and firearms manufacturers are primary beneficiaries (d near 0.0) as the constraint protects their access and market. Gun rights advocacy groups are also beneficiaries, as their mandate is advanced. State and local governments are primary targets (d near 1.0) as their regulatory authority is curtailed. Gun violence victims and their families are also targets, bearing the social costs of reduced regulation. The constraint subsidizes the beneficiaries by extracting regulatory power from the victims.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate, from this reading's perspective, is to protect individual liberty. However, the high extractiveness on state regulatory power and the social costs associated with gun violence suggest a potential for mandatrophy, where the original coordination (individual self-defense) is overshadowed by the extraction of regulatory capacity. The 'contested' status of the founding problem further highlights this tension, indicating that the constraint's function may have shifted from its original intent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    militia_connection_ambiguity,
    'Is the individual right to bear arms truly unconnected to militia service, or does the ''well regulated Militia'' clause still impose a structural condition?',
    'Further Supreme Court rulings clarifying the ''well regulated Militia'' clause''s contemporary relevance, or a constitutional amendment explicitly severing or re-establishing the connection.',
    'If a connection is re-established, the scope of the individual right would narrow, potentially shifting the constraint''s classification towards a collective or civic right reading, reducing its extractiveness on state regulatory power.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(militia_connection_ambiguity, conceptual, 'Ambiguity regarding the militia clause''s impact on individual rights.').

omega_variable(
    regulatory_burden_justification,
    'Are the burdens placed on state and local governments by this reading (e.g., inability to enact certain gun control measures) a necessary cost of individual liberty, or an undue extraction of regulatory capacity?',
    'Empirical studies on the effectiveness of various gun control measures and their impact on public safety, alongside judicial re-evaluation of the ''strict scrutiny'' standard applied to gun laws.',
    'If the burdens are deemed undue extraction, the constraint''s effective extractiveness on state regulatory power would be re-evaluated upward, potentially strengthening the case for a Snare classification from the perspective of state governments.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_burden_justification, empirical, 'Justification of regulatory burdens on state governments.').

omega_variable(
    kernel_reading_identity,
    'This constraint is the ''individual_right_reading'' of the ''second_amendment_scope'' kernel. What would change if the ''collective_right_reading'' or ''civic_right_reading'' were adopted?',
    'A shift in judicial precedent or a constitutional amendment explicitly adopting one of the sibling readings.',
    'The ''collective_right_reading'' would shift beneficiaries from individuals to state entities, drastically reducing extractiveness on state regulatory power. The ''civic_right_reading'' would condition individual rights on militia participation, narrowing the beneficiary set and increasing regulatory leeway for states. Both would fundamentally alter the constraint''s structure and classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Impact of alternative readings of the Second Amendment kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_scope__individual_right_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_scope__individual_right_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(seco_tr_t10, second_amendment_scope__individual_right_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(seco_tr_t20, second_amendment_scope__individual_right_reading, theater_ratio, 20, 0.11).
narrative_ontology:measurement(seco_tr_t30, second_amendment_scope__individual_right_reading, theater_ratio, 30, 0.1).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_scope__individual_right_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(seco_be_t10, second_amendment_scope__individual_right_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(seco_be_t20, second_amendment_scope__individual_right_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(seco_be_t30, second_amendment_scope__individual_right_reading, base_extractiveness, 30, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_scope__individual_right_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(seco_su_t10, second_amendment_scope__individual_right_reading, suppression_requirement, 10, 0.3).
narrative_ontology:measurement(seco_su_t20, second_amendment_scope__individual_right_reading, suppression_requirement, 20, 0.35).
narrative_ontology:measurement(seco_su_t30, second_amendment_scope__individual_right_reading, suppression_requirement, 30, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_scope__individual_right_reading, identity_coordination).
narrative_ontology:affects_constraint(second_amendment_scope__individual_right_reading, gun_control_legislation_scope).
narrative_ontology:affects_constraint(second_amendment_scope__individual_right_reading, public_safety_policy_scope).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'second_amendment_scope' kernel. The other readings are 'collective_right_reading' and 'civic_right_reading', each with distinct structural implications and classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
