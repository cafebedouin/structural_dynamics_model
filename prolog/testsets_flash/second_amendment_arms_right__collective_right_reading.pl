% ============================================================================
% CONSTRAINT STORY: second_amendment_arms_right__collective_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_arms_right__collective_right_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: second_amendment_arms_right__collective_right_reading
 *   human_readable: Second Amendment Collective Right Reading (State Militia Authority)
 *   domain: constitutional_law/political_philosophy/legal_interpretation
 *
 * SUMMARY:
 *   This constraint represents the 'collective right' reading of the Second
 *   Amendment, which interprets the right to keep and bear arms as primarily
 *   protecting the authority of state governments to maintain organized
 *   militias, rather than an individual's right to own arms for any purpose
 *   outside of militia service. Under this reading, individual arms ownership
 *   is largely subject to state regulation. This reading was dominant for
 *   much of US history until challenged by more individualistic
 *   interpretations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_arms_right__collective_right_reading, 0.15).
domain_priors:suppression_score(second_amendment_arms_right__collective_right_reading, 0.2).
domain_priors:theater_ratio(second_amendment_arms_right__collective_right_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_arms_right__collective_right_reading, rope).
narrative_ontology:human_readable(second_amendment_arms_right__collective_right_reading, "Second Amendment Collective Right Reading (State Militia Authority)").
narrative_ontology:topic_domain(second_amendment_arms_right__collective_right_reading, "constitutional_law/political_philosophy/legal_interpretation").

domain_priors:requires_active_enforcement(second_amendment_arms_right__collective_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_arms_right__collective_right_reading, 'adad2303-b3c7-43e7-ab2b-eeaae9faba0a').
narrative_ontology:cs_kernel_codification('adad2303-b3c7-43e7-ab2b-eeaae9faba0a', fixed_text).
narrative_ontology:cs_authority_grounding('adad2303-b3c7-43e7-ab2b-eeaae9faba0a', lineage).
narrative_ontology:cs_interpretation_layer_present('adad2303-b3c7-43e7-ab2b-eeaae9faba0a').
narrative_ontology:cs_reading_relation('adad2303-b3c7-43e7-ab2b-eeaae9faba0a', second_amendment_arms_right__individual_right_reading, forecloses).
narrative_ontology:cs_reading_relation('adad2303-b3c7-43e7-ab2b-eeaae9faba0a', second_amendment_arms_right__civic_republican_reading, coexists_with).
narrative_ontology:cs_axiom('adad2303-b3c7-43e7-ab2b-eeaae9faba0a', foundational, militia_clause_is_operative_clause).
narrative_ontology:cs_axiom_status(militia_clause_is_operative_clause, holdable).
narrative_ontology:cs_axiom_grounding('adad2303-b3c7-43e7-ab2b-eeaae9faba0a', militia_clause_is_operative_clause, conventional).
narrative_ontology:cs_axiom('adad2303-b3c7-43e7-ab2b-eeaae9faba0a', foundational, individual_right_is_subordinate_to_militia).
narrative_ontology:cs_axiom_status(individual_right_is_subordinate_to_militia, holdable).
narrative_ontology:cs_axiom_grounding('adad2303-b3c7-43e7-ab2b-eeaae9faba0a', individual_right_is_subordinate_to_militia, conventional).
narrative_ontology:cs_reference_frame('adad2303-b3c7-43e7-ab2b-eeaae9faba0a', founding_era_state_sovereignty).
narrative_ontology:cs_drift_state('adad2303-b3c7-43e7-ab2b-eeaae9faba0a', post_heller_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('adad2303-b3c7-43e7-ab2b-eeaae9faba0a', '').
narrative_ontology:cs_kernel_id(second_amendment_arms_right__collective_right_reading, second_amendment_arms_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__collective_right_reading, state_governments).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__collective_right_reading, organized_militias).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(second_amendment_arms_right__collective_right_reading, individual_citizens_outside_militia).
narrative_ontology:constraint_vindicates(second_amendment_arms_right__collective_right_reading, state_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(second_amendment_arms_right__collective_right_reading, federalism_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the interpretation that grants them primary authority over the organization and arming of militias, allowing them to regulate arms ownership outside of this context without federal interference. This reading reinforces their power relative to the federal government.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, state_governments, beneficiary,
    institutional, generational, constrained, national).

% Are the direct object of the right's protection, ensuring their ability to be armed and effective. This reading secures their institutional existence and operational capacity.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, organized_militias, beneficiary,
    organized, biographical, constrained, national).

% Is constrained from infringing on state militia authority, but retains broad power to regulate individual arms ownership. Its role is to respect state authority while legislating on arms outside the militia context.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, federal_government, agenda_setter,
    institutional, generational, constrained, national).

% Bear the cost of this interpretation through extensive state-level regulation of their arms ownership, as their right to bear arms is not seen as independently protected by the Second Amendment. Their ability to own certain types of arms or carry them is subject to state legislative discretion.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, individual_citizens_outside_militia, payer,
    powerless, biographical, constrained, local).

% Are excluded from the core interpretive framework of this reading, as their advocacy for an expansive individual right to bear arms is directly contradicted. They would argue for a different reading of the Second Amendment.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, gun_rights_advocates_individualist, excluded,
    organized, generational, constrained, national).

% Analyze and defend this interpretation based on historical context, textual analysis, and constitutional structure. They provide the intellectual grounding for the collective right reading.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, legal_scholars_collective_right, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the relationship between the federal government and state governments regarding military power, ensuring states retain the capacity for self-defense through organized militias without federal interference, while allowing states to regulate individual arms ownership.
% TRANSFER_FUNCTION: Transfers regulatory authority over individual arms ownership from the federal government (as a protected individual right) to state governments (as a matter of state police power), while securing state militia capacity.
% ABSENT_VOICES: Advocates for an expansive individual right to bear arms are largely absent from the foundational logic of this reading; they would argue that the individual right is paramount and not contingent on militia service, and that state regulation is an infringement.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the balance of power between federal and state governments regarding arms regulation would fundamentally shift. Federal authority to regulate individual arms might expand, or a new individual right interpretation would emerge, leading to widespread legal challenges and a reorganization of arms control laws across the nation.
% FOUNDING_PROBLEM: The founding problem was to balance federal power with state sovereignty, particularly concerning the maintenance of state militias for security and defense against both foreign threats and potential federal overreach, while avoiding a standing federal army.
% FOUNDING_PROBLEM_CORROBORATION: Historians and constitutional scholars, independent of state governments, corroborate that the concern for state militias and the balance of power was a central issue during the founding era. The problem of balancing federal and state power, and the role of armed citizens, remains a live issue in contemporary political discourse, though its specific manifestations have evolved.
narrative_ontology:disappearance_verdict(second_amendment_arms_right__collective_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_arms_right__collective_right_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_arms_right__collective_right_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(second_amendment_arms_right__collective_right_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_arms_right__collective_right_reading_tests).
:- end_tests(second_amendment_arms_right__collective_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because the constraint primarily defines a division of power, not a direct extraction from individuals. Suppression is also low (0.2) as it reflects the state's legitimate regulatory power rather than coercive suppression of a fundamental right. Theater ratio is low (0.1) as the function of defining state authority is genuine. The metrics reflect the period before the individual right reading gained significant judicial traction, where this interpretation was largely stable.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state governments, this is a foundational 'rope' that coordinates federal-state relations and secures their sovereign capacity. From the perspective of individual citizens seeking an unfettered right to bear arms, it operates as a 'snare' that denies their perceived liberty. The engine's classification will reflect the structural position of each seat.
 *
 * DIRECTIONALITY LOGIC:
 *   State governments and organized militias are beneficiaries (d near 0.0) as this reading secures their constitutional authority and existence. The federal government acts as an agenda-setter (d near 0.5), bound by the constraint but also defining its scope. Individual citizens outside the militia context are payers (d near 1.0) as their arms ownership is subject to state regulation, which can be extensive. Gun rights advocates for an individualist reading are excluded, as their core premise is rejected by this interpretation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_intent_ambiguity,
    'Was the primary intent of the Second Amendment to protect state militias, an individual right, or a civic duty?',
    'Further historical and textual analysis of founding era documents, debates, and state constitutional provisions, with consensus among constitutional historians.',
    'Resolution would strengthen or weaken the foundational claim of this reading, potentially shifting its perceived legitimacy and resistance levels. If intent is found to be purely individual, this reading''s ''rope'' classification would be challenged.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_intent_ambiguity, empirical, 'Ambiguity regarding the original intent of the Second Amendment.').

omega_variable(
    militia_definition_drift,
    'Has the definition and role of ''militia'' evolved such that the original collective right justification is no longer applicable?',
    'Legal and sociological analysis of the contemporary relevance of ''well-regulated militia'' in modern society, and judicial reinterpretation of the term.',
    'If the militia concept is deemed obsolete, the ''collective right'' reading loses its primary grounding, potentially leading to its reclassification as a ''piton'' or ''snare'' if maintained for other purposes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(militia_definition_drift, conceptual, 'Drift in the meaning of ''militia'' over time.').

omega_variable(
    reading_legitimacy_contest,
    'Is this reading''s claim to constitutional legitimacy genuinely holdable, or is it primarily maintained by institutional inertia and political preference?',
    'Sustained judicial precedent upholding this reading against challenges, or a shift in the composition of the Supreme Court that explicitly reaffirms this interpretation.',
    'If found to be based on inertia, the ''rope'' classification would be challenged, potentially reclassifying it as a ''piton'' or ''tangled_rope'' if it serves to extract political power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_legitimacy_contest, preference, 'Contest over the legitimacy of the collective right reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_arms_right__collective_right_reading, 1791, 2008).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1791, second_amendment_arms_right__collective_right_reading, theater_ratio, 1791, 0.05).
narrative_ontology:measurement(seco_tr_t1850, second_amendment_arms_right__collective_right_reading, theater_ratio, 1850, 0.07).
narrative_ontology:measurement(seco_tr_t1900, second_amendment_arms_right__collective_right_reading, theater_ratio, 1900, 0.08).
narrative_ontology:measurement(seco_tr_t1950, second_amendment_arms_right__collective_right_reading, theater_ratio, 1950, 0.09).
narrative_ontology:measurement(seco_tr_t2008, second_amendment_arms_right__collective_right_reading, theater_ratio, 2008, 0.1).

% Extraction over time
narrative_ontology:measurement(seco_be_t1791, second_amendment_arms_right__collective_right_reading, base_extractiveness, 1791, 0.1).
narrative_ontology:measurement(seco_be_t1850, second_amendment_arms_right__collective_right_reading, base_extractiveness, 1850, 0.12).
narrative_ontology:measurement(seco_be_t1900, second_amendment_arms_right__collective_right_reading, base_extractiveness, 1900, 0.13).
narrative_ontology:measurement(seco_be_t1950, second_amendment_arms_right__collective_right_reading, base_extractiveness, 1950, 0.14).
narrative_ontology:measurement(seco_be_t2008, second_amendment_arms_right__collective_right_reading, base_extractiveness, 2008, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t1791, second_amendment_arms_right__collective_right_reading, suppression_requirement, 1791, 0.15).
narrative_ontology:measurement(seco_su_t1850, second_amendment_arms_right__collective_right_reading, suppression_requirement, 1850, 0.17).
narrative_ontology:measurement(seco_su_t1900, second_amendment_arms_right__collective_right_reading, suppression_requirement, 1900, 0.18).
narrative_ontology:measurement(seco_su_t1950, second_amendment_arms_right__collective_right_reading, suppression_requirement, 1950, 0.19).
narrative_ontology:measurement(seco_su_t2008, second_amendment_arms_right__collective_right_reading, suppression_requirement, 2008, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_arms_right__collective_right_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_arms_right__collective_right_reading, second_amendment_arms_right__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_arms_right__collective_right_reading, second_amendment_arms_right__civic_republican_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Second Amendment arms right. Its 'collective right' interpretation directly influences and is influenced by other readings, particularly the 'individual right' and 'civic republican' interpretations, which offer competing frameworks for arms regulation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
