% ============================================================================
% CONSTRAINT STORY: separation_of_powers_text__unitary_executive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_separation_of_powers_text__unitary_executive_reading, []).

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
 *   constraint_id: separation_of_powers_text__unitary_executive_reading
 *   human_readable: Unitary Executive Principle (as a reading of Separation of Powers)
 *   domain: constitutional_law/political_theory/administrative_law
 *
 * SUMMARY:
 *   This constraint represents the 'unitary executive' reading of the U.S.
 *   Constitution's separation of powers, which posits that all executive
 *   power vests solely in the President, implying that independent agencies
 *   with insulated heads violate this principle. This reading seeks to
 *   consolidate presidential control over the entire executive branch,
 *   challenging the traditional understanding of checks and balances and the
 *   role of independent administrative bodies. The claimed type is
 *   'tangled_rope' because it offers a coordination function (unified
 *   executive action) but with significant asymmetric extraction from
 *   independent agencies and other branches of government.
 *
 * KEY AGENTS:
 *   - the_president: Agenda setter (institutional/constrained)
 *   - executive_branch_officials: Beneficiary (powerful/constrained)
 *   - independent_agencies: Payer (institutional/trapped)
 *   - congressional_oversight_committees: Payer (institutional/constrained)
 *   - judiciary: Payer (institutional/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(separation_of_powers_text__unitary_executive_reading, 0.7).
domain_priors:suppression_score(separation_of_powers_text__unitary_executive_reading, 0.6).
domain_priors:theater_ratio(separation_of_powers_text__unitary_executive_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(separation_of_powers_text__unitary_executive_reading, tangled_rope).
narrative_ontology:human_readable(separation_of_powers_text__unitary_executive_reading, "Unitary Executive Principle (as a reading of Separation of Powers)").
narrative_ontology:topic_domain(separation_of_powers_text__unitary_executive_reading, "constitutional_law/political_theory/administrative_law").

domain_priors:requires_active_enforcement(separation_of_powers_text__unitary_executive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(separation_of_powers_text__unitary_executive_reading, 'f12f2944-a18a-474c-aa29-7a415f7cacd5').
narrative_ontology:cs_kernel_codification('f12f2944-a18a-474c-aa29-7a415f7cacd5', fixed_text).
narrative_ontology:cs_authority_grounding('f12f2944-a18a-474c-aa29-7a415f7cacd5', lineage).
narrative_ontology:cs_interpretation_layer_present('f12f2944-a18a-474c-aa29-7a415f7cacd5').
narrative_ontology:cs_reading_relation('f12f2944-a18a-474c-aa29-7a415f7cacd5', separation_of_powers_text__formalist_reading, forecloses).
narrative_ontology:cs_reading_relation('f12f2944-a18a-474c-aa29-7a415f7cacd5', separation_of_powers_text__functionalist_reading, forecloses).
narrative_ontology:cs_axiom('f12f2944-a18a-474c-aa29-7a415f7cacd5', foundational, all_executive_power_vests_in_president).
narrative_ontology:cs_axiom_status(all_executive_power_vests_in_president, holdable).
narrative_ontology:cs_axiom_grounding('f12f2944-a18a-474c-aa29-7a415f7cacd5', all_executive_power_vests_in_president, deontological).
narrative_ontology:cs_axiom('f12f2944-a18a-474c-aa29-7a415f7cacd5', foundational, presidential_removal_power_is_absolute).
narrative_ontology:cs_axiom_status(presidential_removal_power_is_absolute, holdable).
narrative_ontology:cs_axiom_grounding('f12f2944-a18a-474c-aa29-7a415f7cacd5', presidential_removal_power_is_absolute, deontological).
narrative_ontology:cs_reference_frame('f12f2944-a18a-474c-aa29-7a415f7cacd5', energetic_executive_framing).
narrative_ontology:cs_drift_state('f12f2944-a18a-474c-aa29-7a415f7cacd5', contemporary_administrative_state, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('f12f2944-a18a-474c-aa29-7a415f7cacd5', '').
narrative_ontology:cs_kernel_id(separation_of_powers_text__unitary_executive_reading, separation_of_powers_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(separation_of_powers_text__unitary_executive_reading, the_president).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__unitary_executive_reading, executive_branch_officials).
narrative_ontology:constraint_victim(separation_of_powers_text__unitary_executive_reading, independent_agencies).
narrative_ontology:constraint_victim(separation_of_powers_text__unitary_executive_reading, congressional_oversight_committees).
narrative_ontology:constraint_victim(separation_of_powers_text__unitary_executive_reading, judiciary).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the consolidation of executive power, gaining absolute removal authority over executive officials and independent agency heads. This enhances control over policy implementation and reduces checks on presidential authority.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, the_president, agenda_setter,
    institutional, biographical, constrained, national).

% Benefit from clearer lines of authority and reduced bureaucratic friction, as policy directives flow more directly from the President. Their power is enhanced within the executive hierarchy, but they are also more directly accountable to the President.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, executive_branch_officials, beneficiary,
    powerful, biographical, constrained, national).

% Are the primary targets, as their independence (especially from presidential removal) is challenged. This reading seeks to strip them of their insulation, making them directly subservient to presidential will and potentially undermining their expert-driven, non-partisan functions.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, independent_agencies, payer,
    institutional, generational, trapped, national).

% Lose influence over the executive branch as independent agencies, often created by Congress to implement specific mandates, become less responsive to legislative direction. This diminishes Congress's ability to check executive power through administrative means.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, congressional_oversight_committees, payer,
    institutional, generational, constrained, national).

% Faces challenges to its role in reviewing administrative action, as the unitary executive theory often implies greater deference to presidential interpretations of law and reduced judicial oversight of executive branch decisions. This shifts the balance of power away from judicial review.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, judiciary, payer,
    institutional, civilizational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate executive action under a single, accountable head, ensuring coherent policy implementation and responsiveness to the electorate through the President.
% TRANSFER_FUNCTION: Transfers authority and discretion from independent agencies and legislative/judicial oversight to the President and the executive branch, consolidating power.
% ABSENT_VOICES: Advocates for administrative expertise, deliberative policymaking, and checks and balances would object, arguing that the unitary executive principle undermines good governance and democratic accountability by concentrating too much power.
% DISAPPEARANCE_RATIONALE: If the unitary executive principle (as read here) vanished, the balance of power would shift significantly. Independent agencies would regain their insulation, Congress would reassert its oversight, and the judiciary would likely expand its review of administrative action, leading to a more fragmented but potentially more deliberative executive branch.
% FOUNDING_PROBLEM: The problem of ensuring a strong, energetic, and accountable executive capable of swift action and unified policy implementation, as envisioned by some framers of the Constitution.
% FOUNDING_PROBLEM_CORROBORATION: Proponents within the executive branch and some legal scholars attest to the ongoing need for a strong, unified executive, citing issues of bureaucratic inertia and fragmented authority. Critics (e.g., some constitutional scholars, former agency heads) argue that the problem is overstated or that the proposed solution creates greater risks than it solves, leading to a contested status.
narrative_ontology:disappearance_verdict(separation_of_powers_text__unitary_executive_reading, world_rearranges).
narrative_ontology:founding_problem_status(separation_of_powers_text__unitary_executive_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(separation_of_powers_text__unitary_executive_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(separation_of_powers_text__unitary_executive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(separation_of_powers_text__unitary_executive_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(separation_of_powers_text__unitary_executive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(separation_of_powers_text__unitary_executive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(separation_of_powers_text__unitary_executive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is high (0.7) because this reading, if fully implemented, would significantly reallocate power and resources towards the presidency at the expense of other actors. Suppression (0.6) is moderate but rising, reflecting the ongoing legal and political battles required to assert and defend this interpretation against established institutional structures. The theater ratio is low (0.2) as the debate is highly substantive, not merely performative. Accessibility collapse (0.4) is moderate, as alternative interpretations (formalist, functionalist) remain viable, though under pressure. Resistance (0.75) is high, reflecting strong opposition from Congress, the judiciary, and advocates for independent administration.
 *
 * PERSPECTIVAL GAP:
 *   The President and executive branch officials would experience this as a 'rope' or even a 'mountain' (a return to constitutional first principles), providing necessary coordination and accountability. Independent agencies, Congress, and the judiciary, however, would experience it as a 'snare' or 'tangled_rope', extracting their established powers and autonomy. The engine's computation of per-seat classifications will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The President and executive branch officials are clear beneficiaries, gaining power and control (low d). Independent agencies are direct targets, losing their insulation (high d). Congressional oversight committees and the judiciary are also targets, as their checks on executive power are diminished (high d).
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling a power grab as pure coordination. While proponents argue for coordination, the significant extraction from other constitutional actors and the active enforcement required to overcome resistance indicate it's not a simple rope. The 'tangled_rope' classification captures both the claimed coordination function and the asymmetric extraction, preventing it from being dismissed as either a pure public good or a simple rent-seeking mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    unitary_executive_historical_basis,
    'Is the unitary executive principle a historically accurate and originalist reading of the Constitution, or a modern construct designed to expand presidential power?',
    'Extensive historical and legal scholarship analyzing founding-era debates, early presidential practice, and constitutional amendments.',
    'If historically robust, it strengthens the claim to being a ''mountain'' or ''rope'' (a return to original intent). If a modern construct, it reinforces the ''snare'' or ''tangled_rope'' classification by revealing a constructed justification for extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(unitary_executive_historical_basis, empirical, 'Historical grounding of the unitary executive principle.').

omega_variable(
    impact_on_administrative_expertise,
    'Does absolute presidential control over independent agencies enhance or degrade the quality and impartiality of administrative policymaking?',
    'Empirical studies comparing policy outcomes and agency performance under varying degrees of presidential control, across different administrations and agencies.',
    'If it degrades quality, it weakens the coordination claim and strengthens the extraction argument. If it enhances quality, it supports the coordination function, potentially shifting the classification towards a ''rope'' for the public.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(impact_on_administrative_expertise, empirical, 'Effect of unitary executive on administrative expertise.').

omega_variable(
    unitary_executive_vs_separation_of_powers_framing,
    'Is the unitary executive principle a legitimate interpretation within the broader framework of separation of powers, or does it fundamentally undermine the system of checks and balances?',
    'Conceptual analysis of constitutional theory, judicial rulings, and legislative intent regarding the balance of power among branches.',
    'If it''s a legitimate interpretation, the ''tangled_rope'' classification holds, reflecting a contested but plausible coordination. If it fundamentally undermines checks and balances, it pushes towards a ''snare'' by revealing the coordination story as a cover for pure extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(unitary_executive_vs_separation_of_powers_framing, conceptual, 'Conceptual compatibility of unitary executive with separation of powers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(separation_of_powers_text__unitary_executive_reading, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(sepa_be_t1980, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 1980, 0.4).
narrative_ontology:measurement(sepa_be_t1990, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 1990, 0.5).
narrative_ontology:measurement(sepa_be_t2000, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 2000, 0.6).
narrative_ontology:measurement(sepa_be_t2010, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 2010, 0.65).
narrative_ontology:measurement(sepa_be_t2024, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 2024, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(sepa_su_t1980, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 1980, 0.3).
narrative_ontology:measurement(sepa_su_t1990, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 1990, 0.4).
narrative_ontology:measurement(sepa_su_t2000, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 2000, 0.5).
narrative_ontology:measurement(sepa_su_t2010, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 2010, 0.55).
narrative_ontology:measurement(sepa_su_t2024, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
