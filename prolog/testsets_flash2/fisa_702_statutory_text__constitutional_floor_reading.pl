% ============================================================================
% CONSTRAINT STORY: fisa_702_statutory_text__constitutional_floor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fisa_702_statutory_text__constitutional_floor_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: fisa_702_statutory_text__constitutional_floor_reading
 *   human_readable: FISA Section 702: Constitutional Floor Reading (Warrant Requirement)
 *   domain: constitutional_law/national_security/surveillance_policy
 *
 * SUMMARY:
 *   This constraint represents a 'constitutional floor' reading of FISA
 *   Section 702, asserting that the Fourth Amendment mandates a probable
 *   cause warrant for any government search of U.S. person communications
 *   content, regardless of statutory language or foreign intelligence
 *   context. This reading reclassifies 702 database queries as searches
 *   triggering the warrant requirement, shifting the framework from a foreign
 *   intelligence statute to a criminal procedure question. The claimed type
 *   is 'mountain' because it posits an irreducible constitutional limit, but
 *   it declares beneficiaries (U.S. persons) and victims (executive agencies)
 *   to trigger False Summit Mountain (FSM) evaluation, acknowledging the
 *   contestation over its 'naturalness' as a legal interpretation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fisa_702_statutory_text__constitutional_floor_reading, 0.25).
domain_priors:suppression_score(fisa_702_statutory_text__constitutional_floor_reading, 0.15).
domain_priors:theater_ratio(fisa_702_statutory_text__constitutional_floor_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fisa_702_statutory_text__constitutional_floor_reading, mountain).
narrative_ontology:human_readable(fisa_702_statutory_text__constitutional_floor_reading, "FISA Section 702: Constitutional Floor Reading (Warrant Requirement)").
narrative_ontology:topic_domain(fisa_702_statutory_text__constitutional_floor_reading, "constitutional_law/national_security/surveillance_policy").

domain_priors:requires_active_enforcement(fisa_702_statutory_text__constitutional_floor_reading).
domain_priors:emerges_naturally(fisa_702_statutory_text__constitutional_floor_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fisa_702_statutory_text__constitutional_floor_reading, 'e1bd5925-ed24-46ac-9adc-7155022a7c76').
narrative_ontology:cs_kernel_codification('e1bd5925-ed24-46ac-9adc-7155022a7c76', fixed_text).
narrative_ontology:cs_authority_grounding('e1bd5925-ed24-46ac-9adc-7155022a7c76', lineage).
narrative_ontology:cs_interpretation_layer_present('e1bd5925-ed24-46ac-9adc-7155022a7c76').
narrative_ontology:cs_reading_relation('e1bd5925-ed24-46ac-9adc-7155022a7c76', fisa_702_statutory_text__incidental_collection_reading, forecloses).
narrative_ontology:cs_reading_relation('e1bd5925-ed24-46ac-9adc-7155022a7c76', fisa_702_statutory_text__foreign_target_strict_reading, influences).
narrative_ontology:cs_axiom('e1bd5925-ed24-46ac-9adc-7155022a7c76', foundational, fourth_amendment_search_requires_warrant).
narrative_ontology:cs_axiom_status(fourth_amendment_search_requires_warrant, holdable).
narrative_ontology:cs_axiom_grounding('e1bd5925-ed24-46ac-9adc-7155022a7c76', fourth_amendment_search_requires_warrant, deontological).
narrative_ontology:cs_axiom('e1bd5925-ed24-46ac-9adc-7155022a7c76', foundational, us_person_communications_are_private).
narrative_ontology:cs_axiom_status(us_person_communications_are_private, holdable).
narrative_ontology:cs_axiom_grounding('e1bd5925-ed24-46ac-9adc-7155022a7c76', us_person_communications_are_private, deontological).
narrative_ontology:cs_reference_frame('e1bd5925-ed24-46ac-9adc-7155022a7c76', founding_era_fourth_amendment_principles).
narrative_ontology:cs_drift_state('e1bd5925-ed24-46ac-9adc-7155022a7c76', contemporary_digital_surveillance_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e1bd5925-ed24-46ac-9adc-7155022a7c76', '').
narrative_ontology:cs_kernel_id(fisa_702_statutory_text__constitutional_floor_reading, fisa_702_statutory_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__constitutional_floor_reading, us_persons).
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__constitutional_floor_reading, fourth_amendment_doctrine).
narrative_ontology:constraint_victim(fisa_702_statutory_text__constitutional_floor_reading, executive_branch_agencies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from robust Fourth Amendment protections, ensuring their communications content is not searched by the government without a probable cause warrant. Their identity as U.S. persons is the basis for this protection, making exit from this status impossible.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, us_persons, beneficiary,
    organized, generational, identity_locked, national).

% Bear the cost of increased procedural hurdles and reduced operational flexibility due to the warrant requirement for querying U.S. person communications. They argue this impedes foreign intelligence collection and counterterrorism efforts. Their exit options are constrained by legal obligations and national security mandates.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, executive_branch_agencies, payer,
    institutional, immediate, constrained, global).

% Would be responsible for reviewing probable cause warrant applications for U.S. person queries, ensuring constitutional compliance. Their role shifts from overseeing programmatic collection to individualized judicial review. Their options are constrained by their statutory mandate and constitutional role.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, fisa_court, agenda_setter,
    institutional, biographical, constrained, national).

% Oversees FISA Section 702 and debates its reauthorization, considering the constitutional implications of warrantless searches. They would need to legislate to align statutory text with this constitutional interpretation.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, congress, observer,
    institutional, generational, analytical, national).

% The constitutional principle itself is vindicated by this reading, reinforcing the requirement for probable cause and warrants for government searches of private communications. It benefits from a consistent and robust application of its core tenets.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, fourth_amendment_doctrine, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(fisa_702_statutory_text__constitutional_floor_reading, fourth_amendment_doctrine).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures that government surveillance activities targeting U.S. person communications content are coordinated with constitutional protections, specifically the Fourth Amendment's warrant requirement, thereby maintaining the balance between national security and individual liberties.
% TRANSFER_FUNCTION: Transfers the burden of justifying a search from post-hoc minimization or foreign intelligence purpose to a pre-query probable cause warrant, from the executive branch to the FISA Court, and ultimately to the protection of U.S. persons' privacy.
% ABSENT_VOICES: The executive branch's national security apparatus, particularly intelligence agencies, would argue against this reading, emphasizing the operational costs and potential intelligence gaps created by a warrant requirement. Their perspective is often framed as a necessary trade-off for national security, but this reading prioritizes constitutional limits.
% DISAPPEARANCE_RATIONALE: If this constitutional floor reading disappeared, the legal landscape for surveillance would fundamentally shift. Executive agencies would likely revert to broader interpretations of their authority, leading to increased warrantless access to U.S. person communications, and a significant erosion of Fourth Amendment protections. The balance of power between branches and the rights of citizens would be profoundly altered.
% FOUNDING_PROBLEM: The Fourth Amendment was established to prevent arbitrary government searches and seizures, a problem rooted in colonial-era abuses of general warrants and writs of assistance. It sought to ensure that government intrusion into private affairs is justified by probable cause and overseen by an independent judiciary.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, civil liberties advocates, and a segment of the judiciary consistently attest that the founding problem of arbitrary government intrusion remains live, particularly in the context of modern surveillance technologies. Historical abuses and ongoing debates over executive power corroborate this persistent concern, independent of the executive branch's claims of operational necessity.
narrative_ontology:disappearance_verdict(fisa_702_statutory_text__constitutional_floor_reading, world_rearranges).
narrative_ontology:founding_problem_status(fisa_702_statutory_text__constitutional_floor_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fisa_702_statutory_text__constitutional_floor_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(fisa_702_statutory_text__constitutional_floor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fisa_702_statutory_text__constitutional_floor_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fisa_702_statutory_text__constitutional_floor_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, ExtMetricName, E),
    domain_priors:suppression_score(fisa_702_statutory_text__constitutional_floor_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(fisa_702_statutory_text__constitutional_floor_reading),
    narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(fisa_702_statutory_text__constitutional_floor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.25) reflects the 'cost' to executive agencies in terms of operational speed and secrecy, which they perceive as a burden. Suppression (0.15) is low because the constraint is a constitutional principle, not actively enforced coercion against individuals, but rather a limit on government power. Theater ratio (0.05) is minimal, as the principle is about genuine legal compliance. Accessibility collapse (0.85) is high because, if accepted, this reading would largely eliminate warrantless access to U.S. person communications content. Resistance (0.05) is low from the perspective of the constitutional principle itself, as it is a foundational legal tenet, though executive agencies actively resist its application.
 *
 * PERSPECTIVAL GAP:
 *   Executive agencies view this as an onerous and unnecessary restriction on vital intelligence collection, while civil liberties advocates and some legal scholars see it as a necessary safeguard against government overreach. The 'mountain' claim reflects the view that the Fourth Amendment is a fixed, natural law of the legal system, but the declared beneficiaries and victims highlight the contestation over its application and the 'cost' it imposes on one party.
 *
 * DIRECTIONALITY LOGIC:
 *   U.S. persons are the primary beneficiaries, as their Fourth Amendment rights are robustly protected (d near 0.0). Executive branch agencies are the victims, as their operational flexibility is curtailed by the warrant requirement (d near 1.0). The FISA Court, if it were to implement this reading, would act as an agenda-setter, enforcing the constitutional floor. The Fourth Amendment doctrine itself is a non-agent beneficiary, vindicated by this interpretation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutional_vs_statutory_authority,
    'Is the Fourth Amendment''s warrant requirement an absolute constitutional floor for U.S. person communications content, or can statutory frameworks like FISA Section 702 create exceptions based on foreign intelligence purpose?',
    'Supreme Court ruling directly addressing the constitutionality of warrantless U.S. person queries under Section 702, or a constitutional amendment clarifying surveillance powers.',
    'If the Supreme Court affirms this reading, it would solidify the warrant requirement as a ''mountain'' for U.S. person data, fundamentally altering surveillance practices. If it rejects it, the ''incidental collection'' reading would gain stronger legal footing, increasing executive power.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constitutional_vs_statutory_authority, conceptual, 'Ambiguity regarding the supremacy of constitutional warrant requirements over statutory foreign intelligence authorizations.').

omega_variable(
    operational_cost_vs_constitutional_compliance,
    'What is the true operational cost (in terms of intelligence gaps and missed threats) of implementing a probable cause warrant requirement for U.S. person queries under Section 702, versus the cost of constitutional non-compliance?',
    'Independent, declassified analysis by a non-governmental body, comparing intelligence outcomes in jurisdictions with stricter warrant requirements, or a pilot program implementing warrants for a subset of queries.',
    'If operational costs are demonstrably low, it weakens the executive''s argument against warrants. If constitutional non-compliance is shown to have severe long-term impacts on trust and legitimacy, it strengthens the case for warrants. This would shift the ''extractiveness'' metric''s justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operational_cost_vs_constitutional_compliance, empirical, 'Quantifying the trade-off between national security operational efficiency and constitutional compliance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fisa_702_statutory_text__constitutional_floor_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fisa_tr_t0, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(fisa_tr_t10, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 10, 0.05).

% Extraction over time
narrative_ontology:measurement(fisa_be_t0, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(fisa_be_t10, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 10, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(fisa_su_t0, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(fisa_su_t10, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 10, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
