% ============================================================================
% CONSTRAINT STORY: digital_money_origin__first_held_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_money_origin__first_held_reading, []).

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
 *   constraint_id: digital_money_origin__first_held_reading
 *   human_readable: Digital Money Origin (First Held Reading)
 *   domain: monetary_history/technology_studies/institutional_economics
 *
 * SUMMARY:
 *   This constraint story defines the origin of digital money as the point
 *   when individuals began to practically hold and use non-physical monetary
 *   instruments as stores of value. This reading emphasizes user adoption and
 *   functional utility over theoretical conceivability or formal regulatory
 *   recognition. It implies a later origin date than conceptual emergence and
 *   an earlier one than full regulatory integration, focusing on the
 *   practical, lived experience of digital money. The constraint set includes
 *   the technological and network barriers to adoption, as well as the
 *   benefits to early users and providers.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_origin__first_held_reading, 0.3).
domain_priors:suppression_score(digital_money_origin__first_held_reading, 0.4).
domain_priors:theater_ratio(digital_money_origin__first_held_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_origin__first_held_reading, rope).
narrative_ontology:human_readable(digital_money_origin__first_held_reading, "Digital Money Origin (First Held Reading)").
narrative_ontology:topic_domain(digital_money_origin__first_held_reading, "monetary_history/technology_studies/institutional_economics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_origin__first_held_reading, '749ffe98-9d4e-4595-bb2a-674f769055b5').
narrative_ontology:cs_kernel_codification('749ffe98-9d4e-4595-bb2a-674f769055b5', distributed).
narrative_ontology:cs_authority_grounding('749ffe98-9d4e-4595-bb2a-674f769055b5', practice).
narrative_ontology:cs_reading_relation('749ffe98-9d4e-4595-bb2a-674f769055b5', digital_money_origin__became_thinkable_reading, influences).
narrative_ontology:cs_reading_relation('749ffe98-9d4e-4595-bb2a-674f769055b5', digital_money_origin__regulatory_recognition_reading, influences).
narrative_ontology:cs_axiom('749ffe98-9d4e-4595-bb2a-674f769055b5', foundational, value_is_in_use).
narrative_ontology:cs_axiom_status(value_is_in_use, holdable).
narrative_ontology:cs_axiom_grounding('749ffe98-9d4e-4595-bb2a-674f769055b5', value_is_in_use, empirically_contingent).
narrative_ontology:cs_axiom('749ffe98-9d4e-4595-bb2a-674f769055b5', foundational, practical_utility_defines_monetary_instrument).
narrative_ontology:cs_axiom_status(practical_utility_defines_monetary_instrument, holdable).
narrative_ontology:cs_axiom_grounding('749ffe98-9d4e-4595-bb2a-674f769055b5', practical_utility_defines_monetary_instrument, empirically_contingent).
narrative_ontology:cs_reference_frame('749ffe98-9d4e-4595-bb2a-674f769055b5', individual_practical_adoption).
narrative_ontology:cs_drift_state('749ffe98-9d4e-4595-bb2a-674f769055b5', contemporary_global_adoption, gap(stable, minor, true)).
narrative_ontology:cs_created_at('749ffe98-9d4e-4595-bb2a-674f769055b5', '').
narrative_ontology:cs_kernel_id(digital_money_origin__first_held_reading, digital_money_origin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_origin__first_held_reading, early_adopters).
narrative_ontology:constraint_beneficiary(digital_money_origin__first_held_reading, digital_payment_providers).
narrative_ontology:constraint_victim(digital_money_origin__first_held_reading, unbanked_populations).
narrative_ontology:constraint_victim(digital_money_origin__first_held_reading, traditional_financial_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals who first adopted and utilized non-physical monetary instruments, gaining convenience and new transaction capabilities. They benefit from the utility and efficiency of digital money.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, early_adopters, beneficiary,
    moderate, biographical, mobile, global).

% Entities that developed and offered the infrastructure for non-physical monetary instruments, profiting from transaction fees and network effects. They actively shape the evolution and adoption of digital money.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, digital_payment_providers, agenda_setter,
    institutional, generational, arbitrage, global).

% Populations lacking access to the necessary infrastructure (internet, smartphones, bank accounts) to participate in digital money systems. They bear the cost of exclusion from an increasingly digital economy.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, unbanked_populations, payer,
    powerless, biographical, trapped, local).

% Banks and other institutions whose business models are challenged by the rise of non-physical, decentralized monetary instruments. They face pressure to adapt or lose market share.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, traditional_financial_institutions, payer,
    powerful, generational, constrained, national).

% Government bodies responsible for monetary policy and financial stability. They observe the emergence and adoption of digital money, eventually considering its regulatory implications.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, monetary_authorities, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables individuals to coordinate on a shared, non-physical medium of exchange and store of value, facilitating transactions beyond physical cash and traditional banking hours.
% TRANSFER_FUNCTION: Facilitates the transfer of value between individuals and entities using digital representations, moving transaction fees and network benefits to providers and early adopters, and exclusion costs to the unbanked.
% ABSENT_VOICES: Those without access to digital infrastructure, who would advocate for inclusive design and public access to digital financial services, are largely excluded from the design and policy conversations.
% DISAPPEARANCE_RATIONALE: If the concept of digital money as a practical store of value vanished, the global financial system would revert to purely physical or institutionally-mediated electronic forms, disrupting e-commerce, mobile payments, and the entire fintech industry. Value transfer would become slower and less accessible for many.
% FOUNDING_PROBLEM: The need for more convenient, faster, and globally accessible forms of value transfer and storage than physical cash or traditional bank transfers.
% FOUNDING_PROBLEM_CORROBORATION: Economists and technology historians corroborate the ongoing need for efficient value transfer. Digital payment providers and early adopters attest to the problem's live status, citing continuous innovation in digital payment solutions. Traditional financial institutions acknowledge the shift but emphasize the need for stability and regulation.
narrative_ontology:disappearance_verdict(digital_money_origin__first_held_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_origin__first_held_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_origin__first_held_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(digital_money_origin__first_held_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_origin__first_held_reading_tests).
:- end_tests(digital_money_origin__first_held_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.3) is moderate, reflecting the costs of infrastructure development and network effects that initially exclude some populations, but also the benefits of efficiency. Suppression (0.4) is also moderate, as the constraint's persistence relies on the lack of viable alternatives for those without access, rather than active coercion. Theater ratio (0.1) is low, as the functional utility of digital money is clear and not primarily performative. Accessibility collapse (0.6) is moderate, as alternatives (cash, traditional banking) still exist but are less convenient. Resistance (0.2) is low, as the benefits generally outweigh the costs for those who can access it, though exclusion generates some friction.
 *
 * PERSPECTIVAL GAP:
 *   Early adopters and digital payment providers experience this as a beneficial coordination mechanism, offering efficiency and new opportunities. Unbanked populations and traditional financial institutions, however, experience it as an extractive force, creating new barriers or challenging established models. The engine's per-seat classification will reflect these divergent experiences based on their declared power, exit options, and roles.
 *
 * DIRECTIONALITY LOGIC:
 *   Early adopters and digital payment providers are clear beneficiaries, driving their directionality towards 0.0. Unbanked populations and traditional financial institutions are victims, with their directionality tending towards 1.0 due to exclusion or competitive pressure. Monetary authorities are observers, maintaining a neutral analytical stance.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy in this reading, as the problem it solves (efficient digital value transfer) is still live and evolving. The constraint's function has not atrophied; rather, it has expanded and deepened its impact.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    practical_holding_definition,
    'What constitutes ''practically holding'' a non-physical monetary instrument? Does it require active transaction, or merely possession of a digital wallet?',
    'Historical analysis of user behavior patterns and technological capabilities at different points in time, focusing on the minimum threshold for functional utility.',
    'A stricter definition of ''practical holding'' would push the origin date later, emphasizing widespread utility. A looser definition would bring it earlier, focusing on initial technical availability. This would shift the temporal measurements and potentially the perceived extractiveness of early adoption.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(practical_holding_definition, conceptual, 'Ambiguity in the precise definition of ''practical holding'' for non-physical monetary instruments.').

omega_variable(
    exclusion_mechanism_ambiguity,
    'Is the exclusion of unbanked populations from digital money systems primarily due to structural barriers (lack of infrastructure) or internalized factors (lack of trust, digital literacy)?',
    'Comparative studies of digital money adoption in regions with varying levels of infrastructure and digital literacy, alongside qualitative research on user perceptions and barriers.',
    'If primarily structural, the suppression metric accurately reflects external barriers. If significantly internalized, the effective suppression for unbanked populations is higher than the structural measure suggests, as they carry internal barriers even if infrastructure improves.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exclusion_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for unbanked populations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_origin__first_held_reading, 1990, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t1990, digital_money_origin__first_held_reading, theater_ratio, 1990, 0.05).
narrative_ontology:measurement(digi_tr_t1995, digital_money_origin__first_held_reading, theater_ratio, 1995, 0.07).
narrative_ontology:measurement(digi_tr_t2000, digital_money_origin__first_held_reading, theater_ratio, 2000, 0.08).
narrative_ontology:measurement(digi_tr_t2005, digital_money_origin__first_held_reading, theater_ratio, 2005, 0.09).
narrative_ontology:measurement(digi_tr_t2010, digital_money_origin__first_held_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(digi_tr_t2015, digital_money_origin__first_held_reading, theater_ratio, 2015, 0.1).
narrative_ontology:measurement(digi_tr_t2020, digital_money_origin__first_held_reading, theater_ratio, 2020, 0.1).

% Extraction over time
narrative_ontology:measurement(digi_be_t1990, digital_money_origin__first_held_reading, base_extractiveness, 1990, 0.15).
narrative_ontology:measurement(digi_be_t1995, digital_money_origin__first_held_reading, base_extractiveness, 1995, 0.18).
narrative_ontology:measurement(digi_be_t2000, digital_money_origin__first_held_reading, base_extractiveness, 2000, 0.22).
narrative_ontology:measurement(digi_be_t2005, digital_money_origin__first_held_reading, base_extractiveness, 2005, 0.25).
narrative_ontology:measurement(digi_be_t2010, digital_money_origin__first_held_reading, base_extractiveness, 2010, 0.28).
narrative_ontology:measurement(digi_be_t2015, digital_money_origin__first_held_reading, base_extractiveness, 2015, 0.29).
narrative_ontology:measurement(digi_be_t2020, digital_money_origin__first_held_reading, base_extractiveness, 2020, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t1990, digital_money_origin__first_held_reading, suppression_requirement, 1990, 0.1).
narrative_ontology:measurement(digi_su_t1995, digital_money_origin__first_held_reading, suppression_requirement, 1995, 0.15).
narrative_ontology:measurement(digi_su_t2000, digital_money_origin__first_held_reading, suppression_requirement, 2000, 0.25).
narrative_ontology:measurement(digi_su_t2005, digital_money_origin__first_held_reading, suppression_requirement, 2005, 0.3).
narrative_ontology:measurement(digi_su_t2010, digital_money_origin__first_held_reading, suppression_requirement, 2010, 0.35).
narrative_ontology:measurement(digi_su_t2015, digital_money_origin__first_held_reading, suppression_requirement, 2015, 0.38).
narrative_ontology:measurement(digi_su_t2020, digital_money_origin__first_held_reading, suppression_requirement, 2020, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_origin__first_held_reading, resource_allocation).
narrative_ontology:affects_constraint(digital_money_origin__first_held_reading, digital_money_origin__became_thinkable_reading).
narrative_ontology:affects_constraint(digital_money_origin__first_held_reading, digital_money_origin__regulatory_recognition_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'digital_money_origin' kernel. This 'first_held_reading' emphasizes practical adoption, influencing and being influenced by the 'became_thinkable_reading' (conceptual emergence) and 'regulatory_recognition_reading' (formal integration).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
