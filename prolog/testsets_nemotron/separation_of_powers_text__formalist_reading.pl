% ============================================================================
% CONSTRAINT STORY: separation_of_powers_text__formalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_separation_of_powers_text__formalist_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: separation_of_powers_text__formalist_reading
 *   human_readable: Non-delegation Doctrine (Formalist Reading)
 *   domain: constitutional/administrative
 *
 * SUMMARY:
 *   This constraint story instantiates the formalist reading of the
 *   separation-of-powers kernel. The formalist reading holds that the Vesting
 *   Clauses create strict, impermeable boundaries: Article I's 'All
 *   legislative Powers herein granted shall be vested in a Congress' means
 *   Congress cannot delegate lawmaking authority to executive agencies. The
 *   constraint operates through judicial enforcement — courts strike down
 *   delegations lacking an 'intelligible principle' (in practice,
 *   increasingly, any delegation). The claimed type is snare: the
 *   coordination story (preventing tyranny) is cover; the operational reality
 *   is suppressing the administrative state's regulatory capacity, with
 *   identifiable victims (agencies, regulated publics, Congress's own
 *   practical governance ability). The extraction is high and rising;
 *   suppression is very high and rising; theater is low but growing as the
 *   constraint's performative 'constitutional fidelity' increasingly masks
 *   its extractive function.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(separation_of_powers_text__formalist_reading, 0.68).
domain_priors:suppression_score(separation_of_powers_text__formalist_reading, 0.85).
domain_priors:theater_ratio(separation_of_powers_text__formalist_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(separation_of_powers_text__formalist_reading, snare).
narrative_ontology:human_readable(separation_of_powers_text__formalist_reading, "Non-delegation Doctrine (Formalist Reading)").
narrative_ontology:topic_domain(separation_of_powers_text__formalist_reading, "constitutional/administrative").

domain_priors:requires_active_enforcement(separation_of_powers_text__formalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(separation_of_powers_text__formalist_reading, '9bab4d6f-4df3-4535-9d4c-f9ad2ea740ca').
narrative_ontology:cs_kernel_codification('9bab4d6f-4df3-4535-9d4c-f9ad2ea740ca', fixed_text).
narrative_ontology:cs_authority_grounding('9bab4d6f-4df3-4535-9d4c-f9ad2ea740ca', lineage).
narrative_ontology:cs_interpretation_layer_present('9bab4d6f-4df3-4535-9d4c-f9ad2ea740ca').
narrative_ontology:cs_reading_relation('9bab4d6f-4df3-4535-9d4c-f9ad2ea740ca', separation_of_powers_text__functionalist_reading, forecloses).
narrative_ontology:cs_reading_relation('9bab4d6f-4df3-4535-9d4c-f9ad2ea740ca', separation_of_powers_text__unitary_executive_reading, influences).
narrative_ontology:cs_axiom('9bab4d6f-4df3-4535-9d4c-f9ad2ea740ca', foundational, legislative_power_non_delegable).
narrative_ontology:cs_axiom_status(legislative_power_non_delegable, holdable).
narrative_ontology:cs_axiom_grounding('9bab4d6f-4df3-4535-9d4c-f9ad2ea740ca', legislative_power_non_delegable, deontological).
narrative_ontology:cs_axiom('9bab4d6f-4df3-4535-9d4c-f9ad2ea740ca', foundational, vesting_clauses_create_impermeable_boundaries).
narrative_ontology:cs_axiom_status(vesting_clauses_create_impermeable_boundaries, holdable).
narrative_ontology:cs_axiom_grounding('9bab4d6f-4df3-4535-9d4c-f9ad2ea740ca', vesting_clauses_create_impermeable_boundaries, conventional).
narrative_ontology:cs_reference_frame('9bab4d6f-4df3-4535-9d4c-f9ad2ea740ca', founding_era_separation).
narrative_ontology:cs_drift_state('9bab4d6f-4df3-4535-9d4c-f9ad2ea740ca', contemporary_administrative_state, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('9bab4d6f-4df3-4535-9d4c-f9ad2ea740ca', '').
narrative_ontology:cs_kernel_id(separation_of_powers_text__formalist_reading, separation_of_powers_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(separation_of_powers_text__formalist_reading, originalist_judiciary).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__formalist_reading, legislative_purists).
narrative_ontology:constraint_victim(separation_of_powers_text__formalist_reading, administrative_agencies).
narrative_ontology:constraint_victim(separation_of_powers_text__formalist_reading, regulatory_beneficiaries).
narrative_ontology:constraint_victim(separation_of_powers_text__formalist_reading, congress_delegation_capacity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues and enforces non-delegation rulings that invalidate or constrain agency rulemaking. Justifies the constraint as restoring constitutional design. Controls the judicial machinery that suppresses delegation alternatives.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, originalist_judiciary, agenda_setter,
    institutional, generational, arbitrage, national).

% Members of Congress and allied scholars who argue Congress must make all policy choices itself. They gain institutional legitimacy and rhetorical authority from the constraint, though they bear the practical burden of legislating in detail.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, legislative_purists, beneficiary,
    organized, biographical, mobile, national).

% Lose rulemaking authority when delegations are struck down. Must either cease regulating or seek impossibly detailed statutory authorizations. Their expertise and operational capacity are rendered legally unusable by the constraint.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, administrative_agencies, payer,
    institutional, biographical, trapped, national).

% Public health, environmental, financial, and workplace protections that depend on agency expertise and speed. When delegations fall, these protections vanish or degrade because Congress cannot legislate at the required granularity or responsiveness.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, regulatory_beneficiaries, payer,
    powerless, biographical, trapped, national).

% Congress as an institution loses its practical ability to delegate complex policy implementation. The constraint forces a choice: legislate everything in micromanaged detail (impossible) or leave regulatory gaps unfilled. Its own institutional capacity is the extraction target.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, congress_delegation_capacity, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(separation_of_powers_text__formalist_reading, congress_delegation_capacity, payer).

% Argue that separation of powers permits delegation under an 'intelligible principle' standard. Their framework would allow the modern administrative state; they are excluded from the formalist reading's constitutional order by definitional fiat.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, functionalist_scholars, excluded,
    organized, biographical, mobile, national).

% Argue all executive power must be directly controlled by the President. They would eliminate independent agencies entirely — a different victim set than the formalist reading produces. Excluded because their remedy contradicts the formalist premise.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, unitary_executive_advocates, excluded,
    organized, biographical, mobile, national).

% Compares the three readings of the separation-of-powers kernel. Sees the formalist reading as a high-extraction constraint that suppresses the administrative state's coordination function while claiming natural-law status for its boundary-drawing.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, constitutional_analyst, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, judicially enforceable boundary between legislative and executive power to prevent tyranny from their fusion. Solves the coordination problem of who may make binding rules with the force of law.
% TRANSFER_FUNCTION: Moves regulatory authority and practical governance capacity from administrative agencies (and the publics they protect) to a judicial-legislative axis that insists on congressional micromanagement. The transfer is authority, expertise, and speed — from agencies to a Congress that cannot exercise them.
% ABSENT_VOICES: The regulated public who depend on agency protections (clean air, safe drugs, fair labor standards) are not in the courtroom when non-delegation challenges are litigated. Future generations who inherit a degraded regulatory capacity are structurally absent. Functionalist and unitary-executive readings are excluded as competing constitutional frameworks.
% DISAPPEARANCE_RATIONALE: If the formalist non-delegation constraint vanished overnight, agencies would resume rulemaking under existing delegations, Congress would return to broad delegations with intelligible principles, and the modern administrative state's regulatory output would be restored. The world rearranges because the constraint actively suppresses a functioning coordination system.
% FOUNDING_PROBLEM: The founding problem was preventing legislative abdication to an unaccountable executive — the fear that Congress would hand lawmaking power to royal-like administrators, recreating the tyranny the Revolution rejected.
% FOUNDING_PROBLEM_CORROBORATION: Originalist scholars (Benard, Lawson) attest the founding problem is live and the administrative state is the abdication. Functionalist scholars (Sunstein, Vermeule, Prakash) and historical institutionalists attest the founding problem was solved by the intelligible principle standard and the constraint now serves a different function — disabling governance the founding generation could not have envisioned. The corroboration split is the contest.
narrative_ontology:disappearance_verdict(separation_of_powers_text__formalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(separation_of_powers_text__formalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(separation_of_powers_text__formalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(separation_of_powers_text__formalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(separation_of_powers_text__formalist_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(separation_of_powers_text__formalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(separation_of_powers_text__formalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(separation_of_powers_text__formalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness 0.68: the constraint extracts the entire operational capacity of the modern administrative state — rulemaking, adjudication, enforcement — and transfers it to a Congress that cannot exercise it. The victims (agencies, regulatory beneficiaries) lose functional governance; the beneficiaries (originalist judiciary, legislative purists) gain constitutional authority but not governance capacity. Suppression 0.85: alternatives (intelligible principle delegation, functional overlap) are judicially foreclosed. Theater 0.25: the constraint performs constitutional fidelity while operating as a dismantling mechanism. Accessibility collapse 0.72: once the formalist premise is accepted, the administrative state's legitimacy collapses almost entirely. Resistance 0.45: moderate — agencies and defenders litigate and adapt, but the constraint's judicial enforcement machinery is powerful and hardening.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (originalist judiciary) experiences this as rope — a genuine coordination constraint they maintain. The payer seats (agencies, regulatory beneficiaries, Congress's capacity) experience it as snare — enforced extraction with no exit. The observer seat sees the divergence: a constraint that claims to be mountain (constitutional law) operates as snare (dismantling mechanism). The engine computes this from the declared structural data; the formalist reading's own claim ('this is what the Constitution requires') is the cover story.
 *
 * DIRECTIONALITY LOGIC:
 *   Originalist judiciary: d ≈ 0.15 (full beneficiary — controls the constraint, collects institutional authority). Legislative purists: d ≈ 0.3 (beneficiary — gains rhetorical/institutional capital but bears legislative burden). Administrative agencies: d ≈ 0.95 (full target — lose core function, trapped). Regulatory beneficiaries: d ≈ 0.9 (target — lose protections, powerless). Congress delegation capacity: d ≈ 0.85 (target — loses practical governance tool, constrained exit). Excluded voices (functionalist, unitary executive): structurally excluded from the framework; their exit is mobile (they can advocate) but their framework is foreclosed within this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing legislative abdication to unaccountable executive) is contested as live vs. solved. The formalist reading claims it is live and the administrative state IS the abdication. Functionalist corroboration says the intelligible principle standard solved it and the current constraint serves a new function: disabling governance capacity the Founders could not anticipate. The mandatrophy is unresolved — the constraint persists because its beneficiaries (originalist judiciary) have the power to enforce it, not because its founding problem remains live. This is a classic mandatrophy pattern: a constraint whose mandate has atrophied but whose enforcement machinery has strengthened.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    formalist_vs_functionalist_boundary,
    'Is the formalist reading''s boundary (no delegation whatsoever) structurally distinct from the functionalist reading''s boundary (delegation with intelligible principle), or is the difference one of degree that collapses under scrutiny?',
    'Comparative analysis of Supreme Court opinions: if the Court''s ''intelligible principle'' applications become vacuous (any statute passes), the functionalist boundary collapses into the formalist one. If some delegations are genuinely struck down under intelligible principle while others stand, the readings occupy distinct structural positions.',
    'If the boundaries collapse, the formalist reading is the only coherent anti-delegation position — the functionalist reading is a transitional illusion. If they remain distinct, the kernel genuinely supports multiple structural equilibria.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(formalist_vs_functionalist_boundary, conceptual, 'Whether the formalist/functionalist distinction is structural or performative').

omega_variable(
    regulatory_capacity_replacement,
    'Can Congress practically replace agency rulemaking with detailed legislation, or does the formalist constraint create an irreparable governance gap?',
    'Empirical study of legislative output in jurisdictions with strict non-delegation rules: measure statutory detail, regulatory coverage, and enforcement outcomes.',
    'If Congress cannot replace agency capacity, the constraint''s extraction is irreparable governance loss (snare confirmed). If Congress can legislate at granularity, the constraint''s coordination function is genuine and extraction is the cost of constitutional fidelity (tangled rope possible).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_capacity_replacement, empirical, 'Whether the constraint''s extraction is irreparable or the price of genuine coordination').

omega_variable(
    kernel_framing_underdetermination,
    'Does the separation-of-powers kernel admit only these three readings, or are there additional structurally distinct framings (e.g., a ''democratic accountability'' reading that permits delegation with political control mechanisms)?',
    'Survey constitutional theory literature for readings not captured by the formalist/functionalist/unitary-executive trichotomy. Test each candidate for structural distinctness (different victim sets, different ε, different coordination function).',
    'Additional readings would expand the constraint family and change the network topology. The current three-reading decomposition might be an artifact of current doctrinal discourse, not the kernel''s structural possibilities.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the declared kernel readings exhaust the kernel''s structural possibilities').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(separation_of_powers_text__formalist_reading, 1935, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sepa_tr_t1935, separation_of_powers_text__formalist_reading, theater_ratio, 1935, 0.1).
narrative_ontology:measurement(sepa_tr_t1950, separation_of_powers_text__formalist_reading, theater_ratio, 1950, 0.12).
narrative_ontology:measurement(sepa_tr_t1970, separation_of_powers_text__formalist_reading, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(sepa_tr_t1980, separation_of_powers_text__formalist_reading, theater_ratio, 1980, 0.18).
narrative_ontology:measurement(sepa_tr_t2000, separation_of_powers_text__formalist_reading, theater_ratio, 2000, 0.2).
narrative_ontology:measurement(sepa_tr_t2015, separation_of_powers_text__formalist_reading, theater_ratio, 2015, 0.22).
narrative_ontology:measurement(sepa_tr_t2022, separation_of_powers_text__formalist_reading, theater_ratio, 2022, 0.24).
narrative_ontology:measurement(sepa_tr_t2025, separation_of_powers_text__formalist_reading, theater_ratio, 2025, 0.25).

% Extraction over time
narrative_ontology:measurement(sepa_be_t1935, separation_of_powers_text__formalist_reading, base_extractiveness, 1935, 0.15).
narrative_ontology:measurement(sepa_be_t1950, separation_of_powers_text__formalist_reading, base_extractiveness, 1950, 0.2).
narrative_ontology:measurement(sepa_be_t1970, separation_of_powers_text__formalist_reading, base_extractiveness, 1970, 0.25).
narrative_ontology:measurement(sepa_be_t1980, separation_of_powers_text__formalist_reading, base_extractiveness, 1980, 0.3).
narrative_ontology:measurement(sepa_be_t2000, separation_of_powers_text__formalist_reading, base_extractiveness, 2000, 0.45).
narrative_ontology:measurement(sepa_be_t2015, separation_of_powers_text__formalist_reading, base_extractiveness, 2015, 0.58).
narrative_ontology:measurement(sepa_be_t2022, separation_of_powers_text__formalist_reading, base_extractiveness, 2022, 0.65).
narrative_ontology:measurement(sepa_be_t2025, separation_of_powers_text__formalist_reading, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(sepa_su_t1935, separation_of_powers_text__formalist_reading, suppression_requirement, 1935, 0.4).
narrative_ontology:measurement(sepa_su_t1950, separation_of_powers_text__formalist_reading, suppression_requirement, 1950, 0.45).
narrative_ontology:measurement(sepa_su_t1970, separation_of_powers_text__formalist_reading, suppression_requirement, 1970, 0.55).
narrative_ontology:measurement(sepa_su_t1980, separation_of_powers_text__formalist_reading, suppression_requirement, 1980, 0.65).
narrative_ontology:measurement(sepa_su_t2000, separation_of_powers_text__formalist_reading, suppression_requirement, 2000, 0.72).
narrative_ontology:measurement(sepa_su_t2015, separation_of_powers_text__formalist_reading, suppression_requirement, 2015, 0.78).
narrative_ontology:measurement(sepa_su_t2022, separation_of_powers_text__formalist_reading, suppression_requirement, 2022, 0.82).
narrative_ontology:measurement(sepa_su_t2025, separation_of_powers_text__formalist_reading, suppression_requirement, 2025, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(separation_of_powers_text__formalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(separation_of_powers_text__formalist_reading, 0.1).
narrative_ontology:affects_constraint(separation_of_powers_text__formalist_reading, separation_of_powers_text__functionalist_reading).
narrative_ontology:affects_constraint(separation_of_powers_text__formalist_reading, separation_of_powers_text__unitary_executive_reading).
narrative_ontology:affects_constraint(separation_of_powers_text__formalist_reading, administrative_state_legitimacy).
narrative_ontology:affects_constraint(separation_of_powers_text__formalist_reading, major_questions_doctrine).

% DUAL FORMULATION NOTE:
% Part of the separation-of-powers kernel family. This formalist reading forecloses the functionalist reading (mutually exclusive constitutional frameworks) and influences the unitary executive reading (dual pressure on agency legitimacy from legislative and executive separation premises). The ε values differ sharply: formalist ε≈0.68 (snare), functionalist ε≈0.25 (tangled rope — coordination with extraction), unitary executive ε≈0.55 (snare with different victim set).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(separation_of_powers_text__formalist_reading, institutional, 0.15).
constraint_indexing:directionality_override(separation_of_powers_text__formalist_reading, organized, 0.3).
constraint_indexing:directionality_override(separation_of_powers_text__formalist_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
