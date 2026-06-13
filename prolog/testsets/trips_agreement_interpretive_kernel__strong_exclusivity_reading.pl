% ============================================================================
% CONSTRAINT STORY: trips_agreement_interpretive_kernel__strong_exclusivity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_trips_agreement_interpretive_kernel__strong_exclusivity_reading, []).

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
 *   constraint_id: trips_agreement_interpretive_kernel__strong_exclusivity_reading
 *   human_readable: TRIPS Strong Patent Exclusivity Interpretive Reading
 *   domain: international_trade/intellectual_property/public_health
 *
 * SUMMARY:
 *   TRIPS Article 31 nominally permits compulsory licensing for public health
 *   emergencies, parallel imports, and other flexibilities. The strong
 *   exclusivity reading interprets these as narrow exceptions requiring prior
 *   negotiation attempts, full royalty payment, domestic-market limitation,
 *   and non-revocability protection. This reading instantiates one pole of a
 *   contested kernel: whether TRIPS text mandates high uniform patent
 *   protection with narrow flexibilities (strong exclusivity reading) or
 *   embeds broad compulsory licensing as a routine public health tool
 *   (public_health_flexibility_reading). This constraint models the strong
 *   reading's structural operation: it benefits pharmaceutical patent holders
 *   and high-income country governments; it extracts from low-income patient
 *   populations and generic manufacturers. The sibling
 *   public_health_flexibility_reading would flip beneficiary/victim roles and
 *   lower extractiveness by treating compulsory licensing as routine.
 *
 * KEY AGENTS:
 *   - pharmaceutical_patent_holders: Primary beneficiaries; extract monopoly pricing power across global markets
 *   - high_income_country_governments: Agenda-setters; enforce the strong reading through WTO dispute panels and trade retaliation
 *   - low_income_patient_populations: Trapped victims; face high drug prices and cannot access generics
 *   - generic_drug_manufacturers: Structurally excluded from markets during patent term
 *   - low_income_state_governments: Constrained payers; bear health-system costs and face retaliation if they invoke compulsory licensing
 *   - WTO dispute panels: Operational authority; instantiate the reading through binding rulings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0.78).
domain_priors:suppression_score(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0.81).
domain_priors:theater_ratio(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trips_agreement_interpretive_kernel__strong_exclusivity_reading, tangled_rope).
narrative_ontology:human_readable(trips_agreement_interpretive_kernel__strong_exclusivity_reading, "TRIPS Strong Patent Exclusivity Interpretive Reading").
narrative_ontology:topic_domain(trips_agreement_interpretive_kernel__strong_exclusivity_reading, "international_trade/intellectual_property/public_health").

domain_priors:requires_active_enforcement(trips_agreement_interpretive_kernel__strong_exclusivity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 'e68bfb78-505b-48b0-85d0-d7ff60d1cd48').
narrative_ontology:cs_kernel_codification('e68bfb78-505b-48b0-85d0-d7ff60d1cd48', fixed_text).
narrative_ontology:cs_authority_grounding('e68bfb78-505b-48b0-85d0-d7ff60d1cd48', extraction).
narrative_ontology:cs_interpretation_layer_present('e68bfb78-505b-48b0-85d0-d7ff60d1cd48').
narrative_ontology:cs_reading_relation('e68bfb78-505b-48b0-85d0-d7ff60d1cd48', trips_agreement_interpretive_kernel__public_health_flexibility_reading, coexists_with).
narrative_ontology:cs_reading_relation('e68bfb78-505b-48b0-85d0-d7ff60d1cd48', trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, influences).
narrative_ontology:cs_axiom('e68bfb78-505b-48b0-85d0-d7ff60d1cd48', foundational, patent_exclusivity_primary_incentive).
narrative_ontology:cs_axiom_status(patent_exclusivity_primary_incentive, holdable).
narrative_ontology:cs_axiom_grounding('e68bfb78-505b-48b0-85d0-d7ff60d1cd48', patent_exclusivity_primary_incentive, empirically_contingent).
narrative_ontology:cs_axiom('e68bfb78-505b-48b0-85d0-d7ff60d1cd48', foundational, compulsory_licensing_exceptional_not_routine).
narrative_ontology:cs_axiom_status(compulsory_licensing_exceptional_not_routine, holdable).
narrative_ontology:cs_axiom_grounding('e68bfb78-505b-48b0-85d0-d7ff60d1cd48', compulsory_licensing_exceptional_not_routine, instrumental).
narrative_ontology:cs_reference_frame('e68bfb78-505b-48b0-85d0-d7ff60d1cd48', uniform_high_patent_protection_mandate).
narrative_ontology:cs_drift_state('e68bfb78-505b-48b0-85d0-d7ff60d1cd48', contemporary_public_health_crisis_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e68bfb78-505b-48b0-85d0-d7ff60d1cd48', '').
narrative_ontology:cs_kernel_id(trips_agreement_interpretive_kernel__strong_exclusivity_reading, trips_agreement_interpretive_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__strong_exclusivity_reading, pharmaceutical_patent_holders).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__strong_exclusivity_reading, high_income_country_governments).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__strong_exclusivity_reading, low_income_patient_populations).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__strong_exclusivity_reading, generic_drug_manufacturers).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__strong_exclusivity_reading, low_income_state_governments).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(trips_agreement_interpretive_kernel__strong_exclusivity_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(trips_agreement_interpretive_kernel__strong_exclusivity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__strong_exclusivity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(trips_agreement_interpretive_kernel__strong_exclusivity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness measures the degree to which the constraint transfers resources from low-income markets to patent holders and subsidizes high-income R&D investment via monopoly rents. At 0.78, this reflects high pricing power maintained across the interval despite mounting public health pressure. Suppression at 0.81 reflects active enforcement: WTO dispute panels enforce narrow interpretations, trade retaliation against compulsory licensing invokers (Canada's generic AIDS drugs exports, India's compulsory licensing), and technical barriers to generic scaling. Theater ratio rises from 0.18 to 0.42 over the interval because enforcement increasingly invokes 'innovation incentive' rhetoric while pharmaceutical price inflation in low-income markets accelerates, decoupling the stated rationale from the observable constraint operation. Accessibility collapse at 0.72 reflects that alternatives (generic access, compulsory licensing, parallel imports) exist nominally but are suppressed by enforcement cost and retaliation threat. Resistance at 0.68 reflects sustained public health advocacy, India's and Thailand's compulsory licensing stands, and periodic WTO TRIPS flexibilities declarations that never change enforcement practice — real resistance that does not dislodge the constraint.
 *
 * PERSPECTIVAL GAP:
 *   From the pharmaceutical patent holder and high-income government seats, the strong reading is genuine coordination: it solves a real problem (forum-shopping threatening innovation) and produces fair outcomes (uniform rules, market-clearing prices reflecting R&D cost). From the low-income patient and generic manufacturer seats, the same reading operates as pure extraction: monopoly pricing that has no legitimate basis in innovation cost for off-patent or generic-feasible drugs, enforced by asymmetric trade power. The gap is not resolvable by choosing better metrics — it is structural to the reading's operation. The engine's per-seat classification will capture this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Pharmaceutical patent holders compute near d=0.0 (full beneficiary): they set the rules through high-income government influence, capture the rents, and bear no cost from the constraint's operation. High-income country governments compute near d=0.1 (heavy beneficiary, light cost): they domestically benefit from innovation incentives and hold enforcement power; their nominal cost is reputational (public health criticism) which they can absorb. Low-income patient populations compute near d=1.0 (full target): they pay monopoly prices, have no alternatives, cannot exit the constraint (disease does not negotiate), and derive no benefit. Generic manufacturers compute at d=0.95 (near-full target): they are structurally excluded and bear the cost of market foreclosure. Low-income state governments compute at d=0.82 (heavy target, light benefit): they theoretically benefit from innovation but pay through constrained fiscal capacity; they bear the suppression cost (retaliation threat) when they attempt flexibilities. The asymmetry in directionality is the engine's per-seat classification entry point — the constraint will likely compute as snare from the payer seats and tangled_rope from the high-income setter seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was over-statement: pre-TRIPS forum-shopping was real, but innovation in pharmaceuticals is driven primarily by market size and disease burden, not patent breadth alone. High-income markets provided sufficient incentive; low-income markets were never going to generate pharmaceutical R&D regardless of patent strength. The strong reading persists not because the founding problem is live but because high-income pharmaceutical interests and governments extract rents from the constraint's operation. The mandate (uniform high protection) has outlived its function (ensuring adequate innovation incentive) and now primarily serves extraction. Public health crises (HIV/AIDS, COVID-19) triggered circumvention attempts (compulsory licensing, TRIPS waiver negotiations) that exposed the reading's extractive core — the constraint's persistence despite these crises is theater: the innovation rationale is invoked but not the driver of enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    innovation_incentive_counterfactual,
    'Would pharmaceutical innovation for diseases prevalent in low-income countries differ if patent terms were shorter or compulsory licensing were routine?',
    'Comparative analysis of R&D investment by indication-burden: diseases with large high-income markets (cardiovascular, obesity) vs. diseases endemic to low-income regions (malaria, TB) pre- and post-TRIPS. Economic modeling isolating patent strength from market-size effects.',
    'If R&D patterns show that strong patents in low-income markets drive negligible additional investment for those markets'' diseases, the innovation justification collapses and the constraint becomes pure extraction. If R&D is substantially sensitive to patent breadth across all markets, the coordination rationale holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(innovation_incentive_counterfactual, empirical, 'Whether the strong reading''s innovation incentive claim is structurally accurate for low-income-market diseases.').

omega_variable(
    beneficiary_vs_natural_law_ambiguity,
    'Is the constraint a natural coordination solution to forum-shopping (hence its persistence is functional), or a constructed reading benefiting identifiable institutions (hence its persistence is extractive)?',
    'Historical counterfactual: what would global pharmaceutical markets have done absent TRIPS if high-income countries had acted unilaterally to enforce patent protection? If unilateral enforcement would have occurred anyway, TRIPS is constructed beneficiary capture; if TRIPS coordination was genuinely necessary, it is functional.',
    'If constructed, the constraint is a false-summit mountain — authored as natural law but instantiating a reading that benefits patent holders and high-income governments. If functional, it is genuine tangled_rope with real coordination alongside extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_vs_natural_law_ambiguity, conceptual, 'Whether the strong reading is a natural coordination solution or a constructed beneficiary capture.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.81) primarily structural — enforcement cost, retaliation threat, legal barriers to compulsory licensing invocation — or internalized — do low-income state governments accept the strong reading''s legitimacy and thereby suppress themselves?',
    'Post-suppression analysis: when low-income states invoke compulsory licensing (India 2013, Thailand 2007, 2008, 2013), what drives their choice? If driven by immediate crisis (epidemic emergency) overriding suppression, suppression is structural. If compliance resumes without crisis pressure, suppression is partially internalized.',
    'If structural, the constraint''s suppressive force degrades if high-income countries reduce retaliation threat. If internalized, suppression persists even after enforcement mechanisms weaken, and the constraint exhibits piton characteristics (maintained by institutional inertia).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression is structural enforcement cost or internalized legitimacy acceptance.').

omega_variable(
    reading_foreclosure_and_coexistence,
    'Does the strong exclusivity reading logically foreclose the public_health_flexibility_reading, or do both readings remain live positions that different parties hold simultaneously?',
    'TRIPS text analysis: can a single institutional framework adopt both readings (narrow compulsory licensing as the rule, broad flexibilities as exceptions) without internal logical contradiction? Or does adoption of one reading''s core premise make the other''s core premise incoherent?',
    'If readings foreclose each other, the kernel represents a genuine choice between incompatible frameworks. If readings coexist, the contest is empirical (which reading matches reality) or normative (which is better), not logical.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_and_coexistence, conceptual, 'Whether the strong exclusivity and public health flexibility readings are logically incompatible or operationally coexistent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 1995, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trip_tr_t1995, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 1995, 0.18).
narrative_ontology:measurement(trip_tr_t2002, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 2002, 0.24).
narrative_ontology:measurement(trip_tr_t2009, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 2009, 0.31).
narrative_ontology:measurement(trip_tr_t2016, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 2016, 0.37).
narrative_ontology:measurement(trip_tr_t2020, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 2020, 0.4).
narrative_ontology:measurement(trip_tr_t2024, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(trip_be_t1995, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 1995, 0.61).
narrative_ontology:measurement(trip_be_t2002, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 2002, 0.67).
narrative_ontology:measurement(trip_be_t2009, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 2009, 0.72).
narrative_ontology:measurement(trip_be_t2016, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 2016, 0.75).
narrative_ontology:measurement(trip_be_t2020, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 2020, 0.77).
narrative_ontology:measurement(trip_be_t2024, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(trip_su_t1995, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 1995, 0.64).
narrative_ontology:measurement(trip_su_t2002, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 2002, 0.7).
narrative_ontology:measurement(trip_su_t2009, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 2009, 0.75).
narrative_ontology:measurement(trip_su_t2016, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 2016, 0.78).
narrative_ontology:measurement(trip_su_t2020, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 2020, 0.8).
narrative_ontology:measurement(trip_su_t2024, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 2024, 0.81).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trips_agreement_interpretive_kernel__strong_exclusivity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0.12).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__strong_exclusivity_reading, trips_agreement_interpretive_kernel__public_health_flexibility_reading).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__strong_exclusivity_reading, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a contested kernel (TRIPS Article 31 compulsory licensing) shared with the public_health_flexibility_reading (sibling constraint). The two readings differ in ε (strong reading: 0.78 extractiveness, public health reading: estimated 0.42 extractiveness), beneficiary/victim structure (flipped across readings), and interpretation of text (narrow vs. broad flexibilities). They are NOT measurements of the same constraint from different angles — they are structurally distinct constraints instantiated from the same contested text. Each story carries its own ε, its own beneficiary set, its own six-questions answers. The sibling reading is a separate JSON file linked via this network edge.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(trips_agreement_interpretive_kernel__strong_exclusivity_reading, moderate, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
