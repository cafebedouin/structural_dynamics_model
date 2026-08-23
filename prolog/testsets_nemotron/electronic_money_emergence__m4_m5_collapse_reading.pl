% ============================================================================
% CONSTRAINT STORY: electronic_money_emergence__m4_m5_collapse_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_electronic_money_emergence__m4_m5_collapse_reading, []).

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
 *   constraint_id: electronic_money_emergence__m4_m5_collapse_reading
 *   human_readable: M4/M5 Statistical Distinction as Retroactive Category Constructor
 *   domain: economic_history/monetary_theory/technology_studies
 *
 * SUMMARY:
 *   The M4/M5 statistical distinction — drawn in the early 1980s as a
 *   residual category for 'liquid assets beyond M3' — retroactively created
 *   the empirical object 'electronic money' by giving a measurement bucket a
 *   name. Before the distinction, there was no unified category of electronic
 *   money; there were telegraphic transfers, bank giros, Eurodollar ledgers,
 *   private clearing systems. The statistical line did not discover a
 *   pre-existing phenomenon; it performed the phenomenon into existence by
 *   making the residual category the definition of the thing. The constraint
 *   is the stabilization of this measurement artifact as an ontological
 *   commitment: 'electronic money' is whatever falls on the M5 side of the
 *   M4/M5 line. The beneficiaries (central bank statisticians, policy
 *   framers, regulatory architects) collect epistemic authority and
 *   regulatory capture from the convention's stability. The victims
 *   (alternative historians, pre-digitization practitioners, theoretical
 *   physicists) bear the cost of epistemic exclusion. The constraint is a
 *   piton: its original coordination function (tracking 1980s near-money
 *   substitution) is dead, but the measurement convention persists through
 *   institutional inertia and the self-reinforcing infrastructure of
 *   regulation, reporting, and policy language built atop it.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(electronic_money_emergence__m4_m5_collapse_reading, 0.42).
domain_priors:suppression_score(electronic_money_emergence__m4_m5_collapse_reading, 0.28).
domain_priors:theater_ratio(electronic_money_emergence__m4_m5_collapse_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(electronic_money_emergence__m4_m5_collapse_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(electronic_money_emergence__m4_m5_collapse_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(electronic_money_emergence__m4_m5_collapse_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(electronic_money_emergence__m4_m5_collapse_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(electronic_money_emergence__m4_m5_collapse_reading, piton).
narrative_ontology:human_readable(electronic_money_emergence__m4_m5_collapse_reading, "M4/M5 Statistical Distinction as Retroactive Category Constructor").
narrative_ontology:topic_domain(electronic_money_emergence__m4_m5_collapse_reading, "economic_history/monetary_theory/technology_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(electronic_money_emergence__m4_m5_collapse_reading, '1ddecb22-70c5-4e9d-8c4a-77c8ab8a005a').
narrative_ontology:cs_kernel_codification('1ddecb22-70c5-4e9d-8c4a-77c8ab8a005a', formalized).
narrative_ontology:cs_authority_grounding('1ddecb22-70c5-4e9d-8c4a-77c8ab8a005a', extraction).
narrative_ontology:cs_interpretation_layer_present('1ddecb22-70c5-4e9d-8c4a-77c8ab8a005a').
narrative_ontology:cs_reading_relation('1ddecb22-70c5-4e9d-8c4a-77c8ab8a005a', electronic_money_emergence__became_thinkable_reading, influences).
narrative_ontology:cs_reading_relation('1ddecb22-70c5-4e9d-8c4a-77c8ab8a005a', electronic_money_emergence__first_held_reading, influences).
narrative_ontology:cs_axiom('1ddecb22-70c5-4e9d-8c4a-77c8ab8a005a', foundational, categories_are_measurement_artifacts).
narrative_ontology:cs_axiom_status(categories_are_measurement_artifacts, holdable).
narrative_ontology:cs_axiom_grounding('1ddecb22-70c5-4e9d-8c4a-77c8ab8a005a', categories_are_measurement_artifacts, empirically_contingent).
narrative_ontology:cs_axiom('1ddecb22-70c5-4e9d-8c4a-77c8ab8a005a', foundational, no_pre_measurement_referent_for_electronic_money).
narrative_ontology:cs_axiom_status(no_pre_measurement_referent_for_electronic_money, holdable).
narrative_ontology:cs_axiom_grounding('1ddecb22-70c5-4e9d-8c4a-77c8ab8a005a', no_pre_measurement_referent_for_electronic_money, empirically_contingent).
narrative_ontology:cs_reference_frame('1ddecb22-70c5-4e9d-8c4a-77c8ab8a005a', pre_statistical_monetary_practice_continuum).
narrative_ontology:cs_drift_state('1ddecb22-70c5-4e9d-8c4a-77c8ab8a005a', post_1980s_m4_m5_codification, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('1ddecb22-70c5-4e9d-8c4a-77c8ab8a005a', '').
narrative_ontology:cs_kernel_id(electronic_money_emergence__m4_m5_collapse_reading, electronic_money_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(electronic_money_emergence__m4_m5_collapse_reading, central_bank_statisticians).
narrative_ontology:constraint_beneficiary(electronic_money_emergence__m4_m5_collapse_reading, monetary_policy_framing_establishment).
narrative_ontology:constraint_beneficiary(electronic_money_emergence__m4_m5_collapse_reading, financial_regulatory_architecture).
narrative_ontology:constraint_victim(electronic_money_emergence__m4_m5_collapse_reading, alternative_monetary_historians).
narrative_ontology:constraint_victim(electronic_money_emergence__m4_m5_collapse_reading, pre_digitization_money_practitioners).
narrative_ontology:constraint_victim(electronic_money_emergence__m4_m5_collapse_reading, theoretical_monetary_physicists).
narrative_ontology:constraint_vindicates(electronic_money_emergence__m4_m5_collapse_reading, measurement_creates_its_object).
narrative_ontology:constraint_vindicates(electronic_money_emergence__m4_m5_collapse_reading, statistical_categories_are_performative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and maintain the monetary aggregates (M0 through M5) that define what counts as money. The M4/M5 boundary was drawn by statistical convention in the 1980s to capture 'liquid assets beyond M3'; it retroactively created the empirical object 'electronic money' by giving a residual category a name. They benefit from the stabilization of a measurement regime that makes their expertise indispensable.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, central_bank_statisticians, agenda_setter,
    institutional, generational, arbitrage, global).

% Uses the M4/M5 distinction as a stable reference point for policy communication, inflation targeting, and financial stability narratives. The category 'electronic money' gives them a measurable object to manage, even if the measurement created the object. They collect policy legitimacy from the apparent precision of the distinction.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, monetary_policy_framing_establishment, beneficiary,
    institutional, generational, arbitrage, global).

% Builds capital requirements, liquidity coverage ratios, and resolution frameworks around the M4/M5 line. The distinction stabilizes the regulatory perimeter; changing it would cascade through Basel accords, stress tests, and resolution planning. They administer a constraint they did not originate but now depend on.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, financial_regulatory_architecture, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(electronic_money_emergence__m4_m5_collapse_reading, financial_regulatory_architecture, agenda_setter).

% Document monetary practices (private clearing, ledger money, correspondent banking, shadow banking) that the M4/M5 grid either misses or forces into its categories. Their work is marginalized as 'non-standard' because it does not fit the measurement artifact. They bear the cost of epistemic exclusion: funding, publication venues, and policy uptake all route through the established categories.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, alternative_monetary_historians, payer,
    organized, biographical, constrained, global).

% Operated monetary systems (e.g., 1970s Eurodollar markets, telegraphic transfers, bank giro systems) that functioned as 'electronic money' before the category existed. The M4/M5 distinction retroactively erases their continuity by making 'electronic money' a post-1980s statistical artifact. They cannot exit the misclassification because their historical record is already fixed in archives the categories govern.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, pre_digitization_money_practitioners, payer,
    moderate, biographical, identity_locked, regional).

% Model money as a physical-informational phenomenon (entropy, settlement finality, ledger topology) and find the M4/M5 distinction empirically empty — it slices a continuum at a point of statistical convenience, not physical phase transition. They are excluded from central bank modeling frameworks because their variables do not map to the aggregates.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, theoretical_monetary_physicists, excluded,
    moderate, biographical, trapped, global).

% Analyze how statistical conventions performatively create the objects they purport to measure (Hacking's 'making up people', Bowker & Star's 'classification as infrastructure'). They see the M4/M5 line as a canonical case: a residual category that became an ontological commitment. They neither collect nor pay; they map the mechanism.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, measurement_critique_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, globally comparable statistical grid for central banks to communicate monetary conditions, coordinate policy expectations, and anchor financial regulation. The M4/M5 line solves a genuine coordination problem: without a shared measurement convention, cross-border policy dialogue and regulatory harmonization would lack a common language.
% TRANSFER_FUNCTION: Moves epistemic authority and regulatory capture from diverse monetary practices (private clearing, correspondent banking, shadow ledgers, non-bank payment rails) into the hands of the statistical architects who drew the M4/M5 line. The transfer is not primarily financial — it is the power to define what counts as money, what gets measured, and what becomes invisible.
% ABSENT_VOICES: Practitioners of monetary forms that the M4/M5 grid cannot see — Hawala operators, private clearing house members, crypto settlement layers, pre-1980s Eurodollar market participants — would object that their monetary reality is erased by the category. They are absent because the measurement regime defines them out of existence before they can speak.
% DISAPPEARANCE_RATIONALE: If the M4/M5 distinction vanished overnight, central banks would lose their primary cross-border coordination language for 'broad money', regulatory frameworks would lose their perimeter definitions, and the category 'electronic money' would dissolve into the heterogeneous practices it currently subsumes. A new measurement convention would have to be negotiated — the world would rearrange around a different grid.
% FOUNDING_PROBLEM: The 1970s–1980s explosion of non-bank liquid instruments (money market funds, commercial paper, Eurodollar certificates, repurchase agreements) made M3 an incomplete picture of systemic liquidity. Central banks needed a wider aggregate to track 'money-like' assets that could substitute for deposits in a crisis.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem was attested by the Bank for International Settlements' 1983 review of monetary aggregates and the Federal Reserve's 1980 Monetary Control Act debates — both outside the beneficiary set of current M4/M5 users. The problem (tracking substitution risk from near-moneys) is dead because the substitution margin has moved beyond what M4/M5 captures (into derivatives, repo, shadow banking, stablecoins). The arrangement persists as a classificatory piton: the measurement convention stabilizes itself.
narrative_ontology:disappearance_verdict(electronic_money_emergence__m4_m5_collapse_reading, world_rearranges).
narrative_ontology:founding_problem_status(electronic_money_emergence__m4_m5_collapse_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(electronic_money_emergence__m4_m5_collapse_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(electronic_money_emergence__m4_m5_collapse_reading, 'none', 1).
narrative_ontology:epsilon_provenance(electronic_money_emergence__m4_m5_collapse_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(electronic_money_emergence__m4_m5_collapse_reading_tests).
:- end_tests(electronic_money_emergence__m4_m5_collapse_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) is moderate: the constraint extracts epistemic authority from alternative monetary ontologies and regulatory capture from the perimeter it defines, but it does not extract direct financial rents. Suppression (0.28) is low: the constraint does not actively coerce; it operates by defining the terms of legitimate discourse — alternatives are not forbidden, they are rendered unintelligible. Theater ratio (0.68) is high: the measurement convention performs the object it measures; the 'security' and 'comparability' justifications are increasingly theatrical as the actual monetary system (derivatives, repo, stablecoins, CBDCs) has moved far beyond the M4/M5 grid. Accessibility collapse (0.35) is moderate: alternatives (physical-informational models, practice-based histories) exist but cannot gain traction in the institutional channels that matter. Resistance (0.15) is very low: the constraint meets almost no organized opposition because the beneficiaries control the venues where resistance would be articulated.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (statisticians, regulators), the M4/M5 distinction is a successful coordination mechanism — a shared language that enables global policy dialogue. From the payer seats (alternative historians, displaced practitioners), the same structure is an epistemic enclosure that renders their work invisible. The engine computes this divergence from the structural data; the authored claim (piton) reflects the analytical seat's reading: the coordination function is dead, the theater is high, the constraint persists by inertia.
 *
 * DIRECTIONALITY LOGIC:
 *   Central bank statisticians and the policy/regulatory establishment are structural beneficiaries: they collect epistemic authority, policy legitimacy, and regulatory capture from the convention's stability (d near 0.0–0.2). Alternative historians and pre-digitization practitioners are payers: they bear epistemic exclusion and historical erasure, with constrained or identity-locked exit (d near 0.7–0.9). Theoretical monetary physicists are excluded entirely (trapped exit, d not applicable — they are not in the game). Measurement critique scholars are analytical observers (d = 0.5 by convention).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (tracking 1980s near-money substitution) is dead — the substitution margin has moved to instruments the M4/M5 grid cannot see. The arrangement persists as a classificatory piton: the measurement convention stabilizes itself through the regulatory and reporting infrastructure built atop it. No beneficiary captures enough extraction to maintain it actively; no victim is hurt enough to fix it (the cost of rewriting Basel, stress tests, resolution frameworks, and central bank communication protocols is prohibitive). The theater ratio captures the performative maintenance: the distinction is 'kept alive' by the very infrastructure it created.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disagreement_location,
    'Where exactly do the three readings of the electronic_money_emergence kernel disagree structurally?',
    'Map each reading''s beneficiary/victim structure and claimed emergence date; the disagreement is located in whether a pre-measurement referent exists (became_thinkable/first_held say yes; m4_m5_collapse says no — the measurement created the referent).',
    'If the m4_m5_collapse reading is correct, the other two readings are analyzing a phantom object; their ε values are measuring different constraints (the measurement artifact vs. a hypothesized natural kind). This is an ε-invariance violation across readings of the same kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, conceptual, 'Structural location of disagreement across kernel readings').

omega_variable(
    piton_vs_mountain_ambiguity,
    'Is the M4/M5 distinction a piton (degraded coordination) or a mountain (statistical convention as natural law of measurement)?',
    'Test whether the constraint would persist if the regulatory infrastructure built atop it were removed. If yes → mountain (measurement conventions are irreducible); if no → piton (the convention survives only through the infrastructure it spawned).',
    'Mountain classification would imply the distinction is a genuine coordination achievement that cannot be undone; piton classification implies it is a historical accident stabilized by path dependence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(piton_vs_mountain_ambiguity, empirical, 'Whether the constraint''s persistence is structural or infrastructural').

omega_variable(
    epistemic_exclusion_mechanism,
    'Does the M4/M5 grid suppress alternatives through structural barriers (funding, publication, policy access) or through internalized categorization (practitioners adopt the grid because it is the only language available)?',
    'Post-exclusion trajectory analysis: do alternative monetary historians who leave central bank frameworks recover their epistemic independence, or do they continue to think in M4/M5 terms?',
    'If internalized, the constraint''s effective suppression is higher than the structural measure (0.28) suggests — the target carries the suppression with them. This would push the constraint toward snare/tangled_rope territory for the payer seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epistemic_exclusion_mechanism, empirical, 'Structural vs. internalized suppression mechanism for excluded practitioners').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(electronic_money_emergence__m4_m5_collapse_reading, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(elec_tr_t1970, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(elec_tr_t1980, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 1980, 0.25).
narrative_ontology:measurement(elec_tr_t1990, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 1990, 0.45).
narrative_ontology:measurement(elec_tr_t2000, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 2000, 0.58).
narrative_ontology:measurement(elec_tr_t2010, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 2010, 0.64).
narrative_ontology:measurement(elec_tr_t2025, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 2025, 0.68).

% Extraction over time
narrative_ontology:measurement(elec_be_t1970, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 1970, 0.05).
narrative_ontology:measurement(elec_be_t1980, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 1980, 0.12).
narrative_ontology:measurement(elec_be_t1990, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 1990, 0.28).
narrative_ontology:measurement(elec_be_t2000, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 2000, 0.38).
narrative_ontology:measurement(elec_be_t2010, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 2010, 0.41).
narrative_ontology:measurement(elec_be_t2025, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 2025, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(elec_su_t1970, electronic_money_emergence__m4_m5_collapse_reading, suppression_requirement, 1970, 0.05).
narrative_ontology:measurement(elec_su_t1980, electronic_money_emergence__m4_m5_collapse_reading, suppression_requirement, 1980, 0.15).
narrative_ontology:measurement(elec_su_t1990, electronic_money_emergence__m4_m5_collapse_reading, suppression_requirement, 1990, 0.22).
narrative_ontology:measurement(elec_su_t2000, electronic_money_emergence__m4_m5_collapse_reading, suppression_requirement, 2000, 0.26).
narrative_ontology:measurement(elec_su_t2010, electronic_money_emergence__m4_m5_collapse_reading, suppression_requirement, 2010, 0.27).
narrative_ontology:measurement(elec_su_t2025, electronic_money_emergence__m4_m5_collapse_reading, suppression_requirement, 2025, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(electronic_money_emergence__m4_m5_collapse_reading, information_standard).
narrative_ontology:boltzmann_floor_override(electronic_money_emergence__m4_m5_collapse_reading, 0.03).
narrative_ontology:affects_constraint(electronic_money_emergence__m4_m5_collapse_reading, electronic_money_emergence__became_thinkable_reading).
narrative_ontology:affects_constraint(electronic_money_emergence__m4_m5_collapse_reading, electronic_money_emergence__first_held_reading).
narrative_ontology:affects_constraint(electronic_money_emergence__m4_m5_collapse_reading, basel_regulatory_perimeter).
narrative_ontology:affects_constraint(electronic_money_emergence__m4_m5_collapse_reading, central_bank_communication_protocol).
narrative_ontology:affects_constraint(electronic_money_emergence__m4_m5_collapse_reading, monetary_aggregate_reporting_standard).

% DUAL FORMULATION NOTE:
% This reading decomposes the 'electronic money emergence' kernel by denying the emergence event. The became_thinkable_reading and first_held_reading treat the kernel as a genuine historical transition; this reading treats the transition as a measurement artifact. The three stories form a constraint family linked by the kernel_id. The upstream story (this reading) influences the downstream stories by changing the legitimacy conditions: if the category is a measurement artifact, the other readings' coordination functions are compromised.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(electronic_money_emergence__m4_m5_collapse_reading, institutional, 0.15).
constraint_indexing:directionality_override(electronic_money_emergence__m4_m5_collapse_reading, organized, 0.75).
constraint_indexing:directionality_override(electronic_money_emergence__m4_m5_collapse_reading, moderate, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
