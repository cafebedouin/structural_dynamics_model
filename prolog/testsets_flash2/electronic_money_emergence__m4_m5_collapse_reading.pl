% ============================================================================
% CONSTRAINT STORY: electronic_money_emergence__m4_m5_collapse_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: electronic_money_emergence__m4_m5_collapse_reading
 *   human_readable: Electronic Money Emergence (M4/M5 Collapse Reading)
 *   domain: economic_history/monetary_theory/technology_studies
 *
 * SUMMARY:
 *   This constraint represents a reading of the 'emergence of electronic
 *   money' where the M4/M5 statistical distinction, introduced by central
 *   banks, retroactively created the category itself. From this perspective,
 *   there was no genuine 'emergence' event in the underlying monetary physics
 *   or technology; rather, the distinction is a classificatory artifact that
 *   stabilizes a measurement convention. The constraint is classified as a
 *   Piton because its primary function (accurate measurement of a 'new'
 *   monetary form) has atrophied, but the distinction persists due to
 *   institutional inertia and the administrative cost of changing established
 *   statistical series. It extracts low but persistent costs in conceptual
 *   distortion from historians and theorists.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(electronic_money_emergence__m4_m5_collapse_reading, 0.15).
domain_priors:suppression_score(electronic_money_emergence__m4_m5_collapse_reading, 0.05).
domain_priors:theater_ratio(electronic_money_emergence__m4_m5_collapse_reading, 0.7).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(electronic_money_emergence__m4_m5_collapse_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(electronic_money_emergence__m4_m5_collapse_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 0.7).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(electronic_money_emergence__m4_m5_collapse_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(electronic_money_emergence__m4_m5_collapse_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(electronic_money_emergence__m4_m5_collapse_reading, piton).
narrative_ontology:human_readable(electronic_money_emergence__m4_m5_collapse_reading, "Electronic Money Emergence (M4/M5 Collapse Reading)").
narrative_ontology:topic_domain(electronic_money_emergence__m4_m5_collapse_reading, "economic_history/monetary_theory/technology_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(electronic_money_emergence__m4_m5_collapse_reading, '3f050633-b7ee-45f0-8630-6797de8c3a41').
narrative_ontology:cs_kernel_codification('3f050633-b7ee-45f0-8630-6797de8c3a41', formalized).
narrative_ontology:cs_authority_grounding('3f050633-b7ee-45f0-8630-6797de8c3a41', lineage).
narrative_ontology:cs_interpretation_layer_present('3f050633-b7ee-45f0-8630-6797de8c3a41').
narrative_ontology:cs_reading_relation('3f050633-b7ee-45f0-8630-6797de8c3a41', electronic_money_emergence__became_thinkable_reading, coexists_with).
narrative_ontology:cs_reading_relation('3f050633-b7ee-45f0-8630-6797de8c3a41', electronic_money_emergence__first_held_reading, coexists_with).
narrative_ontology:cs_axiom('3f050633-b7ee-45f0-8630-6797de8c3a41', foundational, statistical_categories_define_monetary_reality).
narrative_ontology:cs_axiom_status(statistical_categories_define_monetary_reality, holdable).
narrative_ontology:cs_axiom_grounding('3f050633-b7ee-45f0-8630-6797de8c3a41', statistical_categories_define_monetary_reality, conventional).
narrative_ontology:cs_axiom('3f050633-b7ee-45f0-8630-6797de8c3a41', foundational, emergence_is_a_measurement_artifact).
narrative_ontology:cs_axiom_status(emergence_is_a_measurement_artifact, holdable).
narrative_ontology:cs_axiom_grounding('3f050633-b7ee-45f0-8630-6797de8c3a41', emergence_is_a_measurement_artifact, empirically_contingent).
narrative_ontology:cs_reference_frame('3f050633-b7ee-45f0-8630-6797de8c3a41', established_monetary_statistics).
narrative_ontology:cs_drift_state('3f050633-b7ee-45f0-8630-6797de8c3a41', contemporary_digital_currency_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3f050633-b7ee-45f0-8630-6797de8c3a41', '').
narrative_ontology:cs_kernel_id(electronic_money_emergence__m4_m5_collapse_reading, electronic_money_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(electronic_money_emergence__m4_m5_collapse_reading, central_banks).
narrative_ontology:constraint_beneficiary(electronic_money_emergence__m4_m5_collapse_reading, economic_statisticians).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(electronic_money_emergence__m4_m5_collapse_reading, monetary_historians).
narrative_ontology:constraint_victim(electronic_money_emergence__m4_m5_collapse_reading, technology_theorists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain the statistical categories (M4/M5) that define 'electronic money' as a distinct aggregate. They benefit from the stability and perceived clarity of these definitions for monetary policy and reporting, even if the underlying monetary reality is more fluid. Changing these definitions would be administratively costly and disrupt historical data series.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, central_banks, agenda_setter,
    institutional, generational, constrained, national).

% Utilize the M4/M5 distinction for their research and reporting. Their professional identity and career paths are tied to the established statistical frameworks. The distinction provides a stable object of study, even if its 'naturalness' is questioned.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, economic_statisticians, beneficiary,
    organized, biographical, identity_locked, global).

% Are forced to interpret historical monetary data through categories that may not reflect the actual evolution of money. They bear the cost of conceptual distortion, having to constantly qualify their findings against the artifact of the statistical distinction. Their 'payment' is the intellectual labor of disambiguation.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, monetary_historians, payer,
    moderate, generational, constrained, global).

% Analyze the actual technological and social changes that constitute new forms of money. They find the M4/M5 distinction to be an unhelpful, anachronistic, or misleading lens through which to understand the 'emergence' of electronic money, as it retroactively imposes a definition that obscures the true historical process. They pay in conceptual friction.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, technology_theorists, payer,
    moderate, biographical, mobile, global).

% Are unaware of the statistical distinctions and their implications. They simply use 'electronic money' as a common-sense term, without understanding its contested definitional history or the way official statistics shape its perception. They are excluded from the definitional debate.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, general_public, excluded,
    powerless, immediate, trapped, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, consistent statistical framework for measuring monetary aggregates, allowing central banks and economists to track and compare 'electronic money' over time, even if the definition is an artifact.
% TRANSFER_FUNCTION: Transfers conceptual clarity and administrative convenience to central banks and statisticians, at the cost of historical and technological accuracy for historians and theorists.
% ABSENT_VOICES: The general public, who use electronic money daily, are unaware of the definitional debates and the retroactive nature of the M4/M5 distinction. Their common-sense understanding of 'emergence' is not reflected in the official statistical categories.
% DISAPPEARANCE_RATIONALE: If the M4/M5 distinction vanished overnight, central banks would lose a key statistical tool, requiring a complete overhaul of monetary reporting. Economic statisticians would need to re-evaluate decades of data. The conceptual landscape of 'electronic money' would become more fluid, aligning more closely with technological and social evolution rather than statistical convention.
% FOUNDING_PROBLEM: The need to accurately measure and categorize different forms of money for monetary policy and economic analysis, particularly as new digital forms began to appear.
% FOUNDING_PROBLEM_CORROBORATION: Central banks and economic statisticians attest that the problem of measuring monetary aggregates is still live and critical for policy. Monetary historians and technology theorists corroborate the *existence* of the problem but contest the *adequacy* of the M4/M5 solution, arguing it creates more conceptual problems than it solves.
narrative_ontology:disappearance_verdict(electronic_money_emergence__m4_m5_collapse_reading, world_rearranges).
narrative_ontology:founding_problem_status(electronic_money_emergence__m4_m5_collapse_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(electronic_money_emergence__m4_m5_collapse_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(electronic_money_emergence__m4_m5_collapse_reading, 'none', 1).
narrative_ontology:epsilon_provenance(electronic_money_emergence__m4_m5_collapse_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is low (0.15) because the 'cost' is primarily conceptual friction and administrative overhead, not direct financial transfer. Suppression is very low (0.05) as there's no active coercion to accept the definition, only the inertia of established practice. Theater ratio is high (0.7) because the continued maintenance of the distinction is largely performative, serving to uphold a historical statistical series rather than reflecting a dynamic, emergent monetary reality. Accessibility collapse is high (0.85) because once the statistical framework is adopted, it becomes very difficult to conceptualize 'electronic money' outside its terms. Resistance is low (0.1) as the 'victims' are primarily academic disciplines, not organized political actors.
 *
 * PERSPECTIVAL GAP:
 *   Central banks and statisticians experience this as a necessary, if imperfect, coordination mechanism for monetary policy. Historians and theorists experience it as an inertial, distorting classification that obscures the true evolution of money. The engine's classification as a Piton reflects the latter, highlighting the atrophied function and performative maintenance.
 *
 * DIRECTIONALITY LOGIC:
 *   Central banks and economic statisticians are beneficiaries, gaining administrative convenience and a stable object of study (d near 0.0). Monetary historians and technology theorists are payers, bearing the cost of conceptual distortion and having to work around the artifact (d near 1.0). The general public is excluded, unaware of the debate.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (to accurately measure new forms of money) has outlived its original function in the sense that the 'newness' it was meant to capture has become an artifact of its own measurement. The classification as a Piton prevents mislabeling it as a Rope (genuine coordination) or Snare (active extraction), instead highlighting its inertial, performative persistence. The low extractiveness and high theater ratio are key to this resolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    statistical_artifact_vs_real_emergence,
    'Is the M4/M5 distinction truly a statistical artifact that retroactively created ''electronic money'', or does it reflect a genuine underlying monetary phenomenon?',
    'Consensus among monetary historians and technology theorists on a non-statistical definition of electronic money''s emergence, or a re-evaluation by central banks that aligns statistical categories with technological evolution.',
    'If it''s a pure artifact, the Piton classification is robust. If it reflects a genuine phenomenon, the constraint might be reclassified as a Rope (coordination of a real distinction) with lower theater.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(statistical_artifact_vs_real_emergence, conceptual, 'Ambiguity over whether ''electronic money'' is a statistical construct or a real emergent category.').

omega_variable(
    cost_of_conceptual_distortion,
    'How significant is the ''cost'' of conceptual distortion borne by monetary historians and technology theorists?',
    'Quantitative analysis of academic citations, research output, and pedagogical materials to measure the effort expended in disambiguating or working around the M4/M5 distinction.',
    'If the cost is higher than currently estimated, the extractiveness metric might increase, potentially shifting the classification towards a Tangled Rope if beneficiaries are also identified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_of_conceptual_distortion, empirical, 'Quantifying the intellectual cost of working with an artifactual definition.').

omega_variable(
    framing_under_determination_m4_m5,
    'Does the choice to frame ''electronic_money_emergence'' as a statistical classification (this reading) versus a conceptual or institutional event (sibling readings) produce different cs_pattern classifications?',
    'Compare the cs_pattern output for this reading with the cs_pattern outputs of ''became_thinkable_reading'' and ''first_held_reading''. If different, analyze the structural elements that drive the divergence.',
    'If alternative framings yield different CS patterns, it highlights the sensitivity of commitment-system classification to the initial choice of kernel and authority. This reading''s Piton classification might be stable, but the overall kernel''s classification would be highly context-dependent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framing_under_determination_m4_m5, conceptual, 'Framing choice between statistical, conceptual, or institutional emergence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(electronic_money_emergence__m4_m5_collapse_reading, 1980, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(elec_tr_t1980, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 1980, 0.5).
narrative_ontology:measurement(elec_tr_t1990, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 1990, 0.6).
narrative_ontology:measurement(elec_tr_t2000, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 2000, 0.65).
narrative_ontology:measurement(elec_tr_t2010, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 2010, 0.68).
narrative_ontology:measurement(elec_tr_t2020, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 2020, 0.7).

% Extraction over time
narrative_ontology:measurement(elec_be_t1980, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 1980, 0.1).
narrative_ontology:measurement(elec_be_t1990, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 1990, 0.12).
narrative_ontology:measurement(elec_be_t2000, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 2000, 0.14).
narrative_ontology:measurement(elec_be_t2010, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 2010, 0.15).
narrative_ontology:measurement(elec_be_t2020, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 2020, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(elec_su_t1980, electronic_money_emergence__m4_m5_collapse_reading, suppression_requirement, 1980, 0.03).
narrative_ontology:measurement(elec_su_t1990, electronic_money_emergence__m4_m5_collapse_reading, suppression_requirement, 1990, 0.04).
narrative_ontology:measurement(elec_su_t2000, electronic_money_emergence__m4_m5_collapse_reading, suppression_requirement, 2000, 0.05).
narrative_ontology:measurement(elec_su_t2010, electronic_money_emergence__m4_m5_collapse_reading, suppression_requirement, 2010, 0.05).
narrative_ontology:measurement(elec_su_t2020, electronic_money_emergence__m4_m5_collapse_reading, suppression_requirement, 2020, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(electronic_money_emergence__m4_m5_collapse_reading, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
