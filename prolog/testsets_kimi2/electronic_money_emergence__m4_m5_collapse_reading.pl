% ============================================================================
% CONSTRAINT STORY: electronic_money_emergence__m4_m5_collapse_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:affects_constraint/2,
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
 *   domain: economic/monetary_theory/technology_studies
 *
 * SUMMARY:
 *   This constraint instantiates the m4_m5_collapse_reading of the
 *   electronic_money_emergence kernel. It treats the 'emergence' of
 *   electronic money not as a historical event in payment technology but as a
 *   retroactive measurement artifact produced by the Bank of England's M4/M5
 *   statistical distinction. The category of electronic money was stabilized
 *   by the need to maintain consistent monetary aggregates, not by a
 *   dematerialization of currency that demanded a new ontological category.
 *   The constraint is the classificatory framework itself: a Piton that
 *   continues to sort monetary reality into 'electronic' and 'other' long
 *   after the distinction has lost its operational mooring in the actual
 *   structure of bank liabilities and private money.
 *
 * KEY AGENTS:
 *   - Central bank statistical office (agenda_setter/institutional/constrained): Administers the M4/M5 categories and could revise them, but preserves them for time-series continuity.
 *   - Academic monetary economists (payer/moderate/constrained): Bear cognitive and theoretical costs of treating a statistical artifact as a natural kind.
 *   - Commercial banks (payer/powerful/constrained): Bear compliance costs and report under categories that misalign with their actual liability structures.
 *   - Digital payment innovators (excluded/moderate/trapped): Operational reality is retroactively categorized; they were absent from the boundary-drawing process.
 *   - Monetary historians (observer/moderate/analytical): Observe the gap between statistical convention and operational history without being bound by the categories.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(electronic_money_emergence__m4_m5_collapse_reading, 0.55).
domain_priors:suppression_score(electronic_money_emergence__m4_m5_collapse_reading, 0.4).
domain_priors:theater_ratio(electronic_money_emergence__m4_m5_collapse_reading, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(electronic_money_emergence__m4_m5_collapse_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(electronic_money_emergence__m4_m5_collapse_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 0.75).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(electronic_money_emergence__m4_m5_collapse_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(electronic_money_emergence__m4_m5_collapse_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(electronic_money_emergence__m4_m5_collapse_reading, piton).
narrative_ontology:human_readable(electronic_money_emergence__m4_m5_collapse_reading, "M4/M5 Statistical Distinction as Retroactive Category Constructor").
narrative_ontology:topic_domain(electronic_money_emergence__m4_m5_collapse_reading, "economic/monetary_theory/technology_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(electronic_money_emergence__m4_m5_collapse_reading, 'dc996cd4-000e-4ca4-a824-20782682c710').
narrative_ontology:cs_kernel_codification('dc996cd4-000e-4ca4-a824-20782682c710', formalized).
narrative_ontology:cs_authority_grounding('dc996cd4-000e-4ca4-a824-20782682c710', practice).
narrative_ontology:cs_interpretation_layer_present('dc996cd4-000e-4ca4-a824-20782682c710').
narrative_ontology:cs_reading_relation('dc996cd4-000e-4ca4-a824-20782682c710', electronic_money_emergence__became_thinkable_reading, coexists_with).
narrative_ontology:cs_reading_relation('dc996cd4-000e-4ca4-a824-20782682c710', electronic_money_emergence__first_held_reading, coexists_with).
narrative_ontology:cs_axiom('dc996cd4-000e-4ca4-a824-20782682c710', foundational, monetary_aggregates_are_measurement_constructs).
narrative_ontology:cs_axiom_status(monetary_aggregates_are_measurement_constructs, holdable).
narrative_ontology:cs_axiom_grounding('dc996cd4-000e-4ca4-a824-20782682c710', monetary_aggregates_are_measurement_constructs, conventional).
narrative_ontology:cs_axiom('dc996cd4-000e-4ca4-a824-20782682c710', foundational, electronic_money_lacks_natural_kind_status).
narrative_ontology:cs_axiom_status(electronic_money_lacks_natural_kind_status, holdable).
narrative_ontology:cs_axiom_grounding('dc996cd4-000e-4ca4-a824-20782682c710', electronic_money_lacks_natural_kind_status, empirically_contingent).
narrative_ontology:cs_reference_frame('dc996cd4-000e-4ca4-a824-20782682c710', monetary_aggregate_continuity).
narrative_ontology:cs_drift_state('dc996cd4-000e-4ca4-a824-20782682c710', post_digital_payment_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('dc996cd4-000e-4ca4-a824-20782682c710', '').
narrative_ontology:cs_kernel_id(electronic_money_emergence__m4_m5_collapse_reading, electronic_money_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_victim(electronic_money_emergence__m4_m5_collapse_reading, academic_monetary_economists).
narrative_ontology:constraint_victim(electronic_money_emergence__m4_m5_collapse_reading, commercial_banks).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the M4/M5 monetary aggregates and publishes them as official statistics. Could technically revise the categories but faces institutional pressure to preserve time-series continuity and historical comparability. Does not capture concentrated rents from the distinction; its persistence is driven by bureaucratic inertia rather than beneficiary interest.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, central_bank_statistical_office, agenda_setter,
    institutional, generational, constrained, national).

% Must use M4/M5 categories in empirical research, policy analysis, and graduate training. Their models and datasets are constructed around these aggregates. Challenging the categories risks peer rejection and data incompatibility, while compliance imposes a cognitive cost: treating a statistical artifact as a natural kind of money.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, academic_monetary_economists, payer,
    moderate, biographical, constrained, national).

% Required to report balance-sheet data formatted to fit M4/M5 boundaries. Bears compliance costs and conceptual distortion where the aggregates misalign with actual liability structures, particularly where demand deposits and non-deposit electronic claims are forced into a single 'electronic money' category. Cannot unilaterally opt out of the reporting framework.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, commercial_banks, payer,
    powerful, biographical, constrained, national).

% Create and operate the actual technical systems for dematerialized value transfer but were not represented in the monetary-statistics discourse that retroactively categorized their activity as 'electronic money.' Their operational reality is shoehorned into a framework designed prior to their existence, and they have no seat at the table where the statistical boundaries are maintained.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, digital_payment_innovators, excluded,
    moderate, biographical, trapped, national).

% Study the history of monetary aggregates and can trace when the M4/M5 distinction was introduced. They observe the gap between the statistical convention and the operational history of payment systems, but do not set the categories and are not bound by them in their own analytical practice.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, monetary_historians, observer,
    moderate, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(electronic_money_emergence__m4_m5_collapse_reading, diffuse).
narrative_ontology:fixing_cost_class(electronic_money_emergence__m4_m5_collapse_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the collection and publication of comparable monetary stock data across UK financial institutions, enabling a continuous time-series of broad money aggregates for macroeconomic analysis and cross-institutional consistency in reporting.
% TRANSFER_FUNCTION: Moves the cognitive and compliance burden of fitting diverse bank liabilities into a dematerialized 'electronic money' category from the statistical office to commercial banks and academic researchers, while retroactively imposing conceptual order on prior payment innovation.
% ABSENT_VOICES: Digital payment innovators and heterodox monetary theorists who would argue that the category 'electronic money' obscures the continuity between physical and dematerialized liabilities; they were not represented in the central banking committees that fixed the M4/M5 boundary.
% DISAPPEARANCE_RATIONALE: If the M4/M5 distinction vanished, official monetary statistics would lose their primary broad-money aggregates, historical time-series would require reconstruction, academic empirical work would need recalibration around new categories, and the very term 'electronic money' would likely dissolve back into broader credit aggregates or payment-system analysis.
% FOUNDING_PROBLEM: The need to measure and control broad money supply in the face of 1980s financial innovation that blurred traditional retail deposit boundaries and made existing narrow aggregates insufficient for monetary policy.
% FOUNDING_PROBLEM_CORROBORATION: Central bank publications attest to the original need for broad money measurement. Independent monetary historians and science-and-technology-studies scholars attest that the specific M4/M5 boundary was always partially arbitrary, and post-2008 monetary policy has shifted away from quantity-targeting toward interest-rate corridors and unconventional tools, corroborating that the founding problem is no longer live.
narrative_ontology:disappearance_verdict(electronic_money_emergence__m4_m5_collapse_reading, world_rearranges).
narrative_ontology:founding_problem_status(electronic_money_emergence__m4_m5_collapse_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(electronic_money_emergence__m4_m5_collapse_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(electronic_money_emergence__m4_m5_collapse_reading, 'none', 1).
narrative_ontology:epsilon_provenance(electronic_money_emergence__m4_m5_collapse_reading, 0.55, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(electronic_money_emergence__m4_m5_collapse_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(electronic_money_emergence__m4_m5_collapse_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(electronic_money_emergence__m4_m5_collapse_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Theater_ratio is high (0.75) because the M4/M5 distinction is maintained primarily through performative statistical publication and methodological continuity rituals rather than by serving a live policy function. Base_extractiveness (0.55) is moderate: the constraint extracts cognitive compliance and reporting labor, but not concentrated rents. Suppression (0.40) is moderate-low because the framework persists through institutionalized data standards rather than active coercion. Resistance (0.25) is low because the costs are diffuse and the categories are entrenched by inertia. Accessibility_collapse (0.60) is moderate: once inside the monetary-economics research framework, alternatives to these categories are hard to articulate because all datasets are pre-structured by them.
 *
 * PERSPECTIVAL GAP:
 *   From the statistical office's seat, the M4/M5 distinction is a practical coordination device preserving data continuity. From the payer seats (academic economists and commercial banks), the same structure appears as a cognitive straitjacket and compliance burden that no longer maps onto the monetary system it purports to describe. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The central bank statistical office is the agenda_setter but not a beneficiary; it administers the constraint without capturing concentrated gains, so its derived directionality sits near symmetric. Academic monetary economists and commercial banks are declared victims (payers): they bear conceptual and compliance costs respectively, with constrained exit options, pushing their directionality toward the target end. No beneficiary is declared because the Piton structure explicitly lacks a concentrated profit-capturing seat; extraction is diffuse and inertial.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was measuring broad money for monetary policy control in an era of financial innovation. That problem is now dead: post-2008 monetary policy operates through interest-rate corridors, forward guidance, and quantitative easing rather than broad-money targeting. Yet the M4/M5 distinction persists because removing it would break decades of time-series continuity and require admitting that a core category was artifactual. This is classic mandatrophy: the constraint's mandate has outlived its function, and what remains is institutional inertia dressed in statistical rigor.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'Does this reading foreclose the sibling readings that posit a genuine emergence event, or merely coexist as an alternative historiography?',
    'Analysis of whether the collapse reading''s core premise (no genuine emergence) is logically compatible with holding that digital money became thinkable or was first held at a specific date within a single framework.',
    'If foreclosing, the collapse reading operates as a stronger refutation of the kernel''s other constraints; if coexisting, the kernel remains genuinely undecided and the readings compete without structural displacement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Structural relationship of this reading to sibling readings in the emergence kernel').

omega_variable(
    measurement_convention_naturalness,
    'Is the M4/M5 distinction a purely arbitrary convention, or does it track a real structural feature of the monetary system that was merely named later?',
    'Historical investigation into whether the boundary between M4 and M5 correlates with independently identifiable behavioral, institutional, or technological differences in money holding and creation.',
    'If the distinction tracks real structure, the constraint is better read as a Scaffold or Rope that coordinated measurement; if purely arbitrary, the Piton reading is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(measurement_convention_naturalness, empirical, 'Whether the statistical distinction corresponds to any underlying monetary reality').

omega_variable(
    theater_vs_inertia,
    'Is the persistence of the M4/M5 distinction driven by active institutional theater, or by passive data-collection inertia?',
    'Compare the resource cost of revising the statistical series against the symbolic and institutional cost of admitting the category is artifactual; identify whether performative maintenance (press releases, methodological defenses) exceeds the technical cost of revision.',
    'A high theater ratio would confirm piton status; pure inertia without performative defense might suggest a different atrophied structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(theater_vs_inertia, conceptual, 'Whether persistence is theatrical or purely inertial').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(electronic_money_emergence__m4_m5_collapse_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(emem_m4m5_tr_t0, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(emem_m4m5_tr_t10, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(emem_m4m5_tr_t20, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 20, 0.5).
narrative_ontology:measurement(emem_m4m5_tr_t30, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 30, 0.65).
narrative_ontology:measurement(emem_m4m5_tr_t40, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 40, 0.75).

% Extraction over time
narrative_ontology:measurement(emem_m4m5_be_t0, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(emem_m4m5_be_t10, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(emem_m4m5_be_t20, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(emem_m4m5_be_t30, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 30, 0.52).
narrative_ontology:measurement(emem_m4m5_be_t40, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 40, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(emem_m4m5_su_t0, electronic_money_emergence__m4_m5_collapse_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(emem_m4m5_su_t10, electronic_money_emergence__m4_m5_collapse_reading, suppression_requirement, 10, 0.3).
narrative_ontology:measurement(emem_m4m5_su_t20, electronic_money_emergence__m4_m5_collapse_reading, suppression_requirement, 20, 0.35).
narrative_ontology:measurement(emem_m4m5_su_t30, electronic_money_emergence__m4_m5_collapse_reading, suppression_requirement, 30, 0.38).
narrative_ontology:measurement(emem_m4m5_su_t40, electronic_money_emergence__m4_m5_collapse_reading, suppression_requirement, 40, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(electronic_money_emergence__m4_m5_collapse_reading, electronic_money_emergence__became_thinkable_reading).
narrative_ontology:affects_constraint(electronic_money_emergence__m4_m5_collapse_reading, electronic_money_emergence__first_held_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the electronic_money_emergence kernel. The m4_m5_collapse_reading decomposes the colloquial label 'emergence of electronic money' into a measurement-artifact claim, distinct from the conceptual-emergence and first-holding claims instantiated by sibling constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
