% ============================================================================
% CONSTRAINT STORY: electronic_money_emergence__m4_m5_collapse_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:suppression_profile/2,
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
 *   human_readable: M4/M5 Statistical Distinction as Retroactive Electronic Money Category
 *   domain: economic/monetary_history
 *
 * SUMMARY:
 *   This constraint is the m4_m5_collapse_reading of the
 *   electronic_money_emergence kernel. It models the claim that the M4/M5
 *   statistical distinction retroactively created the category of electronic
 *   money as a measurement artifact, rather than recording an underlying
 *   emergence event in monetary history. The distinction began as a
 *   coordination device for macroeconomic measurement and hardened into a
 *   classificatory piton: a statistical convention that stabilizes discourse
 *   and policy without corresponding to underlying monetary physics. Sibling
 *   readings are the became_thinkable_reading (conceptual-possibility
 *   framing) and the first_held_reading (institutional-bearer framing).
 *
 * KEY AGENTS:
 *   - Central bank statistical division (agenda_setter, institutional/constrained) â administers the M4/M5 framework and could revise it, but is bound by time-series continuity conventions
 *   - Commercial banks (payer, powerful/constrained) â bear compliance costs of fitting operational reality into categories that predate current payment technology
 *   - Academic monetary economists (observer, organized/constrained) â analytical seat whose research programs are locked into the central bank's categorical framework
 *   - Heterodox monetary theorists (excluded, moderate/constrained) â would challenge the category's empirical validity but are structurally absent from standard-setting
 *   - Payment technology providers (excluded, moderate/constrained) â operate systems that do not map to M4/M5 boundaries and are absent from statistical design
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(electronic_money_emergence__m4_m5_collapse_reading, 0.48).
domain_priors:suppression_score(electronic_money_emergence__m4_m5_collapse_reading, 0.28).
domain_priors:theater_ratio(electronic_money_emergence__m4_m5_collapse_reading, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(electronic_money_emergence__m4_m5_collapse_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(electronic_money_emergence__m4_m5_collapse_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 0.78).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(electronic_money_emergence__m4_m5_collapse_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(electronic_money_emergence__m4_m5_collapse_reading, resistance, 0.18).

% --- Constraint claim ---
narrative_ontology:constraint_claim(electronic_money_emergence__m4_m5_collapse_reading, piton).
narrative_ontology:human_readable(electronic_money_emergence__m4_m5_collapse_reading, "M4/M5 Statistical Distinction as Retroactive Electronic Money Category").
narrative_ontology:topic_domain(electronic_money_emergence__m4_m5_collapse_reading, "economic/monetary_history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(electronic_money_emergence__m4_m5_collapse_reading, '47288a4c-b453-46ba-8e35-8a4c00dca6c2').
narrative_ontology:cs_kernel_codification('47288a4c-b453-46ba-8e35-8a4c00dca6c2', formalized).
narrative_ontology:cs_authority_grounding('47288a4c-b453-46ba-8e35-8a4c00dca6c2', expertise).
narrative_ontology:cs_interpretation_layer_present('47288a4c-b453-46ba-8e35-8a4c00dca6c2').
narrative_ontology:cs_reading_relation('47288a4c-b453-46ba-8e35-8a4c00dca6c2', electronic_money_emergence__became_thinkable_reading, influences).
narrative_ontology:cs_reading_relation('47288a4c-b453-46ba-8e35-8a4c00dca6c2', electronic_money_emergence__first_held_reading, forecloses).
narrative_ontology:cs_axiom('47288a4c-b453-46ba-8e35-8a4c00dca6c2', foundational, monetary_categories_are_conventional).
narrative_ontology:cs_axiom_status(monetary_categories_are_conventional, holdable).
narrative_ontology:cs_axiom_grounding('47288a4c-b453-46ba-8e35-8a4c00dca6c2', monetary_categories_are_conventional, conventional).
narrative_ontology:cs_axiom('47288a4c-b453-46ba-8e35-8a4c00dca6c2', foundational, measurement_artifacts_are_not_emergence_events).
narrative_ontology:cs_axiom_status(measurement_artifacts_are_not_emergence_events, holdable).
narrative_ontology:cs_axiom_grounding('47288a4c-b453-46ba-8e35-8a4c00dca6c2', measurement_artifacts_are_not_emergence_events, conventional).
narrative_ontology:cs_reference_frame('47288a4c-b453-46ba-8e35-8a4c00dca6c2', empirical_monetary_ontology).
narrative_ontology:cs_drift_state('47288a4c-b453-46ba-8e35-8a4c00dca6c2', post_m4_m5_categorization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('47288a4c-b453-46ba-8e35-8a4c00dca6c2', '').
narrative_ontology:cs_kernel_id(electronic_money_emergence__m4_m5_collapse_reading, electronic_money_emergence).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(electronic_money_emergence__m4_m5_collapse_reading, commercial_banks).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the M4/M5 monetary aggregate framework and determines how electronic money is classified within national statistics. Bound by conventions of international statistical comparability and time-series continuity, which make category revision administratively costly even when the empirical fit degrades.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, central_bank_statistical_division, agenda_setter,
    institutional, generational, constrained, national).

% Must report liability data according to M4/M5 categories that do not map cleanly to their actual product structures. Bear the compliance cost of fitting operational reality into statistical categories designed for an earlier monetary regime.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, commercial_banks, payer,
    powerful, biographical, constrained, national).

% Produce research and policy analysis using the M4/M5 framework. Their analytical categories and datasets are structured around the central bank's statistical conventions, making departure from the framework professionally costly even when the underlying concepts are contested.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, academic_monetary_economists, observer,
    organized, biographical, constrained, national).

% Challenge the empirical validity of monetary aggregates and argue that electronic money is a constructed category. Their perspectives are structurally absent from central bank working groups and standard economics curricula despite being analytically relevant.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, heterodox_monetary_theorists, excluded,
    moderate, biographical, constrained, national).

% Operate payment infrastructures that do not fit the M4/M5 liability structure. Would argue for alternative categorizations based on technical architecture rather than institutional form, but are excluded from statistical standard-setting bodies.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, payment_technology_providers, excluded,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(electronic_money_emergence__m4_m5_collapse_reading, diffuse).
narrative_ontology:fixing_cost_class(electronic_money_emergence__m4_m5_collapse_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate cross-institutional measurement of money supply by providing a uniform statistical framework for classifying liabilities across deposit-taking institutions.
% TRANSFER_FUNCTION: Moves reporting burden and analytical framing from commercial banks to central bank statistical divisions; moves historical legitimacy from actual payment-system evolution to retrospective categorical imposition.
% ABSENT_VOICES: Heterodox monetary theorists who reject aggregate-based analysis; commercial banks whose liability structures predate the category; technology firms operating payment systems that do not map cleanly to M4/M5 boundaries.
% DISAPPEARANCE_RATIONALE: If the M4/M5 distinction vanished, central bank statistical publications would require reorganization, academic monetary economics curricula would lose a foundational category, and commercial bank regulatory reporting would need recalibration â but the underlying flow of electronic payments would continue unchanged.
% FOUNDING_PROBLEM: To create a consistent monetary aggregate that could capture dematerialized payment liabilities for macroeconomic policy as physical currency declined.
% FOUNDING_PROBLEM_CORROBORATION: Monetary historians and economic sociologists outside central banking attest that the M4/M5 split was introduced to solve a specific UK statistical reporting problem and was later exported as a general category without independent empirical validation of the boundary.
narrative_ontology:disappearance_verdict(electronic_money_emergence__m4_m5_collapse_reading, world_rearranges).
narrative_ontology:founding_problem_status(electronic_money_emergence__m4_m5_collapse_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(electronic_money_emergence__m4_m5_collapse_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(electronic_money_emergence__m4_m5_collapse_reading, 'none', 1).
narrative_ontology:epsilon_provenance(electronic_money_emergence__m4_m5_collapse_reading, 0.48, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is moderate (0.48) because the constraint imposes real reporting costs and analytical misdirection, but the extraction is diffuse rather than concentrated. Suppression is low (0.28) because the constraint persists by institutional inertia and professional convention rather than active coercion; alternatives are not suppressed so much as ignored. Theater_ratio is high (0.78) and rising, indicating that maintenance of the M4/M5 distinction has become primarily performative â preserving statistical continuity and professional vocabulary rather than tracking an empirical boundary. Accessibility_collapse is moderate-low (0.35) because alternative monetary ontologies exist and are publishable, but they are not adopted in policy discourse. Resistance is very low (0.18) because the statistical framework is treated as background infrastructure rather than a contested political choice.
 *
 * PERSPECTIVAL GAP:
 *   The agenda_setter seat (central bank statisticians) experiences the constraint as a necessary administrative inheritance â a coordination mechanism whose revision would break invaluable time series. The payer seat (commercial banks) experiences it as a compliance burden disconnected from their operational reality. The excluded seats experience it as an arbitrary boundary that distorts both historical understanding and policy design. The engine computes these divergent classifications from the same structural data; the authored claim of piton does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   No concentrated beneficiary is declared because the constraint does not function as a rent-capture mechanism for any specific seat. The central bank statistical division administers the category but does not concentrate extraction from it; commercial banks bear diffuse compliance costs. Academic economists are treated as observers because their professional dependence on the category does not amount to rent collection. Directionality is therefore weakly distributed across seats, with the highest effective extraction falling on commercial banks via reporting obligations and on the public via policy based on misleading categories.
 *
 * MANDATROPHY ANALYSIS:
 *   The piton classification prevents mislabeling the M4/M5 distinction as a rope (it once coordinated measurement, but its empirical function has atrophied) and prevents mislabeling it as a snare (there is no concentrated beneficiary capturing rents from the constraint's persistence). The high theater_ratio signals that the primary activity is now the performance of statistical continuity rather than the tracking of monetary reality. The founding problem â measuring dematerialized liabilities â is dead, but the arrangement persists because the cost of fixing (breaking historical comparability) exceeds the perceived benefit for the agenda_setter.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    m4_m5_conventional_status,
    'Is the M4/M5 boundary a discovered empirical regularity in monetary liabilities, or an administratively imposed statistical convention?',
    'Cross-jurisdictional comparison of monetary aggregate definitions and historical archival analysis of when the M4/M5 category was introduced relative to the phenomena it claims to measure.',
    'If the boundary is conventional, this reading''s claim of retroactive construction is strengthened; if it tracks a natural structural break in liabilities, the reading weakens toward a rope or mountain classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(m4_m5_conventional_status, empirical, 'Whether the M4/M5 split corresponds to underlying monetary physics or is purely conventional.').

omega_variable(
    kernel_reading_foreclosure,
    'Does the m4_m5_collapse reading''s claim of ''no genuine emergence event'' foreclose the first_held reading, or can both remain live in different frameworks?',
    'Analysis of whether the first_held reading requires the emergence event to be category-independent or merely institutionally recognized prior to retrospective categorization.',
    'If the first_held reading can accommodate retrospective categorization, the relation shifts from forecloses to coexists_with; if not, the foreclosure stands and the kernel remains internally contested at the ontological level.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Structural relationship between the no-emergence and first-held readings of the kernel.').

omega_variable(
    piton_vs_snare_diffusion,
    'Is the constraint a genuine piton with diffuse costs and no concentrated beneficiary, or a disguised snare where statistical authorities extract legitimacy and academic economists capture career benefits?',
    'Examine whether central banks actively defend the M4/M5 boundary against reform proposals, and whether academic economists receive concentrated career benefits from the category''s persistence.',
    'Active defense plus concentrated benefits would reclassify toward snare; genuine inertia with diffuse costs confirms piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(piton_vs_snare_diffusion, conceptual, 'Ambiguity about whether the constraint is inertial or actively extractive.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(electronic_money_emergence__m4_m5_collapse_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(emm_m4m5_tr_t0, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(emm_m4m5_tr_t5, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 5, 0.3).
narrative_ontology:measurement(emm_m4m5_tr_t10, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 10, 0.45).
narrative_ontology:measurement(emm_m4m5_tr_t15, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 15, 0.58).
narrative_ontology:measurement(emm_m4m5_tr_t20, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 20, 0.65).
narrative_ontology:measurement(emm_m4m5_tr_t25, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 25, 0.72).
narrative_ontology:measurement(emm_m4m5_tr_t30, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 30, 0.78).

% Extraction over time
narrative_ontology:measurement(emm_m4m5_be_t0, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(emm_m4m5_be_t5, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(emm_m4m5_be_t10, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(emm_m4m5_be_t15, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 15, 0.42).
narrative_ontology:measurement(emm_m4m5_be_t20, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 20, 0.44).
narrative_ontology:measurement(emm_m4m5_be_t25, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 25, 0.46).
narrative_ontology:measurement(emm_m4m5_be_t30, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 30, 0.48).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(electronic_money_emergence__m4_m5_collapse_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(electronic_money_emergence__m4_m5_collapse_reading, electronic_money_emergence__became_thinkable_reading).
narrative_ontology:affects_constraint(electronic_money_emergence__m4_m5_collapse_reading, electronic_money_emergence__first_held_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the electronic_money_emergence kernel. The m4_m5_collapse reading treats the emergence as a measurement artifact; the became_thinkable reading locates emergence in conceptual possibility; the first_held reading locates it in institutional bearing. They are linked as a constraint family because they share the kernel but make structurally incompatible claims about when and whether electronic money emerged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
