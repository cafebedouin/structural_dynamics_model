% ============================================================================
% CONSTRAINT STORY: employment_boundary__hybrid_security_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_employment_boundary__hybrid_security_reading, []).

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
 *   constraint_id: employment_boundary__hybrid_security_reading
 *   human_readable: Platform Worker Hybrid Security Category
 *   domain: labor_economics/platform_economy/social_policy
 *
 * SUMMARY:
 *   This constraint instantiates the hybrid_security_reading of the
 *   employment_boundary kernel: the claim that platform workers constitute a
 *   third legal category distinct from both employees and independent
 *   contractors, entitled to tailored protectionsânotably injury insurance
 *   and limited medical coverageâbut not full employment benefits. The
 *   constraint is actively enforced through labor codes, platform inspection
 *   regimes, and adjudication of classification disputes. It coordinates
 *   genuine protections for a precarious workforce while asymmetrically
 *   extracting through the institutionalization of precarity: workers lack
 *   retirement security, career development pathways, and collective
 *   bargaining rights that formal employment provides. Platform operators are
 *   the structural beneficiaries, capturing cost savings relative to full
 *   employment obligations. The state regulator enforces the boundary. The
 *   authored metrics claim independence: the structural claim is
 *   tangled_rope, while the metrics describe moderate extraction with rising
 *   theater as the category matures.
 *
 * KEY AGENTS:
 *   - platform_workers: Primary target/victim (moderate/constrained) â receive partial protections but bear long-term precarity costs.
 *   - platform_operators: Primary beneficiary (powerful/arbitrage) â avoid full employment obligations while accessing labor.
 *   - state_regulator: Agenda setter (institutional/constrained) â designs and enforces the hybrid category.
 *   - traditional_employers: Excluded voice (organized/constrained) â compete under full employment costs.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(employment_boundary__hybrid_security_reading, 0.55).
domain_priors:suppression_score(employment_boundary__hybrid_security_reading, 0.5).
domain_priors:theater_ratio(employment_boundary__hybrid_security_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(employment_boundary__hybrid_security_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(employment_boundary__hybrid_security_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(employment_boundary__hybrid_security_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(employment_boundary__hybrid_security_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(employment_boundary__hybrid_security_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(employment_boundary__hybrid_security_reading, tangled_rope).
narrative_ontology:human_readable(employment_boundary__hybrid_security_reading, "Platform Worker Hybrid Security Category").
narrative_ontology:topic_domain(employment_boundary__hybrid_security_reading, "labor_economics/platform_economy/social_policy").

domain_priors:requires_active_enforcement(employment_boundary__hybrid_security_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(employment_boundary__hybrid_security_reading, 'ccfa629a-7795-41a1-b0e7-2d413a9aff09').
narrative_ontology:cs_kernel_codification('ccfa629a-7795-41a1-b0e7-2d413a9aff09', formalized).
narrative_ontology:cs_authority_grounding('ccfa629a-7795-41a1-b0e7-2d413a9aff09', lineage).
narrative_ontology:cs_interpretation_layer_present('ccfa629a-7795-41a1-b0e7-2d413a9aff09').
narrative_ontology:cs_reading_relation('ccfa629a-7795-41a1-b0e7-2d413a9aff09', employment_boundary__formalist_employment_reading, coexists_with).
narrative_ontology:cs_reading_relation('ccfa629a-7795-41a1-b0e7-2d413a9aff09', employment_boundary__substantive_employment_reading, coexists_with).
narrative_ontology:cs_axiom('ccfa629a-7795-41a1-b0e7-2d413a9aff09', foundational, labor_protection_without_full_employment_obligation).
narrative_ontology:cs_axiom_status(labor_protection_without_full_employment_obligation, holdable).
narrative_ontology:cs_axiom_grounding('ccfa629a-7795-41a1-b0e7-2d413a9aff09', labor_protection_without_full_employment_obligation, conventional).
narrative_ontology:cs_axiom('ccfa629a-7795-41a1-b0e7-2d413a9aff09', foundational, third_category_necessity).
narrative_ontology:cs_axiom_status(third_category_necessity, holdable).
narrative_ontology:cs_axiom_grounding('ccfa629a-7795-41a1-b0e7-2d413a9aff09', third_category_necessity, instrumental).
narrative_ontology:cs_reference_frame('ccfa629a-7795-41a1-b0e7-2d413a9aff09', classical_employment_boundary).
narrative_ontology:cs_drift_state('ccfa629a-7795-41a1-b0e7-2d413a9aff09', contemporary_platform_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('ccfa629a-7795-41a1-b0e7-2d413a9aff09', '').
narrative_ontology:cs_kernel_id(employment_boundary__hybrid_security_reading, employment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(employment_boundary__hybrid_security_reading, platform_operators).
narrative_ontology:constraint_victim(employment_boundary__hybrid_security_reading, platform_workers).
narrative_ontology:constraint_vindicates(employment_boundary__hybrid_security_reading, third_way_labor_policy).
narrative_ontology:constraint_vindicates(employment_boundary__hybrid_security_reading, flexibility_security_narrative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive basic protections under a hybrid legal categoryâinjury insurance and limited medical coverageâbut lack retirement security, career development pathways, paid leave, and collective bargaining rights. They trade full employment protections for schedule flexibility and platform access, yet remain structurally precarious with limited mobility into formal employment.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, platform_workers, payer,
    moderate, biographical, constrained, national).

% Avoid full employment obligations such as social security contributions, severance pay, and collective bargaining costs while retaining access to a large, flexible workforce. The hybrid category reduces labor costs and regulatory liability relative to formal employment, capturing the difference as structural subsidy.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, platform_operators, beneficiary,
    powerful, generational, arbitrage, global).

% Drafts, legislates, and enforces the hybrid worker category through labor codes, inspection regimes, and classification dispute adjudication. Balances worker protection demands against platform industry growth, tax revenue goals, and employment statistics.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, state_regulator, agenda_setter,
    institutional, generational, constrained, national).

% Compete with platform operators while bearing full employment costs and regulatory burdens. They would argue for a level playing field but are not central to the design or enforcement of the hybrid category.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, traditional_employers, excluded,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(employment_boundary__hybrid_security_reading, platform_operators).
narrative_ontology:fixing_cost_class(employment_boundary__hybrid_security_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legal framework that grants platform workers basic protectionsâprincipally injury insurance and limited medical coverageâwithout imposing full employment obligations on platforms, attempting to reconcile labor market flexibility with minimum security.
% TRANSFER_FUNCTION: Transfers partial social protection costs from platform operators to public or pooled insurance schemes, while transferring the risks of income instability, retirement insecurity, and career stagnation to platform workers.
% ABSENT_VOICES: Traditional employers competing under full employment costs; worker advocates demanding full employment status and collective bargaining rights; future social security systems that will absorb the pension and care gaps created by non-contributory platform work.
% DISAPPEARANCE_RATIONALE: If the hybrid category vanished overnight, platforms would face immediate legal pressure to classify workers as either full employees or pure independent contractors, fundamentally altering cost structures, pricing models, and labor supply. Worker income stability and social protection levels would shift dramatically depending on which boundary replaced the hybrid status.
% FOUNDING_PROBLEM: The rapid growth of digital labor platforms created a large workforce that did not fit neatly into classical employment or independent contractor categories, leaving workers without basic protections and states without clear regulatory tools.
% FOUNDING_PROBLEM_CORROBORATION: Labor unions and academic labor economists attest the founding problem is misdiagnosed: the issue is not a categorical mismatch requiring a third way, but a power asymmetry that the hybrid status papers over. International Labour Organization reports and comparative labor policy analyses from outside the benefiting parties document persistent protection gaps under hybrid regimes.
narrative_ontology:disappearance_verdict(employment_boundary__hybrid_security_reading, world_rearranges).
narrative_ontology:founding_problem_status(employment_boundary__hybrid_security_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(employment_boundary__hybrid_security_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(employment_boundary__hybrid_security_reading, 'none', 1).
narrative_ontology:epsilon_provenance(employment_boundary__hybrid_security_reading, 0.55, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(employment_boundary__hybrid_security_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(employment_boundary__hybrid_security_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(employment_boundary__hybrid_security_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.55) because the constraint transfers substantial costsâretirement, career development, collective bargaining powerâto workers while providing only partial protection. Suppression is moderate (0.50): the hybrid category must be actively enforced against both worker reclassification claims and platform evasion. Theater ratio is moderate (0.40): the protection narrative is partially real but increasingly performs legitimacy for a structurally precarious arrangement. Accessibility collapse is 0.60 because the hybrid category partially forecloses both full employment and pure contractor alternatives by creating a statutory middle path that captures the workforce. Resistance is 0.45: workers and unions actively challenge the boundary, but the partial benefits dampen mobilization. Temporal measurements show gradual institutionalization: extraction, theater, and enforcement requirements rise as the category becomes entrenched and its limitations become visible.
 *
 * PERSPECTIVAL GAP:
 *   From the platform operator seat, the constraint is a necessary regulatory accommodation enabling flexible work models; from the worker seat, it is a partial safety net that locks in precarity by foreclosing full employment pathways. The state regulator experiences it as a pragmatic compromise. The engine computes these divergences from identical structural data via directionality and scope scaling.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform workers are declared victims (high d, amplified effective extraction) because they bear the uncaptured costs of the hybrid status despite partial benefits. Platform operators are declared beneficiaries (low d, damped or inverted extraction) because the constraint subsidizes their labor model by externalizing social reproduction costs. The state regulator sits near symmetric: it does not collect extraction but bears enforcement costs and political friction; its directionality reverts to the institutional power-atom fallback.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mandatrophy mislabeling by requiring both coordination (protections) and asymmetric extraction (precarity) for Tangled Rope classification. A purely protective reading would ignore the victim set and compute as Rope; a purely extractive reading would ignore the genuine injury and medical coverage and compute as Snare. The authored victim and beneficiary declarations force the engine to register the hybrid structure. The founding problem (unprotected platform workers) is partially live but contested, preventing automatic Piton classification despite the moderate theater ratio.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    third_category_stability,
    'Is the third-category hybrid status a durable institutional form or a transitional political compromise that delays full employment classification?',
    'Longitudinal tracking of jurisdictions adopting hybrid categories: if they migrate toward full employment or revert to contractor status over 10-20 years, the form is transitional; if stable, it is durable.',
    'If transitional, the constraint is a Scaffold rather than a Tangled Rope, altering the directionality of its sunset clause and the legitimacy of its enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(third_category_stability, conceptual, 'Whether the hybrid category is a permanent labor market institution or a political holding pattern.').

omega_variable(
    protection_coverage_genuine,
    'Do the reported basic protection coverage rates represent genuine risk reduction or nominal compliance that fails to cover actual worker needs?',
    'Benefit utilization studies and worker outcome data comparing hybrid-category workers to formal employees and pure contractors in the same sectors.',
    'If nominal, the coordination function is weaker than claimed and the constraint shifts toward Snare; if genuine, the Tangled Rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(protection_coverage_genuine, empirical, 'Whether basic protections under hybrid status are effective or performative.').

omega_variable(
    sibling_reading_pressure,
    'Does the hybrid security reading exert structural pressure that forecloses substantive employment classification in practice, despite coexisting with it doctrinally?',
    'Comparative analysis of jurisdictions with hybrid statutes versus those without: if hybrid statutes reduce the rate of successful employment reclassification claims, the reading influences the substantive reading; if rates are independent, they merely coexist.',
    'If influencing or foreclosing in practice, the hybrid reading operates as a functional barrier to full employment despite its nominally intermediate position.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_pressure, conceptual, 'Structural effect of hybrid category on substantive employment claims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(employment_boundary__hybrid_security_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eb_hybrid_tr_t0, employment_boundary__hybrid_security_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(eb_hybrid_tr_t4, employment_boundary__hybrid_security_reading, theater_ratio, 4, 0.22).
narrative_ontology:measurement(eb_hybrid_tr_t8, employment_boundary__hybrid_security_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement(eb_hybrid_tr_t12, employment_boundary__hybrid_security_reading, theater_ratio, 12, 0.33).
narrative_ontology:measurement(eb_hybrid_tr_t16, employment_boundary__hybrid_security_reading, theater_ratio, 16, 0.36).
narrative_ontology:measurement(eb_hybrid_tr_t20, employment_boundary__hybrid_security_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(eb_hybrid_tr_t24, employment_boundary__hybrid_security_reading, theater_ratio, 24, 0.4).

% Extraction over time
narrative_ontology:measurement(eb_hybrid_be_t0, employment_boundary__hybrid_security_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(eb_hybrid_be_t4, employment_boundary__hybrid_security_reading, base_extractiveness, 4, 0.38).
narrative_ontology:measurement(eb_hybrid_be_t8, employment_boundary__hybrid_security_reading, base_extractiveness, 8, 0.45).
narrative_ontology:measurement(eb_hybrid_be_t12, employment_boundary__hybrid_security_reading, base_extractiveness, 12, 0.5).
narrative_ontology:measurement(eb_hybrid_be_t16, employment_boundary__hybrid_security_reading, base_extractiveness, 16, 0.53).
narrative_ontology:measurement(eb_hybrid_be_t20, employment_boundary__hybrid_security_reading, base_extractiveness, 20, 0.54).
narrative_ontology:measurement(eb_hybrid_be_t24, employment_boundary__hybrid_security_reading, base_extractiveness, 24, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(eb_hybrid_su_t0, employment_boundary__hybrid_security_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(eb_hybrid_su_t4, employment_boundary__hybrid_security_reading, suppression_requirement, 4, 0.32).
narrative_ontology:measurement(eb_hybrid_su_t8, employment_boundary__hybrid_security_reading, suppression_requirement, 8, 0.38).
narrative_ontology:measurement(eb_hybrid_su_t12, employment_boundary__hybrid_security_reading, suppression_requirement, 12, 0.44).
narrative_ontology:measurement(eb_hybrid_su_t16, employment_boundary__hybrid_security_reading, suppression_requirement, 16, 0.47).
narrative_ontology:measurement(eb_hybrid_su_t20, employment_boundary__hybrid_security_reading, suppression_requirement, 20, 0.49).
narrative_ontology:measurement(eb_hybrid_su_t24, employment_boundary__hybrid_security_reading, suppression_requirement, 24, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(employment_boundary__hybrid_security_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(employment_boundary__hybrid_security_reading, formalist_employment_reading).
narrative_ontology:affects_constraint(employment_boundary__hybrid_security_reading, substantive_employment_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the employment_boundary kernel, which decomposes the natural-language concept of 'employment status' into three structurally distinct claims: formalist (binary contract-based), substantive (binary dependence-based), and hybrid (third category). Each reading has distinct epsilon, beneficiary/victim structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
