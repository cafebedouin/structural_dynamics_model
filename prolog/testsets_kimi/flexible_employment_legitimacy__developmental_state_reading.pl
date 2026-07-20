% ============================================================================
% CONSTRAINT STORY: flexible_employment_legitimacy__developmental_state_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_flexible_employment_legitimacy__developmental_state_reading, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: flexible_employment_legitimacy__developmental_state_reading
 *   human_readable: Developmental-State Management of Flexible Employment Toward Formalization
 *   domain: labor_economics/platform_economy/social_policy
 *
 * SUMMARY:
 *   This constraint instantiates the developmental-state reading of the
 *   flexible-employment legitimacy kernel: the claim that flexible (dispatch,
 *   gig, and casual) employment is a transitional labor-market form that must
 *   be actively managed by state planning toward a formalized endpoint (here,
 *   a 2027 standardization target backed by a 12-point regulatory plan). The
 *   state sets classification rules, social-insurance phased-entry
 *   requirements, and platform compliance mandates. Platform firms operate
 *   within this framework, capturing labor-cost savings from the flexible
 *   classification while the state defers full formalization to a future
 *   target date. Workers bear the precarity: income volatility, incomplete
 *   social insurance, and exclusion from collective bargaining. The
 *   arrangement is presented as coordinated development; structurally it
 *   couples that coordination to asymmetric extraction from the flexible
 *   workforce. This is one reading of a three-way contested kernel; the
 *   natural-language label 'flexible employment' conflates market-efficiency,
 *   developmental-transition, and precarity-extraction claims, so the kernel
 *   is decomposed into separate constraint stories per the Îµ-invariance
 *   principle.
 *
 * KEY AGENTS:
 *   - state_labor_authority (agenda_setter / institutional / constrained): administers the 12-point plan and 2027 formalization targets, sets platform labor rules, and enforces compliance
 *   - platform_companies (beneficiary / powerful / constrained): operate app-based labor platforms, collect surplus from flexible classification, depend on the regulatory framework for labor-cost minimization
 *   - flexible_workers (payer / powerless / trapped): perform gig, dispatch, and casual labor without full social insurance, wage guarantees, or collective bargaining; bear income volatility and occupational risk
 *   - traditional_unions (excluded / organized / constrained): would advocate for immediate formalization but are structurally excluded from representing flexible workers under current labor-law frameworks
 *   - development_economists (observer / analytical / analytical): study informal-to-formal transitions; provide external corroboration or contestation of the founding problem
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(flexible_employment_legitimacy__developmental_state_reading, 0.68).
domain_priors:suppression_score(flexible_employment_legitimacy__developmental_state_reading, 0.62).
domain_priors:theater_ratio(flexible_employment_legitimacy__developmental_state_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__developmental_state_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__developmental_state_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__developmental_state_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(flexible_employment_legitimacy__developmental_state_reading, tangled_rope).
narrative_ontology:human_readable(flexible_employment_legitimacy__developmental_state_reading, "Developmental-State Management of Flexible Employment Toward Formalization").
narrative_ontology:topic_domain(flexible_employment_legitimacy__developmental_state_reading, "labor_economics/platform_economy/social_policy").

domain_priors:requires_active_enforcement(flexible_employment_legitimacy__developmental_state_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(flexible_employment_legitimacy__developmental_state_reading, '9cbddaf7-7a64-4c15-b00d-422b7eb7bd97').
narrative_ontology:cs_kernel_codification('9cbddaf7-7a64-4c15-b00d-422b7eb7bd97', formalized).
narrative_ontology:cs_authority_grounding('9cbddaf7-7a64-4c15-b00d-422b7eb7bd97', lineage).
narrative_ontology:cs_interpretation_layer_present('9cbddaf7-7a64-4c15-b00d-422b7eb7bd97').
narrative_ontology:cs_reading_relation('9cbddaf7-7a64-4c15-b00d-422b7eb7bd97', flexible_employment_legitimacy__market_efficiency_reading, coexists_with).
narrative_ontology:cs_reading_relation('9cbddaf7-7a64-4c15-b00d-422b7eb7bd97', flexible_employment_legitimacy__precarity_extraction_reading, coexists_with).
narrative_ontology:cs_axiom('9cbddaf7-7a64-4c15-b00d-422b7eb7bd97', foundational, managed_transition_legitimacy).
narrative_ontology:cs_axiom_status(managed_transition_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('9cbddaf7-7a64-4c15-b00d-422b7eb7bd97', managed_transition_legitimacy, conventional).
narrative_ontology:cs_axiom('9cbddaf7-7a64-4c15-b00d-422b7eb7bd97', foundational, state_coordination_necessary_for_labor_transition).
narrative_ontology:cs_axiom_status(state_coordination_necessary_for_labor_transition, holdable).
narrative_ontology:cs_axiom_grounding('9cbddaf7-7a64-4c15-b00d-422b7eb7bd97', state_coordination_necessary_for_labor_transition, instrumental).
narrative_ontology:cs_reference_frame('9cbddaf7-7a64-4c15-b00d-422b7eb7bd97', developmental_transition_framework).
narrative_ontology:cs_drift_state('9cbddaf7-7a64-4c15-b00d-422b7eb7bd97', contemporary_pre_2027, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9cbddaf7-7a64-4c15-b00d-422b7eb7bd97', '').
narrative_ontology:cs_kernel_id(flexible_employment_legitimacy__developmental_state_reading, flexible_employment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__developmental_state_reading, platform_companies).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__developmental_state_reading, flexible_workers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the 12-point plan and 2027 formalization targets, setting rules for platform labor classification, social insurance participation, and compliance audits. Can reform the framework but is constrained by economic growth targets, employment-absorption mandates, and political pressure from platform sectors.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__developmental_state_reading, state_labor_authority, agenda_setter,
    institutional, generational, constrained, national).

% Operate app-based and dispatch labor platforms that depend on flexible employment status to minimize labor costs and avoid social insurance obligations. Collect surplus from the gap between flexible wages and formal labor costs. Their business models are built around the current classification.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__developmental_state_reading, platform_companies, beneficiary,
    powerful, biographical, constrained, national).

% Perform gig, dispatch, and casual labor without full social insurance, wage guarantees, severance pay, or collective bargaining rights. Bear income volatility, occupational risk, and administrative burden while being told the arrangement is temporary and transitional. Many lack the skills, credentials, or geographic mobility to access formal-sector alternatives.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__developmental_state_reading, flexible_workers, payer,
    powerless, immediate, trapped, national).

% Would advocate for immediate formalization, collective bargaining, and full social insurance coverage for flexible workers but are structurally excluded from representing them under current labor law frameworks that classify gig and dispatch workers outside traditional bargaining units.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__developmental_state_reading, traditional_unions, excluded,
    organized, biographical, constrained, national).

% Study the transition from informal to formal labor markets in developing and middle-income economies. Some attest the phased formalization coordination function is genuine and fiscally necessary; others document extraction and delayed worker protections. They provide external analytical corroboration or contestation.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__developmental_state_reading, development_economists, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(flexible_employment_legitimacy__developmental_state_reading, platform_companies).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides labor-market flexibility during economic restructuring and growth transitions while channeling informal and gig workers toward eventual formal employment status through staged regulation, social insurance expansion, and platform compliance mandates.
% TRANSFER_FUNCTION: Moves labor-cost risk, social insurance burden, and income volatility from platform employers to flexible workers, while moving regulatory legitimacy and economic flexibility to the state and platform sector.
% ABSENT_VOICES: Flexible workers are under-represented in policy design; traditional unions are excluded by legal classification barriers; immediate-formalization advocates are backgrounded by the transitional narrative; rival labor-organizing models are suppressed by the exclusive platform-channel structure.
% DISAPPEARANCE_RATIONALE: If the constraint vanished overnight, platform business models would face immediate labor-cost restructuring and social insurance backpayments, the state's 2027 transition plan would lose its enforcement mechanism, and flexible workers would either move rapidly into formal employment or into unregulated informality â the labor market would reorganize around a different classification regime.
% FOUNDING_PROBLEM: High informal employment and rigid labor markets during rapid economic transition, where immediate universal formalization was administratively and fiscally impossible without disrupting growth, employment absorption, and platform-sector expansion.
% FOUNDING_PROBLEM_CORROBORATION: Development economists outside the state planning apparatus attest to genuine fiscal and administrative constraints of immediate formalization in transitioning economies. Labor rights NGOs and informal-worker associations outside the platform sector attest the problem has become a cover story for deferred protections and persistent extraction. The corroboration is split across seats.
narrative_ontology:disappearance_verdict(flexible_employment_legitimacy__developmental_state_reading, world_rearranges).
narrative_ontology:founding_problem_status(flexible_employment_legitimacy__developmental_state_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(flexible_employment_legitimacy__developmental_state_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(flexible_employment_legitimacy__developmental_state_reading, 'none', 1).
narrative_ontology:epsilon_provenance(flexible_employment_legitimacy__developmental_state_reading, 0.68, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(flexible_employment_legitimacy__developmental_state_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(flexible_employment_legitimacy__developmental_state_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(flexible_employment_legitimacy__developmental_state_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68â0.70) is high because the flexible classification shifts labor-cost risk and social-insurance burden from platforms to workers while platform revenue models scale on the margin saved. Suppression (0.48â0.62) rises over the interval as enforcement machinery (12-point plan audits, platform compliance directives, social-insurance collection systems) hardens to meet the 2027 target; alternatives such as independent unionization or immediate universal formalization are legally and administratively suppressed. Theater_ratio (0.25â0.45) climbs because the 2027 target and transition rhetoric provide performative legitimacy even as flexible employment expands in absolute numbers and the gap between transitional promise and stable precarity widens. Accessibility_collapse (0.48) is moderate: formal-sector alternatives exist but are inaccessible to many workers due to skill barriers, geographic mismatch, and platform-channel lock-in. Resistance (0.42) is moderate and fragmented: worker protests occur but lack institutional channels. The metrics are authored on a single shared time grid (0â36) to prevent misaligned temporal sampling.
 *
 * PERSPECTIVAL GAP:
 *   The state and platform seats should compute differently from the worker seat. From the state seat the arrangement is developmental coordination with a sunset; from the platform seat it is a licensed cost structure; from the worker seat it is enforced precarity with deferred protections. The engine derives this divergence from beneficiary/victim declarations combined with exit asymmetry: platforms and state actors have reform or arbitrage exit, while workers are trapped by skill and channel constraints. The claim/metric gap is deliberate: the developmental-state reading claims scaffold-like temporariness, while the authored metrics describe a tangled-rope structure because extraction is structurally coupled to the coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform_companies are declared beneficiaries (low directionality, subsidized by the constraint through avoided labor costs). Flexible_workers are declared victims (high directionality, targeted for extraction). The state_labor_authority sits closer to symmetric: it does not capture the surplus directly, but it captures regulatory legitimacy and developmental-state capacity; its directionality is structurally nearer the middle than the platform or worker extremes. Traditional_unions are excluded from the arrangement entirely, so their effective extraction is undefined within the constraint frame â they are outside the transfer function.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling this as pure coordination (rope) because victims are structurally declared and extractiveness is high. It prevents mislabeling it as pure extraction (snare) because a genuine coordination function exists â the managed transition absorbs labor-market shocks and provides a phased formalization pathway that immediate deregulation or immediate universal formalization might not achieve. The tangled-rope gate requires both beneficiaries and victims plus active enforcement; all three are present, so the classification is structurally grounded.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transitional_genuineness,
    'Is the 2027 formalization target a genuine institutional sunset, or a rolling performative deadline that resets extraction indefinitely?',
    'Observe post-2027 enforcement and labor classification data: if flexible employment categories persist with new target dates, the sunset is theater; if formalization is enforced and platform labor models restructure, the transition was genuine.',
    'Genuine sunset supports scaffold classification; rolling deadline supports tangled_rope or snare and raises theater_ratio.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transitional_genuineness, empirical, 'Whether the 2027 target is a real sunset or performative deadline.').

omega_variable(
    worker_suppression_mechanism,
    'Does worker compliance with flexible employment stem primarily from structural barriers (skill mismatch, geographic immobility, legal classification) or internalized belief in the developmental narrative?',
    'Worker surveys, exit interviews, and post-formalization uptake rates when alternatives become legally available; track whether suppressed voice persists after structural barriers are removed.',
    'Internalized suppression would mean effective extraction exceeds the structural measure; structural suppression confirms external coercion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(worker_suppression_mechanism, empirical, 'Structural versus internalized suppression mechanism for flexible workers.').

omega_variable(
    sibling_reading_divergence,
    'This constraint is one reading of a contested kernel. How would classification change under the precarity_extraction_reading or market_efficiency_reading?',
    'Compare with sibling constraint stories for the same kernel; the identical labor practices produce different Îµ and directionality profiles under different readings.',
    'Under precarity_extraction the constraint would likely classify as snare with higher extractiveness and no coordination credit; under market_efficiency it would classify as rope with lower extraction and no declared victims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_divergence, conceptual, 'How sibling readings of the same kernel change classification.').

omega_variable(
    state_capture_ambiguity,
    'Is the state managing flexible employment as a genuine coordination mechanism for development, or has labor-market policy been captured by platform interests?',
    'Policy-trajectory analysis: compare regulatory draft strictness against final platform-compliance costs over the interval; measure whether rules tighten toward worker protection or loosen toward platform convenience.',
    'Capture would shift the state''s directionality toward beneficiary status; genuine coordination leaves it closer to symmetric.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_capture_ambiguity, conceptual, 'Whether state management is autonomous development policy or platform capture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(flexible_employment_legitimacy__developmental_state_reading, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(flex_dev_state_tr_t0, flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(flex_dev_state_tr_t6, flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 6, 0.28).
narrative_ontology:measurement(flex_dev_state_tr_t12, flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 12, 0.32).
narrative_ontology:measurement(flex_dev_state_tr_t18, flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 18, 0.36).
narrative_ontology:measurement(flex_dev_state_tr_t24, flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 24, 0.4).
narrative_ontology:measurement(flex_dev_state_tr_t30, flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 30, 0.43).
narrative_ontology:measurement(flex_dev_state_tr_t36, flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 36, 0.45).

% Extraction over time
narrative_ontology:measurement(flex_dev_state_be_t0, flexible_employment_legitimacy__developmental_state_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(flex_dev_state_be_t6, flexible_employment_legitimacy__developmental_state_reading, base_extractiveness, 6, 0.56).
narrative_ontology:measurement(flex_dev_state_be_t12, flexible_employment_legitimacy__developmental_state_reading, base_extractiveness, 12, 0.6).
narrative_ontology:measurement(flex_dev_state_be_t18, flexible_employment_legitimacy__developmental_state_reading, base_extractiveness, 18, 0.63).
narrative_ontology:measurement(flex_dev_state_be_t24, flexible_employment_legitimacy__developmental_state_reading, base_extractiveness, 24, 0.66).
narrative_ontology:measurement(flex_dev_state_be_t30, flexible_employment_legitimacy__developmental_state_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(flex_dev_state_be_t36, flexible_employment_legitimacy__developmental_state_reading, base_extractiveness, 36, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(flex_dev_state_su_t0, flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(flex_dev_state_su_t6, flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 6, 0.52).
narrative_ontology:measurement(flex_dev_state_su_t12, flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 12, 0.55).
narrative_ontology:measurement(flex_dev_state_su_t18, flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 18, 0.58).
narrative_ontology:measurement(flex_dev_state_su_t24, flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 24, 0.6).
narrative_ontology:measurement(flex_dev_state_su_t30, flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 30, 0.61).
narrative_ontology:measurement(flex_dev_state_su_t36, flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 36, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(flexible_employment_legitimacy__developmental_state_reading, resource_allocation).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__developmental_state_reading, flexible_employment_legitimacy__market_efficiency_reading).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__developmental_state_reading, flexible_employment_legitimacy__precarity_extraction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the flexible_employment_legitimacy kernel, decomposed per the epsilon-invariance principle because the natural-language label 'flexible employment' conflates structurally distinct claims: market-clearing efficiency, developmental transition, and structural precarity/extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
