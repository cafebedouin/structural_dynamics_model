% ============================================================================
% CONSTRAINT STORY: employment_boundary__formalist_employment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_employment_boundary__formalist_employment_reading, []).

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
 *   constraint_id: employment_boundary__formalist_employment_reading
 *   human_readable: Formalist Employment Boundary (Platform Worker Exclusion)
 *   domain: labor_economics/platform_economy/social_policy
 *
 * SUMMARY:
 *   This constraint instantiates the formalist_employment_reading of the
 *   contested employment_boundary kernel. Under this reading, employment is
 *   defined strictly by formal contract and direct human supervision, which
 *   excludes platform workers from the employment relationship and classifies
 *   them as independent contractors. Sibling readings are the
 *   substantive_employment_reading (economic dependence and algorithmic
 *   control qualify as employment) and the hybrid_security_reading (a third
 *   protective category for platform workers). The formalist reading
 *   generates high extraction by externalizing social-insurance and
 *   income-security costs to workers and the state welfare system, while
 *   preserving legal certainty for traditional firms and competitive cost
 *   structures for platforms.
 *
 * KEY AGENTS:
 *   - Digital labor platforms: Primary beneficiary (institutional/mobile/global) â avoid employment obligations through contractual classification.
 *   - Platform workers: Primary target (powerless/constrained/national) â bear precarity and self-insurance costs under independent-contractor status.
 *   - State welfare system: Secondary target (institutional/constrained/national) â absorbs externalized social costs without offsetting revenue from platforms.
 *   - Judiciary and legislature: Agenda setter (institutional/constrained/national) â maintains and enforces the formalist legal boundary.
 *   - Labor advocates: Excluded voice (organized/constrained/national) â argues for substantive tests but lacks seat at the rule-setting table.
 *   - Independent labor economists: Analytical observer (analytical/analytical/global) â measures the extraction gap without authority to alter the boundary.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(employment_boundary__formalist_employment_reading, 0.82).
domain_priors:suppression_score(employment_boundary__formalist_employment_reading, 0.75).
domain_priors:theater_ratio(employment_boundary__formalist_employment_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(employment_boundary__formalist_employment_reading, tangled_rope).
narrative_ontology:human_readable(employment_boundary__formalist_employment_reading, "Formalist Employment Boundary (Platform Worker Exclusion)").
narrative_ontology:topic_domain(employment_boundary__formalist_employment_reading, "labor_economics/platform_economy/social_policy").

domain_priors:requires_active_enforcement(employment_boundary__formalist_employment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(employment_boundary__formalist_employment_reading, 'ac41373c-1935-4e7c-963d-db6ee2ae72af').
narrative_ontology:cs_kernel_codification('ac41373c-1935-4e7c-963d-db6ee2ae72af', formalized).
narrative_ontology:cs_authority_grounding('ac41373c-1935-4e7c-963d-db6ee2ae72af', lineage).
narrative_ontology:cs_interpretation_layer_present('ac41373c-1935-4e7c-963d-db6ee2ae72af').
narrative_ontology:cs_reading_relation('ac41373c-1935-4e7c-963d-db6ee2ae72af', employment_boundary__substantive_employment_reading, forecloses).
narrative_ontology:cs_reading_relation('ac41373c-1935-4e7c-963d-db6ee2ae72af', employment_boundary__hybrid_security_reading, influences).
narrative_ontology:cs_axiom('ac41373c-1935-4e7c-963d-db6ee2ae72af', foundational, contract_form_determines_status).
narrative_ontology:cs_axiom_status(contract_form_determines_status, holdable).
narrative_ontology:cs_axiom_grounding('ac41373c-1935-4e7c-963d-db6ee2ae72af', contract_form_determines_status, conventional).
narrative_ontology:cs_axiom('ac41373c-1935-4e7c-963d-db6ee2ae72af', foundational, direct_supervision_required_for_employment).
narrative_ontology:cs_axiom_status(direct_supervision_required_for_employment, holdable).
narrative_ontology:cs_axiom_grounding('ac41373c-1935-4e7c-963d-db6ee2ae72af', direct_supervision_required_for_employment, conventional).
narrative_ontology:cs_reference_frame('ac41373c-1935-4e7c-963d-db6ee2ae72af', formalist_statutory_order).
narrative_ontology:cs_drift_state('ac41373c-1935-4e7c-963d-db6ee2ae72af', platform_economy_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ac41373c-1935-4e7c-963d-db6ee2ae72af', '').
narrative_ontology:cs_kernel_id(employment_boundary__formalist_employment_reading, employment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(employment_boundary__formalist_employment_reading, digital_labor_platforms).
narrative_ontology:constraint_victim(employment_boundary__formalist_employment_reading, platform_workers).
narrative_ontology:constraint_victim(employment_boundary__formalist_employment_reading, state_welfare_system).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Classify workers as independent contractors under the formalist boundary, avoiding social insurance contributions, minimum wage obligations, and severance costs. They benefit from a lean cost structure, flexible workforce scaling, and competitive pricing power derived from externalizing labor protections.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, digital_labor_platforms, beneficiary,
    institutional, generational, mobile, global).

% Classified as independent contractors despite algorithmic management and economic dependence. They bear income volatility, lack employer-funded social protections, and must self-insure for illness, unemployment, and retirement. Exit to traditional employment is often blocked by labor market structure and geographic concentration of platform demand.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, platform_workers, payer,
    powerless, biographical, constrained, national).

% Absorbs the externalized costs of worker protections through subsidized healthcare, unemployment transfers, and social assistance that platforms do not fund. The formalist boundary shifts fiscal burden from firms to public budgets without a corresponding revenue stream from the benefiting parties.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, state_welfare_system, payer,
    institutional, generational, constrained, national).

% Defines and interprets the formalist employment boundary through statute and precedent. They maintain the legal categories that classify platform workers as independent contractors, even as the economic reality of algorithmic management diverges from the doctrinal test.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, judiciary_and_legislature, agenda_setter,
    institutional, generational, constrained, national).

% Argue for substantive employment tests that account for economic dependence and algorithmic control, but are structurally excluded from the contract-negotiation table between platforms and workers. Their voice enters only through litigation, legislative lobbying, and public campaigns.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, labor_advocates, excluded,
    organized, biographical, constrained, national).

% Study the gap between formal contract status and economic reality in platform work. They provide empirical estimates of cost externalization and labor-income effects but do not set the legal boundary.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, independent_labor_economists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(employment_boundary__formalist_employment_reading, digital_labor_platforms).
narrative_ontology:fixing_cost_class(employment_boundary__formalist_employment_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a bright-line legal standard for identifying employment relationships based on formal contract terms and direct human supervision, reducing uncertainty for traditional firms about when social-insurance and wage obligations attach.
% TRANSFER_FUNCTION: Moves the cost of labor protections, social insurance, and income security from digital labor platforms to platform workers and the public welfare budget, by excluding algorithmically-managed workers from the employment category under the formalist boundary.
% ABSENT_VOICES: Platform workers in non-unionized jurisdictions without access to labor boards; consumer groups who absorb service-quality degradation from high contractor turnover; developers of portable benefits systems that would operate outside the employment/independent-contractor binary.
% DISAPPEARANCE_RATIONALE: If the formalist boundary disappeared and economic-substance or algorithmic-control tests replaced it, platforms would face reclassification of millions of workers, labor costs would rise, consumer prices would adjust, public welfare expenditure on working-age adults would shift, and the competitive advantage of the asset-light platform model would compress.
% FOUNDING_PROBLEM: Determining when an employment relationship exists for purposes of funding social insurance, ensuring workplace safety, and guaranteeing wages in an industrial economy characterized by informal, casual, and piece-rate labor.
% FOUNDING_PROBLEM_CORROBORATION: Labor historians and the International Labour Organization attest that the formalist test was designed for factory-era supervision and stable single-employer relationships, and that algorithmic management has superseded this framing. Corporate legal teams and some jurists attest the formalist test remains necessary for investment certainty and contractual predictability. No corroborating source outside the benefiting parties fully resolves the dispute.
narrative_ontology:disappearance_verdict(employment_boundary__formalist_employment_reading, world_rearranges).
narrative_ontology:founding_problem_status(employment_boundary__formalist_employment_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(employment_boundary__formalist_employment_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(employment_boundary__formalist_employment_reading, 'none', 1).
narrative_ontology:epsilon_provenance(employment_boundary__formalist_employment_reading, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(employment_boundary__formalist_employment_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(employment_boundary__formalist_employment_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(employment_boundary__formalist_employment_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is high (0.82 at interval end) because the formalist boundary decouples platforms from the full cost of labor, shifting it to workers and public budgets. Suppression (0.75) reflects active legal enforcement of the contract-form test, precedent lock-in, and legislative inaction that blocks reclassification. Theater ratio is moderate (0.45): the formalist doctrine has genuine legal-historical roots, but an increasing share of its maintenance is performativeâcourts treat contract paper as dispositive while sidestepping evidence of algorithmic control. Resistance (0.55) captures ongoing litigation, regulatory proposals, and worker organizing. The measurement series share one time grid to prevent misalignment artifacts.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (judiciary/legislature) and the beneficiary seat (platforms) should compute differently from the payer seats (workers, state welfare). From the agenda-setter perspective, the formalist rule provides necessary legal certainty and avoids a slippery slope of reclassification. From the payer perspective, the same rule operates as an enforced cost shift that subsidizes platform margins at public expense. The engine computes this divergence from the structural asymmetry in power, exit, and directional position.
 *
 * DIRECTIONALITY LOGIC:
 *   Digital labor platforms are declared beneficiaries with mobile, global scopeâdirectionality near the full-beneficiary end, so effective extraction is damped or inverted into subsidy. Platform workers are declared victims with powerless, constrained exitâdirectionality near the full-target end, amplifying effective extraction. The state welfare system is an institutional victim with constrained exit; despite institutional power, its victim declaration and limited exit push directionality toward the target end. The divergence between platform and worker seats is extreme because exit options differ radically: platforms can arbitrage jurisdictions, while workers are trapped by labor market structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The formalist boundary was built to solve a genuine categorization problem in industrial labor markets. Under mandatrophy analysis, the question is whether the coordination function (bright-line legal certainty) has been captured by extraction (platform cost externalization). The temporal measurements show rising extractiveness and rising suppression requirement, indicating that the constraint is not merely atrophied (piton) but is actively hardening around its extractive function. Classifying it as tangled_rope captures the dual nature: the coordination function is real for traditional employment, but the same structure asymmetrically extracts from platform workers.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    algorithmic_control_as_supervision,
    'Does algorithmic management on digital platforms reach the threshold of direct supervision required by the formalist employment test?',
    'Comparative jurisprudential review and empirical workplace studies measuring the granularity, predictability, and disciplinary capacity of algorithmic direction versus human supervisory oversight.',
    'If affirmative, the formalist boundary collapses for platform work and workers would be reclassified as employees, shifting costs back to platforms; if negative, the formalist reading retains its structural integrity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_control_as_supervision, empirical, 'Whether algorithmic direction constitutes direct supervision under formalist legal tests').

omega_variable(
    formalist_coordination_genuineness,
    'Is the bright-line formalist test a genuine coordination mechanism for labor markets, or primarily a retrospective justification for cost externalization?',
    'Historical analysis of the test''s origin and counterfactual assessment of litigation volume and investment behavior under alternative multi-factor or substantive tests.',
    'If the coordination function is genuine, the constraint remains tangled_rope; if the coordination story is entirely cover, reclassification toward snare is warranted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(formalist_coordination_genuineness, conceptual, 'Whether formalist legal certainty is a real coordination function or a cover story').

omega_variable(
    kernel_sibling_foreclosure,
    'Does the formalist employment reading logically foreclose the substantive employment reading within a single legal framework, or can they coexist as interpretive options?',
    'Analysis of whether any single jurisdiction''s highest court has held both contract-form and economic-reality tests as equally valid primary standards for the same workers.',
    'If they cannot coexist in one framework, the engine''s foreclosure computation should register a hard boundary between these kernel readings; if they can, coexists_with is the appropriate relation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_sibling_foreclosure, conceptual, 'Logical relationship between formalist and substantive readings in a single legal framework').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(employment_boundary__formalist_employment_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(empl_tr_t0, employment_boundary__formalist_employment_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(empl_tr_t6, employment_boundary__formalist_employment_reading, theater_ratio, 6, 0.3).
narrative_ontology:measurement(empl_tr_t12, employment_boundary__formalist_employment_reading, theater_ratio, 12, 0.35).
narrative_ontology:measurement(empl_tr_t18, employment_boundary__formalist_employment_reading, theater_ratio, 18, 0.4).
narrative_ontology:measurement(empl_tr_t24, employment_boundary__formalist_employment_reading, theater_ratio, 24, 0.42).
narrative_ontology:measurement(empl_tr_t30, employment_boundary__formalist_employment_reading, theater_ratio, 30, 0.45).

% Extraction over time
narrative_ontology:measurement(empl_be_t0, employment_boundary__formalist_employment_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(empl_be_t6, employment_boundary__formalist_employment_reading, base_extractiveness, 6, 0.55).
narrative_ontology:measurement(empl_be_t12, employment_boundary__formalist_employment_reading, base_extractiveness, 12, 0.65).
narrative_ontology:measurement(empl_be_t18, employment_boundary__formalist_employment_reading, base_extractiveness, 18, 0.74).
narrative_ontology:measurement(empl_be_t24, employment_boundary__formalist_employment_reading, base_extractiveness, 24, 0.79).
narrative_ontology:measurement(empl_be_t30, employment_boundary__formalist_employment_reading, base_extractiveness, 30, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(empl_su_t0, employment_boundary__formalist_employment_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(empl_su_t6, employment_boundary__formalist_employment_reading, suppression_requirement, 6, 0.58).
narrative_ontology:measurement(empl_su_t12, employment_boundary__formalist_employment_reading, suppression_requirement, 12, 0.65).
narrative_ontology:measurement(empl_su_t18, employment_boundary__formalist_employment_reading, suppression_requirement, 18, 0.7).
narrative_ontology:measurement(empl_su_t24, employment_boundary__formalist_employment_reading, suppression_requirement, 24, 0.73).
narrative_ontology:measurement(empl_su_t30, employment_boundary__formalist_employment_reading, suppression_requirement, 30, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(employment_boundary__formalist_employment_reading, substantive_employment_reading).
narrative_ontology:affects_constraint(employment_boundary__formalist_employment_reading, hybrid_security_reading).

% DUAL FORMULATION NOTE:
% The employment_boundary kernel decomposes into three constraint stories because the label 'employment relationship' conflates structurally distinct claims: a formalist legal test (this story), a substantive economic-reality test (substantive_employment_reading), and a hybrid protective category (hybrid_security_reading). Each has different beneficiary/victim structures, different Îµ values, and different failure modes. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
